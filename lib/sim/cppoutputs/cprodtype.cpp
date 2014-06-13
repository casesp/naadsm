// Start 5/16/2014

#include "cprodtype.h"

#include <ar_general_purpose/qmath.h>

NAADSM_disease_state convertState( const HRD_status_t s ) {
  if( s == Susceptible )
    return NAADSM_StateSusceptible;
  else if( s == Latent )
    return NAADSM_StateLatent;
  else if( s == InfectiousSubclinical )
    return NAADSM_StateInfectiousSubclinical;
  else if( s == InfectiousClinical )
    return NAADSM_StateInfectiousClinical;
  else if( s == NaturallyImmune )
    return NAADSM_StateNaturallyImmune;
  else if( s == VaccineImmune )
    return NAADSM_StateVaccineImmune;
  else if( s == Destroyed )
    return NAADSM_StateDestroyed;
  else
    return NAADSM_StateUnspecified;
}


//------------------------------------------------------------------------------
// CProdType
//------------------------------------------------------------------------------
CProdType::CProdType( const QString& ptName, const int ptID ){
  _ptID = ptID;
  _descr = ptName;
  _initialOutputs = new CSMDailyOutput();
  _outputs = new CSMDailyOutput();
}


CProdType::~CProdType() {
  delete _outputs;
  delete _initialOutputs;
}


void CProdType::clearAllRecords() {
  _outputs->clear();
  _initialOutputs->clear();
}


void CProdType::resetIterationRecords() {
  // This restores current data to its original condition.
  _outputs->setAllRecordsFrom( _initialOutputs );
}


void CProdType::prepareForDay() {
  _outputs->clearNewDailyCounts();
}


void CProdType::processDailyRecords( CSMDatabase* db, int iteration, int day ) {
  _outputs->insertDatabaseOutputs( DRTDaily, db, this->id(), iteration, day );
}


void CProdType::processIterationRecords( CSMDatabase* db, int iteration ) {
  _outputs->insertDatabaseOutputs( DRTIteration, db, this->id(), iteration );
}


void CProdType::recordInfectedAtFirstDetection(){
  _outputs->firstDetUInf = _outputs->infcUAll() + _outputs->infcUIni;
  _outputs->firstDetAInf = _outputs->infcAAll() + _outputs->infcAIni;
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// CProdTypeList
//------------------------------------------------------------------------------
CProdTypeList::CProdTypeList( HRD_herd_list_t* herds ): QMap<QString, CProdType*>() {
  int nherds, i;
  HRD_herd_t* h;
  CProdType* pt;

  int ptID;
  QString ptName;

  _herds = herds;

  nherds = HRD_herd_list_length( _herds );

  for( i = 0; i < nherds; ++ i ) {
    h = HRD_herd_list_get( _herds, i );
    ptName = QString( h->production_type_name );
    ptID = int(h->production_type);

    if( ! this->contains( ptName ) ) {
      pt = new CProdType( ptName, ptID );
      this->insert( ptName, pt );
    }
  }
}


CProdTypeList::~CProdTypeList() {
  QMapIterator<QString, CProdType*> it( *this );

  while( it.hasNext())
    delete it.next().value();

  // Don't delete _herds.  It isn't owned here.
}


void CProdTypeList::clearAllRecords() {
  QMapIterator<QString, CProdType*> it( *this );

  while( it.hasNext())
    it.next().value()->clearAllRecords();
}


void CProdTypeList::resetIterationRecords() {
  QMapIterator<QString, CProdType*> it( *this );

  while( it.hasNext())
    it.next().value()->resetIterationRecords();

  // Reset outputs for all production types
  // (These outputs are not simple sums of the individual production type values)
  //-----------------------------------------------------------------------------
  _deswUMax = 0;
  _deswUMaxDay = 0;
  _deswUTimeMax = 0;
  _deswAMax = 0.0;
  _deswAMaxDay = 0;
  _deswUDaysInQueue = 0.0;
  _deswADaysInQueue = 0.0;
  _destrQueueLengthUnits = 0;
  _destrQueueLengthAnimals = 0.0;
  _unitsDestroyed = 0;

  _vacwUMax = 0;
  _vacwUMaxDay = 0;
  _vacwUTimeMax = 0;;
  _vacwAMax = 0.0;
  _vacwAMaxDay = 0;
  _vacwUDaysInQueue = 0.0;
  _vacwADaysInQueue = 0.0;
  _vaccQueueLengthUnits = 0;
  _vaccQueueLengthAnimals = 0.0;
  _unitsVaccinated = 0;

  _firstDetectionHasOccurred = false;
  _firstDetectionProcessed = false;
}


void CProdTypeList::prepareForDay() {
  QMapIterator<QString, CProdType*> it( *this );

  while( it.hasNext())
    it.next().value()->prepareForDay();
}


void CProdTypeList::processDailyRecords( CSMDatabase* db, int iteration, int day ) {
  QMapIterator<QString, CProdType*> it( *this );

  while( it.hasNext())
    it.next().value()->processDailyRecords( db, iteration, day );

  if( _firstDetectionHasOccurred && !_firstDetectionProcessed ) {
    _firstDetectionProcessed = true;

    it.toFront();
    while( it.hasNext() )
      it.next().value()->recordInfectedAtFirstDetection();
  }
}


void CProdTypeList::processIterationRecords( CSMDatabase* db, int iteration ) {
  QMapIterator<QString, CProdType*> it( *this );

  while( it.hasNext())
    it.next().value()->processIterationRecords( db, iteration );

  qDebug() << "FIXME: Do database stuff.";
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// CProdType and CProdTypeList: output recording
//------------------------------------------------------------------------------
void CProdTypeList::changeHerdState( HRD_update_t c, const int simDay ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, c.herd_index );

  NAADSM_disease_state initialState = convertState( h->initial_status );
  NAADSM_disease_state currentState = convertState( h->status );

  // Don't record initial states as changes.
  // Note, though, that herd states can change on day 1.
  if( ( 1 == simDay ) && ( c.status == initialState ) )
    return;
  else {
    CProdType* pt = this->find( h->production_type_name ).value();
    pt->updateDailyRecords( h->size, currentState, c.status );
  }
}


void CProdType::updateDailyRecords (
  const int herdAnimalCount,
  const NAADSM_disease_state oldState,
  const NAADSM_disease_state newState
){
  if( oldState == newState ){
    // do nothing
  }
  else
    _outputs->updateDailyCounts( herdAnimalCount, oldState, newState );
}


void CProdTypeList::infectHerd( HRD_infect_t r, const int simDay ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, r.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addInfectionEvent( h->size, r, simDay );
}


void CProdType::addInfectionEvent(
  const int herdAnimalCount,
  const HRD_infect_t r,
  const int simDay
){
  switch( r.infection_source_type ) {
    case NAADSM_InitiallyInfected:
      if( 1 < simDay ) {
        qFatal( "'Initial' infection occurring after day 1." );
      }
      inc( _outputs->infcUIni );
      inc( _outputs->infcAIni, herdAnimalCount );
      break;

    case NAADSM_AirborneSpread:
      inc( _outputs->infcUAir );
      inc( _outputs->infcAAir, herdAnimalCount );
      inc( _outputs->infnUAir );
      inc( _outputs->infnAAir, herdAnimalCount );
      break;

    case NAADSM_DirectContact:
      inc( _outputs->infcUDir );
      inc( _outputs->infcADir, herdAnimalCount );
      inc( _outputs->infnUDir );
      inc( _outputs->infnADir, herdAnimalCount );
      break;

    case NAADSM_IndirectContact:
      inc( _outputs->infcUInd );
      inc( _outputs->infcAInd, herdAnimalCount );
      inc( _outputs->infnUInd );
      inc( _outputs->infnAInd, herdAnimalCount );
      break;

    default:
      qFatal( QString( "Unrecognized infection mechanism (%1) in TProductionType.addInfectedByMechanism" ).arg( r.infection_source_type ).toLatin1().data() );
  }
}


void CProdTypeList::exposeHerd( HRD_expose_t e ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, e.dest_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addExposureEvent( h->size, e );
}


void CProdType::addExposureEvent( const int herdAnimalCount, const HRD_expose_t e ) {
  switch( e.exposure_method ) {
    case NAADSM_DirectContact:
      inc( _outputs->expcUDir );
      inc( _outputs->expcADir, herdAnimalCount );
      break;

    case NAADSM_IndirectContact:
      inc( _outputs->expcUInd );
      inc( _outputs->expcAInd, herdAnimalCount );
      break;

    case NAADSM_AirborneSpread:
      // Exposures by airborne spread are not (yet?) recorded,
      // but this function will still be called.
      break;

    default:
      qFatal( QString( "Unrecognized exposure method (%1) in CProdType::addExposureEvent" ).arg( e.exposure_method ).toLatin1().data() );
  }
}


void CProdTypeList::detectHerd( const HRD_detect_t d, const int simDay ) {
  qDebug() << "CProdTypeList::detectHerd may need work";

  HRD_herd_t* h = HRD_herd_list_get( _herds, d.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  this->_firstDetectionHasOccurred = true;
  pt->addDetectionEvent( h->size, d, simDay );
}


void CProdType::addDetectionEvent( const int herdAnimalCount, const HRD_detect_t d, const int simDay ){
  switch( d.reason ) {
    case NAADSM_DetectionClinicalSigns:
      inc( _outputs->detnUClin );
      inc( _outputs->detnAClin, herdAnimalCount );
      inc( _outputs->detcUClin );
      inc( _outputs->detcAClin, herdAnimalCount );
      inc( _outputs->appdUInfectious );
      break;

    case NAADSM_DetectionDiagnosticTest:
      inc( _outputs->detnUTest );
      inc( _outputs->detnATest, herdAnimalCount );
      inc( _outputs->detcUTest );
      inc( _outputs->detcATest, herdAnimalCount );
      inc( _outputs->appdUInfectious ); // FIX ME: Think about this...
      break;

    default:
      qFatal( QString( "Unsupported detection reason (%1) in CProdType::addDetectionEvent" ).arg( d.reason ).toLatin1().data() );
  }

  _outputs->lastDetection = simDay;

  if( -1 == _outputs->firstDetection )
    _outputs->firstDetection = simDay;
}


void CProdTypeList::queueHerdForDestruction( int herdIdx, const int simDay ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, herdIdx );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addDestructionQueueEvent( h->size, simDay );

  inc( _destrQueueLengthUnits );
  _destrQueueLengthAnimals = _destrQueueLengthAnimals + h->size;

  if( _destrQueueLengthUnits > _deswUMax ){
    _deswUMax = _destrQueueLengthUnits;
    _deswUMaxDay = simDay;
  }

  if( _destrQueueLengthAnimals > _deswAMax ){
    _deswAMax = _destrQueueLengthAnimals;
    _deswAMaxDay = simDay;
  }
}


void CProdType::addDestructionQueueEvent( const int herdAnimalCount, const int simDay ) {
  _outputs->addToDestrQueue( herdAnimalCount, simDay );
}


void CProdTypeList::destroyHerd( HRD_control_t c, const int simDay ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, c.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();
  pt->addDestructionEvent( h, c, simDay );

  if( NAADSM_ControlInitialState != c.reason ) {
    inc( this->_unitsDestroyed );

    int daysInQueue = simDay - c.day_commitment_made;
    _deswUTimeMax = std::max( _deswUTimeMax, daysInQueue );

    _deswUDaysInQueue = _deswUDaysInQueue + daysInQueue;
    _deswADaysInQueue = _deswADaysInQueue + ( daysInQueue * h->size );

    dec( _destrQueueLengthUnits );
    _destrQueueLengthAnimals = _destrQueueLengthAnimals - h->size;

    // Do some error checking
    if( 0 > _destrQueueLengthUnits )
      qFatal( "Number of units in destruction queue has dropped below 0 in TProductionTypeList.destroyHerd()." );

    if( 0.0 > _destrQueueLengthAnimals )
      qFatal( "Number of animals in destruction queue has dropped below 0 in TProductionTypeList.destroyHerd()." );
  }
}


void CProdType::addDestructionEvent( HRD_herd_t* h, HRD_control_t c, const int simDay ){
  if( ( -1 == _outputs->firstDestruction ) && ( NAADSM_ControlInitialState != c.reason ) )
    _outputs->firstDestruction = simDay;

  switch( c.reason ) {
    case NAADSM_ControlTraceForwardDirect:
      inc( _outputs->descUDirFwd );
      inc( _outputs->descADirFwd, h->size );
      break;

    case NAADSM_ControlTraceForwardIndirect:
      inc( _outputs->descUIndFwd );
      inc( _outputs->descAIndFwd, h->size );
      break;

    case NAADSM_ControlTraceBackDirect:
      inc( _outputs->descUDirBack );
      inc( _outputs->descADirBack, h->size );
      break;

    case NAADSM_ControlTraceBackIndirect:
      inc( _outputs->descUIndBack );
      inc( _outputs->descAIndBack, h->size );
      break;

    case NAADSM_ControlRing:
      inc( _outputs->descURing );
      inc( _outputs->descARing, h->size );
      break;

    case NAADSM_ControlDetection:
      inc( _outputs->descUDet );
      inc( _outputs->descADet, h->size );
      break;

    case NAADSM_ControlInitialState:
      inc( _outputs->descUIni );
      inc( _outputs->descAIni, h->size );
      break;

    default:
      qFatal( QString( "Unrecognized destruction reason (%1) in CProdType::addDestructionEvent" ).arg( c.reason ).toLatin1().data() );
  }

  // Deal with destruction queue
  if( NAADSM_ControlInitialState != c.reason )
    _outputs->processDestruction( h->size, simDay, c.day_commitment_made );

  // Deal with daily totals
  //-----------------------
  // Do this even for initially destroyed units.
  // "day" will have the special value -1 to initially destroyed units.
  inc( _outputs->desnUAll );
  inc( _outputs->desnAAll, h->size );
}


void CProdTypeList::queueHerdForVaccination( int herdIdx, const int simDay ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, herdIdx );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addVaccinationQueueEvent( h->size, simDay );

  inc( _vaccQueueLengthUnits );
  _vaccQueueLengthAnimals = _vaccQueueLengthAnimals + h->size;

  if( _vaccQueueLengthUnits > _vacwUMax ){
    _vacwUMax = _vaccQueueLengthUnits;
    _vacwUMaxDay = simDay;
  }

  if( _vaccQueueLengthAnimals > _vacwAMax ){
    _vaccQueueLengthAnimals = _vacwAMax;
    _vacwAMaxDay = simDay;
  }
}


void CProdType::addVaccinationQueueEvent( const int herdAnimalCount, const int simDay ){
  _outputs->addToVaccQueue( herdAnimalCount, simDay );
}


void CProdTypeList::vaccinateHerd( HRD_control_t c, const int simDay ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, c.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addVaccinationEvent( h, c, simDay );

  inc( _unitsVaccinated );

  int daysInQueue = simDay - c.day_commitment_made;

  _vacwUTimeMax = std::max( _vacwUTimeMax, daysInQueue );

  _vacwUDaysInQueue = _vacwUDaysInQueue + daysInQueue;
  _vacwADaysInQueue = _vacwADaysInQueue + ( daysInQueue * h->size );

  dec( _vaccQueueLengthUnits );
  _vaccQueueLengthAnimals = _vaccQueueLengthAnimals - h->size;

  if( 0 > _vaccQueueLengthUnits )
    qFatal( "Number of units in vaccination queue has dropped below 0 in TProductionTypeList.vaccinateHerd()." );

  if( 0.0 > _vaccQueueLengthAnimals )
    qFatal( "Number of animals in vaccination queue has dropped below 0 in TProductionTypeList.vaccinateHerd()." );
}


void CProdType::addVaccinationEvent( HRD_herd_t* h, HRD_control_t c, const int simDay ){
  if( ( -1 == _outputs->firstVaccination ) && ( NAADSM_ControlInitialState == c.reason ) )
    _outputs->firstVaccination = simDay;

  switch( c.reason ) {
    case NAADSM_ControlRing:
      inc( _outputs->vaccURing );
      inc( _outputs->vaccARing, h->size );
      break;

    case NAADSM_ControlInitialState:
      inc( _outputs->vaccUIni );
      inc( _outputs->vaccAIni, h->size );
      break;

    default:
      qFatal( QString( "Unrecognized vaccination reason (%1) in TProductionType.addVaccinationEvent" ).arg( c.reason ).toLatin1().data() );
  }

  if( NAADSM_ControlInitialState != c.reason )
    _outputs->processVaccination( h->size, simDay, c.day_commitment_made );
}


void CProdTypeList::cancelHerdVaccination( HRD_control_t c ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, c.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->subtractVaccinationQueueEvent( h->size );

  dec( _vaccQueueLengthUnits );
  _vaccQueueLengthAnimals = _vaccQueueLengthAnimals - h->size;

  if( 0 > _vaccQueueLengthUnits )
    qFatal( "Number of units in vaccination queue has dropped below 0 in TProductionTypeList.removeFromVaccQueue()." );

  if( 0.0 > _vaccQueueLengthAnimals )
    qFatal( "Number of animals in vaccination queue has dropped below 0 in TProductionTypeList.removeFromVaccQueue()." );
}


void CProdType::subtractVaccinationQueueEvent( const int herdAnimalCount ) {
  _outputs->removeFromVaccQueue( herdAnimalCount );
}


void CProdTypeList::examineHerd( HRD_exam_t e ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, e.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addHerdExamEvent( h->size, e );
}


void CProdType::addHerdExamEvent( const int herdAnimalCount, const HRD_exam_t e ){
  // Regardless of contact type, increment exmnUAll and exmnAll
  //-----------------------------------------------------------
  inc( _outputs->exmnUAll );
  inc( _outputs->exmnAAll, herdAnimalCount );


  // Depending on contact type, increment the appropriate cumulative values
  // NOTE: Some day, it might be necessary to break down the new (incident)
  // counts by contact type, but we're not doing it yet.
  //-----------------------------------------------------------------------
  if( NAADSM_DirectContact == e.contact_type ){
    switch( e.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        //inc( _outputs->exmnUDirFwd ); // Not yet(?) implemented
        //inc( _outputs->exmnADirFwd, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->exmcUDirFwd );
        inc( _outputs->exmcADirFwd, herdAnimalCount );
        break;

      case NAADSM_TraceBackOrIn:
        //inc( _outputs->exmnUDirBack );  // Not yet(?) implemented
        //inc( _outputs->exmnADirBack, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->exmcUDirBack );
        inc( _outputs->exmcADirBack, herdAnimalCount );
        break;

      default:
        qFatal( QString( "Unrecognized trace type (%1) in TProductionType.addHerdExamEvent" ).arg( e.trace_type ).toLatin1().data() );
    }
  }
  else if( NAADSM_IndirectContact == e.contact_type ){
    switch( e.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        //inc( _outputs->exmnUIndFwd );  // Not yet(?) implemented
        //inc( _outputs->exmnAIndFwd, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->exmcUIndFwd );
        inc( _outputs->exmcAIndFwd, herdAnimalCount );
        break;

      case NAADSM_TraceBackOrIn:
        //inc( _outputs->exmnUIndBack );  // Not yet(?) implemented
        //inc( _outputs->exmnAIndBack, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->exmcUIndBack );
        inc( _outputs->exmcAIndBack, herdAnimalCount );
        break;

      default:
        qFatal( QString( "Unrecognized trace type in TProductionType.addHerdExamEvent" ).arg( e.trace_type ).toLatin1().data() );
    }
  }
  else
    qFatal( QString( "Unrecognized contact type (%1) in TProductionType.addHerdExamEvent" ).arg( e.contact_type ).toLatin1().data() );
}


void CProdTypeList::testHerd( HRD_test_t t ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, t.herd_index );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addDiagnosticTestEvent( h->size, t );
}


void CProdType::addDiagnosticTestEvent( const int herdAnimalCount, const HRD_test_t t ) {
  // Deal with contact type and direction
  //-------------------------------------
  if( NAADSM_DirectContact == t.contact_type ){
    switch( t.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        //inc( _outputs->tstnUDirFwd );  // Not yet(?) implemented
        //inc( _outputs->tstnADirFwd, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->tstcUDirFwd );
        inc( _outputs->tstcADirFwd, herdAnimalCount );
        break;

      case NAADSM_TraceBackOrIn:
        //inc( _outputs->tstnUDirBack );  // Not yet(?) implemented
        //inc( _outputs->tstnADirBack, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->tstcUDirBack );
        inc( _outputs->tstcADirBack, herdAnimalCount );
        break;

      default:
        qFatal( QString( "Unrecognized trace type (%1) in TProductionType.addDiagnosticTestEvent" ).arg( t.trace_type ).toLatin1().data() );
    }
  }
  else if( NAADSM_IndirectContact == t.contact_type ){
    switch( t.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        //inc( _outputs->tstnUIndFwd );  // Not yet(?) implemented
        //inc( _outputs->tstnAIndFwd, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->tstcUIndFwd );
        inc( _outputs->tstcAIndFwd, herdAnimalCount );
        break;

      case NAADSM_TraceBackOrIn:
        //inc( _outputs->tstnUIndBack );  // Not yet(?) implemented
        //inc( _outputs->tstnAIndBack, herdAnimalCount );  // Not yet(?) implemented
        inc( _outputs->tstcUIndBack );
        inc( _outputs->tstcAIndBack, herdAnimalCount );
        break;

      default:
        qFatal( QString( "Unrecognized trace direction (%1) in TProductionType.addDiagnosticTestEvent" ).arg( t.trace_type ).toLatin1().data() );
    }
  }
  else
    qFatal( QString( "Unrecognized contactType (%1) in TProductionType.addDiagnosticTestEvent" ).arg( t.contact_type ).toLatin1().data() );


  // Deal with test result
  //----------------------
  switch( t.test_result ) {
    case NAADSM_TestTruePositive:
      inc( _outputs->tstnUTruePos );
      inc( _outputs->tstnATruePos, herdAnimalCount );
      inc( _outputs->tstcUTruePos );
      inc( _outputs->tstcATruePos, herdAnimalCount );
      break;

    case NAADSM_TestTrueNegative:
      inc( _outputs->tstnUTrueNeg );
      inc( _outputs->tstnATrueNeg, herdAnimalCount );
      inc( _outputs->tstcUTrueNeg );
      inc( _outputs->tstcATrueNeg, herdAnimalCount );
      break;

    case NAADSM_TestFalsePositive:
      inc( _outputs->tstnUFalsePos );
      inc( _outputs->tstnAFalsePos, herdAnimalCount );
      inc( _outputs->tstcUFalsePos );
      inc( _outputs->tstcAFalsePos, herdAnimalCount );
      break;

    case NAADSM_TestFalseNegative:
      inc( _outputs->tstnUFalseNeg );
      inc( _outputs->tstnAFalseNeg, herdAnimalCount );
      inc( _outputs->tstcUFalseNeg );
      inc( _outputs->tstcAFalseNeg, herdAnimalCount );
      break;

    default:
      qFatal( QString( "Unrecognized test result (%1) in TProductionType.addDiagnosticTestEvent" ).arg( t.test_result ).toLatin1().data() );
  }
}


void CProdTypeList::traceHerd( HRD_trace_t t ) {
  HRD_herd_t* identifiedHerd = HRD_herd_list_get( _herds, t.identified_index );
  CProdType* identifiedPt = this->find( identifiedHerd->production_type_name ).value();

  HRD_herd_t* originHerd = HRD_herd_list_get( _herds, t.origin_index );
  CProdType* originPt = this->find( originHerd->production_type_name ).value();

  identifiedPt->addAttemptedTraceEvent( identifiedHerd->size, t );
  originPt->addTraceOrigin( t );
}


void CProdType::addAttemptedTraceEvent( const int herdAnimalCount, const HRD_trace_t t ) {
  if( NAADSM_DirectContact == t.contact_type ) {
    switch( t.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        inc( _outputs->trcUDirpFwd );
        inc( _outputs->trcADirpFwd, herdAnimalCount );
        break;

      case NAADSM_TraceBackOrIn:
        inc( _outputs->trcUDirpBack );
        inc( _outputs->trcADirpBack, herdAnimalCount );
        break;

      default:
        qFatal( "Unrecognized trace type in TProductionType.addAttemptedTraceEvent" );
    }
  }
  else if( NAADSM_IndirectContact == t.contact_type ){
    switch( t.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        inc( _outputs->trcUIndpFwd );
        inc( _outputs->trcAIndpFwd, herdAnimalCount );
        break;

      case NAADSM_TraceBackOrIn:
        inc( _outputs->trcUIndpBack );
        inc( _outputs->trcAIndpBack, herdAnimalCount );
        break;

      default:
        qFatal( QString( "Unrecognized trace direction (%1) in TProductionType.addAttemptedTraceEvent" ).arg( t.trace_type ).toLatin1().data() );
    }
  }
  else
    qFatal( QString( "Unrecognized contact type reason (%1) in TProductionType.addAttemptedTraceEvent" ).arg( t.contact_type ).toLatin1().data() );
}


void CProdType::addTraceOrigin( const HRD_trace_t t ) {
  if( NAADSM_DirectContact == t.contact_type ){
    switch( t.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        inc( _outputs->tonUDirFwd );
        inc( _outputs->tocUDirFwd );
        break;

      case NAADSM_TraceBackOrIn:
        inc( _outputs->tonUDirBack );
        inc( _outputs->tocUDirBack );
        break;

      default:
        qFatal( QString( "Unrecognized trace type (%1) in TProductionType.addTraceOrigin" ).arg( t.trace_type ).toLatin1().data() );
    }
  }
  else if( NAADSM_IndirectContact == t.contact_type ){
    switch( t.trace_type ) {
      case NAADSM_TraceForwardOrOut:
        inc( _outputs->tonUIndFwd );
        inc( _outputs->tocUIndFwd );
        break;

      case NAADSM_TraceBackOrIn:
        inc( _outputs->tonUIndBack );
        inc( _outputs->tocUIndBack );
        break;

      default:
        qFatal( QString( "Unrecognized trace type (%1) in TProductionType.addTraceOrigin" ).arg( t.trace_type ).toLatin1().data() );
    }
  }
  else
    qFatal( QString( "Unrecognized contact type (%1) in TProductionType.addTraceOrigin" ).arg( t.contact_type ).toLatin1().data() );
}


void CProdTypeList::makeHerdZoneFocus( int herdIdx ) {
  HRD_herd_t* h = HRD_herd_list_get( _herds, herdIdx );
  CProdType* pt = this->find( h->production_type_name ).value();

  pt->addZoneFocusEvent();
}


void CProdType::addZoneFocusEvent() {
  inc( _outputs->zonnFoci );
  inc( _outputs->zoncFoci );
}
//------------------------------------------------------------------------------
