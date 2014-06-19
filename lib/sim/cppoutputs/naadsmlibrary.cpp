// Begin 5/16/2014


#include <QDebug>

#include <ar_general_purpose/ccmdline.h>

#include "naadsmlibrary.h"

#include "herd.h"
#include "main.h"
#include "naadsm.h"
#include "prob_dist.h"
#include "rng.h"

// Globals defined and used here
CSMSimulation* _smSim;
//CHerdList* _herds;
CSMDatabase* _smdb;

RAN_gen_t* _rng;

int _diseaseEndDay;
int _outbreakEnd;
int _simDay;
int _simIteration;

int _rngSeed;

int _eventCounter;
//_eventList: TSMEventList;

int _exposureCounter;
//_exposureList: TSMExposureOrTraceList;


QMap<int, HRD_herd_t*>* _herdsInZones;

// See comment for naadsm_detect_herd.  This will not be used in NAADSM 4!
QMap<int, QList<CNAADSM3Detect*>* >* _naadsm3Detections;


void
set_naadsm_fns (void)
{
  set_printf( NULL );
  set_debug( NULL );

  set_cpp_initialize( naadsmCppInitialize );
  set_cpp_finalize( naadsmCppFinalize );

  set_set_rng( naadsmSetRng );
  set_sim_start( naadsmSimStart );
  set_iteration_start( naadsmIterationStart );
  set_day_start( naadsmDayStart );
  set_day_complete( naadsmDayComplete );
  set_disease_end( naadsmDiseaseEnd );
  set_outbreak_end( naadsmOutbreakEnd );
  set_iteration_complete( naadsmIterationComplete );
  set_sim_complete( naadsmSimComplete );

  set_change_herd_state( naadsmChangeHerdState );
  set_infect_herd( naadsmInfectHerd );
  set_expose_herd( naadsmExposeHerd );
  set_detect_herd( naadsmDetectHerd );
  set_trace_herd( naadsmTraceHerd );
  set_examine_herd( naadsmExamineHerd );
  set_test_herd( naadsmTestHerd );
  set_queue_herd_for_destruction( naadsmQueueHerdForDestruction );
  set_destroy_herd( naadsmDestroyHerd );
  set_queue_herd_for_vaccination( naadsmQueueHerdForVaccination );
  set_vaccinate_herd( naadsmVaccinateHerd );
  set_cancel_herd_vaccination( naadsmCancelHerdVaccination );
  set_make_zone_focus( naadsmMakeZoneFocus );
  set_record_zone_change( naadsmRecordZoneChange );
  set_record_zone_area( naadsmRecordZoneArea );
  set_record_zone_perimeter( naadsmRecordZonePerimeter );

  // None of these are used in the CPP version.
  set_set_zone_perimeters( NULL );

  set_show_all_states( NULL );
  set_show_all_prevalences( NULL );
  // set_show_all_zones( NULL );

  set_simulation_stop( NULL );
  set_display_g_message( NULL );

  set_report_search_hits( NULL );
}



int main (int argc, char* argv[]) {
  int result = ReturnCode::Success;

  clear_naadsm_fns();
  clear_rng_fns();
  set_naadsm_fns();


  CCmdLine* cmdLine = new CCmdLine( argc, argv );

  QString configFileName = cmdLine->getSafeArgument( "-c", 0, "" );

  if( configFileName.isEmpty() )
    result = ReturnCode::MissingConfigFile;
  else if( !QFile::exists( configFileName ) )
    result = ReturnCode::CannotOpenConfigFile;

  delete cmdLine;


  if( ReturnCode::Success == result ) {
    _smdb = new CSMDatabase( configFileName );

    result = _smdb->returnValue();

    qDebug() << result;
    _smdb->debug();

    if( ReturnCode::Success == result ) {
      run_sim_main(
        _smdb->herdsFile().toLatin1().data(),    // herd_file
        _smdb->scenarioFile().toLatin1().data(), // parameter_file
        NULL,                                    // output_file (NULL will not generate an output file)
        -1.0,                                    // fixed_rng_value (if negative, values will be generated randomly)
        1,                                       // verbosity (ignored, in this case)
        _smdb->rngSeed()                         // specified seed for RNG (if -1, seed will be automatically generated)
      );
    }

    delete _smdb;
  }

  return result;
}


//-----------------------------------------------------------------------------
// Used for detection in NAADSM 3.
//-----------------------------------------------------------------------------
CNAADSM3Detect::CNAADSM3Detect( HRD_detect_t d ){
  herdIndex = d.herd_index;
  reason = d.reason;
  testResult = d.test_result;
}


CNAADSM3Detect::~CNAADSM3Detect() {
  // Nothing to do here.
}


void naadsmException( const QString& errMsg ) {
  qDebug() << errMsg;
  qFatal( "Died." );
}


void naadsm3ProcessDetections(){
  //CHerd* h;
  QList<CNAADSM3Detect*>* detections;
  QMapIterator<int, QList<CNAADSM3Detect*>* > mit( *_naadsm3Detections );
  CNAADSM3Detect* det;
  HRD_detect_t d;
  int detIdx;

  // For each detected herd...
  //--------------------------
  while( mit.hasNext() ) {
    detections = mit.next().value();

    // Did multiple detection events occur?  If so, choose one at random
    //------------------------------------------------------------------
    if( 1 == detections->count() )
      detIdx = 0;
    else
      detIdx = RAN_int( _rng, 0, detections->count() - 1 );

    det = detections->at( detIdx );

    d.herd_index = det->herdIndex;
    d.reason = det->reason;
    d.test_result = det->testResult;

    // Record the detection
    //---------------------
    _smSim->prodTypeList()->detectHerd( d, _simDay );
  }

  // Once all detections have been processed, clear out the map for later use
  //-------------------------------------------------------------------------
  mit.toFront();
  while( mit.hasNext() ) {
    detections = mit.next().value();

    while( !detections->isEmpty() )
      delete detections->takeLast();

    delete detections;
  }

  // Everything has been deleted, but the map still needs to be cleared.
  _naadsm3Detections->clear();
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// For key simulation- and iteration-level events
//-----------------------------------------------------------------------------
void naadsmSetRng( RAN_gen_t* rng ) {
  _rng = rng;
}


void naadsmCppInitialize( HRD_herd_list_t* herds, ZON_zone_list_t* zones ) {
  //qDebug() << "++ naadsmCppInitialize";

  _smSim = new CSMSimulation( herds, zones, _smdb );
}


void naadsmCppFinalize() {
  //qDebug() << "++ naadsmCppFinalize";

  delete _smSim;
}


void naadsmSimStart() {
  //qDebug() << "++ naadsmSimStart";

  _smdb->initializeAllOutputRecords();
  _smSim->initializeAllOutputRecords();

  _simIteration = -1;
  _simDay = -1;
  _diseaseEndDay = -1;
  _outbreakEnd = -1;

  _herdsInZones = new QMap<int, HRD_herd_t*>();

  _naadsm3Detections = new QMap<int, QList<CNAADSM3Detect*>* >();
}


void naadsmIterationStart( int it ) {
  qDebug() << "++ naadsmIterationStart" << it;

  _smdb->prepareForIteration( it + 1 );
  _smSim->prepareForIteration( it + 1 );

  _simIteration = it + 1;
  _simDay = -1;
  _diseaseEndDay = -1;
  _outbreakEnd = -1;

  _herdsInZones->clear();
}


void naadsmDayStart( int day ) {
  _smSim->prepareForDay();

  _simDay = day;
}


void naadsmDayComplete( int day ) {
  naadsm3ProcessDetections();

  // Update the number of herds and animals of each production type in each zone
  QMapIterator<int, HRD_herd_t*> it( *_herdsInZones );

  while( it.hasNext() ) {
    HRD_herd_t* h = it.next().value();
    CZone* z = _smSim->zoneList()->value( h->zone_level );
    CProdType* pt = _smSim->prodTypeList()->value( h->production_type_name );
    z->addToZoneTotals( pt->id(), h->size );
  }

  _smSim->processDailyRecords( _smdb, day );
}


void naadsmDiseaseEnd( int val ) {
  //qDebug() << "++ naadsmDiseaseEnd";

  _diseaseEndDay = val;
}


void naadsmOutbreakEnd( int val ) {
  //qDebug() << "++ naadsmOutbreakEnd";

  _outbreakEnd = val;
}


void naadsmIterationComplete( int it ) {
  // Record the iteration as complete
  _smdb->processIterationRecords(
    it+1,
    _simDay,
    ( -1 != _outbreakEnd ),
    _diseaseEndDay,
    ( -1 != _diseaseEndDay ),
    _smSim->zoneList()->focusCreated()
  );

  _smSim->processIterationRecords( _smdb, it+1 );
}


void naadsmSimComplete( int val ) {
  if( 0 != val )
    _smdb->recordEndTime();

  _herdsInZones->clear();
  // Do not delete items in _herdsInZones: it does not own them.
  delete _herdsInZones;

  delete _naadsm3Detections;

  _smSim->simComplete();
  _smdb->simComplete();
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Used to update herd status and related events as an iteration runs
//-----------------------------------------------------------------------------
void naadsmChangeHerdState( HRD_update_t c ) {
  //qDebug() << "++ naadsmChangeHerdState";

  _smSim->prodTypeList()->changeHerdState( c, _simDay );
}

void naadsmInfectHerd( HRD_infect_t r ) {
  //qDebug() << "++ naadsmInfectHerd";

  _smSim->prodTypeList()->infectHerd( r, _simDay );
}

void naadsmDetectHerd( HRD_detect_t d ) {
  //qDebug() << "++ naadsmDetectHerd";

  CNAADSM3Detect* det;
  QList<CNAADSM3Detect*>* detections;

  if( _naadsm3Detections->contains( d.herd_index) )
    detections = _naadsm3Detections->value( d.herd_index );
  else {
    detections = new  QList<CNAADSM3Detect*>();
    _naadsm3Detections->insert( d.herd_index, detections );
  }

  det = new CNAADSM3Detect( d );
  detections->append( det );
}

void naadsmExposeHerd( HRD_expose_t e ) {
  //qDebug() << "++ naadsmExposeHerd";

  _smSim->prodTypeList()->exposeHerd( e );
}

void naadsmTraceHerd( HRD_trace_t t ) {
  //qDebug() << "++ naadsmTraceHerd";

  _smSim->prodTypeList()->traceHerd( t );
}

void naadsmExamineHerd( HRD_exam_t e ) {
  //qDebug() << "++ naadsmExamineHerd";

  _smSim->prodTypeList()->examineHerd( e );
}

void naadsmTestHerd( HRD_test_t t ) {
  //qDebug() << "++ naadsmTestHerd";

  _smSim->prodTypeList()->testHerd( t );
}

void naadsmQueueHerdForDestruction( int herdIdx ) {
  //qDebug() << "++ naadsmQueueHerdForDestruction";

  _smSim->prodTypeList()->queueHerdForDestruction( herdIdx, _simDay );
}

void naadsmDestroyHerd( HRD_control_t c ) {
  //qDebug() << "++ naadsmDestroyHerd";

  _smSim->prodTypeList()->destroyHerd( c, _simDay );

  // If this herd was previously "enzoned", it can be taken out of the list now.
  if( _herdsInZones->contains( c.herd_index ) )
    _herdsInZones->remove( c.herd_index );
}

void naadsmQueueHerdForVaccination( int herdIdx ) {
  //qDebug() << "++ naadsmQueueHerdForVaccination";

  _smSim->prodTypeList()->queueHerdForVaccination( herdIdx, _simDay );
}

void naadsmVaccinateHerd( HRD_control_t c ) {
  //qDebug() << "++ naadsmVaccinateHerd";

  _smSim->prodTypeList()->vaccinateHerd( c, _simDay );
}

void naadsmCancelHerdVaccination( HRD_control_t c ) {
  //qDebug() << "++ naadsmCancelHerdVaccination";

  _smSim->prodTypeList()->cancelHerdVaccination( c );
}

void naadsmMakeZoneFocus( int herdIdx ) {
  //qDebug() << "++ naadsmMakeZoneFocus";

  _smSim->zoneList()->setFocusCreatedOnDay( _simDay );
  _smSim->prodTypeList()->makeHerdZoneFocus( herdIdx );
}

void naadsmRecordZoneChange( HRD_zone_t z ) {
  // Add the herd to the list of those "enzoned".
  // At the end of the day, outputs will be recorded.
  HRD_herd_t* h = HRD_herd_list_get( _smSim->prodTypeList()->herds(), z.herd_index );

  if( !_herdsInZones->contains( z.herd_index ) )
    _herdsInZones->insert( z.herd_index, h );
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Zone outputs
//-----------------------------------------------------------------------------
void naadsmRecordZonePerimeter( int zoneLevel, double perim ) {
  //qDebug() << "++ naadsmRecordZonePerimeter";

  if( _smSim->zoneList()->contains( zoneLevel ) ) {
    CZone* z = _smSim->zoneList()->value( zoneLevel );
    z->setPerimeter( perim, _simDay );
  }
  else
    qFatal( QString( "Zone list does not contain level zoneLevel %1 in naadsm_record_zone_perimeter" ).arg( zoneLevel ).toLatin1().data() );
}


void naadsmRecordZoneArea( int zoneLevel, double area ) {
  //qDebug() << "++ naadsmRecordZoneArea";

  if( _smSim->zoneList()->contains( zoneLevel ) ) {
    CZone* z = _smSim->zoneList()->value( zoneLevel );
    z->setArea( area, _simDay );
  }
  else
    qFatal( QString( "Zone list does not contain level zoneLevel %1 in naadsm_record_zone_area" ).arg( zoneLevel ).toLatin1().data() );
}
//-----------------------------------------------------------------------------



