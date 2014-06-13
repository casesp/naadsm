#include "csmptoutput.h"

#include <ar_general_purpose/qmath.h>

#include <QDebug>
#include <QSqlError>
#include <QSqlRecord>

#include "naadsmlibrary.h"

//------------------------------------------------------------------------------
// CSMIterationOutput
//------------------------------------------------------------------------------
CSMIterationOutput::CSMIterationOutput(){
  initialize();
}

CSMIterationOutput::~CSMIterationOutput() {
  // Nothing to do here
}


void CSMIterationOutput::initialize() {
  _ptID = -1;
  clear();
}


void CSMIterationOutput::clearRunningTotals() {
  // Running totals for each disease state
  tscUSusc = 0;
  tscASusc = 0;
  tscULat = 0;
  tscALat = 0;
  tscUSubc = 0;
  tscASubc = 0;
  tscUClin = 0;
  tscAClin = 0;
  tscUNImm = 0;
  tscANImm = 0;
  tscUVImm = 0;
  tscAVImm = 0;
  tscUDest = 0;
  tscADest = 0;

  // Running totals for cause of infection
  infcUIni = 0;
  infcAIni = 0;
  infcUAir = 0;
  infcAAir = 0;
  infcUDir = 0;
  infcADir = 0;
  infcUInd = 0;
  infcAInd = 0;

  // Running totals for exposures
  expcUDir = 0;
  expcADir = 0;
  expcUInd = 0;
  expcAInd = 0;

  // Running totals for traces
  trcUDirFwd = 0;
  trcADirFwd = 0;
  trcUIndFwd = 0;
  trcAIndFwd = 0;
  trcUDirpFwd = 0;
  trcADirpFwd = 0;
  trcUIndpFwd = 0;
  trcAIndpFwd = 0;
  trcUDirBack = 0;
  trcADirBack = 0;
  trcUIndBack = 0;
  trcAIndBack = 0;
  trcUDirpBack = 0;
  trcADirpBack = 0;
  trcUIndpBack = 0;
  trcAIndpBack = 0;

  // Running totals for trace origins
  tocUDirFwd = 0;
  tocUIndFwd = 0;
  tocUDirBack = 0;
  tocUIndBack = 0;

  // Running totals for detection
  detcUClin = 0;
  detcAClin = 0;
  detcUTest = 0;
  detcATest = 0;

  // Running totals for destruction
  descUIni = 0;
  descAIni = 0;
  descUDet = 0;
  descADet = 0;
  descUDirFwd = 0;
  descADirFwd = 0;
  descUIndFwd = 0;
  descAIndFwd = 0;
  descUDirBack = 0;
  descADirBack = 0;
  descUIndBack = 0;
  descAIndBack = 0;
  descURing = 0;
  descARing = 0;

  // Running totals for vaccination
  vaccUIni = 0;
  vaccAIni = 0;
  vaccURing = 0;
  vaccARing = 0;

  //Running totals for herd exams
  exmcUDirFwd = 0;
  exmcADirFwd = 0;
  exmcUIndFwd = 0;
  exmcAIndFwd = 0;
  exmcUDirBack = 0;
  exmcADirBack = 0;
  exmcUIndBack = 0;
  exmcAIndBack = 0;

  // Running totals for testing
  tstcUDirFwd = 0;
  tstcADirFwd = 0;
  tstcUIndFwd = 0;
  tstcAIndFwd = 0;
  tstcUDirBack = 0;
  tstcADirBack = 0;
  tstcUIndBack = 0;
  tstcAIndBack = 0;
  tstcUTruePos = 0;
  tstcATruePos = 0;
  tstcUTrueNeg = 0;
  tstcATrueNeg = 0;
  tstcUFalsePos = 0;
  tstcAFalsePos = 0;
  tstcUFalseNeg = 0;
  tstcAFalseNeg = 0;

  // Running totals for zone foci
  zoncFoci = 0;

  // Values for destruction and vaccination queues
  deswUMax = 0;
  deswUMaxDay = 0;
  deswUTimeMax = 0;
  deswAMax = 0.0;
  deswAMaxDay = 0;
  deswUDaysInQueue = 0.0;
  deswADaysInQueue = 0.0;
  vacwUMax = 0;
  vacwUMaxDay = 0;
  vacwUTimeMax = 0;
  vacwAMax = 0.0;
  vacwAMaxDay = 0;
  vacwUDaysInQueue = 0.0;
  vacwADaysInQueue = 0.0;

  // Intermediate outputs
  _destrQueueLengthUnits = 0;
  _destrQueueLengthAnimals = 0.0;
  _vaccQueueLengthUnits = 0;
  _vaccQueueLengthAnimals = 0.0;

  _vacwUTimeAvg = -1.0;
  _deswUTimeAvg = -1.0;
}


void CSMIterationOutput::setCumulRecordsFrom( CSMIterationOutput* src ) {
  clear();
  addCumulRecordsFrom( src );
}


void CSMIterationOutput::addCumulRecordsFrom( CSMIterationOutput* src ) {
  // Running totals for each disease state
  inc( this->tscUSusc, src->tscUSusc );
  inc( this->tscASusc, src->tscASusc );
  inc( this->tscULat, src->tscULat );
  inc( this->tscALat, src->tscALat );
  inc( this->tscUSubc, src->tscUSubc );
  inc( this->tscASubc, src->tscASubc );
  inc( this->tscUClin, src->tscUClin );
  inc( this->tscAClin, src->tscAClin );
  inc( this->tscUNImm, src->tscUNImm );
  inc( this->tscANImm, src->tscANImm );
  inc( this->tscUVImm, src->tscUVImm );
  inc( this->tscAVImm, src->tscAVImm );
  inc( this->tscUDest, src->tscUDest );
  inc( this->tscADest, src->tscADest );

  // Running totals for cause of infection
  inc( this->infcUIni, src->infcUIni );
  inc( this->infcAIni, src->infcAIni );
  inc( this->infcUAir, src->infcUAir );
  inc( this->infcAAir, src->infcAAir );
  inc( this->infcUDir, src->infcUDir );
  inc( this->infcADir, src->infcADir );
  inc( this->infcUInd, src->infcUInd );
  inc( this->infcAInd, src->infcAInd );

  // Running totals for exposures
  inc( this->expcUDir, src->expcUDir );
  inc( this->expcADir, src->expcADir );
  inc( this->expcUInd, src->expcUInd );
  inc( this->expcAInd, src->expcAInd );

  // Running totals for traces
  inc( this->trcUDirFwd, src->trcUDirFwd );
  inc( this->trcADirFwd, src->trcADirFwd );
  inc( this->trcUIndFwd, src->trcUIndFwd );
  inc( this->trcAIndFwd, src->trcAIndFwd );
  inc( this->trcUDirpFwd, src->trcUDirpFwd );
  inc( this->trcADirpFwd, src->trcADirpFwd );
  inc( this->trcUIndpFwd, src->trcUIndpFwd );
  inc( this->trcAIndpFwd, src->trcAIndpFwd );
  inc( this->trcUDirBack, src->trcUDirBack );
  inc( this->trcADirBack, src->trcADirBack );
  inc( this->trcUIndBack, src->trcUIndBack );
  inc( this->trcAIndBack, src->trcAIndBack );
  inc( this->trcUDirpBack, src->trcUDirpBack );
  inc( this->trcADirpBack, src->trcADirpBack );
  inc( this->trcUIndpBack, src->trcUIndpBack );
  inc( this->trcAIndpBack, src->trcAIndpBack );

  // Running totals for trace origins
  inc( this->tocUDirFwd, src->tocUDirFwd );
  inc( this->tocUIndFwd, src->tocUIndFwd );
  inc( this->tocUDirBack, src->tocUDirBack );
  inc( this->tocUIndBack, src->tocUIndBack );

  // Running totals for detection
  inc( this->detcUClin, src->detcUClin );
  inc( this->detcAClin, src->detcAClin );
  inc( this->detcUTest, src->detcUTest );
  inc( this->detcATest, src->detcATest );

  // Running totals for destruction
  inc( this->descUIni, src->descUIni );
  inc( this->descAIni, src->descAIni );
  inc( this->descUDet, src->descUDet );
  inc( this->descADet, src->descADet );
  inc( this->descUDirFwd, src->descUDirFwd );
  inc( this->descADirFwd, src->descADirFwd );
  inc( this->descUIndFwd, src->descUIndFwd );
  inc( this->descAIndFwd, src->descAIndFwd );
  inc( this->descUDirBack, src->descUDirBack );
  inc( this->descADirBack, src->descADirBack );
  inc( this->descUIndBack, src->descUIndBack );
  inc( this->descAIndBack, src->descAIndBack );
  inc( this->descURing, src->descURing );
  inc( this->descARing, src->descARing );

  // Running totals for vaccination
  inc( this->vaccUIni, src->vaccUIni );
  inc( this->vaccAIni, src->vaccAIni );
  inc( this->vaccURing, src->vaccURing );
  inc( this->vaccARing, src->vaccARing );

  // Running totals for herd exams
  inc( this->exmcUDirFwd, src->exmcUDirFwd );
  inc( this->exmcADirFwd, src->exmcADirFwd );
  inc( this->exmcUIndFwd, src->exmcUIndFwd );
  inc( this->exmcAIndFwd, src->exmcAIndFwd );
  inc( this->exmcUDirBack, src->exmcUDirBack );
  inc( this->exmcADirBack, src->exmcADirBack );
  inc( this->exmcUIndBack, src->exmcUIndBack );
  inc( this->exmcAIndBack, src->exmcAIndBack );

  // Running totals for diagnostic testing
  inc( this->tstcUDirFwd, src->tstcUDirFwd );
  inc( this->tstcADirFwd, src->tstcADirFwd );
  inc( this->tstcUIndFwd, src->tstcUIndFwd );
  inc( this->tstcAIndFwd, src->tstcAIndFwd );
  inc( this->tstcUDirBack, src->tstcUDirBack );
  inc( this->tstcADirBack, src->tstcADirBack );
  inc( this->tstcUIndBack, src->tstcUIndBack );
  inc( this->tstcAIndBack, src->tstcAIndBack );
  inc( this->tstcUTruePos, src->tstcUTruePos );
  inc( this->tstcATruePos, src->tstcATruePos );
  inc( this->tstcUTrueNeg, src->tstcUTrueNeg );
  inc( this->tstcATrueNeg, src->tstcATrueNeg );
  inc( this->tstcUFalsePos, src->tstcUFalsePos );
  inc( this->tstcAFalsePos, src->tstcAFalsePos );
  inc( this->tstcUFalseNeg, src->tstcUFalseNeg );
  inc( this->tstcAFalseNeg, src->tstcAFalseNeg );

  // Running totals for zone foci
  inc( this->zoncFoci, src->zoncFoci );

  // Values for destruction and vaccination queues
  // Note that these are handled somewhat differently than the other outputs
  if( src->deswUMax > this->deswUMax ) {
      this->deswUMax = src->deswUMax;
      this->deswUMaxDay = src->deswUMaxDay;
  }

  if( src->deswAMax > this->deswAMax ) {
      this->deswAMax = src->deswAMax;
      this->deswAMaxDay = src->deswAMaxDay;
}

  if( src->deswUTimeMax > this->deswUTimeMax )
    this->deswUTimeMax = src->deswUTimeMax;

  this->deswUDaysInQueue = this->deswUDaysInQueue + src->deswUDaysInQueue;
  this->deswADaysInQueue = this->deswADaysInQueue + src->deswADaysInQueue;

  if( 0 == this->descUAll() )
    this->_deswUTimeAvg = 0.0;
  else
    this->_deswUTimeAvg = this->deswUDaysInQueue / ( this->descUAll() );

  if( src->vacwUMax > this->vacwUMax ) {
      this->vacwUMax = src->vacwUMax;
      this->vacwUMaxDay = src->vacwUMaxDay;
  }

  if( src->vacwAMax > this->vacwAMax ) {
      this->vacwAMax = src->vacwAMax;
      this->vacwAMaxDay = src->vacwAMaxDay;
  }

  if( src->vacwUTimeMax > this->vacwUTimeMax )
    this->vacwUTimeMax = src->vacwUTimeMax;

  this->vacwUDaysInQueue = this->vacwUDaysInQueue + src->vacwUDaysInQueue;
  this->vacwADaysInQueue = this->vacwADaysInQueue + src->vacwADaysInQueue;

  if( 0 == this->vaccUAll() )
    this->_vacwUTimeAvg = 0.0;
  else
    this->_vacwUTimeAvg = this->vacwUDaysInQueue / ( this->vaccUAll() );
}


void CSMIterationOutput::clear(){
  firstDetection = -1;
  firstDestruction = -1;
  firstVaccination = -1;
  lastDetection = -1;

  // Number infected at time of first detection
  firstDetUInf = -1;
  firstDetAInf = -1;

  clearRunningTotals();
}


void CSMIterationOutput::addToDestrQueue( const int herdSize, const int day ) {
  inc( _destrQueueLengthUnits );

  _destrQueueLengthAnimals = _destrQueueLengthAnimals + herdSize;

  if( _destrQueueLengthUnits > deswUMax ){
    deswUMax = _destrQueueLengthUnits;
    deswUMaxDay = day;
  }

  if( _destrQueueLengthAnimals > deswAMax ){
    deswAMax = _destrQueueLengthAnimals;
    deswAMaxDay = day;
  }
}


void CSMIterationOutput::addToVaccQueue( const int herdSize, const int day ){
  inc( _vaccQueueLengthUnits );
  _vaccQueueLengthAnimals = _vaccQueueLengthAnimals + herdSize;
  //dbcout2( 'Added unit, vacc queue length: ' + intToStr( self.vaccQueueLengthUnits ) );

  if( _vaccQueueLengthUnits > vacwUMax ) {
    vacwUMax = _vaccQueueLengthUnits;
    vacwUMaxDay = day;
  }

  if( _vaccQueueLengthAnimals > vacwAMax ){
    vacwAMax = _vaccQueueLengthAnimals;
    vacwAMaxDay = day;
  }
}


void CSMIterationOutput::removeFromVaccQueue( const int herdSize ){
    dec( _vaccQueueLengthUnits );
    _vaccQueueLengthAnimals = _vaccQueueLengthAnimals - herdSize;
    //dbcout2( 'Removed unit, vacc queue length: ' + intToStr( self.vaccQueueLengthUnits ) );

    if( 0 > _vaccQueueLengthUnits )
      qFatal( "Number of units in vaccination queue has dropped below 0 in TSMIterationOutput.removeFromVaccQueue()." );

    if( 0.0 > _vaccQueueLengthAnimals )
      qFatal( "Number of animals in vaccination queue has dropped below 0 in TSMIterationOutput.removeFromVaccQueue()." );
}


int CSMIterationOutput::expcUAll(){
  return expcUDir + expcUInd;
}


int CSMIterationOutput::expcAAll(){
  return expcADir + expcAInd;
}


int CSMIterationOutput::infcUAll(){
  // infcUTotal intentionally does NOT include units that were infected at the ning of the simulation.
  return infcUAir + infcUDir + infcUInd;
}


int CSMIterationOutput::infcAAll(){
  // infcATotal intentionally does NOT include animals that were infected at the ning of the simulation.
  return infcAAir + infcADir + infcAInd;
}

int CSMIterationOutput::trcUDirAll(){
  return trcUDirFwd + trcUDirBack;
}


int CSMIterationOutput::trcADirAll(){
  return trcADirFwd + trcADirBack;
}


int CSMIterationOutput::trcUIndAll(){
  return trcUIndFwd + trcUIndBack;
}

int CSMIterationOutput::trcAIndAll(){
  return trcAIndFwd + trcAIndBack;
}


int CSMIterationOutput::trcUAll(){
  return trcUDirAll() + trcUIndAll();
}

int CSMIterationOutput::trcAAll(){
  return trcADirAll() + trcAIndAll();
}

int CSMIterationOutput::tocUDirAll(){
  return tocUDirFwd + tocUDirBack;
}

int CSMIterationOutput::tocUIndAll(){
  return tocUIndFwd + tocUIndBack;
}


int CSMIterationOutput::tocUAll(){
  return tocUDirAll() + tocUIndAll();
}


int CSMIterationOutput::detcUAll(){
  return detcUClin + detcUTest;
}

int CSMIterationOutput::detcAAll(){
  return detcAClin + detcATest;
}


int CSMIterationOutput::descUAll(){
  // DO NOT include initially destroyed units
  return descUDet + descUDirFwd + descUIndFwd + descUDirBack + descUIndBack + descURing;
}


int CSMIterationOutput::descAAll(){
  // DO NOT include initially destroyed units
  return descADet + descADirFwd + descAIndFwd + descADirBack + descAIndBack + descARing;
}


int CSMIterationOutput::vaccUAll(){
  // DO NOT include initially vaccinated units
  return vaccURing;
}


int CSMIterationOutput::vaccAAll(){
// DO NOT include initially vaccinated units
  return vaccARing;
}


int CSMIterationOutput::exmcUDirAll(){
  return exmcUDirFwd + exmcUDirBack;
}


int CSMIterationOutput::exmcADirAll(){
  return exmcADirFwd + exmcADirBack;
}


int CSMIterationOutput::exmcUIndAll(){
  return exmcUIndFwd + exmcUIndBack;
}


int CSMIterationOutput::exmcAIndAll(){
  return exmcAIndFwd + exmcAIndBack;
}


int CSMIterationOutput::exmcUAll(){
  return exmcUDirAll() + exmcUIndAll();
}


int CSMIterationOutput::exmcAAll(){
  return exmcADirAll() + exmcAIndAll();
}


int CSMIterationOutput::tstcUDirAll(){
  return tstcUDirFwd + tstcUDirBack;
}


int CSMIterationOutput::tstcADirAll(){
  return tstcADirFwd + tstcADirBack;
}


int CSMIterationOutput::tstcUIndAll(){
  return tstcUIndFwd + tstcUIndBack;
}


int CSMIterationOutput::tstcAIndAll(){
  return tstcAIndFwd + tstcAIndBack;
}


int CSMIterationOutput::tstcUAll(){
  return tstcUDirAll() + tstcUIndAll();
}

int CSMIterationOutput::tstcAAll(){
  return tstcADirAll() + tstcAIndAll();
}


double CSMIterationOutput::deswUTimeAvg(){
  double result;

  if( 0 == descUAll() )
    result = 0.0;
  else
    result = deswUDaysInQueue / descUAll();

  return result;
}


double CSMIterationOutput::vacwUTimeAvg(){
  double result;

  if( 0 == vaccUAll() )
    result = 0.0;
  else
    result = vacwUDaysInQueue / vaccUAll();

  return result;
}


void CSMIterationOutput::processDestruction(
  const int nAnimals,
  const int day,
  const int queueDay
){
  int unitDaysDestr, animalDaysDestr;

  unitDaysDestr = day - queueDay;
  animalDaysDestr = unitDaysDestr * nAnimals;

  // Maximum wait for destruction is handled here.
  //----------------------------------------------
  if( unitDaysDestr > deswUTimeMax )
    deswUTimeMax = unitDaysDestr;

  // Average time to destruction is handled here,
  // but only indirectly. The actual calculation is made later.
  //-----------------------------------------------------------
  deswUDaysInQueue = deswUDaysInQueue + unitDaysDestr;
  deswADaysInQueue = deswADaysInQueue + animalDaysDestr;

  // Outputs related to maximum queue length are handled by
  // self.addToDestrQueue() via TProductionType.addDestructionQueueEvent().
  //-----------------------------------------------------------------------

  // Average time to vaccination and maximum time for vaccination are NOT handled here,
  // because no vaccination took place.
  //-----------------------------------------------------------------------------------

  // Finally, adjust the running lengths of the destruction queues.
  //---------------------------------------------------------------
  dec( _destrQueueLengthUnits );
  _destrQueueLengthAnimals = _destrQueueLengthAnimals - nAnimals;

  if( 0 > _destrQueueLengthUnits )
    qFatal( "Number of units in destruction queue has dropped below 0 in TSMIterationOutput.processDestruction()." );
  if( 0.0 > _destrQueueLengthAnimals )
    qFatal( "Number of animals in destruction queue has dropped below 0 in TSMIterationOutput.processDestruction()." );

  // DO NOT remove this unit from the vaccination queue.
  // That will happen explicitly elsewhere.
}


void CSMIterationOutput::processVaccination( const int nAnimals, const int day, const int queueDay ) {
  int unitDaysVacc, animalDaysVacc;

  unitDaysVacc = day - queueDay;
  animalDaysVacc = unitDaysVacc * nAnimals;

  // Maximum wait for vaccination is handled here.
  //----------------------------------------------
  if( unitDaysVacc > vacwUTimeMax )
    vacwUTimeMax = unitDaysVacc;

  // Average time to vaccination is handled here,
  // but only indirectly. The actual calculation is made later.
  //-----------------------------------------------------------
  vacwUDaysInQueue = vacwUDaysInQueue + unitDaysVacc;
  vacwADaysInQueue = vacwADaysInQueue + animalDaysVacc;

  // Outputs related to maximum queue length are handled by
  //  self.addToVAccQueue() via TProductionType.addDestructionQueueEvent().
  //-----------------------------------------------------------------------

  // Finally, adjust the running length of the vaccination queue.
  // (Leave the destruction queue unchanged.)
  //------------------------------------------------------------
  removeFromVaccQueue( nAnimals );
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// CSMDailyOutput
//------------------------------------------------------------------------------
CSMDailyOutput::CSMDailyOutput() : CSMIterationOutput() {
  initialize();
  prepQueries();
}


void CSMDailyOutput::initialize() {
  CSMIterationOutput::initialize();
  clear();
}


CSMDailyOutput::~CSMDailyOutput() {
  freeQueries();
}


void CSMDailyOutput::prepQueries() {
  qOutDailyByProductionType = new QSqlQuery( QSqlDatabase::database() );
  qOutDailyByProductionType->prepare(
    QString(
      "INSERT INTO outDailyByProductionType("
        " scenarioID,"
        " threadNo,"
        " iteration,"
        " productionTypeID,"

        " day,"

      // Daily numbers for each disease state
        " tsdUSusc,"
        " tsdASusc,"
        " tsdULat,"
        " tsdALat,"
        " tsdUSubc,"
        " tsdASubc,"
        " tsdUClin,"
        " tsdAClin,"
        " tsdUNImm,"
        " tsdANImm,"
        " tsdUVImm,"
        " tsdAVImm,"
        " tsdUDest,"
        " tsdADest,"

      // New daily counts for cause of infection
        " infnUAir,"
        " infnAAir,"
        " infnUDir,"
        " infnADir,"
        " infnUInd,"
        " infnAInd,"

      // New daily counts for detection
        " detnUClin,"
        " detnAClin,"
        " detnUTest,"
        " detnATest,"

      // New daily counts for herd exams
        " exmnUAll,"
        " exmnAAll,"

      // New daily counts for testing
        " tstnUTruePos,"
        " tstnATruePos,"
        " tstnUTrueNeg,"
        " tstnATrueNeg,"
        " tstnUFalsePos,"
        " tstnAFalsePos,"
        " tstnUFalseNeg,"
        " tstnAFalseNeg,"

      // New daily counts for tracing
        " trnUDirFwd,"
        " trnADirFwd,"
        " trnUIndFwd,"
        " trnAIndFwd,"
        " trnUDirBack,"
        " trnADirBack,"
        " trnUIndBack,"
        " trnAIndBack,"

      // new daily counts for trace origins
        " tonUDirFwd,"
        " tonUIndFwd,"
        " tonUDirBack,"
        " tonUIndBack,"

      // New daily counts for destruction for any reason
        " desnUAll,"
        " desnAAll,"

      // Length of the destruction queue
        " deswUAll,"
        " deswAAll,"

      // New daily counts for vaccination for any reason
        " vacnUAll,"
        " vacnAAll,"

      // Length of the vaccination queue
        " vacwUAll,"
        " vacwAAll,"

      // New daily counts for zone foci
        " zonnFoci,"

      // Number of apparently infectious units on this day
        " appdUInfectious,"

      // Running totals for each disease state
        " tscUSusc,"
        " tscASusc,"
        " tscULat,"
        " tscALat,"
        " tscUSubc,"
        " tscASubc,"
        " tscUClin,"
        " tscAClin,"
        " tscUNImm,"
        " tscANImm,"
        " tscUVImm,"
        " tscAVImm,"
        " tscUDest,"
        " tscADest,"

      // Running totals for cause of infection
        " infcUIni,"
        " infcAIni,"
        " infcUAir,"
        " infcAAir,"
        " infcUDir,"
        " infcADir,"
        " infcUInd,"
        " infcAInd,"

      // Running totals for exposures
        " expcUDir,"
        " expcADir,"
        " expcUInd,"
        " expcAInd,"

      // Running totals for traces
        " trcUDirFwd,"
        " trcADirFwd,"
        " trcUIndFwd,"
        " trcAIndFwd,"
        " trcUDirpFwd,"
        " trcADirpFwd,"
        " trcUIndpFwd,"
        " trcAIndpFwd,"
        " trcUDirBack,"
        " trcADirBack,"
        " trcUIndBack,"
        " trcAIndBack,"
        " trcUDirpBack,"
        " trcADirpBack,"
        " trcUIndpBack,"
        " trcAIndpBack,"

      // Running totals for trace origins
        " tocUDirFwd,"
        " tocUIndFwd,"
        " tocUDirBack,"
        " tocUIndBack,"

      // Running totals for detection
        " detcUClin,"
        " detcAClin,"
        " detcUTest,"
        " detcATest,"

      // Running totals for destruction
        " descUIni,"
        " descAIni,"
        " descUDet,"
        " descADet,"
        " descUDirFwd,"
        " descADirFwd,"
        " descUIndFwd,"
        " descAIndFwd,"
        " descUDirBack,"
        " descADirBack,"
        " descUIndBack,"
        " descAIndBack,"
        " descURing,"
        " descARing,"

      // Running totals for vaccination
        " vaccUIni,"
        " vaccAIni,"
        " vaccURing,"
        " vaccARing,"

      // Running totals for herd exams
        " exmcUDirFwd,"
        " exmcADirFwd,"
        " exmcUIndFwd,"
        " exmcAIndFwd,"
        " exmcUDirBack,"
        " exmcADirBack,"
        " exmcUIndBack,"
        " exmcAIndBack,"

      // Running totals for diagnostic testing
        " tstcUDirFwd,"
        " tstcADirFwd,"
        " tstcUIndFwd,"
        " tstcAIndFwd,"
        " tstcUDirBack,"
        " tstcADirBack,"
        " tstcUIndBack,"
        " tstcAIndBack,"
        " tstcUTruePos,"
        " tstcATruePos,"
        " tstcUTrueNeg,"
        " tstcATrueNeg,"
        " tstcUFalsePos,"
        " tstcAFalsePos,"
        " tstcUFalseNeg,"
        " tstcAFalseNeg,"

      // Running totals for zone foci
        " zoncFoci"
      " )"
      " VALUES("
        " :scenarioID,"
        " :threadNo,"
        " :iteration,"
        " :productionTypeID,"

        " :day,"

        " :tsdUSusc,"
        " :tsdASusc,"
        " :tsdULat,"
        " :tsdALat,"
        " :tsdUSubc,"
        " :tsdASubc,"
        " :tsdUClin,"
        " :tsdAClin,"
        " :tsdUNImm,"
        " :tsdANImm,"
        " :tsdUVImm,"
        " :tsdAVImm,"
        " :tsdUDest,"
        " :tsdADest,"

        " :infnUAir,"
        " :infnAAir,"
        " :infnUDir,"
        " :infnADir,"
        " :infnUInd,"
        " :infnAInd,"

        " :detnUClin,"
        " :detnAClin,"
        " :detnUTest,"
        " :detnATest,"

        " :exmnUAll,"
        " :exmnAAll,"

        " :tstnUTruePos,"
        " :tstnATruePos,"
        " :tstnUTrueNeg,"
        " :tstnATrueNeg,"
        " :tstnUFalsePos,"
        " :tstnAFalsePos,"
        " :tstnUFalseNeg,"
        " :tstnAFalseNeg,"

        " :trnUDirFwd,"
        " :trnADirFwd,"
        " :trnUIndFwd,"
        " :trnAIndFwd,"
        " :trnUDirBack,"
        " :trnADirBack,"
        " :trnUIndBack,"
        " :trnAIndBack,"

        " :tonUDirFwd,"
        " :tonUIndFwd,"
        " :tonUDirBack,"
        " :tonUIndBack,"

        " :desnUAll,"
        " :desnAAll,"

        " :deswUAll,"
        " :deswAAll,"

        " :vacnUAll,"
        " :vacnAAll,"

        " :vacwUAll,"
        " :vacwAAll,"

        " :zonnFoci,"

        " :appdUInfectious,"

        " :tscUSusc,"
        " :tscASusc,"
        " :tscULat,"
        " :tscALat,"
        " :tscUSubc,"
        " :tscASubc,"
        " :tscUClin,"
        " :tscAClin,"
        " :tscUNImm,"
        " :tscANImm,"
        " :tscUVImm,"
        " :tscAVImm,"
        " :tscUDest,"
        " :tscADest,"

        " :infcUIni,"
        " :infcAIni,"
        " :infcUAir,"
        " :infcAAir,"
        " :infcUDir,"
        " :infcADir,"
        " :infcUInd,"
        " :infcAInd,"

        " :expcUDir,"
        " :expcADir,"
        " :expcUInd,"
        " :expcAInd,"

        " :trcUDirFwd,"
        " :trcADirFwd,"
        " :trcUIndFwd,"
        " :trcAIndFwd,"
        " :trcUDirpFwd,"
        " :trcADirpFwd,"
        " :trcUIndpFwd,"
        " :trcAIndpFwd,"
        " :trcUDirBack,"
        " :trcADirBack,"
        " :trcUIndBack,"
        " :trcAIndBack,"
        " :trcUDirpBack,"
        " :trcADirpBack,"
        " :trcUIndpBack,"
        " :trcAIndpBack,"

        " :tocUDirFwd,"
        " :tocUIndFwd,"
        " :tocUDirBack,"
        " :tocUIndBack,"

        " :detcUClin,"
        " :detcAClin,"
        " :detcUTest,"
        " :detcATest,"

        " :descUIni,"
        " :descAIni,"
        " :descUDet,"
        " :descADet,"
        " :descUDirFwd,"
        " :descADirFwd,"
        " :descUIndFwd,"
        " :descAIndFwd,"
        " :descUDirBack,"
        " :descADirBack,"
        " :descUIndBack,"
        " :descAIndBack,"
        " :descURing,"
        " :descARing,"

        " :vaccUIni,"
        " :vaccAIni,"
        " :vaccURing,"
        " :vaccARing,"

        " :exmcUDirFwd,"
        " :exmcADirFwd,"
        " :exmcUIndFwd,"
        " :exmcAIndFwd,"
        " :exmcUDirBack,"
        " :exmcADirBack,"
        " :exmcUIndBack,"
        " :exmcAIndBack,"

        " :tstcUDirFwd,"
        " :tstcADirFwd,"
        " :tstcUIndFwd,"
        " :tstcAIndFwd,"
        " :tstcUDirBack,"
        " :tstcADirBack,"
        " :tstcUIndBack,"
        " :tstcAIndBack,"
        " :tstcUTruePos,"
        " :tstcATruePos,"
        " :tstcUTrueNeg,"
        " :tstcATrueNeg,"
        " :tstcUFalsePos,"
        " :tstcAFalsePos,"
        " :tstcUFalseNeg,"
        " :tstcAFalseNeg,"

        " :zoncFoci"
      " )"
    )
  );


  qOutIterationByProductionType = new QSqlQuery( QSqlDatabase::database() );
  qOutIterationByProductionType->prepare(
    QString(
      "INSERT INTO outIterationByProductionType("
        " scenarioID,"
        " threadNo,"
        " iteration,"
        " productionTypeID,"

      // Destruction queue outputs
        " deswUMax,"
        " deswAMax,"
        " deswUMaxDay,"
        " deswAMaxDay,"
        " deswUTimeMax,"
        " deswUTimeAvg,"
        " deswUDaysInQueue,"
        " deswADaysInQueue,"

      // Vaccination queue outputs
        " vacwUMax,"
        " vacwAMax,"
        " vacwUMaxDay,"
        " vacwAMaxDay,"
        " vacwUTimeMax,"
        " vacwUTimeAvg,"

      // First events
        " firstDetection,"
        " firstDetUInf,"
        " firstDetAInf,"

        " firstDestruction,"
        " firstVaccination,"
        " lastDetection,"

      // Running totals for each disease state
        " tscUSusc,"
        " tscASusc,"
        " tscULat,"
        " tscALat,"
        " tscUSubc,"
        " tscASubc,"
        " tscUClin,"
        " tscAClin,"
        " tscUNImm,"
        " tscANImm,"
        " tscUVImm,"
        " tscAVImm,"
        " tscUDest,"
        " tscADest,"

      // Running totals for cause of infection
        " infcUIni,"
        " infcAIni,"
        " infcUAir,"
        " infcAAir,"
        " infcUDir,"
        " infcADir,"
        " infcUInd,"
        " infcAInd,"

      // Running totals for exposures
        " expcUDir,"
        " expcADir,"
        " expcUInd,"
        " expcAInd,"

      // Running totals for traces
        " trcUDirFwd,"
        " trcADirFwd,"
        " trcUIndFwd,"
        " trcAIndFwd,"
        " trcUDirpFwd,"
        " trcADirpFwd,"
        " trcUIndpFwd,"
        " trcAIndpFwd,"
        " trcUDirBack,"
        " trcADirBack,"
        " trcUIndBack,"
        " trcAIndBack,"
        " trcUDirpBack,"
        " trcADirpBack,"
        " trcUIndpBack,"
        " trcAIndpBack,"

      // Running totals for trace origins
        " tocUDirFwd,"
        " tocUIndFwd,"
        " tocUDirBack,"
        " tocUIndBack,"

      // Running totals for detection
        " detcUClin,"
        " detcAClin,"
        " detcUTest,"
        " detcATest,"

      // Running totals for destruction
        " descUIni,"
        " descAIni,"
        " descUDet,"
        " descADet,"
        " descUDirFwd,"
        " descADirFwd,"
        " descUIndFwd,"
        " descAIndFwd,"
        " descUDirBack,"
        " descADirBack,"
        " descUIndBack,"
        " descAIndBack,"
        " descURing,"
        " descARing,"

      // Running totals for vaccination
        " vaccUIni,"
        " vaccAIni,"
        " vaccURing,"
        " vaccARing,"

      // Running totals for herd exams
        " exmcUDirFwd,"
        " exmcADirFwd,"
        " exmcUIndFwd,"
        " exmcAIndFwd,"
        " exmcUDirBack,"
        " exmcADirBack,"
        " exmcUIndBack,"
        " exmcAIndBack,"

      // Running totals for diagnostic testing
        " tstcUDirFwd,"
        " tstcADirFwd,"
        " tstcUIndFwd,"
        " tstcAIndFwd,"
        " tstcUDirBack,"
        " tstcADirBack,"
        " tstcUIndBack,"
        " tstcAIndBack,"
        " tstcUTruePos,"
        " tstcATruePos,"
        " tstcUTrueNeg,"
        " tstcATrueNeg,"
        " tstcUFalsePos,"
        " tstcAFalsePos,"
        " tstcUFalseNeg,"
        " tstcAFalseNeg,"

      // Running totals for zone foci
        " zoncFoci"
      " )"
      " VALUES("
        " :scenarioID,"
        " :threadNo,"
        " :iteration,"
        " :productionTypeID,"

        " :deswUMax,"
        " :deswAMax,"
        " :deswUMaxDay,"
        " :deswAMaxDay,"
        " :deswUTimeMax,"
        " :deswUTimeAvg,"
        " :deswUDaysInQueue,"
        " :deswADaysInQueue,"

        " :vacwUMax,"
        " :vacwAMax,"
        " :vacwUMaxDay,"
        " :vacwAMaxDay,"
        " :vacwUTimeMax,"
        " :vacwUTimeAvg,"

        " :firstDetection,"
        " :firstDetUInf,"
        " :firstDetAInf,"

        " :firstDestruction,"
        " :firstVaccination,"
        " :lastDetection,"

        " :tscUSusc,"
        " :tscASusc,"
        " :tscULat,"
        " :tscALat,"
        " :tscUSubc,"
        " :tscASubc,"
        " :tscUClin,"
        " :tscAClin,"
        " :tscUNImm,"
        " :tscANImm,"
        " :tscUVImm,"
        " :tscAVImm,"
        " :tscUDest,"
        " :tscADest,"

        " :infcUIni,"
        " :infcAIni,"
        " :infcUAir,"
        " :infcAAir,"
        " :infcUDir,"
        " :infcADir,"
        " :infcUInd,"
        " :infcAInd,"

        " :expcUDir,"
        " :expcADir,"
        " :expcUInd,"
        " :expcAInd,"

        " :trcUDirFwd,"
        " :trcADirFwd,"
        " :trcUIndFwd,"
        " :trcAIndFwd,"
        " :trcUDirpFwd,"
        " :trcADirpFwd,"
        " :trcUIndpFwd,"
        " :trcAIndpFwd,"
        " :trcUDirBack,"
        " :trcADirBack,"
        " :trcUIndBack,"
        " :trcAIndBack,"
        " :trcUDirpBack,"
        " :trcADirpBack,"
        " :trcUIndpBack,"
        " :trcAIndpBack,"

        " :tocUDirFwd,"
        " :tocUIndFwd,"
        " :tocUDirBack,"
        " :tocUIndBack,"

        " :detcUClin,"
        " :detcAClin,"
        " :detcUTest,"
        " :detcATest,"

        " :descUIni,"
        " :descAIni,"
        " :descUDet,"
        " :descADet,"
        " :descUDirFwd,"
        " :descADirFwd,"
        " :descUIndFwd,"
        " :descAIndFwd,"
        " :descUDirBack,"
        " :descADirBack,"
        " :descUIndBack,"
        " :descAIndBack,"
        " :descURing,"
        " :descARing,"

        " :vaccUIni,"
        " :vaccAIni,"
        " :vaccURing,"
        " :vaccARing,"

        " :exmcUDirFwd,"
        " :exmcADirFwd,"
        " :exmcUIndFwd,"
        " :exmcAIndFwd,"
        " :exmcUDirBack,"
        " :exmcADirBack,"
        " :exmcUIndBack,"
        " :exmcAIndBack,"

        " :tstcUDirFwd,"
        " :tstcADirFwd,"
        " :tstcUIndFwd,"
        " :tstcAIndFwd,"
        " :tstcUDirBack,"
        " :tstcADirBack,"
        " :tstcUIndBack,"
        " :tstcAIndBack,"
        " :tstcUTruePos,"
        " :tstcATruePos,"
        " :tstcUTrueNeg,"
        " :tstcATrueNeg,"
        " :tstcUFalsePos,"
        " :tstcAFalsePos,"
        " :tstcUFalseNeg,"
        " :tstcAFalseNeg,"

        " :zoncFoci"
      " )"
    )
  );
}


void CSMDailyOutput::freeQueries() {
  delete qOutDailyByProductionType;
  delete qOutIterationByProductionType;
}


void CSMDailyOutput::setDailyRecordsFrom( CSMDailyOutput* src ) {
  clear();
  addDailyRecordsFrom( src );
}


void CSMDailyOutput::setAllRecordsFrom( CSMDailyOutput* src ){
  clear();
  addDailyRecordsFrom( src );
  addCumulRecordsFrom( src );
}


void CSMDailyOutput::addDailyRecordsFrom( CSMDailyOutput* src ) {
  // Daily numbers for each disease state
  inc( this->tsdUSusc, src->tsdUSusc );
  inc( this->tsdASusc, src->tsdASusc );
  inc( this->tsdULat, src->tsdULat );
  inc( this->tsdALat, src->tsdALat );
  inc( this->tsdUSubc, src->tsdUSubc );
  inc( this->tsdASubc, src->tsdASubc );
  inc( this->tsdUClin, src->tsdUClin );
  inc( this->tsdAClin, src->tsdAClin );
  inc( this->tsdUNImm, src->tsdUNImm );
  inc( this->tsdANImm, src->tsdANImm );
  inc( this->tsdUVImm, src->tsdUVImm );
  inc( this->tsdAVImm, src->tsdAVImm );
  inc( this->tsdUDest, src->tsdUDest );
  inc( this->tsdADest, src->tsdADest );

  // New daily counts for cause of infection
  inc( this->infnUAir, src->infnUAir );
  inc( this->infnAAir, src->infnAAir );
  inc( this->infnUDir, src->infnUDir );
  inc( this->infnADir, src->infnADir );
  inc( this->infnUInd, src->infnUInd );
  inc( this->infnAInd, src->infnAInd );

  // New daily counts for detection
  inc( this->detnUClin, src->detnUClin );
  inc( this->detnAClin, src->detnAClin );
  inc( this->detnUTest, src->detnUTest );
  inc( this->detnATest, src->detnATest );

  // New daily counts for herd exams
  inc( this->exmnUAll, src->exmnUAll );
  inc( this->exmnAAll, src->exmnAAll );

  // New daily counts for testing
  inc( this->tstnUTruePos, src->tstnUTruePos );
  inc( this->tstnATruePos, src->tstnATruePos );
  inc( this->tstnUTrueNeg, src->tstnUTrueNeg );
  inc( this->tstnATrueNeg, src->tstnATrueNeg );
  inc( this->tstnUFalsePos, src->tstnUFalsePos );
  inc( this->tstnAFalsePos, src->tstnAFalsePos );
  inc( this->tstnUFalseNeg, src->tstnUFalseNeg );
  inc( this->tstnAFalseNeg, src->tstnAFalseNeg );

  // New daily counts for tracing
  inc( this->trnUDirFwd, src->trnUDirFwd );
  inc( this->trnADirFwd, src->trnADirFwd );
  inc( this->trnUIndFwd, src->trnUIndFwd );
  inc( this->trnAIndFwd, src->trnAIndFwd );
  inc( this->trnUDirBack, src->trnUDirBack );
  inc( this->trnADirBack, src->trnADirBack );
  inc( this->trnUIndBack, src->trnUIndBack );
  inc( this->trnAIndBack, src->trnAIndBack );

  // new daily counts for trace origins
  inc( this->tocUDirFwd, src->tocUDirFwd );
  inc( this->tocUIndFwd, src->tocUIndFwd );
  inc( this->tocUDirBack, src->tocUDirBack );
  inc( this->tocUIndBack, src->tocUIndBack );

  // New daily counts for destruction for any reason
  inc( this->desnUAll, src->desnUAll );
  inc( this->desnAAll, src->desnAAll );

  // New daily counts for vaccination for any reason
  inc( this->vacnUAll, src->vacnUAll );
  inc( this->vacnAAll, src->vacnAAll );

  // New daily counts for zone foci
  inc( this->zonnFoci, src->zonnFoci );

  // Daily number of apparently infectious units
  inc( this->appdUInfectious, src->appdUInfectious );
}


void CSMDailyOutput::clearDailyTotals() {
  // Daily numbers for each disease state
  tsdUSusc = 0;
  tsdASusc = 0;
  tsdULat = 0;
  tsdALat = 0;
  tsdUSubc = 0;
  tsdASubc = 0;
  tsdUClin = 0;
  tsdAClin = 0;
  tsdUNImm = 0;
  tsdANImm = 0;
  tsdUVImm = 0;
  tsdAVImm = 0;
  tsdUDest = 0;
  tsdADest = 0;

  // Number of apparently infectious units
  appdUInfectious = 0;
}


void CSMDailyOutput::clearNewDailyCounts() {
  // New daily counts for cause of infection
  infnUAir = 0;
  infnAAir = 0;
  infnUDir = 0;
  infnADir = 0;
  infnUInd = 0;
  infnAInd = 0;

  // New daily counts for detection
  detnUClin = 0;
  detnAClin = 0;
  detnUTest = 0;
  detnATest = 0;

  // New daily counts for herd exams
  exmnUAll = 0;
  exmnAAll = 0;

  // New daily counts for testing
  tstnUTruePos = 0;
  tstnATruePos = 0;
  tstnUTrueNeg = 0;
  tstnATrueNeg = 0;
  tstnUFalsePos = 0;
  tstnAFalsePos = 0;
  tstnUFalseNeg = 0;
  tstnAFalseNeg = 0;

  // New daily counts for tracing forward
  trnUDirFwd = 0;
  trnADirFwd = 0;
  trnUIndFwd = 0;
  trnAIndFwd = 0;
  trnUDirBack = 0;
  trnADirBack = 0;
  trnUIndBack = 0;
  trnAIndBack = 0;

  // new daily counts for trace origins
  tonUDirFwd = 0;
  tonUIndFwd = 0;
  tonUDirBack = 0;
  tonUIndBack = 0;

  // New daily counts for destruction for any reason
  desnUAll = 0;
  desnAAll = 0;

  // New daily counts for vaccination for any reason
  vacnUAll = 0;
  vacnAAll = 0;

  // New daily counts for zone foci
  zonnFoci = 0;
}


int CSMDailyOutput::infUNew() {
  return infnUAir + infnUDir + infnUInd;
}


int CSMDailyOutput::infANew() {
  return infnAAir + infnADir + infnAInd;
}


void CSMDailyOutput::clear() {
  clearDailyTotals();
  clearNewDailyCounts();
  CSMIterationOutput::clear();
}


void CSMDailyOutput::updateDailyCounts(
  const int herdAnimalCount,
  const NAADSM_disease_state oldState,
  const NAADSM_disease_state newState
){
  // Decrement the daily number of units in oldState
  // Subtract herdAnimalCount from the daily number of animals in oldState
  // Increment the daily number of units in newState
  // Add the herdAnimalCount to the daily number of animals in newState
  // Increment the running number of units in newState
  // Add the herdAnimalCount to the running number of animals in newState

  decrementDailyCounts( herdAnimalCount, oldState );
  incrementDailyCounts( herdAnimalCount, newState );
}


void CSMDailyOutput::incrementDailyCounts( const int herdAnimalCount, const NAADSM_disease_state newState ){
  switch( newState ) {
    case NAADSM_StateSusceptible:
      inc( tsdUSusc );
      inc( tsdASusc, herdAnimalCount );
      inc( tscUSusc );
      inc( tscASusc, herdAnimalCount );
      break;
    case NAADSM_StateLatent:
      inc( tsdULat );
      inc( tsdALat, herdAnimalCount );
      inc( tscULat );
      inc( tscALat, herdAnimalCount );
      break;
    case NAADSM_StateInfectiousSubclinical:
      inc( tsdUSubc );
      inc( tsdASubc, herdAnimalCount );
      inc( tscUSubc );
      inc( tscASubc, herdAnimalCount );
      break;
     case NAADSM_StateInfectiousClinical:
      inc( tsdUClin );
      inc( tsdAClin, herdAnimalCount );
      inc( tscUClin );
      inc( tscAClin, herdAnimalCount );
      break;
    case NAADSM_StateNaturallyImmune:
      inc( tsdUNImm );
      inc( tsdANImm, herdAnimalCount );
      inc( tscUNImm );
      inc( tscANImm, herdAnimalCount );
      break;
    case NAADSM_StateVaccineImmune:
      inc( tsdUVImm );
      inc( tsdAVImm, herdAnimalCount );
      inc( tscUVImm );
      inc( tscAVImm, herdAnimalCount );
      break;
    case NAADSM_StateDestroyed:
      inc( tsdUDest );
      inc( tsdADest, herdAnimalCount );
      inc( tscUDest );
      inc( tscADest, herdAnimalCount );
      break;
    default:
      qFatal( "Unrecognized herd status in TSMDailyOutput.incrementDailyCounts()" );
  }
}


void CSMDailyOutput::decrementDailyCounts( const int herdAnimalCount, const NAADSM_disease_state oldState ) {
  switch( oldState ) {
    case NAADSM_StateSusceptible:
      dec( tsdUSusc );
      dec( tsdASusc, herdAnimalCount );
      break;
    case NAADSM_StateLatent:
      dec( tsdULat );
      dec( tsdALat, herdAnimalCount );
      break;
    case NAADSM_StateInfectiousSubclinical:
      dec( tsdUSubc );
      dec( tsdASubc, herdAnimalCount );
      break;
    case NAADSM_StateInfectiousClinical:
      dec( tsdUClin );
      dec( tsdAClin, herdAnimalCount );
      break;
    case NAADSM_StateNaturallyImmune:
      dec( tsdUNImm );
      dec( tsdANImm, herdAnimalCount );
      break;
    case NAADSM_StateVaccineImmune:
      dec( tsdUVImm );
      dec( tsdAVImm, herdAnimalCount );
      break;
    case NAADSM_StateDestroyed:
      qFatal( "Number of destroyed herd is going down!  This can't happen!" );
      break;
    default:
      qFatal( "Unrecognized disease state in TProductionType.makeDailyRecord" );
  }

  if (
      ( 0 > tsdUSusc )
    ||
      ( 0 > tsdASusc )
    ||
      ( 0 > tsdULat )
    ||
      ( 0 > tsdALat )
    ||
      ( 0 > tsdUSubc )
    ||
      ( 0 > tsdASubc )
    ||
      ( 0 > tsdUClin )
    ||
      ( 0 > tsdAClin )
    ||
      ( 0 > tsdUNImm )
    ||
      ( 0 > tsdANImm )
    ||
      ( 0 > tsdUVImm )
    ||
      ( 0 > tsdAVImm )
    ||
      ( 0 > tsdUDest )
    ||
      ( 0 > tsdADest )
  ) {
    // More debugging statements might be helpful here, some day.
    qFatal( "Number of units or animals in transition state is less than 0" );
  }
}


void CSMDailyOutput::insertDatabaseOutputs(
  const NAADSM_output_type drt,
  CSMDatabase* db,
  const int ptID,
  const int iteration,
  const int day /* = 0 */
) {
  if( DRTDaily == drt ) {
    qOutDailyByProductionType->bindValue( ":scenarioID", db->scenarioID() );
    qOutDailyByProductionType->bindValue( ":threadNo", THREAD_NUMBER );
    qOutDailyByProductionType->bindValue( ":iteration", iteration );
    qOutDailyByProductionType->bindValue( ":productionTypeID", ptID );

    qOutDailyByProductionType->bindValue( ":day", day );

    qOutDailyByProductionType->bindValue( ":tsdUSusc", tsdUSusc );
    qOutDailyByProductionType->bindValue( ":tsdASusc", tsdASusc );
    qOutDailyByProductionType->bindValue( ":tsdULat", tsdULat );
    qOutDailyByProductionType->bindValue( ":tsdALat", tsdALat );
    qOutDailyByProductionType->bindValue( ":tsdUSubc", tsdUSubc );
    qOutDailyByProductionType->bindValue( ":tsdASubc", tsdASubc );
    qOutDailyByProductionType->bindValue( ":tsdUClin", tsdUClin );
    qOutDailyByProductionType->bindValue( ":tsdAClin", tsdAClin );
    qOutDailyByProductionType->bindValue( ":tsdUNImm", tsdUNImm );
    qOutDailyByProductionType->bindValue( ":tsdANImm", tsdANImm );
    qOutDailyByProductionType->bindValue( ":tsdUVImm", tsdUVImm );
    qOutDailyByProductionType->bindValue( ":tsdAVImm", tsdAVImm );
    qOutDailyByProductionType->bindValue( ":tsdUDest", tsdUDest );
    qOutDailyByProductionType->bindValue( ":tsdADest", tsdADest );

    qOutDailyByProductionType->bindValue( ":infnUAir", infnUAir );
    qOutDailyByProductionType->bindValue( ":infnAAir", infnAAir );
    qOutDailyByProductionType->bindValue( ":infnUDir", infnUDir );
    qOutDailyByProductionType->bindValue( ":infnADir", infnADir );
    qOutDailyByProductionType->bindValue( ":infnUInd", infnUInd );
    qOutDailyByProductionType->bindValue( ":infnAInd", infnAInd );

    qOutDailyByProductionType->bindValue( ":detnUClin", detnUClin );
    qOutDailyByProductionType->bindValue( ":detnAClin", detnAClin );
    qOutDailyByProductionType->bindValue( ":detnUTest", detnUTest );
    qOutDailyByProductionType->bindValue( ":detnATest", detnATest );

    qOutDailyByProductionType->bindValue( ":exmnUAll", exmnUAll );
    qOutDailyByProductionType->bindValue( ":exmnAAll", exmnAAll );

    qOutDailyByProductionType->bindValue( ":tstnUTruePos", tstnUTruePos );
    qOutDailyByProductionType->bindValue( ":tstnATruePos", tstnATruePos );
    qOutDailyByProductionType->bindValue( ":tstnUTrueNeg", tstnUTrueNeg );
    qOutDailyByProductionType->bindValue( ":tstnATrueNeg", tstnATrueNeg );
    qOutDailyByProductionType->bindValue( ":tstnUFalsePos", tstnUFalsePos );
    qOutDailyByProductionType->bindValue( ":tstnAFalsePos", tstnAFalsePos );
    qOutDailyByProductionType->bindValue( ":tstnUFalseNeg", tstnUFalseNeg );
    qOutDailyByProductionType->bindValue( ":tstnAFalseNeg", tstnAFalseNeg );

    qOutDailyByProductionType->bindValue( ":trnUDirFwd", trnUDirFwd );
    qOutDailyByProductionType->bindValue( ":trnADirFwd", trnADirFwd );
    qOutDailyByProductionType->bindValue( ":trnUIndFwd", trnUIndFwd );
    qOutDailyByProductionType->bindValue( ":trnAIndFwd", trnAIndFwd );
    qOutDailyByProductionType->bindValue( ":trnUDirBack", trnUDirBack );
    qOutDailyByProductionType->bindValue( ":trnADirBack", trnADirBack );
    qOutDailyByProductionType->bindValue( ":trnUIndBack", trnUIndBack );
    qOutDailyByProductionType->bindValue( ":trnAIndBack", trnAIndBack );

    qOutDailyByProductionType->bindValue( ":tonUDirFwd", tonUDirFwd );
    qOutDailyByProductionType->bindValue( ":tonUIndFwd", tonUIndFwd );
    qOutDailyByProductionType->bindValue( ":tonUDirBack", tonUDirBack );
    qOutDailyByProductionType->bindValue( ":tonUIndBack", tonUIndBack );

    qOutDailyByProductionType->bindValue( ":desnUAll", desnUAll );
    qOutDailyByProductionType->bindValue( ":desnAAll", desnAAll );

    qOutDailyByProductionType->bindValue( ":deswUAll", _destrQueueLengthUnits );
    qOutDailyByProductionType->bindValue( ":deswAAll", _destrQueueLengthAnimals );

    qOutDailyByProductionType->bindValue( ":vacnUAll", vacnUAll );
    qOutDailyByProductionType->bindValue( ":vacnAAll", vacnAAll );

    qOutDailyByProductionType->bindValue( ":vacwUAll", _vaccQueueLengthUnits );
    qOutDailyByProductionType->bindValue( ":vacwAAll", _vaccQueueLengthAnimals );

    qOutDailyByProductionType->bindValue( ":zonnFoci", zonnFoci );

    qOutDailyByProductionType->bindValue( ":appdUInfectious", appdUInfectious );

    qOutDailyByProductionType->bindValue( ":tscUSusc", tscUSusc );
    qOutDailyByProductionType->bindValue( ":tscASusc", tscASusc );
    qOutDailyByProductionType->bindValue( ":tscULat", tscULat );
    qOutDailyByProductionType->bindValue( ":tscALat", tscALat );
    qOutDailyByProductionType->bindValue( ":tscUSubc", tscUSubc );
    qOutDailyByProductionType->bindValue( ":tscASubc", tscASubc );
    qOutDailyByProductionType->bindValue( ":tscUClin", tscUClin );
    qOutDailyByProductionType->bindValue( ":tscAClin", tscAClin );
    qOutDailyByProductionType->bindValue( ":tscUNImm", tscUNImm );
    qOutDailyByProductionType->bindValue( ":tscANImm", tscANImm );
    qOutDailyByProductionType->bindValue( ":tscUVImm", tscUVImm );
    qOutDailyByProductionType->bindValue( ":tscAVImm", tscAVImm );
    qOutDailyByProductionType->bindValue( ":tscUDest", tscUDest );
    qOutDailyByProductionType->bindValue( ":tscADest", tscADest );

    qOutDailyByProductionType->bindValue( ":infcUIni", infcUIni );
    qOutDailyByProductionType->bindValue( ":infcAIni", infcAIni );
    qOutDailyByProductionType->bindValue( ":infcUAir", infcUAir );
    qOutDailyByProductionType->bindValue( ":infcAAir", infcAAir );
    qOutDailyByProductionType->bindValue( ":infcUDir", infcUDir );
    qOutDailyByProductionType->bindValue( ":infcADir", infcADir );
    qOutDailyByProductionType->bindValue( ":infcUInd", infcUInd );
    qOutDailyByProductionType->bindValue( ":infcAInd", infcAInd );

    qOutDailyByProductionType->bindValue( ":expcUDir", expcUDir );
    qOutDailyByProductionType->bindValue( ":expcADir", expcADir );
    qOutDailyByProductionType->bindValue( ":expcUInd", expcUInd );
    qOutDailyByProductionType->bindValue( ":expcAInd", expcAInd );

    qOutDailyByProductionType->bindValue( ":trcUDirFwd", trcUDirFwd );
    qOutDailyByProductionType->bindValue( ":trcADirFwd", trcADirFwd );
    qOutDailyByProductionType->bindValue( ":trcUIndFwd", trcUIndFwd );
    qOutDailyByProductionType->bindValue( ":trcAIndFwd", trcAIndFwd );
    qOutDailyByProductionType->bindValue( ":trcUDirpFwd", trcUDirpFwd );
    qOutDailyByProductionType->bindValue( ":trcADirpFwd", trcADirpFwd );
    qOutDailyByProductionType->bindValue( ":trcUIndpFwd", trcUIndpFwd );
    qOutDailyByProductionType->bindValue( ":trcAIndpFwd", trcAIndpFwd );
    qOutDailyByProductionType->bindValue( ":trcUDirBack", trcUDirBack );
    qOutDailyByProductionType->bindValue( ":trcADirBack", trcADirBack );
    qOutDailyByProductionType->bindValue( ":trcUIndBack", trcUIndBack );
    qOutDailyByProductionType->bindValue( ":trcAIndBack", trcAIndBack );
    qOutDailyByProductionType->bindValue( ":trcUDirpBack", trcUDirpBack );
    qOutDailyByProductionType->bindValue( ":trcADirpBack", trcADirpBack );
    qOutDailyByProductionType->bindValue( ":trcUIndpBack", trcUIndpBack );
    qOutDailyByProductionType->bindValue( ":trcAIndpBack", trcAIndpBack );

    qOutDailyByProductionType->bindValue( ":tocUDirFwd", tocUDirFwd );
    qOutDailyByProductionType->bindValue( ":tocUIndFwd", tocUIndFwd );
    qOutDailyByProductionType->bindValue( ":tocUDirBack", tocUDirBack );
    qOutDailyByProductionType->bindValue( ":tocUIndBack", tocUIndBack );

    qOutDailyByProductionType->bindValue( ":detcUClin", detcUClin );
    qOutDailyByProductionType->bindValue( ":detcAClin", detcAClin );
    qOutDailyByProductionType->bindValue( ":detcUTest", detcUTest );
    qOutDailyByProductionType->bindValue( ":detcATest", detcATest );

    qOutDailyByProductionType->bindValue( ":descUIni", descUIni );
    qOutDailyByProductionType->bindValue( ":descAIni", descAIni );
    qOutDailyByProductionType->bindValue( ":descUDet", descUDet );
    qOutDailyByProductionType->bindValue( ":descADet", descADet );
    qOutDailyByProductionType->bindValue( ":descUDirFwd", descUDirFwd );
    qOutDailyByProductionType->bindValue( ":descADirFwd", descADirFwd );
    qOutDailyByProductionType->bindValue( ":descUIndFwd", descUIndFwd );
    qOutDailyByProductionType->bindValue( ":descAIndFwd", descAIndFwd );
    qOutDailyByProductionType->bindValue( ":descUDirBack", descUDirBack );
    qOutDailyByProductionType->bindValue( ":descADirBack", descADirBack );
    qOutDailyByProductionType->bindValue( ":descUIndBack", descUIndBack );
    qOutDailyByProductionType->bindValue( ":descAIndBack", descAIndBack );
    qOutDailyByProductionType->bindValue( ":descURing", descURing );
    qOutDailyByProductionType->bindValue( ":descARing", descARing );

    qOutDailyByProductionType->bindValue( ":vaccUIni", vaccUIni );
    qOutDailyByProductionType->bindValue( ":vaccAIni", vaccAIni );
    qOutDailyByProductionType->bindValue( ":vaccURing", vaccURing );
    qOutDailyByProductionType->bindValue( ":vaccARing", vaccARing );

    qOutDailyByProductionType->bindValue( ":exmcUDirFwd", exmcUDirFwd );
    qOutDailyByProductionType->bindValue( ":exmcADirFwd", exmcADirFwd );
    qOutDailyByProductionType->bindValue( ":exmcUIndFwd", exmcUIndFwd );
    qOutDailyByProductionType->bindValue( ":exmcAIndFwd", exmcAIndFwd );
    qOutDailyByProductionType->bindValue( ":exmcUDirBack", exmcUDirBack );
    qOutDailyByProductionType->bindValue( ":exmcADirBack", exmcADirBack );
    qOutDailyByProductionType->bindValue( ":exmcUIndBack", exmcUIndBack );
    qOutDailyByProductionType->bindValue( ":exmcAIndBack", exmcAIndBack );

    qOutDailyByProductionType->bindValue( ":tstcUDirFwd", tstcUDirFwd );
    qOutDailyByProductionType->bindValue( ":tstcADirFwd", tstcADirFwd );
    qOutDailyByProductionType->bindValue( ":tstcUIndFwd", tstcUIndFwd );
    qOutDailyByProductionType->bindValue( ":tstcAIndFwd", tstcAIndFwd );
    qOutDailyByProductionType->bindValue( ":tstcUDirBack", tstcUDirBack );
    qOutDailyByProductionType->bindValue( ":tstcADirBack", tstcADirBack );
    qOutDailyByProductionType->bindValue( ":tstcUIndBack", tstcUIndBack );
    qOutDailyByProductionType->bindValue( ":tstcAIndBack", tstcAIndBack );
    qOutDailyByProductionType->bindValue( ":tstcUTruePos", tstcUTruePos );
    qOutDailyByProductionType->bindValue( ":tstcATruePos", tstcATruePos );
    qOutDailyByProductionType->bindValue( ":tstcUTrueNeg", tstcUTrueNeg );
    qOutDailyByProductionType->bindValue( ":tstcATrueNeg", tstcATrueNeg );
    qOutDailyByProductionType->bindValue( ":tstcUFalsePos", tstcUFalsePos );
    qOutDailyByProductionType->bindValue( ":tstcAFalsePos", tstcAFalsePos );
    qOutDailyByProductionType->bindValue( ":tstcUFalseNeg", tstcUFalseNeg );
    qOutDailyByProductionType->bindValue( ":tstcAFalseNeg", tstcAFalseNeg );

    qOutDailyByProductionType->bindValue( ":zoncFoci", zoncFoci );

    if( !qOutDailyByProductionType->exec() ) {
      naadsmException(
        QString( "Database records could not be saved.  Query failed: " ).append( qOutDailyByProductionType->lastQuery() ).append( " " ).append( qOutDailyByProductionType->lastError().text() )
      );
    }
  }
  else if( DRTIteration == drt ) {
    qOutIterationByProductionType->bindValue( ":scenarioID", db->scenarioID() );
    qOutIterationByProductionType->bindValue( ":threadNo", THREAD_NUMBER );
    qOutIterationByProductionType->bindValue( ":iteration", iteration );
    qOutIterationByProductionType->bindValue( ":productionTypeID", ptID );

    qOutIterationByProductionType->bindValue( ":deswUMax", deswUMax );
    qOutIterationByProductionType->bindValue( ":deswAMax", deswAMax );
    qOutIterationByProductionType->bindValue( ":deswUMaxDay", deswUMaxDay );
    qOutIterationByProductionType->bindValue( ":deswAMaxDay", deswAMaxDay );
    qOutIterationByProductionType->bindValue( ":deswUTimeMax", deswUTimeMax );
    qOutIterationByProductionType->bindValue( ":deswUTimeAvg", deswUTimeAvg() );
    qOutIterationByProductionType->bindValue( ":deswUDaysInQueue", deswUDaysInQueue );
    qOutIterationByProductionType->bindValue( ":deswADaysInQueue", deswADaysInQueue );

    qOutIterationByProductionType->bindValue( ":vacwUMax", vacwUMax );
    qOutIterationByProductionType->bindValue( ":vacwAMax", vacwAMax );
    qOutIterationByProductionType->bindValue( ":vacwUMaxDay", vacwUMaxDay );
    qOutIterationByProductionType->bindValue( ":vacwAMaxDay", vacwAMaxDay );
    qOutIterationByProductionType->bindValue( ":vacwUTimeMax", vacwUTimeMax );
    qOutIterationByProductionType->bindValue( ":vacwUTimeAvg", vacwUTimeAvg() );

    if( -1 != firstDetection )
      qOutIterationByProductionType->bindValue( ":firstDetection", firstDetection );
    else
      qOutIterationByProductionType->bindValue( ":firstDetection", QVariant( QVariant::Int ) );

    if( -1 != firstDetUInf )
      qOutIterationByProductionType->bindValue( ":firstDetUInf", firstDetUInf );
    else
      qOutIterationByProductionType->bindValue( ":firstDetUInf",  QVariant( QVariant::Int ) );

    if( -1 != firstDetAInf )
      qOutIterationByProductionType->bindValue( ":firstDetAInf", firstDetAInf );
    else
      qOutIterationByProductionType->bindValue( ":firstDetAInf", QVariant( QVariant::Int ) );

    if( -1 != firstDestruction )
      qOutIterationByProductionType->bindValue( ":firstDestruction", firstDestruction );
    else
      qOutIterationByProductionType->bindValue( ":firstDestruction", QVariant( QVariant::Int ) );

    if( -1 != firstVaccination )
      qOutIterationByProductionType->bindValue( ":firstVaccination", firstVaccination );
    else
      qOutIterationByProductionType->bindValue( ":firstVaccination", QVariant( QVariant::Int ) );

    if( -1 != lastDetection )
      qOutIterationByProductionType->bindValue( ":lastDetection", lastDetection );
    else
      qOutIterationByProductionType->bindValue( ":lastDetection", QVariant( QVariant::Int ) );

    qOutIterationByProductionType->bindValue( ":tscUSusc", tscUSusc );
    qOutIterationByProductionType->bindValue( ":tscASusc", tscASusc );
    qOutIterationByProductionType->bindValue( ":tscULat", tscULat );
    qOutIterationByProductionType->bindValue( ":tscALat", tscALat );
    qOutIterationByProductionType->bindValue( ":tscUSubc", tscUSubc );
    qOutIterationByProductionType->bindValue( ":tscASubc", tscASubc );
    qOutIterationByProductionType->bindValue( ":tscUClin", tscUClin );
    qOutIterationByProductionType->bindValue( ":tscAClin", tscAClin );
    qOutIterationByProductionType->bindValue( ":tscUNImm", tscUNImm );
    qOutIterationByProductionType->bindValue( ":tscANImm", tscANImm );
    qOutIterationByProductionType->bindValue( ":tscUVImm", tscUVImm );
    qOutIterationByProductionType->bindValue( ":tscAVImm", tscAVImm );
    qOutIterationByProductionType->bindValue( ":tscUDest", tscUDest );
    qOutIterationByProductionType->bindValue( ":tscADest", tscADest );

    qOutIterationByProductionType->bindValue( ":infcUIni", infcUIni );
    qOutIterationByProductionType->bindValue( ":infcAIni", infcAIni );
    qOutIterationByProductionType->bindValue( ":infcUAir", infcUAir );
    qOutIterationByProductionType->bindValue( ":infcAAir", infcAAir );
    qOutIterationByProductionType->bindValue( ":infcUDir", infcUDir );
    qOutIterationByProductionType->bindValue( ":infcADir", infcADir );
    qOutIterationByProductionType->bindValue( ":infcUInd", infcUInd );
    qOutIterationByProductionType->bindValue( ":infcAInd", infcAInd );

    qOutIterationByProductionType->bindValue( ":expcUDir", expcUDir );
    qOutIterationByProductionType->bindValue( ":expcADir", expcADir );
    qOutIterationByProductionType->bindValue( ":expcUInd", expcUInd );
    qOutIterationByProductionType->bindValue( ":expcAInd", expcAInd );

    qOutIterationByProductionType->bindValue( ":trcUDirFwd", trcUDirFwd );
    qOutIterationByProductionType->bindValue( ":trcADirFwd", trcADirFwd );
    qOutIterationByProductionType->bindValue( ":trcUIndFwd", trcUIndFwd );
    qOutIterationByProductionType->bindValue( ":trcAIndFwd", trcAIndFwd );
    qOutIterationByProductionType->bindValue( ":trcUDirpFwd", trcUDirpFwd );
    qOutIterationByProductionType->bindValue( ":trcADirpFwd", trcADirpFwd );
    qOutIterationByProductionType->bindValue( ":trcUIndpFwd", trcUIndpFwd );
    qOutIterationByProductionType->bindValue( ":trcAIndpFwd", trcAIndpFwd );
    qOutIterationByProductionType->bindValue( ":trcUDirBack", trcUDirBack );
    qOutIterationByProductionType->bindValue( ":trcADirBack", trcADirBack );
    qOutIterationByProductionType->bindValue( ":trcUIndBack", trcUIndBack );
    qOutIterationByProductionType->bindValue( ":trcAIndBack", trcAIndBack );
    qOutIterationByProductionType->bindValue( ":trcUDirpBack", trcUDirpBack );
    qOutIterationByProductionType->bindValue( ":trcADirpBack", trcADirpBack );
    qOutIterationByProductionType->bindValue( ":trcUIndpBack", trcUIndpBack );
    qOutIterationByProductionType->bindValue( ":trcAIndpBack", trcAIndpBack );

    qOutIterationByProductionType->bindValue( ":tocUDirFwd", tocUDirFwd );
    qOutIterationByProductionType->bindValue( ":tocUIndFwd", tocUIndFwd );
    qOutIterationByProductionType->bindValue( ":tocUDirBack", tocUDirBack );
    qOutIterationByProductionType->bindValue( ":tocUIndBack", tocUIndBack );

    qOutIterationByProductionType->bindValue( ":detcUClin", detcUClin );
    qOutIterationByProductionType->bindValue( ":detcAClin", detcAClin );
    qOutIterationByProductionType->bindValue( ":detcUTest", detcUTest );
    qOutIterationByProductionType->bindValue( ":detcATest", detcATest );

    qOutIterationByProductionType->bindValue( ":descUIni", descUIni );
    qOutIterationByProductionType->bindValue( ":descAIni", descAIni );
    qOutIterationByProductionType->bindValue( ":descUDet", descUDet );
    qOutIterationByProductionType->bindValue( ":descADet", descADet );
    qOutIterationByProductionType->bindValue( ":descUDirFwd", descUDirFwd );
    qOutIterationByProductionType->bindValue( ":descADirFwd", descADirFwd );
    qOutIterationByProductionType->bindValue( ":descUIndFwd", descUIndFwd );
    qOutIterationByProductionType->bindValue( ":descAIndFwd", descAIndFwd );
    qOutIterationByProductionType->bindValue( ":descUDirBack", descUDirBack );
    qOutIterationByProductionType->bindValue( ":descADirBack", descADirBack );
    qOutIterationByProductionType->bindValue( ":descUIndBack", descUIndBack );
    qOutIterationByProductionType->bindValue( ":descAIndBack", descAIndBack );
    qOutIterationByProductionType->bindValue( ":descURing", descURing );
    qOutIterationByProductionType->bindValue( ":descARing", descARing );

    qOutIterationByProductionType->bindValue( ":vaccUIni", vaccUIni );
    qOutIterationByProductionType->bindValue( ":vaccAIni", vaccAIni );
    qOutIterationByProductionType->bindValue( ":vaccURing", vaccURing );
    qOutIterationByProductionType->bindValue( ":vaccARing", vaccARing );

    qOutIterationByProductionType->bindValue( ":exmcUDirFwd", exmcUDirFwd );
    qOutIterationByProductionType->bindValue( ":exmcADirFwd", exmcADirFwd );
    qOutIterationByProductionType->bindValue( ":exmcUIndFwd", exmcUIndFwd );
    qOutIterationByProductionType->bindValue( ":exmcAIndFwd", exmcAIndFwd );
    qOutIterationByProductionType->bindValue( ":exmcUDirBack", exmcUDirBack );
    qOutIterationByProductionType->bindValue( ":exmcADirBack", exmcADirBack );
    qOutIterationByProductionType->bindValue( ":exmcUIndBack", exmcUIndBack );
    qOutIterationByProductionType->bindValue( ":exmcAIndBack", exmcAIndBack );

    qOutIterationByProductionType->bindValue( ":tstcUDirFwd", tstcUDirFwd );
    qOutIterationByProductionType->bindValue( ":tstcADirFwd", tstcADirFwd );
    qOutIterationByProductionType->bindValue( ":tstcUIndFwd", tstcUIndFwd );
    qOutIterationByProductionType->bindValue( ":tstcAIndFwd", tstcAIndFwd );
    qOutIterationByProductionType->bindValue( ":tstcUDirBack", tstcUDirBack );
    qOutIterationByProductionType->bindValue( ":tstcADirBack", tstcADirBack );
    qOutIterationByProductionType->bindValue( ":tstcUIndBack", tstcUIndBack );
    qOutIterationByProductionType->bindValue( ":tstcAIndBack", tstcAIndBack );
    qOutIterationByProductionType->bindValue( ":tstcUTruePos", tstcUTruePos );
    qOutIterationByProductionType->bindValue( ":tstcATruePos", tstcATruePos );
    qOutIterationByProductionType->bindValue( ":tstcUTrueNeg", tstcUTrueNeg );
    qOutIterationByProductionType->bindValue( ":tstcATrueNeg", tstcATrueNeg );
    qOutIterationByProductionType->bindValue( ":tstcUFalsePos", tstcUFalsePos );
    qOutIterationByProductionType->bindValue( ":tstcAFalsePos", tstcAFalsePos );
    qOutIterationByProductionType->bindValue( ":tstcUFalseNeg", tstcUFalseNeg );
    qOutIterationByProductionType->bindValue( ":tstcAFalseNeg", tstcAFalseNeg );

    qOutIterationByProductionType->bindValue( ":zoncFoci", zoncFoci );

    if( !qOutIterationByProductionType->exec() ) {
      naadsmException(
        QString( "Database records could not be saved.  Query failed: " ).append( qOutIterationByProductionType->lastQuery() ).append( " " ).append( qOutIterationByProductionType->lastError().text() )
      );
    }
  }
  else {
    naadsmException( QString( "There is a problem in CSMDailyOutput::insertDatabaseOutputs()" ) );
  }
}







