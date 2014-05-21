#include "csmptoutput.h"

#include <ar_general_purpose/qmath.h>

#include <QDebug>

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
}


void CSMDailyOutput::initialize() {
  CSMIterationOutput::initialize();
  clear();
}


CSMDailyOutput::~CSMDailyOutput() {
  // Nothing to do here
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
  const int ptiD,
  const int iteration,
  const int day /* = 0 */
) {
  qDebug() << "FIXME: Do database stuff in CSMDailyOutput::insertDatabaseOutputs";
}







