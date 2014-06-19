// Begin 5/16/2014

#include "csmsimulation.h"



CSMSimulation::CSMSimulation( HRD_herd_list_t* herds, ZON_zone_list_t* zones, CSMDatabase* db ) {
  _db = db;

  _ptList = new CProdTypeList( herds, db );
  _zoneList = new CZoneList( zones, this );
}


CSMSimulation::~CSMSimulation() {
  delete _ptList;
  delete _zoneList;

  // Don't delete _db: it isn't owned by this object.
}


bool CSMSimulation::includeZonesGlobal() {
  if( NULL == _zoneList )
    return true;
  else
    return( !_zoneList->isEmpty() );
}


void CSMSimulation::initializeAllOutputRecords() {
  _currentIteration = 0;
  _lastCompleteIteration = -1;
  _ptList->clearAllRecords();

  if( includeZonesGlobal() )
    _zoneList->clearAllRecords();
}


// Carried out just before an iteration begins
void CSMSimulation::prepareForIteration( int it ) {
  _currentIteration = it;
  _ptList->resetIterationRecords();

  if( includeZonesGlobal() )
    _zoneList->resetIterationRecords();
}


// Carried out just before a day begins
void CSMSimulation::prepareForDay() {
  _ptList->prepareForDay();

  if( includeZonesGlobal() )
    _zoneList->prepareForDay();
}


void CSMSimulation::processDailyRecords( CSMDatabase* db, int day ) {
  _ptList->processDailyRecords( db, _currentIteration, day );

  if( includeZonesGlobal() )
    _zoneList->processDailyRecords( db, _currentIteration, day );
}


void CSMSimulation::processIterationRecords( CSMDatabase* db, int it ) {
  _lastCompleteIteration = it;

  _ptList->processIterationRecords( db, _lastCompleteIteration );

  if( includeZonesGlobal() )
    _zoneList->processIterationRecords( db, _lastCompleteIteration );
}


void CSMSimulation::simComplete() {
  // _ptList has nothing to do

  if( includeZonesGlobal() )
    _zoneList->simComplete();
}


