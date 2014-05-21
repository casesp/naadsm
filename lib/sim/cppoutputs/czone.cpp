// Begin 5/14/2014

#include "czone.h"

#include <QDebug>

//------------------------------------------------------------------------------
// CZone
//------------------------------------------------------------------------------
CZone::CZone() {
  _level = -1;
  _descr = "";

  _area = 0.0;
  _maxArea = 0.0;
  _maxAreaDay = 0;

  _perimeter = 0.0;
  _maxPerimeter = 0.0;
  _maxAreaDay = 0;

  _herdDays = NULL;
  _animalDays = NULL;
  _herdCount = NULL;
  _animalCount = NULL;
}


CZone::~CZone() {
  freeAndNullMaps();
}


void CZone::freeAndNullMaps() {
  if( NULL != _herdDays )
    delete _herdDays;

  if( NULL != _animalDays )
    delete _animalDays;

  if( NULL != _herdCount )
    delete _herdCount;

  if( NULL != _animalCount )
    delete _animalCount;

  _herdDays = NULL;
  _animalDays = NULL;
  _herdCount = NULL;
  _animalCount = NULL;
}


void CZone::addToZoneTotals( int ptID, int herdSize ) {
  // Update running totals
  if( _herdDays->contains( ptID) )
    _herdDays->insert( ptID, _herdDays->value( ptID ) + 1 );
  else
    _herdDays->insert( ptID, 1 );

  if( _animalDays->contains( ptID ) )
    _animalDays->insert( ptID, _animalDays->value( ptID ) + herdSize );
  else
    _animalDays->insert( ptID, herdSize );

  // Update the daily counts
  if( _herdCount->contains( ptID ) )
    _herdCount->insert( ptID, _herdCount->value( ptID ) + 1 );
  else
    _herdCount->insert( ptID, 1 );

  if( _animalCount->contains( ptID ) )
    _animalCount->insert( ptID, _animalCount->value( ptID ) + herdSize );
  else
    _animalCount->insert( ptID, herdSize );
}


void CZone::setArea( double val, int day ) {
  _area = val;

  if( val > _maxArea ) {
    _maxArea = val;
    _maxAreaDay = day;
  }
}


void CZone::setPerimeter( double val, int day ) {
  _perimeter = val;

  if( val > _maxPerimeter ) {
    _maxPerimeter = val;
    _maxPerimeterDay = day;
  }
}


void CZone::clearAllRecords( CSMDatabase* db ) {
  // Check for existing objects, just in case.
  // (They should never exist at this point.)
  freeAndNullMaps();

  _herdDays = new QMap<int, int>();
  _animalDays = new QMap<int, int>();

  _herdCount = new QMap<int, int>();
  _animalCount = new QMap<int, int>();

  _area = 0.0;
}


void CZone::resetIterationRecords(){
  _herdDays->clear();
  _animalDays->clear();

  _area = 0.0;
  _maxArea = 0.0;
  _maxAreaDay = 0;

  _perimeter = 0.0;
  _maxPerimeter = 0.0;
  _maxPerimeterDay = 0;
}


void CZone::prepareForDay() {
  _herdCount->clear();
  _animalCount->clear();
}


void CZone::processDailyRecords( CSMDatabase* db, int iteration, int day ) {
  qDebug() << "FIXME: do database stuff";
}


void CZone::processIterationRecords( CSMDatabase* db, int iteration, bool focusCreated ) {
  qDebug() << "FIXME: do database stuff";
}


void CZone::simComplete(){
  freeAndNullMaps();
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// CZoneList
//------------------------------------------------------------------------------
CZoneList::CZoneList() : QMap<int, CZone*>() {

}


CZoneList::~CZoneList() {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    delete it.next().value();
}


void CZoneList::clearAllRecords( CSMDatabase* db ) {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->clearAllRecords( db );
}


void CZoneList::resetIterationRecords() {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->resetIterationRecords();
}


void CZoneList::prepareForDay() {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->prepareForDay();
}


void CZoneList::processDailyRecords( CSMDatabase* db, int iteration, int day ) {
  // DO check _focusCreated here.
  // Daily records should be stored only when zones have been created.
  if( false == _focusCreated )
    return;

  // Was the first zone focus just created?
  // If so, don't record zone outputs: there won't really be any until the next day.
  if( day == _firstZoneFocusDay )
    return;

  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->processDailyRecords( db, iteration, day );
}


void CZoneList::processIterationRecords( CSMDatabase* db, int iteration ) {
  // DO NOT check _focusCreated here.
  // Iteration records should have zone outputs, whether zones were created or not.

  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->processIterationRecords( db, iteration, focusCreated() );
}


void CZoneList::simComplete() {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->simComplete();
}


void CZoneList::setFocusCreatedOnDay( int day ) {
  _focusCreated = true;

  if( -1 == _firstZoneFocusDay )
    _firstZoneFocusDay = day;
}
//------------------------------------------------------------------------------



