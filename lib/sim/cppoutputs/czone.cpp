// Begin 5/14/2014

#include "czone.h"

#include <QDebug>
#include <QSqlError>
#include <QSqlRecord>

#include "naadsmlibrary.h"
#include "csmsimulation.h"

//------------------------------------------------------------------------------
// CZone
//------------------------------------------------------------------------------
CZone::CZone( const int level, const QString& descr, CSMSimulation* sim ) {
  _level = level;
  _descr = descr;
  _id = -1;

  _sim = sim;

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

  prepQueries();
}


CZone::~CZone() {
  freeAndNullMaps();
  freeQueries();
}

void CZone::prepQueries() {
  qOutDailyByZoneAndProductionType = new QSqlQuery( QSqlDatabase::database() );
  qOutDailyByZoneAndProductionType->prepare(
    QString(
      "INSERT INTO outDailyByZoneAndProductionType("
        " scenarioID,"
        " threadNo,"
        " iteration,"
        " day,"
        " zoneID,"
        " productionTypeID,"
        " unitDaysInZone,"
        " animalDaysInZone,"
        " unitsInZone,"
        " animalsInZone"
      " )"
      " VALUES("
        " :scenarioID,"
        " :threadNo,"
        " :iteration,"
        " :day,"
        " :zoneID,"
        " :productionTypeID,"
        " :unitDaysInZone,"
        " :animalDaysInZone,"
        " :unitsInZone,"
        " :animalsInZone"
      " )"
    )
  );

  qOutDailyByZone = new QSqlQuery( QSqlDatabase::database() );
  qOutDailyByZone->prepare(
    QString(
      "INSERT INTO outDailyByZone("
        " scenarioID,"
        " threadNo,"
        " iteration,"
        " day,"
        " zoneID,"
        " zoneArea,"
        " zonePerimeter"
      " )"
      " VALUES("
        " :scenarioID,"
        " :threadNo,"
        " :iteration,"
        " :day,"
        " :zoneID,"
        " :zoneArea,"
        " :zonePerimeter"
      " )"
    )
  );

  qOutIterationByZoneAndProductionType = new QSqlQuery( QSqlDatabase::database() );
  qOutIterationByZoneAndProductionType->prepare(
    QString(
      "INSERT INTO outIterationByZoneAndProductionType("
        " scenarioID,"
        " threadNo,"
        " iteration,"
        " zoneID,"
        " productionTypeID,"
        " unitDaysInZone,"
        " animalDaysInZone"
      " )"
      " VALUES("
        " :scenarioID,"
        " :threadNo,"
        " :iteration,"
        " :zoneID,"
        " :productionTypeID,"
        " :unitDaysInZone,"
        " :animalDaysInZone"
      " )"
    )
  );


  qOutIterationByZone = new QSqlQuery( QSqlDatabase::database() );
  qOutIterationByZone->prepare(
    QString(
      "INSERT INTO outIterationByZone("
        " scenarioID,"
        " threadNo,"
        " iteration,"
        " zoneID,"
        " finalZoneArea,"
        " maxZoneArea,"
        " maxZoneAreaDay,"
        " finalZonePerimeter,"
        " maxZonePerimeter,"
        " maxZonePerimeterDay"
      " )"
      " VALUES("
        " :scenarioID,"
        " :threadNo,"
        " :iteration,"
        " :zoneID,"
        " :finalZoneArea,"
        " :maxZoneArea,"
        " :maxZoneAreaDay,"
        " :finalZonePerimeter,"
        " :maxZonePerimeter,"
        " :maxZonePerimeterDay"
      " )"
    )
  );
}


void CZone::freeQueries() {
  delete qOutDailyByZoneAndProductionType;
  delete qOutDailyByZone;
  delete qOutIterationByZoneAndProductionType;
  delete qOutIterationByZone;
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


void CZone::clearAllRecords() {
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
  int prodTypeID;
  CProdTypeList* ptList;

  ptList = _sim->prodTypeList();
  QMapIterator<QString, CProdType*> it(*ptList);
  while( it.hasNext() ) {
    prodTypeID = it.next().value()->id();

    qOutDailyByZoneAndProductionType->bindValue( ":scenarioID", db->scenarioID() );
    qOutDailyByZoneAndProductionType->bindValue( ":threadNo", THREAD_NUMBER );
    qOutDailyByZoneAndProductionType->bindValue( ":iteration", iteration );
    qOutDailyByZoneAndProductionType->bindValue( ":day", day );
    qOutDailyByZoneAndProductionType->bindValue( ":zoneID", this->id() );
    qOutDailyByZoneAndProductionType->bindValue( ":productionTypeID", prodTypeID );

    if( _herdDays->contains( prodTypeID ) ) {
      qOutDailyByZoneAndProductionType->bindValue( ":unitDaysInZone", _herdDays->value( prodTypeID ) );
      qOutDailyByZoneAndProductionType->bindValue( ":animalDaysInZone", _animalDays->value( prodTypeID ) );
      qOutDailyByZoneAndProductionType->bindValue( ":unitsInZone", _herdCount->value( prodTypeID ) );
      qOutDailyByZoneAndProductionType->bindValue( ":animalsInZone", _animalCount->value( prodTypeID ) );
    }
    else {
      qOutDailyByZoneAndProductionType->bindValue( ":unitDaysInZone", 0 );
      qOutDailyByZoneAndProductionType->bindValue( ":animalDaysInZone", 0 );
      qOutDailyByZoneAndProductionType->bindValue( ":unitsInZone", 0 );
      qOutDailyByZoneAndProductionType->bindValue( ":animalsInZone", 0 );
    }

    if( !qOutDailyByZoneAndProductionType->exec() ) {
      naadsmException(
        QString( "Database records could not be saved.  Query failed: " ).append( qOutDailyByZoneAndProductionType->lastQuery() ).append( " " ).append( qOutDailyByZoneAndProductionType->lastError().text() )
      );
    }
  }

  qOutDailyByZone->bindValue( ":scenarioID", db->scenarioID() );
  qOutDailyByZone->bindValue( ":threadNo", THREAD_NUMBER );
  qOutDailyByZone->bindValue( ":iteration", iteration );
  qOutDailyByZone->bindValue( ":day", day );
  qOutDailyByZone->bindValue( ":zoneID", this->id() );
  qOutDailyByZone->bindValue( ":zoneArea", this->_area );
  qOutDailyByZone->bindValue( ":zonePerimeter", this->_perimeter );

  if( !qOutDailyByZone->exec() ) {
    naadsmException(
      QString( "Database records could not be saved.  Query failed: " ).append( qOutDailyByZone->lastQuery() ).append( " " ).append( qOutDailyByZone->lastError().text() )
    );
  }
}


void CZone::processIterationRecords( CSMDatabase* db, int iteration, bool focusCreated ) {
  int prodTypeID;
  CProdTypeList* ptList;

  ptList = _sim->prodTypeList();
  QMapIterator<QString, CProdType*> it(*ptList);
  while( it.hasNext() ) {
    prodTypeID = it.next().value()->id();

    qOutIterationByZoneAndProductionType->bindValue( ":scenarioID", db->scenarioID() );
    qOutIterationByZoneAndProductionType->bindValue( ":threadNo", THREAD_NUMBER );
    qOutIterationByZoneAndProductionType->bindValue( ":iteration", iteration );
    qOutIterationByZoneAndProductionType->bindValue( ":zoneID", this->id() );
    qOutIterationByZoneAndProductionType->bindValue( ":productionTypeID", prodTypeID );

    if( _herdDays->contains( prodTypeID ) ) {
      qOutIterationByZoneAndProductionType->bindValue( ":unitDaysInZone", _herdDays->value( prodTypeID ) );
      qOutIterationByZoneAndProductionType->bindValue( ":animalDaysInZone", _animalDays->value( prodTypeID ) );
    }
    else {
      qOutIterationByZoneAndProductionType->bindValue( ":unitDaysInZone", 0 );
      qOutIterationByZoneAndProductionType->bindValue( ":animalDaysInZone", 0 );
    }

    if( !qOutIterationByZoneAndProductionType->exec() ) {
      naadsmException(
        QString( "Database records could not be saved.  Query failed: " ).append( qOutIterationByZoneAndProductionType->lastQuery() ).append( " " ).append( qOutIterationByZoneAndProductionType->lastError().text() )
      );
    }
  }

  qOutIterationByZone->bindValue( ":scenarioID", db->scenarioID() );
  qOutIterationByZone->bindValue( ":threadNo", THREAD_NUMBER );
  qOutIterationByZone->bindValue( ":iteration", iteration );
  qOutIterationByZone->bindValue( ":zoneID", this->id() );

  if( focusCreated ) {
    qOutIterationByZone->bindValue( ":finalZoneArea", this->_area );
    qOutIterationByZone->bindValue( ":maxZoneArea", this->_maxArea );
    qOutIterationByZone->bindValue( ":maxZoneAreaDay", this->_maxAreaDay  );
    qOutIterationByZone->bindValue( ":finalZonePerimeter", this->_perimeter  );
    qOutIterationByZone->bindValue( ":maxZonePerimeter", this->_maxPerimeter );
    qOutIterationByZone->bindValue( ":maxZonePerimeterDay", this->_maxPerimeterDay );
  }
  else {
    qOutIterationByZone->bindValue( ":finalZoneArea", QVariant( QVariant::Double ) );
    qOutIterationByZone->bindValue( ":maxZoneArea", QVariant( QVariant::Double ) );
    qOutIterationByZone->bindValue( ":maxZoneAreaDay", QVariant( QVariant::Int ) );
    qOutIterationByZone->bindValue( ":finalZonePerimeter", QVariant( QVariant::Double ) );
    qOutIterationByZone->bindValue( ":maxZonePerimeter", QVariant( QVariant::Double ) );
    qOutIterationByZone->bindValue( ":maxZonePerimeterDay", QVariant( QVariant::Int ) );
  }

  if( !qOutIterationByZone->exec() ) {
    naadsmException(
      QString( "Database records could not be saved.  Query failed: " ).append( qOutIterationByZone->lastQuery() ).append( " " ).append( qOutIterationByZone->lastError().text() )
    );
  }
}


void CZone::simComplete(){
  freeAndNullMaps();
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// CZoneList
//------------------------------------------------------------------------------
CZoneList::CZoneList( ZON_zone_list_t* zones, CSMSimulation* sim ) : QMap<int, CZone*>() {
  ZON_zone_t* z;
  int i, nZones;
  CZone* cz;

  nZones = ZON_zone_list_length( zones );
  for( i = 0; i < nZones; ++i ) {
    z = ZON_zone_list_get( zones, i );
    cz = new CZone( z->level, QString( z->name ), sim );
    this->insert( z->level, cz );
  }
}


CZoneList::~CZoneList() {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    delete it.next().value();
}


void CZoneList::clearAllRecords() {
  QMapIterator<int, CZone*> it( *this );

  while( it.hasNext() )
    it.next().value()->clearAllRecords();
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



