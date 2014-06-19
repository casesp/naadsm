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
CZone::CZone( const int level, const int id, const QString& descr, CSMSimulation* sim ) {
  _level = level;
  _descr = descr;
  _id = id;

  _sim = sim;

  _area = 0.0;
  _maxArea = 0.0;
  _maxAreaDay = 0;

  _perimeter = 0.0;
  _maxPerimeter = 0.0;
  _maxAreaDay = 0;

  _herdDays = new QMap<int, int>();
  _animalDays = new QMap<int, int>();

  _herdCount = new QMap<int, int>();
  _animalCount = new QMap<int, int>();

  prepQueries( sim->database() );
}


CZone::~CZone() {
  delete _herdDays;
  delete _animalDays;
  delete _herdCount;
  delete _animalCount;

  freeQueries();
}

void CZone::prepQueries( CSMDatabase* db ) {
  qDebug() << "CZone::prepQueries()";

  qOutDailyByZoneAndProductionType = new QSqlQuery( *(db->database()) );
  qOutDailyByZoneAndProductionType->prepare(
    QString(
      "INSERT INTO %1.outDailyByZoneAndProductionType("
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
    ).arg( db->dbSchema() )
  );

  qOutDailyByZone = new QSqlQuery( *(db->database()) );
  qOutDailyByZone->prepare(
    QString(
      "INSERT INTO %1.outDailyByZone("
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
    ).arg( db->dbSchema() )
  );

  qOutIterationByZoneAndProductionType = new QSqlQuery( *(db->database()) );
  qOutIterationByZoneAndProductionType->prepare(
    QString(
      "INSERT INTO %1.outIterationByZoneAndProductionType("
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
    ).arg( db->dbSchema() )
  );


  qOutIterationByZone = new QSqlQuery( *(db->database()) );
  qOutIterationByZone->prepare(
    QString(
      "INSERT INTO %1.outIterationByZone("
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
    ).arg( db->dbSchema() )
  );
}


void CZone::freeQueries() {
  delete qOutDailyByZoneAndProductionType;
  delete qOutDailyByZone;
  delete qOutIterationByZoneAndProductionType;
  delete qOutIterationByZone;
}


void CZone::addToZoneTotals( int ptID, int herdSize ) {
  // Update running totals
  if( _herdDays->contains( ptID ) )
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
  _herdDays->clear();
  _animalDays->clear();

  _herdCount->clear();
  _animalCount->clear();

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
  _herdDays->clear();
  _animalDays->clear();

  _herdCount->clear();
  _animalCount->clear();
}
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// CZoneList
//------------------------------------------------------------------------------
CZoneList::CZoneList( ZON_zone_list_t* zones, CSMSimulation* sim ) : QMap<int, CZone*>() {
  ZON_zone_t* z;
  int i, nZones;
  CZone* cz;
  QSqlQuery* qInsertZone;

  qInsertZone = new QSqlQuery( *(sim->database()->database()) );
  qInsertZone->prepare(
    QString(
      "INSERT INTO %1.inZone ( zoneID, scenarioID, descr )"
      "VALUES ( :zoneid, :scenarioid, :descr )"
    ).arg( sim->database()->dbSchema() )
  );

  nZones = ZON_zone_list_length( zones );
  for( i = 0; i < nZones; ++i ) {
    z = ZON_zone_list_get( zones, i );

    // All zones except the background zone should have an ID.  If they don't, it's a problem.
    if( ( 0 != QString( "Background" ).compare( z->name ) ) && ( -1 == z->id ) ) {
      naadsmException( QString( "Bad zone XML." ) );
    }

    // Don't add the background zone to the list.  We ignore it.
    else if( 0 != QString( "Background" ).compare( z->name ) ) {
      cz = new CZone( z->level, z->id, QString( z->name ), sim );
      this->insert( z->level, cz );

      qInsertZone->bindValue( ":zoneid", cz->id() );
      qInsertZone->bindValue( ":scenarioid", sim->database()->scenarioID() );
      qInsertZone->bindValue( ":descr", cz->descr() );

      if( !qInsertZone->exec() ) {
        naadsmException(
          QString( "Database records could not be saved.  Query failed: " ).append( qInsertZone->lastQuery() ).append( " " ).append( qInsertZone->lastError().text() )
        );
      }
    }
  }

  delete qInsertZone;
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



