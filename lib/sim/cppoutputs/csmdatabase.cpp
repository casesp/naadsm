// Begin 5/16/2014

#include "csmdatabase.h"

#include <QDebug>
#include <QSqlQuery>
#include <QSqlError>
#include <QSqlRecord>

#include <ar_general_purpose/cencryption.h>
#include <ar_general_purpose/qcout.h>
#include <ar_general_purpose/strutils.h>

#include "naadsmlibrary.h"

const QString APP_PASSWORD_KEY = "ds098Lju3K24";


void appendToMessage( QString* message, QString newMessage ) {
  if( NULL != message )
    message->append( newMessage );
}


int CSMDatabase::processBlock( QStringList block ) {
  int result = ReturnCode::Success; // until shown otherwise

  QString line1 = block.takeFirst().toLower();
  if( 0 == line1.compare( "[database]") ) {
    processDatabaseBlock( block );

    if( isConfigured() && !isValid() ) {
      result = ReturnCode::BadDatabaseConfiguration;
    }
  }
  else if( 0 == line1.left(10).compare( "[scenario]" ) ) {
    processScenarioBlock( block );
  }
  else {
    // This is an an unrecognized/unsupported block.
    // The configuration file is bad.
    result = ReturnCode::BadConfiguration;
  }

  return result;
}


int CSMDatabase::processFile( QFile* file ) {
  // Read the contents of the file, line-by-line and block-by-block,
  // discarding any comments.
  // Once a block has been assembled, send it to the appropriate object
  // to be constructed.

  int result = ReturnCode::Success; // until shown otherwise

  QString line = "";
  QStringList block;
  QTextStream in(file);

  while (!in.atEnd()) {
    line = in.readLine().trimmed();

    if( line.isEmpty() || line.startsWith('#') )
      continue;
    else {
      line = line.left( line.indexOf('#') - 1 );

      // Are we starting a new block?
      if( line.startsWith('[') ) {
        if( block.isEmpty() ) {
          block.append( line );
        }
        else {
          result = processBlock( block );

          if( ReturnCode::Success == result ) {
            block.clear();
            block.append( line );
          }
          else {
            break;
          }
        }
      }
      else {
        block.append( line );
      }
    }
  }

  // Process the last block
  if( ReturnCode::Success == result )
    result = processBlock( block );

  return result;
}





CSMDatabase::CSMDatabase( const QString& specificationFileName ){
  QFile* file = NULL;

  initialize();

  // Until shown otherwise...
  _returnValue = ReturnCode::Success;
  _errorMessage = "";

  // Attempt to parse the file.
  file = new QFile( specificationFileName );

  if( !file->exists() )
    _returnValue =  ReturnCode::MissingConfigFile;
  else if (!file->open( QIODevice::ReadOnly | QIODevice::Text))
    _returnValue = ReturnCode::CannotOpenConfigFile;
  else
    _returnValue = processFile( file );

  delete file;

  // If the file was properly parsed, do a little more error checking.
  if( ReturnCode::Success == _returnValue ) {
    bool result = this->isConfigured();

    if( result ) {
      result = this->isOpen( &_errorMessage );
      if( !result ) {
        _returnValue = ReturnCode::BadDatabaseConfiguration;
      }
    }    
  }

  // Check the NAADSMy bits.
  if( ReturnCode::Success == _returnValue ) {
    if( !QFile::exists( _scenarioFileName ) || !QFile::exists( _herdsFileName ) )
      _returnValue = ReturnCode::BadNAADSMConfiguration;
    else if( _scenarioName.isEmpty() )
      _returnValue = ReturnCode::BadNAADSMConfiguration;
  }

  // If everything is OK, get ready to run the model.
  if( ReturnCode::Success == _returnValue ) {
    prepQueries();
    initiateScenario();
  }
}


CSMDatabase::~CSMDatabase() {
  freeQueries();

  //if( _dbIsOpen )
  //  QSqlDatabase::removeDatabase( "naadsmMainDb" );
}


void CSMDatabase::initialize() {
  _scenarioID = 0;
  _scenarioName = "";
  _scenarioDescr = "";
  _specificationFileName = "";
  _scenarioFileName = "";
  _herdsFileName = "";
  _rngSeed = -1;

  setDbHost( "" );
  setDbPort( 5432 );
  setDbName( "" );
  setDbUser( "" );
  setDbPassword( "" );
  setDbSchema( "" );

  _dbConfigured = false;
  _dbIsOpen = false;
}


QString CSMDatabase::processQuotes( const QString& val ) {
  enum State {Normal, Quote} state = Normal;
  QString temp, result;

  result = "";
  temp = val.trimmed();

  for (int i = 0; i < temp.size(); i++) {
    QChar current = temp.at(i);

    // Normal state
    if (state == Normal) {
      // One quote mark encountered.  Ignore commas until the matching quote mark is encountered.
      if (current == '"') {
        state = Quote;
      }
      // other character encountered
      else {
        result += current;
      }
    }
    // Quote
    else if (state == Quote) {
      // double quote
      if (current == '"') {
        // End of line
        if( ( temp.size() -1 ) == i ) {
          state = Normal;
        }
        else {
          int index = (i+1 < temp.size()) ? i+1 : temp.size();
          QChar next = temp.at(index);
          if (next == '"') {
            result += '"';
            i++;
          } else {
            state = Normal;
          }
        }
      }
      // other
      else {
        result += current;
      }
    }
  }

  return result;
}


void CSMDatabase::processScenarioBlock( QStringList scenarioBlock ) {
  int i;
  QStringList lineParts;
  QString key, val;

  for( i = 0; i < scenarioBlock.count(); ++i ) {
    lineParts.clear();
    lineParts = scenarioBlock.at(i).split( "<-" );

    if( 2 != lineParts.count() ) {
      initialize();
      _returnValue = ReturnCode::BadDatabaseConfiguration;
      return;
    }

    key = lineParts.at(0).trimmed().toLower();
    val = lineParts.at(1).trimmed();

    if( 0 == key.compare( "scenarioname" ) ) {
      _scenarioName = val;
    }
    else if( 0 == key.compare( "scenariodescr" ) ) {
      _scenarioDescr = processQuotes( val );
    }
    else if( 0 == key.compare( "scenariofile" ) ) {
      _scenarioFileName = val;
    }
    else if( 0 == key.compare( "herdsfile" ) ) {
      _herdsFileName = val;
    }
    else if( 0 == key.compare( "randomseed" ) ) {
      _rngSeed = val.toInt();
    }
    else {
      qDebug() << "Unsupported key value encountered in CConfigDatabase::CConfigDatabase:" << lineParts.at(0).trimmed();
      _returnValue = ReturnCode::BadConfiguration;
    }
  }
}


void CSMDatabase::processDatabaseBlock( QStringList configBlock ) {
  int i;
  QStringList lineParts;
  QString key, val;

  for( i = 0; i < configBlock.count(); ++i ) {
    lineParts.clear();
    lineParts = configBlock.at(i).split( "<-" );

    if( 2 != lineParts.count() ) {
      initialize();
      _returnValue = ReturnCode::BadConfiguration;
      return;
    }

    key = lineParts.at(0).trimmed().toLower();
    val = lineParts.at(1).trimmed();

    if( 0 == key.compare( "databasehost" ) ) {
      setDbHost( val );
    }
    else if( 0 == key.compare( "databaseport" ) ) {
      setDbPort( val.toInt() );
    }
    else if( 0 == key.compare( "databasename" ) ) {
      setDbName( val );
    }
    else if( 0 == key.compare( "databaseschemaname" ) ) {
      setDbSchema( val );
    }
    else if( 0 == key.compare( "databaseuser" ) ) {
      setDbUser( val );
    }
    else if( 0 == key.compare( "databasepassword" ) ) {
      setDbPassword( val );
    }
    else {    
      qDebug() << "Unsupported key value encountered in CConfigDatabase::CConfigDatabase:" << lineParts.at(0).trimmed();
      _returnValue = ReturnCode::BadConfiguration;
    }
  }

  _dbConfigured = true;

  openDatabase();
}


bool CSMDatabase::isValid( QString* errMsg /* = NULL */ ) {
  bool result = true; // Until shown otherwise.
  bool test;

  // The following fields are required for a valid database:
  test = !_dbHost.isEmpty();
  if( !test ) {
    appendToMessage( errMsg, "Databse host name is not specified.\n" );
    result = false;
  }

  test = !( 0 == _dbPort );
  if( !test ) {
    appendToMessage( errMsg, "Database port is not specified.\n" );
    result = false;
  }

  test = !_dbName.isEmpty();
  if( !test ) {
    appendToMessage( errMsg, "Database name is not specified.\n" );
    result = false;
  }

  test = !_dbUser.isEmpty();
  if( !test ) {
    appendToMessage( errMsg, "Database user name is not specified.\n" );
    result = false;
  }

  test = !_dbPassword.isEmpty();
  if( !test ) {
    appendToMessage( errMsg, "Database password is not specified.\n" );
    result = false;
  }

  // Schema name is currently optional.

  return result;
}


bool CSMDatabase::isOpen( QString* errMsg /* = NULL */ ) {
  bool result;

  // First see if all of the connection parameters have been set.
  // If they have, attempt to open a connection.  If this attempt is
  // successful, then the database is "working".
  result = isValid( errMsg );

  if( result )
    result = openDatabase();

  return result;
}


bool CSMDatabase::openDatabase() {
  bool result;

  if( _dbIsOpen )
    result = true;
  else {
    result = isValid();

    if( result ) {
      _db = QSqlDatabase::addDatabase( "QPSQL", "naadsmMainDb" );

      _db.setHostName( _dbHost );
      _db.setPort( _dbPort );
      _db.setDatabaseName( _dbName );
      _db.setUserName( _dbUser );
      _db.setPassword( CEncryption::rc4Encrypt( CEncryption::hexDecode( _dbPassword ), APP_PASSWORD_KEY ) );

      result = _db.open();
      _dbIsOpen = result;

     //if( !_dbIsOpen )
     //   QSqlDatabase::removeDatabase( "naadsmMainDb" );
    }
  }

  return result;
}


bool CSMDatabase::openSchema() {
  QString qstr;
  QSqlQuery* query = NULL;
  bool result = true; // until shown otherwise

  if( !_db.isOpen() )
    result = openDatabase();

  if( result && !this->dbSchema().isEmpty() ) {
    query = new QSqlQuery( _db );
    qstr = QString( "SET search_path TO \"%1\"" ).arg( this->dbSchema() );
    result = query->exec( qstr );
  }
  else {
    result = false;
  }

  delete query;
  return result;
}


void CSMDatabase::debug() {
  qDebug() << "Database configuration:";
  qDebug() << "  dbHost:" << _dbHost;
  qDebug() << "  dbPort:" << _dbPort;
  qDebug() << "  dbName:" << _dbName;
  qDebug() << "  dbUser:" << _dbUser;
  qDebug() << "  dbPassword:" << _dbPassword;
  qDebug() << "  dbSchema:" << _dbSchema;
  qDebug() << endl;
  qDebug() << "Database is configured:" << this->isConfigured();
  qDebug() << "Database is valid:" << this->isValid();
  qDebug() << "Database is open:" << this->isOpen();
  qDebug() << endl;
  qDebug() << "Scenario configuration:";
  qDebug() << "  scenarioID:" << _scenarioID;
  qDebug() << "  scenarioName:" << _scenarioName;
  qDebug() << "  scenarioDescr:" << _scenarioDescr;
  qDebug() << "  specificationFileName:" << _specificationFileName;
  qDebug() << "  scenarioFileName:" << _scenarioFileName;
  qDebug() << "  herdsFileName:" << _herdsFileName;
}


bool CSMDatabase::execute( const QString& query ) {
  bool result = true; // Until shown otherwise.

  _db.exec( query );

  if( !( QSqlError::NoError == _db.lastError().type() ) ) {
    naadsmException(  QString( "Query failed: " ).append( query ).append( " " ).append( _db.lastError().text() ) );
    result = false;
  }

  return result;
}


void CSMDatabase::initiateScenario() {
  qDebug() << "++ CSMDatabase::initiateScenario";

  qScenario->bindValue( ":name", this->_scenarioName );
  qScenario->bindValue( ":descr", this->_scenarioDescr );

  if( !qScenario->exec() ) {
    qDebug() << qScenario->lastQuery();

    naadsmException(
      QString( "Database records could not be saved.  Query failed: " ).append( qScenario->lastQuery() ).append( " " ).append( qScenario->lastError().text() )
    );

    _returnValue = ReturnCode::BadDatabaseQuery;
  }
  else {
    _scenarioID = qScenario->lastInsertId().toInt();
    _returnValue = ReturnCode::Success;
  }
}


void CSMDatabase::prepQueries() {
  qDebug() << "CSMDatabase::prepQueries()";

  qScenario = new QSqlQuery( _db );
  qScenario->prepare(
    QString( "INSERT INTO %1.inScenario ( name, descr ) VALUES ( :name, :descr )" ).arg( this->dbSchema() )
  );

  qOutIteration = new QSqlQuery( _db );
  qOutIteration->prepare(
    QString(
      "INSERT INTO %1.outIteration ( scenarioID, threadNo, iteration, outbreakEnded, outbreakEndDay, diseaseEnded, diseaseEndDay, zoneFociCreated )"
      " VALUES ( :scenarioID, :threadNo, :iteration, :outbreakEnded, :outbreakEndDay, :diseaseEnded, :diseaseEndDay, :zoneFociCreated )"
    ).arg( this->dbSchema() )
  );

  qOutEpidemicCurves = new QSqlQuery( _db );
  qOutEpidemicCurves->prepare(
    QString(
      "INSERT INTO %1.outEpidemicCurves ( scenarioID, threadNo, iteration, day, productionTypeID, infectedUnits, infectedAnimals, detectedUnits, detectedAnimals, infectiousUnits, apparentInfectiousUnits )"
      " VALUES ( :scenarioID, :threadNo, :iteration, :day, :productionTypeID, :infectedUnits, :infectedAnimals, :detectedUnits, :detectedAnimals, :infectiousUnits, :apparentInfectiousUnits )"
    ).arg( this->dbSchema() )
  );
}


void CSMDatabase::freeQueries() {
  delete qScenario;
  delete qOutIteration;
  delete qOutEpidemicCurves;
}


void CSMDatabase::initializeAllOutputRecords() {  
  execute( QString( "DELETE FROM %3.outGeneral WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );

  execute( QString( "DELETE FROM %3.outDailyByProductionType WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %3.outDailyByZone WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %3.outDailyByZoneAndProductionType WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );

  execute( QString( "DELETE FROM %3.outIteration WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %3.outIterationByProductionType WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %3.outIterationByZone WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %3.outIterationByZoneAndProductionType WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );

  execute( QString( "DELETE FROM %3.outEpidemicCurves WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );

  execute( QString( "INSERT INTO %3.outGeneral( scenarioID, threadNo, completedIterations, outGeneralID ) VALUES( %1, %2, 0, 'NAADSMXXXX' )" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
}


void CSMDatabase::prepareForIteration( const int iteration ) {
  int leaveIterations = 3;
  int iterationsToDelete = iteration - leaveIterations;

  execute( QString( "DELETE FROM %4.outDailyByProductionType WHERE iteration < %3 AND scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( iterationsToDelete ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %4.outDailyByZone WHERE iteration < %3 AND scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( iterationsToDelete ).arg( this->dbSchema() ) );
  execute( QString( "DELETE FROM %4.outDailyByZoneAndProductionType WHERE iteration < %3 AND scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( iterationsToDelete ).arg( this->dbSchema() ) );
}


void CSMDatabase::recordStartTime() {
  execute(
    QString( "UPDATE %4.outGeneral SET simulationStartTime = NOW(), version = '%3' WHERE scenarioID = %1 AND threadNo = %2" )
    .arg( this->scenarioID() )
    .arg( THREAD_NUMBER )
    .arg( APP_VERSION )
    .arg( this->dbSchema() )
   );
}


void CSMDatabase::incrementCompleteIterations() {
  execute( QString( "UPDATE %3.outGeneral SET completedIterations = completedIterations + 1 WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
}


void CSMDatabase::recordEndTime() {
  execute( QString( "UPDATE %3.outGeneral SET simulationEndTime = NOW() WHERE scenarioID = %1 AND threadNo = %2" ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( this->dbSchema() ) );
}


void CSMDatabase::simComplete() {
  // Nothing to do here.
}


void CSMDatabase::processIterationRecords(
  const int iteration,
  const int outbreakEndDay,
  const bool outbreakEnded,
  const int diseaseEndDay,
  const bool diseaseEnded,
  const bool zoneFociCreated
) {
  // Add an iteration record to the database, with the following information:
  //-------------------------------------------------------------------------
  qOutIteration->bindValue( ":outbreakEnded", outbreakEnded );
  if( outbreakEnded )
    qOutIteration->bindValue( ":outbreakEndDay", outbreakEndDay );
  else
    qOutIteration->bindValue( ":outbreakEndDay", QVariant( QVariant::Int ) );

  qOutIteration->bindValue( ":diseaseEnded", diseaseEnded );
  if( diseaseEnded )
    qOutIteration->bindValue( ":diseaseEndDay", diseaseEndDay );
  else
    qOutIteration->bindValue( ":diseaseEndDay", QVariant( QVariant::Int ) );

  qOutIteration->bindValue( ":zoneFociCreated", zoneFociCreated );
  qOutIteration->bindValue( ":scenarioID", this->scenarioID() );
  qOutIteration->bindValue( ":threadNo", THREAD_NUMBER );
  qOutIteration->bindValue( ":iteration", iteration );

  if( !qOutIteration->exec() ) {
    naadsmException(
      QString( "Database records could not be saved.  Query failed: " ).append( qOutIteration->lastQuery() ).append( " " ).append( qOutIteration->lastError().text() )
    );
  }

  // Populate outEpidemicCurves for the THE DAY PRIOR TO THE START of the iteration
  // (Actual iteration days won't include units/animals initially infected)
  //-------------------------------------------------------------------------------
  QString q = QString(
    "SELECT"
    " infcUIni,"
    " infcAIni,"
    " tsdUSubc + tsdUClin AS infectiousUnits, "
    " productionTypeID"
    " FROM %4.outDailyByProductionType"
    " WHERE scenarioID = %1 AND threadNo = %2 AND iteration = %3"
    " AND day = 1"
  ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( iteration ).arg( this->dbSchema() );

  QSqlQuery* sqlResult = new QSqlQuery( q, _db );
  QSqlRecord row;

  while( sqlResult->next() ){
    row = sqlResult->record();

    qOutEpidemicCurves->bindValue( ":scenarioID", this->scenarioID() );
    qOutEpidemicCurves->bindValue( ":threadNo", THREAD_NUMBER );
    qOutEpidemicCurves->bindValue( ":iteration", iteration );
    qOutEpidemicCurves->bindValue( ":day", 0 ); // Day = 0 before the simulation starts
    qOutEpidemicCurves->bindValue( ":productionTypeID", row.value( "productionTypeID" ) );
    qOutEpidemicCurves->bindValue( ":infectedUnits", row.value( "infcUIni" ) );
    qOutEpidemicCurves->bindValue( ":infectedAnimals", row.value( "infcAIni" ) );
    qOutEpidemicCurves->bindValue( ":detectedUnits", 0 ); // Detected units: none
    qOutEpidemicCurves->bindValue( ":detectedAnimals", 0 ); // Detected animals: none
    qOutEpidemicCurves->bindValue( ":infectiousUnits", row.value( "infectiousUnits" ) );
    qOutEpidemicCurves->bindValue( ":apparentInfectiousUnits", 0 ); // Apparently infectious units: none

    if( !qOutEpidemicCurves->exec() ) {
      naadsmException(
        QString( "Database records could not be saved.  Query failed: " ).append( qOutEpidemicCurves->lastQuery() ).append( " " ).append( qOutEpidemicCurves->lastError().text() )
      );
    }
  }

  delete sqlResult;

  // Populate outEpidemicCurves for ALL OTHER DAYS of the iteration
  //---------------------------------------------------------------
  q = QString(
      "INSERT INTO %4.outEpidemicCurves ("
        " iteration, day, productionTypeID, scenarioID, threadNo,"
        " infectedUnits,"
        " infectedAnimals,"
        " detectedUnits,"
        " detectedAnimals,"
        " infectiousUnits,"
        " apparentInfectiousUnits"
      " )"
      " SELECT"
        " iteration, day, productionTypeID, scenarioID, threadNo,"
        " infnUDir + infnUInd + infnUAir,"
        " infnADir + infnAInd + infnAAir,"
        " detnUClin + detnUTest,"
        " detnAClin + detnATest,"
        " tsdUSubc + tsdUClin,"
        " appdUInfectious"
      " FROM %4.outDailyByProductionType"
      " WHERE "
        " scenarioID = %1 AND threadNo = %2 "
        " AND iteration = %3"
  ).arg( this->scenarioID() ).arg( THREAD_NUMBER ).arg( iteration ).arg( this->dbSchema() );

  execute( q );

  incrementCompleteIterations();
}


