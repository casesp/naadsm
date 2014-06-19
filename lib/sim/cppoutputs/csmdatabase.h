// Begin 5/16/2014

#ifndef CSMDATABASE_H
#define CSMDATABASE_H

#include <QtCore>
#include <QSqlDatabase>

const int THREAD_NUMBER = 1;
const QString APP_VERSION = "3.3.2"; // FIXME: Replace this with something more appropriate.

class CSMDatabase {
  public:
    CSMDatabase( const QString& specificationFileName );
    ~CSMDatabase();

    // NAADSMy bits
    void initializeAllOutputRecords();
    void prepareForIteration( const int iteration );
    void recordStartTime();
    void incrementCompleteIterations();
    void recordEndTime();
    void simComplete();

    void processIterationRecords(
      const int iteration,
      const int outbreakEndDay,
      const bool outbreakEnded,
      const int diseaseEndDay,
      const bool diseaseEnded,
      const bool zoneFociCreated
    );

    // Database operations
    bool execute( const QString& query );

    // Configuration settings
    int scenarioID() { return _scenarioID; }
    QString herdsFile() { return _herdsFileName; }
    QString scenarioFile() { return _scenarioFileName; }
    int rngSeed() { return _rngSeed; }

    // Database configuration
    void debug();

    void setDbHost( QString val ) { _dbHost = val; }
    void setDbPort( int val ) { _dbPort = val; }
    void setDbName( QString val ) { _dbName = val; }
    void setDbUser( QString val ) { _dbUser = val; }
    void setDbPassword( QString val ) { _dbPassword = val; }
    void setDbSchema( QString val ) { _dbSchema = val; }

    QString dbHost( void ) { return _dbHost; }
    int dbPort( void ) { return _dbPort; }
    QString dbName( void ) { return _dbName; }
    QString dbUser( void ) { return _dbUser; }
    QString dbPassword( void ) { return _dbPassword; }
    QString dbSchema( void ) { return _dbSchema; }

    bool openDatabase();
    bool openSchema();
    QSqlDatabase* database() { return &_db; }

    bool isConfigured() { return _dbConfigured; }
    bool isValid( QString* errMsg = NULL );
    bool isOpen( QString* errMsg = NULL );

    int returnValue() { return _returnValue; }

  protected:
    void initialize();
    int processFile( QFile* file );
    int processBlock( QStringList block );
    void processScenarioBlock( QStringList scenarioBlock );
    void processDatabaseBlock( QStringList configBlock );
    QString processQuotes( const QString& val );

    void prepQueries();
    void freeQueries();
    QSqlQuery* qScenario;
    QSqlQuery* qOutIteration;
    QSqlQuery* qOutEpidemicCurves;

    void initiateScenario();

    // NAADSMy bits
    int _scenarioID;
    QString _scenarioName;
    QString _scenarioDescr;
    QString _specificationFileName;
    QString _scenarioFileName;
    QString _herdsFileName;
    int _rngSeed;

    // Databasey bits
    QSqlDatabase _db;

    bool _dbConfigured;
    bool _dbIsOpen;

    QString _dbHost;
    int _dbPort;
    QString _dbName;
    QString _dbUser;
    QString _dbPassword;
    QString _dbSchema;

    QString _errorMessage;
    int _returnValue;
};


#endif // CSMDATABASE_H
