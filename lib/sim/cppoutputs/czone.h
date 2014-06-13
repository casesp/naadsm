// Begin 5/16/2014

#ifndef CZONE_H
#define CZONE_H

#include <QtCore>
#include <QSqlQuery>

#include "zone.h"

#include "csmdatabase.h"

class CSMSimulation;

class CZone {
  public:
    CZone( const int level, const QString& descr, CSMSimulation* sim );
    ~CZone();

    void addToZoneTotals( int ptID, int herdSize );
    void setArea( double val, int day );
    void setPerimeter( double val, int day );

    void clearAllRecords();
    void resetIterationRecords();
    void prepareForDay();
    void processDailyRecords( CSMDatabase* db, int iteration, int day );
    void processIterationRecords( CSMDatabase* db, int iteration, bool focusCreated );
    void simComplete();

    int id() { return _id; }
    int zoneLevel() { return _level; }
    QString descr() { return _descr; }

  protected:
    void freeAndNullMaps();
    void prepQueries();
    void freeQueries();

    QMap<int, int>* _herdDays;
    QMap<int, int>* _animalDays;
    QMap<int, int>* _herdCount;
    QMap<int, int>* _animalCount;

    QSqlQuery* qOutDailyByZoneAndProductionType;
    QSqlQuery* qOutDailyByZone;
    QSqlQuery* qOutIterationByZoneAndProductionType;
    QSqlQuery* qOutIterationByZone;

    CSMSimulation* _sim;
    int _id; //FIXME: Remember to set this when creating the entries in the database!
    int _level;
    QString _descr;

    double _area;
    double _maxArea;
    int _maxAreaDay;

    double _perimeter;
    double _maxPerimeter;
    int _maxPerimeterDay;
};


// Key is the zone's level
class CZoneList : public QMap<int, CZone*> {
  public:
    CZoneList( ZON_zone_list_t* zones, CSMSimulation* sim );
    ~CZoneList();

    void clearAllRecords();
    void resetIterationRecords();
    void prepareForDay();
    void processDailyRecords( CSMDatabase* db, int currentIteration, int day );
    void processIterationRecords( CSMDatabase* db, int it );
    void simComplete();

    void setFocusCreatedOnDay( int day );

    bool focusCreated() { return _focusCreated; }

  protected:
    int _firstZoneFocusDay;
    bool _focusCreated;
};

#endif // CZONE_H
