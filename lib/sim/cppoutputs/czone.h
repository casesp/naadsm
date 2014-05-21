// Begin 5/16/2014

#ifndef CZONE_H
#define CZONE_H

#include <QtCore>

#include "csmdatabase.h"

class CZone {
  public:
    CZone();
    ~CZone();

    void addToZoneTotals( int ptID, int herdSize );
    void setArea( double val, int day );
    void setPerimeter( double val, int day );

    void clearAllRecords( CSMDatabase* db );
    void resetIterationRecords();
    void prepareForDay();
    void processDailyRecords( CSMDatabase* db, int iteration, int day );
    void processIterationRecords( CSMDatabase* db, int iteration, bool focusCreated );
    void simComplete();

    int zoneLevel() { return _level; }
    QString descr() { return _descr; }

  protected:
    void freeAndNullMaps();

    QMap<int, int>* _herdDays;
    QMap<int, int>* _animalDays;
    QMap<int, int>* _herdCount;
    QMap<int, int>* _animalCount;

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
    CZoneList();
    ~CZoneList();

    void clearAllRecords( CSMDatabase* db );
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
