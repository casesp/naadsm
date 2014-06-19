// Begin 5/16/2014

#ifndef CSMSIMULATION_H
#define CSMSIMULATION_H

#include "herd.h"
#include "zone.h"

#include "csmdatabase.h"
#include "cprodtype.h"
#include "czone.h"

class CSMSimulation{
  public:
    CSMSimulation( HRD_herd_list_t* herds, ZON_zone_list_t* zones, CSMDatabase* db );
    ~CSMSimulation();

    void initializeAllOutputRecords();
    void prepareForIteration( int it );
    void prepareForDay();
    void processDailyRecords( CSMDatabase* db, int day );
    void processIterationRecords( CSMDatabase* db, int it );
    void simComplete();

    bool includeZonesGlobal();

    CZoneList* zoneList() { return _zoneList; }
    CProdTypeList* prodTypeList() { return _ptList; }

    CSMDatabase* database() { return _db; }

  protected:
    int _currentIteration;
    int _lastCompleteIteration;
    CSMDatabase* _db;

    CProdTypeList* _ptList;
    CZoneList* _zoneList;
};

#endif // CSMSIMULATION_H
