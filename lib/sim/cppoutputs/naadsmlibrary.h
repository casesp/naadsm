// Begin 5/16/2014

#ifndef CPP_OUTPUTS_FOR_NAADSM_H
#define CPP_OUTPUTS_FOR_NAADSM_H

#include <naadsm.h>
#include "rng.h"

#include "csmdatabase.h"
#include "csmsimulation.h"


// For key simulation- and iteration-level events
void naadsmSetRng( RAN_gen_t* rng );
void naadsmSimStart();
void naadsmIterationStart( int it );
void naadsmDayStart( int day );
void naadsmDayComplete( int day );
void naadsmDiseaseEnd( int val );
void naadsmOutbreakEnd( int val );
void naadsmIterationComplete( int it );
void naadsmSimComplete( int val );

// Used to update herd status and related events as an iteration runs
void naadsmChangeHerdState( HRD_update_t c );
void naadsmInfectHerd( HRD_infect_t c );
void naadsmDetectHerd( HRD_detect_t d );
void naadsmExposeHerd( HRD_expose_t e );
void naadsmTraceHerd( HRD_trace_t t );
void naadsmExamineHerd( HRD_exam_t e );
void naadsmTestHerd( HRD_test_t t );
void naadsmQueueHerdForDestruction( int herdIdx );
void naadsmDestroyHerd( HRD_control_t c );
void naadsmQueueHerdForVaccination( int herdIdx );
void naadsmVaccinateHerd( HRD_control_t c );
void naadsmCancelHerdVaccination( HRD_control_t c );
void naadsmMakeZoneFocus( int herdIdx );
void naadsmRecordZoneChange( HRD_zone_t z );

// Recording zone outputs
void naadsmRecordZonePerimeter( int zoneLevel, double perim );
void naadsmRecordZoneArea( int zoneLevel, double area );

/*
  A class needed for detection events in NAADSM 3.  See comments associated with NAADSMLibrary.naadsm_detect_herd.
   This class will not exist in NAADSM 4!
*/
class CNAADSM3Detect {
  public:
   CNAADSM3Detect( HRD_detect_t d );
   ~CNAADSM3Detect();

    unsigned int herdIndex;
    NAADSM_detection_reason reason;
    NAADSM_test_result testResult;
};


// Variables used for the running simulation
//CSMSimulation* _smSim;
//CSMDatabase* _smdb;

// Additional variables used for the running simulation
// (Declared in the implementation section, but
// listed here for easy reference.)
//---------------------------------------------
/*
_diseaseEndDay: integer;
_outbreakEnd: integer;
_simDay: integer;
_simIteration: integer;

_rngSeed: integer;

_eventCounter: integer;
_eventList: TSMEventList;

_exposureCounter: integer;
_exposureList: TSMExposureOrTraceList;

_badThingsList: TQStringList;

_naadsMap: TNAADSMap;

_herdsInZones: TQIntegerObjectMap;

// See comment for naadsm_detect_herd.  This will not be used in NAADSM 4!
_naadsm3Detections: TQIntegerObjectMap;
*/

#endif // CPP_OUTPUTS_FOR_NAADSM_H
