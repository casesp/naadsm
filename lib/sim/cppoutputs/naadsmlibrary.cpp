// Begin 5/16/2014


#include <QDebug>


#include "naadsmlibrary.h"

#include "rng.h"
#include "prob_dist.h"
#include "herd.h"


// Globals defined in header
//CSMSimulation* _smSim;
//CHerdList* _herds;
//CSMDatabase* _smdb;

// Globals defined and used here
CSMSimulation* _smSim;
//CHerdList* _herds;
CSMDatabase* _smdb;

RAN_gen_t* _rng;

int _diseaseEndDay;
int _outbreakEnd;
int _simDay;
int _simIteration;

int _rngSeed;

int _eventCounter;
//_eventList: TSMEventList;

int _exposureCounter;
//_exposureList: TSMExposureOrTraceList;


QMap<int, HRD_herd_t*>* _herdsInZones;

// See comment for naadsm_detect_herd.  This will not be used in NAADSM 4!
QMap<int, QList<CNAADSM3Detect*>* >* _naadsm3Detections;


//-----------------------------------------------------------------------------
// Used for detection in NAADSM 3.
//-----------------------------------------------------------------------------
CNAADSM3Detect::CNAADSM3Detect( HRD_detect_t d ){
  herdIndex = d.herd_index;
  reason = d.reason;
  testResult = d.test_result;
}


CNAADSM3Detect::~CNAADSM3Detect() {
  // Nothing to do here.
}


void naadsmException( const QString& errMsg ) {
  qDebug() << errMsg;
  qFatal( "Died." );
}


void naadsm3ProcessDetections(){
  //CHerd* h;
  QList<CNAADSM3Detect*>* detections;
  QMapIterator<int, QList<CNAADSM3Detect*>* > mit( *_naadsm3Detections );
  CNAADSM3Detect* det;
  HRD_detect_t d;
  int detIdx;

  // For each detected herd...
  //--------------------------
  while( mit.hasNext() ) {
    detections = mit.next().value();

    // Did multiple detection events occur?  If so, choose one at random
    //------------------------------------------------------------------
    if( 1 == detections->count() )
      detIdx = 0;
    else
      detIdx = RAN_int( _rng, 0, detections->count() - 1 );

    det = detections->at( detIdx );

    d.herd_index = det->herdIndex;
    d.reason = det->reason;
    d.test_result = det->testResult;

    // Record the detection
    //--------------------QMap-
    //qDebug() << "NAADSM: Herd with index" << d.herdIndex" << "will really be detected";

    _smSim->prodTypeList()->detectHerd( d, _simDay );
  }

  // Once all detections have been processed, clear out the map for later use
  //-------------------------------------------------------------------------
  mit.toFront();
  while( mit.hasNext() ) {
    detections = mit.next().value();

    while( !detections->isEmpty() )
      delete detections->takeLast();

    delete detections;
  }
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// For key simulation- and iteration-level events
//-----------------------------------------------------------------------------
void naadsmSetRng( RAN_gen_t* rng ) {
  _rng = rng;
}


void naadsmCppInitialize( HRD_herd_list_t* herds, ZON_zone_list_t* zones, char* dbSpecFileName ) {
  _smdb = new CSMDatabase( QString( dbSpecFileName ) );
  _smSim = new CSMSimulation( herds, zones, _smdb );
}


void naadsmCppFinalize() {
  delete _smSim;
  delete _smdb;
}


void naadsmSimStart() {
  _smdb->initializeAllOutputRecords();
  _smSim->initializeAllOutputRecords();

  _simIteration = -1;
  _simDay = -1;
  _diseaseEndDay = -1;
  _outbreakEnd = -1;

  _herdsInZones = new QMap<int, HRD_herd_t*>();
}


void naadsmIterationStart( int it ) {
  _smdb->prepareForIteration( it + 1 );
  _smSim->prepareForIteration( it + 1 );

  _simIteration = it + 1;
  _simDay = -1;
  _diseaseEndDay = -1;
  _outbreakEnd = -1;

  _herdsInZones->clear();
}


void naadsmDayStart( int day ) {
  _smSim->prepareForDay();

  _simDay = day;
}


void naadsmDayComplete( int day ) {
  naadsm3ProcessDetections();

  // Update the number of herds and animals of each production type in each zone
  QMapIterator<int, HRD_herd_t*> it( *_herdsInZones );
  while( it.hasNext() ) {
    HRD_herd_t* h = it.next().value();
    CZone* z = _smSim->zoneList()->value( h->zone_level );
    CProdType* pt = _smSim->prodTypeList()->value( h->production_type_name );
    z->addToZoneTotals( pt->id(), h->size );
  }

  _smSim->processDailyRecords( _smdb, day );
}


void naadsmDiseaseEnd( int val ) {
  _diseaseEndDay = val;
}


void naadsmOutbreakEnd( int val ) {
  _outbreakEnd = val;
}


void naadsmIterationComplete( int it ) {

  // Record the iteration as complete
  _smdb->processIterationRecords(
    it+1,
    _simDay,
    ( -1 != _outbreakEnd ),
    _diseaseEndDay,
    ( -1 != _diseaseEndDay ),
    _smSim->zoneList()->focusCreated()
  );

  _smSim->processIterationRecords( _smdb, it+1 );
}


void naadsmSimComplete( int val ) {
  if( 0 != val )
    _smdb->recordEndTime();

  _herdsInZones->clear();
  // Do not delete items in _herdsInZones: it does not own them.
  delete _herdsInZones;

  _smSim->simComplete();
  _smdb->simComplete();
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Used to update herd status and related events as an iteration runs
//-----------------------------------------------------------------------------
void naadsmChangeHerdState( HRD_update_t c ) {
  _smSim->prodTypeList()->changeHerdState( c, _simDay );
}

void naadsmInfectHerd( HRD_infect_t r ) {
  _smSim->prodTypeList()->infectHerd( r, _simDay );
}

void naadsmDetectHerd( HRD_detect_t d ) {
  CNAADSM3Detect* det;
  QList<CNAADSM3Detect*>* detections;

  if( _naadsm3Detections->contains( d.herd_index) )
    detections = _naadsm3Detections->value( d.herd_index );
  else {
    detections = new  QList<CNAADSM3Detect*>();
    _naadsm3Detections->insert( d.herd_index, detections );
  }

  det = new CNAADSM3Detect( d );
  detections->append( det );
}

void naadsmExposeHerd( HRD_expose_t e ) {
  _smSim->prodTypeList()->exposeHerd( e );
}

void naadsmTraceHerd( HRD_trace_t t ) {
  _smSim->prodTypeList()->traceHerd( t );
}

void naadsmExamineHerd( HRD_exam_t e ) {
  _smSim->prodTypeList()->examineHerd( e );
}

void naadsmTestHerd( HRD_test_t t ) {
  _smSim->prodTypeList()->testHerd( t );
}

void naadsmQueueHerdForDestruction( int herdIdx ) {
  _smSim->prodTypeList()->queueHerdForDestruction( herdIdx, _simDay );
}

void naadsmDestroyHerd( HRD_control_t c ) {
  _smSim->prodTypeList()->destroyHerd( c, _simDay );

  // If this herd was previously "enzoned", it can be taken out of the list now.
  if( _herdsInZones->contains( c.herd_index ) )
    _herdsInZones->remove( c.herd_index );
}

void naadsmQueueHerdForVaccination( int herdIdx ) {
  _smSim->prodTypeList()->queueHerdForVaccination( herdIdx, _simDay );
}

void naadsmVaccinateHerd( HRD_control_t c ) {
  _smSim->prodTypeList()->vaccinateHerd( c, _simDay );
}

void naadsmCancelHerdVaccination( HRD_control_t c ) {
  _smSim->prodTypeList()->cancelHerdVaccination( c );
}

void naadsmMakeZoneFocus( int herdIdx ) {
  _smSim->zoneList()->setFocusCreatedOnDay( _simDay );
  _smSim->prodTypeList()->makeHerdZoneFocus( herdIdx );
}

void naadsmRecordZoneChange( HRD_zone_t z ) {
  // Add the herd to the list of those "enzoned".
  // At the end of the day, outputs will be recorded.
  HRD_herd_t* h = HRD_herd_list_get( _smSim->prodTypeList()->herds(), z.herd_index );

  if( !_herdsInZones->contains( z.herd_index) )
    _herdsInZones->insert( z.herd_index, h );
}
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Zone outputs
//-----------------------------------------------------------------------------
void naadsmRecordZonePerimeter( int zoneLevel, double perim ) {
  if( _smSim->zoneList()->contains( zoneLevel ) ) {
    CZone* z = _smSim->zoneList()->value( zoneLevel );
    z->setPerimeter( perim, _simDay );
  }
  else
    qFatal( QString( "Zone list does not contain level zoneLevel %1 in naadsm_record_zone_perimeter" ).arg( zoneLevel ).toLatin1().data() );
}


void naadsmRecordZoneArea( int zoneLevel, double area ) {
  if( _smSim->zoneList()->contains( zoneLevel ) ) {
    CZone* z = _smSim->zoneList()->value( zoneLevel );
    z->setArea( area, _simDay );
  }
  else
    qFatal( QString( "Zone list does not contain level zoneLevel %1 in naadsm_record_zone_area" ).arg( zoneLevel ).toLatin1().data() );
}
//-----------------------------------------------------------------------------



