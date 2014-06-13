#ifndef CPRODTYPE_H
#define CPRODTYPE_H

#include <QtCore>

#include "herd.h"

#include "csmptoutput.h"
#include "csmdatabase.h"

class CProdType{
  public:
    CProdType( const QString& ptName, const int ptID );
    ~CProdType();

    void clearAllRecords();
    void resetIterationRecords();
    void prepareForDay();
    void processDailyRecords( CSMDatabase* db, int iteration, int day );
    void processIterationRecords( CSMDatabase* db, int iteration );

    void recordInfectedAtFirstDetection();

    void updateDailyRecords (
      const int herdAnimalCount,
      const NAADSM_disease_state oldState,
      const NAADSM_disease_state newState
    );
    void addInfectionEvent(
      const int herdAnimalCount,
      const HRD_infect_t r,
      const int simDay
    );
    void addExposureEvent( const int herdAnimalCount, const HRD_expose_t e );
    void addDetectionEvent( const int herdAnimalCount, const HRD_detect_t d, const int simDay );
    void addDestructionQueueEvent( const int herdAnimalCount, const int simDay );
    void addDestructionEvent( HRD_herd_t* h, HRD_control_t c, const int simDay );
    void addVaccinationQueueEvent( const int herdAnimalCount, const int simDay );
    void subtractVaccinationQueueEvent( const int herdAnimalCount );
    void addVaccinationEvent( HRD_herd_t* h, HRD_control_t c, const int simDay );
    void addHerdExamEvent( const int herdAnimalCount, const HRD_exam_t e );
    void addDiagnosticTestEvent( const int herdAnimalCount, const HRD_test_t t );
    void addAttemptedTraceEvent( const int herdAnimalCount, const HRD_trace_t t );
    void addTraceOrigin( const HRD_trace_t t );
    void addZoneFocusEvent();

    int id() { return _ptID; }
    QString description() { return _descr; }

  protected:
    CSMDailyOutput* _outputs;
    CSMDailyOutput* _initialOutputs;

    QString _descr;
    int _ptID;
};



// Key is production type name.
class CProdTypeList : public QMap<QString, CProdType*> {
  public:
    CProdTypeList( HRD_herd_list_t* herds );
    ~CProdTypeList();

    void clearAllRecords();
    void resetIterationRecords();
    void prepareForDay();
    void processDailyRecords( CSMDatabase* db, int iteration, int day );
    void processIterationRecords( CSMDatabase* db, int iteration );

    // Herd output recording
    void changeHerdState( HRD_update_t c, const int simDay );
    void infectHerd( HRD_infect_t r, const int simDay );
    void exposeHerd( HRD_expose_t e );
    void traceHerd( HRD_trace_t t );
    void examineHerd( HRD_exam_t e );
    void testHerd( HRD_test_t t );
    void queueHerdForDestruction( int herdIdx, const int simDay );
    void destroyHerd( HRD_control_t c, const int simDay );
    void queueHerdForVaccination( int herdIdx, const int simDay );
    void vaccinateHerd( HRD_control_t c, const int simDay );
    void cancelHerdVaccination( HRD_control_t c );
    void makeHerdZoneFocus( int herdIdx );
    void detectHerd( const HRD_detect_t d, const int simDay );

    HRD_herd_list_t* herds() { return _herds; }

  protected:
    HRD_herd_list_t* _herds;

    int _deswUMax;
    int _deswUMaxDay;
    int _deswUTimeMax;
    double _deswAMax;
    int _deswAMaxDay;
    double _deswUDaysInQueue;
    double _deswADaysInQueue;
    int _destrQueueLengthUnits;
    double _destrQueueLengthAnimals;
    int _unitsDestroyed;

    int _vacwUMax;
    int _vacwUMaxDay;
    int _vacwUTimeMax;
    double _vacwAMax;
    int _vacwAMaxDay;
    double _vacwUDaysInQueue;
    double _vacwADaysInQueue;
    int _vaccQueueLengthUnits;
    double _vaccQueueLengthAnimals;
    int _unitsVaccinated;

    bool _firstDetectionHasOccurred;
    bool _firstDetectionProcessed;
};


#endif // CPRODTYPE_H
