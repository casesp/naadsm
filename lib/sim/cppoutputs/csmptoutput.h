#ifndef SMPTOUTPUT_H
#define SMPTOUTPUT_H

#include "csmdatabase.h"
#include "naadsm.h"

#include <QSqlQuery>


/* Used to indicate the type of output record.  Not sure if this is still needed... */
typedef enum {
  DRTUnspecified,
  DRTDaily,
  DRTIteration
} NAADSM_output_type;


class CSMIterationOutput {
  public:
    CSMIterationOutput();
    ~CSMIterationOutput();
    void clear();

    void addCumulRecordsFrom( CSMIterationOutput* src );
    void setCumulRecordsFrom( CSMIterationOutput* src );

    void addToDestrQueue( const int herdSize, const int day );
    void processDestruction( const int nAnimals, const int day, const int queueDay );

    void addToVaccQueue( const int herdSize, const int day );
    void removeFromVaccQueue( const int herdSize ); // if vaccination is canceled
    void processVaccination( const int nAnimals, const int day, const int queueDay );


    // Output values
    //---------------
    int expcUAll();
    int expcAAll();

    int infcUAll();
    int infcAAll();

    int trcUDirAll();
    int trcADirAll();
    int trcUIndAll();
    int trcAIndAll();
    int trcUAll();
    int trcAAll();

    int tocUDirAll(); // New in 3.2.11 and 4.0.5
    int tocUIndAll(); // New in 3.2.11 and 4.0.5
    int tocUAll();    // New in 3.2.11 and 4.0.5

    int detcUAll();
    int detcAAll();

    int descUAll();
    int descAAll();

    int vaccUAll();
    int vaccAAll();

    int exmcUDirAll();
    int exmcADirAll();
    int exmcUIndAll();
    int exmcAIndAll();
    int exmcUAll();
    int exmcAAll();

    int tstcUDirAll();
    int tstcADirAll();
    int tstcUIndAll();
    int tstcAIndAll();
    int tstcUAll();
    int tstcAAll();

    double vacwUTimeAvg();
    double deswUTimeAvg();

    int firstDetection;
    int firstDetUInf;      // New in 3.2.13
    int firstDetAInf;      // new in 3.2.13
    int firstVaccination;
    int firstDestruction;
    int lastDetection;

    // Running totals for each disease state
    int tscUSusc;
    int tscASusc;
    int tscULat;
    int tscALat;
    int tscUSubc;
    int tscASubc;
    int tscUClin;
    int tscAClin;
    int tscUNImm;
    int tscANImm;
    int tscUVImm;
    int tscAVImm;
    int tscUDest;
    int tscADest;

    // Running totals for cause of infection
    int infcUIni;
    int infcAIni;
    int infcUAir;
    int infcAAir;
    int infcUDir;
    int infcADir;
    int infcUInd;
    int infcAInd;

    // Running totals for exposures
    int expcUDir;
    int expcADir;
    int expcUInd;
    int expcAInd;

    // Running totals for trace-forwards
    int trcUDirFwd;
    int trcADirFwd;
    int trcUIndFwd;
    int trcAIndFwd;
    int trcUDirpFwd;
    int trcADirpFwd;
    int trcUIndpFwd;
    int trcAIndpFwd;

    // Running totals for trace-backs
    int trcUDirBack;
    int trcADirBack;
    int trcUIndBack;
    int trcAIndBack;
    int trcUDirpBack;
    int trcADirpBack;
    int trcUIndpBack;
    int trcAIndpBack;

    // Running totals for trace origins
    int tocUDirFwd;  // new in 3.2.11 and 4.0.5
    int tocUIndFwd;  // new in 3.2.11 and 4.0.5
    int tocUDirBack; // new in 3.2.11 and 4.0.5
    int tocUIndBack; // new in 3.2.11 and 4.0.5

    // Running totals for detection
    int detcUClin;
    int detcAClin;
    int detcUTest;
    int detcATest;

    // Running totals for destruction
    int descUIni;
    int descAIni;
    int descUDet;
    int descADet;
    int descUDirFwd;
    int descADirFwd;
    int descUIndFwd;
    int descAIndFwd;
    int descUDirBack;
    int descADirBack;
    int descUIndBack;
    int descAIndBack;
    int descURing;
    int descARing;

    // Running totals for vaccination
    int vaccUIni;
    int vaccAIni;
    int vaccURing;
    int vaccARing;

    // Running totals for herd exams
    int exmcUDirFwd;
    int exmcADirFwd;
    int exmcUIndFwd;
    int exmcAIndFwd;
    int exmcUDirBack;
    int exmcADirBack;
    int exmcUIndBack;
    int exmcAIndBack;

    // Running totals for diagnostic testing
    int tstcUDirFwd;
    int tstcADirFwd;
    int tstcUIndFwd;
    int tstcAIndFwd;
    int tstcUDirBack;
    int tstcADirBack;
    int tstcUIndBack;
    int tstcAIndBack;
    int tstcUTruePos;
    int tstcATruePos;
    int tstcUTrueNeg;
    int tstcATrueNeg;
    int tstcUFalsePos;
    int tstcAFalsePos;
    int tstcUFalseNeg;
    int tstcAFalseNeg;

    // Running totals for zone foci
    int zoncFoci;

    // Outputs for destruction and vaccination queues
    int deswUMax;
    int deswUMaxDay;
    int deswUTimeMax;
    double deswAMax;
    int deswAMaxDay;
    double deswUDaysInQueue;
    double deswADaysInQueue;

    int vacwUMax;
    int vacwUMaxDay;
    int vacwUTimeMax;
    double vacwAMax;
    int vacwAMaxDay;
    double vacwUDaysInQueue;
    double vacwADaysInQueue;

    // Properties
    int prodTypeID() { return _ptID; }
    void setProdTypeID( int val ) { _ptID = val; }

  protected:
    // For internal use
    void initialize();
    void clearRunningTotals();

    // Properties
    int _ptID;

    double _vacwUTimeAvg;
    double _deswUTimeAvg;

    // Intermediate outputs
    int _destrQueueLengthUnits; // Current length of the queue
    double _destrQueueLengthAnimals; // Current length of the queue
    int _vaccQueueLengthUnits;
    double _vaccQueueLengthAnimals;
};


class CSMDailyOutput: public CSMIterationOutput {
  public:
    CSMDailyOutput( CSMDatabase* db );
    ~CSMDailyOutput();

    void clear();
    void clearNewDailyCounts();
    void updateDailyCounts(
      const int herdAnimalCount,
      const NAADSM_disease_state oldState,
      const NAADSM_disease_state newState
    );

    void setAllRecordsFrom( CSMDailyOutput* src );

    void setDailyRecordsFrom( CSMDailyOutput* src );
    void addDailyRecordsFrom( CSMDailyOutput* src );

    void insertDatabaseOutputs(
      const NAADSM_output_type drt,
      CSMDatabase* db,
      const int ptiD,
      const int iteration,
      const int day = 0
    );

    // New daily counts of units/animals infected for all causes
    int infUNew();
    int infANew();

    // Properties
    //------------
    // Daily numbers for each disease state
    int tsdUSusc;
    int tsdASusc;
    int tsdULat;
    int tsdALat;
    int tsdUSubc;
    int tsdASubc;
    int tsdUClin;
    int tsdAClin;
    int tsdUNImm;
    int tsdANImm;
    int tsdUVImm;
    int tsdAVImm;
    int tsdUDest;
    int tsdADest;

    // New daily counts for cause of infection
    int infnUAir;
    int infnAAir;
    int infnUDir;
    int infnADir;
    int infnUInd;
    int infnAInd;

    // New daily counts for detection
    int detnUClin;
    int detnAClin;
    int detnUTest; // new in 3.2.0
    int detnATest; // new in 3.2.0

    // New daily counts for herd exams (new in 3.2.0)
    int exmnUAll;
    int exmnAAll;

    // New daily counts for testing (new in 3.2.0)
    int tstnUTruePos;
    int tstnATruePos;
    int tstnUTrueNeg;
    int tstnATrueNeg;
    int tstnUFalsePos;
    int tstnAFalsePos;
    int tstnUFalseNeg;
    int tstnAFalseNeg;

    // New daily counts for tracing
    int trnUDirFwd;
    int trnADirFwd;
    int trnUIndFwd;
    int trnAIndFwd;
    int trnUDirBack; // New in 3.2.0
    int trnADirBack; // New in 3.2.0
    int trnUIndBack; // New in 3.2.0
    int trnAIndBack; // New in 3.2.0

    // New daily counts for trace origins (new in 3.2.11 and 4.0.5)
    int tonUDirFwd;
    int tonUIndFwd;
    int tonUDirBack;
    int tonUIndBack;

    // New daily counts for destruction for any reason
    int desnUAll;
    int desnAAll;

    // New daily counts for vaccination for any reason
    int vacnUAll;
    int vacnAAll;

    // New daily count for zone foci
    int zonnFoci;

    // Number of apparently infectious units on a particular day
    int appdUInfectious;

  protected:
    void prepQueries( CSMDatabase* db );
    void freeQueries();
    QSqlQuery* qOutDailyByProductionType;
    QSqlQuery* qOutIterationByProductionType;

    void initialize();
    void clearDailyTotals();

    void decrementDailyCounts( const int herdAnimalCount, const NAADSM_disease_state oldState );
    void incrementDailyCounts( const int herdAnimalCount, const NAADSM_disease_state newState );
};

#endif // SMPTOUTPUT_H
