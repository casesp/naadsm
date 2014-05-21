// Begin 5/16/2014

#ifndef CSMDATABASE_H
#define CSMDATABASE_H

class CSMDatabase {
  public:
    CSMDatabase();
    ~CSMDatabase();

    void initializeAllOutputRecords();
    void prepareForIteration( const int iteration );
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
};

#endif // CSMDATABASE_H
