// Begin 5/16/2014

#include "csmdatabase.h"

#include <QDebug>

CSMDatabase::CSMDatabase(){
  qDebug() << "A lot needs to happen in here.";
}

CSMDatabase::~CSMDatabase() {
  // FIXME: What needs to happen in here?
}

void CSMDatabase::initializeAllOutputRecords() {

}


void CSMDatabase::prepareForIteration( const int iteration ) {

}


void CSMDatabase::recordEndTime() {

}


void CSMDatabase::simComplete() {

}


void CSMDatabase::processIterationRecords(
  const int iteration,
  const int outbreakEndDay,
  const bool outbreakEnded,
  const int diseaseEndDay,
  const bool diseaseEnded,
  const bool zoneFociCreated
) {


}
