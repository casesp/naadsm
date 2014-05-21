// Begin 5/16/2014

#ifndef CHERDLIST_H
#define CHERDLIST_H

#include <naadsm.h>

#include "csmdatabase.h"
#include "cprodtype.h"

#include <herd.h>

class CHerdList {
  public:
    CHerdList( HRD_herd_list_t* herds, CProdTypeList* ptList );
    ~CHerdList();

    // Unused here
    //void initializeAllOutputRecords();
    //void prepareForIteration( int it );
    //void processIterationRecords( CSMDatabase* smdb, int it );



  protected:
    CProdTypeList* _ptList;
    HRD_herd_list_t* _c_herds;
};


#endif // CHERDLIST_H
