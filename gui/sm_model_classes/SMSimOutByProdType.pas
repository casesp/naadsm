unit SMSimOutByProdType;

(*
SMSimOutByProdType.pas
-----------------------
Begin: 2005/07/06
Last revision: $Date: 2011-10-14 15:38:10 $ $Author: areeves $
Version number: $Revision: 1.41.2.17 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

	uses
  	SMDatabase,
    StatusEnums,
    NAADSMLibraryTypes,
    HerdControlActivities
  ;

	type TDailyRecordType = (
  	DRTUnspecified,
    DRTDaily,
    DRTIteration
  );


  type TSMIterationOutput = class
    protected
      // Properties
      _prodTypeID: integer;

      // Intermediate outputs
      _destrQueueLengthUnits: longint; // Current length of the queue
      _destrQueueLengthAnimals: double; // Current length of the queue
      _vaccQueueLengthUnits: longint;
      _vaccQueueLengthAnimals: double;

      _vacwUTimeAvg: double;
      _deswUTimeAvg: double;

      // For internal use
      procedure initialize();
      procedure clearRunningTotals();

      // Properties
      procedure setProdTypeID( val: integer );
      function getProdTypeID(): integer;

      function getExpcUAll(): longint;
      function getExpcAAll(): longint;

      function getAdqcUAll(): longint; // "new" in 4.0
      function getAdqcAAll(): longint; // "new" in 4.0

      function getTrcUDirAll(): longint;
      function getTrcADirAll(): longint;
      function getTrcUIndAll(): longint;
      function getTrcAIndAll(): longint;
      function getTrcUAll(): longint;
      function getTrcAAll(): longint;

      function getTocUDirAll(): longint; // New in 3.2.11 and 4.0.5
      function getTocUIndAll(): longint; // New in 3.2.11 and 4.0.5
      function getTocUAll(): longint;    // New in 3.2.11 and 4.0.5

      function getDescUAll(): longint;
      function getDescAAll(): longint;

      function getVaccUAll(): longint;
      function getVaccAAll(): longint;

      function getExmcUDirAll(): longint;
      function getExmcADirAll(): longint;
      function getExmcUIndAll(): longint;
      function getExmcAIndAll(): longint;
      function getExmcUAll(): longint;
      function getExmcAAll(): longint;

      function getTstcUDirAll(): longint;
      function getTstcADirAll(): longint;
      function getTstcUIndAll(): longint;
      function getTstcAIndAll(): longint;
      function getTstcUAll(): longint;
      function getTstcAAll(): longint;

      function getDeswUTimeAvg(): double;
      function getVacwUTimeAvg(): double;

    public
      // Output values
      //---------------
      firstDetection: integer;
      firstDetUInf: longint;      // New in 3.2.13
      firstDetAInf: longint;      // new in 3.2.13
      firstVaccination: integer;
      firstDestruction: integer;
      lastDetection: integer;

      // Running totals for each disease state
      tscUSusc: longint;
      tscASusc: longint;
      tscULat: longint;
      tscALat: longint;
      tscUSubc: longint;
      tscASubc: longint;
      tscUClin: longint;
      tscAClin: longint;
      tscUNImm: longint;
      tscANImm: longint;
      tscUVImm: longint;
      tscAVImm: longint;
      tscUDest: longint;
      tscADest: longint;
      tscUDead: longint;  // new in 4.0
      tscADead: longint;  // new in 4.0

      // Running totals for infections
      infcUIni: longint;
      infcAIni: longint;
      infcUAll: longint; // "new" in 4.0
      infcAAll: longint; // "new" in 4.0

      // Running totals for causes of adequate exposure.  Renamed in 4.0
      adqcUAir: longint;
      adqcAAir: longint;
      adqcUDir: longint;
      adqcADir: longint;
      adqcUInd: longint;
      adqcAInd: longint;
      adqcULcl: longint; // New in 4.0
      adqcALcl: longint; // New in 4.0

      // Running totals for exposures
      expcUDir: longint;
      expcADir: longint;
      expcUInd: longint;
      expcAInd: longint;

      // Running totals for trace-forwards
      trcUDirFwd: longint;
      trcADirFwd: longint;
      trcUIndFwd: longint;
      trcAIndFwd: longint;
      trcUDirpFwd: longint;
      trcADirpFwd: longint;
      trcUIndpFwd: longint;
      trcAIndpFwd: longint;

      // Running totals for trace-backs
      trcUDirBack: longint;
      trcADirBack: longint;
      trcUIndBack: longint;
      trcAIndBack: longint;
      trcUDirpBack: longint;
      trcADirpBack: longint;
      trcUIndpBack: longint;
      trcAIndpBack: longint;

      // Running totals for trace origins
      tocUDirFwd: longint;  // new in 3.2.11 and 4.0.5
      tocUIndFwd: longint;  // new in 3.2.11 and 4.0.5
      tocUDirBack: longint; // new in 3.2.11 and 4.0.5
      tocUIndBack: longint; // new in 3.2.11 and 4.0.5

      // Running totals for detection
      detcUClin: longint;
      detcAClin: longint;
      detcUTest: longint;
      detcATest: longint;
      detcUDead: longint; // new in v 4.0
      detcADead: longint; // new in v 4.0

      //AR:deadDV
      detcUDeadD: longint; // new in v 4.0.6
      detcADeadD: longint; // new in v 4.0.6
      detcUDeadV: longint; // new in v 4.0.6
      detcADeadV: longint; // new in v 4.0.6

      detcUAll: longint; // new in v 4.0
      detcAAll: longint; // new in v 4.0

      detcUDeadAll: longint; // new in v 4.0.9
      detcADeadAll: longint; // new in v 4.0.9

      // Running totals for destruction
      descUIni: longint;
      descAIni: longint;
      descUDet: longint;
      descADet: longint;
      descUDirFwd: longint;
      descADirFwd: longint;
      descUIndFwd: longint;
      descAIndFwd: longint;
      descUDirBack: longint;
      descADirBack: longint;
      descUIndBack: longint;
      descAIndBack: longint;
      descURing: longint;
      descARing: longint;

      //AR:deadDV
      descUDcd: longint; // new in v 4.0.6
      descADcd: longint; // new in v 4.0.6
      descUDcdInDestrQueue: longint; // new in v 4.0.9
      descADcdInDestrQueue: longint; // new in v 4.0.9

      // Running totals for vaccination
      vaccUIni: longint;
      vaccAIni: longint;
      vaccURing: longint;
      vaccARing: longint;

      // Running totals for herd exams
      exmcUDirFwd: longint;
      exmcADirFwd: longint;
      exmcUIndFwd: longint;
      exmcAIndFwd: longint;
      exmcUDirBack: longint;
      exmcADirBack: longint;
      exmcUIndBack: longint;
      exmcAIndBack: longint;

      // Running totals for diagnostic testing
      tstcUDirFwd: longint;
      tstcADirFwd: longint;
      tstcUIndFwd: longint;
      tstcAIndFwd: longint;
      tstcUDirBack: longint;
      tstcADirBack: longint;
      tstcUIndBack: longint;
      tstcAIndBack: longint;
      tstcUTruePos: longint;
      tstcATruePos: longint;
      tstcUTrueNeg: longint;
      tstcATrueNeg: longint;
      tstcUFalsePos: longint;
      tstcAFalsePos: longint;
      tstcUFalseNeg: longint;
      tstcAFalseNeg: longint;

      // Running totals for zone foci
      zoncFoci: longint;

      //Running totals for death due to disease
      deadcUIni: longint; // new in 4.0
      deadcAIni: longint; // new in 4.0
      deadcU: longint;    // new in 4.0
      deadcA: longint;    // new in 4.0
      deadcUInDestrQueue: longint; // New in 4.0.1
      deadcAInDestrQueue: longint; // New in 4.0.1

      // Outputs for destruction and vaccination queues
      deswUMax: longint;
      deswUMaxDay: integer;
      deswUTimeMax: longint;
      deswAMax: double;
      deswAMaxDay: integer;
      deswUDaysInQueue: double;
      deswADaysInQueue: double;

      vacwUMax: longint;
      vacwUMaxDay: integer;
      vacwUTimeMax: longint;
      vacwAMax: double;
      vacwAMaxDay: integer;
      vacwUDaysInQueue: double;
      vacwADaysInQueue: double;

      constructor create(); virtual;
      destructor destroy(); override;
      procedure clear(); virtual;

      procedure addCumulRecordsFrom( const src: TSMIterationOutput );
      procedure setCumulRecordsFrom( const src: TSMIterationOutput );

      procedure addToDestrQueue( const herdSize: integer; const day: integer );
      procedure removeFromDestrQueue( const herdSize: integer );
      procedure processDestruction( const nAnimals, day, queueDay: integer; const inVaccQueue: boolean );

      procedure addToVaccQueue( const herdSize: integer; const day: integer );
      procedure removeFromVaccQueue( const herdSize: integer ); // if vaccination is canceled
      procedure processVaccination( const nAnimals, day, queueDay: integer );

      // Properties
      //-----------
      property prodTypeID: integer read getProdTypeID write setProdTypeID;

      property expcUAll: longint read getExpcUAll;
      property expcAAll: longint read getExpcAAll;

      property trcUDirAll: longint read getTrcUDirAll;
      property trcADirAll: longint read getTrcADirAll;
      property trcUIndAll: longint read getTrcUIndAll;
      property trcAIndAll: longint read getTrcAIndAll;
      property trcUAll: longint read getTrcUAll;
      property trcAAll: longint read getTrcAAll;

      property tocUDirAll: longint read  getTocUDirAll;
      property tocUIndAll: longint read  getTocUIndAll;
      property tocUAll: longint read  getTocUAll;

      property descUAll: longint read getDescUAll;
      property descAAll: longint read getDescAAll;

      property vaccUAll: longint read getVaccUAll;
      property vaccAAll: longint read getVaccAAll;

      property exmcUDirAll: longint read getExmcUDirAll;
      property exmcADirAll: longint read getExmcADirAll;
      property exmcUIndAll: longint read getExmcUIndAll;
      property exmcAIndAll: longint read getExmcAIndAll;
      property exmcUAll: longint read getExmcUAll;
      property exmcAAll: longint read getExmcAAll;

      property tstcUDirAll: longint read getTstcUDirAll;
      property tstcADirAll: longint read getTstcADirAll;
      property tstcUIndAll: longint read getTstcUIndAll;
      property tstcAIndAll: longint read getTstcAIndAll;
      property tstcUAll: longint read getTstcUAll;
      property tstcAAll: longint read getTstcAAll;

      property destrQueueLengthUnits: longint read _destrQueueLengthUnits;
      property destrQueueLengthAnimals: double read _destrQueueLengthAnimals;

      property vaccQueueLengthUnits: longint read _vaccQueueLengthUnits;
      property vaccQueueLengthAnimals: double read _vaccQueueLengthAnimals;

      property vacwUTimeAvg: double read getVacwUTimeAvg write _vacwUTimeAvg;
      property DeswUTimeAvg: double read getDeswUTimeAvg write _deswUTimeAvg;

      property deadcUAll: longint read deadcU; // new in 4.0  DO NOT include initially dead units
      property deadcAAll: longint read deadcA; // new in 4.0  DO NOT include initially dead units
    end  // of class TSMIterationOutput
  ;


	type TSMDailyOutput = class( TSMIterationOutput )
    protected
      // For internal use
      //------------------
      procedure initialize();
      procedure clearDailyTotals();

      procedure decrementDailyCounts( const herdAnimalCount: longint; const oldState: TNAADSMDiseaseState );

      procedure incrementDailyCounts(
        const herdAnimalCount: integer;
        const newState: TNAADSMDiseaseState;
        const isInDestructionQueue: boolean
      );


    public
      // Properties
      //------------
      // Daily numbers for each disease state
      tsdUSusc: longint;
      tsdASusc: longint;
      tsdULat: longint;
      tsdALat: longint;
      tsdUSubc: longint;
      tsdASubc: longint;
      tsdUClin: longint;
      tsdAClin: longint;
      tsdUNImm: longint;
      tsdANImm: longint;
      tsdUVImm: longint;
      tsdAVImm: longint;
      tsdUDest: longint;
      tsdADest: longint;
      tsdUDead: longint;  // new in 4.0
      tsdADead: longint;  // new in 4.0

      // daily counts for infections
      infnUAll: longint; // "new" in 4.0
      infnAAll: longint; // "new" in 4.0

      // Daily counts for adequate exposures.  "New" in 4.0.
      adqnUAir: longint;
      adqnAAir: longint;
      adqnUDir: longint;
      adqnADir: longint;
      adqnUInd: longint;
      adqnAInd: longint;
      adqnULcl: longint;
      adqnALcl: longint;

      // daily counts for detection
      detnUClin: longint;
      detnAClin: longint;
      detnUTest: longint; // new in 3.2.0
      detnATest: longint; // new in 3.2.0
      detnUDead: longint; // new in 4.0
      detnADead: longint; // new in 4.0

      //AR:deadDV
      detnUDeadD: longint; // new in 4.0.6
      detnADeadD: longint; // new in 4.0.6
      detnUDeadV: longint; // new in 4.0.6
      detnADeadV: longint; // new in 4.0.6

      detnUAll: longint; // new in 4.0
      detnAAll: longint; // new in 4.0

      detnUDeadAll: longint; // new in 4.0
      detnADeadAll: longint; // new in 4.0

      // daily counts for herd exams (new in 3.2.0)
      exmnUAll: longint;
      exmnAAll: longint;

      // daily counts for testing (new in 3.2.0)
      tstnUTruePos: longint;
      tstnATruePos: longint;
      tstnUTrueNeg: longint;
      tstnATrueNeg: longint;
      tstnUFalsePos: longint;
      tstnAFalsePos: longint;
      tstnUFalseNeg: longint;
      tstnAFalseNeg: longint;

      // daily counts for tracing
      trnUDirFwd: longint;
      trnADirFwd: longint;
      trnUIndFwd: longint;
      trnAIndFwd: longint;
      trnUDirBack: longint; // New in 3.2.0
      trnADirBack: longint; // New in 3.2.0
      trnUIndBack: longint; // New in 3.2.0
      trnAIndBack: longint; // New in 3.2.0

      // daily counts for trace origins (new in 3.2.11 and 4.0.5)
      tonUDirFwd: longint;
      tonUIndFwd: longint;
      tonUDirBack: longint;
      tonUIndBack: longint;

      // daily counts for destruction for any reason - new in 3.2.0
      desnUAll: longint;
      desnAAll: longint;

      // Daily counts for destructions of dead-from-disease units - new in 4.0.8
      desnUDcd: longint;
      desnADcd: longint;

      // Daily counts for destructions of dead-from-disease units that were already in queue when they died - new in 4.0.8
      desnUDcdInDestrQueue: longint;
      desnADcdInDestrQueue: longint;

      // daily counts for vaccination for any reason - new in 3.2.0
      vacnUAll: longint;
      vacnAAll: longint;

      // daily count for zone foci
      zonnFoci: longint; // new in 3.2.0

      // Number of apparently infectious units on a particular day
      appdUInfectious: longint;

      // Daily ccounts for deaths from the disease - new in 4.0
      deadnU: longint;
      deadnA: longint;
      deadnUInDestrQueue: longint; // New in 4.0.1
      deadnAInDestrQueue: longint; // New in 4.0.1

    	constructor create(); override;
      destructor destroy(); override;

      procedure clear(); override;
      procedure clearNewDailyCounts();
      procedure updateDailyCounts(
        const herdAnimalCount: integer;
        const oldState: TNAADSMDiseaseState;
        const newState: TNAADSMDiseaseState;
        const isInDestructionQueue: boolean;
        const day: integer
      );

      procedure setAllRecordsFrom( const src: TSMDailyOutput );

      procedure setDailyRecordsFrom( const src: TSMDailyOutput );
      procedure addDailyRecordsFrom( const src: TSMDailyOutput );

      procedure insertDatabaseOutputs(
        drt: TDailyRecordType;
      	db: TSMDatabase;
        ptiD: integer;
        iteration: integer;
        day: integer = 0
      );

      procedure debug();
      procedure debugSel();
  	end
  ;


implementation

	uses
  	SysUtils,
  	SqlClasses,
    MyStrUtils,
    DebugWindow,
    RemoteDatabaseParams
  ;


  const
  	DBSMSIMOUTBYPRODTYPE: boolean = false; // set to true to display debugging messages for this unit.

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TSMIterationOutput.create();
    begin
      inherited create();
      initialize();
    end
  ;

  
  destructor TSMIterationOutput.destroy();
  	begin
    	// There is nothing more to do here.
      inherited destroy();
    end
  ;


  procedure TSMIterationOutput.clearRunningTotals();
    begin
      // Running totals for each disease state
      tscUSusc := 0;
      tscASusc := 0;
      tscULat := 0;
      tscALat := 0;
      tscUSubc := 0;
      tscASubc := 0;
      tscUClin := 0;
      tscAClin := 0;
      tscUNImm := 0;
      tscANImm := 0;
      tscUVImm := 0;
      tscAVImm := 0;
      tscUDest := 0;
      tscADest := 0;
      tscUDead := 0;
      tscADead := 0;

      // Running totals for infections
      infcUIni := 0;
      infcAIni := 0;
      infcUAll := 0;
      infcAAll := 0;

      // Running totals for causes of adequate exposure.  Renamed in 4.0
      adqcUAir := 0;
      adqcAAir := 0;
      adqcUDir := 0;
      adqcADir := 0;
      adqcUInd := 0;
      adqcAInd := 0;
      adqcULcl := 0;
      adqcALcl := 0;

      // Running totals for exposures
      expcUDir := 0;
      expcADir := 0;
      expcUInd := 0;
      expcAInd := 0;

      // Running totals for traces
      trcUDirFwd := 0;
      trcADirFwd := 0;
      trcUIndFwd := 0;
      trcAIndFwd := 0;
      trcUDirpFwd := 0;
      trcADirpFwd := 0;
      trcUIndpFwd := 0;
      trcAIndpFwd := 0;
      trcUDirBack := 0;
      trcADirBack := 0;
      trcUIndBack := 0;
      trcAIndBack := 0;
      trcUDirpBack := 0;
      trcADirpBack := 0;
      trcUIndpBack := 0;
      trcAIndpBack := 0;

      // Running totals for trace origins
      tocUDirFwd := 0;
      tocUIndFwd := 0;
      tocUDirBack := 0;
      tocUIndBack := 0;

      // Running totals for detection
      detcUClin := 0;
      detcAClin := 0;
      detcUTest := 0;
      detcATest := 0;
      detcUDead := 0;
      detcADead := 0;

      //AR:deadDV
      detcUDeadD := 0;
      detcADeadD := 0;
      detcUDeadV := 0;
      detcADeadV := 0;

      detcUAll := 0;
      detcAAll := 0;

      detcUDeadAll := 0;
      detcADeadAll := 0;

      // Running totals for destruction
      descUIni := 0;
      descAIni := 0;
      descUDet := 0;
      descADet := 0;
      descUDirFwd := 0;
      descADirFwd := 0;
      descUIndFwd := 0;
      descAIndFwd := 0;
      descUDirBack := 0;
      descADirBack := 0;
      descUIndBack := 0;
      descAIndBack := 0;
      descURing := 0;
      descARing := 0;

      //AR:deadDV
      descUDcd := 0;
      descADcd := 0;
      descUDcdInDestrQueue := 0;
      descADcdInDestrQueue := 0;

      // Running totals for vaccination
      vaccUIni := 0;
      vaccAIni := 0;
      vaccURing := 0;
      vaccARing := 0;

      //Running totals for herd exams
      exmcUDirFwd := 0;
      exmcADirFwd := 0;
      exmcUIndFwd := 0;
      exmcAIndFwd := 0;
      exmcUDirBack := 0;
      exmcADirBack := 0;
      exmcUIndBack := 0;
      exmcAIndBack := 0;

      // Running totals for testing
      tstcUDirFwd := 0;
      tstcADirFwd := 0;
      tstcUIndFwd := 0;
      tstcAIndFwd := 0;
      tstcUDirBack := 0;
      tstcADirBack := 0;
      tstcUIndBack := 0;
      tstcAIndBack := 0;
      tstcUTruePos := 0;
      tstcATruePos := 0;
      tstcUTrueNeg := 0;
      tstcATrueNeg := 0;
      tstcUFalsePos := 0;
      tstcAFalsePos := 0;
      tstcUFalseNeg := 0;
      tstcAFalseNeg := 0;

      // Running totals for zone foci
      zoncFoci := 0;

      //Running totals for death due to disease
      deadcUIni := 0;
      deadcAIni := 0;
      deadcU := 0;
      deadcA := 0;
      deadcUInDestrQueue := 0;
      deadcAInDestrQueue := 0;

      // Values for destruction and vaccination queues
      deswUMax := 0;
      deswUMaxDay := 0;
      deswUTimeMax := 0;
      deswAMax := 0.0;
      deswAMaxDay := 0;
      deswUDaysInQueue := 0.0;
      deswADaysInQueue := 0.0;
      vacwUMax := 0;
      vacwUMaxDay := 0;
      vacwUTimeMax := 0;
      vacwAMax := 0.0;
      vacwAMaxDay := 0;
      vacwUDaysInQueue := 0.0;
      vacwADaysInQueue := 0.0;

      // Intermediate outputs
      _destrQueueLengthUnits := 0;
      _destrQueueLengthAnimals := 0.0;
      _vaccQueueLengthUnits := 0;
      _vaccQueueLengthAnimals := 0.0;

      _vacwUTimeAvg := -1.0;
      _deswUTimeAvg := -1.0;
    end
  ;


  procedure TSMIterationOutput.setCumulRecordsFrom( const src: TSMIterationOutput );
    begin
      clear();
      addCumulRecordsFrom( src );
    end
  ;

  
  procedure TSMIterationOutput.addCumulRecordsFrom( const src: TSMIterationOutput );
    begin
      // Running totals for each disease state
      inc( self.tscUSusc, src.tscUSusc );
      inc( self.tscASusc, src.tscASusc );
      inc( self.tscULat, src.tscULat );
      inc( self.tscALat, src.tscALat );
      inc( self.tscUSubc, src.tscUSubc );
      inc( self.tscASubc, src.tscASubc );
      inc( self.tscUClin, src.tscUClin );
      inc( self.tscAClin, src.tscAClin );
      inc( self.tscUNImm, src.tscUNImm );
      inc( self.tscANImm, src.tscANImm );
      inc( self.tscUVImm, src.tscUVImm );
      inc( self.tscAVImm, src.tscAVImm );
      inc( self.tscUDest, src.tscUDest );
      inc( self.tscADest, src.tscADest );
      inc( self.tscUDead, src.tscUDead );
      inc( self.tscADead, src.tscADead );

      // Running totals for infections
      inc( self.infcUIni, src.infcUIni );
      inc( self.infcAIni, src.infcAIni );
      inc( self.infcUAll, src.infcUAll );
      inc( self.infcAAll, src.infcAAll );

      // Running totals for causes of adequate exposure.
      inc( self.adqcUAir, src.adqcUAir );
      inc( self.adqcAAir, src.adqcAAir );
      inc( self.adqcUDir, src.adqcUDir );
      inc( self.adqcADir, src.adqcADir );
      inc( self.adqcUInd, src.adqcUInd );
      inc( self.adqcAInd, src.adqcAInd );
      inc( self.adqcULcl, src.adqcULcl );
      inc( self.adqcALcl, src.adqcALcl );

      // Running totals for exposures
      inc( self.expcUDir, src.expcUDir );
      inc( self.expcADir, src.expcADir );
      inc( self.expcUInd, src.expcUInd );
      inc( self.expcAInd, src.expcAInd );

      // Running totals for traces
      inc( self.trcUDirFwd, src.trcUDirFwd );
      inc( self.trcADirFwd, src.trcADirFwd );
      inc( self.trcUIndFwd, src.trcUIndFwd );
      inc( self.trcAIndFwd, src.trcAIndFwd );
      inc( self.trcUDirpFwd, src.trcUDirpFwd );
      inc( self.trcADirpFwd, src.trcADirpFwd );
      inc( self.trcUIndpFwd, src.trcUIndpFwd );
      inc( self.trcAIndpFwd, src.trcAIndpFwd );
      inc( self.trcUDirBack, src.trcUDirBack );
      inc( self.trcADirBack, src.trcADirBack );
      inc( self.trcUIndBack, src.trcUIndBack );
      inc( self.trcAIndBack, src.trcAIndBack );
      inc( self.trcUDirpBack, src.trcUDirpBack );
      inc( self.trcADirpBack, src.trcADirpBack );
      inc( self.trcUIndpBack, src.trcUIndpBack );
      inc( self.trcAIndpBack, src.trcAIndpBack );

      // Running totals for trace origins
      inc( self.tocUDirFwd, src.tocUDirFwd );
      inc( self.tocUIndFwd, src.tocUIndFwd );
      inc( self.tocUDirBack, src.tocUDirBack );
      inc( self.tocUIndBack, src.tocUIndBack );

      // Running totals for detection
      inc( self.detcUClin, src.detcUClin );
      inc( self.detcAClin, src.detcAClin );
      inc( self.detcUTest, src.detcUTest );
      inc( self.detcATest, src.detcATest );
      inc( self.detcUDead, src.detcUDead );
      inc( self.detcADead, src.detcADead );

      //AR:deadDV
      inc( self.detcUDeadD, src.detcUDeadD );
      inc( self.detcADeadD, src.detcADeadD );
      inc( self.detcUDeadV, src.detcUDeadV );
      inc( self.detcADeadV, src.detcADeadV );

      inc( self.detcUAll, src.detcUAll );
      inc( self.detcAAll, src.detcAAll );

      inc( self.detcUDeadAll, src.detcUDeadAll );
      inc( self.detcADeadAll, src.detcADeadAll );

      // Running totals for destruction
      inc( self.descUIni, src.descUIni );
      inc( self.descAIni, src.descAIni );
      inc( self.descUDet, src.descUDet );
      inc( self.descADet, src.descADet );
      inc( self.descUDirFwd, src.descUDirFwd );
      inc( self.descADirFwd, src.descADirFwd );
      inc( self.descUIndFwd, src.descUIndFwd );
      inc( self.descAIndFwd, src.descAIndFwd );
      inc( self.descUDirBack, src.descUDirBack );
      inc( self.descADirBack, src.descADirBack );
      inc( self.descUIndBack, src.descUIndBack );
      inc( self.descAIndBack, src.descAIndBack );
      inc( self.descURing, src.descURing );
      inc( self.descARing, src.descARing );

      //AR:deadDV
      inc( self.descUDcd, src.descUDcd );
      inc( self.descADcd, src.descADcd );
      inc( self.descUDcdInDestrQueue, src.descUDcdInDestrQueue );
      inc( self.descADcdInDestrQueue, src.descADcdInDestrQueue );

      // Running totals for vaccination
      inc( self.vaccUIni, src.vaccUIni );
      inc( self.vaccAIni, src.vaccAIni );
      inc( self.vaccURing, src.vaccURing );
      inc( self.vaccARing, src.vaccARing );

      // Running totals for herd exams
      inc( self.exmcUDirFwd, src.exmcUDirFwd );
      inc( self.exmcADirFwd, src.exmcADirFwd );
      inc( self.exmcUIndFwd, src.exmcUIndFwd );
      inc( self.exmcAIndFwd, src.exmcAIndFwd );
      inc( self.exmcUDirBack, src.exmcUDirBack );
      inc( self.exmcADirBack, src.exmcADirBack );
      inc( self.exmcUIndBack, src.exmcUIndBack );
      inc( self.exmcAIndBack, src.exmcAIndBack );

      // Running totals for diagnostic testing
      inc( self.tstcUDirFwd, src.tstcUDirFwd );
      inc( self.tstcADirFwd, src.tstcADirFwd );
      inc( self.tstcUIndFwd, src.tstcUIndFwd );
      inc( self.tstcAIndFwd, src.tstcAIndFwd );
      inc( self.tstcUDirBack, src.tstcUDirBack );
      inc( self.tstcADirBack, src.tstcADirBack );
      inc( self.tstcUIndBack, src.tstcUIndBack );
      inc( self.tstcAIndBack, src.tstcAIndBack );
      inc( self.tstcUTruePos, src.tstcUTruePos );
      inc( self.tstcATruePos, src.tstcATruePos );
      inc( self.tstcUTrueNeg, src.tstcUTrueNeg );
      inc( self.tstcATrueNeg, src.tstcATrueNeg );
      inc( self.tstcUFalsePos, src.tstcUFalsePos );
      inc( self.tstcAFalsePos, src.tstcAFalsePos );
      inc( self.tstcUFalseNeg, src.tstcUFalseNeg );
      inc( self.tstcAFalseNeg, src.tstcAFalseNeg );

      // Running totals for zone foci
      inc( self.zoncFoci, src.zoncFoci );

     //Running totals for death due to disease
      inc( self.deadcUIni, src.deadcUIni );
      inc( self.deadcAIni, src.deadcAIni );
      inc( self.deadcU, src.deadcU );
      inc( self.deadcA, src.deadcA );
      inc( self.deadcUInDestrQueue, src.deadcUInDestrQueue );
      inc( self.deadcAInDestrQueue, src.deadcAInDestrQueue );

      // Values for destruction and vaccination queues
      // Note that these are handled somewhat differently than the other outputs
      if( src.deswUMax > self.deswUMax ) then
        begin
          self.deswUMax := src.deswUMax;
          self.deswUMaxDay := src.deswUMaxDay;
        end
      ;
      if( src.deswAMax > self.deswAMax ) then
        begin
          self.deswAMax := src.deswAMax;
          self.deswAMaxDay := src.deswAMaxDay;
        end
      ;
      if( src.deswUTimeMax > self.deswUTimeMax ) then
        self.deswUTimeMax := src.deswUTimeMax
      ;

      self.deswUDaysInQueue := self.deswUDaysInQueue + src.deswUDaysInQueue;
      self.deswADaysInQueue := self.deswADaysInQueue + src.deswADaysInQueue;

      if( 0.0 = self.descUAll ) then
        self.deswUTimeAvg := 0.0
      else
        self.deswUTimeAvg := self.deswUDaysInQueue / ( self.descUAll )
      ;

      if( src.vacwUMax > self.vacwUMax ) then
        begin
          self.vacwUMax := src.vacwUMax;
          self.vacwUMaxDay := src.vacwUMaxDay;
        end
      ;
      if( src.vacwAMax > self.vacwAMax ) then
        begin
          self.vacwAMax := src.vacwAMax;
          self.vacwAMaxDay := src.vacwAMaxDay;
        end
      ;
      if( src.vacwUTimeMax > self.vacwUTimeMax ) then
        self.vacwUTimeMax := src.vacwUTimeMax
      ;

      self.vacwUDaysInQueue := self.vacwUDaysInQueue + src.vacwUDaysInQueue;
      self.vacwADaysInQueue := self.vacwADaysInQueue + src.vacwADaysInQueue;

      if( 0.0 = self.vaccUAll ) then
        self.vacwUTimeAvg := 0.0
      else
        self.vacwUTimeAvg := self.vacwUDaysInQueue / ( self.vaccUAll )
      ;
    end
  ;



  procedure TSMIterationOutput.initialize();
    begin
      _prodTypeID := -1;
      clear();
    end
  ;


  procedure TSMIterationOutput.clear();
    begin
      firstDetection := -1;
      firstDestruction := -1;
      firstVaccination := -1;
      lastDetection := -1;

      // Number infected at time of first detection
      firstDetUInf := -1;
      firstDetAInf := -1;
      
      clearRunningTotals();
    end
  ;


  procedure TSMIterationOutput.addToDestrQueue( const herdSize: integer; const day: integer );
    begin
      inc( _destrQueueLengthUnits );
      _destrQueueLengthAnimals := _destrQueueLengthAnimals + herdSize;

      if( _destrQueueLengthUnits > deswUMax ) then
        begin
          deswUMax := _destrQueueLengthUnits;
          deswUMaxDay := day;
        end
      ;

      if( _destrQueueLengthAnimals > deswAMax ) then
        begin
          deswAMax := _destrQueueLengthAnimals;
          deswAMaxDay := day;
        end
      ;
    end
  ;


  procedure TSMIterationOutput.removeFromDestrQueue( const herdSize: integer );
    begin
      dec( _destrQueueLengthUnits );
      _destrQueueLengthAnimals := _destrQueueLengthAnimals - herdSize;

      if( 0 > _destrQueueLengthUnits ) then
        raise exception.Create( 'Number of units in destruction queue has dropped below 0 in TSMIterationOutput.removeFromDestrQueue().' )
      ;
      if( 0.0 > _destrQueueLengthAnimals ) then
        raise exception.Create( 'Number of animals in destruction queue has dropped below 0 in TSMIterationOutput.removeFromDestrQueue().' )
      ;
    end
  ;


  procedure TSMIterationOutput.addToVaccQueue( const herdSize: integer; const day: integer );
    begin
      inc( _vaccQueueLengthUnits );
      _vaccQueueLengthAnimals := _vaccQueueLengthAnimals + herdSize;

      if( _vaccQueueLengthUnits > vacwUMax ) then
        begin
          vacwUMax := _vaccQueueLengthUnits;
          vacwUMaxDay := day;
        end
      ;

      if( _vaccQueueLengthAnimals > vacwAMax ) then
        begin
          vacwAMax := _vaccQueueLengthAnimals;
          vacwAMaxDay := day;
        end
      ;
    end
  ;


  procedure TSMIterationOutput.removeFromVaccQueue( const herdSize: integer );
    begin
      dec( _vaccQueueLengthUnits );
      _vaccQueueLengthAnimals := _vaccQueueLengthAnimals - herdSize;

      if( 0 > _vaccQueueLengthUnits ) then
        raise exception.Create( 'Number of units in vaccination queue has dropped below 0 in TSMIterationOutput.removeFromVaccQueue().' )
      ;
      if( 0.0 > _vaccQueueLengthAnimals ) then
        raise exception.Create( 'Number of animals in vaccination queue has dropped below 0 in TSMIterationOutput.removeFromVaccQueue().' )
      ;      
    end
  ;


  procedure TSMIterationOutput.setProdTypeID( val: integer ); begin _prodTypeID := val; end;
  function TSMIterationOutput.getProdTypeID(): integer; begin result := _prodTypeID; end;


  function TSMIterationOutput.getExpcUAll(): longint;
    begin
      result := expcUDir + expcUInd;
    end
  ;


  function TSMIterationOutput.getExpcAAll(): longint;
    begin
      result := expcADir + expcAInd;
    end
  ;


  function TSMIterationOutput.getAdqcUAll(): longint;
    begin
      result := adqcUAir + adqcUDir + adqcUInd + adqcULcl;
    end
  ;


  function TSMIterationOutput.getAdqcAAll(): longint;
    begin
      result := adqcAAir + adqcADir + adqcAInd + adqcALcl;
    end
  ;

  function TSMIterationOutput.getTrcUDirAll(): longint;
    begin
      result := trcUDirFwd + trcUDirBack;
    end
  ;

  function TSMIterationOutput.getTrcADirAll(): longint;
    begin
      result := trcADirFwd + trcADirBack;
    end
  ;

  function TSMIterationOutput.getTrcUIndAll(): longint;
    begin
      result := trcUIndFwd + trcUIndBack;
    end
  ;

  function TSMIterationOutput.getTrcAIndAll(): longint;
    begin
      result := trcAIndFwd + trcAIndBack;
    end
  ;

  function TSMIterationOutput.getTrcUAll(): longint;
    begin
      result := trcUDirAll + trcUIndAll;
    end
  ;

  function TSMIterationOutput.getTrcAAll(): longint;
    begin
      result := trcADirAll + trcAIndAll;
    end
  ;

  function TSMIterationOutput.getTocUDirAll(): longint;
    begin
      result := tocUDirFwd + tocUDirBack;
    end
  ;

  function TSMIterationOutput.getTocUIndAll(): longint;
    begin
      result := tocUIndFwd + tocUIndBack;
    end
  ;

  function TSMIterationOutput.getTocUAll(): longint;
    begin
      result := tocUDirAll + tocUIndAll;
    end
  ;

  function TSMIterationOutput.getDescUAll(): longint;
    begin
      // DO NOT include initially destroyed units
      //AR:deadDV
      result := descUDet + descUDirFwd + descUIndFwd + descUDirBack + descUIndBack + descURing + descUDcd;
    end
  ;


  function TSMIterationOutput.getDescAAll(): longint;
    begin
      // DO NOT include initially destroyed units
      //AR:deadDV
      result := descADet + descADirFwd + descAIndFwd + descADirBack + descAIndBack + descARing + descADcd;
    end
  ;


  function TSMIterationOutput.getVaccUAll(): longint;
    begin
      // DO NOT include initially vaccinated units
      result := vaccURing;
    end
  ;


  function TSMIterationOutput.getVaccAAll(): longint;
    begin
    // DO NOT include initially vaccinated units
      result := vaccARing;
    end
  ;

  function TSMIterationOutput.getExmcUDirAll(): longint;
    begin
      result := exmcUDirFwd + exmcUDirBack;
    end
  ;

  function TSMIterationOutput.getExmcADirAll(): longint;
    begin
      result := exmcADirFwd + exmcADirBack;
    end
  ;

  function TSMIterationOutput.getExmcUIndAll(): longint;
    begin
      result := exmcUIndFwd + exmcUIndBack;
    end
  ;

  function TSMIterationOutput.getExmcAIndAll(): longint;
    begin
      result := exmcAIndFwd + exmcAIndBack;
    end
  ;

  function TSMIterationOutput.getExmcUAll(): longint;
    begin
      result := exmcUDirAll + exmcUIndAll;
    end
  ;

  function TSMIterationOutput.getExmcAAll(): longint;
    begin
      result := exmcADirAll + exmcAIndAll;
    end
  ;

  function TSMIterationOutput.getTstcUDirAll(): longint;
    begin
      result := tstcUDirFwd + tstcUDirBack;
    end
  ;

  function TSMIterationOutput.getTstcADirAll(): longint;
    begin
      result := tstcADirFwd + tstcADirBack;
    end
  ;

  function TSMIterationOutput.getTstcUIndAll(): longint;
    begin
      result := tstcUIndFwd + tstcUIndBack;
    end
  ;

  function TSMIterationOutput.getTstcAIndAll(): longint;
    begin
      result := tstcAIndFwd + tstcAIndBack;
    end
  ;

  function TSMIterationOutput.getTstcUAll(): longint;
    begin
      result := tstcUDirAll + tstcUIndAll;
    end
  ;

  function TSMIterationOutput.getTstcAAll(): longint;
    begin
      result := tstcADirAll + tstcAIndAll;
    end
  ;

  function TSMIterationOutput.getDeswUTimeAvg(): double;
    begin
      if( 0 = _prodTypeID ) then
        begin
          if( 0.0 > _deswUTimeAvg ) then
            raise exception.Create( 'Average wait time for destruction is not set' )
          ;
          result := _deswUTimeAvg;
        end
      else if( 0.0 = descUAll ) then
        result := 0.0
      else
        result := deswUDaysInQueue / descUAll
      ;
    end
  ;

  function TSMIterationOutput.getVacwUTimeAvg(): double;
    begin
      if( 0 = _prodTypeID ) then
        begin
          if( 0.0 > _vacwUTimeAvg ) then
            raise exception.Create( 'Average wait time for vaccination is not set' )
          ;
          result := _vacwUTimeAvg;
        end
      else if( 0.0 = vaccUAll ) then
        result := 0.0
      else
        result := vacwUDaysInQueue / vaccUAll
      ;
    end
  ;


  procedure TSMIterationOutput.processDestruction( const nAnimals, day, queueDay: integer; const inVaccQueue: boolean );
    var
      unitDaysDestr, animalDaysDestr: integer;
    begin
      unitDaysDestr := day - queueDay;
      animalDaysDestr := unitDaysDestr * nAnimals;

      // Maximum wait for destruction is handled here.
      //----------------------------------------------
      if( unitDaysDestr > deswUTimeMax ) then
        deswUTimeMax := unitDaysDestr
      ;

      // Average time to destruction is handled here,
      // but only indirectly. The actual calculation is made later.
      //-----------------------------------------------------------
      deswUDaysInQueue := deswUDaysInQueue + unitDaysDestr;
      deswADaysInQueue := deswADaysInQueue + animalDaysDestr;

      // Outputs related to maximum queue length are handled by
      // self.addToDestrQueue() via TProductionType.addDestructionQueueEvent().
      //-----------------------------------------------------------------------

      // Average time to vaccination and maximum time for vaccination are NOT handled here,
      // because no vaccination took place.
      //-----------------------------------------------------------------------------------

      // Finally, adjust the running lengths of the destruction queues.
      //---------------------------------------------------------------
      dec( _destrQueueLengthUnits );
      _destrQueueLengthAnimals := _destrQueueLengthAnimals - nAnimals;

      if( 0 > _destrQueueLengthUnits ) then
        raise exception.Create( 'Number of units in destruction queue has dropped below 0 in TSMIterationOutput.processDestruction().' )
      ;
      if( 0.0 > _destrQueueLengthAnimals ) then
        raise exception.Create( 'Number of animals in destruction queue has dropped below 0 in TSMIterationOutput.processDestruction().' )
      ;

      // DO NOT remove this unit from the vaccination queue.
      // That will happen explicitly elsewhere.
    end
  ;


  procedure TSMIterationOutput.processVaccination( const nAnimals, day, queueDay: integer );
    var
      unitDaysVacc, animalDaysVacc: integer;
    begin
      unitDaysVacc := day - queueDay;
      animalDaysVacc := unitDaysVacc * nAnimals;

      // Maximum wait for vaccination is handled here.
      //----------------------------------------------
      if( unitDaysVacc > vacwUTimeMax ) then
        vacwUTimeMax := unitDaysVacc
      ;

      // Average time to vaccination is handled here,
      // but only indirectly. The actual calculation is made later.
      //-----------------------------------------------------------
      vacwUDaysInQueue := vacwUDaysInQueue + unitDaysVacc;
      vacwADaysInQueue := vacwADaysInQueue + animalDaysVacc;

      // Outputs related to maximum queue length are handled by
      //  self.addToVAccQueue() via TProductionType.addDestructionQueueEvent().
      //-----------------------------------------------------------------------

      // Finally, adjust the running length of the vaccination queue.
      // (Leave the destruction queue unchanged.)
      //------------------------------------------------------------
      removeFromVaccQueue( nAnimals );
    end
  ;

  
//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TSMDailyOutput.create();
  	begin
      inherited create();
      initialize();
    end
  ;


  procedure TSMDailyOutput.initialize();
  	begin
      inherited initialize();
      clear();
    end
  ;


  destructor TSMDailyOutput.destroy();
  	begin
    	// There is nothing to do here.
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data handling
//-----------------------------------------------------------------------------
  procedure TSMDailyOutput.setDailyRecordsFrom( const src: TSMDailyOutput );
    begin
      clear();
      addDailyRecordsFrom( src );
    end
  ;


  procedure TSMDailyOutput.setAllRecordsFrom( const src: TSMDailyOutput );
    begin
      clear();
      addDailyRecordsFrom( src );
      addCumulRecordsFrom( src );
    end
  ;


  procedure TSMDailyOutput.addDailyRecordsFrom( const src: TSMDailyOutput );
    begin
      // Daily numbers for each disease state
      inc( self.tsdUSusc, src.tsdUSusc );
      inc( self.tsdASusc, src.tsdASusc );
      inc( self.tsdULat, src.tsdULat );
      inc( self.tsdALat, src.tsdALat );
      inc( self.tsdUSubc, src.tsdUSubc );
      inc( self.tsdASubc, src.tsdASubc );
      inc( self.tsdUClin, src.tsdUClin );
      inc( self.tsdAClin, src.tsdAClin );
      inc( self.tsdUNImm, src.tsdUNImm );
      inc( self.tsdANImm, src.tsdANImm );
      inc( self.tsdUVImm, src.tsdUVImm );
      inc( self.tsdAVImm, src.tsdAVImm );
      inc( self.tsdUDest, src.tsdUDest );
      inc( self.tsdADest, src.tsdADest );
      inc( self.tsdUDead, src.tsdUDead );
      inc( self.tsdADead, src.tsdADead );

      // New daily counts for infections
      inc( self.infnUAll, src.infnUAll );
      inc( self.infnAAll, src.infnAAll );

      // New daily counts for adequate exposures
      inc( self.adqnUAir, src.adqnUAir );
      inc( self.adqnAAir, src.adqnAAir );
      inc( self.adqnUDir, src.adqnUDir );
      inc( self.adqnADir, src.adqnADir );
      inc( self.adqnUInd, src.adqnUInd );
      inc( self.adqnAInd, src.adqnAInd );
      inc( self.adqnULcl, src.adqnULcl );
      inc( self.adqnALcl, src.adqnALcl );

      // New daily counts for detection
      inc( self.detnUClin, src.detnUClin );
      inc( self.detnAClin, src.detnAClin );
      inc( self.detnUTest, src.detnUTest );
      inc( self.detnATest, src.detnATest );
      inc( self.detnUDead, src.detnUDead );
      inc( self.detnADead, src.detnADead );

      //AR:deadDV
      inc( self.detnUDeadD, src.detnUDeadD );
      inc( self.detnADeadD, src.detnADeadD );
      inc( self.detnUDeadV, src.detnUDeadV );
      inc( self.detnADeadV, src.detnADeadV );

      inc( self.detnUAll, src.detnUAll );
      inc( self.detnAAll, src.detnAAll );

      inc( self.detnUDeadAll, src.detnUDeadAll );
      inc( self.detnADeadAll, src.detnADeadAll );

      // New daily counts for herd exams
      inc( self.exmnUAll, src.exmnUAll );
      inc( self.exmnAAll, src.exmnAAll );

      // New daily counts for testing
      inc( self.tstnUTruePos, src.tstnUTruePos );
      inc( self.tstnATruePos, src.tstnATruePos );
      inc( self.tstnUTrueNeg, src.tstnUTrueNeg );
      inc( self.tstnATrueNeg, src.tstnATrueNeg );
      inc( self.tstnUFalsePos, src.tstnUFalsePos );
      inc( self.tstnAFalsePos, src.tstnAFalsePos );
      inc( self.tstnUFalseNeg, src.tstnUFalseNeg );
      inc( self.tstnAFalseNeg, src.tstnAFalseNeg );

      // New daily counts for tracing forward
      inc( self.trnUDirFwd, src.trnUDirFwd );
      inc( self.trnADirFwd, src.trnADirFwd );
      inc( self.trnUIndFwd, src.trnUIndFwd );
      inc( self.trnAIndFwd, src.trnAIndFwd );
      inc( self.trnUDirBack, src.trnUDirBack );
      inc( self.trnADirBack, src.trnADirBack );
      inc( self.trnUIndBack, src.trnUIndBack );
      inc( self.trnAIndBack, src.trnAIndBack );

      // new daily counts for trace origins
      inc( self.tocUDirFwd, src.tocUDirFwd );
      inc( self.tocUIndFwd, src.tocUIndFwd );
      inc( self.tocUDirBack, src.tocUDirBack );
      inc( self.tocUIndBack, src.tocUIndBack );

      // New daily counts for destruction
      inc( self.desnUAll, src.desnUAll );
      inc( self.desnAAll, src.desnAAll );
      inc( self.desnUDcd, src.desnUDcd );
      inc( self.desnADcd, src.desnADcd );
      inc( self.desnUDcdInDestrQueue, src.desnUDcdInDestrQueue );
      inc( self.desnADcdInDestrQueue, src.desnADcdInDestrQueue );

      // New daily counts for vaccination for any reason
      inc( self.vacnUAll, src.vacnUAll );
      inc( self.vacnAAll, src.vacnAAll );

      // New daily counts for zone foci
      inc( self.zonnFoci, src.zonnFoci );

      // Daily counts for death from disease
      inc( self.deadnU, src.deadnU );
      inc( self.deadnA, src.deadnA );
      inc( self.deadnUInDestrQueue, src.deadnUInDestrQueue );
      inc( self.deadnAInDestrQueue, src.deadnAInDestrQueue );


      // Daily number of apparently infectious units
      inc( self.appdUInfectious, src.appdUInfectious );
    end
  ;



  procedure TSMDailyOutput.clearDailyTotals();
    begin
      // Daily numbers for each disease state
      tsdUSusc := 0;
      tsdASusc := 0;
      tsdULat := 0;
      tsdALat := 0;
      tsdUSubc := 0;
      tsdASubc := 0;
      tsdUClin := 0;
      tsdAClin := 0;
      tsdUNImm := 0;
      tsdANImm := 0;
      tsdUVImm := 0;
      tsdAVImm := 0;
      tsdUDest := 0;
      tsdADest := 0;
      tsdUDead := 0;
      tsdADead := 0;

      // Number of apparently infectious units
      appdUInfectious := 0;
    end
  ;


  procedure TSMDailyOutput.clearNewDailyCounts();
    begin
      // New daily counts for infections
      infnUAll := 0;
      infnAAll := 0;

      // New daily counts for adequate exposures
      adqnUAir := 0;
      adqnAAir := 0;
      adqnUDir := 0;
      adqnADir := 0;
      adqnUInd := 0;
      adqnAInd := 0;
      adqnULcl := 0;
      adqnALcl := 0;

      // New daily counts for detection
      detnUClin := 0;
      detnAClin := 0;
      detnUTest := 0;
      detnATest := 0;
      detnUDead := 0;
      detnADead := 0;

      //AR:deadDV
      detnUDeadD := 0;
      detnADeadD := 0;
      detnUDeadV := 0;
      detnADeadV := 0;

      detnUAll := 0;
      detnAAll := 0;

      detnUDeadAll := 0;
      detnADeadAll := 0;

      // New daily counts for herd exams
      exmnUAll := 0;
      exmnAAll := 0;

      // New daily counts for testing
      tstnUTruePos := 0;
      tstnATruePos := 0;
      tstnUTrueNeg := 0;
      tstnATrueNeg := 0;
      tstnUFalsePos := 0;
      tstnAFalsePos := 0;
      tstnUFalseNeg := 0;
      tstnAFalseNeg := 0;

      // New daily counts for tracing forward
      trnUDirFwd := 0;
      trnADirFwd := 0;
      trnUIndFwd := 0;
      trnAIndFwd := 0;
      trnUDirBack := 0;
      trnADirBack := 0;
      trnUIndBack := 0;
      trnAIndBack := 0;

      // new daily counts for trace origins
      tonUDirFwd := 0;
      tonUIndFwd := 0;
      tonUDirBack := 0;
      tonUIndBack := 0;

      // New daily counts for destruction
      desnUAll := 0;
      desnAAll := 0;
      desnUDcd := 0;
      desnADcd := 0;
      desnUDcdInDestrQueue := 0;
      desnADcdInDestrQueue := 0;

      // New daily counts for vaccination for any reason
      vacnUAll := 0;
      vacnAAll := 0;

      // New daily counts for zone foci
      zonnFoci := 0;

      //New daily counts for death from disease
      deadnU := 0;
      deadnA := 0;
      deadnUInDestrQueue := 0;
      deadnAInDestrQueue := 0;
    end
  ;


  procedure TSMDailyOutput.clear();
    begin
      clearDailyTotals();
      clearNewDailyCounts();
      inherited clear();
    end
  ;

  
  procedure TSMDailyOutput.updateDailyCounts(
        const herdAnimalCount: integer;
        const oldState: TNAADSMDiseaseState;
        const newState: TNAADSMDiseaseState;
        const isInDestructionQueue: boolean;
        const day: integer
      );
    begin
      // Decrement the daily number of units in oldState
      // Subtract herdAnimalCount from the daily number of animals in oldState
      // Increment the daily number of units in newState
      // Add the herdAnimalCount to the daily number of animals in newState
      // Increment the running number of units in newState
      // Add the herdAnimalCount to the running number of animals in newState

      decrementDailyCounts( herdAnimalCount, oldState );
      incrementDailyCounts( herdAnimalCount, newState, isInDestructionQueue );
    end
  ;


  procedure TSMDailyOutput.incrementDailyCounts(
        const herdAnimalCount: integer;
        const newState: TNAADSMDiseaseState;
        const isInDestructionQueue: boolean
      );
    begin
      case newState of
        NAADSMStateSusceptible:
          begin
            inc( tsdUSusc );
            inc( tsdASusc, herdAnimalCount );
            inc( tscUSusc );
            inc( tscASusc, herdAnimalCount );
          end
        ;
        NAADSMStateLatent:
          begin
            inc( tsdULat );
            inc( tsdALat, herdAnimalCount );
            inc( tscULat );
            inc( tscALat, herdAnimalCount );
          end
        ;
        NAADSMStateSubclinical:
          begin
            inc( tsdUSubc );
            inc( tsdASubc, herdAnimalCount );
            inc( tscUSubc );
            inc( tscASubc, herdAnimalCount );
          end
        ;
        NAADSMStateClinical:
          begin
            //dbcout2( 'Adding 1 (' + intToStr( herdAnimalCount ) + ') to previously clinical: ' + intToStr( tsdUClin ) );
            inc( tsdUClin );
            inc( tsdAClin, herdAnimalCount );
            inc( tscUClin );
            inc( tscAClin, herdAnimalCount );
          end
        ;
        NAADSMStateNaturallyImmune:
          begin
            inc( tsdUNImm );
            inc( tsdANImm, herdAnimalCount );
            inc( tscUNImm );
            inc( tscANImm, herdAnimalCount );
          end
        ;
        NAADSMStateVaccineImmune:
          begin
            inc( tsdUVImm );
            inc( tsdAVImm, herdAnimalCount );
            inc( tscUVImm );
            inc( tscAVImm, herdAnimalCount );
          end
        ;
        NAADSMStateDestroyed:
          begin
            inc( tsdUDest );
            inc( tsdADest, herdAnimalCount );
            inc( tscUDest );
            inc( tscADest, herdAnimalCount );
          end
        ;
        NAADSMStateDeadFromDisease:
          begin
            inc( tsdUDead );
            inc( tsdADead, herdAnimalCount );
            inc( tscUDead );
            inc( tscADead, herdAnimalCount );
            inc( deadnU );
            inc( deadnA, herdAnimalCount );
            inc( deadcU );
            inc( deadcA, herdAnimalCount );

            if( isInDestructionQueue ) then
              begin
                inc( deadnUInDestrQueue );
                inc( deadnAInDestrQueue, herdAnimalCount );
                inc( deadcUInDestrQueue );
                inc( deadcAInDestrQueue, herdAnimalCount );
              end
            ;
          end
        ;
        else
          raise exception.Create( 'Unrecognized herd status in TSMDailyOutput.incrementDailyCounts()' )
        ;
      end;
    end
  ;


  procedure TSMDailyOutput.decrementDailyCounts( const herdAnimalCount: longint; const oldState: TNAADSMDiseaseState );
    begin
      case oldState of
        NAADSMStateSusceptible:
          begin
            dec( tsdUSusc );
            dec( tsdASusc, herdAnimalCount );
          end
        ;
        NAADSMStateLatent:
          begin
            dec( tsdULat );
            dec( tsdALat, herdAnimalCount );
          end
        ;
        NAADSMStateSubclinical:
          begin
            dec( tsdUSubc );
            dec( tsdASubc, herdAnimalCount );
          end
        ;
        NAADSMStateClinical:
          begin
            //dbcout2( 'Subtracting 1 (' + intToStr( herdAnimalCount ) + ') from previously clinical: ' + intToStr( self.tsdUClin ) );
            dec( tsdUClin );
            dec( tsdAClin, herdAnimalCount );
          end
        ;
        NAADSMStateNaturallyImmune:
          begin
            dec( tsdUNImm );
            dec( tsdANImm, herdAnimalCount );
          end
        ;
        NAADSMStateVaccineImmune:
          begin
            dec( tsdUVImm );
            dec( tsdAVImm, herdAnimalCount );
          end
        ;
        NAADSMStateDestroyed:
          raise exception.Create( 'Number of destroyed herds is going down!  This can''t happen!' )
        ;
        NAADSMStateDeadFromDisease:
          begin
            // The number of herds/animals dead from disease can decrease as they are "destroyed" (subjected to DCD)
            dec( tsdUDead );
            dec( tsdADead, herdAnimalCount );
          end
        ;
        else
          raise exception.Create( 'Unrecognized disease state in TProductionType.makeDailyRecord' )
        ;
      end;

      if
        ( 0 > tsdUSusc )
      or
        ( 0 > tsdASusc )
      or
        ( 0 > tsdULat )
      or
        ( 0 > tsdALat )
      or
        ( 0 > tsdUSubc )
      or
        ( 0 > tsdASubc )
      or
        ( 0 > tsdUClin )
      or
        ( 0 > tsdAClin )
      or
        ( 0 > tsdUNImm )
      or
        ( 0 > tsdANImm )
      or
        ( 0 > tsdUVImm )
      or
        ( 0 > tsdAVImm )
      or
        ( 0 > tsdUDest )
      or
        ( 0 > tsdADest )
      or
        ( 0 > tsdUDead )
      or
        ( 0 > tsdADead )
      then
        begin
          dbcout( 'tsdUSusc:' + intToStr( tsdUSusc ), true );
          dbcout( 'tsdASusc:' + intToStr( tsdASusc ), true );

          dbcout( 'tsdULat:' + intToStr( tsdULat ), true );
          dbcout( 'tsdALat:' + intToStr( tsdALat ), true );

          dbcout( 'tsdUSubc:' + intToStr( tsdUSubc ), true );
          dbcout( 'tsdASubc:' + intToStr( tsdASubc ), true );

          dbcout( 'tsdUClin:' + intToStr( tsdUClin ), true );
          dbcout( 'tsdAClin:' + intToStr( tsdAClin ), true );

          dbcout( 'tsdUNImm:' + intToStr( tsdUNImm ), true );
          dbcout( 'tsdANImm:' + intToStr( tsdANImm ), true );

          dbcout( 'tsdUVImm:' + intToStr( tsdUVImm ), true );
          dbcout( 'tsdAVImm:' + intToStr( tsdAVImm ), true );

          dbcout( 'tsdUDest:' + intToStr( tsdUDest ), true );
          dbcout( 'tsdADest:' + intToStr( tsdADest ), true );

          dbcout( 'tsdUDead:' + intToStr( tsdUDead ), true );
          dbcout( 'tsdADead:' + intToStr( tsdADead ), true );

          raise exception.create( 'Number of units or animals in transition state is less than 0' );
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// SpreadModel-specific functions
//-----------------------------------------------------------------------------
  procedure TSMDailyOutput.insertDatabaseOutputs(
        drt: TDailyRecordType;
      	db: TSMDatabase;
        ptID: integer;
        iteration: integer;
        day: integer = 0
      );
    var
      qDict: TQueryDictionary;
      table: string;
      q: string;
    begin
      qDict := TQueryDictionary.create();

      if( DRTDaily = drt ) then
        begin
          table := 'outDailyByProductionType';

          // These fields are only in the daily output table
          //-------------------------------------------------
          qDict['day'] := intToStr( day );

          // Daily numbers for each disease state
          qDict['tsdUSusc'] := intToStr( tsdUSusc );
          qDict['tsdASusc'] := intToStr( tsdASusc );
          qDict['tsdULat']  := intToStr( tsdULat );
          qDict['tsdALat']  := intToStr( tsdALat );
          qDict['tsdUSubc'] := intToStr( tsdUSubc );
          qDict['tsdASubc'] := intToStr( tsdASubc );
          qDict['tsdUClin'] := intToStr( tsdUClin );
          qDict['tsdAClin'] := intToStr( tsdAClin );
          qDict['tsdUNImm'] := intToStr( tsdUNImm );
          qDict['tsdANImm'] := intToStr( tsdANImm );
          qDict['tsdUVImm'] := intToStr( tsdUVImm );
          qDict['tsdAVImm'] := intToStr( tsdAVImm );
          qDict['tsdUDest'] := intToStr( tsdUDest );
          qDict['tsdADest'] := intToStr( tsdADest );
          qDict['tsdUDead'] := intToStr( tsdUDead );
          qDict['tsdADead'] := intToStr( tsdADead );

          // New daily counts for infections
          qDict['infnUAll'] := intToStr( infnUAll );
          qDict['infnAAll'] := intToStr( infnAAll );

          // New daily counts for adequate exposures
          qDict['adqnUAir'] := intToStr( adqnUAir );
          qDict['adqnAAir'] := intToStr( adqnAAir );
          qDict['adqnUDir'] := intToStr( adqnUDir );
          qDict['adqnADir'] := intToStr( adqnADir );
          qDict['adqnUInd'] := intToStr( adqnUInd );
          qDict['adqnAInd'] := intToStr( adqnAInd );
          qDict['adqnULcl'] := intToStr( adqnULcl );
          qDict['adqnALcl'] := intToStr( adqnALcl );

          // New daily counts for detection
          qDict['detnUClin'] := intToStr( detnUClin );
          qDict['detnAClin'] := intToStr( detnAClin );
          qDict['detnUTest'] := intToStr( detnUTest );
          qDict['detnATest'] := intToStr( detnATest );
          qDict['detnUDead'] := intToStr( detnUDead );
          qDict['detnADead'] := intToStr( detnADead );

          //AR:deadDV
          qDict['detnUDeadD'] := intToStr( detnUDeadD );
          qDict['detnADeadD'] := intToStr( detnADeadD );
          qDict['detnUDeadV'] := intToStr( detnUDeadV );
          qDict['detnADeadV'] := intToStr( detnADeadV );

          qDict['detnUAll'] := intToStr( detnUAll );
          qDict['detnAAll'] := intToStr( detnAAll );

          qDict['detnUDeadAll'] := intToStr( detnUDeadAll );
          qDict['detnADeadAll'] := intToStr( detnADeadAll );

          // New daily counts for herd exams
          qDict['exmnUAll'] := intToStr( exmnUAll );
          qDict['exmnAAll'] := intToStr( exmnAAll );

          // New daily counts for testing
          qDict['tstnUTruePos'] := intToStr( tstnUTruePos );
          qDict['tstnATruePos'] := intToStr( tstnATruePos );
          qDict['tstnUTrueNeg'] := intToStr( tstnUTrueNeg );
          qDict['tstnATrueNeg'] := intToStr( tstnATrueNeg );
          qDict['tstnUFalsePos'] := intToStr( tstnUFalsePos );
          qDict['tstnAFalsePos'] := intToStr( tstnAFalsePos );
          qDict['tstnUFalseNeg'] := intToStr( tstnUFalseNeg );
          qDict['tstnAFalseNeg'] := intToStr( tstnAFalseNeg );

          // New daily counts for tracing
          qDict['trnUDirFwd'] := intToStr( trnUDirFwd );
          qDict['trnADirFwd'] := intToStr( trnADirFwd );
          qDict['trnUIndFwd'] := intToStr( trnUIndFwd );
          qDict['trnAIndFwd'] := intToStr( trnAIndFwd );
          qDict['trnUDirBack'] := intToStr( trnUDirBack );
          qDict['trnADirBack'] := intToStr( trnADirBack );
          qDict['trnUIndBack'] := intToStr( trnUIndBack );
          qDict['trnAIndBack'] := intToStr( trnAIndBack );

          // new daily counts for trace origins
          qDict['tonUDirFwd'] := intToStr( tonUDirFwd );
          qDict['tonUIndFwd'] := intToStr( tonUIndFwd );
          qDict['tonUDirBack'] := intToStr( tonUDirBack );
          qDict['tonUIndBack'] := intToStr( tonUIndBack );

          // New daily counts for destruction
          qDict['desnUAll'] := intToStr( desnUAll );
          qDict['desnAAll'] := intToStr( desnAAll );
          qDict['desnUDcd'] := intToStr( desnUDcd );
          qDict['desnADcd'] := intToStr( desnADcd );
          qDict['desnUDcdInDestrQueue'] := intToStr( desnUDcdInDestrQueue );
          qDict['desnADcdInDestrQueue'] := intToStr( desnADcdInDestrQueue );

          // Length of the destruction queue
          qDict['deswUAll'] := intToStr( destrQueueLengthUnits );
          qDict['deswAAll'] := usFloatToStr( destrQueueLengthAnimals );

          // New daily counts for vaccination for any reason
          qDict['vacnUAll'] := intToStr( vacnUAll );
          qDict['vacnAAll'] := intToStr( vacnAAll );

          // Length of the vaccination queue
          qDict['vacwUAll'] := intToStr( vaccQueueLengthUnits );
          qDict['vacwAAll'] := usFloatToStr( vaccQueueLengthAnimals );

          // New daily counts for zone foci
          qDict['zonnFoci'] := intToStr( zonnFoci );

          // New daily counts for death due to disease
          qDict['deadnU'] := intToStr( deadnU );
          qDict['deadnA'] := intToStr( deadnA );
          qDict['deadnUInDestrQueue'] := intToStr( deadnUInDestrQueue );
          qDict['deadnAInDestrQueue'] := intToStr( deadnAInDestrQueue );

          // Number of apparently infectious units on this day
          qDict['appdUInfectious'] := intToStr( appdUInfectious );
        end
      else if( DRTIteration = drt ) then
        begin
          table := 'outIterationByProductionType';

          // These fields are only in the iteration output table
          //----------------------------------------------------
          // Destruction queue outputs
          qDict['deswUMax'] := intToStr( deswUMax );
          qDict['deswAMax'] := usFloatToStr( deswAMax );
          qDict['deswUMaxDay'] := intToStr( deswUMaxDay );
          qDict['deswAMaxDay'] := intToStr( deswAMaxDay );
          qDict['deswUTimeMax'] := intToStr( deswUTimeMax );
          qDict['deswUTimeAvg'] := usFloatToStr( deswUTimeAvg );
          qDict['deswUDaysInQueue'] := usFloatToStr( deswUDaysInQueue );
          qDict['deswADaysInQueue'] := usFloatToStr( deswADaysInQueue );

          // Vaccination queue outputs
          qDict['vacwUMax'] := intToStr( vacwUMax );
          qDict['vacwAMax'] := usFloatToStr( vacwAMax );
          qDict['vacwUMaxDay'] := intToStr( vacwUMaxDay );
          qDict['vacwAMaxDay'] := intToStr( vacwAMaxDay );
          qDict['vacwUTimeMax'] := intToStr( vacwUTimeMax );
          qDict['vacwUTimeAvg'] := usFloatToStr( vacwUTimeAvg );
          // For NAADSM 3.x, and for at least the time being, these two outputs are no longer saved.
          //qDict['vacwUDaysInQueue'] := usFloatToStr( vacwUDaysInQueue );
          //qDict['vacwADaysInQueue'] := usFloatToStr( vacwADaysInQueue );
          
          // First events
          if( -1 <> firstDetection ) then qDict['firstDetection']     := intToStr( firstDetection );
          if( -1 <> firstDetUInf ) then qDict['firstDetUInf']         := intToStr( firstDetUInf );
          if( -1 <> firstDetAInf ) then qDict['firstDetAInf']         := intToStr( firstDetAInf );
          
          if( -1 <> firstDestruction ) then qDict['firstDestruction'] := intToStr( firstDestruction );
          if( -1 <> firstVaccination ) then qDict['firstVaccination'] := intToStr( firstVaccination );
          if( -1 <> lastDetection ) then qDict['lastDetection'] := intToStr( lastDetection );
        end
      else
        raise exception.Create( 'Unrecognized TDailyRecordType in TSMDailyOutput.updateDatabase' )
      ;

      // These fields are in both the daily and iteration output tables
      //----------------------------------------------------------------
      qDict['iteration'] := intToStr( iteration ); // Remember that iterations are 1-indexed in the database.  (This is taken care of elsewhere.)
      qDict['productionTypeID'] := intToStr( ptID );

      // Running totals for each disease state
      qDict['tscUSusc'] := intToStr( tscUSusc );
      qDict['tscASusc'] := intToStr( tscASusc );
      qDict['tscULat']  := intToStr( tscULat );
      qDict['tscALat']  := intToStr( tscALat );
      qDict['tscUSubc'] := intToStr( tscUSubc );
      qDict['tscASubc'] := intToStr( tscASubc );
      qDict['tscUClin'] := intToStr( tscUClin );
      qDict['tscAClin'] := intToStr( tscAClin );
      qDict['tscUNImm'] := intToStr( tscUNImm );
      qDict['tscANImm'] := intToStr( tscANImm );
      qDict['tscUVImm'] := intToStr( tscUVImm );
      qDict['tscAVImm'] := intToStr( tscAVImm );
      qDict['tscUDest'] := intToStr( tscUDest );
      qDict['tscADest'] := intToStr( tscADest );
      qDict['tscUDead'] := intToStr( tscUDead );
      qDict['tscADead'] := intToStr( tscADead );

      // Running totals for infections
      qDict['infcUIni'] := intToStr( infcUIni );
      qDict['infcAIni'] := intToStr( infcAIni );
      qDict['infcUAll'] := intToStr( infcUAll );
      qDict['infcAAll'] := intToStr( infcAAll );

      // Running totals for causes of adequate exposure.
      qDict['adqcUAir'] := intToStr( adqcUAir );
      qDict['adqcAAir'] := intToStr( adqcAAir );
      qDict['adqcUDir'] := intToStr( adqcUDir );
      qDict['adqcADir'] := intToStr( adqcADir );
      qDict['adqcUInd'] := intToStr( adqcUInd );
      qDict['adqcAInd'] := intToStr( adqcAInd );
      qDict['adqcULcl'] := intToStr( adqcULcl );
      qDict['adqcALcl'] := intToStr( adqcALcl );

      // Running totals for exposures
      qDict['expcUDir'] := intToStr( expcUDir );
      qDict['expcADir'] := intToStr( expcADir );
      qDict['expcUInd'] := intToStr( expcUInd );
      qDict['expcAInd'] := intToStr( expcAInd );

      // Running totals for traces
      qDict['trcUDirFwd']  := intToStr( trcUDirFwd );
      qDict['trcADirFwd']  := intToStr( trcADirFwd );
      qDict['trcUIndFwd']  := intToStr( trcUIndFwd );
      qDict['trcAIndFwd']  := intToStr( trcAIndFwd );
      qDict['trcUDirpFwd'] := intToStr( trcUDirpFwd );
      qDict['trcADirpFwd'] := intToStr( trcADirpFwd );
      qDict['trcUIndpFwd'] := intToStr( trcUIndpFwd );
      qDict['trcAIndpFwd'] := intToStr( trcAIndpFwd );
      qDict['trcUDirBack'] := intToStr( trcUDirBack );
      qDict['trcADirBack'] := intToStr( trcADirBack );
      qDict['trcUIndBack'] := intToStr( trcUIndBack );
      qDict['trcAIndBack'] := intToStr( trcAIndBack );
      qDict['trcUDirpBack'] := intToStr( trcUDirpBack );
      qDict['trcADirpBack'] := intToStr( trcADirpBack );
      qDict['trcUIndpBack'] := intToStr( trcUIndpBack );
      qDict['trcAIndpBack'] := intToStr( trcAIndpBack );

      // Running totals for trace origins
      qDict['tocUDirFwd'] := intToStr( tocUDirFwd );
      qDict['tocUIndFwd'] := intToStr( tocUIndFwd );
      qDict['tocUDirBack'] := intToStr( tocUDirBack );
      qDict['tocUIndBack'] := intToStr( tocUIndBack );

      // Running totals for detection
      qDict['detcUClin'] := intToStr( detcUClin );
      qDict['detcAClin'] := intToStr( detcAClin );
      qDict['detcUTest'] := intToStr( detcUTest );
      qDict['detcATest'] := intToStr( detcATest );
      qDict['detcUDead'] := intToStr( detcUDead );
      qDict['detcADead'] := intToStr( detcADead );

      //AR:deadDV
      qDict['detcUDeadD'] := intToStr( detcUDeadD );
      qDict['detcADeadD'] := intToStr( detcADeadD );
      qDict['detcUDeadV'] := intToStr( detcUDeadV );
      qDict['detcADeadV'] := intToStr( detcADeadV );

      qDict['detcUAll'] := intToStr( detcUAll );
      qDict['detcAAll'] := intToStr( detcAAll );

      qDict['detcUDeadAll'] := intToStr( detcUDeadAll );
      qDict['detcADeadAll'] := intToStr( detcADeadAll );

      // Running totals for destruction
      qDict['descUIni']  := intToStr( descUIni );
      qDict['descAIni']  := intToStr( descAIni );
      qDict['descUDet']  := intToStr( descUDet );
      qDict['descADet']  := intToStr( descADet );
      qDict['descUDirFwd']  := intToStr( descUDirFwd );
      qDict['descADirFwd']  := intToStr( descADirFwd );
      qDict['descUIndFwd']  := intToStr( descUIndFwd );
      qDict['descAIndFwd']  := intToStr( descAIndFwd );
      qDict['descUDirBack'] := intToStr( descUDirBack );
      qDict['descADirBack'] := intToStr( descADirBack );
      qDict['descUIndBack'] := intToStr( descUIndBack );
      qDict['descAIndBack'] := intToStr( descAIndBack );
      qDict['descURing'] := intToStr( descURing );
      qDict['descARing'] := intToStr( descARing );

      //AR:deadDV
      qDict['descUDcd'] := intToStr( descUDcd );
      qDict['descADcd'] := intToStr( descADcd );
      qDict['descUDcdInDestrQueue'] := intToStr( descUDcdInDestrQueue );
      qDict['descADcdInDestrQueue'] := intToStr( descADcdInDestrQueue );

      // Running totals for vaccination
      qDict['vaccUIni'] := intToStr( vaccUIni );
      qDict['vaccAIni'] := intToStr( vaccAIni );
      qDict['vaccURing'] := intToStr( vaccURing );
      qDict['vaccARing'] := intToStr( vaccARing );

      // Running totals for herd exams
      qDict['exmcUDirFwd'] := intToStr( exmcUDirFwd );
      qDict['exmcADirFwd'] := intToStr( exmcADirFwd );
      qDict['exmcUIndFwd'] := intToStr( exmcUIndFwd );
      qDict['exmcAIndFwd'] := intToStr( exmcAIndFwd );
      qDict['exmcUDirBack'] := intToStr( exmcUDirBack );
      qDict['exmcADirBack'] := intToStr( exmcADirBack );
      qDict['exmcUIndBack'] := intToStr( exmcUIndBack );
      qDict['exmcAIndBack'] := intToStr( exmcAIndBack );

      // Running totals for diagnostic testing
      qDict['tstcUDirFwd'] := intToStr( tstcUDirFwd );
      qDict['tstcADirFwd'] := intToStr( tstcADirFwd );
      qDict['tstcUIndFwd'] := intToStr( tstcUIndFwd );
      qDict['tstcAIndFwd'] := intToStr( tstcAIndFwd );
      qDict['tstcUDirBack'] := intToStr( tstcUDirBack );
      qDict['tstcADirBack'] := intToStr( tstcADirBack );
      qDict['tstcUIndBack'] := intToStr( tstcUIndBack );
      qDict['tstcAIndBack'] := intToStr( tstcAIndBack );
      qDict['tstcUTruePos'] := intToStr( tstcUTruePos );
      qDict['tstcATruePos'] := intToStr( tstcATruePos );
      qDict['tstcUTrueNeg'] := intToStr( tstcUTrueNeg );
      qDict['tstcATrueNeg'] := intToStr( tstcATrueNeg );
      qDict['tstcUFalsePos'] := intToStr( tstcUFalsePos );
      qDict['tstcAFalsePos'] := intToStr( tstcAFalsePos );
      qDict['tstcUFalseNeg'] := intToStr( tstcUFalseNeg );
      qDict['tstcAFalseNeg'] := intToStr( tstcAFalseNeg );

      // Running totals for zone foci
      qDict['zoncFoci'] := intToStr( zoncFoci );

      // Running totals for death due to disease
      qDict['deadcUIni']  := intToStr( deadcUIni );
      qDict['deadcAIni']  := intToStr( deadcAIni );
      qDict['deadcU']  := intToStr( deadcU );
      qDict['deadcA']  := intToStr( deadcA );
      qDict['deadcUInDestrQueue'] := intToStr( deadcUInDestrQueue );
      qDict['deadcAInDestrQueue'] := intToStr( deadcAInDestrQueue );


      // Outputs need to be stored at least temporarily in the local database,
      // even if the user wants to record outputs in a remote database.
      // The query has to be written twice: once for each database.
      q := sqlclasses.writeQuery( table, QInsert, qDict );

      db.execute( q );

      if
        ( ( DRTDaily = drt ) and ( db.saveAllDailyOutputs() ) )
      or
        ( DRTIteration = drt )
      then
        begin
          if( remoteDBParams.useRemoteDatabase ) then
            begin
              qDict['jobID'] := intToStr( remoteDBParams.jobID );
              q := sqlclasses.writeQuery( table, QInsert, qDict );
              db.remoteExecute( q );
            end
          ;
        end
      ;

      qDict.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
	procedure TSMDailyOutput.debug();
  	begin
    	dbcout( '----------------begin TSMDailyOutput', true );

      // Daily numbers for each disease state
      dbcout( 'tsdUSusc: ' + intToStr( tsdUSusc ), true );
      dbcout( 'tsdASusc: ' + intToStr( tsdASusc ), true );
      dbcout( 'tsdULat: ' + intToStr( tsdULat ), true );
      dbcout( 'tsdALat: ' + intToStr( tsdALat ), true );
      dbcout( 'tsdUSubc: ' + intToStr( tsdUSubc ), true );
      dbcout( 'tsdASubc: ' + intToStr( tsdASubc ), true );
      dbcout( 'tsdUClin: ' + intToStr( tsdUClin ), true );
      dbcout( 'tsdAClin: ' + intToStr( tsdAClin ), true );
      dbcout( 'tsdUNImm: ' + intToStr( tsdUNImm ), true );
      dbcout( 'tsdANImm: ' + intToStr( tsdANImm ), true );
      dbcout( 'tsdUVImm: ' + intToStr( tsdUVImm ), true );
      dbcout( 'tsdAVImm: ' + intToStr( tsdAVImm ), true );
      dbcout( 'tsdUDest: ' + intToStr( tsdUDest ), true );
      dbcout( 'tsdADest: ' + intToStr( tsdADest ), true );
      dbcout( 'tsdUDead: ' + intToStr( tsdUDead ), true );
      dbcout( 'tsdADead: ' + intToStr( tsdADead ), true );

      // Running totals for each disease state
      dbcout( 'tscUSusc: ' + intToStr( tscUSusc ), true );
      dbcout( 'tscASusc: ' + intToStr( tscASusc ), true );
      dbcout( 'tscULat: ' + intToStr( tscULat ), true );
      dbcout( 'tscALat: ' + intToStr( tscALat ), true );
      dbcout( 'tscUSubc: ' + intToStr( tscUSubc ), true );
      dbcout( 'tscASubc: ' + intToStr( tscASubc ), true );
      dbcout( 'tscUClin: ' + intToStr( tscUClin ), true );
      dbcout( 'tscAClin: ' + intToStr( tscAClin ), true );
      dbcout( 'tscUNImm: ' + intToStr( tscUNImm ), true );
      dbcout( 'tscANImm: ' + intToStr( tscANImm ), true );
      dbcout( 'tscUVImm: ' + intToStr( tscUVImm ), true );
      dbcout( 'tscAVImm: ' + intToStr( tscAVImm ), true );
      dbcout( 'tscUDest: ' + intToStr( tscUDest ), true );
      dbcout( 'tscADest: ' + intToStr( tscADest ), true );
      dbcout( 'tscUDead: ' + intToStr( tscUDead ), true );
      dbcout( 'tscADead: ' + intToStr( tscADead ), true );

      // Running totals for infections
      dbcout( 'infcUIni: ' + intToStr( infcUIni ), true );
      dbcout( 'infcAIni: ' + intToStr( infcAIni ), true );
      dbcout( 'infcUAll: ' + intToStr( infcUAll ), true );
      dbcout( 'infcAAll: ' + intToStr( infcAAll ), true );

      // New daily counts for intections
      dbcout( 'infnUAll: ' + intToStr( infnUAll ), true );
      dbcout( 'infnAAll: ' + intToStr( infnAAll ), true );

      // Running totals for causes of adequate exposure
      dbcout( 'adqcUAir: ' + intToStr( adqcUAir ), true );
      dbcout( 'adqcAAir: ' + intToStr( adqcAAir ), true );
      dbcout( 'adqcUDir: ' + intToStr( adqcUDir ), true );
      dbcout( 'adqcADir: ' + intToStr( adqcADir ), true );
      dbcout( 'adqcUInd: ' + intToStr( adqcUInd ), true );
      dbcout( 'adqcAInd: ' + intToStr( adqcAInd ), true );
      dbcout( 'adqcULcl: ' + intToStr( adqcULcl ), true );
      dbcout( 'adqcALcl: ' + intToStr( adqcALcl ), true );

      // New daily counts for causes of adequate exposure
      dbcout( 'adqnUAir: ' + intToStr( adqnUAir ), true );
      dbcout( 'adqnAAir: ' + intToStr( adqnAAir ), true );
      dbcout( 'adqnUDir: ' + intToStr( adqnUDir ), true );
      dbcout( 'adqnADir: ' + intToStr( adqnADir ), true );
      dbcout( 'adqnUInd: ' + intToStr( adqnUInd ), true );
      dbcout( 'adqnAInd: ' + intToStr( adqnAInd ), true );
      dbcout( 'adqnULcl: ' + intToStr( adqnULcl ), true );
      dbcout( 'adqnALcl: ' + intToStr( adqnALcl ), true );

      // Running totals for exposures
      dbcout( 'expcUDir: ' + intToStr( expcUDir ), true );
      dbcout( 'expcADir: ' + intToStr( expcADir ), true );
      dbcout( 'expcUInd: ' + intToStr( expcUInd ), true );
      dbcout( 'expcAInd: ' + intToStr( expcAInd ), true );

      // Running totals for traces
      dbcout( 'trcUDirFwd: ' + intToStr( trcUDirFwd ), true );
      dbcout( 'trcADirFwd: ' + intToStr( trcADirFwd ), true );
      dbcout( 'trcUIndFwd: ' + intToStr( trcUIndFwd ), true );
      dbcout( 'trcAIndFwd: ' + intToStr( trcAIndFwd ), true );
      dbcout( 'trcUDirpFwd: ' + intToStr( trcUDirpFwd ), true );
      dbcout( 'trcADirpFwd: ' + intToStr( trcADirpFwd ), true );
      dbcout( 'trcUIndpFwd: ' + intToStr( trcUIndpFwd ), true );
      dbcout( 'trcAIndpFwd: ' + intToStr( trcAIndpFwd ), true );
      dbcout( 'trcUDirBack: ' + intToStr( trcUDirBack ), true );
      dbcout( 'trcADirBack: ' + intToStr( trcADirBack ), true );
      dbcout( 'trcUIndBack: ' + intToStr( trcUIndBack ), true );
      dbcout( 'trcAIndBack: ' + intToStr( trcAIndBack ), true );
      dbcout( 'trcUDirpBack: ' + intToStr( trcUDirpBack ), true );
      dbcout( 'trcADirpBack: ' + intToStr( trcADirpBack ), true );
      dbcout( 'trcUIndpBack: ' + intToStr( trcUIndpBack ), true );
      dbcout( 'trcAIndpBack: ' + intToStr( trcAIndpBack ), true );

      // Running totals for trace origins
      dbcout( 'tocUDirFwd: ' + intToStr( tocUDirFwd ), true );
      dbcout( 'tocUIndFwd: ' + intToStr( tocUIndFwd ), true );
      dbcout( 'tocUDirBack: ' + intToStr( tocUDirBack ), true );
      dbcout( 'tocUIndBack: ' + intToStr( tocUIndBack ), true );

      // New daily counts for detection
      dbcout( 'detnUClin: ' + intToStr( detnUClin ), true );
      dbcout( 'detnAClin: ' + intToStr( detnAClin ), true );
      dbcout( 'detnUTest: ' + intToStr( detnUTest ), true );
      dbcout( 'detnATest: ' + intToStr( detnATest ), true );
      dbcout( 'detnUDead: ' + intToStr( detnUDead ), true );
      dbcout( 'detnADead: ' + intToStr( detnADead ), true );

      //AR:deadDV
      dbcout( 'detnUDeadD: ' + intToStr( detnUDeadD ), true );
      dbcout( 'detnADeadD: ' + intToStr( detnADeadD ), true );
      dbcout( 'detnUDeadV: ' + intToStr( detnUDeadV ), true );
      dbcout( 'detnADeadV: ' + intToStr( detnADeadV ), true );

      dbcout( 'detnUAll: ' + intToStr( detnUAll ), true );
      dbcout( 'detnAAll: ' + intToStr( detnAAll ), true );

      dbcout( 'detnUDeadAll: ' + intToStr( detnUDeadAll ), true );
      dbcout( 'detnADeadAll: ' + intToStr( detnADeadAll ), true );

      // New daily counts for herd exams
      dbcout( 'exmnUAll: ' + intToStr( exmnUAll ), true );
      dbcout( 'exmnAAll: ' + intToStr( exmnAAll ), true );

      // New daily counts for testing
      dbcout( 'tstnUTruePos: ' + intToStr( tstnUTruePos ), true );
      dbcout( 'tstnATruePos: ' + intToStr( tstnATruePos ), true );
      dbcout( 'tstnUTrueNeg: ' + intToStr( tstnUTrueNeg ), true );
      dbcout( 'tstnATrueNeg: ' + intToStr( tstnATrueNeg ), true );
      dbcout( 'tstnUFalsePos: ' + intToStr( tstnUFalsePos ), true );
      dbcout( 'tstnAFalsePos: ' + intToStr( tstnAFalsePos ), true );
      dbcout( 'tstnUFalseNeg: ' + intToStr( tstnUFalseNeg ), true );
      dbcout( 'tstnAFalseNeg: ' + intToStr( tstnAFalseNeg ), true );

      // New daily counts for traces
      dbcout( 'trnUDirFwd: ' + intToStr( trnUDirFwd ), true );
      dbcout( 'trnADirFwd: ' + intToStr( trnADirFwd ), true );
      dbcout( 'trnUIndFwd: ' + intToStr( trnUIndFwd ), true );
      dbcout( 'trnAIndFwd: ' + intToStr( trnAIndFwd ), true );
      dbcout( 'trnUDirBack: ' + intToStr( trnUDirBack ), true );
      dbcout( 'trnADirBack: ' + intToStr( trnADirBack ), true );
      dbcout( 'trnUIndBack: ' + intToStr( trnUIndBack ), true );
      dbcout( 'trnAIndBack: ' + intToStr( trnAIndBack ), true );

      // new daily counts for trace origins
      dbcout( 'tocUDirFwd: ' + intToStr( tocUDirFwd ), true );
      dbcout( 'tocUIndFwd: ' + intToStr( tocUIndFwd ), true );
      dbcout( 'tocUDirBack: ' + intToStr( tocUDirBack ), true );
      dbcout( 'tocUIndBack: ' + intToStr( tocUIndBack ), true );

      // New daily counts for destruction
      dbcout( 'desnUAll: ' + intToStr( desnUAll ), true );
      dbcout( 'desnAAll: ' + intToStr( desnAAll ), true );
      dbcout( 'desnUDcd: ' + intToStr( desnUDcd ), true );
      dbcout( 'desnADcd: ' + intToStr( desnADcd ), true );
      dbcout( 'desnUDcdInDestrQueue: ' + intToStr( desnUDcdInDestrQueue ), true );
      dbcout( 'desnADcdInDestrQueue: ' + intToStr( desnADcdInDestrQueue ), true );

      // New daily counts for vaccination for any reason
      dbcout( 'vacnUAll: ' + intToStr( vacnUAll ), true );
      dbcout( 'vacnAAll: ' + intToStr( vacnAAll ), true );

      // New daily counts for death due to disease
      dbcout( 'deadnU: ' + intToStr( deadnU ), true );
      dbcout( 'deadnA: ' + intToStr( deadnA ), true );
      dbcout( 'deadnUInDestrQueue: ' + intToStr( deadnUInDestrQueue ), true );
      dbcout( 'deadnAInDestrQueue: ' + intToStr( deadnAInDestrQueue ), true );

      // Running totals for detection
      dbcout( 'detcUClin: ' + intToStr( detcUClin ), true );
      dbcout( 'detcAClin: ' + intToStr( detcAClin ), true );
      dbcout( 'detcUTest: ' + intToStr( detcUTest ), true );
      dbcout( 'detcATest: ' + intToStr( detcATest ), true );
      dbcout( 'detcUDead: ' + intToStr( detcUDead ), true );
      dbcout( 'detcADead: ' + intToStr( detcADead ), true );

      //AR:deadDV
      dbcout( 'detcUDeadD: ' + intToStr( detcUDeadD ), true );
      dbcout( 'detcADeadD: ' + intToStr( detcADeadD ), true );
      dbcout( 'detcUDeadV: ' + intToStr( detcUDeadV ), true );
      dbcout( 'detcADeadV: ' + intToStr( detcADeadV ), true );

      dbcout( 'detcUAll: ' + intToStr( detcUAll ), true );
      dbcout( 'detcAAll: ' + intToStr( detcAAll ), true );

      dbcout( 'detcUDeadAll: ' + intToStr( detcUDeadAll ), true );
      dbcout( 'detcADeadAll: ' + intToStr( detcADeadAll ), true );

      // Running totals for destruction
      dbcout( 'descUIni: ' + intToStr( descUIni ), true );
      dbcout( 'descAIni: ' + intToStr( descAIni ), true );
      dbcout( 'descUDet: ' + intToStr( descUDet ), true );
      dbcout( 'descADet: ' + intToStr( descADet ), true );
      dbcout( 'descUDirFwd: ' + intToStr( descUDirFwd ), true );
      dbcout( 'descADirFwd: ' + intToStr( descADirFwd ), true );
      dbcout( 'descUIndFwd: ' + intToStr( descUIndFwd ), true );
      dbcout( 'descAIndFwd: ' + intToStr( descAIndFwd ), true );
      dbcout( 'descUDirBack: ' + intToStr( descUDirBack ), true );
      dbcout( 'descADirBack: ' + intToStr( descADirBack ), true );
      dbcout( 'descUIndBack: ' + intToStr( descUIndBack ), true );
      dbcout( 'descAIndBack: ' + intToStr( descAIndBack ), true );
      dbcout( 'descURing: ' + intToStr( descURing ), true );
      dbcout( 'descARing: ' + intToStr( descARing ), true );

      //AR:deadDV
      dbcout( 'descUDcd: ' + intToStr( descUDcd ), true );
      dbcout( 'descADcd: ' + intToStr( descADcd ), true );
      dbcout( 'descUDcdInDestrQueue: ' + intToStr( descUDcdInDestrQueue ), true );
      dbcout( 'descADcdInDestrQueue: ' + intToStr( descADcdInDestrQueue ), true );

      // Running totals for vaccination
      dbcout( 'vaccUIni: ' + intToStr( vaccUIni ), true );
      dbcout( 'vaccAIni: ' + intToStr( vaccAIni ), true );
      dbcout( 'vaccURing: ' + intToStr( vaccURing ), true );
      dbcout( 'vaccARing: ' + intToStr( vaccARing ), true );

      // Running totals for herd exams
      dbcout( 'exmcUDirFwd: ' + intToStr( exmcUDirFwd ), true );
      dbcout( 'exmcADirFwd: ' + intToStr( exmcADirFwd ), true );
      dbcout( 'exmcUIndFwd: ' + intToStr( exmcUIndFwd ), true );
      dbcout( 'exmcAIndFwd: ' + intToStr( exmcAIndFwd ), true );
      dbcout( 'exmcUDirBack: ' + intToStr( exmcUDirBack ), true );
      dbcout( 'exmcADirBack: ' + intToStr( exmcADirBack ), true );
      dbcout( 'exmcUIndBack: ' + intToStr( exmcUIndBack ), true );
      dbcout( 'exmcAIndBack: ' + intToStr( exmcAIndBack ), true );

      // Running totals for diagnostic testing
      dbcout( 'tstcUDirFwd: ' + intToStr( tstcUDirFwd ), true );
      dbcout( 'tstcADirFwd: ' + intToStr( tstcADirFwd ), true );
      dbcout( 'tstcUIndFwd: ' + intToStr( tstcUIndFwd ), true );
      dbcout( 'tstcAIndFwd: ' + intToStr( tstcAIndFwd ), true );
      dbcout( 'tstcUDirBack: ' + intToStr( tstcUDirBack ), true );
      dbcout( 'tstcADirBack: ' + intToStr( tstcADirBack ), true );
      dbcout( 'tstcUIndBack: ' + intToStr( tstcUIndBack ), true );
      dbcout( 'tstcAIndBack: ' + intToStr( tstcAIndBack ), true );
      dbcout( 'tstcUTruePos: ' + intToStr( tstcUTruePos ), true );
      dbcout( 'tstcATruePos: ' + intToStr( tstcATruePos ), true );
      dbcout( 'tstcUTrueNeg: ' + intToStr( tstcUTrueNeg ), true );
      dbcout( 'tstcATrueNeg: ' + intToStr( tstcATrueNeg ), true );
      dbcout( 'tstcUFalsePos: ' + intToStr( tstcUFalsePos ), true );
      dbcout( 'tstcAFalsePos: ' + intToStr( tstcAFalsePos ), true );
      dbcout( 'tstcUFalseNeg: ' + intToStr( tstcUFalseNeg ), true );
      dbcout( 'tstcAFalseNeg: ' + intToStr( tstcAFalseNeg ), true );

      // Running totals for zone foci
      dbcout( 'zoncFoci: ' + intTOStr( zoncFoci ), true );

      // Running totals for death due to disease
      dbcout( 'deadcUIni: ' + intToStr( deadcUIni ), true );
      dbcout( 'deadcAIni: ' + intToStr( deadcAIni ), true );
      dbcout( 'deadcU: ' + intToStr( deadcU ), true );
      dbcout( 'deadcA: ' + intToStr( deadcA ), true );
      dbcout( 'deadcUInDestrQueue: ' + intToStr( deadcUInDestrQueue ), true );
      dbcout( 'deadcAInDestrQueue: ' + intToStr( deadcAInDestrQueue ), true );

      dbcout( 'firstDetection: ' + intToStr( firstDetection ), true );
      
      dbcout( 'firstDetUInf: ' + intToStr( firstDetUInf ), true );
      dbcout( 'firstDetAInf: ' + intToStr( firstDetAInf ), true );
      
      dbcout( 'firstDestruction: ' + intToStr( firstDestruction ), true );
      dbcout( 'firstVaccination: ' + intToStr( firstVaccination ), true );
      dbcout( 'lastDetection: ' + intToStr( lastDetection ), true );

			dbcout( '----------------end TSMDailyOutput' + endl, true );
    end
  ;


	procedure TSMDailyOutput.debugSel();
  	begin
    	dbcout( endl + '----------------begin TSMDailyOutput', true );
      dbcout( 'firstDetection: ' + intToStr( firstDetection ), true );
      dbcout( 'firstDetUInf: ' + intToStr( firstDetUInf ), true );
      dbcout( 'firstDetAInf: ' + intToStr( firstDetAInf ), true );
      dbcout( 'firstDestruction: ' + intToStr( firstDestruction ), true );
      dbcout( 'firstVaccination: ' + intToStr( firstVaccination ), true );
      dbcout( 'lastDetection: ' + intToStr( lastDetection ), true );
			dbcout( '----------------end TSMDailyOutput' + endl, true );
    end
  ;
//-----------------------------------------------------------------------------


end.
