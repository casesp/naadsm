unit SMSimOutByProdType;

(*
SMSimOutByProdType.pas
-----------------------
Begin: 2005/07/06
Last revision: $Date: 2011-09-30 20:36:11 $ $Author: areeves $
Version number: $Revision: 1.38.4.11 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
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

      function getInfcUAll(): longint;
      function getInfcAAll(): longint;

      function getTrcUDirAll(): longint;
      function getTrcADirAll(): longint;
      function getTrcUIndAll(): longint;
      function getTrcAIndAll(): longint;
      function getTrcUAll(): longint;
      function getTrcAAll(): longint;

      function getTocUDirAll(): longint; // New in 3.2.11 and 4.0.5
      function getTocUIndAll(): longint; // New in 3.2.11 and 4.0.5
      function getTocUAll(): longint;    // New in 3.2.11 and 4.0.5

      function getDetcUAll(): longint;
      function getDetcAAll(): longint;

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

      // Running totals for cause of infection
      infcUIni: longint;
      infcAIni: longint;
      infcUAir: longint;
      infcAAir: longint;
      infcUDir: longint;
      infcADir: longint;
      infcUInd: longint;
      infcAInd: longint;

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
      procedure processDestruction( const nAnimals, day, queueDay: integer; const inVaccQueue: boolean );

      procedure addToVaccQueue( const herdSize: integer; const day: integer );
      procedure removeFromVaccQueue( const herdSize: integer ); // if vaccination is canceled
      procedure processVaccination( const nAnimals, day, queueDay: integer );

      // Properties
      //-----------
      property prodTypeID: integer read getProdTypeID write setProdTypeID;

      property expcUAll: longint read getExpcUAll;
      property expcAAll: longint read getExpcAAll;

      property infcUAll: longint read getInfcUAll;
      property infcAAll: longint read getInfcAAll;

      property trcUDirAll: longint read getTrcUDirAll;
      property trcADirAll: longint read getTrcADirAll;
      property trcUIndAll: longint read getTrcUIndAll;
      property trcAIndAll: longint read getTrcAIndAll;
      property trcUAll: longint read getTrcUAll;
      property trcAAll: longint read getTrcAAll;

      property tocUDirAll: longint read  getTocUDirAll;
      property tocUIndAll: longint read  getTocUIndAll;
      property tocUAll: longint read  getTocUAll;

      property detcUAll: longint read getDetcUAll;
      property detcAAll: longint read getDetcAAll;

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
    end
  ;


	type TSMDailyOutput = class( TSMIterationOutput )
    protected
      // For internal use
      //------------------
      procedure initialize();
      procedure clearDailyTotals();

      procedure decrementDailyCounts( const herdAnimalCount: longint; const oldState: TNAADSMDiseaseState );
      procedure incrementDailyCounts( const herdAnimalCount: integer; const newState: TNAADSMDiseaseState );
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

      // New daily counts for cause of infection
      infnUAir: longint;
      infnAAir: longint;
      infnUDir: longint;
      infnADir: longint;
      infnUInd: longint;
      infnAInd: longint;

      // New daily counts for detection
      detnUClin: longint;
      detnAClin: longint;
      detnUTest: longint; // new in 3.2.0
      detnATest: longint; // new in 3.2.0

      // New daily counts for herd exams (new in 3.2.0)
      exmnUAll: longint;
      exmnAAll: longint;

      // New daily counts for testing (new in 3.2.0)
      tstnUTruePos: longint;
      tstnATruePos: longint;
      tstnUTrueNeg: longint;
      tstnATrueNeg: longint;
      tstnUFalsePos: longint;
      tstnAFalsePos: longint;
      tstnUFalseNeg: longint;
      tstnAFalseNeg: longint;

      // New daily counts for tracing
      trnUDirFwd: longint;
      trnADirFwd: longint;
      trnUIndFwd: longint;
      trnAIndFwd: longint;
      trnUDirBack: longint; // New in 3.2.0
      trnADirBack: longint; // New in 3.2.0
      trnUIndBack: longint; // New in 3.2.0
      trnAIndBack: longint; // New in 3.2.0

      // New daily counts for trace origins (new in 3.2.11 and 4.0.5)
      tonUDirFwd: longint;
      tonUIndFwd: longint;
      tonUDirBack: longint;
      tonUIndBack: longint;

      // New daily counts for destruction for any reason
      desnUAll: longint;
      desnAAll: longint;

      // New daily counts for vaccination for any reason
      vacnUAll: longint;
      vacnAAll: longint;

      // New daily count for zone foci
      zonnFoci: longint;

      // Number of apparently infectious units on a particular day
      appdUInfectious: longint;

      // New daily counts of units/animals infected for all causes
      function getInfUNew(): longint;
      function getInfANew(): longint;

    	constructor create(); override;
      destructor destroy(); override;

      procedure clear(); override;
      procedure clearNewDailyCounts();
      procedure updateDailyCounts(
        const herdAnimalCount: integer;
        const oldState: TNAADSMDiseaseState;
        const newState: TNAADSMDiseaseState;
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

      // New daily infections for all causes
      property infUNew: longint read getInfUNew;
      property infANew: longint read getInfANew;
  	end
  ;


implementation

	uses
  	SysUtils,
  	SqlClasses,
    I88n,
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

      // Running totals for cause of infection
      infcUIni := 0;
      infcAIni := 0;
      infcUAir := 0;
      infcAAir := 0;
      infcUDir := 0;
      infcADir := 0;
      infcUInd := 0;
      infcAInd := 0;

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

      // Running totals for cause of infection
      inc( self.infcUIni, src.infcUIni );
      inc( self.infcAIni, src.infcAIni );
      inc( self.infcUAir, src.infcUAir );
      inc( self.infcAAir, src.infcAAir );
      inc( self.infcUDir, src.infcUDir );
      inc( self.infcADir, src.infcADir );
      inc( self.infcUInd, src.infcUInd );
      inc( self.infcAInd, src.infcAInd );

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


  procedure TSMIterationOutput.addToVaccQueue( const herdSize: integer; const day: integer );
    begin
      inc( _vaccQueueLengthUnits );
      _vaccQueueLengthAnimals := _vaccQueueLengthAnimals + herdSize;
      //dbcout2( 'Added unit, vacc queue length: ' + intToStr( self.vaccQueueLengthUnits ) );

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
      //dbcout2( 'Removed unit, vacc queue length: ' + intToStr( self.vaccQueueLengthUnits ) );

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


  function TSMIterationOutput.getInfcUAll(): longint;
    begin
      // infcUTotal intentionally does NOT include units that were infected at the beginning of the simulation.
      result := infcUAir + infcUDir + infcUInd;
    end
  ;


  function TSMIterationOutput.getInfcAAll(): longint;
    begin
      // infcATotal intentionally does NOT include animals that were infected at the beginning of the simulation.
      result := infcAAir + infcADir + infcAInd;
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

  function TSMIterationOutput.getDetcUAll(): longint;
    begin
      result := detcUClin + detcUTest;
    end
  ;

  function TSMIterationOutput.getDetcAAll(): longint;
    begin
      result := detcAClin + detcATest;
    end
  ;


  function TSMIterationOutput.getDescUAll(): longint;
    begin
      // DO NOT include initially destroyed units
      result := descUDet + descUDirFwd + descUIndFwd + descUDirBack + descUIndBack + descURing;
    end
  ;


  function TSMIterationOutput.getDescAAll(): longint;
    begin
      // DO NOT include initially destroyed units
      result := descADet + descADirFwd + descAIndFwd + descADirBack + descAIndBack + descARing;
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

      // New daily counts for cause of infection
      inc( self.infnUAir, src.infnUAir );
      inc( self.infnAAir, src.infnAAir );
      inc( self.infnUDir, src.infnUDir );
      inc( self.infnADir, src.infnADir );
      inc( self.infnUInd, src.infnUInd );
      inc( self.infnAInd, src.infnAInd );

      // New daily counts for detection
      inc( self.detnUClin, src.detnUClin );
      inc( self.detnAClin, src.detnAClin );
      inc( self.detnUTest, src.detnUTest );
      inc( self.detnATest, src.detnATest );

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

      // New daily counts for tracing
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

      // New daily counts for destruction for any reason
      inc( self.desnUAll, src.desnUAll );
      inc( self.desnAAll, src.desnAAll );

      // New daily counts for vaccination for any reason
      inc( self.vacnUAll, src.vacnUAll );
      inc( self.vacnAAll, src.vacnAAll );

      // New daily counts for zone foci
      inc( self.zonnFoci, src.zonnFoci );

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

      // Number of apparently infectious units
      appdUInfectious := 0;
    end
  ;


  procedure TSMDailyOutput.clearNewDailyCounts();
    begin
      // New daily counts for cause of infection
      infnUAir := 0;
      infnAAir := 0;
      infnUDir := 0;
      infnADir := 0;
      infnUInd := 0;
      infnAInd := 0;

      // New daily counts for detection
      detnUClin := 0;
      detnAClin := 0;
      detnUTest := 0;
      detnATest := 0;

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

      // New daily counts for destruction for any reason
      desnUAll := 0;
      desnAAll := 0;

      // New daily counts for vaccination for any reason
      vacnUAll := 0;
      vacnAAll := 0;

      // New daily counts for zone foci
      zonnFoci := 0;
    end
  ;


  function TSMDailyOutput.getInfUNew(): longint;
    begin
      result := infnUAir + infnUDir + infnUInd;
    end
  ;


  function TSMDailyOutput.getInfANew(): longint;
    begin
      result := infnAAir + infnADir + infnAInd;
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
      incrementDailyCounts( herdAnimalCount, newState );
    end
  ;


  procedure TSMDailyOutput.incrementDailyCounts( const herdAnimalCount: integer; const newState: TNAADSMDiseaseState );
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
          raise exception.Create( 'Number of destroyed herd is going down!  This can''t happen!' )
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

          // New daily counts for cause of infection
          qDict['infnUAir'] := intToStr( infnUAir );
          qDict['infnAAir'] := intToStr( infnAAir );
          qDict['infnUDir'] := intToStr( infnUDir );
          qDict['infnADir'] := intToStr( infnADir );
          qDict['infnUInd'] := intToStr( infnUInd );
          qDict['infnAInd'] := intToStr( infnAInd );

          // New daily counts for detection
          qDict['detnUClin'] := intToStr( detnUClin );
          qDict['detnAClin'] := intToStr( detnAClin );
          qDict['detnUTest'] := intToStr( detnUTest );
          qDict['detnATest'] := intToStr( detnATest );

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

          // New daily counts for destruction for any reason
          qDict['desnUAll'] := intToStr( desnUAll );
          qDict['desnAAll'] := intToStr( desnAAll );

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

      // Running totals for cause of infection
      qDict['infcUIni'] := intToStr( infcUIni );
      qDict['infcAIni'] := intToStr( infcAIni );
      qDict['infcUAir'] := intToStr( infcUAir );
      qDict['infcAAir'] := intToStr( infcAAir );
      qDict['infcUDir'] := intToStr( infcUDir );
      qDict['infcADir'] := intToStr( infcADir );
      qDict['infcUInd'] := intToStr( infcUInd );
      qDict['infcAInd'] := intToStr( infcAInd );

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

      // Outputs need to be stored at least temporarily in the local database,
      // even if the user wants to record outputs in a remote database.
      // The query has to be written twice: once for each database.
      q := sqlclasses.writeQuery( table, QInsert, qDict );

      if not( db.execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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
      dbcout( 'tsdUSusc: ' + intToStr( tsdUSusc ), true  );
      dbcout( 'tsdASusc: ' + intToStr( tsdASusc ), true  );
      dbcout( 'tsdULat: ' + intToStr( tsdULat ), true  );
      dbcout( 'tsdALat: ' + intToStr( tsdALat ), true  );
      dbcout( 'tsdUSubc: ' + intToStr( tsdUSubc ), true  );
      dbcout( 'tsdASubc: ' + intToStr( tsdASubc ), true  );
      dbcout( 'tsdUClin: ' + intToStr( tsdUClin ), true  );
      dbcout( 'tsdAClin: ' + intToStr( tsdAClin ), true  );
      dbcout( 'tsdUNImm: ' + intToStr( tsdUNImm ), true  );
      dbcout( 'tsdANImm: ' + intToStr( tsdANImm ), true  );
      dbcout( 'tsdUVImm: ' + intToStr( tsdUVImm ), true  );
      dbcout( 'tsdAVImm: ' + intToStr( tsdAVImm ), true  );
      dbcout( 'tsdUDest: ' + intToStr( tsdUDest ), true  );
      dbcout( 'tsdADest: ' + intToStr( tsdADest ), true  );

      // Running totals for each disease state
      dbcout( 'tscUSusc: ' + intToStr( tscUSusc ), true  );
      dbcout( 'tscASusc: ' + intToStr( tscASusc ), true  );
      dbcout( 'tscULat: ' + intToStr( tscULat ), true  );
      dbcout( 'tscALat: ' + intToStr( tscALat ), true  );
      dbcout( 'tscUSubc: ' + intToStr( tscUSubc ), true  );
      dbcout( 'tscASubc: ' + intToStr( tscASubc ), true  );
      dbcout( 'tscUClin: ' + intToStr( tscUClin ), true  );
      dbcout( 'tscAClin: ' + intToStr( tscAClin ), true  );
      dbcout( 'tscUNImm: ' + intToStr( tscUNImm ), true  );
      dbcout( 'tscANImm: ' + intToStr( tscANImm ), true  );
      dbcout( 'tscUVImm: ' + intToStr( tscUVImm ), true  );
      dbcout( 'tscAVImm: ' + intToStr( tscAVImm ), true  );
      dbcout( 'tscUDest: ' + intToStr( tscUDest ), true  );
      dbcout( 'tscADest: ' + intToStr( tscADest ), true  );

      // Running totals for cause of infection
      dbcout( 'infcUIni: ' + intToStr( infcUIni ), true  );
      dbcout( 'infcAIni: ' + intToStr( infcAIni ), true  );
      dbcout( 'infcUAir: ' + intToStr( infcUAir ), true  );
      dbcout( 'infcAAir: ' + intToStr( infcAAir ), true  );
      dbcout( 'infcUDir: ' + intToStr( infcUDir ), true  );
      dbcout( 'infcADir: ' + intToStr( infcADir ), true  );
      dbcout( 'infcUInd: ' + intToStr( infcUInd ), true  );
      dbcout( 'infcAInd: ' + intToStr( infcAInd ), true  );

      // New daily counts for cause of infection
      dbcout( 'infnUAir: ' + intToStr( infnUAir ), true  );
      dbcout( 'infnAAir: ' + intToStr( infnAAir ), true  );
      dbcout( 'infnUDir: ' + intToStr( infnUDir ), true  );
      dbcout( 'infnADir: ' + intToStr( infnADir ), true  );
      dbcout( 'infnUInd: ' + intToStr( infnUInd ), true  );
      dbcout( 'infnAInd: ' + intToStr( infnAInd ), true  );

      // Running totals for exposures
      dbcout( 'expcUDir: ' + intToStr( expcUDir ), true  );
      dbcout( 'expcADir: ' + intToStr( expcADir ), true  );
      dbcout( 'expcUInd: ' + intToStr( expcUInd ), true  );
      dbcout( 'expcAInd: ' + intToStr( expcAInd ), true  );

      // Running totals for traces
      dbcout( 'trcUDirFwd: ' + intToStr( trcUDirFwd ), true  );
      dbcout( 'trcADirFwd: ' + intToStr( trcADirFwd ), true  );
      dbcout( 'trcUIndFwd: ' + intToStr( trcUIndFwd ), true  );
      dbcout( 'trcAIndFwd: ' + intToStr( trcAIndFwd ), true  );
      dbcout( 'trcUDirpFwd: ' + intToStr( trcUDirpFwd ), true  );
      dbcout( 'trcADirpFwd: ' + intToStr( trcADirpFwd ), true  );
      dbcout( 'trcUIndpFwd: ' + intToStr( trcUIndpFwd ), true  );
      dbcout( 'trcAIndpFwd: ' + intToStr( trcAIndpFwd ), true  );
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
      dbcout( 'detnUClin: ' + intToStr( detnUClin ), true  );
      dbcout( 'detnAClin: ' + intToStr( detnAClin ), true  );
      dbcout( 'detnUTest: ' + intToStr( detnUTest ), true  );
      dbcout( 'detnATest: ' + intToStr( detnATest ), true  );

      // New daily counts for herd exams
      dbcout( 'exmnUAll: ' + intToStr( exmnUAll ), true  );
      dbcout( 'exmnAAll: ' + intToStr( exmnAAll ), true  );

      // New daily counts for testing
      dbcout( 'tstnUTruePos: ' + intToStr( tstnUTruePos ), true  );
      dbcout( 'tstnATruePos: ' + intToStr( tstnATruePos ), true  );
      dbcout( 'tstnUTrueNeg: ' + intToStr( tstnUTrueNeg ), true  );
      dbcout( 'tstnATrueNeg: ' + intToStr( tstnATrueNeg ), true  );
      dbcout( 'tstnUFalsePos: ' + intToStr( tstnUFalsePos ), true  );
      dbcout( 'tstnAFalsePos: ' + intToStr( tstnAFalsePos ), true  );
      dbcout( 'tstnUFalseNeg: ' + intToStr( tstnUFalseNeg ), true  );
      dbcout( 'tstnAFalseNeg: ' + intToStr( tstnAFalseNeg ), true  );

      // New daily counts for traces
      dbcout( 'trnUDirFwd: ' + intToStr( trnUDirFwd ), true  );
      dbcout( 'trnADirFwd: ' + intToStr( trnADirFwd ), true  );
      dbcout( 'trnUIndFwd: ' + intToStr( trnUIndFwd ), true  );
      dbcout( 'trnAIndFwd: ' + intToStr( trnAIndFwd ), true  );
      dbcout( 'trnUDirBack: ' + intToStr( trnUDirBack ), true  );
      dbcout( 'trnADirBack: ' + intToStr( trnADirBack ), true  );
      dbcout( 'trnUIndBack: ' + intToStr( trnUIndBack ), true  );
      dbcout( 'trnAIndBack: ' + intToStr( trnAIndBack ), true  );

      // new daily counts for trace origins
      dbcout( 'tocUDirFwd: ' + intToStr( tocUDirFwd ), true );
      dbcout( 'tocUIndFwd: ' + intToStr( tocUIndFwd ), true );
      dbcout( 'tocUDirBack: ' + intToStr( tocUDirBack ), true );
      dbcout( 'tocUIndBack: ' + intToStr( tocUIndBack ), true );

      // New daily counts for destruction for any reason
      dbcout( 'desnUAll: ' + intToStr( desnUAll ), true  );
      dbcout( 'desnAAll: ' + intToStr( desnAAll ), true  );

      // New daily counts for vaccination for any reason
      dbcout( 'vacnUAll: ' + intToStr( vacnUAll ), true  );
      dbcout( 'vacnAAll: ' + intToStr( vacnAAll ), true  );

      // Running totals for detection
      dbcout( 'detcUClin: ' + intToStr( detcUClin ), true  );
      dbcout( 'detcAClin: ' + intToStr( detcAClin ), true  );
      dbcout( 'detcUTest: ' + intToStr( detcUTest ), true );
      dbcout( 'detcATest: ' + intToStr( detcATest ), true );

      // Running totals for destruction
      dbcout( 'descUIni: ' + intToStr( descUIni ), true  );
      dbcout( 'descAIni: ' + intToStr( descAIni ), true  );
      dbcout( 'descUDet: ' + intToStr( descUDet ), true  );
      dbcout( 'descADet: ' + intToStr( descADet ), true  );
      dbcout( 'descUDirFwd: ' + intToStr( descUDirFwd ), true  );
      dbcout( 'descADirFwd: ' + intToStr( descADirFwd ), true  );
      dbcout( 'descUIndFwd: ' + intToStr( descUIndFwd ), true  );
      dbcout( 'descAIndFwd: ' + intToStr( descAIndFwd ), true  );
      dbcout( 'descUDirBack: ' + intToStr( descUDirBack ), true );
      dbcout( 'descADirBack: ' + intToStr( descADirBack ), true );
      dbcout( 'descUIndBack: ' + intToStr( descUIndBack ), true );
      dbcout( 'descAIndBack: ' + intToStr( descAIndBack ), true );
      dbcout( 'descURing: ' + intToStr( descURing ), true  );
      dbcout( 'descARing: ' + intToStr( descARing ), true  );

      // Running totals for vaccination
      dbcout( 'vaccUIni: ' + intToStr( vaccUIni ), true );
      dbcout( 'vaccAIni: ' + intToStr( vaccAIni ), true );
      dbcout( 'vaccURing: ' + intToStr( vaccURing ), true  );
      dbcout( 'vaccARing: ' + intToStr( vaccARing ), true  );

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
