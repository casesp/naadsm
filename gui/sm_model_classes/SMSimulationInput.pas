unit SMSimulationInput;

(*
SMSimulationInput.pas
----------------------
Begin: 2005/01/06
Last revision: $Date: 2011-07-08 22:11:55 $ $Author: areeves $
Version number: $Revision: 1.94.4.17 $
Project: NAADSM
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
    SimInput,

    Sysutils,
    Dialogs,
    Forms, // for Application.processMessages

    ModelDatabase,
    FunctionDictionary,

    Sdew,

    ContactSpreadParams,
    ProductionType,
    ProductionTypeList,
    ProductionTypePair,
    ProductionTypePairList,
    AirborneSpreadParams,
    SMDatabase,
    DetectionParams,
    DestructionParams,
    VaccinationParams,
    RingVaccParams,
    GlobalControlParams,
    SMOutputOptions,
    FunctionPointers,
    CustomOutputDefinitions,
    SelectDailyOutputs,
    Zone
  ;

  type TSMSimulationInput = class( TSimInput )
    protected
      // Variables for internal use
      //----------------------------
      _db: TSMDatabase;

      // Simulation inputs (properties)
      //--------------------------------
      _zoneList: TZoneList;

      _ptList: TProductionTypeList;

      _ptpList: TProductionTypePairList;

      _includeContactSpread: boolean;

      _includeAirborneSpread: boolean;
      _useAirborneExponentialDecay: boolean;

      _useWithinHerdPrevalence: boolean;

      _ctrl: TGlobalControlParams;

      _destrStartDays: integer;
      _vacStartNumber: integer;

      _costTrackDestruction: boolean;
      _costTrackVaccination: boolean;
      _costTrackZoneSurveillance: boolean;

      _outputOptions: TSMOutputOptions;

      _customOutputDefinitions: TCustomOutputList;
      _useCustomOutputs: boolean;

      _selectDailyOutputs: TSelectDailyOutputs;

      _scenarioDescr: string;

      _simDays: integer;
      _simStopReason: TStopReason;

      // Functions for internal use
      //---------------------------
      procedure initialize();

      // Simulation inputs (properties)
      //--------------------------------
      function getPtList(): TProductionTypeList;
      procedure setPtList( list: TProductionTypeList );

      function getPTPList(): TProductionTypePairList;
      procedure setPTPList( list: TProductionTypePairList );

      function getZoneList(): TZoneList;
      procedure setZoneList( list: TZoneList );

      procedure setVacStartNumber( val: integer );
      procedure setDestrStartDays( val: integer );

      function getVacStartNumber(): integer;
      function getDestrStartDays(): integer;

      procedure setIncludeContactSpread( val: boolean );
      procedure setIncludeAirborneSpread( val: boolean );
      procedure setUseAirborneExponentialDecay( val: boolean );
      procedure setUseWithinHerdPrevalence( val: boolean );

      procedure setCostTrackDestruction( val: boolean );
      procedure setCostTrackVaccination( val: boolean );
      procedure setCostTrackZoneSurveillance( val: boolean );

      procedure setUseCustomOutputs( val: boolean );

      function getIncludeTracingHerdExam(): boolean;
      function getIncludeTracingTesting(): boolean;

      function getIncludeDestruction(): boolean;
      function getIncludeDetection(): boolean;
      function getIncludeContactSpread(): boolean;
      function getIncludeAirborneSpread(): boolean;
      function getUseAirborneExponentialDecay(): boolean;
      function getUseWithinHerdPrevalence(): boolean;
      function getIncludeVaccination(): boolean;

      function getIncludeCosts(): boolean;
      function getCostTrackDestruction(): boolean;
      function getCostTrackVaccination(): boolean;
      function getCostTrackZoneSurveillance(): boolean;

      function getIncludeTracing(): boolean;
      function getIncludeZones(): boolean;
      function getUseCustomOutputs(): boolean;
      function getStoreSelectDailyOutputs(): boolean;

      function getSimDays(): integer;
      function getSimStopReason(): TStopReason;
      function getScenarioDescr(): string;

      procedure setSimDays( val: integer );
      procedure setSimStopReason( val: TStopReason );
      procedure setScenarioDescr( val: string );

      function getUpdated(): boolean; override;
      // Use inherited procedure for setUpdated()

      procedure populateDatabaseInGeneral();

      // XML import
      function importSimSettingsXml( db: TSMDatabase; sdew: TSdew; root: pointer; errMsg: pstring = nil ): boolean;

      // Helpers for XML generation
      function zoneMonitorXml( const writeOutputs: boolean ): string;
      function infectionMonitorXml( const writeOutputs: boolean ): string;
      function exposureMonitorXml( const writeOutputs: boolean ): string;
      function detectionMonitorXml( const writeOutputs: boolean ): string;
      function examMonitorXml( const writeOutputs: boolean ): string;
      function testMonitorXml( const writeOutputs: boolean ): string;
      function traceMonitorXml( const writeOutputs: boolean ): string;
      function destructionMonitorXml( const writeOutputs: boolean ): string;
      function destructionListMonitorXml( const writeOutputs: boolean ): string;
      function vaccinationMonitorXml( const writeOutputs: boolean ): string;
      function vaccinationListMonitorXml( const writeOutputs: boolean ): string;
      function economicMonitorXml( const writeOutputs: boolean ): string;
      function naadsmOutputsXml( const writeOutputs: boolean ): string;

    public
      constructor create(
        const scenarioXmlFileName: string;
        db: TSMDatabase;
        errMsg: pstring = nil;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil
      ); overload;
      constructor create( db: TSMDatabase; setProgressPercent: TObjFnBool1Int = nil; fnsOK: pboolean = nil ); overload;
      constructor create( const src: TSMSimulationInput ); overload;

      destructor destroy(); override;

      function getProdTypeID( typeDescr: string ): integer;
      function getProdTypeName( id: integer ): string;
      function findProdType( typeDescr: string ): TProductionType;
      function prodTypeIDExists( typeID: integer ): boolean;

      function findDestructionParams( typeDescr: string ): TDestructionParams;

      function populateDatabase( const updateAction: TDBUpdateActionType ): boolean; override;
      function removeDbFunction( const fnID: integer ): boolean; override;

      function ssXml(
        const writeOutputs: boolean;
        stopReason: TStopReason;
        nDays: integer
      ): string;
      
      function writeXMLFile(
        const fileName: string;
        const writeOutputs: boolean;
        const stopReason: TStopReason;
        const nDays: integer;
        errMsg: PString = nil
      ): boolean;

      function isValid( skipOutputOptions: boolean; msg: PString = nil ): boolean;


      // Functions for handling model outputs
      //-------------------------------------
      procedure initializeAllOutputRecords(); // upon sim start
      procedure prepareForIteration( iterationAboutToStart: integer ); // Upon iteration start
      procedure prepareForDay( day: integer ); // upon day start
      procedure processDailyRecords( db: TSMDatabase; dayJustCompleted: integer ); // upon day end
      procedure processIterationRecords( db: TSMDatabase; iterationJustCompleted: integer ); // upon iteration end
      procedure simComplete(); // Upon sim end

      // Debugging functions
      //--------------------
      procedure debug();
      procedure debugSimpleProperties();

      // Abstract functions from TSimInput that are not used (yet?) in this class
      //-------------------------------------------------------------------------
      function validate( msg: PString = nil ): boolean; override;
      procedure run( fn: TObjFnVoid0 = nil ); override;

      // Input properties
      //------------------
      property ptList: TProductionTypeList read getPtList write setPtList;
      property ptpList: TProductionTypePairList read getPTPList; // write setPTPList;
      property zoneList: TZoneList read getZoneList; // write setZoneList;
      property controlParams: TGlobalControlParams read _ctrl;
      property database: TSMDatabase read _db;

      property vacStartNumber: integer read getVacStartNumber write setVacStartNumber;
      property destrStartDays: integer read getDestrStartDays write setDestrStartDays;

      property includeDestructionGlobal: boolean read getIncludeDestruction;
      property includeDetectionGlobal: boolean read getIncludeDetection;
      property includeVaccinationGlobal: boolean read getIncludeVaccination;
      property includeTracingGlobal: boolean read getIncludeTracing;
      property includeTracingHerdExamGlobal: boolean read getIncludeTracingHerdExam;
      property includeTracingTestingGlobal: boolean read getIncludeTracingTesting;
      property includeZonesGlobal: boolean read getIncludeZones;

      property includeContactSpreadGlobal: boolean read getIncludeContactSpread  write setIncludeContactSpread;
      property includeAirborneSpreadGlobal: boolean read getIncludeAirborneSpread write setIncludeAirborneSpread;
      property useAirborneExponentialDecay: boolean read getUseAirborneExponentialDecay write setUseAirborneExponentialDecay;
      property useWithinHerdPrevalence: boolean read getUseWithinHerdPrevalence write setUseWithinHerdPrevalence;

      property includeCostsGlobal: boolean read getIncludeCosts;
      property costTrackDestruction: boolean read getCostTrackDestruction write setCostTrackDestruction;
      property costTrackVaccination: boolean read getCostTrackVaccination write setCostTrackVaccination;
      property costTrackZoneSurveillance: boolean read getCostTrackZoneSurveillance write setCostTrackZoneSurveillance;

      property outputOptions: TSMOutputOptions read _outputOptions write _outputOptions;

      property customOutputDefinitions: TCustomOutputList read _customOutputDefinitions;
      property useCustomOutputs: boolean read getUseCustomOutputs write setUseCustomOutputs;

      property selectDailyOutputs: TSelectDailyOutputs read _selectDailyOutputs;
      property storeSelectDailyOutputs: boolean read getStoreSelectDailyOutputs;

      property simDays: integer read getSimDays write setSimDays;
      property simStopReason: TStopReason read getSimStopReason write setSimStopReason;
      property scenarioDescr: string read getScenarioDescr write setScenarioDescr;
    end
  ;

  const
    DBSMSIMULATIONINPUT: boolean = true; // set to true to show debugging messages for this unit.

implementation

  uses
    Variants,
    MyStrUtils,
    MyDelphiArrayUtils,
    DebugWindow,
    SqlClasses,
    UnicodeDev,
    I88n,

    StringConsts
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  procedure TSMSimulationInput.initialize();
    begin
      _includeContactSpread := false;
      _includeAirborneSpread := false;
      _useAirborneExponentialDecay := false;
      _useWithinHerdPrevalence := false;

      _costTrackDestruction := false;
      _costTrackVaccination := false;
      _costTrackZoneSurveillance := false;

      _scenarioDescr := '';

      _simDays := -1;
      _simStopReason := ssStopReasonUndefined;

      _useCustomOutputs := false;

      _db := nil;
    end
  ;


  constructor TSMSimulationInput.create(
        const scenarioXmlFileName: string;
        db: TSMDatabase;
        errMsg: pstring = nil;
        fnPrimaryProgress: TObjFnBool1Int = nil;
        fnProgressMessage: TObjFnVoid1String = nil
      );
    var
      sdew: TSdew;
      root, models: pointer;
      i: integer;
      pt: TProductionType;
      ptp: TProductionTypePair;
      nSteps, nStepsComplete: integer;
    begin
      inherited create();
      initialize();
      _db := db;

      _ctrl := nil;
      _zoneList := nil;
      _ptList := nil;
      _ptpList := nil;

      nSteps := 10; // Update this value as new items to parse are added in future versions.
      nStepsComplete := 0;

      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      if( nil <> @fnProgressMessage ) then fnProgressMessage( tr( 'Reading XML file...' ) );

      sdew := TSdew.createFromFile( pAnsiChar( scenarioXmlFileName ) );
      root := sdew.GetRootTree();
      models := sdew.GetElementByName( root, 'models' );

      // Start with some of the basic parameters.
      //-----------------------------------------
      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      if( nil <> @fnProgressMessage ) then fnProgressMessage( tr( 'Creating scenario parameters...' ) );

      // This can occur if the XML character encoding is incorrect or the XMLis not well formed
      if not( importSimSettingsXml( db, sdew, root, errMsg ) ) then
        begin
          appendToPString( errMsg, tr( 'The XML scenario import file could not be parsed.' ) );  //rbh tr()? I thinks so
          sdew.Free();
          exit;
        end
      ;

      // The function dictionary doesn't need to do anything with the XML.
      // Functions will be created and inserted as needed by the other models.
      //----------------------------------------------------------------------
      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      _fnDictionary := TFunctionDictionary.create( db, self );

      // These items are contained in the XML, and need to be generated.
      //----------------------------------------------------------------
      // Create _ctrl first: it's needed by several of the others.
      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      _ctrl := TGlobalControlParams.create( db, self, sdew, models, errMsg );

      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      _zoneList := TZoneList.create( db, self, sdew, models, errMsg );
      self.controlParams.useZonesGlobal := ( 0 < _zoneList.count );

      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      _ptList := TProductionTypeList.create( db, self, sdew, models, errMsg );

      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      _ptpList := TProductionTypePairList.create( db, self, sdew, models, errMsg );

      // These items are not contained in the XML, and should be generated using the typical defaults.
      //----------------------------------------------------------------------------------------------
      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      _outputOptions := TSMOutputOptions.create( db );
      _customOutputDefinitions := TCustomOutputList.create( db );
      _selectDailyOutputs := TSelectDailyOutputs.create( db );

      sdew.Free();

      // Set some properties of the "parent" object based on what was read in from XML.
      //-------------------------------------------------------------------------------
      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      for i := 0 to _ptList.Count - 1 do
        begin
          pt := _ptList.at( i );
          if( not( strIsEmpty( pt.relPrevalenceName ) ) ) then
            self.useWithinHerdPrevalence := true
          ;
          if( pt.useDetection ) then
            self.controlParams.useDetectionGlobal := true
          ;
          if( pt.useVaccination ) then
            self.controlParams.useVaccGlobal := true
          ;
          if( pt.useTracing ) then
            self.controlParams.useTracingGlobal := true
          ;
          if( pt.useDestruction ) then
            self.controlParams.useDestructionGlobal := true
          ;
          if( pt.useTracingExam ) then
            self.controlParams.useTracingHerdExamGlobal := true
          ;
          if( pt.useTesting ) then
            self.controlParams.useTracingTestingGlobal := true
          ;
        end
      ;

      for i := 0 to _ptpList.Count - 1 do
        begin
          ptp := _ptpList.at( i );

          if( ptp.includeDirect or ptp.includeIndirect ) then
            self.includeContactSpreadGlobal := true
          ;
          if( ptp.includeAirborne ) then
            self.includeAirborneSpreadGlobal := true
          ;
        end
      ;

      inc( nStepsComplete );
      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( trunc( 100 * nStepsComplete / nSteps ) );
      if( nil <> @fnProgressMessage ) then fnProgressMessage( tr( 'Populating scenario database...' ) );
      self.populateDatabase( MDBAForceInsert );

      if( nil <> @fnPrimaryProgress ) then fnPrimaryProgress( 100 );
    end
  ;

  
  constructor TSMSimulationInput.create( db: TSMDatabase; setProgressPercent: TObjFnBool1Int = nil; fnsOK: pboolean = nil );
    var
      res: TSqlResult;
      row: TSqlRow;
      db2: TSqlDatabase;
      q: string;

      str: string;
      nSteps: integer;

      functionsOK: boolean;
    begin
      inherited create();
      initialize();

      functionsOK := true;

      nSteps := 9;

      if( nil <> @setProgressPercent ) then
        Application.ProcessMessages()
      ;
      _db := db;

      db2 := db as TSqlDatabase;

      if( nil <> _fnDictionary ) then
        _fnDictionary.Free()
      ;
      _fnDictionary := TFunctionDictionary.create( db, self );

      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 1 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      //_fnDictionary.debug();

      // Deal with the zone list before the production type list
      _zoneList := TZoneList.create( db, self );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 2 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      if( not _zoneList.functionsAreValid() ) then
        functionsOK := false
      ;

      _ptList := TProductionTypeList.create( db, self );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 3 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      if( not _ptList.functionsAreValid() ) then
        functionsOK := false
      ;

      _ptpList := TProductionTypePairList.create( db, self );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 4 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      if( not _ptpList.functionsAreValid() ) then
        functionsOK := false
      ;

      _ctrl := TGlobalControlParams.create( db, self );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 5 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      if( not _ctrl.functionsAreValid() ) then
        functionsOK := false
      ;

      _outputOptions := TSMOutputOptions.create( db );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 6 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;

      _customOutputDefinitions := TCustomOutputList.create( db );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 7 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      if( not _customOutputDefinitions.functionsAreValid() ) then
        functionsOK := false
      ;

      _selectDailyOutputs := TSelectDailyOutputs.create( db );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 8 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;

      res := TSqlResult.create( db2 );

      // Select values from inGeneral
      //-----------------------------
      q := 'SELECT'
        + ' `includeContactSpread`, `includeAirborneSpread`, `useAirborneExponentialDecay`,'
        + ' `useWithinHerdPrevalence`,'
        + ' `costTrackDestruction`, `costTrackVaccination`, `costTrackZoneSurveillance`,'
        + ' `useFixedRandomSeed`, `randomSeed`,'
        + ' `scenarioDescr`, `iterations`, `days`, `simStopReason`,'
        + ' `useCustomOutputs`'
        + ' FROM `inGeneral`'
      ;

      res.runQuery( q );

      row := res.fetchArrayFirst();

      if( null <> row.field('includeContactSpread') ) then
        _includeContactSpread := boolean( row.field('includeContactSpread') )
      ;

      if( null <> row.field('includeAirborneSpread') ) then
        _includeAirborneSpread := boolean( row.field('includeAirborneSpread') )
      ;

      if( null <> row.field('useAirborneExponentialDecay') ) then
        _useAirborneExponentialDecay := boolean( row.field( 'useAirborneExponentialDecay' ) )
      ;

      if( null <> row.field('useWithinHerdPrevalence') ) then
        _useWithinHerdPrevalence := boolean( row.field('useWithinHerdPrevalence') )
      ;

      if( null <> row.field('costTrackDestruction') ) then
        _costTrackDestruction := boolean( row.field('costTrackDestruction') )
      ;

      if( null <> row.field('costTrackVaccination') ) then
        _costTrackVaccination := boolean( row.field('costTrackVaccination') )
      ;

      if( null <> row.field('costTrackZoneSurveillance') ) then
        _costTrackZoneSurveillance := boolean( row.field('costTrackZoneSurveillance') )
      ;

      if( null <> row.field('useFixedRandomSeed') ) then
        _useFixedRandomSeed := boolean( row.field('useFixedRandomSeed') )
      ;

      if( null <> row.field('randomSeed') ) then
        _randomSeed := integer( row.field('randomSeed') )
      ;

      if( null <> row.field('scenarioDescr') ) then
        _scenarioDescr := row.field('scenarioDescr')
      ;

      if( null <> row.field('iterations') ) then
        _simIterations := row.field('iterations')
      ;

      if( null <> row.field('days') ) then
        _simDays := row.field('days')
      ;

      if( null <> row.field('simStopReason') ) then
        begin
          str := row.field('simStopReason');
          str := fixup( str );
          
          if( 'outbreakend' = str ) then
            _simStopReason := ssStopAtEndOfOutBreak
          else if( 'firstdetection' = str ) then
            _simStopReason := ssStopAtFirstDetection
          else if( 'specifiedday' = str ) then
            _simStopReason := ssStopAtSpecificDay
          else if( 'diseaseend' = str ) then
            _simStopReason := ssStopAtDiseaseEnd
          else
            _simStopReason := ssStopReasonUndefined
          ;
        end
      else
        _simStopReason := ssStopAtEndOfOutBreak
      ;

      if( null <> row.field('useCustomOutputs' ) ) then
        _useCustomOutputs := row.field('useCustomOutputs')
      ;

      freeAndNil( res );

      setUpdated( not functionsOK );

      if( nil <> @setProgressPercent ) then setProgressPercent( 100 );

      if( nil <> fnsOK ) then
        fnsOK^ := functionsOK
      ;
    end
  ;


  constructor TSMSimulationInput.create( const src: TSMSimulationInput );
    begin
      inherited create( src as TSimInput );

      // The database is the only object that the copy gets as a reference.
      // Every other object must be newly created.
      _db := src._db;

      _scenarioDescr := src._scenarioDescr;

      _destrStartDays := src._destrStartDays;
      _includeAirborneSpread := src._includeAirborneSpread;
      _includeContactSpread := src._includeContactSpread;

      _costTrackDestruction := src._costTrackDestruction;
      _costTrackVaccination := src._costTrackVaccination;
      _costTrackZoneSurveillance := src._costTrackZoneSurveillance;

      _simDays := src._simDays;
      _simStopReason := src._simStopReason;
      _useAirborneExponentialDecay := src._useAirborneExponentialDecay;
      _useWithinHerdPrevalence := src._useWithinHerdPrevalence;

      _useCustomOutputs := src._useCustomOutputs;
      _vacStartNumber := src._vacStartNumber;

      // fnDictionary is copied by the inherited constructor

      _zoneList := TZoneList.create( src._zoneList, self );

      _ptList := TProductionTypeList.create( src._ptList, self );

      _ptpList := TProductionTypePairList.create( src._ptpList, _ptList, self );

      _ctrl := TGlobalControlParams.create( src._ctrl, self );

      _outputOptions := TSMOutputOptions.create( src._outputOptions, self );

      _customOutputDefinitions := TCustomOutputList.create( src._customOutputDefinitions, self );

      _selectDailyOutputs := TSelectDailyOutputs.create( src._selectDailyOutputs, self );

      _updated := src._updated;
    end
  ;


  destructor TSMSimulationInput.Destroy();
    begin
      freeAndNil( _ptList );
      freeAndNil( _ptpList );
      freeAndNil( _ctrl );
      freeAndNil( _outputOptions );
      freeAndNil( _zoneList );
      freeAndNil( _customOutputDefinitions );
      freeAndNil( _selectDailyOutputs );

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// database updates
//-----------------------------------------------------------------------------
  procedure TSMSimulationInput.populateDatabaseInGeneral();
    var
      pairedVals: TQueryDictionary;

      q: string;
    begin
      pairedVals := TQueryDictionary.create();

      pairedVals['inGeneralID']                 := _db.sqlQuote( DB_SCHEMA_APPLICATION );

      pairedVals['scenarioDescr']               := _db.sqlQuote(  _scenarioDescr );
      pairedVals['iterations']                  := intToStr( _simIterations );
      pairedVals['days']                        := intToStr( _simDays );

      pairedVals['simStopReason']               := _db.sqlQuote( stopReasonToString( _simStopReason ) );

      pairedVals['includeContactSpread']        := usBoolToText( _includeContactSpread );
      pairedVals['includeAirborneSpread']       := usBoolToText( _includeAirborneSpread );
      pairedVals['useAirborneExponentialDecay'] := usBoolToText( _useAirborneExponentialDecay );
      pairedVals['useWithinHerdPrevalence']    := usBoolToText( _useWithinHerdPrevalence );

      pairedVals['costTrackDestruction']        := usBoolToText( _costTrackDestruction );
      pairedVals['costTrackVaccination']        := usBoolToText( _costTrackVaccination );
      pairedVals['costTrackZoneSurveillance']   := usBoolToText( _costTrackZoneSurveillance );

      pairedVals['useCustomOutputs']            := usBoolToText( _useCustomOutputs );

      pairedVals['useFixedRandomSeed']          := usBoolToText( _useFixedRandomSeed );

      if( _useFixedRandomSeed ) then
        pairedVals['randomSeed']                := intToStr( _randomSeed )
      ;

      if( _db.recordsExist( 'inGeneral' ) ) then
        q := writeQuery( 'inGeneral', QUpdate, pairedVals )
      else
        q := writeQuery( 'inGeneral', QInsert, pairedVals )
      ;

      _db.execute( q );

      freeAndNil( pairedVals );
    end
  ;


  function TSMSimulationInput.populateDatabase( const updateAction: TDBUpdateActionType ): boolean;
    begin
      // FIX ME: Consider dealing with the result, some day.
      result := true;
      
      if( updated ) then
        begin
          populateDatabaseInGeneral();

          if( _fnDictionary.updated ) then
            _fnDictionary.populateDatabase( _db )
          ;

          if( _zoneList.updated ) then
            _zoneList.populateDatabase( _db, _ptList, updateAction )
          ;

          if( _ptList.updated ) then
            _ptList.populateDatabase( _db, updateAction )
          ;

          if( _ptpList.updated ) then
            _ptpList.populateDatabase( _db, updateAction )
          ;

          if( _ctrl.updated ) then
            _ctrl.populateDatabase( _db, MDBAForceUpdate ) // ALWAYS force updates for this class
          ;

          if( _outputOptions.updated ) then
            _outputOptions.populateDatabase( _db, MDBAForceUpdate ) // ALWAYS force updates for this class
          ;

          if( _customOutputDefinitions.updated ) then
            _customOutputDefinitions.populateDatabase( _db )
          ;

          if( _selectDailyOutputs.updated ) then
            _selectDailyOutputs.populateDatabase( _db )
          ;

          setUpdated( false );
        end
      ;
    end
  ;


  function TSMSimulationInput.removeDbFunction( const fnID: integer ): boolean;
    begin
      if( nil <> _db ) then
        result := _db.removeChartFunction( fnID )
      else
        begin
          raise exception.create( '_db is nil in TSMSimulationInput.removeDbFunction()' );
          result := false;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  function TSMSimulationInput.importSimSettingsXml( db: TSMDatabase; sdew: TSdew; root: pointer; errMsg: pstring = nil ): boolean;
    var
      e, ee: pointer;
      xmlVersion, experimentalVersion: string;
    begin
      result := true; // Until shown otherwise

      // Version number
      //---------------
      xmlVersion := '';
      e := sdew.GetElementByName( root, 'naadsm-version' );
      if( nil = e ) then
        begin
          result := false;
          appendToPString( errMsg, tr( 'The selected XML file does not appear to have been created with a recognized version of NAADSM.  This version of NAADSM cannot import this scenario.' ) );
          exit;
        end
      else
        begin
          ee := sdew.GetElementByName( e, 'major-version' );
          if( nil <> ee ) then
            xmlVersion := xmlVersion + fixup( sdew.GetElementContents( ee ) )
          else
            xmlVersion := xmlVersion + '0'
          ;

          ee := sdew.GetElementByName( e, 'minor-version' );
          if( nil <> ee ) then
            xmlVersion := xmlVersion + '.' + fixup( sdew.GetElementContents( ee ) )
          else
            xmlVersion := xmlVersion + '.0'
          ;

          (*
          // Any new XML schema should get at least a new minor version number.
          // The release version doesn't really matter, so we'll skip it for the time being.
          ee := sdew.GetElementByName( e, 'release' );
          if( nil <> ee ) then
            xmlVersion := xmlVersion + '.' + fixup( sdew.GetElementContents( ee ) )
          else
            xmlVersion := xmlVersion + '.0'
          ;
          *)

          ee := sdew.GetElementByName( e, 'experimental' );
          if( nil <> ee ) then
            experimentalVersion := fixup( sdew.GetElementContents( ee ) )
          else
            experimentalVersion := ''
          ;

          if not( arrayContains( ALL_MAJOR_VERSIONS, xmlVersion ) ) then
            begin
              result := false;
              appendToPString( errMsg, tr( 'The selected XML file does not appear to have been created with a recognized version of NAADSM.  This version of NAADSM cannot import this scenario.' ) );
              exit;
            end
          ;

          if( fixup( BRANCHNAME ) <> fixup( experimentalVersion ) ) then
            begin
              result := false;
              appendToPString( errMsg, tr( 'The selected XML file appears to have been generated with an experimental version of NAADSM.  This version of NAADSM cannot import this scenario.' ) );
              exit;
            end
          ;
        end
      ;

      // Scenario description
      //---------------------
      e := sdew.GetElementByName( root, 'description' );
      if( nil <> e ) then
        scenarioDescr := trim( decodeXml( Sdew.GetElementContents( e ) ) )
      else
        scenarioDescr := tr( '(No description provided)' )
      ;

      // Number of iterations
      //---------------------
      e := sdew.GetElementByName( root, 'num-runs' );
      if( nil <> e ) then
        self.simIterations := myStrToInt( sdew.GetElementContents( e ), 1 )
      ;

      // Language setting
      //-----------------
      // For the moment, don't import language setting.
      // FIX ME: Some day, this may need to change.

      // Iteration stop condition
      //-------------------------
      self.simDays := 32767;
      self.simStopReason := ssStopAtEndOfOutBreak;
      e := sdew.GetElementByName( root, 'num-days' );
      if( nil <> e ) then
        self.simDays := myStrToInt( sdew.GetElementContents( e ), 32767 )
      ;
      if( 32767 <> self.simDays ) then
        self.simStopReason := ssStopAtSpecificDay
      ;
      e := sdew.GetElementByName( root, 'exit-condition' );
      if( nil <> e ) then
        begin
          ee := sdew.GetElementByName( e, 'disease-end' );
          if( nil <> ee ) then
            begin
              self.simStopReason := ssStopAtDiseaseEnd;
              self.simDays := 32767;
            end
          else
            begin
              ee := sdew.GetElementByName( e, 'first-detection' );
              if( nil <> ee ) then
                begin
                  self.simStopReason := ssStopAtFirstDetection;
                  self.simDays := 32767;
                end
              ;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
  function TSMSimulationInput.zoneMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then // write a basic zone monitor: it's used to get the area and perimeter of each zone.
        begin
          result := ''
            + '  <zone-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>zoneArea</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>zonePerimeter</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </zone-monitor>' + endl + endl
          ;
        end
      else // Write the full zone monitor
        begin
          result := ''
            + '  <zone-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>unitsInZone</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>unitDaysInZone</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>animalDaysInZone</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>zoneShape</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>zoneArea</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>maxZoneArea</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>maxZoneAreaDay</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>finalZoneArea</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>zonePerimeter</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>maxZonePerimeter</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>maxZonePerimeterDay</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>finalZonePerimeter</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </zone-monitor>' + endl + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.infectionMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not writeOutputs ) then
        begin
          result := ''
            + '  <infection-monitor>' + endl
            + '    <ratio-period>' + endl
            + '      <value>1</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </ratio-period>' + endl
            + '  </infection-monitor>' + endl + endl
          ;
        end
      else
        begin
          result := ''
            + ' <infection-monitor>' + endl
            + '    <ratio-period>' + endl
            + '      <value>1</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </ratio-period>' + endl
            + '    <output>' + endl
            + '      <variable-name>ratio</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>infections</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>infnU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>infnA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>infcU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>infcA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </infection-monitor>' + endl + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.exposureMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <exposure-monitor></exposure-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <exposure-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>exposures</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>expnU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>expnA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>expcU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>expcA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </exposure-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.detectionMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <detection-monitor></detection-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <detection-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>detections</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>firstDetection</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>lastDetection</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>detnU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>detnA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>detcU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>detcA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>detOccurred</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </detection-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.examMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <exam-monitor></exam-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <exam-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>exmcU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>exmcA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </exam-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.testMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <test-monitor></test-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <test-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>tstcU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>tstcA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>tstcUTruePos</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>tstcUTrueNeg</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>tstcUFalsePos</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>tstcUFalseNeg</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </test-monitor>' + endl
          ;
        end
      ;
    end
  ;

  function TSMSimulationInput.traceMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <trace-monitor></trace-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <trace-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>trnUp</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trnU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trnAp</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trnA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trcUp</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trcU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trcAp</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>trcA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </trace-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.destructionMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <destruction-monitor></destruction-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <destruction-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>destructions</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>firstDestruction</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>desnU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>desnA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>descU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>descA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>destrOccurred</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </destruction-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.destructionListMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <destruction-list-monitor></destruction-list-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <destruction-list-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswUMax</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswUMaxDay</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswAMax</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswAMaxDay</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswUTimeAvg</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswUTimeMax</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswUDaysInQueue</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>deswADaysInQueue</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '  </destruction-list-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.vaccinationMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + ' <vaccination-monitor></vaccination-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + ' <vaccination-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccinations</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>firstVaccination</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacnU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacnA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccOccurred</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </vaccination-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.vaccinationListMonitorXml( const writeOutputs: boolean ): string;
    begin
      if( not( writeOutputs ) ) then
        begin
          result := ''
            + '  <vaccination-list-monitor></vaccination-list-monitor>' + endl
          ;
        end
      else
        begin
          result := ''
            + '  <vaccination-list-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwU</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwA</variable-name>' + endl
            + '      <broken-down>yes</broken-down>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwUMax</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwUMaxDay</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwAMax</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwAMaxDay</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwUTimeAvg</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwUTimeMax</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            (*
            // For NAADSM 3.x, and for at least the time being, these two outputs are no longer saved.
            + '    <output>' + endl
            + '      <variable-name>vacwUDaysInQueue</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vacwADaysInQueue</variable-name>' + endl
            + '      <frequency>once</frequency>' + endl
            + '    </output>' + endl
            *)
            + '  </vaccination-list-monitor>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.economicMonitorXml( const writeOutputs: boolean ): string;
    begin
      // This is not really a "monitor", even though it acts like one.
      // The "<economic-model>" tag is in fact correct.

      if( not( writeOutputs ) ) then
        begin
          // Do nothing.  The PC version takes care
          // of cost accounting outputs on its own.
          result := '';
        end
      else
        begin
          result := ''
            + '  <economic-model>' + endl  //rbhXML: no production type is specified (had to relax schema rules to accomodate)
            + '    <output>' + endl
            + '      <variable-name>destrAppraisal</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>destrEuthanasia</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>destrIndemnification</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>destrDisposal</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>destrCleaning</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>destrSubtotal</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccSetup</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccVaccination</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>vaccSubtotal</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>costSurveillance</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>costsTotal</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </economic-model>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.naadsmOutputsXml( const writeOutputs: boolean ): string;
    begin
      result := '';
      result := result + '<output>' + endl;
      result := result + '  <variable-name>all-units-states</variable-name>' + endl;
      result := result + '  <frequency>daily</frequency>' + endl;
      result := result + '</output>' + endl;

      if( writeOutputs ) then
        begin
          result := result + endl
            + '<output>' + endl
            + '  <variable-name>tsdU</variable-name>' + endl
            + '  <frequency>daily</frequency>' + endl
            + '</output>' + endl
            + '<output>' + endl
            + '  <variable-name>tsdA</variable-name>' + endl
            + '  <frequency>daily</frequency>' + endl
            + '</output>' + endl
            + '<output>' + endl
            + '  <variable-name>diseaseDuration</variable-name>' + endl
            + '  <frequency>once</frequency>' + endl
            + '</output>' + endl
            + '<output>' + endl
            + '  <variable-name>outbreakDuration</variable-name>' + endl
            + '  <frequency>once</frequency>' + endl
            + '</output>' + endl
          ;
        end
      ;
    end
  ;


  function TSMSimulationInput.ssXml(
        const writeOutputs: boolean;
        stopReason: TStopReason;
        nDays: integer
      ): string;
    begin
      // Do some error checking
      //-----------------------
      if( nil = _db ) then
        begin
          raise exception.create( 'Database is not set in TSMSimulationInput.ssXml()' );
          result := '';
          exit;
        end
      ;

      if( nil = _zoneList ) then
        begin
          raise exception.create( '_zoneList is not set in TSMSimulationInput.ssXml()' );
          result := '';
          exit;
        end
      ;

      if( nil = _ptList ) then
        begin
          raise exception.create( '_ptList is not set in TSMSimulationInput.ssXml()' );
          result := '';
          exit;
        end
      ;

      if( nil = _ctrl ) then
        begin
          raise exception.create( '_ctrl is not set in TSMSimulationInput.ssXml()' );
          result := '';
          exit;
        end
      ;

      // Write the file header
      //-----------------------
      result := '<?xml version="1.0" encoding="UTF-16" ?>' + endl;
      result := result + '<naadsm:disease-simulation' + endl;
      result := result + '  xmlns:naadsm="http://www.naadsm.org/schema"' + endl;
      result := result + '  xmlns:xsd="http://www.w3.org/2001/XMLSchema"' + endl;
      result := result + '  xmlns:xml="http://www.w3.org/XML/1998/namespace">' + endl;
      //result := result + ' xmlns:xdf="http://xml.gsfc.nasa.gov/XDF">' + endl; // FIX ME: What's up with this??

      { rbh20101001: Just a note about namespaces, when the time comes to set up directories on the NAADSM
        server. The name spaces below are what I used in validating the XML file against the schemas. They
        are locals paths rather than URLs but the folder names as specified worked withStylusStudio.
        schema - Where the xsd schema files for a particular version of the database reside.
        XDF - Where the XDF_017.xsd file resides. This file is no longer available from an external URL.
        - All schema versions use this same XDF file, no need for copies in each schema folder.
        - If the XML parser can also validate it will also need a xsi:schemaLocation string.

        xmlns="C:\NAADSM-XML-Validation\3Line\schema"
        xmlns:naadsm="C:\NAADSM-XML-Validation\3Line\schema"
        xmlns:xdf="C:\NAADSM-XML-Validation\XDF"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="C:\NAADSM-XML-Validation\3Line\schema C:\NAADSM-XML-Validation\3Line\schema\disease_simulation.xsd"
      }

      result := result + '  <description>' + encodeXml( trim( _scenarioDescr ), true ) + '</description>' + endl;
      result := result + '  <naadsm-version>' + endl;
      result := result + '    <major-version>' + MAJORVERSION + '</major-version>' + endl;
      result := result + '    <minor-version>' + MINORVERSION + '</minor-version>' + endl;
      result := result + '    <release>' + RELEASENUMBER + '</release>' + endl;

      if( not( strIsEmpty( BRANCHNAME ) ) ) then
        result := result + '    <experimental>' + BRANCHNAME + '</experimental>' + endl
      ;

      result := result + '  </naadsm-version>' + endl;
      
      if( 0 < length( _db.languageCode ) ) then
        result := result + '  <language>' + _db.languageCode + '</language>' + endl
      else
        result := result + '  <language>' + i88nLanguageCodeString() + '</language>' + endl
      ;
      result := result + endl;

      // Write iteration parameters
      //---------------------------
      result := result + '  <num-runs>'+ intToStr( _simIterations ) + '</num-runs>' + endl;

      // Use the simulation-specified stop reason, unless a different one is already specified
      if( ssStopReasonGuiDefined = stopReason ) then
        begin
          stopReason := _simStopReason;
          nDays := _simDays;
        end
      ;

      case stopReason of
        ssStopAtEndOfOutBreak:
          result := result + '  <num-days>32767</num-days>' + endl
        ;
        ssStopAtSpecificDay:
          result := result + '  <num-days>' + intToStr( nDays ) + '</num-days>' + endl
        ;
        ssStopAtDiseaseEnd:
          begin
            result := result + '  <num-days>32767</num-days>' + endl;
            result := result + '  <exit-condition><disease-end /></exit-condition>' + endl;
          end
        ;
        ssStopAtFirstDetection:
          begin
            result := result + '  <num-days>32767</num-days>' + endl;
            result := result + '  <exit-condition><first-detection /></exit-condition>' + endl;
          end
        ;
      end;

      result := result + endl;

      // Write the models header
      //========================
      result := result + '<models>' + endl;
      result := result + endl;

      // Write the zone models
      //======================
      if( includeZonesGlobal and includeDetectionGlobal ) then
        begin
          result := result + _zoneList.ssXml();
          result := result + zoneMonitorXml( writeOutputs );
        end
      ;

      // Write the production type models (disease only)
      //================================================
      result := result + _ptList.ssDiseaseModelsXml();

      // Write the contact and airborne spread models
      //=============================================
      if( includeContactSpreadGlobal or includeAirborneSpreadGlobal ) then
        result := result + _ptpList.ssXml()
      ;

      // Using the infection monitor is a good way to generate output for reasons for infection, etc.
      // Some simple outputs from the infection monitor are used,
      // even if standard supercomputer outputs are not needed.
      result := result + infectionMonitorXml( writeOutputs );

      // Using the exposure monitor is a good way to generate output for reasons for exposure.
      // Exposure monitor has no parameters, unless standard supercomputer outputs are requested.
      result := result + exposureMonitorXml( writeOutputs );

      // Write the control measures
      //===========================
      
      // Resources and global controls
      //------------------------------
      if( includeDetectionGlobal ) then
        begin
          result := result + _ctrl.ssXml();
        end
      ;

      // Quarantine model (automatic, no parameters)
      //--------------------------------------------
      result := result + endl + '  <quarantine-model></quarantine-model>' + endl + endl;


      // Detection models
      //-----------------
      if( includeDetectionGlobal ) then
        begin
          result := result + endl + _ptList.ssDetectionModelsXml();
          result := result + endl + detectionMonitorXml( writeOutputs );
        end
      ;

      // Tracing models
      //---------------
      if( includeTracingGlobal and includeDetectionGlobal ) then
        begin
          result := result + endl + _ptList.ssTracingModelsXml();
          result := result + traceMonitorXml( writeOutputs );
        end
      ;


      // Herd exam models
      //-----------------
      if( includeTracingHerdExamGlobal and includeDetectionGlobal ) then
        begin
          result := result + endl + _ptList.ssExamModelsXml();
          result := result + endl + examMonitorXml( writeOutputs );
        end
      ;

      // Diagnostic testing models
      //--------------------------
      if( includeTracingTestingGlobal and includeDetectionGlobal ) then
        begin
          result := result + endl + _ptList.ssTestingModelsXml();
          result := result + endl + testMonitorXml( writeOutputs );
        end
      ;
      
      // Destruction models
      //-------------------
      if( includeDestructionGlobal and includeDetectionGlobal ) then
        begin
          // Basic destruction models
          result := result + ptList.ssBasicDestructionModelsXml();

          // Trace destruction models
          result := result + ptList.ssTraceDestructionModelsXml();

          // Ring destruction models
          result := result + ptList.ssRingDestructionModelsXml();
        end
      ;
      // Destruction monitors are needed whether destruction is used or not, to keep track of initially destroyed units
      result := result + endl + destructionMonitorXml( writeOutputs );
      result := result + endl + destructionListMonitorXml( writeOutputs );


      // Vaccination models
      //-------------------
      // Vaccine models must be written if any units are initially vaccinated or if vaccination models are used by any production types.
      if (( includeVaccinationGlobal and includeDetectionGlobal ) or database.containsInitiallyVaccinatedUnits ) then
        result := result + ptList.ssVaccineModelsXml()
      ;
      // Ring vaccination models
      if( includeVaccinationGlobal and includeDetectionGlobal ) then
        result := result + ptList.ssRingVaccModelsXml()
      ;

      // Vaccination monitors are needed whether vaccination is used or not, to keep track of initially vaccinated units
      result := result + endl + vaccinationMonitorXml( writeOutputs );
      result := result + endl + vaccinationListMonitorXml( writeOutputs );


      // Zone models
      //------------
      if( includeZonesGlobal and includeDetectionGlobal ) then
        begin
          result := result + endl + ptList.ssZoneModelsXml();
        end
      ;

      // Economic models (required only for supercomputer version)
      //----------------------------------------------------------
      if( includeCostsGlobal and writeOutputs and includeDetectionGlobal ) then
        begin
          result := result + endl + ptList.ssEconomicModelsXml( includeZonesGlobal, _zoneList );
          result := result + endl + economicMonitorXml( writeOutputs );
        end
      ;

      // Conflict resolver (required, no parameters)
      //--------------------------------------------
      result := result + endl + '  <conflict-resolver></conflict-resolver>' + endl + endl;

      // Write the models footer
      //========================
      result := result +  '</models>' + endl;
      result := result + endl;

      // Final outputs (required for the DLL to generate them)
      //======================================================
      result := result + naadsmOutputsXml( writeOutputs );

      // Write the file footer
      //======================
      result := result + endl + '</naadsm:disease-simulation>' + endl;
    end
  ;


  {*
  WARNING: this function will attempt to overwrite an existing file without notice.
  FIX ME: no error checking or validation is done.
  }
  function TSMSimulationInput.writeXMLFile(
        const fileName: string;
        const writeOutputs: boolean;
        const stopReason: TStopReason;
        const nDays: integer;
        errMsg: PString = nil
      ): boolean;
    var
      xmlFile: TextFile;
    begin
      try
        assignUnicode( xmlFile, fileName );
        rewrite( xmlFile );
        writeln( xmlFile, ssXML( writeOutputs, stopReason, nDays ) );
        closeFile( xmlFile );
        result := true;
      except
        result := false;
      end;
    end
  ;
//-----------------------------------------------------------------------------



function TSMSimulationInput.isValid( skipOutputOptions: boolean; msg: PString = nil ): boolean;
  begin
    result := true;

    //dbcout2( 'Check 0: ' + usBoolToText( updated ) );

    if( includeZonesGlobal ) then
      begin
        if( not( _zoneList.validate( msg ) ) ) then
          result := false
        ;
      end
    ;

    //dbcout2( 'Check 1: ' + usBoolToText( updated ) );

    if( not( _ptList.validate( msg ) ) ) then
      result := false
    ;

    //dbcout2( 'Check 2: ' + usBoolToText( updated ) );

    if( not( _ptpList.validate( msg ) ) ) then
      result := false
    ;

    //dbcout2( 'Check 3: ' + usBoolToText( updated ) );

    if( _useCustomOutputs ) then
      begin
        if( not( _customOutputDefinitions.validate( msg ) ) )  then
          result := false
        ;
      end
    ;

    //dbcout2( 'Check 4: ' + usBoolToText( updated ) );

    if( includeDestructionGlobal or includeVaccinationGlobal ) then
      begin
        if( not( _ctrl.validate( msg ) ) ) then
          result := false
        ;
      end
    ;

    //dbcout2( 'Check 5: ' + usBoolToText( updated ) );

    if( not( skipOutputOptions ) ) then
      begin
        if( not( _outputOptions.validate( msg ) ) )  then
          result := false
        ;
      end
    ;

    //dbcout2( 'Check 6: ' + usBoolToText( updated ) );

    // It is possible for simInput to be updated during validation,
    // for example, when vaccination and destruction priorities are checked.
    if( updated ) then
      populateDatabase( MDBAAuto )
    ;
  end
;




function TSMSimulationInput.prodTypeIDExists( typeID: integer ): boolean;
  begin
    result := _ptList.prodTypeIDExists( typeID );
  end
;


function TSMSimulationInput.getProdTypeID( typeDescr: string ): integer;
  begin
    result := _ptList.findProdTypeID( typeDescr );
  end
;


function TSMSimulationInput.getProdTypeName( id: integer ): string;
  begin
    result := _ptList.findProdTypeName( id );
  end
;


function TSMSimulationInput.findProdType( typeDescr: string ): TProductionType;
  begin
    result := _ptList.findProdType( typeDescr );
  end
;


function TSMSimulationInput.findDestructionParams( typeDescr: string ): TDestructionParams;
  var
    pt: TProductionType;

  begin
    //writeln( 'Finding destruction params for ' + typeDescr );
    pt := findProdType( typeDescr );

    if( pt = nil ) then raise exception.create( 'Cannot find production type' );

    result := pt.destructionParams;

  end
;


//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TSMSimulationInput.debugSimpleProperties();
    begin
      dbcout( '', true );
      dbcout( 'useWithinHerdPrevalence: ' + usBoolToText( _useWithinHerdPrevalence ), true );
      dbcout( 'includeContactSpread: ' + usBoolToText( includeContactSpreadGlobal ), true );
      dbcout( 'useAirborneExponentialDecay: ' + usBoolToText( _useAirborneExponentialDecay ), true );
      dbcout( 'includeAirborneSpread: ' + usBoolToText( includeAirborneSpreadGlobal ), true );
      dbcout( 'includeDetection: ' + usBoolToText( includeDetectionGlobal ), true );
      dbcout( 'includeTracing: ' + usBoolToText( includeTracingGlobal ), true );
      dbcout( 'includeTracingHerdExam: ' + usBoolToText( includeTracingHerdExamGlobal ), true );
      dbcout( 'includeTracingTesting: ' + usBoolToText( includeTracingTestingGlobal ), true );
      dbcout( 'includeVaccination: ' + usBoolToText( includeVaccinationGlobal ), true );
      dbcout( 'includeDestruction: ' + usBoolToText( includeDestructionGlobal ), true );
      dbcout( 'includeVaccination: ' + usBoolToText( includeVaccinationGlobal ), true );
      dbcout( '', true );
      dbcout( 'costTrackDestruction: ' + usBoolToText( _costTrackDestruction ), true );
      dbcout( 'costTrackVaccination: ' + usBoolToText( _costTrackVaccination ), true );
      dbcout( 'costTrackZoneSurveillance: ' + usBoolToText( _costTrackZoneSurveillance ), true );
      dbcout( '', true );
      dbcout( 'destrStartDays: ' + intToStr( _destrStartDays ), true );
      dbcout( 'vacStartNumber: ' + intToStr( _vacStartNumber ), true );
      dbcout( '', true );
      dbcout( 'simDays: ' + intToStr( _simDays ), true );
      dbcout( 'simIterations: ' + intToStr( _simIterations ), true );
      dbcout( 'simStopReason: ' + stopReasonToString( _simStopReason ), true );
      dbcout( '', true );
      dbcout( 'randomSeed: ' + usFloatToStr( _randomSeed ), true );
      dbcout( 'useFixedRandomSeed: ' + usBoolToText( _useFixedRandomSeed ), true );
      dbcout( '', true );
      dbcout( 'updated: ' + usBoolToText( updated ), true );
      dbcout( '', true );
    end
  ;

  procedure TSMSimulationInput.debug();
    begin
      dbcout( '=+=+=+=+=+=+=+=+= TSMSimulationInput.debug()...', true );

      dbcout( 'scenarioDescr:', true );
      dbcout( _scenarioDescr, true );

      debugSimpleProperties();

      // Function list
      if( nil <> _fnDictionary ) then
        begin
          dbcout( '========= BEGIN FUNCTIONS: ' + intToStr( _fnDictionary.Count ), true );
          _fnDictionary.debug();
          dbcout( '========= END FUNCTIONS' + endl, true );
        end
      else
        dbcout( '========= (_fnDict is nil)', true )
      ;

      // Zones
      if( nil <> _zoneList ) then
        begin
          dbcout( '========= BEGIN ZONES: ' + intToStr( _zoneList.Count ), true );
          _zoneList.debug();
          dbcout( '========= END ZONES' + endl, true );
        end
      else
        dbcout( '========= (_zoneList is nil)', true )
      ;

      // Production types
      if( nil <> _ptList ) then
        begin
          dbcout( '========= BEGIN PRODUCTION TYPES: ' + intToStr( _ptList.Count ), true );
          _ptList.debug();
          dbcout( '========= END PRODUCTION TYPES' + endl, true );
        end
      else
        dbcout( '========= (_ptList is nil)', true )
      ;

      // Production type pairs
      if( nil <> _ptpList ) then
        begin
          dbcout( '========= BEGIN PRODUCTION TYPE PAIRS: ' + intToStr( _ptpList.Count ), true );
          _ptpList.debug();
          dbcout( '========= END PRODUCTION TYPE PAIRS' + endl, true );
        end
      else
        dbcout( '========= (_ptpList is nil)', true )
      ;

      // Global controls
      if( nil <> _ctrl ) then
        begin
          dbcout( '========= BEGIN GLOBAL CONTROLS', true );
          _ctrl.debug();
          dbcout( '========= END GLOBAL CONTROLS' + endl, true );
        end
      else
        dbcout( '========= (_ctrl is nil)', true )
      ;

      // Output options
      if( nil <> _outputOptions ) then
        begin
          dbcout( '========= BEGIN OUTPUT OPTIONS', true );
          _outputOptions.debug();
          dbcout( '========= END OUTPUT OPTIONS' + endl, true );
        end
      else
        dbcout( '========= (_outputOptions is nil)', true )
      ;

      // Custom output definitions
      if( nil <> _customOutputDefinitions ) then
        begin
          dbcout( '========= BEGIN CUSTOM OUTPUT DEFINITIONS', true );
          _customOutputDefinitions.debug();
          dbcout( '========= END CUSTOM OUTPUT DEFINITIONS' + endl, true );
        end
      else
        dbcout( '========= (_customOutputDefinitions is nil)', true )
      ;

      // Selected daily outputs
      if( nil <> _selectDailyOutputs ) then
        begin
          dbcout( '========= BEGIN SELECTED DAILY DEFINITIONS', true );
          _selectDailyOutputs.debug();
          dbcout( '========= END SELECTED DAILY DEFINITIONS' + endl, true );
        end
      else
        dbcout( '========= (_selectDailyOutputs is nil)', true )
      ;

      dbcout( '=+=+=+=+=+=+=+=+= Done TSMSimulationInput.debug()', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// simulation outputs
//-----------------------------------------------------------------------------
  // Carried out before a simulation begins
  procedure TSMSimulationInput.initializeAllOutputRecords();
    begin
      _currentIteration := 0;
      _lastCompleteiteration := -1;
      _ptList.clearAllRecords( _db );

      if( includeZonesGlobal ) then
        _zoneList.clearAllRecords( _db )
      ;

      if( storeSelectDailyOutputs ) then
        _selectDailyOutputs.prepareQueries()
      ;
    end
  ;


  // Carried out just before an iteration begins
  procedure TSMSimulationInput.prepareForIteration( iterationAboutToStart: integer );
    begin
      _currentIteration := iterationAboutToStart;
      _ptList.resetIterationRecords();

      if( includeZonesGlobal ) then
        _zoneList.resetIterationRecords();
    end
  ;


  // Carried out just before a day begins
  procedure TSMSimulationInput.prepareForDay( day: integer );
    begin
      _ptList.prepareForDay( day );

      if( includeZonesGlobal ) then
        _zoneList.prepareForDay( day );
    end
  ;


  // Carried out at the end of the day
  procedure TSMSimulationInput.processDailyRecords( db: TSMDatabase; dayJustCompleted: integer );
    begin
      _ptList.processDailyRecords( db, _currentIteration, dayJustCompleted );

      if( includeZonesGlobal ) then
        _zoneList.processDailyRecords( db, _currentIteration, dayJustCompleted )
      ;
    end
  ;


  // Carried out when an iteration ends
  procedure TSMSimulationInput.processIterationRecords( db: TSMDatabase; iterationJustCompleted: integer );
    begin
      _lastCompleteIteration := iterationJustCompleted;
      _ptList.processIterationRecords( db, _lastCompleteIteration );

      if( includeZonesGlobal ) then
        _zoneList.processIterationRecords( db, _lastCompleteIteration )
      ;

      if( storeSelectDailyOutputs ) then
        _selectDailyOutputs.processRecords( db, _lastCompleteIteration, includeZonesGlobal )
      ;
    end
  ;


  // Called at the end of the simulation
  procedure TSMSimulationInput.simComplete();
    begin
      // _ptList has nothing to do

      if( includeZonesGlobal ) then
        _zoneList.simComplete()
      ;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Abstract functions from TSimInput that are not used (yet?) in this class
//-----------------------------------------------------------------------------
  function TSMSimulationInput.validate( msg: PString = nil ): boolean;
    begin
      raise exception.create( 'Function TSMSimulationInput.validate() is abstract.' );
      result := false;
    end
  ;


  procedure TSMSimulationInput.run( fn: TObjFnVoid0 = nil );
    begin
      raise exception.create( 'Function TSMSimulationInput.run() is abstract.' );
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  function TSMSimulationInput.getPTList(): TProductionTypeList; begin result := _ptList; end;
  procedure TSMSimulationInput.setPtList( list: TProductionTypeList ); begin _ptList := list; setUpdated( true ); end;

  function TSMSimulationInput.getPTPList(): TProductionTypePairList; begin result := _ptpList; end;
  procedure TSMSimulationInput.setPTPList( list: TProductionTypePairList ); begin _ptpList := list; setUpdated( true ); end;

  function TSMSimulationInput.getZoneList(): TZoneList; begin result := _zoneList; end;
  procedure TSMSimulationInput.setZoneList( list: TZoneList ); begin _zoneList := list; setUpdated( true ); end;

  procedure TSMSimulationInput.setVacStartNumber( val: integer ); begin _vacStartNumber := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setDestrStartDays( val: integer ); begin _destrStartDays := val; setUpdated( true ); end;

  function TSMSimulationInput.getVacStartNumber(): integer; begin Result := _vacStartNumber; end;
  function TSMSimulationInput.getDestrStartDays(): integer; begin Result := _destrStartDays; end;

  procedure TSMSimulationInput.setIncludeContactSpread( val: boolean ); begin _includeContactSpread := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setIncludeAirborneSpread( val: boolean ); begin _includeAirborneSpread := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setUseAirborneExponentialDecay( val: boolean ); begin _useAirborneExponentialDecay := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setUseWithinHerdPrevalence( val: boolean ); begin _useWithinHerdPrevalence := val; setUpdated( true ); end;

  procedure TSMSimulationInput.setCostTrackDestruction( val: boolean ); begin _costTrackDestruction := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setCostTrackVaccination( val: boolean ); begin _costTrackVaccination := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setCostTrackZoneSurveillance( val: boolean ); begin _costTrackZoneSurveillance := val; setUpdated( true ); end;

  procedure TSMSimulationInput.setUseCustomOutputs( val: boolean ); begin _useCustomOutputs := val; setUpdated( true ); end;


  function TSMSimulationInput.getIncludeDestruction(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useDestructionGlobal
      else
        result := false
      ;
    end
  ;


  function TSMSimulationInput.getIncludeDetection(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useDetectionGlobal
      else
        result := false
      ;
    end
  ;


  function TSMSimulationInput.getIncludeTracing(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useTracingGlobal
      else
        result := false
      ;
    end
  ;


  function TSMSimulationInput.getIncludeTracingHerdExam(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useTracingGlobal and _ctrl.useTracingHerdExamGlobal
      else
        result := false
      ;
    end
  ;


  function TSMSimulationInput.getIncludeTracingTesting(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useTracingGlobal and _ctrl.useTracingTestingGlobal
      else
        result := false
      ;
    end
  ;


  function TSMSimulationInput.getIncludeVaccination(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useVaccGlobal
      else
        result := false
      ;
    end
  ;

  function TSMSimulationInput.getIncludeZones(): boolean;
    begin
      if( nil <> _ctrl ) then
        result := _ctrl.useZonesGlobal
      else
        result := false
      ;
    end
  ;

  function TSMSimulationInput.getIncludeContactSpread(): boolean; begin Result := _includeContactSpread; end;
  function TSMSimulationInput.getIncludeAirborneSpread(): boolean; begin Result := _includeAirborneSpread; end;
  function TSMSimulationInput.getUseAirborneExponentialDecay(): boolean; begin Result := _useAirborneExponentialDecay; end;
  function TSMSimulationInput.getUseWithinHerdPrevalence(): boolean; begin result := _useWithinHerdPrevalence; end;

  function TSMSimulationInput.getIncludeCosts(): boolean;
    begin
      result :=
        _costTrackDestruction
      or
        _costTrackVaccination
      or
        _costTrackZoneSurveillance
      ;
    end
  ;

  function TSMSimulationInput.getCostTrackDestruction(): boolean; begin Result := _costTrackDestruction; end;
  function TSMSimulationInput.getCostTrackVaccination(): boolean; begin Result := _costTrackVaccination; end;
  function TSMSimulationInput.getCostTrackZoneSurveillance(): boolean; begin Result := _costTrackZoneSurveillance; end;

  function TSMSimulationInput.getUseCustomOutputs(): boolean; begin result := _useCustomOutputs; end;

  function TSMSimulationInput.getStoreSelectDailyOutputs(): boolean;
    begin
      result := _selectDailyOutputs.storeSelectDailyOutputs;
    end
  ;

  function TSMSimulationInput.getSimDays(): integer; begin result := _simDays; end;
  function TSMSimulationInput.getSimStopReason(): TStopReason; begin result := _simStopReason; end;
  function TSMSimulationInput.getScenarioDescr(): string; begin result := _scenarioDescr; end;

  procedure TSMSimulationInput.setSimDays( val: integer ); begin _simDays := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setSimStopReason( val: TStopReason ); begin _simStopReason := val; setUpdated( true ); end;
  procedure TSMSimulationInput.setScenarioDescr( val: string ); begin _scenarioDescr := val; setUpdated( true ); end;

  function TSMSimulationInput.getUpdated(): boolean;
    begin
      result := inherited getUpdated();
      //dbcout2( 'nextCheck 0: ' + usBoolToText( result ) );
      if( false = result ) then result := _zoneList.updated;
      //dbcout2( 'nextCheck 1: ' + usBoolToText( result ) );
      if( false = result ) then result := _ptList.updated;
      //dbcout2( 'nextCheck 2: ' + usBoolToText( result ) );
      if( false = result ) then result := _ptpList.updated;
      //dbcout2( 'nextCheck 3: ' + usBoolToText( result ) );
      if( false = result ) then result := _ctrl.updated;
      //dbcout2( 'nextCheck 4: ' + usBoolToText( result ) );
      if( false = result ) then result := _outputOptions.updated;
      //dbcout2( 'nextCheck 5: ' + usBoolToText( result ) );
      if( false = result ) then result := _customOutputDefinitions.updated;
      //dbcout2( 'nextCheck 6: ' + usBoolToText( result ) );
      if( false = result ) then result := _selectDailyOutputs.updated;
      //dbcout2( 'nextCheck 7: ' + usBoolToText( result ) );
    end
  ;
//-----------------------------------------------------------------------------

end.
