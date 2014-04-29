unit SMSimulationInput;

(*
SMSimulationInput.pas
----------------------
Begin: 2005/01/06
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.74 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University
                                       
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

    FunctionDictionary,

    ContactModel,
    ProductionType,
    ProductionTypeList,
    ProductionTypePair,
    ProductionTypePairList,
    AirborneSpreadModel,
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

  		procedure setDefaultDestrPriorities( db: TSMDatabase );
  		procedure setDefaultVaccPriorities( db: TSMDatabase );
			procedure setParamQuestions( db: TSMDatabase );

      procedure setIncludeContactSpread( val: boolean );
      procedure setIncludeAirborneSpread( val: boolean );
      procedure setUseAirborneExponentialDecay( val: boolean );
      procedure setUseWithinHerdPrevalence( val: boolean );

      procedure setCostTrackDestruction( val: boolean );
      procedure setCostTrackVaccination( val: boolean );
      procedure setCostTrackZoneSurveillance( val: boolean );

      procedure setUseCustomOutputs( val: boolean );

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

    public
      constructor create(); overload;
      constructor create( db: TSMDatabase; setProgressPercent: TObjFnBool1Int = nil; fnsOK: pboolean = nil ); overload;
      constructor create( const src: TSMSimulationInput ); overload;

      destructor destroy(); override;

      function getProdTypeID( typeDescr: string ): integer;
      function getProdTypeName( id: integer ): string;
      function findProdType( typeDescr: string ): TProductionType;
      function prodTypeIDExists( typeID: integer ): boolean;

      function findDestructionParams( typeDescr: string ): TDestructionParams;

      procedure populateDatabase();
      procedure removeDbFunction( const fnID: integer ); override;
      
      function ssXml( writeOutputs: boolean ): string;
      function writeXMLFile( fileName: string; writeOutputs: boolean; errMsg: PString = nil ): boolean;

			function isValid( skipOutputOptions: boolean; msg: PString = nil ): boolean; override;


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

      // Input properties
      //------------------
      property ptList: TProductionTypeList read getPtList write setPtList;
      property ptpList: TProductionTypePairList read getPTPList; // write setPTPList;
      property zoneList: TZoneList read getZoneList; // write setZoneList;
      property controlParams: TGlobalControlParams read _ctrl;
      property database: TSMDatabase read _db;

      property vacStartNumber: integer read getVacStartNumber write setVacStartNumber;
      property destrStartDays: integer read getDestrStartDays write setDestrStartDays;

      property includeDestructionGlobal: boolean read getIncludeDestruction; // write setIncludeDestruction;
      property includeDetectionGlobal: boolean read getIncludeDetection; // write setIncludeDetection;
      property includeVaccinationGlobal: boolean read getIncludeVaccination; // write setIncludeVaccination;
      property includeTracingGlobal: boolean read getIncludeTracing; // write setIncludeTracing;
      property includeZonesGlobal: boolean read getIncludeZones; // write setIncludeZones;

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
    USStrUtils,
    DebugWindow,
    SqlClasses,
    UnicodeDev
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

  constructor TSMSimulationInput.create();
    begin
      inherited create();
      initialize();

      _zoneList := TZoneList.create( self );

      _ptList := TProductionTypeList.create( self );

      //_ptList.debug();

//      _ctrl := TGlobalControlParams.create( self );

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

      if( nil <> @setProgressPercent ) then Application.ProcessMessages();
      _db := db;

      db2 := db as TSqlDatabase;

      _fnDictionary := TFunctionDictionary.create( db, self );
      if( nil <> @setProgressPercent ) then
        begin
          setProgressPercent( round( 1 * 100 / nSteps ) );
          Application.ProcessMessages();
        end
      ;
      _fnDictionary.debug();

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
            _simStopReason := ssStartAndStopAtEndOfOutBreak
          else if( 'firstdetection' = str ) then
            _simStopReason := ssStartAndStopAtFirstDetection
          else if( 'specifiedday' = str ) then
            _simStopReason := ssStartAndStopAtSpecificDay
          else if( 'diseaseend' = str ) then
            _simStopReason := ssStartAndStopAtDiseaseEnd
          else
            _simStopReason := ssStopReasonUndefined
          ;
        end
      else
        _simStopReason := ssStartAndStopAtEndOfOutBreak
      ;

      if( null <> row.field('useCustomOutputs' ) ) then
        _useCustomOutputs := row.field('useCustomOutputs')
      ;

      freeAndNil( res );

      _updated := not functionsOK;

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

      pairedVals['inGeneralID']					      	:= _db.sqlQuote( DB_SCHEMA_APPLICATION );

      pairedVals['scenarioDescr'] 				      := _db.sqlQuote(  _scenarioDescr );
      pairedVals['iterations'] 						      := intToStr( _simIterations );
      pairedVals['days'] 									      := intToStr( _simDays );

      pairedVals['simStopReason']               := _db.sqlQuote( stopReasonToString( _simStopReason ) );

      pairedVals['includeContactSpread'] 	      := boolToStr( _includeContactSpread );
      pairedVals['includeAirborneSpread']       := boolToStr( _includeAirborneSpread );
      pairedVals['useAirborneExponentialDecay'] := boolToStr( _useAirborneExponentialDecay );
      pairedVals['useWithinHerdPrevalence']    := boolToStr( _useWithinHerdPrevalence );

      pairedVals['costTrackDestruction'] 			  := boolToStr( _costTrackDestruction );
      pairedVals['costTrackVaccination'] 			  := boolToStr( _costTrackVaccination );
      pairedVals['costTrackZoneSurveillance'] 	:= boolToStr( _costTrackZoneSurveillance );

      pairedVals['useCustomOutputs']            := boolToStr( _useCustomOutputs );

      pairedVals['useFixedRandomSeed']          := boolToStr( _useFixedRandomSeed );

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


  procedure TSMSimulationInput.populateDatabase();
    begin
      if( updated ) then
        begin
          populateDatabaseInGeneral();

          if( _fnDictionary.updated ) then
            _fnDictionary.populateDatabase( _db )
          ;

          if( _zoneList.updated ) then
            _zoneList.populateDatabase( _db, _ptList )
          ;

          if( _ptList.updated ) then
            _ptList.populateDatabase( _db )
          ;

          if( _ptpList.updated ) then
            _ptpList.populateDatabase( _db, true )
          ;

          if( _ctrl.updated ) then
            _ctrl.populateDatabase( _db, true )
          ;

          if( _outputOptions.updated ) then
            _outputOptions.populateDatabase( _db, true )
          ;

          if( _customOutputDefinitions.updated ) then
            _customOutputDefinitions.populateDatabase( _db )
          ;

          if( _selectDailyOutputs.updated ) then
            _selectDailyOutputs.populateDatabase( _db )
          ;

          _updated := false;
        end
      ;
    end
  ;


  procedure TSMSimulationInput.removeDbFunction( const fnID: integer );
    begin
      if( nil <> _db ) then
        _db.removeChartFunction( fnID )
      else
        raise exception.create( '_db is nil in TSMSimulationInput.removeDbFunction()' )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
  function TSMSimulationInput.ssXml( writeOutputs: boolean ): string;
    begin
      if( _db = nil ) then
        raise exception.create( 'Database is not set in TSMSimulationInput' )
      ;

      debugSimpleProperties();

      // Write the file header
      //-----------------------
      result := '<?xml version="1.0" encoding="UTF-16" ?>' + endl;
      result := result + '<ergadm:disease-simulation' + endl;
      result := result + '  xmlns:ergadm="http://hebb.cis.uoguelph.ca/~dastacey/Grid/ERG_ADM"' + endl;
      result := result + '  xmlns:xdf="http://xml.gsfc.nasa.gov/XDF">' + endl;
      result := result + '  <description>' + encodeXml( _scenarioDescr ) + '</description>' + endl;

      if( _simStopReason in [ssStartAndStopAtEndOfOutBreak, ssStartAndStopAtFirstDetection] ) then
        result := result + '  <num-days>32767</num-days>' + endl
      else
        result := result + '  <num-days>' + intToStr( _simDays ) + '</num-days>' + endl
      ;
      result := result + '  <num-runs>'+ intToStr( _simIterations ) + '</num-runs>' + endl;
      result := result + endl;

      // Write the models header
      //-------------------------
      result := result + '<models>' + endl;
      result := result + endl;

      // Write the zone models
      //----------------------
      if( includeZonesGlobal ) then
        begin
          if( nil <> _zoneList ) then
            result := result + _zoneList.ssXml()
          else
            raise exception.Create( '_zoneList is nil in TSMSimulationInput.ssXml' );
          ;

          if( writeOutputs ) then // Write the zone monitor
            begin
              result := result
                + '  <zone-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-in-zone</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animal-days-in-zone-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>zone-shape</variable-name>' + endl
                + '      <frequency>weekly</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>zone-area</variable-name>' + endl
                + '      <frequency>weekly</frequency>' + endl
                + '    </output>' + endl
                + '  </zone-monitor>' + endl + endl
              ;
            end
          else // Write the zone monitor anyway: it's used to get the area of each zone
            begin
              result := result
                + '  <zone-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>zone-area</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '  </zone-monitor>' + endl + endl
              ;
            end
          ;
        end
      ;

      // Write the production type models (disease only)
      //-------------------------------------------------
      if( nil <> _ptList ) then
        result := result + _ptList.ssDiseaseModelsXml()
      ;

      // Write the contact and airborne spread models
      //---------------------------------------------
      if( includeContactSpreadGlobal or includeAirborneSpreadGlobal ) then
        begin
          if( nil <> _ptpList ) then
            result := result + _ptpList.ssXml()
          ;
        end
      ;

      // Using the infection monitor is a good way to generate output for reasons for infection, etc.
      // Some simple outputs from the infection monitor are used,
      // even if standard supercomputer outputs are not needed.
      if( writeOutputs ) then
        begin
          result := result + endl
            + '  <infection-monitor>' + endl
            + '    <ratio-period>' + endl
            + '      <value>7</value>' + endl
            + '      <units>day</units>' + endl
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
            + '      <variable-name>num-units-infected</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-infected-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-infected-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-infected-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-infected</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-infected-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-infected-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '     <output>' + endl
            + '      <variable-name>num-animals-infected-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-infected</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '     <output>' + endl
            + '      <variable-name>cumulative-num-units-infected-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-infected-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-infected-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-infected</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-infected-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-infected-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-infected-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </infection-monitor>' + endl
          ;
        end
      else
        begin
          result := result + endl
            + '  <infection-monitor>' + endl
            + '    <ratio-period>' + endl
            + '      <value>1</value>' + endl
            + '      <units>day</units>' + endl
            + '    </ratio-period>' + endl
            + '  </infection-monitor>' + endl
          ;
        end
      ;

      // Using the exposure monitor is a good way to generate output for reasons for exposure.
      // Exposure monitor has no parameters, unless standard supercomputer outputs are requested.
      if( writeOutputs ) then
        begin
          result := result + endl
            + '  <exposure-monitor>' + endl
            + '    <output>' + endl
            + '      <variable-name>exposures</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-exposed</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-exposed-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-exposed-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-units-exposed-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-exposed</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-exposed-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-exposed-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>num-animals-exposed-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-exposed</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-exposed-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-exposed-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-units-exposed-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-exposed</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-exposed-by-cause</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-exposed-by-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '    <output>' + endl
            + '      <variable-name>cumulative-num-animals-exposed-by-cause-and-production-type</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
            + '  </exposure-monitor>' + endl
          ;
        end
      else
        begin
          result := result + endl
            + '  <exposure-monitor>' + endl
            + '  </exposure-monitor>' + endl
          ;
        end
      ;


      // Write the control measures
      //----------------------------

      // Detection models
      if( includeDetectionGlobal ) then
      	begin
      		if( nil <> ptList ) then result := result + endl + _ptList.ssDetectionModelsXml();

          // Detection monitor has no parameters, unless standard supercomputer outputs are requested.

          if( writeOutputs ) then
            begin
              result := result + endl
                + '  <detection-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>detections</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-detected</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-detected-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-detected</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-detected-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>time-to-first-detection</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '  </detection-monitor>' + endl
              ;
            end
          else
            begin
              result := result + endl
                + '  <detection-monitor>' + endl
                + '  </detection-monitor>' + endl
              ;
            end
          ;
        end
      ;

      // Global controls
      if( nil <> _ctrl ) then result := result + _ctrl.ssXml();

      // (Quarantine model has no parameters)
      result := result + endl + '  <quarantine-model>' + endl + '  </quarantine-model>' + endl + endl;

      if( includeTracingGlobal ) then
        begin
          // Trace-back and destruction models
          if( ( nil <> ptList ) and ( nil <> _ctrl ) ) then
            begin
              result := result + ptList.ssTracebackDestructionModelsXml( _ctrl.ssDestrPriorities, includeDestructionGlobal );

              // Trace-back monitor is used only when supercomputer outputs are requested.
              if( writeOutputs ) then
                begin
                  result := result + endl
                    + '  <trace-back-monitor>' + endl
                    + '    <output>' + endl
                    + '      <variable-name>num-traces-attempted</variable-name>' + endl
                    + '      <frequency>daily</frequency>' + endl
                    + '    </output>' + endl
                    + '    <output>' + endl
                    + '      <variable-name>cumulative-num-traces-attempted</variable-name>' + endl
                    + '      <frequency>daily</frequency>' + endl
                    + '    </output>' + endl
                    + '    <output>' + endl
                    + '      <variable-name>num-contacts-potentially-traced</variable-name>' + endl
                    + '      <frequency>daily</frequency>' + endl
                    + '    </output>' + endl
                    + '    <output>' + endl
                    + '      <variable-name>cumulative-num-contacts-potentially-traced</variable-name>' + endl
                    + '      <frequency>daily</frequency>' + endl
                    + '    </output>' + endl
                    + '    <output>' + endl
                    + '      <variable-name>num-contacts-traced</variable-name>' + endl
                    + '      <frequency>daily</frequency>' + endl
                    + '    </output>' + endl
                    + '    <output>' + endl
                    + '      <variable-name>cumulative-num-contacts-traced</variable-name>' + endl
                    + '      <frequency>daily</frequency>' + endl
                    + '    </output>' + endl
                    + '  </trace-back-monitor>' + endl
                  ;
                end
              ;
            end
          ;
        end
      ;


      if( includeDestructionGlobal ) then
      	begin
        	// Basic destruction models
          if( ( nil <> ptList ) and ( nil <> _ctrl ) ) then
            result := result + ptList.ssBasicDestructionModelsXml( _ctrl.ssDestrPriorities )
          ;


          // Ring destruction models
          if( ( nil <> ptList ) and ( nil <> _ctrl ) ) then
            result := result + ptList.ssRingDestructionModelsXml()
          ;

          // Destruction monitor has no parameters, unless standard supercomputer outputs are requested.
          // For now, at least, the destruction list monitor is used only when SC outputs are requested.
          if( writeOutputs ) then
            begin
              result := result + endl
                + '  <destruction-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>destructions</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-destroyed</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-destroyed-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-destroyed-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-destroyed-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-destroyed</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-destroyed-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-destroyed-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-destroyed-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-destroyed</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-destroyed-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-destroyed-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-destroyed-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-destroyed</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-destroyed-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-destroyed-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-destroyed-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '  </destruction-monitor>' + endl
              ;

              result := result + endl
                + '  <destruction-list-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-awaiting-destruction</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-awaiting-destruction-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-awaiting-destruction</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-awaiting-destruction-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>peak-num-units-awaiting-destruction</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>peak-num-animals-awaiting-destruction</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>average-destruction-wait-time</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>peak-destruction-wait-time</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '  </destruction-list-monitor>' + endl
              ;
            end
          else
            begin
              result := result + endl
                + '  <destruction-monitor>' + endl
                + '  </destruction-monitor>' + endl
            end
          ;
        end
      ;


			if( includeVaccinationGlobal ) then
      	begin
          // Vaccination models
          if( nil <> ptList ) then
            result := result + ptList.ssVaccineModelsXml()
          ;

          // Ring vaccination models
          if( nil <> ptList ) then
            result := result + ptList.ssRingVaccModelsXml()
          ;

          // Vaccination monitor has no parameters, unless standard supercomputer outputs are requested.
          // For now, at least, the vaccination list monitor is used only when SC outputs are requested.
          if( writeOutputs ) then
            begin
              result := result + endl
                + '  <vaccination-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>vaccinations</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-vaccinated</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '     <output>' + endl
                + '      <variable-name>num-units-vaccinated-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-vaccinated-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-vaccinated-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-vaccinated</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-vaccinated-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-vaccinated-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-vaccinated-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-vaccinated</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-vaccinated-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-vaccinated-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-units-vaccinated-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-vaccinated</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-vaccinated-by-reason</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-vaccinated-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>cumulative-num-animals-vaccinated-by-reason-and-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '  </vaccination-monitor>' + endl
              ;

              result := result + endl
                + '  <vaccination-list-monitor>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-awaiting-vaccination</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-units-awaiting-vaccination-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-awaiting-vaccination</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>num-animals-awaiting-vaccination-by-production-type</variable-name>' + endl
                + '      <frequency>daily</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>peak-num-units-awaiting-vaccination</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>peak-num-animals-awaiting-vaccination</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>average-vaccination-wait-time</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '    <output>' + endl
                + '      <variable-name>peak-vaccination-wait-time</variable-name>' + endl
                + '      <frequency>once</frequency>' + endl
                + '    </output>' + endl
                + '  </vaccination-list-monitor>' + endl
              ;
            end
          else
            begin
              result := result + endl
                + '  <vaccination-monitor>' + endl
                + '  </vaccination-monitor>' + endl
              ;
            end
          ;
        end
      ;

      if( includeZonesGlobal ) then
        begin
          // Zone models for each production type
          if( nil <> ptList ) then
            result := result + endl + ptList.ssZoneModelsXml()
          ;
        end
      ;

      // Write the models footer
      //-------------------------
      // This seems to be required for the DLL to function properly
      // (Conflict resolver has no parameters)
      result := result + endl + '  <conflict-resolver>' + endl + '  </conflict-resolver>' + endl + endl;

      result := result +  '</models>' + endl;
      result := result + endl;


      // Write the outputs list (required for the DLL to generate them)
      //----------------------------------------------------------------
      result := result + '<output>' + endl;
      result := result + '  <variable-name>all-units-states</variable-name>' + endl;
      result := result + '  <frequency>daily</frequency>' + endl;
      result := result + '</output>' + endl;

      if( writeOutputs ) then
        begin
          result := result + endl
            + '<output>' + endl
            + '  <variable-name>num-units-in-each-state</variable-name>' + endl
            + '  <frequency>daily</frequency>' + endl
            + '</output>' + endl
            + '<output>' + endl
            + '  <variable-name>num-animals-in-each-state</variable-name>' + endl
            + '  <frequency>daily</frequency>' + endl
            + '</output>' + endl
            + '<output>' + endl
            + '  <variable-name>time-to-end-of-outbreak</variable-name>' + endl
            + '  <frequency>once</frequency>' + endl
            + '</output>' + endl
          ;
        end
      ;

      // Write the file footer
      //-----------------------
      result := result + endl + '</ergadm:disease-simulation>' + endl;
    end
  ;


  {*
  WARNING: this function will attempt to overwrite an existing file without notice.
  FIX ME: no error checking or validation is done.
  }
  function TSMSimulationInput.writeXMLFile( fileName: string; writeOutputs: boolean; errMsg: PString = nil ): boolean;
    var
      xmlFile: TextFile;
    begin
    	try
        assignUnicode( xmlFile, fileName );
        rewrite( xmlFile );
        writeln( xmlFile, ssXML( writeOutputs ) );
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

    if( includeZonesGlobal ) then
      begin
        if( not( _zoneList.validate( msg ) ) ) then
          result := false
        ;
      end
    ;

    if( not( _ptList.validate( msg ) ) ) then
    	result := false
    ;

    if( not( _ptpList.validate( msg ) ) ) then
      result := false
    ;

    if( _useCustomOutputs ) then
      begin
        if( not( _customOutputDefinitions.validate( msg ) ) )  then
          result := false
        ;
      end
    ;

    if( includeDestructionGlobal or includeVaccinationGlobal ) then
      begin
        if( not( _ctrl.validate( msg ) ) ) then
          result := false
        ;
      end
    ;

    if( not( skipOutputOptions ) ) then
      begin
        if( not( _outputOptions.validate( msg ) ) )  then
          result := false
        ;
      end
    ;
  end
;





procedure TSMSimulationInput.setParamQuestions( db: TSMDatabase );
  begin
    // FIX ME: Write this!!
    dbcout( 'WARNING: TSpreadModelSimulationInput doesn''t populate the new database yet', true );
  end
;


procedure TSMSimulationInput.setDefaultVaccPriorities( db: TSMDatabase );
  begin
    // FIX ME: Write this!!
    dbcout( 'WARNING: TSpreadModelSimulationInput doesn''t populate the new database yet', true );
  end
;


procedure TSMSimulationInput.setDefaultDestrPriorities( db: TSMDatabase );
  begin
    // FIX ME: Write this!!
    dbcout( 'WARNING: TSpreadModelSimulationInput doesn''t populate the new database yet', true );
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
      dbcout( 'updated: ' + usBoolToText( _updated ), true );
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
// properties
//-----------------------------------------------------------------------------
	function TSMSimulationInput.getPTList(): TProductionTypeList; begin result := _ptList; end;
  procedure TSMSimulationInput.setPtList( list: TProductionTypeList ); begin _ptList := list; _updated := true; end;

	function TSMSimulationInput.getPTPList(): TProductionTypePairList; begin result := _ptpList; end;
  procedure TSMSimulationInput.setPTPList( list: TProductionTypePairList ); begin _ptpList := list; _updated := true; end;

	function TSMSimulationInput.getZoneList(): TZoneList; begin result := _zoneList; end;
  procedure TSMSimulationInput.setZoneList( list: TZoneList ); begin _zoneList := list; _updated := true; end;

  procedure TSMSimulationInput.setVacStartNumber( val: integer ); begin _vacStartNumber := val; _updated := true; end;
  procedure TSMSimulationInput.setDestrStartDays( val: integer ); begin _destrStartDays := val; _updated := true; end;

  function TSMSimulationInput.getVacStartNumber(): integer; begin Result := _vacStartNumber; end;
  function TSMSimulationInput.getDestrStartDays(): integer; begin Result := _destrStartDays; end;

  procedure TSMSimulationInput.setIncludeContactSpread( val: boolean ); begin _includeContactSpread := val; _updated := true; end;
  procedure TSMSimulationInput.setIncludeAirborneSpread( val: boolean ); begin _includeAirborneSpread := val; _updated := true; end;
  procedure TSMSimulationInput.setUseAirborneExponentialDecay( val: boolean ); begin _useAirborneExponentialDecay := val; _updated := true; end;
  procedure TSMSimulationInput.setUseWithinHerdPrevalence( val: boolean ); begin _useWithinHerdPrevalence := val; _updated := true; end;

  procedure TSMSimulationInput.setCostTrackDestruction( val: boolean ); begin _costTrackDestruction := val; _updated := true; end;
  procedure TSMSimulationInput.setCostTrackVaccination( val: boolean ); begin _costTrackVaccination := val; _updated := true; end;
  procedure TSMSimulationInput.setCostTrackZoneSurveillance( val: boolean ); begin _costTrackZoneSurveillance := val; _updated := true; end;

  procedure TSMSimulationInput.setUseCustomOutputs( val: boolean ); begin _useCustomOutputs := val; _updated := true; end;


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

  procedure TSMSimulationInput.setSimDays( val: integer ); begin _simDays := val; _updated := true; end;
  procedure TSMSimulationInput.setSimStopReason( val: TStopReason ); begin _simStopReason := val; _updated := true; end;
  procedure TSMSimulationInput.setScenarioDescr( val: string ); begin _scenarioDescr := val; _updated := true; end;

  function TSMSimulationInput.getUpdated(): boolean;
    begin
      result := inherited getUpdated();

      if( false = result ) then result := _zoneList.updated;

      if( false = result ) then result := _ptList.updated;

      if( false = result ) then result := _ptpList.updated;

      if( false = result ) then result := _ctrl.updated;

      if( false = result ) then result := _outputOptions.updated;

      if( false = result ) then result := _customOutputDefinitions.updated;

      if( false = result ) then result := _selectDailyOutputs.updated;
    end
  ;
//-----------------------------------------------------------------------------

end.
