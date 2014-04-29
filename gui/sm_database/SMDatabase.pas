unit SMDatabase;

(*                            
SMDatabase.pas
--------------
Begin: 2005/01/07
Last revision: $Date: 2013-05-14 17:01:49 $ $Author: areeves $
Version: $Revision: 1.140.2.31 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{$INCLUDE ../Defs.inc}

interface

  uses
    // Standard Delphi units
    Forms,

    // General purpose units
    QStringMaps,
    SqlClasses,
    ChartFunction,
    I88n,
    RemoteMessenger,

    // The base class
    ModelDatabase
  ;

  const
    // These constants identify the version of the database schema
    // supported by the current application version.
    // The database schema is identified based on the application version
    // for which it was introduced.
    //
    // There is often but not necessarily a 1-to-1 relationship between
    // application version and database schema version: several application
    // versions may use the same database schema (e.g. applications 3.0.63
    // and 3.0.64 both used database schema 3.0.63).
    DB_SCHEMA_APPLICATION = 'NAADSMXXXX';
    DB_SCHEMA_VERSION = '4.1.0';

    DB_SCHEMA_DATE = '08/13/2013 6:03:25 PM';
    DB_SCHEMA_ID = '1376438605'; // Number of seconds from 1970-01-01 00:00:00 to schema date.
    DB_SCHEMA_INFO_URL = '';


  type TStopReason = (
    ssStopAtEndOfOutBreak = 0,
    ssStopAtFirstDetection = 1,
    ssStopAtSpecificDay = 2,
    ssStopAtDiseaseEnd = 3,
    ssStopReasonGuiDefined = 4,
    ssStopReasonUndefined = 6
  );


  type TSMDatabase = class( TModelDatabase )
    protected
      _remoteMessageID: integer;
      _remoteQuerySetCounter: integer;
      _remoteQuerySet: string;

      // Database functionality
      function executeWithoutChange( q: string ): boolean; override;
      procedure dropAllConstraints( const table: string );

      { All *should* return true on success, false on failure. }
      procedure setInGeneralDefaults();
      procedure setInControlsGlobalDefaults();
      procedure setupDynBlob();

      procedure deleteUnusedContactSpreadParams();

      function saveDailyOutputsForIterations(): integer;

      procedure makeDBTables(); override;
      procedure fillChartFields(); override;
      procedure setWorkingDBHasChanged( val: boolean ); override;
      function setUpdateReason(): TDBSchemaUpdateReason; override;
      function getSimulationComplete(): boolean; override;
      function getContainsOutput(): boolean; override;
      function getContainsValidOutput(): boolean;

      function getProdTypeCount(): integer;
      function getProdTypePairCount(): integer;

      procedure setSimDays( numDays: integer );
      function getSimDays(): integer;

      procedure setSimStopReason( val: TStopReason );
      function getSimStopReason(): TStopReason;

      procedure disconnectCustomOutputTables();
      procedure connectCustomOutputTables();

      procedure disconnectSelectDailyOutputTables();
      procedure connectSelectDailyOutputTables();

      function updateObsoleteCharts4_0_0(): boolean;
      function doubleCheckFirstDetectionOutputsFor4_0_8(): boolean;

      class function setSampleDatabaseLanguage( const fileName: string ): boolean;

      function validDateAndNumber( vNumber: string; vDate: TDateTime ): boolean;
      function schemaIDOK(): boolean; override;
      function versionObsolete(): boolean; override;
      function getCurrentDbSchemaVersion(): string; override;
      function getLanguageCode(): string; override;

      // Functions for database updates
      function populateSelectDailyOutputsFor4_0_1(): boolean;
      function populateSelectDailyOutputsFor4_0_5(): boolean;
      function populateSelectDailyOutputsFor4_0_6(): boolean;
      function populateSelectDailyOutputsFor4_0_8(): boolean;
      function populateSelectDailyOutputsFor4_0_9(): boolean;

    public
      // WARNING: it is up to the interface to check for the existence of the permanent database file.
      constructor create(
        const fileName: string;
        const dbAction: TSqlDBAction;
        errMsg: PChar = nil;
        parent: TForm = nil
      ); override;

      destructor destroy(); override;

      procedure remoteExecute( q: string );

      function save( newFileName: string = '' ): boolean; override;

      // Database functionality
      //-----------------------
      function execute( q: string ): boolean; override;

      // Function for checking that the database schema hasn't been screwed up by users
      //-------------------------------------------------------------------------------
      function tablesOK( errMsg: pstring = nil ): boolean;

      // Functions related to database schema
      //--------------------------------------
      function checkVersion( var updateReason: TDBSchemaUpdateReason ): TDBCheckResult; override;
      function versionUpdateReason( versionID: pstring = nil ): TVersionUpdateReason; override;


      { Update database to the latest schema. Return true if schema update was required.
        Set updateSuccess if update was attempted but unsuccessful. }
      function updateSchema( var updateSuccess: boolean ): boolean; override;

      function saveAllDailyOutputs(): boolean;

      // Functions related to model parameters
      //--------------------------------------
      function removeProductionType( const id: integer ): boolean;
      procedure removeProductionTypePairs();
      procedure removeZone( const id: integer );

      //function addProductionType( descr: string; ptid: integer = -1 ): integer;

      function changeProductionTypeDescr( const ptid: integer; const newName: string ): boolean;

      procedure makeProductionTypePair( src, dest: integer );

      procedure clearHerds();

      // Functions related to model output
      //----------------------------------
      procedure recordStartTime( const versionNumber: string ); override;
      procedure recordEndTime(); override;

      procedure initializeCustomOutputTables( list: TObject );
      procedure dropCustomOutputTables();

      procedure initializeSelectDailyOutputTables( sdo: TObject );
      procedure dropSelectDailyOutputTables();

      procedure setRngSeed( const val: integer ); // Used to record the seed used by the RNG for the simulation
      procedure initializeAllOutputRecords(); override; // Upon sim start
      procedure clearExistingFinalHerdStates( const setDatabaseChanged: boolean = true );
      procedure initializeRemoteDatabase();
      procedure prepareForIteration( currentIt: integer ); // Upon iteration start
      procedure incrementCompleteIterations(); override;
      procedure simComplete(); // upon sim complete

      // Upon iteration end
      procedure processIterationRecords(
        it: integer;
        outbreakEndDay: integer;
        outbreakEnded: boolean;
        diseaseEndDay: integer;
        diseaseEnded: boolean;
        zoneFociCreated: boolean
      );

      // WARNING: these functions assume that there are custom outputs to process.
      // Check list.hasIterationOutputs or list.hasProductionTypeOutputs before calling these functions.
      procedure processCustomIterationRecords( iteration: integer; list: TObject );
      procedure processCustomProductionTypeRecords( iteration: integer; list: TObject; ptList: TObject {TProductionTypeList} );
      procedure processCustomZoneRecords( iteration: integer; list: TObject; zoneList: TObject );
      procedure processCustomZonePTRecords( iteration: integer; list: TObject; zoneList: TObject; ptList: TObject );

      procedure deleteIncompleteIteration();

      // Functions for handling BLOBs
      //-----------------------------
      procedure clearZonePerimeters( const setDatabaseChanged: boolean = true );
      function containsZonePerimeters(): boolean;

      // Functions for handling herd imports
      //------------------------------------
      function lastHerdID(): integer;
      procedure makeTemporaryHerdTable();

      // Returns true on success, otherwise false.
      function mergeHerdTables(): boolean;


      // Function for creating a populated sample database
      //--------------------------------------------------
      class function makeSampleDatabase(
        const fileName: string;
        errCode: pinteger = nil;
        errMsg: pstring = nil
      ): boolean;

      
      // Properties and property-like functions
      //---------------------------------------
      function containsIncompleteIterations( completedIt: PInteger = nil; currentIt: PInteger = nil ): boolean;
      function lastIteration(): integer; // NOTE: this last iteration may or may not be complete!
      property containsValidOutput: boolean read getContainsValidOutput;
      
      function daysInLongestIteration(): integer;
      function daysInIteration( const it: integer ): integer;

      function containsInitiallyVaccinatedUnits( const prodTypeID: integer = -1 ): boolean;
      function containsInitiallyDestroyedUnits( const prodTypeID: integer = -1 ): boolean;

      property prodTypeCount: integer read getProdTypeCount;
      property prodTypePairCount: integer read getProdTypePairCount;

      property simDays: integer read getSimDays write setSimDays;
      property simStopReason: TStopReason read getSimStopReason write setSimStopReason;
    end
  ;

  function stopReasonToString( const val: TStopReason ): string;

  const
    DBSMDATABASE: boolean = false; // Set to true to enable debugging messages in this unit.

implementation

  {$R 'sm_database\DatabaseCreation.res' 'sm_database\DatabaseCreation.rc'}

  uses
    // Standard Delphi units
    Windows,
    SysUtils,
    Variants,
    StrUtils,
    Classes,
    
    // General purpose units
    //CStringList,
    Resources,
    MyDialogs,
    MyStrUtils,
    DebugWindow,
    WindowsUtils,
    NetworkUtils,
    FunctionPointers,
    ProbDensityFunctions,
    ZipFunctions,

    // More database functions
    SMDatabaseFnsVers3,
    RemoteDatabaseParams,

    // Application specific data structures
    FunctionDictionary,
    SMSimulationInput,
    CustomOutputDefinitions,
    StringConsts,
    Herd,
    RelFunction,
    FunctionEnums,
    ProductionType,
    ProductionTypeList,
    Zone,
    SelectDailyOutputs

    {$IFNDEF CONSOLEAPP}
    ,
    // Application-specific widgets
    FormMain, Dialogs
    {$ENDIF}
  ;


// ----------------------------------------------------------------------------
// Global helper functions
// ----------------------------------------------------------------------------
  function stopReasonToString( const val: TStopReason ): string;
    begin
      case val of
        ssStopAtEndOfOutBreak: result := 'outbreakEnd';
        ssStopAtFirstDetection: result := 'firstDetection';
        ssStopAtSpecificDay: result := 'specifiedDay';
        ssStopAtDiseaseEnd: result := 'diseaseEnd';
      else
        begin
          dbmsg( 'Unrecognized stop reason in stopReasonToString', true );
          result := 'outbreakEnd';
        end
      ;
      end;
    end
  ;


  function stopReasonFromString( const val: string ): TStopReason;
    var
      val2: string;
    begin
      val2 := fixup( val );

      if( 'outbreakend' = val2 ) then
        result := ssStopAtEndOfOutBreak
      else if( 'firstdetection' = val2 ) then
        result := ssStopAtFirstDetection
      else if( 'specifiedday' = val2 ) then
        result := ssStopAtSpecificDay
      else if( 'diseaseend' = val2 ) then
        result := ssStopAtDiseaseEnd
      else
        begin
          dbmsg( 'Unrecognized stop reason in stopReasonFromString', true );
          result := ssStopAtEndOfOutBreak;
        end
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Private functions for error checking in specific versions under specialized
// conditions.
// ----------------------------------------------------------------------------
  // Returns true if the scenario database contains more than 1 zone.
  // See http://www.naadsm.org/bugs?id=180 for description.
  function zoneBug4_0_12( db: TSMDatabase ): boolean;
    var
      res: TSqlResult;
    begin
      // If this scenario uses two or more levels of zones, a situation
      // can occur where contacts that should be forbidden may still occur.
      // If fewer than 2 zone levels are present, then outputs are OK.

      res := TSqlResult.create( 'SELECT zoneID FROM `inZone`', (db as TSqlDatabase) );

      result := ( 0 < res.numRows );

      res.Free();
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Construction/initialization/destruction
// ----------------------------------------------------------------------------
  constructor TSMDatabase.create(
        const fileName: string;
        const dbAction: TSqlDBAction;
        errMsg: PChar = nil;
        parent: TForm = nil
      );
    begin
      inherited create( fileName, dbAction, errMsg, parent );

      _remoteMessageID := 1;
      _remoteQuerySetCounter := 0;
      _remoteQuerySet := '';

      if( not( self.containsOutput ) ) then
        self.clearExistingFinalHerdStates( false )
      ;
    end
  ;


  procedure TSMDatabase.fillChartFields();
    begin
      // Table inDiseaseSpread
      _chartFields.add( TChartField.create( 'inDiseaseSpread', 'movementControlRelID' ) );
      _chartFields.add( TChartField.create( 'inDiseaseSpread', 'distancePdfID' ) );
      _chartFields.add( TChartField.create( 'inDiseaseSpread', 'dirPropnUnitsInShipmentPdfID' ) );

      // Table inProductionType
      _chartFields.add( TChartField.create( 'inProductionType', 'disLatentPeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disSubclinicalPeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disClinicalPeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disImmunePeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disPrevalenceInfectedRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disPrevalenceSheddingRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'detProbObsVsTimeClinicalRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'detProbObsVsTimeDeadRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'detProbReportClinVsFirstDetectionRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'detProbReportDeadVsFirstDetectionRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'traceDelayPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'vaccImmunePeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'testDelayPdfID' ) );

      // Table inControlsGlobal
      _chartFields.add( TChartField.create( 'inControlsGlobal', 'destrCapacityRelID' ) );
      _chartFields.add( TChartField.create( 'inControlsGlobal', 'vaccCapacityRelID' ) );

      // Table inZoneProductionTypePair
      _chartFields.add( TChartField.create( 'inZoneProductionTypePair', 'zoneDirectMovementRelID' ) );
      _chartFields.add( TChartField.create( 'inZoneProductionTypePair', 'zoneIndirectMovementRelID' ) );
    end
  ;


  destructor TSMDatabase.destroy();
    begin
      inherited destroy();
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Database functionality
// ----------------------------------------------------------------------------
  function TSMDatabase.execute( q: string ): boolean;
    begin
      result := inherited execute( q );

      if( not result ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:' ) + ' ' + q )
      ;
    end
  ;


  function TSMDatabase.executeWithoutChange( q: string ): boolean;
    begin
      result := inherited executeWithoutChange( q );

      if( not result ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:' ) + ' ' + q )
      ;
    end
  ;


  procedure TSMDatabase.dropAllConstraints( const table: string );
    var
      list: TStringList;
      i: integer;
      q: string;
    begin
      list := createTableConstraintList( table );

      for i := 0 to list.count - 1 do
        begin
          q := 'ALTER TABLE `' + table + '` DROP CONSTRAINT `' + list.Strings[ i ] + '`';
          executeWithoutChange( q );
        end
      ;

      list.Free();
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Database schema update
// ----------------------------------------------------------------------------
  function TSMDatabase.setUpdateReason(): TDBSchemaUpdateReason;
    begin
      result := checkUpdateReasonsVers3( _vNumber );

      if( '4.0.0' = _vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateSpecChange;
        end

      else if( '4.0.1' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      else if( '4.0.3' = _vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateSpecChange;
        end

      else if( '4.0.5' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      else if( '4.0.6' = _vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateSpecChange;
        end

      else if( '4.0.7' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      else if( '4.0.8' = _vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateSpecChange;
        end

      else if( '4.0.9' = _vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateMinorChanges;
        end

      // There will be more to do here some day.

      // The last entry here should be the current schema version.
      // This exception ensures that someone doesn't get too carried away with copy/paste
      else if( DBUpdateOK = result ) then
        raise exception.create( 'Looks like you forgot something in TSMDatabase.setUpdateReason()')
      else if( '4.1.0' = _vNumber ) then
        begin
          if( DBUpdateOK > result ) then result := DBUpdateOK;
        end
      ;
    end
  ;


  function TSMDatabase.updateObsoleteCharts4_0_0(): boolean;
    var
      row: TSqlRow;
      q: string;
      r1, r2: boolean;
    begin
       if( nil = _sqlResult ) then
        createSqlResult()
      ;

      q := 'SELECT `chartID` FROM `inChart`'
        + ' WHERE `fieldName` = "AIRDelay"'
        + ' OR `fieldName` = "CMDelayIndirect"'
        + ' OR `fieldName` = "CMDelayDirect"'
      ;

      _sqlResult.runQuery( q );

      r1 := true;

      if( _sqlResult.success ) then
        begin
          row := _sqlResult.fetchArrayFirst();
          while( nil <> row ) do
            begin
              r1 :=
                r1
              and
                removeChartFunction( integer( row.field( 'chartID' ) ) )
              ;

              row := _sqlResult.fetchArrayNext();
            end
          ;
        end
      else
        r1 := false
      ;

      r2 := self.execute( 'UPDATE `inChart` SET `fieldName` = "DPrevInfected" WHERE `fieldName` = "DPrevalence"' );

      result := ( r1 and r2 );
    end
  ;


  {*
    Returns true if the database schema was actually updated.
  }
  function TSMDatabase.updateSchema( var updateSuccess: boolean ): boolean;
    var
      execSuccess: boolean;
    begin
      updateSuccess := true;

      result := updateSchemaVers3( self, _vNumber, updateSuccess );

      if( not( updateSuccess ) ) then
        exit
      ;

      // If very old updates went OK, keep going...

      // For all subsequent updates, make sure that constraints on the
      // custom output tables don't prevent the updates from being applied.
      disconnectCustomOutputTables();
      disconnectSelectDailyOutputTables();

      // Update 3.2.2 or 3.2.5 to 4.0.0
      //-------------------------------
      if
        ( '3.2.2' = _vNumber )
      or
        ( '3.2.5' = _vNumber )
      then
        begin
          try
            dbcout( 'Updating from 3.2.2 or 3.2.5', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema4_0_0' ) )
            and
              updateObsoleteCharts4_0_0()
            ;

            // Other updates made to schema 3.2.5 are taken care of in the update to 4.0.3 below.

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;


      // Update schema 3.2.11 to 3.2.13
      //-------------------------------
      if( '3.2.11' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.11', true );

            execSuccess :=
              self.execute( 'alter table outIterationByProductionType add column firstDetUInf long' )
            and
              self.execute( 'alter table outIterationByProductionType add column firstDetAInf long' )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.13';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update schema 3.2.13 or 3.2.18 directly to 4.0.5, skipping intermediate steps.
      //-------------------------------------------------------------------------------
      if( ( '3.2.13' = _vNumber ) or ( '3.2.18' = _vNumber ) ) then
        begin
          try
            dbcout( 'Updating from 3.2.13 or 3.2.18', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema3_2_13_to_4_0_5' ) )
            and
              updateObsoleteCharts4_0_0()
            and
              populateSelectDailyOutputsFor4_0_1()
            and
              populateSelectDailyOutputsFor4_0_5()
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.5';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 4.0.0 to 4.0.1
      //----------------------
      if( '4.0.0' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.0', true );

            execSuccess :=
              ( processDDLCommands( getResourceAsString( 'DBSchema4_0_1' ) ) )
            and
              ( populateSelectDailyOutputsFor4_0_1() )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.1';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 4.0.1 to 4.0.3
      //----------------------
      if( '4.0.1' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.1', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema4_0_3' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.3';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 4.0.3 to 4.0.5
      //----------------------
      if( '4.0.3' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.3', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema4_0_5' ) )
            and
              populateSelectDailyOutputsFor4_0_5()
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.5';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 4.0.5 to 4.0.6
      //----------------------
      if( '4.0.5' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.5', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema4_0_6' ) )
            and
              populateSelectDailyOutputsFor4_0_6()
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.6';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 4.0.6 to 4.0.7
      //----------------------
      if( '4.0.6' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.6', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema4_0_7' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.7';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 4.0.7 to 4.0.8
      //----------------------
      if( '4.0.7' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.7', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema4_0_8' ) )
            and
              populateSelectDailyOutputsFor4_0_8()
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.8';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Double-check 4.0.8 to make sure that firstDetUInf and firstDetAInf actually exist
      // (Due to some bugs in 4.0.6 and 4.0.7, they didn't...)
      //-----------------------------------------------------------------------------------
      if not( doubleCheckFirstDetectionOutputsFor4_0_8() ) then
        begin
          updateSuccess := false;
          exit;
        end
      ;

      // Update 4.0.8 to 4.0.9
      //----------------------
      if( '4.0.8' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.8', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema4_0_9' ) )
            and
              populateSelectDailyOutputsFor4_0_9()
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.0.9';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Updated 4.0.9 to 4.1.0
      //-----------------------
      if( '4.0.9' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 4.0.9', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema4_1_0' ) )
            and
              self.execute( 'UPDATE `inGeneral` SET `useLASSizeAdjustment` = TRUE' )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '4.1.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      //------ IMPORTANT -----------------------------------------------------
      // DON'T FORGET: when updating this function, also update makeDBTables()
      //----------------------------------------------------------------------

      // (There will eventually be more to do here...)

      // Finally, restore constraints
      // and update the version numbers, etc.
      //-------------------------------------
      connectCustomOutputTables();
      connectSelectDailyOutputTables();

      if( result ) then
        setSchemaVersion( DB_SCHEMA_VERSION, DB_SCHEMA_APPLICATION, DB_SCHEMA_DATE, DB_SCHEMA_INFO_URL, DB_SCHEMA_ID )
      ;
    end
  ;


  function TSMDatabase.doubleCheckFirstDetectionOutputsFor4_0_8(): boolean;
    begin
      try
        result := true;

        if( not( self.fieldExists( 'firstDetUInf', 'outIterationByProductionType' ) ) ) then
          result := result and self.execute( 'ALTER TABLE outIterationByProductionType ADD COLUMN firstDetUInf LONG' )
        ;

        if( not( self.fieldExists( 'firstDetAInf', 'outIterationByProductionType' ) ) ) then
          result := result and self.execute( 'ALTER TABLE outIterationByProductionType ADD COLUMN firstDetAInf LONG' )
        ;
      except
        result := false;
      end;
    end
  ;


  function TSMDatabase.schemaIDOK(): boolean;
    var
      row: TSqlRow;
    begin
      // Recall that a database schema version corresponds to the
      // application version in which it was introduced, and that
      // not all new application versions introduce changes to the schema.
      // Consequently, database schema versions are not necessarily sequential.

      result := false;

      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT `VersionID` FROM `DBSchemaVersion`' );

      if( _sqlResult.success ) then
        begin
          row := _sqlResult.fetchArrayFirst();
          if( null <> row.field('VersionID') ) then
            begin
              _vID := row.field('VersionID');

              if
                ( ( '4.0.0' = _vNumber ) and ( 1257620062 = _vID ) )
              or
                ( ( '4.0.1' = _vNumber ) and ( 1264532621 = _vID ) )
              or
                ( ( '4.0.3' = _vNumber ) and ( 1297726375 = _vID ) )
              or
                ( ( '4.0.5' = _vNumber ) and ( 1300929986 = _vID ) )
              or
                ( ( '4.0.6' = _vNumber ) and ( 1314034139 = _vID ) )
              or
                ( ( '4.0.7' = _vNumber ) and ( 1315944560 = _vID ) )
              or
                ( ( '4.0.8' = _vNumber ) and ( 1317423145 = _vid ) )
              or
                ( ( '4.0.9' = _vNumber ) and ( 1318546862 = _vid ) )
              or
                ( ( '4.1.0' = _vNumber ) and ( 1376438605 = _vid ) )
              then
                result := true
              ;
            end
          ;
        end
      ;
    end
  ;


  function TSMDatabase.validDateAndNumber( vNumber: string; vDate: TDateTime ): boolean;
    begin
      // Recall that a database schema version corresponds to the
      // application version in which it was introduced, and that
      // not all new application versions introduce changes to the schema.
      // Consequently, database schema versions are not necessarily sequential.

      if
        ( schemaIDOK() )
      or
       versionIsValidVers3( self, vNumber, vDate )
      then // the date and number/ID are valid.
        result := true
      else // the date and number/ID are not recognized as NAADSM databases.
        result := false
      ;
    end
  ;


  function TSMDatabase.versionObsolete(): boolean;
    begin
      // Add more numbers here, if it ever becomes necessary.
      // (Hopefully it won't for a long, long time.)

      result := versionObsoleteVers3( _vNumber );
    end
  ;


  function TSMDatabase.getCurrentDbSchemaVersion(): string;
    begin
      result := DB_SCHEMA_VERSION;
    end
  ;


  // No language code is set in the database until it is saved.
  // When saved for the first time, a database will get the language
  // code of the running GUI.  This language code then sticks with
  // the database for life.
  function TSMDatabase.getLanguageCode(): string;
    var
      res: TSqlResult;
      row: TSqlRow;
      query: string;
    begin
      query := 'SELECT `language` FROM `inGeneral`';
      res := TSqlResult.create( query, ( self as TSqlDatabase ) );
      row := res.fetchArrayFirst();

      if( null = row.field( 'language' ) ) then
        result := ''
      else
        result := row.field( 'language' )
      ;

      res.Free();
    end
  ;


  // There are many, many ways that users could screw up the database.
  // This function checks for only the ones that have actually been reported.
  // Updating it is not a high priority, but new capabilites could be added as needed.
  function TSMDatabase.tablesOK( errMsg: pstring = nil ): boolean;
    begin
      if
        ( 0 <> self.count( 'SELECT herdID FROM dynHerd WHERE productionTypeID IS NULL' ) )
      or
        ( 0 <> self.count( 'SELECT herdID FROM dynHerd WHERE latitude IS NULL' ) )
      or
        ( 0 <> self.count( 'SELECT herdID FROM dynHerd WHERE longitude IS NULL' ) )
      or
        ( 0 <> self.count( 'SELECT herdID FROM dynHerd WHERE initialSize IS NULL' ) )
      or
        ( 0 <> self.count( 'SELECT herdID FROM dynHerd WHERE initialStateCode IS NULL' ) )
      then
        begin
          if( nil <> errMsg ) then
            errMsg^ := tr( 'Some units are improperly specified.' );
          result := false;
        end
      else
        result := true
      ;
    end
  ;



  // This function overrides the function in the base class entirely:
  // NAADSM needs to handle some obsolete situations that the base class can safely ignore.
  function TSMDatabase.checkVersion( var updateReason: TDBSchemaUpdateReason ): TDBCheckResult;
    var
      row: TSqlRow;
      vDate: TDateTime;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT `versionNumber`, `versionDate` FROM `DBSchemaVersion`' );

      if( not( _sqlResult.success ) ) then
        result := DBVersionUnrecognized
      else if( 1 <> _sqlResult.numRows ) then
        result := DBVersionUnrecognized
      else
        begin
          row := _sqlResult.fetchArrayFirst();
          _vNumber := fixup( row.field('VersionNumber') );
          vDate := row.field('VersionDate');

          if( not( validDateAndNumber( _vNumber, vDate ) ) ) then
            result := DBVersionUnrecognized
          else
            begin
              if( versionObsolete() ) then
                result := DBVersionObsolete
              else if( DB_SCHEMA_VERSION = _vNumber ) then
                begin
                  updateReason := DBUpdateOK;
                  result := DBVersionCurrent;
                end
              else
                begin
                  // This database can be updated.
                  updateReason := setUpdateReason();
                  result := DBVersionUpdated;
                end
              ;
            end
          ;
        end
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// New database creation
// ----------------------------------------------------------------------------
  function TSMDatabase.populateSelectDailyOutputsFor4_0_1(): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := self.sqlQuote( DB_SCHEMA_APPLICATION );

      // This is tedious, but it's the only safe way to guard against future changes.
      dict.insert( 'sinfnUAll', 'false' );
      dict.insert( 'sinfnAAll', 'false' );
      dict.insert( 'sinfcUAll', 'false' );
      dict.insert( 'sinfcAAll', 'false' );

      dict.insert( 'sdetnUDead', 'false' );
      dict.insert( 'sdetnADead', 'false' );
      dict.insert( 'sdetcUDead', 'false' );
      dict.insert( 'sdetcADead', 'false' );

      dict.insert( 'sdetnUAll', 'false' );
      dict.insert( 'sdetnAAll', 'false' );
      dict.insert( 'sdetcUAll', 'false' );
      dict.insert( 'sdetcAAll', 'false' );

      dict.insert( 'sdeadnUInDestrQueue', 'false' );
      dict.insert( 'sdeadnAInDestrQueue', 'false' );
      dict.insert( 'sdeadcUInDestrQueue', 'false' );
      dict.insert( 'sdeadcAInDestrQueue', 'false' );

      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := self.execute( q );
    end
  ;


  function TSMDatabase.populateSelectDailyOutputsFor4_0_5(): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := self.sqlQuote( DB_SCHEMA_APPLICATION );

      dict.insert( 'stonUDirFwd', 'false' );
      dict.insert( 'stonUIndFwd', 'false' );
      dict.insert( 'stonUDirBack', 'false' );
      dict.insert( 'stonUIndBack', 'false' );

      dict.insert( 'stocUDirFwd', 'false' );
      dict.insert( 'stocUIndFwd', 'false' );
      dict.insert( 'stocUDirBack', 'false' );
      dict.insert( 'stocUIndBack', 'false' );

      // This is tedious, but it's the only safe way to guard against future changes.
      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := self.execute( q );
    end
  ;


  function TSMDatabase.populateSelectDailyOutputsFor4_0_6(): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := self.sqlQuote( DB_SCHEMA_APPLICATION );

      //AR:deadDV
      dict.insert( 'sdetnUDeadD', 'false' );
      dict.insert( 'sdetnADeadD', 'false' );
      dict.insert( 'sdetnUDeadV', 'false' );
      dict.insert( 'sdetnADeadV', 'false' );
      dict.insert( 'sdetcUDeadD', 'false' );
      dict.insert( 'sdetcADeadD', 'false' );
      dict.insert( 'sdetcUDeadV', 'false' );
      dict.insert( 'sdetcADeadV', 'false' );
      dict.insert( 'sdescUDcd', 'false' );
      dict.insert( 'sdescADcd', 'false' );

      // This is tedious, but it's the only safe way to guard against future changes.
      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := self.execute( q );
    end
  ;


  function TSMDatabase.populateSelectDailyOutputsFor4_0_8(): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := self.sqlQuote( DB_SCHEMA_APPLICATION );

      dict.insert( 'sdesnUDcd', 'false' );
      dict.insert( 'sdesnADcd', 'false' );
      dict.insert( 'sdesnUDcdInDestrQueue', 'false' );
      dict.insert( 'sdesnADcdInDestrQueue', 'false' );

      // This is tedious, but it's the only safe way to guard against future changes.
      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := self.execute( q );
    end
  ;


  function TSMDatabase.populateSelectDailyOutputsFor4_0_9(): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := self.sqlQuote( DB_SCHEMA_APPLICATION );

      dict.insert( 'sdetnUDeadAll', 'false' );
      dict.insert( 'sdetnADeadAll', 'false' );
      dict.insert( 'sdetcUDeadAll', 'false' );
      dict.insert( 'sdetcADeadAll', 'false' );

      dict.insert( 'sdescUDcdInDestrQueue', 'false' );
      dict.insert( 'sdescADcdInDestrQueue', 'false' );

      // This is tedious, but it's the only safe way to guard against future changes.
      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := self.execute( q );
    end
  ;


  procedure TSMDatabase.makeDBTables();
    begin
      // FIX ME: Consider making one new database completely from stratch.
      makeTablesVers3( self );
      processDDLCommands( getResourceAsString( 'DBSchema4_0_0' ) );
      processDDLCommands( getResourceAsString( 'DBSchema4_0_1' ) );
      // Additional values for the table inSelectDailyOutputs are new in version 4.0.1
      populateSelectDailyOutputsFor4_0_1();
      // 4.0.3 makes the same adjustments to y units in relational functions as 3.2.5
      processDDLCommands( getResourceAsString( 'DBSchema4_0_3' ) );
      // 4.0.5 adds a separate function for probability of reporting dead units
      processDDLCommands( getResourceAsString( 'DBSchema4_0_5' ) );
      populateSelectDailyOutputsFor4_0_5();
      // 4.0.6 adds detection of dead units by destruction or vaccination crews
      processDDLCommands( getResourceAsString( 'DBSchema4_0_6' ) );
      populateSelectDailyOutputsFor4_0_6();
      // 4.0.7 distinguishes between different types of detection in outDailyEvents
      processDDLCommands( getResourceAsString( 'DBSchema4_0_7' ) );
      // 4.0.8 changes field types for deswAAll and vacwAAll and adds some new fields for daily destructions.
      // Also, double-check fields for firstDetUInf and firstDetAInf.
      processDDLCommands( getResourceAsString( 'DBSchema4_0_8' ) );
      populateSelectDailyOutputsFor4_0_8();
      doubleCheckFirstDetectionOutputsFor4_0_8();
      // 4.0.9 introduced outputs for number of unique herds detected in the dead from disease state.
      // These are necessary for proper cost accounting.
      processDDLCommands( getResourceAsString( 'DBSchema4_0_9' ) );
      populateSelectDailyOutputsFor4_0_9();
      // 4.1.0 introduced options to limit the use of size adjustments for local-area spread
      // and to use previously experimental capabilities to randomize selection of initially infected herds.
      processDDLCommands( getResourceAsString( 'DBSchema4_1_0' ) );

      // Add additional DDL 'files' here as needed

      // Populate the version table
      setSchemaVersion( DB_SCHEMA_VERSION, DB_SCHEMA_APPLICATION, DB_SCHEMA_DATE, DB_SCHEMA_INFO_URL, DB_SCHEMA_ID );

      // Populate outGeneral
      self.execute(
        'INSERT INTO `outGeneral` ( `outGeneralID`, `simulationStartTime`, `simulationEndTime`, `version`, `completedIterations` ) '
          + ' VALUES ( ' + sqlQuote( DB_SCHEMA_APPLICATION ) + ', NULL, NULL, NULL, 0 )'
        )
      ;

      // Populate the database with any necessary default values.
      setInGeneralDefaults();
      setInControlsGlobalDefaults();
      setupDynBlob();
    end
  ;


  procedure TSMDatabase.setInGeneralDefaults();
    var
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['inGeneralID'] := sqlQuote( DB_SCHEMA_APPLICATION );

      dict['scenarioDescr'] := sqlQuote( 'New scenario' );
      dict['iterations'] := intToStr( 1 );
      dict['days'] := intToStr( 10 );

      dict['includeContactSpread'] := self.sqlBool( false );
      dict['includeAirborneSpread'] := self.sqlBool( false );
      dict['includeLocalAreaSpread'] := self.sqlBool( false );
      dict['useWithinHerdPrevalence'] := self.sqlBool( false );

      dict['costTrackDestruction'] := self.sqlBool( false );
      dict['costTrackVaccination'] := self.sqlBool( false );
      dict['costTrackZoneSurveillance'] := self.sqlBool( false );

      dict['useFixedRandomSeed'] := self.sqlBool( false );
      dict['randomSeed'] := intToStr( 527 );

      dict['saveAllDailyOutputs'] := self.sqlBool( false );
      dict['saveDailyOutputsForIterations'] := intToStr( 3 );

      dict['writeDailyStatesFile'] := self.sqlBool( false );
      dict['dailyStatesFileName'] := DATABASE_NULL_VALUE;

      dict['saveDailyEvents'] := self.sqlBool( false );
      dict['saveDailyExposures'] := self.sqlBool( false );
      dict['saveIterationOutputsForHerds'] := self.sqlBool( false );

      dict['useCustomOutputs'] := self.sqlBool( false );

      dict['writeNAADSMapOutput'] := self.sqlBool( false );
      dict['NAADSMapDirectory'] := DATABASE_NULL_VALUE;

      dict['initInfectedRandomize'] := usBoolToText( false );

      dict['useLASSizeAdjustment'] := usBoolToText( false );

      execute( writeQuery( 'inGeneral', QInsert, dict ) );
      dict.Free();
    end
  ;


  procedure TSMDatabase.setInControlsGlobalDefaults();
    var
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['controlsGlobalID']   := sqlQuote( DB_SCHEMA_APPLICATION );
      dict['includeDestruction'] := self.sqlBool( false );
      dict['includeDetection']   := self.sqlBool( false );
      dict['includeVaccination'] := self.sqlBool( false );
      dict['includeTracing']     := self.sqlBool( false );
      dict['includeTracingHerdExam']     := self.sqlBool( false );
      dict['includeTracingTesting']     := self.sqlBool( false );
      dict['includeZones']       := self.sqlBool( false );

      execute( writeQuery( 'inControlsGlobal', QInsert, dict ) );
      dict.Free();
    end
  ;


  procedure TSMDatabase.setupDynBlob();
    var
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['dynBlobID'] := sqlQuote( DB_SCHEMA_APPLICATION );
      dict['zonePerimeters'] := DATABASE_NULL_VALUE;

      execute( writeQuery( 'dynBlob', QInsert, dict ) );
      
      dict.Free();
    end
  ;


  procedure TSMDatabase.deleteUnusedContactSpreadParams();
    var
      q: string;
    begin
      (*
      //--------------------------------------------
      // The original query:
      //--------------------------------------------
        DELETE FROM inDiseaseSpread
        WHERE spreadID NOT IN
        (
        SELECT
          inDiseaseSpread.spreadID
        # inDiseaseSpread.spreadMethodCode AS code,
        # inProductionTypePair.directContactSpreadID AS ptpDirID,
        # inProductionTypePair.indirectContactSpreadID AS ptpIndID,
        # inProductionTypePair.airborneContactSpreadID AS ptpAirID,
        # inProductionTypePair.sourceProductionTypeID AS ptpSrcID,
        # inProductionTypePair.destProductionTypeID AS ptpDestID
        FROM  inProductionTypePair
        LEFT JOIN  inDiseaseSpread
          ON inDiseaseSpread.spreadID = inProductionTypePair.directContactSpreadID
          OR inDiseaseSpread.spreadID = inProductionTypePair.indirectContactSpreadID
          OR inDiseaseSpread.spreadID = inProductionTypePair.airborneContactSpreadID
        ORDER BY inDiseaseSpread.spreadID
        );
      //--------------------------------------------
      *)

      dbcout( '*** deleteUnusedContactSpreadParams', DBSMDATABASE );

      q :=
        'DELETE FROM inDiseaseSpread'
        + ' WHERE spreadID NOT IN'
        + ' ('
        + ' SELECT'
        + '   inDiseaseSpread.spreadID'
        + ' FROM  inProductionTypePair'
        + ' LEFT JOIN  inDiseaseSpread'
        + '   ON inDiseaseSpread.spreadID = inProductionTypePair.directContactSpreadID'
        + '   OR inDiseaseSpread.spreadID = inProductionTypePair.indirectContactSpreadID'
        + '   OR inDiseaseSpread.spreadID = inProductionTypePair.airborneContactSpreadID'
        + '   OR inDiseaseSpread.spreadID = inProductionTypePair.localAreaSpreadID'
        + ' ORDER BY inDiseaseSpread.spreadID'
        + ' )'
      ;

      if( not isReadOnly ) then
        inherited execute( q ) // Yes, there is a reason for this.
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// File I/O
// ----------------------------------------------------------------------------
  function TSMDatabase.save( newFileName: string = '' ): boolean;
    begin
      // Right before saving, clean up the database
      deleteUnusedContactSpreadParams();

      if( '' = languageCode ) then
        self.execute( 'UPDATE `inGeneral` SET `language` = ' + sqlQuote( i88nLanguageCodeString() ) )
      ;
      result := inherited save( newFileName );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Database modification
// ----------------------------------------------------------------------------
  procedure TSMDatabase.remoteExecute( q: string );
    begin
      if( nil <> _rm ) then
        begin
          _remoteQuerySet := _remoteQuerySet + q + ';' + endl;
          inc( _remoteQuerySetCounter );

          //if( 0 < _remoteQuerySetCounter ) then
          if( 6000 < length( _remoteQuerySet ) ) then
            begin
              _remoteQuerySet := _remoteQuerySet
                + 'INSERT INTO `message` ( `messageID` , `jobID` )'
                + ' VALUES ('
                + ' ' + intToStr( _remoteMessageID )
                + ', ' + intToStr( remoteDBParams.jobID )
                + ' );'
              ;
              inc( _remoteMessageID );

              _rm.queueRaw( _remoteQuerySet ); // use queuePost() to send a form-encoded message
              _remoteQuerySet := '';
              _remoteQuerySetCounter := 0;
              Application.ProcessMessages();
            end
          ;
        end
      else
        raise exception.create( '_rm is nil in TSMDatabase.remoteExecute()' )
      ;
    end
  ;


  procedure TSMDatabase.recordStartTime( const versionNumber: string );
    var
      q: string;
      begin
      inherited recordStartTime( versionNumber );

      // Set up the remote database, if necessary
      //-----------------------------------------
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'INSERT INTO `outGeneral`'
            + ' ( `jobID`, `outGeneralID`, `simulationStartTime`, `simulationEndTime`, `completedIterations`, `version` )'
            + ' VALUES ( '
            + intToStr( remoteDBParams.jobID )
            + ', ''' + DB_SCHEMA_APPLICATION + ''', NOW(), NULL, 0, ''' + versionNumber + ''' )'
          ;
          remoteExecute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.recordEndTime();
    var
      q: string;
    begin
      inherited recordEndTime();

      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'UPDATE `outGeneral` SET `simulationEndTime` = NOW() WHERE `jobID` = ' + intToStr( remoteDBParams.jobID );
          remoteExecute( q );
        end
      ;
    end
  ;

  procedure TSMDatabase.setRngSeed( const val: integer );
    var
      q: string;
    begin
      q := 'UPDATE `inGeneral` SET `randomSeed` = ' + intToStr( val );
      execute( q );

      (*
      // FIX ME: Right now, remote databases don't contain table inGeneral.
      // Right now, I don't think that they should...
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := q + ' WHERE `jobID` = ' + intToStr( remoteDBParams.jobID );
          remoteExecute( q );
        end
      ;
      *)
    end
  ;


  procedure TSMDatabase.setSimStopReason( val: TStopReason );
    var
      q: string;
    begin
      q := 'UPDATE `inGeneral` SET `simStopReason` = "' + stopReasonToString( val ) + '"';
      execute( q );
    end
  ;


  function TSMDatabase.getSimStopReason(): TStopReason;
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      q := 'SELECT simStopReason FROM inGeneral';
      db2 := self as TSqlDatabase;
      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();

      if( NULL <> row.field('simStopReason') ) then
        result := stopReasonFromString( row.field('simStopReason') )
      else
        result := ssStopReasonUndefined
      ;
    end
  ;


  procedure TSMDatabase.setSimDays( numDays: integer );
    begin
      execute( 'UPDATE `inGeneral` SET `days` = ' + intToStr( numDays ) );
    end
  ;


  function TSMDatabase.getSimDays(): integer;
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      q := 'SELECT days FROM inGeneral';
      db2 := self as TSqlDatabase;
      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();

      if( NULL <> row.field('days') ) then
        result := row.field('days')
      else
        result := -1
      ;
    end
  ;


  procedure TSMDatabase.incrementCompleteIterations();
    begin
      if( remoteDBParams.useRemoteDatabase ) then
        remoteExecute( 'UPDATE outGeneral SET completedIterations = completedIterations + 1 WHERE jobID = ' + intToStr( remoteDBParams.jobID ) )
      else
        execute( 'UPDATE outGeneral SET completedIterations = completedIterations + 1' )
      ;
    end
  ;


  procedure TSMDatabase.processIterationRecords(
        it: integer;
        outbreakEndDay: integer;
        outbreakEnded: boolean;
        diseaseEndDay: integer;
        diseaseEnded: boolean;
        zoneFociCreated: boolean
      );
    var
      qDict: TQueryDictionary;
      q: string;
      q2: string;

      row: TSqlRow;
    begin
      // Add an iteration record to the database, with the following information:
      qDict := TQueryDictionary.create();

      // if -1 = _outbreakEnd, the outbreak did not end before time ran out.
      qDict['outbreakEnded'] := self.sqlBool( outbreakEnded );
      if( outbreakEnded ) then
        qDict['outbreakEndDay'] := intToStr( outbreakEndDay )
      else
        qDict['outbreakEndDay'] := DATABASE_NULL_VALUE
      ;

      // if -1 = _diseaseEndDay, the active disease phase of the outbreak
      // did not end before time ran out.
      qDict['diseaseEnded'] := self.sqlBool( diseaseEnded );
      if( diseaseEnded ) then
        qDict['diseaseEndDay'] := intToStr( diseaseEndDay )
      else
        qDict['diseaseEndDay'] := DATABASE_NULL_VALUE
      ;

      qDict['zoneFociCreated'] := self.sqlBool( zoneFociCreated );

      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := sqlClasses.writeQuery(
            'outIteration',
            QUpdate,
            qDict,
            'WHERE `iteration` = ' + intToStr( it ) + ' AND `jobID` = ' + intToStr( remoteDBParams.jobID )
          );
          remoteExecute( q )
        end
      else
        begin
          qDict['iteration'] := intToStr( it ); // Remember that iterations are 1-indexed in the database
          q := sqlClasses.writeQuery( 'outIteration', QInsert, qDict );
          execute( q );
        end
      ;

      // Populate outEpidemicCurves for the THE DAY PRIOR TO THE START of the iteration
      // (Actual iteration days won't include units/animals initially infected)
      //-------------------------------------------------------------------------------
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      q := 'SELECT'
        + ' `infcUIni`,'
        + ' `infcAIni`,'
        + ' `tsdUSubc` + `tsdUClin` AS `infectiousUnits`, '
        + ' `productionTypeID`'
        + ' FROM `outDailyByProductionType`'
        + ' WHERE `iteration` = ' + intToStr( it )
        + ' AND `day` = 1'
      ;

      _sqlResult.runQuery( q );

      q2 := '';
      row := _sqlResult.fetchArrayFirst();
      while( nil <> row ) do
        begin
          qDict.clear();

          qDict['iteration'] := intToStr( it );
          qDict['day'] := '0'; // Day = 0 before the simulation starts
          qDict['productionTypeID'] := string( row.field('productionTypeID') );
          qDict['infectedUnits'] := string( row.field('infcUIni') );
          qDict['infectedAnimals'] := string( row.field('infcAIni') );
          qDict['detectedUnits'] := '0'; // Detected units: none
          qDict['detectedAnimals'] := '0'; // Detected animals: none
          qDict['infectiousUnits'] := string( row.field('infectiousUnits') );
          qDict['apparentInfectiousUnits'] := '0'; // Apparently infectious units: none

          if( remoteDBParams.useRemoteDatabase ) then
            qDict['jobID'] := intToStr( remoteDBParams.jobID )
          ;

          q := sqlClasses.writeQuery( 'outEpidemicCurves', QInsert, qDict );

          dbcout( q, DBSMDATABASE );

          if( remoteDBParams.useRemoteDatabase ) then
            q2 := q2 + q + ';' + endl
          else
            execute( q )
          ;

          row := _sqlResult.fetchArrayNext();
        end
      ;

      if( remoteDBParams.useRemoteDatabase ) then
        remoteExecute( q2 )
      ;

      // Populate outEpidemicCurves for ALL OTHER DAYS of the iteration
      //---------------------------------------------------------------
      // For a local database or a remote database with daily output, this is a single query.
      // For a remote database that does not have daily output, a separate query is needed for every day.
      if( not( remoteDBParams.useRemoteDatabase ) ) then
        begin
          q := 'INSERT INTO outEpidemicCurves ('
                + ' `iteration`, `day`, `productionTypeID`,'
                + ' `infectedUnits`,'
                + ' `infectedAnimals`,'
                + ' `detectedUnits`,'
                + ' `detectedAnimals`,'
                + ' `infectiousUnits`,'
                + ' `apparentInfectiousUnits`'
              + ' )'
              + ' SELECT'
                + ' `iteration`, `day`, `productionTypeID`,'
                + ' `infnUAll`,'
                + ' `infnAAll`,'
                //AR:deadDV - following two lines:
                + ' `detnUClin` + `detnUTest` + `detnUDead` + `detnUDeadD` + `detnUDeadV`,'
                + ' `detnAClin` + `detnATest` + `detnADead`+ `detnADeadD` + `detnADeadV`,'
                + ' `tsdUSubc` + `tsdUClin`,'
                + ' `appdUInfectious`'
              + ' FROM `outDailyByProductionType`'
              + ' WHERE '
                + ' `iteration` = ' + intToStr( it )
          ;
          execute( q );
        end
      else if( remoteDBParams.useRemoteDatabase and saveAllDailyOutputs() ) then
        begin
          q := 'INSERT INTO outEpidemicCurves ('
                + ' `iteration`, `day`, `productionTypeID`, `jobID`,'
                + ' `infectedUnits`,'
                + ' `infectedAnimals`,'
                + ' `detectedUnits`,'
                + ' `detectedAnimals`,'
                + ' `infectiousUnits`,'
                + ' `apparentInfectiousUnits`'
              + ' )'
              + ' SELECT'
                + ' `iteration`, `day`, `productionTypeID`, `jobID`,'
                + ' `infnUAll`,'
                + ' `infnAAll`,'
                //AR:deadDV - following two lines:
                + ' `detnUClin` + `detnUTest` + `detnUDead` + `detnUDeadD` + `detnUDeadV`,'
                + ' `detnAClin` + `detnATest` + `detnADead`+ `detnADeadD` + `detnADeadV`,'
                + ' `tsdUSubc` + `tsdUClin`,'
                + ' `appdUInfectious`'
              + ' FROM `outDailyByProductionType`'
              + ' WHERE '
                + ' `iteration` = ' + intToStr( it )
                + ' AND jobID = ' + intToStr( remoteDBParams.jobID )
          ;
          remoteExecute( q );
        end
      else
        begin
          q := ' SELECT'
            + '   `iteration`, `day`, `productionTypeID`,'
            + '   `infnUAll` AS `infectedUnits`,'
            + '   `infnAAll` AS `infectedAnimals`,'
            + '   `tsdUSubc` + `tsdUClin` AS `infectiousUnits`,'
            + '   `appdUInfectious`,'
            //AR:deadDV - following two lines:
            + '   `detnUClin` + `detnUTest` + `detnUDead` + `detnUDeadD` + `detnUDeadV` AS `detectedUnits`,'
            + '   `detnAClin` + `detnATest` + `detnADead` + `detnADeadD` + `detnADeadV`  AS `detectedAnimals`'
            + ' FROM `outDailyByProductionType`'
            + ' WHERE '
            + '   `iteration` = ' + intToStr( it )
          ;

          _sqlResult.runQuery( q );

          q2 := '';
          row := _sqlResult.fetchArrayFirst();
          while( nil <> row ) do
            begin
              qDict.clear();

              qDict['iteration'] := string( row.field( 'iteration' ) );
              qDict['day'] := string( row.field( 'day' ) );
              qDict['productionTypeID'] := string( row.field('productionTypeID') );
              qDict['infectedUnits'] := string( row.field('infectedUnits') );
              qDict['infectedAnimals'] := string( row.field('infectedAnimals') );
              qDict['detectedUnits'] := string( row.field('detectedUnits') );
              qDict['detectedAnimals'] := string( row.field('detectedAnimals') );
              qDict['infectiousUnits'] := string( row.field('infectiousUnits') );
              qDict['apparentInfectiousUnits'] := string( row.field('appdUInfectious') );
              qDict['jobID'] := intToStr( remoteDBParams.jobID );

              q := sqlClasses.writeQuery( 'outEpidemicCurves', QInsert, qDict );
              remoteExecute( q );
              row := _sqlResult.fetchArrayNext();
            end
          ;
        end
      ;

      incrementCompleteIterations();

      qDict.Free();
    end
  ;


  procedure TSMDatabase.simComplete();
    begin
      if
        ( remoteDBParams.useRemoteDatabase )
      and
        ( 0 < length( _remoteQuerySet ) )
      and
        ( nil <> _rm )
      then
        begin
           _remoteQuerySet := _remoteQuerySet + ';' + endl
             + 'INSERT INTO `message` ( `messageID`, `jobID` )'
             + ' VALUES ('
             + ' ' + intToStr( _remoteMessageID ) + ','
             + ' ' + intToStr( remoteDBParams.jobID )
             + ' );'
           ;
           inc( _remoteMessageID );

           _rm.queueRaw( _remoteQuerySet ); // use queuePost() to send a form-encoded message
           Application.ProcessMessages();
        end
      ;
    end
  ;


  procedure TSMDatabase.deleteIncompleteIteration();
    var
      q: string;
      itStr: string;
      trash: integer;
      currentIt: integer;
    begin
      // containsIncompleteIterations is being abused in order to
      // fill currentIt with the ID of the incomplete iteration.
      containsIncompleteIterations( @trash, @currentIt );
      itStr := intToStr( currentIt );

      q := 'DELETE FROM outDailyByProductionType WHERE iteration = ' + itStr;
      execute( q );

      q := 'DELETE FROM outDailyByZone WHERE iteration = ' + itStr;
      execute( q );

      q := 'DELETE FROM outDailyEvents WHERE iteration = ' + itStr;
      execute( q );

      q := 'DELETE FROM outDailyExposures WHERE iteration = ' + itStr;
      execute( q );

      q := 'DELETE FROM outDailyByZoneAndProductionType WHERE iteration = ' + itStr;

      // Add any other daily tables here.

      clearZonePerimeters();
    end
  ;


  procedure TSMDatabase.initializeCustomOutputTables(  list: TObject );
    var
      q: string;
      cList: TCustomOutputList;
      c: TCustomOutputDefinition;
    begin
      cList := list as TCustomOutputList;

      // Get rid of any old tables
      //--------------------------
      dropCustomOutputTables();


      // Create table outCustIteration
      //------------------------------
      q := 'CREATE TABLE `outCustIteration`( `iteration` INTEGER, ';
      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFIteration = c.frequency ) then
            q := q + '`' + c.outputName + '`' + ' ' + variableTypeToString( c.variableType ) + ', '
          ;
          c := cList.next();
        end
      ;
      q := q + 'CONSTRAINT `outCustIteration_PK` PRIMARY KEY (`iteration`) )';

      self.execute( q );

      // Add the columns in the remote database, if necessary
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          c := cList.first();
          while( nil <> c ) do
            begin
              if( OFIteration = c.frequency ) then
                begin
                  q := 'ALTER TABLE `outCustIteration` ADD COLUMN '
                    + '`' + c.outputName + '`'
                    + ' ' + variableTypeToString( c.variableType )
                  ;
                  remoteExecute( q );
                end
              ;
              c := cList.next();
            end
          ;
        end
      ;

      // Create table outCustIterationByProductionType
      //-----------------------------------------------
      q := 'CREATE TABLE `outCustIterationByProductionType`( `iteration` INTEGER, `productionTypeID` INTEGER, ';
      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFProductionTypeIteration = c.frequency ) then
            q := q + '`' + c.outputName + '`' + ' ' + variableTypeToString( c.variableType ) + ', '
          ;
          c := cList.next();
        end
      ;
      q := q + 'CONSTRAINT `outCustIterationByProductionType_PK` PRIMARY KEY (`iteration`, `productionTypeID`) )';

      self.execute( q );

       //  Add foreign key constraint to table outCustIterationByProductionType
       q := 'ALTER TABLE `outCustIterationByProductionType`'
        + ' ADD CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`'
        + ' FOREIGN KEY (`productionTypeID`) '
        + ' REFERENCES `inProductionType` (`productionTypeID`)'
       ;
       self.execute( q );

      // Add the columns in the remote database, if necessary
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          c := cList.first();
          while( nil <> c ) do
            begin
              if( OFProductionTypeIteration = c.frequency ) then
                begin
                  q := 'ALTER TABLE `outCustIterationByProductionType` ADD COLUMN '
                    + '`' + c.outputName + '`'
                    + ' ' + variableTypeToString( c.variableType )
                  ;
                  remoteExecute( q );
                end
              ;
              c := cList.next();
            end
          ;
        end
      ;

      // Create table outCustIterationByZone
      //------------------------------------
      q := 'CREATE TABLE `outCustIterationByZone`( `iteration` INTEGER, `zoneID` INTEGER, ';
      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFZoneIteration = c.frequency ) then
            q := q + '`' + c.outputName + '`' + ' ' + variableTypeToString( c.variableType ) + ', '
          ;
          c := cList.next();
        end
      ;
      q := q + 'CONSTRAINT `outCustIterationByZone_PK` PRIMARY KEY (`iteration`, `zoneID`) )';

      self.execute( q );

       //  Add foreign key constraint to table outCustIterationByProductionType
       q := 'ALTER TABLE `outCustIterationByZone`'
        + ' ADD CONSTRAINT `inZone_outCustIterationByZone_FK1`'
        + ' FOREIGN KEY (`zoneID`) '
        + ' REFERENCES `inZone` (`zoneID`)'
       ;
       self.execute( q );

      // Add the columns in the remote database, if necessary
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          c := cList.first();
          while( nil <> c ) do
            begin
              if( OFZoneIteration = c.frequency ) then
                begin
                  q := 'ALTER TABLE `outCustIterationByZone` ADD COLUMN '
                    + '`' + c.outputName + '`'
                    + ' ' + variableTypeToString( c.variableType )
                  ;
                  remoteExecute( q );
                end
              ;
              c := cList.next();
            end
          ;
        end
      ;

      // Create table outCustIterationByZoneAndProductionType
      //-----------------------------------------------------
      q := 'CREATE TABLE `outCustIterationByZoneAndProductionType`( `iteration` INTEGER, `zoneID` INTEGER, `productionTypeID` INTEGER, ';
      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFZoneProductionTypeIteration = c.frequency ) then
            q := q + '`' + c.outputName + '`' + ' ' + variableTypeToString( c.variableType ) + ', '
          ;
          c := cList.next();
        end
      ;
      q := q + 'CONSTRAINT `outCustIterationByZoneAndProductionType_PK` PRIMARY KEY (`iteration`, `zoneID`, `productionTypeID`) )';

      self.execute( q );

       //  Add zone foreign key constraint to table outCustIterationByZoneAndProductionType
       q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
        + ' ADD CONSTRAINT `inZone_outCustIterationByZoneAndProductionType_FK1`'
        + ' FOREIGN KEY (`zoneID`) '
        + ' REFERENCES `inZone` (`zoneID`)'
       ;
       self.execute( q );

       //  Add PT foreign key constraint to table outCustIterationByZoneAndProductionType
       q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
        + ' ADD CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`'
        + ' FOREIGN KEY (`productionTypeID`) '
        + ' REFERENCES `inProductionType` (`productionTypeID`)'
       ;
       self.execute( q );

      // Add the columns in the remote database, if necessary
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          c := cList.first();
          while( nil <> c ) do
            begin
              if( OFZoneProductionTypeIteration = c.frequency ) then
                begin
                  q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` ADD COLUMN '
                    + '`' + c.outputName + '`'
                    + ' ' + variableTypeToString( c.variableType )
                  ;
                  remoteExecute( q );
                end
              ;
              c := cList.next();
            end
          ;
        end
      ;
    end
  ;


  procedure TSMDatabase.initializeSelectDailyOutputTables( sdo: TObject );
    begin
      dropSelectDailyOutputTables();

      (sdo as TSelectDailyOutputs).initializeDBTables( self );

      connectSelectDailyOutputTables();

      workingDBHasChanged := true;
    end
  ;


  procedure TSMDatabase.disconnectCustomOutputTables();
    var
      q: string;
    begin
      // Use executeWithoutChange() below.
      // Otherwise, the database file will appear to have been modified even when
      // nothing of consequence has actually happened.

      // Table outCustIteration currently has no constraints to worry about.

      // Drop foreign key constraint from table outCustIterationByProductionType
      if( tableExists( 'outCustIterationByProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByProductionType` DROP CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`';
          executeWithoutChange( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZone
      if( tableExists( 'outCustIterationByZone' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByZone` DROP CONSTRAINT `inZone_outCustIterationByZone_FK1`';
          executeWithoutChange( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZoneAndProductionType
      if( tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` DROP CONSTRAINT `inZone_outCustIterationByZoneAndProductionType_FK1`';
          executeWithoutChange( q );
          q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` DROP CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`';
          executeWithoutChange( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.connectCustomOutputTables();
    var
      q: string;
    begin
      // Use executeWithoutChange() below.
      // Otherwise, the database file will appear to have been modified even when
      // nothing of consequence has actually happened.

      // Table outCustIteration currently has no constraints to worry about.

      // Add foreign key constraint to table outCustIterationByProductionType
      if( tableExists( 'outCustIterationByProductionType' ) ) then
        begin
           q := 'ALTER TABLE `outCustIterationByProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           executeWithoutChange( q );
        end
      ;

      // Add foreign key constraint to table outCustIterationByZone
      if( tableExists( 'outCustIterationByZone' ) ) then
        begin
           q := 'ALTER TABLE `outCustIterationByZone`'
            + ' ADD CONSTRAINT `inZone_outCustIterationByZone_FK1`'
            + ' FOREIGN KEY (`zoneID`) '
            + ' REFERENCES `inZone` (`zoneID`)'
           ;
           executeWithoutChange( q );
        end
      ;

      // Add foreign key constraints to table outCustIterationByZoneAndProductionType
      if( tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
           q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
            + ' ADD CONSTRAINT `inZone_outCustIterationByZoneAndProductionType_FK1`'
            + ' FOREIGN KEY (`zoneID`) '
            + ' REFERENCES `inZone` (`zoneID`)'
           ;
           executeWithoutChange( q );

           q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           executeWithoutChange( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.disconnectSelectDailyOutputTables();
    var
      q: string;
    begin
      // Use executeWithoutChange() below.
      // Otherwise, the database file will appear to have been modified even when
      // nothing of consequence has actually happened.

      // Drop foreign key constraint from table outSelectDailyByProductionType
      if( tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outSelectDailyByProductionType` DROP CONSTRAINT `inProductionType_outSelectDailyByProductionType_FK1`';
          executeWithoutChange( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZoneAndProductionType
      if( tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType` DROP CONSTRAINT `inZone_outSelectDailyByZoneAndProductionType_FK1`';
          executeWithoutChange( q );
          q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType` DROP CONSTRAINT `inProductionType_outSelectDailyByZoneAndProductionType_FK1`';
          executeWithoutChange( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.connectSelectDailyOutputTables();
    var
      q: string;
    begin
      // Use executeWithoutChange() below.
      // Otherwise, the database file will appear to have been modified even when
      // nothing of consequence has actually happened.

      // Add foreign key constraint to table outSelectDailyByProductionType
      if( tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
           q := 'ALTER TABLE `outSelectDailyByProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outSelectDailyByProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           executeWithoutChange( q );
        end
      ;

      // Add foreign key constraint to table outSelectDailyByZoneAndProductionType
      if( tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
           q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType`'
            + ' ADD CONSTRAINT `inZone_outSelectDailyByZoneAndProductionType_FK1`'
            + ' FOREIGN KEY (`zoneID`) '
            + ' REFERENCES `inZone` (`zoneID`)'
           ;
           executeWithoutChange( q );

           q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outSelectDailyByZoneAndProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           executeWithoutChange( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.dropCustomOutputTables();
    begin
      // Remove constraints involving custom output tables before dropping them.
      // (This step may not be necessary, but it seems like a good idea.)
      disconnectCustomOutputTables();

      if( self.tableExists( 'outCustIteration' ) ) then
        self.execute( 'DROP TABLE outCustIteration' )
      ;

      if( self.tableExists( 'outCustIterationByProductionType' ) ) then
        self.execute( 'DROP TABLE outCustIterationByProductionType' )
      ;

      if( self.tableExists( 'outCustIterationByZone' ) ) then
        self.execute( 'DROP TABLE outCustIterationByZone' )
      ;

      if( self.tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        self.execute( 'DROP TABLE outCustIterationByZoneAndProductionType' )
      ;
    end
  ;


  procedure TSMDatabase.dropSelectDailyOutputTables();
    begin
      disconnectSelectDailyOutputTables();

      if( self.tableExists( 'outSelectDailyByProductionType' ) ) then
        self.execute( 'DROP TABLE outSelectDailyByProductionType' )
      ;

      if( self.tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        self.execute( 'DROP TABLE outSelectDailyByZoneAndProductionType' )
      ;
    end
  ;


  procedure TSMDatabase.processCustomIterationRecords( iteration: integer; list: TObject );
    var
      insertQuery: string;
      dict: TQueryDictionary;
      cList: TCustomOutputList;
      c: TCustomOutputDefinition;

      outputQuery: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      cList := list as TCustomOutputList;
      db2 := self as TSqlDatabase;
      res := TSqlResult.create( db2 );
      dict:= TQueryDictionary.create();

      // There will be one new record per iteration,
      // so a single INSERT query will be sufficient.
      dict['iteration'] := intToStr( iteration );
      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFIteration = c.frequency ) then
            begin
              outputQuery := stringReplace( c.sql, '$lastIteration', intToStr( iteration ), [rfReplaceAll] );

              res.runQuery( outputQuery );

              // For iteration outputs, this result should include only one row with only one field.
              // That field will be the value that goes into the output table.
              if( ( 1 <> res.numRows ) or ( 1 <> res.numFields ) ) then
                begin
                  dbcout( 'Bad record in outCustIteration: row or field count does not match for query', true );
                  dbcout( c.sql, true );

                  // Record "-999" or "**ERROR" in the database, to indicate a query problem.
                  case c.variableType of
                    VTInteger: dict[c.outputName] := intToStr( -999 );
                    VTDouble: dict[c.outputName] := usFloatToStr( -999.0 );
                    VTString: dict[c.outputName] := sqlQuote( '**ERROR' );
                  end;
                end
              else
                begin
                  row := res.fetchArrayFirst();

                  if( null = row.field(0) ) then
                    dict[c.outputName] := DATABASE_NULL_VALUE
                  else
                    begin
                      case c.variableType of
                        VTInteger: dict[c.outputName] := intToStr( row.field(0) );
                        VTDouble: dict[c.outputName] := usFloatToStr( row.field(0) );
                        VTString: dict[c.outputName] := sqlQuote( row.field(0) );
                      end;
                    end
                  ;
                end
              ;

            end
          ;

          c := cList.next();
        end
      ;

      if( remoteDBParams.useRemoteDatabase ) then
        dict['jobID'] := intToStr( remoteDBParams.jobID )
      ;

      insertQuery := writeQuery( 'outCustIteration', QInsert, dict );

      if( remoteDBParams.useRemoteDatabase ) then
        remoteExecute( insertQuery )
      else
        execute( insertQuery )
      ;

      res.free();
      dict.Free();
    end
  ;


  procedure TSMDatabase.processCustomProductionTypeRecords( iteration: integer; list: TObject; ptList: TObject );
    var
      realPTList: TProductionTypeList;

      insertQuery: string;
      superDict: TQStringObjectMap;
      dict: TQueryDictionary;

      cList: TCustomOutputList;
      c: TCustomOutputDefinition;

      outputQuery: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;

      i: integer;
    begin
      // Separate INSERT statements are needed for each production type.
      // To accomplish this, use a separate array element in superDict for each
      // production type.
      // There will be as many elements in this "super dictionary" as there are production types.
      // Each element will be a string dictionary, which will have as many key/value
      // pairs as there are custom production type outputs.
      realPTList := ptList as TProductionTypeList;

      cList := list as TCustomOutputList;
      db2 := self as TSqlDatabase;
      res := TSqlResult.create( db2 );

      // Set up the array
      superDict := TQStringObjectMap.Create();

      for i := 0 to (realPTList.Count - 1) do
        begin
          dict := TQueryDictionary.create();
          dict['iteration'] := intToStr( iteration );
          superDict[intToStr( realPTList[i].productionTypeID )] := dict;
        end
      ;

      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFProductionTypeIteration = c.frequency ) then
            begin
              outputQuery := stringReplace( c.sql, '$lastIteration', intToStr( iteration ), [rfReplaceAll] );

              res.runQuery( outputQuery );

              // For production type outputs, this result should include ptCount rows with 2 fields.
              // The first field will be the production type ID,
              // and the second field will contain the value that goes into the output table.

              if( ( realPTList.Count <> res.numRows ) or ( 2 <> res.numFields ) ) then
                begin
                  dbcout( 'Bad record in outCustIterationByProductionType: row or field count does not match for query', true );
                  dbcout( c.sql, true );

                  for i := 0 to (realPTList.Count - 1) do
                    begin
                      // It should be safe to use valueAtIndex here, because the original order doesn't matter.
                      dict := superDict.itemAtIndex(i) as TQueryDictionary;
                      // Record "-999" or "**ERROR" in the database, to indicate a query problem.
                      case c.variableType of
                        VTInteger: dict[c.outputName] := intToStr( -999 );
                        VTDouble: dict[c.outputName] := usFloatToStr( -999.0 );
                        VTString: dict[c.outputName] := sqlQuote( '**ERROR' );
                      end;
                    end
                  ;
                end
              else
                begin
                  row := res.fetchArrayFirst();

                  while( nil <> row ) do
                    begin
                      dict := superDict[intToStr( row.field(0) )] as TQueryDictionary;

                      if( nil = dict ) then
                        raise exception.Create( 'Missing dict in processCustomProductionTypeRecords' )
                      ;

                      if( not( dict.HasKey('productionTypeID') ) ) then
                        dict['productionTypeID'] := intToStr( row.field(0) )
                      ;

                      if( null = row.field(1) ) then
                        dict[c.outputName] := DATABASE_NULL_VALUE
                      else
                        begin
                          case c.variableType of
                            VTInteger: dict[c.outputName] := intToStr( row.field(1) );
                            VTDouble: dict[c.outputName] := usFloatToStr( row.field(1) );
                            VTString: dict[c.outputName] := sqlQuote( row.field(1) );
                          end;
                        end
                      ;

                      row := res.fetchArrayNext();
                    end
                  ;
                end
              ;
            end
          ;
          c := cList.next();
        end
      ;

      for i := 0 to (realPTList.Count - 1) do
        begin
          dict := superDict.itemAtIndex(i) as TQueryDictionary;

          if( remoteDBParams.useRemoteDatabase ) then
            dict['jobID'] := intToStr( remoteDBParams.jobID )
          ;

          insertQuery := writeQuery( 'outCustIterationByProductionType', QInsert, dict );

          if( remoteDBParams.useRemoteDatabase ) then
            remoteExecute( insertQuery )
          else
            execute( insertQuery )
          ;
        end
      ;

      res.free();
      superDict.deleteValues();
      superDict.Free();
    end
  ;


  procedure TSMDatabase.processCustomZoneRecords( iteration: integer; list: TObject; zoneList: TObject );
    var
      realZoneList: TZoneList;

      insertQuery: string;
      superDict: TQStringObjectMap;
      dict: TQueryDictionary;

      cList: TCustomOutputList;
      c: TCustomOutputDefinition;

      outputQuery: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;

      i: integer;
    begin
      // Separate INSERT statements are needed for each zone.
      // To accomplish this, use a separate array element in superDict for each zone.
      // There will be as many elements in this "super dictionary" as there are zones.
      // Each element will be a string dictionary, which will have as many key/value
      // pairs as there are custom zone outputs.
      realZoneList := zoneList as TZoneList;

      cList := list as TCustomOutputList;
      db2 := self as TSqlDatabase;
      res := TSqlResult.create( db2 );

      // Set up the array
      superDict := TQStringObjectMap.Create();

      for i := 0 to (realZoneList.Count - 1) do
        begin
          dict := TQueryDictionary.create();
          dict['iteration'] := intToStr( iteration );
          superDict[intToStr( realZoneList[i].zoneID )] := dict;
        end
      ;

      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFZoneIteration = c.frequency ) then
            begin
              outputQuery := stringReplace( c.sql, '$lastIteration', intToStr( iteration ), [rfReplaceAll] );

              res.runQuery( outputQuery );

              // For zone outputs, this result should include zoneCount rows with 2 fields.
              // The first field will be the zone ID,
              // and the second field will contain the value that goes into the output table.

              if( ( realZoneList.Count <> res.numRows ) or ( 2 <> res.numFields ) ) then
                begin
                  dbcout( 'Bad record in outCustIterationByZone: row or field count does not match for query', true );
                  dbcout( c.sql, true );

                  for i := 0 to (realZoneList.Count - 1) do
                    begin
                      // It should be safe to use valueAtIndex here, because the original order doesn't matter.
                      dict := superDict.itemAtIndex(i) as TQueryDictionary;
                      // Record "-999" or "**ERROR" in the database, to indicate a query problem.
                      case c.variableType of
                        VTInteger: dict[c.outputName] := intToStr( -999 );
                        VTDouble: dict[c.outputName] := usFloatToStr( -999.0 );
                        VTString: dict[c.outputName] := sqlQuote( '**ERROR' );
                      end;
                    end
                  ;
                end
              else
                begin
                  (*
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      //row.debug();
                      row := res.fetchArrayNext();
                    end
                  ;
                  *)

                  row := res.fetchArrayFirst();

                  while( nil <> row ) do
                    begin
                      dict := superDict[intToStr( row.field(0) )] as TQueryDictionary;

                      if( nil = dict ) then
                        raise exception.Create( 'Missing dict in processCustomZoneRecords' )
                      ;

                      if( not( dict.HasKey('zoneID') ) ) then
                        dict['zoneID'] := intToStr( row.field(0) )
                      ;

                      if( null = row.field(1) ) then
                        dict[c.outputName] := DATABASE_NULL_VALUE
                      else
                        begin
                          case c.variableType of
                            VTInteger: dict[c.outputName] := intToStr( row.field(1) );
                            VTDouble: dict[c.outputName] := usFloatToStr( row.field(1) );
                            VTString: dict[c.outputName] := sqlQuote( row.field(1) );
                          end;
                        end
                      ;

                      row := res.fetchArrayNext();
                    end
                  ;
                end
              ;
            end
          ;
          c := cList.next();
        end
      ;

      for i := 0 to (realZoneList.Count - 1) do
        begin
          dict := superDict.itemAtIndex(i) as TQueryDictionary;

          if( remoteDBParams.useRemoteDatabase ) then
            dict['jobID'] := intToStr( remoteDBParams.jobID )
          ;

          insertQuery := writeQuery( 'outCustIterationByZone', QInsert, dict );

          if( remoteDBParams.useRemoteDatabase ) then
            remoteExecute( insertQuery )
          else
            execute( insertQuery )
          ;
        end
      ;

      res.free();
      superDict.deleteValues();
      superDict.Free();
    end
  ;


  procedure TSMDatabase.processCustomZonePTRecords( iteration: integer; list: TObject; zoneList: TObject; ptList: TObject );
    var
      realZoneList: TZoneList;
      realPTList: TProductionTypeList;
      
      insertQuery: string;
      superSuperDict: TQStringObjectMap;
      superDict: TQStringObjectMap;
      dict: TQueryDictionary;

      cList: TCustomOutputList;
      c: TCustomOutputDefinition;

      outputQuery: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;

      i, j: integer;
    begin
      // Separate INSERT statements are needed for each combination of zone and production type.
      realPTList := ptList as TProductionTypeList;
      realZoneList := zoneList as TZoneList;

      cList := list as TCustomOutputList;
      db2 := self as TSqlDatabase;
      res := TSqlResult.create( db2 );

      // Set up the arrays
      superSuperDict := TQStringObjectMap.Create();

      for i := 0 to (realZoneList.Count - 1) do
        begin
          superDict := TQStringObjectMap.Create();

          for j := 0 to (realPTList.count - 1) do
            begin
              dict := TQueryDictionary.create();
              dict['iteration'] := intToStr( iteration );
              superDict[intToStr( realPTList[j].productionTypeID )] := dict;
            end
          ;
          superSuperDict[intToStr( realZoneList[i].zoneID )] := superDict; 
        end
      ;

      c := cList.first();
      while( nil <> c ) do
        begin
          if( OFZoneProductionTypeIteration = c.frequency ) then
            begin
              outputQuery := stringReplace( c.sql, '$lastIteration', intToStr( iteration ), [rfReplaceAll] );

              res.runQuery( outputQuery );

              // For zone/PT outputs, this result should include (zoneCount*ptCount) rows with 3 fields.
              // The first field will be the zone ID, the second field the production type ID,
              // and the third field will contain the value that goes into the output table.
              
              if( ( (realZoneList.count * realPTList.count) <> res.numRows ) or ( 3 <> res.numFields ) ) then
                begin
                  dbcout( 'Bad record in outCustIterationByZoneAndProductionType: row or field count does not match for query', true );
                  dbcout( c.sql, true );

                  for i := 0 to (realZoneList.count - 1) do
                    begin
                      superDict := superSuperDict.itemAtIndex(i) as TQStringObjectMap;
                      
                      for j := 0 to (realPTList.Count - 1) do
                        begin
                          // It should be safe to use valueAtIndex here, because the original order doesn't matter.
                          dict := superDict.itemAtIndex(j) as TQueryDictionary;

                          if( nil = dict ) then
                            raise exception.Create( 'Missing dict in processCustomZonePTRecords (1)' )
                          ;

                          // Record "-999" or "**ERROR" in the database, to indicate a query problem.
                          case c.variableType of
                            VTInteger: dict[c.outputName] := intToStr( -999 );
                            VTDouble: dict[c.outputName] := usFloatToStr( -999.0 );
                            VTString: dict[c.outputName] := sqlQuote( '**ERROR' );
                          end;
                        end
                      ;
                    end
                  ;
                end
              else // The results of the custom SQL query look OK.  Write an insert query with the real values.
                begin
                  row := res.fetchArrayFirst();

                  while( nil <> row ) do
                    begin
                      superDict := superSuperDict[intToStr( row.field(0) )] as TQStringObjectMap;
                      dict := superDict[intToStr( row.field(1) )] as TQueryDictionary;

                      if( nil = dict ) then
                        raise exception.Create( 'Missing dict in processCustomZonePTRecords (2)' )
                      ;

                      if( not( dict.HasKey('zoneID') ) ) then
                        dict['zoneID'] := intToStr( row.field(0) )
                      ;
                      if( not( dict.HasKey('productionTypeID') ) ) then
                        dict['productionTypeID'] := intToStr( row.field(1) )
                      ;

                      if( null = row.field(2) ) then
                        dict[c.outputName] := DATABASE_NULL_VALUE
                      else
                        begin
                          case c.variableType of
                            VTInteger: dict[c.outputName] := intToStr( row.field(2) );
                            VTDouble: dict[c.outputName] := usFloatToStr( row.field(2) );
                            VTString: dict[c.outputName] := sqlQuote( row.field(2) );
                          end;
                        end
                      ;

                      row := res.fetchArrayNext();
                    end
                  ;
                end
              ;
            end
          ;
          c := cList.next();
        end
      ;

      // Run the insert queries
      //-----------------------
      for i := 0 to (realZoneList.Count - 1) do
        begin
          superDict := superSuperDict.itemAtIndex(i) as TQStringObjectMap;
          
          for j := 0 to (realPTList.count - 1) do
            begin
              dict := superDict.itemAtIndex(j) as TQueryDictionary;

              if( remoteDBParams.useRemoteDatabase ) then
                dict['jobID'] := intToStr( remoteDBParams.jobID )
              ;

              insertQuery := writeQuery( 'outCustIterationByZoneAndProductionType', QInsert, dict );

              if( remoteDBParams.useRemoteDatabase ) then
                remoteExecute( insertQuery )
              else
                execute( insertQuery )
              ;
            end
          ;
        end
      ;


      // Clean up
      //---------
      res.free();
      
      for i := 0 to (realZoneList.count - 1) do
        begin
          superDict := superSuperDict.itemAtIndex(i) as TQStringObjectMap;          
          superDict.deleteValues();
        end
      ;
      
      superSuperDict.deleteValues();
      superSuperDict.free();
    end
  ;


  procedure TSMDatabase.initializeRemoteDatabase();
    var
      q: string;
    begin
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'INSERT INTO `scenario` ( `scenarioID`, `descr` ) VALUES ( '
            + intToStr( remoteDBParams.scenarioID )
            + ', ' + DATABASE_NULL_VALUE + ' )'
          ;
          cout( q );
          remoteExecute( q );

          q := 'INSERT INTO `job` ( `jobID`, `scenarioID` ) VALUES ( '
            + intToStr( remoteDBParams.jobID )
            + ', ' + intToStr( remoteDBParams.scenarioID )
            + ' )'
          ;
          cout( q );
          remoteExecute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.initializeAllOutputRecords();
    var
      q: string;
    begin
      dbcout( 'initializeAllOutputRecords', DBSMDATABASE );

      inherited initializeAllOutputRecords();

      q := 'UPDATE `dynHerd` SET '
        + ' `finalStateCode` = ' + DATABASE_NULL_VALUE + ','
        + ' `finalControlStateCode` = ' + DATABASE_NULL_VALUE + ','
        + ' `finalDetectionStateCode` = ' + DATABASE_NULL_VALUE + ','
        + ' `cumulInfected` = 0,'
        + ' `cumulDetected` = 0,'
        + ' `cumulDestroyed` = 0,'
        + ' `cumulVaccinated` = 0'
      ;
      execute( q );

      clearExistingFinalHerdStates();

      // Clear daily outputs
      //---------------------
      q := 'DELETE FROM outDailyByProductionType';
      execute( q );

      q := 'DELETE FROM outDailyEvents';
      execute( q );

      q := 'DELETE FROM outDailyExposures';
      execute( q );

      q := 'DELETE FROM outDailyByZoneAndProductionType';
      execute( q );

      q := 'DELETE FROM outDailyByZone';
      execute( q );

      // Clear iteration output records
      //-------------------------------
      q := 'DELETE FROM outIteration';
      execute( q );

      q := 'DELETE FROM outIterationByProductionType';
      execute( q );

      q := 'DELETE FROM outIterationCosts';
      execute( q );

      q := 'DELETE FROM outIterationByZoneAndProductionType';
      execute( q );

      q := 'DELETE FROM outIterationByZone';
      execute( q );

      // Clear epidemic curves
      //----------------------
      q := 'DELETE FROM outEpidemicCurves';
      execute( q );

      // Clear custom outputs
      //---------------------
      if( self.tableExists( 'outCustIteration' ) ) then
        begin
          q := 'DELETE FROM outCustIteration';
          execute( q );
        end
      ;

      if( self.tableExists( 'outCustIterationByProductionType' ) ) then
        begin
          q := 'DELETE FROM outCustIterationByProductionType';
          execute( q );
        end
      ;

      if( self.tableExists( 'outCustIterationByZone' ) ) then
        begin
          q := 'DELETE FROM outCustIterationByZone';
          execute( q );
        end
      ;

      if( self.tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
          q := 'DELETE FROM outCustIterationByZoneAndProductionType';
          execute( q );
        end
      ;

      // Clear selected daily outputs
      //-----------------------------
      if( self.tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
          q := 'DELETE FROM outSelectDailyByProductionType';
          execute( q );
        end
      ;

      if( self.tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
          q := 'DELETE FROM outSelectDailyByZoneAndProductionType';
          execute( q );
        end
      ;

      dbcout( 'done initializeAllOutputRecords', DBSMDATABASE );
    end
  ;


  procedure TSMDatabase.prepareForIteration( currentIt: integer );
    var
      q: string;
      leaveIterations: integer;
      iterationsToDelete: string;
    begin
      if( not( saveAllDailyOutputs() ) ) then
        begin
          // Leave daily records for the specified number of iterations
          if( remoteDBParams.useRemoteDatabase ) then
            leaveIterations := 3
          else
            leaveiterations := saveDailyOutputsForIterations() - 1
          ;

          iterationsToDelete := intToStr( currentIt - leaveIterations );

          q := 'DELETE FROM outDailyByProductionType WHERE iteration < ' + iterationsToDelete;
          execute( q );

          q := 'DELETE FROM outDailyEvents WHERE iteration < ' + iterationsToDelete;
          execute( q );

          q := 'DELETE FROM outDailyExposures WHERE iteration < ' + iterationsToDelete;
          execute( q );

          q := 'DELETE FROM outDailyByZone WHERE iteration < ' + iterationsToDelete;
          execute( q );

          q := 'DELETE FROM outDailyByZoneAndProductionType WHERE iteration < ' + iterationsToDelete;
          execute( q );
        end
      ;

      // Set up the remote database, if necessary
      //-----------------------------------------
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'INSERT INTO `outIteration`'
            + ' ( `jobID`, `iteration`, `diseaseEnded`, `diseaseEndDay`, `outbreakEnded`, `outbreakEndDay`, `zoneFociCreated` )'
            + ' VALUES'
            + ' ( ' + intToStr( remoteDBParams.jobID ) + ', ' + intToStr( currentIt ) + ', ' + DATABASE_NULL_VALUE + ', ' + DATABASE_NULL_VALUE + ', ' + DATABASE_NULL_VALUE + ', ' + DATABASE_NULL_VALUE + ', ' + DATABASE_NULL_VALUE + ' )'
          ;
          remoteExecute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.clearExistingFinalHerdStates( const setDatabaseChanged: boolean = true );
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();

      if( self.fieldExists( 'finalStateCode', 'dynHerd' ) ) then
        dict.insert( 'finalStateCode', 'initialStateCode' )
      ;

      if( self.fieldExists( 'finalControlStateCode', 'dynHerd' ) ) then
        dict.insert( 'finalControlStateCode', '"U"' )
      ;

      if( self.fieldExists( 'finalDetectionStateCode', 'dynHerd' ) ) then
        dict.insert( 'finalDetectionStateCode', DATABASE_NULL_VALUE )
      ;

      if( 0 < dict.count ) then
        begin
          q := writeQuery( 'dynHerd', QUpdate, dict );
          
          if( setDatabaseChanged ) then
            execute( q )
          else
            executeWithoutChange( q )
          ;
        end
      ;

      dict.Free();

      clearZonePerimeters( setDatabaseChanged );
    end
  ;

  
   function TSMDatabase.changeProductionTypeDescr( const ptid: integer; const newName: string ): boolean;
    var
      q: string;
    begin
      q := 'UPDATE `inProductionType` SET '
        + '`descr` = ' + sqlQuote( newName )
        + ' WHERE '
        + '`productionTypeID` = ' + intToStr( ptid )
      ;
      result := execute( q );
    end
   ;


  function TSMDatabase.removeProductionType( const id: integer ): boolean;
    var
      q: string;
      ptid: string;
      success1, success2, success3, success4, success5: boolean;
    begin
      initializeAllOutputRecords();

      ptid := intToStr( id );

      q := 'DELETE FROM `dynHerd` WHERE `productionTypeID` = ' + ptid;
      success1 := execute( q );

      q := 'DELETE FROM `inProductionTypePair` WHERE `sourceProductionTypeID` = ' + ptid;
      success3 := execute( q );

      q := 'DELETE FROM `inProductionTypePair` WHERE `destProductionTypeID` = ' + ptid;
      success4 := execute( q );

      q := 'DELETE FROM `inZoneProductionTypePair` WHERE `productionTypeID` = ' + ptid;
      success5 := execute( q );

      q := 'DELETE FROM `inProductionType` WHERE `productionTypeID` = ' + ptid;
      success2 := execute( q );

      result := success1 and success2 and success3 and success4 and success5;
    end
  ;


  procedure TSMDatabase.removeProductionTypePairs();
    var
      q: string;
    begin
      q := 'Delete from inProductionTypePair';
      execute( q );
    end
  ;



  procedure TSMDatabase.removeZone( const id: integer );
    var
      q: string;
      zoneID: string;
    begin
      initializeAllOutputRecords();

      zoneID := intToStr( id );

      q := 'DELETE FROM `inZoneProductionTypePair` WHERE `zoneID` = ' + zoneID;
      execute( q );

      q := 'DELETE FROM `inZone` WHERE `zoneID` = ' + zoneID;
      execute( q );
    end
  ;


  procedure TSMDatabase.makeProductionTypePair( src, dest: integer );
    var
      q: string;
    begin
     if( nil = _sqlResult ) then
      createSqlResult()
     ;

      q := 'SELECT `sourceProductionTypeID` FROM `inProductionTypePair` '
        + 'WHERE `sourceProductionTypeID` = ' + intToStr( src ) + ' '
        + 'AND `destProductionTypeID` = ' + intToStr( dest )
      ;

      _sqlResult.runQuery( q );

      if( _sqlResult.numRows = 0 ) then
        self.execute(
          'INSERT INTO `inProductionTypePair` ( `sourceProductionTypeID`, `destProductionTypeID` ) '
            + 'VALUES( ' + intToStr( src ) + ', ' + intToStr( dest ) + ' )'
        )
      ;
    end
  ;

  procedure TSMDatabase.clearHerds();
    begin
      initializeAllOutputRecords();
      execute( 'DELETE FROM `dynHerd`' );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Functions for handling herd imports
// ----------------------------------------------------------------------------
  function TSMDatabase.lastHerdID(): integer;
    var
      row: TSqlRow;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT MAX(herdID) as maxID FROM `dynHerd`' );

      if( not( _sqlResult.success ) ) then
        result := 0
      else
        begin
          row := _sqlResult.fetchArrayFirst();

          if( null <> row.field('maxID') ) then
            result := myStrToInt( row.field('maxID') )
          else
            result := 0
          ;
        end
      ;

    end
  ;

  procedure TSMDatabase.makeTemporaryHerdTable();
    var
      q: string;
    begin
      q := 'CREATE TABLE `dynHerd2` ('
        + ' `herdID` INTEGER,' // In the actual table, this is a counter field.
        + ' `productionTypeID` INTEGER,'
        + ' `latitude` DOUBLE,'
        + ' `longitude` DOUBLE,'
        + ' `initialStateCode` CHAR(1),'
        + ' `daysInInitialState` INTEGER,'
        + ' `daysLeftInInitialState` INTEGER,'
        + ' `initialSize` INTEGER'
        + ' )'
      ;

      self.execute( q );
    end
  ;


  function TSMDatabase.mergeHerdTables(): boolean;
    var
      q: string;
    begin
      q := 'INSERT INTO  `dynHerd` (`herdID`, `productionTypeID`, `latitude`, `longitude`, `initialStateCode`, `daysInInitialState`, `daysLeftInInitialState`, `initialSize`)'
        + ' SELECT `dynHerd2`.`herdID`,'
        + ' `dynHerd2`.`productionTypeID`,'
        + ' `dynHerd2`.`latitude`,'
        + ' `dynHerd2`.`longitude`,'
        + ' `dynHerd2`.`initialStateCode`,'
        + ' `dynHerd2`.`daysInInitialState`,'
        + ' `dynHerd2`.`daysLeftInInitialState`,'
        + ' `dynHerd2`.`initialSize`'
        + ' FROM `dynHerd2`'
      ;

      result := self.execute( q );

      self.execute( 'DROP TABLE dynHerd2' );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Functions for handling BLOBs
// ----------------------------------------------------------------------------
  procedure TSMDatabase.clearZonePerimeters( const setDatabaseChanged: boolean = true );
    var
      q: string;
    begin
      q := 'UPDATE `dynBlob` SET `zonePerimeters` = ' + DATABASE_NULL_VALUE;
      
      if( setDatabaseChanged ) then
        self.execute( q )
      else
        self.executeWithoutChange( q )
      ;
    end
  ;


  function TSMDatabase.containsZonePerimeters(): boolean;
    var
      row: TSqlRow;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT `zonePerimeters` FROM `dynBlob`' );
      row := _sqlResult.fetchArrayFirst();

      if( nil = row ) then
        result := false
      else if( null = row.field('zonePerimeters') ) then
        result := false
      // For reasons that I don't understand, "unassigned" no longer works since implementing
      // the faster code for handling SELECT queries in SqlClasses (AR 1/26/11)
      //else if( unassigned = row.field('zonePerimeters') ) then
        //result := false
      else
        result := true
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Creating a populated sample database
// ----------------------------------------------------------------------------
  class function TSMDatabase.makeSampleDatabase(
        const fileName: string;
        errCode: pinteger = nil;
        errMsg: pstring = nil
      ): boolean;
    var
      tempFileName: string;
    begin
      // Sequence of steps:
      // ------------------
      // - Get a temporary file name from the OS for the temporary ZIP archive
      // - Save the resource file to a temporary ZIP archive
      // - Unzip the ZIP archive file to a temporary scenario file
      // - Delete the temporary ZIP archive

      tempFileName := WindowsUtils.tempFileName( '', false );

      if( saveResourceAsFile( 'SampleDatabase', 'MDBZIP', tempFileName ) ) then
        begin
          if( extractFileFromZip( tempFileName, 'SampleScenario.mdb', fileName, errCode, errMsg ) ) then
            begin
              if( I88nEnglish <> i88nLanguage() ) then
                result := setSampleDatabaseLanguage( fileName )
              else
                result := true
              ;

              if( not result ) then
                deleteFile( fileName )
              ;
            end
          else
            result := false
          ;

          deleteFile( tempFileName );
        end
      else
        result := false
      ;

    end
  ;


  class function TSMDatabase.setSampleDatabaseLanguage( const fileName: string ): boolean;
    var
      db: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;

      query: string;
      updateStatement: string;
      name: string;
      id: integer;
    begin
      db := TSqlDatabase.create( DBMSAccess, fileName, DBOpen );
      res := nil;
      result := true; // until proven otherwise

      if( not( db.isOpen ) ) then
        result := false
      else
        begin
          res := TSqlResult.create( db );

          // Set the language code
          //----------------------
          db.execute( 'UPDATE `inGeneral` SET `language` = ' + db.sqlQuote( i88nLanguageCodeString() ) );

          // Rename production types
          //------------------------
          query := 'SELECT `productionTypeID`, `descr` FROM `inProductionType`';
          res.runQuery( query );
          if( res.success ) then
            begin
              row := res.fetchArrayFirst();
              while( nil <> row ) do
                begin
                  name := row.field(1);
                  id := row.field(0);
                  updateStatement :=
                    'UPDATE `inProductionType` SET `descr` = ' + db.sqlQuote( tr( name ) )
                      + ' WHERE `productionTypeID` = ' + intToStr( id )
                  ;
                  result := db.execute( updateStatement );

                  if( not( result ) ) then
                    break
                  else
                    row := res.fetchArrayNext()
                  ;
                end
              ;
            end
          else
            result := false
          ;

          // Rename zones
          //-------------
          if( result ) then
            begin
              query := 'SELECT `zoneID`, `descr` FROM `inZone`';
              res.runQuery( query );
              if( res.success ) then
                begin
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      name := row.field(1);
                      id := row.field(0);
                      updateStatement :=
                        'UPDATE `inZone` SET `descr` = ' + db.sqlQuote( tr( name ) )
                          + ' WHERE `zoneID` = ' + intToStr( id )
                      ;
                      result := db.execute( updateStatement );

                      if( not( result ) ) then
                        break
                      else
                        row := res.fetchArrayNext()
                      ;
                    end
                  ;
                end
              else
                result := false
              ;
            end
          ;

          // Rename 'charts'
          //----------------
          if( result ) then
            begin
              query := 'SELECT `chartID`, `chartName` FROM `inChart`';
              res.runQuery( query );
              if( res.success ) then
                begin
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      name := row.field(1);
                      id := row.field(0);
                      updateStatement :=
                        'UPDATE `inChart` SET `chartName` = ' + db.sqlQuote( tr( name ) )
                          + ' WHERE `chartID` = ' + intToStr( id )
                      ;
                      result := db.execute( updateStatement );

                      if( not( result ) ) then
                        break
                      else
                        row := res.fetchArrayNext()
                      ;
                    end
                  ;
                end
              else
                result := false
              ;
            end
          ;

          // Change the scenario notes
          //--------------------------
          if( result ) then
            begin
              updateStatement := 'UPDATE `inGeneral` SET `scenarioDescr` = '
                + db.sqlQuote( tr(
                  'This file contains a sample scenario for an outbreak of a highly contagious disease.'
                  + '  This file may serve as an example that can be modified for other uses, but parameters'
                  + ' in this file should not be considered definitive or accurate for any particular disease or situation.'
                  ) )
              ;

              result := db.execute( updateStatement );
            end
          ;
        end
      ;

      freeAndNil( res );

      if( db.isOpen ) then db.close();
      freeAndNil( db );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Properties
// ----------------------------------------------------------------------------
  procedure TSMDatabase.setWorkingDBHasChanged( val: boolean );
    {$IFNDEF CONSOLEAPP}
    var
      frm: TFormMain;
    {$ENDIF}
    begin
      inherited setWorkingDBHasChanged( val );

      {$IFNDEF CONSOLEAPP}
      if( _frmMain <> nil ) then
        begin
          frm := _frmMain as TFormMain;
          frm.updateCaption();
        end
      ;
      {$ENDIF}
    end
  ;


  function TSMDatabase.getSimulationComplete(): boolean;
    var
      row: TSqlRow;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT simulationEndTime FROM outGeneral' );
      row := _sqlResult.fetchArrayFirst();

      result := not ( null = row.field('simulationEndTime') );
    end
  ;

  
  function TSMDatabase.getContainsValidOutput(): boolean;
    var
      nExpectedIterations: integer;
      nPTs, nZones: integer;
      nExpectedPTRecords, nExpectedZoneRecords, nExpectedZonePTRecords: integer;
      useCosts, useZones: boolean;
      row: TSqlRow;
    begin
      result := true; // until shown otherwise

      // Confirm that the number of outputs recorded in each output table is the expected number.
      // Instances have arisen where the numbers don't match, which wreaks havoc with the rest of the application.

      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT completedIterations FROM outGeneral' );
      row := _sqlResult.fetchArrayFirst();
      nExpectedIterations := row.field( 'completedIterations' );

      if( 0 = nExpectedIterations ) then
        begin
          result := true;
          exit;
        end
      ;

      _sqlResult.runQuery( 'SELECT COUNT(*) AS nPts FROM inProductionType' );
      row := _sqlResult.fetchArrayFirst();
      nPTs := row.field( 'nPts' );

      _sqlResult.runQuery( 'SELECT includeZones FROM inControlsGlobal' );
      row := _sqlResult.fetchArrayFirst();
      useZones := boolean( row.field('includeZones' ));

      if ( useZones ) then
        begin
          _sqlResult.runQuery( 'SELECT COUNT(*) AS nZones FROM inZone' );
          row := _sqlResult.fetchArrayFirst();
          nZones := row.field( 'nZones' );
        end
      else nZones := 0;  // Issue 2468

      _sqlResult.runQuery( 'SELECT costTrackDestruction, costTrackVaccination, costTrackZoneSurveillance FROM inGeneral' );
      row := _sqlResult.fetchArrayFirst();
      useCosts := boolean( row.field('costTrackDestruction') ) or boolean( row.field('costTrackVaccination') ) or boolean( row.field('costTrackZoneSurveillance') );

      nExpectedPTRecords :=   nPTs * nExpectedIterations;
      nExpectedZoneRecords := nZones * nExpectedIterations;
      nExpectedZonePTRecords := nZones * nPTs * nExpectedIterations;

      if( 0 = nPTs ) then
        begin
          result := false;
          exit;
        end
      ;

      // The following tables should have nExpectedIterations
      //----------------------------------------------------
      _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM outIteration' );
      row := _sqlResult.fetchArrayFirst();
      if( nExpectedIterations <> row.field( 'nRecords' ) ) then
        begin
          result := false;
          exit;
        end
      ;

      // The following tables should have nExpectedPTRecords
      //----------------------------------------------------
      _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM outIterationByProductionType' );
      row := _sqlResult.fetchArrayFirst();
      if( nExpectedPTRecords <> row.field( 'nRecords' ) ) then
        begin
          result := false;
          exit;
        end
      ;

      if( useCosts ) then
        begin
          _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM outIterationCosts' );
          row := _sqlResult.fetchArrayFirst();
          if( nExpectedPTRecords <> row.field( 'nRecords' ) ) then
            begin
              result := false;
              exit;
            end
          ;
        end
      ;

      _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM outIterationByProductionType' );
      row := _sqlResult.fetchArrayFirst();
      if( nExpectedPTRecords <> row.field( 'nRecords' ) ) then
        begin
          result := false;
          exit;
        end
      ;

      _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM (SELECT DISTINCT iteration, productionTypeID FROM outEpidemicCurves)' );
      row := _sqlResult.fetchArrayFirst();
      if( nExpectedPTRecords <> row.field( 'nRecords' ) ) then
        begin
          result := false;
          exit;
        end
      ;


      // The following tables should have nExpectedZoneRecords
      //------------------------------------------------------
      _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM outIterationByZone' );
      row := _sqlResult.fetchArrayFirst();
      if( nExpectedZoneRecords <> row.field( 'nRecords' ) ) then
        begin
          result := false;
          exit;
        end
      ;

      // The following tables should have nExpectedZonePTRecords
      //--------------------------------------------------------
      _sqlResult.runQuery( 'SELECT COUNT(*) AS nRecords FROM outIterationByZoneAndProductionType' );
      row := _sqlResult.fetchArrayFirst();
      if( nExpectedZonePTRecords <> row.field( 'nRecords' ) ) then
        begin
          result := false;
          exit;
        end
      ;
    end
  ;


  function TSMDatabase.getContainsOutput(): boolean;
    begin
      result := ( ( 0 < completedIterations() ) or ( containsIncompleteIterations() ) );
    end
  ;


  // NOTE: The last iteration may or may not be complete!
  function TSMDatabase.lastIteration(): integer;
    var
      row: TSqlRow;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT MAX( iteration ) AS lastIteration FROM outDailyByProductionType' );
      row := _sqlResult.fetchArrayFirst();

      if( nil = row ) then
        result := 0
      else if( null = row.field('lastIteration') ) then
        result := 0
      else
        result := row.field('lastIteration')
      ;
    end
  ;


  function TSMDatabase.daysInIteration( const it: integer ): integer;
    var
      row: TSqlRow;
      stopStr: string;
      query: string;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT `simStopReason` FROM `inGeneral`' );
      row := _sqlResult.fetchArrayFirst();

      if( nil = row ) then
        result := 0
      else
        begin
          stopStr := row.field(0);

          if( 'firstDetection' = stopStr ) then
            query := 'SELECT firstDetection AS maxLen FROM outIterationByProductionType WHERE iteration = ' + intToStr( it )
          else if( 'specifiedDay' = stopStr ) then
            query := 'SELECT `days` AS maxLen FROM `inGeneral`'
          else if( 'diseaseEnd' = stopStr ) then
            query := 'SELECT diseaseEndDay AS maxLen FROM outIteration WHERE iteration = ' + intToStr( it )
          else if( 'outbreakEnd' = stopStr ) then
            query := 'SELECT outbreakEndDay AS maxLen FROM outIteration WHERE iteration = ' + intToStr( it )
          else
            raise exception.Create( 'No stop reason in TSMDatabase.daysInIteration()' )
          ;

          _sqlResult.runQuery( query );
          row := _sqlResult.fetchArrayFirst();

          if( nil = row ) then
            result := 0
          else if( null = row.field( 'maxLen' ) ) then
            result := 0
          else
            result := row.field( 'maxLen' ) + 1 // Remember to add one for day 0.
          ;
        end
      ;
    end
  ;


  function TSMDatabase.daysInLongestIteration(): integer;
    var
      row: TSqlRow;
      stopStr: string;
      query: string;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT `simStopReason` FROM `inGeneral`' );
      row := _sqlResult.fetchArrayFirst();

      if( nil = row ) then
        result := 0
      else
        begin
          stopStr := row.field(0);

          if( 'firstDetection' = stopStr ) then
            query := 'SELECT MAX( firstDetection ) AS maxLen FROM outIterationByProductionType'
          else if( 'specifiedDay' = stopStr ) then
            query := 'SELECT `days` AS maxLen FROM `inGeneral`'
          else if( 'diseaseEnd' = stopStr ) then
            query := 'SELECT MAX( diseaseEndDay ) AS maxLen FROM outIteration'
          else if( 'outbreakEnd' = stopStr ) then
            query := 'SELECT MAX( outbreakEndDay ) AS maxLen FROM outIteration'
          else
            raise exception.Create( 'No stop reason in TSMDatabase.daysInLongestIteration()' )
          ;

          _sqlResult.runQuery( query );
          row := _sqlResult.fetchArrayFirst();

          if( nil = row ) then
            result := 0
          else if( null = row.field( 'maxLen' ) ) then
            result := 0
          else
            result := row.field( 'maxLen' ) + 1 // Remember to add one for day 0.
          ;
        end
      ;
    end
  ;



  function TSMDatabase.containsIncompleteIterations( completedIt: PInteger = nil; currentIt: PInteger = nil ): boolean;
    var
      row: TSqlRow;
      lastCompleteIteration, currentIteration: integer;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      lastCompleteIteration := completedIterations();

      _sqlResult.runQuery( 'SELECT MAX(iteration) AS maxIt FROM outDailyByProductionType' );
      row := _sqlResult.fetchArrayFirst();

      if( nil = row ) then
        currentIteration := 0
      else if( null = row.field('maxIt') ) then
        currentIteration := 0
      else
        currentIteration := row.field('maxIt')
      ;

      if( nil <> completedIt ) then completedIt^ := lastCompleteIteration;
      if( nil <> currentIt ) then currentIt^ := currentIteration;

      result := ( currentIteration > lastCompleteIteration );
    end
  ;


  function TSMDatabase.getProdTypeCount(): integer; begin result := getDatabaseCount( 'inProductionType' ); end;
  function TSMDatabase.getProdTypePairCount(): integer; begin result := getDatabaseCount( 'inProductionTypePair' ); end;

  function TSMDatabase.containsInitiallyVaccinatedUnits( const prodTypeID: integer = -1 ): boolean;
    var
      row: TSqlRow;
      val: integer;
      prodTypeClause: string;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      if( -1 <> prodTypeID ) then
        prodTypeClause := ' AND productionTypeID = ' + intToStr( prodTypeID )
      else
        prodTypeClause := ''
      ;

      _sqlResult.runQuery( 'SELECT COUNT(*) AS val FROM dynHerd WHERE initialStateCode = "V"' + prodTypeClause );
      row := _sqlResult.fetchArrayFirst();

      val := integer( row.field('val') );

      result := ( 0 <> val );
    end
  ;


  function TSMDatabase.containsInitiallyDestroyedUnits( const prodTypeID: integer = -1 ): boolean;
    var
      row: TSqlRow;
      val: integer;
      prodTypeClause: string;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      if( -1 <> prodTypeID ) then
        prodTypeClause := ' AND productionTypeID = ' + intToStr( prodTypeID )
      else
        prodTypeClause := ''
      ;

      _sqlResult.runQuery( 'SELECT COUNT(*) AS val FROM dynHerd WHERE initialStateCode = "D"' + prodTypeClause );
      row := _sqlResult.fetchArrayFirst();

      val := integer( row.field('val') );

      result := ( 0 <> val );
    end
  ;
// ----------------------------------------------------------------------------


  function TSMDatabase.saveAllDailyOutputs(): boolean;
    var
      row: TSqlRow;
    begin
      result := false;

      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT saveAllDailyOutputs FROM inGeneral' );

      if( 1 = _sqlResult.numRows ) then
        begin
          row := _sqlResult.fetchArrayFirst();
          if( null <> row.field('saveAllDailyOutputs') ) then result := boolean( row.field('saveAllDailyOutputs') );
        end
      ;
    end
  ;


  function TSMDatabase.saveDailyOutputsForIterations(): integer;
    var
      row: TSqlRow;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      result := 3;

      _sqlResult.runQuery( 'SELECT saveDailyOutputsForIterations FROM inGeneral' );

      if( 1 = _sqlResult.numRows ) then
        begin
          row := _sqlResult.fetchArrayFirst();
          if( null <> row.field('saveDailyOutputsForIterations') ) then result := integer( row.field('saveDailyOutputsForIterations') );
        end
      ;
    end
  ;


//-----------------------------------------------------------------------------
//  Why does a version of NAADSM need to be updated?
//-----------------------------------------------------------------------------
  function TSMDatabase.versionUpdateReason( versionID: pstring = nil ): TVersionUpdateReason;
    var
      res: TSqlResult;
      row: TSqlRow;
      outputVersion: string;
    begin
      // If this is a version 4 database, does it contain output?
      //----------------------------------------------------------
      res := TSqlResult.create( 'SELECT `version` FROM `outGeneral`', self );
      if( ( not res.success ) or ( 1 <> res.numRows ) ) then
        raise exception.Create( 'Could not run query in TSMDatabase.versionUpdateReason()' )
      ;

      row := res.fetchArrayFirst();

      if( null = row.field('version') ) then
        begin
          // There is no output, hence there is no problem.
          if( nil <> versionID ) then versionID^ := '';
          result := VERSOK;
        end
      else
        begin
          // There is output.  See if the version that produced it was OK.
          outputVersion := trim( row.field('version') );

          if( nil <> versionID ) then
            versionID^ := outputVersion
          ;

          if( versionIsVers3( outputVersion ) ) then
            result := VERSModelSpecChange
          else if
            ( '4.0.0' = outputVersion )
          or
            ( '4.0.1' = outputVersion )
          or
            ( '4.0.2' = outputVersion )
          or
            ( '4.0.3' = outputVersion )
          or
            ( '4.0.4' = outputVersion )
          or
            ( '4.0.5' = outputVersion )
          or
            ( '4.0.6' = outputVersion )
          or
            ( '4.0.7' = outputVersion )
          or
            ( '4.0.8' = outputVersion )
          then
            result := VERSModelSpecChange
          else if
            ( '4.0.9' = outputVersion )
          or
            ( '4.0.10' = outputVersion )
          or
            ( '4.0.11' = outputVersion )
          then
            result := VERSBug
          else if
            ( '4.0.12' = outputVersion )
          then
            begin
              if( zoneBug4_0_12( self ) ) then
                result := VERSBug
              else
                result := VERSOK
              ;
            end

          else if
            ( '4.0.13' = outputVersion )
          or
            ( '4.1.0' = outputVersion )
          then
            result := VERSOK

            // As new versions are released into the wild, add items here to describe the reasons for the updates.

          else // If outputVersion is anything else, something is screwed up.
            result :=  VERSUnrecognized
          ;
        end
      ;

      res.free();
    end
  ;


end.
