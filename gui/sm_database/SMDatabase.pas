unit SMDatabase;

(*                            
SMDatabase.pas
---------------
Begin: 2005/01/07
Last revision: $Date: 2013-06-27 19:11:23 $ $Author: areeves $
Version: $Revision: 1.129.4.37 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@sruc.ac.uk>
--------------------------------------------------
Copyright (C) 2005 - 2014 NAADSM Development Team

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
    DB_SCHEMA_VERSION = '3.3.0';

    DB_SCHEMA_DATE = '08/02/13 11:29:36 PM';
    DB_SCHEMA_ID = '1375507776'; // Number of seconds from 1970-01-01 00:00:00 to schema date.
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

      // Database management
      function executeWithoutChange( q: string ): boolean; override;

      { All *should* return true on success, false on failure. }
      //function processDDLCommands( resFileContents: string ): boolean; // Now a public function, for use with OldDatabaseFns
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

      class function setSampleDatabaseLanguage( const fileName: string ): boolean;

      function validDateAndNumber( vNumber: string; vDate: TDateTime ): boolean;
      function schemaIDOK(): boolean; override;
      function versionObsolete(): boolean; override;
      function getCurrentDbSchemaVersion(): string; override;
      function getLanguageCode(): string; override;

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

      // Function for checking that the database schema hasn't been screwed up by users
      //-------------------------------------------------------------------------------
      function tablesOK( errMsg: pstring = nil ): boolean;

      // Functions related to application version (which may or may not involve a database schema update)
      //-------------------------------------------------------------------------------------------------
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

      function addProductionType(
          descr: string;
          simulateTransition: boolean = false;
          ptid: integer = -1
        ): integer;

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
      procedure initializeRemoteDatabase();
      procedure prepareForIteration( currentIt: integer ); // Upon iteration start
      procedure clearExistingFinalHerdStates();
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
      procedure clearZonePerimeters();
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
    
    // General purpose units
    Resources,
    MyDialogs,
    MyStrUtils,
    DebugWindow,
    WindowsUtils,
    NetworkUtils,
    FunctionPointers,
    ProbDensityFunctions,
    ZipFunctions,
    RoundToXReplacement_3c,

    // QClasses
    QVectors,

    // More database functions
    OldDatabaseFns,
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
    FormMain
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
    end
  ;


  procedure TSMDatabase.fillChartFields();
    begin
      // Table inDiseaseSpread
      _chartFields.add( TChartField.create( 'inDiseaseSpread', 'movementControlRelID' ) );
      _chartFields.add( TChartField.create( 'inDiseaseSpread', 'distancePdfID' ) );
      _chartFields.add( TChartField.create( 'inDiseaseSpread', 'transportDelayPdfID' ) );

      // Table inProductionType
      _chartFields.add( TChartField.create( 'inProductionType', 'disLatentPeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disSubclinicalPeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disClinicalPeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disImmunePeriodPdfID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'disPrevalenceRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'detProbObsVsTimeClinicalRelID' ) );
      _chartFields.add( TChartField.create( 'inProductionType', 'detProbReportVsFirstDetectionRelID' ) );
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
  function TSMDatabase.executeWithoutChange( q: string ): boolean;
    begin
      result := inherited executeWithoutChange( q );

      if( not result ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:' ) + ' ' + q )
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Database schema update
// ----------------------------------------------------------------------------
  function TSMDatabase.setUpdateReason(): TDBSchemaUpdateReason;
    begin
      result := checkVeryOldUpdateReasons( _vNumber );

      // There was no schema 3.0.84

      if( '3.0.85' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.0.86' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.1.0' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.1.1' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.1.2' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no schema 3.1.3

      else if( '3.1.4' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end
      else if( '3.1.5' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no schema 3.1.6

      else if( '3.1.7' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end
       else if( '3.1.8' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.1.9' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.1.10' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no schema 3.1.11

      else if( '3.1.12' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.1.13' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

        // There was no schema 3.1.14, 3.1.15, or 3.1.16.

      else if( '3.1.17' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There was no schema 3.1.17.

      else if( '3.1.18' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no schema 3.1.19.

      else if( '3.1.20' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no schema 3.1.21.

      else if( '3.1.22' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There were no schema updates for versions 3.1.23, 3.1.24, 3.1.25, or 3.1.26

      else if( '3.2.0' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      else if( '3.2.1' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      else if( '3.2.2' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There were no schema updates for versions 3.2.3 or 3.2.4

      else if( '3.2.5' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There were no schema updates for versions 3.2.6 through 3.2.10

      else if( '3.2.11' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There was no schema update for versions 3.2.12

      else if( '3.2.13' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There were no schema updates for versions 3.2.14 through 3.2.17

      else if( '3.2.18' = _vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no schema update for version 3.2.19

      // There were no schema updates for versions 3.3.1 or 3.3.2

      // There were no schema updates for version 3.4.0

      // The last entry here should be the current schema version.
      // This exception ensures that someone doesn't get too carried away with copy/paste
      else if( DBUpdateOK = result ) then
        raise exception.create( 'Looks like you forgot something in TSMDatabase.setUpdateReason()')
      else if( '3.3.0' = _vNumber ) then
        begin
          if( DBUpdateOK > result ) then result := DBUpdateOK;
        end
      ;
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

      result := handleVeryOldDatabaseSchemas( self, _vNumber, updateSuccess );

      if( not( updateSuccess ) ) then
        exit
      ;

      // If very old updates when OK, keep going...

      // For all subsequent updates, make sure that constraints on the
      // custom output tables don't prevent the updates from being applied.
      disconnectCustomOutputTables();
      disconnectSelectDailyOutputTables();

      // Update 3.0.85 to 3.0.86
      //------------------------
      if( '3.0.85' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.85', true );
            // Capability for custom outputs was added
            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_0_86' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.0.86';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.86 to 3.1.0
      //-----------------------
      if( '3.0.86' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.86', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema3_1_0' ) )
            and
              // New fields were added for zones
              updateInGeneralFor3_1_0( self )
            and
              self.execute( 'UPDATE `inProductionType` SET `zoneDetectionIsTrigger` = FALSE, `zoneDirectTraceIsTrigger` = FALSE, `zoneIndirectTraceIsTrigger` = FALSE' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.0 to 3.1.1
      //----------------------
      if( '3.1.0' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.0', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part1' ) )
            and
              // New fields were added for zone parameters
              ( self.execute( 'UPDATE `inZoneProductionTypePair` SET `useDirectMovementControl` = FALSE, `useIndirectMovementControl` = FALSE, `useDetectionMultiplier` = FALSE' ) )
            and
              // New table for BLOBS was added
              ( self.execute( 'INSERT INTO `dynBlob` (`dynBlobID`) VALUES ("' + DB_SCHEMA_APPLICATION + '")' ) )
            and
              // Zone parameter moved from inGeneral to InControlsGlobal
              adjustZoneParametersFor3_1_1( self )
            and
              processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part2' ) )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.1';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.1 to 3.1.2
      //----------------------
      if( '3.1.1' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.1', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema3_1_2' ) )
            and
              // New fields were added for within-herd prevalence
              self.execute( 'UPDATE `inGeneral` SET `useWithinHerdPrevalence` = FALSE' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.2';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.2 to 3.1.4
      //----------------------
      if( '3.1.2' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.2', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_4' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.4';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.4 to 3.1.5
      //----------------------
      if( '3.1.4' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.4', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema3_1_5' ) )
            and
              self.execute( 'INSERT INTO `readEventCodes` ( eventCode, definition ) VALUES ("Z", "Zone focus created")' )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.5';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.5 to 3.1.7
      // (There was no 3.1.6)
      //----------------------
      if( '3.1.5' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.5', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_7' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.7';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.7 to 3.1.8
      //----------------------
      if( '3.1.7' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.7', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_8' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.8';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.8 to 3.1.9
      //----------------------
      if( '3.1.8' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.8', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema3_1_9' ) )
            and
              updateInGeneralFor3_1_9( self )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.9';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.9 to 3.1.10
      //-----------------------
      if( '3.1.9' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.9', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_10' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.10';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.10 to 3.1.12
      // (There was no schema 3.1.11)
      //-----------------------------
      if( '3.1.10' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.10', true );

            execSuccess := self.execute( 'INSERT INTO `readEventCodes` ( eventCode, definition ) VALUES ("C", "Zone changed")' );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.12';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.12 to 3.1.13
      //-----------------------------
      if( '3.1.12' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.12', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_13' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.13';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.13 to 3.1.17
      //------------------------
      if( '3.1.13' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.13', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part1' ) )
            and
              // output frequency code was introduced in version 3.1.17
              adjustCustomOutputDefinitionsFor3_1_17( self )
            and
              populateSelectDailyOutputsFor3_1_17( self )
            and
              processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part2' ) )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.17';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.17 to 3.1.18
      //------------------------
      if( '3.1.17' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.17', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema3_1_18' ) )
            and
              populateSelectDailyOutputsFor3_1_18( self )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.18';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.18 to 3.1.20
      //------------------------
      if( '3.1.18' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.18', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_20' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.20';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;


      // Update 3.1.20 to 3.1.22
      //------------------------
      if( '3.1.20' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.20', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_1_22' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.1.22';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;


      // Update 3.1.22 to 3.2.0
      //-----------------------
      if( '3.1.22' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.22', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewInputs' ) )
            and
              processDDLCommands( getResourceAsString( 'DBSchema3_2_0_RenamedOutputs' ) )
            and
              processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewOutputs' ) )
            and
              processDDLCommands( getResourceAsString( 'DBSchema3_2_0_AlteredHerdTable' ) )
            and
              updateEventAndControlStateCodesFor3_2_0( self )
            and
              populateDefaultInputsFor3_2_0( self )
            and
              populateSelectDailyOutputsFor3_2_0( self )
            and
              projectZoneCoordinatesFor3_2_0( self )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.0 to 3.2.1
      //----------------------
      if( '3.2.0' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.0', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_2_1' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.1';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.1 to 3.2.2
      //----------------------
      if( '3.2.1' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.1', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_2_2' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.2';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.2 to 3.2.5
      //----------------------
      if( '3.2.2' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.2', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_2_5' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.5';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;


      // Update 3.2.5 to 3.2.11
      //-----------------------
      if( '3.2.5' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.5', true );

            execSuccess := (
              processDDLCommands( getResourceAsString( 'DBSchema3_2_11' ) )
            and
              populateSelectDailyOutputsFor3_2_11( self )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.11';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.11 to 3.2.13
      //------------------------
      if( '3.2.11' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.11', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_2_13' ) );

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

      // Double-check 3.2.13 to make sure that firstDetUInf and firstDetAInf actually exist
      // (Due to some bugs in 3.2.13, 14, and 15, they didn't...)
      //-----------------------------------------------------------------------------------
      if( '3.2.13' = _vNumber ) then
        begin
          try
            execSuccess := true;

            if( not( self.fieldExists( 'firstDetUInf', 'outIterationByProductionType' ) ) ) then
              execSuccess := execSuccess and self.execute( 'ALTER TABLE outIterationByProductionType ADD COLUMN firstDetUInf LONG' )
            ;

            if( not( self.fieldExists( 'firstDetAInf', 'outIterationByProductionType' ) ) ) then
              execSuccess := execSuccess and self.execute( 'ALTER TABLE outIterationByProductionType ADD COLUMN firstDetAInf LONG' )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.13 to 3.2.18
      //------------------------
      if( '3.2.13' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.13', true );

            execSuccess := processDDLCommands( getResourceAsString( 'DBSchema3_2_18' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.2.18';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.18 to 3.3.0
      //-----------------------
      if( '3.2.18' = _vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.18', true );

            execSuccess :=
              processDDLCommands( getResourceAsString( 'DBSchema3_3_0' ) )
            and
              self.execute( 'UPDATE `inGeneral` SET `initInfectedRandomize` = FALSE' )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            _vNumber := '3.3.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      //--- IMPORTANT ---
      // When updating this function, don't forget to update makeDBTables()
      //--- IMPORTANT ---

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
                ( ( '3.1.9' = _vNumber ) and ( 1176823233 = _vID ) )
              or
                ( ( '3.1.10' = _vNumber ) and ( 1176836388 = _vID ) )
              or
                ( ( '3.1.12' = _vNumber ) and ( 1177957221 = _vID ) )
              or
                ( ( '3.1.13' = _vNumber ) and (  1178121515 = _vID ) )
              or
                ( ( '3.1.17' = _vNumber ) and ( 1189206366 = _vID ) )
              or
                ( ( '3.1.18' = _vNumber ) and ( 1224203103 = _vID ) )
              or
                ( ( '3.1.20' = _vNumber ) and ( 1231364931 = _vID ) )
              or
                ( ( '3.1.22' = _vNumber ) and ( 1233602101 = _vID ) )
              or
                ( ( '3.2.0' = _vNumber ) and ( 1253287889 = _vID ) )
              or
                ( ( '3.2.1' = _vNumber ) and ( 1256249352 = _vID ) )
              or
                ( ( '3.2.2' = _vNumber ) and ( 1257620055 = _vID ) )
              or
                ( ( '3.2.5' = _vNumber ) and ( 1274302573 = _vID ) )
              or
                ( ( '3.2.11' = _vNumber ) and ( 1305654509 = _vID ) )
              or
                ( ( '3.2.13' = _vNumber ) and ( 1309203153 = _vID ) )
              or
                ( ( '3.2.18' = _vNumber ) and ( 1317332651 = _vID ) )
              or
                ( ( '3.3.0' = _vNumber ) and ( 1375507776 = _vID ) )
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
    var
      currentShortDateFormat: string;
    begin
      // Recall that a database schema version corresponds to the
      // application version in which it was introduced, and that
      // not all new application versions introduce changes to the schema.
      // Consequently, database schema versions are not necessarily sequential.

      currentShortDateFormat := ShortDateFormat;
      ShortDateFormat := 'mm/dd/yyyy';

      if
        veryOldVersionIsValid( vNumber, vDate )
      or
        ( ( '3.0.85' = vNumber ) and ( varToDateTime( '10/13/2006 11:57:00 AM' ) = vDate ) )
      or
        ( ( '3.0.86' = vNumber ) and ( varToDateTime( '1/30/2007 11:00:00 AM' ) = vDate ) )
      or
        ( ( '3.1.0' = vNumber ) and ( varToDateTime( '12/19/2006 12:54:00 PM' ) = vDate ) )
      or
        ( ( '3.1.1' = vNumber ) and ( varToDateTime( '1/16/2007 10:51:00 AM' ) = vDate ) )
      or
        ( ( '3.1.2' = vNumber ) and ( varToDateTime( '1/17/2007 4:29:00 PM' ) = vDate ) )
      or
        ( ( '3.1.4' = vNumber ) and ( varToDateTime( '2/1/2007 7:13:00 PM' ) = vDate ) )
      or
        ( ( '3.1.5' = vNumber ) and ( varToDateTime( '2/4/2007 6:11:00 PM' ) = vDate ) )
      or
        ( ( '3.1.7' = vNumber ) and ( varToDateTime( '2/8/2007 11:15 AM' ) = vDate ) )
      or
        // In some cases, due to user error or misconfiguration,
        // the date format for this version is transposed.
        ( ( '3.1.7' = vNumber ) and ( varToDateTime( '8/2/2007 11:15 AM' ) = vDate ) )
      or
        ( ( '3.1.8' = vNumber ) and ( varToDateTime( '3/23/2007 2:42:00 PM' ) = vDate ) )
      or
        // Beginning with version 3.1.9, use the schema ID instead of the schema date.
        // This solves some of the internationalization problems.
        ( schemaIDOK() )
      then // the date and number/ID are valid.
        result := true
      else // the date and number/ID are not recognized as NAADSM databases.
        result := false
      ;

      ShortDateFormat := currentShortDateFormat;
    end
  ;


  function TSMDatabase.versionObsolete(): boolean;
    begin
      // Add more numbers here, if it ever becomes necessary.
      // (Hopefully it won't for a long, long time.)

      if
        ( '3.0.9' = _vNumber )
      or
        ( '3.0.13' = _vNumber )
      then
        result := true
      else
        result := false
      ;
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
  procedure TSMDatabase.makeDBTables();
    begin
      makeVeryOldTables( self );

      processDDLCommands( getResourceAsString( 'DBSchema3_0_85' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_0_86' ) );
      // The jump was made here from 3.0 to 3.1
      processDDLCommands( getResourceAsString( 'DBSchema3_1_0' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part1' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part2' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_2' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_4' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_5' ) );
      // The "Z" code is new in version 3.1.5.  All other event codes are populated in an earlier DDL file.
      self.execute( 'INSERT INTO `readEventCodes` (eventCode, definition) VALUES ("Z", "Zone focus created")' );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_7' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_8' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_9' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_10' ) );
      // The "C" code is new in version 3.1.12.  No other schema changes were made.
      self.execute( 'INSERT INTO `readEventCodes` (eventCode, definition) VALUES ("C", "Zone changed")' );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_13' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part1' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part2' ) );
      // Values for the table inSelectDailyOutputs are new in version 3.1.17
      populateSelectDailyOutputsFor3_1_17( self );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_18' ) );
      // Additional values for the table inSelectDailyOutputs are new in version 3.1.17
      populateSelectDailyOutputsFor3_1_18( self );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_20' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_1_22' ) );

      // The jump was made here from version 3.1 to version 3.2.
      processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewInputs' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_0_RenamedOutputs' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewOutputs' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_0_AlteredHerdTable' ) );
      updateEventAndControlStateCodesFor3_2_0( self );
      populateSelectDailyOutputsFor3_2_0( self );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_1' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_2' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_5' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_11' ) );
      populateSelectDailyOutputsFor3_2_11( self );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_13' ) );
      processDDLCommands( getResourceAsString( 'DBSchema3_2_18' ) );

      // The jump was made here from version 3.2 to version 3.3.
      processDDLCommands( getResourceAsString( 'DBSchema3_3_0' ) );

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
      q: string;
    begin
      dict := TQueryDictionary.create();

      dict['inGeneralID'] := sqlQuote( DB_SCHEMA_APPLICATION );

      dict['scenarioDescr'] := sqlQuote( 'New scenario' );
      dict['iterations'] := intToStr( 1 );
      dict['days'] := intToStr( 10 );

      dict['includeContactSpread'] := usBoolToText( false );
      dict['includeAirborneSpread'] := usBoolToText( false );
      dict['useAirborneExponentialDecay'] := usBoolToText( false );
      dict['useWithinHerdPrevalence'] := usBoolToText( false );

      dict['costTrackDestruction'] := usBoolToText( false );
      dict['costTrackVaccination'] := usBoolToText( false );
      dict['costTrackZoneSurveillance'] := usBoolToText( false );

      dict['useFixedRandomSeed'] := usBoolToText( false );
      dict['randomSeed'] := intToStr( 527 );

      dict['saveAllDailyOutputs'] := usBoolToText( false );
      dict['saveDailyOutputsForIterations'] := intToStr( 3 );

      dict['writeDailyStatesFile'] := usBoolToText( false );
      dict['dailyStatesFileName'] := DATABASE_NULL_VALUE;

      dict['saveDailyEvents'] := usBoolToText( false );
      dict['saveDailyExposures'] := usBoolToText( false );
      dict['saveIterationOutputsForHerds'] := usBoolToText( false );

      dict['useCustomOutputs'] := usBoolToText( false );

      dict['writeNAADSMapOutput'] := usBoolToText( false );
      dict['NAADSMapDirectory'] := DATABASE_NULL_VALUE;

      dict['initInfectedRandomize'] := usBoolToText( false );

      q := writeQuery( 'inGeneral', QInsert, dict );
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
      dict.Free();
    end
  ;


  procedure TSMDatabase.setInControlsGlobalDefaults();
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();

      dict['controlsGlobalID']   := sqlQuote( DB_SCHEMA_APPLICATION );
      dict['includeDestruction'] := usBoolToText( false );
      dict['includeDetection']   := usBoolToText( false );
      dict['includeVaccination'] := usBoolToText( false );
      dict['includeTracing']     := usBoolToText( false );
      dict['includeTracingHerdExam']     := usBoolToText( false );
      dict['includeTracingTesting']     := usBoolToText( false );
      dict['includeZones']       := usBoolToText( false );

      q := writeQuery( 'inControlsGlobal', QInsert, dict );
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
      dict.Free();
    end
  ;


  procedure TSMDatabase.setupDynBlob();
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();

      dict['dynBlobID'] := sqlQuote( DB_SCHEMA_APPLICATION );
      dict['zonePerimeters'] := DATABASE_NULL_VALUE;

      q := writeQuery( 'dynBlob', QInsert, dict );
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
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
        + ' ORDER BY inDiseaseSpread.spreadID'
        + ' )'
      ;

      if( not isReadOnly ) then
        begin
          // Yes, there is a reason for this inherited call.  (I don't remember what it is.)
          if not( inherited execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// File I/O
// ----------------------------------------------------------------------------
  function TSMDatabase.save( newFileName: string = '' ): boolean;
    var
      q: string;
    begin
      // Right before saving, clean up the database
      deleteUnusedContactSpreadParams();

      if( '' = languageCode ) then
        begin
          q := 'UPDATE `inGeneral` SET `language` = ' + sqlQuote( i88nLanguageCodeString() );
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
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
            + ', ''NAADSMXXXX'', NOW(), NULL, 0, ''' + versionNumber + ''' )'
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
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
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
    var
      q: string;
    begin
      q := 'UPDATE `inGeneral` SET `days` = ' + intToStr( numDays );
      
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
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
    var
      q: string;
    begin
      if( remoteDBParams.useRemoteDatabase ) then
        remoteExecute( 'UPDATE outGeneral SET completedIterations = completedIterations + 1 WHERE jobID = ' + intToStr( remoteDBParams.jobID ) )
      else
        begin
          q := 'UPDATE outGeneral SET completedIterations = completedIterations + 1';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
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
      qDict['outbreakEnded'] := usBoolToText( outbreakEnded );
      if( outbreakEnded ) then
        qDict['outbreakEndDay'] := intToStr( outbreakEndDay )
      else
        qDict['outbreakEndDay'] := DATABASE_NULL_VALUE
      ;

      // if -1 = _diseaseEndDay, the active disease phase of the outbreak
      // did not end before time ran out.
      qDict['diseaseEnded'] := usBoolToText( diseaseEnded );
      if( diseaseEnded ) then
        qDict['diseaseEndDay'] := intToStr( diseaseEndDay )
      else
        qDict['diseaseEndDay'] := DATABASE_NULL_VALUE
      ;

      qDict['zoneFociCreated'] := usBoolToText( zoneFociCreated );

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

          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
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
            begin
              if not( execute( q ) ) then
                raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
              ;
            end
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
                + ' `infnUDir` + `infnUInd` + `infnUAir`,'
                + ' `infnADir` + `infnAInd` + `infnAAir`,'
                + ' `detnUClin` + `detnUTest`,'
                + ' `detnAClin` + `detnATest`,'
                + ' `tsdUSubc` + `tsdUClin`,'
                + ' `appdUInfectious`'
              + ' FROM `outDailyByProductionType`'
              + ' WHERE '
                + ' `iteration` = ' + intToStr( it )
          ;

          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
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
                + ' `infnUDir` + `infnUInd` + `infnUAir`,'
                + ' `infnADir` + `infnAInd` + `infnAAir`,'
                + ' `detnUClin` + `detnUTest`,'
                + ' `detnAClin` + `detnATest`,'
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
            + '   `infnUDir` + `infnUInd` + `infnUAir` AS `infectedUnits`,'
            + '   `infnADir` + `infnAInd` + `infnAAir` AS `infectedAnimals`,'
            + '   `tsdUSubc` + `tsdUClin` AS `infectiousUnits`,'
            + '   `appdUInfectious`,'
            + '   `detnUClin` + `detnUTest` AS `detectedUnits`,'
            + '   `detnAClin` + `detnATest` AS `detectedAnimals`'
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
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyByZone WHERE iteration = ' + itStr;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyEvents WHERE iteration = ' + itStr;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyExposures WHERE iteration = ' + itStr;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyByZoneAndProductionType WHERE iteration = ' + itStr;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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

      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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

      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

       //  Add foreign key constraint to table outCustIterationByProductionType
       q := 'ALTER TABLE `outCustIterationByProductionType`'
        + ' ADD CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`'
        + ' FOREIGN KEY (`productionTypeID`) '
        + ' REFERENCES `inProductionType` (`productionTypeID`)'
       ;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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

      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

       //  Add foreign key constraint to table outCustIterationByProductionType
       q := 'ALTER TABLE `outCustIterationByZone`'
        + ' ADD CONSTRAINT `inZone_outCustIterationByZone_FK1`'
        + ' FOREIGN KEY (`zoneID`) '
        + ' REFERENCES `inZone` (`zoneID`)'
       ;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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

      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

       //  Add zone foreign key constraint to table outCustIterationByZoneAndProductionType
       q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
        + ' ADD CONSTRAINT `inZone_outCustIterationByZoneAndProductionType_FK1`'
        + ' FOREIGN KEY (`zoneID`) '
        + ' REFERENCES `inZone` (`zoneID`)'
       ;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

       //  Add PT foreign key constraint to table outCustIterationByZoneAndProductionType
       q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
        + ' ADD CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`'
        + ' FOREIGN KEY (`productionTypeID`) '
        + ' REFERENCES `inProductionType` (`productionTypeID`)'
       ;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

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
      // Use the executeWithoutChange() below.
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
      // Use the executeWithoutChange() below.
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
      // Use the executeWithoutChange() below.
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
    var
      q: string;
    begin
      // Remove constraints involving custom output tables before dropping them.
      // (This step may not be necessary, but it seems like a good idea.)
      disconnectCustomOutputTables();

      if( self.tableExists( 'outCustIteration' ) ) then
        begin
          q := 'DROP TABLE outCustIteration';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outCustIterationByProductionType' ) ) then
        begin
          q := 'DROP TABLE outCustIterationByProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outCustIterationByZone' ) ) then
        begin
          q := 'DROP TABLE outCustIterationByZone';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
          q := 'DROP TABLE outCustIterationByZoneAndProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;
    end
  ;


  procedure TSMDatabase.dropSelectDailyOutputTables();
    var
      q: string;
    begin
      disconnectSelectDailyOutputTables();

      if( self.tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
          q := 'DROP TABLE outSelectDailyByProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
          q := 'DROP TABLE outSelectDailyByZoneAndProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
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
        begin
          if not( execute( insertQuery ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + insertQuery )
          ;
        end
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
                  (*
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      row.debug();
                      row := res.fetchArrayNext();
                    end
                  ;
                  *)

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
            begin
              if not( execute( insertQuery ) ) then
                raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + insertQuery )
              ;
            end
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
            begin
              if not( execute( insertQuery ) ) then
                raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + insertQuery )
              ;
            end
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
                  (*
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      row.debug();
                      row := res.fetchArrayNext();
                    end
                  ;
                  *)

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
                begin
                  if not( execute( insertQuery ) ) then
                    raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + insertQuery )
                  ;
                end
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
            + ', NULL )'
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
        + ' `finalStateCode` = NULL,'
        + ' `finalControlStateCode` = NULL,'
        + ' `finalDetectionStateCode` = NULL,'
        + ' `cumulInfected` = 0,'
        + ' `cumulDetected` = 0,'
        + ' `cumulDestroyed` = 0,'
        + ' `cumulVaccinated` = 0'
      ;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      clearExistingFinalHerdStates();

      // Clear daily outputs
      //---------------------
      q := 'DELETE FROM outDailyByProductionType';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyEvents';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyExposures';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyByZoneAndProductionType';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outDailyByZone';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      // Clear iteration output records
      //-------------------------------
      q := 'DELETE FROM outIteration';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outIterationByProductionType';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outIterationCosts';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outIterationByZoneAndProductionType';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM outIterationByZone';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      // Clear epidemic curves
      //----------------------
      q := 'DELETE FROM outEpidemicCurves';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      // Clear custom outputs
      //---------------------
      if( self.tableExists( 'outCustIteration' ) ) then
        begin
          q := 'DELETE FROM outCustIteration';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outCustIterationByProductionType' ) ) then
        begin
          q := 'DELETE FROM outCustIterationByProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outCustIterationByZone' ) ) then
        begin
          q := 'DELETE FROM outCustIterationByZone';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
          q := 'DELETE FROM outCustIterationByZoneAndProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      // Clear selected daily outputs
      //-----------------------------
      if( self.tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
          q := 'DELETE FROM outSelectDailyByProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      if( self.tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
          q := 'DELETE FROM outSelectDailyByZoneAndProductionType';
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
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
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;

          q := 'DELETE FROM outDailyEvents WHERE iteration < ' + iterationsToDelete;
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;

          q := 'DELETE FROM outDailyExposures WHERE iteration < ' + iterationsToDelete;
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;

          q := 'DELETE FROM outDailyByZone WHERE iteration < ' + iterationsToDelete;
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;

          q := 'DELETE FROM outDailyByZoneAndProductionType WHERE iteration < ' + iterationsToDelete;
          if not( execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;
        end
      ;

      // Set up the remote database, if necessary
      //-----------------------------------------
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'INSERT INTO `outIteration`'
            + ' ( `jobID`, `iteration`, `diseaseEnded`, `diseaseEndDay`, `outbreakEnded`, `outbreakEndDay`, `zoneFociCreated` )'
            + ' VALUES'
            + ' ( ' + intToStr( remoteDBParams.jobID ) + ', ' + intToStr( currentIt ) + ', NULL, NULL, NULL, NULL, NULL )'
          ;
          remoteExecute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.clearExistingFinalHerdStates();
    var
      q: string;
    begin
      q := 'UPDATE `dynHerd` SET '
        + ' `finalStateCode` = initialStateCode,'
        + ' `finalControlStateCode` = "U",'
        + ' `finalDetectionStateCode` = NULL'
      ;
      dbcout( q, DBSMDATABASE );
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
      clearZonePerimeters();
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
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
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
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;

      q := 'DELETE FROM `inZone` WHERE `zoneID` = ' + zoneID;
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
    end
  ;

  
  function TSMDatabase.addProductionType(
      descr: string;
      simulateTransition: boolean = false;
      ptid: integer = -1
    ): integer;
    var
      q: string;
    begin
      initializeAllOutputRecords();
      
      if( ptid < 1 ) then
        begin
          q := 'INSERT INTO `inProductionType` (`descr`, `useDiseaseTransition`) VALUES ('
            + sqlQuote( descr )
            + ', '
            + usBoolToText( simulateTransition )
            + ')'
          ;
        end
      else
        begin
          q := 'INSERT INTO `inProductionType` (`productionTypeID`, `descr`, `useDiseaseTransition`) VALUES ('
            + intToStr( ptid )
            + ', '
            + sqlQuote( descr )
            + ', '
            + usBoolToText( simulateTransition )
            + ')'
          ;
        end
      ;

      if( execute( q ) ) then
        result := lastInsertID()
      else
        result := -75
      ;

    end
  ;

  procedure TSMDatabase.makeProductionTypePair( src, dest: integer );
    var
      q, q2: string;
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
        begin
          q2 := 'INSERT INTO `inProductionTypePair` ( `sourceProductionTypeID`, `destProductionTypeID` ) '
            + 'VALUES( ' + intToStr( src ) + ', ' + intToStr( dest ) + ' )'
          ;
          if not( execute( q2 ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q2 )
          ;
        end
      ;
    end
  ;

  procedure TSMDatabase.clearHerds();
    var
      q: string;
    begin
      initializeAllOutputRecords();
      q := 'DELETE FROM `dynHerd`';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
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

      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
      ;
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
  procedure TSMDatabase.clearZonePerimeters();
    var
      q: string;
    begin
      q := 'UPDATE `dynBlob` SET `zonePerimeters` = NULL';
      if not( execute( q ) ) then
        raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
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
      q: string;
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
          q := 'UPDATE `inGeneral` SET `language` = ' + db.sqlQuote( i88nLanguageCodeString() );
          if not( db.execute( q ) ) then
            raise exception.create( tr( 'Database records could not be saved.  Query failed:') + ' ' + q )
          ;

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
  (*
  "Private" functions used to determine update reason:
    function cheyenneUpdateReason( const oldVersion: string ): TVersionUpdateReason;
    function laramieUpdateReason( db: TSMDatabase; const oldVersion: string ): TVersionUpdateReason;
    function rivertonUpdateReason( db: TSMDatabase; const oldVersion: string ): TVersionUpdateReason;

  "Private" functions used to check from bugs related to very specialized conditions:
    function bug3_1_19( db: TSMDatabase ): boolean;
    function bug3_1_22( db: TSMDatabase ): boolean;
    function bug3_1_25( db: TSMDatabase ): boolean;
    function zoneBug3_2_18( db: TSMDatabase ): boolean;
  *)


  // Returns true if the scenario database contains more than 1 zone.
  // See http://www.naadsm.org/bugs?id=180 for description.
  function zoneBug3_2_18( db: TSMDatabase ): boolean;
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


  // Returns true if the scenario database contains any shipping or airborne transport delay pdfs
  // that are something other than fixe value( 0 ) or fixe value( 1 )
  function bug3_1_25( db: TSMDatabase ): boolean;
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;
      badCharts: TQIntegerVector;
      mode: integer;
      i: integer;
    begin
      // If this scenario uses a bad delay pdf AND if at least
      // one airborne spread or contact spread model uses that pdf,
      // then there is a problem. Otherwise, calculations are fine.

      db2 := db as TSqlDatabase;
      res := TSqlResult.create( 'SELECT `chartID`, `chartType`, `mode` FROM `inChart` WHERE `fieldName` = "CMDelayDirect" OR `fieldName`="CMDelayIndirect" OR `fieldName`="AIRDelay"', db2 );
      row := res.fetchArrayFirst();

      badCharts := TQIntegerVector.create();

      while( nil <> row ) do
        begin
          if( 'Point' <> row.field( 'chartType' ) ) then
            badCharts.append( integer( row.field( 'chartID' ) ) )
          else
            begin
              if( null = row.field( 'mode' ) ) then
                badCharts.append( integer( row.field( 'chartID' ) ) )
              else
                begin
                  mode := RoundDbl( double( row.field( 'mode' ) ) );

                  if( ( 0 <> mode ) and ( 1 <> mode ) ) then
                    badCharts.append( integer( row.field( 'chartID' ) ) )
                  ;
                end
              ;
            end
          ;

          row := res.fetchArrayNext();
        end
      ;

      result := false; // Until shown otherwise

      if( 0 <> badCharts.count ) then
        begin
          for i := 0 to badCharts.count - 1 do
            begin
              q := 'SELECT COUNT(*) AS n FROM `inDiseaseSpread` WHERE `transportDelayPdfID` = ' + intToStr( badCharts.at( i ) );
              res.runQuery( q );
              row := res.fetchArrayFirst();

              if( 0 < integer( row.field( 'n' ) ) ) then
                begin
                  result := true;
                  break;
                end
              ;
            end
          ;
        end
      ;

      if( false = result ) then
        result := zoneBug3_2_18( db )
      ;

      badCharts.Free();
      res.free();
    end
  ;


  // Returns true if there is a calculation problem as a result of the bug concerning
  // max distance of airborne spread set to 1 km
  function bug3_1_22( db: TSMDatabase ): boolean;
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;
      usesLinearDecline: boolean;
      nModelsWithProblems: integer;
    begin
      // If this scenario uses airborne spread with linear decline  AND if at least
      // one airborne spread model has a max distance of spread of 1 km or less
      // then there is a problem. Otherwise, calculations are fine.

      db2 := db as TSqlDatabase;
      res := TSqlResult.create( 'SELECT `includeAirborneSpread`, `useAirborneExponentialDecay` FROM `inGeneral`', db2 );
      row := res.fetchArrayFirst();

      usesLinearDecline := ( row.field( 'includeAirborneSpread' ) ) and ( not( row.field( 'useAirborneExponentialDecay' ) ) );

      if( usesLinearDecline ) then
        begin
          q := 'SELECT COUNT(*) AS n FROM `inDiseaseSpread` WHERE'
             + '`spreadID` IN (SELECT `airborneContactSpreadID` FROM `inProductionTypePair` WHERE useAirborneSpread = TRUE)'
             + ' AND `spreadMethodCode` = "A"'
             + ' AND `maxDistAirborneSpread` <= 1'
          ;

          res.runQuery( q );
          row := res.fetchArrayFirst();
          nModelsWithProblems := row.field( 'n' );

          result := ( 0 < nModelsWithProblems ); // There is at least one model with a max distance of 1 km or less.
        end
      else
        result := false
      ;

      if( false = result ) then
        result := zoneBug3_2_18( db )
      ;

      res.free();
    end
  ;


  // Returns true if there is a calculation problem as a result the bug concerning days-left-in-status and within-unit prevalence.
  function bug3_1_19( db: TSMDatabase ): boolean;
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      usesPrevalence: boolean;
      usesDaysLeftInInitialState: integer;
    begin
      // If this scenario uses within-unit prevalence
      // AND if at least one unit has a daysLeftInInitialState of something other than -1,
      // then there is a problem. Otherwise, calculations are fine.

      db2 := db as TSqlDatabase;
      res := TSqlResult.create( 'SELECT `useWithinHerdPrevalence` FROM `inGeneral`', db2 );
      row := res.fetchArrayFirst();

      usesPrevalence := row.field( 'useWithinHerdPrevalence' );

      if( usesPrevalence ) then
        begin
          res.runQuery( 'SELECT COUNT(*) AS n FROM `DynHerd` WHERE `daysLeftInInitialState` > -1');
          row := res.fetchArrayFirst();
          usesDaysLeftInInitialState := row.field( 'n' );
          
          if( 0 < usesDaysLeftInInitialState ) then
            result := true
          else
            result := false
          ;
        end
      else
        result := false
      ;

      if( false = result ) then
        result := zoneBug3_2_18( db )
      ;

      res.Free();
    end
  ;


  function cheyenneUpdateReason( const oldVersion: string ): TVersionUpdateReason;
    begin
      if( rightStr( oldVersion, 8 ) <> 'Cheyenne' ) then
        result := VERSModelSpecChange
      else if
        ( '3.0.80-Cheyenne' = oldVersion )
      or
        // There was no 3.0.81-Cheyenne
        ( '3.0.82-Cheyenne' = oldVersion )
      or
        // There was no 3.0.83-Cheyenne
        ( '3.0.84-Cheyenne' = oldVersion )
      then
        begin
          // See comment below RE specification change for version 3.1.15.
          result := VERSModelSpecChange;
        end
      else if
        // There was no 3.0.85-Cheyenne
        // There were no versions of Cheyenne that correspond to 3.1.0 through 3.1.12.
        ( '3.1.13-Cheyenne' = oldVersion )
      or
        ( '3.1.14-Cheyenne' = oldVersion )
      or
        ( '3.1.15-Cheyenne' = oldVersion )
      then
        begin
          // See comments below RE bugs in versions 3.1.0 - 3.1.15.
          result := VERSBUG;
        end
      else
        // There have been no versions of Cheyenne since 3.1.15.
        begin
          result := VERSUnrecognized;
        end
      ;
    end
  ;


  function laramieUpdateReason( db: TSMDatabase; const oldVersion: string ): TVersionUpdateReason;
    begin
      if( rightStr( oldVersion, 7 ) <> 'Laramie' ) then
        result := VERSModelSpecChange
      else if
        ( '3.1.18-Laramie' = oldVersion )
      or
        ( '3.1.19-Laramie' = oldVersion )
      then
        begin
          if( bug3_1_19( db ) or bug3_1_25( db ) ) then
            result := VERSBUG
          else
            result := VERSOK
          ;
        end
      else
        begin
          // There have been no versions of Laramie since 3.1.19.
          result := VERSUnrecognized;
        end
      ;
    end
  ;


  function rivertonUpdateReason( db: TSMDatabase; const oldVersion: string ): TVersionUpdateReason;
    begin
      if( rightStr( oldVersion, 8 ) <> 'Riverton' ) then
        result := VERSModelSpecChange
      else if ( '3.1.22-Riverton' = oldVersion ) then
        begin
          if( bug3_1_22( db ) or bug3_1_25( db ) ) then
            result := VERSBUG
          else
            result := VERSOK
          ;
        end
      else if( '3.1.23-Riverton' = oldVersion ) then
        begin
          if( zoneBug3_2_18( db ) ) then
            result := VERSBUG
          else
            result := VERSOK
          ;
        end
      else if( '3.3.2-Riverton' = oldVersion ) then
        result := VERSOK
      else // There are no other versions of Riverton
        result := VERSUnrecognized
      ;
    end
  ;


  function TSMDatabase.versionUpdateReason( versionID: pstring = nil ): TVersionUpdateReason;
    var
      res: TSqlResult;
      row: TSqlRow;
      oldVersion: string;
      usingCheyenne, usingLaramie, usingRiverton: boolean;
    begin
      {$IFDEF CHEYENNE}
        usingCheyenne := true;
      {$ELSE}
        usingCheyenne := false;
      {$ENDIF}

      {$IFDEF LARAMIE}
        usingLaramie := true;
      {$ELSE}
        usingLaramie := false;
      {$ENDIF}

      {$IFDEF RIVERTON}
        usingRiverton := true;
      {$ELSE}
        usingRiverton := false;
      {$ENDIF}

      res := TSqlResult.create( 'SELECT `version` FROM `outGeneral`', self );

      result := VERSUnrecognized;

      // Versions through 3.0.58 are automatically updated.
      // (outGeneral.version wasn't introduced until 3.0.59)

      if( not res.success ) then
        begin
          if( nil <> versionID ) then versionID^ := '';
          // Do nothing else
        end
      else
        begin
          row := res.fetchArrayFirst();

          if( nil = row ) then
            begin
              if( nil <> versionID ) then versionID^ := '';
              result := VERSOK;
              // Do nothing else
            end
          else if( null = row.field('version') ) then
            begin
              if( nil <> versionID ) then versionID^ := '';
              result := VERSOK;
              // Do nothing else
            end
          else
            begin
              oldVersion := trim( row.field('version') );

              if( nil <> versionID ) then
                versionID^ := oldVersion
              ;

              // As new versions are released into the wild, add items here to describe the reasons for the updates.

              // Take care of experimental versions first...
              if( usingCheyenne ) then
                result := cheyenneUpdateReason( oldVersion )
              else if( usingLaramie ) then
                result := laramieUpdateReason( self, oldVersion )
              else if( usingRiverton ) then
                result := rivertonUpdateReason( self, oldVersion )

              // ...then deal with standard versions.
              else if
                ( '3.0.59' = oldVersion )
              or
                ( '3.0.60' = oldVersion )
              or
                ( '3.0.61' = oldVersion )
              or
                ( '3.0.62' = oldVersion )
              or
                ( '3.0.63' = oldVersion )
              or
                ( '3.0.64' = oldVersion )
              or
                ( '3.0.65' = oldVersion )
              or
                ( '3.0.66' = oldVersion )
              or
                ( '3.0.67' = oldVersion )
              or
                ( '3.0.68' = oldVersion )
              or
                ( '3.0.69' = oldVersion )
              or
                ( '3.0.70' = oldVersion )
              or
                ( '3.0.71' = oldVersion )
              or
                ( '3.0.72' = oldVersion )
              or
                ( '3.0.73' = oldVersion )
              or
                ( '3.0.74' = oldVersion )
              or
                ( '3.0.75' = oldVersion )
              or
                ( '3.0.76' = oldVersion )
              or
                ( '3.0.77' = oldVersion )
              then
                begin
                  // Anything prior to the first public release (3.0.78) should be
                  // considered "buggy" for one reason or another: see _ReleaseNotes.txt.
                  // (Since these versions were never used for analytical purposes, it's not
                  // a problem to treat them as invalid.)
                  result := VERSBug;
                end
              else if
                ( '3.0.78' = oldVersion )
              or
                ( '3.0.79' = oldVersion )
              or
                ( '3.0.80' = oldVersion )
              or
                ( '3.0.81' = oldVersion )
              or
                ( '3.0.82' = oldVersion )
              or
                ( '3.0.83' = oldVersion )
              or
                ( '3.0.84' = oldVersion )
              or
                ( '3.0.85' = oldVersion )
              then
                begin
                  // Released versions 3.0.78 and 3.0.85 introduce some new features,
                  // but don't invalidate anything done in a previous "good" release.
                  // Version 3.1.15 fixes a problem with RTree searches in the core model.
                  // The problem existed but was not detected in versions 3.0.78 through 3.0.84.
                  // Becasuse results from version 3.1.15 may vary from older versions,
                  // version 3.1.15 should be considered a specification change.
                  result := VERSModelSpecChange;
                end
              else if
                ( '3.1.0' = oldVersion )
              or
                ( '3.1.1' = oldVersion )
              or
                ( '3.1.2' = oldVersion )
              or
                ( '3.1.3' = oldVersion )
              or
                ( '3.1.4' = oldVersion )
              or
                ( '3.1.5' = oldVersion )
              or
                ( '3.1.6' = oldVersion )
              or
                ( '3.1.7' = oldVersion )
              or
                ( '3.1.8' = oldVersion )
              or
                ( '3.1.9' = oldVersion )
              or
                ( '3.1.10' = oldVersion )
              or
                ( '3.1.11' = oldVersion )
              or
                ( '3.1.12' = oldVersion )
              or
                ( '3.1.13' = oldVersion )
              or
                ( '3.1.14' = oldVersion )
              or
                ( '3.1.15' = oldVersion ) // First public release of 3.1.x. Fixes bugs related to RTree searches.
              or
                ( '3.1.16' = oldVersion ) // Version 3.1.16 fixes more bugs related to RTree searches.
              then
                begin
                  result := VERSBUG;
                end
              else if
                ( '3.1.17' = oldVersion )
              or
                ( '3.1.18' = oldVersion )
              or
                ( '3.1.19' = oldVersion )
              then
                begin
                  if( bug3_1_19( self ) or bug3_1_25( self ) ) then
                    result := VERSBUG
                  else
                    result := VERSOK
                  ;
                end
              else if
                ( '3.1.20' = oldVersion )
              or
                ( '3.1.21' = oldVersion )
              or
                ( '3.1.22' = oldVersion )
              then
                begin
                  if( bug3_1_22( self ) or bug3_1_25( self ) ) then
                    result := VERSBUG
                  else
                    result := VERSOK
                  ;
                end
              else if
                ( '3.1.23' = oldVersion )
              or
                ( '3.1.24' = oldVersion )
              or
                ( '3.1.25' = oldVersion )
              then
                begin
                  if( bug3_1_25( self ) ) then
                    result := VERSBUG
                  else
                    result := VERSOK
                  ;
                end
              else if
                ( '3.1.26' = oldVersion )
              or
                ( '3.1.27' = oldVersion )
              or
                ( '3.1.28' = oldVersion )
              then
                begin
                  if( zoneBug3_2_18( self ) ) then
                    result := VERSBUG
                  else
                    result := VERSOK
                  ;
                end


              // Make the jump here to 3.2.x
              //----------------------------
              else if
                ( '3.2.0' = oldVersion )
              or
                ( '3.2.1' = oldVersion )
              or
                ( '3.2.2' = oldVersion )
              or
                ( '3.2.3' = oldVersion )
              or
                ( '3.2.4' = oldVersion )
              or
                ( '3.2.5' = oldVersion )
              or
                ( '3.2.6' = oldVersion )
              or
                ( '3.2.7' = oldVersion )
              or
                ( '3.2.8' = oldVersion )
              or
                ( '3.2.9' = oldVersion )
              or
                ( '3.2.10' = oldVersion )
              or
                ( '3.2.11' = oldVersion )
              or
                ( '3.2.12' = oldVersion )
              or
                ( '3.2.13' = oldVersion )
              or
                ( '3.2.14' = oldVersion )
              or
                ( '3.2.15' = oldVersion )
              then
                result := VERSBUG

              else if
                ( '3.2.16' = oldVersion )
              or
                ( '3.2.17' = oldVersion )
              or
                ( '3.2.18' = oldVersion )                
              then
                begin
                  if( zoneBug3_2_18( self ) ) then
                    result := VERSBUG
                  else
                    result := VERSOK
                  ;
                end

              else if
                ( '3.2.19' = oldVersion )
              then
                result := VERSOK

              // Make the jump here to 3.3.x
              //----------------------------
              else if
                ( '3.3.2' = oldVersion )
              then
                result := VERSOK

              // Make the jump here to 3.4.x
              //----------------------------
              else if
                ( '3.4.0' = oldVersion )
              then
                result := VERSOK

              // If oldVersion is anything else, something is screwed up.
              else
                result :=  VERSUnrecognized
              ;
            end
          ;
        end
      ;

      res.free();
    end
  ;


end.
