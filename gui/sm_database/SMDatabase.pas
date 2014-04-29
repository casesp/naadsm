unit SMDatabase;

(*                            
SMDatabase.pas
---------------
Begin: 2005/01/07
Last revision: $Date: 2008/11/25 21:59:18 $ $Author: areeves $
Version: $Revision: 1.104 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

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
    RemoteMessenger
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
    DB_SCHEMA_VERSION = '3.1.18';
    DB_SCHEMA_DATE = '10/16/2008 6:25:03 PM';
    DB_SCHEMA_ID = 1224203103; // Number of seconds from 1970-01-01 00:00:00 to 2008-10-16 18:25:xx
    DB_SCHEMA_INFO_URL = '';

  type TDBCheckResult = (
    DBVersionUnspecified,
    DBVersionCurrent,
    DBVersionUpdated,
    DBVersionObsolete,
    DBVersionUnrecognized
  );


  type TDBSchemaUpdateReason = ( // Listed in order of severity
    DBUpdateUnspecified,
    DBUpdateOK, // This is the current version of the DB schema
    DBUpdateMinorChanges, // The reason for the database schema update has no real effect on the end user
    DBUpdateOutputsAdded, // New outputs are generated, but existing outputs are still valid
    DBUpdateSpecChange   // A fairly substantial change to the model specification occurred
    //DBUpdateOutputsDropped // Some existing outputs are no longer available: this would be a good reason to save a backup copy!
  );


  type TStopReason = (
    ssStartAndStopAtEndOfOutBreak = 0,
    ssStartAndStopAtFirstDetection = 1,
    ssStartAndStopAtSpecificDay = 2,
    ssStartAndStopAtDiseaseEnd = 3,
    ssStopReasonUndefined = 6
  );


  type TSMDatabase = class( TSqlDatabase )
    protected
      _workingDBFileName: string;
      _permanentDBFileName: string;
      _workingDBHasChanged: boolean;
      _dbSaved: boolean;
      _frmMain: TForm;
      _vNumber: string;
      _isReadOnly: boolean;

      _remoteMessageID: integer;
      _remoteQuerySetCounter: integer;
      _remoteQuerySet: string;

      procedure setSchemaVersion();

      function getWorkingDBFileName(): string;
      function getPermanentDBFileName(): string;
      function getWorkingDBHasChanged(): boolean;
      function getDBSaved(): boolean;

      procedure setPermanentDBFileName( const val: string );
      procedure setWorkingDBHasChanged( val: boolean );
      procedure setDBSaved( val: boolean );

      procedure makeDBTables();

      { All *should* return true on success, false on failure. }
      //function processDDLCommands( resFileContents: string ): boolean; // Now a public function, for use with OldDatabaseFns
      procedure setInGeneralDefaults();
      procedure setInControlsGlobalDefaults();
      procedure setupDynBlob();

      procedure deleteUnusedContactModels();

      function saveDailyOutputsForIterations(): integer;

      function setUpdateReason(): TDBSchemaUpdateReason;

      function getDatabaseBoolean( const fieldName: string; const tableName: string ): boolean;
      function getDatabaseCount( const tableName: string ): integer;

      function getProdTypeCount(): integer;
      function getProdTypePairCount(): integer;

      function getSimulationComplete(): boolean;

      function getContainsOutput(): boolean;

      procedure setSimDays( numDays: integer );
      function getSimDays(): integer;

      procedure setSimStopReason( val: TStopReason );
      function getSimStopReason(): TStopReason;

      procedure disconnectCustomOutputTables();
      procedure connectCustomOutputTables();

      procedure disconnectSelectDailyOutputTables();
      procedure connectSelectDailyOutputTables();

      class function setSampleDatabaseLanguage( const fileName: string ): boolean;

    public
      // WARNING: it is up to the interface to check for the existence of the permanent database file.
      constructor create( fileName: string; dbAction: TSqlDBAction; errMsg: PChar = nil; parent: TForm = nil );

      destructor destroy(); override;

			function execute( q: string ): boolean; override;
      procedure remoteExecute( q: string );

      function save( newFileName: string = '' ): boolean;

      // Functions related to database schema
      //--------------------------------------
      function checkVersion( var updateReason: TDBSchemaUpdateReason ): TDBCheckResult;

      function schemaIDOK( const vNumber: string ): boolean;

      function validDateAndNumber( vNumber: string; vDate: TDateTime ): boolean;

      function versionObsolete( vNumber: string ): boolean;

      { Update database to the latest schema. Return true if schema update was required.
        Set updateSuccess if update was attempted but unsuccessful. }
      function updateSchema( var updateSuccess: boolean ): boolean;

      { *Should* return true on success, false on failure. }
      function processDDLCommands( resFileContents: string ): boolean;

      function saveAllDailyOutputs(): boolean;

      // Functions related to model parameters
      //--------------------------------------
      function compareFunctions( fieldName:String; Chart:TChartFunction; var nFunctionsWithSameName: integer ): Integer;

      function removeProductionType( const id: integer ): boolean;
      procedure removeChartFunction( const id: integer );
      procedure removeFunctionPoints( const id: integer );
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
      procedure setStartTime();
      procedure setEndTime();

      procedure initializeCustomOutputTables( list: TObject );
      procedure dropCustomOutputTables();

      procedure initializeSelectDailyOutputTables( sdo: TObject );
      procedure dropSelectDailyOutputTables();

      procedure setRngSeed( const val: integer ); // Used to record the seed used by the RNG for the simulation
      procedure initializeAllOutputRecords(); // Upon sim start
      procedure initializeRemoteDatabase();
      procedure prepareForIteration( currentIt: integer ); // Upon iteration start
      procedure clearExistingFinalHerdStates();
      procedure incrementCompleteIterations();
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
      procedure mergeHerdTables();


      // Function for creating a populated sample database
      //--------------------------------------------------
      class function makeSampleDatabase(
        const fileName: string;
        errCode: pinteger = nil;
        errMsg: pstring = nil
      ): boolean;

      
      // Properties and property-like functions
      //---------------------------------------
      function completedIterations(): integer;
      function containsIncompleteIterations( completedIt: PInteger = nil; currentIt: PInteger = nil ): boolean;
      function lastIteration(): integer; // NOTE: this last iteration may or may not be complete!

      function daysInLongestIteration(): integer;
      function daysInIteration( const it: integer ): integer;

      procedure quickUpdate( indexValue: variant; fieldName: string; newValue: variant ); override;
      function quickInsert( dict: TQStringVariantMap; keyField: string = '' ): string; override;

      property workingDBFileName: string read getWorkingDBFileName;
      property permanentDBFileName: string read getPermanentDBFileName write setPermanentDBFileName;
      property workingDBHasChanged: boolean read getWorkingDBHasChanged write setWorkingDBHasChanged;
      property dbSaved: boolean read getDBSaved write setDBSaved;

      property simulationComplete: boolean read getSimulationComplete;

      property prodTypeCount: integer read getProdTypeCount;
      property prodTypePairCount: integer read getProdTypePairCount;

      property containsOutput: boolean read getContainsOutput;

      property isReadOnly: boolean read _isReadOnly;

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
    CStringList,
    Resources,
    MyDialogs,
    MyStrUtils,
    USStrUtils,
    DebugWindow,
    WindowsUtils,
    NetworkUtils,
    FunctionPointers,
    ProbDensityFunctions,
    ZipFunctions,

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
        ssStartAndStopAtEndOfOutBreak: result := 'outbreakEnd';
        ssStartAndStopAtFirstDetection: result := 'firstDetection';
        ssStartAndStopAtSpecificDay: result := 'specifiedDay';
        ssStartAndStopAtDiseaseEnd: result := 'diseaseEnd';
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
        result := ssStartAndStopAtEndOfOutBreak
      else if( 'firstdetection' = val2 ) then
        result := ssStartAndStopAtFirstDetection
      else if( 'specifiedday' = val2 ) then
        result := ssStartAndStopAtSpecificDay
      else if( 'diseaseend' = val2 ) then
        result := ssStartAndStopAtDiseaseEnd
      else
        begin
          dbmsg( 'Unrecognized stop reason in stopReasonFromString', true );
          result := ssStartAndStopAtEndOfOutBreak;
        end
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Construction/initialization/destruction
// ----------------------------------------------------------------------------
  constructor TSMDatabase.create( fileName: string; dbAction: TSqlDBAction; errMsg: PChar = nil; parent: TForm = nil  );
  	var
    	copied: boolean;
      tmp: string;
  	begin
    	_frmMain := parent;

      _remoteMessageID := 1;
      _remoteQuerySetCounter := 0;
      _remoteQuerySet := '';

      // Network drives are unreliable.
      // Make sure that the working file is saved to a local drive.
      if( isNetworkDrive( currentDir() ) ) then
        tmp := tempFileName( tempDir() )
      else
    	  tmp := tempFileName( currentDir() )
      ;

      if( dbAction = DBCreate ) then
        begin
        	_permanentDBFileName := fileName;
          _workingDBFileName := directory( tmp ) + '$$$' + shortFileName( tmp );
          workingDBHasChanged := true;
          _dbSaved := false;

          // Create the working database, not the permanent one.
          inherited create( DBMSAccess, _workingDBFileName, DBCreate );

          _isReadOnly := false;

          makeDBTables();

        end
      else if( dbAction = DBOpen ) then
      	begin
          // Store some file names
          _permanentDBFileName := fileName;
          _workingDBFileName := directory( tmp ) + '$$$' + shortFileName( tmp );
          workingDBHasChanged := false;
          _dbSaved := true;

          _isReadOnly := fileIsReadOnly( fileName );

        	// Make a temporary copy of the selected database
          copied := windowsCopyFile( _permanentDBFileName, _workingDBFileName );
          if( not copied ) then
          	raise exception.create( 'TSMDatabase.create: Could not copy the selected scenario file.' )
          ;

          // Open the copy
      		inherited create( DBMSAccess, _workingDBFileName, DBOpen );

          if( not _errorOnOpen ) then
            begin
              // Compact the database
              // FIX ME: It would be better to compact the database upon saving, not opening.
              // (This can be done, but it's harder )
              compact( true ); // it's pointless to raise an exception if compact fails.

              if( not(isOpen) ) then open();
            end
          else
            _isOpen := false
          ;

        end
      ;

    end
  ;


	destructor TSMDatabase.destroy();
  	begin
      // FIX ME: for some reason, after an exception occurs, the temporary file just won't go away.
      close();

   		if( 0 <> length( self.workingDBFileName ) ) then
        begin
          dbcout( 'Deleting ' + self.workingDBFileName, DBSMDATABASE );

          setFileReadWrite( self.workingDBFileName );

          if( fileExists( self.workingDBFileName ) ) then
            begin
              if( not deleteFile( self.workingDBFileName ) ) then
                {$IFDEF DEBUG}
                  msgOK( ansiReplaceStr( tr( 'The temporary file xyz could not be deleted.' ), 'xyz', self.workingDBFileName ) )
                {$ENDIF}
              ;
            end
          ;
        end
      ;

      inherited destroy();
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

        // There was no schema 3.1.14, 3.1.15, or 3.1.16

      else if( '3.1.17' = _vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // The last entry here should be the current schema version.
      // This exception ensures that someone doesn't get too carried away with copy/paste
      else if( DBUpdateOK = result ) then
        raise exception.create( 'Looks like you forgot something in TSMDatabase.setUpdateReason()')
      else if( '3.1.18' = _vNumber ) then
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


      // (There will eventually be more to do here...)

      // Finally, restore constraints
      // and update the version numbers, etc.
      //------------------------------------------
      connectCustomOutputTables();
      connectSelectDailyOutputTables();

      if( result ) then
        setSchemaVersion()
      ;
    end
  ;


  function TSMDatabase.schemaIDOK( const vNumber: string ): boolean;
  	var
      row: TSqlRow;
      vID: integer;
  	begin
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
              vID := row.field('VersionID');

              if
                ( ( '3.1.9' = vNumber ) and ( 1176823233 = vID ) )
              or
                ( ( '3.1.10' = vNumber ) and ( 1176836388 = vID ) )
              or
                ( ( '3.1.12' = vNumber ) and ( 1177957221 = vID ) )
              or
                ( ( '3.1.13' = vNumber ) and (  1178121515 = vID ) )
              or
                ( ( '3.1.17' = vNumber ) and ( 1189206366 = vID ) )
              or
                ( ( '3.1.18' = vNumber ) and ( 1224203103 = vID ) )
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
    	// Add more dates and numbers here as necessary.
      // Recall that a database schema version corresponds to the
      // application version in which it was introduced, and that
      // not all new application versions introduce changes to the schema.
      // Consequently, database schema versions are not necessarily sequential.

      // It would be nice if I could figure out how to consistently include the timestamp
      // portion along with the date.

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
        ( schemaIDOK( vNumber ) )
      then // the date and number/ID are valid.
      	result := true
      else // the date and number/ID are not recognized as NAADSM databases.
      	result := false
      ;

      ShortDateFormat := currentShortDateFormat;
    end
  ;


  function TSMDatabase.versionObsolete( vNumber: string ): boolean;
  	begin
    	// Add more numbers here, if it ever becomes necessary.
      // (Hopefully it won't for a long, long time.)

    	if
      	( '3.0.9' = vNumber )
      or
      	( '3.0.13' = vNumber )
      then
      	result := true
      else
      	result := false
      ;
    end
  ;


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
              if( versionObsolete( _vNumber ) ) then
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
  function TSMDatabase.processDDLCommands( resFileContents: string ): boolean;
    var
      str: string;
      startingList: TCStringList;
      ddlList: TCStringList;
      ptr: pchar;
      testStr: string;
      cmd: PChar;
  	begin
      result := true;

      startingList := TCStringList.create();
      startingList.text := resFileContents;

      // Remove SQL comments and empty lines from the starting list
      ptr := startingList.first();

      while ptr <> nil do
        begin
          testStr := trim( ptr );

          if( leftStr( testStr, 2 ) = '--' ) or( length( testStr ) = 0 ) then
            begin
              startingList.remove();
              ptr := startingList.current();
            end
          else
            ptr := startingList.next()
          ;

        end
      ;

      // Explode remaining strings at each semicolon to produce the DDL table and key creation commands
      str := startingList.text;
      ddlList := TCStringList.create( str, ';', true );

      //if( DBSMDATABASE ) then ddlList.debug( true );

      // Execute the commands!
      cmd := ddlList.first();

      while( cmd <> nil ) do
      	begin
        	str := trim( cmd );

          if( length( str ) > 0 ) then
            begin
              if( not( self.execute( str ) ) ) then
                begin
                  result := false;
                  break;
                end
              ;
            end
          ;

          cmd := ddlList.next();
        end
      ;

      // clean up
      startingList.Clear();
      ddlList.Clear();
      freeAndNil( startingList );
      freeAndNil( ddlList );
    end
  ;


  procedure TSMDatabase.setSchemaVersion();
    var
      currentShortDateFormat: string;
      vDict: TQStringVariantMap;
    begin
      currentShortDateFormat := ShortDateFormat;
      ShortDateFormat := 'mm/dd/yyyy';

      vDict := TQStringVariantMap.Create();

      execute( 'DELETE FROM `DBSchemaVersion`' );

      startQuickInsert( 'DBSchemaVersion' );

      vDict['VersionNumber'] := DB_SCHEMA_VERSION;
      vDict['VersionApplication'] := DB_SCHEMA_APPLICATION;
      vDict['VersionDate'] := VarToDateTime( DB_SCHEMA_DATE );
      vDict['VersionInfoURL'] := DB_SCHEMA_INFO_URL;
      vDict['VersionID'] := DB_SCHEMA_ID;

      quickInsert( vDict );

      endQuickInsert();

      ShortDateFormat := currentShortDateFormat;
      vDict.Free();
    end
  ;


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

      // Add additional DDL 'files' here as needed

      // Populate the version table
      setSchemaVersion();

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

      dict['includeContactSpread'] := boolToStr( false );
      dict['includeAirborneSpread'] := boolToStr( false );
      dict['useAirborneExponentialDecay'] := boolToStr( false );
      dict['useWithinHerdPrevalence'] := boolToStr( false );

      dict['costTrackDestruction'] := boolToStr( false );
      dict['costTrackVaccination'] := boolToStr( false );
      dict['costTrackZoneSurveillance'] := boolToStr( false );

      dict['useFixedRandomSeed'] := boolToStr( false );
      dict['randomSeed'] := intToStr( 527 );

      dict['saveAllDailyOutputs'] := boolToStr( false );
      dict['saveDailyOutputsForIterations'] := intToStr( 3 );

      dict['writeDailyStatesFile'] := boolToStr( false );
      dict['dailyStatesFileName'] := DATABASE_NULL_VALUE;

      dict['saveDailyEvents'] := boolToStr( false );
      dict['saveDailyExposures'] := booltoStr( false );

      dict['useCustomOutputs'] := boolToStr( false );

      dict['writeNAADSMapOutput'] := boolToStr( false );
      dict['NAADSMapDirectory'] := DATABASE_NULL_VALUE;

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
      dict['includeDestruction'] := boolToStr( false );
      dict['includeDetection'] 	 := boolToStr( false );
      dict['includeVaccination'] := boolToStr( false );
      dict['includeTracing']     := boolToStr( false );
      dict['includeZones']       := boolToStr( false );

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


  procedure TSMDatabase.deleteUnusedContactModels();
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
        #	inDiseaseSpread.spreadMethodCode AS code,
        #	inProductionTypePair.directContactSpreadID AS ptpDirID,
        #	inProductionTypePair.indirectContactSpreadID AS ptpIndID,
        #	inProductionTypePair.airborneContactSpreadID AS ptpAirID,
        #	inProductionTypePair.sourceProductionTypeID AS ptpSrcID,
        #	inProductionTypePair.destProductionTypeID AS ptpDestID
        FROM  inProductionTypePair
        LEFT JOIN  inDiseaseSpread
          ON inDiseaseSpread.spreadID = inProductionTypePair.directContactSpreadID
          OR inDiseaseSpread.spreadID = inProductionTypePair.indirectContactSpreadID
          OR inDiseaseSpread.spreadID = inProductionTypePair.airborneContactSpreadID
        ORDER BY inDiseaseSpread.spreadID
        );
      //--------------------------------------------
      *)

      dbcout( '*** deleteUnusedContactModels', DBSMDATABASE );

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
      deleteUnusedContactModels();

      if( 0 = length( newFileName ) ) then
        newFileName := permanentDBFileName
      ;

    	if( ( 0 = length( workingDBFileName )  ) or ( 0 = length( newFileName ) ) ) then
      	begin
       		raise exception.create( 'Missing file name in SMDatabase.save()' );
          result := false;
          exit;
        end
      ;

      dbcout( 'TSMDatabase.save()', DBSMDATABASE );

      if( windowsCopyFile( self.workingDBFileName, newFileName ) ) then
        begin
          self.dbSaved := true;
          self.workingDBHasChanged := false;
          self.permanentDBFileName := newFileName;

          if( _isReadOnly ) then // The database must be closed and reopened to allow read/write privileges
            begin
              close();

              setFileReadWrite( newFileName );
              setFileReadWrite( self.workingDBFileName );
              _isReadOnly := false;

              open();
            end
          ;

          result := true;
        end
      else
        begin
          self.workingDBHasChanged := true;
          result := false;
        end
      ;
 		end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// PDF/REL function handling
// ----------------------------------------------------------------------------
  function TSMDatabase.compareFunctions( fieldName: String; Chart: TChartFunction; var nFunctionsWithSameName: integer ): Integer;
    var
      ret_val:Integer;
    	q: string;
      db2: TSqlDatabase;
      res, res2: TSqlResult;
      row: TSqlRow;
      _chart: TChartFunction;
      I, Count, chartID:Integer;
      chartName: string;

      // FIX ME: should this go into MyStrUtils?  See also ChartFunction.compare()
      function stripParens( str: string ): string;
        var
          strLen: integer;
          parenPos: integer;
        begin
          result := trim( str );

          strLen := length( str );
          if( ')' = charAt( result, strLen - 1 ) ) then
            begin
              parenPos := lastDelimiter( '(', result );
              if( 0 < parenPos ) then
                result := trim( leftStr( result, parenPos - 1 ) )
              ;
            end
          ;
        end
      ;
    begin
      ret_val := -1;

    	q := 'SELECT '
        + 'inChart.chartID, inChart.fieldName '
        //+ 'FROM inChart WHERE LEFT( inChart.fieldName, ' + intToStr( length( fieldName ) ) + ') = ' + SQLQuote( fieldName )
        + 'FROM inChart WHERE inChart.fieldName = ' + SQLQuote( fieldName )
      ;

      db2 := self as TSqlDatabase;
      res := TSqlResult.create( q, db2 );

      Count := res.numRows;

      if ( Count > 0 ) then
        begin
          // Are there other charts with a similar name?  If so, then the
          // name suffix needs to be changed (e.g. from "Distance distribution" to "Distance distribution (1)"
          chartName := stripParens( chart.name );
          res2 := TSqlResult.create(
            'SELECT inChart.chartID FROM inChart WHERE LEFT( inChart.chartName, ' + intToStr( length( chartName ) ) + ' ) = ' + sqlQuote( chartName ),
            db2
          );

          nFunctionsWithSameName := res2.numRows;
          res2.free();

          row := res.fetchArrayFirst();

          for I := 1 to Count do
            begin
              if ( row.field( 'chartID' ) ) then
                begin
                  chartID :=  StrToInt( row.field( 'chartID' ) );
                  _chart := functionFromDB( self, chartID );

                  if ( Chart.compare( _chart ) ) then
                    begin
                      ret_val := chartID;
                      _chart.destroy();
                      break;
                    end;
                  _chart.destroy();
                end;

              row := res.fetchArrayNext();
            end
          ;
        end
      ;

      freeAndNil( res );

      result := ret_val;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Database modification
// ----------------------------------------------------------------------------
	function TSMDatabase.execute( q: string ): boolean;
		begin
      // If the file/database is read-only, don't even attempt to make changes.
      
      if( _isReadOnly ) then
        result := false
      else
        begin
          workingDBHasChanged := true;
          result := inherited execute( q );
        end
      ;
		end
	;


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


  procedure TSMDatabase.quickUpdate( indexValue: variant; fieldName: string; newValue: variant );
    begin
      workingDBHasChanged := true;
      inherited quickUpdate( indexValue, fieldName, newValue );
    end
  ;


  function TSMDatabase.quickInsert( dict: TQStringVariantMap; keyField: string = '' ): string;
    begin
      workingDBHasChanged := true;
      result := inherited quickInsert( dict, keyField );
    end
  ;


  procedure TSMDatabase.setStartTime();
  	var
    	q: string;
      begin
      {$IF Defined( CHEYENNE ) }
        q := 'UPDATE `outGeneral` SET `simulationStartTime` = NOW(), `version` = "' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Cheyenne"';
      {$ELSEIF Defined( LARAMIE ) }
        q := 'UPDATE `outGeneral` SET `simulationStartTime` = NOW(), `version` = "' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-Laramie"';
      {$ELSE}
        q := 'UPDATE `outGeneral` SET `simulationStartTime` = NOW(), `version` = "' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '"';
      {$IFEND}
    	execute( q );

      // Set up the remote database, if necessary
      //-----------------------------------------
      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := 'INSERT INTO `outGeneral`'
            + ' ( `jobID`, `outGeneralID`, `simulationStartTime`, `simulationEndTime`, `completedIterations`, `version` )'
            + ' VALUES ( '
            + intToStr( remoteDBParams.jobID )
            + ', ''NAADSMXXXX'', NOW(), NULL, 0, ''' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ''' )'
          ;
          remoteExecute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.setEndTime();
  	var
    	q: string;
  	begin
      q := 'UPDATE `outGeneral` SET `simulationEndTime` = NOW()';
    	execute( q );

      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := q + ' WHERE `jobID` = ' + intToStr( remoteDBParams.jobID );
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
      qDict['outbreakEnded'] := boolToStr( outbreakEnded );
      if( outbreakEnded ) then
        qDict['outbreakEndDay'] := intToStr( outbreakEndDay )
      else
        qDict['outbreakEndDay'] := DATABASE_NULL_VALUE
      ;

      // if -1 = _diseaseEndDay, the active disease phase of the outbreak
      // did not end before time ran out.
      qDict['diseaseEnded'] := boolToStr( diseaseEnded );
      if( diseaseEnded ) then
        qDict['diseaseEndDay'] := intToStr( diseaseEndDay )
      else
        qDict['diseaseEndDay'] := DATABASE_NULL_VALUE
      ;

      qDict['zoneFociCreated'] := boolToStr( zoneFociCreated );

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
                + ' `infnUDir` + `infnUInd` + `infnUAir`,'
                + ' `infnADir` + `infnAInd` + `infnAAir`,'
                + ' `detnUClin`,'
                + ' `detnAClin`,'
                + ' `tsdUSubc` + `tsdUClin` AS `infectiousUnits`,'
                + ' `appUInfectious`'
              + ' FROM `outDailyByProductionType`'
              + ' WHERE '
                + ' `iteration` = ' + intToStr( it )
              //+ ' AND '
              //+ ' day > 1'
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
                + ' `infnUDir` + `infnUInd` + `infnUAir`,'
                + ' `infnADir` + `infnAInd` + `infnAAir`,'
                + ' `detnUClin`,'
                + ' `detnAClin`,'
                + ' `tsdUSubc` + `tsdUClin` AS `infectiousUnits`,'
                + ' `appUInfectious`'
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
            + '   `appUInfectious`,'
            + '   `detnUClin`,'
            + '   `detnAClin`'
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
              qDict['detectedUnits'] := string( row.field('detnUClin') );
              qDict['detectedAnimals'] := string( row.field('detnAClin') );
              qDict['infectiousUnits'] := string( row.field('infectiousUnits') );
              qDict['apparentInfectiousUnits'] := string( row.field('appUInfectious') );
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
      // Table outCustIteration currently has no constraints to worry about.

      // Drop foreign key constraint from table outCustIterationByProductionType
      if( tableExists( 'outCustIterationByProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByProductionType` DROP CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`';
          self.execute( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZone
      if( tableExists( 'outCustIterationByZone' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByZone` DROP CONSTRAINT `inZone_outCustIterationByZone_FK1`';
          self.execute( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZoneAndProductionType
      if( tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` DROP CONSTRAINT `inZone_outCustIterationByZoneAndProductionType_FK1`';
          self.execute( q );
          q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` DROP CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`';
          self.execute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.connectCustomOutputTables();
    var
      q: string;
    begin
      // Table outCustIteration currently has no constraints to worry about.

      // Add foreign key constraint to table outCustIterationByProductionType
      if( tableExists( 'outCustIterationByProductionType' ) ) then
        begin
           q := 'ALTER TABLE `outCustIterationByProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           self.execute( q );
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
           self.execute( q );
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
           self.execute( q );

           q := 'ALTER TABLE `outCustIterationByZoneAndProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           self.execute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.disconnectSelectDailyOutputTables();
    var
      q: string;
    begin
      // Drop foreign key constraint from table outSelectDailyByProductionType
      if( tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outSelectDailyByProductionType` DROP CONSTRAINT `inProductionType_outSelectDailyByProductionType_FK1`';
          self.execute( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZoneAndProductionType
      if( tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType` DROP CONSTRAINT `inZone_outSelectDailyByZoneAndProductionType_FK1`';
          self.execute( q );
          q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType` DROP CONSTRAINT `inProductionType_outSelectDailyByZoneAndProductionType_FK1`';
          self.execute( q );
        end
      ;
    end
  ;


  procedure TSMDatabase.connectSelectDailyOutputTables();
    var
      q: string;
    begin
      // Add foreign key constraint to table outSelectDailyByProductionType
      if( tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
           q := 'ALTER TABLE `outSelectDailyByProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outSelectDailyByProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           self.execute( q );
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
           self.execute( q );

           q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType`'
            + ' ADD CONSTRAINT `inProductionType_outSelectDailyByZoneAndProductionType_FK1`'
            + ' FOREIGN KEY (`productionTypeID`) '
            + ' REFERENCES `inProductionType` (`productionTypeID`)'
           ;
           self.execute( q );
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
                      row.debug();
                      row := res.fetchArrayNext();
                    end
                  ;

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
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      row.debug();
                      row := res.fetchArrayNext();
                    end
                  ;

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
                      row.debug();
                      row := res.fetchArrayNext();
                    end
                  ;

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

    	// Clear status times and other general properties
      //------------------------------------------------
      q := 'UPDATE `outGeneral` SET'
      	+ ' `simulationStartTime` = NULL,'
        + ' `simulationEndTime` = NULL,'
        + ' `completedIterations` = 0,'
        + ' `version` = NULL'
      ;
			execute( q );

      q := 'UPDATE `dynHerd` SET '
        + ' `finalStateCode` = NULL,'
        + ' `finalApparentStateCode` = NULL,'
        + ' `cumInfected` = 0,'
        + ' `cumDetected` = 0,'
        + ' `cumDestroyed` = 0,'
        + ' `cumVaccinated` = 0'
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
        + ' `finalApparentStateCode` = "U"'
      ;
      dbcout( q, DBSMDATABASE );
      execute( q );
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
      execute( q );
    end;

	procedure TSMDatabase.removeChartFunction( const id: integer );
  	var
    	q: string;
  	begin
      // Update any/all fields that might use this chart.
      //-------------------------------------------------

      // Table inDiseaseSpread
      q := 'UPDATE `inDiseaseSpread` SET `movementControlRelID` = NULL WHERE `movementControlRelID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inDiseaseSpread` SET `distancePdfID` = NULL WHERE `distancePdfID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inDiseaseSpread` SET `transportDelayPdfID` = NULL WHERE `transportDelayPdfID` = ' + intToStr( id );
      execute( q );

      // Table inProductionType
      q := 'UPDATE `inProductionType` SET `disLatentPeriodPdfID` = NULL WHERE `disLatentPeriodPdfID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inProductionType` SET `disSubclinicalPeriodPdfID` = NULL WHERE `disSubclinicalPeriodPdfID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inProductionType` SET `disClinicalPeriodPdfID` = NULL WHERE `disClinicalPeriodPdfID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inProductionType` SET `disImmunePeriodPdfID` = NULL WHERE `disImmunePeriodPdfID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inProductionType` SET `detProbObsVsTimeClinicalRelID` = NULL WHERE `detProbObsVsTimeClinicalRelID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inProductionType` SET `detProbReportVsFirstDetectionRelID` = NULL WHERE `detProbReportVsFirstDetectionRelID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inProductionType` SET `vaccImmunePeriodPdfID` = NULL WHERE `vaccImmunePeriodPdfID` = ' + intToStr( id );
      execute( q );

      // Table inControlsGlobal
      q := 'UPDATE `inControlsGlobal` SET `destrCapacityRelID` = NULL WHERE `destrCapacityRelID` = ' + intToStr( id );
      execute( q );
      q := 'UPDATE `inControlsGlobal` SET `vaccCapacityRelID` = NULL WHERE `vaccCapacityRelID` = ' + intToStr( id );
      execute( q );

      // Remove the chart points
      removeFunctionPoints( id );

      // Finally, delete the chart itself
   		q := 'DELETE FROM `inChart` WHERE `chartID` = ' + intToStr( id );
      execute( q );
    end
  ;


  procedure TSMDatabase.removeFunctionPoints( const id: integer );
    var
    	q: string;
  	begin
   		q := 'DELETE FROM `inChartDetail` WHERE `chartID` = ' + intToStr( id );
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
            + boolToStr( simulateTransition )
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
            + boolToStr( simulateTransition )
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
        + ' `daysLeftInInitialState` INTEGER,'
        + ' `initialSize` INTEGER'
        + ' )'
      ;

      self.execute( q );
    end
  ;


  procedure TSMDatabase.mergeHerdTables();
    var
      q: string;
    begin
      q := 'INSERT INTO  `dynHerd` (`herdID`, `productionTypeID`, `latitude`, `longitude`, `initialStateCode`, `daysLeftInInitialState`, `initialSize`)'
        + ' SELECT `dynHerd2`.`herdID`,'
        + ' `dynHerd2`.`productionTypeID`,'
        + ' `dynHerd2`.`latitude`,'
        + ' `dynHerd2`.`longitude`,'
        + ' `dynHerd2`.`initialStateCode`,'
        + ' `dynHerd2`.`daysLeftInInitialState`,'
        + ' `dynHerd2`.`initialSize`'
        + ' FROM `dynHerd2`'
      ;

      self.execute( q );

      self.execute( 'DROP TABLE dynHerd2' );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Functions for handling BLOBs
// ----------------------------------------------------------------------------
  procedure TSMDatabase.clearZonePerimeters();
    begin
      self.execute( 'UPDATE `dynBlob` SET `zonePerimeters` = NULL' );
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
      else if( unassigned = row.field('zonePerimeters') ) then
        result := false
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

          // Rename production types
          //------------------------
          query := 'SELECT productionTypeID, descr FROM inProductionType';
          res.runQuery( query );
          if( res.success ) then
            begin
              row := res.fetchArrayFirst();
              while( nil <> row ) do
                begin
                  name := row.field(1);
                  id := row.field(0);
                  updateStatement :=
                    'UPDATE inProductionType SET descr = ' + db.sqlQuote( tr( name ) )
                      + ' WHERE productionTypeID = ' + intToStr( id )
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
              query := 'SELECT zoneID, descr FROM inZone';
              res.runQuery( query );
              if( res.success ) then
                begin
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      name := row.field(1);
                      id := row.field(0);
                      updateStatement :=
                        'UPDATE inZone SET descr = ' + db.sqlQuote( tr( name ) )
                          + ' WHERE zoneID = ' + intToStr( id )
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
              query := 'SELECT chartID, chartName FROM inChart';
              res.runQuery( query );
              if( res.success ) then
                begin
                  row := res.fetchArrayFirst();
                  while( nil <> row ) do
                    begin
                      name := row.field(1);
                      id := row.field(0);
                      updateStatement :=
                        'UPDATE inChart SET chartName = ' + db.sqlQuote( tr( name ) )
                          + ' WHERE chartID = ' + intToStr( id )
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
              updateStatement := 'UPDATE inGeneral SET scenarioDescr = '
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
  function TSMDatabase.getWorkingDBFileName(): string; begin result := _workingDBFileName end;
  function TSMDatabase.getPermanentDBFileName(): string; begin result := _permanentDBFileName end;
  function TSMDatabase.getWorkingDBHasChanged(): boolean; begin result := _workingDBHasChanged end;
  function TSMDatabase.getDBSaved(): boolean; begin result := _dbSaved end;
  procedure TSMDatabase.setDBSaved( val: boolean ); begin _dbSaved := val; end;


  procedure TSMDatabase.setWorkingDBHasChanged( val: boolean );
    {$IFNDEF CONSOLEAPP}
  	var
    	frm: TFormMain;
    {$ENDIF}
    begin
    	_workingDBHasChanged := val;

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


  procedure TSMDatabase.setPermanentDBFileName( const val: string ); begin _permanentDBFileName := val; end;


  function TSMDatabase.getDatabaseBoolean( const fieldName: string; const tableName: string ): boolean;
  	var
      row: TSqlRow;
  	begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT ' + fieldName + ' FROM ' + tableName );

      row := _sqlResult.fetchArrayFirst();
      if( true = row.field(0) ) then
      	result := true
      else
      	result := false
      ;
    end
  ;

  
  function TSMDatabase.getDatabaseCount( const tableName: string ): integer;
  	var
      row: TSqlRow;
  	begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT COUNT(*) AS val FROM ' + tableName );

      row := _sqlResult.fetchArrayFirst();

      result := row.field('val');
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


  function TSMDatabase.completedIterations(): integer;
    var
      row: TSqlRow;
    begin
      if( nil = _sqlResult ) then
        createSqlResult()
      ;

      _sqlResult.runQuery( 'SELECT completedIterations FROM outGeneral' );
      row := _sqlResult.fetchArrayFirst();

      if( nil = row ) then
        result := 0
      else if( null = row.field('completedIterations') ) then
        result := 0
      else
        result := row.field('completedIterations')
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


end.
