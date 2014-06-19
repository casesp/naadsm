unit OldDatabaseFns;

(*
OldDatabaseFns.pas
------------------------------
Begin: 2007/02/08
Last revision: $Date: 2013-06-27 19:11:23 $ $Author: areeves $
Version number: $Revision: 1.27.4.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2007 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


(*
  Several functions in SMDatabase were becoming unwieldy as the database schema was repeatedly updated.
  These very old updates are now handled by this unit, which keeps SMDatabase more readable.
*)
interface
	
	uses
    ModelDatabase,
		SMDatabase
	;

  function handleVeryOldDatabaseSchemas( db: TSMDatabase; var vNumber: string; var updateSuccess: boolean ): boolean;

  function checkVeryOldUpdateReasons( const vNumber: string ): TDBSchemaUpdateReason;

  function veryOldVersionIsValid( const vNumber: string; const vDate: TDateTime ): boolean;

  procedure makeVeryOldTables( db: TSMDatabase );

  { Return true on success, false on failure }
  function populateSelectDailyOutputsFor3_2_0( db: TSMDatabase ): boolean;
  function populateDefaultInputsFor3_2_0( db: TSMDatabase ): boolean;
  function updateEventAndControlStateCodesFor3_2_0( db: TSMDatabase ): boolean;
  function projectZoneCoordinatesFor3_2_0( db: TSMDatabase ): boolean;
  function populateSelectDailyOutputsFor3_2_11( db: TSMDatabase ): boolean;

  function populateSelectDailyOutputsFor3_1_18( db: TSMDatabase ): boolean;

  function populateSelectDailyOutputsFor3_1_17( db: TSMDatabase ): boolean;
  function adjustCustomOutputDefinitionsFor3_1_17( db: TSMDatabase ): boolean;

  function updateInGeneralFor3_1_9( db: TSMDatabase ): boolean;

  function adjustZoneParametersFor3_1_1( db: TSMDatabase ): boolean;

  function updateInGeneralFor3_1_0( db: TSMDatabase ): boolean;

implementation

  uses
    Windows,
    Variants,
    SysUtils,
    TypInfo,
    StrUtils,
    Classes,

    Resources,
    SqlClasses,
    MyStrUtils,
    DebugWindow,
    Points,
    FunctionPointers,
    BasicGIS,

    Proj4,

    ChartFunction,
    ProbDensityFunctions,

    FunctionEnums,
    OutputDescriptions,
    ZonePerimeter,
    Herd
  ;


  function adjustFixedContactRateParameters( db: TSMDatabase ): boolean;
    var
      row: TSqlRow;
      useFixedPoissonValue: boolean;
      fixedPoissonValue: double;

      fixedRate: integer;
      meanRate: double;

      dllHandle: THandle;
      fixed_contacts_old: TCFnDouble_2_Double_Double;

      db2: TSqlDatabase;
      sqlResult: TSqlResult;

      execSuccess: boolean;
  	begin
      execSuccess := true;

      dbcout( 'adjustFixedContactRateParameters...', DBSMDATABASE );

    	db2 := db as TSqlDatabase;
      sqlResult := TSqlResult.create( db2 );

      dllHandle := loadLibrary( 'libaphi.dll' );
      fixed_contacts_old := GetProcAddress( dllHandle, 'fixed_contacts_old' );

      sqlResult.runQuery( 'SELECT `useFixedPoissonValue`, `poissonValue` FROM `inGeneral`' );

      if( not( sqlResult.success ) ) then
        execSuccess := false
      ;

      row := sqlResult.fetchArrayFirst();

      if
        ( null = row.field('useFixedPoissonValue') )
      or
        ( null = row.field('poissonValue') )
      then
        begin
          sqlResult.free();
          result := execSuccess;
          exit;
        end
      ;

      useFixedPoissonValue := row.field('useFixedPoissonValue');

      if( not( useFixedPoissonValue ) ) then
        begin
          sqlResult.free();
          result := execSuccess;
          exit;
        end
      ;

      fixedPoissonValue := row.field('poissonValue');

      dbcout( 'fixedPoissonValue: ' + usFloatToStr( fixedPoissonValue ), DBSMDATABASE );

      sqlResult.runQuery( 'SELECT `spreadID`, `meanContactRate` FROM `inDiseaseSpread` WHERE `spreadMethodCode` = "D" OR `spreadMethodCode` = "I"' );

      if( not( sqlResult.success ) ) then
        execSuccess := false
      ;

      //dbcout( 'Matching rows: ' + intToStr( sqlResult.numRows ), DBSMDATABASE );

      row := sqlResult.fetchArrayFirst();

      while( nil <> row ) do
        begin
          if( null <> row.field('meanContactRate') ) then
            begin
              meanRate := row.field('meanContactRate');
              //dbcout( 'meanRate: ' + usFloatToStr( meanRate ), DBSMDATABASE );

              fixedRate := round( fixed_contacts_old( fixedPoissonValue, meanRate ) );
              //dbcout( 'fixedRate: ' + intToStr( fixedRate ), DBSMDATABASE );

              if( not(
                db.execute(
                  'UPDATE `inDiseaseSpread` SET'
                  + ' `useFixedContactRate` = -1,'
                  + ' `fixedContactRate` = ' + intToStr( fixedRate )
                  + ' WHERE `spreadID` = ' + intToStr( row.field('spreadID') )
                 )
              )) then
                execSuccess := false
              ;
            end
          ;

          row := sqlResult.fetchArrayNext();
        end
      ;

      FreeLibrary( dllHandle );

      dbcout( 'Done with adjustFixedContactRateParameters', DBSMDATABASE );

      sqlResult.free();

      result := execSuccess;
    end
  ;


   // NOTE: this function uses some obsolete field names (survDirect, survIndirect, and includeSurveillance).
  // The version of the database schema that this function updates still uses these obsolete names,
  // so they should not be changed here.
  function adjustTracingParameters( db: TSMDatabase ): boolean;
    var
      row: TSqlRow;
      q: string;
      q2: string;

      db2: TSqlDatabase;
      sqlResult: TSqlResult;

      execSuccess: boolean;
    begin
      dbcout( 'adjustTracingParameters...', DBSMDATABASE );

      execSuccess := true;

    	db2 := db as TSqlDatabase;
      sqlResult := TSqlResult.create( db2 );

      q := 'SELECT'
        + ' productionTypeID,'
        + ' survDirect,'
        + ' survIndirect'
        + ' FROM'
        + ' inProductionType'
      ;

      dbcout( q, DBSMDATABASE );

      sqlResult.runQuery( q );

      if( not( sqlResult.success ) ) then
        execSuccess := false
      ;

      row := sqlResult.fetchArrayFirst();
      while( nil <> row ) do
        begin
          //row.debug();

          q2 := 'UPDATE `inProductionType` SET'
            + ' `destrDirectTraces` = ' + usBoolToText( row.field( 'survDirect' ) ) + ','
            + ' `destrIndirectTraces` = ' + usBoolToText( row.field( 'survIndirect' ) )
            + ' WHERE `productionTypeID` = ' + intToStr( row.field('productionTypeID') )
          ;

          dbcout( q2, DBSMDATABASE );

          if( not ( db.execute( q2 ) ) ) then
            execSuccess := false
          ;

          row := sqlResult.fetchArrayNext();
        end
      ;

      q2 := 'UPDATE `inControlsGlobal` SET `includeSurveillance` = true';

      if( not( db.execute( q2 ) ) ) then
        execSuccess := false
      ;

      result := execSuccess;
      sqlResult.Free();
      dbcout( 'Done with adjustTracingParameters', DBSMDATABASE );
    end
  ;


  function handleVeryOldDatabaseSchemas( db: TSMDatabase; var vNumber: string; var updateSuccess: boolean ): boolean;
    var
      execSuccess: boolean;
  	begin
      result := false;
      updateSuccess := true;

      // Update 3.0.50 to 3.0.51
      //------------------------
			if( '3.0.50' = vNumber ) then
      	begin
          try
            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_51' ) )
            and
              // Create initial record in outGeneral
              db.execute(
                'INSERT INTO `outGeneral` ( `outGeneralID`, `simulationStartTime`, `simulationEndTime`, `completedIterations` ) '
                  + ' VALUES ( ' + db.sqlQuote( DB_SCHEMA_APPLICATION ) + ', NULL, NULL, 0 ) '
              )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.51';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.51 to 3.0.52
      //-------------------------
      if( '3.0.51' = vNumber ) then
      	begin
          try
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_52' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.52';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.52 to 3.0.54
      //------------------------
      if( '3.0.52' = vNumber ) then
      	begin
          try
            // Output field names were changed in table outDailyByProdType
            // New output fields were added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_54' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.54';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.54 to 3.0.55
      //------------------------
      if( '3.0.54' = vNumber ) then
      	begin
          try
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_55' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.55';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.55 to 3.0.56
      //------------------------
      if( '3.0.55' = vNumber ) then
      	begin
          try
            // New output fields were added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_56' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.56';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.56 to 3.0.57
      //------------------------
      if( '3.0.56' = vNumber ) then
      	begin
          try
            // New output fields were added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_57' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.57';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.57 to 3.0.58
      //------------------------
      if( '3.0.57' = vNumber ) then
        begin
          try
            // New output fields were added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_58' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.58';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.58 to 3.0.59
      //------------------------
      if( '3.0.58' = vNumber ) then
        begin
          try
            // New output fields were added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_59' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.59';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.59 to 3.0.60
      //-------------------------
      if( '3.0.59' = vNumber ) then
        begin
          try
            // New output fields were added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_60' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.60';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.60 to 3.0.61
      //-------------------------
      if( '3.0.60' = vNumber ) then
        begin
          try
            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_61' ) )
            and
              // New fields were added for random seed, fixed Poisson distribution, and optional outputs
              // (Fields for fixed Poisson distribution were subsequently dropped.)
              db.execute(
                'UPDATE `inGeneral` SET '
                + ' `useFixedRandomSeed` = 0,'
                + ' `randomSeed` = 527,'
                //+ ' `useFixedPoissonValue` = 0, ' // See comment above
                //+ ' `poissonValue` = 0.5, ' // See comment above
                + ' `saveAllDailyOutputs` = 0, '
                + ' `saveDailyOutputsForIterations` = 3, '
                + ' `writeDailyStatesFile` = 0 '
              )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            // A new field was added to DBSchemaVersion (the table was recreated)
            // It will be populated below

            vNumber := '3.0.61';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.61 to 3.0.62
      //------------------------
      if( '3.0.61' = vNumber ) then
        begin
          try
            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_62' ) )
            and
              // New fields were added for events and exposures
              db.execute(
                'UPDATE `inGeneral` SET '
                + ' `saveDailyEvents` = 0, '
                + ' `saveDailyExposures` = 0, '
                + ' `simStopReason` = "specifiedDay"'
              )
            and
              // Field for days left in status added to dynHerd
              db.execute( 'UPDATE `dynHerd` SET `daysLeftInInitialState` = -1' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.62';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.62 to 3.0.63
      //------------------------
      if( '3.0.62' = vNumber ) then
        begin
          try
            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_63' ) )
            and
              // Field sizes were altered for inGeneral.scenarioDescr and inGeneral.simStopReason
              // Event code for new infections was changed from N to F
              db.execute( 'UPDATE `outDailyEvents` SET `eventCode` = "F" WHERE eventCode = "N"' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.63';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.63 to 3.0.65
      // (There was no 3.0.64)
      //------------------------
      if( '3.0.63' = vNumber ) then
        begin
          try
            // Fields were added for new PDF types
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_65' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.65';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.65 to 3.0.66
      //------------------------
      if( '3.0.65' = vNumber ) then
        begin
          try
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_66' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.66';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.66 to 3.0.68
      // (There was no 3.0.67)
      //------------------------
      if( '3.0.66' = vNumber ) then
        begin
          try
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_68' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.68';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.68 to 3.0.69
      //------------------------
      if( '3.0.68' = vNumber ) then
        begin
          try
            // Fixed contact rate mechanism was changed.
            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_69_part1' ) )
            and
              adjustFixedContactRateParameters( db )
            and
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_69_part2' ) )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.69';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.69 to 3.0.70
      //-------------------------
      if( '3.0.69' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.69', DBSMDATABASE );

            // Ring destruction was changed
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_70' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.70';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.70 to 3.0.72
      //-------------------------
      if( '3.0.70' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.70', DBSMDATABASE );

            // Ring destruction was changed
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_72' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.72';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.72 to 3.0.73
      //-------------------------
      if( '3.0.72' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.72', DBSMDATABASE );

            // Tracing was introduced
            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_73' ) )
            and
              adjustTracingParameters( db )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.73';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.73 to 3.0.76
      //-------------------------
      if( '3.0.73' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.73', DBSMDATABASE );
            // Read-only tables created
            // exposing and exposed herds are now recorded for daily exposures
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_76' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.76';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.76 to 3.0.78
      //------------------------
      if( '3.0.76' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.76', DBSMDATABASE );
            // New outputs for herds destroyed before the simulation begins
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_78' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.78';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.78 to 3.0.79
      //------------------------
      if( '3.0.78' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.78', DBSMDATABASE );

            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_0_79' ) )
            and
              // New field was added for airborne exponential decay
              db.execute( 'UPDATE `inGeneral` SET `useAirborneExponentialDecay` = FALSE' )
            and
              // field name used in inChart was changed from 'DetProbReportVsDaysInfectious'
              // to 'DetProbObsVsTimeClinical'
              db.execute( 'UPDATE `inChart` SET `fieldName` = "DetProbObsVsTimeClinical" WHERE `fieldName` = "DetProbReportVsDaysInfectious"' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.79';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.79 to 3.0.82
      //------------------------
      if( '3.0.79' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.79', DBSMDATABASE );
            // New outputs for end of active disease were added to table outIteration
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_82' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.82';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.82 to 3.0.83
      //------------------------
      if( '3.0.82' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.82', DBSMDATABASE );
            // New outputs for end of active disease were added to table outIteration
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_83' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.83';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.83 to 3.0.85
      // (There was no 3.0.84)
      //------------------------
      if( '3.0.83' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.83', true );
            // Capability for custom outputs was added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_85' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.85';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;
    end
  ;


  function checkVeryOldUpdateReasons( const vNumber: string ): TDBSchemaUpdateReason;
    begin
      result := DBUpdateUnspecified;

      if( '3.0.52' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end
      else if( '3.0.59' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end
      else if( '3.0.60' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end
      else if( '3.0.61' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end
      else if( '3.0.62' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.0.63' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end

      // There was no DB Schema 3.0.64.

      else if( '3.0.65' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.0.66' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There was no DB Schema 3.0.67.

      else if( '3.0.68' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.0.69' = vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateSpecChange;
        end
      else if( '3.0.70' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There was no DB schema 3.0.71

      else if( '3.0.72' = vNumber ) then
        begin
          if( DBUpdateSpecChange > result ) then result := DBUpdateSpecChange;
        end
      else if( '3.0.73' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There were no schemas 3.0.74 or 3.0.75

      else if( '3.0.76' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There was no schema 3.0.77

      else if( '3.0.78' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.0.79' = vNumber ) then
        begin
          if( DBUpdateOutputsAdded > result ) then result := DBUpdateOutputsAdded;
        end

      // There were no schemas 3.0.80 or 3.0.81

      else if( '3.0.82' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      else if( '3.0.83' = vNumber ) then
        begin
          if( DBUpdateMinorChanges > result ) then result := DBUpdateMinorChanges;
        end
      ;
    end
  ;


  function veryOldVersionIsValid( const vNumber: string; const vDate: TDateTime ): boolean;
    begin
    	if
      	( ( '3.0.9' = vNumber ) and ( VarToDateTime( '7/18/2004 12:00:00 AM' ) = vDate ) )
      or
      	( ( '3.0.13' = vNumber ) and ( VarToDateTime( '1/12/2005 4:00:00 PM' ) = vDate ) )
      or
      	( ( '3.0.50' = vNumber ) and ( VarToDateTime( '6/1/2005 12:00:00 AM' ) = vDate ) )
      or
      	( ( '3.0.51' = vNumber ) and ( VarToDateTime( '6/18/2005 1:00:00 PM' ) = vDate ) )
      or
      	( ( '3.0.52' = vNumber ) and ( VarToDateTime( '6/30/2005 5:00:00 PM' ) = vDate ) )
      or
      	( ( '3.0.54' = vNumber ) and ( VarToDateTime( '7/5/2005 2:32:00 PM' ) = vDate ) )
      or
      	( ( '3.0.55' = vNumber ) and ( VarToDateTime( '7/5/2005 4:39:00 PM' ) = vDate ) )
      or
      	( ( '3.0.56' = vNumber ) and ( VarToDateTime( '7/6/2005 1:03:00 AM' ) = vDate ) )
      or
        ( ( '3.0.57' = vNumber ) and ( VarToDateTime( '7/6/2005 10:52:00 PM' ) = vDate ) )
      or
        ( ( '3.0.58' = vNumber ) and ( VarToDateTime( '7/7/2005 5:20:00 PM' ) = vDate ) )
      or
        ( ( '3.0.59' = vNumber ) and ( VarToDateTime( '7/12/2005 1:42:00 PM' ) = vDate ) )
      or
        ( ( '3.0.60' = vNumber ) and ( VarToDateTime( '7/15/2005 6:39:00 PM' ) = vDate ) )
      or
        ( ( '3.0.61' = vNumber ) and ( VarToDateTime( '8/15/2005 12:48:00 PM' ) = vDate ) )
      or
        ( ( '3.0.62' = vNumber ) and ( VarToDateTime( '8/22/2005 3:14:00 PM' ) = vDate ) )
      or
        ( ( '3.0.63' = vNumber ) and ( VarToDateTime( '9/16/2005 9:11:00 PM' ) = vDate ) )
      or
        ( ('3.0.65' = vNumber ) and ( varToDateTime( '11/1/2005 12:00:00 PM' ) = vDate ) )
      or
        ( ('3.0.66' = vNumber ) and ( varToDateTime( '11/2/2005 9:08:00 AM' ) = vDate ) )
      or
        ( ('3.0.68' = vNumber ) and ( varToDateTime( '12/14/2005 2:27:00 PM' ) = vDate ) )
      or
        ( ('3.0.69' = vNumber ) and ( varToDateTime( '12/21/2005 1:35:00 PM' ) = vDate ) )
      or
        ( ('3.0.70' = vNumber ) and (varToDateTime( '01/03/2006 12:21:00 PM' ) = vDate ) )
      or
        ( ('3.0.72' = vNumber ) and (varToDateTime( '01/20/2006 11:51:00 AM' ) = vDate ) )
      or
        ( ('3.0.73' = vNumber ) and (varToDateTime( '02/05/2006 5:32:00 PM' ) = vDate ) )
      or
        ( ('3.0.76' = vNumber ) and ( varToDateTime( '02/09/2006 5:16:00 PM' ) = vDate ) )
      or
        ( ('3.0.78' = vNumber ) and ( varToDateTime( '03/29/2006 11:40:00 AM' ) = vDate ) )
      or
        ( ('3.0.79' = vNumber ) and ( varToDateTime( '06/01/2006 09:26:00 AM' ) = vDate ) )
      or
        ( ('3.0.82' = vNumber ) and ( varToDateTime( '08/14/2006 11:13:00 AM' ) = vDate ) )
      or
        ( ('3.0.83' = vNumber ) and ( varToDateTime( '09/12/2006 6:05:00 PM' ) = vDate ) )
      then
        result := true
      else
        result := false
      ;
    end
  ;

  procedure makeVeryOldTables( db: TSMDatabase );
    begin
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_50' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_51' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_52' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_54' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_55' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_56' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_57' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_58' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_59' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_60' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_61' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_62' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_63' ) );
      // There was no schema 3.0.64
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_65' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_66' ) );
      // There was no schema 3.0.67
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_68' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_69_part1' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_69_part2' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_70' ) );
      // There was no schema 3.0.71
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_72' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_73' ) );
      // There was no schema 3.0.74
      // There was no schema 3.0.75
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_76' ) );
      // There was no schema 3.0.77
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_78' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_79' ) );
      // There were no schemas 3.0.80 or 3.0.81
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_82' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_83' ) );
    end
  ;


  function updateInGeneralFor3_1_0( db: TSMDatabase ): boolean;
    begin
      // NOTE: the name of the field `includeSurveillanceZones` was changed to `includeZones` in a later schema
      result := db.execute( 'UPDATE `inGeneral` SET `includeSurveillanceZones` = FALSE' );
    end
  ;


  function adjustZoneParametersFor3_1_1( db: TSMDatabase ): boolean;
    var
      row: TSqlRow;
      useZones: boolean;
      execSuccess: boolean;

    	db2: TSqlDatabase;
      sqlResult: TSqlResult;
    begin
    	db2 := db as TSqlDatabase;
      sqlResult := TSqlResult.create( db2 );

      execSuccess := true;

      // NOTE: the name of the field `includeSurveillanceZones`
      // was changed to `includeZones` in a later schema
      sqlResult.runQuery( 'SELECT `includeSurveillanceZones` FROM `inGeneral`' );

      if( not( sqlResult.success ) ) then
        execSuccess := false
      else
        begin
          row := sqlResult.fetchArrayFirst();

          if( null = row.field('includeSurveillanceZones') ) then
            useZones := false
          else
            useZones := boolean( row.field('includeSurveillanceZones') )
          ;

          if( not( db.execute( 'UPDATE `inControlsGlobal` SET `includeSurveillanceZones` = ' + usBoolToText( useZones ) ) ) ) then
            execSuccess := false
          ;
        end
      ;

      result := execSuccess;

      sqlResult.free();
    end
  ;


  function updateInGeneralFor3_1_9( db: TSMDatabase ): boolean;
    var
      row: TSqlRow;
      useCosts: boolean;
      updateQuery: string;
      execSuccess: boolean;

    	db2: TSqlDatabase;
      sqlResult: TSqlResult;
    begin
    	db2 := db as TSqlDatabase;
      sqlResult := TSqlResult.create( db2 );

      execSuccess := true;

      // NOTE: this field is dropped in a later schema version
      sqlResult.runQuery( 'SELECT `includeCosts` FROM `inGeneral`' );

      if( not( sqlResult.success ) ) then
        execSuccess := false
      else
        begin
          row := sqlResult.fetchArrayFirst();

          if( null = row.field('includeCosts') ) then
            useCosts := false
          else
            useCosts := boolean( row.field('includeCosts') )
          ;

          updateQuery := 'UPDATE `inGeneral` SET'
            + ' `costTrackDestruction` = ' + usBoolToText( useCosts )
            + ', `costTrackVaccination` = ' + usBoolToText( useCosts )
            + ', `costTrackZoneSurveillance` = ' + usBoolToText( useCosts )
          ;

          if( not( db.execute( updateQuery ) ) ) then
            execSuccess := false
          ;
        end
      ;

      result := execSuccess;

      sqlResult.free();
    end
  ;


  function adjustCustomOutputDefinitionsFor3_1_17( db: TSMDatabase ): boolean;
    begin
      // NOTE: fields 'isProdTypeOutput' and 'isIterationOutput' are dropped in a later schema version
      result :=
        db.execute( 'UPDATE `inCustomOutputDefinitions` SET `outputFrequencyCode` = "IP" WHERE `isProdTypeOutput` = true' )
      and
        db.execute( 'UPDATE `inCustomOutputDefinitions` SET `outputFrequencyCode` = "II" WHERE `isIterationOutput` = true' )
      ;
    end
  ;


  function populateSelectDailyOutputsFor3_1_17( db: TSMDatabase ): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );

      // This is tedious, but it's the only safe way to guard against future changes.
      dict.insert( 'sunitDaysInZone', 'false' );
      dict.insert( 'sanimalDaysInZone', 'false' );
      dict.insert( 'sunitsInZone', 'false' );
      dict.insert( 'sanimalsInZone', 'false' );
      dict.insert( 'stsdUSusc', 'false' );
      dict.insert( 'stsdASusc', 'false' );
      dict.insert( 'stsdULat', 'false' );
      dict.insert( 'stsdALat', 'false' );
      dict.insert( 'stsdUSubc', 'false' );
      dict.insert( 'stsdASubc', 'false' );
      dict.insert( 'stsdUClin', 'false' );
      dict.insert( 'stsdAClin', 'false' );
      dict.insert( 'stsdUNImm', 'false' );
      dict.insert( 'stsdANImm', 'false' );
      dict.insert( 'stsdUVImm', 'false' );
      dict.insert( 'stsdAVImm', 'false' );
      dict.insert( 'stsdUDest', 'false' );
      dict.insert( 'stsdADest', 'false' );
      dict.insert( 'stscUSusc', 'false' );
      dict.insert( 'stscASusc', 'false' );
      dict.insert( 'stscULat', 'false' );
      dict.insert( 'stscALat', 'false' );
      dict.insert( 'stscUSubc', 'false' );
      dict.insert( 'stscASubc', 'false' );
      dict.insert( 'stscUClin', 'false' );
      dict.insert( 'stscAClin', 'false' );
      dict.insert( 'stscUNImm', 'false' );
      dict.insert( 'stscANImm', 'false' );
      dict.insert( 'stscUVImm', 'false' );
      dict.insert( 'stscAVImm', 'false' );
      dict.insert( 'stscUDest', 'false' );
      dict.insert( 'stscADest', 'false' );
      dict.insert( 'sinfnUAir', 'false' );
      dict.insert( 'sinfnAAir', 'false' );
      dict.insert( 'sinfnUDir', 'false' );
      dict.insert( 'sinfnADir', 'false' );
      dict.insert( 'sinfnUInd', 'false' );
      dict.insert( 'sinfnAInd', 'false' );
      dict.insert( 'sinfcUIni', 'false' );
      dict.insert( 'sinfcAIni', 'false' );
      dict.insert( 'sinfcUAir', 'false' );
      dict.insert( 'sinfcAAir', 'false' );
      dict.insert( 'sinfcUDir', 'false' );
      dict.insert( 'sinfcADir', 'false' );
      dict.insert( 'sinfcUInd', 'false' );
      dict.insert( 'sinfcAInd', 'false' );
      dict.insert( 'sexpcUDir', 'false' );
      dict.insert( 'sexpcADir', 'false' );
      dict.insert( 'sexpcUInd', 'false' );
      dict.insert( 'sexpcAInd', 'false' );
      dict.insert( 'strcUDir', 'false' );
      dict.insert( 'strcADir', 'false' );
      dict.insert( 'strcUInd', 'false' );
      dict.insert( 'strcAInd', 'false' );
      dict.insert( 'strcUDirp', 'false' );
      dict.insert( 'strcADirp', 'false' );
      dict.insert( 'strcUIndp', 'false' );
      dict.insert( 'strcAIndp', 'false' );
      dict.insert( 'sdetnUClin', 'false' );
      dict.insert( 'sdetnAClin', 'false' );
      dict.insert( 'sdetcUClin', 'false' );
      dict.insert( 'sdetcAClin', 'false' );
      dict.insert( 'sdesnUAll', 'false' );
      dict.insert( 'sdesnAAll', 'false' );
      dict.insert( 'sdescUIni', 'false' );
      dict.insert( 'sdescAIni', 'false' );
      dict.insert( 'sdescUDet', 'false' );
      dict.insert( 'sdescADet', 'false' );
      dict.insert( 'sdescUDir', 'false' );
      dict.insert( 'sdescADir', 'false' );
      dict.insert( 'sdescUInd', 'false' );
      dict.insert( 'sdescAInd', 'false' );
      dict.insert( 'sdescURing', 'false' );
      dict.insert( 'sdescARing', 'false' );
      dict.insert( 'svaccnUAll', 'false' );
      dict.insert( 'svaccnAAll', 'false' );
      dict.insert( 'svaccUIni', 'false' );
      dict.insert( 'svaccAIni', 'false' );
      dict.insert( 'svaccURing', 'false' );
      dict.insert( 'svaccARing', 'false' );
      dict.insert( 'szonnFoci', 'false' );
      dict.insert( 'szoncFoci', 'false' );
      dict.insert( 'sappUInfectious', 'false' );

      q := writeQuery( 'inSelectDailyOutputs', QInsert, dict );
      dict.free();

      result := db.execute( q );
    end
  ;


  function populateSelectDailyOutputsFor3_1_18( db: TSMDatabase ): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );

      dict.insert( 'strnUDir', 'false' );
      dict.insert( 'strnADir', 'false' );
      dict.insert( 'strnUInd', 'false' );
      dict.insert( 'strnAInd', 'false' );

      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := db.execute( q );
    end
  ;


  function populateSelectDailyOutputsFor3_2_0( db: TSMDatabase ): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );

      dict.insert( 'strcUDirBack', 'false' );
      dict.insert( 'strcADirBack', 'false' );
      dict.insert( 'strcUIndBack', 'false' );
      dict.insert( 'strcAIndBack', 'false' );
      dict.insert( 'strcUDirpBack', 'false' );
      dict.insert( 'strcADirpBack', 'false' );
      dict.insert( 'strcUIndpBack', 'false' );
      dict.insert( 'strcAIndpBack', 'false' );
      
      dict.insert( 'strnUDirBack', 'false' );
      dict.insert( 'strnADirBack', 'false' );
      dict.insert( 'strnUIndBack', 'false' );
      dict.insert( 'strnAIndBack', 'false' );

      dict.insert( 'sexmcUDirFwd', 'false' );
      dict.insert( 'sexmcADirFwd', 'false' );
      dict.insert( 'sexmcUIndFwd', 'false' );
      dict.insert( 'sexmcAIndFwd', 'false' );
      dict.insert( 'sexmcUDirBack', 'false' );
      dict.insert( 'sexmcADirBack', 'false' );
      dict.insert( 'sexmcUIndBack', 'false' );
      dict.insert( 'sexmcAIndBack', 'false' );

      dict.insert( 'ststcUDirFwd', 'false' );
      dict.insert( 'ststcADirFwd', 'false' );
      dict.insert( 'ststcUIndFwd', 'false' );
      dict.insert( 'ststcAIndFwd', 'false' );
      dict.insert( 'ststcUDirBack', 'false' );
      dict.insert( 'ststcADirBack', 'false' );
      dict.insert( 'ststcUIndBack', 'false' );
      dict.insert( 'ststcAIndBack', 'false' );

      dict.Insert( 'sexmnUAll', 'false' );
      dict.Insert( 'sexmnAAll', 'false' );

      dict.insert( 'ststcUTruePos', 'false' );
      dict.insert( 'ststcATruePos', 'false' );
      dict.insert( 'ststcUTrueNeg', 'false' );
      dict.insert( 'ststcATrueNeg', 'false' );
      dict.insert( 'ststcUFalsePos', 'false' );
      dict.insert( 'ststcAFalsePos', 'false' );
      dict.insert( 'ststcUFalseNeg', 'false' );
      dict.insert( 'ststcAFalseNeg', 'false' );

      dict.insert( 'ststnUTruePos', 'false' );
      dict.insert( 'ststnATruePos', 'false' );
      dict.insert( 'ststnUTrueNeg', 'false' );
      dict.insert( 'ststnATrueNeg', 'false' );
      dict.insert( 'ststnUFalsePos', 'false' );
      dict.insert( 'ststnAFalsePos', 'false' );
      dict.insert( 'ststnUFalseNeg', 'false' );
      dict.insert( 'ststnAFalseNeg', 'false' );

      dict.insert( 'sdetnUTest', 'false' );
      dict.insert( 'sdetnATest', 'false' );
      dict.insert( 'sdetcUTest', 'false' );
      dict.insert( 'sdetcATest', 'false' );

      dict.insert( 'sdescUDirBack', 'false' );
      dict.insert( 'sdescADirBack', 'false' );
      dict.insert( 'sdescUIndBack', 'false' );
      dict.insert( 'sdescAIndBack', 'false' );

      dict.insert( 'sdeswUAll', 'false' );
      dict.insert( 'sdeswAAll', 'false' );

      dict.insert( 'svacwUAll', 'false' );
      dict.insert( 'svacwAAll', 'false' );

      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := db.execute( q );
    end
  ;


  function populateSelectDailyOutputsFor3_2_11( db: TSMDatabase ): boolean;
    var
      dict: TQueryDictionary;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );

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

      result := db.execute( q );
    end
  ;


  function updateEventAndControlStateCodesFor3_2_0( db: TSMDatabase ): boolean;
    var
      list: TStringList;
      i: integer;
    begin
      list := TStringList.create();

      list.append( 'update `readEventCodes` set `definition` = "Traced forward - direct contact" where `eventCode` = "T"' );
      list.append( 'update `readEventCodes` set `definition` = "Traced forward - indirect contact" where `eventCode` = "I"' );
      list.append( 'insert into `readEventCodes` ( `eventCode`, `definition` ) values ( "K", "Traced back - direct contact" )' );
      list.append( 'insert into `readEventCodes` ( `eventCode`, `definition` ) values ( "J", "Traced back - indirect contact" )' );
      list.append( 'insert into `readEventCodes` ( `eventCode`, `definition` ) values ( "X", "Herd examination" )' );
      list.append( 'insert into `readEventCodes` ( `eventCode`, `definition` ) values ( "S", "Diagnostic test" )' );

      list.append( 'update `readControlStateCodes` set `definition` = "No control" where `controlStateCode` = "U"' );
      list.append( 'update `readControlStateCodes` set `definition` = "Traced forward - direct contact" where `controlStateCode` = "T"' );
      list.append( 'update `readControlStateCodes` set `definition` = "Traced forward - indirect contact" where `controlStateCode` = "I"' );
      list.append( 'update `readControlStateCodes` set `definition` = "Detected" where `controlStateCode` = "E"' );
      list.append( 'insert into `readControlStateCodes` ( `controlStateCode`, `definition` ) values ( "K", "Traced back - direct contact" )' );
      list.append( 'insert into `readControlStateCodes` ( `controlStateCode`, `definition` ) values ( "J", "Traced back - indirect contact" )' );
      list.Append( 'insert into `readControlStateCodes` ( `controlStateCode`, `definition` ) values ( "Q", "Quarantined only" )' );

      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "U", "Uninfected, no disease control activity" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "I", "Infected but undetected" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "X", "Examined" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "N", "Test true negative" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "O", "Test false negative" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "Q", "Test false positive" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "P", "Test true positive" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "E", "Detected by clinical signs" )' );
      list.append( 'insert into `readDetectionStateCodes` ( `detectionStateCode`, `definition` ) values ( "D", "Destroyed" )' );

      list.append( 'update `dynHerd` set `finalDetectionStateCode` = "U"' );

      result := true;
      for i := 0 to list.count - 1 do
        result := result and db.execute( list[i] )
      ;

      list.free();
    end
  ;


  function populateDefaultInputsFor3_2_0( db: TSMDatabase ): boolean;
    var
      dict: TQueryDictionary;
      q: string;

      success: boolean;
      pdf: TPdfPoint;
      pdfID: integer;

      res: TSqlResult;
      row: TSqlRow;
    begin
      success := true;
      dict := TQueryDictionary.create();

      // Update inControlsGlobal
      //------------------------
      dict['includeTracingHerdExam'] := 'false';
      dict['includeTracingTesting'] := 'false';

      q := writeQuery( 'inControlsGlobal', QUpdate, dict );
      success := success and db.execute( q );

      // Create default chart for tracing delay
      //---------------------------------------
      pdf := TPdfPoint.create( 0, UDays );
      pdf.name := 'No tracing delay [NAADSM update default]';
      pdf.dbField := word( TrDelay );
      pdfID := pdf.populateDatabase( ( db as TSqlDatabase ) );
      success := success and ( -1 <> pdfID );
      pdf.Free();

      // Update inProductionType
      //------------------------
      if( success ) then
        begin
          dict.clear();

          dict['traceDelayPdfID'] := intToStr( pdfID );

          // Tracing for forward traces is already specified
          dict['traceDirectBack'] := 'false';
          dict['traceIndirectBack'] := 'false';

          // Destruction of forward-traced units is already specified
          dict['destrDirectBackTraces'] := 'false';
          dict['destrIndirectBackTraces'] := 'false';

          dict['examDirectForward'] := 'false';
          dict['examIndirectForward'] := 'false';
          dict['examDirectBack'] := 'false';
          dict['examIndirectBack'] := 'false';

          dict['testDirectForward'] := 'false';
          dict['testIndirectForward'] := 'false';
          dict['testDirectBack'] := 'false';
          dict['testIndirectBack'] := 'false';

          q := writeQuery( 'inProductionType', QUpdate, dict );
          success := success and db.execute( q );
        end
      ;

      // Update destruction priority order
      //----------------------------------
      if( success ) then
        begin
          res := TSqlResult.create( 'SELECT `destrReasonOrder` FROM `inControlsGlobal`', db );
          row := res.fetchArrayFirst();
          if ( row.field( 'destrReasonOrder' ) <> null ) then
            begin
              q := row.field( 'destrReasonOrder' );
              res.Free();

              q := ansiReplaceStr( q, 'indirect', 'TMP'  );
              q := ansiReplaceStr( q, 'direct', 'direct-forward' );
              q := ansiReplaceStr( q, 'TMP', 'indirect-forward' );
              q := q + 'direct-back,indirect-back';

              dict.clear();
              dict['destrReasonOrder'] := db.sqlQuote( q );
              q := writeQuery( 'inControlsGlobal', QUpdate, dict );
              success := success and db.execute( q );
            end
          ;
        end
      ;
      dict.Free();

      result := success;
    end
  ;


  function projectZoneCoordinatesFor3_2_0( db: TSMDatabase ): boolean;
    var
      perimeterList: TZonePerimeterList;
      mStream: TMemoryStream;
      i, j: integer;
      perim: TZonePerimeter;
      proj: TProj4;
      xyr: RPoint;
      gpcxy: gpc_vertex;
      minLat, minLon, maxLat, maxLon: double;
      res: TSqlResult;
      row: TSqlRow;
      q: string;
    begin
      result := true;

      if( db.containsZonePerimeters ) then
        begin
          // Load the perimeter list from the database
          //------------------------------------------
          perimeterList := TZonePerimeterList.create();
          mStream := db.createStreamFromBlob( 'dynBlob', 'zonePerimeters' );
          perimeterList.loadFromStream( mStream );
          mStream.Free();

          // Determine the extreme lats and lons
          //------------------------------------
          maxLat := LAT_LON_UNDEFINED;
          minLat := LAT_LON_UNDEFINED;
          maxLon := LAT_LON_UNDEFINED;
          minLon := LAT_LON_UNDEFINED;

          res := TSqlResult.create( db as TSqlDatabase );

          q := 'SELECT'
            + '   MAX( latitude ) AS maxLat,'
            + '   MIN( latitude ) AS minLat,'
            + '   MAX( longitude ) AS maxLon,'
            + '   MIN( longitude ) AS minLon'
            + ' FROM dynHerd'
          ;

          res.runQuery( q );
          row := res.fetchArrayFirst();

          if( null <> row.field('maxLat') ) then maxLat := row.field('maxLat');
          if( null <> row.field('minLat') ) then minLat := row.field('minLat');
          if( null <> row.field('maxLon') ) then maxLon := row.field('maxLon');
          if( null <> row.field('minLon') ) then minLon := row.field('minLon');

          // FIX ME: Some error checking of lats and lons might be in order.

          res.Free();

          // Set up the projection system
          //-----------------------------
          proj := TProj4.create( THerdList.defaultProjection( minLat, minLon, maxLat, maxLon ) );

          // Project all of the zone vertices
          //---------------------------------
          for j := 0 to perimeterList.count - 1 do
            begin
              perim := perimeterList[j] as TZonePerimeter;

              if ( 0 < perim.count ) then
                begin
                  for i := 0 to perim.count - 1 do
                    begin
                      try
                        xyr := proj.pjFwd( perim[i].x, perim[i].y, true );
                        gpcxy.x := xyr.x;
                        gpcxy.y := xyr.y;
                        perim[i] := gpcxy;
                      except
                        result := false;
                        freeAndNil( perimeterList );
                        freeAndNil( proj );
                        exit;
                      end;
                    end
                  ;
                end
              ;
            end
          ;

          proj.free();

          //perimeterList.debug();

          // Save the projected perimeter list back to the database
          //-------------------------------------------------------
          mStream := TMemoryStream.Create();
          mStream.Position := 0;
          perimeterList.saveToStream( mStream );
          db.writeBlobFromStream( mStream, 'dynBlob', 'zonePerimeters' );
          mStream.free();

          // Clean up
          //---------
          freeAndNil( perimeterList );
        end
      ;
    end
  ;

end.