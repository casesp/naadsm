unit OldDatabaseFns;

(*
OldDatabaseFns.pas
------------------------------
Begin: 2007/02/08
Last revision: $Date: 2008/10/23 17:58:29 $ $Author: areeves $
Version number: $Revision: 1.13 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

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
		SMDatabase
	;

  function handleVeryOldDatabaseSchemas( db: TSMDatabase; var vNumber: string; var updateSuccess: boolean ): boolean;

  function checkVeryOldUpdateReasons( const vNumber: string ): TDBSchemaUpdateReason;

  function veryOldVersionIsValid( const vNumber: string; const vDate: TDateTime ): boolean;

  procedure makeVeryOldTables( db: TSMDatabase );

  { Return true on success, false on failure }
  function updateInGeneralFor3_1_0( db: TSMDatabase ): boolean;
  function adjustZoneParametersFor3_1_1( db: TSMDatabase ): boolean;
  function updateInGeneralFor3_1_9( db: TSMDatabase ): boolean;
  function adjustCustomOutputDefinitionsFor3_1_17( db: TSMDatabase ): boolean;
  function populateSelectDailyOutputsFor3_1_17( db: TSMDatabase ): boolean;
  function populateSelectDailyOutputsFor3_1_18( db: TSMDatabase ): boolean;

implementation

  uses
    Windows,
    Variants,
    SysUtils,
    TypInfo,
    StrUtils,

    Resources,

    SqlClasses,
    MyStrUtils,
    USStrUtils,
    DebugWindow,
    FunctionPointers,

    OutputDescriptions
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
          row.debug();

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
      i: TDailyOutputVariable;
      s: string;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );

      for i := DEunitDaysInZone to DEtrcAIndp do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputVariable ), ord(i) );
          s := 's' + rightStr( s, length( s ) - 2 );
          dict[ s ] := 'false';
        end
      ;

      // Skip a few in the middle that were introduced in 3.1.18...

      for i := DEdetnUClin to DEappUInfectious do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputVariable ), ord(i) );
          s := 's' + rightStr( s, length( s ) - 2 );
          dict[ s ] := 'false';
        end
      ;

      q := writeQuery( 'inSelectDailyOutputs', QInsert, dict );
      dict.free();

      result := db.execute( q );
    end
  ;


  function populateSelectDailyOutputsFor3_1_18( db: TSMDatabase ): boolean;
    var
      dict: TQueryDictionary;
      i: TDailyOutputVariable;
      s: string;
      q: string;
    begin
      dict := TQueryDictionary.create();
      dict['inSelectDailyOutputID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );

      for i := DEtrnUDir to DEtrnAInd do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputVariable ), ord(i) );
          s := 's' + rightStr( s, length( s ) - 2 );
          dict[ s ] := 'false';
        end
      ;

      q := writeQuery( 'inSelectDailyOutputs', QUpdate, dict );
      dict.free();

      result := db.execute( q );
    end
  ;


end.