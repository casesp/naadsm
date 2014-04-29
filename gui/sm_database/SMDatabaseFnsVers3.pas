unit SMDatabaseFnsVers3;

(*
SMDatabaseFnsVers3.pas
----------------------
Begin: 2007/02/08
Last revision: $Date: 2013-04-01 18:58:52 $ $Author: areeves $
Version number: $Revision: 1.1.6.11 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2007 - 2013 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

(*
  Several functions in SMDatabase were becoming unwieldy as the database schema was repeatedly updated.
  These very old updates are now handled by this unit, which keeps SMDatabase more readable.
*)
interface
  
  uses
    ModelDatabase,
    SMDatabase
  ;

  function checkUpdateReasonsVers3( const vNumber: string ): TDBSchemaUpdateReason;
  function updateSchemaVers3( db: TSMDatabase; var vNumber: string; var updateSuccess: boolean ): boolean;
  function versionIsValidVers3( db: TSMDatabase; const vNumber: string; const vDate: TDateTime ): boolean;
  procedure makeTablesVers3( db: TSMDatabase );
  function versionObsoleteVers3( const vNumber: string ): boolean;
  function versionIsVers3( const oldVersion: string ): boolean;

  // The following functions can all be considered "private" in this unit:
  (*
  { Return true on success, false on failure }
  function populateSelectDailyOutputsFor3_2_0( db: TSMDatabase ): boolean;
  function populateDefaultInputsFor3_2_0( db: TSMDatabase ): boolean;
  function updateEventAndControlStateCodesFor3_2_0( db: TSMDatabase ): boolean;
  function projectZoneCoordinatesFor3_2_0( db: TSMDatabase ): boolean;

  function populateSelectDailyOutputsFor3_1_18( db: TSMDatabase ): boolean;

  function populateSelectDailyOutputsFor3_1_17( db: TSMDatabase ): boolean;
  function adjustCustomOutputDefinitionsFor3_1_17( db: TSMDatabase ): boolean;

  function updateInGeneralFor3_1_9( db: TSMDatabase ): boolean;

  function adjustZoneParametersFor3_1_1( db: TSMDatabase ): boolean;

  function updateInGeneralFor3_1_0( db: TSMDatabase ): boolean;
  *)
  
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


  function schemaIDOKVers3( db: TSMDatabase; const vNumber: string ): boolean;
    var
      sqlResult: TSqlResult;
      row: TSqlRow;
      vID: integer;
    begin
      // Recall that a database schema version corresponds to the
      // application version in which it was introduced, and that
      // not all new application versions introduce changes to the schema.
      // Consequently, database schema versions are not necessarily sequential.

      result := false;

      sqlResult := TSqlResult.create( 'SELECT `VersionID` FROM `DBSchemaVersion`', (db as TSqlDatabase ) );

      if( sqlResult.success ) then
        begin
          row := sqlResult.fetchArrayFirst();
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
              or
                ( ( '3.1.20' = vNumber ) and ( 1231364931 = vID ) )
              or
                ( ( '3.1.22' = vNumber ) and ( 1233602101 = vID ) )
              or
                ( ( '3.2.0' = vNumber ) and ( 1253287889 = vID ) )
              or
                ( ( '3.2.1' = vNumber ) and ( 1256249352 = vID ) )
              or
                ( ( '3.2.2' = vNumber ) and ( 1257620055 = vID ) )
              or
                ( ( '3.2.5' = vNumber ) and ( 1274302573 = vID ) )
              or
                ( ( '3.2.11' = vNumber ) and ( 1305654509 = vID ) )
              or
                ( ( '3.2.13' = vNumber ) and ( 1309203153 = vID ) )
              or
                ( ( '3.2.18' = vNumber ) and ( 1317332651 = vID ) )
              then
                result := true
              ;
            end
          ;
        end
      ;

      sqlResult.free();
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

          if( not( db.execute( 'UPDATE `inControlsGlobal` SET `includeSurveillanceZones` = ' + db.sqlBool( useZones ) ) ) ) then
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
            + ' `costTrackDestruction` = ' + db.sqlBool( useCosts )
            + ', `costTrackVaccination` = ' + db.sqlBool( useCosts )
            + ', `costTrackZoneSurveillance` = ' + db.sqlBool( useCosts )
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
      list.Append( 'insert into `readControlStateCodes` ( `controlStateCode`, `definition` ) values ( "Q", "In destruction queue" )' );

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
            + ' `destrDirectTraces` = ' + db.sqlBool( row.field( 'survDirect' ) ) + ','
            + ' `destrIndirectTraces` = ' + db.sqlBool( row.field( 'survIndirect' ) )
            + ' WHERE `productionTypeID` = ' + db.sqlBool( row.field('productionTypeID') )
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


  procedure disconnectSelectDailyOutputTables(  db: TSMDatabase );
    var
      q: string;
    begin
      // Drop foreign key constraint from table outSelectDailyByProductionType
      if( db.tableExists( 'outSelectDailyByProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outSelectDailyByProductionType` DROP CONSTRAINT `inProductionType_outSelectDailyByProductionType_FK1`';
          db.execute( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZoneAndProductionType
      if( db.tableExists( 'outSelectDailyByZoneAndProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType` DROP CONSTRAINT `inZone_outSelectDailyByZoneAndProductionType_FK1`';
          db.execute( q );
          q := 'ALTER TABLE `outSelectDailyByZoneAndProductionType` DROP CONSTRAINT `inProductionType_outSelectDailyByZoneAndProductionType_FK1`';
          db.execute( q );
        end
      ;
    end
  ;


  procedure disconnectCustomOutputTables( db: TSMDatabase );
    var
      q: string;
    begin
      // Table outCustIteration currently has no constraints to worry about.

      // Drop foreign key constraint from table outCustIterationByProductionType
      if( db.tableExists( 'outCustIterationByProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByProductionType` DROP CONSTRAINT `inProductionType_outCustIterationByProductionType_FK1`';
          db.execute( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZone
      if( db.tableExists( 'outCustIterationByZone' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByZone` DROP CONSTRAINT `inZone_outCustIterationByZone_FK1`';
          db.execute( q );
        end
      ;

      // Drop foreign key constraint from table outCustIterationByZoneAndProductionType
      if( db.tableExists( 'outCustIterationByZoneAndProductionType' ) ) then
        begin
          q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` DROP CONSTRAINT `inZone_outCustIterationByZoneAndProductionType_FK1`';
          db.execute( q );
          q := 'ALTER TABLE `outCustIterationByZoneAndProductionType` DROP CONSTRAINT `inProductionType_outCustIterationByZoneAndProductionType_FK1`';
          db.execute( q );
        end
      ;
    end
  ;


  function updateSchemaVers3( db: TSMDatabase; var vNumber: string; var updateSuccess: boolean ): boolean;
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
                  + ' VALUES ( ' + db.sqlQuote( DB_SCHEMA_APPLICATION ) + ', ' + DATABASE_NULL_VALUE + ', ' + DATABASE_NULL_VALUE + ', 0 ) '
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

      // For all subsequent updates, make sure that constraints on the
      // custom output tables don't prevent the updates from being applied.
      disconnectCustomOutputTables( db );
      disconnectSelectDailyOutputTables( db );

      // Update 3.0.85 to 3.0.86
      //------------------------
      if( '3.0.85' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.85', true );
            // Capability for custom outputs was added
            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_0_86' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.0.86';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.0.86 to 3.1.0
      //-----------------------
      if( '3.0.86' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.0.86', true );

            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_0' ) )
            and
              // New fields were added for zones
              updateInGeneralFor3_1_0( db )
            and
              db.execute( 'UPDATE `inProductionType` SET `zoneDetectionIsTrigger` = FALSE, `zoneDirectTraceIsTrigger` = FALSE, `zoneIndirectTraceIsTrigger` = FALSE' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.0 to 3.1.1
      //----------------------
      if( '3.1.0' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.0', true );

            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part1' ) )
            and
              // New fields were added for zone parameters
              ( db.execute( 'UPDATE `inZoneProductionTypePair` SET `useDirectMovementControl` = FALSE, `useIndirectMovementControl` = FALSE, `useDetectionMultiplier` = FALSE' ) )
            and
              // New table for BLOBS was added
              ( db.execute( 'INSERT INTO `dynBlob` (`dynBlobID`) VALUES ("' + DB_SCHEMA_APPLICATION + '")' ) )
            and
              // Zone parameter moved from inGeneral to InControlsGlobal
              adjustZoneParametersFor3_1_1( db )
            and
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part2' ) )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.1';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.1 to 3.1.2
      //----------------------
      if( '3.1.1' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.1', true );

            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_2' ) )
            and
              // New fields were added for within-herd prevalence
              db.execute( 'UPDATE `inGeneral` SET `useWithinHerdPrevalence` = FALSE' )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.2';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.2 to 3.1.4
      //----------------------
      if( '3.1.2' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.2', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_4' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.4';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.4 to 3.1.5
      //----------------------
      if( '3.1.4' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.4', true );

            execSuccess :=
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_5' ) )
            and
              db.execute( 'INSERT INTO `readEventCodes` ( eventCode, definition ) VALUES ("Z", "Zone focus created")' )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.5';
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
      if( '3.1.5' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.5', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_7' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.7';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.7 to 3.1.8
      //----------------------
      if( '3.1.7' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.7', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_8' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.8';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.8 to 3.1.9
      //----------------------
      if( '3.1.8' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.8', true );

            execSuccess :=
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_9' ) )
            and
              updateInGeneralFor3_1_9( db )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.9';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.9 to 3.1.10
      //-----------------------
      if( '3.1.9' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.9', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_10' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.10';
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
      if( '3.1.10' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.10', true );

            execSuccess := db.execute( 'INSERT INTO `readEventCodes` ( eventCode, definition ) VALUES ("C", "Zone changed")' );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.12';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.12 to 3.1.13
      //-----------------------------
      if( '3.1.12' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.12', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_13' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.13';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.13 to 3.1.17
      //------------------------
      if( '3.1.13' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.13', true );

            execSuccess := (
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part1' ) )
            and
              // output frequency code was introduced in version 3.1.17
              adjustCustomOutputDefinitionsFor3_1_17( db )
            and
              populateSelectDailyOutputsFor3_1_17( db )
            and
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part2' ) )
            );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.17';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.17 to 3.1.18
      //------------------------
      if( '3.1.17' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.17', true );

            execSuccess :=
              db.processDDLCommands( getResourceAsString( 'DBSchema3_1_18' ) )
            and
              populateSelectDailyOutputsFor3_1_18( db )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.18';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.1.18 to 3.1.20
      //------------------------
      if( '3.1.18' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.18', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_20' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.20';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;


      // Update 3.1.20 to 3.1.22
      //------------------------
      if( '3.1.20' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.20', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_1_22' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.1.22';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;


      // Update 3.1.22 to 3.2.0
      //-----------------------
      if( '3.1.22' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.1.22', true );

            execSuccess :=
              db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewInputs' ) )
            and
              db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_RenamedOutputs' ) )
            and
              db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewOutputs' ) )
            and
              db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_AlteredHerdTable' ) )
            and
              updateEventAndControlStateCodesFor3_2_0( db )
            and
              populateDefaultInputsFor3_2_0( db )
            and
              populateSelectDailyOutputsFor3_2_0( db )
            and
              projectZoneCoordinatesFor3_2_0( db )
            ;

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.2.0';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.0 to 3.2.1
      //----------------------
      if( '3.2.0' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.0', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_2_1' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.2.1';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Update 3.2.1 to 3.2.2
      //----------------------
      if( '3.2.1' = vNumber ) then
        begin
          try
            dbcout( 'Updating from 3.2.1', true );

            execSuccess := db.processDDLCommands( getResourceAsString( 'DBSchema3_2_2' ) );

            if( not( execSuccess ) ) then
              begin
                updateSuccess := false;
                exit;
              end
            ;

            vNumber := '3.2.2';
            result := true;
          except
            updateSuccess := false;
            exit;
          end;
        end
      ;

      // Schema update 3.2.5 isn't necessary here.  It is taken care of in the update to 4.0.3.
    end
  ;


  function checkUpdateReasonsVers3( const vNumber: string ): TDBSchemaUpdateReason;
    begin
      result := DBUpdateUnspecified;

      if( '3.0.52' = vNumber )
      or( '3.0.59' = vNumber )
      or( '3.0.60' = vNumber )
      or( '3.0.61' = vNumber )
      or( '3.0.62' = vNumber )
      or( '3.0.63' = vNumber )
      // There was no DB Schema 3.0.64
      or( '3.0.65' = vNumber )
      or( '3.0.66' = vNumber )
      // There was no DB Schema 3.0.67
      or( '3.0.68' = vNumber )
      or( '3.0.69' = vNumber )
      or( '3.0.70' = vNumber )
      // There was no DB schema 3.0.71
      or( '3.0.72' = vNumber )
      or( '3.0.73' = vNumber )
      or( '3.0.76' = vNumber )
      or( '3.0.78' = vNumber )
      or( '3.0.79' = vNumber )
      or( '3.0.82' = vNumber )
      or( '3.0.83' = vNumber )
      // There was no schema 3.0.84
      or( '3.0.85' = vNumber )
      or( '3.0.86' = vNumber )
      or( '3.1.0' = vNumber )
      or( '3.1.1' = vNumber )
      or( '3.1.2' = vNumber )
      // There was no schema 3.1.3
      or( '3.1.4' = vNumber )
      or( '3.1.5' = vNumber )
      // There was no schema 3.1.6
      or( '3.1.7' = vNumber )
      or( '3.1.8' = vNumber )
      or( '3.1.9' = vNumber )
      or( '3.1.10' = vNumber )
      // There was no schema 3.1.11
      or( '3.1.12' = vNumber )
      or( '3.1.13' = vNumber )
      // There was no schema 3.1.14, 3.1.15, or 3.1.16
      or( '3.1.17' = vNumber )
      // There was no schema 3.1.17
      or( '3.1.18' = vNumber )
      // There was no schema 3.1.19
      or( '3.1.20' = vNumber )
      // There was no schema 3.1.21
      or( '3.1.22' = vNumber )
      // There was no schema 3.1.23
      or( '3.2.0' = vNumber )
      or( '3.2.1' = vNumber )
      or( '3.2.2' = vNumber )
      or( '3.2.5' = vNumber )
      // There were no schemas from 3.2.6 to 3.2.10
      or( '3.2.11' = vNumber )
      // There was no schema 3.2.12
      or( '3.2.13' = vNumber )
      // There were no schemas from 3.2.14 to 3.2.17
      or( '3.2.18' = vNumber )
      then
        result := DBUpdateSpecChange
      ;
    end
  ;


  function versionIsValidVers3( db: TSMDatabase; const vNumber: string; const vDate: TDateTime ): boolean;
    var
      currentShortDateFormat: string;
    begin
      currentShortDateFormat := ShortDateFormat;
      ShortDateFormat := 'mm/dd/yyyy';

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
        ( schemaIDOKVers3( db, vNumber ) )
      then // the date and number/ID are valid.
        result := true
      else // the date and number/ID are not recognized as NAADSM databases.
        result := false
      ;

      ShortDateFormat := currentShortDateFormat;
    end
  ;

  
  procedure makeTablesVers3( db: TSMDatabase );
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
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_85' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_0_86' ) );
      // The jump was made here from 3.0 to 3.1
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_0' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part1' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_1_part2' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_2' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_4' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_5' ) );
      // The "Z" code is new in version 3.1.5.  All other event codes are populated in an earlier DDL file.
      db.execute( 'INSERT INTO `readEventCodes` (eventCode, definition) VALUES ("Z", "Zone focus created")' );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_7' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_8' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_9' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_10' ) );
      // The "C" code is new in version 3.1.12.  No other schema changes were made.
      db.execute( 'INSERT INTO `readEventCodes` (eventCode, definition) VALUES ("C", "Zone changed")' );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_13' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part1' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_17_part2' ) );
      // Values for the table inSelectDailyOutputs are new in version 3.1.17
      populateSelectDailyOutputsFor3_1_17( db );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_18' ) );
      // Additional values for the table inSelectDailyOutputs are new in version 3.1.17
      populateSelectDailyOutputsFor3_1_18( db );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_20' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_1_22' ) );

      // The jump was made here from version 3.1 to version 3.2.
      db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewInputs' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_RenamedOutputs' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_NewOutputs' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_2_0_AlteredHerdTable' ) );
      updateEventAndControlStateCodesFor3_2_0( db );
      populateSelectDailyOutputsFor3_2_0( db );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_2_1' ) );
      db.processDDLCommands( getResourceAsString( 'DBSchema3_2_2' ) );
    end
  ;


  function versionObsoleteVers3( const vNumber: string ): boolean;
    begin
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


  function isCheyenneVers3( const oldVersion: string ): boolean;
    begin
      result := false; // until shown otherwise.

      {$IFNDEF CHEYENNE}
        if
          ( '3.0.80-Cheyenne' = oldVersion )
        or
          // There was no 3.0.81-Cheyenne
          ( '3.0.82-Cheyenne' = oldVersion )
        or
          // There was no 3.0.83-Cheyenne
          ( '3.0.84-Cheyenne' = oldVersion )
        or
          // There was no 3.0.85-Cheyenne
          // There were no versions of Cheyenne that correspond to 3.1.0 through 3.1.12.
          ( '3.1.13-Cheyenne' = oldVersion )
        or
          ( '3.1.14-Cheyenne' = oldVersion )
        or
          ( '3.1.15-Cheyenne' = oldVersion )
        then
          result := true
        ;
      {$ENDIF}
    end
  ;


  function isLaramieVers3( const oldVersion: string ): boolean;
    begin
      result := false; // until shown otherwise.

      {$IFNDEF LARAMIE}
        if
          ( '3.1.18-Laramie' = oldVersion )
        or
          ( '3.1.19-Laramie' = oldVersion )
        // There have been no versions of Laramie in NAADSM 3 since 3.1.19.
        then
          result := true
        ;
      {$ENDIF}
    end
  ;


  function isRivertonVers3( const oldVersion: string ): boolean;
    begin
      result := false; // until shown otherwise.

      {$IFNDEF RIVERTON}
        if
          ( '3.1.22-Riverton' = oldVersion )
        or
          ( '3.1.23-Riverton' = oldVersion )
        // There are no other versions of Riverton in NAADSM 3
        then
          result := true
        ;
      {$ENDIF}
    end
  ;


  function isTorringtonVers3( const oldVersion: string ): boolean;
    begin
      result := false; // until shown otherwise.

      {$IFNDEF TORRINGTON}
        if
          ( '3.1.28-Torrington' = oldVersion )
        or
          ( '3.2.14-Torrington' = oldVersion )
        or
          ( '3.2.15-Torrington' = oldVersion )
        or
          ( '3.2.16-Torrington' = oldVersion )
        or
          ( '3.2.17-Torrington' = oldVersion )
        // There are no other versions of Torrington in NAADSM 3
        then
          result := true
        ;
      {$ENDIF}
    end
  ;


  function versionIsVers3( const oldVersion: string ): boolean;
    begin
      // Check version numbers for experimental versions.
      if
        ( isCheyenneVers3( oldVersion ) )
      or
        ( isLaramieVers3( oldVersion ) )
      or
        ( isRivertonVers3( oldVersion ) )
      or
        ( isTorringtonVers3( oldVersion ) )


      // These version numbers are recognized for the standard version.
      or
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
      or
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
      or
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
      or
        ( '3.1.17' = oldVersion )
      or
        ( '3.1.18' = oldVersion )
      or
        ( '3.1.19' = oldVersion )
      or
        ( '3.1.20' = oldVersion )
      or
        ( '3.1.21' = oldVersion )
      or
        ( '3.1.22' = oldVersion )
      or
        ( '3.1.23' = oldVersion )
      or
        ( '3.1.24' = oldVersion )
      or
        ( '3.1.25' = oldVersion )
      or
        ( '3.1.26' = oldVersion )
      or
        ( '3.1.27' = oldVersion )
      or
        ( '3.1.28' = oldVersion )

      or
        ( '3.2.0' = oldVersion )
      or
        ( '3.2.1' = oldVersion )
      or
        ( '3.2.2' = oldVersion )
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
      or
        ( '3.2.16' = oldVersion )
      or
        ( '3.2.17' = oldVersion )
      or
        ( '3.2.18' = oldVersion )
      then
        result := true
      else
        result := false
      ;
    end
  ;
  


end.