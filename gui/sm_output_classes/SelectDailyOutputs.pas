unit SelectDailyOutputs;

(*
SelectDailyOutputs.pas
-----------------------
Begin: 2007/09/17
Last revision: $Date: 2011-10-19 01:30:22 $ $Author: areeves $
Version: $Revision: 1.2.10.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2007 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    QVectors,

    SMDatabase
  ;

  type TSelectDailyOutputs = class
    protected
      _epiOffset: integer;
      _epiOutputs: TQBooleanVector;

      _zoneOffset: integer;
      _zoneOutputs: TQBooleanVector;

      _updated: boolean; // FIX ME: For now, always false.  Consider changing this when there is a UI for selected daily outputs

      _storeEpiOutputs: boolean;
      _storeZoneOutputs: boolean;

      _localEpiQuery, _remoteEpiQuery: string;
      _localZoneQuery, _remoteZoneQuery: string;

      function getStoreDailyOutputs(): boolean;

      procedure initialize();
      
    public
      constructor create( smdb: TSMDatabase ); overload;
      constructor create( const src: TSelectDailyOutputs; sim: TObject = nil ); overload;

      destructor destroy(); override;

      procedure debug();

      procedure populateDatabase( smdb: TSMDatabase ); // FIX ME: write this when there is a UI for these things.

      procedure initializeDBTables( smdb: TSMDatabase );

      procedure prepareQueries();
      procedure processRecords( smdb: TSMDatabase; const lastCompleteIteration: integer; const includeZones: boolean );

      property updated: boolean read _updated;
      property storeSelectDailyEpiOutputs: boolean read _storeEpiOutputs;
      property storeSelectDailyZoneOutputs: boolean read _storeZoneOutputs;
      property storeSelectDailyOutputs: boolean read getStoreDailyOutputs;
    end
  ;
  

implementation

  uses
    SysUtils,
    TypInfo,
    StrUtils,
    Variants,

    DebugWindow,
    MyStrUtils,
    SqlClasses,
    I88n,

    OutputDescriptions,
    RemoteDatabaseParams
  ;

  const DBSHOWMSG = false; // Set to true to enable debugging messages

  constructor TSelectDailyOutputs.create( smdb: TSMDatabase );
    var
      db: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      i: TDailyOutputType;
      s: string;
    begin
      inherited create();
      _epiOutputs := TQBooleanVector.create();
      _zoneOutputs := TQBooleanVector.create();
      initialize();

      db := smdb as TSqlDatabase;
      res := TSqlResult.create( db );

      // Build the epi query
      q := '';
      for i := DEtsdUSusc to DEappdUInfectious do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
          s := 's' + rightStr( s, length( s ) - 2 );
          q := q + '`' + s + '`,';
        end
      ;

      // Lop off that last comma
      q := leftStr( q, length( q ) - 1 );


      q := 'SELECT ' + q + ' FROM `inSelectDailyOutputs`';

      // Run the epi query
      res.runQuery( q );
      row := res.fetchArrayFirst();
      if( nil <> row ) then
        begin
          for i := DEtsdUSusc to DEappdUInfectious do
            begin
              s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
              s := 's' + rightStr( s, length( s ) - 2 );

              if( row.fieldExists( s ) ) then
                begin
                  if( null = row.field( s ) ) then
                    begin
                      dbcout( 'inSelectDailyOutputs field is null: ' + s, true );
                      _epiOutputs[ ord(i) - _epiOffset ] := false;
                    end
                  else
                    _epiOutputs[ ord(i) - _epiOffset ] := row.field( s )
                  ;
                  if( true = _epiOutputs[ ord(i) - _epiOffset ] ) then
                    _storeEpiOutputs := true
                  ;
                end
              else
                dbcout( 'Field "' + s + '" is missing in TSelectedDailyOutputs.create()', DBSHOWMSG )
              ;
            end
          ;
        end
      ;

      // Build the zone query
      q := '';
      for i := DZunitDaysInZone to DZanimalsInZone do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
          s := 's' + rightStr( s, length( s ) - 2 );
          q := q + '`' + s + '`,';
        end
      ;

      // Lop off that last comma
      q := leftStr( q, length( q ) - 1 );

      q := 'SELECT ' + q + ' FROM `inSelectDailyOutputs`';


      dbcout( endl + endl, DBSHOWMSG );
      dbcout( q, DBSHOWMSG );


      // Run the zone query
      res.runQuery( q );
      row := res.fetchArrayFirst();
      if( nil <> row ) then
        begin
          for i := DZunitDaysInZone to DZanimalsInZone do
            begin
              s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
              s := 's' + rightStr( s, length( s ) - 2 );

              if( row.fieldExists( s ) ) then
                begin
                  _zoneOutputs[ ord(i) - _zoneOffset ] := row.field( s );
                  if( true = _zoneOutputs[ ord(i) - _zoneOffset ] ) then
                    _storeZoneOutputs := true
                  ;
                end
              else
                dbcout( 'Field "' + s + '" is missing in TSelectedDailyOutputs.create()', DBSHOWMSG )
              ;
            end
          ;
        end
      ;
      
      res.Free();

      _updated := false;
    end
  ;


  constructor TSelectDailyOutputs.create( const src: TSelectDailyOutputs; sim: TObject = nil );
    begin
      // NOTE: the sim parameter is currently unused

      inherited create();

      _epiOutputs := TQBooleanVector.create( src._epiOutputs );
      _zoneOutputs := TQBooleanVector.create( src._zoneOutputs );

      _zoneOffset := src._zoneOffset;
      _epiOffset := src._epiOffset;

      _storeEpiOutputs := src._storeEpiOutputs;
      _storeZoneOutputs := src._storeZoneOutputs;

      _localEpiQuery := '';
      _remoteEpiQuery := '';
      _localZoneQuery := '';
      _remoteZoneQuery := '';

      _updated := src._updated;
    end
  ;


  procedure TSelectDailyOutputs.initialize();
    var
      i: TDailyOutputType;
    begin
      _zoneOffset := 0;
      _epiOffset := ord( DEtsdUSusc );

      _storeEpiOutputs := false;
      _storeZoneOutputs := false;

      _localEpiQuery := '';
      _remoteEpiQuery := '';
      _localZoneQuery := '';
      _remoteZoneQuery := '';

      for i := DEtsdUSusc to DEappdUInfectious do
        _epiOutputs.append( false )
      ;

      for i := DZunitDaysInZone to DZanimalsInZone do
        _zoneOutputs.append( false );
      ;
    end
  ;


  destructor TSelectDailyOutputs.destroy();
    begin
      freeAndNil( _epiOutputs );
      freeAndNil( _zoneOutputs );

      inherited destroy();
    end
  ;


  procedure TSelectDailyOutputs.debug();
    var
      i: TDailyOutputType;
      s: string;
    begin
      for i := DEtsdUSusc to DEappdUInfectious do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
          dbcout( s + ' ' + usBoolToText( _epiOutputs[ ord( i ) - _epiOffset ] ), true );
        end
      ;

      dbcout( endl, true );

      for i := DZunitDaysInZone to DZanimalsInZone do
        begin
          s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
          dbcout( s + ' ' + usBoolToText( _zoneOutputs[ ord( i ) - _zoneOffset ] ), true );
        end
      ;
    end
  ;

  
  procedure TSelectDailyOutputs.populateDatabase( smdb: TSMDatabase );
    begin
      // FIX ME: write this function when there is a UI for these things.

      _updated := false;
    end
  ;


  function TSelectDailyOutputs.getStoreDailyOutputs(): boolean;
    begin
      result := _storeEpiOutputs or _storeZoneOutputs;
    end
  ;


  procedure TSelectDailyOutputs.initializeDBTables( smdb: TSMDatabase );
    var
      q, s: string;
      i: TDailyOutputType;
    begin
      if( _storeEpiOutputs ) then
        begin
          // Build the epi table
          q := 'CREATE TABLE `outSelectDailyByProductionType`( `iteration` INTEGER, `day` INTEGER, `productionTypeID` INTEGER, ';
          for i := DEtsdUSusc to DEappdUInfectious do
            begin
              if( true = _epiOutputs[ ord(i) - _epiOffset] ) then
                begin
                  s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
                  s := rightStr( s, length( s ) - 2 );

                  if( i in [ DEdeswAAll, DEvacwAAll ] ) then
                    q := q + '`' + s + '` DOUBLE,'
                  else
                    q := q + '`' + s + '` LONG,'
                  ;

                  // Add the columns in the remote database, if necessary
                  if( remoteDBParams.useRemoteDatabase ) then
                    begin
                      if( i in [ DEdeswAAll, DEvacwAAll ] ) then
                        smdb.remoteExecute( 'ALTER TABLE `outSelectDailyByProductionType` ADD COLUMN `' + s + '` DOUBLE' )
                      else
                        smdb.remoteExecute( 'ALTER TABLE `outSelectDailyByProductionType` ADD COLUMN `' + s + '` INT UNSIGNED' )
                      ;
                    end
                  ;
                end
              ;
            end
          ;
          q := q + 'CONSTRAINT `outSelectDailyByProductionType_PK` PRIMARY KEY (`iteration`, `day`, `productionTypeID`) )';
          smdb.execute( q );
        end
      ;

      if( _storeZoneOutputs ) then
        begin
          // Build the zone table
          q := 'CREATE TABLE `outSelectDailyByZoneAndProductionType`( `iteration` INTEGER, `day` INTEGER, `zoneID` INTEGER, `productionTypeID` INTEGER, ';
          for i := DZunitDaysInZone to DZanimalsInZone do
            begin
              if( true = _zoneOutputs[ ord(i) - _zoneOffset] ) then
                begin
                  s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
                  s := rightStr( s, length( s ) - 2 );
                  q := q + '`' + s + '` LONG,';

                  // Add the columns in the remote database, if necessary
                  if( remoteDBParams.useRemoteDatabase ) then
                    smdb.remoteExecute( 'ALTER TABLE `outSelectDailyByZoneAndProductionType` ADD COLUMN `' + s + '` INT UNSIGNED' )
                  ;
                end
              ;
            end
          ;
          q := q + 'CONSTRAINT `outSelectDailyByZoneAndProductionType_PK` PRIMARY KEY (`iteration`, `day`, `zoneID`, `productionTypeID`) )';
          smdb.execute( q );
        end
      ;
    end
  ;


  procedure TSelectDailyOutputs.prepareQueries();
    var
      i: TDailyOutputType;
      q, s: string;
    begin
      _localEpiQuery := '';
      _remoteEpiQuery := '';
      _localZoneQuery := '';
      _remoteZoneQuery := '';

      if( storeSelectDailyEpiOutputs ) then
        begin
          q := '`iteration`, `day`, `productionTypeID`, ';
          for i := DEtsdUSusc to DEappdUInfectious do
            begin
              if( true = _epiOutputs[ ord(i) - _epiOffset] ) then
                begin
                  s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
                  s := rightStr( s, length( s ) - 2 );
                  q := q + '`' + s + '`,';
                end
              ;
            end
          ;
          q := leftStr( q, length( q ) - 1 );
          _localEpiQuery := q;
        end
      ;

      if( storeSelectDailyZoneOutputs ) then
        begin
          q := '`iteration`, `day`, `zoneID`, `productionTypeID`, ';
          for i := DZunitDaysInZone to DZanimalsInZone do
            begin
              if( true = _zoneOutputs[ ord(i) - _zoneOffset] ) then
                begin
                  s := GetEnumName( TypeInfo( TDailyOutputType ), ord(i) );
                  s := rightStr( s, length( s ) - 2 );
                  q := q + '`' + s + '`,';
                end
              ;
            end
          ;
          q := leftStr( q, length( q ) - 1 );
          _localZoneQuery := q;
        end
      ;
    end
  ;


  procedure TSelectDailyOutputs.processRecords( smdb: TSMDatabase; const lastCompleteIteration: integer; const includeZones: boolean );
    var
      q, rq, s: string;
      res: TSqlResult;
      row: TSqlRow;
      i, j: integer;
    begin
      if( not( remoteDBParams.useRemoteDatabase ) ) then // Handle the local database
        begin
          if( storeSelectDailyEpiOutputs ) then
            begin
              q := 'INSERT INTO `outSelectDailyByProductionType` (' + _localEpiQuery + ')'
                + ' SELECT ' + _localEpiQuery + ' FROM `outDailyByProductionType`'
                + ' WHERE `iteration` = ' + intToStr( lastCompleteIteration )
              ;

              smdb.execute( q );
            end
          ;

          if( storeSelectDailyZoneOutputs and includeZones ) then
            begin
              q := 'INSERT INTO `outSelectDailyByZoneAndProductionType` (' + _localZoneQuery + ')'
                + ' SELECT ' + _localZoneQuery + ' FROM `outDailyByZoneAndProductionType`'
                + ' WHERE `iteration` = ' + intToStr( lastCompleteIteration )
              ;

              smdb.execute( q );
            end
          ;
        end
      else // The remote database is a little more complicated...
        begin
          if( storeSelectDailyEpiOutputs ) then
            begin
              rq := '';

              res := TSqlResult.create(
                'SELECT ' + _localEpiQuery
                  + ' FROM `outDailyByProductionType`'
                  + ' WHERE `iteration` = ' + intToStr( lastCompleteIteration ),
                (smdb as TSqlDatabase)
              );

              row := res.fetchArrayFirst();
              i := 0;
              while( nil <> row ) do
                begin
                  rq := rq + '( ' + intToStr( remoteDBParams.jobID ) + ',';
                  s := '';
                  for j := 0 to row.numFields - 1 do
                    begin
                      if( null = row.field(j) ) then
                        s := s + DATABASE_NULL_VALUE + ','
                      else
                        begin
                          dbcout( j, DBSHOWMSG );
                          dbcout( row.field(j), DBSHOWMSG );
                          s := s + string( row.field(j) ) + ',';
                        end
                      ;
                    end
                  ;

                  s := leftStr( s, length( s ) - 1 );
                  rq := rq + s + '),';

                  if( 99 = i ) then
                    begin
                      rq := leftStr( rq, length( rq ) - 1 ); // Strip the last comma
                      smdb.remoteExecute(
                        'INSERT INTO `outSelectDailyByProductionType`'
                          + ' ( `jobID`, ' + _localEpiQuery + ' ) VALUES '
                          + rq
                      );
                      rq := '';
                      i := 0;
                    end
                  else
                    inc( i )
                  ;
                  row := res.fetchArrayNext();
                end
              ;

              if( 0 < length( rq ) ) then
                begin
                  rq := leftStr( rq, length( rq ) - 1 ); // Strip the last comma
                  smdb.remoteExecute(
                    'INSERT INTO `outSelectDailyByProductionType`'
                      + ' ( `jobID`, ' + _localEpiQuery + ' ) VALUES '
                      + rq
                  );
                end
              ;

              res.free();
            end
          ; // End of remote "epi" outputs


          // Do remote zone outputs
          //-----------------------
          if( storeSelectDailyZoneOutputs and includeZones ) then
            begin
              rq := '';

              res := TSqlResult.create(
                'SELECT ' + _localZoneQuery
                  + ' FROM `outDailyByZoneAndProductionType`'
                  + ' WHERE `iteration` = ' + intToStr( lastCompleteIteration ),
                (smdb as TSqlDatabase)
              );

              row := res.fetchArrayFirst();
              i := 0;
              while( nil <> row ) do
                begin
                  rq := rq + '( ' + intToStr( remoteDBParams.jobID ) + ',';
                  s := '';
                  for j := 0 to row.numFields - 1 do
                    begin
                      if( null = row.field(j) ) then
                        s := s + DATABASE_NULL_VALUE + ','
                      else
                        begin
                          dbcout( j, DBSHOWMSG );
                          dbcout( row.field(j), DBSHOWMSG );
                          s := s + string( row.field(j) ) + ',';
                        end
                      ;
                    end
                  ;

                  s := leftStr( s, length( s ) - 1 );
                  rq := rq + s + '),';

                  if( 99 = i ) then
                    begin
                      rq := leftStr( rq, length( rq ) - 1 ); // Strip the last comma
                      smdb.remoteExecute(
                        'INSERT INTO `outSelectDailyByZoneAndProductionType`'
                          + ' ( `jobID`, ' + _localZoneQuery + ' ) VALUES '
                          + rq
                      );
                      rq := '';
                      i := 0;
                    end
                  else
                    inc( i )
                  ;
                  row := res.fetchArrayNext();
                end
              ;

              if( 0 < length( rq ) ) then
                begin
                  rq := leftStr( rq, length( rq ) - 1 ); // Strip the last comma
                  smdb.remoteExecute(
                    'INSERT INTO `outSelectDailyByZoneAndProductionType`'
                      + ' ( `jobID`, ' + _localZoneQuery + ' ) VALUES '
                      + rq
                  );
                end
              ;

              res.free();
            end
          ; // End of remote zone outputs

        end
      ;
    end
  ;

end.