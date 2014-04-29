unit SMOutputOptions;

(*
SMOutputOptions.pas
-------------------
Begin: 2005/09/01
Last revision: $Date: 2008/03/12 22:10:53 $ $Author: areeves $
Version number: $Revision: 1.15 $
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
    SMDatabase,
    Models
  ;

  type TSMOutputOptions = class( TModel )
    protected
      _updated: boolean;

      _writeDailyStatesFile: boolean;
      _dailyStatesFileName: string;
      _saveAllDailyOutputs: boolean;
      _dailyOutputsForIterations: integer;
      _saveDailyExposures: boolean;
      _saveDailyEvents: boolean;

      // Outputs for NAADSMap (C. Chioino's GIS application)
      _writeMapOutput: boolean;
      _mapOutputDir: string;

      procedure initialize();

      // For properties
      procedure setWriteDailyStatesFile( val: boolean );
      procedure setDailyStatesFileName( val: string );
      procedure setSaveAllDailyOutputs( val: boolean );
      procedure setDailyOutputsForIterations( val: integer );
      procedure setSaveDailyExposures( val: boolean );
      procedure setSaveDailyEvents( val: boolean );

      function getWriteDailyStatesFile(): boolean;
      function getDailyStatesFileName(): string;
      function getSaveAllDailyOutputs(): boolean;
      function getDailyOutputsForIterations(): integer;
      function getSaveDailyExposures(): boolean;
      function getSaveDailyEvents(): boolean;

      // NAADSMap properties
      procedure setWriteMapOutput( val: boolean );
      procedure setMapOutputDir( val: string );
      function getMapOutputDir(): string;

      function getUpdated(): boolean; override;

    public
      constructor create(); overload;
      constructor create( const src: TSMOutputOptions; sim: TObject ); overload;
      constructor create( db: TSMDatabase ); overload;

      destructor destroy(); override;

      procedure populateDatabase( db: TSMDatabase; update: boolean = false ); reintroduce;

      function validate( err: PString = nil ): boolean; override;

      procedure debug(); override;

      property writeDailyStatesFile: boolean read getWriteDailyStatesFile write setWriteDailyStatesFile;
      property dailyStatesFileName: string read getDailyStatesFileName write setDailyStatesFileName;
      property saveAllDailyOutputs: boolean read getSaveAllDailyOutputs write setSaveAllDailyOutputs;
      property dailyOutputsForIterations: integer read getDailyOutputsForIterations write setDailyOutputsForIterations;
      property saveDailyExposures: boolean read getSaveDailyExposures write setSaveDailyExposures;
      property saveDailyEvents: boolean read getSaveDailyEvents write setSaveDailyEvents;

      property writeNAADSMapOutput: boolean read _writeMapOutput write setWriteMapOutput;
      property NAADSMapOutputDirectory: string read getMapOutputDir write setMapOutputDir;

      property updated: boolean read getUpdated;
    end
  ;

  const
    DBSMOUTPUTOPTIONS: boolean = false; // Set to true to enable debugging for this unit.

implementation

  uses
    SysUtils,
    Variants,
    WindowsUtils,
    
    MyStrUtils,
    USStrUtils,
    DebugWindow,
    SqlClasses,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TSMOutputOptions.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor TSMOutputOptions.create( db: TSMDatabase );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      inherited create();
      initialize();

      db2 := db as TSqlDatabase;

      q := 'SELECT '
        + ' saveAllDailyOutputs,'
        + ' saveDailyOutputsForIterations,'
        + ' writeDailyStatesFile,'
        + ' dailyStatesFileName,'
        + ' saveDailyEvents,'
        + ' saveDailyExposures,'
        + ' writeNAADSMapOutput,'
        + ' NAADSMapDirectory'
        + ' FROM inGeneral'
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( nil <> row ) then
        begin
          if( null <> row.field('writeDailyStatesFile') ) then _writeDailyStatesFile := boolean( row.field('writeDailyStatesFile') );
          if( null <> row.field('dailyStatesFileName') ) then _dailyStatesFileName := row.field('dailyStatesFileName');
          if( null <> row.field('saveAllDailyOutputs') ) then _saveAllDailyOutputs := boolean( row.field('saveAllDailyOutputs') );
          if( null <> row.field('saveDailyOutputsForIterations') ) then _dailyOutputsForIterations := integer( row.field('saveDailyOutputsForIterations') );
          if( null <> row.field('saveDailyEvents') ) then _saveDailyEvents := boolean( row.field('saveDailyEvents') );
          if( null <> row.field('saveDailyExposures') ) then _saveDailyExposures := boolean( row.field('saveDailyExposures') );

          if( null <> row.field('writeNAADSMapOutput') ) then _writeMapOutput := boolean( row.field('writeNAADSMapOutput') );
          if( null <> row.field('NAADSMapDirectory') ) then _mapOutputDir := row.field('NAADSMapDirectory');
        end
      ;

      res.Free();
    end
  ;


  constructor TSMOutputOptions.create( const src: TSMOutputOptions; sim: TObject );
    begin
      inherited create( src );
      _sim := sim;

      _writeDailyStatesFile := src._writeDailyStatesFile;
      _dailyStatesFileName := src._dailyStatesFileName;
      _saveAllDailyOutputs := src._saveAllDailyOutputs;
      _dailyOutputsForIterations := src._dailyOutputsForIterations;
      _saveDailyEvents := src._saveDailyEvents;
      _saveDailyExposures := src._saveDailyExposures;

      _writeMapOutput := src._writeMapOutput;
      _mapOutputDir := src._mapOutputDir;

      _updated := src._updated;
    end
  ;


  procedure TSMOutputOptions.initialize();
    begin
      _writeDailyStatesFile := false;
      _dailyStatesFileName := '';
      _saveAllDailyOutputs := false;
      _dailyOutputsForIterations := 3;
      _saveDailyEvents := false;
      _saveDailyExposures := false;

      _writeMapOutput := false;
      _mapOutputDir := '';

      _updated := false;
    end
  ;


  destructor TSMOutputOptions.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



  procedure TSMOutputOptions.populateDatabase( db: TSMDatabase; update: boolean = false );
    var
      qDict: TQueryDictionary;
      q: string;
    begin
      qDict := TQueryDictionary.create();

      qDict['saveAllDailyOutputs'] := boolToStr( saveAllDailyOutputs );
      qDict['saveDailyOutputsForIterations'] := intToStr( dailyOutputsForIterations );
      qDict['writeDailyStatesFile'] := boolToStr( writeDailyStatesFile );
      qDict['saveDailyEvents'] := boolToStr( _saveDailyEvents );
      qDict['saveDailyExposures'] := boolToStr( _saveDailyExposures );

      if( 0 < length( trim( dailyStatesFileName ) ) ) then
        qDict['dailyStatesFileName'] := db.sqlQuote( dailyStatesFileName )
      ;

      qDict['writeNAADSMapOutput'] := boolToStr( writeNAADSMapOutput );

      if( 0 < length( trim( NAADSMapOutputDirectory ) ) ) then
        qDict['NAADSMapDirectory'] := db.sqlQuote( NAADSMapOutputDirectory )
      else
        qDict['NAADSMapDirectory'] := DATABASE_NULL_VALUE
      ;

      if( update ) then
        q := writeQuery( 'inGeneral', QUpdate, qDict )
      else
        q := writeQuery( 'inGeneral', QInsert, qDict )
      ;

      dbcout( q, DBSMOUTPUTOPTIONS );

      db.execute( q );

      _updated := false;

      qDict.free();
    end
  ;


  function TSMOutputOptions.validate( err: PString = nil ): boolean;
    begin
      result := true;

      if( writeDailyStatesFile ) then
        begin
          if ( 0 = length( dailyStatesFileName ) ) then
            begin
              result := false;
              if( nil <> err ) then err^ := tr( 'File name for daily states file is unspecified.' );
            end
          ;

          // Can the selected directory be written to?
          if( not( canWriteToDirectory( directory( dailyStatesFileName ) ) ) ) then
            begin
              result := false;
              if( nil <> err ) then err^ := tr( 'The daily states file cannot be written in the specified directory.' );
            end
          ;
        end
      ;



      if( writeNAADSMapOutput ) then
        begin
          if( 0 = length( NAADSMapOutputDirectory ) ) then
            begin
              result := false;
              if( nil <> err ) then err^ := tr( 'File name for NAADSMap file is unspecified.' );
            end
          ;

          // Can the selected directory be written to?
          if( not( canWriteToDirectory( NAADSMapOutputDirectory ) ) ) then
            begin
              result := false;
              if( nil <> err ) then err^ := tr( 'The folder selected for NAADSMap output cannot be written to.' );
            end
          ;
        end
      ;
    end
  ;


//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TSMOutputOptions.debug();
    begin
      dbcout( endl + '--Begin output options:', true );
      dbcout( '_writeDailyStatesFile: ' + boolToStr( _writeDailyStatesFile ), true );
      dbcout( '_dailyStatesFileName: ' + _dailyStatesFileName, true );
      dbcout( '_saveAllDailyOutputs: ' + boolToStr( _saveAllDailyOutputs ), true );
      dbcout( '_dailyOutputsForIterations: ' + intToStr( _dailyOutputsForIterations ), true );
      dbcout( '_saveDailyEvents: ' + boolToStr( _saveDailyEvents ), true );
      dbcout( '_saveDailyExposures: ' + boolToStr( _saveDailyExposures ), true );

      dbcout( '_writeMapOutput: ' + boolToStr( _writeMapOutput ), true );
      dbcout( '_mapOutputDir: ' + _mapOutputDir, true );

      dbcout( '--End output options' + endl, true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TSMOutputOptions.setWriteDailyStatesFile( val: boolean ); begin _writeDailyStatesFile := val; _updated := true; end;
  procedure TSMOutputOptions.setDailyStatesFileName( val: string ); begin _dailyStatesFileName := val; _updated := true; end;
  procedure TSMOutputOptions.setSaveAllDailyOutputs( val: boolean ); begin _saveAllDailyOutputs := val; _updated := true; end;
  procedure TSMOutputOptions.setDailyOutputsForIterations( val: integer ); begin _dailyOutputsForIterations := val; _updated := true; end;
  procedure TSMOutputOptions.setSaveDailyExposures( val: boolean ); begin _saveDailyExposures := val; _updated := true; end;
  procedure TSMOutputOptions.setSaveDailyEvents( val: boolean ); begin _saveDailyEvents := val; _updated := true; end;

  function TSMOutputOptions.getWriteDailyStatesFile(): boolean; begin result := _writeDailyStatesFile; end;
  function TSMOutputOptions.getDailyStatesFileName(): string; begin result := trim( _dailyStatesFileName ); end;
  function TSMOutputOptions.getSaveAllDailyOutputs(): boolean; begin result := _saveAllDailyOutputs; end;
  function TSMOutputOptions.getDailyOutputsForIterations(): integer; begin result := _dailyOutputsForIterations; end;
  function TSMOutputOptions.getSaveDailyExposures(): boolean; begin result := _saveDailyExposures; end;
  function TSMOutputOptions.getSaveDailyEvents(): boolean; begin result := _saveDailyEvents; end;

  // NAADSMap properties
  procedure TSMOutputOptions.setWriteMapOutput( val: boolean ); begin _writeMapOutput := val; _updated := true; end;
  procedure TSMOutputOptions.setMapOutputDir( val: string ); begin _mapOutputDir := val; _updated := true; end;
  function TSMOutputOptions.getMapOutputDir(): string; begin result := trim( _mapOutputDir ); end;

  function TSMOutputOptions.getUpdated(): boolean; begin result := _updated; end;
//-----------------------------------------------------------------------------


end.
