unit CustomOutputDefinitions;

(*
CustomeOuputDefinition.pas
--------------------------
Begin: 2006/10/21
Last revision: $Date: 2009-06-05 19:52:36 $ $Author: areeves $
Version number: $Revision: 1.11 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2009 Animal Population Health Institute, Colorado State University
                                       
This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    Contnrs,

    Models,
    SMDatabase
  ;


  type TVariableTypeCode = (
    VTUndefined,
    VTString,
    VTInteger,
    VTDouble
  );


  type TOutputFrequencyCode = (
    OFUndefined,
    OFIteration,
    OFProductionTypeIteration,
    OFZoneIteration,
    OFZoneProductionTypeIteration
  );


  type TCustomOutputDefinition = class( TModel )
    protected
      // Properties
      _defID: integer;
      _outputName: string;
      _variableType: TVariableTypeCode;
      _frequency: TOutputFrequencyCode;
      _sql: string;

      _removed: boolean;

      // Functions for internal use
      procedure initialize();

      // Housekeeping properties
      function getUpdated(): boolean; override;
      procedure setRemoved( val: boolean );

      // Useful properties
      procedure setDefID( val: integer );
      procedure setOutputName( val: string );
      procedure setVariableType( val: TVariableTypeCode );
      procedure setFrequency( val: TOutputFrequencyCode );
      procedure setSql( val: string );

      function getDefID(): integer;
      function getOutputName(): string;
      function getVariableType(): TVariableTypeCode;
      function getFrequency(): TOutputFrequencyCode;
      function getSql(): string;

    public
      constructor create(); overload;
      constructor create( const id: integer; const name: string ); overload;
      constructor create( const src: TCustomOutputDefinition; sim: TObject ); overload;
      destructor destroy(); override;

      procedure debug(); override;
      function validate( err: pstring = nil ): boolean; override;
      function ssXml(): string; override;

      function populateDatabase( db: TSMDatabase ): integer; reintroduce;

      property defID: integer read getDefID write setDefID;
      property outputName: string read getOutputName write setOutputName;
      property variableType: TVariableTypeCode read getVariableType write setVariableType;
      property frequency: TOutputFrequencyCode read getFrequency write setFrequency;
      property sql: string read getSql write setSql;

      property removed: boolean read _removed write setRemoved;
    end
  ;


  type TCustomOutputList = class( TModelList )
    protected
      // For typical list capability
      _currentIndex: integer;

      procedure setObject( index: integer; item: TCustomOutputDefinition );
      function getObject( index: integer ): TCustomOutputDefinition;

      // Useful internal functions
      function getUpdated(): boolean;
      function getIsValid(): boolean;

    public
      constructor create( smdb: TSMDatabase ); overload;
      constructor create( const src: TCustomOutputList; sim: TObject ); overload;
      destructor destroy(); override;

      procedure populateDatabase( db: TSMDatabase );

      procedure debug(); override;
      function validate( err: pstring = nil ): boolean; override;
      function functionsAreValid(): boolean; override;
      
      function findDefinition( dfnName: string ): TCustomOutputDefinition;

      function hasOutputs( const f: TOutputFrequencyCode ): boolean;

      property updated: boolean read getUpdated;
      property isValid: boolean read getIsValid;

      // Typical list functions
      function append( dm: TCustomOutputDefinition ): integer; reintroduce;
      procedure insert( index: integer; dm: TCustomOutputDefinition );
      property objects[ index: integer]: TCustomOutputDefinition read getObject write setObject; default;
      function first(): TCustomOutputDefinition;
      function last(): TCustomOutputDefinition;
      function next(): TCustomOutputDefinition;
      function at( i: integer ): TCustomOutputDefinition;
      function current(): TCustomOutputDefinition;
    end
  ;

  function variableTypeToString( val: TVariableTypeCode ): string;
  function frequencyTypeToString( val: TOutputFrequencyCode ): string;
  function variableTypeFromCode( val: string ): TVariableTypeCode;
  function variableTypeToCode( val: TVariableTypeCode ): string;

implementation

  uses
    SysUtils,

    DebugWindow,
    SqlClasses,
    MyStrUtils
  ;

  const
    DBSHOWMSG = false; // Set to true to enable debugging messages for this unit


//-----------------------------------------------------------------------------
// Global helper functions
//-----------------------------------------------------------------------------
  function variableTypeToString( val: TVariableTypeCode ): string;
    begin
      case val of
        VTString: result := 'VARCHAR(255)';
        VTInteger: result := 'INTEGER';
        VTDouble: result := 'DOUBLE';
        VTUndefined:
          begin
            raise exception.create( 'Unrecognized variable type code in variableTypeToString' );
            result := 'Undefined';
          end;
      end;
    end
  ;


  function frequencyTypeToString( val: TOutputFrequencyCode ): string;
    begin
      case val of
        OFUndefined: result := 'Undefined';
        OFIteration: result := 'Iteration';
        OFProductionTypeIteration: result := 'ProductionTypeIteration';
        OFZoneIteration: result := 'ZoneIteration';
        OFZoneProductionTypeIteration: result := 'ZoneProductionTypeIteration';
        else
          begin
            raise exception.create( 'Unrecognized frequency code in frequencyTypeToString' );
            result := 'Undefined';
          end
        ;
      end;
    end
  ;


  function variableTypeFromCode( val: string ): TVariableTypeCode;
    begin
      val := fixup( val );

      if( 's' = val ) then
        result := VTString
      else if( 'i' = val ) then
        result := VTInteger
      else if( 'd' = val ) then
        result := VTDouble
      else
        begin
          raise exception.create( 'Unrecognized variable type code in variableTypeFromCode' );
          result := VTUndefined;
        end
      ;
    end
  ;


  function variableTypeToCode( val: TVariableTypeCode ): string;
    begin
      case val of
        VTString: result := 'S';
        VTInteger: result := 'I';
        VTDouble: result := 'D';
        VTUndefined:
          begin
            raise exception.create( 'Unrecognized variable type code in variableTypeToCode' );
            result := 'U';
          end
        ;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputDefinition: creation/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TCustomOutputDefinition.create();
    begin
      inherited create();
      initialize();
    end
  ;

  constructor TCustomOutputDefinition.create( const id: integer; const name: string );
    begin
      inherited create();
      initialize();

      _defID := id;
      _outputName := name;

      _updated := true;
    end
  ;

  constructor TCustomOutputDefinition.create( const src: TCustomOutputDefinition; sim: TObject );
    begin
      inherited create( src as TModel );
      _sim := sim;

      _defID := src._defID;
      _outputName := src._outputName;
      _variableType := src._variableType;
      _frequency := src._frequency;
      _sql := src._sql;
      
      _removed := src._removed;
    end
  ;


  procedure TCustomOutputDefinition.initialize();
    begin
      _defID := -1;
      _outputName := '';
      _variableType := VTUndefined;
      _frequency := OFUndefined;
      _sql := '';

      _updated := false;
      _removed := false;
    end
  ;


  destructor TCustomOutputDefinition.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputDefinition: Database population
//-----------------------------------------------------------------------------
  function TCustomOutputDefinition.populateDatabase( db: TSMDatabase ): integer;
    var
      q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['outputName'] := db.sqlQuote( outputName );
      dict['outputTypeCode'] := db.sqlQuote( variableTypeToCode( variableType ) );
      dict['sql'] := db.sqlQuote( sql );

      case frequency of
        OFIteration: dict['outputFrequencyCode'] := '"II"';
        OFProductionTypeIteration: dict['outputFrequencyCode'] := '"IP"';
        OFZoneIteration: dict['outputFrequencyCode'] := '"IZ"';
        OFZoneProductionTypeIteration: dict['outputFrequencyCode'] := '"ZP"';
      end;

      if( 0 < defID ) then
        begin
          q := writeQuery(
            'inCustomOutputDefinitions',
            QUpdate,
            dict,
            'WHERE `defID` = ' + intToStr( defID )
          );
          db.execute( q );
        end
      else
        begin
          q := writeQuery(
            'inCustomOutputDefinitions',
            QInsert,
            dict
          );
          db.execute( q );
          _defID := db.lastInsertID;
        end
      ;

      _updated := false;
      _removed := false;

      dict.free();

      result := defID;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputDefinition: Debugging and validation
//-----------------------------------------------------------------------------
  procedure TCustomOutputDefinition.debug();
    begin
      dbcout( '-----BEGIN TCustomOutputDefinition', true );
      dbcout( 'ID: ' + intToStr( _defID ), true );
      dbcout( 'Name: ' + _outputName, true );
      dbcout( 'Variable type: ' + variableTypeToString( _variableType ), true );
      dbcout( 'Frequency: ' + frequencyTypeToString( _frequency ), true );
      dbcout( 'SQL: ' + _sql, true );
      dbcout( 'Updated: ' + usBoolToText( _updated ), true );
      dbcout( 'Removed: ' + usBoolToText( _removed ), true );
      dbcout( '------END TCustomOutputDefinition', true );
    end
  ;

  function TCustomOutputDefinition.validate( err: pstring = nil ): boolean;
    begin
      // FIX ME: write a real validation function
      result := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputDefinition: fake XML function
//-----------------------------------------------------------------------------
  function TCustomOutputDefinition.ssXml(): string;
    begin
      result := '';
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputDefinition: Properties
//-----------------------------------------------------------------------------
  procedure TCustomOutputDefinition.setDefID( val: integer ); begin _defID := val; _updated := true; end;
  procedure TCustomOutputDefinition.setOutputName( val: string ); begin _outputName := val; _updated := true; end;
  procedure TCustomOutputDefinition.setVariableType( val: TVariableTypeCode ); begin _variableType := val; _updated := true; end;
  procedure TCustomOutputDefinition.setFrequency( val: TOutputFrequencyCode ); begin _frequency := val; _updated := true; end;
  procedure TCustomOutputDefinition.setSql( val: string ); begin _sql := val; _updated := true; end;

  function TCustomOutputDefinition.getDefID(): integer; begin result := _defID; end;
  function TCustomOutputDefinition.getOutputName(): string; begin result := _outputName; end;
  function TCustomOutputDefinition.getVariableType(): TVariableTypeCode; begin result := _variableType; end;
  function TCustomOutputDefinition.getFrequency(): TOutputFrequencyCode; begin result := _frequency; end;
  function TCustomOutputDefinition.getSql(): string; begin result := _sql; end;

  function TCustomOutputDefinition.getUpdated(): boolean; begin result := _updated; end;
  procedure TCustomOutputDefinition.setRemoved( val: boolean ); begin _removed := val; _updated := true; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TCustomOutputList.create( smdb: TSMDatabase );
    var
      c: TCustomOutputDefinition;
      db: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      query: string;
    begin
      inherited create( true );

      db := smdb as TSqlDatabase;

      query := 'SELECT'
        + ' `defID`,'
        + ' `outputName`,'
        + ' `outputTypeCode`,'
        + ' `outputFrequencyCode`,'
        + ' `sql`'
        + ' FROM `inCustomOutputDefinitions`'
      ;

      res := TSqlResult.create( query, db );
      row := res.fetchArrayFirst();
      while( row <> nil ) do
      	begin
          c := TCustomOutputDefinition.create();
          c.defID := row.field('defID');
          c.outputName := row.field('outputName');

          if( 'II' = row.field('outputFrequencyCode') ) then
            c.frequency := OFIteration
          else if( 'IP' = row.field('outputFrequencyCode') ) then
            c.frequency := OFProductionTypeIteration
          else if( 'IZ' = row.field('outputFrequencyCode') ) then
            c.frequency := OFZoneIteration
          else if( 'ZP' = row.field('outputFrequencyCode') ) then
            c.frequency := OFZoneProductionTypeIteration
          else
            c.frequency := OFUndefined
          ;

          c.variableType := variableTypeFromCode( row.field('outputTypeCode') );
          c.sql := row.field('sql');

          c._updated := false;

          self.append( c );
          row := res.fetchArrayNext();
        end
      ;

      freeAndNil( res );
    end
  ;


  constructor TCustomOutputList.create( const src: TCustomOutputList; sim: TObject );
    var
      c, d: TCustomOutputDefinition;
    begin
      inherited create( true );

      c := src.first();

      while( nil <> c ) do
        begin
          d := TCustomOutputDefinition.create( c, sim );
          self.append( d );
          c := src.next();
        end
      ;
    end
  ;


  destructor TCustomOutputList.destroy();
    begin
      dbcout( '*** Destroying TCustomOutputList', DBSHOWMSG );
      // The base class takes care of this.
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: Database population
//-----------------------------------------------------------------------------
  procedure TCustomOutputList.populateDatabase( db: TSMDatabase );
  	var
    	d: TCustomOutputDefinition;
      q: string;
  	begin
      d := self.first();

      while( d <> nil ) do
        begin
          d.debug();

          if( d.removed ) then
            begin
              if( 0 < d.defID ) then
                begin
                  q := 'DELETE FROM `inCustomOutputDefinitions` WHERE `defID` = ' + intToStr( d.defID );
                  db.execute( q );
                end
              ;
              
              self.Remove( d );
              d := self.current();
            end
          else
            begin
              if( d.updated ) then d.populateDatabase( db );
              d := self.next();
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: properties
//-----------------------------------------------------------------------------
  function TCustomOutputList.hasOutputs( const f: TOutputFrequencyCode ): boolean;
    var
      d: TCustomOutputDefinition;
    begin
      result := false;
      d := self.first();

      while( nil <> d ) do
        begin
          if( f = d.frequency ) then
            begin
              result := true;
              break;
            end
          ;
          d := self.next();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: Debugging  and validation
//-----------------------------------------------------------------------------
  procedure TCustomOutputList.debug();
    var
      d: TCustomOutputDefinition;
    begin
      dbcout( '----- Custom output list debug...', true );
      d := self.first();

      while( nil <> d ) do
        begin
          d.debug();
          d := self.next();
        end
      ;
      dbcout( '----- Done with custom output list.' + endl, true );
    end
  ;

  
  function TCustomOutputList.validate( err: pstring = nil ): boolean;
    var
      d: TCustomOutputDefinition;
    begin
      result := true;

      d := self.first();

      while( nil <> d ) do
        begin
          if( not( d.validate( err ) ) ) then
            begin
              result := false;
              break;
            end
          ;
          d := self.next();
        end
      ;
    end
  ;


  function TCustomOutputList.getIsValid(): boolean;
    begin
      result := validate( nil );
    end
  ;


  function TCustomOutputList.functionsAreValid(): boolean;
    begin
      // There are currently no functions to validate to TCustomOutputList.
      result := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: properties
//-----------------------------------------------------------------------------
  function TCustomOutputList.getUpdated(): boolean;
    var
      d: TCustomOutputDefinition;
    begin
      result := false;

      d := self.first();

      while( nil <> d ) do
        begin
          if( d.updated ) then
            begin
              result := true;
              break;
            end
          ;
          d := self.next();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: Special data handling
//-----------------------------------------------------------------------------
  function TCustomOutputList.findDefinition( dfnName: string ): TCustomOutputDefinition;
    var
      d: TCustomOutputDefinition;
    begin
      result := nil;
      
      d := self.first();

      while( nil <> d ) do
        begin
          if( fixup( dfnName ) = fixup( d.outputName ) ) then
            begin
              result := d;
              break;
            end
          ;
          d := self.next();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TCustomOutputList: Typical list functions
//-----------------------------------------------------------------------------
  function TCustomOutputList.append( dm: TCustomOutputDefinition ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TCustomOutputList.setObject( index: integer; item: TCustomOutputDefinition );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TCustomOutputList.getObject( index: integer ): TCustomOutputDefinition;
    begin
      result := inherited GetItem( index ) as TCustomOutputDefinition;
    end
  ;


  procedure TCustomOutputList.insert(index: integer; dm: TCustomOutputDefinition);
    begin
      inherited Insert(index, dm);
    end
  ;


  function TCustomOutputList.first() : TCustomOutputDefinition;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TCustomOutputList.last() : TCustomOutputDefinition;
    begin
      if( self.Count = 0 ) then
      	result := nil
      else
        begin
          _currentIndex := self.Count - 1;
          result := getObject( _currentIndex );
        end
      ;
    end
  ;


  function TCustomOutputList.next() : TCustomOutputDefinition;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TCustomOutputList.current() : TCustomOutputDefinition;
    begin
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TCustomOutputList.at( i: integer ): TCustomOutputDefinition;
  	begin
      if( i > self.Count-1 ) then
      	raise exception.Create( 'Index out of bounds in TCustomOutputList' )
      else
      	result := getObject( i )
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.

