unit HerdRandomizationOptions;

(*
HerdRandomizationOptions.pas
----------------------------
Begin: 2013/08/03
Last revision: 
Version number: 
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QLists,
    
    Models,
    ModelDatabase,
    
    SMDatabase
  ;

  type THerdRandomizationSettings = (
    HerdRNDCanBeSelected,
    HerdRNDCannotBeSelected,
    HerdRNDNoSelection
  );


  type THerdRandomizationOptions = class( TModel )
    protected
      _updated: boolean;

      _initInfectedRandomize: boolean;
      
      _initInfectedSelectedProdTypes: boolean;
      _initInfectedProdTypes: TQIntegerList;
      
      _initInfectedStateNumbers: TQIntegerList;
      
      _initInfectedUseRange: boolean;
      _initInfectedLatNW: double;
      _initInfectedLonNW: double;
      _initInfectedLatSE: double;
      _initInfectedLonSE: double;

      procedure initialize();

      // For properties
      procedure setInitInfectedRandomize( val: boolean );
      
      procedure setInitInfectedSelectedProdTypes( val: boolean );

      procedure setInitInfectedUseRange( val: boolean );
      procedure setInitInfectedLatNW( val: double );
      procedure setInitInfectedLonNW( val: double );
      procedure setInitInfectedLatSE( val: double );
      procedure setInitInfectedLonSE( val: double );

      function getInitInfectedRandomize(): boolean;
      
      function getInitInfectedSelectedProdTypes(): boolean;
      function getInitInfectedProdTypes(): TQIntegerList;
      
      function getInitInfectedStateNumbers(): TQIntegerList;

      function getInitInfectedUseRange(): boolean;
      function getInitInfectedLatNW(): double;
      function getInitInfectedLonNW(): double;
      function getInitInfectedLatSE(): double;
      function getInitInfectedLonSE(): double;

      function getUpdated(): boolean; override;

    public
      constructor create(); overload;
      constructor create( const src: THerdRandomizationOptions; sim: TObject ); overload;
      constructor create( db: TSMDatabase ); overload;

      destructor destroy(); override;

      procedure assign( src: THerdRandomizationOptions );

      procedure populateDatabase( db: TSMDatabase ); reintroduce;

      function ssXml( const indent: integer ): string; reintroduce;

      procedure removeProductionType( const ptID: integer );

      function validate( hList: TObject; err: PString = nil ): boolean; reintroduce;

      procedure debug(); override;

      // Properties and property-like functions
      //---------------------------------------
      procedure setInitInfectedProdTypesFromString( const str: string );
      procedure setInitInfectedStateNumbersFromString( const str: string );

      property initInfectedRandomize: boolean read getInitInfectedRandomize write setInitInfectedRandomize;
      
      property initInfectedSelectedProdTypes: boolean read getInitInfectedSelectedProdTypes write setInitInfectedSelectedProdTypes;
      property initInfectedProdTypes: TQIntegerList read getInitInfectedProdTypes;
      
      property initInfectedStateNumbers: TQIntegerList read getInitInfectedStateNumbers;
      
      property initInfectedUseRange: boolean read getInitInfectedUseRange write setInitInfectedUseRange;
      property initInfectedLatNW: double read getInitInfectedLatNW write setInitInfectedLatNW;
      property initInfectedLonNW: double read getInitInfectedLonNW write setInitInfectedLonNW;
      property initInfectedLatSE: double read getInitInfectedLatSE write setInitInfectedLatSE;
      property initInfectedLonSE: double read getInitInfectedLonSE write setInitInfectedLonSE;

      property updated: boolean read getUpdated;
    end
  ;

  const
    DBSHOWMSG: boolean = true; // Set to true to enable debugging for this unit.

implementation

  uses
    Types,
    SysUtils,
    Variants,
    Math,
    WindowsUtils,

    BasicGIS,
    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    Herd
  ;
  

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor THerdRandomizationOptions.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor THerdRandomizationOptions.create( db: TSMDatabase );
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
        + ' initInfectedRandomize,'
        + ' initInfectedSelectedProdTypes,'
        + ' initInfectedProdTypes,'
        + ' initInfectedStateNumbers,'
        + ' initInfectedUseRange,'
        + ' initInfectedLatNW,'
        + ' initInfectedLonNW,'
        + ' initInfectedLatSE,'
        + ' initInfectedLonSE'
        + ' FROM inGeneral'
      ;
      
      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( nil <> row ) then
        begin
          if( null <> row.field('initInfectedRandomize') ) then _initInfectedRandomize := boolean( row.field('initInfectedRandomize') );
          
          if( null <> row.field('initInfectedSelectedProdTypes') ) then _initInfectedSelectedProdTypes := boolean( row.field('initInfectedSelectedProdTypes') );
          if( null <> row.field('initInfectedProdTypes') ) then _initInfectedProdTypes.setFromString( row.field('initInfectedProdTypes') );
          
          if( null <> row.field('initInfectedStateNumbers') ) then _initInfectedStateNumbers.setFromString( row.field('initInfectedStateNumbers') );

          if( null <> row.field('initInfectedUseRange') ) then _initInfectedUseRange := boolean( row.field('initInfectedUseRange') );
          if( null <> row.field('initInfectedLatNW') ) then _initInfectedLatNW := double( row.field('initInfectedLatNW') );
          if( null <> row.field('initInfectedLonNW') ) then _initInfectedLonNW := double( row.field('initInfectedLonNW') );
          if( null <> row.field('initInfectedLatSE') ) then _initInfectedLatSE := double( row.field('initInfectedLatSE') );
          if( null <> row.field('initInfectedLonSE') ) then _initInfectedLonSE := double( row.field('initInfectedLonSE') );
        end
      ;

      res.Free();

      self.debug();
    end
  ;


  constructor THerdRandomizationOptions.create( const src: THerdRandomizationOptions; sim: TObject );
    begin
      inherited create( src );
      initialize();
      _sim := sim;

      assign( src );
    end
  ;

  procedure THerdRandomizationOptions.assign( src: THerdRandomizationOptions );
    begin
      _initInfectedRandomize := src._initInfectedRandomize;

      _initInfectedSelectedProdTypes := src._initInfectedSelectedProdTypes;
      _initInfectedProdTypes.assign( src._initInfectedProdTypes );

      _initInfectedStateNumbers.assign( src._initInfectedStateNumbers );

      _initInfectedUseRange := src._initInfectedUseRange;
      _initInfectedLatNW := src._initInfectedLatNW;
      _initInfectedLonNW := src._initInfectedLonNW;
      _initInfectedLatSE := src._initInfectedLatSE;
      _initInfectedLonSE := src._initInfectedLonSE;

      _updated := src._updated;
    end
  ;


  procedure THerdRandomizationOptions.initialize();
    begin
      _initInfectedRandomize := false;
      
      _initInfectedSelectedProdTypes := false;
      _initInfectedProdTypes := TQIntegerList.create();

      _initInfectedStateNumbers := TQIntegerList.create();

      // By default, there is one latent unit.
      _initInfectedStateNumbers.setFromString( '1,0,0,0,0' );

      _initInfectedUseRange := false;
      _initInfectedLatNW := NaN;
      _initInfectedLonNW := NaN;
      _initInfectedLatSE := NaN;
      _initInfectedLonSE := NaN;

      _updated := false;
    end
  ;


  destructor THerdRandomizationOptions.destroy();
    begin
      _initInfectedProdTypes.free();
      _initInfectedStateNumbers.free();
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



  procedure THerdRandomizationOptions.populateDatabase( db: TSMDatabase );
    var
      qDict: TQueryDictionary;
      q: string;
    begin
      qDict := TQueryDictionary.create();

      qDict['initInfectedRandomize'] := usBoolToText( initInfectedRandomize );

      qDict['initInfectedSelectedProdTypes'] := usBoolToText( initInfectedSelectedProdTypes ); 
      qDict['initInfectedProdTypes'] := db.sqlQuote( initInfectedProdTypes.asString() );

      qDict['initInfectedStateNumbers'] := db.sqlQuote( initInfectedStateNumbers.asString() );

      qDict['initInfectedUseRange'] := usBoolToText( initInfectedUseRange ); 

      if( gisValidLat( initInfectedLatNW ) ) then
        qDict['initInfectedLatNW'] := usFloatToStr( initInfectedLatNW )
      else
        qDict['initInfectedLatNW'] := DATABASE_NULL_VALUE
      ;

      if( gisValidLon( initInfectedLonNW ) ) then
        qDict['initInfectedLonNW'] := usFloatToStr( initInfectedLonNW )
      else
        qDict['initInfectedLonNW'] := DATABASE_NULL_VALUE
      ;

      if( gisValidLat( initInfectedLatSE ) ) then
        qDict['initInfectedLatSE'] := usFloatToStr( initInfectedLatSE )
      else
        qDict['initInfectedLatSE'] := DATABASE_NULL_VALUE
      ;

      if( gisValidLon( initInfectedLonSE ) ) then
        qDict['initInfectedLonSE'] := usFloatToStr( initInfectedLonSE )
      else
        qDict['initInfectedLonSE'] := DATABASE_NULL_VALUE
      ;

      // This class always updates an existing record.  It never inserts a new one.
      q := writeQuery( 'inGeneral', QUpdate, qDict );

      dbcout( q, DBSHOWMSG );

      db.execute( q );

      _updated := false;

      qDict.free();
    end
  ;


  function THerdRandomizationOptions.ssXml( const indent: integer ): string;
    var
      str: string;
      indentStr: string;
      i: integer;
    begin
      str := '';

      indentStr := '';
      for i := 0 to indent - 1 do
        indentStr := indentStr + ' '
      ;

      str := str + indentStr + '<randomize-initially-infected-herds>' + usBoolToText( self.initInfectedRandomize ) + '</randomize-initially-infected-herds>' + endl;

      if( self.initInfectedRandomize ) then
        begin
          str := str + indentStr + '<num-initially-latent-units>' + intToStr( self.initInfectedStateNumbers.at(0) ) + '</num-initially-latent-units>' + endl;
          str := str + indentStr + '<num-initially-subclinical-units>' + intToStr( self.initInfectedStateNumbers.at(1) ) + '</num-initially-subclinical-units>' + endl;
          str := str + indentStr + '<num-initially-clinical-units>' + intToStr( self.initInfectedStateNumbers.at(2) ) + '</num-initially-clinical-units>' + endl;
          str := str + indentStr + '<num-initially-naturally-immune-units>' + intToStr( self.initInfectedStateNumbers.at(3) ) + '</num-initially-naturally-immune-units>' + endl;
          str := str + indentStr + '<num-initially-vaccine-immune-units>' + intToStr( self.initInfectedStateNumbers.at(4) ) + '</num-initially-vaccine-immune-units>' + endl;
        end
      ;

      result := str;
    end
  ;


  procedure THerdRandomizationOptions.removeProductionType( const ptID: integer );
    var
      list: TQIntegerList;
      i: integer;
    begin
      list := TQIntegerList.create( _initInfectedProdTypes );

      _initInfectedProdTypes.clear();

      for i := 0 to list.count - 1 do
        begin
          if( ptID <> list.at(i) ) then
            _initInfectedProdTypes.append( i )
          ;
        end
      ;

      list.Free();
    end
  ;


  function THerdRandomizationOptions.validate( hList: TObject; err: PString = nil ): boolean;
    var
      totalInitiallyInfected: integer;
      nMatches: integer;
      it: THerdListIterator;
    begin
      result := true; // Until shown otherwise.

      // Are we using randomization at all?  If not, don't bother with anything else.
      if( not( initInfectedRandomize ) ) then
        begin
          exit;
        end
      ;

      // If we are doing randomization...

      // How many total herds does the user want to be infected?
      totalInitiallyInfected := initInfectedStateNumbers.at(0) + initInfectedStateNumbers.at(1) + initInfectedStateNumbers.at(2);

      if( 0 = totalInitiallyInfected ) then
        begin
          result := false;
          if( nil <> err ) then err^ := err^ + tr( 'No initially infected (latent, subclinical, or clinical) units are specified.' ) + endl;
          exit;
        end
      ;

      // Has the user requested any production types?  If so, at least one
      // production type must be selected.
      if( initInfectedSelectedProdTypes ) then
        begin
          if( 0 = initInfectedProdTypes.count ) then
            begin
              result := false;
              if( nil <> err ) then err^ := err^ + tr( 'No production types are specified for the initial unit randomization requirements.' ) + endl;
              exit;
            end
          ;
        end
      ;

      // Has the user specified a legitimate geographical range?
      if( initInfectedUseRange ) then
        begin
          if
            ( equalsValue = compareValue( 0.0, abs( initInfectedLatNW - initInfectedLatSE ) ) )
          or
            ( equalsValue = compareValue( 0.0, abs( initInfectedLonNW - initInfectedLonSE ) ) )
          then
            begin
              result := false;
              if( nil <> err ) then err^ := err^ + tr( 'Geographic range has an area of zero and is is invalid for the initial unit randomization requirements.' ) + endl;
              exit;
            end
          ;
        end
      ;

      if( nil = hList ) then
        raise exception.create( 'hList is nil in THerdRandomizationOptions.validate()' )
      ;

      // If the user has requested infected units, then there must be at least that many
      // units that meet the geographic and production type parameters.
      // Iterate over the herd list until this number has been found.
      nMatches := 0;

      it := THerdListIterator.create( hList as THerdList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( it.current().meetsRandomizationCriteria( self ) ) then
            inc( nMatches )
          ;

          if( nMatches = totalInitiallyInfected ) then
            break
          ;

          it.incr();
        end
      ;

      it.Free();

      if( nMatches < totalInitiallyInfected ) then
        begin
          result := false;
          if( nil <> err ) then err^ := err^ + tr( 'Too few units match the specified initial unit randomization requirements.' ) + endl;
          exit;
        end
      ;
    end
  ;


//------------------------------------------------------------------------------
// Debugging
//------------------------------------------------------------------------------
  procedure THerdRandomizationOptions.debug();
    begin
      dbcout( endl + '--Begin herd randomization options:', true );
      
      dbcout( 'initInfectedRandomize: ' + dbStr( _initInfectedRandomize ), true );
      
      dbcout( 'initInfectedSelectedProdTypes: ' + dbStr( _initInfectedSelectedProdTypes ), true );
      dbcout( 'initInfectedProdTypes: ' + _initInfectedProdTypes.asString(), true );
      
      dbcout( 'initInfectedStateNumbers: ' + _initInfectedStateNumbers.asString(), true );
      
      dbcout( 'initInfectedUseRange: ' + dbStr( _initInfectedUseRange ), true );
      dbcout( 'initInfectedLatNW: ' + dbStr( _initInfectedLatNW ), true );
      dbcout( 'initInfectedLonNW: ' + dbStr( _initInfectedLonNW ), true );
      dbcout( 'initInfectedLatSE: ' + dbStr( _initInfectedLatSE ), true );
      dbcout( 'initInfectedLonSE: ' + dbStr( _initInfectedLonSE ), true );

      dbcout( '--End herd randomization options' + endl, true );
    end
  ;
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// Properties
//------------------------------------------------------------------------------
  procedure THerdRandomizationOptions.setInitInfectedRandomize( val: boolean );
    begin
      if( val <> _initInfectedRandomize ) then
        begin
          _initInfectedRandomize := val;
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedSelectedProdTypes( val: boolean );
    begin
      if( val <> _initInfectedSelectedProdTypes ) then
        begin
          _initInfectedSelectedProdTypes := val;
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedUseRange( val: boolean );
    begin
      if( val <> _initInfectedUseRange ) then
        begin
          _initInfectedUseRange := val;
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedLatNW( val: double );
    begin
      if( isNaN( val ) or isNaN( _initInfectedLatNW ) ) then
        begin
          _initInfectedLatNW := val;
          _updated := true;
        end
      else if( val <> _initInfectedLatNW ) then
        begin
          _initInfectedLatNW := val;
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedLonNW( val: double );
    begin
      if( isNaN( val ) or isNaN( _initInfectedLonNW ) ) then
        begin
          _initInfectedLonNW := val;
          _updated := true;
        end
      else if( val <> _initInfectedLonNW ) then
        begin
          _initInfectedLonNW := val;
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedLatSE( val: double );
    begin
      if( isNaN( val ) or isNaN( _initInfectedLatSE ) ) then
        begin
          _initInfectedLatSE := val;
          _updated := true;
        end
      else if( val <> _initInfectedLatSE ) then
        begin
          _initInfectedLatSE := val;
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedLonSE( val: double );
    begin
      if( isNaN( val ) or isNaN( _initInfectedLonSE ) ) then
        begin
          _initInfectedLonSE := val;
          _updated := true;
        end
      else if( val <> _initInfectedLonSE ) then
        begin
          _initInfectedLonSE := val;
          _updated := true;
        end
      ;
    end
  ;

  procedure THerdRandomizationOptions.setInitInfectedProdTypesFromString( const str: string );
    var
      test: string;
    begin
      test := initInfectedProdTypes.asString();
      if( test <> str ) then
        begin
          initInfectedProdTypes.setFromString( str );
          _updated := true;
        end
      ;
    end
  ;


  procedure THerdRandomizationOptions.setInitInfectedStateNumbersFromString( const str: string );
    var
      test: string;
    begin
      test := initInfectedStateNumbers.asString();
      if( test <> str ) then
        begin
          initInfectedStateNumbers.setFromString( str );
          _updated := true;
        end
      ;
    end
  ;


  function THerdRandomizationOptions.getInitInfectedRandomize(): boolean; begin result := _initInfectedRandomize; end;

  function THerdRandomizationOptions.getInitInfectedSelectedProdTypes(): boolean; begin result := _initInfectedSelectedProdTypes; end;
  function THerdRandomizationOptions.getInitInfectedProdTypes(): TQIntegerList; begin result := _initInfectedProdTypes; end;

  function THerdRandomizationOptions.getInitInfectedStateNumbers(): TQIntegerList; begin result := _initInfectedStateNumbers; end;

  function THerdRandomizationOptions.getInitInfectedUseRange(): boolean; begin result := _initInfectedUseRange; end;
  function THerdRandomizationOptions.getInitInfectedLatNW(): double; begin result := _initInfectedLatNW; end;
  function THerdRandomizationOptions.getInitInfectedLonNW(): double; begin result := _initInfectedLonNW; end;
  function THerdRandomizationOptions.getInitInfectedLatSE(): double; begin result := _initInfectedLatSE; end;
  function THerdRandomizationOptions.getInitInfectedLonSE(): double; begin result := _initInfectedLonSE; end;

  function THerdRandomizationOptions.getUpdated(): boolean; begin result := _updated; end;
//-----------------------------------------------------------------------------


end.  
  