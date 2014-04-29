unit SMSimulationStats;

(*
SMSimulationStats.pas
---------------------
Begin: 2005/07/28
Last revision: $Date: 2008/10/16 19:26:05 $ $Author: areeves $
Version number: $Revision: 1.23 $
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
  	Contnrs,

    QOrderedDictionaries,
    QVectors,
    SqlClasses,
    CStringList,

    SMDatabase
  ;


  {*
   FIX ME: consider deriving this class from TDoubleArray
  }
  type TSimulationResultSet = class( TObject )
    protected
      _ptID: integer;

      _mean: extended;
      _median: extended;
      _stddev: extended;
      _low: extended;
      _high: extended;
      _p5: extended;
      _p25: extended;
      _p50: extended;
      _p75: extended;
      _p95: extended;

      _name: string;
      _hint: string;

      _incompleteDataset: boolean;
      _dataCount: integer;

      _values: TQDoubleVector;

      procedure initialize();

      function getMean(): extended;
      function getStddev(): extended;
      function getLow(): extended;
      function getHigh(): extended;
      function getP5(): extended;
      function getP25(): extended;
      function getP50(): extended;
      function getP75(): extended;
      function getP95(): extended;

      function getDataCount(): integer;

      procedure calculate();
      procedure removeInvalidValues( dest, src: TQDoubleVector );

    public
      constructor create( nameStr: string; hintStr: string = ''; incompleteDataset: boolean = false ); overload;
      constructor create( ptID: integer; nameStr: string; hintStr: string = ''; incompleteDataset: boolean = false ); overload;
      destructor destroy(); override;

      class function createCalculationNameList(): TCStringList;
      class function calculationCount(): integer;

      function calculationAtPosition( i: integer ): double;

      procedure debug();

      property mean: extended read getMean;
      property median: extended read getP50;
      property stddev: extended read getStddev;
      property low: extended read getLow;
      property high: extended read getHigh;
      property p5: extended read getP5;
      property p25: extended read getP25;
      property p50: extended read getP50;
      property p75: extended read getP75;
      property p95: extended read getP95;

      property dataCount: integer read getDataCount;

      property name: string read _name;
      property hint: string read _hint;

      property values: TQDoubleVector read _values write _values;

      property isIncompleteDataset: boolean read _incompleteDataset;
    end
  ;


  {*
   FIX ME: I don't like this class at all.  It needs work.
  }
  type TSMIterationOutputArraySet = class( TObject )
    protected
      _ptID: integer;
      _ptDescr: string;
      _smdb: TSMDatabase;

      procedure initialize();

      procedure setPtID( val: integer );
      procedure setPtDescr( val: string );
      function getPtID(): integer;
      function getPtDescr(): string;

      function getArraySize(): integer;
      
    public
      // Running totals for each disease state
      tscUSusc: TSimulationResultSet;
      tscASusc: TSimulationResultSet;
      tscULat: TSimulationResultSet;
      tscALat: TSimulationResultSet;
      tscUSubc: TSimulationResultSet;
      tscASubc: TSimulationResultSet;
      tscUClin: TSimulationResultSet;
      tscAClin: TSimulationResultSet;
      tscUNImm: TSimulationResultSet;
      tscANImm: TSimulationResultSet;
      tscUVImm: TSimulationResultSet;
      tscAVImm: TSimulationResultSet;
      tscUDest: TSimulationResultSet;
      tscADest: TSimulationResultSet;

      // Running totals for cause of infection
      infcUIni: TSimulationResultSet;
      infcAIni: TSimulationResultSet;
      infcUAir: TSimulationResultSet;
      infcAAir: TSimulationResultSet;
      infcUDir: TSimulationResultSet;
      infcADir: TSimulationResultSet;
      infcUInd: TSimulationResultSet;
      infcAInd: TSimulationResultSet;
      infcUTotal: TSimulationResultSet;
      infcATotal: TSimulationResultSet;

      // Running totals for exposures
      expcUDir: TSimulationResultSet;
      expcADir: TSimulationResultSet;
      expcUInd: TSimulationResultSet;
      expcAInd: TSimulationResultSet;
      expcUTotal: TSimulationResultSet;
      expcATotal: TSimulationResultSet;

      // Running totals for traces
      trcUDir: TSimulationResultSet;
      trcADir: TSimulationResultSet;
      trcUInd: TSimulationResultSet;
      trcAInd: TSimulationResultSet;
      trcUDirp: TSimulationResultSet;
      trcADirp: TSimulationResultSet;
      trcUIndp: TSimulationResultSet;
      trcAIndp: TSimulationResultSet;

      // Running totals for detection
      detcUClin: TSimulationResultSet;
      detcAClin: TSimulationResultSet;

      // Running totals for destruction
      descUIni: TSimulationResultSet;
      descAIni: TSimulationResultSet;
      descUDet: TSimulationResultSet;
      descADet: TSimulationResultSet;
      descUDir: TSimulationResultSet;
      descADir: TSimulationResultSet;
      descUInd: TSimulationResultSet;
      descAInd: TSimulationResultSet;
      descURing: TSimulationResultSet;
      descARing: TSimulationResultSet;
      descUTotal: TSimulationResultSet;
      descATotal: TSimulationResultSet;

      // Running totals for vaccination
      vaccUIni: TSimulationResultSet;
      vaccAIni: TSimulationResultSet;
      vaccURing: TSimulationResultSet;
      vaccARing: TSimulationResultSet;

      // These two aren't really necessary, since ring vaccination is
      // currently the only reason for vaccination.
      //vaccUTotal: TSimulationResultSet;
      //vaccATotal: TSimulationResultSet;

      // Running totals for zone foci
      zoncFoci: TSimulationResultSet;

      // First events
      firstDetection: TSimulationResultSet;
      firstVaccination: TSimulationResultSet;
      firstDestruction: TSimulationResultSet;

      // Outbreak duration
      diseaseDuration: TSimulationResultSet;
      outbreakDuration: TSimulationResultSet;
      
      constructor create(); overload;
      constructor create( db: TSMDatabase; ptID: integer; ptDescr: string ); overload;
      constructor create( const original: TSMIterationOutputArraySet ); overload;
      destructor destroy(); override;

      procedure assign( const original: TSMIterationOutputArraySet );

      procedure appendRecordsFrom( row: TSqlRow );
      procedure sumFrom( arr: TSMIterationOutputArraySet );

      class function createEpiOutputDictionary(): TQOrderedStringStringDictionary;

      class function epiOutputCount(): integer;

      function resultSetAt( arrPos: integer ): TSimulationResultSet;
      function valueAt( arrPos, calcPos: integer ): double;
      function valueAsString( arrPos, calcPos: integer ): string;

      property productionTypeID: integer read getPtID write setPtID;
      property productionType: string read getPtDescr write setPtDescr;

      property arraySize: integer read getArraySize;
    end
  ;



  type TSMIterationOutputSuperList = class( TObjectList )
    protected
      _db: TSMDatabase;

      // For basic list operations
      //---------------------------
      _currentIndex: integer;

      function writeBasicQuery(): string;
      //function writeJoinQuery(): string;

      // For basic list operations
      //---------------------------
      procedure setObject( index: integer; item: TSMIterationOutputArraySet );
      function getObject( index: integer ): TSMIterationOutputArraySet;

    public
      constructor create( db: TSMDatabase );
      destructor destroy(); override;

      function arraysForProductionType( const ptID: integer ): TSMIterationOutputArraySet;

      // Basic list functions
      //----------------------
      function append( dm: TSMIterationOutputArraySet ): integer;
      procedure insert( index: integer; dm: TSMIterationOutputArraySet );

      function first(): TSMIterationOutputArraySet;
      function last(): TSMIterationOutputArraySet;
      function next(): TSMIterationOutputArraySet;
      function current(): TSMIterationOutputArraySet;
      function at( const i: integer ): TSMIterationOutputArraySet;

      property objects[index: integer]: TSMIterationOutputArraySet read getObject write setObject; default;
    end
  ;


  const
    UNSET: integer = -1;

  	DBSMSIMULATIONSTATS: boolean = false; // set to true to enable debugging messages for this unit.

implementation

  uses
    SysUtils,
    Math,
    Variants,

    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    I88n,

    ARMath
  ;


//-----------------------------------------------------------------------------
// TSimulationResultSet
//-----------------------------------------------------------------------------
  constructor TSimulationResultSet.create( nameStr: string; hintStr: string = ''; incompleteDataset: boolean = false );
    begin
      inherited create();

      initialize();

      _name := nameStr;
      _hint := hintStr;
      _incompleteDataset := incompleteDataset;
    end
  ;


  constructor TSimulationResultSet.create( ptID: integer; nameStr: string; hintStr: string = ''; incompleteDataset: boolean = false );
    begin
      inherited create();

      initialize();

      _ptID := ptID;
      _name := nameStr;
      _hint := hintStr;
      _incompleteDataset := incompleteDataset;
    end
  ;


  procedure TSimulationResultSet.initialize();
    begin
      _ptID := -1;
      _name := '';
      _hint := '';
      _incompleteDataset := false;

      _values := TQDoubleVector.Create();

      _mean := UNSET;
      _stddev := UNSET;
      _low := UNSET;
      _high := UNSET;
      _p5 := UNSET;
      _p25 := UNSET;
      _p50 := UNSET;
      _p75 := UNSET;
      _p95 := UNSET;

      _dataCount := UNSET;
    end
  ;


  destructor TSimulationResultSet.destroy();
    begin
      dbcout( 'Destroying array values for ' + _name + '...', DBSMSIMULATIONSTATS );
      _values.Free();  
      dbcout( 'Done.', DBSMSIMULATIONSTATS );
      inherited destroy();
    end
  ;


  class function TSimulationResultSet.createCalculationNameList(): TCStringList;
    var
      colHeaders: TCStringList;
    begin
      colHeaders := TCStringList.Create();
      with colHeaders do
        begin
          append( tr( 'Mean' ) );
          append( tr( 'StdDev' ) );
          append( tr( 'Low' ) );
          append( tr( 'High' ) );
          append( tr( 'p5' ) );
          append( tr( 'p25' ) );
          append( tr( 'p50' ) );
          append( tr( 'p75' ) );
          append( tr( 'p95' ) );
        end
      ;

      result := colHeaders;
    end
  ;


  function TSimulationResultSet.calculationAtPosition( i: integer ): double;
    begin
      case i of
        0: result := self.mean;
        1:
          begin
            if isNaN( self.stddev ) then
              result := NaN
            else
              result := self.stddev
            ;
          end
        ;
        2: result := self.low;
        3: result := self.high;
        4: result := self.p5;
        5: result := self.p25;
        6: result := self.p50;
        7: result := self.p75;
        8: result := self.p95;
        else
          raise exception.Create( 'Index out of bounds in TSimulationResultSet.calculationAtPosition()' );
      end;
    end
  ;


  class function TSimulationResultSet.calculationCount(): integer;
    begin
      result := 9;
    end
  ;


  procedure TSimulationResultSet.calculate();
    var
      tempArr: TQDoubleVector;
      i: integer;
    begin
      dbcout( 'Beginning calculation', DBSMSIMULATIONSTATS );

      // Don't sort values directly in the array: we'll need
      // the array just as it is to display convergence. Create
      // and sort a copy instead.
      if( not( _incompleteDataset ) ) then
        tempArr := TQDoubleVector.Create( _values )
      else
        begin
          tempArr := TQDoubleVector.Create();
          removeInvalidValues( tempArr, _values )
        end
      ;


      dbcout( 'Elements in array: ' + intToStr( tempArr.Count ), DBSMSIMULATIONSTATS );

      if ( 0 = tempArr.Count ) then
        begin
          _mean := NaN;
          _stddev := NaN;
          _low := NaN;
          _high := NaN;
          _p5 := NaN;
          _p25 := NaN;
          _p50 := NaN;
          _p75 := NaN;
          _p95 := NaN;
          exit;
        end
      ;
      
      if ( 1 = tempArr.Count ) then
        begin
          dbcout( 'XXXXXXXXXXXXXXX stddev will be undefined', DBSMSIMULATIONSTATS );
          dbcout( 'Setting mean to ' + uiFloatToStr( tempArr.at(0) ), DBSMSIMULATIONSTATS  );
          _mean := tempArr.at(0);
          _stddev := NaN;
        end
      else
        begin
          tempArr.Sort();
          dbcout( 'stddev should be defined', DBSMSIMULATIONSTATS );
          _mean := tempArr.mean();
          _stddev := tempArr.stddev();
        end
      ;

      if( DBSMSIMULATIONSTATS ) then
        begin
          dbcout( '*** in calculate function', DBSMSIMULATIONSTATS );
          for i := 0 to tempArr.Count - 1 do
            dbcout( tempArr[i], DBSMSIMULATIONSTATS )
          ;
        end
      ;

      dbcout( 'Setting high and low', DBSMSIMULATIONSTATS );
      _low := tempArr.at(0);
      _high := tempArr.at( tempArr.Count - 1 );

      dbcout( 'Getting quantiles', DBSMSIMULATIONSTATS );
      _p5 := tempArr.quantile( 0.05 );
      _p25 := tempArr.quantile( 0.25 );
      _p50 := tempArr.quantile( 0.50 );
      _p75 := tempArr.quantile( 0.75 );
      _p95 := tempArr.quantile( 0.95 );

      dbcout( 'Calculation was successful', DBSMSIMULATIONSTATS );
      tempArr.Free();
    end
  ;


  procedure TSimulationResultSet.removeInvalidValues( dest, src: TQDoubleVector );
    var
      i: integer;
      arr: TQDoubleVector;
    begin
      dbcout( '+++++++++++++++++++++++Starting removing invalid values', DBSMSIMULATIONSTATS );
      _dataCount := 0;
      arr := TQDoubleVector.Create();

      dbcout( 'Array ' + self.name + ' for PT ' + intToStr( _ptID ) + ': ' + intToStr( src.Count ) + ' elements', DBSMSIMULATIONSTATS );

      for i := 0 to src.Count - 1 do
        begin
          dbcout( 'Index ' + intToStr( i ) + ', value ' +  uiFloatToStr( src[i] ), DBSMSIMULATIONSTATS );
          if( 0 <= src[i] ) then
            begin
              arr.append( src[i] );
              inc( _dataCount );
            end
          ;
        end
      ;
      dest.Assign( arr );

      dbcout( '**************************Done removing invalid values', DBSMSIMULATIONSTATS );
      arr.Free();
    end
  ;


  function TSimulationResultSet.getMean(): extended;
    begin
      //dbcout( 'Getting mean for ' + self.name, DBSMSIMULATIONSTATS );
      //dbcout( 'mean is ' + uiFloatToStr( _mean ), DBSMSIMULATIONSTATS );

      if( not( isNaN( _mean ) ) ) then
        begin
          //dbcout( 'Does UNSET = _mean?', DBSMSIMULATIONSTATS );
          //dbcout( UNSET = _mean, DBSMSIMULATIONSTATS );
          
          if( UNSET = _mean ) then
            begin
              dbcout( 'Calculating for mean', DBSMSIMULATIONSTATS );
              calculate();
            end
          ;
        end
      ;
      //dbcout( 'Mean is ' + uiFloatToStr( _mean ), DBSMSIMULATIONSTATS );
      result := _mean;
      //dbcout( 'result is ' + uiFloatToStr( result ), DBSMSIMULATIONSTATS );
    end
  ;


  function TSimulationResultSet.getStddev(): extended;
    begin
      dbcout( 'Getting stddev for ' + self.name, DBSMSIMULATIONSTATS );
      if( not( isNaN( _mean ) ) ) then
        begin
          if( not( isNaN( _stddev ) ) ) then
            if( UNSET = _stddev ) then calculate()
          ;
        end
      else
        dbcout( 'Calculation is done!', DBSMSIMULATIONSTATS )
      ;

      if( isNaN( _stddev ) ) then
        result := NaN
      else
        result := _stddev
      ;
    end
  ;


  function TSimulationResultSet.getLow(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _low ) then calculate();
        end
      ;
      result := _low;
    end
  ;


  function TSimulationResultSet.getHigh(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _high ) then calculate();
        end
      ;
      result := _high;
    end
  ;


  function TSimulationResultSet.getP5(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _p5 ) then calculate();
        end
      ;
      result := _p5;
    end
  ;


  function TSimulationResultSet.getP25(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _p25 ) then calculate();
        end
      ;
      result := _p25;
    end
  ;


  function TSimulationResultSet.getP50(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _p50 ) then calculate();
        end
      ;
      result := _p50;
    end
  ;


  function TSimulationResultSet.getP75(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _p75 ) then calculate();
        end
      ;
      result := _p75;
    end
  ;


  function TSimulationResultSet.getP95(): extended;
    begin
      if( not( isNaN( _mean ) ) ) then
        begin
          if( UNSET = _p95 ) then calculate();
        end
      ;
      result := _p95;
    end
  ;

  
  function TSimulationResultSet.getDataCount(): integer;
    begin
      if( _incompleteDataset ) then
        begin
          if( UNSET = _dataCount ) then calculate();
          result := _dataCount;
        end
      else
        result := _values.Count
      ;
    end
  ;


  procedure TSimulationResultSet.debug();
    var
      i: integer;
    begin
      dbcout( '---------- Begin TSimulationResultSet', true );
      dbcout( 'Items in array: ' + intToStr( _values.Count ), true );

      for i := 0 to _values.Count - 1 do
        dbcout( _values[i], true )
      ;

      dbcout( '---------- End TSimulationResultSet', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMIterationOutputArraySet
//-----------------------------------------------------------------------------
  constructor TSMIterationOutputArraySet.create();
    begin
      inherited create();
       initialize();
    end
  ;


  constructor TSMIterationOutputArraySet.create( db: TSMDatabase; ptID: integer; ptDescr: string );
    begin
      inherited create();

        _ptID := ptID;
       initialize();

      _smdb := db;
      _ptDescr := ptDescr;
    end
  ;


  {*
    WARNING: As currently implemented, calculations made in instances of
    TSimulationResultSet are not copied by this copy constructor.  Only the
    raw data in the arrays is copied. If this becomes a problem,
    TSimulationResultSet will need a copy constructor of its own.
  }
  constructor TSMIterationOutputArraySet.create( const original: TSMIterationOutputArraySet );
    begin
      inherited create();
      _ptID := original.productionTypeID;
      initialize();
      assign( original );
    end
  ;

  procedure TSMIterationOutputArraySet.initialize();
    begin
      // Running totals for each disease state
      self.tscUSusc := TSimulationResultSet.create( _ptID, 'tscUSusc', tr( 'Number of units that are or become susceptible over the course of an iteration' ) );
      self.tscASusc := TSimulationResultSet.create( _ptID, 'tscASusc', tr( 'Total number of animals in all units that are or become susceptible over the course of an iteration' ) );
      self.tscULat := TSimulationResultSet.create( _ptID, 'tscULat', tr( 'Number of units that are or become latent over the course of an iteration' ) );
      self.tscALat := TSimulationResultSet.create( _ptID, 'tscALat', tr( 'Total number of animals in all units that are or become latent over the course of an iteration' ) );
      self.tscUSubc := TSimulationResultSet.create( _ptID, 'tscUSubc', tr( 'Number of units that are or become subclinically infectious over the course of an iteration' ) );
      self.tscASubc := TSimulationResultSet.create( _ptID, 'tscASubc', tr( 'Total number of animals in all units that are or become infectious over the course of an iteration' ) );
      self.tscUClin := TSimulationResultSet.create( _ptID, 'tscUClin', tr( 'Number of units that are or become clinical over the course of an iteration' ) );
      self.tscAClin := TSimulationResultSet.create( _ptID, 'tscAClin', tr( 'Total number of animals in all units that are or become clinical over the course of an iteration' ) );
      self.tscUNImm := TSimulationResultSet.create( _ptID, 'tscUNImm', tr( 'Number of units that are or become naturally immune over the course of an iteration' ) );
      self.tscANImm := TSimulationResultSet.create( _ptID, 'tscANImm', tr( 'Total number of animals in all units that are or become naturally immune over the course of an iteration' ) );
      self.tscUVImm := TSimulationResultSet.create( _ptID, 'tscUVImm', tr( 'Number of units that are or become vaccine immune over the course of an iteration' ) );
      self.tscAVImm := TSimulationResultSet.create( _ptID, 'tscAVImm', tr( 'Total number of animals in all units that are or become vaccine immune over the course of an iteration' ) );
      self.tscUDest := TSimulationResultSet.create( _ptID, 'tscUDest', tr( 'Number of units that are destroyed over the course of an iteration' ) );
      self.tscADest := TSimulationResultSet.create( _ptID, 'tscADest', tr( 'Total number of animals in all units that are destroyed over the course of an iteration' ) );

      // Running totals for cause of infection
      self.infcUIni := TSimulationResultSet.create( _ptID, 'infcUIni', tr( 'Number of units that are initially infected at the beginning of an iteration' ) );
      self.infcAIni := TSimulationResultSet.create( _ptID, 'infcAIni', tr( 'Number of animals in units that are initially infected at the beginning of an iteration' ) );
      self.infcUAir := TSimulationResultSet.create( _ptID, 'infcUAir', tr( 'Number of units that become infected by airborne spread over the course of an iteration' ) );
      self.infcAAir := TSimulationResultSet.create( _ptID, 'infcAAir', tr( 'Number of animals in units that become infected by airborne spread over the course of an iteration' ) );
      self.infcUDir := TSimulationResultSet.create( _ptID, 'infcUDir', tr( 'Number of units that become infected by direct contact over the course of an iteration' ) );
      self.infcADir := TSimulationResultSet.create( _ptID, 'infcADir', tr( 'Number of animals in units that become infected by direct contact over the course of an iteration' ) );
      self.infcUInd := TSimulationResultSet.create( _ptID, 'infcUInd', tr( 'Number of units that become infected by indirect contact over the course of an iteration' ) );
      self.infcAInd := TSimulationResultSet.create( _ptID, 'infcAInd', tr( 'Number of animals in units that become infected by indirect contact over the course of an iteration' ) );
      self.infcUTotal := TSimulationResultSet.create( _ptID, 'infcUTotal', tr( 'Total number of units that become infected over the course of an iteration' ) );
      self.infcATotal := TSimulationResultSet.create( _ptID, 'infcATotal', tr( 'Total number of animals in units that become infected over the course of an iteration' ) );

      // Running totals for exposures
      self.expcUDir := TSimulationResultSet.create( _ptID, 'expcUDir', tr( 'Total number of units directly exposed to any infected unit over the course of an iteration' ) );
      self.expcADir := TSimulationResultSet.create( _ptID, 'expcADir', tr( 'Total number of animals in units directly exposed to any infected unit over the course of an iteration' ) );
      self.expcUInd := TSimulationResultSet.create( _ptID, 'expcUInd', tr( 'Total number of units indirectly exposed to any infected unit over the course of an iteration' ) );
      self.expcAInd := TSimulationResultSet.create( _ptID, 'expcAInd', tr( 'Total number of animals in units indirectly exposed to any infected unit over the course of an iteration' ) );
      self.expcUTotal := TSimulationResultSet.create( _ptID, 'expcUTotal', tr( 'Total number units exposed by any contact to any infected unit over the course of an iteration' ) );
      self.expcATotal := TSimulationResultSet.create( _ptID, 'expcATotal', tr( 'Total number of animals in units exposed by any contact to any infected unit over the course of an iteration' ) );

      // Running totals for traces
      self.trcUDir := TSimulationResultSet.create( _ptID, 'trcUDir', tr( 'Number of units directly exposed and successfully traced over the course of an iteration' ) );
      self.trcADir := TSimulationResultSet.create( _ptID, 'trcADir', tr( 'Total number of animals in all units directly exposed and successfully traced over the course of an iteration' ) );
      self.trcUInd := TSimulationResultSet.create( _ptID, 'trcUInd', tr( 'Number of units indirectly exposed and successfully traced over the course of an iteration' ) );
      self.trcAInd := TSimulationResultSet.create( _ptID, 'trcAInd', tr( 'Total number of animals in all units indirectly exposed and successfully traced over the course of an iteration' ) );
      self.trcUDirp := TSimulationResultSet.create( _ptID, 'trcUDirp', tr( 'Number of units directly exposed that could possibly have been traced over the course of an iteration' ) );
      self.trcADirp := TSimulationResultSet.create( _ptID, 'trcADirp', tr( 'Total number of animals in all units directly exposed that could possibly have been traced over the course of an iteration' ) );
      self.trcUIndp := TSimulationResultSet.create( _ptID, 'trcUIndp', tr( 'Number of units indirectly exposed that could possibly have been traced over the course of an iteration' ) );
      self.trcAIndp := TSimulationResultSet.create( _ptID, 'trcAIndp', tr( 'Total number of animals in units indirectly exposed that could possibly have been traced over the course of an iteration' ) );

      // Running totals for detection
      self.detcUClin := TSimulationResultSet.create( _ptID, 'detcUClin', tr( 'Number of units detected by clinical signs over the course of an iteration' ) );
      self.detcAClin := TSimulationResultSet.create( _ptID, 'detcAClin', tr( 'Total number of animals in all units detected by clinical signs over the course of an iteration' ) );

      // Running totals for destruction
      self.descUIni := TSimulationResultSet.create( _ptID, 'descUIni', tr( 'Number of units destroyed prior to the start of the simulation' ) );
      self.descAIni := TSimulationResultSet.create( _ptID, 'descAIni', tr( 'Total number of animals in units destroyed prior to the start of the simulation' ) );
      self.descUDet := TSimulationResultSet.create( _ptID, 'descUDet', tr( 'Number of units destroyed because they were detected positive over the course of an iteration' ) );
      self.descADet := TSimulationResultSet.create( _ptID, 'descADet', tr( 'Total number of animals in all units destroyed because they were detected positive over the course of an iteration' ) );
      self.descUDir := TSimulationResultSet.create( _ptID, 'descUDir', tr( 'Number of units destroyed because they were direct traces over the course of an iteration' ) );
      self.descADir := TSimulationResultSet.create( _ptID, 'descADir', tr( 'Total number of animals in units destroyed because they were direct traces over the course of an iteration' ) );
      self.descUInd := TSimulationResultSet.create( _ptID, 'descUInd', tr( 'Number of units destroyed because they were indirect traces over the course of an iteration' ) );
      self.descAInd := TSimulationResultSet.create( _ptID, 'descAInd', tr( 'Total number of animals in units destroyed because they were indirect traces over the course of an iteration' ) );
      self.descURing := TSimulationResultSet.create( _ptID, 'descURing', tr( 'Number of units destroyed because they were in a destruction ring over the course of an iteration' ) );
      self.descARing := TSimulationResultSet.create( _ptID, 'descARing', tr( 'Total number of animals in all units destroyed because they were in a destruction ring over the course of an iteration' ) );
      self.descUTotal := TSimulationResultSet.create( _ptID, 'descUTotal', tr( 'Number of units destroyed for any reason over the course of an iteration' ) );
      self.descATotal := TSimulationResultSet.create( _ptID, 'descATotal', tr( 'Total number of animals in all units destroyed for any reason over the course of an iteration' ) );

      // Running totals for vaccination
      self.vaccUIni := TSimulationResultSet.create( _ptID, 'vaccUIni', tr( 'Number of units that were vaccine immune prior to the start of the simulation' ) );
      self.vaccAIni := TSimulationResultSet.create( _ptID, 'vaccAIni', tr( 'Total number of animals in units that were vaccine immune prior to the start of the simulation' ) );
      self.vaccURing := TSimulationResultSet.create( _ptID, 'vaccURing', tr( 'Number of units vaccinated in rings around detected-infected units over the course of an iteration' ) );
      self.vaccARing := TSimulationResultSet.create( _ptID, 'vaccARing', tr( 'Total number of animals in all units vaccinated in rings around detected-infected units over the course of an iteration' ) );
      //self.vaccUTotal := ...
      //self.vaccATotal := ...

      // Running totals for zone foci
      self.zoncFoci := TSimulationResultSet.create( _ptID, 'zoncFoci', tr( 'Total number of new zone foci created around units of the indicated type over the course of an iteration' ) );

      // First events
      self.firstDetection := TSimulationResultSet.create( _ptID, 'firstDet', tr( 'Day of first detection of an infected unit in the specified iteration' ), true );
      self.firstVaccination := TSimulationResultSet.create( _ptID, 'firstVacc', tr( 'Day of first vaccination of an infected unit in the specified iteration' ), true );
      self.firstDestruction := TSimulationResultSet.create( _ptID, 'firstDestr', tr( 'Day of first destruction of an infected unit in the specified iteration' ), true );

      // Outbreak duration
      self.diseaseDuration := TSimulationResultSet.create( _ptID, 'diseaseLen', tr( 'Duration of the active disease phase in the specified iteration' ), true );
      self.outbreakDuration := TSimulationResultSet.create( _ptID, 'outbreakLen', tr( 'Duration of the outbreak in the specified iteration' ), true );

      _smdb := nil;
      _ptDescr := '';
    end
  ;


  procedure TSMIterationOutputArraySet.assign( const original: TSMIterationOutputArraySet );
    begin
      _ptDescr := original.productionType;
      _smdb := original._smdb;
      
      // Running totals for each disease state
      self.tscUSusc.values.Assign( original.tscUSusc.values );
      self.tscASusc.values.Assign( original.tscASusc.values );
      self.tscULat.values.Assign( original.tscULat.values );
      self.tscALat.values.Assign( original.tscALat.values );
      self.tscUSubc.values.Assign( original.tscUSubc.values );
      self.tscASubc.values.Assign( original.tscASubc.values );
      self.tscUClin.values.Assign( original.tscUClin.values );
      self.tscAClin.values.Assign( original.tscAClin.values );
      self.tscUNImm.values.Assign( original.tscUNImm.values );
      self.tscANImm.values.Assign( original.tscANImm.values );
      self.tscUVImm.values.Assign( original.tscUVImm.values );
      self.tscAVImm.values.Assign( original.tscAVImm.values );
      self.tscUDest.values.Assign( original.tscUDest.values );
      self.tscADest.values.Assign( original.tscADest.values );

      // Running totals for cause of infection
      self.infcUIni.values.Assign( original.infcUIni.values );
      self.infcAIni.values.Assign( original.infcAIni.values );
      self.infcUAir.values.Assign( original.infcUAir.values );
      self.infcAAir.values.Assign( original.infcAAir.values );
      self.infcUDir.values.Assign( original.infcUDir.values );
      self.infcADir.values.Assign( original.infcADir.values );
      self.infcUInd.values.Assign( original.infcUInd.values );
      self.infcAInd.values.Assign( original.infcAInd.values );
      self.infcUTotal.values.Assign( original.infcUTotal.values );
      self.infcATotal.values.Assign( original.infcATotal.values );

      // Running totals for exposures
      self.expcUDir.values.Assign( original.expcUDir.values );
      self.expcADir.values.Assign( original.expcADir.values );
      self.expcUInd.values.Assign( original.expcUInd.values );
      self.expcAInd.values.Assign( original.expcAInd.values );
      self.expcUTotal.values.Assign( original.expcUTotal.values );
      self.expcATotal.values.Assign( original.expcATotal.values );

      // Running totals for traces
      self.trcUDir.values.Assign( original.trcUDir.values );
      self.trcADir.values.Assign( original.trcADir.values );
      self.trcUInd.values.Assign( original.trcUInd.values );
      self.trcAInd.values.Assign( original.trcAInd.values );
      self.trcUDirp.values.Assign( original.trcUDirp.values );
      self.trcADirp.values.Assign( original.trcADirp.values );
      self.trcUIndp.values.Assign( original.trcUIndp.values );
      self.trcAIndp.values.Assign( original.trcAIndp.values );

      // Running totals for detection
      self.detcUClin.values.Assign( original.detcUClin.values );
      self.detcAClin.values.Assign( original.detcAClin.values );

      // Running totals for destruction
      self.descUIni.values.Assign( original.descUIni.values );
      self.descAIni.values.Assign( original.descAIni.values );
      self.descUDet.values.Assign( original.descUDet.values );
      self.descADet.values.Assign( original.descADet.values );
      self.descUDir.values.Assign( original.descUDir.values );
      self.descADir.values.Assign( original.descADir.values );
      self.descUInd.values.Assign( original.descUInd.values );
      self.descAInd.values.Assign( original.descAInd.values );
      self.descURing.values.Assign( original.descURing.values );
      self.descARing.values.Assign( original.descARing.values );
      self.descUTotal.values.Assign( original.descUTotal.values );
      self.descATotal.values.Assign( original.descATotal.values );

      // Running totals for vaccination
      self.vaccUIni.values.Assign( original.vaccUIni.values );
      self.vaccAIni.values.Assign( original.vaccAIni.values );
      self.vaccURing.values.Assign( original.vaccURing.values );
      self.vaccARing.values.Assign( original.vaccARing.values );
      //self.vaccUTotal...
      //self.vaccATotal...

      // Running totals for zone foci
      self.zoncFoci.values.Assign( original.zoncFoci.values );

      // First events
      self.firstDetection.values.Assign( original.firstDetection.values );
      self.firstVaccination.values.Assign( original.firstVaccination.values );
      self.firstDestruction.values.Assign( original.firstDestruction.values );

      //outbreak duration
      self.diseaseDuration.values.Assign( original.diseaseDuration.values );
      self.outbreakDuration.values.Assign( original.outbreakDuration.values );
    end
  ;


  destructor TSMIterationOutputArraySet.destroy();
    begin
      // Running totals for each disease state
      self.tscUSusc.free();
      self.tscASusc.free();
      self.tscULat.free();
      self.tscALat.free();
      self.tscUSubc.free();
      self.tscASubc.free();
      self.tscUClin.free();
      self.tscAClin.free();
      self.tscUNImm.free();
      self.tscANImm.free();
      self.tscUVImm.free();
      self.tscAVImm.free();
      self.tscUDest.free();
      self.tscADest.free();

      // Running totals for cause of infection
      self.infcUIni.free();
      self.infcAIni.free();
      self.infcUAir.free();
      self.infcAAir.free();
      self.infcUDir.free();
      self.infcADir.free();
      self.infcUInd.free();
      self.infcAInd.free();
      self.infcUTotal.free();
      self.infcATotal.free();

      // Running totals for exposures
      self.expcUDir.free();
      self.expcADir.free();
      self.expcUInd.free();
      self.expcAInd.free();
      self.expcUTotal.free();
      self.expcATotal.free();

      // Running totals for traces
      self.trcUDir.free();
      self.trcADir.free();
      self.trcUInd.free();
      self.trcAInd.free();
      self.trcUDirp.free();
      self.trcADirp.free();
      self.trcUIndp.free();
      self.trcAIndp.free();

      // Running totals for detection
      self.detcUClin.free();
      self.detcAClin.free();

      // Running totals for destruction
      self.descUIni.free();
      self.descAIni.free();
      self.descUDet.free();
      self.descADet.free();
      self.descUDir.free();
      self.descADir.free();
      self.descUInd.free();
      self.descAInd.free();
      self.descURing.free();
      self.descARing.free();
      self.descUTotal.Free();
      self.descATotal.Free();

      // Running totals for vaccination
      self.vaccUIni.Free();
      self.vaccAIni.Free();
      self.vaccURing.free();
      self.vaccARing.free();
      //self.vaccUTotal.free();
      //self.vaccATotal.free();

      // Running totals for zone foci
      self.zoncFoci.Free();

      // First events
      self.firstDetection.free();
      self.firstVaccination.free();
      self.firstDestruction.free();

      // Outbreak duration
      self.diseaseDuration.Free();
      self.outbreakDuration.Free();
    end
  ;


  class function TSMIterationOutputArraySet.createEpiOutputDictionary(): TQOrderedStringStringDictionary;
    var
      dict: TQOrderedStringStringDictionary;
    begin
      dict := TQOrderedStringStringDictionary.Create();

      // Running totals for each disease state
      dict[ 'tscUSusc' ] := tr( 'Number of units that are or become susceptible over the course of an iteration' );
      dict[ 'tscASusc' ] := tr( 'Total number of animals in all units that are or become susceptible over the course of an iteration' );
      dict[ 'tscULat' ] := tr( 'Number of units that are or become latent over the course of an iteration' );
      dict[ 'tscALat' ] := tr( 'Total number of animals in all units that are or become latent over the course of an iteration' );
      dict[ 'tscUSubc' ] := tr( 'Number of units that are or become subclinically infectious over the course of an iteration' );
      dict[ 'tscASubc' ] := tr( 'Total number of animals in all units that are or become infectious over the course of an iteration' );
      dict[ 'tscUClin' ] := tr( 'Number of units that are or become clinical over the course of an iteration' );
      dict[ 'tscAClin' ] := tr( 'Total number of animals in all units that are or become clinical over the course of an iteration' );
      dict[ 'tscUNImm' ] := tr( 'Number of units that are or become naturally immune over the course of an iteration' );
      dict[ 'tscANImm' ] := tr( 'Total number of animals in all units that are or become naturally immune over the course of an iteration' );
      dict[ 'tscUVImm' ] := tr( 'Number of units that are or become vaccine immune over the course of an iteration' );
      dict[ 'tscAVImm' ] := tr( 'Total number of animals in all units that are or become vaccine immune over the course of an iteration' );
      dict[ 'tscUDest' ] := tr( 'Number of units that are destroyed over the course of an iteration' );
      dict[ 'tscADest' ] := tr( 'Total number of animals in all units that are destroyed over the course of an iteration' );

      // Running totals for cause of infection
      dict[ 'infcUIni' ] := tr( 'Number of units that are initially infected at the beginning of an iteration' );
      dict[ 'infcAIni' ] := tr( 'Number of animals in units that are initially infected at the beginning of an iteration' );
      dict[ 'infcUAir' ] := tr( 'Number of units that become infected by airborne spread over the course of an iteration' );
      dict[ 'infcAAir' ] := tr( 'Number of animals in units that become infected by airborne spread over the course of an iteration' );
      dict[ 'infcUDir' ] := tr( 'Number of units that become infected by direct contact over the course of an iteration' );
      dict[ 'infcADir' ] := tr( 'Number of animals in units that become infected by direct contact over the course of an iteration' );
      dict[ 'infcUInd' ] := tr( 'Number of units that become infected by indirect contact over the course of an iteration' );
      dict[ 'infcAInd' ] := tr( 'Number of animals in units that become infected by indirect contact over the course of an iteration' );
      dict[ 'infcUTotal' ] := tr( 'Total number of units that become infected over the course of an iteration' );
      dict[ 'infcATotal' ] := tr( 'Total number of animals in units that become infected over the course of an iteration' );

      // Running totals for exposures
      dict[ 'expcUDir' ] := tr( 'Total number of units directly exposed to any infected unit over the course of an iteration' );
      dict[ 'expcADir' ] := tr( 'Total number of animals in units directly exposed to any infected unit over the course of an iteration' );
      dict[ 'expcUInd' ] := tr( 'Total number of units indirectly exposed to any infected unit over the course of an iteration' );
      dict[ 'expcAInd' ] := tr( 'Total number of animals in units indirectly exposed to any infected unit over the course of an iteration' );
      dict[ 'expcUTotal' ] := tr( 'Total number units exposed by any contact to any infected unit over the course of an iteration' );
      dict[ 'expcATotal' ] := tr( 'Total number of animals in units exposed by any contact to any infected unit over the course of an iteration' );

      // Running totals for traces
      dict[ 'trcUDir' ] := tr( 'Number of units directly exposed and successfully traced over the course of an iteration' );
      dict[ 'trcADir' ] := tr( 'Total number of animals in all units directly exposed and successfully traced over the course of an iteration' );
      dict[ 'trcUInd' ] := tr( 'Number of units indirectly exposed and successfully traced over the course of an iteration' );
      dict[ 'trcAInd' ] := tr( 'Total number of animals in all units indirectly exposed and successfully traced over the course of an iteration' );
      dict[ 'trcUDirp' ] := tr( 'Number of units directly exposed that could possibly have been traced over the course of an iteration' );
      dict[ 'trcADirp' ] := tr( 'Total number of animals in all units directly exposed that could possibly have been traced over the course of an iteration' );
      dict[ 'trcUIndp' ] := tr( 'Number of units indirectly exposed that could possibly have been traced over the course of an iteration' );
      dict[ 'trcAIndp' ] := tr( 'Total number of animals in units indirectly exposed that could possibly have been traced over the course of an iteration' );

      // Running totals for detection
      dict[ 'detcUClin' ] := tr( 'Number of units detected by clinical signs over the course of an iteration' );
      dict[ 'detcAClin' ] := tr( 'Total number of animals in all units detected by clinical signs over the course of an iteration' );

      // Running totals for destruction
      dict[ 'descUIni' ] := tr( 'Number of units destroyed prior to the start of the simulation' );
      dict[ 'descAIni' ] := tr( 'Total number of animals in units destroyed prior to the start of the simulation' );
      dict[ 'descUDet' ] := tr( 'Number of units destroyed because they were detected positive over the course of an iteration' );
      dict[ 'descADet' ] := tr( 'Total number of animals in all units destroyed because they were detected positive over the course of an iteration' );
      dict[ 'descUDir' ] := tr( 'Number of units destroyed because they were direct traces over the course of an iteration' );
      dict[ 'descADir' ] := tr( 'Total number of animals in units destroyed because they were direct traces over the course of an iteration' );
      dict[ 'descUInd' ] := tr( 'Number of units destroyed because they were indirect traces over the course of an iteration' );
      dict[ 'descAInd' ] := tr( 'Total number of animals in units destroyed because they were indirect traces over the course of an iteration' );
      dict[ 'descURing' ] := tr( 'Number of units destroyed because they were in a destruction ring over the course of an iteration' );
      dict[ 'descARing' ] := tr( 'Total number of animals in all units destroyed because they were in a destruction ring over the course of an iteration' );
      dict[ 'descUTotal' ] := tr( 'Number of units destroyed for any reason over the course of an iteration.' );
      dict[ 'descATotal' ] := tr( 'Total number of animals in all units destroyed for any reason over the course of an iteration.' );

      // Running totals for vaccination
      dict[ 'vaccUIni' ] := tr( 'Number of units that were vaccine immune prior to the start of the simulation' );
      dict[ 'vaccAIni' ] := tr( 'Total number of animals in units that were vaccine immune prior to the start of the simulation' );
      dict[ 'vaccURing' ] := tr( 'Number of units vaccinated in rings around detected-infected units over the course of an iteration' );
      dict[ 'vaccARing' ] := tr( 'Total number of animals in all units vaccinated in rings around detected-infected units over the course of an iteration' );
      //dict[ 'vaccUTotal' ]...
      //dict[ 'vaccATotal' ]...

      // Running totals for zone foci
      dict[ 'zoncFoci' ] := tr( 'Total number of new zone foci created around units of the indicated type over the course of an iteration' );

      // First events
      dict[ 'detOccurred'] := tr( 'Number of iterations in which infected units were detected' );
      dict[ 'firstDet' ] := tr( 'Day of first detection of an infected unit in the specified iteration' );
      dict[ 'vaccOccurred' ] := tr( 'Number of iterations in which vaccination occurred' );
      dict[ 'firstVacc' ] := tr( 'Day of first vaccination of an infected unit in the specified iteration' );
      dict[ 'destrOccurred' ] := tr( 'Number of iterations in which destruction occurred' );
      dict[ 'firstDestr' ] := tr( 'Day of first destruction of an infected unit in the specified iteration' );

      // Outbreak duration
      dict[ 'diseaseEnded' ] :=  tr( 'Number of iterations in which the active disease phase ended' );
      dict[ 'diseaseLen' ] :=  tr( 'Length of the active disease phase in the specified iteration' );
      dict['outbreakEnded'] := tr( 'Number of iterations in which the outbreak ended' );
      dict[ 'outbreakLen' ] := tr( 'Length of the outbreak in the specified iteration' );

      if( dict.Count <>  self.epiOutputCount() ) then
        raise exception.Create( 'Output count mismatch in TSMIterationOutputArraySet.createEpiOutputDictionary' )
      ;

      result := dict;
    end
  ;


  class function TSMIterationOutputArraySet.epiOutputCount(): integer;
    begin
      result := 67;
    end
  ;


  function TSMIterationOutputArraySet.valueAsString( arrPos, calcPos: integer ): string;
    var
      d: double;
    begin
      case arrPos of
        53:
          if( 0 = calcPos ) then
            result := intToStr( firstDetection.dataCount ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smdb.completedIterations ) + ' ' + tr( 'iterations' )
          else
            result := ''
          ;
        55:
          if( 0 = calcPos ) then
            result := intToStr( firstVaccination.dataCount ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smdb.completedIterations ) + ' ' + tr( 'iterations' )
          else
            result := ''
          ;
        57:
          if( 0 = calcPos ) then
            result := intToStr( firstDestruction.dataCount ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smdb.completedIterations ) + ' ' + tr( 'iterations' )
          else
            result := ''
          ;
        59:
          if( 0 = calcPos ) then
            result := intToStr( diseaseDuration.dataCount ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smdb.completedIterations ) + ' ' + tr( 'iterations' )
          else
            result := ''
          ;
        61:
          if( 0 = calcPos ) then
            result := intToStr( outbreakduration.dataCount ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smdb.completedIterations ) + ' ' + tr( 'iterations' )
          else
            result := ''
          ;
        else
          begin
            dbcout( 'Generating value at array ' + intToStr( arrPos ) + ', calc ' + intToStr( calcPos ), DBSMSIMULATIONSTATS );
            d := valueAt( arrPos, calcPos );
            dbcout( 'd was successfully calculated', DBSMSIMULATIONSTATS );
            if( isNaN( d ) ) then
              result := 'n/a'
            else
              result := uiFloatToStr( d, 2 )
            ;
          end
        ;
      end;
      
    end
  ;


  function TSMIterationOutputArraySet.resultSetAt( arrPos: integer ): TSimulationResultSet;
    begin
      case arrPos of
        // Running totals for each disease state
        0: result := tscUSusc;
        1: result := tscASusc;
        2: result := tscULat;
        3: result := tscALat;
        4: result := tscUSubc;
        5: result := tscASubc;
        6: result := tscUClin;
        7: result := tscAClin;
        8: result := tscUNImm;
        9: result := tscANImm;
        10: result := tscUVImm;
        11: result := tscAVImm;
        12: result := tscUDest;
        13: result := tscADest;

        // Running totals for cause of infection
        14: result := infcUIni;
        15: result := infcAIni;
        16: result := infcUAir;
        17: result := infcAAir;
        18: result := infcUDir;
        19: result := infcADir;
        20: result := infcUInd;
        21: result := infcAInd;
        22: result := infcUTotal;
        23: result := infcATotal;

        // Running totals for exposures
        24: result := expcUDir;
        25: result := expcADir;
        26: result := expcUInd;
        27: result := expcAInd;
        28: result := expcUTotal;
        29: result := expcATotal;

        // Running totals for traces
        30: result := trcUDir;
        31: result := trcADir;
        32: result := trcUInd;
        33: result := trcAInd;
        34: result := trcUDirp;
        35: result := trcADirp;
        36: result := trcUIndp;
        37: result := trcAIndp;

        // Running totals for detection
        38: result := detcUClin;
        39: result := detcAClin;

        // Running totals for destruction
        40: result := descUIni;
        41: result := descAIni;
        42: result := descUDet;
        43: result := descADet;
        44: result := descUDir;
        45: result := descADir;
        46: result := descUInd;
        47: result := descAInd;
        48: result := descURing;
        49: result := descARing;
        50: result := descUTotal;
        51: result := descATotal;

        // Running totals for vaccination
        52: result := vaccUIni;
        53: result := vaccAIni;
        54: result := vaccURing;
        55: result := vaccARing;
        //xx: result := vaccUTotal;
        //xx: result := vaccATotal;

        56: result := zoncFoci;

        // First events
        57: result := nil;
        58: result := firstDetection;
        59: result := nil;
        60: result := firstVaccination;
        61: result := nil;
        62: result := firstDestruction;

        // Outbreak duration
        63: result := nil;
        64: result := diseaseDuration;
        65: result := nil;
        66: result := outbreakDuration;

        else
          raise exception.Create( 'index (' + intToStr(arrPos) +') out of bounds in TSMIterationOutputArraySet.resultSetAt()' )
        ;
      end;
    end
  ;


  function TSMIterationOutputArraySet.valueAt( arrPos, calcPos: integer ): double;
    begin
      case arrPos of
          // Running totals for each disease state
          0: result := tscUSusc.calculationAtPosition( calcPos );
          1: result := tscASusc.calculationAtPosition( calcPos );
          2: result := tscULat.calculationAtPosition( calcPos );
          3: result := tscALat.calculationAtPosition( calcPos );
          4: result := tscUSubc.calculationAtPosition( calcPos );
          5: result := tscASubc.calculationAtPosition( calcPos );
          6: result := tscUClin.calculationAtPosition( calcPos );
          7: result := tscAClin.calculationAtPosition( calcPos );
          8: result := tscUNImm.calculationAtPosition( calcPos );
          9: result := tscANImm.calculationAtPosition( calcPos );
          10: result := tscUVImm.calculationAtPosition( calcPos );
          11: result := tscAVImm.calculationAtPosition( calcPos );
          12: result := tscUDest.calculationAtPosition( calcPos );
          13: result := tscADest.calculationAtPosition( calcPos );

          // Running totals for cause of infection
          14: result := infcUIni.calculationAtPosition( calcPos );
          15: result := infcAIni.calculationAtPosition( calcPos );
          16: result := infcUAir.calculationAtPosition( calcPos );
          17: result := infcAAir.calculationAtPosition( calcPos );
          18: result := infcUDir.calculationAtPosition( calcPos );
          19: result := infcADir.calculationAtPosition( calcPos );
          20: result := infcUInd.calculationAtPosition( calcPos );
          21: result := infcAInd.calculationAtPosition( calcPos );
          22: result := infcUTotal.calculationAtPosition( calcPos );
          23: result := infcATotal.calculationAtPosition( calcPos );

          // Running totals for exposures
          24: result := expcUDir.calculationAtPosition( calcPos );
          25: result := expcADir.calculationAtPosition( calcPos );
          26: result := expcUInd.calculationAtPosition( calcPos );
          27: result := expcAInd.calculationAtPosition( calcPos );
          28: result := expcUTotal.calculationAtPosition( calcPos );
          29: result := expcATotal.calculationAtPosition( calcPos );

          // Running totals for traces
          30: result := trcUDir.calculationAtPosition( calcPos );
          31: result := trcADir.calculationAtPosition( calcPos );
          32: result := trcUInd.calculationAtPosition( calcPos );
          33: result := trcAInd.calculationAtPosition( calcPos );
          34: result := trcUDirp.calculationAtPosition( calcPos );
          35: result := trcADirp.calculationAtPosition( calcPos );
          36: result := trcUIndp.calculationAtPosition( calcPos );
          37: result := trcAIndp.calculationAtPosition( calcPos );

          // Running totals for detection
          38: result := detcUClin.calculationAtPosition( calcPos );
          39: result := detcAClin.calculationAtPosition( calcPos );

          // Running totals for destruction
          40: result := descUIni.calculationAtPosition( calcPos );
          41: result := descAIni.calculationAtPosition( calcPos );
          42: result := descUDet.calculationAtPosition( calcPos );
          43: result := descADet.calculationAtPosition( calcPos );
          44: result := descUDir.calculationAtPosition( calcPos );
          45: result := descADir.calculationAtPosition( calcPos );
          46: result := descUInd.calculationAtPosition( calcPos );
          47: result := descAInd.calculationAtPosition( calcPos );
          48: result := descURing.calculationAtPosition( calcPos );
          49: result := descARing.calculationAtPosition( calcPos );
          50: result := descUTotal.calculationAtPosition( calcPos );
          51: result := descATotal.calculationAtPosition( calcPos );

          // Running totals for vaccination
          52: result := vaccUIni.calculationAtPosition( calcPos );
          53: result := vaccAIni.calculationAtPosition( calcPos );
          54: result := vaccURing.calculationAtPosition( calcPos );
          55: result := vaccARing.calculationAtPosition( calcPos );
          //xx: result := vaccUTotal...
          //xx: result := vaccATotal...

          56: result := zoncFoci.calculationAtPosition( calcPos );

          // First events
          57: result := firstDetection.dataCount;
          58: result := firstDetection.calculationAtPosition( calcPos );
          59: result := firstVaccination.dataCount;
          60: result := firstVaccination.calculationAtPosition( calcPos );
          61: result := firstDestruction.dataCount;
          62: result := firstDestruction.calculationAtPosition( calcPos );

          // Outbreak duration
          63: result := diseaseDuration.dataCount;
          64: result := diseaseDuration.calculationAtPosition( calcPos );
          65: result := outbreakDuration.dataCount;
          66: result := outbreakDuration.calculationAtPosition( calcPos );

          else
            begin
              raise exception.Create( 'index out of bounds in TSMIterationOutputArraySet.valueAt()' );
              result := NaN;
            end
          ;
      end;
      
    end
  ;

  procedure TSMIterationOutputArraySet.appendRecordsFrom( row: TSqlRow );
    begin
      // Running totals for each disease state
      self.tscUSusc.values.append( row.field('tscUSusc') );
      self.tscASusc.values.append( row.field('tscASusc') );
      self.tscULat.values.append( row.field('tscULat') );
      self.tscALat.values.append( row.field('tscALat') );
      self.tscUSubc.values.append( row.field('tscUSubc') );
      self.tscASubc.values.append( row.field('tscASubc') );
      self.tscUClin.values.append( row.field('tscUClin') );
      self.tscAClin.values.append( row.field('tscAClin') );
      self.tscUNImm.values.append( row.field('tscUNImm') );
      self.tscANImm.values.append( row.field('tscANImm') );
      self.tscUVImm.values.append( row.field('tscUVImm') );
      self.tscAVImm.values.append( row.field('tscAVImm') );
      self.tscUDest.values.append( row.field('tscUDest') );
      self.tscADest.values.append( row.field('tscADest') );

      // Running totals for cause of infection
      self.infcUIni.values.append( row.field('infcUIni') );
      self.infcAIni.values.append( row.field('infcAIni') );
      self.infcUAir.values.append( row.field('infcUAir') );
      self.infcAAir.values.append( row.field('infcAAir') );
      self.infcUDir.values.append( row.field('infcUDir') );
      self.infcADir.values.append( row.field('infcADir') );
      self.infcUInd.values.append( row.field('infcUInd') );
      self.infcAInd.values.append( row.field('infcAInd') );
      self.infcUTotal.values.append( row.field('infcUAir') + row.field('infcUDir') + row.field('infcUInd') );
      self.infcATotal.values.append( row.field('infcAAir') + row.field('infcADir') + row.field('infcAInd') );

      // Running totals for exposures
      self.expcUDir.values.append( row.field('expcUDir') );
      self.expcADir.values.append( row.field('expcADir') );
      self.expcUInd.values.append( row.field('expcUInd') );
      self.expcAInd.values.append( row.field('expcAInd') );
      self.expcUTotal.values.append( row.field('expcUDir') + row.field('expcUInd') );
      self.expcATotal.values.append( row.field('expcADir') + row.field('expcAInd') );

      // Running totals for traces
      self.trcUDir.values.append( row.field('trcUDir') );
      self.trcADir.values.append( row.field('trcADir') );
      self.trcUInd.values.append( row.field('trcUInd') );
      self.trcAInd.values.append( row.field('trcAInd') );
      self.trcUDirp.values.append( row.field('trcUDirp') );
      self.trcADirp.values.append( row.field('trcADirp') );
      self.trcUIndp.values.append( row.field('trcUIndp') );
      self.trcAIndp.values.append( row.field('trcAIndp') );

      // Running totals for detection
      self.detcUClin.values.append( row.field('detcUClin') );
      self.detcAClin.values.append( row.field('detcAClin') );

      // Running totals for destruction
      self.descUIni.values.append( row.field('descUIni') );
      self.descAIni.values.append( row.field('descAIni') );
      self.descUDet.values.append( row.field('descUDet') );
      self.descADet.values.append( row.field('descADet') );
      self.descUDir.values.append( row.field('descUDir') );
      self.descADir.values.append( row.field('descADir') );
      self.descUInd.values.append( row.field('descUInd') );
      self.descAInd.values.append( row.field('descAInd') );
      self.descURing.values.append( row.field('descURing') );
      self.descARing.values.append( row.field('descARing') );
      self.descUTotal.values.append( row.field('descUDet') + row.field('descUDir') + row.field('descUInd') + row.field('descURing') );
      self.descATotal.values.append( row.field('descADet') + row.field('descADir') + row.field('descAInd') + row.field('descARing') );

      // Running totals for vaccination
      self.vaccUIni.values.append( row.field('vaccUIni') );
      self.vaccAIni.values.append( row.field('vaccAIni') );
      self.vaccURing.values.append( row.field('vaccURing') );
      self.vaccARing.values.append( row.field('vaccARing') );
      //self.vaccUTotal...
      //self.vaccATotal...

      // Running totals for zone foci
      self.zoncFoci.values.append( row.field('zoncFoci') );

      // First events
      if( null <> row.field('firstDetection') ) then
        self.firstDetection.values.append( row.field('firstDetection') )
      else
         self.firstDetection.values.append( UNSET )
      ;

      if( null <> row.field('firstVaccination') ) then
        self.firstVaccination.values.append( row.field('firstVaccination') )
      else
        self.firstVaccination.values.append( UNSET )
      ;

      if( null <> row.field('firstDestruction') ) then
        self.firstDestruction.values.append( row.field('firstDestruction') )
      else
        self.firstDestruction.values.append( UNSET )
      ;

      // Outbreak duration
      // Don't do anything for this particular output.
      // It is taken care of elsewhere
    end
  ;


  procedure TSMIterationOutputArraySet.sumFrom( arr: TSMIterationOutputArraySet );
    var
      i: integer;
    begin
      dbcout( 'SUMFROM', DBSMSIMULATIONSTATS );
      if( arr.arraySize <> self.arraySize ) then
        raise exception.create( 'Array size mismatch in TSMIterationOutputArraySet.sumFrom()' )
      ;

      for i := 0 to self.arraySize - 1 do
        begin
          // Running totals for each disease state
          self.tscUSusc.values[i] := self.tscUSusc.values[i] + arr.tscUSusc.values[i];
          self.tscASusc.values[i] := self.tscASusc.values[i] + arr.tscASusc.values[i];
          self.tscULat.values[i] := self.tscULat.values[i] + arr.tscULat.values[i];
          self.tscALat.values[i] := self.tscALat.values[i] + arr.tscALat.values[i];
          self.tscUSubc.values[i] := self.tscUSubc.values[i] + arr.tscUSubc.values[i];
          self.tscASubc.values[i] := self.tscASubc.values[i] + arr.tscASubc.values[i];
          self.tscUClin.values[i] := self.tscUClin.values[i] + arr.tscUClin.values[i];
          self.tscAClin.values[i] := self.tscAClin.values[i] + arr.tscAClin.values[i];
          self.tscUNImm.values[i] := self.tscUNImm.values[i] + arr.tscUNImm.values[i];
          self.tscANImm.values[i] := self.tscANImm.values[i] + arr.tscANImm.values[i];
          self.tscUVImm.values[i] := self.tscUVImm.values[i] + arr.tscUVImm.values[i];
          self.tscAVImm.values[i] := self.tscAVImm.values[i] + arr.tscAVImm.values[i];
          self.tscUDest.values[i] := self.tscUDest.values[i] + arr.tscUDest.values[i];
          self.tscADest.values[i] := self.tscADest.values[i] + arr.tscADest.values[i];

          // Running totals for cause of infection
          self.infcUIni.values[i] := self.infcUIni.values[i] + arr.infcUIni.values[i];
          self.infcAIni.values[i] := self.infcAIni.values[i] + arr.infcAIni.values[i];
          self.infcUAir.values[i] := self.infcUAir.values[i] + arr.infcUAir.values[i];
          self.infcAAir.values[i] := self.infcAAir.values[i] + arr.infcAAir.values[i];
          self.infcUDir.values[i] := self.infcUDir.values[i] + arr.infcUDir.values[i];
          self.infcADir.values[i] := self.infcADir.values[i] + arr.infcADir.values[i];
          self.infcUInd.values[i] := self.infcUInd.values[i] + arr.infcUInd.values[i];
          self.infcAInd.values[i] := self.infcAInd.values[i] + arr.infcAInd.values[i];
          self.infcUTotal.values[i] := self.infcUTotal.values[i] + arr.infcUTotal.values[i];
          self.infcATotal.values[i] := self.infcATotal.values[i] + arr.infcATotal.values[i];

          // Running totals for exposures
          self.expcUDir.values[i] := self.expcUDir.values[i] + arr.expcUDir.values[i];
          self.expcADir.values[i] := self.expcADir.values[i] + arr.expcADir.values[i];
          self.expcUInd.values[i] := self.expcUInd.values[i] + arr.expcUInd.values[i];
          self.expcAInd.values[i] := self.expcAInd.values[i] + arr.expcAInd.values[i];
          self.expcUTotal.values[i] := self.expcUTotal.values[i] + arr.expcUTotal.values[i];
          self.expcATotal.values[i] := self.expcATotal.values[i] + arr.expcATotal.values[i];

          // Running totals for traces
          self.trcUDir.values[i] := self.trcUDir.values[i] + arr.trcUDir.values[i];
          self.trcADir.values[i] := self.trcADir.values[i] + arr.trcADir.values[i];
          self.trcUInd.values[i] := self.trcUInd.values[i] + arr.trcUInd.values[i];
          self.trcAInd.values[i] := self.trcAInd.values[i] + arr.trcAInd.values[i];
          self.trcUDirp.values[i] := self.trcUDirp.values[i] + arr.trcUDirp.values[i];
          self.trcADirp.values[i] := self.trcADirp.values[i] + arr.trcADirp.values[i];
          self.trcUIndp.values[i] := self.trcUIndp.values[i] + arr.trcUIndp.values[i];
          self.trcAIndp.values[i] := self.trcAIndp.values[i] + arr.trcAIndp.values[i];

          // Running totals for detection
          self.detcUClin.values[i] := self.detcUClin.values[i] + arr.detcUClin.values[i];
          self.detcAClin.values[i] := self.detcAClin.values[i] + arr.detcAClin.values[i];

          // Running totals for destruction
          self.descUIni.values[i] := self.descUIni.values[i] + arr.descUIni.values[i];
          self.descAIni.values[i] := self.descAIni.values[i] + arr.descAIni.values[i];
          self.descUDet.values[i] := self.descUDet.values[i] + arr.descUDet.values[i];
          self.descADet.values[i] := self.descADet.values[i] + arr.descADet.values[i];
          self.descUDir.values[i] := self.descUDir.values[i] + arr.descUDir.values[i];
          self.descADir.values[i] := self.descADir.values[i] + arr.descADir.values[i];
          self.descUInd.values[i] := self.descUInd.values[i] + arr.descUInd.values[i];
          self.descAInd.values[i] := self.descAInd.values[i] + arr.descAInd.values[i];
          self.descURing.values[i] := self.descURing.values[i] + arr.descURing.values[i];
          self.descARing.values[i] := self.descARing.values[i] + arr.descARing.values[i];
          self.descUTotal.values[i] := self.descUTotal.values[i] + arr.descUTotal.values[i];
          self.descATotal.values[i] := self.descATotal.values[i] + arr.descATotal.values[i];

          // Running totals for vaccination
          self.vaccUIni.values[i] := self.vaccUIni.values[i] + arr.vaccUIni.values[i];
          self.vaccAIni.values[i] := self.vaccAIni.values[i] + arr.vaccAIni.values[i];
          self.vaccURing.values[i] := self.vaccURing.values[i] + arr.vaccURing.values[i];
          self.vaccARing.values[i] := self.vaccARing.values[i] + arr.vaccARing.values[i];
          //self.vaccUTotal...
          //self.vaccATotal...

          // Running totals for zone foci
          self.zoncFoci.values[i] := self.zoncFoci.values[i] + arr.zoncFoci.values[i];
          
          // First events
          dbcout( '---First detection is ' + uiFloatToStr( arr.firstDetection.values[i] ), DBSMSIMULATIONSTATS );
          dbcout( 'Condition 1:', DBSMSIMULATIONSTATS );
          dbcout( UNSET <> arr.firstDetection.values[i], DBSMSIMULATIONSTATS );
          dbcout( 'Condition 2:', DBSMSIMULATIONSTATS );
          dbcout(
              (
                ( arr.firstDetection.values[i] < self.firstDetection.values[i] )
              or
                ( UNSET = self.firstDetection.values[i] )
              )
              , DBSMSIMULATIONSTATS
           );

          if
            ( UNSET <> arr.firstDetection.values[i] )
          and
            (
              ( arr.firstDetection.values[i] < self.firstDetection.values[i] )
            or
              ( UNSET = self.firstDetection.values[i] )
            )
          then
            begin
              dbcout( 'Setting first detection to ' + uiFloatToStr( arr.firstDetection.values[i] ), DBSMSIMULATIONSTATS );
              self.firstDetection.values[i] := arr.firstDetection.values[i];
            end
          ;

          if
            ( UNSET <> arr.firstVaccination.values[i] )
          and
            (
              ( arr.firstVaccination.values[i] < self.firstVaccination.values[i] )
            or
              ( UNSET = self.firstVaccination.values[i] )
            )
          then
            begin
              dbcout( 'Setting first vaccination to ' + uiFloatToStr( arr.firstVaccination.values[i] ), DBSMSIMULATIONSTATS );
              self.firstVaccination.values[i] := arr.firstVaccination.values[i];
            end
          ;

          if
            ( UNSET <> arr.firstDestruction.values[i] )
          and
            (
              ( arr.firstDestruction.values[i] < self.firstDestruction.values[i] )
            or
              ( UNSET = self.firstDestruction.values[i] )
            )
          then
            begin
              dbcout( 'Setting first destruction to ' + uiFloatToStr( arr.firstDestruction.values[i] ), true (*DBSMSIMULATIONSTATS*) );
              self.firstDestruction.values[i] := arr.firstDestruction.values[i];
            end
          ;

          // Outbreak duration
          // This is constant for all production types.
          // Don't do any addition, just copy the existing value to the new array
          self.diseaseDuration.values[i] := arr.diseaseDuration.values[i];
          self.outbreakDuration.values[i] := arr.outbreakDuration.values[i];
        end
      ;

    end
  ;


  function TSMIterationOutputArraySet.getArraySize(): integer;
    begin
      result := self.tscUSusc.values.Count;
    end
  ;


  procedure TSMIterationOutputArraySet.setPtID( val: integer ); begin _ptID := val; end;
  procedure TSMIterationOutputArraySet.setPtDescr( val: string ); begin _ptDescr := val; end;
  function TSMIterationOutputArraySet.getPtID(): integer; begin result := _ptID; end;
  function TSMIterationOutputArraySet.getPtDescr(): string; begin result := _ptDescr; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TSMIterationOutputSuperList
//-----------------------------------------------------------------------------
  // Construction/destruction
  //-------------------------
  constructor TSMIterationOutputSuperList.create( db: TSMDatabase );
    var
      ptRes: TSqlResult;
      ptRow: TSqlRow;

      dataRes: TSqlResult;
      dataRow: TSqlRow;
      dataQuery: string;

      db2: TSqlDatabase;

      arr: TSMIterationOutputArraySet;

      arr2: TSMIterationOutputArraySet;
      i: integer;
    begin
      inherited create( true );
      _db := db;

      // List position 0 is saved for all production
      // types, and will be filled at the end.
      self.append( TSMIterationOutputArraySet.create() );

      db2 := _db as TSqlDatabase;

      dataRes := TSqlResult.create( db2 );

      dataQuery := writeBasicQuery();

      ptRes := TSqlResult.create( 'SELECT productionTypeID, descr FROM inProductionType ORDER BY productionTypeID', db2 );

      ptRow := ptRes.fetchArrayFirst();
      while( nil <> ptRow ) do
        begin
          // Create the arrays for all outputs except for outbreak duration
          dataRes.runQuery(
            dataQuery
              + ' WHERE outIterationByProductionType.productionTypeID = '
              + intToStr( ptRow.field('productionTypeID') )
              + ' ORDER BY outIterationByProductionType.iteration'
          );

          dbcout( 'Building array for ptID ' + intToStr( ptRow.field('productionTypeID') ), DBSMSIMULATIONSTATS );

          arr := TSMIterationOutputArraySet.create( _db, ptRow.field('productionTypeID'), ptRow.field('descr') );

          dataRow := dataRes.fetchArrayFirst();
          while( nil <> dataRow ) do
            begin
              arr.appendRecordsFrom( dataRow );
              dataRow := dataRes.fetchArrayNext();
            end
          ;

          // Create the array for outbreak duration
          dataRes.runQuery(
            'SELECT'
            + ' diseaseEnded, diseaseEndDay,'
            + ' outbreakEnded, outbreakEndDay'
            + ' FROM outIteration ORDER BY iteration'
          );

          dataRow := dataRes.fetchArrayFirst();
          while( nil <> dataRow ) do
            begin
              if( true = dataRow.field('diseaseEnded') ) then
                arr.diseaseDuration.values.append( dataRow.field('diseaseEndDay') )
              else
                arr.diseaseDuration.values.append( UNSET )
              ;

              if( true = dataRow.field('outbreakEnded') ) then
                arr.outbreakDuration.values.append( dataRow.field('outbreakEndDay') )
              else
                arr.outbreakDuration.values.append( UNSET )
              ;

              dataRow := dataRes.fetchArrayNext();
            end
          ;

          self.append( arr );

          ptRow := ptRes.fetchArrayNext();
        end
      ;


      // Now that each individual production type is in,
      // Fill up the array that we saved for all pts.
      arr := self.at(0);

      // Copy array set 1 to array set 0
      arr.assign( self.at(1) );

      arr.productionType := tr( 'All production types' );
      arr.productionTypeID := 0;

      // If there are more array sets, add their values to array set 0
      if( 2 < self.Count ) then
        begin
          for i := 2 to self.Count - 1 do
            begin
              arr2 := self.at(i);
              arr.sumFrom( arr2 );
            end
          ;
        end
      ;

      // Clean up
      ptRes.Free();
      dataRes.Free();
    end
  ;

  destructor TSMIterationOutputSuperList.destroy();
    begin
      // This list owns its child objects, and will
      // free them automatically.
      inherited destroy();
    end
  ;


  function TSMIterationOutputSuperList.arraysForProductionType( const ptID: integer ): TSMIterationOutputArraySet;
    var
      i: integer;
      arr: TSMIterationOutputArraySet;
    begin
      arr := nil;

      for i := 0 to self.Count - 1 do
        begin
          if( ptID = self.at(i).productionTypeID ) then
            begin
              arr := self.at(i);
              break;
            end
          ;
        end
      ;

      result := arr;
    end
  ;


  // Basic list functions
  //---------------------
  function TSMIterationOutputSuperList.append( dm: TSMIterationOutputArraySet ): integer;
    begin
      result := inherited Add( dm );
    end
  ;


  procedure TSMIterationOutputSuperList.setObject( index: integer; item: TSMIterationOutputArraySet );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TSMIterationOutputSuperList.getObject( index: integer ): TSMIterationOutputArraySet;
    begin
      result := inherited GetItem( index ) as TSMIterationOutputArraySet;
    end
  ;

  
  function TSMIterationOutputSuperList.at( const i: integer ): TSMIterationOutputArraySet;
    begin
      if( ( 0 > i ) or ( self.count - 1 < i ) ) then
        result := nil
      else
        result := self.objects[i]
      ;
    end
  ;


  procedure TSMIterationOutputSuperList.insert( index: integer; dm: TSMIterationOutputArraySet);
    begin
      inherited Insert(index, dm);
    end
  ;


  function TSMIterationOutputSuperList.first() : TSMIterationOutputArraySet;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMIterationOutputSuperList.last() : TSMIterationOutputArraySet;
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


  function TSMIterationOutputSuperList.next() : TSMIterationOutputArraySet;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
      	result := getObject( _currentIndex )
      ;
    end
  ;


  function TSMIterationOutputSuperList.current() : TSMIterationOutputArraySet;
    begin
      if( _currentIndex > (self.Count - 1) ) then
      	result := nil
      else
        result := getObject( _currentIndex )
      ;
    end
  ;

  (*
  function TSMIterationOutputSuperList.writeBasicQuery(): string;
    begin
      result := 'SELECT'
          + ' productionTypeID, '
          + ' iteration, '
          + ' tscUSusc, '
          + ' tscASusc, '
          + ' tscULat, '
          + ' tscALat, '
          + ' tscUSubc, '
          + ' tscASubc, '
          + ' tscUClin, '
          + ' tscAClin, '
          + ' tscUNImm, '
          + ' tscANImm, '
          + ' tscUVImm, '
          + ' tscAVImm, '
          + ' tscUDest, '
          + ' tscADest, '
          + ' infcUIni, '
          + ' infcAIni, '
          + ' infcUAir, '
          + ' infcAAir, '
          + ' infcUDir, '
          + ' infcADir, '
          + ' infcUInd, '
          + ' infcAInd, '
          // infcUTotal is a calculated field
          // infcATotal is a calculated field
          + ' expcUDir, '
          + ' expcADir, '
          + ' expcUInd, '
          + ' expcAInd, '
          // expcUTotal is a calculated field
          // expcATotal is a calculated field
          + ' trcUDir, '
          + ' trcADir, '
          + ' trcUInd, '
          + ' trcAInd, '
          + ' trcUDirp, '
          + ' trcADirp, '
          + ' trcUIndp, '
          + ' trcAIndp, '
          + ' detcUClin, '
          + ' detcAClin, '
          + ' descUIni, '
          + ' descAIni, '
          + ' descUDet, '
          + ' descADet, '
          + ' descUDir, '
          + ' descADir, '
          + ' descUInd, '
          + ' descAInd, '
          + ' descURing, '
          + ' descARing, '
          // descUTotal is a calculated field
          // descATotal is a calculated field
          + ' vaccUIni, '
          + ' vaccAIni, '
          + ' vaccURing, '
          + ' vaccARing, '
          // vaccUTotal (when it exists) will be a calculated field
          // vaccATotal (when it exists) will be a calculated field
          + ' zoncFoci, '
          + ' firstDetection, '
          + ' firstDestruction, '
          + ' firstVaccination '
        + ' FROM outIterationByProductionType'
      ;
    end
  ;
  *)

  function TSMIterationOutputSuperList.writeBasicQuery(): string;
    begin
      result := 'SELECT '
        // Epi outputs
        //------------
        + ' outIterationByProductionType.tscUSusc AS tscUSusc,'
        + ' outIterationByProductionType.tscASusc AS tscASusc,'
        + ' outIterationByProductionType.tscULat AS tscULat,'
        + ' outIterationByProductionType.tscALat AS tscALat,'
        + ' outIterationByProductionType.tscUSubc AS tscUSubc,'
        + ' outIterationByProductionType.tscASubc AS tscASubc,'
        + ' outIterationByProductionType.tscUClin AS tscUClin,'
        + ' outIterationByProductionType.tscAClin AS tscAClin,'
        + ' outIterationByProductionType.tscUNImm AS tscUNImm,'
        + ' outIterationByProductionType.tscANImm AS tscANImm,'
        + ' outIterationByProductionType.tscUVImm AS tscUVImm,'
        + ' outIterationByProductionType.tscAVImm AS tscAVImm,'
        + ' outIterationByProductionType.tscUDest AS tscUDest,'
        + ' outIterationByProductionType.tscADest AS tscADest,'
        + ' outIterationByProductionType.infcUIni AS infcUIni,'
        + ' outIterationByProductionType.infcAIni AS infcAIni,'
        + ' outIterationByProductionType.infcUAir AS infcUAir,'
        + ' outIterationByProductionType.infcAAir AS infcAAir,'
        + ' outIterationByProductionType.infcUDir AS infcUDir,'
        + ' outIterationByProductionType.infcADir AS infcADir,'
        + ' outIterationByProductionType.infcUInd AS infcUInd,'
        + ' outIterationByProductionType.infcAInd AS infcAInd,'
        + ' outIterationByProductionType.expcUDir AS expcUDir,'
        + ' outIterationByProductionType.expcADir AS expcADir,'
        + ' outIterationByProductionType.expcUInd AS expcUInd,'
        + ' outIterationByProductionType.expcAInd AS expcAInd,'
        + ' outIterationByProductionType.trcUDir AS trcUDir,'
        + ' outIterationByProductionType.trcADir AS trcADir,'
        + ' outIterationByProductionType.trcUInd AS trcUInd,'
        + ' outIterationByProductionType.trcAInd AS trcAInd,'
        + ' outIterationByProductionType.trcUDirp AS trcUDirp,'
        + ' outIterationByProductionType.trcADirp AS trcADirp,'
        + ' outIterationByProductionType.trcUIndp AS trcUIndp,'
        + ' outIterationByProductionType.trcAIndp AS trcAIndp,'
        + ' outIterationByProductionType.detcUClin AS detcUClin,'
        + ' outIterationByProductionType.detcAClin AS detcAClin,'
        + ' outIterationByProductionType.descUIni AS descUIni,'
        + ' outIterationByProductionType.descAIni AS descAIni,'
        + ' outIterationByProductionType.descUDet AS descUDet,'
        + ' outIterationByProductionType.descADet AS descADet,'
        + ' outIterationByProductionType.descUDir AS descUDir,'
        + ' outIterationByProductionType.descADir AS descADir,'
        + ' outIterationByProductionType.descUInd AS descUInd,'
        + ' outIterationByProductionType.descAInd AS descAInd,'
        + ' outIterationByProductionType.descURing AS descURing,'
        + ' outIterationByProductionType.descARing AS descARing,'
        // descUTotal is a calculated field
        // descATotal is a calculated field
        + ' outIterationByProductionType.vaccUIni AS vaccUIni,'
        + ' outIterationByProductionType.vaccAIni AS vaccAIni,'
        + ' outIterationByProductionType.vaccURing AS vaccURing,'
        + ' outIterationByProductionType.vaccARing AS vaccARing,'
        // vaccUTotal (when it exists) will be a calculated field
        // vaccATotal (when it exists) will be a calculated field
        + ' outIterationByProductionType.zoncFoci as zoncFoci,'
        + ' outIterationByProductionType.firstDetection AS firstDetection,'
        + ' outIterationByProductionType.firstDestruction AS firstDestruction,'
        + ' outIterationByProductionType.firstVaccination  AS firstVaccination'

        + ' FROM outIterationByProductionType'
        + ' LEFT JOIN outIterationCosts'
        + ' ON outIterationCosts.productionTypeID = outIterationByProductionType.productionTypeID'
        + ' AND outIterationCosts.iteration = outIterationByProductionType.iteration'
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
