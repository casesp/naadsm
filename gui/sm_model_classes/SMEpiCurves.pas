unit SMEpiCurves;

(*
SMEpiCurves.pas
---------------
Begin: 2005/10/14
Last revision: $Date: 2013-06-27 19:11:35 $ $Author: areeves $
Version number: $Revision: 1.18.6.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    Contnrs,

    QOrderedDictionaries,
    QVectors,

    SMDatabase,
    ProductionType
  ;


  type TSummaryEpiCurveCalculation = (
    EpiCalcUnspecified,
    EpiCalcHigh,
    EpiCalc95Percentile,
    EpiCalc75Percentile,
    EpiCalc50Percentile,
    EpiCalcMedian,
    EpiCalcMean,
    EpiCalc25Percentile,
    EpiCalc5Percentile,
    EpiCalcLow
  );


  type TSummaryEpiCurveType = (
    EpiCurveUnspecified,
    EpiCurveActualHerds,
    EpiCurveActualAnimals,
    EpiCurveApparentHerds,
    EpiCurveApparentAnimals
  );

  (**
    This list consists of arrays.
    There is a list item (an array) for each simulation day.
    There must be enough list items to accomodate the longest iteration/outbreak.

    Each array is associated with a particular day.
    Each array position is the number of cases on that particular day in an iteration.

    For example:
    Let n indicate the number of iterations, i.e., the number of positions in each array.
    Let m indicate the number of days in the longest iteration, i.e. the number of list items.

    list[0].array[0] contains the number of cases on day 0 of the first iteration (iteration 1).
    list[0].array[1] contains the number of cases on day 0 of iteration 2.
    ...
    list[0].array[n-1] contains the number of cases on day 0 of iteration n.


    list[1].array[0] contains the number of cases on day 1 of the first iteration (iteration 1).
    list[1].array[1] contains the number of cases on day 1 of iteration 2.
    ...
    list[1].array[n-1] contains the number of cases on day 1 of iteration n.


    list[m].array[0] contains the number of cases on day m of the first iteration (iteration 1).
    list[m].array[1] contains the number of cases on day m of iteration 2.
    ...
    list[m].array[n-1] contains the number of cases on day m of iteration n.

    Notice from above that n (the number of iterations) is offset by 1.

    FIX ME: check this.
    Also notice that m (the number of days in the longest outbreak) is not offset: there
    are actually m+1 elements in the array.  Recall that day 0 has special meaning.

    Each list is associated with an individual production type.
  *)
  type TSummaryEpiCurveArrayList = class( TObjectList )
    protected
      // Basic list operations
      //-----------------------
      procedure setObject( index: integer; item: TQDoubleVector );
      function getObject( index: integer ): TQDoubleVector;
      property objects[index: integer]: TQDoubleVector read getObject write setObject;
      function append( dm: TQDoubleVector ): integer;
      function at( const i: integer ): TQDoubleVector;

    public
      constructor create( iterationCount, dayCount: integer );
      destructor destroy(); override;

      procedure setValue( day, iteration: integer; val: double );
      function getValue( day, iteration: integer ): double;
      procedure sort();

      function lowForDay( day: integer ): extended;
      function p5ForDay( day: integer ): extended;
      function p25ForDay( day: integer ): extended;
      function p50ForDay( day: integer ): extended;
      function p75ForDay( day: integer ): extended;
      function p95ForDay( day: integer ): extended;
      function meanForDay( day: integer ): extended;
      function medianForDay( day: integer ): extended;
      function highForDay( day: integer ): extended;

    end
  ;


  type TSMSummaryEpiCurve = class
    protected
      _ptid: integer;
      _days: integer;
      _iterations: integer;

      _actualHerds: TSummaryEpiCurveArrayList;
      _actualAnimals: TSummaryEpiCurveArrayList;
      _apparentHerds: TSummaryEpiCurveArrayList;
      _apparentAnimals: TSummaryEpiCurveArrayList;

      procedure initialize( iterations, days: integer );

    public
      constructor create(
        db: TSMDatabase;
        ptid: integer;
        iterations: integer = -1;
        days: integer = -1
      ); overload;

      constructor create( iterations, days: integer ); overload;
      destructor destroy(); override;

      procedure addFrom( curve: TSMSummaryEpiCurve );

      function actualHerdsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
      function actualAnimalsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
      function apparentHerdsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
      function apparentAnimalsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;

      function actualHerdsOnDayByIteration( day: integer; iteration: integer ): extended;
      function actualAnimalsOnDayByIteration( day: integer; iteration: integer ): extended;
      function apparentHerdsOnDayByIteration( day: integer; iteration: integer ): extended;
      function apparentAnimalsOnDayByIteration( day: integer; iteration: integer ): extended;

      function valueOnDay(
        day: integer;
        curveType: TSummaryEpiCurveType;
        calcType: TSummaryEpiCurveCalculation
      ): extended;

      property daysInCurve: integer read _days;

      procedure debug();
    end
  ;


  type TSMSummaryEpiCurves = class( TQOrderedStringObjectDictionary )
    public
      constructor create( db: TSMDatabase ); reintroduce;
      destructor destroy(); override;

      function curveForProductionType( const prodType: string ): TSMSummaryEpiCurve;
    end
  ;

  const
    DBSMEPICURVES: boolean = false; // set to true to enable debugging messages for this unit

implementation

  uses
    SysUtils,
    Math,
    Variants,

    MyStrUtils,
    DebugWindow,
    ARMath,
    SqlClasses,
    I88n
  ;




  constructor TSummaryEpiCurveArrayList.create( iterationCount, dayCount: integer );
    var
      i: integer;
    begin
      inherited create( true );

      dbcout( 'Creating TSummaryEpiCurveArrayList', DBSMEPICURVES );

      // Create the right number of list items (one for each day).
      //-----------------------------------------------------------
      for i := 0 to dayCount - 1 do
        self.append( TQDoubleVector.Create() )
      ;

      // Set the length for the list items (the same as
      // the number of iterations).
      // Also zero out all array positions.
      //-------------------------------------------------
      for i := 0 to dayCount - 1 do
        begin
          self.at(i).resize( iterationCount );
          self.at(i).Fill( 0.0 );
        end
      ;

    end
  ;


  destructor TSummaryEpiCurveArrayList.destroy();
    begin
      // This list owns its child objects, and will
      // free them automatically.
      inherited destroy();
    end
  ;

  
  function TSummaryEpiCurveArrayList.getValue( day, iteration: integer ): double;
    begin
//      self.at(day).Sort();
//      _sorted[day] := true;

      //  Must subtract one from each of these because this array is zero based...
      result := self.at(day)[iteration - 1];
    end
  ;


  procedure TSummaryEpiCurveArrayList.setValue( day, iteration: integer; val: double );
    begin
      // self.at(day) is an array.
      // Set the value of the array at [iteration - 1] to val.
      // (Remember that iterations are offset by 1, but days are not.)
      // (Yes, there is actually a reason for this.)

      //dbcout( 'Setting day %d, iteration %d to %f', [day, iteration-1, val], DBSMEPICURVES );
      self.at(day)[iteration - 1] := val;
    end
  ;


  procedure TSummaryEpiCurveArrayList.sort();
    var
      i: integer;
    begin
      for i := 0 to self.Count - 1 do
        self.at(i).Sort()
      ;
    end
  ;


  function TSummaryEpiCurveArrayList.lowForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day)[0];
    end
  ;


  function TSummaryEpiCurveArrayList.p5ForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day).quantile( 0.05 );
    end
  ;

  function TSummaryEpiCurveArrayList.p25ForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day).quantile( 0.25 );
    end
  ;

  function TSummaryEpiCurveArrayList.p50ForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day).quantile( 0.50 );
    end
  ;

  function TSummaryEpiCurveArrayList.p75ForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day).quantile( 0.75 );
    end
  ;

  function TSummaryEpiCurveArrayList.p95ForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day).quantile( 0.95 );
    end
  ;

  function TSummaryEpiCurveArrayList.highForDay( day: integer ): extended;
    begin
      self.at(day).Sort();
      result := self.at(day)[self.at(day).count - 1];
    end
  ;

  function TSummaryEpiCurveArrayList.meanForDay( day: integer ): extended;
    begin
      result := self.at(day).mean();
    end
  ;

  function TSummaryEpiCurveArrayList.medianForDay( day: integer ): extended;
    begin
      result := p50ForDay( day );
    end
  ;


  // Basic list functions
  //---------------------
  function TSummaryEpiCurveArrayList.append( dm: TQDoubleVector ): integer;
    begin
      result := inherited Add( dm );
    end
  ;

  procedure TSummaryEpiCurveArrayList.setObject( index: integer; item: TQDoubleVector );
    begin
      inherited SetItem( index, item );
    end
  ;


  function TSummaryEpiCurveArrayList.getObject( index: integer ): TQDoubleVector;
    begin
      result := inherited GetItem( index ) as TQDoubleVector;
    end
  ;


  function TSummaryEpiCurveArrayList.at( const i: integer ): TQDoubleVector;
    begin
      if( ( 0 > i ) or ( self.count - 1 < i ) ) then
        result := nil
      else
        result := self.objects[i]
      ;
    end
  ;


  constructor TSMSummaryEpiCurve.create( iterations, days: integer );
    begin
      initialize( iterations, days );
    end
  ;

  procedure TSMSummaryEpiCurve.initialize( iterations, days: integer );
    begin
      _ptid := -1;

      _iterations := iterations;
      _days := days;

      dbcout( 'Creating the lists...', DBSMEPICURVES );
      _actualHerds := TSummaryEpiCurveArrayList.create( _iterations, _days );
      _actualAnimals := TSummaryEpiCurveArrayList.create( _iterations, _days );
      _apparentHerds := TSummaryEpiCurveArrayList.create( _iterations, _days );
      _apparentAnimals := TSummaryEpiCurveArrayList.create( _iterations, _days );
       dbcout( 'Lists created', DBSMEPICURVES );
    end
  ;


  constructor TSMSummaryEpiCurve.create(
        db: TSMDatabase;
        ptid: integer;
        iterations: integer = -1;
        days: integer = -1
      );
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      val: integer;
    begin
      dbcout( 'Creating TSMSummaryEpiCurve', DBSMEPICURVES );

      if( -1 = iterations ) then iterations := db.completedIterations();
      if( -1 = days ) then days := db.daysInLongestIteration();

      initialize( iterations, days );
      _ptid := ptid;

      db2 := db as TSqlDatabase;

      // Fill the lists
      //----------------
      dbcout( 'Filling the lists...', DBSMEPICURVES );
      q := 'SELECT'
        + '   iteration,'
        + '   day,'
        + '   infectedUnits,'
        + '   infectedAnimals,'
        + '   detectedUnits,'
        + '   detectedAnimals'
        + ' FROM outEpidemicCurves'
        + ' WHERE productionTypeID = ' + intToStr( ptid )
        + ' ORDER BY iteration, day'
      ;

      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();
      while( nil <> row ) do
        begin
          if( ( null = row.field('day') ) or ( null = row.field('iteration') ) ) then
            raise exception.Create( '"day" or "iteration" is null in TSMSummaryEpiCurve.create()' )
          else
            begin
              if( null = row.field('infectedUnits') ) then
                val := 0
              else
                val := row.field('infectedUnits')
              ;
              _actualHerds.setValue( row.field('day'), row.field('iteration'), val );

              if( null = row.field('infectedAnimals') ) then
                val := 0
              else
                val := row.field('infectedAnimals')
              ;
              _actualAnimals.setValue( row.field('day'), row.field('iteration'), val );

              if( null = row.field('detectedUnits') ) then
                val := 0
              else
                val := row.field('detectedUnits')
              ;
              _apparentHerds.setValue( row.field('day'), row.field('iteration'), val );

              if( null = row.field('detectedAnimals') ) then
                val := 0
              else
                val := row.field('detectedAnimals')
              ;
              _apparentAnimals.setValue( row.field('day'), row.field('iteration'), val );
            end
          ;

          row := res.fetchArrayNext();
        end
      ;

      dbcout( 'Lists full.', DBSMEPICURVES );
      res.Free();
      dbcout( 'Done with TSMSummaryEpiCurve.create()', DBSMEPICURVES );
    end
  ;


  destructor TSMSummaryEpiCurve.destroy();
    begin
      _actualHerds.Free();
      _actualAnimals.Free();
      _apparentHerds.Free();
      _apparentAnimals.Free();

      inherited destroy();
    end
  ;


  procedure TSMSummaryEpiCurve.debug();
    var
      i: integer;
    begin
      dbcout( '----------------- Summary epi curve', true );
      dbcout( 'Day in curve: ' + intToStr( daysInCurve ), true );
      dbcout( 'Mean actual herds for day: ', true );

      for i := 0 to daysInCurve - 1 do
        dbcout( '%d, %f', [i, actualHerdsOnDay( i, EpiCalcMean )], true )
      ;

      dbcout( '----------------- End summary epi curve', true );
    end
  ;

  procedure TSMSummaryEpiCurve.addFrom( curve: TSMSummaryEpiCurve );
    var
      i, j: integer;
    begin
      for j := 0 to _iterations - 1 do
        begin
          for i := 0 to _days - 1 do
            begin
              self._actualHerds.at(i)[j] := self._actualHerds.at(i)[j] + curve._actualHerds.at(i)[j];
              self._actualAnimals.at(i)[j] := self._actualAnimals.at(i)[j] + curve._actualAnimals.at(i)[j];
              self._apparentHerds.at(i)[j] := self._apparentHerds.at(i)[j] + curve._apparentHerds.at(i)[j];
              self._apparentAnimals.at(i)[j] := self._apparentAnimals.at(i)[j] + curve._apparentAnimals.at(i)[j];
            end
          ;
        end
      ;
    end
  ;


  function TSMSummaryEpiCurve.valueOnDay(
        day: integer;
        curveType: TSummaryEpiCurveType;
        calcType: TSummaryEpiCurveCalculation
      ): extended;
    begin
      case curveType of
        EpiCurveActualHerds: result := actualHerdsOnDay( day, calcType );
        EpiCurveActualAnimals: result := actualAnimalsOnDay( day, calcType );
        EpiCurveApparentHerds: result := apparentHerdsOnDay( day, calcType );
        EpiCurveApparentAnimals: result := apparentAnimalsOnDay( day, calcType );
      else
        raise exception.create( 'Unrecognized curveType in TSMSummaryEpiCurve.valueOnDay' );
      end;
    end
  ;


  function TSMSummaryEpiCurve.actualHerdsOnDayByIteration( day: integer; iteration: integer ): extended;
    begin
      result := _actualHerds.getValue( day, iteration );
    end
  ;

  function TSMSummaryEpiCurve.actualHerdsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
    begin
      case calcType of
        EpiCalcHigh: result := _actualHerds.highForDay( day );
        EpiCalc95Percentile: result := _actualHerds.p95ForDay( day );
        EpiCalc75Percentile: result := _actualHerds.p75ForDay( day );
        EpiCalc50Percentile, EpiCalcMedian: result := _actualHerds.p50ForDay( day );
        EpiCalcMean: result := _actualHerds.meanForDay( day );
        EpiCalc25Percentile: result := _actualHerds.p25ForDay( day );
        EpiCalc5Percentile: result := _actualHerds.p5ForDay( day );
        EpiCalcLow: result := _actualHerds.lowForDay( day );
      else
        raise exception.create( 'Unrecognized calcType in TSMSummaryEpiCurve.actualHerdsOnDay' );
      end;
    end
  ;

  function TSMSummaryEpiCurve.actualAnimalsOnDayByIteration( day: integer; iteration: integer ): extended;
    begin
      result := _actualAnimals.getValue( day, iteration );
    end
  ;

  function TSMSummaryEpiCurve.actualAnimalsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
    begin
      case calcType of
        EpiCalcHigh: result := _actualAnimals.highForDay( day );
        EpiCalc95Percentile: result := _actualAnimals.p95ForDay( day );
        EpiCalc75Percentile: result := _actualAnimals.p75ForDay( day );
        EpiCalc50Percentile, EpiCalcMedian: result := _actualAnimals.p50ForDay( day );
        EpiCalcMean: result := _actualAnimals.meanForDay( day );
        EpiCalc25Percentile: result := _actualAnimals.p25ForDay( day );
        EpiCalc5Percentile: result := _actualAnimals.p5ForDay( day );
        EpiCalcLow: result := _actualAnimals.lowForDay( day );
      else
        raise exception.create( 'Unrecognized calcType in TSMSummaryEpiCurve.actualAnimalsOnDay' );
      end;
    end
  ;

  function TSMSummaryEpiCurve.apparentHerdsOnDayByIteration( day: integer; iteration: integer ): extended;
    begin
      result := _apparentHerds.getValue( day, iteration );
    end
  ;

  function TSMSummaryEpiCurve.apparentHerdsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
    begin
      case calcType of
        EpiCalcHigh: result := _apparentHerds.highForDay( day );
        EpiCalc95Percentile: result := _apparentHerds.p95ForDay( day );
        EpiCalc75Percentile: result := _apparentHerds.p75ForDay( day );
        EpiCalc50Percentile, EpiCalcMedian: result := _apparentHerds.p50ForDay( day );
        EpiCalcMean: result := _apparentHerds.meanForDay( day );
        EpiCalc25Percentile: result := _apparentHerds.p25ForDay( day );
        EpiCalc5Percentile: result := _apparentHerds.p5ForDay( day );
        EpiCalcLow: result := _apparentHerds.lowForDay( day );
      else
        raise exception.create( 'Unrecognized calcType in TSMSummaryEpiCurve.apparentHerdsOnDay' );
      end;
    end
  ;

  function TSMSummaryEpiCurve.apparentAnimalsOnDayByIteration( day: integer; iteration: integer ): extended;
    begin
      result := _apparentAnimals.getValue( day, iteration );
    end
  ;

  function TSMSummaryEpiCurve.apparentAnimalsOnDay( day: integer; calcType: TSummaryEpiCurveCalculation ): extended;
    begin
      case calcType of
        EpiCalcHigh: result := _apparentAnimals.highForDay( day );
        EpiCalc95Percentile: result := _apparentAnimals.p95ForDay( day );
        EpiCalc75Percentile: result := _apparentAnimals.p75ForDay( day );
        EpiCalc50Percentile, EpiCalcMedian: result := _apparentAnimals.p50ForDay( day );
        EpiCalcMean: result := _apparentAnimals.meanForDay( day );
        EpiCalc25Percentile: result := _apparentAnimals.p25ForDay( day );
        EpiCalc5Percentile: result := _apparentAnimals.p5ForDay( day );
        EpiCalcLow: result := _apparentAnimals.lowForDay( day );
      else
        raise exception.create( 'Unrecognized calcType in TSMSummaryEpiCurve.apparentAnimalsOnDay' );
      end;
    end
  ;




  constructor TSMSummaryEpiCurves.create( db: TSMDatabase );
    var
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;

      i: integer;

      curve: TSMSummaryEpiCurve;
      curve2: TSMSummaryEpiCurve;
      iterations: integer;
      days: integer;
    begin
      inherited create();

      days := db.daysInLongestIteration();
      iterations := db.completedIterations();

      dbcout( 'Days = ' + intToStr( days ), DBSMEPICURVES );
      dbcout( 'Iterations = ' + intToStr( iterations ), DBSMEPICURVES );

      // Create epi curves for the individual production types.
      //-------------------------------------------------------
      db2 := db as TSqlDatabase;
      q := 'SELECT productionTypeID, descr FROM inProductionType';

      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();
      while( nil <> row ) do
        begin
          dbcout( 'Creating curve for ' + row.field('descr') + '...', DBSMEPICURVES );
          curve := TSMSummaryEpiCurve.create( db, row.field('productionTypeID'), iterations, days );
          self[row.field('descr')] := curve;
          dbcout( 'Curve created.', DBSMEPICURVES );

          row := res.fetchArrayNext();
        end
      ;

      res.free();

      dbcout( 'Creating curve for all pts...', DBSMEPICURVES );
      // Create one more epi curve for all production types.
      curve := TSMSummaryEpiCurve.create( iterations, days );
      dbcout( 'Done creating curve.', DBSMEPICURVES );


      dbcout( 'Filling curve for all pts...', DBSMEPICURVES );
      for i := 0 to self.Count - 1 do
        begin
          dbcout( 'Adding from curve ' + intToStr( i ) + '...', DBSMEPICURVES );
          curve2 := self.getItemByIndex( i ) as TSMSummaryEpiCurve;
          curve.addFrom( curve2 );
          dbcout( 'Done adding.', DBSMEPICURVES );
        end
      ;
      dbcout( 'Done filling.', DBSMEPICURVES );

      // Finally, add this last curve to the dictionary.
      self[ tr( 'All production types' ) ] := curve;

      dbcout( 'Done', DBSMEPICURVES );
    end
  ;


  destructor TSMSummaryEpiCurves.destroy();
    begin
      self.FreeItems();
      inherited destroy();
    end
  ;


  function TSMSummaryEpiCurves.curveForProductionType( const prodType: string ): TSMSummaryEpiCurve;
    begin
      result := self[prodType] as TSMSummaryEpiCurve;
    end
  ;

end.
