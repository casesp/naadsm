unit FrameSummaryEpiCurveTable;

(*
FrameSummaryEpiCurveTable.pas/dfm
---------------------------------
Begin: 2005/09/20
Last revision: $Date: 2008/03/12 22:10:51 $ $Author: areeves $
Version number: $Revision: 1.10 $
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
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    Grids,

    FrameStringGridBase,

    SMEpiCurves,
    ProductionType, ARSyncGrid
  ;

  type TFrameSummaryEpiCurveTable = class( TFrameStringGridBase )
    protected
      _epiCurves: TSMSummaryEpiCurves;
      _pt: TProductionType;
      _useActualCurve: boolean;
      _useHerds: boolean;
      _daysPerInterval: integer;

      procedure fillTable();
      procedure setHeaderRow();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure showCurves(
        epiCurves: TSMSummaryEpiCurves;
        prodType: TProductionType;
        useActualCurve: boolean;
        useHerds: boolean;
        daysPerInterval: integer
      );

    end
  ;


  const
    COL_DAY = 0;
    COL_MEAN = 1;
    COL_LOW = 2;
    COL_HIGH = 3;
    COL_P5 = 4;
    COL_P25 = 5;
    COL_P50 = 6;
    COL_P75 = 7;
    COL_P95 = 8;


implementation

  {$R *.dfm}

  uses
    MyStrUtils,
    GuiStrUtils,
    MyDelphiArrayUtils,
    I88n
  ;


  constructor TFrameSummaryEpiCurveTable.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
    end
  ;

  destructor TFrameSummaryEpiCurveTable.destroy();
    begin
      inherited destroy();
    end
  ;


  procedure TFrameSummaryEpiCurveTable.showCurves(
        epiCurves: TSMSummaryEpiCurves;
        prodType: TProductionType;
        useActualCurve: boolean;
        useHerds: boolean;
        daysPerInterval: integer
      );
    begin
      _epiCurves := epiCurves;
      _pt := prodType;
      _useActualCurve := useActualCurve;
      _useHerds := useHerds;
      _daysPerInterval := daysPerInterval;


      fillTable();
    end
  ;


  procedure TFrameSummaryEpiCurveTable.fillTable();
    var
      epiCurve: TSMSummaryEpiCurve;
      key: string;
      curveType: TSummaryEpiCurveType;
      x, y: integer;
      i: integer;
      val: array of extended;
      startDay, endDay, dayCounter: integer;
      rowCounter: integer;
    begin
      endDay := 0;

      clearGrid( true );

      if( nil = _pt ) then
        key := tr( 'All production types' )
      else
        key := _pt.productionTypeDescr
      ;

      epiCurve := _epiCurves[key] as TSMSummaryEpiCurve;

      // Set the size of the grid for the appropriate number of intervals
      //------------------------------------------------------------------
      x := epiCurve.daysInCurve - 1;
      y := x div _daysPerInterval;
      if( 0 <> x mod _daysPerInterval ) then inc( y );

      inc( y ); // allow for day 0 to have its own row
      stgGrid.RowCount := y + 1; // Allow for the fixed header row
      stgGrid.ColCount := COL_P95 + 1; // The number of calculations, plus one for the fixed column

      setHeaderRow();

      // Determine which curve to draw
      //------------------------------
      curveType := EpiCurveUnspecified;

      if( _useHerds and _useActualCurve ) then
        curveType := EpiCurveActualHerds
      else if( _useHerds and not( _useActualCurve ) ) then
        curveType := EpiCurveApparentHerds
      else if( not( _useHerds ) and _useActualCurve ) then
        curveType := EpiCurveActualAnimals
      else if( not( _useHerds ) and not( _useActualCurve ) ) then
        curveType := EpiCurveApparentAnimals
      ;

      // Remember: day 0 is a special case.  Alway show it on its own.
      stgGrid.Cells[COL_DAY, 1] := '0';
      stgGrid.Cells[COL_MEAN, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalcMean ) );
      stgGrid.Cells[COL_LOW, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalcLow ) );
      stgGrid.Cells[COL_HIGH, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalcHigh ) );
      stgGrid.Cells[COL_P5, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalc5Percentile ) );
      stgGrid.Cells[COL_P25, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalc25Percentile ) );
      stgGrid.Cells[COL_P50, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalc50Percentile ) );
      stgGrid.Cells[COL_P75, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalc75Percentile ) );
      stgGrid.Cells[COL_P95, 1] := uiFloatToStr( epiCurve.valueOnDay( 0, curveType, EpiCalc95Percentile ) );

      // Fill the table for all other days, based on the selected number of days per interval
      setLength( val, COL_P95 + 1 );
      zeroArray( val );
      startDay := 1;
      dayCounter := 0;
      rowCounter := 1;

      for i := 1 to epiCurve.daysInCurve - 1 do
        begin
          endDay := i;
          inc( dayCounter );
          val[COL_MEAN] := val[COL_MEAN] + epiCurve.valueOnDay( i, curveType, EpiCalcMean );
          val[COL_LOW] := val[COL_LOW] + epiCurve.valueOnDay( i, curveType, EpiCalcLow );
          val[COL_HIGH] := val[COL_HIGH] + epiCurve.valueOnDay( i, curveType, EpiCalcHigh );
          val[COL_P5] := val[COL_P5] + epiCurve.valueOnDay( i, curveType, EpiCalc5Percentile );
          val[COL_P25] := val[COL_P25] + epiCurve.valueOnDay( i, curveType, EpiCalc25Percentile );
          val[COL_P50] := val[COL_P50] + epiCurve.valueOnDay( i, curveType, EpiCalc50Percentile );
          val[COL_P75] := val[COL_P75] + epiCurve.valueOnDay( i, curveType, EpiCalc75Percentile );
          val[COL_P95] := val[COL_P95] + epiCurve.valueOnDay( i, curveType, EpiCalc95Percentile );

          if( _daysPerInterval = dayCounter ) then
            begin
              inc( rowCounter );

              if( 1 = _daysPerInterval ) then
                stgGrid.Cells[COL_DAY, rowCounter] := intToStr( i )
              else
                stgGrid.Cells[COL_DAY, rowCounter] := intToStr( startDay ) + ' - ' + intToStr( endDay )
              ;

              stgGrid.Cells[COL_MEAN, rowCounter] := uiFloatToStr( val[COL_MEAN] );
              stgGrid.Cells[COL_LOW, rowCounter] := uiFloatToStr( val[COL_LOW] );
              stgGrid.Cells[COL_HIGH, rowCounter] := uiFloatToStr( val[COL_HIGH] );
              stgGrid.Cells[COL_P5, rowCounter] := uiFloatToStr( val[COL_P5] );
              stgGrid.Cells[COL_P25, rowCounter] := uiFloatToStr( val[COL_P25] );
              stgGrid.Cells[COL_P50, rowCounter] := uiFloatToStr( val[COL_P50] );
              stgGrid.Cells[COL_P75, rowCounter] := uiFloatToStr( val[COL_P75] );
              stgGrid.Cells[COL_P95, rowCounter] := uiFloatToStr( val[COL_P95] );

              dayCounter := 0;
              startDay := i + 1;
              zeroArray( val );
            end
          ;
        end
      ;

      i := epiCurve.daysInCurve - 1;

      // Write the last row, if needed
      if( 0 <> dayCounter ) then
        begin
          inc( rowCounter );

          if( 1 = _daysPerInterval ) then
            stgGrid.Cells[COL_DAY, rowCounter] := intToStr( i )
          else
            stgGrid.Cells[COL_DAY, rowCounter] := intToStr( startDay ) + ' - ' + intToStr( endDay )
          ;

          stgGrid.Cells[COL_MEAN, rowCounter] := uiFloatToStr( val[COL_MEAN] );
          stgGrid.Cells[COL_LOW, rowCounter] := uiFloatToStr( val[COL_LOW] );
          stgGrid.Cells[COL_HIGH, rowCounter] := uiFloatToStr( val[COL_HIGH] );
          stgGrid.Cells[COL_P5, rowCounter] := uiFloatToStr( val[COL_P5] );
          stgGrid.Cells[COL_P25, rowCounter] := uiFloatToStr( val[COL_P25] );
          stgGrid.Cells[COL_P50, rowCounter] := uiFloatToStr( val[COL_P50] );
          stgGrid.Cells[COL_P75, rowCounter] := uiFloatToStr( val[COL_P75] );
          stgGrid.Cells[COL_P95, rowCounter] := uiFloatToStr( val[COL_P95] );
        end
      ;

      // Clean up
      setLength( val, 0 );
    end
  ;


  procedure TFrameSummaryEpiCurveTable.setHeaderRow();
    begin
      // FIX ME: what about StdDev?

      stgGrid.Cells[COL_DAY,0] := tr( 'Day' );
      stgGrid.Cells[COL_MEAN,0] := tr( 'Mean' );
      stgGrid.Cells[COL_LOW,0] := tr( 'Low' );
      stgGrid.Cells[COL_HIGH,0] := tr( 'High' );
      stgGrid.Cells[COL_P5,0] := tr( 'p5' );
      stgGrid.Cells[COL_P25,0] := tr( 'p25' );
      stgGrid.Cells[COL_P50,0] := tr( 'p50' );
      stgGrid.Cells[COL_P75,0] := tr( 'p75' );
      stgGrid.Cells[COL_P95,0] := tr( 'p95' );
    end
  ;


end.
