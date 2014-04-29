unit FrameSummaryEpiCurves;

(*
FrameSummaryEpiCurves.pas/dfm
-----------------------------
Begin: 2005/09/20
Last revision: $Date: 2009-06-05 19:52:36 $ $Author: areeves $
Version number: $Revision: 1.16 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Animal Population Health Institute, Colorado State University

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
    TeEngine,
    Series,
    TeeProcs,
    Chart,
    StdCtrls,
    ExtCtrls,

		FrameChartBase,
		
    SMEpiCurves,
    ProductionType
  ;

  type TFrameSummaryEpiCurves = class( TFrameChartBase )
      pnlSummaryEpiCurveOptions: TPanel;
      
      chtSummaryEpiCurves: TChart;
      ser95: TLineSeries;
      ser75: TLineSeries;
      ser50: TLineSeries;
      serMean: TLineSeries;
      ser25: TLineSeries;
      ser5: TLineSeries;

      pnlPercentileOptions: TPanel;
      cbx95: TCheckBox;
      line95: TPanel;
      cbx75: TCheckBox;
      line75: TPanel;
      cbx50: TCheckBox;
      line50: TPanel;
      cbxMean: TCheckBox;
      lineMean: TPanel;
      cbx25: TCheckBox;
      line25: TPanel;
      cbx5: TCheckBox;
      line5: TPanel;

      pnlCumulative: TPanel;
      cbxCumulative: TCheckBox;

      pnl3D: TPanel;
      cbx3D: TCheckBox;

      procedure redrawSeries( Sender: TObject );

    protected
      _epiCurves: TSMSummaryEpiCurves;
      _pt: TProductionType;
      _useActualCurve: boolean;
      _useHerds: boolean;
      _daysPerInterval: integer;

      procedure translateUI();
      procedure translateUIManual();

      procedure clearSeries();
      procedure drawAllSeries();

      procedure drawSeries(
        series: TLineSeries;
        epiCurve: TSMSummaryEpiCurve;
        curveType: TSummaryEpiCurveType;
        calcType: TSummaryEpiCurveCalculation
      );

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
    COLOR_95 = clRed;
    COLOR_75 = clYellow;
    COLOR_50 = clGreen;
    COLOR_MEAN = clFuchsia;
    COLOR_25 = clOlive;
    COLOR_5 = clBlue;

implementation

{$R *.dfm}

  uses
    StrUtils,
    
    MyStrUtils,
    I88n
  ;

  constructor TFrameSummaryEpiCurves.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      line95.Color := COLOR_95;
      ser95.SeriesColor := COLOR_95;

      line75.Color := COLOR_75;
      ser75.SeriesColor := COLOR_75;

      line50.Color := COLOR_50;
      ser50.SeriesColor := COLOR_50;

      lineMean.Color := COLOR_MEAN;
      serMean.SeriesColor := COLOR_MEAN;

      line25.Color := COLOR_25;
      ser25.SeriesColor := COLOR_25;

      line5.Color := COLOR_5;
      ser5.SeriesColor := COLOR_5;

      clearSeries();
    end
  ;


  procedure TFrameSummaryEpiCurves.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 20:54:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameSummaryEpiCurves.dfm
      // File date: Thu Feb 28 09:42:47 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbx95.Caption := tr( '95th percentile' );
          cbx75.Caption := tr( '75th percentile' );
          cbx50.Caption := tr( '50th percentile (median)' );
          cbxMean.Caption := tr( 'Mean' );
          cbx25.Caption := tr( '25th percentile' );
          cbx5.Caption := tr( '5th percentile' );
          cbxCumulative.Caption := tr( 'Cumulative data' );
          cbx3D.Caption := tr( '3-D View' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtSummaryEpiCurves.BottomAxis.Title.Caption := tr( 'Simulation day' );
          chtSummaryEpiCurves.LeftAxis.Title.Caption := tr( 'Number of infected units' );
          chtSummaryEpiCurves.Title.Text.Strings[0] := tr( 'TChart' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameSummaryEpiCurves.translateUIManual();
    begin
      with self.chtSummaryEpiCurves do
        begin
          Series[0].Title := tr( '95th percentile' );
          Series[0].Title := tr( '75th percentile' );
          Series[0].Title := tr( 'Median' );
          Series[0].Title := tr( 'Mean' );
          Series[0].Title := tr( '25th percentile' );
          Series[0].Title := tr( '5th percentile' );
        end
      ;
    end
  ;


  destructor TFrameSummaryEpiCurves.destroy();
    begin
      inherited destroy();
    end
  ;

  procedure TFrameSummaryEpiCurves.clearSeries();
    begin
      ser95.Clear();
      ser75.Clear();
      ser50.Clear();
      serMean.Clear();
      ser25.Clear();
      ser5.Clear();
    end
  ;



  procedure TFrameSummaryEpiCurves.showCurves(
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

      drawAllSeries();
    end
  ;




  procedure TFrameSummaryEpiCurves.drawAllSeries();
    var
      epiCurve: TSMSummaryEpiCurve;
      chartTitle, leftAxisLabel: string;
      key: string;
      curveType: TSummaryEpiCurveType;
    begin
      clearSeries();

      chtSummaryEpiCurves.View3D := cbx3D.Checked;

      if( nil = _pt ) then
        key := tr( 'All production types' )
      else
        key := _pt.productionTypeDescr
      ;

      epiCurve := _epiCurves[key] as TSMSummaryEpiCurve;


      // Write the chart titles
      //-----------------------
      leftAxisLabel := '';
      chartTitle := '';

      if( _useActualCurve and _useHerds ) then
        begin
          chartTitle := tr( 'Actual epidemic curve (Units)' );
          leftAxisLabel := ansiReplaceStr( tr( 'Newly infected units for each xyz-day interval' ), 'xyz', intToStr( _daysPerInterval ) );
        end
      else if( _useActualCurve and not( _useHerds ) ) then
        begin
          chartTitle := tr( 'Actual epidemic curve (Animals)' );
          leftAxisLabel := ansiReplaceStr( tr( 'Animals in newly infected units for each xyz-day interval' ), 'xyz', intToStr( _daysPerInterval ) );
        end
      else if( not( _useActualCurve ) and _useHerds ) then
        begin
          chartTitle := tr( 'Apparent epidemic curve (Units)' );
          leftAxisLabel := ansiReplaceStr( tr( 'Newly detected units for each xyz-day interval' ), 'xyz', intToStr( _daysPerInterval ) );
        end
      else if( not( _useActualCurve ) and not( _useHerds ) ) then
        begin
          chartTitle := tr( 'Apparent epidemic curve (Animals)' );
          leftAxisLabel := ansiReplaceStr( tr( 'Animals in newly detected units for each xyz-day interval' ), 'xyz', intToStr( _daysPerInterval ) );
        end
      ;

      chartTitle := chartTitle + ': ' + key;

      chtSummaryEpiCurves.Title.Text.Clear();
      chtSummaryEpiCurves.Title.Text.Add( chartTitle );
      chtSummaryEpiCurves.LeftAxis.Title.Caption := leftAxisLabel;

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

      // Draw the series
      //----------------
      if( cbx5.Checked ) then drawSeries( ser5, epiCurve, curveType, EpiCalc5Percentile );
      if( cbx25.Checked ) then drawSeries( ser25, epiCurve, curveType, EpiCalc25Percentile );
      if( cbx50.Checked ) then drawSeries( ser50, epiCurve, curveType, EpiCalc50Percentile );
      if( cbx75.Checked ) then drawSeries( ser75, epiCurve, curveType, EpiCalc75Percentile );
      if( cbx95.Checked ) then drawSeries( ser95, epiCurve, curveType, EpiCalc95Percentile );
      if( cbxMean.Checked ) then drawSeries( serMean, epiCurve, curveType, EpiCalcMean );
    end
  ;


  procedure TFrameSummaryEpiCurves.drawSeries(
        series: TLineSeries;
        epiCurve: TSMSummaryEpiCurve;
        curveType: TSummaryEpiCurveType;
        calcType: TSummaryEpiCurveCalculation
      );
    var
      i, j: integer;
      val: extended;
    begin
      // Remember: day 0 is a special case.  Alway show it on its own.
      series.AddXY( 0, epiCurve.valueOnDay( 0, curveType, calcType ) );

      j := 0;
      val := 0.0;
      for i := 1 to epiCurve.daysInCurve - 1 do
        begin
          val := val + epiCurve.valueOnDay( i, curveType, calcType );
          inc( j );
          if( _daysPerInterval = j ) then
            begin
              series.AddXY( i, val );
              j := 0;
              if( not( cbxCumulative.Checked ) ) then val := 0.0;
            end
          ;
        end
      ;

      i := epiCurve.daysInCurve - 1;

      if( 0 <> j ) then
        begin
          series.AddXY( i, val );
          if( not( cbxCumulative.Checked ) and ( 0 < val ) ) then series.AddXY( i + 1, 0.0 );
        end
      ;
    end
  ;

  procedure TFrameSummaryEpiCurves.redrawSeries( Sender: TObject );
    begin
      drawAllSeries();
    end
  ;

end.
