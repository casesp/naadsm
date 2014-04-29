{*
FrameDailyStatusByProdTypeDetectionStats.pas/dfm
------------------------------------------------
Begin: 2005/12/13
Last revision: $Date: 2010-06-22 22:18:15 $ $Author: areeves $
Version number: $Revision: 1.1.2.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
Author: Ric Hupalo <Ric.Hupalo@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

  This frame displays a graph showing the number of herds in each Testing state,
  and the Detection Status on each day of a single iteration.
  The frame is dependant on being housed on FormDailyStatusByProdType.
  The frame has dependencies on this container unit and FormMain.
}

unit FrameDailyStatusByProdTypeDetectionStats;

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
    Series, 
    TeEngine, 
    TeeProcs, 
    Chart, 
    StdCtrls,
    ExtCtrls,
    MyStrUtils,
    Math,
    StrUtils,

    FrameChartBase,
    debugWindow,
    StatusEnums,

    // Application-specific data structures
    SMSimOutByProdType

  ;


  type
    TFrameDailyStatusByProdTypeDetectionStats = class(TFrameChartBase)

      SeriesExamined: TLineSeries;
      SeriesDetected: TLineSeries;
      SeriesDestroyed: TLineSeries;
      SeriesTestTruePos: TLineSeries;
      SeriesTestTrueNeg: TLineSeries;
      SeriesTestFalsePos: TLineSeries;
      SeriesTestFalseNeg: TLineSeries;

      SeriesPDetection: TPointSeries;
      SeriesPDestruction: TPointSeries;
      SeriesPVaccination: TPointSeries;
      SeriesPOver: TPointSeries;

      pnlChartOptions: TPanel;
      cbxThreeD: TCheckBox;
      chtOutputs: TChart;
      pnlCheckBoxes: TPanel;
      cbxExamined: TCheckBox;
      lineExamined: TPanel;
      cbxTestFalseNeg: TCheckBox;
      cbxDestroyed: TCheckBox;
      lineTestFalseNeg: TPanel;
      lineDestroyed: TPanel;
      cbxDetected: TCheckBox;
      lineDetected: TPanel;
      cbxTestTrueNeg: TCheckBox;
      lineTestTrueNeg: TPanel;
      cbxTestFalsePos: TCheckBox;
      lineTestFalsePos: TPanel;
      cbxTestTruePos: TCheckBox;
      lineTestTruePos: TPanel;
      cbxCumulative: TCheckBox;
      cbxMilestones: TCheckBox;

      procedure cbxThreeDClick(Sender: TObject);
      procedure cbxClick(Sender: TObject);
      procedure cbxCumulativeClick(Sender: TObject);

    protected
      _myMainForm: TForm;       // pointer to the main form to access properties
      _meCreated: boolean;      // avoid access violations on create (prevents event handlers from firing)
      _chartFixNeeded: boolean; // Flag for TChart bug work around.
      
      procedure translateUI();
      procedure translateUIManual();
      procedure setVertAxisMinMax( min, max: double );
      procedure setVertAxisAutomatic();
      procedure setHorizAxisMinMax( min, max: double );
      procedure setHorizAxisAutomatic();
      procedure initializeChartColors();

      function seriesMaxY(): double;
      procedure redrawEvents();
      procedure fixTChartBug();
      
    public
      constructor create( AOwner: TComponent ); override;

      procedure drawAllSeries(day: integer; DailyData: TSMDailyOutput );
      procedure drawFirstDetection( const day: integer );
      procedure drawFirstDestruction( const day: integer );
      procedure drawFirstVaccination( const day: integer );
      procedure drawOutbreakEnd( const day: integer );

      procedure reset();
    end
  ;


implementation

{$R *.dfm}

  uses
    Types,
    
    I88n,
    RoundToXReplacement_3c,
    
    FormMain,
    FormDailyStatusByProdType
  ;
  

  constructor TFrameDailyStatusByProdTypeDetectionStats.create( AOwner: TComponent );
    begin
      inherited create( AOwner );

      _meCreated := false;
      _chartFixNeeded := true;
      
      // Validate and set access to FormDailyStatusByProdType properties and methods
      if ( AOwner.name <> 'FormDailyStatusByProdType' ) then
        begin
          raise exception.Create('Frame ' + self.Name + ' expects to be in FormDailyStatusByProdType.');
          self.Enabled := false;  // this is a frame, so the user can still close the owner form
        end
      ;

      // Provide access to FormMain variables
      if( AOwner.Owner is TFormMain ) then  //Owner should be FormDailyStatusByProdType
        try
          _myMainForm := FormMain.frmMain;  //Need access to its properties
        except
           _myMainForm := nil;
          raise exception.Create('Frame ' + self.Name + ' requires FormMain be running.');
          self.Enabled := false;  // this is a frame, so the user can still close the owner form
        end
      else
        begin
          _myMainForm := nil;
          raise exception.Create('Unit ' + self.Name + ' has dependencies on FormMain.');
          self.Enabled := false;
        end
      ;

      translateUI();
      chtOutputs.View3D := False;
      cbxMilestones.Checked := true;

      // These are not just properties, their click events fire! (hence need for _meCreated)
      cbxExamined.Checked := true;
      cbxDetected.Checked := true;
      cbxDestroyed.Checked := true;
      cbxTestTruePos.Checked := true;
      cbxTestTrueNeg.Checked := true;
      cbxTestFalsePos.Checked := true;
      cbxTestFalseNeg.Checked := true;

      initializeChartColors;

      _meCreated:= true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.4.
      // Generation date: Fri Nov 6 09:26:07 2009
      // File name: C:/Documents and Settings/areeves/My Documents/NAADSM/Interface-naadsm_3_line_interface/sm_forms/FrameDailyStatusByProdTypeDetectionStats.dfm
      // File date: Fri Nov 6 09:07:12 2009

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxThreeD.Caption := tr( '3-D View' );
          cbxMilestones.Caption := tr( 'Events' );
          cbxCumulative.Caption := tr( 'Cumulative' );
          cbxExamined.Caption := tr( 'Examined' );
          cbxTestFalseNeg.Caption := tr( 'Test false neg' );
          cbxDestroyed.Caption := tr( 'Destroyed' );
          cbxDetected.Caption := tr( 'Detected' );
          cbxTestTrueNeg.Caption := tr( 'Test true neg' );
          cbxTestFalsePos.Caption := tr( 'Test false pos' );
          cbxTestTruePos.Caption := tr( 'Test true pos' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtOutputs.BottomAxis.Title.Caption := tr( 'Day of simulation' );
          chtOutputs.LeftAxis.Title.Caption := tr( 'Daily number of units' );
          chtOutputs.Series[0].Title := tr( 'Examined' );
          chtOutputs.Series[1].Title := tr( 'Detected' );
          chtOutputs.Series[2].Title := tr( 'Destroyed' );
          chtOutputs.Series[3].Title := tr( 'Test true positive' );
          chtOutputs.Series[4].Title := tr( 'Test true negative' );
          chtOutputs.Series[5].Title := tr( 'Test false positive' );
          chtOutputs.Series[6].Title := tr( 'Test false negative' );
          chtOutputs.Series[7].Title := tr( 'First Detection' );
          chtOutputs.Series[8].Title := tr( 'First destruction' );
          chtOutputs.Series[9].Title := tr( 'First vaccination' );
          chtOutputs.Series[10].Title := tr( 'Outbreak over' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.translateUIManual();
    begin
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.initializeChartColors();
    begin
      // These colors and enums are defined in Unit StatusEnums

      chtOutputs.Series[0].SeriesColor := detectionStatusColor(dsExamined);
      chtOutputs.Series[1].SeriesColor := detectionStatusColor(dsDetectedClinical);
      chtOutputs.Series[2].SeriesColor := detectionStatusColor(dsDestroyed);
      chtOutputs.Series[3].SeriesColor := detectionStatusColor(dsTestTruePos);
      chtOutputs.Series[4].SeriesColor := detectionStatusColor(dsTestTrueNeg);
      chtOutputs.Series[5].SeriesColor := detectionStatusColor(dsTestFalsePos);
      chtOutputs.Series[6].SeriesColor := detectionStatusColor(dsTestFalseNeg);

      { Observation regarding the TPointSeries (versus the TLineSeries):
        Setting the series color here like:
          chtOutputs.Series[8].SeriesColor := eventStatusColor(esFirstDetection);
        or on the container form like:
         cdDiseaseStatus: fraDiseaseStatus.seriesPDetection.addXY( day, fraDiseaseStatus.eventY(), '', eventStatusColor(esFirstDetection) );
        has NO effect on the run-time point symbol color; in the legend or on the chart!
        The cast below works:
      }

      (chtOutputs.Series[7] as TPointSeries).Pointer.Brush.Color := eventStatusColor(esFirstDetection);
      (chtOutputs.Series[8] as TPointSeries).Pointer.Brush.Color := eventStatusColor(esFirstDestruction);
      (chtOutputs.Series[9] as TPointSeries).Pointer.Brush.Color := eventStatusColor(esFirstVaccination);
      (chtOutputs.Series[10] as TPointSeries).Pointer.Brush.Color := eventStatusColor(esOutbreakOver);

      lineExamined.Color := detectionStatusColor(dsExamined);
      lineDetected.Color := detectionStatusColor(dsDetectedClinical);
      lineDestroyed.Color := detectionStatusColor(dsDestroyed);
      lineTestTruePos.Color := detectionStatusColor(dsTestTruePos);
      lineTestTrueNeg.Color := detectionStatusColor(dsTestTrueNeg);
      lineTestFalsePos.Color := detectionStatusColor(dsTestFalsePos);
      lineTestFalseNeg.Color := detectionStatusColor(dsTestFalseNeg);
    end
  ;


  {*
    Adds a day of values to it's chart data series
    @param day The day  (0 to n) in the iteration to update
    @param DailyData Data structure holding the data
  }
  procedure TFrameDailyStatusByProdTypeDetectionStats.drawAllSeries( day: integer; DailyData: TSMDailyOutput );
    begin
      if( ( not cbxCumulative.Checked ) or ( 0 = SeriesExamined.YValues.Count ) ) then
        begin
          SeriesExamined.AddXY(     day, Dailydata.exmnUAll );
          SeriesDetected.AddXY(     day, ( Dailydata.detnUClin + Dailydata.detnUTest ) );
          SeriesDestroyed.AddXY(    day, Dailydata.desnUAll );
          SeriesTestTruePos.AddXY(  day, Dailydata.tstnUTruePos );
          SeriesTestTrueNeg.AddXY(  day, Dailydata.tstnUTrueNeg );
          SeriesTestFalsePos.AddXY( day, Dailydata.tstnUFalsePos );
          SeriesTestFalseNeg.AddXY( day, Dailydata.tstnUFalseNeg );
        end
      else
        begin
          SeriesExamined.AddXY(     day, Dailydata.exmnUAll + seriesExamined.YValues.Last );
          SeriesDetected.AddXY(     day, ( Dailydata.detnUClin + Dailydata.detnUTest ) + SeriesDetected.YValues.Last );
          SeriesDestroyed.AddXY(    day, Dailydata.desnUAll + SeriesDestroyed.YValues.Last );
          SeriesTestTruePos.AddXY(  day, Dailydata.tstnUTruePos + SeriesTestTruePos.YValues.Last );
          SeriesTestTrueNeg.AddXY(  day, Dailydata.tstnUTrueNeg + SeriesTestTrueNeg.YValues.Last );
          SeriesTestFalsePos.AddXY( day, Dailydata.tstnUFalsePos + SeriesTestFalsePos.YValues.Last );
          SeriesTestFalseNeg.AddXY( day, Dailydata.tstnUFalseNeg + SeriesTestFalseNeg.YValues.Last );
        end
      ;

      fixTChartBug();
    end
  ;

  
  {*
    Scales the charts for the proper min and max vlues for all the data series
    @return True if the current X,Y min and max values will sufffice for the latest data point.
  }
  procedure TFrameDailyStatusByProdTypeDetectionStats.fixTChartBug();
    var
      yNeededFix, xNeededFix: boolean;
      minX, maxX, minY, maxY: double;
      i: integer;
    begin
      if( not _chartFixNeeded ) then
        exit
      ;

      // if all series have the same min and max Y then
      // determine what the proper vert axis should be, and apply it to all series.
      // Otherwise, use automatic vert axis for all series.
      // Do the same for X values and horiz axis.

      // Points are added repeatedly to each series in this unit.
      // Once we get to the point where neither axis needed to have the fix applied when a new point
      // was added, it is no longer necessary to continue to carry out this check.
      // The result is used to indicate whether or not it is OK to stop checking.

      minX := 0.0;
      maxX := 0.0;
      minY := 0.0;
      maxY := 0.0;

      for i := 0 to chtOutputs.SeriesList.Count - 1 do
        begin
          if( not( chtOutputs.SeriesList.Series[ i ] is TPointSeries ) ) then
            begin
              if( chtOutputs.SeriesList.Series[ i ].Active ) then
                begin
                  minX := math.Min( minX, chtOutputs.Series[ i ].XValues.MinValue );
                  maxX := math.Max( maxX, chtOutputs.Series[ i ].XValues.MaxValue );
                  minY := math.Min( minY, chtOutputs.Series[ i ].YValues.MinValue );
                  maxY := math.Max( maxY, chtOutputs.Series[ i ].YValues.MaxValue );
                end
              ;
            end
          ;
        end
      ;

      if( EqualsValue = CompareValue( minY, maxY ) ) then
        begin
          setVertAxisMinMax( 0.0, minY + 1.0 );
          yNeededFix := true;
        end
      else
        begin
          setVertAxisAutomatic();
          yNeededFix := false;
        end
      ;

      if( EqualsValue = CompareValue( minX, maxX ) ) then
        begin
          setHorizAxisMinMax( 0.0, minX + 1.0 );
          xNeededFix := true;
        end
      else
        begin
          setHorizAxisAutomatic();
          xNeededFix := false;
        end
      ;

      _chartFixNeeded := ( yNeededFix or xNeededFix );
    end
  ;


  {*
    Resets the chart data series by clearing their data
  }
  procedure TFrameDailyStatusByProdTypeDetectionStats.reset();
    begin
      SeriesExamined.Clear();
      SeriesDetected.Clear();
      SeriesDestroyed.Clear();
      SeriesTestTruePos.Clear();
      SeriesTestTrueNeg.Clear();
      SeriesTestFalsePos.Clear();
      SeriesTestFalseNeg.Clear();

      SeriesPDetection.Clear();
      SeriesPDestruction.Clear();
      SeriesPVaccination.Clear();
      SeriesPOver.Clear();

      setHorizAxisMinMax( 0.0, 1.0 );
      setVertAxisMinMax( 0.0, 1.0 );

      _chartFixNeeded := true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.setHorizAxisAutomatic();
    begin
      SeriesExamined.GetHorizAxis.Automatic := true;
      SeriesDetected.GetHorizAxis.Automatic := true;
      SeriesDestroyed.GetHorizAxis.Automatic := true;
      SeriesTestTruePos.GetHorizAxis.Automatic := true;
      SeriesTestTrueNeg.GetHorizAxis.Automatic := true;
      SeriesTestFalsePos.GetHorizAxis.Automatic := true;
      SeriesTestFalseNeg.GetHorizAxis.Automatic := true;

      SeriesPDetection.GetHorizAxis.Automatic := true;
      SeriesPDestruction.GetHorizAxis.Automatic := true;
      SeriesPVaccination.GetHorizAxis.Automatic := true;
      SeriesPOver.GetHorizAxis.Automatic := true;
    end
  ;

  procedure TFrameDailyStatusByProdTypeDetectionStats.setHorizAxisMinMax(min, max: double);
    begin
      SeriesExamined.GetHorizAxis.SetMinMax( min, max );
      SeriesDetected.GetHorizAxis.SetMinMax( min, max );
      SeriesDestroyed.GetHorizAxis.SetMinMax( min, max );
      SeriesTestTruePos.GetHorizAxis.SetMinMax( min, max );
      SeriesTestTrueNeg.GetHorizAxis.SetMinMax( min, max );
      SeriesTestFalsePos.GetHorizAxis.SetMinMax( min, max );
      SeriesTestFalseNeg.GetHorizAxis.SetMinMax( min, max );

      SeriesPDetection.GetHorizAxis.SetMinMax( min, max );
      SeriesPDestruction.GetHorizAxis.SetMinMax( min, max );
      SeriesPVaccination.GetHorizAxis.SetMinMax( min, max );
      SeriesPOver.GetHorizAxis.SetMinMax( min, max );
    end
  ;

  procedure TFrameDailyStatusByProdTypeDetectionStats.setVertAxisAutomatic();
    begin
      SeriesExamined.GetVertAxis.Automatic := true;
      SeriesDetected.GetVertAxis.Automatic := true;
      SeriesDestroyed.GetVertAxis.Automatic := true;
      SeriesTestTruePos.GetVertAxis.Automatic := true;
      SeriesTestTrueNeg.GetVertAxis.Automatic := true;
      SeriesTestFalsePos.GetVertAxis.Automatic := true;
      SeriesTestFalseNeg.GetVertAxis.Automatic := true;

      SeriesPDetection.GetVertAxis.Automatic := true;
      SeriesPDestruction.GetVertAxis.Automatic := true;
      SeriesPVaccination.GetVertAxis.Automatic := true;
      SeriesPOver.GetVertAxis.Automatic := true;
    end
  ;

  procedure TFrameDailyStatusByProdTypeDetectionStats.setVertAxisMinMax(min, max: double);
    begin
      SeriesExamined.GetVertAxis.SetMinMax( min, max );
      SeriesDetected.GetVertAxis.SetMinMax( min, max );
      SeriesDestroyed.GetVertAxis.SetMinMax( min, max );
      SeriesTestTruePos.GetVertAxis.SetMinMax( min, max );
      SeriesTestTrueNeg.GetVertAxis.SetMinMax( min, max );
      SeriesTestFalsePos.GetVertAxis.SetMinMax( min, max );
      SeriesTestFalseNeg.GetVertAxis.SetMinMax( min, max );

      SeriesPDetection.GetVertAxis.SetMinMax( min, max );
      SeriesPDestruction.GetVertAxis.SetMinMax( min, max );
      SeriesPVaccination.GetVertAxis.SetMinMax( min, max );
      SeriesPOver.GetVertAxis.SetMinMax( min, max );
    end
  ;


  function TFrameDailyStatusByProdTypeDetectionStats.seriesMaxY(): double;
    var
      i: integer;
    begin
      result := 0.0;

      for i := 0 to chtOutputs.SeriesList.Count - 1 do
        begin
          if( not( chtOutputs.SeriesList.Series[ i ] is TPointSeries ) ) then
            begin
              if( chtOutputs.SeriesList.Series[ i ].Active ) then
                result := math.Max( result, chtOutputs.Series[ i ].YValues.MaxValue )
              ;
            end
          ;
        end
      ;

      if( EqualsValue = CompareValue( 0.0, result ) ) then
        result := 1.0
      ;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.drawFirstDetection( const day: integer );
    begin
      seriesPDetection.Clear();
      seriesPDetection.AddXY( day, 0 );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.drawFirstDestruction( const day: integer );
    begin
      seriesPDestruction.Clear();
      seriesPDestruction.AddXY( day, ( seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.drawFirstVaccination( const day: integer );
    begin
      seriesPVaccination.Clear();
      seriesPVaccination.AddXY( day, ( 2 * seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.drawOutbreakEnd( const day: integer );
    begin
      seriesPOver.Clear();
      seriesPOver.AddXY( day, ( 3 * seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.redrawEvents();
    begin
      if( 0 < seriesPDetection.XValues.Count ) then
        drawFirstDetection( RoundDbl( seriesPDetection.XValues[ 0 ] ) )
      ;

      if( 0 < seriesPDestruction.XValues.Count ) then
        drawFirstDestruction( RoundDbl( seriesPDestruction.XValues[ 0 ] ) )
      ;

      if( 0 < seriesPVaccination.XValues.Count ) then
        drawFirstVaccination( RoundDbl( seriesPVaccination.XValues[ 0 ] ) )
      ;
      
      if( 0 < seriesPOver.XValues.Count ) then
        drawOutbreakEnd( RoundDbl( seriesPOver.XValues[ 0 ] ) )
      ;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.cbxClick(Sender: TObject);
    begin
      if( _meCreated ) then
        begin
          if( nil <> _myMainForm ) then
            begin
              SeriesDestroyed.Active := cbxDestroyed.Checked;
              SeriesExamined.Active := cbxExamined.Checked;
              SeriesDetected.Active := cbxDetected.Checked;
              SeriesTestTruePos.Active := cbxTestTruePos.Checked;
              SeriesTestTrueNeg.Active := cbxTestTrueNeg.Checked;
              SeriesTestFalseNeg.Active := cbxTestFalseNeg.Checked;
              SeriesTestFalsePos.Active := cbxTestFalsePos.Checked;

              SeriesPDetection.Active := cbxMilestones.Checked;
              SeriesPDestruction.Active := cbxMilestones.Checked;
              SeriesPVaccination.Active := cbxMilestones.Checked;
              SeriesPOver.Active := cbxMilestones.Checked;

              if( cbxMilestones.Checked ) then
                redrawEvents()
              ;
            end
          ;
        end
      ;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.cbxThreeDClick(Sender: TObject);
    begin
      chtOutputs.View3D := not chtOutputs.View3D;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDetectionStats.cbxCumulativeClick(Sender: TObject);
    begin
      inherited;

      if( cbxCumulative.Checked ) then
        chtOutputs.LeftAxis.Title.Caption := tr( 'Cumulative number of units' )
      else
        chtOutputs.LeftAxis.Title.Caption := tr( 'Daily number of units' )
      ;

      // Retally the daily data and chart daily cumulative (this forces the chart rebuild)
      frmDailyStatusByProdType.cbxChanged(Sender);
    end
  ;


end.
