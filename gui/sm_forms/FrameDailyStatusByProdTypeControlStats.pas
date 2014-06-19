{*
FrameDailyStatusByProdTypeControlStats.pas/dfm
----------------------------------------------
Begin: 2005/12/13
Last revision: $Date: 2013-06-27 19:11:31 $ $Author: areeves $
Version number: $Revision: 1.1.2.6 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
Author: Ric Hupalo <Ric.Hupalo@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

  This frame displays a graph showing the number of herds in each Tracing type,
  and the Control Status on each day of a single iteration.
  The frame is dependant on being housed on FormDailyStatusByProdType.
  The frame has dependencies on this container unit and FormMain.
}


unit FrameDailyStatusByProdTypeControlStats;

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
    TFrameDailyStatusByProdTypeControlStats = class(TFrameChartBase)

      SeriesPDetection: TPointSeries;
      SeriesPDestruction: TPointSeries;
      SeriesPVaccination: TPointSeries;
      SeriesPOver: TPointSeries;  // infected but nothing else has happened
      SeriesDetected: TLineSeries;
      SeriesDestroyed: TLineSeries;
      SeriesVaccinated: TLineSeries;
      SeriesTraceDirFwd: TLineSeries;
      SeriesTraceIndFwd: TLineSeries;
      SeriesTraceDirBack: TLineSeries;
      SeriesTraceIndBack: TLineSeries;

      pnlChartOptions: TPanel;

      cbxDetected: TCheckBox;
      cbxDestroyed: TCheckBox;
      cbxVaccinated: TCheckBox;
      cbxTraceDirFwd: TCheckBox;
      cbxTraceIndFwd: TCheckBox;
      cbxTraceDirBack: TCheckBox;
      cbxTraceIndBack: TCheckBox;
      cbxMilestones: TCheckBox;

      lineDetected: TPanel;
      lineDestroyed: TPanel;
      lineVaccinated: TPanel;
      lineTraceDirFwd: TPanel;
      lineTraceIndFwd: TPanel;
      lineTraceDirBack: TPanel;
      lineTraceIndBack: TPanel;

      cbxThreeD: TCheckBox;
      chtOutputs: TChart;
      cbxCumulative: TCheckBox;

      procedure cbxThreeDClick(Sender: TObject);
      procedure cbxClick(Sender: TObject);
      procedure cbxCumulativeClick(Sender: TObject);

    protected
      _myMainForm: TForm;       // pointer to the main form to access properties
      _meCreated: boolean;      // prevent access violations on create
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


  constructor TFrameDailyStatusByProdTypeControlStats.create( AOwner: TComponent );
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
      cbxDetected.Checked := true;
      cbxDestroyed.Checked := true;
      cbxVaccinated.Checked := true;
      cbxTraceDirFwd.Checked := true;
      cbxTraceIndFwd.Checked := true;
      cbxTraceDirBack.Checked := true;
      cbxTraceIndBack.Checked := true;

      initializeChartColors;

      _meCreated:= true;
    end
  ;

  
  procedure TFrameDailyStatusByProdTypeControlStats.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.4.
      // Generation date: Fri Nov 6 09:26:07 2009
      // File name: C:/Documents and Settings/areeves/My Documents/NAADSM/Interface-naadsm_3_line_interface/sm_forms/FrameDailyStatusByProdTypeControlStats.dfm
      // File date: Fri Nov 6 09:14:38 2009

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxThreeD.Caption := tr( '3-D View' );
          cbxMilestones.Caption := tr( 'Events' );
          cbxCumulative.Caption := tr( 'Cumulative' );
          cbxTraceDirBack.Caption := tr( 'Traced back-Direct' );
          cbxTraceIndBack.Caption := tr( 'Traced back-Indirect' );
          cbxDestroyed.Caption := tr( 'Destroyed' );
          cbxDetected.Caption := tr( 'Detected' );
          cbxTraceDirFwd.Caption := tr( 'Traced fwd-Direct' );
          cbxTraceIndFwd.Caption := tr( 'Traced fwd-Indirect' );
          cbxVaccinated.Caption := tr( 'Vaccinated' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtOutputs.BottomAxis.Title.Caption := tr( 'Day of simulation' );
          chtOutputs.LeftAxis.Title.Caption := tr( 'Daily number of units' );
          chtOutputs.Series[0].Title := tr( 'Detected' );
          chtOutputs.Series[1].Title := tr( 'Destroyed' );
          chtOutputs.Series[2].Title := tr( 'Vaccinated' );
          chtOutputs.Series[3].Title := tr( 'Traced fwd-Direct' );
          chtOutputs.Series[4].Title := tr( 'Traced fwd-Indirect' );
          chtOutputs.Series[5].Title := tr( 'Traced back-Direct' );
          chtOutputs.Series[6].Title := tr( 'Traced back-Indirect' );
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


  procedure TFrameDailyStatusByProdTypeControlStats.translateUIManual();
    begin
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.initializeChartColors();
    begin
      // These colors and enums are defined in Unit StatusEnums

      chtOutputs.Series[0].SeriesColor := controlStatusColor(asDetected);
      chtOutputs.Series[1].SeriesColor := controlStatusColor(asDestroyed);
      chtOutputs.Series[2].SeriesColor := controlStatusColor(asVaccinated);
      chtOutputs.Series[3].SeriesColor := controlStatusColor(asTracedDirectFwd);
      chtOutputs.Series[4].SeriesColor := controlStatusColor(asTracedIndirectFwd);
      chtOutputs.Series[5].SeriesColor := controlStatusColor(asTracedDirectBack);
      chtOutputs.Series[6].SeriesColor := controlStatusColor(asTracedIndirectBack);

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

      lineDetected.Color := controlStatusColor(asDetected);
      lineDestroyed.Color := controlStatusColor(asDestroyed);
      lineVaccinated.Color := controlStatusColor(asVaccinated);
      lineTraceDirFwd.Color := controlStatusColor(asTracedDirectFwd);
      lineTraceIndFwd.Color := controlStatusColor(asTracedIndirectFwd);
      lineTraceDirBack.Color := controlStatusColor(asTracedDirectBack);
      lineTraceIndBack.Color := controlStatusColor(asTracedIndirectBack);
    end
  ;


  {*
    Adds a day of values to it's chart data series
    @param day The day  (0 to n) in the iteration to update
    @param DailyData Data structure holding the data
  }
  procedure TFrameDailyStatusByProdTypeControlStats.drawAllSeries(day: integer; DailyData: TSMDailyOutput );
    begin
      if( ( not cbxCumulative.Checked ) or ( 0 = SeriesDetected.YValues.Count ) ) then
        begin
          SeriesDetected.AddXY(     day, ( Dailydata.detnUClin + Dailydata.detnUTest ) );
          SeriesDestroyed.AddXY(    day, Dailydata.desnUAll );
          SeriesVaccinated.AddXY(   day, Dailydata.vacnUAll );
          SeriesTraceDirFwd.AddXY(  day, Dailydata.trnUDirFwd );
          SeriesTraceIndFwd.AddXY(  day, Dailydata.trnUIndFwd );
          SeriesTraceDirBack.AddXY( day, Dailydata.trnUDirBack );
          SeriesTraceIndBack.AddXY( day, Dailydata.trnUIndBack );
        end
      else
        begin
          SeriesDetected.AddXY(     day, ( Dailydata.detnUClin + Dailydata.detnUTest ) + SeriesDetected.YValues.Last );
          SeriesDestroyed.AddXY(    day, Dailydata.desnUAll + SeriesDestroyed.YValues.Last );
          SeriesVaccinated.AddXY(   day, Dailydata.vacnUAll + SeriesVaccinated.YValues.Last );
          SeriesTraceDirFwd.AddXY(  day, Dailydata.trnUDirFwd + SeriesTraceDirFwd.YValues.Last );
          SeriesTraceIndFwd.AddXY(  day, Dailydata.trnUIndFwd + SeriesTraceIndFwd.YValues.Last );
          SeriesTraceDirBack.AddXY( day, Dailydata.trnUDirBack + SeriesTraceDirBack.YValues.Last );
          SeriesTraceIndBack.AddXY( day, Dailydata.trnUIndBack + SeriesTraceIndBack.YValues.Last );
        end
      ;

      fixTChartBug();
    end
  ;

  
  {*
    Scales the charts for the proper min and max vlues for all the data series
    @return True if the current X,Y min and max values will sufffice for the latest data point
    theresult is used to indicate whether or not to continue checking.
  }
  procedure TFrameDailyStatusByProdTypeControlStats.fixTChartBug();
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
  procedure TFrameDailyStatusByProdTypeControlStats.reset();
    begin
      SeriesDetected.Clear();
      SeriesDestroyed.Clear();
      SeriesVaccinated.Clear();
      SeriesTraceDirFwd.Clear();
      SeriesTraceIndFwd.Clear();
      SeriesTraceDirBack.Clear();
      SeriesTraceIndBack.Clear();

      SeriesPDetection.Clear();
      SeriesPDestruction.Clear();
      SeriesPVaccination.Clear();
      SeriesPOver.Clear();

      setHorizAxisMinMax( 0.0, 1.0 );
      setVertAxisMinMax( 0.0, 1.0 );

      _chartFixNeeded := true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.setHorizAxisAutomatic;
    begin
      SeriesDetected.GetHorizAxis.Automatic := true;
      SeriesDestroyed.GetHorizAxis.Automatic := true;
      SeriesVaccinated.GetHorizAxis.Automatic := true;
      SeriesTraceDirFwd.GetHorizAxis.Automatic := true;
      SeriesTraceIndFwd.GetHorizAxis.Automatic := true;
      SeriesTraceDirBack.GetHorizAxis.Automatic := true;
      SeriesTraceIndBack.GetHorizAxis.Automatic := true;

      SeriesPDetection.GetHorizAxis.Automatic := true;
      SeriesPDestruction.GetHorizAxis.Automatic := true;
      SeriesPVaccination.GetHorizAxis.Automatic := true;
      SeriesPOver.GetHorizAxis.Automatic := true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.setHorizAxisMinMax(min, max: double);
    begin
      SeriesDetected.GetHorizAxis.SetMinMax( min, max );
      SeriesDestroyed.GetHorizAxis.SetMinMax( min, max );
      SeriesVaccinated.GetHorizAxis.SetMinMax( min, max );
      SeriesTraceDirFwd.GetHorizAxis.SetMinMax( min, max );
      SeriesTraceIndFwd.GetHorizAxis.SetMinMax( min, max );
      SeriesTraceDirBack.GetHorizAxis.SetMinMax( min, max );
      SeriesTraceIndBack.GetHorizAxis.SetMinMax( min, max );

      SeriesPDetection.GetHorizAxis.SetMinMax( min, max );
      SeriesPDestruction.GetHorizAxis.SetMinMax( min, max );
      SeriesPVaccination.GetHorizAxis.SetMinMax( min, max );
      SeriesPOver.GetHorizAxis.SetMinMax( min, max );
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.setVertAxisAutomatic;
    begin
      SeriesDetected.GetVertAxis.Automatic := true;
      SeriesDestroyed.GetVertAxis.Automatic := true;
      SeriesVaccinated.GetVertAxis.Automatic := true;
      SeriesTraceDirFwd.GetVertAxis.Automatic := true;
      SeriesTraceIndFwd.GetVertAxis.Automatic := true;
      SeriesTraceDirBack.GetVertAxis.Automatic := true;
      SeriesTraceIndBack.GetVertAxis.Automatic := true;

      SeriesPDetection.GetVertAxis.Automatic := true;
      SeriesPDestruction.GetVertAxis.Automatic := true;
      SeriesPVaccination.GetVertAxis.Automatic := true;
      SeriesPOver.GetVertAxis.Automatic := true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.setVertAxisMinMax(min, max: double);
    begin
      SeriesDetected.GetVertAxis.SetMinMax( min, max );
      SeriesDestroyed.GetVertAxis.SetMinMax( min, max );
      SeriesVaccinated.GetVertAxis.SetMinMax( min, max );
      SeriesTraceDirFwd.GetVertAxis.SetMinMax( min, max );
      SeriesTraceIndFwd.GetVertAxis.SetMinMax( min, max );
      SeriesTraceDirBack.GetVertAxis.SetMinMax( min, max );
      SeriesTraceIndBack.GetVertAxis.SetMinMax( min, max );

      SeriesPDetection.GetVertAxis.SetMinMax( min, max );
      SeriesPDestruction.GetVertAxis.SetMinMax( min, max );
      SeriesPVaccination.GetVertAxis.SetMinMax( min, max );
      SeriesPOver.GetVertAxis.SetMinMax( min, max );
    end
  ;


  function TFrameDailyStatusByProdTypeControlStats.seriesMaxY(): double;
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


  procedure TFrameDailyStatusByProdTypeControlStats.drawFirstDetection( const day: integer );
    begin
      seriesPDetection.Clear();
      seriesPDetection.AddXY( day, 0 );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.drawFirstDestruction( const day: integer );
    begin
      seriesPDestruction.Clear();
      seriesPDestruction.AddXY( day, ( seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.drawFirstVaccination( const day: integer );
    begin
      seriesPVaccination.Clear();
      seriesPVaccination.AddXY( day, ( 2 * seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.drawOutbreakEnd( const day: integer );
    begin
      seriesPOver.Clear();
      seriesPOver.AddXY( day, ( 3 * seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.redrawEvents();
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


  procedure TFrameDailyStatusByProdTypeControlStats.cbxClick(Sender: TObject);
    begin
      if( _meCreated ) then
        begin
          if( nil <> _myMainForm ) then
            begin
              SeriesDetected.Active := cbxDetected.Checked;
              SeriesDestroyed.Active := cbxDestroyed.Checked;
              SeriesVaccinated.Active := cbxVaccinated.Checked;
              SeriesTraceIndFwd.Active := cbxTraceIndFwd.Checked;
              SeriesTraceDirBack.Active := cbxTraceDirBack.Checked;
              SeriesTraceIndBack.Active := cbxTraceIndBack.Checked;
              SeriesTraceDirFwd.Active := cbxTraceDirFwd.Checked;

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


  procedure TFrameDailyStatusByProdTypeControlStats.cbxThreeDClick(Sender: TObject);
    begin
      chtOutputs.View3D := not chtOutputs.View3D;
    end
  ;


  procedure TFrameDailyStatusByProdTypeControlStats.cbxCumulativeClick(Sender: TObject);
    begin
      inherited;

      if( cbxCumulative.Checked ) then
        chtOutputs.LeftAxis.Title.Caption := tr( 'Cumulative number of units' )
      else
        chtOutputs.LeftAxis.Title.Caption := tr( 'Daily number of units' )
      ;

      // Retally the daily data and chart daily cumulative (this forces the chart rebuild)
      frmDailyStatusByProdType.cbxChanged(Sender);

      dbcout2( 'Done with TFrameDailyStatusByProdTypeControlStats.cbxCumulativeClick()' );
    end
  ;



end.
