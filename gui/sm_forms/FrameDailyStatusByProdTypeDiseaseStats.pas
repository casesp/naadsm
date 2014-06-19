{*
FrameDailyStatusByProdTypeDiseaseStats.pas/dfm
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

  This frame displays a graph showing the number of herds in each Disease State,
  on each day of a single iteration. The frame is dependant on being housed on
  FormDailyStatusByProdType. The frame has dependencies on this container unit and FormMain.
}


unit FrameDailyStatusByProdTypeDiseaseStats;

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
    NAADSMLibraryTypes,

    // Application-specific data structures
    SMSimOutByProdType

  ;

  
  type
    TFrameDailyStatusByProdTypeDiseaseStats = class(TFrameChartBase)
      
      SeriesPDetection: TPointSeries;
      SeriesPDestruction: TPointSeries;
      SeriesPVaccination: TPointSeries;
      SeriesPOver: TPointSeries;
      SeriesSusc: TLineSeries;
      SeriesLatent: TLineSeries;
      SeriesSubClinical: TLineSeries;
      SeriesClinical: TLineSeries;
      SeriesNatImmune: TLineSeries;
      SeriesVacImmune: TLineSeries;
      SeriesDestroyed: TLineSeries;

      pnlChartOptions: TPanel;
      cbxThreeD: TCheckBox;
      chtOutputs: TChart;
      pnlCheckBoxes: TPanel;
      cbxSusceptible: TCheckBox;
      lineSusceptible: TPanel;
      cbxNatImmune: TCheckBox;
      cbxLatent: TCheckBox;
      cbxVacImmune: TCheckBox;
      cbxSubClinical: TCheckBox;
      cbxDestroyed: TCheckBox;
      cbxClinical: TCheckBox;
      lineNatImmune: TPanel;
      lineLatent: TPanel;
      lineVacImmune: TPanel;
      lineSubclinical: TPanel;
      lineDestroyed: TPanel;
      lineClinical: TPanel;
      cbxMilestones: TCheckBox;

      procedure cbxClick(Sender: TObject);

      procedure cbxThreeDClick(Sender: TObject);

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
  

  constructor TFrameDailyStatusByProdTypeDiseaseStats.create( AOwner: TComponent );
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

      // These are not just properties, their click events fire! (hence need for _meCreated)
      cbxSusceptible.Checked := true;
      cbxLatent.Checked := true;
      cbxSubClinical.Checked := true;
      cbxClinical.Checked := true;
      cbxNatImmune.Checked := true;
      cbxVacImmune.Checked := true;
      cbxDestroyed.Checked := true;
      cbxMilestones.Checked := true;

      initializeChartColors;

      _meCreated:= true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.4.
      // Generation date: Fri Nov 6 09:26:07 2009
      // File name: C:/Documents and Settings/areeves/My Documents/NAADSM/Interface-naadsm_3_line_interface/sm_forms/FrameDailyStatusByProdTypeDiseaseStats.dfm
      // File date: Fri Nov 6 09:19:13 2009

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxThreeD.Caption := tr( '3-D View' );
          cbxMilestones.Caption := tr( 'Events' );
          cbxSusceptible.Caption := tr( 'Susceptible' );
          cbxNatImmune.Caption := tr( 'Nat immune' );
          cbxLatent.Caption := tr( 'Latent' );
          cbxVacImmune.Caption := tr( 'Vac immune' );
          cbxSubClinical.Caption := tr( 'Subclinical' );
          cbxDestroyed.Caption := tr( 'Destroyed' );
          cbxClinical.Caption := tr( 'Clinical' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtOutputs.BottomAxis.Title.Caption := tr( 'Day of simulation' );
          chtOutputs.LeftAxis.Title.Caption := tr( 'Daily number of units' );
          chtOutputs.Series[0].Title := tr( 'Susceptible' );
          chtOutputs.Series[1].Title := tr( 'Latent' );
          chtOutputs.Series[2].Title := tr( 'Subclinical' );
          chtOutputs.Series[3].Title := tr( 'Clinical' );
          chtOutputs.Series[4].Title := tr( 'NatImmune' );
          chtOutputs.Series[5].Title := tr( 'VacImmune' );
          chtOutputs.Series[6].Title := tr( 'Destroyed' );
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


  procedure TFrameDailyStatusByProdTypeDiseaseStats.translateUIManual();
    begin
      with self do
        begin
          cbxMilestones.Caption := capitalize( tr( 'events' ) );
        end
      ;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.initializeChartColors;
    begin
      // These colors and enums are defined in Unit StatusEnums

      chtOutputs.Series[0].SeriesColor := naadsmDiseaseStateColor(NAADSMStateSusceptible);
      chtOutputs.Series[1].SeriesColor := naadsmDiseaseStateColor(NAADSMStateLatent);
      chtOutputs.Series[2].SeriesColor := naadsmDiseaseStateColor(NAADSMStateSubclinical);
      chtOutputs.Series[3].SeriesColor := naadsmDiseaseStateColor(NAADSMStateClinical);
      chtOutputs.Series[4].SeriesColor := naadsmDiseaseStateColor(NAADSMStateNaturallyImmune);
      chtOutputs.Series[5].SeriesColor := naadsmDiseaseStateColor(NAADSMStateVaccineImmune);
      chtOutputs.Series[6].SeriesColor := naadsmDiseaseStateColor(NAADSMStateDestroyed);

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

      lineSusceptible.Color := naadsmDiseaseStateColor(NAADSMStateSusceptible);
      lineLatent.Color := naadsmDiseaseStateColor(NAADSMStateLatent);
      lineSubClinical.Color := naadsmDiseaseStateColor(NAADSMStateSubclinical);
      lineClinical.Color := naadsmDiseaseStateColor(NAADSMStateClinical);
      lineNatImmune.Color := naadsmDiseaseStateColor(NAADSMStateNaturallyImmune);
      lineVacImmune.Color := naadsmDiseaseStateColor(NAADSMStateVaccineImmune);
      lineDestroyed.Color := naadsmDiseaseStateColor(NAADSMStateDestroyed);
    end
  ;

  
  {*
    Adds a day of values to it's chart data series
    @param day The day  (0 to n) in the iteration to update
    @param DailyData Data structure holding the data
    @param Total number of units for all production types, unless one has been selected
  }
  procedure TFrameDailyStatusByProdTypeDiseaseStats.drawAllSeries( day: integer; DailyData: TSMDailyOutput );
    begin
      SeriesSusc.AddXY(          day, Dailydata.tsdUSusc );
      SeriesLatent.AddXY(        day, Dailydata.tsdULat );
      SeriesSubClinical.AddXY(   day, Dailydata.tsdUSubc );
      SeriesClinical.AddXY(      day, Dailydata.tsdUClin );
      SeriesNatImmune.AddXY(     day, Dailydata.tsdUNImm );
      SeriesVacImmune.AddXY(     day, Dailydata.tsdUVImm );
      SeriesDestroyed.AddXY(     day, Dailydata.tsdUDest );

      fixTChartBug();
    end
  ;


  {*
    Scales the charts for the proper min and max vlues for all the data series
    @return True if the current X,Y min and max values will sufffice for the latest data point
    theresult is used to indicate whether or not to continue checking.
  }
  procedure TFrameDailyStatusByProdTypeDiseaseStats.fixTChartBug();
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
          setVertAxisMinMax( 0.0, maxY + 1.0 );
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
          setHorizAxisMinMax( 0.0, maxX + 1.0 );
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
  procedure TFrameDailyStatusByProdTypeDiseaseStats.reset();
    begin
      SeriesSusc.Clear();
      SeriesLatent.Clear;
      SeriesSubClinical.Clear();
      SeriesClinical.Clear();
      SeriesNatImmune.Clear();
      SeriesVacImmune.Clear();
      SeriesDestroyed.Clear();
      SeriesPDetection.Clear();
      SeriesPDestruction.Clear();
      SeriesPVaccination.Clear();
      SeriesPOver.Clear();

      setHorizAxisMinMax( 0.0, 1.0 );
      setVertAxisMinMax( 0.0, 1.0 );

      _chartFixNeeded := true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.setHorizAxisAutomatic();
    begin
      SeriesSusc.GetHorizAxis.Automatic := true;
      SeriesLatent.GetHorizAxis.Automatic := true;
      SeriesSubClinical.GetHorizAxis.Automatic := true;
      SeriesClinical.GetHorizAxis.Automatic := true;
      SeriesNatImmune.GetHorizAxis.Automatic := true;
      SeriesVacImmune.GetHorizAxis.Automatic := true;
      SeriesDestroyed.GetHorizAxis.Automatic := true;

      SeriesPDetection.GetHorizAxis.Automatic := true;
      SeriesPDestruction.GetHorizAxis.Automatic := true;
      SeriesPVaccination.GetHorizAxis.Automatic := true;
      SeriesPOver.GetHorizAxis.Automatic := true;
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.setHorizAxisMinMax(min, max: double);
    begin
      SeriesSusc.GetHorizAxis.SetMinMax( min, max );
      SeriesLatent.GetHorizAxis.SetMinMax( min, max );
      SeriesSubClinical.GetHorizAxis.SetMinMax( min, max );
      SeriesClinical.GetHorizAxis.SetMinMax( min, max );
      SeriesNatImmune.GetHorizAxis.SetMinMax( min, max );
      SeriesVacImmune.GetHorizAxis.SetMinMax( min, max );
      SeriesDestroyed.GetHorizAxis.SetMinMax( min, max );

      SeriesPDetection.GetHorizAxis.SetMinMax( min, max );
      SeriesPDestruction.GetHorizAxis.SetMinMax( min, max );
      SeriesPVaccination.GetHorizAxis.SetMinMax( min, max );
      SeriesPOver.GetHorizAxis.SetMinMax( min, max );
    end
  ;

  procedure TFrameDailyStatusByProdTypeDiseaseStats.setVertAxisAutomatic();
    begin
      SeriesSusc.GetVertAxis.Automatic := true;
      SeriesLatent.GetVertAxis.Automatic := true;
      SeriesSubClinical.GetVertAxis.Automatic := true;
      SeriesClinical.GetVertAxis.Automatic := true;
      SeriesNatImmune.GetVertAxis.Automatic := true;
      SeriesVacImmune.GetVertAxis.Automatic := true;
      SeriesDestroyed.GetVertAxis.Automatic := true;

      SeriesPDetection.GetVertAxis.Automatic := true;
      SeriesPDestruction.GetVertAxis.Automatic := true;
      SeriesPVaccination.GetVertAxis.Automatic := true;
      SeriesPOver.GetVertAxis.Automatic := true;
    end
  ;

  procedure TFrameDailyStatusByProdTypeDiseaseStats.setVertAxisMinMax(min, max: double);
    begin
      SeriesSusc.GetVertAxis.SetMinMax( min, max );
      SeriesLatent.GetVertAxis.SetMinMax( min, max );
      SeriesSubClinical.GetVertAxis.SetMinMax( min, max );
      SeriesClinical.GetVertAxis.SetMinMax( min, max );
      SeriesNatImmune.GetVertAxis.SetMinMax( min, max );
      SeriesVacImmune.GetVertAxis.SetMinMax( min, max );
      SeriesDestroyed.GetVertAxis.SetMinMax( min, max );

      SeriesPDetection.GetVertAxis.SetMinMax( min, max );
      SeriesPDestruction.GetVertAxis.SetMinMax( min, max );
      SeriesPVaccination.GetVertAxis.SetMinMax( min, max );
      SeriesPOver.GetVertAxis.SetMinMax( min, max );
    end
  ;


  function TFrameDailyStatusByProdTypeDiseaseStats.seriesMaxY(): double;
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


  procedure TFrameDailyStatusByProdTypeDiseaseStats.drawFirstDetection( const day: integer );
    begin
      seriesPDetection.Clear();
      seriesPDetection.AddXY( day, 0 );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.drawFirstDestruction( const day: integer );
    begin
      seriesPDestruction.Clear();
      seriesPDestruction.AddXY( day, ( seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.drawFirstVaccination( const day: integer );
    begin
      seriesPVaccination.Clear();
      seriesPVaccination.AddXY( day, ( 2 * seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.drawOutbreakEnd( const day: integer );
    begin
      seriesPOver.Clear();
      seriesPOver.AddXY( day, ( 3 * seriesMaxY() / 4.0 ) );
      fixTChartBug();
    end
  ;


  procedure TFrameDailyStatusByProdTypeDiseaseStats.redrawEvents();
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


  procedure TFrameDailyStatusByProdTypeDiseaseStats.cbxClick(Sender: TObject);
    begin
      if( _meCreated ) then
        begin
          if( nil <> _myMainForm ) then
            begin
              SeriesSusc.Active := cbxSusceptible.Checked;
              SeriesLatent.Active := cbxLatent.Checked;
              SeriesSubClinical.Active := cbxSubClinical.Checked;
              SeriesClinical.Active := cbxClinical.Checked;
              SeriesNatImmune.Active := cbxNatImmune.Checked;
              SeriesVacImmune.Active := cbxVacImmune.Checked;
              SeriesDestroyed.Active := cbxDestroyed.Checked;

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

  
  procedure TFrameDailyStatusByProdTypeDiseaseStats.cbxThreeDClick(Sender: TObject);
    begin
      chtOutputs.View3D := not chtOutputs.View3D;
    end
  ;



end.
