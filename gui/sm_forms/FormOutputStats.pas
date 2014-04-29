unit FormOutputStats;

(*
FormOutputStats.pas/dfm
-----------------------
Begin: 2005/08/01
Last revision: $Date: 2011-02-24 15:01:55 $ $Author: rhupalo $
Version: $Revision: 1.38.6.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

	uses
    // Standard Delphi units
		Windows, 
		Messages,
		SysUtils, 
		Variants, 
		Classes, 
		Graphics, 
		Controls, 
		Forms,
		Dialogs,
    StdCtrls,
    Buttons,
    TeEngine,
    Series,
    TeeProcs,
    Chart,
    ExtCtrls,
    ToolWin,
    ActnMan,
    ActnCtrls,
    XPStyleActnCtrls,
    ActnList,
    StdActns,
    ActnMenus,
    ImgList,
    ClipBrd,
    ComCtrls,

    // Custom standard widgets
		PBPageControl,

    // Application-specific data structures
    SMDatabase,
    SMSimulationInput,
    ProductionType,
    Herd,

    IterationOutputs,
    OutputDescriptions,

    // Application-specific widgets
    FormSMOutputBase, // the base class!
    FrameOutputStatsTable,
    FrameOutputStats,
    FrameChartBase,
		FrameStringGridBase
	;


  {*
    This form displays summary statistics for multiple iterations.
    Separate tabs are displayed for epidemiological, cost, and zone outputs.
  }
	type TFormOutputStats = class( TFormSMOutputBase )
      pgcOutputs: TPBPageControl;

      tabEpiOutputs: TTabSheet;
      fraStatsEpi: TFrameOutputStats;

      tabCostOutputs: TTabSheet;
      fraStatsCost: TFrameOutputStats;
      
      tabPTZoneOutputs: TTabSheet;
      fraStatsPTZones: TFrameOutputStats;

      tabZoneOutputs: TTabSheet;
      fraStatsZones: TFrameOutputStats;

      procedure pgcOutputsChange(Sender: TObject);

		protected
      _scenarioStats: TScenarioOutputSet;

      _resizingForm: boolean;

      _showCharts: boolean;

      _histogramBins: integer;

      procedure translateUI();

      procedure setHistogramBinNumbers( sender: TComponent; const nBins: integer );

      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillStringGridDict(); override;

      { Used to create a list of all grids containing text that might be saved/copied/printed }
      procedure fillChartDict(); override;

      function textHeader(): string; override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the selected zone changes }
      procedure zoneChanged(); override;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); override;

      procedure initialize();

      procedure copyRawData( sender: TObject );
      procedure exportRawData( sender: TObject );

		public
			constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase ); reintroduce;
      destructor destroy(); override;
		end
	;

	var
		frmOutputStats: TFormOutputStats;


implementation

{$R *.dfm}

  uses
    // Standard Delphi units
    Printers,
    Grids,
    StrUtils,

    // General purpose units
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    MyGraphicsUtils,
    I88n,

    // APHI Modeling Library
    HistogramData,

    // Application-specific units
    FrameArrayHistogram,
    StringConsts
  ;



  const
    DBFORMOUTPUTSTATS: boolean = false; // Set to true to enable debugging messages for this unit.

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormOutputStats.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase  );
    var
      frm: TForm;
    begin
      inherited create( AOwner );
      translateUI();

      //pgcOutputs.Visible := false; // Set at run-time

      lblIteration.Visible := false;
      cboIteration.Visible := false;

      _smSim := sim;
      _smdb := db;
      _selectedPT := nil;

      _resizingForm := false;

      _histogramBins := 50;

      if( AOwner is TForm ) then
        begin
          frm := AOwner as TForm;
          self.Height := round( frm.ClientHeight * 0.9 );
        end
      else
        begin
          self.Height := 650;
        end
      ;

      _dm.acnCopyRawData.visible := true;
      _dm.acnExportRawData.visible := true;

      acnCopyRawData.OnExecute := copyRawData;
      acnExportRawData.OnExecute := exportRawData;

      setupProdTypeComboBox();

      // FIX ME: check for completed output before displaying anything.
      // (FormMain should also do a check.  This is just a backup.)
      pgcOutputs.ActivePage := tabEpiOutputs;

      self.fraStatsEpi.setDatabase( _smdb );
      self.fraStatsCost.setDatabase( _smdb );
      self.fraStatsPTZones.setDatabase( _smdb );
      self.fraStatsZones.setDatabase( _smdb );

      self.fraStatsEpi.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      self.fraStatsCost.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      self.fraStatsPTZones.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      self.fraStatsZones.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;

      _formUsesZones := true;

      initialize();

      pgcOutputs.Visible := true;
    end
  ;


  procedure TFormOutputStats.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormOutputStats.dfm
      // File date: Wed May 2 17:17:12 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Output statistics' );
          tabEpiOutputs.Caption := tr( 'Epidemiology' );
          tabCostOutputs.Caption := tr( 'Cost accounting' );
          tabPTZoneOutputs.Caption := tr( 'Zones/production types' );
          tabZoneOutputs.Caption := tr( 'Zones' );
        end
      ;
    end
  ;


  procedure TFormOutputStats.initialize();
    begin
      _scenarioStats := TScenarioOutputSet.create( _smdb, _smSim.includeCostsGlobal, _smSIm.includeZonesGlobal, _smSim.costTrackZoneSurveillance );

      fraStatsEpi.setOutputType( StatsEpi );
			fraStatsEpi.setEpiStats( _scenarioStats.epiStats );
			fraStatsEpi.setProdType( _selectedPT );
      fraStatsEpi.resizeContents();

      if( not( _smSim.includeCostsGlobal ) ) then
        begin
          pgcOutputs.ActivePage := tabEpiOutputs;
          tabCostOutputs.Enabled := false;
        end
      else
        begin
          tabCostOutputs.enabled := true;
          fraStatsCost.setOutputType( StatsCosts );
          fraStatsCost.setCostStats( _scenarioStats.costStats );
          fraStatsCost.setProdType( _selectedPT );
          fraStatsCost.resizeContents();
        end
      ;

      if( not( _smSim.includeZonesGlobal ) ) then
        begin
          pgcOutputs.ActivePage := tabEpiOutputs;
          tabPTZoneOutputs.Enabled := false;

          tabZoneOutputs.Enabled := false;
        end
      else
        begin
          tabPTZoneOutputs.enabled := true;
          fraStatsPTZones.setOutputType( StatsPTZones );
          fraStatsPTZones.setZonePTStats( _scenarioStats.zonePTStats );
          fraStatsPTZones.setProdType( _selectedPT );
          fraStatsPTZones.setZone( _selectedZone );
          fraStatsPTZones.resizeContents();

          tabZoneOutputs.enabled := true;
          fraStatsZones.setOutputType( StatsZones );
          fraStatsZones.setZoneStats( _scenarioStats.zoneStats );
          fraStatsZones.setProdType( _selectedPT );
          fraStatsZones.setZone( _selectedZone );
          fraStatsZones.resizeContents();
        end
      ;

      pgcOutputs.forceRepaint();
      pgcOutputsChange( nil );
    end
  ;


  destructor TFormOutputStats.destroy();
    begin
      freeAndNil( _scenarioStats );
      inherited destroy();
    end
  ;


  procedure TFormOutputStats.fillStringGridDict();
    begin
      _stringGridDict.Clear();

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        _stringGridDict['Epidemiological statistics'] := fraStatsEpi.fraTable
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        _stringGridDict['Cost statistics'] := fraStatsCost.fraTable
      else if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        _stringGridDict['Zone/production type statistics'] := fraStatsPTZones.fraTable
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        _stringGridDict['Zone statistics'] := fraStatsZones.fraTable
      ;
    end
  ;


  procedure TFormOutputStats.fillChartDict();
    begin
      _chartDict.Clear();

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        _chartDict['EpiHistogram'] := fraStatsEpi.fraHistogram
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        _chartDict['CostHistogram'] := fraStatsCost.fraHistogram
      else if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        _chartDict['ZonePTHistogram'] := fraStatsPTZones.fraHistogram
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        _chartDict['ZoneHistogram'] := fraStatsZones.fraHistogram
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  procedure TFormOutputStats.simChanged();
    begin
      freeAndNil( _scenarioStats );

      initialize();
    end
  ;


  procedure TFormOutputStats.productionTypeChanged();
    var
      activePage: integer;
    begin
      try
        screen.Cursor := crHourGlass;
        setControlsEnabled( false );

        activePage := pgcOutputs.ActivePageIndex;

        fraStatsEpi.setProdType( _selectedPT );

        if( _smSim.includeCostsGlobal ) then
          begin
            fraStatsCost.setProdType( _selectedPT );
            pgcOutputs.ActivePageIndex := activePage;
          end
        else
          pgcOutputs.ActivePage := tabEpiOutputs
        ;

        if( _smSim.includeZonesGlobal ) then
          begin
            fraStatsPTZones.setProdType( _selectedPT );
            fraStatsZones.setProdType( _selectedPT );
            pgcOutputs.ActivePageIndex := activePage;
          end
        else
          pgcOutputs.ActivePage := tabEpiOutputs
        ;
      finally
        fillChartDict();
        fillStringGridDict();
        
        setControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormOutputStats.zoneChanged();
    var
      activePage: integer;
    begin
      try
        screen.Cursor := crHourGlass;
        setControlsEnabled( false );

        activePage := pgcOutputs.ActivePageIndex;

        if( _smSim.includeZonesGlobal ) then
          begin
            fraStatsPTZones.setZone( _selectedZone );
            fraStatsZones.setZone( _selectedZone );
            pgcOutputs.ActivePageIndex := activePage;
          end
        else
          pgcOutputs.ActivePage := tabEpiOutputs
        ;
      finally
        fillChartDict();
        fillStringGridDict();

        setControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormOutputStats.setHistogramBinNumbers( sender: TComponent; const nBins: integer );
    var
      frame: TFrameArrayHistogram;
    begin
      if( sender is TFrameArrayHistogram ) then
        frame := sender as TFrameArrayHistogram
      else
        begin
          raise exception.create( 'Unexpected sender type in TFormOutputStats.setHistogramBinNumbers()' );
          exit;
        end
      ;

      if( frame <> self.fraStatsEpi.fraHistogram ) then
        self.fraStatsEpi.setHistogramBinNumber( frame.breakType, nBins )
      ;
      if( tabCostOutputs.Enabled and ( frame <> self.fraStatsCost.fraHistogram ) ) then
        self.fraStatsCost.setHistogramBinNumber( frame.breakType, nBins )
      ;
      if( tabPTZoneOutputs.Enabled and ( frame <> self.fraStatsPTZones.fraHistogram ) ) then
        self.fraStatsPTZones.setHistogramBinNumber( frame.breakType, nBins )
      ;
      if( tabZoneOutputs.Enabled and ( frame <> self.fraStatsZones.fraHistogram ) ) then
        self.fraStatsZones.setHistogramBinNumber( frame.breakType, nBins )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling
//-----------------------------------------------------------------------------
  procedure TFormOutputStats.pgcOutputsChange(Sender: TObject);
    begin
      inherited;

      dbcout( '*** Active page changed to ' + intToStr( pgcOutputs.ActivePageIndex ), DBFORMOUTPUTSTATS );

      if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        begin
          cboProdTypes.Visible := true;
          cboZones.Visible := true;
        end
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        begin
          cboProdTypes.Visible := false;
          cboZones.Visible := true;
        end
      else
        begin
          cboProdTypes.Visible := true;
          cboZones.Visible := false;
        end
      ;

      fillStringGridDict();
      fillChartDict();

      setControlsEnabled( true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Save/copy/print data
//-----------------------------------------------------------------------------
  function TFormOutputStats.textHeader(): string;
    var
      ptName, zoneName: string;
    begin
      if( nil = _selectedPT ) then
        ptName := tr( 'All production types' )
      else
        ptName := _selectedPT.productionTypeDescr
      ;

      if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        begin
          if( nil = _selectedZone ) then
            zoneName := tr( 'All zones' )
          else
            zoneName := _selectedZone.descr
          ;
        end
      ;
      
      if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        begin
          if( nil = _selectedZone ) then
            zoneName := tr( 'All zones' )
          else
            zoneName := _selectedZone.descr
          ;
        end
      ;

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        result := '## ' + tr( 'NAADSM scenario output summary (epidemiology)' ) + endl
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        result := '## ' + tr( 'NAADSM scenario output summary (direct costs)' ) + endl
      else if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        result := '## ' + tr( 'NAADSM scenario output summary (zones and production types)' ) + endl  
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        result := '## ' + tr( 'NAADSM scenario output summary (zones)' ) + endl
      else
        result := '## ' + tr( 'NAADSM scenario output summary' ) + endl
      ;

      result := result
        + '## ' + tr( 'Application version:' ) + ' ' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + '-' + BUILDNUMBER + endl
        + '## ' + tr( 'Date:' ) + ' ' + dateTimeToStr( now() ) + endl
        + '## ' + tr( 'Scenario file:' ) + ' ' + _smdb.permanentDBFileName + endl
        + '## ' + tr( 'Production type:' ) + ' ' + ptName + endl
      ;

      if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        result := result + '## ' + tr( 'Zone:' ) + ' ' + zoneName + endl
      ;
      
      if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        result := result + '## ' + tr( 'Zone:' ) + ' ' + zoneName + endl
      ;

      result := result
        + '## ' + ansiReplaceStr( tr( 'Summary of xyz iterations' ), 'xyz', intToStr( _smdb.completedIterations ) ) + endl
        + endl
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form-specific menu items
//-----------------------------------------------------------------------------
  procedure TFormOutputStats.copyRawData( sender: TObject );
    begin
      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        fraStatsEpi.fraTable.writeOutputToClipboard()
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        fraStatsCost.fraTable.writeOutputToClipboard()
      else if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        fraStatsPTZones.fraTable.writeOutputToClipboard()
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        fraStatsZones.fraTable.writeOutputToClipboard()
      else
        raise exception.Create( 'There is a problem in TFormOutputStats.copyRawData()' )
      ;
    end
  ;


  procedure TFormOutputStats.exportRawData( sender: TObject );
    begin
      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        fraStatsEpi.fraTable.writeOutputToFile()
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        fraStatsCost.fraTable.writeOutputToFile()
      else if( tabPTZoneOutputs = pgcOutputs.ActivePage ) then
        fraStatsPTZones.fraTable.writeOutputToFile()
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        fraStatsZones.fraTable.writeOutputToFile()
      else
        raise exception.Create( 'There is a problem in TFormOutputStats.exportRawData()' )
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
