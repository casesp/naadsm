unit FormOutputStats;

(*
FormOutputStats.pas/dfm
-----------------------
Begin: 2005/08/01
Last revision: $Date: 2008/03/12 22:10:47 $ $Author: areeves $
Version: $Revision: 1.33 $
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

    SMSimulationStats,
    SMZoneStats,
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

      tabZoneOutputs: TTabSheet;
      fraStatsZones: TFrameOutputStats;

      procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
      procedure pgcOutputsChange(Sender: TObject);

		protected
      _superList: TSMIterationOutputSuperList;
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
    GuiStrUtils,
    DebugWindow,
    MyDialogs,
    MyGraphicsUtils,
    I88n,


    // Application-specific units
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

      setupComboBox();

      // FIX ME: check for completed output before displaying anything.
      // (FormMain should also do a check.  This is just a backup.)
      pgcOutputs.ActivePage := tabEpiOutputs;

      self.fraStatsEpi.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      self.fraStatsCost.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
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
          tabZoneOutputs.Caption := tr( 'Zones' );
        end
      ;

    end
  ;


  procedure TFormOutputStats.initialize();
    begin
      _superList := TSMIterationOutputSuperList.create( _smdb );
      _scenarioStats := TScenarioOutputSet.create( _smdb, _smSim.includeCostsGlobal, _smSIm.includeZonesGlobal );

      fraStatsEpi.setOutputType( StatsEpi );
			fraStatsEpi.setSuperList( _superList );
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
          tabZoneOutputs.Enabled := false;
        end
      else
        begin
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
      freeAndNil( _superList );
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
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        _stringGridDict['Zone statistics'] := fraStatsZones.fraTable
      ;
    end
  ;


  procedure TFormOutputStats.fillChartDict();
    begin
      _chartDict.Clear();

      if( tabEpiOutputs = pgcOutputs.ActivePage ) then
        begin
          _chartDict['EpiHistogram'] := fraStatsEpi.fraHistogram;
          _chartDict['EpiConvergence'] := fraStatsEpi.fraConvergence;
        end
      else if( tabCostOutputs = pgcOutputs.ActivePage ) then
        begin
          _chartDict['CostHistogram'] := fraStatsCost.fraHistogram;
          _chartDict['CostConvergence'] := fraStatsCost.fraConvergence;
        end
      else if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        begin
          _chartDict['ZoneHistogram'] := fraStatsZones.fraHistogram;
          _chartDict['ZoneConvergence'] := fraStatsZones.fraConvergence;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  procedure TFormOutputStats.simChanged();
    begin
      freeAndNil( _superList );
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
            fraStatsZones.setProdType( _selectedPT );
            pgcOutputs.ActivePageIndex := activePage;
          end
        else
          pgcOutputs.ActivePage := tabEpiOutputs
        ;
      finally
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
            fraStatsZones.setZone( _selectedZone );
            pgcOutputs.ActivePageIndex := activePage;
          end
        else
          pgcOutputs.ActivePage := tabEpiOutputs
        ;
      finally
        setControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormOutputStats.setHistogramBinNumbers( sender: TComponent; const nBins: integer );
    begin
      if( sender <> self.fraStatsEpi.fraHistogram ) then
        self.fraStatsEpi.fraHistogram.nHistogramBins := nBins
      ;
      if( fraStatsCost.Enabled and ( sender <> self.fraStatsCost.fraHistogram ) ) then
        self.fraStatsCost.fraHistogram.nHistogramBins := nBins
      ;
      if( fraStatsZones.Enabled and ( sender <> self.fraStatsZones.fraHistogram ) ) then
        self.fraStatsZones.fraHistogram.nHistogramBins := nBins
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

      if( tabZoneOutputs = pgcOutputs.ActivePage ) then
        cboZones.Visible := true
      else
        cboZones.Visible := false
      ;

      fillStringGridDict();
      fillChartDict();

      setControlsEnabled( true );
    end
  ;



  // FIX ME: why is this commented out??  Can it be discarded?
  procedure TFormOutputStats.FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);
    begin
      (*
      inherited;

      dbcout( 'My width is ' + intToStr( self.Width ), DBFORMOUTPUTSTATS );

      if( not( _resizingForm ) ) then
        begin
          if( newHeight < self.Height ) then
            begin
              _resizingForm := true;
              dbcout( '@@@ Form is shrinking to ' + intToStr( newHeight ), DBFORMOUTPUTSTATS );

              // When shrinking the form, remember to break the alignment temporarily.
              fraStatsEpi.Align := alNone;
              fraStatsCost.Align := alNone;
              self.Height := newHeight;
							fraStatsEpi.resizeContents();
              fraStatsCost.resizeContents();
              fraStatsEpi.Align := alClient;
              fraStatsCost.Align := alClient;

              _resizingForm := false;
            end
          ;

          if( newHeight > self.Height ) then
            begin
              _resizingForm := true;
              dbcout( '*** Form is embiggened to ' + intToStr( newHeight ), DBFORMOUTPUTSTATS );
              self.Height := newHeight;
              fraStatsEpi.resizeContents();
              fraStatsCost.resizeContents();
              _resizingForm := false;
            end
          ;
        end
      ;
      *)
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


end.
