unit FormScenarioComparison;

(*
FormScenarioComparison.pas/dfm
------------------------------
Begin: 2006/10/31
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version: $Revision: 1.17.4.9 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
Author: Snehal Shetye <snehal@goku.engr.colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2011 Colorado State University

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
    ToolWin,
    ActnMan,
    ActnCtrls,
    ActnMenus,
    StdCtrls,
    Buttons,
    ExtCtrls,
    ComCtrls,

    TeEngine,
    Series,
    TeeProcs,
    Chart,

    PBPageControl,
    ARSyncGrid,

    FormSMOutputBase,
    FrameOutputStats,
    FrameArrayHistogram,
    SMSimulationInput,
    SMDatabase,
    ProductionType,

    IterationOutputs,
    OutputDescriptions
  ;

  type TFormScenarioComparison = class( TFormSMOutputBase )
      pnlFrames: TPanel;

      pnlA: TPanel;
      pnlFileNameA: TPanel;
      pgcOutputsA: TPBPageControl;
      tabEpiOutputsA: TTabSheet;
      fraStatsEpiA: TFrameOutputStats;
      tabCostOutputsA: TTabSheet;
      fraStatsCostA: TFrameOutputStats;
      tabPTZoneOutputsA: TTabSheet;
      fraStatsPTZonesA: TFrameOutputStats;
      tabZoneOutputsA: TTabSheet;
      fraStatsZonesA: TFrameOutputStats;

      pnlB: TPanel;
      pnlFileNameB: TPanel;
      pgcOutputsB: TPBPageControl;
      tabEpiOutputsB: TTabSheet;
      fraStatsEpiB: TFrameOutputStats;
      tabCostOutputsB: TTabSheet;
      fraStatsCostB: TFrameOutputStats;
      tabPTZoneOutputsB: TTabSheet;
      fraStatsPTZonesB: TFrameOutputStats;
      tabZoneOutputsB: TTabSheet;
      fraStatsZonesB: TFrameOutputStats;

      procedure pgcOutputsAMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure pgcOutputsBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure FormResize(Sender: TObject);

    protected
      _resizingForm: boolean;

      _scenarioStatsA: TScenarioOutputSet;
      _scenarioStatsB: TScenarioOutputSet;

      _smSimB: TSMSimulationInput;
      _smdbB: TSMDatabase;

      _showZoneTabs: boolean;

      procedure translateUI();

      procedure initialize();

      procedure productionTypeChanged(); override;
      procedure zoneChanged(); override;

      procedure setHistogramBinNumbers( sender: TComponent; const nBins: integer );
      procedure rescaleChartAxes(); overload;
      procedure rescaleChartAxes( a, b: TChartSeries; isHistogram: boolean ); overload;

      property _smSimA: TSMSimulationInput read _smSim write _smSim;
      property _smdbA: TSMDatabase read _smdb write _smdb;

		public
			constructor create(
        AOwner: TComponent;
        simA: TSMSimulationInput; dbA: TSMDatabase;
        dbB: TSMDatabase
      ); reintroduce;

      destructor destroy(); override;
    end
  ;

  var
    frmCompareStats: TFormScenarioComparison;

implementation

{$R *.dfm}

  uses
    Math,
    
    MyStrUtils,
    SqlClasses,
    DebugWindow,
    MyDialogs,
    I88n,

    HistogramData,

    FrameOutputStatsTable
  ;

  const
    debug: boolean = false; // Set to true to enable debugging messages for this unit.

  constructor TFormScenarioComparison.create(
        AOwner: TComponent;
        simA: TSMSimulationInput; dbA: TSMDatabase;
        dbB: TSMDatabase
      );
    var
      frm: TForm;
    begin
      inherited create( AOwner );
      translateUI();

      lblIteration.Visible := false;
      cboIteration.Visible := false;

      fraStatsEpiA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsEpiB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsCostA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsCostB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsPTZonesA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsPTZonesB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsZonesA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsZonesB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;

      fraStatsEpiA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsEpiB.setAxisValuesPtr := rescaleChartAxes;
      fraStatsCostA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsCostB.setAxisValuesPtr := rescaleChartAxes;
      fraStatsPTZonesA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsPTZonesB.setAxisValuesPtr := rescaleChartAxes;
      fraStatsZonesA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsZonesB.setAxisValuesPtr := rescaleChartAxes;

      pnlFileNameA.Caption := '';
      pnlFileNameB.Caption := '';

      // Intialize items needed for grid synchronization
      //------------------------------------------------
      makeSyncGridPair( self.fraStatsEpiA.fraTable.stgGrid, self.fraStatsEpiB.fraTable.stgGrid );
      makeSyncGridPair( self.fraStatsCostA.fraTable.stgGrid, self.fraStatsCostB.fraTable.stgGrid );
      makeSyncGridPair( self.fraStatsPTZonesA.fraTable.stgGrid, self.fraStatsPTZonesB.fraTable.stgGrid );
      makeSyncGridPair( self.fraStatsZonesA.fraTable.stgGrid, self.fraStatsZonesB.fraTable.stgGrid );

      // Set the first database
      //-----------------------
      dbcout( 'Setting database and sim A...', debug );
      _smdbA := dbA;
      _smSimA := simA;

      // Load the second database file
      //------------------------------
      _smdbB := dbB;
      _smSimB := TSMSimulationInput.create( _smdbB );

      // Compare production types in _smSimB and _smSimA.
      // If they aren't exactly the same, disable the production type combo box
      // and display a message that the only comparison allowed is for all production types.
      //------------------------------------------------------------------------------------
      if( not( _smSimA.ptList.equals( _smSimB.ptList ) ) ) then
        begin
          msgOK(
            tr( 'The two selected scenario files contain different production types.' ) + '  '
              + tr( 'It will not be possible to compare simulation outputs for individual production types.' ),
            tr( 'Production types do not match' ),
            IMGInformation,
            self
          );

          cboProdTypes.Enabled := false;
        end
      ;


      // Compare zones in _smSimB and _smSimA.
      // If one scenario uses zones and the other doesn't, do not display the zone tabs.
      // If both use zones, compare the lists.  If they aren't exactly the same, disable the
      // zone combo box and display a message like the one above.
      //--------------------------------------------------------------------------------
      if( _smSimA.includeZonesGlobal <> _smSimB.includeZonesGlobal ) then
        begin
          msgOK(
            tr( 'Only one of the two selected scenario files uses zones.' ) + '  '
              + tr( 'It will not be possible to compare zone outputs.' ),
            tr( 'Zones do not match' ),
            IMGInformation,
            self
          );

          _showZoneTabs := false;
          cboZones.Enabled := false;
          cboZones.Visible := false;
        end
      else if( _smSimA.includeZonesGlobal and  _smSimB.includeZonesGlobal ) then
        begin
          _showZoneTabs := true;

          if( not( _smSimA.zoneList.equals( _smSimB.zoneList ) ) ) then
            begin
              msgOK(
                tr( 'The two selected scenario files contain different zones.' ) + '  '
                  + tr( 'It will not be possible to compare simulation outputs for individual zones.' ),
                tr( 'Zones do not match' ),
                IMGInformation,
                self
              );

              cboZones.Enabled := false;
            end
          ;
        end
      else // Neither scenario includes zones
        begin
          _showZoneTabs := false;
          cboZones.Enabled := false;
          cboZones.Visible := false;
        end
      ;

      self.Perform( WM_SETREDRAW, 0, 0 );

      pnlFileNameA.Hint := _smdbA.permanentDBFileName;
      pnlFileNameA.Caption := abbrevPath( _smdbA.permanentDBFileName, 40 );

      pnlFileNameB.Hint := _smdbB.permanentDBFileName;
      pnlFileNameB.Caption := abbrevPath( _smdbB.permanentDBFileName, 40 );

      _selectedPT := nil;

      _resizingForm := false;

      dbcout( 'Setting up combo box...', debug );
      setupProdTypeComboBox();

      pgcOutputsA.ActivePage := tabEpiOutputsA;
      pgcOutputsB.ActivePage := tabEpiOutputsB;

      dbcout( 'Initializing...', debug );
      initialize();
      dbcout( 'Done with initializer!', debug );

      // Function Pointers for Epidemiology tab
      //---------------------------------------
      fraStatsEpiA.fraHistogram.rdoHistoTypeClickPtr := fraStatsEpiB.fraHistogram.rdoBinNumberClick;
      fraStatsEpiB.fraHistogram.rdoHistoTypeClickPtr := fraStatsEpiA.fraHistogram.rdoBinNumberClick;

      fraStatsEpiA.fraHistogram.cbx3DViewClickPtr := fraStatsEpiB.fraHistogram.cbx3DViewClick;
      fraStatsEpiB.fraHistogram.cbx3DViewClickPtr := fraStatsEpiA.fraHistogram.cbx3DViewClick;

      fraStatsEpiA.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsEpiB.fraHistogram.rleHistoBinsEnter;
      fraStatsEpiB.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsEpiA.fraHistogram.rleHistoBinsEnter;

      fraStatsEpiA.fraHistogram.btnCancelClickPtr := fraStatsEpiB.fraHistogram.btnCancelClick;
      fraStatsEpiB.fraHistogram.btnCancelClickPtr := fraStatsEpiA.fraHistogram.btnCancelClick;

      fraStatsEpiA.fraHistogram.btnAcceptClickPtr := fraStatsEpiB.fraHistogram.btnAcceptClick;
      fraStatsEpiB.fraHistogram.btnAcceptClickPtr := fraStatsEpiA.fraHistogram.btnAcceptClick;

      fraStatsEpiA.cbxClickPtr := fraStatsEpiB.changeCheck;
      fraStatsEpiB.cbxClickPtr := fraStatsEpiA.changeCheck;

      fraStatsEpiA.Splitter1TopPtr := @fraStatsEpiB.Splitter1.Top;
      fraStatsEpiB.Splitter1TopPtr := @fraStatsEpiA.Splitter1.Top;

      fraStatsEpiA.pnlTableHeightPtr := @fraStatsEpiB.pnlTable.Height;
      fraStatsEpiB.pnlTableHeightPtr := @fraStatsEpiA.pnlTable.Height;

      fraStatsEpiA.pnlTopSectionHeightPtr := @fraStatsEpiB.pnlTopSection.Height;
      fraStatsEpiB.pnlTopSectionHeightPtr := @fraStatsEpiA.pnlTopSection.Height;

      fraStatsEpiA.Splitter1MovedPtr := fraStatsEpiB.Splitter1Moved;
      fraStatsEpiB.Splitter1MovedPtr := fraStatsEpiA.Splitter1Moved;

      fraStatsEpiA.selectCellPtr := fraStatsEpiB.selectCell;
      fraStatsEpiB.selectCellPtr := fraStatsEpiA.selectCell;


      // Function Pointers for Cost Tab
      //-------------------------------
      fraStatsCostA.fraHistogram.rdoHistoTypeClickPtr := fraStatsCostB.fraHistogram.rdoBinNumberClick;
      fraStatsCostB.fraHistogram.rdoHistoTypeClickPtr := fraStatsCostA.fraHistogram.rdoBinNumberClick;

      fraStatsCostA.fraHistogram.cbx3DViewClickPtr := fraStatsCostB.fraHistogram.cbx3DViewClick;
      fraStatsCostB.fraHistogram.cbx3DViewClickPtr := fraStatsCostA.fraHistogram.cbx3DViewClick;

      fraStatsCostA.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsCostB.fraHistogram.rleHistoBinsEnter;
      fraStatsCostB.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsCostA.fraHistogram.rleHistoBinsEnter;

      fraStatsCostA.fraHistogram.btnCancelClickPtr := fraStatsCostB.fraHistogram.btnCancelClick;
      fraStatsCostB.fraHistogram.btnCancelClickPtr := fraStatsCostA.fraHistogram.btnCancelClick;

      fraStatsCostA.fraHistogram.btnAcceptClickPtr := fraStatsCostB.fraHistogram.btnAcceptClick;
      fraStatsCostB.fraHistogram.btnAcceptClickPtr := fraStatsCostA.fraHistogram.btnAcceptClick;

      fraStatsCostA.cbxClickPtr := fraStatsCostB.changeCheck;
      fraStatsCostB.cbxClickPtr := fraStatsCostA.changeCheck;

      fraStatsCostA.Splitter1TopPtr := @fraStatsCostB.Splitter1.Top;
      fraStatsCostB.Splitter1TopPtr := @fraStatsCostA.Splitter1.Top;

      fraStatsCostA.pnlTableHeightPtr := @fraStatsCostB.pnlTable.Height;
      fraStatsCostB.pnlTableHeightPtr := @fraStatsCostA.pnlTable.Height;

      fraStatsCostA.pnlTopSectionHeightPtr := @fraStatsCostB.pnlTopSection.Height;
      fraStatsCostB.pnlTopSectionHeightPtr := @fraStatsCostA.pnlTopSection.Height;

      fraStatsCostA.Splitter1MovedPtr := fraStatsCostB.Splitter1Moved;
      fraStatsCostB.Splitter1MovedPtr := fraStatsCostA.Splitter1Moved;

      fraStatsCostA.selectCellPtr := fraStatsCostB.selectCell;
      fraStatsCostB.selectCellPtr := fraStatsCostA.selectCell;

      // Function pointers for Zones/production types tab
      //-------------------------------------------------
      fraStatsPTZonesA.fraHistogram.rdoHistoTypeClickPtr := fraStatsPTZonesB.fraHistogram.rdoBinNumberClick;
      fraStatsPTZonesB.fraHistogram.rdoHistoTypeClickPtr := fraStatsPTZonesA.fraHistogram.rdoBinNumberClick;

      fraStatsPTZonesA.fraHistogram.cbx3DViewClickPtr := fraStatsPTZonesB.fraHistogram.cbx3DViewClick;
      fraStatsPTZonesB.fraHistogram.cbx3DViewClickPtr := fraStatsPTZonesA.fraHistogram.cbx3DViewClick;

      fraStatsPTZonesA.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsPTZonesB.fraHistogram.rleHistoBinsEnter;
      fraStatsPTZonesB.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsPTZonesA.fraHistogram.rleHistoBinsEnter;

      fraStatsPTZonesA.fraHistogram.btnCancelClickPtr := fraStatsPTZonesB.fraHistogram.btnCancelClick;
      fraStatsPTZonesB.fraHistogram.btnCancelClickPtr := fraStatsPTZonesA.fraHistogram.btnCancelClick;

      fraStatsPTZonesA.fraHistogram.btnAcceptClickPtr := fraStatsPTZonesB.fraHistogram.btnAcceptClick;
      fraStatsPTZonesB.fraHistogram.btnAcceptClickPtr := fraStatsPTZonesA.fraHistogram.btnAcceptClick;

      fraStatsPTZonesA.cbxClickPtr := fraStatsPTZonesB.changeCheck;
      fraStatsPTZonesB.cbxClickPtr := fraStatsPTZonesA.changeCheck;

      fraStatsPTZonesA.Splitter1TopPtr := @fraStatsPTZonesB.Splitter1.Top;
      fraStatsPTZonesB.Splitter1TopPtr := @fraStatsPTZonesA.Splitter1.Top;

      fraStatsPTZonesA.pnlTableHeightPtr := @fraStatsPTZonesB.pnlTable.Height;
      fraStatsPTZonesB.pnlTableHeightPtr := @fraStatsPTZonesA.pnlTable.Height;

      fraStatsPTZonesA.pnlTopSectionHeightPtr := @fraStatsPTZonesB.pnlTopSection.Height;
      fraStatsPTZonesB.pnlTopSectionHeightPtr := @fraStatsPTZonesA.pnlTopSection.Height;

      fraStatsPTZonesA.Splitter1MovedPtr := fraStatsPTZonesB.Splitter1Moved;
      fraStatsPTZonesB.Splitter1MovedPtr := fraStatsPTZonesA.Splitter1Moved;

      fraStatsPTZonesA.selectCellPtr := fraStatsPTZonesB.selectCell;
      fraStatsPTZonesB.selectCellPtr := fraStatsPTZonesA.selectCell;

      // Function pointers for Zones tab
      //--------------------------------
      fraStatsZonesA.fraHistogram.rdoHistoTypeClickPtr := fraStatsZonesB.fraHistogram.rdoBinNumberClick;
      fraStatsZonesB.fraHistogram.rdoHistoTypeClickPtr := fraStatsZonesA.fraHistogram.rdoBinNumberClick;

      fraStatsZonesA.fraHistogram.cbx3DViewClickPtr := fraStatsZonesB.fraHistogram.cbx3DViewClick;
      fraStatsZonesB.fraHistogram.cbx3DViewClickPtr := fraStatsZonesA.fraHistogram.cbx3DViewClick;

      fraStatsZonesA.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsZonesB.fraHistogram.rleHistoBinsEnter;
      fraStatsZonesB.fraHistogram.rleHistoBinsEnterPtr :=  fraStatsZonesA.fraHistogram.rleHistoBinsEnter;

      fraStatsZonesA.fraHistogram.btnCancelClickPtr := fraStatsZonesB.fraHistogram.btnCancelClick;
      fraStatsZonesB.fraHistogram.btnCancelClickPtr := fraStatsZonesA.fraHistogram.btnCancelClick;

      fraStatsZonesA.fraHistogram.btnAcceptClickPtr := fraStatsZonesB.fraHistogram.btnAcceptClick;
      fraStatsZonesB.fraHistogram.btnAcceptClickPtr := fraStatsZonesA.fraHistogram.btnAcceptClick;

      fraStatsZonesA.cbxClickPtr := fraStatsZonesB.changeCheck;
      fraStatsZonesB.cbxClickPtr := fraStatsZonesA.changeCheck;

      fraStatsZonesA.Splitter1TopPtr := @fraStatsZonesB.Splitter1.Top;
      fraStatsZonesB.Splitter1TopPtr := @fraStatsZonesA.Splitter1.Top;

      fraStatsZonesA.pnlTableHeightPtr := @fraStatsZonesB.pnlTable.Height;
      fraStatsZonesB.pnlTableHeightPtr := @fraStatsZonesA.pnlTable.Height;

      fraStatsZonesA.pnlTopSectionHeightPtr := @fraStatsZonesB.pnlTopSection.Height;
      fraStatsZonesB.pnlTopSectionHeightPtr := @fraStatsZonesA.pnlTopSection.Height;

      fraStatsZonesA.Splitter1MovedPtr := fraStatsZonesB.Splitter1Moved;
      fraStatsZonesB.Splitter1MovedPtr := fraStatsZonesA.Splitter1Moved;

      fraStatsZonesA.selectCellPtr := fraStatsZonesB.selectCell;
      fraStatsZonesB.selectCellPtr := fraStatsZonesA.selectCell;

      // Finishing the contruction
      //--------------------------
      pgcOutputsA.Visible := true;
      pgcOutputsB.Visible := true;

      self.Perform( WM_SETREDRAW, 1, 0 );
      RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );

      if( AOwner is TForm ) then
        begin
          frm := AOwner as TForm;
          self.Height := round( frm.ClientHeight * 0.9 );
        end
      else
        self.Height := 650
      ;
    end
  ;


  procedure TFormScenarioComparison.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormScenarioComparison.dfm
      // File date: Thu May 10 16:40:18 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario comparison' );
          pnlFileNameB.Caption := tr( 'FileNameB' );
          tabEpiOutputsB.Caption := tr( 'Epidemiology' );
          tabCostOutputsB.Caption := tr( 'Cost accounting' );
          tabPTZoneOutputsB.Caption := tr( 'Zones/production types' );
          tabZoneOutputsB.Caption := tr( 'Zones' );
          pnlFileNameA.Caption := tr( 'FileNameA' );
          tabEpiOutputsA.Caption := tr( 'Epidemiology' );
          tabCostOutputsA.Caption := tr( 'Cost accounting' );
          tabPTZoneOutputsA.Caption := tr( 'Zones/production types' );
          tabZoneOutputsA.Caption := tr( 'Zones' );
        end
      ;

    end
  ;


  procedure TFormScenarioComparison.initialize();
    begin
      dbcout( 'Creating output list A...', debug );
      _scenarioStatsA := TScenarioOutputSet.create( _smdbA, _smSimA.includeCostsGlobal, _smSimA.includeZonesGlobal, _smSimA.costTrackZoneSurveillance );

      dbcout( 'Creating output list B...', debug );
      _scenarioStatsB := TScenarioOutputSet.create( _smdbB, _smSimB.includeCostsGlobal, _smSimB.includeZonesGlobal, _smSimB.costTrackZoneSurveillance );

      dbcout( 'Setting up frame A...', debug );
      fraStatsEpiA.setOutputType( StatsEpi );
			fraStatsEpiA.setEpiStats( _scenarioStatsA.epiStats );
			fraStatsEpiA.setProdType( _selectedPT );
      fraStatsEpiA.resizeContents();

      dbcout( 'Setting up frame B...', debug );
      fraStatsEpiB.setOutputType( StatsEpi );
			fraStatsEpiB.setEpiStats( _scenarioStatsB.epiStats );
			fraStatsEpiB.setProdType( _selectedPT );
      fraStatsEpiB.resizeContents();

      dbcout( 'Setting up cost tabs...', debug );

      if( not( _smSimA.includeCostsGlobal ) ) then
        begin
          pgcOutputsA.ActivePage := tabEpiOutputsA;
          tabCostOutputsA.Enabled := false;
        end
      else
        begin
          tabCostOutputsA.enabled := true;
          fraStatsCostA.setOutputType( StatsCosts );
          fraStatsCostA.setCostStats( _scenarioStatsA.costStats );
          fraStatsCostA.setProdType( _selectedPT );
          fraStatsCostA.resizeContents();
        end
      ;

      if( not( _smSimB.includeCostsGlobal ) ) then
        begin
          pgcOutputsB.ActivePage := tabEpiOutputsA;
          tabCostOutputsB.Enabled := false;
        end
      else
        begin
          tabCostOutputsB.enabled := true;
          fraStatsCostB.setOutputType( StatsCosts );
          fraStatsCostB.setCostStats( _scenarioStatsB.costStats );
          fraStatsCostB.setProdType( _selectedPT );
          fraStatsCostB.resizeContents();
        end
      ;

      if( not( _showZoneTabs ) or not( _smSimA.includeZonesGlobal ) ) then
        begin
          pgcOutputsA.ActivePage := tabEpiOutputsA;
          tabPTZoneOutputsA.Enabled := false;
          tabZoneOutputsA.Enabled := false;
        end
      else
        begin
          tabPTZoneOutputsA.enabled := true;
          fraStatsPTZonesA.setOutputType( StatsPTZones );
          fraStatsPTZonesA.setZonePTStats( _scenarioStatsA.zonePTStats );
          fraStatsPTZonesA.setProdType( _selectedPT );
          fraStatsPTZonesA.setZone( nil );
          fraStatsPTZonesA.resizeContents();

          tabZoneOutputsA.Enabled := true;
          fraStatsZonesA.setOutputType( StatsZones );
          fraStatsZonesA.setZoneStats( _scenarioStatsA.zoneStats );
          fraStatsZonesA.setProdType( _selectedPT );
          fraStatsZonesA.setZone( nil );
          fraStatsZonesA.resizeContents();
        end
      ;

      if( not( _showZoneTabs ) or not( _smSimA.includeZonesGlobal ) ) then
        begin
          pgcOutputsA.ActivePage := tabEpiOutputsA;
          tabPTZoneOutputsB.Enabled := false;
          tabZoneOutputsB.Enabled := false;
        end
      else
        begin
          tabPTZoneOutputsB.enabled := true;
          fraStatsPTZonesB.setOutputType( StatsPTZones );
          fraStatsPTZonesB.setZonePTStats( _scenarioStatsB.zonePTStats );
          fraStatsPTZonesB.setProdType( _selectedPT );
          fraStatsPTZonesB.setZone( nil );
          fraStatsPTZonesB.resizeContents();

          tabZoneOutputsB.enabled := true;
          fraStatsZonesB.setOutputType( StatsZones );
          fraStatsZonesB.setZoneStats( _scenarioStatsB.zoneStats );
          fraStatsZonesB.setProdType( _selectedPT );
          fraStatsZonesB.setZone( nil );
          fraStatsZonesB.resizeContents();
        end
      ;

      pgcOutputsA.forceRepaint();
      pgcOutputsB.forceRepaint();
    end
  ;


  destructor TFormScenarioComparison.destroy();
    begin
      freeAndNil( _scenarioStatsA );
      freeAndNil( _scenarioStatsB );

      freeAndNil( _smSimB );

      _smdbB.close();
      deleteFile( _smdbB.workingDBFileName );
      freeAndNil( _smdbB );

      inherited destroy();
    end
  ;


  procedure TFormScenarioComparison.pgcOutputsAMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      ActiveIndex: integer;
    begin
      inherited;
      ActiveIndex := pgcOutputsA.ActivePageIndex;
      pgcOutputsB.ActivePageIndex := ActiveIndex;
      cboZones.Visible := ( ( pgcOutputsA.ActivePage = tabPTZoneOutputsA ) or ( pgcOutputsA.ActivePage = tabZoneOutputsA ) );
      cboProdTypes.Visible := ( not( pgcOutputsA.ActivePage = tabZoneOutputsA ) );
    end
  ;


  procedure TFormScenarioComparison.pgcOutputsBMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    var
      ActiveIndex: integer;
    begin
      inherited;
      ActiveIndex := pgcOutputsB.ActivePageIndex;
      pgcOutputsA.ActivePageIndex := ActiveIndex;
      cboZones.Visible := ( ( pgcOutputsA.ActivePage = tabPTZoneOutputsA ) or ( pgcOutputsA.ActivePage = tabZoneOutputsA ) );
      cboProdTypes.Visible := ( not( pgcOutputsA.ActivePage = tabZoneOutputsA ) );
    end
  ;


  procedure TFormScenarioComparison.zoneChanged();
    var
      activePage: integer;
    begin
      try
        screen.Cursor := crHourGlass;
        self.Perform( WM_SETREDRAW, 0, 0 );

        setControlsEnabled( false );

        activePage := pgcOutputsA.ActivePageIndex;

        if( _smSim.includeZonesGlobal ) then
          begin
            fraStatsPTZonesA.setZone( _selectedZone );
            fraStatsZonesA.setZone( _selectedZone );

            if( nil = _selectedZone ) then
              begin
                fraStatsPTZonesB.setZone( nil );
                fraStatsZonesB.setZone( nil );
              end
            else
              begin
                fraStatsPTZonesB.setZone( _smSimB.zoneList.find( _selectedZone.id ) );
                fraStatsZonesB.setZone( _smSimB.zoneList.find( _selectedZone.id ) );
              end
            ;
            pgcOutputsA.ActivePageIndex := activePage;
            pgcOutputsB.ActivePageIndex := activePage;
          end
        else
          begin
            pgcOutputsA.ActivePage := tabEpiOutputsA;
            pgcOutputsB.ActivePage := tabEpiOutputsB;
          end
        ;

      finally
        setControlsEnabled( true );
        screen.Cursor := crDefault;

        rescaleChartAxes();

        self.Perform( WM_SETREDRAW, 1, 0 );
        RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
      end;
    end
  ;


  procedure TFormScenarioComparison.productionTypeChanged();
    var
      activePage: integer;
    begin
      try
        screen.Cursor := crHourGlass;
        self.Perform( WM_SETREDRAW, 0, 0 );

        setControlsEnabled( false );

        activePage := pgcOutputsA.ActivePageIndex;

        fraStatsEpiA.setProdType( _selectedPT );
        if( nil = _selectedPT ) then
          fraStatsEpiB.setProdType( nil )
        else
          fraStatsEpiB.setProdType( _smSimB.ptList.findProdType( _selectedPT.productionTypeID ) )
        ;

        if( _smSim.includeCostsGlobal ) then
          begin
            fraStatsCostA.setProdType( _selectedPT );

            if( nil = _selectedPT ) then
              fraStatsCostB.setProdType( nil )
            else
              fraStatsCostB.setProdType( _smSimB.ptList.findProdType( _selectedPT.productionTypeID ) )
            ;
            pgcOutputsA.ActivePageIndex := activePage;
            pgcOutputsB.ActivePageIndex := activePage;
          end
        else
          begin
            pgcOutputsA.ActivePage := tabEpiOutputsA;
            pgcOutputsB.ActivePage := tabEpiOutputsB;
          end
        ;

        if( _smSim.includeZonesGlobal ) then
          begin
            fraStatsPTZonesA.setProdType( _selectedPT );
            fraStatsZonesA.setProdType( _selectedPT );

            if( nil = _selectedPT ) then
              begin
                fraStatsPTZonesB.setProdType( nil );
                fraStatsZonesB.setProdType( nil );
              end
            else
              begin
                fraStatsPTZonesB.setProdType( _smSimB.ptList.findProdType( _selectedPT.productionTypeID ) );
                fraStatsZonesB.setProdType( _smSimB.ptList.findProdType( _selectedPT.productionTypeID ) );
              end
            ;
            pgcOutputsA.ActivePageIndex := activePage;
            pgcOutputsB.ActivePageIndex := activePage;
          end
        else
          begin
            pgcOutputsA.ActivePage := tabEpiOutputsA;
            pgcOutputsB.ActivePage := tabEpiOutputsB;
          end
        ;

      finally
        setControlsEnabled( true );
        screen.Cursor := crDefault;

        self.Perform( WM_SETREDRAW, 1, 0 );
        RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
      end;
    end
  ;


  procedure TFormScenarioComparison.setHistogramBinNumbers( sender: TComponent; const nBins: integer );
    var
      frame: TFrameArrayHistogram;
    begin
      if( sender is TFrameArrayHistogram ) then
        frame := sender as TFrameArrayHistogram
      else
        begin
          raise exception.create( 'Unexpected sender type in TFormScenarioComparison.setHistogramBinNumbers()' );
          exit;
        end
      ;

      if( frame <> self.fraStatsEpiA.fraHistogram ) then
        self.fraStatsEpiA.fraHistogram.setBinNumber( frame.breakType, nBins )
      ;
      if( frame <> self.fraStatsEpiB.fraHistogram ) then
        self.fraStatsEpiB.fraHistogram.setBinNumber( frame.breakType, nBins )
      ;

      if( frame <> self.fraStatsCostA.fraHistogram ) then
        begin
          if( _smSimA.includeCostsGlobal ) then
            self.fraStatsCostA.fraHistogram.setBinNumber( frame.breakType, nBins )
          ;
        end
      ;
      if( frame <> self.fraStatsCostB.fraHistogram ) then
        begin
          if( _smSimB.includeCostsGlobal ) then
            self.fraStatsCostB.fraHistogram.setBinNumber( frame.breakType, nBins )
          ;
        end
      ;

      if( _showZoneTabs ) then
        begin
          if( _smSimA.includeZonesGlobal ) then
            begin
              if( frame <> self.fraStatsPTZonesA.fraHistogram ) then
                self.fraStatsPTZonesA.fraHistogram.setBinNumber( frame.breakType, nBins )
              ;
              if( frame <> self.fraStatsZonesA.fraHistogram ) then
                self.fraStatsZonesA.fraHistogram.setBinNumber( frame.breakType, nBins )
              ;
            end
          ;

          if( _smSimB.includeZonesGlobal ) then
            begin
              if( frame <> self.fraStatsPTZonesB.fraHistogram ) then
                self.fraStatsPTZonesB.fraHistogram.setBinNumber( frame.breakType, nBins )
              ;
              if( frame <> self.fraStatsPTZonesB.fraHistogram ) then
                self.fraStatsZonesB.fraHistogram.setBinNumber( frame.breakType, nBins )
              ;
            end
          ;
        end
      ;

      rescaleChartAxes();
    end
  ;


  procedure TFormScenarioComparison.rescaleChartAxes();
    begin
      rescaleChartAxes( fraStatsEpiA.fraHistogram.chtHistogram.series[0], fraStatsEpiB.fraHistogram.chtHistogram.series[0], true );

      if( pgcOutputsA.Pages[1].Enabled ) then
        rescaleChartAxes( fraStatsCostA.fraHistogram.chtHistogram.Series[0], fraStatsCostB.fraHistogram.chtHistogram.Series[0], true )
      ;

      if( pgcOutputsA.Pages[2].Enabled ) then
        rescaleChartAxes( fraStatsPTZonesA.fraHistogram.chtHistogram.Series[0], fraStatsPTZonesB.fraHistogram.chtHistogram.Series[0], true )
      ;

      if( pgcOutputsA.Pages[3].Enabled ) then
        rescaleChartAxes( fraStatsZonesA.fraHistogram.chtHistogram.Series[0], fraStatsZonesB.fraHistogram.chtHistogram.Series[0], true )
      ;
    end
  ;
  

  procedure TFormScenarioComparison.rescaleChartAxes( a, b: TChartSeries; isHistogram: boolean );
    var
      minX, maxX: double;
      minY, maxY: double;
      range: double;
    begin
      minY := min( a.YValues.minValue, b.YValues.minValue );
      maxY := max( a.YValues.maxValue, b.YValues.maxValue );

      minX := min( a.XValues.minValue, b.XValues.minValue );
      maxX := max( a.XValues.maxValue, b.XValues.maxValue );

      if( isHistogram ) then
        begin
          a.GetVertAxis.setMinMax( 0.0, maxY );
          b.GetVertAxis.SetMinMax( 0.0, maxY );

          if( 100 < (maxX - minX) ) then
            range := ( maxX - minX) / 10
          else
            range := 5.0
          ;
          maxX := maxX + range;
          minX := minX - range;
        end
      else
        begin
          a.GetVertAxis.setMinMax( minY, maxY );
          b.GetVertAxis.SetMinMax( minY, maxY );
        end
      ;

      a.GetHorizAxis.SetMinMax( minX, maxX );
      b.GetHorizAxis.setMinMax( minX, maxX );
    end
  ;


  procedure TFormScenarioComparison.FormResize(Sender: TObject);
    begin
      inherited;
      pnlA.Width := pnlFrames.ClientWidth div 2;
    end
  ;


end.



