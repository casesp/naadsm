unit FormScenarioComparison;

(*
FormScenarioComparison.pas/dfm
------------------------------
Begin: 2006/10/31
Last revision: $Date: 2008/04/18 20:35:17 $ $Author: areeves $
Version: $Revision: 1.13 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
Author: Snehal Shetye <snehal@goku.engr.colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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
    FrameArrayConvergence,
    SMSimulationInput,
    SMDatabase,
    ProductionType,

    SMSimulationStats,
    SMZoneStats,
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
      tabZoneOutputsA: TTabSheet;
      fraStatsZonesA: TFrameOutputStats;

      pnlB: TPanel;
      pnlFileNameB: TPanel;
      pgcOutputsB: TPBPageControl;
      tabEpiOutputsB: TTabSheet;
      fraStatsEpiB: TFrameOutputStats;
      tabCostOutputsB: TTabSheet;
      fraStatsCostB: TFrameOutputStats;
      tabZoneOutputsB: TTabSheet;
      fraStatsZonesB: TFrameOutputStats;

      procedure pgcOutputsAMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure pgcOutputsBMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

      procedure FormResize(Sender: TObject);

    protected
      // A whole mess of function pointers...
      // ...for the epi frames...
      rleHistoBinsEnterEpiPtrA: TPtr;
      rleHistoBinsEnterEpiPtrB: TPtr;
      btnCancelClickEpiPtrA: TPtr;
      btnCancelClickEpiPtrB: TPtr;
      btnAcceptClickEpiPtrA: TPtr;
      btnAcceptClickEpiPtrB: TPtr;
      cbxClickEpiPtrA: TPtrBool;
      cbxClickEpiPtrB: TPtrBool;
      rdgConvergenceParamClickEpiPtrA: TPtr;
      rdgConvergenceParamClickEpiPtrB: TPtr;
      fraTablestgOutputStatsSelectCellEpiPtrA: TPtrGrid;
      fraTablestgOutputStatsSelectCellEpiPtrB: TPtrGrid;
      Splitter1MovedEpiPtrA: TPtr;
      Splitter2MovedEpiPtrA: TPtr;
      Splitter1MovedEpiPtrB: TPtr;
      Splitter2MovedEpiPtrB: TPtr;

      // ...for the cost frames...
      rleHistoBinsEnterCostPtrA: TPtr;
      rleHistoBinsEnterCostPtrB: TPtr;
      btnCancelClickCostPtrA: TPtr;
      btnCancelClickCostPtrB: TPtr;
      btnAcceptClickCostPtrA: TPtr;
      btnAcceptClickCostPtrB: TPtr;
      cbxClickCostPtrA: TPtrBool;
      cbxClickCostPtrB: TPtrBool;
      rdgConvergenceParamClickCostPtrA: TPtr;
      rdgConvergenceParamClickCostPtrB: TPtr;
      fraTablestgOutputStatsSelectCellCostPtrA: TPtrGrid;
      fraTablestgOutputStatsSelectCellCostPtrB: TPtrGrid;
      Splitter1MovedCostPtrA: TPtr;
      Splitter2MovedCostPtrA: TPtr;
      Splitter1MovedCostPtrB: TPtr;
      Splitter2MovedCostPtrB: TPtr;

      // ...and for the zone frames.
      rleHistoBinsEnterZonesPtrA: TPtr;
      rleHistoBinsEnterZonesPtrB: TPtr;
      btnCancelClickZonesPtrA: TPtr;
      btnCancelClickZonesPtrB: TPtr;
      btnAcceptClickZonesPtrA: TPtr;
      btnAcceptClickZonesPtrB: TPtr;
      cbxClickZonesPtrA: TPtrBool;
      cbxClickZonesPtrB: TPtrBool;
      rdgConvergenceParamClickZonesPtrA: TPtr;
      rdgConvergenceParamClickZonesPtrB: TPtr;
      fraTablestgOutputStatsSelectCellZonesPtrA: TPtrGrid;
      fraTablestgOutputStatsSelectCellZonesPtrB: TPtrGrid;
      Splitter1MovedZonesPtrA: TPtr;
      Splitter2MovedZonesPtrA: TPtr;
      Splitter1MovedZonesPtrB: TPtr;
      Splitter2MovedZonesPtrB: TPtr;

      _resizingForm: boolean;

      _superListA: TSMIterationOutputSuperList;
      _scenarioStatsA: TScenarioOutputSet;
      _superListB: TSMIterationOutputSuperList;
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
    GuiStrUtils,
    SqlClasses,
    DebugWindow,
    MyDialogs,
    I88n,

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

      fraStatsEpiA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsEpiB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsCostA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsCostB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsZonesA.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;
      fraStatsZonesB.fraHistogram.setBinNumberPtr := setHistogramBinNumbers;

      fraStatsEpiA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsEpiB.setAxisValuesPtr := rescaleChartAxes;
      fraStatsCostA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsCostB.setAxisValuesPtr := rescaleChartAxes;
      fraStatsZonesA.setAxisValuesPtr := rescaleChartAxes;
      fraStatsZonesB.setAxisValuesPtr := rescaleChartAxes;

      pnlFileNameA.Caption := '';
      pnlFileNameB.Caption := '';

      // Intialize items needed for grid synchronization
      //------------------------------------------------
      makeSyncGridPair( self.fraStatsEpiA.fraTable.stgGrid, self.fraStatsEpiB.fraTable.stgGrid );
      makeSyncGridPair( self.fraStatsCostA.fraTable.stgGrid, self.fraStatsCostB.fraTable.stgGrid );
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
      setupComboBox();

      pgcOutputsA.ActivePage := tabEpiOutputsA;
      pgcOutputsB.ActivePage := tabEpiOutputsB;

      dbcout( 'Initializing...', debug );
      initialize();
      dbcout( 'Done with initializer!', debug );

      // Function Pointers for Epidemiology tab
      //---------------------------------------
      rleHistoBinsEnterEpiPtrA := fraStatsEpiA.fraHistogram.rleHistoBinsEnter;
      rleHistoBinsEnterEpiPtrB := fraStatsEpiB.fraHistogram.rleHistoBinsEnter;
      fraStatsEpiA.fraHistogram.rleHistoBinsEnterPtr :=  rleHistoBinsEnterEpiPtrB;
      fraStatsEpiB.fraHistogram.rleHistoBinsEnterPtr :=  rleHistoBinsEnterEpiPtrA;

      btnCancelClickEpiPtrA := fraStatsEpiA.fraHistogram.btnCancelClick;
      btnCancelClickEpiPtrB := fraStatsEpiB.fraHistogram.btnCancelClick;
      fraStatsEpiA.fraHistogram.btnCancelClickPtr := btnCancelClickEpiPtrB;
      fraStatsEpiB.fraHistogram.btnCancelClickPtr := btnCancelClickEpiPtrA;

      btnAcceptClickEpiPtrA := fraStatsEpiA.fraHistogram.btnAcceptClick;
      btnAcceptClickEpiPtrB := fraStatsEpiB.fraHistogram.btnAcceptClick;
      fraStatsEpiA.fraHistogram.btnAcceptClickPtr := btnAcceptClickEpiPtrB;
      fraStatsEpiB.fraHistogram.btnAcceptClickPtr := btnAcceptClickEpiPtrA;

      cbxClickEpiPtrA := fraStatsEpiB.changeCheck;
      cbxClickEpiPtrB := fraStatsEpiA.changeCheck;
      fraStatsEpiA.cbxClickPtr := cbxClickEpiPtrA;
      fraStatsEpiB.cbxClickPtr := cbxClickEpiPtrB;

      rdgConvergenceParamClickEpiPtrA := fraStatsEpiA.fraConvergence.rdgConvergenceParamClick;
      rdgConvergenceParamClickEpiPtrB := fraStatsEpiB.fraConvergence.rdgConvergenceParamClick;
      fraStatsEpiA.fraConvergence.rdgConvergenceParamClickPtr := rdgConvergenceParamClickEpiPtrB;
      fraStatsEpiB.fraConvergence.rdgConvergenceParamClickPtr := rdgConvergenceParamClickEpiPtrA;

      fraStatsEpiA.fraHistogram.entryPtr := @fraStatsEpiB.fraHistogram._entry;
      fraStatsEpiB.fraHistogram.entryPtr := @fraStatsEpiA.fraHistogram._entry;

      fraStatsEpiA.fraConvergence.rdgItemIndexPtr := @fraStatsEpiB.fraConvergence.rdgConvergenceParam.ItemIndex;
      fraStatsEpiB.fraConvergence.rdgItemIndexPtr := @fraStatsEpiA.fraConvergence.rdgConvergenceParam.ItemIndex;

      fraTablestgOutputStatsSelectCellEpiPtrA := fraStatsEpiA.fraTablestgOutputStatsSelectCell;
      fraTablestgOutputStatsSelectCellEpiPtrB := fraStatsEpiB.fraTablestgOutputStatsSelectCell;
      fraStatsEpiA.fraTablestgOutputStatsSelectCellPtr := fraTablestgOutputStatsSelectCellEpiPtrB;
      fraStatsEpiB.fraTablestgOutputStatsSelectCellPtr := fraTablestgOutputStatsSelectCellEpiPtrA;

      fraStatsEpiA.Splitter1TopPtr := @fraStatsEpiB.Splitter1.Top;
      fraStatsEpiB.Splitter1TopPtr := @fraStatsEpiA.Splitter1.Top;
      fraStatsEpiA.Splitter2TopPtr := @fraStatsEpiB.Splitter2.Top;
      fraStatsEpiB.Splitter2TopPtr := @fraStatsEpiA.Splitter2.Top;

      fraStatsEpiA.pnlTableHeightPtr := @fraStatsEpiB.pnlTable.Height;
      fraStatsEpiB.pnlTableHeightPtr := @fraStatsEpiA.pnlTable.Height;

      fraStatsEpiA.pnlTopSectionHeightPtr := @fraStatsEpiB.pnlTopSection.Height;
      fraStatsEpiB.pnlTopSectionHeightPtr := @fraStatsEpiA.pnlTopSection.Height;

      Splitter1MovedEpiPtrA := fraStatsEpiA.Splitter1Moved;
      Splitter1MovedEpiPtrB := fraStatsEpiB.Splitter1Moved;
      fraStatsEpiA.Splitter1MovedPtr := Splitter1MovedEpiPtrB;
      fraStatsEpiB.Splitter1MovedPtr := Splitter1MovedEpiPtrA;

      Splitter2MovedEpiPtrA := fraStatsEpiA.Splitter2Moved;
      Splitter2MovedEpiPtrB := fraStatsEpiB.Splitter2Moved;
      fraStatsEpiA.Splitter2MovedPtr := Splitter2MovedEpiPtrB;
      fraStatsEpiB.Splitter2MovedPtr := Splitter2MovedEpiPtrA;

      // Function Pointers for Cost Tab
      //-------------------------------
      rleHistoBinsEnterCostPtrA := fraStatsCostA.fraHistogram.rleHistoBinsEnter;
      rleHistoBinsEnterCostPtrB := fraStatsCostB.fraHistogram.rleHistoBinsEnter;
      fraStatsCostA.fraHistogram.rleHistoBinsEnterPtr :=  rleHistoBinsEnterCostPtrB;
      fraStatsCostB.fraHistogram.rleHistoBinsEnterPtr :=  rleHistoBinsEnterCostPtrA;

      btnCancelClickCostPtrA := fraStatsCostA.fraHistogram.btnCancelClick;
      btnCancelClickCostPtrB := fraStatsCostB.fraHistogram.btnCancelClick;
      fraStatsCostA.fraHistogram.btnCancelClickPtr := btnCancelClickCostPtrB;
      fraStatsCostB.fraHistogram.btnCancelClickPtr := btnCancelClickCostPtrA;

      btnAcceptClickCostPtrA := fraStatsCostA.fraHistogram.btnAcceptClick;
      btnAcceptClickCostPtrB := fraStatsCostB.fraHistogram.btnAcceptClick;
      fraStatsCostA.fraHistogram.btnAcceptClickPtr := btnAcceptClickCostPtrB;
      fraStatsCostB.fraHistogram.btnAcceptClickPtr := btnAcceptClickCostPtrA;

      cbxClickCostPtrA := fraStatsCostB.changeCheck;
      cbxClickCostPtrB := fraStatsCostA.changeCheck;
      fraStatsCostA.cbxClickPtr := cbxClickCostPtrA;
      fraStatsCostB.cbxClickPtr := cbxClickCostPtrB;

      rdgConvergenceParamClickCostPtrA := fraStatsCostA.fraConvergence.rdgConvergenceParamClick;
      rdgConvergenceParamClickCostPtrB := fraStatsCostB.fraConvergence.rdgConvergenceParamClick;
      fraStatsCostA.fraConvergence.rdgConvergenceParamClickPtr := rdgConvergenceParamClickCostPtrB;
      fraStatsCostB.fraConvergence.rdgConvergenceParamClickPtr := rdgConvergenceParamClickCostPtrA;

      fraStatsCostA.fraHistogram.entryPtr := @fraStatsCostB.fraHistogram._entry;
      fraStatsCostB.fraHistogram.entryPtr := @fraStatsCostA.fraHistogram._entry;

      fraStatsCostA.fraConvergence.rdgItemIndexPtr := @fraStatsCostB.fraConvergence.rdgConvergenceParam.ItemIndex;
      fraStatsCostB.fraConvergence.rdgItemIndexPtr := @fraStatsCostA.fraConvergence.rdgConvergenceParam.ItemIndex;

      fraTablestgOutputStatsSelectCellCostPtrA := fraStatsCostA.fraTablestgOutputStatsSelectCell;
      fraTablestgOutputStatsSelectCellCostPtrB := fraStatsCostB.fraTablestgOutputStatsSelectCell;
      fraStatsCostA.fraTablestgOutputStatsSelectCellPtr := fraTablestgOutputStatsSelectCellCostPtrB;
      fraStatsCostB.fraTablestgOutputStatsSelectCellPtr := fraTablestgOutputStatsSelectCellCostPtrA;

      fraStatsCostA.Splitter1TopPtr := @fraStatsCostB.Splitter1.Top;
      fraStatsCostB.Splitter1TopPtr := @fraStatsCostA.Splitter1.Top;
      fraStatsCostA.Splitter2TopPtr := @fraStatsCostB.Splitter2.Top;
      fraStatsCostB.Splitter2TopPtr := @fraStatsCostA.Splitter2.Top;

      fraStatsCostA.pnlTableHeightPtr := @fraStatsCostB.pnlTable.Height;
      fraStatsCostB.pnlTableHeightPtr := @fraStatsCostA.pnlTable.Height;

      fraStatsCostA.pnlTopSectionHeightPtr := @fraStatsCostB.pnlTopSection.Height;
      fraStatsCostB.pnlTopSectionHeightPtr := @fraStatsCostA.pnlTopSection.Height;

      Splitter1MovedCostPtrA := fraStatsCostA.Splitter1Moved;
      Splitter1MovedCostPtrB := fraStatsCostB.Splitter1Moved;
      fraStatsCostA.Splitter1MovedPtr := Splitter1MovedCostPtrB;
      fraStatsCostB.Splitter1MovedPtr := Splitter1MovedCostPtrA;

      Splitter2MovedCostPtrA := fraStatsCostA.Splitter2Moved;
      Splitter2MovedCostPtrB := fraStatsCostB.Splitter2Moved;
      fraStatsCostA.Splitter2MovedPtr := Splitter2MovedCostPtrB;
      fraStatsCostB.Splitter2MovedPtr := Splitter2MovedCostPtrA;

      // Function Pointers for Zones Tab
      //--------------------------------
      rleHistoBinsEnterZonesPtrA := fraStatsZonesA.fraHistogram.rleHistoBinsEnter;
      rleHistoBinsEnterZonesPtrB := fraStatsZonesB.fraHistogram.rleHistoBinsEnter;
      fraStatsZonesA.fraHistogram.rleHistoBinsEnterPtr :=  rleHistoBinsEnterZonesPtrB;
      fraStatsZonesB.fraHistogram.rleHistoBinsEnterPtr :=  rleHistoBinsEnterZonesPtrA;

      btnCancelClickZonesPtrA := fraStatsZonesA.fraHistogram.btnCancelClick;
      btnCancelClickZonesPtrB := fraStatsZonesB.fraHistogram.btnCancelClick;
      fraStatsZonesA.fraHistogram.btnCancelClickPtr := btnCancelClickZonesPtrB;
      fraStatsZonesB.fraHistogram.btnCancelClickPtr := btnCancelClickZonesPtrA;

      btnAcceptClickZonesPtrA := fraStatsZonesA.fraHistogram.btnAcceptClick;
      btnAcceptClickZonesPtrB := fraStatsZonesB.fraHistogram.btnAcceptClick;
      fraStatsZonesA.fraHistogram.btnAcceptClickPtr := btnAcceptClickZonesPtrB;
      fraStatsZonesB.fraHistogram.btnAcceptClickPtr := btnAcceptClickZonesPtrA;

      cbxClickZonesPtrA := fraStatsZonesB.changeCheck;
      cbxClickZonesPtrB := fraStatsZonesA.changeCheck;
      fraStatsZonesA.cbxClickPtr := cbxClickZonesPtrA;
      fraStatsZonesB.cbxClickPtr := cbxClickZonesPtrB;

      rdgConvergenceParamClickZonesPtrA := fraStatsZonesA.fraConvergence.rdgConvergenceParamClick;
      rdgConvergenceParamClickZonesPtrB := fraStatsZonesB.fraConvergence.rdgConvergenceParamClick;
      fraStatsZonesA.fraConvergence.rdgConvergenceParamClickPtr := rdgConvergenceParamClickZonesPtrB;
      fraStatsZonesB.fraConvergence.rdgConvergenceParamClickPtr := rdgConvergenceParamClickZonesPtrA;

      fraStatsZonesA.fraHistogram.entryPtr := @fraStatsZonesB.fraHistogram._entry;
      fraStatsZonesB.fraHistogram.entryPtr := @fraStatsZonesA.fraHistogram._entry;

      fraStatsZonesA.fraConvergence.rdgItemIndexPtr := @fraStatsZonesB.fraConvergence.rdgConvergenceParam.ItemIndex;
      fraStatsZonesB.fraConvergence.rdgItemIndexPtr := @fraStatsZonesA.fraConvergence.rdgConvergenceParam.ItemIndex;

      fraTablestgOutputStatsSelectCellZonesPtrA := fraStatsZonesA.fraTablestgOutputStatsSelectCell;
      fraTablestgOutputStatsSelectCellZonesPtrB := fraStatsZonesB.fraTablestgOutputStatsSelectCell;
      fraStatsZonesA.fraTablestgOutputStatsSelectCellPtr := fraTablestgOutputStatsSelectCellZonesPtrB;
      fraStatsZonesB.fraTablestgOutputStatsSelectCellPtr := fraTablestgOutputStatsSelectCellZonesPtrA;

      fraStatsZonesA.Splitter1TopPtr := @fraStatsZonesB.Splitter1.Top;
      fraStatsZonesB.Splitter1TopPtr := @fraStatsZonesA.Splitter1.Top;
      fraStatsZonesA.Splitter2TopPtr := @fraStatsZonesB.Splitter2.Top;
      fraStatsZonesB.Splitter2TopPtr := @fraStatsZonesA.Splitter2.Top;

      fraStatsZonesA.pnlTableHeightPtr := @fraStatsZonesB.pnlTable.Height;
      fraStatsZonesB.pnlTableHeightPtr := @fraStatsZonesA.pnlTable.Height;

      fraStatsZonesA.pnlTopSectionHeightPtr := @fraStatsZonesB.pnlTopSection.Height;
      fraStatsZonesB.pnlTopSectionHeightPtr := @fraStatsZonesA.pnlTopSection.Height;

      Splitter1MovedZonesPtrA := fraStatsZonesA.Splitter1Moved;
      Splitter1MovedZonesPtrB := fraStatsZonesB.Splitter1Moved;
      fraStatsZonesA.Splitter1MovedPtr := Splitter1MovedZonesPtrB;
      fraStatsZonesB.Splitter1MovedPtr := Splitter1MovedZonesPtrA;

      Splitter2MovedZonesPtrA := fraStatsZonesA.Splitter2Moved;
      Splitter2MovedZonesPtrB := fraStatsZonesB.Splitter2Moved;
      fraStatsZonesA.Splitter2MovedPtr := Splitter2MovedZonesPtrB;
      fraStatsZonesB.Splitter2MovedPtr := Splitter2MovedZonesPtrA;

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
          tabZoneOutputsB.Caption := tr( 'Zones' );
          pnlFileNameA.Caption := tr( 'FileNameA' );
          tabEpiOutputsA.Caption := tr( 'Epidemiology' );
          tabCostOutputsA.Caption := tr( 'Cost accounting' );
          tabZoneOutputsA.Caption := tr( 'Zones' );
        end
      ;

    end
  ;


  procedure TFormScenarioComparison.initialize();
    begin
      dbcout( 'Creating output list A...', debug );
      _superListA := TSMIterationOutputSuperList.create( _smdbA );
      _scenarioStatsA := TScenarioOutputSet.create( _smdbA, _smSimA.includeCostsGlobal, _smSimA.includeZonesGlobal );

      dbcout( 'Creating output list B...', debug );
      _superListB := TSMIterationOutputSuperList.create( _smdbB );
      _scenarioStatsB := TScenarioOutputSet.create( _smdbB, _smSimB.includeCostsGlobal, _smSimB.includeZonesGlobal );

      dbcout( 'Setting up frame A...', debug );
      fraStatsEpiA.setOutputType( StatsEpi );
			fraStatsEpiA.setSuperList( _superListA );
			fraStatsEpiA.setProdType( _selectedPT );
      fraStatsEpiA.resizeContents();

      dbcout( 'Setting up frame B...', debug );
      fraStatsEpiB.setOutputType( StatsEpi );
			fraStatsEpiB.setSuperList( _superListB );
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
          tabZoneOutputsA.Enabled := false;
        end
      else
        begin
          tabZoneOutputsA.enabled := true;
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
          tabZoneOutputsB.Enabled := false;
        end
      else
        begin
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

      //pgcOutputsChange( nil );
      //pgcOutputsChange( nil );
    end
  ;


  destructor TFormScenarioComparison.destroy();
    begin
      freeAndNil( _superListA );
      freeAndNil( _superListB );
      
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
      cboZones.Visible := ( pgcOutputsA.ActivePage = tabZoneOutputsA );
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
      cboZones.Visible := ( pgcOutputsA.ActivePage = tabZoneOutputsA );
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
            fraStatsZonesA.setZone( _selectedZone );

            if( nil = _selectedZone ) then
              fraStatsZonesB.setZone( nil )
            else
              fraStatsZonesB.setZone( _smSimB.zoneList.find( _selectedZone.id ) )
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
            fraStatsZonesA.setProdType( _selectedPT );

            if( nil = _selectedPT ) then
              fraStatsZonesB.setProdType( nil )
            else
              fraStatsZonesB.setProdType( _smSimB.ptList.findProdType( _selectedPT.productionTypeID ) )
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
    begin
      if( sender <> self.fraStatsEpiA.fraHistogram ) then
        self.fraStatsEpiA.fraHistogram.nHistogramBins := nBins
      ;
      if( sender <> self.fraStatsEpiB.fraHistogram ) then
        self.fraStatsEpiB.fraHistogram.nHistogramBins := nBins
      ;

      if( sender <> self.fraStatsCostA.fraHistogram ) then
        self.fraStatsCostA.fraHistogram.nHistogramBins := nBins
      ;
      if( sender <> self.fraStatsCostB.fraHistogram ) then
        self.fraStatsCostB.fraHistogram.nHistogramBins := nBins
      ;

      if( sender <> self.fraStatsZonesA.fraHistogram ) then
        self.fraStatsZonesA.fraHistogram.nHistogramBins := nBins
      ;
      if( sender <> self.fraStatsZonesB.fraHistogram ) then
        self.fraStatsZonesB.fraHistogram.nHistogramBins := nBins
      ;

      rescaleChartAxes();

      //self.fraStatsEpiA.fraHistogram.repaint();
      //self.fraStatsEpiB.fraHistogram.repaint();
    end
  ;


  procedure TFormScenarioComparison.rescaleChartAxes();
    begin
      rescaleChartAxes( fraStatsEpiA.fraHistogram.chtHistogram.series[0], fraStatsEpiB.fraHistogram.chtHistogram.series[0], true );
      rescaleChartAxes( fraStatsEpiA.fraConvergence.chtConvergence.Series[0], fraStatsEpiB.fraConvergence.chtConvergence.Series[0], false );

      if( pgcOutputsA.Pages[1].Enabled ) then
        begin
          rescaleChartAxes( fraStatsCostA.fraHistogram.chtHistogram.Series[0], fraStatsCostB.fraHistogram.chtHistogram.Series[0], true );
          rescaleChartAxes( fraStatsCostA.fraConvergence.chtConvergence.Series[0], fraStatsCostB.fraConvergence.chtConvergence.Series[0], false );
        end
      ;

      if( pgcOutputsB.Pages[2].Enabled ) then
        begin
          rescaleChartAxes( fraStatsZonesA.fraHistogram.chtHistogram.Series[0], fraStatsZonesB.fraHistogram.chtHistogram.Series[0], true );
          rescaleChartAxes( fraStatsZonesA.fraConvergence.chtConvergence.Series[0], fraStatsZonesB.fraConvergence.chtConvergence.Series[0], false );
        end
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



