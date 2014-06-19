unit FrameOutputStats;

(*
FrameOutputStats.pas/dfm
------------------------
Begin: 2005/08/01
Last revision: $Date: 2013-06-27 19:11:32 $ $Author: areeves $
Version number: $Revision: 1.36.4.10 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

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

		FormSMOutputBase,

    FrameOutputStatsTable,
    FrameChartBase,
    FrameArrayHistogram,
		FrameStringGridBase,

    HistogramData,

    SMDatabase,
    SMSimulationInput,
    ProductionType,
    Zone,
    
    IterationOutputs,
    OutputDescriptions
	;

  
  type TPtrBool = procedure( Sender: TObject; HistIsChecked: Boolean ) of object;

  type TPtrCellSelect = procedure( const c, r: integer ) of object;

  type TPtrSplitter = procedure( Sender: TObject; TopHeight: integer ) of object;

  type TProcPtr = procedure() of object;

	type TFrameOutputStats = class( TFrame )
      // See widget diagram, APHI programming log p. 3.15
      pnlWholeSection: TPanel;

      pnlTopSection: TPanel;
      Splitter1: TSplitter;

      pnlTable: TPanel;
      pnlTableHeader: TPanel;
      lblTable: TLabel;
      fraTable: TFrameOutputStatsTable;

      pnlHistogram: TPanel;
      pnlHistogramHeader: TPanel;
      cbxHistogram: TCheckBox;
      fraHistogram: TFrameArrayHistogram;

      procedure fraTablestgOutputStatsSelectCell( Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean );

      procedure fraTablestgOutputStatsMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );

      procedure cbxClick(Sender: TObject);
      procedure Splitter1Moved(Sender: TObject);
      procedure FrameResize(Sender: TObject);

      procedure fraTablestgGridKeyUp(
        Sender: TObject;
        var Key: Word;
        Shift: TShiftState
      );

		protected
      _arrIndex: integer;

      _ignoreClick: boolean;

      _showCharts: boolean;

      _creating: boolean;

      _statsType: TStatsType;

      _superEpiStats: TProductionTypeOutputSet;
      _superCostStats: TProductionTypeOutputSet;
      _superZonePTStats: TZonePTOutputSet;
      _superZoneStats: TZoneOutputSet;
      _selectedPT: TProductionType;
      _selectedZone: TZone;

      _resizeCalled: boolean;

      procedure translateUI();

      procedure populateChartsFromArray();
      procedure populateCharts();
      
      procedure hideCharts();
      procedure showCharts();

		public
      Splitter1TopPtr: ^integer;
      pnlTableHeightPtr: ^integer;
      pnlTopSectionHeightPtr: ^integer;
      Splitter1MovedPtr: TPtr;
      pnlTableResizePtr: TPtr;
      cbxClickPtr: TPtrBool;
      setAxisValuesPtr: TProcPtr;
      selectCellPtr: TPtrCellSelect;

			constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure changeCheck(Sender: TObject; HistIsChecked: Boolean);
			procedure resizeContents();

      procedure selectCell( const c, r: integer );

      procedure setOutputType( val: TStatsType );
			procedure setEpiStats( val: TProductionTypeOutputSet );
      procedure setCostStats( val: TProductionTypeOutputSet );
      procedure setZonePTStats( val: TZonePTOutputSet );
      procedure setZoneStats( val: TZoneOutputSet );
			procedure setProdType( pt: TProductionType );
      procedure setZone( z: TZone );
      procedure setDatabase( db: TSMDatabase );
      procedure setHistogramBinNumber( const breakType: THistBreakType; const nBins: integer );
		end
	;


implementation

{$R *.dfm}

  uses
    TypInfo,
    Printers,
    Grids,
    
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    MyGraphicsUtils,
    I88n,

    QVectors,

    StringConsts
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit.


  constructor TFrameOutputStats.create( AOwner: TComponent );
    var
      myRect: TGridRect;
    begin
      inherited create( AOwner );
      translateUI();

      // FIX ME: _creating and myRect aren't doing what I
      // thought they should.  It can probably be deleted.
      _creating := true;

      _selectedPT := nil;
      _selectedZone := nil;

      _superEpiStats := nil;
      _superCostStats := nil;
      _superZonePTStats := nil;
      _superZoneStats := nil;
      _ignoreClick := false;
      _resizeCalled := false;
      _arrIndex := 0;

      _statsType := StaNAADSMStateUnspecified;

      myRect.Top := -1;
      myRect.Bottom := -1;
      myRect.Left := -1;
      myRect.Right := -1;
      //fraTable.stgGrid.Selection := myRect;

      Splitter1MovedPtr := nil;
      pnlTableResizePtr := nil;
      cbxClickPtr := nil;
      selectCellPtr := nil;
      setAxisValuesPtr := nil;

      _creating := false;
    end
  ;


  procedure TFrameOutputStats.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameOutputStats.dfm
      // File date: Wed Apr 25 11:56:53 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblTable.Caption := tr( 'Output statistics' );
          pnlHistogram.Caption := tr( 'Select an output above to display this chart' );
          cbxHistogram.Caption := tr( 'Histogram' );
        end
      ;

    end
  ;


  procedure TFrameOutputStats.setOutputType( val: TStatsType );
    begin
      _statsType := val;
      fraTable.setOutputType( _statsType );
    end
  ;


	procedure TFrameOutputStats.setEpiStats( val: TProductionTypeOutputSet );
		begin
      _superEpiStats := val;
			fraTable.setEpiStats( val );
		end
	;


  procedure TFrameOutputStats.setZonePTStats( val: TZonePTOutputSet );
    begin
      _superZonePTStats := val;
      fraTable.setZonePTStats( val );
    end
  ;


  procedure TFrameOutputStats.setZoneStats( val: TZoneOutputSet );
    begin
      _superZoneStats := val;
      fraTable.setZoneStats( val );
    end
  ;


   procedure TFrameOutputStats.setCostStats( val: TProductionTypeOutputSet );
    begin
      _superCostStats := val;
      fraTable.setCostStats( val );
    end
  ;


	procedure TFrameOutputStats.setProdType( pt: TProductionType );
    var
      myParent: TWinControl;
		begin
      // Changing the parent temporarily prevents the
      // tabs from flickering when the contents of this frame
      // are updated.
      myParent := self.Parent;
      self.Parent := nil;

      _selectedPT := pt;
			fraTable.setProdType( pt );

			if( -1 < _arrIndex ) then
        populateChartsFromArray()
      ;

      self.Parent := myParent;
		end
	;


  procedure TFrameOutputStats.setZone( z: TZone );
    var
      myParent: TWinControl;
		begin
      // Changing the parent temporarily prevents the
      // tabs from flickering when the contents of this frame
      // are updated.
      myParent := self.Parent;
      self.Parent := nil;

      _selectedZone := z;
			fraTable.setZone( z );

			if( -1 < _arrIndex ) then
        populateChartsFromArray()
      ;

      self.Parent := myParent;
		end
	;


  procedure TFrameOutputStats.setDatabase( db: TSMDatabase );
    begin
      self.fraTable.setDatabase( db );
    end
  ;


  procedure TFrameOutputStats.setHistogramBinNumber( const breakType: THistBreakType; const nBins: integer );
    begin
      self.fraHistogram.setBinNumber( breakType, nBins );
    end
  ;


  destructor TFrameOutputStats.destroy();
    var
      temp: string;
    begin
      temp := self.name;

      inherited destroy();
    end
  ;


  procedure TFrameOutputStats.fraTablestgGridKeyUp(
        Sender: TObject;
        var Key: Word;
        Shift: TShiftState
      );
    var
      c, r: integer;
    begin
      inherited;

      // Make sure that the charts are updated for the new selection
      
      if
        ( ( VK_UP = key ) or ( VK_DOWN = key ) )
      and
        ( sender = self.fraTable.stgGrid )
      then
        begin
          // Figure out which cell should be selected.
          c := 1; // The column doesn't really matter.
          r := self.fraTable.stgGrid.Selection.Top;

          if( nil <> @selectCellPtr ) then
            selectCellPtr( c, r )
          ;
        end
      ;
    end
  ;


  procedure TFrameOutputStats.fraTablestgOutputStatsMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );
    var
      c, r: integer;
      unused: boolean;
    begin
      try
        try
          inherited;
        except
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (1)', true );
        end;

        // Show the appropriate hint.
        try
          fraTable.stgGridMouseDown(Sender, Button, Shift, X, Y);
        except
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (2)', true );
        end;

        try
          fraTable.stgGrid.MouseToCell( x, y, c, r );

          if
            ( 0 > c )
          or
            ( 0 > r )
          or
            ( fraTable.stgGrid.ColCount - 1 < c )
          or
            ( fraTable.stgGrid.RowCount - 1 < r )
          then
            exit
          ;
        except
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (3)', true );
        end;

        try
          fraTablestgOutputStatsSelectCell( self, c, r, unused );
        except
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (4)', true );
        end;

      except
        dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (5)', true );
      end;
    end
  ;


  procedure TFrameOutputStats.selectCell( const c, r: integer );
    var
      myRect: TGridRect;
    begin
      if( 0 < r ) then
        begin
          myRect.Left := fraTable.stgGrid.FixedCols;
          myRect.Top := r;
          myRect.Bottom := r;
          myRect.Right := fraTable.stgGrid.ColCount - 1;

          fraTable.stgGrid.Selection := myRect;

          fraTable.clearColumn( 0 );
          fraTable.stgGrid.Cells[0, r] := '>>';
          _arrIndex := r - fraTable.stgGrid.fixedRows;

          populateChartsFromArray();
        end
      ;
    end
  ;

  
  procedure TFrameOutputStats.fraTablestgOutputStatsSelectCell(
        Sender: TObject;
        ACol, ARow: Integer;
        var CanSelect: Boolean
      );
    begin
      if( nil <> @selectCellPtr ) then
        selectCellPtr( ACol, ARow )
      ;

      if( not _creating ) then
        selectCell( ACol, ARow )
      ;
      
      inherited;
    end
  ;


  procedure TFrameOutputStats.hideCharts();
    begin
      _showCharts := false;
      fraHistogram.Hide();
    end
  ;


  procedure TFrameOutputStats.showCharts();
    begin
      _showCharts := true;
      try
        if( cbxHistogram.Checked and not( fraHistogram.Visible ) ) then
          fraHistogram.show()
        ;
      except
        dbcout( 'EXCEPTION: Caught in TFrameOutputStats.showCharts()', true );
      end;
    end
  ;


  procedure TFrameOutputStats.populateChartsFromArray();
    begin
      if( ( nil = _superEpiStats) and ( StatsEpi = _statsType ) ) then
        begin
          dbcout( '*** _superEpiStats is nil', DBSHOWMSG );
          hideCharts();
          exit;
        end
      else if( ( nil = _superCostStats ) and ( StatsCosts = _statsType ) ) then
        begin
          dbcout( '*** _superCostStats is nil', DBSHOWMSG );
          hideCharts();
          exit;
        end
      else if( ( nil = _superZonePTStats ) and ( StatsPTZones = _statsType ) ) then
        begin
          dbcout( '*** _superZonePTStats is nil', DBSHOWMSG );
          hideCharts();
          exit;
        end
      else if( ( nil = _superZoneStats ) and ( StatsZones = _statsType ) ) then
        begin
          dbcout( '*** _superZoneStats is nil', DBSHOWMSG );
          hideCharts();
          exit;
        end
      else
        dbcout( '@@@ _superEpiStats or _superZoneStats is not nil', DBSHOWMSG )
      ;

      populateCharts();
    end
  ;


  procedure TFrameOutputStats.populateCharts();
    var
      results: TIterationOutputArray;
      ptID: integer;
      zoneID: integer;
      arr: TQDoubleVector;
      arrCreated: boolean;
      i: integer;
      v: TIterationOutputType;
    begin
      if( nil = _selectedPT ) then
        ptID := 0
      else
        ptID := _selectedPT.productionTypeID
      ;

      if( nil = _selectedZone ) then
        zoneID := 0
      else
        zoneID := _selectedZone.id
      ;

      dbcout( endl, DBSHOWMSG );
      dbcout( '--- populateCharts (' + self.Name + ')', DBSHOWMSG );
      dbcout( '*** showing charts for _arrIndex ' + intToStr( _arrIndex ), DBSHOWMSG );

      case _statsType of
        StatsEpi:
          begin
            v := TIterationOutputType( _arrIndex + TZoneOutputSet.zoneOutputCount() + TProductionTypeOutputSet.costsOutputCount() + TZonePTOutputSet.zonePTOutputCount() );
            results := _superEpiStats.resultSet( v, ptID );
          end
        ;
        StatsCosts:
          begin
            v := TIterationOutputType( _arrIndex + TZoneOutputSet.zoneOutputCount() + TZonePTOutputSet.zonePTOutputCount() );
            results := _superCostStats.resultSet( v, ptID );
          end
        ;
        StatsPTZones:
          begin
            v := TIterationOutputType( _arrIndex + TZoneOutputSet.zoneOutputCount() );
            results := _superZonePTStats.resultSet( v, ptID, zoneID );
          end
        ;
        StatsZones:
          begin
            v := TIterationOutputType( _arrIndex );
            results := _superZoneStats.resultSet( v, zoneID );
          end
        ;
        else
          raise exception.create( 'No Stats specified in TFrameOutputStats.populateCharts()' )
        ;
      end;

      if( v in [ EpiDetOccurred, EpiVaccOccurred, EpiDestrOccurred, EpiDiseaseEnded, EpiOutbreakEnded ] ) then
        begin
          hideCharts();
          exit;
        end
      ;

      if( ZZonesOccurred = v ) then
        begin
          hideCharts();
          exit;
        end
      ;

      if( ( 0 = zoneID ) and ( StatsZones = _statsType ) ) then
        begin
          hideCharts();
          exit;
        end
      ;

      if( nil = results ) then
        begin
          hideCharts();
          exit;
        end
      ;


      dbcout( 'Data count: ' + intToStr( results.dataCount ), DBSHOWMSG );
      dbcout( 'Is incomplete dataset: ' + uiBoolToText( results.isIncompleteDataset ), DBSHOWMSG );

      if( 0 < results.dataCount ) then
        begin
          if( results.isIncompleteDataset ) then
            begin
              arr := TQDoubleVector.create();
              arrCreated := true;

              // Remove missing values from the incomplete dataset
              for i := 0 to results.values.Count - 1 do
                if( -1 < results.values[i] ) then arr.append( results.values[i] )
              ;
            end
          else
            begin
              arr := results.values;
              arrCreated := false;
            end
          ;

          // use the array with missing values removed for the histogram
          fraHistogram.populate( arr );
          fraHistogram.setTitle( fraTable.stgGrid.Cells[ fraTable.nameColumn, _arrIndex + 1 ] );

          showCharts();
          if( arrCreated ) then arr.free();
        end
      else
        hideCharts()
      ;

      if( nil <> @setAxisValuesPtr ) then
        setAxisValuesPtr()
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFrameOutputStats.resizeContents();
    var
      factor: integer;
      visible: boolean;
    begin
      _resizeCalled := true;
      pnlTable.Height := pnlTable.Constraints.MinHeight;
      pnlTopSection.Height := 40;

      pnlWholeSection.Align := alNone;
      pnlWholeSection.Height := 80;

      pnlWholeSection.Height := self.Parent.ClientHeight;

      if( cbxHistogram.Checked ) then
        begin
          dbcout( 'Only histogram checked', DBSHOWMSG );
          Splitter1.Enabled := true;
          pnlTopSection.Height := pnlWholeSection.Height;

          if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;
          factor := pnlTopsection.Height div (fraTable.stgGrid.DefaultRowHeight
            + fraTable.stgGrid.GridLineWidth);
          if((factor mod 2) <> 0) then
            factor := factor + 1;
          ;
          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
          (factor) * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) div 2;
          if(not(visible)) then
            begin
              pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height
                + factor * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) div 2
                + 2
              ;
            end
          ;
          pnlHistogram.Constraints.MaxHeight := 0;
          pnlHistogram.Height := pnlTopSection.Height - pnlTable.Height;
        end
      else
        begin
          dbcout( 'No boxes checked', DBSHOWMSG );
          Splitter1.Enabled := false;

          pnlHistogram.Height := pnlHistogram.Constraints.MinHeight;
          pnlHistogram.Constraints.MaxHeight := pnlHistogram.Constraints.MinHeight;

          pnlTopSection.Height := pnlWholeSection.Height - pnlHistogram.Constraints.MinHeight;

          pnlTable.Height := pnlTopSection.Height;
        end
      ;
      pnlWholeSection.Align := alClient;
      _resizeCalled := false;
    end
  ;

  
  procedure TFrameOutputStats.Splitter1Moved(Sender: TObject);
    var
      factor: integer;
      visible: boolean;
    begin
      // If the charts are collapsed, don't let the user stretch them out.
      // Keep them collapsed.
      if( not( cbxHistogram.Checked ) ) then
        begin
          pnlTable.Height := pnlTopSection.Height - pnlHistogram.Height - 2;
          Splitter1.Enabled := false;
        end
      else
        Splitter1.Enabled := true
      ;

      if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
        visible := true
      else
        visible := false
      ;
      if(Sender = self.Splitter1) then
        begin
          factor := fraTable.stgGrid.Height div (fraTable.stgGrid.DefaultRowHeight +
            fraTable.stgGrid.GridLineWidth);

          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
          (fraTable.stgGrid.DefaultRowHeight + fraTable.stgGrid.GridLineWidth) * factor;

          if(not(visible)) then
            pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
             (fraTable.stgGrid.DefaultRowHeight + fraTable.stgGrid.GridLineWidth) * factor + 2;
          if(nil <> @Splitter1MovedPtr) then
            begin
              pnlTableHeightPtr^ := pnlTable.Height;
              Splitter1MovedPtr(Sender);
            end
          ;
        end
      ;
      pnlTable.Align := alTop;
      Splitter1.Align := alTop;
      fraTable.Align := alClient;
      pnlHistogram.Align := alClient;
    end
  ;


  procedure TFrameOutputStats.changeCheck( Sender: TObject; HistIsChecked: Boolean );
    begin
      _ignoreClick := true;
      if( HistIsChecked ) then
        begin
          cbxHistogram.Checked := true;
          fraHistogram.Visible := cbxHistogram.Checked and _showCharts;
          resizeContents();
        end
      else
        begin
          cbxHistogram.Checked := false;
          fraHistogram.Visible := cbxHistogram.Checked and _showCharts;
          //resizeContents();
        end
      ;
      _ignoreClick := false;
    end
  ;


  procedure TFrameOutputStats.cbxClick(Sender: TObject);
    begin
      if(_ignoreClick = false) then
        begin
          if( Sender = self.cbxHistogram ) then
            begin
              fraHistogram.Visible := cbxHistogram.Checked and _showCharts;

              if(nil <> @cbxClickPtr) then
                cbxClickPtr(Sender, cbxHistogram.Checked )
              ;
            end
          ;
        end
      ;

      resizeContents();
    end
  ;


  procedure TFrameOutputStats.FrameResize(Sender: TObject);
    begin
      if(_resizeCalled = false) then
        resizeContents()
      ;
    end
  ;


end.

//-----------------------------------------------------------------------------
