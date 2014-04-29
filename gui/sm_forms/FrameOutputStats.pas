unit FrameOutputStats;

(*
FrameOutputStats.pas/dfm
------------------------
Begin: 2005/08/01
Last revision: $Date: 2008/04/21 19:26:20 $ $Author: areeves $
Version number: $Revision: 1.32 $
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
    FrameArrayConvergence,
		FrameStringGridBase,

    SMDatabase,
    SMSimulationInput,
    ProductionType,
    Zone,
    
    SMSimulationStats,
    SMZoneStats,
    OutputDescriptions
	;

  
  type TPtrBool = procedure( Sender: TObject; HistIsChecked: Boolean; ConvIsChecked: Boolean ) of object;

  type TPtrGrid = procedure( Sender: TObject; X, Y: integer; var CanSelect: Boolean ) of object;

  type TPtrSplitter = procedure( Sender: TObject; TopHeight: integer ) of object;

  type TProcPtr = procedure() of object;

	type TFrameOutputStats = class( TFrame )
      // See widget diagram, APHI programming log p. 3.15
      pnlWholeSection: TPanel;
      Splitter2: TSplitter;

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

      pnlConvergence: TPanel;
      pnlConvergenceHeader: TPanel;
      cbxConvergence: TCheckBox;
      fraConvergence: TFrameArrayConvergence;

      procedure fraTablestgOutputStatsSelectCell( Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean );

      procedure fraTablestgOutputStatsMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );

      procedure cbxClick(Sender: TObject);
      procedure Splitter1Moved(Sender: TObject);
      procedure Splitter2Moved(Sender: TObject);
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

      _superList: TSMIterationOutputSuperList;
      _superCostStats: TProductionTypeOutputSet;
      _superZoneStats: TZoneOutputSet;
      _selectedPT: TProductionType;
      _selectedZone: TZone;

      _resizeCalled: boolean;

      procedure translateUI();

      procedure populateChartsFromArray( idx: integer );
      procedure populateCharts();
      
      procedure hideCharts();
      procedure showCharts();

		public
      Splitter1TopPtr: ^integer;
      Splitter2TopPtr: ^integer;
      pnlTableHeightPtr: ^integer;
      pnlTopSectionHeightPtr: ^integer;
      Splitter1MovedPtr: TPtr;
      Splitter2MovedPtr: TPtr;
      pnlTableResizePtr: TPtr;
      cbxClickPtr: TPtrBool;
      fraTablestgOutputStatsSelectCellPtr: TPtrGrid;

      setAxisValuesPtr: TProcPtr;

			constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure changeCheck(Sender: TObject; HistIsChecked: Boolean; ConvIsChecked: Boolean);
			procedure resizeContents();

      procedure setOutputType( val: TStatsType );
			procedure setSuperList( val: TSMIterationOutputSuperList );
      procedure setCostStats( val: TProductionTypeOutputSet );
      procedure setZoneStats( val: TZoneOutputSet );
			procedure setProdType( pt: TProductionType );
      procedure setZone( z: TZone );
		end
	;


implementation

{$R *.dfm}

  uses
    Printers,
    Grids,
    
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    AppLog,
    MyDialogs,
    MyGraphicsUtils,
    I88n,

    QVectors,

    StringConsts
  ;

  const
    DBFRAMEOUTPUTSTATS: boolean = false; // Set to true to enable debugging messages for this unit.


  constructor TFrameOutputStats.create( AOwner: TComponent );
    var
      myRect: TGridRect;
    begin
      addAppLog( 'Begin TFrameOutputStats.create...' );

      inherited create( AOwner );
      translateUI();

      //dbcout2(_scrollBarWidth);
      // FIX ME: _creating and myRect aren't doing what I
      // thought they should.  It can probably be deleted.
      _creating := true;

      _selectedPT := nil;
      _selectedZone := nil;

      _superList := nil;
      _superCostStats := nil;
      _superZoneStats := nil;
      _ignoreClick := false;
      _resizeCalled := false;
      _arrIndex := 0;

      _statsType := StatsUnspecified;

      myRect.Top := -1;
      myRect.Bottom := -1;
      myRect.Left := -1;
      myRect.Right := -1;
      //fraTable.stgGrid.Selection := myRect;

      Splitter1MovedPtr := nil;
      Splitter2MovedPtr := nil;
      pnlTableResizePtr := nil;
      cbxClickPtr := nil;
      fraTablestgOutputStatsSelectCellPtr := nil;

      setAxisValuesPtr := nil;

      _creating := false;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' created.' );
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
          pnlConvergence.Caption := tr( 'Select an output above to display this chart' );
          cbxConvergence.Caption := tr( 'Convergence' );
        end
      ;

    end
  ;


  procedure TFrameOutputStats.setOutputType( val: TStatsType );
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' setOutputType...' );

      _statsType := val;
      fraTable.setOutputType( _statsType );

      addAppLog( 'TFrameOutputStats ' + self.Name + ' setOutputType done.' );
    end
  ;


	procedure TFrameOutputStats.setSuperList( val: TSMIterationOutputSuperList );
		begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' setSuperList...' );

      _superList := val;
			fraTable.setSuperList( val );

      addAppLog( 'TFrameOutputStats ' + self.Name + ' setSuperList done.' );
		end
	;


  procedure TFrameOutputStats.setZoneStats( val: TZoneOutputSet );
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' setZoneStats...' );

      _superZoneStats := val;
      fraTable.setZoneStats( val );

      addAppLog( 'TFrameOutputStats ' + self.Name + ' setZoneStats done.' );
    end
  ;


   procedure TFrameOutputStats.setCostStats( val: TProductionTypeOutputSet );
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' setCostStats...' );

      _superCostStats := val;
      fraTable.setCostStats( val );

      addAppLog( 'TFrameOutputStats ' + self.Name + ' setCostStats done.' );
    end
  ;


	procedure TFrameOutputStats.setProdType( pt: TProductionType );
    var
      myParent: TWinControl;
		begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' setProdType...' );

      // Changing the parent temporarily prevents the
      // tabs from flickering when the contents of this frame
      // are updated.
      myParent := self.Parent;
      self.Parent := nil;

      _selectedPT := pt;
			fraTable.setProdType( pt );
      dbcout( '--- setProdType', DBFRAMEOUTPUTSTATS );
      dbcout( '*** showing charts for _arrIndex ' + intToStr( _arrIndex ), DBFRAMEOUTPUTSTATS );

			if( -1 < _arrIndex ) then populateChartsFromArray( _arrIndex );

      self.Parent := myParent;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' setProdType done.' );
		end
	;


  procedure TFrameOutputStats.setZone( z: TZone );
    var
      myParent: TWinControl;
		begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' setZone...' );

      // Changing the parent temporarily prevents the
      // tabs from flickering when the contents of this frame
      // are updated.
      myParent := self.Parent;
      self.Parent := nil;

      _selectedZone := z;
			fraTable.setZone( z );

			if( -1 < _arrIndex ) then populateChartsFromArray( _arrIndex );

      self.Parent := myParent;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' setZone done.' );
		end
	;



  destructor TFrameOutputStats.destroy();
    var
      temp: string;
    begin
      temp := self.name;

      addAppLog( 'TFrameOutputStats ' + temp + ' destroy...' );

      inherited destroy();

      addAppLog( 'TFrameOutputStats ' + temp + ' destroy done.' );
    end
  ;


  procedure TFrameOutputStats.fraTablestgGridKeyUp(
        Sender: TObject;
        var Key: Word;
        Shift: TShiftState
      );
    var
      c, r: integer;
      unusedDummy: boolean;
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' fraTablestgGridKeyUp...' );

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

          // Call the appropriate method.
          if(nil <> @fraTablestgOutputStatsSelectCellPtr) then
            begin
              self.fraTablestgOutputStatsSelectCellPtr(Sender, c, r, unusedDummy);
            end
          ;
        end
      ;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' fraTablestgGridKeyUp done.' );
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
(**)
      try
        addAppLog( 'TFrameOutputStats ' + self.Name + ' fraTablestgOutputStatsMouseDown...' );

        try
          inherited;
        except
          addAppLog( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (1)' );
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (1)', true );
        end;

        // Show the appropriate hint.
        try
          (**)fraTable.stgGridMouseDown(Sender, Button, Shift, X, Y);
        except
          addAppLog( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (2)' );
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (2)', true );
        end;

        try
          fraTable.stgGrid.MouseToCell( x, y, c, r );
          //dbcout2('Mouse down in cell: (' + intToStr( c ) + ', ' + intToStr( r ) + ')');
          //dbcout2('X: ' + intToStr(X) + ', Y: ' + intToStr(Y));
          //dbcout( 'Mouse down in cell: (' + intToStr( c ) + ', ' + intToStr( r ) + ')', DBFRAMEOUTPUTSTATS );


          if
            ( 0 > c )
          or
            ( 0 > r )
          or
            ( fraTable.stgGrid.ColCount - 1 < c )
          or
            ( fraTable.stgGrid.RowCount - 1 < r )
          then
            begin
              addAppLog( 'Leaving TFrameOutputStats ' + self.Name + ' early.' );
              exit;
            end
          ;
        except
          addAppLog( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (3)' );
        end;

        try
          //dbcout2(intToStr(c));
          fraTablestgOutputStatsSelectCell( self, c, r, unused );
        except
          addAppLog( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (4)' );
          dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (4)', true );
        end;

        addAppLog( 'TFrameOutputStats ' + self.Name + ' fraTablestgOutputStatsMouseDown done.' );
      except
        dbcout( 'EXCEPTION: caught in TFrameOutputStats.fraTablestgOutputStatsMouseDown() (5)', true );
      end;
(**)
    end
  ;

  
  procedure TFrameOutputStats.fraTablestgOutputStatsSelectCell(
        Sender: TObject;
        ACol, ARow: Integer;
        var CanSelect: Boolean
      );
    var
      myRect: TGridRect;
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' fraTablestgOutputStatsSelectCell...' );

      inherited;
      
      //dbcout2( '+++ Selecting cell!' );

      if(Sender = self) then
        if(nil <> @fraTablestgOutputStatsSelectCellPtr) then
          begin
            self.fraTablestgOutputStatsSelectCellPtr(Sender, ACol, ARow, CanSelect);
          end
        ;
      ;
      if( _creating ) then
        exit
      ;

      if(0 < ACol) then
        if( 0 < ARow ) then
          begin
            //dbcout2( 'Cell selected: (' + intToStr( ACol ) + ', ' + intToStr( ARow ) + ')', DBFRAMEOUTPUTSTATS );

            myRect.Left := fraTable.stgGrid.FixedCols;
            myRect.Top := ARow;
            myRect.Bottom := ARow;
            myRect.Right := fraTable.stgGrid.ColCount;
            fraTable.stgGrid.Selection := myRect;

            //dbcout2( 'Row ' + intToStr( ARow ) + ' selected', DBFRAMEOUTPUTSTATS );
            fraTable.clearColumn( 0 );
            fraTable.stgGrid.Cells[0, ARow] := '>>';

            //dbcout2( 'aRow is ' + intToStr( aRow ), DBFRAMEOUTPUTSTATS );
            populateChartsFromArray( ARow - fraTable.stgGrid.fixedRows );
          end
        ;
      ;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' fraTablestgOutputStatsSelectCell done.' );
    end
  ;


  procedure TFrameOutputStats.hideCharts();
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' hideCharts...' );

      _showCharts := false;
      fraHistogram.Hide();
      fraConvergence.Hide();

      addAppLog( 'TFrameOutputStats ' + self.Name + ' hideCharts done.' );
    end
  ;


  procedure TFrameOutputStats.showCharts();
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' showCharts...' );

      _showCharts := true;
      try
        if( cbxHistogram.Checked and not( fraHistogram.Visible ) ) then
          fraHistogram.show()
        ;
        if( cbxConvergence.Checked and not( fraConvergence.Visible ) ) then
          fraConvergence.Show()
        ;
      except
        dbcout( 'EXCEPTION: Caught in TFrameOutputStats.showCharts()', true );
      end;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' showCharts done.' );
    end
  ;


  procedure TFrameOutputStats.populateChartsFromArray( idx: integer );
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' populateChartsFromArray...' );

      if( ( nil = _superList) and ( StatsEpi = _statsType ) ) then
        begin
          dbcout( '*** _superList is nil', DBFRAMEOUTPUTSTATS );
          hideCharts();
          exit;
        end
      else if( ( nil = _superCostStats ) and ( StatsCosts = _statsType ) ) then
        begin
          dbcout( '*** _superCostStats is nil', DBFRAMEOUTPUTSTATS );
          hideCharts();
          exit;
        end
      else if( ( nil = _superZoneStats ) and ( StatsZones = _statsType ) ) then
        begin
          dbcout( '*** _superZoneStats is nil', DBFRAMEOUTPUTSTATS );
          hideCharts();
          exit;
        end
      else
        dbcout( '@@@ _superList or _superZoneStats is not nil', DBFRAMEOUTPUTSTATS )
      ;


      dbcout( 'idx: ' + intToStr( idx ), DBFRAMEOUTPUTSTATS );
      dbcout( '_arrIndex: ' + intToStr( _arrIndex ), DBFRAMEOUTPUTSTATS );


      case _statsType of
        StatsZones: _arrIndex := idx;
        StatsCosts: _arrIndex := idx + TZoneOutputSet.zoneOutputCount();
        StatsEpi: _arrIndex := idx; // FIX ME: this will change...
      end;

      dbcout( '_arrIndex: ' + intToStr( _arrIndex ), DBFRAMEOUTPUTSTATS );

      dbcout( 'populateChartsFromArray', DBFRAMEOUTPUTSTATS );
      dbcout( _arrIndex, DBFRAMEOUTPUTSTATS );

      populateCharts();

      addAppLog( 'TFrameOutputStats ' + self.Name + ' populateChartsFromArray done.' );
    end
  ;


  procedure TFrameOutputStats.populateCharts();
    var
      arraySet: TSMIterationOutputArraySet;
      results: TSimulationResultSet;
      ptID: integer;
      zoneID: integer;
      arr: TQDoubleVector;
      arrCreated: boolean;
      i: integer;
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' populateCharts...' );

      results := nil;

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

      dbcout2( 'TFrameOutputStats ' + self.Name + ' populateCharts...' );
      dbcout2( _arrIndex, DBFRAMEOUTPUTSTATS );


      case _statsType of
        StatsEpi:
          begin
            arraySet := _superList.arraysForProductionType( ptID );
            results := arraySet.resultSetAt( _arrIndex );
          end
        ;
        StatsCosts:
          results := _superCostStats.resultSet( TIterationOutputVariable( _arrIndex ), ptID )
        ;
        StatsZones:
          results := _superZoneStats.resultSet( TIterationOutputVariable( _arrIndex ), ptID, zoneID )
        ;
      end;

      if( nil = results ) then
        begin
          hideCharts();
          exit;
        end
      ;

      dbcout2( 'Data count: ' + intToStr( results.dataCount ), DBFRAMEOUTPUTSTATS );
      dbcout2( 'Is incomplete dataset: ' + uiBoolToText( results.isIncompleteDataset ), DBFRAMEOUTPUTSTATS );

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
          fraHistogram.populateFromArray( arr );
          fraHistogram.setTitle( fraTable.stgGrid.Cells[ fraTable.nameColumn, _arrIndex + 1 ] );

          // Use the raw data values for the convergence plot
          fraConvergence.populateFromArray( arr );
          fraConvergence.prependToTitle( fraTable.stgGrid.Cells[ fraTable.nameColumn, _arrIndex + 1 ] );
          showCharts();
          if( arrCreated ) then arr.free();
        end
      else
        hideCharts()
      ;

      if( nil <> @setAxisValuesPtr ) then
        setAxisValuesPtr()
      ;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' populateCharts done.' );
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
      addAppLog( 'TFrameOutputStats ' + self.Name + ' resizeContents...' );

      _resizeCalled := true;
      pnlTable.Height := pnlTable.Constraints.MinHeight;
      pnlTopSection.Height := 40;

      pnlWholeSection.Align := alNone;
      pnlWholeSection.Height := 80;

      pnlWholeSection.Height := self.Parent.ClientHeight;

      if( cbxHistogram.Checked and cbxConvergence.Checked ) then
        begin
          dbcout( 'Both boxes checked', DBFRAMEOUTPUTSTATS );
          Splitter1.Enabled := true;
          Splitter2.Enabled := true;
          if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;

          pnlTopSection.Height := 2 * pnlWholeSection.Height div 3;

          factor := pnlTopsection.Height div (fraTable.stgGrid.DefaultRowHeight
            + fraTable.stgGrid.GridLineWidth);
          if((factor mod 2) <> 0) then
            factor := factor + 1;
          ;
          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
            factor * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) div 2;
          if(not(visible)) then
             pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
            factor * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) div 2 +2;
          pnlHistogram.Constraints.MaxHeight := 0;
          //pnlHistogram.Height := pnlTopSection.Height - pnlTable.Height;
          //pnlConvergence.Height := pnlWholeSection.Height - pnlTopSection.Height;
          pnlConvergence.Constraints.MaxHeight := 0;
        end
      else if( cbxHistogram.Checked ) then
        begin

          dbcout( 'Only histogram checked', DBFRAMEOUTPUTSTATS );
          Splitter1.Enabled := true;
          Splitter2.Enabled := false;
          pnlTopSection.Height := pnlWholeSection.Height - pnlConvergence.Constraints.MinHeight - Splitter2.Height;

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
             pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
            factor * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) div 2 +2;
          pnlHistogram.Constraints.MaxHeight := 0;
          pnlHistogram.Height := pnlTopSection.Height - pnlTable.Height;
          pnlConvergence.Height := pnlConvergence.Constraints.MinHeight;
          pnlConvergence.Constraints.MaxHeight:= pnlConvergence.Constraints.MinHeight;

        end
      else if( cbxConvergence.Checked ) then
        begin

          dbcout( 'Only convergence checked', DBFRAMEOUTPUTSTATS );
          Splitter1.Enabled := false;
          Splitter2.Enabled := true;
          if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;
          pnlTopSection.Height := pnlWholeSection.Height div 2;
          factor := (pnlTopSection.Height - pnlHistogram.Constraints.MinHeight) div (fraTable.stgGrid.DefaultRowHeight
            + fraTable.stgGrid.GridLineWidth);
          if((factor mod 2) <> 0) then
            factor := factor + 1;
          ;
          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
          (factor) * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth);
          if(not(visible)) then
             pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
          (factor) * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) + 2;
          //pnlTable.Height := (pnlTopSection.Height - pnlHistogram.Constraints.MinHeight);

          pnlHistogram.Height := pnlHistogram.Constraints.MinHeight;
          pnlHistogram.Constraints.MaxHeight:= pnlHistogram.Constraints.MinHeight;

          pnlConvergence.Constraints.MaxHeight := 0;
          pnlConvergence.Height := pnlWholeSection.Height - pnlTopSection.Height;

        end
      else  // Neither box is checked
        begin
          dbcout( 'No boxes checked', DBFRAMEOUTPUTSTATS );
          Splitter1.Enabled := false;
          Splitter2.Enabled := false;
          if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;
          pnlHistogram.Height := pnlHistogram.Constraints.MinHeight;
          pnlHistogram.Constraints.MaxHeight := pnlHistogram.Constraints.MinHeight;

          pnlConvergence.Height := pnlConvergence.Constraints.MinHeight;
          pnlConvergence.Constraints.MaxHeight := pnlConvergence.Constraints.MinHeight;

          pnlTopSection.Height := pnlWholeSection.Height - pnlConvergence.Constraints.MinHeight {*-pnlHistogram.Constraints.MinHeight*}  - Splitter2.Height;

          factor := (pnlTopSection.Height - pnlHistogram.Constraints.MinHeight - Splitter1.Height) div (fraTable.stgGrid.DefaultRowHeight
            + fraTable.stgGrid.GridLineWidth);
          if((factor mod 2) <> 0) then
            factor := factor - 1;
          ;
          //dbcout2(factor);
          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
          (factor) * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth);
          if(not(visible)) then
            pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
              (factor) * (fraTable.stgGrid.DefaultRowHeight+ fraTable.stgGrid.GridLineWidth) + 2;
          //dbcout2(pnlConvergence.ClientHeight);
        end
      ;
      pnlWholeSection.Align := alClient;
      _resizeCalled := false;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' resizeContents done.' );
    end
  ;

  procedure TFrameOutputStats.Splitter1Moved(Sender: TObject);
    var
      factor: integer;
      visible: boolean;
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' Splitter1Moved...' );

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

      if( not( cbxConvergence.Checked ) ) then
        begin
          pnlTopSection.Height := pnlWholeSection.Height - pnlConvergence.Height - 2;
          Splitter2.Enabled := false;
        end
      else
        Splitter2.Enabled := true
      ;
      dbcout2('Splitter1Moved called.');
      if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
        visible := true
      else
        visible := false
      ;
      if(Sender = self.Splitter1) then
        begin
          factor := fraTable.stgGrid.Height div (fraTable.stgGrid.DefaultRowHeight +
            fraTable.stgGrid.GridLineWidth);
          dbcout2(factor);

          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height +
          (fraTable.stgGrid.DefaultRowHeight + fraTable.stgGrid.GridLineWidth) * factor;
          dbcout2(pnlTable.Height);
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
      pnlConvergence.Align := alClient;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' Splitter1Moved done.' );
    end
  ;


  procedure TFrameOutputStats.Splitter2Moved(Sender: TObject);
    var
      factor, diff: integer;
      visible: boolean;
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' Splitter2Moved...' );

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

      if( not( cbxConvergence.Checked ) ) then
      begin
        pnlTopSection.Height := pnlWholeSection.Height - pnlConvergence.Height - 2;
        Splitter2.Enabled := false;
      end
      else
        Splitter2.Enabled := true
      ;
      if (GetWindowlong(fraTable.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;
      dbcout2('Splitter2Moved called.');
      if(Sender = Splitter2) then
        begin

          dbcout2(pnlTopSection.Height);
          diff := pnlTopSection.Height - pnlTable.Height;
          factor := fraTable.stgGrid.Height div (fraTable.stgGrid.DefaultRowHeight +
            fraTable.stgGrid.GridLineWidth);
          pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height + (fraTable.stgGrid.DefaultRowHeight +
            fraTable.stgGrid.GridLineWidth) * factor;
          if(not(visible)) then
            pnlTable.Height := pnlTable.BevelWidth +  pnlTableHeader.BevelWidth * 2 + pnlTableHeader.Height + (fraTable.stgGrid.DefaultRowHeight +
              fraTable.stgGrid.GridLineWidth) * factor + 2;
          pnlTopSection.Height := pnlTable.Height + diff;
              dbcout2(factor);
              dbcout2(pnlTable.Height);
          if(nil <> @Splitter2MovedPtr) then
            begin
              pnlTopSectionHeightPtr^ := pnlTopSection.Height;
              pnltableHeightPtr^ := pnlTable.Height;
              Splitter2MovedPtr(Sender);
            end
          ;
        end
      ;
      pnlTable.Align := alTop;
      Splitter2.Align := alTop;
      fraTable.Align := alClient;
      //pnlTopSection.Align := alTop;
      pnlHistogram.Align := alClient;
      pnlConvergence.Align := alClient;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' Splitter2Moved done.' );
    end
  ;


  procedure TFrameOutputStats.changeCheck(Sender: TObject; HistIsChecked: Boolean; ConvIsChecked: Boolean);
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' changeCheck...' );

      _ignoreClick := true;
      if((HistIsChecked = true) and (ConvIsChecked = true)) then
        begin
          cbxHistogram.Checked := true;
          fraHistogram.Visible := cbxHistogram.Checked and _showCharts;
          cbxConvergence.Checked := true;
          fraConvergence.Visible := cbxConvergence.Checked and _showCharts;
          resizeContents();
        end
      else if((HistIsChecked = true) and (ConvIsChecked = false)) then
        begin
          cbxHistogram.Checked := true;
          fraHistogram.Visible := cbxHistogram.Checked and _showCharts;
          cbxConvergence.Checked := false;
          fraConvergence.Visible := cbxConvergence.Checked and _showCharts;
          resizeContents();
        end
      else if((ConvIsChecked = true) and (HistIsChecked = false)) then
        begin
          cbxConvergence.Checked := true;
          fraConvergence.Visible := cbxConvergence.Checked and _showCharts;
          cbxHistogram.Checked := false;
          fraHistogram.Visible := cbxHistogram.Checked and _showCharts;
          resizeContents();
        end
      else if((HistIsChecked = false) and (ConvIsChecked = false)) then
        begin
          cbxHistogram.Checked := false;
          fraHistogram.Visible := cbxHistogram.Checked and _showCharts;
          cbxConvergence.Checked := false;
          fraConvergence.Visible := cbxConvergence.Checked and _showCharts;
          //resizeContents();
        end
      ;
      _ignoreClick := false;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' changeCheck done.' );
    end
  ;


  procedure TFrameOutputStats.cbxClick(Sender: TObject);
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' cbxClick...' );

      if(_ignoreClick = false) then
        begin
          if((Sender = self.cbxHistogram) or (Sender = self.cbxConvergence)) then
            begin
              fraHistogram.Visible := cbxHistogram.Checked and _showCharts;

              fraConvergence.Visible := cbxConvergence.Checked and _showCharts;
              if(nil <> @cbxClickPtr) then
                begin
                  cbxClickPtr(Sender, cbxHistogram.Checked, cbxConvergence.Checked);
                end
              ;
            end
          ;
        end
      ;
    //dbcout2('resizeContents called from cbxClick.');
    resizeContents();

      addAppLog( 'TFrameOutputStats ' + self.Name + ' cbxClick done.' );
    end
  ;


  procedure TFrameOutputStats.FrameResize(Sender: TObject);
    begin
      addAppLog( 'TFrameOutputStats ' + self.Name + ' FrameResize...' );

      if(_resizeCalled = false) then
        resizeContents()
      ;

      addAppLog( 'TFrameOutputStats ' + self.Name + ' FrameResize done.' );
    end
  ;


end.

//-----------------------------------------------------------------------------
