unit FormOutputExposures;

(*
FormOutputExposures.pas/dfm
---------------------------
Begin: 2005/02/01
Last revision: $Date: 2013-06-27 19:11:27 $ $Author: areeves $
Version: $Revision: 1.30.4.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2006 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


(*
THINGS TO DO:
-------------

  - Data entry validation for filter RLEs (in accept button click event).

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
    ToolWin,
    ActnMan,
    ActnCtrls,
    ActnMenus,
    StdCtrls,
    Buttons,
    ExtCtrls,
    Grids,


    // General purpose units
    SqlClasses,

    // General purpose widgets
    ARSortGrid,
    FrameAcceptCancel,
    REEdit,

    // Application-specific data structures
    SMSimulationInput,
    SMDatabase,
    EventsAndExposures,
    ProductionType,
    ProductionTypeList,
    
    // Application-specific widgets
    FormSMOutputBase // the base class!
  ;


  type TFormOutputExposures = class(TFormSMOutputBase)
      spacerPanel: TPanel;
      lblDiseaseState: TLabel;
      cboDiseaseState: TComboBox;
      pnlSortControls: TPanel;
      lblMainSort: TLabel;
      lblSortOrder: TLabel;
      cboMainSort: TComboBox;
      rdoAscending: TRadioButton;
      rdoDescending: TRadioButton;

      lblSourcePT: TLabel;
      lblRecipientPT: TLabel;
      cboRecipientProdTypes: TComboBox;
      pnlCaption: TPanel;
      pnlFilterControls: TPanel;
      lblMainFilter: TLabel;
      lblTextEntry: TLabel;
      lblExposureType: TLabel;
      cboMainFilter: TComboBox;
      cboExposureType: TComboBox;
      rleTextEntry: TREEdit;
      fraAcceptCancel: TFrameAcceptCancel;
      pnlEventCounter: TPanel;
      stgGrid: TARSortGrid;
      pnlExposureSuccess: TPanel;
      rdoSuccess: TRadioButton;
      rdoUnsuccess: TRadioButton;

      procedure cboRecipientProdTypesCloseUp(Sender: TObject);
      procedure stgGridEndSort(Sender: TObject; Col: Integer);
      procedure sortControlChange(Sender: TObject);

      procedure stgGridGetCellFormat(Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
      procedure stgGridBeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
      procedure cboMainFilterChange(Sender: TObject);
      procedure rleTextEntryEnter(Sender: TObject);
      procedure rleTextEntryExit(Sender: TObject);
      procedure rleTextEntryKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure cboExposuresChange(Sender: TObject);
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);
      procedure rdoClick(Sender: TObject);

      procedure FormResize(Sender: TObject);

    protected
      _firstRow: boolean;
      _presorting: boolean;
      _filtering: boolean;
      _creating: boolean;
      _sortInProgress: boolean;
      _lastColumnSorted: integer;

      _recipientPT: TProductionType;

      _ScrollBarVisibleCheck: boolean;

      procedure translateUI();
      procedure translateUIManual();

      procedure resetGrid();

      procedure filter();

      procedure updateExposureCounter();

	    procedure setupRecipientProdTypeComboBox();

      { Handles form updates from the database when a simulation is not running or when _selectedPT is changed }
      procedure setupFromDatabase();
      procedure simChanged(); override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

      { Updates the contents of the form when the selected iteration changes }
      procedure iterationChanged(); override;

      { Used to enable/disable controls on the form when needed }
      procedure setControlsEnabled( val: boolean ); override;

      { Used to create a list of all grids containing text that might be saved/copied/printed }
      procedure fillStringGridDict(); override;

      procedure fillOutForm();

      procedure setCaption();

    public
      constructor create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase ); reintroduce;
      destructor destroy(); override;

      procedure resetIteration( const iteration: Integer );

      procedure updateSimComplete(); override;

      { Experimental feature: not currently used }
      //procedure appendExposure( exp: TSMExposure );
    end
  ;

  
  var
    frmOutputExposures: TFormOutputExposures;

  const
    DBFORMOUTPUTEXPOSURES: boolean = false; // Set to true to enable debugging messages for this unit.

implementation

  {$R *.dfm}

  uses
    MyStrUtils,
    DebugWindow,
    RegExpDefs,
    I88n,

    ControlUtils,

    StatusEnums,
    NAADSMLibraryTypes,
    FormMain
  ;


  const
    COL_DAY: integer = 0;
    COL_EXPOSURE: integer = 1;
    COL_EXPOSURE_CODE: integer = 2;
    COL_SOURCE_HERDID: integer = 3;
    COL_SOURCE_TYPE_DESCR: integer = 4;
    COL_SOURCE_STATUS: integer = 5;
    COL_SOURCE_ZONE: integer = 6;
    COL_EXPOSURE_IS_ADEQUATE: integer = 7;
    COL_RECIPENT_HERDID: integer = 8;
    COL_RECIPIENT_TYPE_DESCR: integer = 9;
    COL_RECIPIENT_STATUS: integer = 10;
    COL_RECIPIENT_ZONE: integer = 11;
    COL_INITIATED_DAY: integer = 12;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormOutputExposures.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
    var
      frm: TForm;
		begin
      inherited create( AOwner );
      translateUI();

      _ScrollBarVisibleCheck := false;
      _creating := true;

      _sortInProgress := false;
      _filtering := false;

      _lastColumnSorted := 0;

      // Set up widgets
      //---------------
      lblExposureType.Top := lblTextEntry.Top;
      lblExposureType.Left := lblTextEntry.Left;

      lblDiseaseState.Top := lblTextEntry.Top;
      lblDiseaseState.Left := lblTextEntry.Left;

      cboExposureType.Top := rleTextEntry.Top;
      cboExposureType.Left := rleTextEntry.Left;

      pnlExposureSuccess.Top := lblTextEntry.Top - 4;
      pnlExposureSuccess.Left := lblTextEntry.Left;

      cboDiseaseState.Top := rleTextEntry.Top;
      cboDiseaseState.Left := rleTextEntry.Left;

      pnlEventCounter.Caption := '';
      pnlCaption.Caption := '';

      setChartControlsEnabled( false );

      // Initialize pointers
      //--------------------
      _smSim := sim;
      _smdb := db;

      _selectedPT := nil;
      _recipientPT := nil;


      // Size the form
      //--------------
      if( AOwner is TForm ) then
        begin
          frm := AOwner as TForm;
          self.Height := round( frm.ClientHeight * 0.75 );
        end
      else
        begin
          self.Height := 650;
        end
      ;

      // Set up the grid and show the data
      //-----------------------------------
      setupIterationComboBox();
      setupProdTypeComboBox();
      setupRecipientProdTypeComboBox();

      productionTypeChanged();

      cboMainFilter.ItemIndex := 0;

      _creating := false;
		end
	;


  procedure TFormOutputExposures.translateUI();
    begin
      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Exposures and traces for 1 iteration' );
          lblSourcePT.Caption := tr( 'S/O type:' );
          lblRecipientPT.Caption := tr( 'R/I type:' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
          lblMainFilter.Caption := tr( 'Filter by:' );
          lblTextEntry.Caption := tr( 'Day:' );
          lblExposureType.Caption := tr( 'Event type:' );
          lblDiseaseState.Caption := tr( 'Disease state:' );
          cboMainFilter.Text := tr( '(No filter)' );
          rdoSuccess.Caption := tr( 'Adequate/successful' );
          rdoUnsuccess.Caption := tr( 'Inadequate/unsuccessful' );
          pnlEventCounter.Caption := tr( 'pnlEventCounter' );
          lblMainSort.Caption := tr( 'Sort by:' );
          lblSortOrder.Caption := tr( 'Sort order:' );
          cboMainSort.Text := tr( 'Day' );
          rdoAscending.Caption := tr( 'Ascending' );
          rdoDescending.Caption := tr( 'Descending' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          cboMainFilter.Items[0] := tr( '(No filter)' );
          cboMainFilter.Items[1] := tr( 'Day' );
          cboMainFilter.Items[2] := tr( 'Event type' );
          cboMainFilter.Items[3] := tr( 'S/O unit ID' );
          cboMainFilter.Items[4] := tr( 'S/O status' );
          cboMainFilter.Items[5] := tr( 'S/O zone' );
          cboMainFilter.Items[6] := tr( 'Is adequate/successful' );
          cboMainFilter.Items[7] := tr( 'R/I unit ID' );
          cboMainFilter.Items[8] := tr( 'R/I status' );
          cboMainFilter.Items[9] := tr( 'R/I zone' );
          cboMainFilter.Items[10] := tr( 'Day of initiation' );

          cboExposureType.Items[0] := tr( 'Direct contact' );
          cboExposureType.Items[1] := tr( 'Indirect contact' );
          cboExposureType.Items[2] := tr( 'Airborne exposure' );
          cboExposureType.Items[3] := tr( 'Trace forward, direct' );
          cboExposureType.Items[4] := tr( 'Trace forward, indirect' );
          cboExposureType.Items[5] := tr( 'Trace back, direct' );
          cboExposureType.Items[6] := tr( 'Trace back, indirect' );

          //cboDiseaseState is taken care of in the manual translation function below.

          cboMainSort.Items[0] := tr( 'Day' );
          cboMainSort.Items[1] := tr( 'Event on day' );
          cboMainSort.Items[3] := tr( 'Event type' );
          cboMainSort.Items[4] := tr( 'S/O unit ID' );
          cboMainSort.Items[5] := tr( 'S/O type' );
          cboMainSort.Items[6] := tr( 'S/O status' );
          cboMainSort.Items[7] := tr( 'S/O zone' );
          cboMainSort.Items[7] := tr( 'Is adequate/successful' );
          cboMainSort.Items[8] := tr( 'R/I unit ID' );
          cboMainSort.Items[9] := tr( 'R/I type' );
          cboMainSort.Items[10] := tr( 'R/I status' );
          cboMainSort.Items[11] := tr( 'R/I zone' );
          cboMainSort.Items[12] := tr( 'Day of initiation' );
        end
      ;

      translateUIManual();
    end
  ;


  procedure TFormOutputExposures.translateUIManual();
    var
      j: TNAADSMDiseaseState;
    begin
      cboDiseaseState.Items.Clear();
      for j := naadsmFirstDiseaseState() to naadsmLastDiseaseState() do
        cboDiseaseState.Items.Append( naadsmDiseaseStateStr( j ) )
      ;
    end
  ;


	destructor TFormOutputExposures.destroy();
		begin
      inherited destroy();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Initialization functions
//-----------------------------------------------------------------------------
  procedure TFormOutputExposures.fillStringGridDict();
    begin
      _stringGridDict['Events'] := stgGrid;
    end
  ;


	procedure TFormOutputExposures.setupRecipientProdTypeComboBox();
  	var
    	it: TProductionTypeListIterator;
  	begin
      cboRecipientProdTypes.Clear();

      if( assigned( _smSim ) ) then
      	begin
        	cboRecipientProdTypes.AddItem( tr( 'All production types' ), nil );
          it := TProductionTypeListIterator.create( _smSim.ptList );
          it.toFirst();

          while( nil <> it.current() ) do
            begin
              cboRecipientProdTypes.addItem( it.current().productionTypeDescr, it.current() );
              it.incr();
            end
          ;

          it.free();
          
          cboRecipientProdTypes.ItemIndex := 0;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  {*
    This function handles form updates from the database: either when
    a simulation is not running, or when the selected production type is
    changed in midstream.  Function updateForDay() is used for dynamic
    updates.
  }
  procedure TFormOutputExposures.setupFromDatabase();
    begin
      resetGrid();

      stgGrid.SearchOptions.Filtered := false;

      // If a simulation is running, then disable the iteration combo box.
      // Otherwise, populate it from the database.
      if( frmMain.simIsRunning ) then
        disableIterationComboBox()
      else
        begin
          setupIterationComboBox();
          fillOutForm();
        end
      ;

      updateExposureCounter();
    end
  ;


  procedure TFormOutputExposures.simChanged();
    begin
      _recipientPT := nil;
      
      setupIterationComboBox();
      setupProdTypeComboBox();
      setupRecipientProdTypeComboBox();

      productionTypeChanged();
    end
  ;

  
  procedure TFormOutputExposures.productionTypeChanged();
    begin
      try
        screen.Cursor := crHourGlass;
        setControlsEnabled( false );

        setUpFromDatabase();
        setCaption();
      finally
        setControlsEnabled( true );
        screen.Cursor := crDefault;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFormOutputExposures.resetGrid();
    begin
      stgGrid.Clear();

      _firstRow := true;

      stgGrid.RowCount := 2;
      stgGrid.ColCount := 13;

      stgGrid.Cells[ COL_DAY, 0 ] := tr( 'Day' );
      stgGrid.Cells[ COL_EXPOSURE, 0 ] := tr( 'Event on day' );
      stgGrid.Cells[ COL_SOURCE_HERDID, 0 ] := tr( 'S/O unit ID' );
      stgGrid.Cells[ COL_SOURCE_TYPE_DESCR, 0 ] := tr( 'S/O type' );
      stgGrid.Cells[ COL_SOURCE_STATUS, 0 ] := tr( 'S/O status' );
      stgGrid.Cells[ COL_SOURCE_ZONE, 0 ] := tr( 'S/O zone' );
      stgGrid.Cells[ COL_EXPOSURE_CODE, 0 ] := tr( 'Event type' );
      stgGrid.Cells[ COL_EXPOSURE_IS_ADEQUATE, 0 ] := tr( 'Is adequate/successful' );
      stgGrid.Cells[ COL_RECIPENT_HERDID, 0 ] := tr( 'R/I unit ID' );
      stgGrid.Cells[ COL_RECIPIENT_TYPE_DESCR, 0 ] := tr( 'R/I type' );
      stgGrid.Cells[ COL_RECIPIENT_STATUS, 0 ] := tr( 'R/I status' );
      stgGrid.Cells[ COL_RECIPIENT_ZONE, 0 ] := tr( 'R/I zone' );
      stgGrid.cells[ COL_INITIATED_DAY, 0 ] := tr( 'Day of initiation' );

      stgGrid.AutoSizeColumns();

      stgGrid.SortByColumn( COL_DAY );

      repaint();
    end
  ;


  procedure TFormOutputExposures.setControlsEnabled( val: boolean );
    begin
      inherited setControlsEnabled( val );

      cboRecipientProdTypes.Enabled := val;

      setChildrenEnabled( pnlFilterControls, val, false );
      setChildrenEnabled( pnlSortControls, val, false );
      
      setDataControlsEnabled( val );
      setChartControlsEnabled( false );
    end
  ;


  procedure TFormOutputExposures.updateExposureCounter();
    var
      str: string;
    begin
      if( 2 = stgGrid.RowCount ) then
        begin
          if( 0 = length( trim( stgGrid.Cells[ 0, 1 ] ) ) ) then
            str := '0 ' + tr( 'events' )
          else
            str := '1 ' + tr( 'events' )
          ;
        end
      else
        str := intToStr( stgGrid.RowCount - 1 ) + ' ' + tr( 'events' )
      ;

      if( stgGrid.SearchOptions.Filtered ) then
        str := str + ' ' + tr( '(filtered)' )
      ;

      pnlEventCounter.Caption := str;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Grid handling functions
//-----------------------------------------------------------------------------
  procedure TFormOutputExposures.stgGridGetCellFormat(Sender: TObject; Col,
    Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
    begin
      if( 0 = Row ) then FormatOptions.Font.Style := [fsbold];
    end
  ;


  { Ensures that sorting happens first by day, then by event, then by whatever other criterion is selected. }
  procedure TFormOutputExposures.stgGridBeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
    var
      mainSortDirection: TSortDirection;
    begin
      dbcout( '--- TFormOutputExposures.stgGridBeginSort...', DBFORMOUTPUTEXPOSURES );
      screen.cursor := crHourglass;

      setControlsEnabled( false );

      //LockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      _sortInProgress := true;

      if( not( _presorting ) ) then
        begin
          _presorting := true;

          mainSortDirection := stgGrid.SortDirection;

          stgGrid.SortDirection := sdAscending;

          if
            ( COL_EXPOSURE <> Col )
          and
            ( COL_DAY <> col )
          then
            begin
              stgGrid.SortByColumn( COL_EXPOSURE );
              stgGrid.SortByColumn( COL_DAY );
            end
          else if( COL_DAY = col ) then
            stgGrid.SortByColumn( COL_EXPOSURE )
          else if( COL_EXPOSURE = col ) then
            stgGrid.SortByColumn( COL_DAY )
          ;

          stgGrid.SortDirection := mainSortDirection;

          _presorting := false;
        end
      ;

      dbcout( '--- Done with TFormOutputExposures.stgGridBeginSort', DBFORMOUTPUTEXPOSURES );
    end
  ;

  
  procedure TFormOutputExposures.stgGridEndSort( Sender: TObject; Col: Integer );
    begin
      dbcout( '--- TFormOutputExposures.stgGridEndSort...', DBFORMOUTPUTEXPOSURES );

      if( not _presorting ) then
        begin
          cboMainSort.ItemIndex := col;

          if( sdAscending = stgGrid.SortDirection ) then
            rdoAscending.Checked := true
          else
            rdoDescending.Checked := true
          ;

          screen.Cursor := crDefault;

          _sortInProgress := false;

          setControlsEnabled( true );

          //LockWindowUpdate( 0 );
          self.Perform( WM_SETREDRAW, 1, 0 );
          RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
        end
      ;

      dbcout( '--------- Done with TFormOutputExposures.stgGridEndSort', DBFORMOUTPUTEXPOSURES );
    end
  ;


  procedure TFormOutputExposures.sortControlChange(Sender: TObject);
    begin
      if( not( _sortInProgress ) ) then
        begin
          if( rdoAscending.Checked ) then
            stgGrid.SortDirection := sdAscending
          else
            stgGrid.SortDirection := sdDescending
          ;

          stgGrid.sortByColumn( cboMainSort.ItemIndex );
          _lastColumnSorted := cboMainSort.ItemIndex;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Filtering functions
//-----------------------------------------------------------------------------
  procedure TFormOutputExposures.filter();
    var
      s: string;
    begin
      dbcout( '--- TFormOutputExposures.filter...', DBFORMOUTPUTEXPOSURES );

      //lockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      s := cboMainFilter.Items[ cboMainFilter.ItemIndex ];

      try
        stgGrid.SearchOptions.Filtered := false;

        // Using strings here makes life easier when options are added or changed.
        if( tr( '(No filter)' ) = s ) then
          begin
            _filtering := false;
            stgGrid.SearchOptions.Filtered := false;
          end
        else if( tr( 'Day' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_DAY;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end
        else if( tr( 'S/O unit ID' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_SOURCE_HERDID;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end

        // Filtering by source type is taken care of by a different mechanism

        else if( tr( 'S/O status' ) = s ) then
          begin
            if( -1 = cboDiseaseState.ItemIndex ) then
              stgGrid.SearchOptions.Filtered := false
            else
              begin
                dbcout( 'Filtering S/O status by ' + cboDiseaseState.Items.Strings[cboDiseaseState.ItemIndex], DBFORMOUTPUTEXPOSURES );
                stgGrid.SearchOptions.SearchCol := COL_SOURCE_STATUS;
                stgGrid.SearchOptions.SearchText := cboDiseaseState.Items.Strings[cboDiseaseState.ItemIndex];
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            ;
          end
        else if( tr( 'S/O zone' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_SOURCE_ZONE;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end
        else if( tr( 'Event type' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_EXPOSURE_CODE;

            case cboExposureType.ItemIndex of
              0: // Direct contact
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Direct contact' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              1: // Indirect contact
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Indirect contact' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              2: // Airborne spread
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Airborne exposure' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              3: // Trace forward, direct
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Trace forward, direct' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              4: // Trace forward, indirect
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Trace forward, indirect' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              5: // Trace back, direct
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Trace back, direct' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              6: // Trace back, indirect
                begin
                  stgGrid.SearchOptions.SearchText := tr( 'Trace back, indirect' );
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
              else // cancel the filtering
                stgGrid.SearchOptions.Filtered := false
              ;
            end;
          end
        else if( tr( 'Is adequate/successful' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_EXPOSURE_IS_ADEQUATE;

            if( rdoSuccess.checked ) then
              begin
                stgGrid.SearchOptions.SearchText := tr( 'true' );
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            else if( rdoUnsuccess.checked ) then
              begin
                stgGrid.SearchOptions.SearchText := tr( 'false' );
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            else // cancel the filtering
              stgGrid.SearchOptions.Filtered := false
            ;
          end
        else if( tr( 'R/I unit ID' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_RECIPENT_HERDID;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end

        // Filtering by recipient type is taken care of by a different mechanism

        else if( tr( 'R/I status' ) = s ) then
          begin
            if( -1 = cboDiseaseState.ItemIndex ) then
              stgGrid.SearchOptions.Filtered := false
            else
              begin
                dbcout( 'Filtering R/I status by ' + cboDiseaseState.Items.Strings[cboDiseaseState.ItemIndex], DBFORMOUTPUTEXPOSURES );
                stgGrid.SearchOptions.SearchCol := COL_RECIPIENT_STATUS;
                stgGrid.SearchOptions.SearchText := cboDiseaseState.Items.Strings[cboDiseaseState.ItemIndex];
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            ;
          end
        else if( tr( 'R/I zone' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_RECIPIENT_ZONE;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end
        else if( tr( 'Day of initiation' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_INITIATED_DAY;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end
        else
          raise exception.create( 'Unhandled string (' + s + ') in TFormOutputExposures.filter()' )
        ;

        //lockWindowUpdate( 0 );
        self.Perform( WM_SETREDRAW, 1, 0 );
        RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );

        updateExposureCounter();
      except
        // fail silently
        dbcout( '+++ Exception occurred and was handled in TFormOutputExposures.filter.', DBFORMOUTPUTEXPOSURES );
      end;

      dbcout( '--- Done with TFormOutputExposures.filter', DBFORMOUTPUTEXPOSURES );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormOutputExposures.cboMainFilterChange(Sender: TObject);
    var
      s: string;
    begin
      s := cboMainFilter.Items[ cboMainFilter.ItemIndex ];

      // Using strings here makes life easier when options are added or changed.
      if( tr( '(No filter)' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := false;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          fraAcceptCancel.Visible := false;

          _filtering := false;

          filter();
        end
      else if( tr( 'Day' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'Day:' );
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := true;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := RE_SIGNED_INTEGER_INPUT;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          _filtering := true;

          rleTextEntry.SetFocus();
        end
      else if( tr( 'S/O unit ID' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'Source unit ID:' );
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.visible := true;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := RE_INTEGER_INPUT;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          _filtering := true;

          rleTextEntry.SetFocus();
        end

      // Filtering by source type is taken care of by a different mechanism

      else if( tr( 'S/O status' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := true;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := false;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := true;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      else if( tr( 'S/O zone' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'S/O zone:' );
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := true;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := '';
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          _filtering := true;

          rleTextEntry.SetFocus();
        end
      else if( tr( 'Event type' ) = s ) then
        begin
          lblExposureType.Visible := true;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := true;
          rleTextEntry.Visible := false;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      else if( tr( 'Is adequate/successful' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := false;
          pnlExposureSuccess.Visible := true;
          cboDiseaseState.Visible := false;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      else if( tr( 'R/I unit ID' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'R/I unit ID:' );
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := true;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := RE_INTEGER_INPUT;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          _filtering := true;

          rleTextEntry.SetFocus();
        end

      // Filtering by recipient type is taken care of by a different mechanism

      else if( tr( 'R/I status' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := true;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := false;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := true;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      else if( tr( 'R/I zone' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'R/I zone:' );
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := true;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := '';
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          _filtering := true;

          rleTextEntry.SetFocus();
        end
      else if( tr( 'Day of initiation' ) = s ) then
        begin
          lblExposureType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'Day of initiation:' );
          lblDiseaseState.Visible := false;

          cboExposureType.Visible := false;
          rleTextEntry.Visible := true;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := RE_SIGNED_INTEGER_INPUT;
          pnlExposureSuccess.Visible := false;
          cboDiseaseState.Visible := false;

          _filtering := true;

          rleTextEntry.SetFocus();
        end
      else
        raise exception.Create( 'Unrecognized string (' + s + ') in TFormOutputExposures.cboMainFilterChange()' )
      ;
    end
  ;


  procedure TFormOutputExposures.rleTextEntryEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
    end
  ;


  procedure TFormOutputExposures.cboExposuresChange(Sender: TObject);
    begin
      filter();
    end
  ;


  procedure TFormOutputExposures.rdoClick(Sender: TObject);
    begin
      filter();
    end
  ;


  procedure TFormOutputExposures.fraAcceptCancelbtnAcceptClick(Sender: TObject);
    var
      s: string;
    begin
      s := cboMainFilter.Items[ cboMainFilter.ItemIndex ];

      if( not( rleTextEntry.Visible ) ) then
        updateExposureCounter()
      else
        begin
          // Deal with days
          //---------------
          if( ( tr( 'Day' ) = s ) or ( tr( 'Day of initiation' ) = s ) ) then
            begin
              if( -2 <> myStrToInt( rleTextEntry.Text, -2 ) ) then
                filter()
              ;
            end

          // Deal with other stuff
          //----------------------
          else if( 0 < length( trim( rleTextEntry.text ) ) ) then
            filter()
          ;
        end
      ;

      fraAcceptCancel.Visible := false;
    end
  ;


  procedure TFormOutputExposures.fraAcceptCancelbtnCancelClick(Sender: TObject);
    begin
      if( rleTextEntry.Visible ) then
        rleTextEntry.Text := ''
      ;

      fraAcceptCancel.Visible := false;
      stgGrid.SearchOptions.Filtered := false;

      updateExposureCounter();
    end
  ;



  procedure TFormOutputExposures.rleTextEntryExit(Sender: TObject);
    begin
      if
        ( fraAcceptCancel.btnAccept = self.ActiveControl )
      or
        ( fraAcceptCancel.btnCancel = self.ActiveControl )
      then
        // Do nothing: the button click events do the work.
      else
        begin
          // Force accept
          fraAcceptCancelbtnAcceptClick( nil );
        end
      ;
    end
  ;


  procedure TFormOutputExposures.rleTextEntryKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
    begin
      if( 13 = key ) then
        begin
          fraAcceptCancelbtnAcceptClick( nil );
          pnlFilterControls.SetFocus();
        end
      else if( 27 = key ) then
        begin
          fraAcceptCancelbtnCancelClick( nil );
          pnlFilterControls.SetFocus();
        end
      ;
    end
  ;


  procedure TFormOutputExposures.cboRecipientProdTypesCloseUp( Sender: TObject );
    begin
      _recipientPT := cboRecipientProdTypes.items.objects[cboRecipientProdTypes.itemIndex] as TProductionType;

      productionTypeChanged();
    end
  ;
//-----------------------------------------------------------------------------






  procedure TFormOutputExposures.FormResize(Sender: TObject);
    var
      factor: integer;
      visible: boolean;
    begin
      inherited;
      if(_ScrollBarVisibleCheck = false) then
        begin
          visible := true;
          _ScrollBarVisibleCheck := true;
        end
      else
        begin
          if (GetWindowlong(self.stgGrid.Handle, GWL_STYLE) and WS_HSCROLL) <> 0 then
            visible := true
          else
            visible := false
          ;
        end
      ;
      factor := (self.ClientHeight - self.pnlCaption.Height - self.pnlFilterControls.Height - self.pnlControls.Height - self.pnlSortControls.Height - self.pnlEventCounter.Height - 6 ) div
        (self.stgGrid.DefaultRowHeight +
        self.stgGrid.GridLineWidth );

      if(visible) then
        self.stgGrid.Height := factor * (self.stgGrid.DefaultRowHeight + self.stgGrid.GridLineWidth )
      else
        self.stgGrid.Height := factor * (self.stgGrid.DefaultRowHeight + self.stgGrid.GridLineWidth ) + 4
      ;

      self.stgGrid.Align := alTop;
      self.pnlEventCounter.Align := alTop;
      self.spacerPanel.Align := alClient;
    end
  ;


  procedure TFormOutputExposures.updateSimComplete();
    begin
      raise exception.Create( 'updateSimComplete is not yet supported in TFormOutputExposures' );
    end
  ;


  procedure TFormOutputExposures.resetIteration( const iteration: Integer );
    begin
      if( 0 <= cboIteration.Items.IndexOf( IntToStr( iteration ) ) ) then
        begin
          cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( iteration ) );
          _displayedIteration := iteration;
          fillOutForm();
          updateExposureCounter();
        end
      else
        raise exception.Create( 'Bad iteration (' + intToStr( iteration ) + ' in TFormOutputExposures.resetIteration()' )
      ;
    end
  ;


  procedure TFormOutputExposures.iterationChanged();
    begin
      if ( assigned( frmMain ) ) then
        frmMain.displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex])
      else
        begin
          _displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex]);
          fillOutForm();
          updateExposureCounter();
        end
      ;
    end
  ;


  procedure TFormOutputExposures.fillOutForm();
    var
      res: TSqlResult;
      row: TSqlRow;
      q: string;
      selectedPTIDClause: string;
      i: integer;
      filterUsed: boolean;
    begin
      resetGrid();

      // Remember the previous filter/sort options
      //------------------------------------------
      filterUsed := stgGrid.SearchOptions.Filtered;

      stgGrid.SearchOptions.Filtered := false;

      // Was the last/current iteration completed?
      //-------------------------------------------
      q := 'SELECT completedIterations FROM outGeneral';
      res := TSqlResult.create( q, ( _smdb as TSqlDatabase ) );
      row := res.fetchArrayFirst();

      if( null = row.field('completedIterations') ) then
        pnlCaption.Caption := tr( 'Iteration status: aborted' )
      else if( _displayedIteration > row.field('completedIterations') ) then
        pnlCaption.Caption := tr( 'Iteration status: aborted' )
      else
        pnlCaption.Caption := tr( 'Iteration status: complete' )
      ;

      // Determine the last day of the last/current iteration
      //------------------------------------------------------
      if( 0 >= _displayedIteration ) then
        raise exception.create( 'Bad iteration (' + intToStr( _displayedIteration ) + ' in TFormOutputExposures.fillOutForm()' )
      ;

      q := 'SELECT MAX(day) AS maxDay FROM outDailyExposures WHERE iteration = ' + intToStr( _displayedIteration );
      res.runQuery( q );
      row := res.fetchArrayFirst();

      if( null = row.field('maxDay') ) then // There is no data in the database.
        begin
          res.Free();
          updateExposureCounter();
          exit;
        end
      ;

      // Determine the ID of the currently selected source production type
      //------------------------------------------------------------------
      if( nil = _selectedPT ) then
        selectedPTIDClause := ''
      else
        selectedPTIDClause := ' AND dynHerd.productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;

      // Determine the ID of the currently selected recipient production type
      //---------------------------------------------------------------------
      if( nil <> _recipientPT ) then
        selectedPTIDClause := selectedPTIDClause + ' AND exposedHerd.productionTypeID = ' + intToStr( _recipientPT.productionTypeID )
      ;

      // select exposures for indicated pts, last/current iteration
      //------------------------------------------------------------
      (*
      // The original query
      //-------------------
      SELECT TOP 10
        outDailyExposures.iteration,
        outDailyExposures.day,
        outDailyExposures.initiatedDay,
        outDailyExposures.exposure,
        outDailyExposures.exposingHerdID,
        ##dynHerd.herdID AS exposingHerdID2,
        ##dynHerd.productionTypeID AS exposingTypeID,
        ##inProductionType.productionTypeID AS exposingTypeID2,
        exposingType.descr AS exposingTypeDescr,
        outDailyExposures.exposingHerdStatusCode,
        ##outDailyExposures.exposingZoneID,
        exposingZone.descr AS exposingZoneDescr,
        outDailyExposures.spreadMethodCode,
        outDailyExposures.isAdequate,
        outDailyExposures.exposedHerdID,
        ##exposedHerd.herdID AS exposedHerdID2,
        ##exposedHerd.productionTypeID AS exposedTypeID,
        ##exposedType.productionTypeID AS exposedTypeID2,
        exposedType.descr AS exposedTypeDescr,
        outDailyExposures.exposedHerdStatusCode,
        ##outDailyExposures.exposedZoneID,
        exposedZone.descr AS exposedZoneDescr
      FROM ( ( ( ( (
        outDailyExposures
      LEFT OUTER JOIN dynHerd ON dynHerd.herdID = outDailyExposures.exposingHerdID )
      LEFT OUTER JOIN inProductionType AS exposingType ON exposingType.productionTypeID = dynHerd.productionTypeID )
      LEFT OUTER JOIN inZone AS exposingZone ON exposingZone.zoneID = outDailyExposures.exposingZoneID )
      LEFT OUTER JOIN dynHerd AS exposedHerd ON exposedHerd.herdID = outDailyExposures.exposedHerdID )
      LEFT OUTER JOIN inProductionType AS exposedType ON exposedType.productionTypeID = exposedHerd.productionTypeID )
      LEFT OUTER JOIN inZone AS exposedZone ON exposedZone.zoneID = outDailyExposures.exposedZoneID
      WHERE outDailyExposures.exposingZoneID IS NOT NULL
      ORDER BY outDailyExposures.day, outDailyExposures.exposure
      ;
      *)
      q := 'SELECT'
        //+ ' TOP 10' // Useful for debugging
        + ' outDailyExposures.day,'
        + ' outDailyExposures.initiatedDay,'
        + ' outDailyExposures.exposure,'
        + ' outDailyExposures.exposingHerdID,'
        + ' exposingType.descr AS exposingTypeDescr,'
        + ' outDailyExposures.exposingHerdStatusCode,'
        + ' exposingZone.descr AS exposingZoneDescr,'
        + ' outDailyExposures.spreadMethodCode,'
        + ' outDailyExposures.isAdequate,'
        + ' outDailyExposures.exposedHerdID,'
        + ' exposedType.descr AS exposedTypeDescr,'
        + ' outDailyExposures.exposedHerdStatusCode,'
        + ' exposedZone.descr AS exposedZoneDescr'
        + ' FROM ( ( ( ( ('
        + ' outDailyExposures'
        + ' LEFT OUTER JOIN dynHerd ON dynHerd.herdID = outDailyExposures.exposingHerdID )'
        + ' LEFT OUTER JOIN inProductionType AS exposingType ON exposingType.productionTypeID = dynHerd.productionTypeID )'
        + ' LEFT OUTER JOIN inZone AS exposingZone ON exposingZone.zoneID = outDailyExposures.exposingZoneID )'
        + ' LEFT OUTER JOIN dynHerd AS exposedHerd ON exposedHerd.herdID = outDailyExposures.exposedHerdID )'
        + ' LEFT OUTER JOIN inProductionType AS exposedType ON exposedType.productionTypeID = exposedHerd.productionTypeID )'
        + ' LEFT OUTER JOIN inZone AS exposedZone ON exposedZone.zoneID = outDailyExposures.exposedZoneID'
        + '  WHERE'
        + '    outDailyExposures.iteration = ' + intToStr( _displayedIteration )
        +      selectedPTIDClause
        + '  ORDER BY'
        + '    outDailyExposures.day, outDailyExposures.exposure'
      ;

      res.runQuery( q );

      // Fill the grid
      //--------------
      if( 1 < res.numRows ) then
        stgGrid.RowCount := res.numRows + 1
      else
        stgGrid.RowCount := 2
      ;

      row := res.fetchArrayFirst();
      i := 1;
      while( nil <> row ) do
        begin
          stgGrid.Cells[ COL_DAY, i ] := intToStr( row.field('day') );
          stgGrid.Cells[ COL_EXPOSURE, i ] := intToStr( row.field('exposure') );

          stgGrid.Cells[ COL_SOURCE_HERDID, i ] := intToStr( row.field('exposingHerdID') );
          stgGrid.Cells[ COL_SOURCE_TYPE_DESCR, i ] := row.field('exposingTypeDescr');
          stgGrid.Cells[ COL_SOURCE_STATUS, i ] := naadsmDiseaseStateStr( naadsmDiseaseStateFromCode( charAt( string( row.field('exposingHerdStatusCode') ), 0 ) ) );

          if( null <> row.field('exposingZoneDescr') ) then
            stgGrid.Cells[ COL_SOURCE_ZONE, i ] := row.field('exposingZoneDescr')
          else
            stgGrid.Cells[ COL_SOURCE_ZONE, i ] := ''
          ;

          stgGrid.Cells[ COL_EXPOSURE_CODE, i ] := TSMExposureOrTrace.getExposureCodeString( row.field('spreadMethodCode') );
          stgGrid.Cells[ COL_EXPOSURE_IS_ADEQUATE, i ] := uiBoolToText( row.field('isAdequate') );

          stgGrid.Cells[ COL_RECIPENT_HERDID, i ] := intToStr( row.field('exposedHerdID') );
          stgGrid.Cells[ COL_RECIPIENT_TYPE_DESCR, i ] := row.field('exposedTypeDescr');
          stgGrid.Cells[ COL_RECIPIENT_STATUS, i ] := naadsmDiseaseStateStr( naadsmDiseaseStateFromCode( charAt( string( row.field('exposedHerdStatusCode') ), 0 ) ) );

          if( null <> row.field('exposedZoneDescr') ) then
            stgGrid.Cells[ COL_RECIPIENT_ZONE, i ] := row.field('exposedZoneDescr')
          else
            stgGrid.Cells[ COL_RECIPIENT_ZONE, i ] := ''
          ;

          if( null <> row.field('initiatedDay') ) then
            stgGrid.Cells[ COL_INITIATED_DAY, i ] := intToStr( row.field('initiatedDay') )
          else
            stgGrid.Cells[ COL_INITIATED_DAY, i ] := tr( 'N/A' )
          ;

          inc( i );
          row := res.fetchArrayNext();
        end
      ;

      if( not( _creating ) ) then
        begin
          // Reapply the previous filter/sort options
          //------------------------------------------
          if( 0 <> _lastColumnSorted ) then stgGrid.SortByColumn( _lastColumnSorted );

          if( filterUsed ) then
            begin
              stgGrid.SearchOptions.Filtered := true;
              filter();
            end
          ;
        end
      ;

      res.free();
    end
  ;


  procedure TFormOutputExposures.setCaption();
    begin
      if( frmMain.simIsRunning ) then
        pnlCaption.Caption := tr( 'Iteration status: running' )
      else
        begin
          if( _smdb.containsIncompleteIterations() ) then
            pnlCaption.Caption := tr( 'Iteration status: aborted' )
          else
            pnlCaption.Caption := tr( 'Iteration status: complete' )
          ;
        end
      ;
    end
  ;

end.
