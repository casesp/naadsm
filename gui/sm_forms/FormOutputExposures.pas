unit FormOutputExposures;

(*
FormOutputExposures.pas/dfm
---------------------------
Begin: 2005/02/01
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.23 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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

      lblStatus: TLabel;
      cboStatus: TComboBox;
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
      lblDay: TLabel;
      lblExposureType: TLabel;
      lblSourceHerdID: TLabel;
      cboMainFilter: TComboBox;
      cboExposureType: TComboBox;
      rleDay: TREEdit;
      fraAcceptCancel: TFrameAcceptCancel;
      rleSourceHerdID: TREEdit;
      pnlEventCounter: TPanel;
      stgGrid: TARSortGrid;
      lblRecipientHerdID: TLabel;
      rleRecipientHerdID: TREEdit;
      pnlExposureSuccess: TPanel;
      rdoSuccess: TRadioButton;
      rdoUnsuccess: TRadioButton;
      lblIteration: TLabel;
      cboIteration: TComboBox;

      procedure cboRecipientProdTypesCloseUp(Sender: TObject);
      procedure stgGridEndSort(Sender: TObject; Col: Integer);
      procedure sortControlChange(Sender: TObject);

      procedure stgGridGetCellFormat(Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
      procedure stgGridBeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
      procedure cboMainFilterChange(Sender: TObject);
      procedure rleSourceHerdIDEnter(Sender: TObject);
      procedure rleRecipientHerdIDEnter(Sender: TObject);
      procedure rleDayEnter(Sender: TObject);
      procedure cboExposuresChange(Sender: TObject);
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);
      procedure rleExit(Sender: TObject);
      procedure rleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure rdoClick(Sender: TObject);

      procedure FormResize(Sender: TObject);
      procedure cboIterationChange(Sender: TObject);

    protected
      _firstRow: boolean;
      _presorting: boolean;
      _filtering: boolean;
      _creating: boolean;
      _sortInProgress: boolean;
      _lastColumnSorted: integer;

      _recipientPT: TProductionType;

      _sqlRes: TSqlResult;
      _ScrollBarVisibleCheck: boolean;
      _displayedIteration: integer;
      _inSimComplete: boolean;
      
      procedure translateUI();      
      
      procedure resetGrid();

      procedure filter();

      procedure updateExposureCounter();

	    procedure setupRecipientComboBox();

      { Handles form updates from the database when a simulation is not running or when _selectedPT is changed }
      procedure setupFromDatabase();
      procedure simChanged(); override;

      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); override;

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

      procedure updateSimComplete();    

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
    GuiStrUtils,
    DebugWindow,
    RegExpDefs,
    I88n,

    ControlUtils,

    StatusEnums,
    FormMain
  ;


  const
    COL_DAY: integer = 0;
    COL_EXPOSURE: integer = 1;
    COL_SOURCE_HERDID: integer = 2;
    COL_SOURCE_TYPE_DESCR: integer = 3;
    COL_SOURCE_STATUS: integer = 4;
    COL_SOURCE_ZONE: integer = 5;
    COL_EXPOSURE_CODE: integer = 6;
    COL_EXPOSURE_SUCCESS: integer = 7;
    COL_RECIPENT_HERDID: integer = 8;
    COL_RECIPIENT_TYPE_DESCR: integer = 9;
    COL_RECIPIENT_STATUS: integer = 10;
    COL_RECIPIENT_ZONE: integer = 11;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormOutputExposures.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
    var
      frm: TForm;
		begin
      inherited create( AOwner );
      translateUI();
      
      self._inSimComplete := false;
      _ScrollBarVisibleCheck := false;
      _creating := true;

      _sortInProgress := false;
      _filtering := false;

      _lastColumnSorted := 0;
      cboIteration.Enabled := false;
      _displayedIteration := frmMain.displayedIteration;


      // Set up widgets
      //---------------
      lblExposureType.Top := lblDay.Top;
      lblExposureType.Left := lblDay.Left;

      lblSourceHerdID.Top := lblDay.Top;
      lblSourceHerdID.Left := lblDay.Left;

      lblRecipientHerdID.Top := lblDay.Top;
      lblRecipientHerdID.Left := lblDay.Left;

      lblStatus.Top := lblDay.Top;
      lblStatus.Left := lblDay.Left;

      cboExposureType.Top := rleDay.Top;
      cboExposureType.Left := rleDay.Left;

      rleSourceHerdID.Top := rleDay.Top;
      rleSourceHerdID.Left := rleDay.left;

      rleRecipientHerdID.Top := rleDay.Top;
      rleRecipientHerdID.Left := rleDay.left;

      pnlExposureSuccess.Top := rleDay.Top;
      pnlExposureSuccess.Left := lblDay.Left;

      cboStatus.Top := rleDay.Top;
      cboStatus.Left := rleDay.Left;

      rleDay.InputExpression := RE_SIGNED_INTEGER_INPUT;
      rleSourceHerdID.InputExpression := RE_INTEGER_INPUT;
      rleRecipientHerdID.InputExpression := RE_INTEGER_INPUT;

      pnlEventCounter.Caption := '';
      pnlCaption.Caption := '';


      setChartControlsEnabled( false );

      // Initialize pointers
      //--------------------
      _smSim := sim;
      _smdb := db;
      _sqlRes := TSqlResult.create( db as TSqlDatabase );

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
      setupComboBox();
      setupRecipientComboBox();

      setCaption();

      productionTypeChanged();

      _creating := false;
		end
	;


  procedure TFormOutputExposures.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormOutputExposures.dfm
      // File date: Wed May 30 11:08:12 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Exposures for 1 iteration' );
          lblSourcePT.Caption := tr( 'Source production type:' );
          lblRecipientPT.Caption := tr( 'Recipient production type:' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
          lblMainFilter.Caption := tr( 'Filter by:' );
          lblDay.Caption := tr( 'Day:' );
          lblExposureType.Caption := tr( 'Exposure type:' );
          lblSourceHerdID.Caption := tr( 'Source unit ID:' );
          lblRecipientHerdID.Caption := tr( 'Recipient unit ID:' );
          lblStatus.Caption := tr( 'Status:' );
          cboMainFilter.Text := tr( '(No filter)' );
          rdoSuccess.Caption := tr( 'Successful exposures' );
          rdoUnsuccess.Caption := tr( 'Unsuccessful exposures' );
          pnlEventCounter.Caption := tr( 'pnlEventCounter' );
          lblMainSort.Caption := tr( 'Sort by:' );
          lblSortOrder.Caption := tr( 'Sort order:' );
          lblIteration.Caption := tr( 'Iteration:' );
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
          cboMainFilter.Items[2] := tr( 'Source unit ID' );
          cboMainFilter.Items[3] := tr( 'Source unit status' );
          cboMainFilter.Items[4] := tr( 'Exposure type' );
          cboMainFilter.Items[5] := tr( 'Exposure success' );
          cboMainFilter.Items[6] := tr( 'Recipient unit ID' );
          cboMainFilter.Items[7] := tr( 'Recipient unit status' );

          cboExposureType.Items[0] := tr( 'Direct contacts' );
          cboExposureType.Items[1] := tr( 'Indirect contacts' );
          cboExposureType.Items[2] := tr( 'Airborne spread' );

          cboStatus.Items[0] := tr( 'Susceptible' );
          cboStatus.Items[1] := tr( 'Latent' );
          cboStatus.Items[2] := tr( 'Subclinical' );
          cboStatus.Items[3] := tr( 'Clinical' );
          cboStatus.Items[4] := tr( 'Naturally immune' );
          cboStatus.Items[5] := tr( 'Vaccine immune' );
          cboStatus.Items[6] := tr( 'Destroyed' );

          cboMainSort.Items[0] := tr( 'Day' );
          cboMainSort.Items[1] := tr( 'Exp. on day' );
          cboMainSort.Items[2] := tr( 'Source unit ID' );
          cboMainSort.Items[3] := tr( 'Source type' );
          cboMainSort.Items[4] := tr( 'Source status' );
          cboMainSort.Items[5] := tr( 'Exp. type' );
          cboMainSort.Items[6] := tr( 'Exp. success' );
          cboMainSort.Items[7] := tr( 'Recipient unit ID' );
          cboMainSort.Items[8] := tr( 'Recipient type' );
          cboMainSort.Items[9] := tr( 'Recipient status' );
        end
      ;

    end
  ;


	destructor TFormOutputExposures.destroy();
		begin
      _sqlRes.Free();
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


	procedure TFormOutputExposures.setupRecipientComboBox();
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
    var
      row: TSqlRow;
      q: string;
    begin
      resetGrid();
      cboIteration.Items.Clear();

      stgGrid.SearchOptions.Filtered := false;

      if ( ( not frmMain.simIsRunning ) or (_inSimComplete) ) then
        begin
      q := 'SELECT DISTINCT (iteration) FROM outDailyEvents order by iteration desc;';
      _sqlRes.runQuery( q );
      row := _sqlRes.fetchArrayFirst();

      if ( row <> nil ) then
        begin
          cboIteration.Enabled := true;
          while ( row <> nil ) do
            begin
              cboIteration.Items.Add(row.field('iteration'));
              row := _sqlRes.fetchArrayNext();
            end;

          cboIteration.ItemIndex := 0;

          if ( -1 = _displayedIteration ) then
            _displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex])
          else
            begin
              if ( cboIteration.Items.IndexOf( IntToStr( _displayedIteration ) ) >= 0 ) then
                begin
                  cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( _displayedIteration ) );
                end
              else
                begin
                  _displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex]);
//                  frmMain.displayedIteration := _displayedIteration; // FIX ME: Why is this commented out?
                end;
            end;
          fillOutForm();
          updateExposureCounter();
        end
      else
        begin
          cboIteration.Enabled := false;
          updateExposureCounter();
          exit;
        end;
        end;
    end
  ;


  procedure TFormOutputExposures.simChanged();
    begin
      _recipientPT := nil;
      
      setupRecipientComboBox();
      
      freeAndNil( _sqlRes );
      
      _sqlRes := TSqlResult.create( _smdb as TSqlDatabase );
      setupFromDatabase();
      setCaption();
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
      stgGrid.ColCount := 12;

      stgGrid.Cells[ COL_DAY, 0 ] := tr( 'Day' );
      stgGrid.Cells[ COL_EXPOSURE, 0 ] := tr( 'Exp. on day' );
      stgGrid.Cells[ COL_SOURCE_HERDID, 0 ] := tr( 'Source unit ID' );
      stgGrid.Cells[ COL_SOURCE_TYPE_DESCR, 0 ] := tr( 'Source type' );
      stgGrid.Cells[ COL_SOURCE_STATUS, 0 ] := tr( 'Source status' );
      stgGrid.Cells[ COL_SOURCE_ZONE, 0 ] := tr( 'Source zone' );
      stgGrid.Cells[ COL_EXPOSURE_CODE, 0 ] := tr( 'Exp. type' );
      stgGrid.Cells[ COL_EXPOSURE_SUCCESS, 0 ] := tr( 'Exp. success' );
      stgGrid.Cells[ COL_RECIPENT_HERDID, 0 ] := tr( 'Recipient unit ID' );
      stgGrid.Cells[ COL_RECIPIENT_TYPE_DESCR, 0 ] := tr( 'Recipient type' );
      stgGrid.Cells[ COL_RECIPIENT_STATUS, 0 ] := tr( 'Receipient status' );
      stgGrid.Cells[ COL_RECIPIENT_ZONE, 0 ] := tr( 'Receipient zone' );

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
            str := '0 ' + tr( 'exposures' )
          else
            str := '1 ' + tr( 'exposure' )
          ;
        end
      else
        str := intToStr( stgGrid.RowCount - 1 ) + ' ' + tr( 'exposures' )
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
    begin
      dbcout( '--- TFormOutputExposures.filter...', DBFORMOUTPUTEXPOSURES );

      //lockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      try
        stgGrid.SearchOptions.Filtered := false;

        case cboMainFilter.ItemIndex of
          0: // No filter
            begin
              _filtering := false;
              stgGrid.SearchOptions.Filtered := false;
            end
          ;
          1: // Day
            begin
             stgGrid.SearchOptions.SearchCol := COL_DAY;
             stgGrid.SearchOptions.SearchText := rleDay.Text;
             stgGrid.SearchOptions.Filtered := _filtering;
            end
          ;
          2: // Source ID
            begin
              stgGrid.SearchOptions.SearchCol := COL_SOURCE_HERDID;
              stgGrid.SearchOptions.SearchText := rleSourceHerdID.Text;
              stgGrid.SearchOptions.Filtered := _filtering;
            end
          ;
          3: // Source status
            begin
              if( -1 = cboStatus.ItemIndex ) then
                stgGrid.SearchOptions.Filtered := false
              else
                begin
                  dbcout( 'Filtering source status by ' + cboStatus.Items.Strings[cboStatus.ItemIndex], DBFORMOUTPUTEXPOSURES );
                  stgGrid.SearchOptions.SearchCol := COL_SOURCE_STATUS;
                  stgGrid.SearchOptions.SearchText := cboStatus.Items.Strings[cboStatus.ItemIndex];
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
            end
          ;
          4: // Exposure type
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
                    stgGrid.SearchOptions.SearchText := tr( 'Airborne' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                else // cancel the filtering
                  stgGrid.SearchOptions.Filtered := false
                ;
              end;

            end
          ;
          5: // Exposure success
            begin
              stgGrid.SearchOptions.SearchCol := COL_EXPOSURE_SUCCESS;

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
          ;
          6: // Recipient ID
            begin
              stgGrid.SearchOptions.SearchCol := COL_RECIPENT_HERDID;
              stgGrid.SearchOptions.SearchText := rleRecipientHerdID.Text;
              stgGrid.SearchOptions.Filtered := _filtering;
            end
          ;
          7: // Recipient status
            begin
              if( -1 = cboStatus.ItemIndex ) then
                stgGrid.SearchOptions.Filtered := false
              else
                begin
                  dbcout( 'Filtering Recipient status by ' + cboStatus.Items.Strings[cboStatus.ItemIndex], DBFORMOUTPUTEXPOSURES );
                  stgGrid.SearchOptions.SearchCol := COL_RECIPIENT_STATUS;
                  stgGrid.SearchOptions.SearchText := cboStatus.Items.Strings[cboStatus.ItemIndex];
                  stgGrid.SearchOptions.Filtered := _filtering;
                end
              ;
            end
          ;
        end;

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
    begin
      case cboMainFilter.ItemIndex of
        0: // No filter
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := false;

            cboExposureType.Visible := false;
            rleDay.Visible := false;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := false;

            filter();
          end
        ;
        1: // Day
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := true;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := false;

            cboExposureType.Visible := false;
            rleDay.Visible := true;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := false;

            //fraAcceptCancel.Visible := false;

            _filtering := true;

            rleDay.SetFocus();
          end
        ;
        2: // Source unit ID
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := true;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := false;

            cboExposureType.Visible := false;
            rleDay.Visible := false;
            rleSourceHerdID.visible := true;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := false;

            //fraAcceptCancel.Visible := false;

            _filtering := true;

            rleSourceHerdID.SetFocus();
          end
        ;
        3: // Source status
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := true;

            cboExposureType.Visible := false;
            rleDay.Visible := false;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := true;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();
          end
        ;
        4: // Exposure type
          begin
            lblExposureType.Visible := true;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := false;

            cboExposureType.Visible := true;
            rleDay.Visible := false;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();

            //cboEvents.SetFocus();
          end
        ;
        5: // Exposure success
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := false;

            cboExposureType.Visible := false;
            rleDay.Visible := false;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := true;
            cboStatus.Visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();
          end
        ;
        6: // Recipient unit ID
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := true;
            lblStatus.Visible := false;

            cboExposureType.Visible := false;
            rleDay.Visible := false;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := true;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := false;

            //fraAcceptCancel.Visible := false;

            _filtering := true;

            rleRecipientHerdID.SetFocus();
          end
        ;
        7: // Recipient status
          begin
            lblExposureType.Visible := false;
            lblDay.Visible := false;
            lblSourceHerdID.Visible := false;
            lblRecipientHerdID.Visible := false;
            lblStatus.Visible := true;

            cboExposureType.Visible := false;
            rleDay.Visible := false;
            rleSourceHerdID.visible := false;
            rleRecipientHerdID.Visible := false;
            pnlExposureSuccess.Visible := false;
            cboStatus.Visible := true;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();
          end
        ;
      end;
    end
  ;


  procedure TFormOutputExposures.rleSourceHerdIDEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
      //fraAcceptCancel.Enabled := true;
    end
  ;


  procedure TFormOutputExposures.rleRecipientHerdIDEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
      //fraAcceptCancel.Enabled := true;
    end
  ;


  procedure TFormOutputExposures.rleDayEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
      //fraAcceptCancel.Enabled := true;
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
    begin
      if( rleDay.Visible ) then
        begin
          if( -2 <> myStrToInt( rleDay.Text, -2 ) ) then filter();
        end
      else if( rleSourceHerdID.Visible ) then
        begin
          if( -2 <> myStrToInt( rleSourceHerdID.Text, -2 ) ) then filter();
        end
      else if( rleRecipientHerdID.Visible ) then
        begin
          if( -2 <> myStrToInt( rleRecipientHerdID.Text, -2 ) ) then filter();
        end
      else
        updateExposureCounter()
      ;

      fraAcceptCancel.Visible := false;
    end
  ;


  procedure TFormOutputExposures.fraAcceptCancelbtnCancelClick(Sender: TObject);
    begin
      if( rleDay.Visible ) then
        rleDay.Text := ''
      else if( rleSourceHerdID.Visible ) then
        rleSourceHerdID.Text := ''
      else if( rleRecipientHerdID.Visible ) then
        rleRecipientHerdID.Text := ''
      ;

      fraAcceptCancel.Visible := false;
      stgGrid.SearchOptions.Filtered := false;

      updateExposureCounter();
    end
  ;



  procedure TFormOutputExposures.rleExit(Sender: TObject);
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


  procedure TFormOutputExposures.rleKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
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
      _inSimComplete := true;
      self.simChanged();
      if ( -1 <> frmMain.displayedIteration ) then
        self.resetIteration( frmMain.displayedIteration )
      ;
      _inSimComplete := false;

      setCaption();
    end
  ;


  procedure TFormOutputExposures.resetIteration( const iteration: Integer );
    begin
      if ( cboIteration.Items.IndexOf( IntToStr( iteration ) ) >= 0 ) then
        begin
          cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( iteration ) );
          _displayedIteration := iteration;
          fillOutForm();
          updateExposureCounter();
        end
      ;
    end
  ;


  procedure TFormOutputExposures.cboIterationChange(Sender: TObject);
    begin
      inherited;
      if ( assigned( frmMain ) ) then
        begin
          frmMain.displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex]);
        end
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
      _sqlRes.runQuery( q );
      row := _sqlRes.fetchArrayFirst();

      if( null = row.field('completedIterations') ) then
        pnlCaption.Caption := tr( 'Iteration status: aborted' )
      else if( _displayedIteration > row.field('completedIterations') ) then
        pnlCaption.Caption := tr( 'Iteration status: aborted' )
      else
        pnlCaption.Caption := tr( 'Iteration status: complete' )
      ;

      // Determine the last day of the last/current iteration
      //------------------------------------------------------
      q := 'SELECT MAX(day) AS maxDay FROM outDailyEvents WHERE iteration = ' + intToStr( _displayedIteration );
      _sqlRes.runQuery( q );
      row := _sqlRes.fetchArrayFirst();

      if( null = row.field('maxDay') ) then // There is no data in the database.
        begin
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
        outDailyExposures.success,
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
        + ' outDailyExposures.exposure,'
        + ' outDailyExposures.exposingHerdID,'
        + ' exposingType.descr AS exposingTypeDescr,'
        + ' outDailyExposures.exposingHerdStatusCode,'
        + ' exposingZone.descr AS exposingZoneDescr,'
        + ' outDailyExposures.spreadMethodCode,'
        + ' outDailyExposures.success,'
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

      _sqlRes.runQuery( q );

      // Fill the grid
      //--------------
      if( 1 < _sqlRes.numRows ) then
        stgGrid.RowCount := _sqlres.numRows + 1
      else
        stgGrid.RowCount := 2
      ;

      row := _sqlRes.fetchArrayFirst();
      i := 1;
      while( nil <> row ) do
        begin
          stgGrid.Cells[ COL_DAY, i ] := intToStr( row.field('day') );
          stgGrid.Cells[ COL_EXPOSURE, i ] := intToStr( row.field('exposure') );

          stgGrid.Cells[ COL_SOURCE_HERDID, i ] := intToStr( row.field('exposingHerdID') );
          stgGrid.Cells[ COL_SOURCE_TYPE_DESCR, i ] := row.field('exposingTypeDescr');
          stgGrid.Cells[ COL_SOURCE_STATUS, i ] := transitionStateString( transitionStateFromCode( row.field('exposingHerdStatusCode') ) );

          if( null <> row.field('exposingZoneDescr') ) then
            stgGrid.Cells[ COL_SOURCE_ZONE, i ] := row.field('exposingZoneDescr')
          else
            stgGrid.Cells[ COL_SOURCE_ZONE, i ] := ''
          ;

          stgGrid.Cells[ COL_EXPOSURE_CODE, i ] := TSMExposure.getExposureCodeString( row.field('spreadMethodCode') );
          stgGrid.Cells[ COL_EXPOSURE_SUCCESS, i ] := uiBoolToText( row.field('success') );

          stgGrid.Cells[ COL_RECIPENT_HERDID, i ] := intToStr( row.field('exposedHerdID') );
          stgGrid.Cells[ COL_RECIPIENT_TYPE_DESCR, i ] := row.field('exposedTypeDescr');
          stgGrid.Cells[ COL_RECIPIENT_STATUS, i ] := transitionStateString( transitionStateFromCode( row.field('exposedHerdStatusCode') ) );;

          if( null <> row.field('exposedZoneDescr') ) then
            stgGrid.Cells[ COL_RECIPIENT_ZONE, i ] := row.field('exposedZoneDescr')
          else
            stgGrid.Cells[ COL_RECIPIENT_ZONE, i ] := ''
          ;

          inc( i );
          row := _sqlRes.fetchArrayNext();
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
