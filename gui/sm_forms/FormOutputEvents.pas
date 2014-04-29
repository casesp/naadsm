unit FormOutputEvents;

(*
FormOutputEvents.pas/dfm
-------------------------
Begin: 2005/02/01
Last revision: $Date: 2011-10-04 16:55:32 $ $Author: rhupalo $
Version: $Revision: 1.34.4.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2011 Animal Population Health Institute, Colorado State University

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
    REEdit,
    FrameAcceptCancel,

    // Application-specific data structures
    SMSimulationInput,
    SMDatabase,
    EventsAndExposures,

    // Application-specific widgets
    FormSMOutputBase // the base class!
  ;


  type TFormOutputEvents = class( TFormSMOutputBase )
      spacerPanel: TPanel;

      pnlCaption: TPanel;
      stgGrid: TARSortGrid;
      pnlFilterControls: TPanel;
      cboMainFilter: TComboBox;
      lblMainFilter: TLabel;
      lblTextEntry: TLabel;
      lblEventType: TLabel;
      lblHerdID: TLabel;
      cboEvents: TComboBox;
      rleTextEntry: TREEdit;
      fraAcceptCancel: TFrameAcceptCancel;
      pnlEventCounter: TPanel;

      pnlSortControls: TPanel;
      lblMainSort: TLabel;
      cboMainSort: TComboBox;
      lblSortOrder: TLabel;
      rdoAscending: TRadioButton;
      rdoDescending: TRadioButton;
      cboDiseaseState: TComboBox;
      lblDiseaseState: TLabel;
      cboTestResult: TComboBox;

      procedure stgGridGetCellFormat(Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
      procedure stgGridBeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
      procedure cboMainFilterChange(Sender: TObject);
      procedure sortControlChange(Sender: TObject);
      procedure rleTextEntryEnter(Sender: TObject);
      procedure cboEventsChange(Sender: TObject);
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);
      procedure rleExit(Sender: TObject);
      procedure rleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure stgGridEndSort(Sender: TObject; Col: Integer);
      procedure FormResize(Sender: TObject);
      procedure cboDiseaseStateChange(Sender: TObject);
      procedure cboTestResultChange(Sender: TObject);

    protected
      _firstRow: boolean;
      _presorting: boolean;
      _filtering: boolean;
      _creating: boolean;
      _sortInProgress: boolean;
      _lastColumnSorted: integer;

      _ScrollBarVisibleCheck: boolean;
//      _inSimComplete: boolean;  // AR 9/18/09: This is currently unnecessary, and complicates things...

      procedure translateUI();
      procedure translateUIManual();

      procedure resetGrid();

      procedure filter();

      procedure updateEventCounter();

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

      { Experimental feature: not currently used }
      procedure appendEvent( evt: TSMEvent );

      procedure resetIteration( iteration: Integer );
      procedure updateSimComplete(); override;     
    end
  ;


  var
    frmOutputEvents: TFormOutputEvents;

implementation

  {$R *.dfm}

  uses
    StrUtils,
    
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
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.

    COL_DAY: integer = 0;
    COL_EVENT: integer = 1;
    COL_HERDID: integer = 2;
    COL_HERD_TYPE: integer = 3;
    COL_ZONE: integer = 4;
    COL_EVENTCODE: integer = 5;
    COL_NEWSTATE: integer = 6;
    COL_TEST_RESULT: integer = 7;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormOutputEvents.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
    var
      frm: TForm;
		begin
      dbcout( '+++ TFormOutputEvents.create', DBSHOWMSG );
      _ScrollBarVisibleCheck := false;
      _creating := true;

      _lastColumnSorted := 0;
      _displayedIteration := frmMain.displayedIteration;

      inherited create( AOwner );
      translateUI();

      _presorting := false;
      _sortInProgress := false;
      _filtering := false;
      cboIteration.Enabled := false;

      // Set up widgets
      //---------------
      placeIterationControls();

      lblEventType.Top := lblTextEntry.Top;
      lblEventType.Left := lblTextEntry.Left;

      lblDiseaseState.Top := lblTextEntry.Top;
      lblDiseaseState.Left := lblTextEntry.Left;

      cboEvents.Top := rleTextEntry.Top;
      cboEvents.Left := rleTextEntry.Left;

      cboTestResult.Top := rleTextEntry.Top;
      cboTestResult.Left := rleTextEntry.Left;

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
      resetGrid();
      stgGrid.AutoSizeColumns();

      cboMainFilter.ItemIndex := 0;

      setupIterationComboBox();
      setupProdTypeComboBox();

      productionTypeChanged();

      stgGrid.SortByColumn( COL_DAY );

      setCaption();

      _creating := false;

      dbcout( '--- TFormOutputEvents.create done.', DBSHOWMSG );
		end
	;


  procedure TFormOutputEvents.translateUI();
    begin
      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Events for 1 iteration' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
          lblMainFilter.Caption := tr( 'Filter by:' );
          lblTextEntry.Caption := tr( 'Day:' );
          lblDiseaseState.Caption := tr( 'Disease state:' );
          lblEventType.Caption := tr( 'Event type:' );
          cboMainFilter.Text := tr( '(No filter)' );
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
          cboMainFilter.Items[2] := tr( 'Unit ID' );
          cboMainFilter.Items[3] := tr( 'Zone' );
          cboMainFilter.Items[4] := tr( 'Event type' );
          cboMainFilter.Items[5] := tr( 'New state' );
          cboMainFilter.Items[6] := tr( 'Test result' );

          // cboEvents is handled in the manual translation function below

          // cboDiseaseState is handled in the manual translation function below

          // cboTestResult is handled in the manual translation function below

          cboMainSort.Items[0] := tr( 'Day' );
          cboMainSort.Items[1] := tr( 'Event order on day' );
          cboMainSort.Items[2] := tr( 'Unit ID' );
          cboMainSort.Items[3] := tr( 'Unit type' );
          cboMainSort.Items[4] := tr( 'Zone' );
          cboMainSort.Items[5] := tr( 'Event type' );
          cboMainSort.Items[6] := tr( 'New state' );
          cboMainSort.Items[7] := tr( 'Test result' );
        end
      ;

      translateUIManual();
    end
  ;


  procedure TFormOutputEvents.translateUIManual();
    var
      i: TEventCode;
      j: TNAADSMDiseaseState;
      k: TNAADSMTestResult;
    begin
      cboEvents.Items.Clear();
      for i := firstEventCode() to lastEventCode() do
        cboEvents.Items.Append( eventCodeString( i ) )
      ;

      cboDiseaseState.Items.Clear();
      for j := naadsmFirstDiseaseState() to naadsmLastDiseaseState() do
        cboDiseaseState.Items.Append( naadsmDiseaseStateStr( j ) )
      ;

      cboTestResult.Items.Clear();
      for k := NAADSMTestTruePositive to NAADSMTestFalseNegative do
        cboTestResult.Items.Append( naadsmTestResultStr( k ) )
      ;
    end
  ;


	destructor TFormOutputEvents.destroy();
		begin
      inherited destroy();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Initialization functions
//-----------------------------------------------------------------------------
  procedure TFormOutputEvents.fillStringGridDict();
    begin
      _stringGridDict['Events'] := stgGrid;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Form updating functions
//-----------------------------------------------------------------------------
  procedure TFormOutputEvents.fillOutForm();
    var
      res: TSqlResult;
      row: TSqlRow;
      q: string;
      selectedPTIDClause: string;
      i: integer;
      filterUsed: boolean;
      eventCode: TEventCode;
      lastCol: integer;
    begin
      resetGrid();

      lastCol := _lastColumnSorted;

      // Remember the previous filter/sort options
      //------------------------------------------
      filterUsed := stgGrid.SearchOptions.Filtered;

      stgGrid.SearchOptions.Filtered := false;

      // Was the last/current iteration completed?
      //-------------------------------------------
      q := 'SELECT completedIterations FROM outGeneral';
      res := TSqlResult.create( q, (_smdb as TSqlDatabase) );
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
      q := 'SELECT MAX(day) AS maxDay FROM outDailyEvents WHERE iteration = ' + intToStr( _displayedIteration );
      res.runQuery( q );
      row := res.fetchArrayFirst();

      if( null = row.field('maxDay') ) then // There is no data in the database.
        begin
          res.free();
          updateEventCounter();
          exit;
        end
      ;

      // Determine the ID of the currently selected production type
      //-----------------------------------------------------------
      if( nil = _selectedPT ) then
        selectedPTIDClause := ''
      else
        selectedPTIDClause := ' AND inProductionType.productionTypeID = ' + intToStr( _selectedPT.productionTypeID )
      ;

      // The original query
      //-------------------
      (*
      SELECT
      outDailyEvents.herdID,
      inProductionType.descr AS typeDescr,
      outDailyEvents.zoneID,
      inZone.descr AS zoneDescr,
      outDailyEvents.iteration,
      outDailyEvents.day,
      outDailyEvents.event,
      outDailyEvents.eventCode,
      outDailyEvents.newStateCode,
      outDailyEvents.testResultCode
      FROM (
      inProductionType
      INNER JOIN ( dynHerd INNER JOIN outDailyEvents ON dynHerd.herdID = outDailyEvents.herdID  )
      ON inProductionType.productionTypeID = dynHerd.productionTypeID )
      LEFT OUTER JOIN inZone ON inZone.zoneID = outDailyEvents.zoneID
      WHERE outDailyEvents.iteration = 2
      ORDER BY outDailyEvents.day, outDailyEvents.event
      *)

      // select events for indicated pts, last/current iteration
      //---------------------------------------------------------
      q := ' SELECT'
        + '    outDailyEvents.herdID,'
        //+ '    dynHerd.productionTypeID AS typeID,'
        //+ '    inProductionType.productionTypeID AS typeID2,'
        + '    inProductionType.descr AS typeDescr,'
        //+ '    outDailyEvents.zoneID,'
        + '    inZone.descr AS zoneDescr,'
        + '    outDailyEvents.iteration,'
        + '    outDailyEvents.day,'
        + '    outDailyEvents.event,'
        + '    outDailyEvents.eventCode,'
        + '    outDailyEvents.newStateCode,'
        + '    outDailyEvents.testResultCode'
        + '  FROM ('
        + '    inProductionType' 
        + '  INNER JOIN ('
        + '    dynHerd'
        + '      INNER JOIN'
        + '        outDailyEvents'
        + '      ON'
        + '    dynHerd.herdID = outDailyEvents.herdID'
        + '  )'
        + '  ON'
        + '    inProductionType.productionTypeID = dynHerd.productionTypeID )'
        + '  LEFT OUTER JOIN inZone ON inZone.zoneID = outDailyEvents.zoneID'
        + '  WHERE'
        + '    outDailyEvents.iteration = ' + intToStr( _displayedIteration )
        +     selectedPTIDClause
        + '  ORDER BY'
        + '    outDailyEvents.day, outDailyEvents.event'
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
          // An alternative (although probably considerably slower) would be
          // to create an instance of TSMEvent from the database fields, and then
          // call the function appendEvent().

          eventCode := eventFromCode( charAt( row.field('eventCode'), 0 ) );

          stgGrid.Cells[ COL_DAY, i ] := intToStr( row.field('day') );
          stgGrid.Cells[ COL_EVENT, i ] := intToStr( row.field('event') );

          stgGrid.Cells[ COL_HERDID, i ] := intToStr( row.field('herdID') );
          stgGrid.Cells[ COL_HERD_TYPE, i ] := row.field('typeDescr');

          if( null <> row.field('zoneDescr') ) then
            stgGrid.Cells[ COL_ZONE, i ] := row.field('zoneDescr')
          else
            stgGrid.Cells[ COL_ZONE, i ] := ''
          ;

          stgGrid.Cells[ COL_EVENTCODE, i ] := eventCodeString( eventCode );

          if( eventIsStateChange( eventCode ) ) then
            stgGrid.Cells[ COL_NEWSTATE, i ] := naadsmDiseaseStateStr( naadsmDiseaseStateFromCode( charAt( string( row.field('newStateCode') ), 0 ) ) )
          else
            stgGrid.Cells[ COL_NEWSTATE, i ] := ''
          ;

          if( eventIsTest( eventCode ) ) then
            stgGrid.Cells[ COL_TEST_RESULT, i ] := naadsmTestResultStr( naadsmTestResultFromCode( charAt( string( row.field('testResultCode') ), 0 ) ) )
          else
            stgGrid.Cells[ COL_TEST_RESULT, i ] := ''
          ;

          inc( i );
          row := res.fetchArrayNext();
        end
      ;

      if( not( _creating ) ) then
        begin
          // Reapply the previous filter/sort options
          //------------------------------------------
          stgGrid.SortByColumn( lastCol );
          _lastColumnSorted := lastCol;

          if( filterUsed ) then
            begin
              dbcout( 'Reapplying filter', DBSHOWMSG );
              //stgGrid.SearchOptions.Filtered := true;
              filter();
            end
          ;
        end
      ;

      res.free();
    end
  ;


  {*
    This function handles form updates from the database: either when
    a simulation is not running, or when the selected production type is
    changed in midstream.  Function updateForDay() is used for dynamic
    updates.
  }
  procedure TFormOutputEvents.setupFromDatabase();
    begin
      dbcout( '+++ TFormOutputEvents.setupFromDatabase...', DBSHOWMSG );

      resetGrid();

      // Remmed out as a followup to resolved issue 2500, filtering was still lost when production type changes.
      //stgGrid.SearchOptions.Filtered := false;

      if ( frmMain.simIsRunning ) then
        disableIterationComboBox()
      else
        begin
          setupIterationComboBox();
          fillOutForm(); // needs to know the filter state.
        end
      ;
      
      updateEventCounter();

      dbcout( '--- TFormOutputEvents.setupFromDatabase done', DBSHOWMSG );
    end
  ;


  procedure TFormOutputEvents.simChanged();
    begin
      setupIterationComboBox();
      productionTypeChanged();
    end
  ;


  procedure TFormOutputEvents.appendEvent( evt: TSMEvent );
    var
      row: integer;
    begin
      // FIX ME: check the filter options??

      if( _firstRow ) then
        begin
          row := 1;
          _firstRow := false;
        end
      else
        row := stgGrid.appendRow()
      ;

      stgGrid.Cells[ COL_DAY, row ] := intToStr( evt.day );
      stgGrid.Cells[ COL_EVENT, row ] := intToStr( evt.eventID );

      stgGrid.Cells[ COL_HERDID, row ] := intToStr( evt.herdID );
      stgGrid.Cells[ COL_EVENTCODE, row ] := evt.eventCodeString;

      if( eventIsStateChange( evt.eventCode ) ) then
        stgGrid.Cells[ COL_NEWSTATE, row ] := naadsmDiseaseStateStr( evt.newStatus )
      else
        stgGrid.Cells[ COL_NEWSTATE, row ] := ''
      ;

      if( eventIsTest( evt.eventCode ) ) then
        stgGrid.Cells[ COL_TEST_RESULT, row ] := naadsmTestResultStr( evt.testResult )
      else
        stgGrid.Cells[ COL_TEST_RESULT, row ] := ''
      ;

      if( 0 <> _lastColumnSorted ) then
        stgGrid.SortByColumn( _lastColumnSorted )
      ;

      updateEventCounter();
    end
  ;


  procedure TFormOutputEvents.productionTypeChanged();
    begin
      dbcout( 'Production type changed!', DBSHOWMSG );

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
  procedure TFormOutputEvents.resetGrid();
    begin
      stgGrid.Clear();

      _firstRow := true;

      stgGrid.RowCount := 2;
      stgGrid.ColCount := 8;

      stgGrid.Cells[ COL_DAY, 0 ] := tr( 'Day' );
      stgGrid.Cells[ COL_EVENT, 0 ] := tr( 'Event order on day' );
      stgGrid.Cells[ COL_HERDID, 0 ] := tr( 'Unit ID' );
      stgGrid.Cells[ COL_HERD_TYPE, 0 ] := tr( 'Unit type' );
      stgGrid.Cells[ COL_ZONE, 0 ] := tr( 'Zone' );
      stgGrid.Cells[ COL_EVENTCODE, 0 ] := tr( 'Event type' );
      stgGrid.Cells[ COL_NEWSTATE, 0 ] := tr( 'New state' );
      stgGrid.Cells[ COL_TEST_RESULT, 0 ] := tr( 'Test result' );

      repaint();
    end
  ;


  procedure TFormOutputEvents.setControlsEnabled( val: boolean );
    begin
      inherited setControlsEnabled( val );

      setChildrenEnabled( pnlFilterControls, val, false );
      setChildrenEnabled( pnlSortControls, val, false );

      setDataControlsEnabled( val );
      setChartControlsEnabled( false );
    end
  ;


  procedure TFormOutputEvents.updateEventCounter();
    var
      str: string;
    begin
      if( 2 = stgGrid.RowCount ) then
        begin
          if( 0 = length( trim( stgGrid.Cells[ 0, 1 ] ) ) ) then
            str := '0 ' + tr( 'events' )
          else
            str := '1 ' + tr( 'event' )
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
  procedure TFormOutputEvents.stgGridGetCellFormat(Sender: TObject; Col,
    Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
    begin
      if( 0 = Row ) then FormatOptions.Font.Style := [fsbold];
    end
  ;


  { Ensures that sorting happens first by day, then by event, then by whatever other criterion is selected. }
  procedure TFormOutputEvents.stgGridBeginSort(Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
    var
      mainSortDirection: TSortDirection;
    begin
      dbcout( '+++ TFormOutputEvents.stgGridBeginSort', DBSHOWMSG );

      screen.cursor := crHourglass;

      setControlsEnabled( false );

      _sortInProgress := true;

      //LockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      if( not( _presorting ) ) then
        begin
          dbcout( '_presorting was false', DBSHOWMSG );

          _presorting := true;

          mainSortDirection := stgGrid.SortDirection;

          stgGrid.SortDirection := sdAscending;

          dbcout( COL_EVENT, DBSHOWMSG );

          if
            ( COL_EVENT <> Col )
          and
            ( COL_DAY <> col )
          then
            begin
              dbcout( 'Presorting by event then day.', DBSHOWMSG );
              stgGrid.SortByColumn( COL_EVENT );
              stgGrid.SortByColumn( COL_DAY );
            end
          else if( COL_DAY = col ) then
            begin
              dbcout( 'Presorting by event only.', DBSHOWMSG );
              stgGrid.SortByColumn( COL_EVENT );
            end
          else if( COL_EVENT = col ) then
            begin
              dbcout( 'Presorting by day only.', DBSHOWMSG );
              stgGrid.SortByColumn( COL_DAY );
            end
          ;

          stgGrid.SortDirection := mainSortDirection;
          
          _presorting := false;
        end
      else
        dbcout( '_presorting was true: nothing should change.', DBSHOWMSG );
      ;

      dbcout( '--- TFormOutputEvents.stgGridBeginSort done.', DBSHOWMSG );
    end
  ;


  procedure TFormOutputEvents.stgGridEndSort(Sender: TObject; Col: Integer);
    begin
      dbcout( '+++ TFormOutputEvents.stgGridEndSort', DBSHOWMSG );

      if( not _presorting ) then
        begin
          dbcout( '_presorting is false.', DBSHOWMSG );
          cboMainSort.ItemIndex := col;
          _lastColumnSorted := col;

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
      else
        dbcout( '_presorting is false: nothing should happen.', DBSHOWMSG )
      ;

      dbcout( '--- TFormOutputEvents.stgGridEndSort done.', DBSHOWMSG );
    end
  ;


  procedure TFormOutputEvents.sortControlChange(Sender: TObject);
    var
      colToSort: integer;
      s: string;
    begin
      dbcout( '+++ TFormOutputEvents.sortControlChange', DBSHOWMSG );

      if( not( _sortInProgress ) ) then
        begin
          dbcout( 'Sort should occur...', DBSHOWMSG );
          if( rdoAscending.Checked ) then
            stgGrid.SortDirection := sdAscending
          else
            stgGrid.SortDirection := sdDescending
          ;

          dbcout( 'Sorting by column...', DBSHOWMSG );

          // Using strings will make life easier if these options are ever changed
          s := cboMainSort.Items[ cboMainSort.ItemIndex ];

          if( tr( 'Day' ) = s ) then
            colToSort := COL_DAY
          else if( tr( 'Event order on day' ) = s ) then
            colToSort := COL_EVENT
          else if( tr( 'Unit ID' ) = s ) then
            colToSort := COL_HERDID
          else if( tr( 'Unit type') = s ) then
            colToSort := COL_HERD_TYPE
          else if( tr( 'Zone' ) = s ) then
            colToSort := COL_ZONE
          else if( tr( 'Event type' ) = s ) then
            colToSort := COL_EVENTCODE
          else if( tr( 'New state' ) = s ) then
            colToSort := COL_NEWSTATE
          else if( tr( 'Test result' ) = s ) then
            colToSort := COL_TEST_RESULT
          else
            raise exception.create( 'Unrecognized sort index (' + intToStr( cboMainSort.ItemIndex ) + ') in TFormOutputEvents.sortControlChange' );
          ;

          stgGrid.sortByColumn( colToSort );
          _lastColumnSorted := colToSort;
          dbcout( 'Done sorting by column.', DBSHOWMSG );
        end
      else
        dbcout( '_sortInProgress is false: sort won''t occur.', DBSHOWMSG )
      ;

      dbcout( '--- TFormOutputEvents.sortControlChange done.', DBSHOWMSG );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Filtering functions
//-----------------------------------------------------------------------------
  procedure TFormOutputEvents.filter();
    var
      s: string;
    begin
      try
        //lockWindowUpdate( self.Handle );
        self.Perform( WM_SETREDRAW, 0, 0 );

        // Clear the existing filter before attempting to run a new one.
        if( stgGrid.SearchOptions.Filtered ) then
          stgGrid.SearchOptions.Filtered := false
        ;

        s := cboMainFilter.Items[ cboMainFilter.ItemIndex ];

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
        else if( tr( 'Unit ID' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_HERDID;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end

        // Filtering by unit type is taken care of by a different mechanism

        else if( tr( 'Zone' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_ZONE;
            stgGrid.SearchOptions.SearchText := trim( rleTextEntry.Text );
            stgGrid.SearchOptions.Filtered := _filtering;
          end
        else if( tr( 'Event type' ) = s ) then
          begin
            stgGrid.SearchOptions.SearchCol := COL_EVENTCODE;

            if( 0 <= cboEvents.ItemIndex ) then
              begin
                stgGrid.SearchOptions.SearchText := trim( cboEvents.Items[ cboEvents.ItemIndex ] );
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            else // cancel the filtering
              stgGrid.SearchOptions.Filtered := false
            ;
          end
        else if( tr( 'New state' ) = s ) then
          begin
            if( -1 = cboDiseaseState.ItemIndex ) then
              stgGrid.SearchOptions.Filtered := false
            else
              begin
                stgGrid.SearchOptions.SearchCol := COL_NEWSTATE;
                stgGrid.SearchOptions.SearchText := cboDiseaseState.Items.Strings[cboDiseaseState.ItemIndex];
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            ;
          end
        else if( tr( 'Test result' ) = s ) then
          begin
            if( -1 = cboTestResult.ItemIndex ) then
              stgGrid.SearchOptions.Filtered := false
            else
              begin
                stgGrid.SearchOptions.SearchCol := COL_TEST_RESULT;
                stgGrid.SearchOptions.SearchText := cboTestResult.Items.Strings[cboTestResult.ItemIndex];
                stgGrid.SearchOptions.Filtered := _filtering;
              end
            ;
          end
        ;

        updateEventCounter();
        stgGrid.SortByColumn( _lastColumnSorted );
        
        //lockWindowUpdate( 0 );
        self.Perform( WM_SETREDRAW, 1, 0 );
        RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
      except
        // fail silently
        dbcout( '+++ Exception occurred here.', DBSHOWMSG );
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormOutputEvents.cboMainFilterChange(Sender: TObject);
    var
      s: string;
    begin
      // Using strings will make life easier when new options are added.
      s := cboMainFilter.Items[ cboMainFilter.ItemIndex ];

      if( tr( '(No filter)' ) = s ) then
        begin
          lblEventType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := false;

          cboEvents.Visible := false;
          cboDiseaseState.Visible := false;
          cboTestResult.Visible := false;
          rleTextEntry.Visible := false;
          fraAcceptCancel.Visible := false;

          _filtering := false;

          filter();
        end
      else if( tr( 'Day' ) = s ) then
        begin
          lblEventType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'Day:' );
          lblDiseaseState.Visible := false;

          cboEvents.Visible := false;
          cboDiseaseState.Visible := false;
          cboTestResult.Visible := false;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := RE_SIGNED_INTEGER_INPUT;
          rleTextEntry.Visible := true;

          _filtering := true;

          rleTextEntry.SetFocus();
        end
      else if( tr( 'Unit ID' ) = s ) then
        begin
          lblEventType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'Unit ID:' );
          lblDiseaseState.Visible := false;

          cboEvents.Visible := false;
          cboDiseaseState.Visible := false;
          cboTestResult.Visible := false;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := RE_INTEGER_INPUT;
          rleTextEntry.visible := true;
          
          _filtering := true;

          rleTextEntry.SetFocus();
        end

      // Filtering by unit type is taken care of by a different mechanism

      else if( tr( 'Zone' ) = s ) then
        begin
          lblEventType.Visible := false;
          lblTextEntry.Visible := true;
          lblTextEntry.Caption := tr( 'Zone:' );
          lblDiseaseState.Visible := false;

          cboEvents.Visible := false;
          cboDiseaseState.Visible := false;
          cboTestResult.Visible := false;
          rleTextEntry.Text := '';
          rleTextEntry.InputExpression := '';
          rleTextEntry.visible := true;

          _filtering := true;

          rleTextEntry.SetFocus();
        end
      else if( tr( 'Event type' ) = s ) then
        begin
          lblEventType.Visible := true;
          lblTextEntry.Visible := false;
          lblDiseaseState.Visible := false;

          cboEvents.Visible := true;
          cboDiseaseState.Visible := false;
          cboTestResult.Visible := false;
          rleTextEntry.Visible := false;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      else if( tr( 'New state' ) = s ) then
        begin
          lblEventType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Caption := tr( 'Disease state:' );
          lblDiseaseState.Visible := true;

          cboEvents.Visible := false;
          cboDiseaseState.Visible := true;
          cboTestResult.Visible := false;
          rleTextEntry.Visible := false;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      else if( tr( 'Test result' ) = s ) then
        begin
          lblEventType.Visible := false;
          lblTextEntry.Visible := false;
          lblDiseaseState.Caption := tr( 'Test result:' );
          lblDiseaseState.Visible := true;

          cboEvents.Visible := false;
          cboDiseaseState.Visible := false;
          cboTestResult.Visible := true;
          rleTextEntry.Visible := false;

          fraAcceptCancel.Visible := false;

          _filtering := true;

          filter();
        end
      ;
    end
  ;


  procedure TFormOutputEvents.rleTextEntryEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
    end
  ;


  procedure TFormOutputEvents.cboEventsChange(Sender: TObject);
    begin
      filter();
    end
  ;


  procedure TFormOutputEvents.cboDiseaseStateChange(Sender: TObject);
    begin
      filter();
    end
  ;


  procedure TFormOutputEvents.cboTestResultChange(Sender: TObject);
    begin
      filter();
    end
  ;


  procedure TFormOutputEvents.fraAcceptCancelbtnAcceptClick(Sender: TObject);
    var
      s: string;
    begin
      s := cboMainFilter.Items[ cboMainFilter.ItemIndex ];
      
      if( rleTextEntry.Visible ) then
        begin
          // Deal with days
          //---------------
          if( tr( 'Day' ) = s ) then
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
      else
        updateEventCounter()
      ;

      fraAcceptCancel.Visible := false;
    end
  ;


  procedure TFormOutputEvents.fraAcceptCancelbtnCancelClick(Sender: TObject);
    begin
      if( rleTextEntry.Visible ) then
        rleTextEntry.Text := ''
      ;

      fraAcceptCancel.Visible := false;
      stgGrid.SearchOptions.Filtered := false;

      updateEventCounter();
    end
  ;



  procedure TFormOutputEvents.rleExit(Sender: TObject);
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


  procedure TFormOutputEvents.rleKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );
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
//-----------------------------------------------------------------------------



  procedure TFormOutputEvents.FormResize(Sender: TObject);
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
      begin
         self.stgGrid.Height := factor * (self.stgGrid.DefaultRowHeight +
            self.stgGrid.GridLineWidth );
      end
      else
        begin
          self.stgGrid.Height := factor * (self.stgGrid.DefaultRowHeight +
            self.stgGrid.GridLineWidth ) + 4;
        end
      ;

      self.stgGrid.Align := alTop;
      self.pnlEventCounter.Align := alTop;
      self.spacerPanel.Align := alClient;
    end
  ;


  procedure TFormOutputEvents.updateSimComplete();
    begin
      raise exception.Create( 'updateSimComplete is not yet supported in TFormOutputEvents' );
    end
  ;


  procedure TFormOutputEvents.resetIteration( iteration: Integer );
    begin
      if ( cboIteration.Items.IndexOf( IntToStr( iteration ) ) >= 0 ) then
        begin
          cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( iteration ) );
          _displayedIteration := iteration;
          fillOutForm();
          updateEventCounter();
        end
      ;
    end
  ;


  procedure TFormOutputEvents.iterationChanged();
    begin
      if ( assigned( frmMain ) ) then
        frmMain.displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex])
      else
        begin
          _displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex]);
          fillOutForm();
          updateEventCounter();
        end
      ;
    end
  ;


  procedure TFormOutputEvents.setCaption();
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
