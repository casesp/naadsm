unit FormOutputEvents;

(*
FormOutputEvents.pas/dfm
-------------------------
Begin: 2005/02/01
Last revision: $Date: 2008/10/21 18:51:49 $ $Author: areeves $
Version: $Revision: 1.26 $
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
      lblDay: TLabel;
      lblEventType: TLabel;
      lblHerdID: TLabel;
      cboEvents: TComboBox;
      rleDay: TREEdit;
      fraAcceptCancel: TFrameAcceptCancel;
      rleHerdID: TREEdit;
      pnlEventCounter: TPanel;

      pnlSortControls: TPanel;
      lblMainSort: TLabel;
      cboMainSort: TComboBox;
      lblSortOrder: TLabel;
      rdoAscending: TRadioButton;
      rdoDescending: TRadioButton;
    lblIteration: TLabel;
    cboIteration: TComboBox;

      procedure stgGridGetCellFormat(Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
      procedure stgGridBeginSort( Sender: TObject; Col: LongInt; var SortOptions: TSortOptions );
      procedure cboMainFilterChange(Sender: TObject);
      procedure sortControlChange(Sender: TObject);
      procedure rleHerdIDEnter(Sender: TObject);
      procedure rleDayEnter(Sender: TObject);
      procedure cboEventsChange(Sender: TObject);
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);
      procedure rleExit(Sender: TObject);
      procedure rleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure stgGridEndSort(Sender: TObject; Col: Integer);
      procedure FormResize(Sender: TObject);
      procedure cboIterationChange(Sender: TObject);

    protected
      _firstRow: boolean;
      _presorting: boolean;
      _filtering: boolean;
      _creating: boolean;
      _sortInProgress: boolean;
      _lastColumnSorted: integer;

      _sqlRes: TSqlResult;
      _ScrollBarVisibleCheck: boolean;
      _displayedIteration: Integer;
      _inSimComplete: boolean;

      procedure translateUI();

      procedure resetGrid();

      procedure filter();

      procedure updateEventCounter();

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

      { Experimental feature: not currently used }
      procedure appendEvent( evt: TSMEvent );

      procedure resetIteration( iteration: Integer );
      procedure updateSimComplete();     
    end
  ;


  var
    frmOutputEvents: TFormOutputEvents;

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
    DBFORMOUTPUTEVENTS: boolean = false; // set to true to enable debugging messages for this unit.

    COL_DAY: integer = 0;
    COL_EVENT: integer = 1;
    COL_HERDID: integer = 2;
    COL_HERD_TYPE: integer = 3;
    COL_ZONEID: integer = 4;
    COL_EVENTCODE: integer = 5;
    COL_NEWSTATE: integer = 6;
    COL_TRACESUCCESS: integer = 7;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFormOutputEvents.create( AOwner: TComponent; sim: TSMSimulationInput; db: TSMDatabase );
    var
      frm: TForm;
		begin
      dbcout( '+++ TFormOutputEvents.create', DBFORMOUTPUTEVENTS );
      _ScrollBarVisibleCheck := false;
      _creating := true;

      _lastColumnSorted := 0;
      _displayedIteration := frmMain.displayedIteration;

      dbcout( 'Calling inherited creator...', DBFORMOUTPUTEVENTS );
      inherited create( AOwner );
      translateUI();
      
      _inSimComplete := false;
      dbcout( 'Done with inherited creator.', DBFORMOUTPUTEVENTS );

      _presorting := false;
      _sortInProgress := false;
      _filtering := false;
      cboIteration.Enabled := false;

      // Set up widgets
      //---------------
      lblEventType.Top := lblDay.Top;
      lblEventType.Left := lblDay.Left;

      lblHerdID.Top := lblDay.Top;
      lblHerdID.Left := lblDay.Left;

      cboEvents.Top := rleDay.Top;
      cboEvents.Left := rleDay.Left;

      rleHerdID.Top := rleDay.Top;
      rleHerdID.Left := rleDay.left;

      rleDay.InputExpression := RE_SIGNED_INTEGER_INPUT;
      rleHerdID.InputExpression := RE_INTEGER_INPUT;

      pnlEventCounter.Caption := '';
      pnlCaption.Caption := '';
      
      setChartControlsEnabled( false );

      // Initialize pointers
      //--------------------
      _smSim := sim;
      _smdb := db;
      _sqlRes := TSqlResult.create( _smdb as TSqlDatabase );

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
      setupComboBox();

      productionTypeChanged();

      setCaption();

      _creating := false;

      dbcout( '--- TFormOutputEvents.create done.', DBFORMOUTPUTEVENTS );
		end
	;


  procedure TFormOutputEvents.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormOutputEvents.dfm
      // File date: Wed May 2 09:53:35 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Events for 1 iteration' );
          pnlCaption.Caption := tr( 'Iteration status: completed/aborted/running' );
          lblMainFilter.Caption := tr( 'Filter by:' );
          lblDay.Caption := tr( 'Day:' );
          lblEventType.Caption := tr( 'Event type:' );
          lblHerdID.Caption := tr( 'Unit ID:' );
          cboMainFilter.Text := tr( '(No filter)' );
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
          cboMainFilter.Items[2] := tr( 'Unit ID' );
          cboMainFilter.Items[3] := tr( 'Event type' );

          cboEvents.Items[0] := tr( 'Infections' );
          cboEvents.Items[1] := tr( 'State changes' );
          cboEvents.Items[2] := tr( 'Detections' );
          cboEvents.Items[3] := tr( 'Destructions' );
          cboEvents.Items[4] := tr( 'Vaccinations' );
          cboEvents.Items[5] := tr( 'Traces of direct contact' );
          cboEvents.Items[6] := tr( 'Traces of indirect contact' );
          cboEvents.Items[7] := tr( 'Zone changes' );
          cboEvents.Items[8] := tr( 'Creation of zone foci' );

          cboMainSort.Items[0] := tr( 'Day' );
          cboMainSort.Items[1] := tr( 'Event order on day' );
          cboMainSort.Items[2] := tr( 'Unit ID' );
          cboMainSort.Items[3] := tr( 'Unit type' );
          cboMainSort.Items[4] := tr( 'Event type' );
          cboMainSort.Items[5] := tr( 'New state' );
          cboMainSort.Items[6] := tr( 'Successful trace' );
        end
      ;

    end
  ;


	destructor TFormOutputEvents.destroy();
		begin
      _sqlRes.Free();
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
        outDailyEvents.traceSuccess
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
          + '    outDailyEvents.traceSuccess'
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
            stgGrid.Cells[ COL_EVENT, i ] := intToStr( row.field('event') );

            stgGrid.Cells[ COL_HERDID, i ] := intToStr( row.field('herdID') );
            stgGrid.Cells[ COL_HERD_TYPE, i ] := row.field('typeDescr');

            if( null <> row.field('zoneDescr') ) then
              stgGrid.Cells[ COL_ZONEID, i ] := row.field('zoneDescr')
            else
              stgGrid.Cells[ COL_ZONEID, i ] := ''
            ;

            stgGrid.Cells[ COL_EVENTCODE, i ] := TSMEvent.getEventCodeString( row.field('eventCode') );

            if( EVT_TRANSITION_STATE_CHANGE = row.field('eventCode') ) then
              stgGrid.Cells[ COL_NEWSTATE, i ] := transitionStateString( transitionStateFromCode( row.field('newStateCode') ) )
            else
              stgGrid.Cells[ COL_NEWSTATE, i ] := ''
            ;

            if
              ( EVT_TRACED_DIRECT = row.field('eventCode') )
            or
              ( EVT_TRACED_INDIRECT = row.field('eventCode') )
            then
              stgGrid.Cells[ COL_TRACESUCCESS, i ] := uiBoolToText( row.field('traceSuccess') )
            else
              stgGrid.Cells[ COL_TRACESUCCESS, i ] := ''
            ;

            //---------------------------------------------------------------------
            // I'm not positive, but I think the method shown below is
            // considerably slower than writing directly to the table directly.
            //---------------------------------------------------------------------
            (*
            evt := TSMEvent.create(
              row.field('event'),
              row.field('iteration'),
              row.field('day'),
              row.field('herdID'),
              row.field('eventCode')
            );

            if( null <> row.field('newStateCode') ) then evt.newStatus := transitionStateFromCode( row.field('newStateCode') );
            if( null <> row.field('traceSuccess') ) then evt.traceSuccess := row.field('traceSuccess');

            appendEvent( evt, false );

            evt.Free();
            *)
            //---------------------------------------------------------------------

            inc( i );
            row := _sqlRes.fetchArrayNext();
          end
        ;

        if( not( _creating ) ) then
          begin
            // Reapply the previous filter/sort options
            //------------------------------------------
            if( 0 <> _lastColumnSorted ) then
              begin
                dbcout( 'Reapplying sort by column ' + intToStr( _lastColumnSorted ), DBFORMOUTPUTEVENTS );
                stgGrid.SortByColumn( _lastColumnSorted );
              end
            ;

            if( filterUsed ) then
              begin
                dbcout( 'Reapplying filter', DBFORMOUTPUTEVENTS );
                //stgGrid.SearchOptions.Filtered := true;
                filter();
              end
            ;
          end
        ;
    end
  ;


  {*
    This function handles form updates from the database: either when
    a simulation is not running, or when the selected production type is
    changed in midstream.  Function updateForDay() is used for dynamic
    updates.
  }
  procedure TFormOutputEvents.setupFromDatabase();
    var
      row: TSqlRow;
      q: string;
    begin
      dbcout( '+++ TFormOutputEvents.setupFromDatabase...', DBFORMOUTPUTEVENTS );

      resetGrid();
      cboIteration.Items.Clear();

      stgGrid.SearchOptions.Filtered := false;

      if ( ( not frmMain.simIsRunning ) or (_inSimComplete) ) then
        begin
          q := 'SELECT DISTINCT (iteration) FROM outDailyEvents order by iteration desc';
          _sqlRes.runQuery( q );
          row := _sqlRes.fetchArrayFirst();

          if ( row <> nil ) then
            begin
              cboIteration.Enabled := true;
              while ( row <> nil ) do
                begin
                  cboIteration.Items.Add(row.field('iteration'));
                  row := _sqlRes.fetchArrayNext();
                end
              ;

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
                    end
                  ;
                end
              ;
              fillOutForm();
              updateEventCounter();
            end
          else
            begin
              cboIteration.Enabled := false;
              updateEventCounter();
              exit;
            end
          ;
        end
      ;

      dbcout( '--- TFormOutputEvents.setupFromDatabase done', DBFORMOUTPUTEVENTS );
    end
  ;


  procedure TFormOutputEvents.simChanged();
    begin
      freeAndNil( _sqlRes );
      _sqlRes := TSqlResult.create( _smdb as TSqlDatabase );

      setupFromDatabase();
      setCaption();
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
      stgGrid.Cells[ COL_EVENT, row ] := intToStr( evt.event );

      stgGrid.Cells[ COL_HERDID, row ] := intToStr( evt.herdID );
      stgGrid.Cells[ COL_EVENTCODE, row ] := evt.eventCodeString;

      if( EVT_TRANSITION_STATE_CHANGE = evt.eventCode ) then
        stgGrid.Cells[ COL_NEWSTATE, row ] := transitionStateString( evt.newStatus )
      else
        stgGrid.Cells[ COL_NEWSTATE, row ] := ''
      ;

      if
        ( EVT_TRACED_DIRECT = evt.eventCode )
      or
        ( EVT_TRACED_INDIRECT = evt.eventCode )
      then
        stgGrid.Cells[ COL_TRACESUCCESS, row ] := uiBoolToText( evt.traceSuccess )
      else
        stgGrid.Cells[ COL_NEWSTATE, row ] := ''
      ;

      if( 0 <> _lastColumnSorted ) then
        stgGrid.SortByColumn( _lastColumnSorted )
      ;

      updateEventCounter();
    end
  ;


  procedure TFormOutputEvents.productionTypeChanged();
    begin
      dbcout( 'Production type changed!', DBFORMOUTPUTEVENTS );

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

      cboMainFilter.ItemIndex := 0;

      _firstRow := true;

      stgGrid.RowCount := 2;
      stgGrid.ColCount := 8;

      stgGrid.Cells[ COL_DAY, 0 ] := tr( 'Day' );
      stgGrid.Cells[ COL_EVENT, 0 ] := tr( 'Event order on day' );
      stgGrid.Cells[ COL_HERDID, 0 ] := tr( 'Unit ID' );
      stgGrid.Cells[ COL_HERD_TYPE, 0 ] := tr( 'Unit type' );
      stgGrid.Cells[ COL_ZONEID, 0 ] := tr( 'Unit zone' );
      stgGrid.Cells[ COL_EVENTCODE, 0 ] := tr( 'Event type' );
      stgGrid.Cells[ COL_NEWSTATE, 0 ] := tr( 'New state' );
      stgGrid.Cells[ COL_TRACESUCCESS, 0 ] := tr( 'Successful trace' );

      stgGrid.AutoSizeColumns();

      stgGrid.SortByColumn( COL_DAY );

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
      dbcout( '+++ TFormOutputEvents.stgGridBeginSort', DBFORMOUTPUTEVENTS );

      screen.cursor := crHourglass;

      setControlsEnabled( false );

      _sortInProgress := true;

      //LockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      if( not( _presorting ) ) then
        begin
          dbcout( '_presorting was false', DBFORMOUTPUTEVENTS );

          _presorting := true;

          mainSortDirection := stgGrid.SortDirection;

          stgGrid.SortDirection := sdAscending;

          dbcout( COL_EVENT, DBFORMOUTPUTEVENTS );

          if
            ( COL_EVENT <> Col )
          and
            ( COL_DAY <> col )
          then
            begin
              dbcout( 'Presorting by event then day.', DBFORMOUTPUTEVENTS );
              stgGrid.SortByColumn( COL_EVENT );
              stgGrid.SortByColumn( COL_DAY );
            end
          else if( COL_DAY = col ) then
            begin
              dbcout( 'Presorting by event only.', DBFORMOUTPUTEVENTS );
              stgGrid.SortByColumn( COL_EVENT );
            end
          else if( COL_EVENT = col ) then
            begin
              dbcout( 'Presorting by day only.', DBFORMOUTPUTEVENTS );
              stgGrid.SortByColumn( COL_DAY );
            end
          ;

          stgGrid.SortDirection := mainSortDirection;
          
          _presorting := false;
        end
      else
        dbcout( '_presorting was true: nothing should change.', DBFORMOUTPUTEVENTS );
      ;

      dbcout( '--- TFormOutputEvents.stgGridBeginSort done.', DBFORMOUTPUTEVENTS );
    end
  ;


  procedure TFormOutputEvents.stgGridEndSort(Sender: TObject; Col: Integer);
    begin
      dbcout( '+++ TFormOutputEvents.stgGridEndSort', DBFORMOUTPUTEVENTS );

      if( not _presorting ) then
        begin
          dbcout( '_presorting is false.', DBFORMOUTPUTEVENTS );
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
      else
        dbcout( '_presorting is false: nothing should happen.', DBFORMOUTPUTEVENTS )
      ;

      dbcout( '--- TFormOutputEvents.stgGridEndSort done.', DBFORMOUTPUTEVENTS );
    end
  ;


  procedure TFormOutputEvents.sortControlChange(Sender: TObject);
    begin
      dbcout( '+++ TFormOutputEvents.sortControlChange', DBFORMOUTPUTEVENTS );

      if( not( _sortInProgress ) ) then
        begin
          dbcout( 'Sort should occur...', DBFORMOUTPUTEVENTS );
          if( rdoAscending.Checked ) then
            stgGrid.SortDirection := sdAscending
          else
            stgGrid.SortDirection := sdDescending
          ;

          dbcout( 'Sorting by column...', DBFORMOUTPUTEVENTS );
          stgGrid.sortByColumn( cboMainSort.ItemIndex );
          _lastColumnSorted := cboMainSort.ItemIndex;
          dbcout( 'Done sorting by column.', DBFORMOUTPUTEVENTS );
        end
      else
        dbcout( '_sortInProgress is false: sort won''t occur.', DBFORMOUTPUTEVENTS )
      ;

      dbcout( '--- TFormOutputEvents.sortControlChange done.', DBFORMOUTPUTEVENTS );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Filtering functions
//-----------------------------------------------------------------------------
  procedure TFormOutputEvents.filter();
    begin
      try
        //lockWindowUpdate( self.Handle );
        self.Perform( WM_SETREDRAW, 0, 0 );

        // Clear the existing filter before attempting to run a new one.
        if( stgGrid.SearchOptions.Filtered ) then
          stgGrid.SearchOptions.Filtered := false
        ;

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
          2: // Unit ID
            begin
              stgGrid.SearchOptions.SearchCol := COL_HERDID;
              stgGrid.SearchOptions.SearchText := rleHerdID.Text;
              stgGrid.SearchOptions.Filtered := _filtering;
            end
          ;
          3: // Event type
            begin
              stgGrid.SearchOptions.SearchCol := COL_EVENTCODE;

              case cboEvents.ItemIndex of
                0: // Infections
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'Infection' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                1: // State changes
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'State change' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                2: // Detections
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'Detection' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                3: // Destructions
                  begin
                  stgGrid.SearchOptions.SearchText := tr( 'Destruction' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                4: // Vaccinations
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'Vaccination' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                5: // Dir trace
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'Trace-direct' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                6: // Ind trace
                  begin
                    stgGrid.SearchOptions.SearchText := 'Trace-indirect';
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                7: // Zone change
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'Zone change' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                8: // Zone focus creation
                  begin
                    stgGrid.SearchOptions.SearchText := tr( 'Zone focus created' );
                    stgGrid.SearchOptions.Filtered := _filtering;
                  end
                ;
                else // cancel the filtering
                  stgGrid.SearchOptions.Filtered := false
                ;
              end;

            end
          ;
        end;

        updateEventCounter();

        //lockWindowUpdate( 0 );
        self.Perform( WM_SETREDRAW, 1, 0 );
        RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
      except
        // fail silently
        dbcout( '+++ Exception occurred here.', DBFORMOUTPUTEVENTS );
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormOutputEvents.cboMainFilterChange(Sender: TObject);
    begin
      case cboMainFilter.ItemIndex of
        0: // No filter
          begin
            lblEventType.Visible := false;
            lblDay.Visible := false;
            lblHerdID.Visible := false;

            cboEvents.Visible := false;
            rleDay.Visible := false;
            rleHerdID.visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := false;

            filter();
          end
        ;
        1: // Day
          begin
            lblEventType.Visible := false;
            lblDay.Visible := true;
            lblHerdID.Visible := false;

            cboEvents.Visible := false;
            rleDay.Visible := true;
            rleHerdID.visible := false;

            _filtering := true;

            rleDay.SetFocus();
          end
        ;
        2: // Unit ID
          begin
            lblEventType.Visible := false;
            lblDay.Visible := false;
            lblHerdID.Visible := true;

            cboEvents.Visible := false;
            rleDay.Visible := false;
            rleHerdID.visible := true;

            _filtering := true;

            rleHerdID.SetFocus();
          end
        ;
        3: // Event type
          begin
            lblEventType.Visible := true;
            lblDay.Visible := false;
            lblHerdID.Visible := false;

            cboEvents.Visible := true;
            rleDay.Visible := false;
            rleHerdID.visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();
          end
        ;
      end;
    end
  ;


  procedure TFormOutputEvents.rleHerdIDEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
      //fraAcceptCancel.Enabled := true;
    end
  ;


  procedure TFormOutputEvents.rleDayEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
      //fraAcceptCancel.Enabled := true;
    end
  ;


  procedure TFormOutputEvents.cboEventsChange(Sender: TObject);
    begin
      filter();
    end
  ;


  procedure TFormOutputEvents.fraAcceptCancelbtnAcceptClick(Sender: TObject);
    begin
      if( rleDay.Visible ) then
        begin
          if( -2 <> myStrToInt( rleDay.Text, -2 ) ) then filter();
        end
      else if( rleHerdID.Visible ) then
        begin
          if( -2 <> myStrToInt( rleHerdID.Text, -2 ) ) then filter();
        end
      else
        updateEventCounter()
      ;

      fraAcceptCancel.Visible := false;
    end
  ;


  procedure TFormOutputEvents.fraAcceptCancelbtnCancelClick(Sender: TObject);
    begin
      if( rleDay.Visible ) then
        begin
          rleDay.Text := '';
        end
      else if( rleHerdID.Visible ) then
        begin
          rleHerdID.Text := '';
        end
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
      _inSimComplete := true;
      self.simChanged();
      if ( -1 <> frmMain.displayedIteration ) then
        self.resetIteration( frmMain.displayedIteration )
      ;
      _inSimComplete := false;

      setCaption();
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


  procedure TFormOutputEvents.cboIterationChange(Sender: TObject);
    begin
      inherited;
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
