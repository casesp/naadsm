unit FrameHerdListEditor;

(*
FrameHerdListEditor.pas/dfm
---------------------------
Begin: 2005/02/03
Last revision: $Date: 2011-07-12 23:51:06 $ $Author: areeves $
Version number: $Revision: 1.44.2.6 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE ../Defs.inc}

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
    ExtCtrls,
    Grids,
    StdCtrls,
    Menus,

    REEdit,
	  ARSortGrid,

    StringSuperList,
    RegExpDefs,
    FrameAcceptCancel,
    FormLatLonRange,

    Herd,
    ProductionType,
    ProductionTypeList
  ;

  type TUpdateDisplayEvent = procedure( selectedRows: integer ) of object;


  type TFrameHerdListEditor = class( TFrame )
      stgHerds: TARSortGrid;
      cboProdType: TComboBox;
      cboTransitionState: TComboBox;
      pnlMessage: TPanel;

      pnlBeforeProdType: TPanel;
      pnlAfterProdType: TPanel;
      pnlBeforeDiseaseState: TPanel;
      pnlAfterDiseaseState: TPanel;

      pmnPopup: TPopupMenu;
      mnuRemoveUnits: TMenuItem;

      pnlSortControls: TPanel;

      pnlFilterControls: TPanel;
      lblMainFilter: TLabel;
      lblHerdSize: TLabel;
      lblProdTypeFilter: TLabel;
      cboMainFilter: TComboBox;
      cboProdTypeFilter: TComboBox;
      rleHerdSize: TREEdit;
      rleLat: TREEdit;
      fraAcceptCancel: TFrameAcceptCancel;
      rleLon: TREEdit;
      lblLat: TLabel;
      lblLon: TLabel;
      lblDaysLeft: TLabel;
      rleDaysLeft: TREEdit;
      cboStatus: TComboBox;
      lblStatus: TLabel;

      pnlGeoRange: TPanel;
      lblGeoRange: TLabel;
      btnGeoRange: TButton;

      pnlHerdCounter: TPanel;
      lblMessage: TLabel;
      pnlSortControlsLeft: TPanel;
      cboMainSort: TComboBox;
      lblMainSort: TLabel;
      pnlSortControlsRight: TPanel;
      lblSortOrder: TLabel;
      rdoAscending: TRadioButton;
      rdoDescending: TRadioButton;
      btnTest: TButton;

      // GUI event handlers
      //-------------------
      { Prepares the selected cell for data entry }
      procedure stgHerdsSelectCell( Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean );

      { Responds to several CTRL keys keys }
      procedure stgHerdsKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

      { Allows some shortcuts for data entry }
      procedure stgHerdsKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );

      { Sets _tmpText to the contents of the selected cell, in case it needs to be restored later }
      procedure stgHerdsMouseDown( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );

      { Occurs when the user edits the value of a cell.  Validates data while data entry is in progress. }
      procedure stgHerdsSetEditText( Sender: TObject; ACol, ARow: Integer; const Value: String );

      { Validates contents of the selected cell.  Overridden by the containing form. }
      procedure stgHerdsExit( Sender: TObject );

      { This function is currently broken and doesn't do anything }
      procedure stgHerdsDrawCell( Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState );

      { Intercept and cancel key events in the combo boxes }
      procedure cboKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
      procedure cboKeyPress( Sender: TObject; var Key: Char );
      procedure cboKeyUp( Sender: TObject; var Key: Word; Shift: TShiftState );

      { Intercept and properly handle combo box exit events }
      procedure cboExit(Sender: TObject);


      // Data entry
      //------------
      { Changes the production type of the selected herd }
      procedure cboProdTypeChange( Sender: TObject );

      { Changes the transition state of the selected herd }
      procedure cboTransitionStateChange( Sender: TObject );

      procedure pnlBeforeProdTypeEnter(Sender: TObject);
      procedure pnlAfterProdTypeEnter(Sender: TObject);
      procedure pnlBeforeDiseaseStateEnter(Sender: TObject);
      procedure pnlAfterDiseaseStateEnter(Sender: TObject);

      procedure stgHerdsEnter(Sender: TObject);
      procedure mnuRemoveUnitsClick(Sender: TObject);

      procedure stgHerdsBeginSort(Sender: TObject; Col: Integer; var SortOptions: TSortOptions);
      procedure stgHerdsEndSort(Sender: TObject; Col: Integer);
      procedure cboMainFilterChange(Sender: TObject);
      procedure cboFilterChange(Sender: TObject);
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);

      procedure sortControlChange(Sender: TObject);
      procedure rleEnter(Sender: TObject);
      procedure rleExit(Sender: TObject);

      procedure stgHerdsGetCellFormat(Sender: TObject; Col, Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
      procedure cboMainFilterEnter(Sender: TObject);

      procedure rleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure btnTestClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelExit(Sender: TObject);
      procedure btnGeoRangeClick(Sender: TObject);
      procedure stgHerdsScroll(Sender: TObject);
      procedure stgHerdsMouseWheel(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);

    protected
      // Housekeeping function pointers
      //-------------------------------
      _fnUpdateDisplay: TUpdateDisplayEvent;
      _windowLocks: integer;
      _waitCursors: integer;

      // Housekeeping properties
      //------------------------
      _dataUpdated: boolean;
      _initialFocusOccurred: boolean;
      _sortInProgress: boolean;
      _presorting: boolean;
      _filtering: boolean;
      _filterInProgress: boolean;
      _filterValidationInProgress: boolean;
      _standardSortOptions: TSortOptions;
      _rowsSelectedForDeletion: boolean;
      _llFormShown: boolean;
      _rowsDeleted: boolean;

      // Pointers to data structures
      //----------------------------
      _herdList: THerdList;
      _ptList: TProductionTypeList; // Strictly a reference to an existing list

      // Custom forms/dialogs used here
      //-------------------------------
      _llForm: TFormLatLonRange;


      // Variables for internal use
      //---------------------------
      _tmpText: string; // Stores the value of a cell before editing.
                        // This value is restored if invalid data is entered.

      _regExpSuperList: TStringSuperList; // Regular expressions for data entry

      _myForm: TForm; // Pointer to the containing form.
                      // Mostly used for message box placement.

      _lastCboKey: word; // Used to keep track of the last key pressed in a combo box

      _checkingCell: boolean;

      procedure translateUI();      

      // Initialization: called only once per form instance
      //---------------------------------------------------
      { Regular expressions used to validate data entry }
      function createRegExpListStatusDays(): TRegExpList;

      { Sets up the production type combo box }
      procedure setProdTypeList( val: TProductionTypeList );

      { Sets up the transition state combo box }
      procedure setupCboTransitionState();

      { Sets all internal panels to the same color at run time. }
      procedure fixPanelColors( container: TWinControl );

      // Handling the herd list
      //------------------------
      { Returns a pointer to _herdList. WARNING: _herdList will be destroyed, so make a copy! }
      function getHerdList(): THerdList;

      { Creates _herdList as a copy of hList and fills the grid. }
      procedure setHerdList( hList: THerdList );

      { Writes information from the herd list into the grid. }
      procedure writeHerdsToGrid();

      // Data entry
      //------------
      { Records changes after data entry }
      procedure setHerdValue();

      // Simple internal functions
      //--------------------------
      procedure showNoUnitsMessage();
      procedure setControlsEnabled( val: boolean );
      procedure disableEditing();
      procedure enableEditing();
      function zeroHerdsDisplayed(): boolean;
      procedure lockWindow();
      procedure unlockWindow();
      procedure showWaitCursor();
      procedure hideWaitCursor();
      procedure setupLLForm();
      function ShowLLForm(): boolean;

      // Not-so-simple internal functions
      //---------------------------------
      procedure filter();

      // Simple properties
      //------------------
      function getIsReadOnly(): boolean;
      procedure setIsReadOnly( val: boolean );

      function getDataUpdated(): boolean;

      procedure updateHerdCounter();

      function filterHerdInRow( grid: TARSortGrid; iRow: integer ): boolean;

       // Data entry
      //------------
      { Performs data entry validation for the current cell }
      function currentCellOK(): boolean;

    public
      // Construction/destruction
      //--------------------------
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      // Handling the herd list
      //------------------------
      { Changes the herd list, and indicates that a change was made. }
      procedure changeHerdList( hList: THerdList );

      { Adds herds from hList to the existing herd list and fills the grid. }
      procedure addHerdsFromList( hList: THerdList );

      procedure removeSelectedHerds();
      procedure selectAllHerds();

      // Function pointers/event handlers
      //---------------------------------
      property onUpdateDisplay: TUpdateDisplayEvent read _fnUpdateDisplay write _fnUpdateDisplay;

      // Data structure (pointer) properties
      //-------------------------------------
      property herdList: THerdList read getHerdList write setHerdList;
      property prodTypeList: TProductionTypeList write setProdTypeList;

      // Error handling
      //---------------
      procedure showError( msg: string );

      // Simple properties and property-like functions
      //----------------------------------------------
      procedure setForm( frm: TForm );

      property isReadOnly: boolean read getIsReadOnly write setIsReadOnly;
      property dataUpdated: boolean read getDataUpdated;
    end
  ;


implementation

{$R *.dfm}

  uses
    ClipBrd,
    StrUtils,

    ControlUtils,
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    BasicGIS,
    I88n,

    StatusEnums,
    NAADSMLibraryTypes
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to display debugging messages for this unit

  const
    COL_IDX = 0;
    COL_ID = 1;
    COL_PT = 2;
    COL_SIZE = 3;
    COL_LAT = 4;
    COL_LON = 5;
    COL_STATUS = 6;
    COL_DAYS_IN_STATE = 7;
    COL_DAYS_LEFT = 8;

    FILTER_NONE = 0;
    FILTER_PT = 1;
    FILTER_SIZE = 2;
    FILTER_GEO_RANGE = 3;
    FILTER_LAT = 4;
    FILTER_LON = 5;
    FILTER_STATUS = 6;
    FILTER_DAYS_IN_STATE = 7;
    FILTER_DAYS_LEFT = 8;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFrameHerdListEditor.create( AOwner: TComponent );
  var
    factor (*, diff *): integer;
    begin
      inherited create( AOwner );
      translateUI();
      
      fixPanelColors( self );

      _llForm := TFormLatLonRange.create( self );

      btnTest.Visible := {$IFDEF DEBUG} true {$ELSE} false {$ENDIF};

      pnlFilterControls.Height := 33;

      _fnUpdateDisplay := nil;

      _initialFocusOccurred := false;
      _sortInProgress := false;
      _presorting := false;
      _filtering := false;
      _filterValidationInProgress := false;
      _windowLocks := 0;
      _waitCursors := 0;
      _rowsSelectedForDeletion := false;

      _checkingCell := false;

      // Set up widgets
      pnlBeforeProdType.Height := 0;
      pnlAfterProdType.Height := 0;
      pnlBeforeDiseaseState.Height := 0;
      pnlAfterDiseaseState.Height := 0;

      pnlBeforeProdType.Width := 0;
      pnlAfterProdType.Width := 0;
      pnlBeforeDiseaseState.Width := 0;
      pnlAfterDiseaseState.Width := 0;

      lblMessage.Visible := false;

      lblLat.Top := lblHerdSize.Top;
      lblLat.Left := lblHerdSize.Left;

      lblLon.Top := lblHerdSize.Top;
      lblLon.Left := lblHerdSize.Left;

      lblProdTypeFilter.Top := lblHerdSize.Top;
      lblProdTypeFilter.Left := lblHerdSize.Left;

      lblStatus.Top := lblHerdSize.Top;
      lblStatus.Left := lblHerdSize.Left;

      lblDaysLeft.Top := lblHerdSize.Top;
      lblDaysLeft.Left := lblHerdSize.Left;

      rleDaysLeft.Top := rleHerdSize.Top;
      rleDaysLeft.Left := rleherdSize.Left;

      rleLat.Top := rleHerdSize.Top;
      rleLat.Left := rleherdSize.Left;

      rleLon.Top := rleHerdSize.Top;
      rleLon.Left := rleherdSize.Left;

      cboProdTypeFilter.Top := rleHerdSize.Top;
      cboProdTypeFilter.Left := rleherdSize.Left;

      cboStatus.Top := rleHerdSize.Top;
      cboStatus.Left := rleherdSize.Left;

      pnlGeoRange.Top := lblHerdSize.Top - 4;
      pnlGeoRange.Left := lblHerdSize.Left;

      rleHerdSize.InputExpression := RE_INTEGER_INPUT;

      rleLat.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLon.InputExpression := RE_SIGNED_DECIMAL_INPUT;

      rleDaysLeft.InputExpression := RE_INTEGER_INPUT;

      _myForm := nil;
      _herdList := nil;

      _tmpText := '';
      _lastCboKey := 0;
      _dataUpdated := false;
      _llFormShown := false;

      stgHerds.DefaultRowHeight := cboProdType.Height;
      cboProdType.Visible := False;
      
      //Code to fix TStringGrid last column scrolling bug. DO NOT REMOVE!
      // AR 6/23/11: Which bug does this comment refer to?

      factor := stgHerds.Height div (stgHerds.DefaultRowHeight + stgHerds.GridLineWidth);
      //diff := stgHerds.Height - factor*(stgHerds.DefaultRowHeight + stgHerds.GridLineWidth);
      //pnlFilterControls.Height := pnlFilterControls.Height + diff; // AR 6/23/11: Commented this out.
      // It may now be obsolete, and it's not very attractive. If this problem (whatever it is) still exists, we need a better solution.
      stgHerds.Height := factor*(stgHerds.DefaultRowHeight + stgHerds.GridLineWidth);
      pnlFilterControls.Align := alTop;
      stgHerds.Align := alClient;

      /////////////////////////////////////////////////////////////////////////////////////

      setupCboTransitionState();
      cboTransitionState.Visible := false;

      _standardSortOptions.SortStyle := stgHerds.SortStyle;
      _standardSortOptions.SortCaseSensitive := stgHerds.CaseSensitive;
      _standardSortOptions.SortCompare := nil;
      _standardSortOptions.SortDirection := sdAscending;


      // Set up regular expressions used to validate data entry
      _regExpSuperList := TStringSuperList.create();
      _regExpSuperList.append( TRegExpList.create() ); // Order (read-only: no need for reg exp)
      _regExpSuperList.append( TRegExpList.create() ); // ID (read-only: no need for reg exp)
      _regExpSuperList.append( TRegExpList.create() ); // Prod type (uses combo box)
      _regExpSuperList.append( RegExpDefs.createRegExpListPosInteger() ); // Herd size
      _regExpSuperList.append( RegExpDefs.createRegExpListFloat() ); // Lat
      _regExpSuperList.append( RegExpDefs.createRegExpListFloat() ); // Lon
      _regExpSuperList.append( TRegExpList.create() ); // Herd states (uses combo box)
      _regExpSuperList.append( self.createRegExpListStatusDays() ); // Days in state
      _regExpSuperList.append( self.createRegExpListStatusDays() ); // Days left in state
    end
  ;


  procedure TFrameHerdListEditor.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameHerdListEditor.dfm
      // File date: Fri May 4 11:39:04 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          pnlMessage.Caption := tr( '(Change this message)' );
          lblMessage.Caption := tr( 'lblMessage' );
          pnlBeforeProdType.Caption := tr( 'pnlBeforeProdType' );
          pnlAfterProdType.Caption := tr( 'pnlAfterProdType' );
          pnlBeforeDiseaseState.Caption := tr( 'pnlBeforeDiseaseState' );
          pnlAfterDiseaseState.Caption := tr( 'pnlAfterDiseaseState' );
          lblMainSort.Caption := tr( 'Sort by:' );
          lblSortOrder.Caption := tr( 'Sort order:' );
          cboMainSort.Text := tr( 'Order' );
          rdoAscending.Caption := tr( 'Ascending' );
          rdoDescending.Caption := tr( 'Descending' );
          btnTest.Caption := tr( 'btnTest' );
          lblMainFilter.Caption := tr( 'Filter by:' );
          lblHerdSize.Caption := tr( 'Unit size:' );
          lblProdTypeFilter.Caption := tr( 'Production type:' );
          lblLat.Caption := tr( 'Latitude:' );
          lblLon.Caption := tr( 'Longitude:' );
          lblDaysLeft.Caption := tr( 'Days left in state:' );
          lblStatus.Caption := tr( 'Status:' );
          cboMainFilter.Text := tr( '(No filter)' );
          lblGeoRange.Caption := tr( 'Please specify a geographic range.' );
          btnGeoRange.Caption := tr( 'Edit...' );
          pnlHerdCounter.Caption := tr( 'pnlHerdCounter' );
          mnuRemoveUnits.Caption := tr( 'Remove unit' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          cboMainSort.Items[0] := tr( 'Order' );
          cboMainSort.Items[1] := tr( 'ID' );
          cboMainSort.Items[2] := tr( 'Production type' );
          cboMainSort.Items[3] := tr( 'Unit size' );
          cboMainSort.Items[4] := tr( 'Latitude' );
          cboMainSort.Items[5] := tr( 'Longitude' );
          cboMainSort.Items[6] := tr( 'Status' );
          cboMainSort.Items[7] := tr( 'Days in state' );
          cboMainSort.Items[8] := tr( 'Days left in state' );

          cboMainFilter.Items[0] := tr( '(No filter)' );
          cboMainFilter.Items[1] := tr( 'Production type' );
          cboMainFilter.Items[2] := tr( 'Unit size' );
          cboMainFilter.Items[3] := tr( 'Geographic range' );
          cboMainFilter.Items[4] := tr( 'Specific latitude' );
          cboMainFilter.Items[5] := tr( 'Specific longitude' );
          cboMainFilter.Items[6] := tr( 'Status' );
          cboMainFilter.Items[7] := tr( 'Days in state' );
          cboMainFilter.Items[8] := tr( 'Days left in state' );
        end
      ;

    end
  ;

  
  destructor TFrameHerdListEditor.destroy();
    begin
      dbcout( 'Destroying list form', DBSHOWMSG );

      _regExpSuperList.Free();

      _llForm.Release();

      if( nil <> _herdList ) then _herdList.Free();

      // Don't free the production type list: it's just a reference.

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Initialization functions
//-----------------------------------------------------------------------------
  { Handles regular expressions for data entry }
  function TFrameHerdListEditor.createRegExpListStatusDays(): TRegExpList;
    var
      list: TRegExpList;
      tmp: string;
    begin
      // The contents of these cells can be any part of the word "Unspecified",
      // or it can be an integer.  This function generates a list of regular expressions
      // to match any of the allowed patterns.
      list := TRegExpList.create( tr( 'Unspecified' ) );

      // Don't forget to allow integers!
      tmp := list[0];
      tmp := '(^[-+]?\d+$)|' + tmp;
      list.Insert( 0, tmp );
      list.Append( '^[-+]?$' );
      
      result := list;

      // Remember that this list must be destroyed someplace!
    end
  ;


  { Set up the production type combo box with appropriate production type descriptions }
  procedure TFrameHerdListEditor.setProdTypeList( val: TProductionTypeList );
    var
      it: TProductionTypeListIterator;
    begin
      _ptList := val;

      it := TProductionTypeListIterator.create( _ptList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          cboProdType.AddItem( it.current().productionTypeDescr + ' (#' + intToStr( it.current().productionTypeID ) + ')', it.current() );
          cboProdTypeFilter.Items.Add( it.current().productionTypeDescr + ' (#' + intToStr( it.current().productionTypeID ) + ')' );
          it.incr();
        end
      ;

      it.free();
    end
  ;


  procedure TFrameHerdListEditor.setupCboTransitionState();
    begin
      cboTransitionState.Clear();
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateSusceptible ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateLatent ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateSubclinical ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateClinical ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateNaturallyImmune ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateDeadFromDisease ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateVaccineImmune ) );
      cboTransitionState.Items.Add( naadsmDiseaseStateStr( NAADSMStateDestroyed ) );

      cboStatus.Clear();
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateSusceptible ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateLatent ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateSubclinical ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateClinical ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateNaturallyImmune ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateDeadFromDisease ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateVaccineImmune ) );
      cboStatus.Items.Add( naadsmDiseaseStateStr( NAADSMStateDestroyed ) );
    end
  ;


  { Internal panels are different colors for ease of use at design time.
    This function makes them all uniform at run time. }
  procedure TFrameHerdListEditor.fixPanelColors( container: TWinControl );
    var
      i: integer;
      ctrl: TWinControl;
    begin
      for i := 0 to container.ControlCount - 1 do
        begin
          if( container.Controls[i] is TWinControl ) then
            begin
              ctrl := container.Controls[i] as TWinControl;

              if( ctrl is TPanel ) then
                (ctrl as TPanel).Color := clBtnFace
              ;

              fixPanelColors( ctrl );
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data entry
//-----------------------------------------------------------------------------
  { Records changes after data entry }
  procedure TFrameHerdListEditor.setHerdValue();
    var
      c, r: integer;
      index: integer;
      tempDaysInState: integer;
    begin
      r := stgHerds.Row;
      c := stgHerds.Col;

      index := myStrToInt( stgHerds.Cells[ COL_IDX, stgHerds.Row ] );
      dec( index ); // The list is 0-indexed;

      if( ( 0 > index ) or ( _herdList.Count - 1 < index ) ) then
        //raise exception.Create( 'Index (' + intToStr( index ) + ') out of bounds in TFrameHerdListEditor.setHerdValue')
        exit
      ;

      case c of
        COL_IDX, COL_ID:
          // Do nothing
        ;
        COL_PT:
          // Do nothing: production type is taken care of when the combo box changes
        ;
        COL_SIZE:
          begin
            if( myStrToInt( stgHerds.cells[c,r], -1 ) <> _herdList.at(index).initialSize ) then
              begin
                _herdList.at(index).initialSize := myStrToInt( stgHerds.cells[c,r], -1 );
                _dataUpdated := true;
              end
            ;
          end
        ;
        COL_LAT:
          begin
            if( uiStrToFloat( stgHerds.cells[c,r] ) <>  _herdList.at(index).lat ) then
              begin
                _herdList.at(index).setLatLon(
                  uiStrToFloat( stgHerds.cells[COL_LAT,r] ),
                  uiStrToFloat( stgHerds.cells[COL_LON,r] )
                );
                _dataUpdated := true;
              end
            ;
          end
        ;
        COL_LON:
          begin
            if( uiStrToFloat( stgHerds.cells[c,r] ) <> _herdList.at(index).lon ) then
              begin
                _herdList.at(index).setLatLon(
                  uiStrToFloat( stgHerds.cells[COL_LAT,r] ),
                  uiStrToFloat( stgHerds.cells[COL_LON,r] )
                );
                _dataUpdated := true;
              end
            ;
          end
        ;
        COL_STATUS:
          // Do nothing: status is taken care of when the combo box changes
        ;
        COL_DAYS_IN_STATE:
          begin
            dbcout( 'Processing COL_DAYS_IN_STATE with value of ' + stgHerds.cells[c,r], DBSHOWMSG );

            if
              ( 0 = length( trim( stgHerds.Cells[c,r] ) ) )
            or
              ( lower( tr( 'Unspecified' ) ) = lower( trim( stgHerds.cells[c,r] ) ) )
            or
              ( NAADSMStateSusceptible = _herdList.at(index).initialStatus )
            then
              tempDaysInState := -1
            else
              tempDaysInState := strToInt( stgHerds.cells[c,r] )
            ;

            if( tempDaysInState <> _herdList.at(index).daysInInitialState ) then
              begin
                _herdList.at(index).daysInInitialState := tempDaysInState;
                _dataUpdated := true;
              end
            ;
          end
        ;
        COL_DAYS_LEFT:
          begin
            dbcout( 'Processing COL_DAYS_LEFT with value of ' + stgHerds.cells[c,r], DBSHOWMSG );

            if
              ( 0 = length( trim( stgHerds.Cells[c,r] ) ) )
            or
              ( lower( tr( 'Unspecified' ) ) = lower( trim( stgHerds.cells[c,r] ) ) )
            or
              ( NAADSMStateSusceptible = _herdList.at(index).initialStatus )
            or
              ( NAADSMStateDestroyed = _herdList.at(index).initialStatus )
            then
              tempDaysInState := -1
            else
              tempDaysInState := strToInt( stgHerds.cells[c,r] )
            ;

            if( tempDaysInState <> _herdList.at(index).daysLeftInInitialState ) then
              begin
                _herdList.at(index).daysLeftInInitialState := tempDaysInState;
                _dataUpdated := true;
              end
            ;
          end
        ;
      end;

    end
  ;


  { Changes the production type of the selected herd }
  procedure TFrameHerdListEditor.cboProdTypeChange( Sender: TObject );
    var
      listPosition: integer;
    begin
      inherited;
      dbcout( '--- TFrameHerdListEditor.cboProdTypeChange', DBSHOWMSG );

      if( not( zeroHerdsDisplayed ) ) then
        begin
          if( COL_PT <> stgHerds.Col ) then
            begin
              dbcout( '$$$$$$$$$$$$$ FLAKINESS 1', DBSHOWMSG );
              exit;
            end
          ;

          listPosition := myStrToInt( stgHerds.Cells[ COL_IDX, stgHerds.Row ] );
          dec( listPosition ); // The list is 0-indexed;

          if( 0 > listPosition ) then
            exit
          else
            begin
              // Get the ComboBox selection and place in the grid.
              stgHerds.Cells[ COL_PT, stgHerds.Row ] := cboProdType.Items[cboProdType.ItemIndex];

              // Update the selected herd.
              if( -1 < cboProdType.ItemIndex ) then
                begin
                  _herdList.at( listPosition ).setProdType( cboProdType.Items.objects[cboProdType.ItemIndex] as TProductionType );
                  _dataUpdated := true;
                end
              ;
            end
          ;
        end
      ;
    end
  ;


  { Changes the transition state of the selected herd }
  procedure TFrameHerdListEditor.cboTransitionStateChange( Sender: TObject );
    var
      listPosition: integer;
    begin
      inherited;

      if( not( zeroHerdsDisplayed ) ) then
        begin
          if( COL_STATUS <> stgHerds.Col ) then
            begin
              dbcout( '$$$$$$$$$$$$$ FLAKINESS 2', DBSHOWMSG );
              exit;
            end
          ;

          listPosition := myStrToInt( stgHerds.Cells[ COL_IDX, stgHerds.Row ] );
          dec( listPosition ); // The list is 0-indexed;

          if( 0 > listPosition ) then
            exit
          else
            begin
              // Get the ComboBox selection and place in the grid.
              //if( COL_STATUS = stgHerds.Col ) then
                stgHerds.Cells[ COL_STATUS, stgHerds.Row ] := cboTransitionState.Items[cboTransitionState.ItemIndex]
              //else
                //stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] := cboTransitionState.Items[cboTransitionState.ItemIndex]
              ;

              // Update the selected herd.
              case cboTransitionState.ItemIndex of
                0: _herdList.at( listPosition ).initialStatus := NAADSMStateSusceptible;
                1: _herdList.at( listPosition ).initialStatus := NAADSMStateLatent;
                2: _herdList.at( listPosition ).initialStatus := NAADSMStateSubclinical;
                3: _herdList.at( listPosition ).initialStatus := NAADSMStateClinical;
                4: _herdList.at( listPosition ).initialStatus := NAADSMStateNaturallyImmune;
                5: _herdList.at( listPosition ).initialStatus := NAADSMStateDeadFromDisease;
                6: _herdList.at( listPosition ).initialStatus := NAADSMStateVaccineImmune;
                7: _herdList.at( listPosition ).initialStatus := NAADSMStateDestroyed;
                else
                  begin
                    raise exception.create( 'Unrecognized transition state in TFrameHerdListEditor.cboTransitionStateChange()' );
                    _herdList.at( listPosition ).initialStatus := NAADSMStateSusceptible;
                  end
                ;
              end;

              // The block above is a safer way to accomplish what the line below used to do.
              // (Next time someone runs across this function, the line below can be deleted.)
              //_herdList.at( listPosition ).initialStatus := naadsmDiseaseStateFromString( cboTransitionState.Items[cboTransitionState.ItemIndex] );
              _dataUpdated := true;
            end
          ;
        end
      ;
    end
  ;


  { Performs data entry validation for the current cell }
  function TFrameHerdListEditor.currentCellOK(): boolean;
    var
      exprList: TRegExpList;
      tmpDouble: double;
    begin
      if( isReadOnly ) then
        begin
          result := true;
          exit;
        end
      ;

      // Is a row selected?
      if( _rowsSelectedForDeletion ) then
        begin
          result := true;
          exit;
        end
      ;

      // First check for regular expression matching
      exprList := _regExpSuperList.at( stgHerds.Col ) as TRegExpList;

      if( not( _checkingCell ) ) then
        begin
          _checkingCell := true;

          if not( exprList.isFullMatch( trim( stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] ) ) ) then
            begin
              msgOK(
                tr( 'The value in the current cell does not have a valid format.' ) + ' '
                  + tr( 'Please correct this problem before continuing.' ),
                tr( 'Invalid data' ),
                IMGWarning,
                _myForm
              );
              stgHerds.Col := stgHerds.Col;
              stgHerds.Row := stgHerds.Row;
              stgHerds.SetFocus();
              result := false;
              _checkingCell := false;
              exit;
            end
          ;

          // Next, check that lats and lons are within valid limits
          if( COL_LAT = stgHerds.Col ) then
            begin
              tmpDouble := uiStrToFloat( stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] );
              if( ( -90.0 > tmpDouble ) or ( 90.0 < tmpDouble ) ) then
                begin
                  msgOK(
                    tr( 'Latitude values must be between -90 and 90 degrees, inclusive.' ) + ' '
                      + tr( 'Please correct this problem before continuing.' ),
                    tr( 'Invalid data' ),
                    IMGWarning,
                    _myForm
                  );
                  stgHerds.Col := stgHerds.Col;
                  stgHerds.Row := stgHerds.Row;
                  stgHerds.SetFocus();
                  result := false;
                  _checkingCell := false;
                  exit;
                end
              ;
            end
          ;

          if( COL_LON = stgHerds.Col ) then
            begin
              tmpDouble := uiStrToFloat( stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] );
              if( ( -180.0 > tmpDouble ) or ( 180.0 < tmpDouble ) ) then
                begin
                  msgOK(
                    tr( 'Longitude values must be between -180 and 180 degrees, inclusive.' ) + ' '
                      + tr( 'Please correct this problem before continuing.' ),
                    tr( 'Invalid data' ),
                    IMGWarning,
                    _myForm
                  );
                  stgHerds.Col := stgHerds.Col;
                  stgHerds.Row := stgHerds.Row;
                  stgHerds.SetFocus();
                  _checkingCell := false;
                  result := false;
                  exit;
                end
              ;
            end
          ;

          _checkingCell := false;
        end
      ;

      // If the function reaches this point, everything is OK
      setHerdValue();
      result := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Error handling
//-----------------------------------------------------------------------------
  procedure TFrameHerdListEditor.showError( msg: string );
    begin
      pnlSortControls.Visible := false;
      pnlFilterControls.Visible := false;

      pnlBeforeProdType.visible := false;
      pnlAfterProdType.visible := false;
      pnlBeforeDiseaseState.Visible := false;
      pnlAfterDiseaseState.Visible := false;

      stgHerds.Visible := false;
      stgHerds.FixedRows := 0;

      pnlMessage.Align := alClient;
      pnlMessage.Width := self.ClientWidth;
      pnlMessage.Height := self.ClientHeight;
      pnlMessage.Caption := '';

      lblMessage.Visible := true;
      lblMessage.Width := round( 0.6 * pnlMessage.Width );
      lblMessage.Caption := msg;
      horizVertCenterInside( lblMessage, pnlMessage );

      pnlHerdCounter.Visible := false;
      pnlMessage.Visible := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Handling the herd list
//-----------------------------------------------------------------------------
  {*
    Changes the herd list, and indicates that a change was made.
  }
  procedure TFrameHerdListEditor.changeHerdList( hList: THerdList );
    begin
      try
        Screen.Cursor := crHourGlass;
        setHerdList( hList );
        _dataUpdated := true;
      finally
        Screen.Cursor := crDefault;
      end;
    end
  ;


  {*
    Adds herds from hList to the existing herd list and fills the grid.
  }
  procedure TFrameHerdListEditor.addHerdsFromList( hList: THerdList );
    var
      h, newH: THerd;
    begin
      try
        Screen.Cursor := crHourGlass;

        h := hList.first();
        while( nil <> h ) do
          begin
            newH := THerd.create( h, _herdList );

            _herdList.append( newH );
            h := hList.next();
          end
        ;

      _herdList.reproject();

        _dataUpdated := true;

        setupLLForm();

        writeHerdsToGrid();

      finally
        Screen.Cursor := crDefault;
      end;
    end
  ;


  {*
    Creates a copy of hList, and fills the string grid.
  }
  procedure TFrameHerdListEditor.setHerdList( hList: THerdList );
    begin
      dbcout( '*********************TFrameHerdListEditor.setHerdList()', DBSHOWMSG );
      
      if( nil <> _herdList ) then
        _herdList.Free()
      ;

      _herdList := THerdList.create( hList );

      if( ( nil = _herdList ) or ( nil = _ptList ) ) then
        begin
          stgHerds.RowCount := 1;
          exit;
        end
      ;

      setupLLForm();

      writeHerdsToGrid();
    end
  ;


  procedure TFrameherdListEditor.writeHerdsToGrid();
    var
      h: THerd;
      i: integer;

      dummy: boolean;

      gridWasFiltered: boolean;
    begin
      // If there are a ton of herds, punt.
      //-----------------------------------
      if( 65535 < _herdList.Count ) then
        begin
          showError( intToStr( _herdList.count ) + ' units in list.' );

          msgOK(
            ansiReplaceStr( tr( 'There are too many units (xyz) to display practically in this program.' ), 'xyz', intToStr( _herdList.Count ) ) + ' '
              + tr( 'You may use Microsoft Access or a similar application to edit units in your scenario database.' ),
            tr( 'Too many units' ),
            IMGInformation,
            _myForm
          );

          exit;
        end
      ;

      // Set up the grid
      if( 0 = _herdList.Count - _herdList.removedCount ) then
        showNoUnitsMessage()
      else // Fill the grid
        begin
          lockWindow();

          // Remember previous filter settings, if there were any.
          // The filter will be reapplied at the end.
          gridWasFiltered := stgHerds.SearchOptions.Filtered;
          stgHerds.Clear();
          stgHerds.SearchOptions.Filtered := false;

          pnlHerdCounter.Visible := true;

          // Write the header row of the table
          with stgHerds do
            begin
              Cells[ COL_IDX, 0 ] := tr( 'Order' );
              Cells[ COL_ID, 0 ] := tr( 'ID' );
              Cells[ COL_PT, 0 ] := tr( 'Production type' );
              Cells[ COL_SIZE, 0 ] := tr( 'Unit size' );
              Cells[ COL_LAT, 0 ] := tr( 'Latitude' );
              Cells[ COL_LON, 0 ] := tr( 'Longitude' );
              Cells[ COL_STATUS, 0 ] := tr( 'Status' ) + '           '; // Yes, these spaces are intentional.
              Cells[ COL_DAYS_IN_STATE, 0 ] := tr( 'Days in state' );
              Cells[ COL_DAYS_LEFT, 0 ] := tr( 'Days left in state' );
            end
          ;

          stgHerds.RowCount := _herdList.Count - _herdList.removedCount + 1;
          stgHerds.ColCount := COL_DAYS_LEFT + 1;

          stgHerds.Visible := true;
          stgHerds.FixedRows := 1;

          pnlMessage.Visible := false;
          pnlMessage.Align := alNone;

          i := 0;
          h := _herdList.first();
          while( nil <> h ) do
            begin
              if( not( h.removeFromDatabase ) ) then
                begin
                  inc( i );

                  stgHerds.Cells[ COL_IDX, i ] := intToStr( _herdList.currentPosition + 1 );

                  if( -1 <> h.id ) then
                    stgHerds.Cells[ COL_ID, i ] := intToStr( h.id )
                  else
                    stgHerds.Cells[ COL_ID, i ] := ''
                  ;

                  stgHerds.Cells[ COL_PT, i ] := h.prodTypeName + ' (#' + intToStr( h.prodTypeID ) + ')';

                  stgHerds.Cells[ COL_SIZE, i ] := intToStr( h.initialSize );

                  stgHerds.Cells[ COL_LAT, i ] := uiFloatToStr( h.lat, LAT_LON_PRECISION );
                  stgHerds.Cells[ COL_LON, i ] := uiFloatToStr( h.lon, LAT_LON_PRECISION );

                  try
                    stgHerds.Cells[COL_STATUS, i ] := naadsmDiseaseStateStr( h.initialStatus );
                  except
                    stgHerds.Cells[COL_STATUS, i ] := naadsmDiseaseStateStr( NAADSMStateSusceptible );
                    h.initialStatus := NAADSMStateSusceptible;
                  end;

                  if( -1 <> h.daysInInitialState ) then
                    stgHerds.Cells[ COL_DAYS_IN_STATE, i ] := intToStr( h.daysInInitialState )
                  else
                    stgHerds.Cells[ COL_DAYS_IN_STATE, i ] := tr( 'Unspecified' )
                  ;

                  if( -1 <> h.daysLeftInInitialState ) then
                    stgHerds.Cells[ COL_DAYS_LEFT, i ] := intToStr( h.daysLeftInInitialState )
                  else
                    stgHerds.Cells[ COL_DAYS_LEFT, i ] := tr( 'Unspecified' )
                  ;
                end
              ;

              h := _herdList.next();
            end
          ;

          stgHerds.Row := 1;
          stgHerds.Col := COL_PT;
          stgHerds.AutoSizeColumns();

          disableEditing();
          stgHerds.SortByColumn( COL_IDX, _standardSortOptions );
          // Editing will be re-enabled upon the next mouseDown event.

          stgHerdsSelectCell( self, COL_PT, 1, dummy );

          // Reapply filter
          if( gridWasFiltered ) then stgHerds.SearchOptions.Filtered := true;

          updateHerdCounter();

          unlockWindow();
        end
      ;
    end
  ;


  {*
    Returns a pointer to _herdList.

    NOTE: _herdList will be destroyed, so the caller must make a copy if a more permanent
    list is needed.
  }
  function TFrameHerdListEditor.getHerdList(): THerdList; begin result := _herdList; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  {*
    Responds to the following keys:
      ctrl-": copies the contents ot the cell above to the current cell
      ctrl-c: copies the contents of the current cell to the clipboard
      ctrl-p: pastes the contents of the clipboard to the current cell
  }
  procedure TFrameHerdListEditor.stgHerdsKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    var
      clipboardContents: pchar;
    begin
      dbcout( 'Key down: ' + intToStr( key ) + ' ' + stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] , true );

      clipboardContents := '';

      if( isReadOnly ) then exit;

      _tmpText := stgHerds.Cells[ stgHerds.Col, stgHerds.row ];

      if( Shift = [ssCtrl] ) then
        begin
          // ctrl-": copy the contents of the cell above to the current cell
          if( ( 222 = key ) and ( stgHerds.FixedRows < stgHerds.row ) ) then
            stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] := stgHerds.Cells[ stgHerds.Col, stgHerds.row - 1 ]
          ;

          // ctrl-c: copy the contents of the current cell to the clipboard
          if
            ( 67 = key )
          and
            ( stgHerds.FixedRows < stgHerds.row )
          and
            ( stgHerds.FixedCols < stgHerds.col )
          then
            ClipBoard.SetTextBuf( PChar(stgHerds.Cells[ stgHerds.Col, stgHerds.row]) )
          ;

          // ctrl-p: paste the contents of the clipboard to the current cell,
          // as long as it isn't ridiculously long
          if
            ( 80 = key )
          and
            ( stgHerds.FixedRows < stgHerds.row )
          and
            ( stgHerds.FixedCols < stgHerds.col )
          then
            begin
              ClipBoard.getTextBuf( clipboardContents, 255 );
              stgHerds.Cells[ stgHerds.Col, stgHerds.row] := string( clipboardContents );
            end
          ;
        end
      ;

    end
  ;


  procedure TFrameHerdListEditor.stgHerdsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      dbcout( 'Key up: ' + intToStr( key ), true );

      // If the current cell is in Column COL_DAYS_IN_STATE or COL_DAYS_LEFT
      // and if the user is trying to enter "unspecified" then give him a hand.
      //-----------------------------------------------------------------------
      if
        ( ( COL_DAYS_IN_STATE = stgHerds.Col ) or ( COL_DAYS_LEFT = stgHerds.Col ) )
      and
        ( 0 < stgHerds.Row )
      and
        ( 0 < length( stgHerds.Cells[ stgHerds.Col, stgHerds.row] ) )
      and
        ( lower( stgHerds.Cells[ stgHerds.Col, stgHerds.row] ) = lower( ansiLeftStr( tr( 'Unspecified' ), length( stgHerds.Cells[ stgHerds.Col, stgHerds.row] ) ) ) )
      then
        stgHerds.Cells[ stgHerds.Col, stgHerds.row] := tr( 'Unspecified' )
      ;
    end
  ;


  {*
    Occurs when the user edits the value of a cell.  Validates data while data entry is in progress.
  }
  procedure TFrameHerdListEditor.stgHerdsSetEditText(
        Sender: TObject;
        ACol, ARow: Integer;
        const Value: String
      );
    var
      success: boolean;
      exprList: TRegExpList;
    begin
      dbcout( 'stgHerdsSetEditText: ' + value, true );

      if( isReadOnly ) then exit;

      //stgHerds.lockCell();

      exprList := _regExpSuperList.at( ACol ) as TRegExpList;

      success := exprList.isPartialMatch( stgHerds.Cells[ ACol, ARow ] );

      if( not( success ) ) then
        begin
          dbcout( 'No match for "' + stgHerds.Cells[ ACol, ARow ] + '": reverting', true );
          stgHerds.Cells[ ACol, ARow ] := _tmpText;
          stgHerds.setCursorPosition( ACol, ARow, length( stgHerds.cells[ ACol, ARow ] ) );
        end
      else
        dbcout( 'Match found', DBSHOWMSG )
      ;

      //stgHerds.unlockCell();
    end
  ;


  {*
    Sets _tmpText to the contents of the selected cell, in case it needs to be restored later
  }
  procedure TFrameHerdListEditor.stgHerdsMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );
    var
      c, r: integer;
      myRect: TGridRect;
      topR, bottomR: integer;
      cursorPoint: TPoint;

      dummy: boolean;

      si: SCROLLINFO;
    begin
      dbcout( 'TFrameHerdListEditor.stgHerdsMouseDown', DBSHOWMSG );

      if( zeroHerdsDisplayed ) then
        exit
      ;

      _tmpText := stgHerds.Cells[ stgHerds.Col, stgHerds.row ];

      getScrollInfo( stgHerds.Handle, SB_HORZ, si );
      dbcout2( si.nTrackPos );

      stgHerds.MouseToCell( x, y, c, r );
      dbcout2( 'Mouse down in Col: ' + intToStr( c ) + ', Row: ' + intToStr( r ) + ', leftCol: ' + intToStr( stgHerds.LeftCol ), DBSHOWMSG );

      // Default to single-cell selection
      stgHerds.Options := stgHerds.Options - [ goRowSelect ] + [ goAlwaysShowEditor ];

      // If the click occurred out of bounds, then exit.
      if
        ( 0 > c )
      or
        ( 0 > r )
      or
        ( stgHerds.ColCount - 1 < c )
      or
        ( stgHerds.RowCount - 1 < r )
      then
        exit
      ;


      // If the top row is empty then exit
      if( 0 = length( trim( stgHerds.Cells[ COL_IDX, 1 ] ) ) ) then
        exit
      ;


      // If the click occurred in the header row, disable editing and get ready to sort.
      if
        ( r = 0 )
      and
        ( c >= 0 )
      and
        ( c <= COL_DAYS_LEFT )
      and
        ( not( _sortInProgress ) )
      then
        begin
          disableEditing();
          exit;
        end
      ;


      // If the click occurred anywhere else, get ready to edit.

      enableEditing();

      myRect.Top := 0;
      myRect.Bottom := -1;
      myRect.Left := COL_IDX;
      myRect.Right := stgHerds.ColCount - 1;

      if( ( COL_IDX = c ) or ( COL_ID = c ) ) then
        begin
          // ROW SELECTION
          stgHerds.Options := stgHerds.Options + [ goRowSelect, goAlwaysShowEditor ];

          if( cboProdType.Visible ) then
            begin
              cboProdType.Visible := false;
              stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] := cboProdType.Items[ cboProdType.ItemIndex ];
            end
          ;

          if( cboTransitionState.Visible ) then
            begin
              cboTransitionState.Visible := false;
              stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] := cboTransitionState.Items[ cboTransitionState.ItemIndex ];
            end
          ;

          if( ssShift in shift ) then
            begin
              // Select multiple contiguous rows

              if( r < stgHerds.Selection.Top ) then
                topR := r
              else
                topR := stgHerds.Selection.Top
              ;

              if( r > stgHerds.Selection.Bottom ) then
                bottomR := r
              else
                bottomR := stgHerds.Selection.Bottom
              ;

              myRect.Top := topR;
              myRect.Bottom := bottomR;

              stgHerds.selection := myRect;
            end
          else if( ( 0 < stgHerds.Selection.Bottom - stgHerds.Selection.Top ) and ( mbRight = button ) ) then
            // Do nothing: don't change the exiting multi-selection
          else
            begin
              // Select a single row

              myRect.Top := r;
              myRect.Bottom := r;

              stgHerds.selection := myRect;
            end
          ;


          if( 0 < stgHerds.Selection.Bottom - stgHerds.Selection.Top ) then
            pmnPopup.Items[0].Caption := tr( '&Remove selected units' )
          else
            pmnPopup.Items[0].Caption := tr( '&Remove selected unit' )
          ;

          if( assigned( _fnUpdateDisplay ) ) then _fnUpdateDisplay( 1 );

          _rowsSelectedForDeletion := true;

          if( mbRight = button ) then
            begin
              getCursorPos( cursorPoint );
              pmnPopup.Popup( cursorPoint.X, cursorPoint.Y );
          end
          ;
        end
      else
        begin
          // SINGLE CELL SELECTION

          if( stgHerds.FixedRows > r ) then r := stgHerds.FixedRows;

          myRect.Left := c;
          myRect.Top := r;
          myRect.Right := c;
          myRect.Bottom := r;

          stgHerds.Options := stgHerds.Options - [ goRowSelect ] + [ goAlwaysShowEditor ];
          
          stgHerds.selection := myRect;

          stgHerdsSelectCell( self, c, r, dummy );
        end
      ;
    end
  ;


  {*
    Validates contents of the selected cell.
  }
  procedure TFrameHerdListEditor.stgHerdsExit(Sender: TObject);
    begin
      dbcout( '********** TFrameHerdListEditor.stgHerdsExit', DBSHOWMSG );
      currentCellOK();
      dbcout( '********** Done TFrameHerdListEditor.stgHerdsExit', DBSHOWMSG );
    end
  ;


  {*
    Prepares the selected cell for data entry
  }
  procedure TFrameHerdListEditor.stgHerdsSelectCell(
        Sender: TObject;
        ACol, ARow: Integer;
        var CanSelect: Boolean
      );
    var
      fixedColsWidth: integer;
      leftColsWidth: integer;
      rowHeight: integer;
      i: integer;
    begin
      dbcout( '--- TFrameHerdListEditor.stgHerdsSelectCell', DBSHOWMSG );

      (*
      if( isReadOnly ) then
        begin
          dbcout( '--- I am read-only: stgHerdsSelectCell will exit' );
          exit;
        end
      ;
      *)

      if( _filterInProgress or _sortInProgress or zeroHerdsDisplayed() ) then
        begin
          dbcout( '--- stgHerdsSelectCell will not occur!', DBSHOWMSG );
          exit;
        end
      else
        dbcout( '--- stgHerdsSelectCell should occur...', DBSHOWMSG )
      ;

      dbcout( '--- stgHerdsSelectCell', DBSHOWMSG );

      // Row height is constant across all rows.
      rowHeight := stgHerds.RowHeights[0] + stgHerds.GridLineWidth;
      // Width of fixed columns is also constant.
      fixedColsWidth := stgHerds.ColWidths[0] + stgHerds.ColWidths[1] + ( 3 * stgHerds.GridLineWidth );

      // Set up combo box for production type, if needed
      //------------------------------------------------
      if( ( COL_PT = ACol ) and ( 0 <> ARow ) ) then
        begin
          dbcout( endl + 'Showing PT combo box', DBSHOWMSG );

          // Size and position the combo box to fit the cell, then show it.
          // Left and top will change, depending on the scroll position
          // of the string grid, and will need to be adjusted.

          // As noted above, row height is constant across all rows, so the top is relatively straight-forward:
          cboProdType.Top :=  stgHerds.Top + 1 + ( rowHeight * ( ARow - stgHerds.TopRow + stgHerds.FixedRows ) );

          // The left position will depend on how many columns are visible to the left.
          leftColsWidth := 0;
          for i := stgHerds.LeftCol to ACol - 1 do
            leftColsWidth := leftColsWidth + stgHerds.ColWidths[i] + 1
          ;
          cboProdType.Left := stgHerds.Left + 1 + leftColsWidth + fixedColsWidth;

          // Height and width should be OK
          cboProdType.Width := stgHerds.ColWidths[COL_PT];
          cboProdType.Height := rowHeight + 8;

          cboProdType.ItemIndex := cboProdType.Items.IndexOf( stgHerds.Cells[ACol, ARow] );
          cboProdType.Visible := True;
          if( _myForm.Visible ) then cboProdType.SetFocus();

          dbcout( 'Done showing PT combo box' + endl, DBSHOWMSG );
        end
      else
        cboProdType.Visible := false
      ;

      // Set up combo box for transition state, if needed
      //-------------------------------------------------
      if( ( COL_STATUS = ACol ) and ( 0 <> ARow ) ) then
        begin
          dbcout( endl + 'Showing status combo box', DBSHOWMSG );

          // Size and position the combo box to fit the cell, then show it.
          // Left and top will change, depending on the scroll position
          // of the string grid, and will need to be adjusted.

          // As noted above, row height is constant across all rows, so the top is relatively straight-forward:
          cboTransitionState.Top :=  stgHerds.Top + 1 + ( rowHeight * ( ARow - stgHerds.TopRow + stgHerds.FixedRows ) );

          // The left position will depend on how many columns are visible to the left.
          leftColsWidth := 0;
          for i := stgHerds.LeftCol to ACol - 1 do
            leftColsWidth := leftColsWidth + stgHerds.ColWidths[i] + 1
          ;
          cboTransitionState.Left := stgHerds.Left + 1 + leftColsWidth + fixedColsWidth;

          // Height and width should be OK
          cboTransitionState.Width := stgHerds.ColWidths[COL_STATUS];
          cboTransitionState.Height := rowHeight + 8;

          cboTransitionState.ItemIndex := cboTransitionState.Items.IndexOf(stgHerds.Cells[ACol, ARow]);
          cboTransitionState.Visible := True;
          cboTransitionState.SetFocus();
        end
      else
        cboTransitionState.Visible := false
      ;

      // Validate the current cell before
      // allowing the selection of a new cell
      //--------------------------------------
      canSelect := currentCellOK();

      stgHerds.selectCellContents();

      dbcout( '--- Done TFrameHerdListEditor.stgHerdsSelectCell', DBSHOWMSG );
    end
  ;


  procedure TFrameHerdListEditor.stgHerdsScroll(Sender: TObject);
    begin
      dbcout( 'Scrolling!', DBSHOWMSG );
      //rbh Fix for issue 2356; Aaron was not certain that disable editing is needed here.
      //disableEditing();
    end
  ;


  procedure TFrameHerdListEditor.stgHerdsMouseWheel(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    begin
      dbcout( 'Mouse wheel!', true );
      //rbh Fix for issue 2356; Aaron was not certain that disable editing is needed here.
      //disableEditing();
    end
  ;


  procedure TFrameHerdListEditor.cboKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    begin
      _lastCboKey := key;

      if( ( 32 > key ) or ( 126 < key ) ) then key := 0;
    end
  ;


  procedure TFrameHerdListEditor.cboKeyPress( Sender: TObject; var Key: Char );
    begin
      _lastCboKey := ord( key );

      if( ( 32 > ord( key ) ) or ( 126 < ord( key ) ) ) then key := char( 0 );
    end
  ;


  procedure TFrameHerdListEditor.cboKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      _lastCboKey := key;

      if( ( 32 > key ) or ( 126 < key ) ) then
        key := 0
      ;
    end
  ;


  procedure TFrameHerdListEditor.cboExit(Sender: TObject);
    var
      myRect: TGridRect;
    begin
      dbcout( '--- TFrameHerdListEditor.cboExit...', DBSHOWMSG );
      dbcout( '*** Exit received by combo box', DBSHOWMSG );
      dbcout( 'Col: ' + intToStr( stgHerds.Col ) + ' Row: ' + intToStr( stgHerds.Row ), DBSHOWMSG );
      dbcout( '_lastCboKey: ' + intToStr( _lastCboKey ), DBSHOWMSG );

      // This function just updates the display.  Data structures are taken
      // care of elsewhere (e.g. when the combo box changes).
      if( ( sender as TComboBox ).Visible ) then
        begin
          if( sender = cboProdType ) then
            begin
              cboProdType.Visible := false;
              stgHerds.Cells[ COL_PT, stgHerds.Row ] := cboProdType.Items[ cboProdType.ItemIndex ];
            end
          else if( sender = cboTransitionState ) then
            begin
              cboTransitionState.Visible := false;
              stgHerds.Cells[ COL_STATUS, stgHerds.Row ] := cboTransitionState.Items[ cboTransitionState.ItemIndex ];
            end
          ;

          stgHerds.Invalidate();
          stgHerds.Repaint();

          myRect.Left := stgHerds.Col + 1;
          myRect.Top := stgHerds.Row;
          myRect.Right := stgHerds.Col + 1;
          myRect.Bottom := stgHerds.Row;
          stgHerds.Selection := myRect;
        end
      ;
    end
  ;


  procedure TFrameHerdListEditor.pnlBeforeProdTypeEnter(Sender: TObject);
    begin
      // Tabbing backward out of cboProdType
      dbcout( 'Tabbing backward out of cboProdType', DBSHOWMSG );
      if( 1 < stgHerds.Row ) then stgHerds.Row := stgHerds.Row - 1;

      if( _initialFocusOccurred ) then
        stgHerds.Col := stgHerds.ColCount - 1
      else
        begin
          _initialFocusOccurred := true;
          stgHerds.Col := COL_SIZE;
        end
      ;
      stgHerds.SetFocus();
    end
  ;


  procedure TFrameHerdListEditor.pnlAfterProdTypeEnter(Sender: TObject);
    begin
      // Tabbing forward out of cboProdType
      dbcout( 'Tabbing forward out of cboProdType', DBSHOWMSG );
      stgHerds.SetFocus();
    end
  ;


  procedure TFrameHerdListEditor.pnlBeforeDiseaseStateEnter(Sender: TObject);
    begin
      // Tabbing backward out of cboTransitionState
      dbcout( 'Tabbing backward out of cboTransitionState', DBSHOWMSG );
      stgHerds.Col := COL_STATUS - 1;
      stgHerds.SetFocus();
    end
  ;


  procedure TFrameHerdListEditor.pnlAfterDiseaseStateEnter(Sender: TObject);
    begin
      // Tabbing forward out of cboTransitionState
      dbcout( 'Tabbing forward out of cboTransitionState', DBSHOWMSG );
      stgHerds.SetFocus();
    end
  ;


  procedure TFrameHerdListEditor.stgHerdsEnter(Sender: TObject);
    var
      myRect: TGridRect;
    begin
      dbcout( '--- TFrameHerdListEditor.stgHerdsEnter', DBSHOWMSG );

      _initialFocusOccurred := true;

      myRect.Top := stgHerds.Row;
      myRect.Bottom := stgHerds.Row;

      if( COL_PT >= stgHerds.Col ) then
        begin
          myRect.Left := COL_SIZE;
          myRect.Right := COL_SIZE;
        end
      else if( COL_STATUS = stgHerds.Col ) then
        begin
          myRect.Left := COL_DAYS_IN_STATE;
          myRect.Right := COL_DAYS_IN_STATE;
        end
      else
        begin
          myRect.Left := stgHerds.Col;
          myRect.Right := stgHerds.Col;
        end
      ;

      stgHerds.Selection := myRect;

      dbcout( '--- Done TFrameHerdListEditor.stgHerdsEnter', DBSHOWMSG );
    end
  ;


  procedure TFrameHerdListEditor.mnuRemoveUnitsClick(Sender: TObject);
    begin
      removeSelectedHerds();
    end
  ;


  procedure TFrameHerdListEditor.selectAllHerds();
    var
      myRect: TGridRect;
    begin
      myRect.Top := stgHerds.FixedRows;
      myRect.Bottom := stgHerds.RowCount - 1;
      myRect.Left := COL_IDX;
      myRect.Right := stgHerds.ColCount - 1;

      stgHerds.selection := myRect;

      if( assigned( _fnUpdateDisplay ) ) then _fnUpdateDisplay( 1 );
      _rowsSelectedForDeletion := true;
    end
  ;


  procedure TFrameHerdListEditor.removeSelectedHerds();
    var
      i: integer;
      listPosition: integer;
      lotsOfRows: boolean;
    begin
      dbcout( 'TFrameHerdListEditor.removeSelectedHerds', DBSHOWMSG );

      if( assigned( _fnUpdateDisplay ) ) then _fnUpdateDisplay( 0 );

      //_rowsSelectedForDeletion := false;

      setControlsEnabled( false );

      showWaitCursor();

      lockWindow();

      // If there aren't very many herds, we don't need to be too concerned about efficiency.
      if( 300 > _herdList.Count ) then
        lotsOfRows := false
      else
        begin
          // If we have quite a few selected rows, deleting rows can be very slow.
          // It might be faster to rewrite the entire grid than to individually delete rows.
          // It doesn't take many rows to add up to "quite a few" when filtering is enabled.

          if
            ( ( stgHerds.Selection.Bottom - stgHerds.Selection.Top ) > 10 )
          and
            ( stgHerds.SearchOptions.Filtered )
          then
            lotsOfRows := true
          else if ( ( stgHerds.Selection.Bottom - stgHerds.Selection.Top ) > 10 ) then
            lotsOfRows := true
          else
            lotsOfRows := false
          ;
        end
      ;

      dbcout( lotsOfRows, DBSHOWMSG );


      for i := stgHerds.Selection.Bottom downto stgHerds.Selection.Top do
        begin
          dbcout( '*** Removing herd', DBSHOWMSG );

          listPosition := myStrToInt( stgHerds.Cells[ COL_IDX, i ] );
          dec( listPosition ); // The list is 0-indexed, but for the user's sake counting starts at 1;

          _herdList.scheduleHerdRemoval( listPosition );
        end
      ;

      if( not( lotsOfRows ) ) then
        begin
          dbcout( 'Not lots of rows...', DBSHOWMSG );
          stgHerds.DeleteRows( stgHerds.Selection.Top, stgHerds.Selection.Bottom );

          updateHerdCounter();

          if( stgHerds.FixedRows + 1 = stgHerds.RowCount ) then
            begin
              if( 0 = length( trim( stgHerds.Cells[ COL_IDX, 1 ] ) ) ) then
                begin
                  if( not( stgHerds.SearchOptions.Filtered ) ) then
                    showNoUnitsMessage()
                  ;
                end
              ;
            end
          ;
        end
      else
        begin
          dbcout( 'Lots of rows!', DBSHOWMSG );
          writeHerdsToGrid();
        end
      ;

      setControlsEnabled( true );

      _rowsSelectedForDeletion := false;

      disableEditing();

      unlockWindow();

      hideWaitCursor();
    end
  ;


  procedure TFrameHerdListEditor.disableEditing();
    begin
      dbcout( '=== TFrameHerdListEditor.disableEditing', DBSHOWMSG );

      cboProdType.Visible := false;
      cboTransitionState.Visible := false;

      stgHerds.Options := stgHerds.Options - [goEditing, goAlwaysShowEditor];
      stgHerds.EditorMode := false;
    end
  ;


  procedure TFrameHerdListEditor.enableEditing();
    begin
      dbcout( '=== TFrameHerdListEditor.enableEditing', DBSHOWMSG );

      if( not( zeroHerdsDisplayed() ) ) then
        begin
          stgHerds.EditorMode := true;
          stgherds.Options := stgherds.Options + [goEditing, goAlwaysShowEditor];
        end
      else
        begin
          stgHerds.EditorMode := false;
          stgherds.Options := stgherds.Options - [goEditing, goAlwaysShowEditor];
        end
      ;
    end
  ;


  procedure TFrameHerdListEditor.stgHerdsBeginSort(
      Sender: TObject;
      Col: Integer;
      var SortOptions: TSortOptions
    );
    var
      mainSortDirection: TSortDirection;
    begin
      dbcout( '+++ TFrameHerdListEditor.stgHerdsBeginSort...', DBSHOWMSG );

      if( 100 < _herdList.Count ) then setControlsEnabled( false );

      _sortInProgress := true;

      if( not( _presorting ) ) then
        begin
          showWaitCursor();
          lockWindow();

          dbcout( 'Beginning presort:', DBSHOWMSG );
          _presorting := true;

          mainSortDirection := stgHerds.SortDirection;

          stgHerds.SortDirection := sdAscending;

          if
            ( COL_IDX <> Col )
          then
            begin
              dbcout( 'Presorting by herd index.', DBSHOWMSG );
              stgHerds.SortByColumn( COL_IDX, _standardSortOptions );
            end
          ;

          stgHerds.SortDirection := mainSortDirection;

          _presorting := false;

          dbcout( 'Done with preSort.', DBSHOWMSG );
        end
      else
        dbcout( '_presorting already in progress.', DBSHOWMSG )
      ;

      dbcout( '--- Done TFrameHerdListEditor.stgHerdsBeginSort', DBSHOWMSG );
    end
  ;


  procedure TFrameHerdListEditor.stgHerdsEndSort(Sender: TObject; Col: Integer);
    begin
      dbcout( '+++ TFrameHerdListEditor.stgHerdsEndSort...', DBSHOWMSG );

      if( not _presorting ) then
        begin
          dbcout( 'Not presorting: endSort will occur.', DBSHOWMSG );

          cboMainSort.ItemIndex := col;

          if( sdAscending = stgHerds.SortDirection ) then
            rdoAscending.Checked := true
          else
            rdoDescending.Checked := true
          ;

          hideWaitCursor();

          _sortInProgress := false;

          setControlsEnabled( true );

          unlockWindow();
        end
      else
        dbcout( 'presort in progress.', DBSHOWMSG )
      ;

      dbcout( '--- Done TFrameHerdListEditor.stgHerdsEndSort', DBSHOWMSG );
    end
  ;
    

  procedure TFrameHerdListEditor.showNoUnitsMessage();
    begin
      stgHerds.Visible := false;
      stgHerds.FixedRows := 0;

      pnlHerdCounter.Visible := false;

      pnlMessage.Align := alClient;
      pnlMessage.Caption := tr( '(No units exist in this scenario)' );
      pnlMessage.Visible := true;

      setControlsEnabled( false );
    end
  ;

  procedure TFrameHerdListEditor.setControlsEnabled( val: boolean );
    begin
      setChildrenEnabled( pnlFilterControls, val, false );
      setChildrenEnabled( pnlSortControls, val, false );

      //setDataOptionsEnabled( val );
      //setChartOptionsEnabled( false );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Simple properties
//-----------------------------------------------------------------------------
  procedure TFrameHerdListEditor.setForm( frm: TForm );
    begin
      _myForm := frm;
    end
  ;


  function TFrameHerdListEditor.getIsReadOnly(): boolean;
    begin
      result := not( goEditing in stgHerds.Options );
    end
  ;


  procedure TFrameHerdListEditor.setIsReadOnly( val: boolean );
    begin
      // Don't allow editing if the frame is read-only

      if( val ) then
        stgHerds.Options := stgHerds.Options - [goEditing]
      else
        stgherds.Options := stgherds.Options + [goEditing]
      ;
    end
  ;


  function TFrameHerdListEditor.getDataUpdated(): boolean;
    begin
      if( nil <> _herdList ) then
        result := _herdList.updated
      else
        result := false
      ;
    end
  ;
//-----------------------------------------------------------------------------



//*****************************************************************************
// Revision needed
//*****************************************************************************
  procedure TFrameHerdListEditor.stgHerdsDrawCell(
        Sender: TObject;
        ACol, ARow: Integer;
        Rect: TRect;
        State: TGridDrawState
      );
    (*
    var
      r: TRect;
    *)
    begin
      //dbcout( 'Current col: ' + intToStr( stgHerds.Col ) + ', current row: ' + intToStr( stgHerds.Row ) );

      // This block resizes the combo box (which was the point), but also
      // causes stgHerdsDrawCell to be called recursively by an infinite loop.
      // I'm not sure yet how to fix that.
      (*
      if( ( cboProdType.Visible ) and ( stgHerds.Col = 2 ) and ( stgHerds.Row <> 0 ) ) then
        begin
          dbcout( 'Resizing combo box' );

          {Size and position the combo box to fit the cell}
          R := stgHerds.CellRect( stgHerds.Col, stgHerds.Row );
          R.Left := R.Left + stgHerds.Left;
          R.Right := R.Right + stgHerds.Left;
          R.Top := R.Top + stgHerds.Top;
          R.Bottom := R.Bottom + stgHerds.Top;

          {Show the combobox}
          with cboProdType do
            begin
              Left := R.Left + 1;
              Top := R.Top + 1;
              Width := (R.Right + 2) - R.Left;
              Height := (R.Bottom + 2) - R.Top;

              //ItemIndex := Items.IndexOf(stgHerds.Cells[stgHerds.Col, stgHerds.row]);
              //Visible := True;
              //SetFocus;
            end
          ;

        end
      ;
      *)
    end
  ;
//*****************************************************************************







  procedure TFrameHerdListEditor.cboMainFilterChange(Sender: TObject);
    var
      useLLRange: boolean;
    begin
      dbcout( '--- TFrameHerdListEditor.cboMainFilterChange: ' + intToStr( cboMainFilter.ItemIndex ), DBSHOWMSG );

      disableEditing();

      case cboMainFilter.ItemIndex of
        FILTER_NONE:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := false;

            filter();
          end
        ;
        FILTER_PT: // Production type
          begin
            lblProdTypeFilter.Visible := true;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := true;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();
          end
        ;
        FILTER_SIZE:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := true;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := true;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := true;

            _filtering := true;

            rleHerdSize.SetFocus();
          end
        ;
        FILTER_GEO_RANGE:
          begin
            if( not( _llFormShown ) ) then
              useLLRange := showLLForm()
            else
              useLLRange := true
            ;

            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := true;

            fraAcceptCancel.Visible := false;

            _filtering := useLLRange;
            filter();
          end
        ;
        FILTER_LAT:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := true;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := true;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := true;

            _filtering := true;

            rleLat.SetFocus();
          end
        ;
        FILTER_LON:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := true;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := true;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := true;

            _filtering := true;

            rleLon.SetFocus();
          end
        ;
        FILTER_STATUS:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := true;
            lblDaysLeft.Visible := false;

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := true;
            rleDaysLeft.Visible := false;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := false;

            _filtering := true;

            filter();
          end
        ;
        FILTER_DAYS_IN_STATE:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := true;
            lblDaysLeft.Caption := tr( 'Days in state:' );

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := true;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := true;

            _filtering := true;

            rleDaysLeft.SetFocus();
          end
        ;
        FILTER_DAYS_LEFT:
          begin
            lblProdTypeFilter.Visible := false;
            lblHerdSize.Visible := false;
            lblLat.Visible := false;
            lblLon.Visible := false;
            lblStatus.Visible := false;
            lblDaysLeft.Visible := true;
            lblDaysLeft.Caption := tr( 'Days left in state:' );

            cboProdTypeFilter.Visible := false;
            rleHerdSize.Visible := false;
            rleLat.Visible := false;
            rleLon.Visible := false;
            cboStatus.Visible := false;
            rleDaysLeft.Visible := true;

            pnlGeoRange.Visible := false;

            fraAcceptCancel.Visible := true;

            _filtering := true;

            rleDaysLeft.SetFocus();
          end
        ;
      end;

      dbcout( '-- Done TFrameHerdListEditor.cboMainFilterChange().', DBSHOWMSG );
    end
  ;



procedure TFrameHerdListEditor.cboFilterChange(Sender: TObject);
begin
  filter();
end;

  procedure TFrameHerdListEditor.fraAcceptCancelbtnAcceptClick(Sender: TObject);
    var
      d: double;
      i: integer;
    begin
      if( _filterValidationInProgress ) then
        exit
      ;

      _filterValidationInProgress := true;

      if( rleHerdSize.Visible ) then
        begin
          i := myStrToInt( rleHerdSize.Text, -2 );

          if( 0 > i ) then
            begin
              msgOK( 
                tr( 'Please specify a valid unit size' ), 
                tr( 'No unit size' ), 
                IMGWarning, 
                _myForm
              );
              rleHerdSize.SetFocus();
              fraAcceptCancel.Visible := true;
            end
          else
            begin
              filter();
              fraAcceptCancel.Visible := false;
            end
          ;
        end
      else if( rleDaysLeft.Visible ) then
        begin
          i := myStrToInt( rleDaysLeft.Text, -2 );

          if( 0 > i ) then
            begin
              msgOK( 
                tr( 'Please specify a valid number of days' ), 
                tr( 'Days unspecified' ),
                IMGWarning, 
                _myForm 
              );
              rleDaysLeft.SetFocus();
              fraAcceptCancel.Visible := true;
            end
          else
            begin
              filter();
              fraAcceptCancel.Visible := false;
            end
          ;
        end
      else if( rleLat.Visible ) then
        begin
          d := uiStrToFloat( rleLat.Text, LAT_LON_UNDEFINED );

          if( ( -90.0 > d ) or ( 90.0 < d ) ) then
            begin
              msgOK( 
                tr( 'Latitude values must be between -90 and 90 degrees, inclusive.' ), 
                tr( 'Parameter out of range' ),
                IMGWarning,
                _myForm 
              );
              rleLat.SetFocus();
              fraAcceptCancel.Visible := true;
            end
          else
            begin
              filter();
              fraAcceptCancel.Visible := false;
            end
          ;
        end
      else if( rleLon.Visible ) then
        begin
          d := uiStrToFloat( rleLon.Text, LAT_LON_UNDEFINED );

          if( ( -180.0 > d ) or ( 180.0 < d ) ) then
            begin
              msgOK( 
                tr( 'Longitude values must be between -180 and 180 degrees, inclusive.' ), 
                tr( 'Parameter out of range' ),
                IMGWarning,
                _myForm 
              );
              rleLon.SetFocus();
              fraAcceptCancel.Visible := true;
            end
          else
            begin
              filter();
              fraAcceptCancel.Visible := false;
            end
          ;
        end
      else
        updateHerdCounter()
      ;

      _filterValidationInProgress := false;
    end
  ;


  procedure TFrameHerdListEditor.fraAcceptCancelbtnCancelClick( Sender: TObject );
    begin
      if( rleHerdSize.Visible ) then
        begin
          rleHerdSize.Text := '';
        end
      else if( rleDaysLeft.Visible ) then
        begin
          rleDaysLeft.Text := '';
        end
      else if( rleLat.Visible ) then
        begin
          rleLat.Text := '';
        end
      else if( rleLon.Visible ) then
        begin
          rleLon.Text := '';
        end
      ;

      fraAcceptCancel.Visible := false;
      rleHerdSize.Visible := false;
      lblHerdSize.Visible := false;
      rleLat.Visible := false;
      lblLat.Visible := false;
      rleLon.Visible := false;
      lblLon.Visible := false;
      rleDaysLeft.Visible := false;
      lblDaysLeft.Visible := false;
      cboMainFilter.ItemIndex := 0;
      stgHerds.SearchOptions.Filtered := false;

      updateHerdCounter();
    end
  ;


  function TFrameHerdListEditor.zeroHerdsDisplayed(): boolean;
    begin
      if( 2 < stgHerds.RowCount ) then
        result := false
      else if( 1 >= stgHerds.RowCount ) then
        result := true
      else if ( 2 = stgHerds.RowCount ) and ( 0 = length( trim( stgHerds.Cells[ COL_IDX, 1 ] ) ) ) then
        result := true
      else
        result := false
      ;
    end
  ;


  function TFrameHerdListEditor.filterHerdInRow( grid: TARSortGrid; iRow: integer ): boolean;
    var
      iHerd: integer;
    begin
      iHerd := myStrToInt( grid.Cells[ COL_IDX, iRow], 0 );
      assert( 0 < iHerd );

      dec( iHerd ); // Remember: the list is 0-indexed, but the grid starts at 1.

      case _llForm.shape of
        rsRectangle: result := _herdList.at(iHerd).isBoundedBy( _llForm.latNW, _llForm.lonNW, _llForm.latSE, _llForm.lonSE );
        rsCircle: result := _herdList.at(iHerd).isInCircle( _llForm.latCenter, _llForm.lonCenter, _llForm.radius );
        else
          begin
            raise exception.Create( 'Unsupported shape in TFrameHerdListEditor.filterHerdInRow' );
            result := false;
          end
        ;
      end;

      if( _llForm.excludeRegion ) then result := not( result );
    end
  ;


  procedure TFrameHerdListEditor.btnTestClick(Sender: TObject);
    var
      myRect: TGridRect;
    begin
      // Show the Lat/Lon form.
      if( mrOK <> _llForm.ShowModal() ) then
        exit
      ;

      // Get ready to apply the filter...
      //---------------------------------
      stgHerds.SearchOptions.FilterFunction := filterHerdInRow;

      _filterInProgress := true;

      lockWindow();

      // Clear the existing filter before attempting to run a new one.
      if( stgHerds.SearchOptions.Filtered ) then
        stgHerds.SearchOptions.Filtered := false
      ;

      // Run the filter...
      //------------------
      stgHerds.SearchOptions.Filtered := true;

      // ... and clean up the mess.
      //---------------------------
      disableEditing(); // Editing will be re-enabled on the next mouseDown event.
      stgHerds.SearchOptions.FilterFunction := nil;
      stgHerds.SortByColumn( stgHerds.SortColumn );


      // Finally, update the display
      //----------------------------
      updateHerdCounter();

      myRect.Top := stgHerds.Row;
      myRect.Bottom := stgHerds.Row;

      if( COL_PT >= stgHerds.Col ) then
        begin
          myRect.Left := COL_SIZE;
          myRect.Right := COL_SIZE;
        end
      else if( COL_STATUS = stgHerds.Col ) then
        begin
          myRect.Left := COL_DAYS_IN_STATE; // COL_DAYS_LEFT;
          myRect.Right := COL_DAYS_IN_STATE; // COL_DAYS_LEFT;
        end
      else
        begin
          myRect.Left := stgHerds.Col;
          myRect.Right := stgHerds.Col;
        end
      ;

      stgHerds.Selection := myRect;

      _filterInProgress := false;
      unlockWindow();
    end
  ;


  procedure TFrameHerdListEditor.filter();
    var
      myRect: TGridRect;
    begin
      dbcout( '--- TFrameHerdListEditor.filter()', DBSHOWMSG );

      try
        try
          dbcout( '+++ Set _filterInProgress to true', DBSHOWMSG );
          _filterInProgress := true;

          lockWindow();

          // Clear the existing filter before attempting to run a new one.
          if( stgHerds.SearchOptions.Filtered ) then
            stgHerds.SearchOptions.Filtered := false
          ;

          case cboMainFilter.ItemIndex of
            FILTER_NONE:
              begin
                _filtering := false;
                stgHerds.SearchOptions.Filtered := false;
              end
            ;
            FILTER_PT:
              begin
                stgHerds.SearchOptions.SearchCol := COL_PT;

                if( -1 = cboProdTypeFilter.ItemIndex ) then
                  stgHerds.SearchOptions.Filtered := false
                else
                  begin
                    stgHerds.SearchOptions.SearchText := cboProdTypeFilter.Items[cboProdTypeFilter.ItemIndex];
                    stgHerds.SearchOptions.Filtered := _filtering;
                  end
                ;
              end
            ;
            FILTER_SIZE:
              begin
                stgHerds.SearchOptions.SearchCol := COL_SIZE;
                stgHerds.SearchOptions.SearchText := rleHerdSize.Text;
                stgHerds.SearchOptions.Filtered := _filtering;
              end
            ;
            FILTER_GEO_RANGE:
              begin
                stgHerds.SearchOptions.FilterFunction := filterHerdInRow;
                stgHerds.SearchOptions.Filtered := _filtering;
                stgHerds.SearchOptions.FilterFunction := nil;
              end
            ;
            FILTER_LAT:
              begin
                stgHerds.SearchOptions.SearchCol := COL_LAT;
                stgHerds.SearchOptions.SearchText := rleLat.Text;
                stgHerds.SearchOptions.Filtered := _filtering;
              end
            ;
            FILTER_LON:
              begin
                stgHerds.SearchOptions.SearchCol := COL_LON;
                stgHerds.SearchOptions.SearchText := rleLon.Text;
                stgHerds.SearchOptions.Filtered := _filtering;
              end
            ;
            FILTER_STATUS:
              begin
                stgHerds.SearchOptions.SearchCol := COL_STATUS;

                if( -1 = cboStatus.ItemIndex ) then
                  stgHerds.SearchOptions.Filtered := false
                else
                  begin
                    stgHerds.SearchOptions.SearchText := cboStatus.Items[ cboStatus.ItemIndex ];
                    stgHerds.SearchOptions.Filtered := _filtering;
                  end
                ;
              end
            ;
            FILTER_DAYS_IN_STATE:
              begin
                stgHerds.SearchOptions.SearchCol := COL_DAYS_IN_STATE;
                stgHerds.SearchOptions.SearchText := rleDaysLeft.Text;
                stgHerds.SearchOptions.Filtered := _filtering;
              end
            ;
            FILTER_DAYS_LEFT:
              begin
                stgHerds.SearchOptions.SearchCol := COL_DAYS_LEFT;
                stgHerds.SearchOptions.SearchText := rleDaysLeft.Text;
                stgHerds.SearchOptions.Filtered := _filtering;
              end
            ;
          end;

          disableEditing();
          stgHerds.SortByColumn( stgHerds.SortColumn );
          // Editing will be re-enabled on the next mouseDown event.

          updateHerdCounter();

          myRect.Top := stgHerds.Row;
          myRect.Bottom := stgHerds.Row;

          if( COL_PT >= stgHerds.Col ) then
            begin
              myRect.Left := COL_SIZE;
              myRect.Right := COL_SIZE;
            end
          else if( COL_STATUS = stgHerds.Col ) then
            begin
              myRect.Left := COL_DAYS_IN_STATE; // COL_DAYS_LEFT;
              myRect.Right := COL_DAYS_IN_STATE; // COL_DAYS_LEFT;
            end
          else
            begin
              myRect.Left := stgHerds.Col;
              myRect.Right := stgHerds.Col;
            end
          ;

          stgHerds.Selection := myRect;
          
        except
          // fail silently
          dbcout( '+++ Exception occurred here in TFrameHerdListEditor.filter.', DBSHOWMSG );
        end;
      finally
        _filterInProgress := false;
        unlockWindow();
      end;

      dbcout( '-- Done TFrameHerdListEditor.filter()', DBSHOWMSG );
    end
  ;


  procedure TFrameHerdListEditor.sortControlChange(Sender: TObject);
    begin
      dbclear();
      dbcout( '+++ TFrameHerdListEditor.sortControlChange...', DBSHOWMSG );

      if( not( _sortInProgress ) ) then
        begin
          disableEditing();

          if( rdoAscending.Checked ) then
            begin
              stgHerds.SortDirection := sdAscending;
              dbcout( 'Sort ascending...', DBSHOWMSG );
            end
          else
            begin
              stgHerds.SortDirection := sdDescending;
              dbcout( 'Sort descending...', DBSHOWMSG );
            end
          ;

          dbcout( 'Sorting by column ' + intToStr( cboMainSort.ItemIndex ) + '...', DBSHOWMSG );
          stgHerds.sortByColumn( cboMainSort.ItemIndex );
          dbcout( 'Done sorting.', DBSHOWMSG );

          dbcout( '--- TFrameHerdListEditor.sortControlChange done.', DBSHOWMSG );
        end
      ;
    end
  ;


  procedure TFrameHerdListEditor.rleEnter(Sender: TObject);
    begin
      fraAcceptCancel.Visible := true;
    end
  ;


  procedure TFrameHerdListEditor.rleExit(Sender: TObject);
    begin
      if
        ( fraAcceptCancel.btnAccept = _myForm.ActiveControl )
      or
        ( fraAcceptCancel.btnCancel = _myForm.ActiveControl )
      or
        ( fraAcceptCancel = _myForm.ActiveControl )
      or
        ( pnlFilterControls = _myForm.ActiveControl )
      or
        ( nil = _myForm.ActiveControl )
      then
        // Do nothing: the button click events do the work.
      else if( controlIsInObject( _myForm.ActiveControl, self ) ) then
        begin
          // The user has selected another control in the frame.
          // Force accept.
          fraAcceptCancelbtnAcceptClick( nil );
        end
      else
        begin
          // The user has clicked on a control outside the frame.
          // Force cancel.
          fraAcceptCancelbtnCancelClick( nil );
        end
      ;
    end
  ;


  procedure TFrameHerdListEditor.stgHerdsGetCellFormat(Sender: TObject; Col,
    Row: Integer; State: TGridDrawState; var FormatOptions: TFormatOptions);
    begin
      if( 0 = Row ) then FormatOptions.Font.Style := [fsbold];
    end
  ;


  procedure TFrameHerdListEditor.cboMainFilterEnter(Sender: TObject);
    begin
      if( cboProdType.Visible ) then
        begin
          cboProdType.Visible := false;
          stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] := cboProdType.Items[ cboProdType.ItemIndex ];
        end
      ;

      if( cboTransitionState.Visible ) then
        begin
          cboTransitionState.Visible := false;
          stgHerds.Cells[ stgHerds.Col, stgHerds.Row ] := cboTransitionState.Items[ cboTransitionState.ItemIndex ];
        end
      ;

      stgHerds.Invalidate();
      stgHerds.Repaint();
    end
  ;


  procedure TFrameHerdListEditor.updateHerdCounter();
    var
      str: string;
      //str2: string;
    begin
      if( 2 = stgHerds.RowCount ) then
        begin
          if( 0 = length( trim( stgHerds.Cells[ 0, 1 ] ) ) ) then
            begin
              str := '0' + ' ' + tr( 'units' );
              disableEditing();
            end
          else
            str := '1' + ' ' + tr( 'unit' )
          ;
        end
      else
        str := intToStr( stgHerds.RowCount - 1 ) + ' ' + tr( 'units' )
      ;

      (*
      // FIX ME: What was this for??
      if( 1 = _herdList.count - _herdList.removedCount ) then
        str2 := tr( 'unit'  )
      else
        str2 := tr( 'units' )
      ;
      *)

      if( stgHerds.SearchOptions.Filtered ) then
        begin
          str := ansiReplaceStr( tr( 'abc units shown (of xyz units total)' ), tr( 'abc units' ), str );
          str := ansiReplaceStr( str, 'xyz', intToStr( _herdList.Count - _herdList.removedCount ) );
        end
      ;

      pnlHerdCounter.Caption := str;
    end
  ;

  
  // This function deals with a little bug in TREEdit.
  procedure TFrameHerdListEditor.rleKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      inherited;

      if( 13 = key ) then
        begin
          pnlFilterControls.SetFocus();
          fraAcceptCancelbtnAcceptClick( nil );
        end
      else if( 27 = key ) then
        begin
          fraAcceptCancelbtnCancelClick( nil );
          pnlFilterControls.SetFocus();
        end
      ;
    end
  ;

  
  procedure TFrameHerdListEditor.fraAcceptCancelbtnCancelExit( Sender: TObject );
    begin
      fraAcceptCancelBtnCancelClick( sender );
    end
  ;


  procedure TFrameHerdListEditor.lockWindow();
    begin
      if( 0 = _windowLocks ) then
        self.Perform( WM_SETREDRAW, 0, 0 )
      ;

      inc( _windowLocks );
    end
  ;


  procedure TFrameHerdListEditor.unlockWindow();
    begin
      dec( _windowLocks );

      if( 0 = _windowLocks ) then
        begin
          self.Perform( WM_SETREDRAW, 1, 0 );
          RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
        end
      ;
    end
  ;

  procedure TFrameHerdListEditor.showWaitCursor();
    begin
      if( 0 = _waitCursors ) then screen.Cursor := crHourglass;
      inc( _waitCursors );
    end
  ;


  procedure TFrameHerdListEditor.hideWaitCursor();
    begin
      dec( _waitCursors );
      if( 0 = _waitCursors ) then screen.Cursor := crDefault;
    end
  ;


  procedure TFrameHerdListEditor.setupLLForm();
    var
      radius: double;
    begin
      // Set up the corners for rectangular regions
      _llForm.latNW := _herdList.northLat;
      _llForm.lonNW := _herdList.westLon;
      _llForm.latSE := _herdList.southLat;
      _llForm.lonSE := _herdList.eastLon;


      // Set up the parameters for circular regions
      radius := gisLocalDistance( _llForm.latNW, _llForm.lonNW, _llForm.latSE, _llForm.lonSE ) / 2;
      _llForm.radius := radius;
      _llForm.latCenter := gisAverageLat( _llForm.latNW, _llForm.latSE );
      _llForm.lonCenter := gisAverageLon( _llForm.lonNW, _llForm.lonSE );
    end
  ;


  function TFrameHerdListEditor.ShowLLForm(): boolean;
    begin
      if( mrOK = _llForm.showModal() ) then
        begin
          case _llForm.shape of
            rsCircle:
              lblGeoRange.Caption := tr( 'Circle with radius' ) + ' '
                + uiFloatToStr( _llForm.radius, 3 )
                + ' ' + tr( 'at' ) + ' (' + uiFloatToStr( _llForm.latCenter, LAT_LON_PRECISION )
                + ', ' + uiFloatToStr( _llForm.lonCenter, LAT_LON_PRECISION )
                + ')'
              ;
            rsRectangle:
              lblGeoRange.Caption := tr( 'Rectangle' )
                + ' (' + uiFloatToStr( _llForm.latNW, LAT_LON_PRECISION ) + ', ' + uiFloatToStr( _llForm.lonNW, LAT_LON_PRECISION ) + ') '
                +  tr( 'and' )
                + ' (' + uiFloatToStr( _llForm.latSE, LAT_LON_PRECISION ) + ', ' + uiFloatToStr( _llForm.lonSE, LAT_LON_PRECISION ) + ')'
              ;
          end;

          if( _llForm.excludeRegion ) then
            lblGeoRange.Caption := tr( 'NOT in' ) + ' '
              + lower( leftStr( lblGeoRange.Caption, 1 ) )
              + rightStr( lblGeoRange.Caption, length( lblGeoRange.Caption ) - 1 )
          ;

          btnGeoRange.Left := lblGeoRange.Left + lblGeoRange.Width + 20;

          result := true;
          _llFormShown := true;
        end
      else
        begin
          result := false;
        end
      ;
    end
  ;


  procedure TFrameHerdListEditor.btnGeoRangeClick(Sender: TObject);
    begin
      if( showLLForm() ) then
        begin
          _filtering := true;
          filter();
        end
      ;
    end
  ;

end.

