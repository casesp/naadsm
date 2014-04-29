unit FormZoneCreation;

(*
FormZoneCreation.pas/dfm
------------------------
Begin: 2006/12/19
Last revision: $Date: 2010-09-09 14:29:38 $ $Author: rhupalo $
Version: $Revision: 1.10.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    SysUtils,
    ExtCtrls,
    StdCtrls,
    Buttons,
    Controls,
    Classes,
    Dialogs,
    Forms,
    Menus,
    ActnPopupCtrl,

    REEdit,

    FormSMWizardBase,
    SMScenario,
    Zone
  ;


  {*
    This form handles removal or renaming of existing zones and the
    creation of new zones.

    New zones are assigned a temporary negative ID number. The negative
    zone ID number is just a placeholder until the database generates an appropriate ID.

    To prevent referential integrity errors, update the database with removed
    zones first, then with modified zones, and finally with newly created zones.
  }
  type TFormZoneCreation = class( TFormSMWizardBase )
      pnlZoneList: TPanel;
      pnlZoneListHeader: TPanel;
      pnlCaption: TPanel;
      pnlBody: TPanel;
      lstZones: TListBox;
      BitBtnAdd: TBitBtn;
      BitBtnRename: TBitBtn;
      BitBtnRemove: TBitBtn;
      gbxZoneParams: TGroupBox;
      lblRadius: TLabel;
      rleRadius: TREEdit;

      // Main GUI event handlers
      //------------------------
      procedure BitBtnAddClick(Sender: TObject);
      procedure BitBtnRenameClick(Sender: TObject);
      procedure BitBtnRemoveClick(Sender: TObject);

      // Simple GUI event handlers
      //--------------------------
      procedure lstZonesChange(Sender: TObject);
      procedure lstZonesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

      procedure processTextEntry( Sender: TObject );

    protected
      _nextAddID: integer; // Used to keep track of the temporary (negative) ID number to assign to new zones
      _warningShown: boolean;
      _removedCount: integer;

      _selectedZone: TZone;

      procedure translateUI();

      // Reimplemented functions
      //-------------------------
      { Properties }
      function getSelectedZoneIndex(): integer; override;

      { Load zone names from database and populate the list box }
      procedure initializeFromSim(); override;

      { Data entry validation }
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

      // GUI display updates
      //--------------------
      procedure UpdateButtons();
      procedure updateListBox();

    public
      // Construction/destruction
      //-------------------------
      constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

    end
  ;

  const
  	DBFORMZONE: boolean = false; // set to true to display debugging messages for this form.

implementation

  {$R *.dfm}

  uses
    StrUtils,
    
    RegExpDefs,
    SMDatabase,
    SqlClasses,
    MyDialogs,
    MyStrUtils,
    DebugWindow,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFormZoneCreation.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      rleRadius.InputExpression := RE_DECIMAL_INPUT;

      _nextAddID := -1;
      _warningShown := false;
      _removedCount := 0;
    end
  ;


  procedure TFormZoneCreation.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormZoneCreation.dfm
      // File date: Wed May 30 11:08:14 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Zones' );
          pnlZoneListHeader.Caption := tr( 'Zones' );
          pnlCaption.Caption := tr( 'Zones' );
          BitBtnAdd.Caption := tr( 'Add zone' );
          BitBtnRename.Caption := tr( 'Rename selected zone' );
          BitBtnRemove.Caption := tr( 'Remove selected zone' );
          gbxZoneParams.Caption := tr( 'Parameters of selected zone' );
          lblRadius.Caption := tr( 'Zone radius (km):' );
          rleRadius.Text := tr( '1.5' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          lstZones.Items[0] := tr( 'High risk' );
          lstZones.Items[1] := tr( 'Moderate risk' );
          lstZones.Items[2] := tr( 'Low risk' );
        end
      ;

    end
  ;


  destructor TFormZoneCreation.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Parameter and database functions
//-----------------------------------------------------------------------------
  procedure TFormZoneCreation.initializeFromSim();
    begin
      updateListBox();
    end
  ;


  function TFormZoneCreation.dataIsValid(): boolean;
    var
      i: integer;
      zList: TZoneList;
      z : TZone;

      it: TZoneListIterator;
    begin
      result := true;

      zList := _smScenarioCopy.simInput.zoneList;

      // Radii must all be greater than 0
      for i := 0 to zList.Count - 1 do
        begin
          z := zList.at(i);

          if( ( 0 >= z.radius ) and not( z.removed ) ) then
            begin
              msgOK( 
                tr( 'The radius for each zone must be greater than 0.' ), 
                tr( 'Invalid zone radius' ), 
                IMGWarning, 
                self 
              );
              result := false;
              exit;
            end
          ;
        end
      ;

      // No two radii can be the same.
      it := TZoneListIterator.create( zList );
      while( nil <> it.current() ) do
        begin
          if( not( it.current().removed ) ) then
            begin
              for i := 0 to zList.Count - 1 do
                begin
                  z := zList.at(i);
                  if
                    ( not( z = it.current() ) )
                  and
                    ( z.radius = it.current().radius )
                  and
                    ( not( z.removed ) )
                  then
                    begin
                      msgOK( 
                        tr( 'Each zone must have a unique radius.' ), 
                        tr( 'Invalid zone radius' ), 
                        IMGWarning, 
                        self 
                      );
                      result := false;
                      break;
                    end
                  ;
                end
              ;

              if( false = result ) then
                break
              ;
            end
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;



  function TFormZoneCreation.getDataUpdated(): boolean;
    begin
      // Adding new zones or renaming existing zones doesn't require that output be deleted.
      // Removing an existing zone does.

      result := ( _removedCount > 0 );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Main GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormZoneCreation.BitBtnAddClick(Sender: TObject);
    var
      NewZoneDescr: string;
      answer: integer;
      zList: TZoneList;
      z : TZone;
    begin
      zList := _smScenarioCopy.simInput.zoneList;

      NewZoneDescr := msgInput(
        tr( 'Please enter a name for the new zone.' ),
        '', // regexp
        '', // caption
        IMGQuestion,
        self
      );
      NewZoneDescr := trim( NewZoneDescr );

      if( '' = NewZoneDescr ) then
        exit
      else
        begin
          z := zList.find( NewZoneDescr );
          if( nil <> z ) then
            begin
              if( not( z.removed ) ) then
                begin
                  msgOK( 
                    tr( 'A zone with this name is already present.' ),
                    tr( 'Cannot add zone' ),
                    IMGCritical,
                    self 
                  );
                  exit;
                end
              ;
            end
          ;
        end
      ;

      // If we get this far...
      if( not _warningShown ) then
        answer := msgYesNo(
          tr( 'To add a new zone it will be necessary to delete all current output records, if present.  Continue?' ),
          tr( 'Add zone' ),
          IMGQuestion,
          self )
      else
        answer := mrYes
      ;

      if( answer = mrYes) then
        begin
          showStar();
          _warningShown := true;
          z := TZone.create( _nextAddID, NewZoneDescr, 0.0, zList.sim );
          zList.append( z );
          dec( _nextAddID );

          _selectedZoneIndex := 32000; // This will force a reset to the end of the list in updateListBox()

          updateListBox();
        end
      ;
    end
  ;


  procedure TFormZoneCreation.BitBtnRenameClick(Sender: TObject);
    var
      newZName, oldZName: string;
      zList: TZoneList;
      z : TZone;
    begin
      zList := _smScenarioCopy.simInput.zoneList;

      oldZName := lstZones.Items.Strings[lstZones.ItemIndex];

      newZName := msgInput(
        ansiReplaceStr( tr( 'Modify zone name for xyz.' ), 'xyz', oldZName ),
        '', // regexp
        '', // caption
        IMGQuestion,
        self,
        oldZName
      );

      newZName := trim( newZName );

      // If the new name is the same as the old name, don't mess with anything
      if( oldZName = newZName ) then
        exit
      ;

      // Make sure newZ is unique
      if( '' = newZName ) then
        // do nothing
      else if( nil <> ZList.find( newZName ) ) then
        begin
          msgOK( 
            tr( 'A zone with this name is already present.' ),
            tr( 'Cannot rename zone' ),
            IMGCritical,
            self )
          ;
        end
      else // everything is OK.  Modify the name.
      	begin
          z := lstZones.Items.Objects[lstZones.ItemIndex] as TZone;
          z.descr := newZName;
          updateListBox();
          showStar();
        end
      ;
    end
  ;


  procedure TFormZoneCreation.BitBtnRemoveClick(Sender: TObject);
    var
      messageText: string;
      answer: integer;
      i: integer;

      z : TZone;
    begin
      if lstZones.selCount = 1 then
        messageText := lstZones.Items[lstZones.ItemIndex]
      else
        messageText := tr( 'these selected zones' )
      ;

      if( not _warningShown ) then
        answer := msgYesNo(
          tr( 'By continuing, this zone and all output records, if present, will be deleted.' )
            + ' ' + ansiReplaceStr( tr( 'Are you sure you wish to remove xyz?' ), 'xyz', MessageText ),
          tr( 'Remove zone' ),
          IMGQuestion,
          self )
      else
      	answer := mrYes
      ;

      if( answer = mrYes ) then
        begin
          showStar();
        	_warningShown := true;

          for i := 0 to (lstZones.Count-1) do
            begin
              if( lstZones.Selected[i] ) then
                begin
                  z := lstZones.Items.Objects[i] as TZone;
                  z.removed := true;
                end
              ;
            end
          ;

        end
      ;

      _selectedZoneIndex := -1; // This will force a reset to the head of the list in updateListBox()

      updateListBox();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormZoneCreation.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      // need detection in order to conduct tracing
    	if (( _smScenarioCopy.simInput.includeZonesGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
        result := inherited showModal( nextFormToShow, formDisplayed, currentFormIndex )
      else
      	begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;


  procedure TFormZoneCreation.updateButtons();
    begin
      if( nil <> _selectedZone ) then
        begin
          BitBtnRename.Enabled := true;
          BitBtnRemove.Enabled := true;

          if( 0 < _selectedZone.radius ) then
            rleRadius.Text := uiFloatToStr( _selectedZone.radius )
          else
            rleRadius.text := ''
          ;
          gbxZoneParams.Visible := true;
        end
      else
        begin
          BitBtnRename.Enabled := false;
          BitBtnRemove.Enabled := false;

          gbxZoneParams.Visible := false;
        end
      ;
    end
  ;


  procedure TFormZoneCreation.updateListBox();
    var
      z: TZone;
  	begin
      lstZones.Clear();

      z := _smScenarioCopy.simInput.zoneList.first();

      while( nil <> z ) do
        begin
          if( not( z.removed ) ) then
            lstZones.Items.AddObject( z.descr, z )
          ;

          z := _smScenarioCopy.simInput.zoneList.next();
        end
      ;

      lstZones.ClearSelection();

      if
        ( 0 < _smScenarioCopy.simInput.zoneList.Count )
      and
        ( 0 < lstZones.Items.Count )
      then
        begin
          if( 0 > _selectedZoneIndex ) then _selectedZoneIndex := 0;
          if( lstZones.Items.Count <= _selectedZoneIndex ) then _selectedZoneIndex := lstZones.Items.Count - 1;

          lstZones.ItemIndex := _selectedZoneIndex;
          _selectedZone := lstZones.items.objects[ lstZones.ItemIndex ] as TZone;
        end
      else
        _selectedZone := nil;
      ;

      updateButtons();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Simple GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormZoneCreation.lstZonesChange(Sender: TObject);
    begin
      _selectedZoneIndex := lstZones.ItemIndex;
      _selectedZone := lstZones.Items.Objects[_selectedZoneIndex] as TZone;
      updateButtons();
    end
  ;

  procedure TFormZoneCreation.lstZonesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin updateButtons(); end;

  
  procedure TFormZoneCreation.processTextEntry( Sender: TObject );
  	begin
      if( nil <> _selectedZone ) then
        _selectedZone.radius := uiStrToFloat( rleRadius.text, -1.0 )
      else
        raise exception.create( 'There is no _selectedZone in TFormZoneCreation.processTextEntry' )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties reimplemented from TFormSMWizardBase
//-----------------------------------------------------------------------------
  function TFormZoneCreation.getSelectedZoneIndex(): integer;
    begin
      result := lstZones.ItemIndex;
    end
  ;
//-----------------------------------------------------------------------------


initialization

  RegisterClass( TFormZoneCreation );

end.

