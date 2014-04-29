unit FormSMWizardBase;

(*
FormSMWizardBase.pas/dfm
-------------------------
Begin: 2005/03/03
Last revision: $Date: 2008/03/12 22:10:48 $ $Author: areeves $
Version: $Revision: 1.32 $
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
    Buttons,
    ExtCtrls,
    StdCtrls,
    Menus,
    ActnMan,
    ActnPopupCtrl,
    
    IniHandler,

    SMScenario,
    SMSimulationInput
  ;

  const
    NF_NEXT = -4;
    NF_BACK = -3;
    NF_NONE = -2;
  //type TNextForm = ( NFNone, NFBack, NFNext );


  {*
    This form provides a base class for all parameter input screens used in NAADSM.
  }
  type TFormSMWizardBase = class( TForm )
      pnlBase: TPanel;
      pnlWizardButtons: TPanel;
      LabelSave: TLabel;
      btnFinish: TBitBtn;
      btnNext: TBitBtn;
      btnBack: TBitBtn;
      btnCancel: TBitBtn;

      btnSelect: TButton;
      PopupActionBarEx1: TPopupActionBarEx;
      Startsetup1: TMenuItem;

			procedure FormCreate( Sender: TObject );
			
      procedure wizardButtonClick( sender: TObject );

      // FIX ME: some day, hook up these two procedures to handle scrollwheel events
      procedure FormMouseWheelDown(
        Sender: TObject;
        Shift: TShiftState;
        MousePos: TPoint;
        var Handled: Boolean
      );

      procedure FormMouseWheelUp(
        Sender: TObject;
        Shift: TShiftState;
        MousePos: TPoint;
        var Handled: Boolean
      );

      procedure FormShow(Sender: TObject);
      procedure FormHide(Sender: TObject);

      procedure btnSelectMouseUp(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );

    protected
      _myForm: TForm;
      
      _smScenarioOriginal: TSMScenario;
      _smScenarioCopy: TSMScenario;

      _showMultiuseWarning: boolean;
      _showFunctionRemovalWarning: boolean;
      _showTestFeatureWarning: boolean;
      _showApplyToAllWarning: boolean;
      _selectedProdTypePairIndex: integer;
      _selectedProdTypeIndex: integer;
      _iniHandler: TIniHandler;

      _outputCleared: boolean;

      _popupMenuItemIndex: integer;

      _currentFormIndex: integer;
      _nextForm: integer; // Has value of NF_NONE, NF_BACK, NF_NEXT, or a valid index of frmMain._paramFormList

      procedure translateUI();

      procedure popupMenuClick( sender: TObject );
      procedure updateMenuItems();

      function getShowMultiUseWarning(): boolean;
      procedure setShowMultiUseWarning( val: boolean );
      function getShowTestFeatureWarning(): boolean;
      procedure setShowTestFeatureWarning( val: boolean );
      function getShowFunctionRemovalWarning(): boolean;
      procedure setShowFunctionRemovalWarning( val: boolean );
      function getShowApplyToAllWarning(): boolean;
      procedure setShowApplyToAllWarning( val: boolean );

      procedure setSelectedProdTypePairIndex( val: integer ); virtual;
      procedure setSelectedProdTypeIndex( val: integer ); virtual;
      function getSelectedProdTypePairIndex(): integer; virtual;
      function getSelectedProdTypeIndex(): integer; virtual;

      procedure setIniHandler( val: TIniHandler );
      function getIniHandler(): TIniHandler;

      function getOutputCleared(): boolean;

      procedure centerForm( parent: TComponent );

      procedure initializeFromSim(); virtual; abstract;
      procedure updateScenarioData(); virtual;
      function dataIsValid(): boolean; virtual; abstract;

      function getDataUpdated(): boolean; virtual; abstract;

    public
      constructor create( AOwner: TComponent); override;
      destructor destroy(); override;

      procedure setContextMenuItems( ac: TActionClient );

      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; reintroduce; virtual;

      procedure setParams( smScenario: TSMScenario ); virtual;

      procedure enableWizardButtons();
      procedure disableWizardButtons();

      procedure updateMasterDisplay(); virtual;

      procedure showStar( sender: TObject = nil ); virtual;
      procedure hideStar( sender: TObject = nil ); virtual;

      property showMultiUseWarning: boolean read getShowMultiUseWarning write setShowMultiUseWarning;
      property showFunctionRemovalWarning: boolean read getShowFunctionRemovalWarning write setShowFunctionRemovalWarning;
      property showTestFeatureWarning: boolean read getShowTestFeatureWarning write setShowTestFeatureWarning;
      property showApplyToAllWarning: boolean read getShowApplyToAllWarning write setShowApplyToAllWarning;
      property selectedProdTypePairIndex: integer read getSelectedProdTypePairIndex write setSelectedProdTypePairIndex;
      property selectedProdTypeIndex: integer read getSelectedProdTypeIndex write setSelectedProdTypeIndex;
      property iniHandler: TIniHandler read getIniHandler write setIniHandler;

      property dataUpdated: boolean read getDataUpdated;
      property outputCleared: boolean read getOutputCleared;

      // NextForm is used by Used by TFormMain.showParamForm to determine whether the
      // next or previous form in paramFormList should be shown.  NextForm is set
      // by wizardButtonClick.
      property NextForm: integer read _nextForm write _nextForm;
    end
  ;


implementation

{$R *.dfm}

	uses
    ActnList,

    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    MyDialogs,
    ControlUtils,
    I88n,

    FormMain
  ;

  const
  	DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.

//-----------------------------------------------------------------------------
// Construction/iniitialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormSMWizardBase.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      dbcout( endl + 'Creating ' + self.name, DBSHOWMSG );

      _myForm := AOwner as TForm;

      self.height := 588;
      self.width := 700;

      _iniHandler := nil;

      hideStar( nil );

      _outputCleared := false;

      _smScenarioCopy := nil;
      _smScenarioOriginal := nil;

      dbcout( 'Done with constructor', DBSHOWMSG );
    end
  ;


  procedure TFormSMWizardBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSMWizardBase.dfm
      // File date: Mon Sep 24 12:04:52 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'FormSMWizardBase' );
          LabelSave.Caption := tr( '*' );
          btnFinish.Caption := tr( '&Finish' );
          btnFinish.Hint := tr( 'Complete working on the screen by saving the information' );
          btnNext.Caption := tr( '&Next >' );
          btnNext.Hint := tr( 'Proceed to the next screen, saving this information' );
          btnBack.Caption := tr( '< &Back' );
          btnBack.Hint := tr( 'Return to the previous screen, saving this information' );
          btnCancel.Caption := tr( '&Cancel' );
          btnCancel.Hint := tr( 'Abort current operations, not saving information on the screen and return to the main menu' );
          btnSelect.Caption := tr( '&Select...' );
          btnSelect.Hint := tr( 'Save this information and select another screen' );
          Startsetup1.Caption := tr( 'Start setup' );
        end
      ;

    end
  ;


	procedure TFormSMWizardBase.FormCreate(Sender: TObject);
		begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;


  destructor TFormSMWizardBase.destroy();
  	begin
      dbcout( 'Destroying ' + self.name, DBSHOWMSG );
      
      if( nil <> _smScenarioCopy ) then freeAndNil( _smScenarioCopy );

      // Don't delete the original smScenario!!

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Functions for showing (or hiding) the form
//-----------------------------------------------------------------------------
  procedure TFormSMWizardBase.FormShow(Sender: TObject);
    begin
      // Once a param form is showing, enable drawing of all windows
      dbcout( self.name + ' is now showing.', DBSHOWMSG );
      //dbcout( 'Unlocking windows from TFormSMWizardBase.formShow()', DBSHOWMSG );
      frmMain.unlockWindows();
    end
  ;


  procedure TFormSMWizardBase.FormHide(Sender: TObject);
    begin
      // Do nothing.
    end
  ;


	function TFormSMWizardBase.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      _currentFormIndex := currentFormIndex;
      initializeFromSim();

      formDisplayed := true;

    	result := inherited showModal();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  procedure TFormSMWizardBase.centerForm( parent: TComponent );
    const
      VSize = 500;
      HSize = 725;
    var
      parentForm: TForm;
    begin
      parentForm := parent as TForm;

      Width := HSize;
      Height := VSize;
      Left := parentForm.Left + ((parentForm.Width - Width) div 2);
      Top := parentForm.top + ((parentForm.Height - Height) div 2);
      WindowState := wsNormal;
      BorderIcons := [biSystemMenu,biMaximize];
    end
	;


  procedure TFormSMWizardBase.setContextMenuItems( ac: TActionClient );
    var
      i, j: integer;
      menuItem, subMenuItem: TMenuItem;
    begin
      menuItem := self.PopupActionBarEx1.Items[0];
      self.PopupActionBarEx1.Items.Remove( menuItem );

      // We don't need the last three items from frmMain's scenario parameters menu.
      for i := 0 to ac.Items.count - 4 do
        begin
          dbcout( ac.Items[i].Caption, DBSHOWMSG );
          self.PopupActionBarEx1.Images := frmMain.ImageList1;
          menuItem := TMenuItem.Create( self );

          if( nil <> ac.Items[i].Action ) then
            menuItem.Name := ac.Items[i].Action.Name;
          menuItem.Caption := ac.Items[i].Caption;
          menuItem.ImageIndex := ac.Items[i].ImageIndex;
          menuItem.OnClick := self.popupMenuClick;
          if( not( ac.Items[i].Separator ) ) then
            menuItem.Enabled := ( ac.Items[i].Action as TAction ).Enabled;

          // For now, we only need to recurse one level, so this will do.
          for j := 0 to ac.Items[i].Items.Count - 1 do
            begin
              subMenuItem := TMenuItem.create( self );
              if( nil <> ac.Items[i].Items[j].Action ) then
                subMenuItem.Name := ac.Items[i].Items[j].Action.Name;
              subMenuItem.Caption := ac.Items[i].Items[j].Caption;
              subMenuItem.ImageIndex := ac.Items[i].Items[j].ImageIndex;
              subMenuItem.OnClick := self.popupMenuClick;
              if( not( ac.Items[i].Items[j].Separator ) ) then
                subMenuItem.Enabled := ( ac.Items[i].Items[j].Action as TAction ).Enabled;
              menuItem.Insert( j , subMenuItem );
            end
          ;

          self.PopupActionBarEx1.Items.Insert( i, menuItem );
        end
      ;
    end
  ;


  procedure TFormSMWizardBase.updateMenuItems();
    var
      ac: TActionClient;
      i, j: integer;
      menuItem, subMenuItem: TMenuItem;
    begin
      ac := frmMain.ActionMainMenuBar1.ActionClient.Items[1];

      // We don't need the last three items from frmMain's scenario parameters menu.
      for i := 0 to ac.Items.count - 4 do
        begin
          dbcout( ac.Items[i].Caption, DBSHOWMSG );
          menuItem := self.PopupActionBarEx1.Items[i];
          if( not( ac.Items[i].Separator ) ) then
            menuItem.Enabled := (ac.Items[i].Action as TAction).Enabled
          ;

          // For now, we only need to recurse one level, so this will do.
          for j := 0 to ac.Items[i].Items.Count - 1 do
            begin
              subMenuItem := self.PopupActionBarEx1.Items[i].Items[j];
              if( not( ac.Items[i].Items[j].Separator ) ) then
                subMenuItem.Enabled := (ac.Items[i].Items[j].Action as TAction).Enabled
              ;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormSMWizardBase.wizardButtonClick( sender: TObject );
    var
      clickedButton: TBitBtn;
    begin
      screen.Cursor := crHourGlass;

      clickedButton := TBitBtn( sender );

      if( clickedButton.Name = 'btnCancel' ) then
      	begin
        	nextForm := NF_NONE;
          close();
          exit;
        end
      ;

      dbcout( 'dataUpdated: ' + uiBoolToText( dataUpdated ), DBSHOWMSG  );
      dbcout( '_smScenarioCopy.updated: ' + uiBoolToText( _smScenarioCopy.updated ), DBSHOWMSG );

      dbcout( 'Checking for valid data...', DBSHOWMSG );

      if( dataIsValid() ) then
        begin
          // Prevent any window from redrawing until processing on this form is complete.
          // (Drawing will be re-enabled with the next form is shown, or, if this is the last form,
          // when this form is freed.)

          if( ( clickedButton = btnBack ) or ( clickedButton = btnNext ) ) then
            begin
              SendMessage( self.Handle, WM_SETREDRAW, 0, 0 );
              //dbcout( 'Locking windows from TFormSMWizardBase.wizardButtonClick()', DBSHOWMSG );
              frmMain.lockWindows();
            end
          ;

          dbcout( 'Data is valid.', DBSHOWMSG );
          dbcout( 'Updating database', DBSHOWMSG );
          _outputCleared := _smScenarioCopy.updated;

          updateScenarioData();

          if( clickedButton.Name = 'btnBack' ) then
            nextForm := NF_BACK
          else if( clickedButton.Name = 'btnNext' ) then
            nextForm := NF_NEXT
          else if( clickedButton.Name = 'btnFinish' ) then
            nextForm := NF_NONE
          else
            begin
              raise exception.create( self.name + ': illegal button' );
              nextForm := NF_NONE;
            end
          ;
          close();
        end
      ;

      screen.Cursor := crDefault;
    end
  ;


  procedure TFormSMWizardBase.btnSelectMouseUp( Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer );
    var
      p: TPoint;
    begin
      if( dataIsValid() ) then
        begin
          // Prevent any window from redrawing until processing on this form is complete.
          // (Drawing will be re-enabled with the next form is shown, or, if this is the last form,
          // when this form is freed.)

          dbcout( 'Data is valid.', DBSHOWMSG );
          dbcout( 'Updating database', DBSHOWMSG );
          _outputCleared := _smScenarioCopy.updated;

          updateScenarioData();

          // This ugly little bit of business enables/disables the popup menu controls as needed.
          frmMain.updateScenarioParamsMenuItems( true );
          updateMenuItems();
          //frmMain.updateScenarioParamsMenuItems( false );

          p := btnSelect.clientToScreen( point( x, y ) );
          PopupActionBarEx1.Popup( p.x, p.y );
        end
      ;

      screen.Cursor := crDefault;
    end
  ;


  procedure TFormSMWizardBase.popupMenuClick( sender: TObject );
    var
      senderName: string;
      nextParamFormName: string;
      nextFormIndex: integer;
    begin
      if( not( sender is TComponent ) ) then
        exit
      else
        senderName := (sender as TComponent).Name
      ;

      nextParamFormName := frmMain.paramFormForSenderName( senderName );
      nextFormIndex := frmMain.paramFormIndex( nextParamFormName );

      if( ( _currentFormIndex <> nextFormIndex ) and ( -1 <> nextFormIndex ) ) then
        begin
          SendMessage( self.Handle, WM_SETREDRAW, 0, 0 );
          frmMain.lockWindows();

          nextForm := nextFormIndex;

          close();
        end
      ;
    end
  ;


  procedure TFormSMWizardBase.FormMouseWheelUp(
  		Sender: TObject;
    	Shift: TShiftState;
      MousePos: TPoint;
      var Handled: Boolean
  	);
    begin
      inherited;
      dbcout( 'mousing up', DBSHOWMSG );
    end
  ;


  procedure TFormSMWizardBase.FormMouseWheelDown(
      Sender: TObject;
      Shift: TShiftState;
      MousePos: TPoint;
      var Handled: Boolean
    );
    begin
      inherited;
      dbcout( 'mousing down', DBSHOWMSG );
    end
  ;


	procedure TFormSMWizardBase.showStar( sender: TObject = nil );
  	begin
    	labelSave.Visible := true;
    end
  ;


	procedure TFormSMWizardBase.hideStar( sender: TObject = nil );
  	begin
    	labelSave.Visible := false;
    end
  ;


  procedure TFormSMWizardBase.updateMasterDisplay();
  	begin
    	// Don't do anything here.
      // This function may be reimplemented in derived classes.
    end
  ;


  procedure TFormSMWizardBase.enableWizardButtons();
  	begin
      setChildrenEnabled( pnlWizardButtons, true, true );
    end
  ;


  procedure TFormSMWizardBase.disableWizardButtons();
  	begin
    	setChildrenEnabled( pnlWizardButtons, false, true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data handling
//-----------------------------------------------------------------------------
  procedure TFormSMWizardBase.setParams( smScenario: TSMScenario );
    begin
      // This is a REFERENCE to the original scenario.
      _smScenarioOriginal := smScenario;

      // This is a COPY of the original scenario.
      // All changes should be made to the copy.
      // The second parameter of the copy constructor disables copying of the
      // herd list: copying a herd list is a time- and memory-consuming process,
      // and the only form that cares about the herd list is FormHerdListEditor.
      _smScenarioCopy := TSMScenario.create( smScenario, false );
    end
  ;


  procedure TFormSMWizardBase.updateScenarioData();
    begin
      if( DBSHOWMSG ) then
        begin
          dbcout( '******** DEBUGGING the original simInput', DBSHOWMSG );
          _smScenarioOriginal.simInput.debug();
        end
      ;

      _smScenarioOriginal.resetSimInput( _smScenarioCopy.simInput );


      if( DBSHOWMSG ) then
        begin
          dbcout( '******** DEBUGGING the copy of simInput', DBSHOWMSG );
          _smScenarioOriginal.simInput.debug();
        end
      ;

      _smScenarioOriginal.simInput.populateDatabase();

      // The herd list only needs to be updated by TFormHerdListEditor,
      // which handles the updates on its own.  Don't mess with it here.

      // Whenever the simInput is changed, the herdList needs to be notified, so that
      // it can update the production types associated with each herd.
      // If possible, it would be nice to make this process a little more efficient...
      _smScenarioOriginal.herdList.resetSim( _smScenarioOriginal.simInput, false );

      hideStar();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFormSMWizardBase.getShowMultiUseWarning(): boolean; begin result := _showMultiUseWarning; end;
  procedure TFormSMWizardBase.setShowMultiUseWarning( val: boolean ); begin _showMultiUseWarning := val; end;

  function TFormSMWizardBase.getShowFunctionRemovalWarning(): boolean; begin result := _showFunctionRemovalWarning; end;
  procedure TFormSMWizardBase.setShowFunctionRemovalWarning( val: boolean ); begin _showFunctionRemovalWarning := val; end;

  function TFormSMWizardBase.getShowTestFeatureWarning(): boolean; begin result := _showTestFeatureWarning; end;
  procedure TFormSMWizardBase.setShowTestFeatureWarning( val: boolean ); begin _showTestFeatureWarning := val; end;

  function TFormSMWizardBase.getShowApplyToAllWarning(): boolean; begin result := _showApplyToAllWarning; end;
  procedure TFormSMWizardBase.setShowApplyToAllWarning( val: boolean ); begin _showApplyToAllWarning := val; end;

  procedure TFormSMWizardBase.setSelectedProdTypePairIndex( val: integer ); begin _selectedProdTypePairIndex := val; end;
  procedure TFormSMWizardBase.setSelectedProdTypeIndex( val: integer ); begin _selectedProdTypeIndex := val; end;

  function TFormSMWizardBase.getSelectedProdTypePairIndex(): integer; begin result := _selectedProdTypePairIndex; end;
  function TFormSMWizardBase.getSelectedProdTypeIndex(): integer; begin result := _selectedProdTypeIndex; end;

  procedure TFormSMWizardBase.setIniHandler( val: TIniHandler ); begin _iniHandler := val; end;
  function TFormSMWizardBase.getIniHandler(): TIniHandler; begin result := _iniHandler; end;

  function TFormSMWizardBase.getOutputCleared(): boolean; begin result := _outputCleared; end;
//-----------------------------------------------------------------------------


end.
