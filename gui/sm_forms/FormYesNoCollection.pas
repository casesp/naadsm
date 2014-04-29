unit FormYesNoCollection;

(*
FormYesNoCollection.pas/dfm
---------------------------
Begin: 2005/04/02
Last revision: $Date: 2013-06-27 19:11:29 $ $Author: areeves $
Version: $Revision: 1.25.6.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

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
    ExtCtrls,
    StdCtrls,
    DBCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    FormSMWizardBase,
    SMDatabase
  ;

  type TFormYesNo = class( TFormSMWizardBase )
      pnlCaption: TPanel;
      pnlBody: TPanel;
      grpButtons: TGroupBox;
      btnYes: TRadioButton;
      btnNo: TRadioButton;
      
      procedure valueChanged( sender: TObject );

    protected
      _valueChanged: boolean;
      _originalVal: boolean;
      _settingUp: boolean;

      procedure translateUI();

      procedure changed( sender:TObject ); virtual; abstract;

      // Nothing to validate.  Return true.
      function dataIsValid(): boolean; override;

      function getDataUpdated(): boolean; override;

    public
      constructor create( AOwner: TComponent ); override;

    end
  ;


  type TFormYesNoPrevalence = class( TFormYesNo )
    public
      constructor create( AOwner: TComponent ); override;

      procedure Changed( sender: TObject ); override;
      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
    end
  ;


  type TFormYesNoDetection = class( TFormYesNo )
    public
      constructor create( AOwner: TComponent ); override;

      procedure Changed( sender: TObject ); override;
      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
    end
  ;


  type TFormYesNoZones = class( TFormYesNo )
    public
      constructor create( AOwner: TComponent ); override;

      procedure Changed( sender: TObject ); override;
 			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
    end
  ;

  
  type TFormYesNoCustomOutputs = class( TFormYesNo )
    public
      constructor create( AOwner: TComponent ); override;

      procedure Changed( sender: TObject ); override;
 			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
    end
  ;

implementation

{$R *.dfm}

  uses
    MyDialogs,
    MyStrUtils,
    ControlUtils,
    I88n
  ;

//-----------------------------------------------------------------------------
// TFormYesNo: The base class
//-----------------------------------------------------------------------------
	constructor TFormYesNo.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      horizCenter( grpButtons, pnlBody );
    end
  ;

  procedure TFormYesNo.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormYesNoCollection.dfm
      // File date: Fri Jan 5 12:41:06 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters' );
          pnlCaption.Caption := tr( 'FORM CAPTION' );
          grpButtons.Caption := tr( 'grpButtons' );
          btnYes.Caption := tr( 'btnYes' );
          btnNo.Caption := tr( 'btnNo' );
        end
      ;

    end
  ;


  function TFormYesNo.dataIsValid(): boolean;
  	begin
   		result := true;
    end
  ;

	procedure TFormYesNo.valueChanged( sender: TObject );
  	begin
    	if( not _settingUp ) then
      	begin
          if( btnYes.checked = _originalVal ) then
            begin
              _valueChanged := false;
              hideStar();
            end
          else
            begin
              _valueChanged := true;
              showStar();
            end
          ;
          Changed( sender );
        end
      ;
    end
  ;


  function TFormYesNo.getDataUpdated(): boolean;
    begin
      result := _valueChanged;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TFormYesNoPrevalence
//-----------------------------------------------------------------------------
	constructor TFormYesNoPrevalence.create( AOwner: TComponent );
  	begin
    	inherited create( AOwner );
      self.Name := 'FormYesNoPrevalence';
      self.Caption := tr( 'Scenario parameters: Disease options' );
    	pnlCaption.Caption := tr( 'Disease options' );
      grpButtons.Caption := tr( 'Would you like to include within-unit prevalence?' ) + ' ';
      btnYes.Caption := tr( 'Yes, use within-unit prevalence' );
      btnNo.Caption := tr( 'No, use a specified probability of infection transfer instead' );
      //field := Detection;
    end
  ;


  procedure TFormYesNoPrevalence.Changed( sender: TObject );
    begin
      _smScenarioCopy.simInput.useWithinHerdPrevalence := btnYes.Checked;
    end
  ;


  function TFormYesNoPrevalence.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
      frm:TForm;
    begin
      _settingUp := true;
      frm := self as TForm;
      formDisplayed := true;
      _originalVal := _smScenarioCopy.simInput.useWithinHerdPrevalence;
      btnYes.Checked := _smScenarioCopy.simInput.useWithinHerdPrevalence;
      btnNo.Checked := not _smScenarioCopy.simInput.useWithinHerdPrevalence;
   		nextForm := nextFormToShow;
      _settingUp := false;
      _currentFormIndex := currentFormIndex;

      result := frm.showModal();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TFormYesNoDetection
//-----------------------------------------------------------------------------
	constructor TFormYesNoDetection.create( AOwner: TComponent );
  	begin
    	inherited create( AOwner );
      self.Name := 'FormYesNoDetection';
      self.Caption := tr( 'Scenario parameters: Detection options' );
    	pnlCaption.Caption := tr( 'Detection options' );
      grpButtons.Caption := tr( 'Would you like to include DISEASE DETECTION in simulation runs?' ) + ' ';
      btnYes.Caption := tr( 'Yes, include detection' );
      btnNo.Caption := tr( 'No, do not include detection' );
      //field := Detection;
    end
  ;


  procedure TFormYesNoDetection.Changed( sender: TObject );
    begin
      _smScenarioCopy.simInput.controlParams.useDetectionGlobal := btnYes.Checked;
    end
  ;


  function TFormYesNoDetection.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
      frm:TForm;
    begin
      _settingUp := true;
      frm := self as TForm;
      formDisplayed := true;
      _originalVal := _smScenarioCopy.simInput.includeDetectionGlobal;
      btnYes.Checked := _smScenarioCopy.simInput.includeDetectionGlobal;
      btnNo.Checked := not _smScenarioCopy.simInput.includeDetectionGlobal;
   		nextForm := nextFormToShow;
      _settingUp := false;
      _currentFormIndex := currentFormIndex;

      result := frm.showModal();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TFormYesNoZones
//-----------------------------------------------------------------------------
	constructor TFormYesNoZones.create( AOwner: TComponent );
  	begin
    	inherited create( AOwner );
      self.Name := 'FormYesNoZones';
      self.Caption := tr( 'Scenario parameters: zone options' );
    	pnlCaption.Caption := tr( 'Zone options' );
      grpButtons.Caption := tr( 'Would you like to include ZONES in simulation runs?' ) + ' ';
      btnYes.Caption := tr( 'Yes, include zones' );
      btnNo.Caption := tr( 'No, do not include zones' );
      //field := Zones;
    end
  ;


  procedure TFormYesNoZones.Changed( sender: TObject );
    begin
      _smScenarioCopy.simInput.controlParams.useZonesGlobal := btnYes.Checked;
    end
  ;


  function TFormYesNoZones.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
      frm:TForm;
    begin
      // must first detect disease in order to implement zone restrictions
      if ( _smScenarioCopy.simInput.includeDetectionGlobal ) then
        begin
          _settingUp := true;
          frm := self as TForm;
          formDisplayed := true;
          _originalVal := _smScenarioCopy.simInput.includeZonesGlobal;
          btnYes.Checked := _smScenarioCopy.simInput.includeZonesGlobal;
          btnNo.Checked := not _smScenarioCopy.simInput.includeZonesGlobal;
          nextForm := nextFormToShow;
          _settingUp := false;
          _currentFormIndex := currentFormIndex;

          result := frm.showModal();
        end
      else
        begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          _currentFormIndex := currentFormIndex;
          result := 0;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TFormYesNoCustomOutputs
//-----------------------------------------------------------------------------
	constructor TFormYesNoCustomOutputs.create( AOwner: TComponent );
  	begin
    	inherited create( AOwner );
      self.Name := 'FormYesNoCustomOutputs';
      self.Caption := tr( 'Scenario parameters: Custom outputs' );
    	pnlCaption.Caption := tr( 'Custom output options' );
      grpButtons.Caption := tr( 'Would you like to define and use CUSTOM OUTPUTS?' ) + ' ';
      btnYes.Caption := tr( 'Yes, use custom output definitions (EXPERIMENTAL FEATURE FOR EXPERTS ONLY)' );
      btnNo.Caption := tr( 'No, do not use custom output definitions' );
      //field := Costs;
    end
  ;


  procedure TFormYesNoCustomOutputs.Changed( sender: TObject );
    begin
      _smScenarioCopy.simInput.useCustomOutputs := btnYes.Checked;
      btnNext.Enabled := _smScenarioCopy.simInput.useCustomOutputs;
    end
  ;


  function TFormYesNoCustomOutputs.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
      frm:TForm;
    begin
      _settingUp := true;
      frm := self as TForm;
      _originalVal := _smScenarioCopy.simInput.useCustomOutputs;
      btnYes.Checked := _smScenarioCopy.simInput.useCustomOutputs;
      btnNo.Checked := not _smScenarioCopy.simInput.useCustomOutputs;
      btnNext.Enabled := _smScenarioCopy.simInput.useCustomOutputs;
      formDisplayed := true;
   		nextForm := nextFormToShow;
      _settingUp := false;
      _currentFormIndex := currentFormIndex;
      
      result := frm.showModal();
    end
  ;
//-----------------------------------------------------------------------------

initialization

  registerClass( TFormYesNoPrevalence );
  registerClass( TFormYesNoDetection );
  registerClass( TFormYesNoZones );
  registerClass( TFormYesNoCustomOutputs );

end.

