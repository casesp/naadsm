unit FormCostOptions;

(*
FormCostOptions.pas/dfm
----------------------
Begin: 2007/04/17
Last revision: $Date: 2013-06-27 19:11:24 $ $Author: areeves $
Version: $Revision: 1.4.6.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2007 - 2009 Colorado State University

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

  type TFormCostOptions = class( TFormSMWizardBase )
      pnlCaption: TPanel;
      pnlBody: TPanel;
      grpButtons: TGroupBox;
      cbxDestrCosts: TCheckBox;
      cbxVaccCosts: TCheckBox;
      cbxSurvCosts: TCheckBox;
      
      procedure valueChanged( sender: TObject );

    protected
      _valueChanged: boolean;
      _settingUp: boolean;

      procedure translateUI();

      // Nothing to validate.  Return true.
      function dataIsValid(): boolean; override;

      function getDataUpdated(): boolean; override;

    public
      constructor create( AOwner: TComponent ); override;

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

  constructor TFormCostOptions.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      horizCenter( grpButtons, pnlBody );
    end
  ;


  procedure TFormCostOptions.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:28:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormCostOptions.dfm
      // File date: Wed Apr 25 11:56:53 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Cost accounting options' );
          pnlCaption.Caption := tr( 'Cost accounting options' );
          grpButtons.Caption := tr( 'For which control measures do you want to track DIRECT COSTS?' );
          cbxDestrCosts.Caption := tr( 'Track costs of destruction' );
          cbxVaccCosts.Caption := tr( 'Track costs of vaccination' );
          cbxSurvCosts.Caption := tr( 'Track costs of surveillance in zones' );
        end
      ;

    end
  ;


  function TFormCostOptions.dataIsValid(): boolean;
  	begin
   		result := true;
    end
  ;


  procedure TFormCostOptions.valueChanged( sender: TObject );
  	begin
    	if( not _settingUp ) then
      	begin
          _smScenarioCopy.simInput.costTrackDestruction := cbxDestrCosts.Checked;
          _smScenarioCopy.simInput.costTrackVaccination := cbxVaccCosts.Checked;
          _smScenarioCopy.simInput.costTrackZoneSurveillance := cbxSurvCosts.Checked;

          _valueChanged := ( cbxDestrCosts.Checked <> _smScenarioOriginal.simInput.costTrackDestruction )
            or ( cbxVaccCosts.Checked <>  _smScenarioOriginal.simInput.costTrackVaccination )
            or ( cbxSurvCosts.Checked <> _smScenarioOriginal.simInput.costTrackZoneSurveillance )
          ;

          if( _valueChanged ) then
            showStar()
          else
            hideStar()
          ;
        end
      ;
    end
  ;


  function TFormCostOptions.getDataUpdated(): boolean;
    begin
      result := _valueChanged;
    end
  ;


  function TFormCostOptions.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
      frm: TForm;
    begin
      if
        // there are no control measure costs unless disease can be detected
        (  _smScenarioCopy.simInput.includeDetectionGlobal  )
      and
        // at least one type of control measure is being simulated
        (( _smScenarioCopy.simInput.includeDestructionGlobal )
      or
        ( _smScenarioCopy.simInput.includeVaccinationGlobal )
      or
        ( _smScenarioCopy.simInput.includeZonesGlobal ))
      then // This form should be shown
        begin
          _settingUp := true;
          frm := self as TForm;
          formDisplayed := true;

          if( _smScenarioCopy.simInput.includeDestructionGlobal ) then
            begin
              cbxDestrCosts.Enabled := true;
              cbxDestrCosts.Checked := _smScenarioCopy.simInput.costTrackDestruction;
            end
          else
            cbxDestrCosts.Enabled := false
          ;

          if( _smScenarioCopy.simInput.includeVaccinationGlobal ) then
            begin
              cbxVaccCosts.Enabled := true;
              cbxVaccCosts.Checked := _smScenarioCopy.simInput.costTrackVaccination;
            end
          else
            cbxVaccCosts.Enabled := false
          ;

          if( _smScenarioCopy.simInput.includeZonesGlobal ) then
            begin
              cbxSurvCosts.Enabled := true;
              cbxSurvCosts.Checked := _smScenarioCopy.simInput.costTrackZoneSurveillance;
            end
          else
            cbxSurvCosts.Enabled := false
          ;

          nextForm := nextFormToShow;
          _settingUp := false;
          _currentFormIndex := currentFormIndex;

          result := frm.showModal();
        end
      else // there is no reason to display this form
        begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



initialization

  registerClass( TFormCostOptions );

end.

