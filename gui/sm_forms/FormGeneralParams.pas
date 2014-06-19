unit FormGeneralParams;

(*
FormGeneralParams.pas/dfm
-------------------------
Begin: 2005/04/02
Last revision: $Date: 2013-06-27 19:11:26 $ $Author: areeves $
Version: $Revision: 1.28.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
  	// Widgets and standard Delphi controls
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Mask,
    DBCtrls,
    StdCtrls,
    ActnList,
    ToolWin,
    ActnMan,
    ActnCtrls,
    XPStyleActnCtrls,
    Buttons,
    ActnMenus,
    DB,
    ExtCtrls,
    REEdit,
    Menus,
    ActnPopupCtrl,

    // Inherited classes
    FormSMWizardBase,

    // Application-specific data structures
    SMDatabase
  ;

  type TFormGeneralParams = class( TFormSMWizardBase )
    	pnlHeader: TPanel;
    	pnlParams: TPanel;

      lblDescr: TLabel;
      mmoDescr: TMemo;

      lblIterations: TLabel;
    	rleIterations: TREEdit;

    	lblIterationNumber: TLabel;
      lblRepNo: TLabel;

      lblTimeToRun: TLabel;
      lblTimeToRun2: TLabel;

      gbxRandomSeed: TGroupBox;
      rdoAutoSeed: TRadioButton;
      rdoSpecifySeed: TRadioButton;
      lblSeedValue: TLabel;
      rleSeedValue: TREEdit;

      procedure dataChanged( sender: TObject );
      procedure radioClick(Sender: TObject);

    public
      constructor create( AOwner: TComponent ); override;

    protected
      _dataChanged: boolean;
      _criticalDataChanged: boolean;

      procedure translateUI();      

      procedure initializeFromSim(); override;
      procedure updateScenarioData(); override;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

      procedure updateDisplay();
    end
  ;

	const
  	DBFORMGENERALPARAMS: boolean = false; // set to true to display debugging messages for this unit.

implementation


	{$R *.dfm}

  uses
    SqlClasses,
    RegExpDefs,
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    I88n,

    FormMain,

    SMSimulationInput
  ;

// ----------------------------------------------------------------------------
// Construction/initialization/destruction
// ----------------------------------------------------------------------------
	constructor TFormGeneralParams.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      rleIterations.InputExpression := RE_INTEGER_INPUT;
      rleSeedValue.InputExpression := RE_INTEGER_INPUT;

      _dataChanged := false;
      _criticalDataChanged := false;

      updateDisplay();
    end
  ;
  
  
  procedure TFormGeneralParams.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormGeneralParams.dfm
      // File date: Thu Nov 9 21:48:58 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Start setup' );
          Hint := tr( 'General description of the simulation session' );
          pnlHeader.Caption := tr( 'Start setup' );
          lblDescr.Caption := tr( 'Scenario description:' );
          lblTimeToRun.Caption := tr( '(Time to run)' );
          lblIterations.Caption := tr( 'Number of iterations' );
          lblIterationNumber.Caption := tr( 'Currently on iteration number' );
          lblTimeToRun2.Caption := tr( 'Time to run this set of iterations:' );
          gbxRandomSeed.Caption := tr( 'Random number generator seed' ) + ' ';
          lblSeedValue.Caption := tr( 'Seed value (integer):' );
          rdoAutoSeed.Caption := tr( 'Generate seed automatically' );
          rdoSpecifySeed.Caption := tr( 'Specify a seed value' );
        end
      ;

    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Display functions
// ----------------------------------------------------------------------------
  procedure TFormGeneralParams.updateDisplay();
    begin
      lblSeedValue.visible := rdoSpecifySeed.Checked;
      rleSeedValue.Visible := rdoSpecifySeed.Checked;
    end
  ;


  procedure TFormGeneralParams.dataChanged( sender: TObject );
  	begin
  		showStar();
      _dataChanged := true;

      if( sender <> mmoDescr ) then _criticalDataChanged := true;
    end
  ;


  procedure TFormGeneralParams.radioClick(Sender: TObject);
    begin
      updateDisplay();
      
      dataChanged( nil );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Data entry validation
// ----------------------------------------------------------------------------
  function TFormGeneralParams.dataIsValid(): boolean;
    begin
      Result := True;
      if( MyStrToInt( rleIterations.text, -1 ) <= 0 ) then
        begin
          msgOK(
            tr( 'At least one iteration must be entered.' ),
            tr( 'Invalid value' ),
            IMGWarning,
            self
          );
          rleIterations.SetFocus();
          Result := False;
          exit;
        end
      ;

      if( rdoSpecifySeed.Checked ) then
        begin
          if( '' = trim( rleSeedValue.Text ) ) then
            begin
              msgOK(
                tr( 'Please specify a seed value.' ),
                tr( 'Invalid value' ),
                IMGWarning,
                self
              );
              rleSeedValue.SetFocus();
              result := false;
              exit;
            end
          ;
        end
      ;

    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// Database functions
// ----------------------------------------------------------------------------
  procedure TFormGeneralParams.initializeFromSim();
    var
      sim: TSMSimulationInput;
    begin
      sim := _smScenarioCopy.simInput;

      mmoDescr.lines.Text := sim.scenarioDescr;

      if( 0 < sim.simIterations ) then
        rleIterations.Text := intToStr( sim.simIterations )
      else
        rleIterations.Text := '1'
      ;

      rdoSpecifySeed.checked := sim.useFixedRandomSeed;

      rleSeedValue.Text := intToStr( sim.randomSeed );

      _dataChanged := false;
      _criticalDataChanged := false;
      
      hideStar();
    end
  ;



  procedure TFormGeneralParams.updateScenarioData();
  	var
    	sim: TSMSimulationInput;
    begin
    	if( _dataChanged ) then
      	begin
        	dbcout( 'updateScenarioData', DBFORMGENERALPARAMS );
          sim := _smScenarioCopy.simInput;

          sim.scenarioDescr := mmoDescr.Lines.Text;
          sim.simIterations := MyStrToInt( rleIterations.Text, -1 );
          sim.useFixedRandomSeed := rdoSpecifySeed.Checked;

          if( rdoSpecifySeed.Checked ) then
            sim.randomSeed := MyStrToInt( rleSeedValue.Text, -1 )
          ;

          dbcout( 'useFixedRandomSeed: ' + uiBoolToText( rdoSpecifySeed.Checked ), DBFORMGENERALPARAMS );

          inherited updateScenarioData();
        end
      ;
    end
  ;


  function TFormGeneralParams.getDataUpdated(): boolean;
    begin
      result := _criticalDataChanged;
    end
  ;
// ----------------------------------------------------------------------------


initialization
	RegisterClass( TFormGeneralParams );

end.
