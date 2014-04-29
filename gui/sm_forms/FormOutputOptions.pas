unit FormOutputOptions;

(*
FormOutputOptions.pas/dfm
-------------------------
Begin: 2005/07/06
Last revision: $Date: 2008/04/18 20:35:16 $ $Author: areeves $
Version number: $Revision: 1.18 $
Project: (various)
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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
    Buttons,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls,
    Spin,
    Menus,
    ActnPopupCtrl,

    FrameFileSelector,

    FormSMWizardBase,

    SMOutputOptions
  ;

  type TFormOutputOptions = class( TFormSMWizardBase )
      pnlCaption: TPanel;
      pnlBasic: TPanel;
      lblInstructions: TLabel;
      gbxDailyStates: TGroupBox;
      cbxDailyStates: TCheckBox;
      fraDailyStatesFile: TFrameFileSelector;
      gbxDailyOutputIterations: TGroupBox;
      rdoDailyOutputAll: TRadioButton;
      rdoDailyOutputSpecified: TRadioButton;
      lblDailyOutputIterations: TLabel;
      speDailyOutputIterations: TSpinEdit;

      gbxEventsAndExposures: TGroupBox;
      cbxEvents: TCheckBox;
      cbxExposures: TCheckBox;

      gbxMapOutputs: TGroupBox;
      cbxMapOutput: TCheckBox;
      btnMapOptions: TButton;
      lblNAADSMapFolder: TLabel;
      btnChooseNAADSMapDir: TButton;
      lblNAADSMapLabel: TLabel;

      procedure updateControls( Sender: TObject );
    procedure btnChooseNAADSMapDirClick(Sender: TObject);

    protected
      _options: TSMOutputOptions;
      _naadsMapDir: string;

      procedure translateUI();

      procedure initializeFromSim(); override;

      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

    public
    	constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

      procedure updateMasterDisplay(); override;

    end
  ;


implementation

{$R *.dfm}

  uses
    MyDialogs,
    MyStrUtils,
    GuiStrUtils,
    ControlUtils,
    I88n,

    FormFolderSelect,

    SMSimulationInput
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormOutputOptions.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      centerChildren( pnlBasic, false );

      _options := nil;
    end
  ;


  procedure TFormOutputOptions.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormOutputOptions.dfm
      // File date: Mon Mar 19 11:05:32 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Output options' );
          pnlCaption.Caption := tr( 'Output options' );
          lblInstructions.Caption := tr( 'Select optional outputs that you would like to generate and record from those listed below.  Some of these selections will result in decreased performance and very large scenario database files: use these options with caution.' );
          gbxDailyStates.Caption := tr( 'Daily unit states' );
          cbxDailyStates.Caption := tr( 'Write a plain text file containing daily states for all units' );
          fraDailyStatesFile.SaveDialog1.Filter := tr( 'Plain text file (*.txt)|*.txt|All files (*.*)|*.*' );
          gbxDailyOutputIterations.Caption := tr( 'Daily output for iterations' );
          lblDailyOutputIterations.Caption := tr( 'Number of iterations:' );
          rdoDailyOutputAll.Caption := tr( 'Save all daily output for every iteration (warning: this option may produce very large scenario files)' );
          rdoDailyOutputSpecified.Caption := tr( 'Save all daily output for a specified number of iterations' );
          gbxEventsAndExposures.Caption := tr( 'Daily events and exposures' );
          cbxEvents.Caption := tr( 'Save information for each event (scenario files may be very large)' );
          cbxExposures.Caption := tr( 'Save information for each exposure (scenario files may be very large)' );
          gbxMapOutputs.Caption := tr( 'NAADSMap output' );
          lblNAADSMapFolder.Caption := tr( 'lblNAADSMapDir' );
          lblNAADSMapLabel.Caption := tr( 'Folder for NAADSMap output:' );
          cbxMapOutput.Caption := tr( 'Generate NAADSMap output' );
          btnMapOptions.Caption := tr( 'Options...' );
          btnChooseNAADSMapDir.Caption := tr( 'Select folder...' );
        end
      ;

    end
  ;


  destructor TFormOutputOptions.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------
  procedure TFormOutputOptions.initializeFromSim();
    begin
      _options := _smScenarioCopy.simInput.outputOptions;

      cbxDailyStates.Checked := _options.writeDailyStatesFile;
      fraDailyStatesFile.fileName := _options.dailyStatesFileName;

      rdoDailyOutputAll.Checked := _options.saveAllDailyOutputs;
      rdoDailyOutputSpecified.Checked := not( _options.saveAllDailyOutputs );

      speDailyOutputIterations.Value := _options.dailyOutputsForIterations;

      cbxEvents.Checked := _options.saveDailyEvents;
      cbxExposures.Checked := _options.saveDailyExposures;

      cbxMapOutput.Checked := _options.writeNAADSMapOutput;

      _naadsMapDir := _options.NAADSMapOutputDirectory;
      if( 0 < length( _naadsMapDir ) ) then
        lblNAADSMapFolder.Caption := _naadsMapDir
      else
        lblNAADSMapFolder.Caption := tr( '(No folder selected)' )
      ;

      updateMasterDisplay();
    end
  ;


  function TFormOutputOptions.dataIsValid(): boolean;
    var
      msg: string;
    begin
      getDataUpdated();

      result := _options.validate( @msg );

      // The only reason that false might be returned is if
      // a file name is not specified.
      if( false = result ) then
        msgOK( msg, tr( 'Data entry problem' ), IMGWarning, self )
      ;
    end
  ;


  function TFormOutputOptions.getDataUpdated(): boolean;
    begin
      if( _options.writeDailyStatesFile <> cbxDailyStates.Checked ) then
        _options.writeDailyStatesFile := cbxDailyStates.Checked
      ;

      if( _options.writeDailyStatesFile ) then
        begin
          if( _options.dailyStatesFileName <> fraDailyStatesFile.fileName ) then
            _options.dailyStatesFileName := fraDailyStatesFile.fileName
          ;
        end
      ;

      if( _options.saveAllDailyOutputs <> rdoDailyOutputAll.Checked ) then
        _options.saveAllDailyOutputs := rdoDailyOutputAll.Checked
      ;

      if( _options.dailyOutputsForIterations <> speDailyOutputIterations.Value ) then
        _options.dailyOutputsForIterations := speDailyOutputIterations.Value
      ;

      if( _options.saveDailyExposures <> cbxExposures.Checked ) then
        _options.saveDailyExposures := cbxExposures.Checked
      ;

      if( _options.saveDailyEvents <> cbxEvents.Checked ) then
        _options.saveDailyEvents := cbxEvents.Checked
      ;

      if( _options.writeNAADSMapOutput <> cbxMapOutput.Checked ) then
        _options.writeNAADSMapOutput := cbxMapOutput.Checked
      ;

      if( _options.writeNAADSMapOutput ) then
        begin
          if( _options.NAADSMapOutputDirectory <> _naadsMapDir ) then
            _options.NAADSMapOutputDirectory := _naadsMapDir
          ;
        end
      ;

      result := _options.updated;
    end
  ;


  procedure TFormOutputOptions.btnChooseNAADSMapDirClick(Sender: TObject);
    var
      frm: TFormFolderSelect;
    begin
      frm := TFormFolderSelect.create( self );

      if(  mrOK = frm.ShowModal() ) then
        begin
          _naadsMapDir := frm.selectedDirectory;
          lblNAADSMapFolder.Caption := abbrevPath( frm.selectedDirectory, 50 );
        end
      ;

      frm.Free();
    end
  ;

//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  procedure TFormOutputOptions.updateMasterDisplay();
    begin
      updateControls( nil );
    end
  ;


  procedure TFormOutputOptions.updateControls( Sender: TObject );
    begin
      fraDailyStatesFile.Visible := cbxDailyStates.Checked;
      lblDailyOutputIterations.Visible := rdoDailyOutputSpecified.Checked;
      speDailyOutputIterations.Visible := rdoDailyOutputSpecified.Checked;

      lblNAADSMapLabel.Visible := cbxMapOutput.Checked;
      lblNAADSMapFolder.Visible := cbxMapOutput.Checked;
      btnChooseNAADSMapDir.Visible := cbxMapOutput.Checked;
    end
  ;
//-----------------------------------------------------------------------------



initialization
	RegisterClass( TFormOutputOptions );

end.
