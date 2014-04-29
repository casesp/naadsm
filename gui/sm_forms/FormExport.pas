unit FormExport;

(*
FormExport.pas/dfm
------------------
Begin: 2005/03/15
Last revision: $Date: 2008/04/18 20:35:16 $ $Author: areeves $
Version: $Revision: 1.29 $
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
  StdCtrls,
  IniHandler,

  SMScenario,
  SMSimulationInput
;

type TExportResult = (
	ExpUnspecified,
  ExpSuccess,
  ExpFileFailure,
  ExpValidationFailure
);


type TFormExport = class(TForm)
    leScenario: TEdit;
    leHerds: TEdit;
    btnScenario: TButton;
    btnHerds: TButton;
    lblScenario: TLabel;
    lblHerds: TLabel;
    btnExport: TButton;
    GroupBox1: TGroupBox;
    cbxScenario: TCheckBox;
    cbxHerds: TCheckBox;
    SaveDialog1: TSaveDialog;
    btnCancel: TButton;
    cbxWriteOutputs: TCheckBox;
    
    procedure FormCreate(Sender: TObject );
    
    procedure btnScenarioClick(Sender: TObject);
    procedure btnHerdsClick(Sender: TObject);

    // FIX ME: do some major error checking in here!
    procedure btnExportClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbxScenarioClick(Sender: TObject);
    procedure cbxHerdsClick(Sender: TObject);

  private
  	_ini: TIniHandler;
    _scenario: TSMScenario;

    function exportScenario( smSim: TSMSimulationInput; scenarioFileName: string ): TExportResult;
		function exportHerdList( smSim: TSMSimulationInput; herdFileName: string ): TExportResult;

    procedure setScenarioEnabled( val: boolean );
    procedure setHerdsEnabled( val: boolean );

  protected
    procedure translateUI();

  public
    constructor create( AOwner: TComponent; scenario: TSMScenario; iniSettings: TIniHandler ); reintroduce;
    destructor destroy(); override;

  end
;


implementation

{$R *.dfm}

  uses
    StrUtils,
    
    MyStrUtils,
    GuiStrUtils,
    MyDialogs,
    DialogLongMessage,
    Herd,
    StringConsts,
    ControlUtils,
    I88n
  ;


  constructor TFormExport.create( AOwner: TComponent; scenario: TSMScenario; iniSettings: TIniHandler );
    begin
      inherited create( AOwner );
      translateUI();

      _ini := iniSettings;
      _scenario := scenario;

      leScenario.Text := _ini.str('LastScenarioExportFile' );
      leHerds.Text := _ini.str( 'LastHerdExportFile' );

      setScenarioEnabled( false );
      setHerdsEnabled( false );
    end
  ;


  procedure TFormExport.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormExport.dfm
      // File date: Mon Oct 30 19:41:36 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Export scenario' );
          lblScenario.Caption := tr( 'Scenario parameters file:' );
          lblHerds.Caption := tr( 'List of units:' );
          btnScenario.Caption := tr( 'Browse...' );
          btnHerds.Caption := tr( 'Browse...' );
          btnExport.Caption := tr( 'Export' );
          GroupBox1.Caption := tr( 'Items to import:' );
          cbxScenario.Caption := tr( 'Export scenario parameters file' );
          cbxHerds.Caption := tr( 'Export list of units' );
          cbxWriteOutputs.Caption := tr( 'Include output specification for NAADSM/SC' );
          btnCancel.Caption := tr( 'Cancel' );
          SaveDialog1.Filter := tr( 'XML files (*.xml)|*.xml|All files (*.*)|*.*' );
        end
      ;

    end
  ;


  procedure TFormExport.FormCreate(Sender: TObject);
    begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if( Screen.PixelsPerInch <> 96 ) then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
    end
  ;
	

  destructor TFormExport.destroy();
    begin
      if( cbxScenario.Checked ) then
        _ini.update( 'LastScenarioExportFile', leScenario.Text )
      ;

      if( cbxHerds.Checked ) then
        _ini.update( 'LastHerdExportFile', leHerds.Text )
      ;

      inherited destroy();
    end
  ;


  function TFormExport.exportScenario( smSim: TSMSimulationInput; scenarioFileName: string ): TExportResult;
    var
      simIsValid: boolean;
      dlgLongMessage: TDialogLongMessage;
      err: string;
      tmpCursor: TCursor;
    begin
      err := '';

      simIsValid := smSim.isValid( true, @err );

      if( simIsValid ) then
        begin
          if( smSim.writeXMLFile( scenarioFileName, cbxWriteOutputs.Checked ) ) then
            result := ExpSuccess
          else
            begin
              tmpCursor := screen.Cursor;
              screen.Cursor := crDefault;

              msgOK(
                tr( 'This scenario file could not be written.  Please check your file name and available disk space.' ),
                tr( 'Cannot write scenario file' ),
                IMGCritical,
                self
              );

              screen.Cursor := tmpCursor;

              result := ExpFileFailure;
            end
          ;
        end
      else
        begin
          tmpCursor := screen.Cursor;
          screen.Cursor := crDefault;

          dlgLongMessage := TDialogLongMessage.create(
            self,
            SHORTERMASTERCAPTION,
            tr( 'Problems were found with this scenario.  These problems must be corrected before this scenario can be exported:' ),
            err
          );
          dlgLongMessage.showModal();
          dlgLongMessage.free();

          screen.Cursor := tmpCursor;

          result := ExpValidationFailure;
        end
      ;

    end
  ;


  function TFormExport.exportHerdList( smSim: TSMSimulationInput; herdFileName: string ): TExportResult;
    var
      localHerdList: THerdList;
      herdsAreValid: boolean;
      dlgLongMessage: TDialogLongMessage;
      err: string;

      tmpCursor: TCursor;
    begin
      err := '';

      localHerdList := _scenario.herdList;

      herdsAreValid := localHerdList.isValid( @err );

      if( herdsAreValid ) then
        begin
          //dbcout( 'Writing herd list to file ' + herdFileName );

          if( localHerdList.writeXMLFile( herdFileName ) ) then
            result := ExpSuccess
          else
            begin
              tmpCursor := screen.Cursor;
              screen.Cursor := crDefault;

              msgOK(
                tr( 'This unit file could not be written.  Please check your file name and available disk space.' ),
                tr( 'Cannot write unit file' ),
                IMGCritical,
                self
              );

              screen.Cursor := tmpCursor;

              result := ExpFileFailure;
            end
          ;

        end
      else
        begin
          tmpCursor := screen.Cursor;
          screen.Cursor := crDefault;

          dlgLongMessage := TDialogLongMessage.create(
            self,
            SHORTERMASTERCAPTION,
            'Problems were found with this herd list.  These problems' + endl
              + ' must be corrected before this herd list can be exported:',
            err
          );
          dlgLongMessage.showModal();
          dlgLongMessage.free();

          screen.Cursor := tmpCursor;

          result := ExpValidationFailure;
        end
      ;
    end
  ;



  procedure TFormExport.btnExportClick(Sender: TObject);
    var
      sfn, hfn: string;
      sResult, hResult: TExportResult;
      scenarioSuccess, herdSuccess, finalSuccess: boolean;
    begin
      sResult := ExpUnspecified;
      hResult := ExpUnspecified;

      screen.Cursor := crHourglass;

      sfn := trim( leScenario.Text );
      hfn := trim( leHerds.Text );


      if ( LowerCase( RightStr( sfn, 4 ) ) <> '.xml'  )  then
        sfn := sfn + '.xml';

      if ( LowerCase( RightStr( hfn, 4 ) ) <> '.xml'  )  then
        hfn := hfn + '.xml';

      if( cbxScenario.Checked ) then sResult := exportScenario( _scenario.simInput, sfn );
      if( cbxHerds.Checked ) then hResult := exportHerdList( _scenario.simInput, hfn );

      // This mess will indicate whether everything that should have been exported was successfully exported
      scenarioSuccess := ( ( cbxScenario.Checked and ( ExpSuccess = sResult ) ) or ( not cbxScenario.Checked ) );
      herdSuccess := ( ( cbxHerds.Checked and ( ExpSuccess = hResult ) ) or ( not cbxHerds.Checked ) );
      finalSuccess := scenarioSuccess and herdSuccess;

      screen.Cursor := crDefault;

      // If either export failed due to validation problems, close this form.
      // (There is nothing else that the user can do here.)

      // If either export failed because of file problems, leave this form open.
      // (The user might be able to fix the file name or select a different one.)

      // If export was successful, show a dialog box to that effect

      // Default action: close this form.

      if( ( ExpValidationFailure = sResult ) or ( ExpValidationFailure = hResult ) ) then
        close()
      else if( ( ExpFileFailure = sResult ) or ( ExpFileFailure = hResult ) ) then
        // don't do anything
      else if( finalSuccess ) then
        begin
          msgOK(
            tr( 'The file export process was successful.' ),
            SHORTMASTERCAPTION,
            IMGSuccess,
            Self
          );
          close();
        end
      else
        close()
      ;
    end
  ;


  procedure TFormExport.setScenarioEnabled( val: boolean );
    begin
      lblScenario.Enabled := val;
      leScenario.Enabled := val;
      btnScenario.Enabled := val;
      cbxWriteOutputs.Enabled := val;

      btnExport.Enabled := ( cbxScenario.Checked or cbxHerds.Checked );
    end
  ;


  procedure TFormExport.setHerdsEnabled( val: boolean );
    begin
      lblHerds.Enabled := val;
      leHerds.Enabled := val;
      btnHerds.Enabled := val;

      btnExport.Enabled := ( cbxScenario.Checked or cbxHerds.Checked );
    end
  ;


  procedure TFormExport.btnScenarioClick(Sender: TObject);
    begin
      saveDialog1.Title := tr( 'Create a scenario parameter file' );

      if( length( trim(leScenario.text) ) > 0 ) then
        saveDialog1.fileName := trim( leScenario.text )
      else if( _ini.hasKey( 'LastExportDirectory' ) ) then
        saveDialog1.initialDir := _ini.val( 'LastExportDirectory' )
      ;

      if saveDialog1.Execute() then
        begin
          leScenario.text := saveDialog1.FileName;
          _ini.update( 'LastExportDirectory', directory( saveDialog1.FileName ) );
        end
      ;
    end
  ;


  procedure TFormExport.btnHerdsClick(Sender: TObject);
    begin
      saveDialog1.Title := tr( 'Create a unit file' );

      if( length( trim(leHerds.text) ) > 0 ) then
        saveDialog1.fileName := trim( leScenario.text )
      else if( _ini.hasKey( 'LastExportDirectory' ) ) then
        saveDialog1.initialDir := _ini.val( 'LastExportDirectory' )
      ;

      if saveDialog1.Execute() then
        begin
          leHerds.text := saveDialog1.FileName;
          _ini.update( 'LastExportDirectory', directory( saveDialog1.FileName ) );
        end
      ;
    end
  ;


  procedure TFormExport.cbxScenarioClick(Sender: TObject);
    begin
      setScenarioEnabled( cbxScenario.Checked );
    end
  ;


  procedure TFormExport.cbxHerdsClick(Sender: TObject);
    begin
      setHerdsEnabled( cbxHerds.Checked );
    end
  ;


  procedure TFormExport.btnCancelClick(Sender: TObject);
    begin
      close();
    end
  ;


end.
