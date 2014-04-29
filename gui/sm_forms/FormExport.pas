unit FormExport;

(*
FormExport.pas/dfm
------------------
Begin: 2005/03/15
Last revision: $Date: 2013-06-27 19:11:26 $ $Author: areeves $
Version: $Revision: 1.35.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

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
    ExtCtrls,

    REEdit,

    FrameAcceptCancel,

    IniHandler,

    SMScenario,
    SMDatabase,
    SMSimulationInput
  ;

  type TExportResult = (
    ExpUnspecified,
    ExpSuccess,
    ExpFileFailure,
    ExpValidationFailure
  );


  type TFormExport = class(TForm)
      gbxScenarioExport: TGroupBox;
      SaveDialog1: TSaveDialog;
      cbxWriteOutputs: TCheckBox;
      gbxIterationEnd: TGroupBox;
      rdoOutbreakEnd: TRadioButton;
      rdoDiseaseEnd: TRadioButton;
      rdoFirstDetection: TRadioButton;
      rdoSpecDay: TRadioButton;
      pnlSpacer: TPanel;
      btnScenario: TButton;
      leScenario: TEdit;
      lblScenario: TLabel;
      GroupBox1: TGroupBox;
      cbxScenario: TCheckBox;
      cbxHerds: TCheckBox;
      GroupBox2: TGroupBox;
      btnHerds: TButton;
      leHerds: TEdit;
      lblHerds: TLabel;
      pnlSpacer2: TPanel;
      pnlSpacer3: TPanel;
      pnlSpacer4: TPanel;
      pnlButtons: TPanel;
      btnExport: TButton;
      btnCancel: TButton;
      fraAcceptCancel: TFrameAcceptCancel;
      rleSpecDay: TREEdit;
    
      procedure FormCreate(Sender: TObject );
    
      procedure btnScenarioClick(Sender: TObject);
      procedure btnHerdsClick(Sender: TObject);

      // FIX ME: do some major error checking in here!
      procedure btnExportClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
      procedure cbxScenarioClick(Sender: TObject);
      procedure cbxHerdsClick(Sender: TObject);
      procedure rdoEndOptionClick(Sender: TObject);
      procedure rleSpecDayEnter(Sender: TObject);
      procedure rleSpecDayExit(Sender: TObject);
      procedure rleSpecDayKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure fraAcceptCancelbtnAcceptClick(Sender: TObject);
      procedure fraAcceptCancelbtnCancelClick(Sender: TObject);

    protected
      _ini: TIniHandler;
      _scenario: TSMScenario;
      _stopReason: TStopReason;
      _nDays: integer;

      function exportScenario( smSim: TSMSimulationInput; scenarioFileName: string ): TExportResult;
      function exportHerdList( smSim: TSMSimulationInput; herdFileName: string ): TExportResult;

      procedure setScenarioEnabled( val: boolean );
      procedure setHerdsEnabled( val: boolean );
      procedure setBtnExportEnabled();

      procedure translateUI();
      procedure translateUIManual();

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
    MyDialogs,
    DialogLongMessage,
    Herd,
    StringConsts,
    ControlUtils,
    I88n,
    RegExpDefs,
    DebugWindow
  ;


  constructor TFormExport.create( AOwner: TComponent; scenario: TSMScenario; iniSettings: TIniHandler );
    begin
      inherited create( AOwner );
      translateUI();

      _ini := iniSettings;
      _scenario := scenario;

      leScenario.Text := _ini.str('LastScenarioExportFile' );
      leHerds.Text := _ini.str( 'LastHerdExportFile' );

      rleSpecDay.InputExpression := RE_INTEGER_INPUT;
      
      _stopReason := ssStopAtEndOfOutBreak;
      _nDays := -1;
      
      setScenarioEnabled( false );
      setHerdsEnabled( false );
    end
  ;


  procedure TFormExport.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.3.
      // Generation date: Thu Nov 5 16:48:07 2009
      // File name: C:/Documents and Settings/areeves/My Documents/NAADSM/Interface-naadsm_3_line_interface/sm_forms/FormExport.dfm
      // File date: Mon Aug 24 11:20:25 2009

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Export scenario' );
          gbxScenarioExport.Caption := tr( 'Scenario parameters:' );
          lblScenario.Caption := tr( 'Scenario parameters file:' );
          cbxWriteOutputs.Caption := tr( 'Include output specification for NAADSM/SC' );
          gbxIterationEnd.Caption := tr( 'Iteration end condition:' );
          rdoOutbreakEnd.Caption := tr( 'End of outbreak (including all control activities)' );
          rdoDiseaseEnd.Caption := tr( 'End of active disease phase' );
          rdoFirstDetection.Caption := tr( 'First detection' );
          rdoSpecDay.Caption := tr( 'Specific day' );
          btnScenario.Caption := tr( 'Browse...' );
          GroupBox1.Caption := tr( 'Files to export:' );
          cbxScenario.Caption := tr( 'Export scenario parameters file' );
          cbxHerds.Caption := tr( 'Export list of units' );
          GroupBox2.Caption := tr( 'List of units:' );
          lblHerds.Caption := tr( 'Units file:' );
          btnHerds.Caption := tr( 'Browse...' );
          btnExport.Caption := tr( 'Export' );
          btnCancel.Caption := tr( 'Cancel' );
          SaveDialog1.Filter := tr( 'XML files (*.xml)|*.xml|All files (*.*)|*.*' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormExport.translateUIManual();
    begin
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

      simIsValid := smSim.isValid( true, _scenario.herdList, @err );

      if( simIsValid ) then
        begin
          if( smSim.writeXMLFile( scenarioFileName, cbxWriteOutputs.Checked, _stopReason, _nDays ) ) then
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
          dlgLongMessage.Release();

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

      herdsAreValid := localHerdList.isValid( smSim, @err );

      if( herdsAreValid ) then
        begin
          //dbcout( 'Writing herd list to file ' + herdFileName );

          // FIX ME: Use of the projected coordinate system is not currently allowed by herd export.
          if( localHerdList.writeXMLFile( herdFileName, smSim.herdRandomizationOptions, false ) ) then
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
          dlgLongMessage.Release();

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


      if ( '.xml' <> ansiLowerCase( RightStr( sfn, 4 ) ) )  then
        sfn := sfn + '.xml'
      ;

      if ( '.xml' <> ansiLowerCase( RightStr( hfn, 4 ) ) )  then
        hfn := hfn + '.xml'
      ;

      if( cbxScenario.Checked ) then
        sResult := exportScenario( _scenario.simInput, sfn )
      ;

      if( cbxHerds.Checked ) then
        hResult := exportHerdList( _scenario.simInput, hfn )
      ;

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
      gbxIterationEnd.Enabled := val;
      setChildrenEnabled( gbxIterationEnd, val );

      setBtnExportEnabled();
    end
  ;


  procedure TFormExport.setHerdsEnabled( val: boolean );
    begin
      lblHerds.Enabled := val;
      leHerds.Enabled := val;
      btnHerds.Enabled := val;

      setBtnExportEnabled();
    end
  ;


  procedure TFormExport.setBtnExportEnabled();
    begin
      _nDays := myStrToInt( rleSpecDay.Text, -1 );

      if( not( cbxScenario.Checked or cbxHerds.Checked ) ) then
        btnExport.Enabled := false
      else if( rdoSpecDay.Checked and ( 0 >= _nDays ) ) then
        btnExport.Enabled := false
      else
        btnExport.Enabled := true
      ;
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


  procedure TFormExport.fraAcceptCancelbtnAcceptClick(Sender: TObject);
    begin
      fraAcceptCancel.Visible := false;
      setBtnExportEnabled();
    end
  ;



  procedure TFormExport.fraAcceptCancelbtnCancelClick(Sender: TObject);
    begin
      if( rleSpecDay.Visible ) then
        rleSpecDay.Text := ''
      ;

      fraAcceptCancel.Visible := false;
      setBtnExportEnabled();
    end
  ;


  procedure TFormExport.rleSpecDayExit(Sender: TObject);
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


  procedure TFormExport.rleSpecDayKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      if( 13 = key ) then
        fraAcceptCancelbtnAcceptClick( nil )
      else if( 27 = key ) then
        fraAcceptCancelbtnCancelClick( nil )
      ;
    end
  ;


  procedure TFormExport.rdoEndOptionClick(Sender: TObject);
    begin
      if( rdoOutbreakEnd.Checked ) then
        begin
          _stopReason := ssStopAtEndOfOutBreak;
          _nDays := -1;
        end
      else if( rdoDiseaseEnd.Checked ) then
        begin
          _stopReason := ssStopAtDiseaseEnd;
          _nDays := -1;
        end
      else if( rdoFirstDetection.Checked ) then
        begin
          _stopReason := ssStopAtFirstDetection;
          _nDays := -1;
        end
      else if( rdoSpecDay.Checked ) then
        _stopReason := ssStopAtSpecificDay
      else
        raise exception.Create( 'Out of options in TFormExport.rdoEndOptionClick()' )
      ;

      rleSpecDay.Visible := rdoSpecDay.checked;
      setBtnExportEnabled();
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


  procedure TFormExport.rleSpecDayEnter(Sender: TObject);
    begin
      btnExport.Enabled := false;
      fraAcceptCancel.Visible := true;
    end
  ;


end.
