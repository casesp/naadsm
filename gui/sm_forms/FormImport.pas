unit FormImport;

(*
FormImport.pas/dfm
------------------
Begin: 2006/06/17
Last revision: $Date: 2013-06-27 19:11:26 $ $Author: areeves $
Version number: $Revision: 1.25.4.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2010 Colorado State University

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
    SMDatabase,
    SMScenario,
    FormMain
  ;

  type TFormImport = class(TForm)
      leScenario: TEdit;
      leHerds: TEdit;
      leDB: TEdit;
      btnScenario: TButton;
      btnHerds: TButton;
      btnDB: TButton;
      lblScenario: TLabel;
      lblHerds: TLabel;
      lblDB: TLabel;
      btnImport: TButton;
      Label2: TLabel;
      GroupBox1: TGroupBox;
      cbxScenario: TCheckBox;
      cbxHerds: TCheckBox;
      OpenDialog1: TOpenDialog;
      SaveDialog1: TSaveDialog;
      btnCancel: TButton;
    
      procedure FormCreate( Sender: TObject );
    
      procedure btnScenarioClick(Sender: TObject);
      procedure btnHerdsClick(Sender: TObject);
      procedure btnDBClick(Sender: TObject);

      // FIX ME: do some major error checking in here!
      procedure btnImportClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
      procedure cbxScenarioClick(Sender: TObject);
      procedure cbxHerdsClick(Sender: TObject);
      procedure leKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    protected
      _ini: TIniHandler;
      _db: TSMDatabase;
      _scenario: TSMScenario;

      procedure updateUI();
    
      procedure import( const databaseFileName, scenarioXmlFileName, herdXmlFileName: string );

      procedure translateUI();

    public
      constructor create( AOwner: TComponent; iniSettings: TIniHandler ); reintroduce;

      property database: TSMDatabase read _db;
      property scenario: TSMScenario read _scenario;
    end
  ;


implementation

{$R *.dfm}

  uses
    StrUtils,

    DebugWindow,
    MyStrUtils,
    MyDialogs,
    DialogLongMessage,
    I88n,
    SqlClasses,
    FunctionPointers,
    
    FormProgress,

    SMSimulationInput,
    Herd
  ;
  

  constructor TFormImport.create( AOwner: TComponent; iniSettings: TIniHandler );
    begin
      inherited create( AOwner );
        translateUI();
 		
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest. 		
 		
      _ini := iniSettings;
      _db := nil;
      _scenario := nil;

      updateUI();
    end
  ;


  procedure TFormImport.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormImport.dfm
      // File date: Thu Nov 9 21:48:58 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Import scenario' );
          lblScenario.Caption := tr( 'Scenario parameters file:' );
          lblHerds.Caption := tr( 'List of units:' );
          lblDB.Caption := tr( 'Scenario database file to create:' );
          Label2.Caption := tr( 'Import existing XML files to the database' );
          btnScenario.Caption := tr( 'Browse...' );
          btnHerds.Caption := tr( 'Browse...' );
          btnDB.Caption := tr( 'Browse...' );
          btnImport.Caption := tr( 'Import' );
          GroupBox1.Caption := tr( 'Items to import:' );
          cbxScenario.Caption := tr( 'Import scenario parameters file' );
          cbxHerds.Caption := tr( 'Import list of units' );
          btnCancel.Caption := tr( 'Cancel' );
          OpenDialog1.Filter := tr( 'XML files (*.xml)|*.xml|All files (*.*)|*.*' );
          SaveDialog1.Filter := tr( 'NAADSM scenario databases (*.mdb)|*.mdb|All files (*.*)|*.*' );
        end
      ;

    end
  ;


  procedure TFormImport.FormCreate(Sender: TObject);
    begin
      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );
          self.width := round( self.width * ( screen.pixelsPerInch / 96 ) );
          self.height := round( self.height * ( screen.pixelsPerInch / 96 ) );
        end
      ;
    end
  ;


  procedure TFormImport.import( const databaseFileName, scenarioXmlFileName, herdXmlFileName: string );
    var
      dlg: TDialogLongMessage;
      err: string;
      sim: TSMSimulationInput;
      hList: THerdList;
      frm: TFormProgress;
      fnPrimaryProgress: TObjFnBool1Int;
      fnSecondaryProgress: TObjFnBool1Int;
      fnProgressMessage: TObjFnVoid1String;
    begin
      // This should never happen: the menu items should be disabled if a scenario is open.
      // This block only acts as a double-check.
      if( nil <> frmMain.smdb ) then
        begin
          msgOK(
              tr( 'An existing scenario file is currently open. Please close this file before importing a new scenario.' ),
              tr( 'A scenario is already open' ),
              IMGInformation,
              self
          );
          exit;
        end
      ;

      // File names should have all been checked before getting this far.
      // We assume that the necessary files exist when we start the XML import.
      freeAndNil( _db );
      freeAndNil( _scenario );
      sim := nil;
      hList := nil;

      try
        Screen.Cursor := crHourGlass;
        err := '';

        // Set up the progress form
        //-------------------------
        frm := TFormProgress.create( self, PRDoubleBar, true, tr( 'Import XML' ) );
        fnPrimaryProgress := frm.setPrimary;
        fnSecondaryProgress := frm.setSecondary;
        fnProgressMessage := frm.setMessage;
        frm.Show();

        // Create an empty database
        //-------------------------
        frm.setMessage( tr( 'Creating scenario database...' ) );
        _db := TSMDatabase.create( databaseFileName, DBCreate, PChar( err ) );
        _db.simStopReason := ssStopAtEndOfOutBreak;
        frm.setPrimary( 100 );
        frm.setSecondary( 25 );

        // Create the simulation object
        //-----------------------------
        frm.setPrimary( 0 );
        frm.setMessage( tr( 'Importing scenario parameters...' ) );

        // Create the sim object using values from the XML scenario import file
        sim := TSMSimulationInput.create( scenarioXmlFileName, _db, @err, fnPrimaryProgress, fnProgressMessage  );
        // Check if there were errors on importing the scenario file
        if ( 0 < length(err) ) then
          begin
            freeAndNil( sim );

            if( nil <> _db ) then
              begin
                _db.close();
                freeAndNil( _db );
              end
            ;
            frm.setMessage( tr( 'An error occurred during file import.' ) );
          end
        ;

        // If there were no errors importing scenario parameters then try importing the herds
        if not ( 0 < length(err) ) then
          begin
            frm.setSecondary( 50 );
            frm.setPrimary( 100 );
            
            // Create the herd list
            //---------------------
            frm.setPrimary( 0 );
            if( strIsEmpty( herdXmlFileName ) ) then
              begin
               // Create empty herd list
               hList := THerdList.create( _db, sim );
              end
            else
              hList := THerdList.create( herdXmlFileName, _db, sim, @err, fnPrimaryProgress, fnProgressMessage )
            ;

            // Check if there was errors importing the herd file
            if ( 0 < length(err) ) then
              begin
                // Destruction order is important because hList has references to sim structures
                freeAndNil( hList );
                freeAndNil( sim );

                if( nil <> _db ) then
                  begin
                    _db.close();
                    freeAndNil( _db );
                  end
                ;
                frm.setMessage( tr( 'An error occurred during file import.' ) );
              end
            ;
          end
        ;

        // If there were no errors importing either files then save the new data
        if not ( 0 < length(err) ) then
          begin
            frm.setSecondary( 75 );
            frm.setPrimary( 100 );
    
            // Create the scenario object
            //---------------------------
            frm.setPrimary( 0 );
            frm.setMessage( tr( 'Completing XML import...' ) );
            _scenario := TSMScenario.create( sim, hList );
    
            // Finalize the database settings
            //-------------------------------
            _db.permanentDBFileName := databaseFileName;
            _db.save();
    
            frm.setPrimary( 100 );
            frm.setSecondary( 100 );
            frm.setMessage( tr( 'Done!' ) );
          end
        ;

        frm.Release();

        screen.Cursor := crDefault;

        self.close();

        // If any errors occurred above then alert the user
        if( 0 = length( err ) ) then
          msgOK(
            tr( 'The parameter file was successfully imported.  The resulting scenario database is now open.' ),
            tr( 'Import successful' ),
            IMGSuccess,
            self
          )
        else
          begin
            dlg := TDialogLongMessage.create(
              self,
              tr( 'Potential problems during import' ),
              tr( 'The parameter file was imported, with the following errors and/or warnings:' ),
              err
            );
            dlg.showModal();
            dlg.Release();
          end
        ;
      except
        on e: exception do
          begin
            Screen.Cursor := crDefault;

            freeAndNil( sim );
            freeAndNil( hList );
            freeAndNil( _scenario );

            if( nil <> _db ) then
              begin
                _db.close();
                freeAndNil( _db );
              end
            ;

            msgExceptionOK(
              tr( 'NAADSM is unable to create a new scenario file. You may need to check your available hard disk space or your parameter file.' ) + endl + endl,
              e,
              self
            );
          end
        ;
      end;
    end
  ;


  // FIX ME: do some major error checking in here!
  procedure TFormImport.btnImportClick(Sender: TObject);
    var
      dbfn, pfn, hfn: string;
    begin
      dbfn := trim( leDB.text );
      pfn := trim( leScenario.Text );
      hfn := trim( leHerds.Text );

      if( 0 = length( dbfn ) ) then
        begin
          msgOK(
              tr( 'Please select a file name for the new scenario.' ),
              tr( 'Database filename is missing' ),
              IMGWarning,
              self
          );
          exit;
        end
      ;

      if( cbxScenario.checked ) then
        begin
          if( strIsEmpty( pfn ) ) then
            begin
              msgOK(
                  tr( 'Please select a scenario parameter file to import.' ),
                  tr( 'Parameter XML filename is missing' ),
                  IMGWarning,
                  self
              );
              exit;
            end
          ;

          if( not( fileExists( pfn ) ) ) then
            begin
              msgOK(
                  tr( 'The selected scenario parameter file does not exist.' ),
                  tr( 'Parameter XML file is missing' ),
                  IMGWarning,
                  self
              );
              exit;
            end
          ;
        end
      ;

      if( cbxHerds.checked ) then
        begin
          if( strIsEmpty( hfn ) ) then
            begin
              msgOK(
                  tr( 'Please select a unit file to import.' ),
                  tr( 'Unit XML filename is missing' ),
                  IMGWarning,
                  self
              );
              exit;
            end
          ;

          if( not( fileExists( hfn ) ) ) then
            begin
              msgOK(
                  tr( 'The selected unit file does not exist.' ),
                  tr( 'Unit XML file is missing' ),
                  IMGWarning,
                  self
              );
              exit;
            end
          ;
        end
      ;

      if( '.mdb' <> ansiLowerCase( RightStr( dbfn, 4 ) ) ) then
        dbfn := dbfn + '.mdb'
      ;

      screen.Cursor := crHourglass;
      try
        import( dbfn, pfn, hfn );
      finally
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormImport.btnScenarioClick(Sender: TObject);
    begin
      OpenDialog1.Title := 'Select a scenario parameter file';

      if( length( trim(leScenario.text) ) > 0 ) then
        openDialog1.fileName := trim( leScenario.text )
      else if( _ini.hasKey( 'LastImportDirectory' ) ) then
        OpenDialog1.initialDir := _ini.val( 'LastImportDirectory' )
      ;

      if OpenDialog1.Execute() then
        begin
          leScenario.text := OpenDialog1.FileName;
          _ini.update( 'LastImportDirectory', directory( OpenDialog1.FileName ) );
        end
      ;

      updateUI();
    end
  ;


  procedure TFormImport.btnHerdsClick(Sender: TObject);
    begin
      OpenDialog1.Title := 'Select a herd file';

      if( length( trim(leHerds.text) ) > 0 ) then
        openDialog1.fileName := trim( leScenario.text )
      else if( _ini.hasKey( 'LastImportDirectory' ) ) then
        OpenDialog1.initialDir := _ini.val( 'LastImportDirectory' )
      ;

      if OpenDialog1.Execute() then
        begin
          leHerds.text := OpenDialog1.FileName;
          _ini.update( 'LastImportDirectory', directory( OpenDialog1.FileName ) );
        end
      ;

      updateUI();
    end
  ;


  procedure TFormImport.btnDBClick(Sender: TObject);
    begin
      saveDialog1.Title := tr( 'Create a scenario database' );

      if( length( trim(leDB.text) ) > 0 ) then
        saveDialog1.fileName := trim( leScenario.text )
      else if( _ini.hasKey( 'LastSaveDirectory' ) ) then
        saveDialog1.initialDir := _ini.val( 'LastSaveDirectory' )
      ;

      if saveDialog1.Execute() then
        begin
          leDB.text := saveDialog1.FileName;
          _ini.update( 'LastSaveDirectory', directory( saveDialog1.FileName ) );
        end
      ;

      updateUI();
    end
  ;


  procedure TFormImport.updateUI();
    var
      scenarioReady: boolean;
      herdsReady: boolean;
      dbReady: boolean;
    begin
      if( cbxScenario.Checked ) then
        scenarioReady := ( 0 < length( leScenario.Text ) )
      else
        scenarioReady := true
      ;

      if( cbxHerds.Checked ) then
        herdsReady := ( 0 < length( leHerds.Text ) )
      else
        herdsReady := true
      ;
    
      dbReady := ( 0 < length( leDB.Text ) );
    
      btnImport.Enabled :=( scenarioReady and herdsReady and dbReady );
    end
  ;


  procedure TFormImport.leKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    begin
      updateUI();
    end
  ;


  procedure TFormImport.btnCancelClick(Sender: TObject);
    begin
      freeAndNil( _db );
      close();
    end
  ;


  procedure TFormImport.cbxScenarioClick(Sender: TObject);
    begin
      if ( cbxScenario.Checked ) then
        begin
          cbxHerds.Enabled := true;
          leScenario.Enabled := true;
          btnScenario.Enabled := true;
          leDB.Enabled := true;
          btnDB.Enabled := true;
        end
      else
        begin
          cbxHerds.Checked := false;
          cbxHerds.Enabled := false;
          leScenario.Enabled := false;
          btnScenario.Enabled := false;
          leDB.Enabled := false;
          btnDB.Enabled := false;
        end
      ;

      updateUI();
    end
  ;


  procedure TFormImport.cbxHerdsClick(Sender: TObject);
    begin
      if ( cbxHerds.Checked )  then
        begin
          leHerds.Enabled := true;
          btnHerds.Enabled := true;
        end
      else
        begin
          leHerds.Enabled := false;
          btnHerds.Enabled := false;
        end
      ;

      updateUI();
    end
  ;



end.
