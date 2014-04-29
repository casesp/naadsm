unit FormImport;

(*
FormImport.pas/dfm
------------------
Begin: 2006/06/17
Last revision: $Date: 2008/04/18 20:35:16 $ $Author: areeves $
Version number: $Revision: 1.21 $
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
  SMDatabase,
  ReadXMLInput,
  SMScenario,
  FormMain,
  SQLClasses
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
  	ini: TIniHandler;
    db: TSMDatabase;

    _Convertor:TXMLConvert;
    _smDB:TSMDatabase;

    procedure updateUI();
    
    procedure import( dbfn, pfn, hfn: string );

    procedure translateUI();

    // properties
    function getDB(): TSMDatabase;

  public
    constructor create( AOwner: TComponent; iniSettings: TIniHandler ); reintroduce;

    property database: TSMDatabase read getDB;

  end
;


implementation

{$R *.dfm}

  uses
    StrUtils,

    DebugWindow,
    MyStrUtils,
    GuiStrUtils,
    MyDialogs,
    DialogLongMessage,
    I88n
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
 		
    ini := iniSettings;
    db := nil;

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



procedure TFormImport.import( dbfn, pfn, hfn: string );
	var
    dlg: TDialogLongMessage;
  	err: string;
    _Scenario:TSMScenarioPTR;
	begin
    err := '';
    _smDB := frmMain.smDB;

    if ( ( length(dbfn ) >  0 ) and ( ( length(hfn) > 0 ) or (length(pfn) > 0) ) ) then
      begin
      if ( _smDB <> nil ) then
        begin
          msgOK(
              tr( 'An existing scenario file is currently open. This file will now be closed and a new one created.' ),
              tr( 'Closing current scenario' ),
              IMGInformation,
              self
          );
          frmMain.setDB( nil );
        end;

      try try
      	Screen.Cursor:=crHourGlass;

        _smdb := TSMDatabase.create( dbfn, DBCreate, PChar( err ), frmMain );
        _smdb.simStopReason := ssStartAndStopAtEndOfOutBreak;
        
        frmMain.setDB( _smDB );

        if( not frmMain.createSimObjects() ) then
          begin
            Screen.Cursor := crDefault;
            msgOK(
              tr( 'An scenario database could not be created: there may be a problem with your parameter file.' )
                + '  ' + tr( 'Please check with the developers for more assistance.' ),
              tr( 'Import failed' ),
              IMGCritical,
              self )
            ;
          end
        else
          begin
            _Scenario := frmMain.Scenario;
            Screen.Cursor:=crHourGlass;            
            _Convertor := TXMLConvert.create( hfn, pfn, _Scenario );
            _Convertor.ConvertXMLToDatabase( @err );
            _smdb.permanentDBFileName := dbfn;
            _smdb.save();
            db := _smDB;

            self.close();

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
                dlg.free();
              end
            ;

            frmMain.showMap();
          end;
        ;

      except
        on e: exception do
        	begin
      			freeAndNil( _smdb );

            msgExceptionOK(
              tr( 'NAADSM is unable to create a new scenario file. You may need to check your available hard disk space or your parameter file.' )
                + endl + endl,
            	e, self )
            ;
          end
        ;
      end;
      finally
        Screen.Cursor:=crDefault;
      end;
      end
    else
      begin
        if ( length(dbfn) <= 0 ) then
          msgOk( tr( 'Please select a database name to use.' ), '', IMGCritical )
        else
          msgOk( tr( 'Please select an XML file to import.' ), '', IMGCritical );
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
            tr( 'Scenario filename is missing' ),
            IMGWarning,
            self
        );
        exit;
      end
    ;

    if ( LowerCase( RightStr( dbfn, 4 ) ) <> '.mdb' ) then
      dbfn := dbfn + '.mdb';
      
    screen.Cursor := crHourglass;
    import( dbfn, pfn, hfn );
    screen.Cursor := crDefault;
  end
;


procedure TFormImport.btnScenarioClick(Sender: TObject);
  begin
    OpenDialog1.Title := 'Select a scenario parameter file';

    if( length( trim(leScenario.text) ) > 0 ) then
      openDialog1.fileName := trim( leScenario.text )
    else if( ini.hasKey( 'LastImportDirectory' ) ) then
      OpenDialog1.initialDir := ini.val( 'LastImportDirectory' )
    ;

    if OpenDialog1.Execute() then
      begin
        leScenario.text := OpenDialog1.FileName;
        ini.update( 'LastImportDirectory', directory( OpenDialog1.FileName ) );
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
    else if( ini.hasKey( 'LastImportDirectory' ) ) then
      OpenDialog1.initialDir := ini.val( 'LastImportDirectory' )
    ;

    if OpenDialog1.Execute() then
      begin
        leHerds.text := OpenDialog1.FileName;
        ini.update( 'LastImportDirectory', directory( OpenDialog1.FileName ) );
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
    else if( ini.hasKey( 'LastSaveDirectory' ) ) then
      saveDialog1.initialDir := ini.val( 'LastSaveDirectory' )
    ;

    if saveDialog1.Execute() then
      begin
        leDB.text := saveDialog1.FileName;
        ini.update( 'LastSaveDirectory', directory( saveDialog1.FileName ) );
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

    dbcout2( endl );
    dbcout2( length( leScenario.Text ) );
    dbcout2( scenarioReady );
    dbcout2( length( leHerds.Text ) );
    dbcout2( herdsReady );
    dbcout2( length( leDB.Text ) );
    dbcout2(  dbReady );
    dbcout2( endl );
    
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
		freeAndNil( db );
    close();
  end
;


// Properties
function TFormImport.getDB(): TSMDatabase; begin result := db; end;



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
