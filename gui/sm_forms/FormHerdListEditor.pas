unit FormHerdListEditor;

(*
FormHerdListEditor.pas/dfm
--------------------------
Begin: 2005/09/02
Last revision: $Date: 2011-07-18 21:24:00 $ $Author: rhupalo $
Version number: $Revision: 1.36.6.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
  	// Standard includes
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
    ImgList,
    ActnList,
    XPStyleActnCtrls,
    ActnMan,
    ToolWin,
    ActnCtrls,
    ActnMenus,
    Buttons,
    Menus,
    ActnPopupCtrl,

    // QClasses,
    QVectors,

    // Base class
    FormSMWizardBase,
    
    // Other widgets
    FrameHerdListEditor,

		// Data structures
    SMScenario,
    Herd
  ;


  type TFormHerdListEditor = class( TFormSMWizardBase )
      ActionManager1: TActionManager;
      acnImportAppend: TAction;
      acnImportReplace: TAction;
      ImageList1: TImageList;
      acnExport: TAction;
      pnlCaption: TPanel;
      dlgOpenImport: TOpenDialog;
      pnlControls: TPanel;
      pnlButtons: TPanel;
      btnExport: TBitBtn;
      btnPrint: TBitBtn;
      btnImportReplace: TBitBtn;
      pnlDecorator1: TPanel;
      pnlDecorator2: TPanel;
      pnlMenu: TPanel;
      mnuMain: TActionMainMenuBar;
      btnImportAppend: TBitBtn;
      btnRemoveSelected: TBitBtn;
      acnPrint: TAction;
      acnRemoveSelected: TAction;
      acnFinish: TAction;
      acnCancel: TAction;
      acnSelectAll: TAction;
      
      fraHerdListEditor: TFrameHerdListEditor;

      // Import/export
      //--------------
      procedure acnExportExecute(Sender: TObject);
      procedure acnImportReplaceExecute(Sender: TObject);
      procedure acnImportAppendExecute(Sender: TObject);

      // Selections
      //-----------
      procedure acnRemoveSelectedExecute(Sender: TObject);
      procedure acnSelectAllExecute(Sender: TObject);

      // Accept/cancel
      //--------------
      procedure acnCancelExecute(Sender: TObject);
      procedure acnFinishExecute(Sender: TObject);

    protected
      // Variables and variable-like properties for internal use
      //--------------------------------------------------------
      _replaceHerdsInDatabase: boolean;
      _indexExists: boolean;

      procedure translateUI();

      function getHerdList(): THerdList;
      procedure setHerdList( hList: THerdList );
      procedure addHerdsFromList( hList: THerdList );

      procedure updateDisplay( selectedRows: integer );

      property _herdList: THerdList read getHerdList; // A variable-like property, returns fraHerdListEditor._herdList.


      // Special event handling
      //-----------------------
      { Handles tab key and focus events when a combo box is in use }
      //procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;


      // Parameter and database handling
      //---------------------------------
      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;
      procedure updateScenarioData(); override;
      function getDataUpdated(): boolean; override;


      // Import/export
      //--------------
      { Creates a new list from a CSV or XML file }
      function createListFromFile( const fileName: string; fileFormat: integer; ids: TQIntegerVector = nil ): THerdList;


      // Simple properties
      //------------------
      function getIsReadOnly(): boolean;
      procedure setIsReadOnly( val: boolean );

    public
      // Construction/Destruction
      //-------------------------
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      // Simple properties
      //------------------
      property isReadOnly: boolean read getIsReadOnly write setIsReadOnly;
    end
  ;


implementation

{$R *.dfm}

  uses
     // Simple Delphi Expat Wrapper
    Sdew,

    StrUtils,
    MyStrUtils,
    ControlUtils,
    DebugWindow,
    MyDialogs,
    FunctionPointers,
    I88n,
    CsvParser,

    DialogLongMessage,

    FormHerdExportOptions,
    FormProgress,
    ProductionType
  ;


  const
    DBFORMHERDLISTEDITOR: boolean = false; // set to true to enable debugging messages for this unit
    

//----------------------------------------------------------------------------
// Construction/Destruction
//----------------------------------------------------------------------------
  constructor TFormHerdListEditor.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      fraHerdListEditor.setForm( self );
      fraHerdListEditor.onUpdateDisplay := self.updateDisplay;

      acnRemoveSelected.Enabled := false;
      btnRemoveSelected.Enabled := false;

      _replaceHerdsInDatabase := false;

      self.Width := 810;
    end
  ;


  procedure TFormHerdListEditor.translateUI();
    begin
      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Set up starting units' );
          pnlCaption.Caption := tr( 'Set up starting units' );
          btnExport.Hint := tr( 'Export unit list to file' );
          btnPrint.Hint := tr( 'Print unit list' );
          btnImportReplace.Hint := tr( 'Import and replace existing unit list' );
          btnImportAppend.Hint := tr( 'Import and append to existing unit list' );
          btnRemoveSelected.Hint := tr( 'Remove selected unit(s)' );
          mnuMain.Caption := tr( 'mnuMain' );
          acnImportAppend.Caption := tr( 'Import and append to existing unit list...' );
          acnImportReplace.Caption := tr( 'Import and replace existing unit list...' );
          acnExport.Caption := tr( 'Export to file...' );
          acnPrint.Caption := tr( 'Print unit list' );
          acnRemoveSelected.Caption := tr( 'Remove selected unit(s)' );
          acnFinish.Caption := tr( 'Save changes and finish' );
          acnCancel.Caption := tr( 'Discard changes and exit' );
          acnSelectAll.Caption := tr( 'Select &all visible units' );
          dlgOpenImport.Filter := tr( 'All NAADSM files (*.csv, *.xml)|*.csv;*.xml|Comma separated values (*.csv)|*.csv|NAADSM XML (*.xml)|*.xml|All files (*.*)|*.*' );
        end
      ;

      // Set action Caption properties
      with self do
        begin
          acnImportAppend.Caption := tr( '&Import and append to existing unit list...' );
          acnImportReplace.Caption := tr( 'I&mport and replace existing unit list...' );
          acnExport.Caption := tr( '&Export to file...' );
          acnPrint.Caption := tr( '&Print unit list' );
          acnFinish.Caption := tr( '&Save changes and finish' );
          acnCancel.Caption := tr( '&Discard changes and exit' );
          acnRemoveSelected.Caption := tr( '&Remove selected unit(s)' );
        end
      ;

      mnuMain.ActionManager.ActionBars[0].Items[0].Caption := tr( '&File' );
      mnuMain.ActionManager.ActionBars[0].Items[1].Caption := tr( '&Edit' );
    end
  ;


  destructor TFormHerdListEditor.destroy();
    begin
      dbcout( 'Destroying TFormHerdListEditor', DBFORMHERDLISTEDITOR );
      inherited destroy();
    end
  ;
//----------------------------------------------------------------------------



//----------------------------------------------------------------------------
// Parameter and database handling
//----------------------------------------------------------------------------
  procedure TFormHerdListEditor.initializeFromSim();
    begin
      dbcout( 'initializeFromSim start: herd list is updated: ' + uiBoolToText( _smScenarioOriginal.herdList.updated ), DBFORMHERDLISTEDITOR );

      _indexExists := _smScenarioOriginal.simInput.database.indexExists( 'dynHerd_PK', 'dynHerd' );

      if( _indexExists ) then
        begin
          // Pass the appropriate lists to the string grid
          fraHerdListEditor.prodTypeList := _smScenarioOriginal.simInput.ptList;
          fraHerdListEditor.herdList := _smScenarioOriginal.herdList; // Remember that the frame actually makes a copy.
        end
      else
        begin
          setChildrenEnabled( pnlButtons, false, true );
          setChildrenEnabled( pnlMenu, false, true );
          fraHerdListEditor.showError(
            'The schema of this scenario database has been altered.  It is not possible to modify existing units.'
            + endl + endl + '(Technical information: index "dynHerd_PK" does not exist on table "dynHerd".)'
          );
        end
      ;

      dbcout( 'initializeFromSim end: herd list is updated: ' + uiBoolToText( _smScenarioOriginal.herdList.updated ), DBFORMHERDLISTEDITOR );
      dbcout( 'initializeFromSim end: herd list is updated: ' + uiBoolToText( fraHerdListEditor.herdList.updated ), DBFORMHERDLISTEDITOR );
    end
  ;


  procedure TFormHerdListEditor.updateScenarioData();
    var
      fnProgress: TObjFnBool1Int;
      frm: TFormProgress;
      hListUpdateSuccess: boolean;
      errMsg: string;
    begin

      if( not( _indexExists ) ) then
        begin
          dbcout( 'There is no index: database won''t be updated.', DBFORMHERDLISTEDITOR );
          exit;
        end
      ;

      // If there were no changes to the herd list, just walk away.
      //-----------------------------------------------------------
      if( not( fraHerdListEditor.herdList.Updated ) ) then
        exit
      ;

      // Otherwise...
      //-------------
      
      // Create the progress indicator screen,
      // if it looks like this will take a while.
      // FIX ME: use a double progress bar here.
      //-----------------------------------------
      if( ( 200 < fraHerdListEditor.herdList.Count ) ) then
        begin
          frm := TFormProgress.create( self, PRSingleBar, true );
          fnProgress := frm.setPrimary;
          frm.setMessage( tr( 'Updating units in scenario database...' ) );
          frm.Show();
          application.ProcessMessages();
        end
      else
        begin
          fnProgress := nil;
          frm := nil;
        end
      ;

      Screen.Cursor := crHourglass;

      // Don't delete the herd list, since
      // other units (e.g. FormMap) depend
      // on it. Just clear and refill it.
      //----------------------------------
      _smScenarioOriginal.herdList.clear();

      // If any herds in the list have been updated, they all must be reset to their initial status.
      // If all herds are unchanged, it's neither necessary nor desirable to reset them.
      dbcout( 'fraHerdListEditor.herdList.updated: ' + uiBoolToText( fraHerdListEditor.herdList.updated ), DBFORMHERDLISTEDITOR );

      _smScenarioOriginal.herdList.assign( fraHerdListEditor.herdList, fraHerdListEditor.herdList.updated );

      // If necessary, force the production type list to recount units/animals.
      if( fraHerdListEditor.herdList.updated ) then
        begin
          dbcout( '*** Forcing recount', DBFORMHERDLISTEDITOR );
          _smScenarioOriginal.simInput.ptList.recountUnits( _smScenarioOriginal.herdList );
        end
      else
        dbcout( '??? NO RECOUNT', DBFORMHERDLISTEDITOR )
      ;

      _smScenarioOriginal.herdList.updated := fraHerdListEditor.herdList.updated;

      (*
      dbcout( endl + '--- THE NEW HERD LIST:', DBFORMHERDLISTEDITOR );
      if( DBFORMHERDLISTEDITOR ) then _smScenarioOriginal.herdList.debug();
      *)
      
      // Update/populate the database with the changed/new herd list.
      //-------------------------------------------------------------
      _smScenarioOriginal.simInput.database.initializeAllOutputRecords();

      if( _replaceHerdsInDatabase ) then _smScenarioOriginal.simInput.database.clearHerds();

      hListUpdateSuccess := _smScenarioOriginal.herdList.populateDatabase( _smScenarioOriginal.simInput.database, fnProgress, @errMsg );

      Screen.Cursor := crDefault;

      if( not hListUpdateSuccess ) then
        begin
          msgOK(
            errMsg,
            tr( 'Problems encountered with list of units' ),
            imgWarning,
            self
          );
        end
      ;

      if( nil <> frm ) then
        frm.Release()
      ;
      application.ProcessMessages(); //rbh to resolve issue 2408
      self.SetFocus();
    end
  ;
//----------------------------------------------------------------------------



//----------------------------------------------------------------------------
// Special event handling
//----------------------------------------------------------------------------
  {*
    When tabbing out of a combo box, focus should be restored to the string grid control,
    rather than go to another control on the form.

    I would prefer that this procedure belonged to TFrameHerdListEditor,
    but I don't think events work properly that way...
  }
  (*
  procedure TFormHerdListEditor.CMDialogKey( var msg: TCMDialogKey );
    begin

      if
        ( ActiveControl = fraHerdListEditor.cboProdType )
      or
        (ActiveControl = fraHerdListEditor.cboTransitionState )
      then
        begin
          if
            ( VK_TAB = msg.CharCode )
          or
            ( VK_RETURN = msg.CharCode )
          or
            ( VK_ESCAPE = msg.CharCode )
          then
            begin
              //set focus back to the grid and pass the tab key to it.
              msg.CharCode := VK_TAB;
              fraHerdListEditor.stgHerds.SetFocus();
              fraHerdListEditor.stgHerds.Perform( WM_KEYDOWN, msg.CharCode, msg.KeyData );
              // Swallow this message.
              msg.result := 1;
              Exit;
            end
          ;
        end
      ;

      inherited;
    end
  ;
  *)
//----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Import/export
//-----------------------------------------------------------------------------
  procedure TFormHerdListEditor.acnExportExecute(Sender: TObject);
    var
      frm: TFormHerdExportOptions;
      success: boolean;
    begin
      inherited;

      frm := TFormHerdExportOptions.create( self );

      // FIX ME: Use of the projected coordinate system is not currently allowed by herd export.
      if( frm.execute() ) then
        begin
          case frm.fileFormat of
            XML_FILE_FORMAT: success := _herdList.writeXMLFile( frm.fileName, false );
            CSV_FILE_FORMAT: success := _herdList.writeCSVFile( frm.fileName, frm.productionTypeByName, frm.initialStateAsCharacter, false );
            else success := false;
          end;

          if( success ) then
            msgOK(
              ansiReplaceStr( tr( 'List of units successfully exported to file xyz.' ), 'xyz', abbrevPath( frm.fileName, 35 ) ),
              tr( 'Export success' ),
              IMGSuccess,
              self
            )
          else
            msgOK(
              ansiReplaceStr( tr( 'Export of unit list to file xyz failed.  Please check the file name, your disk space, and your permissions.' ), 'xyz', abbrevPath( frm.fileName, 35 ) ),
              tr( 'Export failed' ),
              IMGCritical,
              self
            )
          ;

        end
      ;

      frm.Release();
    end
  ;


  function TFormHerdListEditor.createListFromFile( const fileName: string; fileFormat: integer; ids: TQIntegerVector = nil ): THerdList;
    var
      h: THerd;
      localHerdList: THerdList;
      success: boolean;
      tmpPT: TProductionType;

      frm: TFormProgress;
      fn1: TObjFnBool1Int;
      fn2: TObjFnBool1Int;
      fn3: TObjFnVoid1String;
      counter: integer;

      csv: TCSVContents;

      errFrm: TDialogLongMessage;
      errCaption, errHeader, errMsg: string;

      sdew: TSdew;
      root: pointer;
      n: integer;
    begin
      Screen.Cursor := crHourglass;

      // Do some easy checks first...
      //-----------------------------
      if( not( fileExists( fileName ) ) ) then
        begin
          Screen.Cursor := crDefault;

          msgOK(
            ansiReplaceStr( tr( 'The selected file xyz does not exist.' ), 'xyz', abbrevPath( fileName ) ),
            tr( 'Missing file' ),
            IMGCritical,
            self
          );
          result := nil;
          exit;
        end
      ;


      // Try to guess the file format, if necessary.
      //--------------------------------------------
      if( UNKNOWN_FILE_FORMAT = fileFormat ) then
        begin
          if( 'csv' = lower( rightStr( fileName, 3 ) ) ) then
            fileFormat := CSV_FILE_FORMAT
          else if( 'xml' = lower( rightStr( fileName, 3 ) ) ) then
            fileFormat := XML_FILE_FORMAT
          else
            begin
              // Try parsing as a CSV file.
              csv := TCSVContents.createFromFile( fileName, csvListSep(), true );
              // Presently, ANY text file that can be opened will return parseSuccess of true.
              // So, if there typically is >5 fields per line then this might be a CSV herd file.
              if( csv.parseSuccess and ( csv.avgFieldCount > 5 )) then
                fileFormat := CSV_FILE_FORMAT
              ;
              csv.Free();

              // if not a CSV file then try to parse the file as XML
              n:= 0;
              sdew := nil;
              if( UNKNOWN_FILE_FORMAT = fileFormat ) then
                begin
                  try
                    // _rootTree will be nil if the XML is misformed or non-existant.
                    sdew := TSdew.createFromFile( pAnsiChar( fileName ) );
                    if assigned(sdew) then
                      begin
                        root := sdew.GetRootTree();
                        if (root <> nil) then n := sdew.GetElementCount( root );
                      end
                    ;
                  finally
                    if (0 < n) then fileFormat := XML_FILE_FORMAT;
                    if assigned(sdew) then sdew.Free();
                  end;
                end
              ;

              if( UNKNOWN_FILE_FORMAT = fileFormat ) then
                begin
                  Screen.Cursor := crDefault;

                  msgOK(
                    ansiReplaceStr( tr( 'The format of the selected file xyz' ), 'xyz', abbrevPath( fileName ) )  + ' ' + tr( 'could not be determined.' )
                      + '  ' + tr( 'Files must be XML or CSV format' )
                      + ' ' + tr( 'and file names must have the appropriate three-letter extension.' )
                      + ' ' + tr( 'For more information, please see your user''s documentation.' ),
                    tr( 'Unrecognized file format' ),
                    IMGCritical,
                    self
                  );
                  result := nil;
                  exit;
                end
              ;
            end
          ;
        end
      ;

      
      // Set up the data structure.
      //---------------------------
      localHerdList := THerdList.create();
      localHerdList.setDB( _smScenarioOriginal.simInput.database );
      localHerdList.setSim( _smScenarioOriginal.simInput );

      // Set up the progress form.
      //--------------------------
      frm := TFormProgress.create( self, PRDoubleBar, true );
      fn1 := frm.setPrimary;
      fn2 := frm.setSecondary;
      fn3 := frm.setMessage;
      frm.Show();


      // Try to parse the file.
      //-----------------------
      if
        ( CSV_FILE_FORMAT = fileFormat )
      or
        ( XML_FILE_FORMAT = fileFormat )
      then
        success := localHerdList.importFromFile( fileName, fileFormat, @errMsg, fn1, fn2, fn3, false, ids )
      else // file format is not supported
        success := false
      ;


      // Failure here will be due to a file problem.
      if( not( success ) ) then
        begin
          frm.Close();
          frm.Release();

          errCaption := tr( 'File import failed' );
          errHeader := ansiReplaceStr( tr( 'The file xyz could not be parsed.' ), 'xyz', abbrevPath( fileName, 35 ) )
            + ' ' + tr( 'Please check that the file exists, is not open in another application, and is formatted properly.' )
            + ' ' + tr( 'The following errors were reported.' )
            + ' ' + tr( 'For more information, please see your user''s documentation:' )
          ;

          Screen.Cursor := crDefault;

          errFrm := TDialogLongMessage.create( self , errCaption, errHeader, errMsg );
          errFrm.showModal();
          errFrm.Release();

          freeAndNil( localHerdList );
          result := nil;
          exit;
        end
      ;
      

      // If the file is successfully parsed, we still need to make sure that
      // the appropriate production types exist in the scenario.
      //-------------------------------------------------------------
      success := true;
      counter := 0;

      frm.setMessage( tr( 'Checking production types...' ) );
      frm.setPrimary( 0 );

      h := localHerdList.first();
      while( nil <> h ) do
        begin
          inc( counter );

          if( 0 = ( counter mod 100 ) ) then
            frm.setPrimary( round( ( counter/localHerdList.Count ) * 99 ) )
          ;

          if( -1 = h.actualProdTypeID ) then // Find production type by name
            tmpPT := _smScenarioOriginal.simInput.ptList.findProdType( h.actualProdTypeName )
          else // Find production type by ID
            tmpPT := _smScenarioOriginal.simInput.ptList.findProdType( h.actualProdTypeID )
          ;
          if( nil = tmpPT ) then
            begin
              success := false;
              break;
            end
          ;
          h := localHerdList.next();
        end
      ;

      frm.setPrimary( 100 );
      frm.setSecondary( 100 );

      frm.Close();
      frm.Release();

      self.setFocus();

      if( not( success ) ) then
        begin
          Screen.Cursor := crDefault;

          msgOK(
            ansiReplaceStr( tr( 'The file xyz' ), 'xyz', abbrevPath( fileName, 25 ) ) + ' ' + tr( 'was successfully parsed,' )
              + ' '
              + ansiReplaceStr( tr( 'but contains at least one production type (xyz) that does not exist in this scenario.' ), 'xyz', h.actualProdTypeName  )
              + endl + endl + ' ' + tr( 'The unit list cannot be imported.' ),
            tr( 'Missing production type(s)' ),
            IMGWarning,
            Self
          );
          freeAndNil( localHerdList );
          result := nil;
          exit;
        end
      ;

      // If everything is OK this far, set sim for the herd list and use it.
      localHerdList.resetSim( _smScenarioOriginal.simInput );

      result := localHerdList;

      Screen.Cursor := crDefault;
    end
  ;


  procedure TFormHerdListEditor.acnImportAppendExecute(Sender: TObject);
    var
      hList: THerdList;
      h: THerd;
      fileFormatIndex: integer;
    begin
      inherited;

      if( _iniHandler.hasKey('LastImportDirectory') ) then
        dlgOpenImport.InitialDir := _iniHandler.val( 'LastImportDirectory' )
      ;

      if( not( dlgOpenImport.Execute() ) ) then
        exit
      else
        begin
          _iniHandler.update( 'LastImportDirectory', directory( dlgOpenImport.FileName ) );

          fileFormatIndex := dlgOpenImport.FilterIndex - 1;
          // make adjustment for when filterIndex was 4 (*.*)
          if (2 < fileFormatIndex) then fileFormatIndex := UNKNOWN_FILE_FORMAT;
          hList := createListFromFile( dlgOpenImport.FileName, fileFormatIndex );

          if( nil <> hList ) then
            begin
              // Reset the ID for each herd to -1. Actual herd IDs
              // will be automatically assigned by the database.
              h := hList.first();
              while( nil <> h ) do
                begin
                  h.id := -1;
                  h := hList.next();
                end
              ;

              // Reset the herd list for the editor.
              addHerdsFromList( hList );

              // Remember that the editor makes a copy of the list:
              // we no longer need the newly created one.
              hList.Free();

              msgOK(
                ansiReplaceStr( tr( 'New units from xyz have been added to the current list of units.' ), 'xyz', abbrevPath( dlgOpenImport.FileName, 35 ) )
                  + endl + endl
                  + tr( 'Click ''Cancel'' to revert to the previous list of units, or any other button to save to new list of units to the scenario database.' ),
                tr( 'Import success' ),
                IMGSuccess,
                self
              );
            end
          ;

        end
      ;

    end
  ;



  procedure TFormHerdListEditor.acnImportReplaceExecute(Sender: TObject);
    var
      hList: THerdList;
      fileFormatIndex: integer;
      ids: TQIntegerVector;
      uniqueIds: boolean;
      response: integer;
      i: integer;
    begin
      inherited;

      if( _iniHandler.hasKey('LastImportDirectory') ) then
        dlgOpenImport.InitialDir := _iniHandler.val( 'LastImportDirectory' )
      ;

      if( not( dlgOpenImport.Execute() ) ) then
        exit
      else
        begin
          _iniHandler.update( 'LastImportDirectory', directory( dlgOpenImport.FileName ) );

          fileFormatIndex := dlgOpenImport.FilterIndex - 1;
          // make adjustment for when filterIndex was 4 (*.*)
          if (2 < fileFormatIndex) then fileFormatIndex := UNKNOWN_FILE_FORMAT;

          // This vector will store a list of herds IDs as they are imported from the selected file.
          // Remember that every herd ID must be unique.  This requirement will be enforced below.
          ids := TQIntegerVector.create();

          hList := createListFromFile( dlgOpenImport.FileName, fileFormatIndex, ids );

          if( nil <> hList ) then
            begin
              // Are all assigned herd IDs unique?
              uniqueIds := true; // Until shown otherwise
              ids.sort();
              for i := 0 to ids.count - 2 do
                begin
                  if( ids.at( i ) = ids.at( i+1 ) ) then
                    begin
                      uniqueIDs := false;
                      break;
                    end
                  ;
                end
              ;

              if( not( uniqueIds ) ) then
                begin
                  response := msgYesNo(
                    tr( 'Not all unit ID numbers in this file are unique.  If you continue with this import operation, all unit ID numbers will be replaced.' )
                      + endl + endl
                      + tr( 'Continue?' ),
                    tr( 'Unit IDs are not unique' ),
                    IMGWarning,
                    self
                  );

                  if( mrNo = response ) then
                    begin
                      hList.Free();
                      ids.Free();
                      exit;
                    end
                  else
                    begin
                      for i := 0 to hList.Count - 1 do
                        hList.at(i).id := -1
                      ;
                    end
                  ;
                end
              ;

              // Reset the herd list for the editor.
              setHerdList( hList );

              // Remember that the editor makes a copy of the list:
              // we no longer need the newly created one.
              hList.Free();

              _replaceHerdsInDatabase := true;

              msgOK(
                ansiReplaceStr( tr( 'Existing units have been replaced with those from xyz.' ), 'xyz', abbrevPath( dlgOpenImport.FileName, 35 ) )
                  + endl + endl
                  + tr( 'Click ''Cancel'' to revert to the previous list of units, or any other button to save to new list of units to the scenario database.' ),
                tr( 'Import success' ),
                IMGSuccess,
                self
              );
            end
          ;

          ids.Free();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//----------------------------------------------------------------------------
// Variable-like properties
//----------------------------------------------------------------------------
  function TFormHerdListEditor.getHerdList(): THerdList;
    begin
      result := fraHerdListEditor.herdList;
    end
  ;


  procedure TFormHerdListEditor.setHerdList( hList: THerdList );
    begin
      fraHerdListEditor.changeHerdList( hList );
    end
  ;


  procedure TFormHerdListEditor.addHerdsFromList( hList: THerdList );
    begin
      fraHerdListEditor.addHerdsFromList( hList );
    end
  ;
//----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Simple properties, passed through to fraHerdListEditor
//-----------------------------------------------------------------------------
  function TFormHerdListEditor.getIsReadOnly(): boolean;
    begin
      result := fraHerdListEditor.isReadOnly;
    end
  ;


  procedure TFormHerdListEditor.setIsReadOnly( val: boolean );
    begin
      fraHerdListEditor.isReadOnly := val;
    end
  ;


  function TFormHerdListEditor.dataIsValid(): boolean;
    begin
      // Validation is taken care of elsewhere.
      result := true;
    end
  ;


  function TFormHerdListEditor.getDataUpdated(): boolean;
    begin
      dbcout( '+++Herd list data updated: ' + uiBoolToText( fraHerdListEditor.dataUpdated ), DBFORMHERDLISTEDITOR );
      result := fraHerdListEditor.dataUpdated;
    end
  ;
//-----------------------------------------------------------------------------


procedure TFormHerdListEditor.updateDisplay( selectedRows: integer );
  begin
    dbcout( '******** Display should be updated!', DBFORMHERDLISTEDITOR );

    acnRemoveSelected.Enabled := ( 0 < selectedRows );
    btnRemoveSelected.Enabled := ( 0 < selectedRows );
  end
;



procedure TFormHerdListEditor.acnRemoveSelectedExecute(Sender: TObject);
begin
  inherited;
  fraHerdListEditor.removeSelectedHerds();
end;


procedure TFormHerdListEditor.acnCancelExecute(Sender: TObject);
begin
  inherited;
  btnCancel.Click();
end;


procedure TFormHerdListEditor.acnFinishExecute(Sender: TObject);
begin
  inherited;
  fraHerdListEditor.stgHerdsExit(self); // fixes issue 2217
  btnFinish.Click();
end;

procedure TFormHerdListEditor.acnSelectAllExecute(Sender: TObject);
begin
  inherited;
  fraHerdListEditor.selectAllHerds();
end;

initialization
	RegisterClass( TFormHerdListEditor );

end.
