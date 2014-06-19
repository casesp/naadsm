unit FormSMOutputBase;

(*
FormSMOutputBase.pas/dfm
-------------------------
Begin: 2005/12/07
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version: $Revision: 1.35.4.7 $
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
    // Standard Delphi units
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
    Buttons,
    ComCtrls,
    Spin,
    ExtCtrls,
    ActnList,
    XPStyleActnCtrls,
    ActnMan,
    ImgList,
    ToolWin,
    ActnCtrls,
    ActnMenus,
    Menus,

    // General purpose units
    QOrderedDictionaries,

    // Application-specific data structures
    SMDatabase,
    SMSimulationInput,
    Herd,
    ProductionType,
    ProductionTypeList,
    Zone,

    // Application-specific controls
    DMOutputActionManager
  ;


  {*
    This class provides basic operations for all output screens:
      - Creating toolbar buttons, menus, and other controls used by all output screens
      - Saving/copying/printing charts
      - Saving/copying/printing tabular (text) data
      - Selecting a different production type for which to display data
  }
	type TFormSMOutputBase = class( TForm )
      { Top-aligned panel that contains all other controls }
      pnlControls: TPanel;

      { Production type, zone, and iteration selection }
      pnlProdTypes: TPanel;
      cboProdTypes: TComboBox;
      cboZones: TComboBox;
      lblIteration: TLabel;
      cboIteration: TComboBox;

      { Toolbar buttons for saving/copying/printing }
      pnlButtons: TPanel;
      btnSaveData: TBitBtn;
      btnPrintData: TBitBtn;
      btnCopyData: TBitBtn;
      btnSaveCharts: TBitBtn;
      btnCopyCharts: TBitBtn;
      btnPrintCharts: TBitBtn;

      { The menus are set up using the data module }
      pnlMenu: TPanel;
      mainMenuBar: TActionMainMenuBar;

      { File and print dialogs }
      dlgSaveWMF: TSaveDialog;
      dlgSaveCSV: TSaveDialog;
      dlgPrint: TPrintDialog;

      // These "panels" are the decorative markers on the toolbars
      pnlDecorator1: TPanel;
      pnlDecorator2: TPanel;
      pnlDecorator3: TPanel;
      pnlDecorator4: TPanel;

      { Notifies the main window that this form is closing }
      procedure FormClose(Sender: TObject; var Action: TCloseAction);

      { Changes the selected production type, and calls productionTypeChanged }
      procedure cboProdTypesCloseUp(Sender: TObject);

      { Changes the selected zone, and calls zoneChanged }
      procedure cboZonesCloseUp(Sender: TObject);

      { Changes the selected iteration and calls iterationChanged }
      procedure cboIterationChange(Sender: TObject);

    protected
      _borderDisabled: boolean;

      _smdb: TSMDatabase; // Reference to the database that is currently open in the application
      _smSim: TSMSimulationInput; // Reference to the simulation that is currently loaded by the app
      _smHerds: THerdList; // Reference to the herd list that is currently loaded by the app
      _selectedPT: TProductionType; // Reference to the production type selected on this form
      _selectedZone: TZone; // Reference to the zone selected on this form, if applicable
      _displayedIteration: integer;

      _myForm: TForm; // Reference to the main form

      _formUsesZones: boolean;

      _showGridName: boolean;

      _stringGridDict: TQOrderedStringObjectDictionary; // List of string grids that can be saved/copied/printed
      _chartDict: TQOrderedStringObjectDictionary; // List of charts that can be saved/copied/printed

      { Delphi inheritance sucks rocks, so a data module is needed for the action manager }
      _dm: TDMOutputActionManager; // Stores menu items used on all derived forms

      procedure translateUI();

      procedure WMNCPaint( var Msg: TWMNCPaint ); message WM_NCPAINT;

      // Reimplement these protected functions in each derived class
      //------------------------------------------------------------
      { Updates the contents of the form when the selected production type changes }
      procedure productionTypeChanged(); virtual; abstract;

      { Updates the contents of the form when the selected zone changes }
      procedure zoneChanged(); virtual;

      { Updates the contents of the form when the selected iteration changes }
      procedure iterationChanged(); virtual;

      { Updates the contents of the form when the loaded database/simulation changes }
      procedure simChanged(); virtual; abstract;

      { Used to create a list of all charts on the form that might be saved/copied/printed }
      procedure fillChartDict(); virtual;

      { Used to create a list of all grids containing text that might be saved/copied/printed }
      procedure fillStringGridDict(); virtual;

      { Writes the header that precedes any text exported from the grid(s) }
      function textHeader(): string; virtual;

      { Writes the footer that precedes any text exported from the grid(s) }
      function textFooter(): string; virtual;


      // Optionally reimplement this protected function
      //------------------------------------------------
      { Used to enable/disable controls on the form when needed }
      procedure setControlsEnabled( val: boolean ); virtual;


      { Enables/disables controls for saving/copying/printing text }
      procedure setDataControlsEnabled( val: boolean );

      { Enables/disables controls for saving/copying/printing charts }
      procedure setChartControlsEnabled( val: boolean );

      { Nicely positions the label and combo box for iterations }
      procedure placeIterationControls();

      { Adds all production types in the scenario to the dropdown box }
      procedure setupProdTypeComboBox();

      { Adds all iterations in the scenario to the appropriate dropdown box.  Used for single-iteration forms }
      procedure setupIterationComboBox();

      procedure disableIterationComboBox();

      { Should be self-explanatory... }
      procedure saveCopyPrintCharts(Sender: TObject);
      procedure saveCopyData( Sender: TObject );
      procedure printData(Sender: TObject);

      { Procedure associated with the menu item that closes this window }
      procedure closeForm( sender: TObject );

      { Properties used with the data module to set up menu items.  Ignore in derived classes. }
      function getAcnSaveData(): TAction;
      function getAcnSaveCharts(): TAction;
      function getAcnPrintData(): TAction;
      function getAcnPrintCharts(): TAction;
      function getAcnCopyData(): TAction;
      function getAcnCopyCharts(): TAction;
      function getAcnClose(): TAction;

      function getAcnCopyRawData(): TAction;
      function getAcnExportRawData(): TAction;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      { Called when something big changes... }
      procedure resetSim( db: TSMDatabase; sim: TSMSimulationInput; herds: THerdList );

      procedure updateSimStarted(); virtual;
      procedure updateSimComplete(); virtual;

      property borderDisabled: boolean read _borderDisabled write _borderDisabled;

      { Properties used with the data module to set up menu items. Ignore in derived classes. }
      property acnSaveData: TAction read getAcnSaveData;
      property acnSaveCharts: TAction read getAcnSaveCharts;
      property acnPrintData: TAction read getAcnPrintData;
      property acnPrintCharts: TAction read getAcnPrintCharts;
      property acnCopyData: TAction read getAcnCopyData;
      property acnCopyCharts: TAction read getAcnCopyCharts;
      property acnClose: TAction read getAcnClose;

      property acnCopyRawData: TAction read getAcnCopyRawData;
      property acnExportRawData: TAction read getAcnExportRawData;
    end
  ;

implementation

  {$R *.dfm}

  uses
    // Standard Delphi units
    ClipBrd,
    StrUtils,
    
    // General purpose units
    MyStrUtils,
    DebugWindow,
    ControlUtils,
    MyDialogs,
    MyGraphicsUtils,
    I88n,
    SqlClasses,

    // General purpose widgets
    FrameStringGridBase,
    ARSortGrid,
    
    // Application-specific widgets
    FrameChartBase,
    FormMain
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit


  constructor TFormSMOutputBase.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      // Deal with scaling issues (it's better to do this here than in FormCreate,
      // because FormCreate may not be called until after some of the children have
      // already sized themselves properly.)
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );
          
          if( 0 < self.Constraints.MinWidth ) then
            self.Constraints.MinWidth := round( self.Constraints.MinWidth * screen.PixelsPerInch / 96 )
          ;
          if( 0 < self.Constraints.MinHeight ) then
            self.Constraints.MinHeight := round( self.Constraints.MinHeight * screen.PixelsPerInch / 96 )
          ;
          if( 0 < self.Constraints.MaxHeight ) then
            self.Constraints.MaxHeight := round( self.Constraints.MaxHeight * screen.PixelsPerInch / 96 )
          ;
          if( 0 < self.Constraints.MaxWidth ) then
            self.Constraints.MaxWidth := round( self.Constraints.MaxWidth * screen.PixelsPerInch / 96 )
          ;
        end
      ;

      if( AOwner is TForm ) then
        _myForm := AOwner as TForm
      else
        _myForm := nil
      ;      

      _borderDisabled := false;

      _selectedPT := nil;
      _selectedZone := nil;
      _displayedIteration := frmMain.displayedIteration;

      _dm := TDMOutputActionManager.Create( self );

      _dm.ActionManager1.ActionBars[0].ActionBar := mainMenuBar;

      _stringGridDict := TQOrderedStringObjectDictionary.create();

      fillStringGridDict();

      _chartDict := TQOrderedStringObjectDictionary.create();

      fillChartDict();

      // Set function pointers for events
      //---------------------------------

      if( 0 < _stringGridDict.Count ) then
        begin
          acnSaveData.OnExecute := saveCopyData;
          acnPrintData.OnExecute := printData;
          acnCopyData.OnExecute := saveCopyData;
          btnSaveData.onClick := saveCopyData;
          btnCopyData.onClick := saveCopyData;
          btnPrintData.onClick := printData;
        end
      else
        begin
          acnSaveData.OnExecute := nil;
          acnPrintData.OnExecute := nil;
          acnCopyData.OnExecute := nil;
          btnSaveData.onClick := nil;
          btnCopyData.onClick := nil;
          btnPrintData.onClick := nil;
          btnSaveData.Enabled := false;
          btnCopyData.Enabled := false;
          btnPrintData.Enabled := false;
        end
      ;

      if( 0 < _chartDict.Count ) then
        begin
          acnSaveCharts.OnExecute := saveCopyPrintCharts;
          acnPrintCharts.OnExecute := saveCopyPrintCharts;
          acnCopyCharts.OnExecute := saveCopyPrintCharts;
          btnSaveCharts.onClick := saveCopyPrintCharts;
          btnCopyCharts.onClick := saveCopyPrintCharts;
          btnPrintCharts.OnClick := saveCopyPrintCharts;
        end
      else
        begin
          acnSaveCharts.OnExecute := nil;
          acnPrintCharts.OnExecute := nil;
          acnCopyCharts.OnExecute := nil;
          btnSaveCharts.onClick := nil;
          btnCopyCharts.onClick := nil;
          btnPrintCharts.onClick := nil;
          btnSaveCharts.enabled := false;
          btnCopyCharts.enabled := false;
          btnPrintCharts.enabled := false;
        end
      ;

      acnClose.OnExecute := closeForm;

      _showGridName := true;

      _formUsesZones := false;
      cboZones.Visible := false;

      dbcout( '--- TFormSMOutputBase.create done', DBSHOWMSG );
    end
  ;


  procedure TFormSMOutputBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormSMOutputBase.dfm
      // File date: Mon Sep 24 12:04:52 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'SM Output Base Class' );
          btnSaveData.Hint := tr( 'Save data to file' );
          btnPrintData.Hint := tr( 'Print data' );
          btnCopyData.Hint := tr( 'Copy data to clipboard' );
          btnSaveCharts.Hint := tr( 'Save chart to file' );
          btnCopyCharts.Hint := tr( 'Copy chart to clipboard' );
          btnPrintCharts.Hint := tr( 'Print chart' );
          lblIteration.Caption := tr( 'Iteration:' );
          mainMenuBar.Caption := tr( 'mainMenuBar' );
          dlgSaveWMF.Filter := tr( 'Windows Metafile (*.wmf)|*.wmf|All files (*.*)|*.*' );
          dlgSaveCSV.Filter := tr( 'CSV (Comma delimted) (*.csv)|*.csv|All files (*.*)|*.*' );
        end
      ;

    end
  ;


  destructor TFormSMOutputBase.destroy();
    begin
      // _dm is owned by the form, and does not need to be deleted separately.

      // DO NOT delet items in either dictionary: they are owned by the form, and
      // will be freed automatically when the form is closed.
      freeAndNil( _stringGridDict );
      freeAndNil( _chartDict );

      dbcout( endl + '--- ' + self.Name + ' destroyed!' + endl, DBSHOWMSG );
      inherited destroy();
    end
  ;

  procedure TFormSMOutputBase.resetSim( db: TSMDatabase; sim: TSMSimulationInput; herds: THerdList );
    var
      selectedPTName: string;
      i: integer;
      prodTypeFound: boolean;

    begin
      dbcout( '-- TFormSMOutputBase.resetSim()', DBSHOWMSG );

      _smSim := sim;
      _smdb := db;
      _smHerds := herds;

      // FIX ME: try to find a matching pt name in the new list, and set
      // _selectedPT to it, if possible.
      //_selectedPT := nil;  // Fixed - see issue 2517 below

      // Issue 2517 - maintain the user's selected production type unless it was deleted
      selectedPTName := cboProdTypes.Text;
      setupProdTypeComboBox();

      if ( _selectedPT <> nil ) then
        begin
          prodTypeFound := false;
          for i := 0 to cboProdTypes.Items.Count - 1 do
            begin
              if ( cboProdTypes.Items.Strings[i] = selectedPTName ) then
                begin
                  cboProdTypes.ItemIndex := i; // reselect their production type
                  prodTypeFound := true;
                  break;
                end
              ;
            end
          ;

          if ( not prodTypeFound ) then  _selectedPT := nil;
        end
      ;

      simChanged();

      dbcout( '-- Done TFormSMOutputBase.resetSim()', DBSHOWMSG );
    end
  ;


  procedure TFormSMOutputBase.updateSimStarted();
    begin
      // Do nothing here.  Override if necessary in derived classes.
    end
  ;


  procedure TFormSMOutputBase.updateSimComplete();
    begin
      raise exception.create( 'Calling "abstract" function TFormSMOutputBase.updateSimComplete()' );
    end
  ;


  procedure TFormSMOutputBase.placeIterationControls();
    begin
      lblIteration.Left := cboProdTypes.Left + cboProdTypes.Width + 8;
      cboIteration.Left := lblIteration.Left + lblIteration.Canvas.TextWidth( tr( 'Iteration:' ) ) + 8;
    end
  ;


  procedure TFormSMOutputBase.disableIterationComboBox();
    begin
      cboIteration.Items.Clear();
      cboIteration.Enabled := false;
    end
  ;


  procedure TFormSMOutputBase.setupIterationComboBox();
    var
      res: TSqlResult;
      row: TSqlRow;
      q: string;
    begin
      cboIteration.Items.Clear();

      q := 'SELECT DISTINCT (iteration) FROM outDailyByProductionType order by iteration desc';
      res := TSqlResult.create( q, ( _smdb as TSqlDatabase ) );

      // If the database contains iteration data, then fill in the combo box.
      // Otherwise, leave it empty and disable it.

      if ( 0 = res.numRows ) then
        cboIteration.Enabled := false
      else
        begin
          // Enable and populate the combo box
          //----------------------------------
          cboIteration.Enabled := true;

          row := res.fetchArrayFirst();
          while( nil <> row ) do
            begin
              cboIteration.Items.Add(row.field('iteration'));
              row := res.fetchArrayNext();
            end
          ;

          // Which iteration should be displayed?
          //-------------------------------------
          _displayedIteration := frmMain.displayedIteration;
          if ( -1 = _displayedIteration ) then
            begin
              cboIteration.ItemIndex := 0;
              _displayedIteration := StrToInt(cboIteration.Items[cboIteration.ItemIndex]);
              frmMain.displayedIteration := _displayedIteration;
            end
          else
            begin
              if( 0 <= cboIteration.Items.IndexOf( IntToStr( _displayedIteration ) ) ) then
                cboIteration.ItemIndex := cboIteration.Items.IndexOf( IntToStr( _displayedIteration ) )
              else
                begin
                  if( -1 = cboIteration.ItemIndex ) then
                    _displayedIteration := -1
                  else
                    _displayedIteration := StrToInt( cboIteration.Items[cboIteration.ItemIndex] )
                  ;

                  frmMain.displayedIteration := _displayedIteration;
                end
              ;
            end
          ;
        end
      ;

      res.free();
    end
  ;


	procedure TFormSMOutputBase.setupProdTypeComboBox();
  	var
    	it: TProductionTypeListIterator;
      zit: TZoneListIterator;
  	begin
      cboProdTypes.Clear();

      if( assigned( _smSim ) ) then
      	begin
        	cboProdTypes.AddItem( tr( 'All production types' ), nil );
          it := TProductionTypeListIterator.create( _smSim.ptList );
          it.toFirst();
          while( nil <> it.current() ) do
            begin
              cboProdTypes.addItem( it.current().productionTypeDescr, it.current() );
              it.incr();
            end
          ;

          it.free();

          cboProdTypes.ItemIndex := 0;
        end
      ;

      cboZones.Clear();

      if( ( assigned( _smSim ) ) and _smSim.includeZonesGlobal ) then
        begin
          cboZones.AddItem( tr( 'All zones' ), nil );
          zit := TZoneListIterator.create( _smSim.zoneList );
          zit.toFirst();
          while( nil <> zit.current() ) do
            begin
              cboZones.addItem( zit.current().descr, zit.current() );
              zit.incr();
            end
          ;

          zit.free();

          cboZones.ItemIndex := 0;
        end
      ;
    end
  ;


  procedure TFormSMOutputBase.cboProdTypesCloseUp(Sender: TObject);
    begin
      _selectedPT := cboProdTypes.items.objects[cboProdTypes.itemIndex] as TProductionType;

      productionTypeChanged();
    end
  ;


  procedure TFormSMOutputBase.cboZonesCloseUp(Sender: TObject);
    begin
      _selectedZone := cboZones.items.objects[cboZones.itemIndex] as TZone;

      zoneChanged();
    end
  ;


  procedure TFormSMOutputBase.cboIterationChange(Sender: TObject);
    begin
      iterationChanged();
    end
  ;


  procedure TFormSMOutputBase.setControlsEnabled( val: boolean );
    begin
      cboProdTypes.Enabled := val;
      mainMenuBar.Enabled := val;

      if( cboZones.Visible and _formUsesZones and _smSim.includeZonesGlobal ) then
        cboZones.Enabled := val
      ;

      setChildrenEnabled( pnlButtons, val, false );
    end
  ;


  procedure TFormSMOutputBase.setDataControlsEnabled( val: boolean );
    begin
      btnSaveData.Enabled := val;
      btnCopyData.Enabled := val;
      btnPrintData.Enabled := val;

      _dm.acnSaveData.Enabled := val;
      _dm.acnPrintData.Enabled := val;
      _dm.acnCopyData.Enabled := val;
      
      _dm.acnExportRawData.Enabled := val;
      _dm.acnCopyRawData.Enabled := val;
    end
  ;


  procedure TFormSMOutputBase.setChartControlsEnabled( val: boolean );
    begin
      btnSaveCharts.Enabled := val;
      btnCopyCharts.Enabled := val;
      btnPrintCharts.Enabled := val;

      _dm.acnSaveCharts.Enabled := val;
      _dm.acnPrintCharts.Enabled := val;
      _dm.acnCopyCharts.Enabled := val;
    end
  ;


  procedure TFormSMOutputBase.fillChartDict();
    begin
      // In the base class, the dictionary is empty.
      // Fill it when necessary in derived classes.
    end
  ;


  procedure TFormSMOutputBase.fillStringGridDict();
    begin
      // In the base class, the dictionary is empty.
      // Fill it when necessary in derived classes.
    end
  ;


  procedure TFormSMOutputBase.zoneChanged();
    begin
      // Nothing happens in the base class.
      // Reimplement as needed in derived classes
    end
  ;


  procedure TFormSMOutputBase.iterationChanged();
    begin
      // Nothing happens in the base class.
      // Reimplement as needed in derived classes
    end
  ;


  procedure TFormSMOutputBase.WMNCPaint( var Msg: TWMNCPaint );
    begin
      if( not _borderDisabled ) then
        inherited
      ;
    end
  ;


  procedure TFormSMOutputBase.FormClose( Sender: TObject; var Action: TCloseAction );
    begin
      dbcout( '-- TFormSMOutputBase.FormClose()', DBSHOWMSG );
      try
        (_myForm as TFormMain).uncheckWindowMenuItem( self.name );
      except
        // fail silently
      end;

      Action := caFree;

      dbcout( '-- Done TFormSMOutputBase.FormClose()', DBSHOWMSG );
    end
  ;


  procedure TFormSMOutputBase.closeForm( sender: TObject );
    begin
      dbcout( '-- TFormSMOutputBase.closeForm()', DBSHOWMSG );
      close();
      dbcout( '-- Done TFormSMOutputBase.closeForm()', DBSHOWMSG );
    end
  ;


  function TFormSMOutputBase.getAcnSaveData(): TAction;
    begin
      result := _dm.acnSaveData;
    end
  ;


  function TFormSMOutputBase.getAcnSaveCharts(): TAction;
    begin
      result := _dm.acnSaveCharts;
    end
  ;


  function TFormSMOutputBase.getAcnPrintData(): TAction;
    begin
      result := _dm.acnPrintData;
    end
  ;


  function TFormSMOutputBase.getAcnPrintCharts(): TAction;
    begin
      result := _dm.acnPrintCharts;
    end
  ;


  function TFormSMOutputBase.getAcnCopyData(): TAction;
    begin
      result := _dm.acnCopyData;
    end
  ;


  function TFormSMOutputBase.getAcnCopyCharts(): TAction;
    begin
      result := _dm.acnCopyCharts;
    end
  ;


  function TFormSMOutputBase.getAcnClose(): TAction;
    begin
      result := _dm.acnClose;
    end
  ;


  function TFormSMOutputBase.getAcnCopyRawData(): TAction;
    begin
      result := _dm.acnCopyRawData;
    end
  ;


  function TFormSMOutputBase.getAcnExportRawData(): TAction;
    begin
      result := _dm.acnExportRawData;
    end
  ;


//-----------------------------------------------------------------------------
// Save or copy data
//-----------------------------------------------------------------------------
  function TFormSMOutputBase.textHeader(): string;
    begin
      result := '';
    end
  ;


  function TFormSMOutputBase.textFooter(): string;
    begin
      result := '';
    end
  ;

  procedure TFormSMOutputBase.saveCopyData( Sender: TObject );
    var
      key: string;
      i, j: integer;

      fileName: string;
      outFile: textFile;
      s: string;
      success: boolean;
      senderName: string;

      msgStr: string;
    begin
      senderName := (sender as TComponent).Name;


      // Are any items currently displayed?
      //------------------------------------
      j := 0;
      for i := 0 to _stringGridDict.Count - 1 do
        if( (_stringGridDict.GetItemByIndex(i) as TWinControl).visible ) then inc( j )
      ;

      if( 0 = j ) then
        begin
            msgOK(
              tr( 'No tables are currently displayed.' ),
              '',
              IMGInformation,
              _myForm
            )
          ;
          exit;
        end
      ;


      // Get the file name to use, if necessary
      //----------------------------------------
      if( ( 'acnSaveData' = senderName ) or ( 'btnSaveData' = senderName ) ) then
        begin
          if( dlgSaveCSV.Execute() ) then
            fileName := dlgSaveCSV.FileName
          else
            exit
          ;
        end
      ;

      repaint();


      // Build the string
      //-----------------
      s := textHeader();

      for i := 0 to _stringGridDict.Count - 1 do
        begin
          key := _stringGridDict.GetKeyByIndex(i);
          if( ( _stringGridDict.Item[ key ] as TWinControl).Visible ) then
            begin
              if( _stringGridDict.Item[ key ] is TFrameStringGridBase ) then
                begin
                  if( _showGridName ) then
                    s := s + endl + key + endl + ( _stringGridDict.Item[ key ] as TFrameStringGridBase).csvText()
                  else
                    s := s + endl + ( _stringGridDict.Item[ key ] as TFrameStringGridBase).csvText()
                  ;
                end
              else if( _stringGridDict.Item[ key ] is TARSortGrid ) then
                begin
                  if( _showGridName ) then
                    s := s + endl + key + endl + ( _stringGridDict.Item[ key ] as TARSortGrid).csvText()
                  else
                    s := s + endl + ( _stringGridDict.Item[ key ] as TARSortGrid).csvText()
                  ;
                end
              ;
            end
          ;
        end
      ;

      s := s + textFooter();


      // Write the file or copy the string to the clipboard
      //----------------------------------------------------
      if( ( 'acnSaveData' = senderName ) or ( 'btnSaveData' = senderName ) ) then
        begin
          try
            assignFile( outFile, fileName );
            rewrite( outFile );
            writeln( outFile, s );
            closeFile( outFile );
            success := true;
          except
            success := false;
          end;

          if( success ) then
            msgOK(
              ansiReplaceStr( tr( 'File xyz has been successfully saved.' ), 'xyz', abbrevPath( fileName, 40 ) ),
              tr( 'File saved' ),
              IMGSuccess,
              _myForm
            )
          else
            msgOK(
              ansiReplaceStr( tr( 'File xyz could not be written.  Please check your disk and your folder permissions.' ), 'xyz', abbrevPath( fileName, 40 ) ),
              tr( 'Save failed' ),
              IMGCritical,
              _myForm
            )
          ;
        end
      else if( ( 'acnCopyData' = senderName ) or ('btnCopyData' = senderName ) ) then
        begin
          ClipBoard.SetTextBuf( PChar(s) );
          if( 1 = _stringGridDict.Count ) then
            msgStr := tr( 'The visible table has been copied to the clipboard.' )
          else
            msgStr := tr( 'The visible tables have been copied to the clipboard.' )
          ;

          if( 'acnCopyData' = senderName ) then
            msgOK(
              msgStr,
              tr( 'Copy successful' ),
              IMGSuccess,
              _myForm
            )
          ;
        end
      ;
    end
  ;


  procedure TFormSMOutputBase.printData(Sender: TObject);
    var
      i, j: integer;
    begin
      // Are any items currently displayed?
      //------------------------------------
      j := 0;
      for i := 0 to _stringGridDict.Count - 1 do
        if( (_stringGridDict.GetItemByIndex(i) as TWinControl).visible ) then inc( j )
      ;

      if( 0 = j ) then
        begin
            msgOK(
              tr( 'No tables are currently displayed.' ),
              '',
              IMGInformation,
              _myForm
            )
          ;
          exit;
        end
      ;

      repaint();
      self.Enabled := false;


      // Get the printer, if menu item
      // If button, use current printer
      //--------------------------------
      if( ( 'acnPrintData' = (sender as TComponent).Name ) and not ( dlgPrint.Execute() ) ) then
        exit
      ;


      // Print each grid
      //-----------------
      for i := 0 to _stringGridDict.Count - 1 do
        begin
          if( (_stringGridDict.GetItemByIndex(i) as TWinControl).visible ) then
            begin
              if( _stringGridDict.GetItemByIndex(i) is TFrameStringGridBase ) then
                (_stringGridDict.GetItemByIndex(i) as TFrameStringGridBase).printGrid(
                  //textHeader() + _stringGridDict.GetKeyByIndex(i) // FIX ME: title printing doesn't work well
                  _stringGridDict.GetKeyByIndex(i), textHeader()
                )
              else if( _stringGridDict.GetItemByIndex(i) is TARSortGrid ) then
                (_stringGridDict.GetItemByIndex(i) as TARSortGrid).print() // FIX ME: this needs work.
              ;
            end
          ;
        end
      ;


      self.Enabled := true;
    end
  ;
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Save or copy charts
//-----------------------------------------------------------------------------
  procedure TFormSMOutputBase.saveCopyPrintCharts(Sender: TObject);
    var
      key: string;
      i, j: integer;

      canvas: TCanvas;
      metaFileCanvas: TMetafileCanvas;
      metaFile: TMetafile;
      chartMetaFile: TMetaFile;
      maxChartWidth: integer;
      totalHeight: integer;
      vertPosition: integer;
      notUsed: HWND;

      fileName: string;
      success: boolean;

      aFormat: word;
      aData: Cardinal;
      aPalette: HPALETTE;

      msgStr: string;
    begin
      metaFile := nil;
      canvas := nil;

      // Are any items currently displayed?
      //------------------------------------
      j := 0;
      for i := 0 to _chartDict.Count - 1 do
        if( (_chartDict.GetItemByIndex(i) as TFrameChartBase).visible ) then inc( j )
      ;

      if( 0 = j ) then
        begin
            msgOK(
              tr( 'No charts are currently displayed.' ),
              '',
              IMGInformation,
              _myForm
            )
          ;
          exit;
        end
      ;


      // Get the file name or printer, if appropriate
      //----------------------------------------------
      if
        ( 'acnSaveCharts' = (sender as TComponent).Name )
      or
        ( 'btnSaveCharts' = (sender as TComponent).name )
      then
        begin
          if( dlgSaveWMF.Execute() ) then
            begin
              fileName := dlgSaveWMF.FileName;
              if( '.wmf' <> rightStr( fileName, 4 ) ) then fileName := fileName + '.wmf';
            end
          else
            exit
          ;
        end
      else if( 'acnPrintCharts' = (sender as TComponent).name ) then
        if( not( dlgPrint.Execute() ) ) then exit;
      ;

      Screen.Cursor := crHourGlass;
      repaint();


      // Build the metafile
      //--------------------
      // Determine the maximum chart width and total height
      maxChartWidth := 0;
      totalHeight := 0;
      for i := 0 to _chartDict.Count -1 do
        begin
          if( (_chartDict.GetItemByIndex(i) as TFrameChartBase).chartWidth > maxChartWidth ) then
            maxChartWidth := (_chartDict.GetItemByIndex(i) as TFrameChartBase).chartWidth
          ;
          inc( totalHeight, ( ( i * 10 ) + (_chartDict.GetItemByIndex(i) as TFrameChartBase).chartHeight ) );
        end
      ;

      // Set up the necessary objects
      try
        try
          canvas := TCanvas.Create();
          metaFile := TMetafile.Create();
          metaFile.Width := maxChartWidth;
          metaFile.Height := totalHeight;

          canvas.Handle := GetDeviceContext( notUsed );
          metaFileCanvas := TMetafileCanvas.Create( metaFile, canvas.Handle );
          vertPosition := 0;

          // Add each chart to the metafile
          try
            for i := 0 to _chartDict.Count -1 do
              begin
                key := _chartDict.GetKeyByIndex(i);
                if( (_chartDict.GetItemByIndex(i) as TFrameChartBase).visible ) then
                  begin
                    chartMetaFile := (_chartDict.GetItemByIndex(i) as TFrameChartBase).createMetafile();
                    metaFileCanvas.Draw( 0, vertPosition, chartMetaFile );
                    inc( vertPosition, chartMetaFile.Height + 10 );
                    chartMetaFile.Free();
                  end
                ;
              end
            ;
          finally
            metaFileCanvas.Free();
          end;

          // Save or copy the metafile
          if
            ( 'acnSaveCharts' = (sender as TComponent).Name )
          or
            ( 'btnSaveCharts' = (sender as TComponent).Name )
          then
            begin
              try
                metaFile.SaveToFile( fileName );
                success := true;
              except
                success := false;
              end;

              if( success ) then
                msgOK(
                  ansiReplaceStr( tr( 'File xyz has been successfully saved.' ), 'xyz', abbrevPath( fileName, 40 ) ),
                  tr( 'File saved' ),
                  IMGSuccess,
                  _myForm
                )
              else
                msgOK(
                  ansiReplaceStr( tr( 'File xyz' ), 'xyz', abbrevPath( fileName, 40 ) ) + ' could not be written.  Please check your disk and your folder permissions.',
                  tr( 'Save failed' ),
                  IMGCritical,
                  _myForm
                )
              ;
            end
          else if
            ( 'acnCopyCharts' = (sender as TComponent).Name )
          or
            ( 'btnCopyCharts' = (sender as TComponent).Name )
          then
            begin
              metaFile.SaveToClipboardFormat( aFormat, aData, aPalette);
              ClipBoard.SetAsHandle( aFormat, aData );
              if( 1 = _chartDict.Count ) then
                msgStr := tr( 'The visible chart has been copied to the clipboard.' )
              else
                msgStr := tr( 'The visible charts have been copied to the clipboard.' )
              ;

              if( 'acnCopyCharts' = (sender as TComponent).Name ) then
                msgOK(
                  msgStr,
                  tr( 'Copy successful' ),
                  IMGSuccess,
                  _myForm
                )
              ;
            end
          else if
            ( 'acnPrintCharts' = (sender as TComponent).name )
          or
            ( 'btnPrintCharts' = (sender as TComponent).name )
          then
            begin
              printImageSinglePage( metaFile );
              if( 1 = _chartDict.Count ) then
                msgStr := 'The visible chart has been sent to the printer.'
              else
                msgStr := 'The visible charts have been sent to the printer.'
              ;

              if( 'acnPrintCharts' = (sender as TComponent).name ) then
                msgOK(
                  msgStr,
                  'Printing image',
                  IMGInformation,
                  _myForm
                )
              ;
            end
          ;

        finally
          freeAndNil( metaFile );
          freeAndNil( canvas );
          Screen.Cursor := crDefault;
        end;

      except
        msgOK(
          tr( 'An application error occurred.  The image could not be created.' ),
          '',
          IMGCritical,
          _myForm
        );
      end;

    end
  ;
//-----------------------------------------------------------------------------




end.
