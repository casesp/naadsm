unit FormMain;

(*
FormMain.pas/dfm
----------------
Begin: 2005/05/10
Last revision: $Date: 2013-06-27 19:11:27 $ $Author: areeves $
Version: $Revision: 1.123.4.26 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)



{$INCLUDE ../Defs.inc}

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
    ActnList,
    ImgList,
    ToolWin,
    ActnMan,
    ActnCtrls,
    ActnMenus,
    XPStyleActnCtrls,
    Menus,
    StdCtrls,
    ExtCtrls,
    Buttons,
    ComCtrls,
    Grids,
    ActnPopupCtrl,

    // General-purpose units
    CStringList,
    IniHandler,  // For *.ini settings
    CmdLine,

    // APHI modeling library
    ModelDatabase,

    // Application-specific data structures
    SMScenario,
    SMSimulationInput,
    SMDatabase,
    Herd,
    ProductionType,
    SMI88nSettings,
    StatusEnums,

    // Needed here for "next form" constants
		FormSMWizardBase,

		// Used for interaction with the NAADSM DLL
    NAADSMLibrary,

    // Application-specific constants
    StringConsts
  ;

  
  type TFormMain = class( TForm )
      // Main menu-associated controls
      //-------------------------------
      ActionMainMenuBar1: TActionMainMenuBar;
      ActionManager1: TActionManager;
      ImageList1: TImageList;

      // File menu actions
      //------------------
      ActionNew: TAction;
      acnEmptyScenario: TAction;
      acnSampleScenario: TAction;
      ActionOpen: TAction;
      ActionSave: TAction;
      ActionSaveAs: TAction;
      ActionImport: TAction;
      ActionExport: TAction;
      ActionClose: TAction;
      ActionExit: TAction;

      // Scenario Parameters menu actions
      //----------------------------------
      ActionGeneralParams: TAction;
      ActionProdType: TAction;
      ActionUnitsMenu: TAction;
      ActionInitialUnitOptions: TAction;
      ActionHerdListEditor: TAction;

      ActionDiseaseMenu: TAction;
      ActionDiseaseOptions: TAction;
      ActionDisease: TAction;

      ActionSpreadMenu: TAction;
      ActionSpreadOptions: TAction;
      ActionProdTypePairs: TAction;
      ActionContactSpread: TAction;
      ActionAirborneSpread: TAction;

      ActionDetectionMenu: TAction;
      ActionYesNoDetection: TAction;
      ActionDetection: TAction;

      ActionTracingMenu: TAction;
      ActionTracingOptions: TAction;
      ActionTracing: TAction;
      ActionTracingHerdExam: TAction;
      ActionTracingTesting: TAction;

      ActionZonesMenu: TAction;
      ActionZonesOptions: TAction;
      ActionZonesDefinition: TAction;
      ActionZones: TAction;

      ActionDestructionMenu: TAction;
      ActionDestrGlobal: TAction;
      ActionDestrPriority: TAction;
      ActionDestruction: TAction;

      ActionVaccinationMenu: TAction;
      ActionVaccGlobal: TAction;
      ActionVaccPriority: TAction;
      ActionVaccination: TAction;

      ActionCostMenu: TAction;
      ActionCostOptions: TAction;
      ActionCostsDestr: TAction;
      ActionCostsVacc: TAction;
      ActionCostsZones: TAction;

			ActionOutputOptions: TAction;

      ActionCustomOutputMenu: TAction;
      ActionCustomOutputOptions: TAction;
      ActionCustomOutputDefinitions: TAction;

      ActionVerifyHerds: TAction;
      ActionVerifyScenario: TAction;
      ActionClearOutput: TAction;

      // Ouput windows actions
      //----------------------
      ActionMap: TAction;
      ActionOutChart: TAction;
      ActionOutZoneChart: TAction;
      ActionOutputStats: TAction;
      ActionEpiCurve: TAction;
      ActionCompareStats: TAction;
      ActionEvents: TAction;
      ActionExposures: TAction;
      ActionSummary: TAction;

      ActionCascade: TAction;
      ActionArrange: TAction;
      ActionCloseWindows: TAction;

      // Run menu actions
      //------------------
      ActionRunUntilOutbreakEnd: TAction;
      ActionRunUntilDetection: TAction;
      ActionRunUntilDay: TAction;
      ActionRunUntilDiseaseEnd: TAction;

      // Tools menu actions
      //-------------------
      acnLanguageSettings: TAction;
      acnRegionalSettings: TAction;

      // Help menu actions
      //------------------
      ActionAbout: TAction;
      ActionWebsite: TAction;
      ActionForums: TAction;

      // Other components
      //------------------
      OpenDialog1: TOpenDialog;
      SaveDialog1: TSaveDialog;
      DatabaseActivityTimer: TTimer;

      // Status panel
      //-------------
      pnlRunStatus: TPanel;
      pbrIterations: TProgressBar;
      pbrIterationDays: TProgressBar;
      lblIterationCounter: TLabel;
      lblDayCounter: TLabel;
      pnlRunMessage: TPanel;
      lblRunMessage: TLabel;
      btnStopSim: TSpeedButton;
      ActionStop: TAction;
      pnlCounters: TPanel;
      pnlProgressBars: TPanel;
      pnlStop: TPanel;

      // For trivial testing
      //---------------------
    	btnTest: TButton;


      // File menu commands
      //--------------------
      procedure ActionNewExecute(Sender: TObject);
      procedure acnEmptyScenarioExecute(Sender: TObject);
      procedure acnSampleScenarioExecute(Sender: TObject);
      procedure ActionOpenExecute(Sender: TObject);
      procedure ActionSaveAsExecute(Sender: TObject);
      procedure ActionSaveExecute(Sender: TObject);
      procedure ActionImportExecute( Sender: TObject );
      procedure ActionExportExecute( Sender: TObject );
      procedure ActionCloseExecute( Sender: TObject );
      procedure ActionExitExecute(Sender: TObject);

      // scenario Parameters menu commands
      //-------------------------------
      procedure ActionScenarioParamExecute( sender: TObject );

      procedure ActionClearOutputExecute(Sender: TObject);
      procedure ActionVerifyScenarioExecute(Sender: TObject);

      // Run menu commands
      //--------------------
      procedure ActionRunExecute(Sender: TObject);
      procedure btnStopSimClick(Sender: TObject);

      //Output windows menu commands
      //----------------------------
      procedure ActionOutputFormExecute( Sender: TObject );

      procedure ActionArrangeExecute(Sender: TObject);
      procedure ActionCascadeExecute(Sender: TObject);
      procedure ActionCloseWindowsExecute(Sender: TObject);

      // Tools menu commands
      //--------------------
      procedure ActionLanguageSettingsExecute( Sender: TObject );
      procedure acnRegionalSettingsExecute( Sender: TObject );

      // Help menu commands
      //--------------------
      procedure ActionAboutExecute(Sender: TObject);
      procedure ActionWebsiteExecute(Sender: TObject);
      procedure ActionSupportForumsExecute(Sender: TObject);

      // Form events
      //--------------
      procedure FormCreate( Sender: TObject );
      procedure FormClose(Sender: TObject; var Action: TCloseAction);

      // Other controls
      //---------------
      procedure DatabaseActivityTimerTimer(Sender: TObject);

      // For trivial testing
      //---------------------
  		procedure BtnTestClick(Sender: TObject);

    protected
    	// Files
      //-------
      _smdb: TSMDatabase;
      _ini: TIniHandler;

      // Data structures
      //----------------
      _smScenario: TSMScenario;
      _i88nSettings: TSMI88nSettings;

      // File handling
      //--------------
      _saveCanceled: boolean;

      // Properties
      //-----------
      _userStop: integer;
      _simIsRunning: boolean;
      _displayedIteration: integer; /// Last Iteration displayed in summary forms...keeps them all in sync with each other
      _iterationInProgress: integer; /// ID number of the currently running iteration, if there is one.

      _showOutputWarning: boolean;

      _perimeterColors: TColorArray;

      // For input screens
      //-------------------
      _windowsLocked: boolean;
      _lastParamForm: TForm;
      _paramFormList: TCStringList;
      _paramChangesInProgress: boolean;
      _showMultiUseWarning: boolean;
      _showFunctionRemovalWarning: boolean;
      _showTestFeatureWarning: boolean;
      _showApplyToAllWarning: boolean;
      _selectedProdTypeIndex: integer;
      _selectedProdTypePairIndex: integer;
      _selectedZoneIndex: integer;

      // For output screens
      //--------------------
      _openWindows: integer;

      // For use during a simulation run
      //---------------------------------
      // If the simulation is running too fast, this variable can be used
      // to introduce a delay (e.g. wait at least x milliseconds before moving
      // on to the next simulation day).  See function dayComplete().
      _simDayStartTime: comp; // Funky type: see Delphi help for an explanation.
      _firstOverallDet: integer;
      _firstOverallDestr: integer;
      _firstOverallVacc: integer;

      procedure translateUI();
      procedure translateUIManual();

      procedure WMNCActivate( var Msg: TWMNCActivate ); message WM_NCACTIVATE;

      procedure initialize();

      procedure handleAppUpdateMessage( success: boolean; updateMessage: string );

      // File handling
      //---------------
      procedure updateIniSettings();
      procedure closeScenarioFile();
      procedure closeDB();
      function selectScenarioFile( var fileName: string ): boolean;
      function getSaveScenarioName( var fileName: string ): boolean;
      function getUntitledMDB( path: string ): string;
      function getScenario(): TSMScenario;
      function getsmDB():TSMDatabase;
      procedure clearScenarioOutput();

      // Functions for input screens
      //-----------------------------
      procedure fillParamFormList();
      procedure showParamForm(
        const formIndex: integer;
        var clearOutput: boolean;
        nextFormIndex: integer = NF_NONE;
        frmLeft: integer = -1;
        frmTop: integer = -1
      ); overload;

      procedure showParamForm( name: string ); overload;

      // Functions for opening scenario files
      //--------------------------------------
      procedure openScenario( const fileName: string );
      function processUpdates( schemaUpdateReason: TDBSchemaUpdateReason ): boolean;
      procedure checkForIncompleteIteration();

      // Display functions
      //-------------------
      procedure updateDisplay();
      procedure setInitialWindowPosition();
      procedure setOpenWindows( val: integer );
      procedure updateOutputWindows( const clearOutput: boolean );
      procedure closeOutputWindows();
      procedure closeCumulativeOutputWindows();
      procedure closeIterationOutputWindows();
      procedure frmMapToggle();
  		procedure frmDailyStatusByProdTypeToggle();
  		procedure frmDailyZoneStatusByProdTypeToggle();
			procedure frmIterationSummaryToggle();
      procedure frmOutputStatsToggle();
      procedure frmCompareStatsToggle();
      procedure frmEpiCurveToggle();
      procedure frmOutputEventsToggle();
      procedure frmOutputExposuresToggle();

      // Properties
      //-----------
      function getScenarioIsOpen(): boolean;
      function getFirstOverallVaccination(): integer;
      function getFirstOverallDetection(): integer;
      function getFirstOverallDestruction(): integer;

      function getDisplayedIteration(): integer;
      procedure setDisplayedIteration( _iteration: integer );

      function getIterationInProgress(): integer;

      function getSimStatusStr(): string;
    public
      // Construction/destruction
      //--------------------------
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      // Command line parameters
      //------------------------
      procedure setCommandParams( cmdParams: TCmdLine );

      //procedure showAuxilliaryWindows();

      // File handling, needed by main app
      //----------------------------------
      {* Used by FormImport when a new database object is created. }
      procedure showMap();

      function createSimObjects(): boolean;

      // Display functions required by the general public
      //--------------------------------------------------
      procedure lockWindows();
      procedure unlockWindows();
      procedure uncheckWindowMenuItem( frmName: string );
      procedure updateMenuItems( disableAll: boolean; temporarilyEnable: boolean = false );
      procedure updateScenarioParamsMenuItems( const paramsEnabled: boolean );

      procedure updateCaption();

      // Functions used by the parameter forms
      function paramFormForSenderName( const senderName: string ): string;
      function paramFormIndex( const paramFormName: string ): integer;

      // For DLL interaction
			//---------------------
      procedure simStart();
      procedure iterationStart( it: integer );
      procedure dayStart( day: integer );

      procedure changeHerdState( h: THerd );
      procedure traceHerd( h: THerd );
      procedure examineHerd( h: THerd );
      procedure quarantineHerd( h: THerd );

      procedure detectHerd( h: THerd; wasFirstEventForProdType: boolean; day: integer );
      procedure destroyHerd( h: THerd; wasFirstEventForProdType: boolean; day: integer );
      procedure vaccinateHerd( h: THerd; wasFirstEventForProdType: boolean; day: integer );

      procedure dayComplete( day: integer );
      procedure outbreakEnd( day: integer );
      procedure iterationComplete( it: integer );
      procedure simComplete( success: boolean );

      // Properties
      //------------
      property scenarioIsOpen: boolean read getScenarioIsopen;
      property Scenario: TSMScenario read getScenario;
      property smDB:TSMDatabase read getsmDB;

      property simStatusStr: string read getSimStatusStr;
      property simIsRunning: boolean read _simIsRunning;
      property iterationInProgress: integer read getIterationInProgress;
      property userStop: integer read _userStop;

      property firstOverallVaccination: integer read getFirstOverallVaccination;
      property firstOverallDetection: integer read getFirstOverallDetection;
      property firstOverallDestruction: integer read getFirstOverallDestruction;
      property displayedIteration: integer read getDisplayedIteration write setDisplayedIteration;
    end
  ;

  var
    frmMain: TFormMain;

implementation

	{$R *.dfm}

  uses
    // Standard Delphi units
    ShellAPI,
    Registry,
    Math,
    StrUtils,

    // General-purpose units
    MyStrUtils,
    DebugWindow,
    SqlClasses,
    WindowsUtils,
    FunctionPointers,
    RegExpDefs,
    ZipFunctions,
    ARMath,
    NetworkUtils,
    ControlUtils,
    I88n,

    // General-purpose graphical units
    MyDialogs,
    FormProgress,

    // Application-specific data structures
    ChartFunction,
    ProbDensityFunctions,
    NAADSMLibraryTypes,
    SMExceptionHandler,

    // Various application-specific forms and dialogs
    FormImport,
    FormExport,

    FormAboutExperimental,
    FormAbout,
    FormSplashExperimental,
    FormSplash,
    FormAppUpdate,
    FormRegistration,
    FormLanguageSettings,
    FormRegionalSettings,

    // Output forms
    FormIterationSummary,
    FormMap,
    FormDailyStatusByProdType,
    FormDailyZoneStatusByProdType,
    FormOutputStats,
    FormScenarioComparison,
    FormSummaryEpiCurves,
    FormOutputEvents,
    FormOutputExposures,

    // Specialized dialogs
    DialogRunSimException,
    DialogLongMessage

    // for testing
    , sdew
  ;


	const
  	DBSHOWMSG: boolean = false; // set to true to show debugging messages for this unit.

// ----------------------------------------------------------------------------
// Creation/initialization/destruction
// ----------------------------------------------------------------------------
  {*
  Pretty typical constructor.  Reads INI file (see class TIniHandler) and
  creates the list of forms used to set the scenario parameters (see
  fillParamFormList() and class TFormWizardBase).

  @param AOWner TComponent, passed to inherited TForm constructor.
  }
  constructor TFormMain.create( AOwner: TComponent );
    var
      httpThread: TSimpleHTTPThread;

      userRegistered: boolean;
      regValue: integer;
      frmRegistration: TFormRegistration;

      callHome: boolean;
      urlMsg: string;
    begin
      inherited create( AOwner );
      translateUI();

      initialize();

      Application.Title := MASTERCAPTION;

      _i88nSettings := TSMI88nSettings.create();
      i88nSetSettings( _i88nSettings );

      DatabaseActivityTimer.Interval := 1000 * 60 * 10; // 10 minutes
      DatabaseActivityTimer.Enabled := true;

      ActionImport.Visible := true;

      _windowsLocked := false;
      _openWindows := 0;
      _paramChangesInProgress := false;
      _lastParamForm := nil;
      _smScenario := nil;
      _smdb := nil;

      _ini := TIniHandler.create( appPath() + 'spreadmodel.ini' );


      // Set up this window
      //-------------------
      setInitialWindowPosition();
      updateCaption();

      lblRunMessage.Caption := '';
      btnTest.visible := {$IFDEF DEBUG} true {$ELSE} false {$ENDIF};

      updateMenuItems( false );

      {$IFDEF ENGLISHONLY}
        acnLanguageSettings.Visible := false;
      {$ENDIF}

      // Setup parameter wizard windows/forms
      //-------------------------------------
      _showOutputWarning := true;
      _showMultiUseWarning := true;
      _showFunctionRemovalWarning := true;
      _showTestFeatureWarning := true;
      _showApplyToAllWarning := true;
      _paramFormList := TCStringList.create();
      fillParamFormList();


      // Set up output windows/forms
      //----------------------------
      frmMap := nil;
      frmDailyStatusByProdType := nil;
      frmDailyZoneStatusByProdType := nil;
      frmIterationSummary := nil;
      frmOutputStats := nil;
      frmCompareStats := nil;
      frmEpiCurve := nil;
      frmOutputEvents := nil;
      frmOutputExposures := nil;

      // See if the user is registered
      //------------------------------
      try
        regValue := getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','UserIsRegistered' );
      except
        regValue := 0;
      end;

      userRegistered := ( regValue > 0 );

      if( not( userRegistered ) ) then
        begin
          frmRegistration := TFormRegistration.create( self );
          // The form shows itself...
          frmRegistration.Release();
        end
      ;

      {$IFDEF DEBUG}
        {$IFDEF DISABLEWEBCHECK}
          callHome := false;
        {$ELSE}
          callHome := true;
        {$ENDIF}
      {$ELSE}
        callHome := true;
      {$ENDIF}

      if( callHome ) then
        begin
          // Launch the HTTP thread to check for updates
          //--------------------------------------------
          urlMsg := 'http://www.naadsm.org/naadsm-update.php?'
            + 'vers=' + MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER
            + '&reg=' + intToStr( regValue )
          ;

          if( 0 < length( BRANCHNAME ) ) then
            urlMsg := urlMsg + '&exptvers=' + BRANCHNAME
          ;

          httpThread := TSimpleHTTPThread.create(
            true,
            urlMsg,
            APPNAME + '/'+ MAJORVERSIONNUMBER + '.' + MINORVERSIONNUMBER + ' (' + getShortOSVersionText() + ')',
            handleAppUpdateMessage
          );

          httpThread.Resume();
        end
      ;
    end
  ;
  
  
  procedure TFormMain.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormMain.dfm
      // File date: Mon Sep 24 12:04:52 2007

      // Manually updated on 4/28/08 to include new parameter forms.

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM 3.2' );
          Hint := tr( 'Open a file to start a new session' );
          ActionMainMenuBar1.Caption := tr( 'ActionMainMenuBar1' );
          lblIterationCounter.Caption := tr( 'lblIterationCounter' );
          lblDayCounter.Caption := tr( 'lblDayCounter' );
          btnStopSim.Caption := tr( 'Stop' );
          lblRunMessage.Caption := tr( 'Message' );
          btnTest.Caption := tr( 'btnTest' );
          ActionExit.Caption := tr( 'E&xit' );
          ActionGeneralParams.Caption := tr( '&Start setup' );
          ActionProdType.Caption := tr( '&Production type(s)' );
          ActionHerdListEditor.Caption := tr( 'Se&t up units' );
          ActionAbout.Caption := tr( '&About NAADSM...' );
          ActionOpen.Caption := tr( '&Open scenario file...' );
          ActionDiseaseMenu.Caption := tr( '&Disease' );
          ActionDiseaseOptions.Caption := tr( '&Disease options' );
          ActionDisease.Caption := tr( '&Production type settings for disease' );
          ActionSpreadMenu.Caption := tr( 'Disease sp&read' );
          ActionSpreadOptions.Caption := tr( '&Spread options' );
          ActionProdTypePairs.Caption := tr( '&Production type combinations' );
          ActionContactSpread.Caption := tr( '&Contact spread' );
          ActionAirborneSpread.Caption := tr( '&Airborne spread' );
          ActionDetectionMenu.Caption := tr( 'Detect&ion' );
          ActionYesNoDetection.Caption := tr( '&Detection options' );
          ActionDetection.Caption := tr( '&Production type settings for detection' );
          ActionTracingMenu.Caption := tr( 'Trac&ing' );
          ActionTracingOptions.Caption := tr( '&Global tracing options' );
          ActionTracing.Caption := tr( '&Production type settings for tracing' );
          ActionTracingHerdExam.Caption := tr( '&Examination of traced units' );
          ActionTracingTesting.Caption := tr( '&Diagnostic testing of traced units' );
          ActionDestructionMenu.Caption := tr( 'Destructio&n' );
          ActionDestrGlobal.Caption := tr( '&Global destruction options' );
          ActionDestrPriority.Caption := tr( '&Destruction priorities' );
          ActionDestruction.Caption := tr( '&Production type settings for destruction' );
          ActionVaccinationMenu.Caption := tr( '&Vaccination' );
          ActionVaccGlobal.Caption := tr( '&Global vaccination options' );
          ActionVaccPriority.Caption := tr( '&Vaccination priorities' );
          ActionVaccination.Caption := tr( '&Production type settings for vaccination' );
          ActionCostMenu.Caption := tr( '&Cost accounting' );
          ActionCostOptions.Caption := tr( '&Cost accounting options' );
          ActionCostsDestr.Caption := tr( 'Production type settings for &destruction costs' );
          ActionCostsVacc.Caption := tr( 'Production type settings for &vaccination costs' );
          ActionCostsZones.Caption := tr( 'Production type settings for &zone surveillance costs' );
          ActionMap.Caption := tr( '&Map of units for 1 iteration' );
          ActionOutChart.Caption := tr( '&Daily unit status for 1 iteration' );
          ActionEvents.Caption := tr( '&Events by day' );
          ActionSummary.Caption := tr( '&Summary of 1 iteration' );
          ActionOutputStats.Caption := tr( 'Output statistics' );
          ActionEpiCurve.Caption := tr( 'Summary epidemic curves' );
          ActionRunUntilDay.Caption := tr( 'Start and run until specified day...' );
          ActionRunUntilDetection.Caption := tr( 'Start and run until first detection(s)' );
          ActionRunUntilDiseaseEnd.Caption := tr( 'Start and run to the end of active disease phase(s)' );
          ActionRunUntilOutbreakEnd.Caption := tr( 'Start and run until end of outbreak(s)' );
          ActionExposures.Caption := tr( 'Exposures and traces by day' );
          ActionCascade.Caption := tr( '&Cascade' );
          ActionArrange.Caption := tr( '&Arrange' );
          ActionStop.Caption := tr( 'S&top simulation' );
          ActionCloseWindows.Caption := tr( 'Close all &windows' );
          ActionNew.Caption := tr( '&New scenario file' );
          ActionSave.Caption := tr( '&Save scenario file' );
          ActionSaveAs.Caption := tr( 'Sa&ve As...' );
          ActionImport.Caption := tr( '&Import scenario...' );
          ActionClose.Caption := tr( '&Close scenario file' );
          ActionExport.Caption := tr( '&Export scenario...' );
          ActionOutputOptions.Caption := tr( 'Output options' );
          ActionClearOutput.Caption := tr( 'Clear scenario output' );
          ActionVerifyHerds.Caption := tr( 'Verify current units' );
          ActionVerifyScenario.Caption := tr( 'C&heck scenario validity' );
          acnEmptyScenario.Caption := tr( '&New empty scenario file' );
          acnSampleScenario.Caption := tr( '&Sample scenario file' );
          ActionWebsite.Caption := tr( 'Go to the NAADSM &website' );
          ActionWebsite.Hint := tr( 'Opens the NAADSM website in your default browser' );
          ActionCustomOutputMenu.Caption := tr( 'Custom outputs' );
          ActionCustomOutputOptions.Caption := tr( 'Custom output options' );
          ActionCustomOutputDefinitions.Caption := tr( 'Define custom outputs' );
          ActionZonesMenu.Caption := tr( 'Zones' );
          ActionZonesOptions.Caption := tr( 'Zone options' );
          ActionZonesDefinition.Caption := tr( 'Create/modify zones' );
          ActionZones.Caption := tr( 'Production type settings for zones' );
          ActionCompareStats.Caption := tr( 'Com&pare to another scenario...' );
          ActionOutZoneChart.Caption := tr( 'Da&ily zone status for 1 iteration' );
          acnLanguageSettings.Caption := tr( '&Language settings...' );
          acnRegionalSettings.Caption := tr( '&Regional settings...' );
          OpenDialog1.Filter := tr( 'NAADSM scenario databases (*.mdb)|*.mdb|All files (*.*)|*.*' );
          SaveDialog1.Filter := tr( 'NAADSM scenario databases (*.mdb)|*.mdb|All files (*.*)|*.*' );
        end
      ;

      // Set action Caption properties
      with self do
        begin
          //acnEmptyScenario.Caption := tr( '&New empty scenario file' );
          //acnSampleScenario.Caption := tr( '&Sample scenario file' );
          //ActionExit.Caption := tr( 'E&xit' );
          //ActionTracingMenu.Caption := tr( 'Trac&ing' );
          ActionZonesOptions.Caption := tr( '&Zone options' );
          ActionZonesDefinition.Caption := tr( '&Create/modify zones' );
          ActionZones.Caption := tr( '&Production type settings for zones' );
          ActionZonesMenu.Caption := tr( '&Zones' );
          ActionOutputOptions.Caption := tr( '&Output options' );
          ActionCustomOutputOptions.Caption := tr( '&Custom output options' );
          ActionCustomOutputDefinitions.Caption := tr( '&Define custom outputs' );
          ActionCustomOutputMenu.Caption := tr( 'C&ustom outputs' );
          ActionClearOutput.Caption := tr( '&Clear scenario output' );
          ActionRunUntilDay.Caption := tr( '&Start and run until specified day...' );
          ActionRunUntilDetection.Caption := tr( 'St&art and run until first detection(s)' );
          ActionRunUntilDiseaseEnd.Caption := tr( 'Start a&nd run to the end of active disease phase(s)' );
          ActionRunUntilOutbreakEnd.Caption := tr( 'Sta&rt and run until end of outbreak(s)' );
          //ActionStop.Caption := tr( 'S&top simulation' );
          //ActionMap.Caption := tr( '&Map of units for 1 iteration' );
          //ActionOutChart.Caption := tr( '&Daily unit status for 1 iteration' );
          //ActionOutZoneChart.Caption := tr( 'Da&ily zone status for 1 iteration' );
          //ActionSummary.Caption := tr( '&Summary of 1 iteration' );
          ActionExposures.Caption := tr( 'E&xposures and traces by day' );
          ActionOutputStats.Caption := tr( 'O&utput statistics' );
          ActionEpiCurve.Caption := tr( 'Summa&ry epidemic curves' );
        end
      ;

      translateUIManual();
    end
  ;


  procedure TFormMain.translateUIManual();
    begin
      // Manually added
      with self do
        begin
          ActionManager1.ActionBars[0].items[0].caption := tr( '&File' );
          ActionManager1.ActionBars[0].items[1].caption := tr( '&Scenario parameters' );
          ActionManager1.ActionBars[0].items[2].caption := tr( '&Run' );
          ActionManager1.ActionBars[0].items[3].caption := tr( '&Output windows' );
          ActionManager1.ActionBars[0].items[4].caption := tr( '&Tools' );
          ActionManager1.ActionBars[0].items[5].caption := tr( '&Help' );
        end
      ;
    end
  ;


 	procedure TFormMain.FormCreate(Sender: TObject);
 		begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if( Screen.PixelsPerInch <> 96 ) then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
 		end
	;


  procedure TFormMain.handleAppUpdateMessage( success: boolean; updateMessage: string );
    var
      frmAppUpdate: TFormAppUpdate;
      splashScreen: TForm;
    begin
      if( success ) then
        begin
          if( 0 < length( updateMessage ) ) then
            begin
              if( IS_EXPERIMENTAL ) then
                splashScreen := frmSplashExperimental
              else
                splashScreen := frmSplash
              ;
              frmAppUpdate := TFormAppUpdate.create( self, updateMessage, MAJORVERSIONNUMBER, MINORVERSIONNUMBER, splashScreen );

              // The form shows itself...
              frmAppUpdate.Release();
            end
          ;
        end
      ;
    end
  ;


  procedure TFormMain.initialize();
    begin
      _selectedProdTypeIndex := 0;
      _selectedProdTypePairIndex := 0;
      _selectedZoneIndex := 0;
      
      _displayedIteration := -1;
      _iterationInProgress := -1;
      _simIsRunning := false;

      // These will be used by other output windows
      setLength( _perimeterColors, 4 );
      _perimeterColors[0] := clMaroon;
      _perimeterColors[1] := clTeal;
      _perimeterColors[2] := clNavy;
      _perimeterColors[3] := clOlive;
    end
  ;


  destructor TFormMain.destroy();
    begin
      freeAndNil( _ini );
      freeAndNil( _paramFormList );
      freeAndNil( _i88nSettings );

      if( _smdb <> nil ) then freeAndNil( _smdb );

      inherited destroy();
    end
  ;


  procedure TFormMain.setCommandParams( cmdParams: TCmdLine );
    var
      dbParam: string;
      dbFileName: string;
    begin
      if( cmdParams.hasSwitch( '--database' ) and cmdParams.hasSwitch( '-d' ) ) then
        begin
          msgOK(
            tr( 'Ambiguous "--database" switch repetition.  Switch will be ignored.' ),
            tr( 'Bad command line switch' ),
            IMGWarning,
            self
          );
          exit;
        end
      ;

      if( cmdParams.hasSwitch( '--database' ) ) then
        dbParam := '--database'
      else if( cmdParams.hasSwitch( '-d' ) ) then
        dbParam := '-d'
      else // there is no switch.
        exit
      ;

      if(  1 <> cmdParams.getArgumentCount( dbParam ) ) then
        begin
          msgOK(
            ansiReplaceStr( tr( 'Switch "xyz"' ), 'xyz', dbParam ) + ' ' + tr( 'has the wrong number of arguments.  Switch will be ignored.' ),
            tr( 'Bad command line switch' ),
            IMGWarning,
            self
          );
          exit;
        end
      ;

      dbFileName := cmdParams.getArgument( dbParam, 0 );

      openScenario( dbFileName );
    end
  ;


  procedure TFormMain.showMap();
    begin
      if( nil <> _smdb )  then
        begin
          if( not( Assigned( frmMap ) ) ) then
            frmMapToggle()
          ;
        end
      ;
    end
  ;


  function TFormMain.getsmDB():TSMDatabase;
    begin
      result := _smdb;
    end
  ;


  function TFormMain.getScenario(): TSMScenario;
    begin
      result := _smScenario;
    end
  ;

  
  procedure TFormMain.setInitialWindowPosition();
    const
      MINWINDOWWIDTH: integer = 700;
      MINWINDOWHEIGHT: integer = 450;
    var
      maxWindowWidth, maxWindowHeight: integer;
      prefWindowWidth, prefWindowHeight: integer;
      prefWindowLeft, prefWindowTop: integer;

      iniKeyFound: boolean;
    begin
      // If the user wants the window to be
      // maximized, just do it and exit.
      //----------------------------------
      if( _ini.HasKey( 'MainWindowState' ) ) then
        begin
          if( 'maximized' <> fixup( _ini.val( 'MainWindowState' ) ) ) then
            begin
              self.windowState := wsNormal;
            end
          else
            begin
              self.WindowState := wsMaximized;
              exit;
            end
          ;
        end
      else
        begin
          self.WindowState := wsMaximized;
          exit;
        end
      ;


      // Otherwise, check for ini key settings.
      //--------------------------------------------
      iniKeyFound := false;

      maxWindowWidth := screen.WorkAreaWidth;
      maxWindowHeight := screen.WorkAreaHeight;

      prefWindowWidth := 0;
      prefWindowHeight := 0;
      prefWindowLeft := 0;
      prefWindowTop := 0;

      if( _ini.HasKey( 'MainWindowTop' ) ) then
        begin
          prefWindowTop := integer( _ini.val('MainWindowTop') );
          iniKeyFound := true;
        end
      ;

      if( _ini.HasKey( 'MainWindowLeft' ) ) then
        begin
          prefWindowLeft := _ini.val('MainWindowLeft');
          iniKeyFound := true;
        end
      ;

      if( _ini.HasKey( 'MainWindowHeight' ) ) then
        begin
          prefWindowHeight := _ini.val('MainWindowHeight');
          iniKeyFound := true;
        end
      ;

      if( _ini.HasKey( 'MainWindowWidth' ) ) then
        begin
          prefWindowWidth := _ini.val('MainWindowWidth');
          iniKeyFound := true;
        end
      ;


      // If none of the ini keys were found,
      // center the main window and exit.
      //-------------------------------------
      if( not iniKeyFound ) then
        begin
          prefWindowWidth := 2 * maxWindowWidth div 3;
          prefWindowHeight := 2 * maxWindowHeight div 3;

          if( MINWINDOWWIDTH > prefWindowWidth ) then prefWindowWidth := MINWINDOWWIDTH;
          if( MINWINDOWHEIGHT > prefWindowHeight ) then prefWindowHeight := MINWINDOWHEIGHT;

          self.Left := (maxWindowWidth - prefWindowWidth) div 2;
          self.top := (maxWindowHeight - prefWindowHeight) div 2;

          self.Width := prefWindowWidth;
          self.Height := prefWindowHeight;
          
          exit;
        end
      ;


      // Otherwise, the user is more discriminating.
      // Make the appropriate adjustments.
      //---------------------------------------------
      if( 0 > prefWindowLeft ) then prefWindowLeft := 0;
      if( 0 > prefWindowTop ) then prefWindowTop := 0;

      if( MINWINDOWWIDTH > prefWindowWidth ) then prefWindowWidth := MINWINDOWWIDTH;
      if( MINWINDOWHEIGHT > prefWindowHeight ) then prefWindowHeight := MINWINDOWHEIGHT;

      if( maxWindowWidth < prefWindowWidth ) then
        begin
          prefWindowWidth := maxWindowWidth;
          prefWindowLeft := 0;
        end
      ;

      if( maxWindowHeight < prefWindowHeight ) then
        begin
          prefWindowHeight := maxWindowHeight;
          prefWindowTop := 0;
        end
      ;

      if( maxWindowWidth < (prefWindowLeft + prefWindowWidth) ) then
        prefWindowLeft := (maxWindowWidth - prefWindowWidth) div 2
      ;

      if( maxWindowHeight < (prefWindowTop + prefWindowHeight) ) then
        prefWindowTop := (maxWindowHeight - prefWindowHeight) div 2
      ;

      self.Top := prefWindowTop;
      self.Left := prefWindowLeft;
      self.Height := prefWindowHeight;
      self.Width := prefWindowWidth;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
// File menu actions and related functions
// ----------------------------------------------------------------------------
  procedure TFormMain.ActionNewExecute(Sender: TObject);
    begin
      // This procedure must be defined for the menu item to be active.
    end
  ;

  procedure TFormMain.acnSampleScenarioExecute(Sender: TObject);
    var
      msg: string;
      errCode: integer;
      fileName: string;
    begin
      // This check shouldn't really be necessary: file->import
      // should be disabled whenever a database is open.
      if( _smdb <> nil ) then
        begin
          msgOK(
            tr( 'Please close the current scenario file first.' ),
            tr( 'File is open' ),
            IMGWarning,
            self
          );
          exit;
        end
      ;

    	SaveDialog1.Title := tr( 'Create sample model scenario database with the file name...' );

      if( _ini.hasKey( 'LastSaveDirectory' ) ) then
      	SaveDialog1.InitialDir := _ini.val( 'LastSaveDirectory' )
      ;

      SaveDialog1.FileName := tr( 'SampleScenario.mdb' );

      if( SaveDialog1.Execute() ) then
      	begin
      		fileName := SaveDialog1.FileName;

          if( ansiLowerCase( rightStr( fileName, 4 ) ) <> '.mdb' ) then
            fileName := fileName + '.mdb'
          ;

          _ini.update( 'LastSaveDirectory', directory( fileName ) );

          if( TSMDatabase.makeSampleDatabase( fileName, @errCode, @msg ) ) then
            begin
              msgOK(
                tr( 'A sample scenario file has been created.  Please note that this is an example only, and that the parameters in this file do not represent any actual disease or situation.' ),
                tr( 'Sample scenario created' ),
                IMGInformation,
                self
              );
              openScenario( fileName );
            end
          else
            msgOK(
              tr( 'The sample scenario could not be created.' )
                + '  ' + tr( 'Please check your permissions on the selected folder as well as available disk space.' ),
              tr( 'Scenario creation failed' ),
              IMGCritical,
              self
            )
          ;
        end
      ;
    end
  ;


  procedure TFormMain.acnEmptyScenarioExecute(Sender: TObject);
    var
      fileName: string;
      errMsg: string;
    begin
      // FIX ME: check to see if a database is currently open

      try
        try
          Screen.Cursor := crHourGlass;
          fileName := getUntitledMDB( currentDir() );
          _ini.update( 'LastOpenDirectory', currentDir() );
          _smdb := TSMDatabase.create( fileName, DBCreate, PChar( errMsg ), self );

          if( createSimObjects() ) then
            begin
              Screen.Cursor := crDefault;
              msgOK(
                tr( 'An empty scenario file has been created. Use the "Scenario parameters" menu to set up the scenario.' ),
                tr( 'Scenario file created' ),
                IMGSuccess,
                self )
              ;
            end
          ;

        except
          on e: exception do
            begin
              freeAndNil( _smdb );

              msgExceptionOK(
                tr( 'NAADSM is unable to create a new scenario file. You may need to check your available hard disk space.' )
                  + endl + endl,
                e, self )
              ;
            end
          ;
        end;
      finally
      	updateDisplay();
        Screen.Cursor := crDefault;
      end;

    end
  ;


  procedure TFormMain.ActionSaveExecute(Sender: TObject);
    var
      success: boolean;
    begin
    	if( not( _smdb.dbSaved ) ) then
      	actionSaveAsExecute( sender )
      else
        begin
          screen.cursor := crHourGlass;
          success := _smdb.save();
          screen.Cursor := crDefault;

          if( success ) then
            _saveCanceled := false
          else
            begin
              msgOK(
                tr( 'The current scenario file cannot be saved.' ) + ' '
                  + tr( 'You may need to check your available hard disk space, or close any scenario files that are open in external applications.' ),
                tr( 'Cannot save scenario file' ),
                IMGWarning,
                Self
              );
            end
          ;

          updateCaption();
        end
      ;
    end
  ;


  procedure TFormMain.ActionSaveAsExecute(Sender: TObject);
  	var
    	saveFileName: string;
    begin
    	if( getSaveScenarioName( saveFileName ) ) then
      	begin
          screen.cursor := crHourGlass;

          if( not( _smdb.save( saveFileName ) ) ) then
            begin
              msgOK(
                tr( 'The current scenario file cannot be saved.' ) + ' '
                  + tr( 'You may need to check your available hard disk space, or close any scenario files that are open in external applications.' ),
                tr( 'Cannot save scenario file' ),
                IMGWarning,
                Self
              );
            end
          else
            _saveCanceled := false
          ;

          screen.Cursor := crDefault;
          updateDisplay();
        end
      else
      	_saveCanceled := true
      ;
    end
  ;


  function TFormMain.getSaveScenarioName( var fileName: string ): boolean;
    begin
    	SaveDialog1.Title := tr( 'Save model scenario database as...' );

      if( _ini.hasKey( 'LastSaveDirectory' ) ) then
      	SaveDialog1.InitialDir := _ini.val( 'LastSaveDirectory' )
      ;

      SaveDialog1.FileName := _smdb.permanentDBFileName;

      if( SaveDialog1.Execute() ) then
      	begin
      		fileName := SaveDialog1.FileName;

          if( ansiLowerCase( rightStr( fileName, 4 ) ) <> '.mdb' ) then
            fileName := fileName + '.mdb'
          ;

          _ini.update( 'LastSaveDirectory', directory( SaveDialog1.FileName ) );
          result := true;
        end
      else
      	result := false
      ;
    end
  ;


  function TFormMain.selectScenarioFile( var fileName: string ): boolean;
    begin
      OpenDialog1.Title := tr( 'Open a model scenario database' );

      if( _ini.hasKey( 'LastOpenDirectory' ) ) then
        OpenDialog1.initialDir := _ini.val( 'LastOpenDirectory' )
      ;

      if OpenDialog1.Execute() then
        begin
          fileName := OpenDialog1.FileName;
          _ini.update( 'LastOpenDirectory', directory( OpenDialog1.FileName ) );
          result := true;
        end
      else
        result := false
      ;
    end
  ;


  function TFormMain.getUntitledMDB( path: string ): string;
    var
      i: integer;
    begin
      if( not( fileExists( path + 'Untitled.mdb' ) ) ) then
        result := path + 'Untitled.mdb'
      else
        begin
          i := 1;

          while( fileExists( path + 'Untitled' + intToStr( i ) + '.mdb' ) ) do
            inc( i )
          ;

          result := path + 'Untitled' + intToStr( i ) + '.mdb';
        end
      ;
    end
  ;


  procedure TFormMain.ActionExitExecute(Sender: TObject);
    begin
      Close();
    end
  ;


  procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
    	if( _smdb <> nil ) then
      	begin
      		if( _smdb.workingDBHasChanged ) then
          	ActionCloseExecute( nil )
					else
          	begin
              closeOutputWindows();
          		closeDB();
            end
          ;
        end
      ;

      if ( true = _saveCanceled ) then Action := caNone;
      updateIniSettings();
			// Bye bye!
    end
  ;


  procedure TFormMain.ActionImportExecute( Sender: TObject );
    var
    	frm: TFormImport;
    begin

    	// This check shouldn't really be necessary: file->import should be disabled
      // whenever a database is open.
      if( _smdb <> nil ) then
      	begin
        	msgOK(
          	tr( 'Please close the current scenario file first.' ),
            tr( 'File is open' ),
            IMGWarning,
            self
          );
        	exit;
        end
      ;

    	frm := TFormImport.create( self, _ini );
      frm.showModal();

      _smdb := frm.database;
      _smScenario := frm.scenario;

      // If problems occurred during import then frm.database will have been set to nil.
      if( nil <> _smdb ) then
        begin
          updateDisplay();
          if( not( assigned( frmMap ) ) ) then
            frmMapToggle()
          ;
        end
      ;

      // There is no need here for any dialog boxes.
      // Dialogs for both success and failure are handled on the form itself.

      frm.Release();
    end
  ;


  procedure TFormMain.updateIniSettings();
    begin
      _ini.update( 'MainWindowTop', self.top );
      _ini.update( 'MainWindowLeft', self.left );
      _ini.update( 'MainWindowHeight', self.height );
      _ini.update( 'MainWindowWidth', self.width );

      if( self.WindowState = wsMaximized ) then
        _ini.update( 'MainWindowState', 'Maximized' )
      else
        _ini.update( 'MainWindowState', 'Normal' )
      ;

      _ini.writeFile();
    end
  ;


  procedure TFormMain.ActionExportExecute( Sender: TObject );
  	var
      frm: TFormExport;
    begin
    	dbcout( 'Creating form', DBSHOWMSG );
      frm := TFormExport.create( self, _smScenario, _ini );
      dbcout( 'Showing form', DBSHOWMSG );
			frm.showModal();
      dbcout( 'Freeing form', DBSHOWMSG );
      frm.Release();
    end
  ;

  
	procedure TFormMain.ActionCloseExecute( Sender: TObject );
  	var
    	response: integer;
		begin
      dbcout( 'Closing db', DBSHOWMSG );

			if( _smdb = nil ) then
      	exit
      else
      	begin
          _saveCanceled := false;

          if( _smdb.workingDBHasChanged ) then // New file that hasn't been permanently saved or existing permanent file needs to be updated
            begin
              response := msgYesNoCancel(
                tr( 'The current scenario file has changed. Save before closing?' ),
                tr( 'Save current file?' ),
                IMGQuestion,
                self
              );

              case response of
                mrYes: actionSaveExecute( nil );
                mrNo: _saveCanceled := false;
                mrCancel:
                	begin
                 		_saveCanceled := true;
                  	exit;
                  end
                ;
              end;

            end
          ;

          if( false = _saveCanceled ) then
            closeScenarioFile()
          ;
        end
      ;

      dbcout( 'Done closing db', DBSHOWMSG );
		end
	;


  procedure TFormMain.closeScenarioFile();
    begin
      closeDB();

      dbcout( 'Destroying _smScenario', DBSHOWMSG );
      try
        _smScenario.Free();
      except
        {$IFDEF DEBUG}
          msgOK( 'Can''t destroy _smScenario' );
        {$ENDIF}
      end;
      dbcout( 'Done destroying _smScenario', DBSHOWMSG );

      _smScenario := nil;
      dbcout( 'Closing output windows', DBSHOWMSG );
      closeOutputWindows();
      dbcout( 'Done closing output windows', DBSHOWMSG );
      dbcout( 'Updating display', DBSHOWMSG );
      updateDisplay();
      dbcout( 'Done updating display', DBSHOWMSG );
    end
  ;


  procedure TFormMain.closeDB();
  	begin
      _smdb.close();
      // Freeing TSMDatabase will automatically delete the temporary file.
      freeAndNil( _smdb );
      freeAndNil( _smScenario );
      initialize();
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Data handling functions
// ----------------------------------------------------------------------------
  procedure TFormMain.checkForIncompleteIteration();
    var
      lastCompleteIt: integer;                  
      currentIt: integer;
      msg: string;
    begin
      if( _smdb.containsIncompleteIterations( @lastCompleteIt, @currentIt ) ) then
        begin
          if( 0 < lastCompleteIt ) then
            begin
              msg :=
                ansiReplaceStr( tr( 'This file contains output for an incomplete iteration, as well as xyz complete iteration(s).' ), 'xyz', intToStr( lastCompleteIt ) )
                + ' ' + tr( 'It is recommended that you delete the incomplete data, and use a complete iteration to generate any single-iteration outputs.' )
                + endl + endl
                + tr( 'Delete partial iteration?' )
              ;
            end
          else
            msg :=
              tr( 'This file contains partial output for only one incomplete iteration.' )
              + ' ' + tr( 'It is recommended that you delete this partial output, and run the scenario again to generate any single-iteration outputs.' )
              + endl + endl
              + tr( 'Delete partial iteration?' )
          ;

          if( mrYes = msgYesNo( msg, tr( 'Incomplete iteration detected' ), IMGWarning, self ) ) then
            _smdb.deleteIncompleteIteration()
          ;
        end
      ;
    end
  ;

  function TFormMain.createSimObjects(): boolean;
    var
      frm: TFormProgress;
      fn: TObjFnBool1Int;
      sim: TSMSimulationInput;
      hList: THerdList;
      mathFunctionsOK: boolean;
    begin
      dbcout( '--- TFormMain.createSimObjects', DBSHOWMSG );
      
      screen.Cursor := crHourGlass;
      updateMenuItems( true );
      frm := TFormProgress.create( self, PRDoubleBar, true );
      fn := frm.setPrimary;
      frm.show();

      try
        try
          frm.setPrimary( 0 );
          frm.setSecondary( 0 );
          frm.setMessage( tr( 'Loading production types...' ) );

          if( nil <> _smScenario ) then freeAndNil( _smScenario );

          dbcout( 'Creating sim', DBSHOWMSG );
          sim := TSMSimulationInput.create( _smdb, fn, @mathFunctionsOK );
          dbcout( 'Done creating sim', DBSHOWMSG );

          frm.setSecondary( 50 );
          frm.setPrimary( 0 );
          frm.setMessage( tr( 'Loading units...' ) );

          dbcout( 'Creating hList', DBSHOWMSG );
          hList := THerdList.create( _smdb, sim, fn );
          dbcout( 'Done creating hList', DBSHOWMSG );

          dbcout( 'HERD LIST IS UPDATED: ' + uiBoolToText( hList.updated ), DBSHOWMSG );

          _smScenario := TSMScenario.create( sim, hList );

          frm.setPrimary( 100 );
          frm.setSecondary( 100 );

          if( not( mathFunctionsOK ) ) then
            begin
              msgOK(
                tr( 'An error occurred while loading at least one relational or probability density function.' ) + ' '
                  + tr( 'This may have occurred if the scenario database was changed with an application other than NAADSM.' ) + ' '
                  + tr( 'NAADSM will attempt to automatically correct this situation, but some problems may still exist.' ) + ' '
                  + tr( 'Please double-check your parameters before running this scenario.' ),
                tr( 'Database error' ),
                IMGWarning,
                self
              );
              _smdb.workingDBHasChanged := true;
            end
          ;

          result := true;
        except
          on e: exception do
            begin
              frm.Close();
              
              msgOK(
                tr( 'An application error occurred, and the selected scenario file could not be opened.' ) + ' '
                  + tr( 'Please check with the developers.' )
                  + endl + endl
                  + tr( 'Technical information:' ) + ' ' + e.Message,
                tr( 'Application error' ),
                IMGCritical,
                self
              );

              freeAndNil( _smScenario );
              _smdb.close();
              freeAndNil( _smdb );

              result := false;
            end
          ;
        end;

      finally
        frm.Close();
        frm.Release();
        self.SetFocus();
        screen.Cursor := crDefault;
        updateMenuItems( false );
      end;

    end
  ;


  procedure TFormMain.openScenario( const fileName: string );
    var
      updateReason: TDBSchemaUpdateReason;
      dbCheckResult: TDBCheckResult;

      response: integer;
      dbCheck: boolean;
      errMsg: string;
      msg: string;
    begin
      dbcout( 'TFormMain.openScenario', DBSHOWMSG );

      // Before doing anything else, see if the file exists.
      //----------------------------------------------------
      if( not( fileExists( fileName ) ) ) then
        begin
          msgOK(
            ansiReplaceStr( tr( 'The selected scenario file xyz does not exist.' ), 'xyz', abbrevPath( fileName ) )
              + endl + endl + tr( 'Please select another scenario file.' ),
            tr( 'Cannot open scenario file' ),
            IMGCritical,
            self
          );
          exit;
        end
      ;

      try
        try
          updateReason := DBUpdateUnspecified;

          Screen.Cursor:= crHourGlass;
          _smdb := TSMDatabase.create( fileName, DBOpen, nil, self );
          Screen.Cursor := crDefault;

          // See if the file opened at all.
          //-------------------------------
          if( not( _smdb.isOpen ) ) then
            begin
              msgOK(
                ansiReplaceStr( tr( 'The selected scenario file xyz cannot be opened.' ), 'xyz', abbrevPath( fileName ) )
                  + endl + endl + tr( 'Please double-check the file format.' ),
                tr( 'Cannot open scenario file' ),
                IMGCritical,
                self
              );

              deleteFile( _smdb.workingDBFileName );
              freeAndNil( _smdb );
              exit;
            end
          ;

          // Determine if the file is read-only.
          //------------------------------------
          if( _smdb.isReadOnly ) then
            begin
              response := msgYesNo(
                tr( 'This scenario file is read-only.  NAADSM requires write access if you wish to modify or run this scenario.' )
                  + endl + endl + tr( 'Continue?' ),
                tr( 'Read-only file' ),
                IMGWarning,
                Self )
              ;

              if( mrYes <> response ) then
                begin
                  // Delete the copy
                  _smdb.close();
                  deleteFile( _smdb.workingDBFileName );
                  freeAndNil( _smdb );
                  exit;
                end
              ;
            end
          ;

          // Check the version.
          //-------------------
          dbCheckResult := _smdb.checkVersion( updateReason );

          case dbCheckResult of
            DBVersionCurrent, DBVersionUpdated:
              begin
                // Make sure that the herd table is properly specified.
                //-----------------------------------------------------
                dbCheck := _smdb.tablesOK( @errMsg );

                if( not( dbCheck ) ) then
                  begin
                    msgOK(
                      tr( 'The contents of this scenario database appear to have been altered by some mechanism outside of NAADSM, and the scenario cannot be opened.' )
                        + '  ' + tr( 'Please contact the developers for more information.' )
                        + endl + endl
                        + tr( 'Technical information:' ) + endl
                        + errMsg,
                      tr( 'Altered database schema detected' ),
                      IMGCritical,
                      Self )
                    ;

                    // Delete the copy
                    _smdb.close();
                    deleteFile( _smdb.workingDBFileName );
                    freeAndNil( _smdb );
                    exit;
                  end
                ;

                // Make sure that the database has all required indices.
                //------------------------------------------------------
                // Right now, there is only one required index, but some day, there may be more.
                dbCheck := _smdb.indexExists( 'dynHerd_PK', 'dynHerd' );

                if( not( dbCheck ) ) then
                  begin
                    response := msgYesNo(
                      tr( 'The schema of this scenario database has been altered.  Some output will not be recorded correctly, and some features may not be available or will not work properly. Unless you are an expert user, it is suggested that you do not continue.' )
                        + endl + endl
                        + tr( '(Technical information: index "dynHerd_PK" does not exist on table "dynHerd".)' )
                        + endl + endl
                        + tr( 'Continue opening the current file?' ),
                      tr( 'Altered database schema detected' ),
                      IMGWarning,
                      Self )
                    ;

                    if( mrYes <> response ) then
                      begin
                        // Delete the copy
                        _smdb.close();
                        deleteFile( _smdb.workingDBFileName );
                        freeAndNil( _smdb );
                        exit;
                      end
                    ;
                  end
                ;

                // Update the database as necessary
                //---------------------------------
                if( processUpdates( updateReason ) ) then
                  begin
                    if( createSimObjects() ) then
                      begin
                        if( not( _smdb.containsValidOutput ) ) then
                          begin
                            msg := tr( 'This file contains apparently invalid output.  The number of iteration records does not match the number of iterations run.' )
                              + ' ' + tr( 'If you proceed, output will be deleted, and the scenario will need to be run again.' )
                              + ' ' + tr( 'Continue?' )
                            ;

                            if( mrYes = msgYesNo( msg, tr( 'Invalid output detected' ), IMGWarning, self ) ) then
                              clearScenarioOutput()
                            else
                              begin
                                // Delete the copy
                                _smdb.close();
                                deleteFile( _smdb.workingDBFileName );
                                freeAndNil( _smdb );
                                freeAndNil( _smScenario );
                                exit;
                              end
                            ;
                          end
                        else
                          checkForIncompleteIteration()
                        ;


                        if( not( Assigned( frmMap ) ) ) then frmMapToggle();
                      end
                    ;
                  end
                ;
              end
            ;
            DBVersionObsolete:
              begin
                // Delete the copy
                _smdb.close();
                deleteFile( _smdb.workingDBFileName );
                freeAndNil( _smdb );

                msgOK(
                  tr( 'The format of the selected scenario file is out of date, and conversion to the new format requires a separate conversion utility.' )
                    + '  ' + tr( 'For further assistance, please contact the programmers.' ),
                  tr( 'Obsolete scenario file' ),
                  IMGWarning,
                  Self )
                ;
              end
            ;
            DBVersionUnrecognized:
              begin
                // Delete the copy
                _smdb.close();
                deleteFile( _smdb.workingDBFileName );
                freeAndNil( _smdb );

                msgOK(
                  tr( 'This file does not appear to be a valid NAADSM scenario file or may have been created with a newer version of NAADSM.' )
                    + ' ' + tr( 'Please select another file.' ),
                  tr( 'Unrecognized scenario file' ),
                  IMGWarning,
                  self )
                ;
              end
            ;
          end;

        except
          on e: exception do
            begin
              freeAndNil( _smdb );
              freeAndNil( _smScenario );

              msgExceptionOK(
                tr( 'NAADSM is unable to open the selected scenario file. You may need to check your available hard disk space.' )
                  + endl + endl,
                e, self
              );
            end
          ;
        end;
      finally
        updateDisplay();
        screen.Cursor := crDefault;
      end;
    end
  ;


  function TFormMain.processUpdates( schemaUpdateReason: TDBSchemaUpdateReason ): boolean;
    var
      response: integer;
      oldVersion: string;
      clearOutputs: boolean;
      proceedWithUpdate: boolean;
      updateMsg: string;
      schemaUpdated: boolean;
      thisVersion: string;
      updateSuccess: boolean;
      vUpdateReason: TVersionUpdateReason;
    begin
      clearOutputs := false;
      proceedWithUpdate := false;

      thisVersion := MAJORVERSIONNUMBER + '.' +  MINORVERSIONNUMBER;

      if( 0 < length( BRANCHNAME ) ) then
        thisVersion := thisVersion + '-' + BRANCHNAME
      ;

      // First, check the version of the application that
      // created any outputs stored in the database.
      //--------------------------------------------------
      vUpdateReason := _smdb.versionUpdateReason( @oldVersion );
      case vUpdateReason of
        VERSBug:
          begin
            response := msgYesNo(
              ansiReplaceStr( tr( 'This file contains output generated by a previous version (xyz) of NAADSM.' ), 'xyz', oldVersion )
                + endl + endl
                + ansiReplaceStr( tr( 'This version (xyz) corrects calculation errors that existed in the previous version.' ), 'xyz', thisVersion )
                + endl + endl
                + ansiReplaceStr( tr( 'Please visit the NAADSM website (xyz) for information about these errors to determine how they might affect your work.' ), '(xyz)', endl + '(' + WEBSITE + ')' + endl )
                + endl + endl
                + tr( 'Would you like to clear existing output in order to rerun this scenario now?' ),
              tr( 'Updated application version' ),
              IMGCritical,
              self
            );

            proceedWithUpdate := ( response = mrYes );
            clearOutputs := proceedWithUpdate;
          end
        ;
        VERSModelSpecChange:
          begin
            response := msgYesNo(
              ansiReplaceStr( tr( 'This file contains output generated by a previous version (xyz) of NAADSM.' ), 'xyz', oldVersion )
                + endl + endl
                + ansiReplaceStr( tr( 'This version (xyz) implements a new, incompatible version of the conceptual disease spread model.' ), 'xyz', thisVersion )
                + endl + endl
                + ansiReplaceStr( tr( 'Please visit the NAADSM website (xyz) for information about this new version.' ), '(xyz)', endl + '(' + WEBSITE + ')' + endl )
                + endl + endl
                + tr( 'Would you like to clear existing output and convert this scenario to the new version now?' ),
              tr( 'New conceptual model' ),
              IMGCritical,
              self
            );

            proceedWithUpdate := ( response = mrYes );
            clearOutputs := proceedWithUpdate;
          end
        ;
        VERSUnrecognized:
          begin
            response := msgYesNo(
              tr( 'This file contains output generated by an experimental, updated, unrecognized, or unsupported application.' )
              + endl + endl
              + tr( 'Would you like to clear existing output in order to rerun this scenario now?' ),
              tr( 'Unrecognized application version' ),
              IMGCritical,
              self
            );

            proceedWithUpdate := ( response = mrYes );
            clearOutputs := proceedWithUpdate;
          end
        ;
        VERSOK: // If the app version is OK, check the version of the database
          begin
            case schemaUpdateReason of
              DBUpdateOK:
                begin
                  proceedWithUpdate := true;
                  clearOutputs := false;
                end
              ;
              DBUpdateSpecChange:
                begin
                  response := msgYesNo(
                    tr( 'The format of the selected scenario file needs to be updated for use with this version of NAADSM.' )
                      + endl + endl
                      + tr( 'The output stored in this scenario will be deleted.' )
                      + endl + endl
                      + tr( 'Continue?' ),
                    tr( 'Scenario file is out of date' ),
                    IMGCritical,
                    self
                  );

                  proceedWithUpdate := ( response = mrYes );
                  clearOutputs := true;
                end
              ;
              DBUpdateMinorChanges:
                begin
                  response := msgYesNo(
                    tr( 'The format of the selected scenario file needs to be updated for use with this version of NAADSM.' )
                      + endl + endl
                      + tr( 'The information stored in this scenario will not be altered.' )
                      + endl + endl
                      + tr( 'Continue?' ),
                    tr( 'Scenario file is out of date' ),
                    IMGQuestion,
                    self
                  );

                  proceedWithUpdate := ( response = mrYes );
                  clearOutputs := false;
                end
              ;
              DBUpdateOutputsAdded:
                begin
                  response := msgYesNo(
                    tr( 'The format of the selected scenario file needs to be updated for use with this version of NAADSM.' )
                      + endl + endl
                      + tr( 'Some outputs reported by this version were not recorded in the file, but those that were are still valid.' )
                      + endl + endl
                      + tr( 'The scenario must be run again if you want to generate the new outputs.' )
                      + ' ' + ansiReplaceStr( tr( 'Please visit the NAADSM website (xyz) for information about these changes.' ), '(xyz)', (*endl +*) '(' + WEBSITE + ')' (*+ endl*) )
                      + endl + endl
                      + tr( 'If you continue now, existing outputs will be unchanged.  Continue?' ),
                    tr( 'Scenario file is out of date' ),
                    IMGQuestion,
                    self
                  );

                  proceedWithUpdate := ( response = mrYes );
                  clearOutputs := false;
                end
              ;
            end;

          end
        ;
      end;


      if( proceedWithUpdate ) then
        begin
          schemaUpdated := _smdb.updateSchema( updateSuccess );

          if( not( updateSuccess ) ) then
            begin
              msgOK(
                tr( 'An error occurred, and this database could not be updated (the original file is unchanged).' )
                  + '  ' + tr( 'This version of NAADSM cannot run this scenario.' ) + ' '
                  + endl + endl + tr( 'If the problem persists, please contact the developers.' ),
                tr( 'Scenario file update failed' ),
                IMGCritical,
                self
              );
              proceedWithUpdate := false;
              _smdb.close();
              deleteFile( _smdb.workingDBFileName );
              freeAndNil( _smdb );
            end
          else
            begin
              if( clearOutputs ) then
                begin
                  _smdb.initializeAllOutputRecords();
                  updateMsg := tr( 'The format of the selected scenario file has been updated, and output has been cleared.' );
                end
              else
                updateMsg := tr( 'The format of the selected scenario file has been updated.' )
              ;

              updateMsg := updateMsg
                + endl + endl
                + tr( 'If you wish to keep the old version, select File -> Save As... now to save the updated scenario file with a new name, or File -> Close to discard the changes.' )
              ;

              dbcout( 'Should show scenario file updated message', DBSHOWMSG );
              if( schemaUpdated ) then
                msgOK( updateMsg, tr( 'Scenario file updated' ), IMGSuccess, self )
              ;
            end
          ;
        end
      else
        begin
          _smdb.close();
          deleteFile( _smdb.workingDBFileName );
          freeAndNil( _smdb );
        end
      ;

      result := proceedWithUpdate;
    end
  ;


  procedure TFormMain.ActionOpenExecute(Sender: TObject);
    var
      fileName: string;
      response: integer;
    begin
      if( nil <> _smdb ) then
        begin
          if( _smdb.workingDBHasChanged ) then
            begin
              response := msgYesNoCancel(
                tr( 'The current scenario file has changed. Save before closing?' ),
                tr( 'Save current file?' ),
                IMGQuestion,
                self
              );

              case response of
                mrYes:
                  begin
                    ActionSaveExecute( sender );
                    closeScenarioFile();
                  end
                ;
                mrNo:
                  closeScenarioFile()
                ;
                mrCancel: exit;
              end;
            end
          else
            begin
              response := msgYesNo(
                tr( 'Close current scenario file?' ),
                tr( 'Close current file?' ),
                IMGQuestion,
                self
              );

              case response of
                mrYes: closeScenarioFile();
                mrNo: exit;
              end;
            end
          ;
        end
      ;

      if( selectScenarioFile( fileName ) ) then
        begin
          repaint();
          openScenario( fileName );
        end
      ;
    end
  ;


  procedure TFormMain.clearScenarioOutput();
    begin
      screen.Cursor := crHourglass;
      _smdb.initializeAllOutputRecords();
      _smScenario.herdList.prepareForIteration( 0 );

      _firstOverallDet := -1;
      _firstOverallDestr := -1;
      _firstOverallVacc := -1;

      _displayedIteration := -1;
      _iterationInProgress := -1;

      updateOutputWindows( true );
      updateDisplay();
      screen.Cursor := crDefault;
    end
  ;


  procedure TFormMain.ActionClearOutputExecute(Sender: TObject);
    var
      reply: integer;
      snippet: string;
      nIterations: integer;
    begin
      if( nil <> _smdb ) then
        begin
          if( _smdb.containsOutput ) then
            begin
              nIterations := _smdb.completedIterations();

              if( 0 = nIterations ) then
                begin
                  nIterations := 1;
                  snippet := tr( 'incomplete iteration' );
                end
              else if( 1 = nIterations ) then
                snippet := tr( 'complete iteration' )
              else
                snippet := tr( 'complete iterations' )
              ;

              reply := msgYesNo(
                ansiReplaceStr( ansiReplaceStr( tr( 'This scenario file contains results for xyz complete iterations.' ), 'xyz', intToStr( nIterations ) ), 'complete iterations', snippet )
                  + '  ' + tr ( 'Are you sure that you want to erase this data?' ),
                tr( 'Delete existing data?' ),
                IMGWarning,
                self
              );

              if( mrYes = reply ) then
                clearScenarioOutput()
              ;
            end
          ;
        end
      ;
    end
  ;

  procedure TFormMain.ActionVerifyScenarioExecute(Sender: TObject);
    var
      errMsg: string;
      dlg: TDialogLongMessage;
    begin
      if( nil = _smScenario ) then // this should never happen
        begin
          msgOK( 
            tr( 'Please open a scenario file first.' ), 
            tr( 'No scenario' ), 
            IMGInformation, 
            self 
          );
          exit;
        end
      ;

      Screen.Cursor := crHourGlass;

      if( _smScenario.simInput.isValid( false, _smScenario.herdList, @errMsg ) ) then
        begin
          if( _smScenario.herdList.isValid( _smScenario.simInput, @errMsg ) ) then
            begin
              Screen.Cursor := crDefault;

              msgOK(
                tr( 'No problems were detected in this scenario.' ),
                tr( 'Scenario OK' ),
                IMGSuccess,
                self
              );
            end
          else
            begin
              Screen.Cursor := crDefault;

              dlg := TDialogLongMessage.create(
                self,
                tr( 'Problems found' ),
                tr( 'Problems were found with some of the current units.  These problems must be corrected before this scenario can be run:' ),
                errMsg
              );
              dlg.showModal();
              dlg.Release();
            end
          ;
        end
      else
        begin
          Screen.Cursor := crDefault;

          dlg := TDialogLongMessage.create(
            self,
            tr( 'Problems found' ),
            tr( 'Problems were found with this scenario.  These problems must be corrected before this scenario can be run:' ),
            errMsg
          );
          dlg.showModal();
          dlg.Release();
        end
      ;
      Screen.Cursor := crDefault;

      updateCaption();
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Scenario parameters menu items and related functions
// ----------------------------------------------------------------------------
  {*
  The functions fillParamFormList and both versions of showParamForm are
  used for the series of "wizard" screens used to set scenario parameters.

  fillParamFormList is called by the constructor.  All forms that are used
  to set scenario parameters must be included in this list.  As the user
  navigates forward or backward through the wizard, the appropriate item in
  the list is displayed.  This function is very simple: just append the
  forms/classes needed to the list.

  The main form has no knowledge regarding whether a particular form needs
  to be shown: the main form simply calls the forms in sequence. It is up to
  each individual form to determine if it needs to be shown, based on
  parameters set previously.
  }
	procedure TFormMain.fillParamFormList();
  	begin
    	// Remember to make sure classes listed here are registered!
    	with _paramFormList do
      	begin
          // General parameters
      		append( 'TFormGeneralParams' );

          // Basic production type parameters
          append( 'TFormProdType' );

          // The herd population
          append( 'TFormHerdListEditor' );
          append( 'TFormInitialUnitOptions' );

          // Disease parameters
          append( 'TFormYesNoPrevalence' );
          append( 'TFormDisease' );
          
          // Spread parameters
					append( 'TFormSpreadOptions' );
          append( 'TFormProdTypePairs' );
          append( 'TFormContactSpread' );
          append( 'TFormAirborneSpread' );
           
          // Detection parameters
          append( 'TFormYesNoDetection' );
          append( 'TFormDetection' );

          // Tracing parameters
          append( 'TFormTracingGlobal' );
          append( 'TFormTracing' );
          append( 'TFormTracingHerdExam' );
          append( 'TFormTracingTesting' );

          // Zone parameters
          append( 'TFormYesNoZones' );
          append( 'TFormZoneCreation' );
          append( 'TFormZone' );

          // Destruction parameters
          append( 'TFormDestrGlobal' );
          append( 'TFormDestruction' );
          append( 'TFormDestrPriority' );
          
          // Vaccination parameters
          append( 'TFormVaccGlobal' );
          append( 'TFormVaccination' );
          append( 'TFormVaccPriority' );

          // Cost parameters
          append( 'TFormCostOptions' );
          append( 'TFormCostsZones' );
          append( 'TFormCostsDestr' );
          append( 'TFormCostsVacc' );

          // Output options
          append( 'TFormOutputOptions' );

          // Custom outputs
          append( 'TFormYesNoCustomOutputs' );
          append( 'TFormCustomOutputs' );
				end
      ;
    end
  ;

  procedure TFormMain.lockWindows();
    begin
      // Prevent any output maps from being re-drawn.
      // This eliminates a lot of annoying flicker between parameter input screens.
      _windowsLocked := true;

      if( assigned( frmMap ) ) then
        begin
          frmMap.borderDisabled := true;
          lockWindow( frmMap );
        end
      ;
      if( assigned( frmDailyStatusByProdType ) ) then
        begin
          frmDailyStatusByProdType.borderDisabled := true;
          lockWindow( frmDailyStatusByProdType );
        end
      ;
      if( assigned( frmDailyZoneStatusByProdType ) ) then
        begin
          frmDailyZoneStatusByProdType.borderDisabled := true;
          lockWindow( frmDailyZoneStatusByProdType );
        end
      ;
      if( assigned( frmIterationSummary ) ) then
        begin
          frmIterationSummary.borderDisabled := true;
          lockWindow( frmIterationSummary );
        end
      ;
      if( assigned( frmOutputEvents ) ) then
        begin
          frmOutputEvents.borderDisabled := true;
          lockWindow( frmOutputEvents );
        end
      ;
      if( assigned( frmOutputExposures ) ) then
        begin
          frmOutputExposures.borderDisabled := true;
          lockWindow( frmOutputExposures );
        end
      ;
      if( assigned( frmEpiCurve ) ) then
        begin
          frmEpiCurve.borderDisabled := true;
          lockWindow( frmEpiCurve );
        end
      ;
      if( assigned( frmOutputStats ) ) then
        begin
          frmOutputStats.borderDisabled := true;
          lockWindow( frmOutputStats );
        end
      ;
      if( assigned( frmCompareStats ) ) then
        begin
          frmCompareStats.borderDisabled := true;
          lockWindow( frmCompareStats );
        end
      ;

      // For some reason, I don't want to do this...
      //self.Perform( WM_SETREDRAW, 0, 0 );
    end
  ;


  procedure TFormMain.unlockWindows();
    begin
      _windowsLocked := false;

      // Get rid of the last parameter window
      //-------------------------------------
      if( assigned( _lastParamForm ) ) then
        begin
          // Redraw _lastParamForm if it's hanging off the edge of the main window.
          // (The current param form will flicker, but at least _lastParamForm will go away completely.)
          // If _lastParamForm is completely contained by the main window, it isn't necessary to redraw it here:
          // it will go away when the main window is redrawn below.  (Not redrawing unnecessarily may eliminate
          // some screen flicker.)

          // Location of the main window as well as _lastParamForm are specified in screen coordinates,
          // which makes the comparison straight-forward.
          if
            ( self.Left > _lastParamForm.Left )
          or
            ( self.Top > _lastParamForm.Top )
          or
            ( (self.Left + self.Width) < (_lastParamForm.Left + _lastParamForm.Width) )
          or
            ( (self.Top + self.Height) < (_lastParamForm.Top + _lastParamForm.Height) )
          then
            begin
              _lastParamForm.Perform( WM_SETREDRAW, 1, 0 );
              RedrawWindow( _lastParamForm.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
            end
          ;

          _lastParamForm.Release();
          _lastParamForm := nil;
        end
      ;

      // Re-enable drawing of all output windows
      //----------------------------------------
      if( assigned( frmMap ) ) then
        begin
          frmMap.borderDisabled := false;
          unlockWindow( frmMap );
          frmMap.updateCaption();
        end
      ;
      if( assigned( frmDailyStatusByProdType ) ) then
        begin
          frmDailyStatusByProdType.borderDisabled := false;
          unlockWindow( frmDailyStatusByProdType );
        end
      ;
      if( assigned( frmDailyZoneStatusByProdType ) ) then
        begin
          frmDailyZoneStatusByProdType.borderDisabled := false;
          unlockWindow( frmDailyZoneStatusByProdType );
        end
      ;
      if( assigned( frmIterationSummary ) ) then
        begin
          frmIterationSummary.borderDisabled := false;
          unlockWindow( frmIterationSummary );
        end
      ;
      if( assigned( frmOutputEvents ) ) then
        begin
          frmOutputEvents.borderDisabled := false;
          unlockWindow( frmOutputEvents );
        end
      ;
      if( assigned( frmOutputExposures ) ) then
        begin
          frmOutputExposures.borderDisabled := false;
          unlockWindow( frmOutputExposures );
        end
      ;
      if( assigned( frmEpiCurve ) ) then
        begin
          frmEpiCurve.borderDisabled := false;
          unlockWindow( frmEpiCurve );
        end
      ;
      if( assigned( frmOutputStats ) ) then
        begin
          frmOutputStats.borderDisabled := false;
          unlockWindow( frmOutputStats );
        end
      ;
      if( assigned( frmCompareStats ) ) then
        begin
          frmCompareStats.borderDisabled := false;
          unlockWindow( frmCompareStats );
        end
      ;

      // Repaint this window
      //--------------------
      self.Perform( WM_SETREDRAW, 1, 0 );
      RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
    end
  ;


  {*
  The functions fillParamFormList and both versions of showParamForm are
  used for the series of "wizard" screens used to set scenario parameters.

  This function displays the parameter form at list position i.
  }
  procedure TFormMain.showParamForm(
        const formIndex: integer;
        var clearOutput: boolean;
        nextFormIndex: integer = NF_NONE;
        frmLeft: integer = -1;
        frmTop: integer = -1
      );
  	var
  		aClass: TPersistentClass;
  		aFormClass: TFormClass;
    	frm: TFormSMWizardBase;
      formType: PChar;

      frmCorner: TPoint;
      frmDisplayed: boolean;
    begin
      if( -1 = frmTop ) then frmTop := 100;
      if( -1 = frmLeft ) then frmLeft := 100;

      formType := _paramFormList.at( formIndex );

      if( formType <> nil ) then
      	begin
          aClass := FindClass( formType );
          aFormClass := TFormClass( aClass );

          frm := aFormClass.Create( self ) as TFormSMWizardBase;
          frm.Visible := false;
          // frm will be deleted when windows are unlocked: see TMainForm.unlockWindows().

          frm.showMultiUseWarning := _showMultiUseWarning;
          frm.showFunctionRemovalWarning := _showFunctionRemovalWarning;
          frm.showTestFeatureWarning := _showTestFeatureWarning;
          frm.showApplyToAllWarning := _showApplyToAllWarning;
          frm.iniHandler := _ini;
          frm.setContextMenuItems( self.ActionMainMenuBar1.ActionClient.Items[1] );

          dbcout( 'Setting indices: ' + intToStr(_selectedProdTypeIndex) + ', ' + intToStr(_selectedProdTypePairIndex), DBSHOWMSG );

          frm.selectedProdTypeIndex := _selectedProdTypeIndex;
          frm.selectedProdTypePairIndex := _selectedProdTypePairIndex;
          frm.selectedZoneIndex := _selectedZoneIndex;
          if( formIndex = 0 ) then
            frm.btnBack.Enabled := false
          ;
          if( formIndex = _paramFormList.Count-1 ) then
            frm.btnNext.Enabled := false
          ;

          frm.setParams( _smScenario );

          // Lock all output windows until the parameter form is shown.
          // Once the form is displayed, drawing will be re-enabled:
          // see TFormSMWizardBase.FormShow().
          dbcout( 'Locking windows from TFormMain.showParamForm()', DBSHOWMSG );
          lockWindows();

          frmCorner := clientToScreen( point( frmLeft, frmTop ) );
          frm.Left := frmCorner.X;
          frm.Top := frmCorner.Y;

          frm.ShowModal( nextFormIndex, frmDisplayed, formIndex );

          frmCorner := screenToClient( point( frm.Left, frm.Top ) );
          frmLeft := frmCorner.X;
          frmTop := frmCorner.Y;

          nextFormIndex := frm.NextForm;

          _showMultiUseWarning := frm.showMultiUseWarning;
          _showFunctionRemovalWarning := frm.showFunctionRemovalWarning;
          _showTestFeatureWarning := frm.showTestFeatureWarning;
          _showApplyToAllWarning := frm.showApplyToAllWarning;
          _selectedProdTypePairIndex := frm.selectedProdTypePairIndex;
          _selectedProdTypeIndex := frm.selectedProdTypeIndex;
          _selectedZoneIndex := frm.selectedZoneIndex;

          clearOutput := frm.outputCleared;

          if( frmDisplayed ) then // The form was displayed, and should be freed as soon as the next form is shown.
            _lastParamForm := frm
          else // The form was created, but never shown.  Free it now.
            frm.Release()
          ;

          case nextFormIndex of
          	NF_NONE: begin dbcout( 'Unlocking windows from NFNone', DBSHOWMSG ); unlockWindows(); end;
            NF_BACK: showParamForm( formIndex - 1, clearOutput, NF_BACK, frmLeft, frmTop );
            NF_NEXT: showParamForm( formIndex + 1, clearOutput, NF_NEXT, frmLeft, frmTop );
            else showParamForm( nextFormIndex, clearOutput, NF_NEXT, frmLeft, frmTop );
          end;
        end
      ;
    end
	;


  {*
  The functions fillParamFormList and both versions of showParamForm are
  used for the series of "wizard" screens used to set scenario parameters.

  This version of showParamForm is called by menu items.  It translates the
  name parameter to the appropriate list index, and calls the other version
  of showParamForm.  The param form class name must be given in
  paramFormList (see @ref TFormMain#fillParamFormList) for this function to
  display it.

  @param name A string containing the name of the "wizard" form to show.
  }
  procedure TFormMain.showParamForm( name: string );
  	var
      response: integer;
      tmpBool: boolean;

    	i: integer;
      clearOutput: boolean;
  	begin
      try
        if( _showOutputWarning and _smdb.containsOutput ) then
          begin
            response := msgYesNoCheckbox(
              tr( 'This scenario file contains output.  Changing parameters will require the deletion of existing output.' )
                + endl + endl
                + tr( 'Continue?' ),
                tr( 'Do not show this message again' ),
                tmpBool,
              tr( 'File contains output' ),
              IMGWarning,
              Self
            );

            _showOutputWarning := not( tmpBool );

            if( mrNo = response ) then
              exit
            ;
          end
        ;

        Screen.Cursor := crHourGlass;

        // Lock all windows
        // Disable the Redraw flag for the specified window
        //SendMessage( self.Handle, WM_SETREDRAW, 0, 0 );

        clearOutput := false;

        // disable menu options while changes are in progress.
        // Otherwise, the user can really screw things up.
        _paramChangesInProgress := true;
        updateMenuItems( true );

        // The param form class name must be given in paramFormList (see fillParamFormList)
        // for this function to display it.
        i := _paramFormList.indexOf( name );

        dbcout( 'Showing param form ''' + name + '''', DBSHOWMSG );

        // The value of 'clearOutput' may be changed by the form when it is shown.

        if( i <> -1 ) then
          showParamForm( i, clearOutput )
        ;

        if( clearOutput ) then
          begin
            dbcout( '*** Herds should be restored to their initial condition, and database will be cleared.', true );

            _smdb.initializeAllOutputRecords();
            _smScenario.herdList.initializeAllOutputRecords();
            _smScenario.herdList.prepareForIteration( -1 );
          end
        ;
        
        dbcout( 'Done with showParamForm', DBSHOWMSG );
      finally
        // Unlock all windows
        // Disable the Redraw flag for the specified window
        //SendMessage( self.Handle, WM_SETREDRAW, 1, 0 );
        //self.Refresh();

        _paramChangesInProgress := false;
        updateOutputWindows( clearOutput );
        updateDisplay();

        screen.Cursor := crDefault;
      end;

      dbcout( 'HERD LIST IS UPDATED: ' + uiBoolToText( _smScenario.herdList.updated ), DBSHOWMSG );
    end
  ;


  procedure TFormMain.ActionScenarioParamExecute( sender: TObject );
    var
      senderName: string;
      paramFormName: string;
    begin
      // If a menu item is selected, show the appropriate form.
      // If a submenu is selected, don't do anything.
      // If an item from a submenu is selected, show the appropriate form.

      if( not( sender is TComponent ) ) then
        exit
      else
        senderName := (sender as TComponent).Name
      ;

      paramFormName := paramFormForSenderName( senderName );

      if( 0 < length( paramFormName ) ) then
        showParamForm( paramFormName )
      ;
    end
  ;


  function TFormMain.paramFormForSenderName( const senderName: string ): string;
    begin
      if( senderName = 'ActionGeneralParams' ) then result := 'TFormGeneralParams'
      else if( senderName = 'ActionProdType' ) then result := 'TFormProdType'
      else if( senderName = 'ActionInitialUnitOptions' ) then result := 'TFormInitialUnitOptions'
      else if( senderName = 'ActionHerdListEditor' ) then result := 'TFormHerdListEditor'

      // Disease submenu
      else if( senderName = 'ActionDiseaseOptions' ) then result := 'TFormYesNoPrevalence'
      else if( senderName = 'ActionDisease' ) then result := 'TFormDisease'

      // Disease spread submenu
      else if( senderName = 'ActionSpreadOptions' ) then result := 'TFormSpreadOptions'
      else if( senderName = 'ActionProdTypePairs' ) then result := 'TFormProdTypePairs'
      else if( senderName = 'ActionContactSpread' ) then result := 'TFormContactSpread'
      else if( senderName = 'ActionAirborneSpread' ) then result := 'TFormAirborneSpread'

      // Detection submenu
      else if( senderName = 'ActionYesNoDetection' ) then result := 'TFormYesNoDetection'
      else if( senderName = 'ActionDetection' ) then result := 'TFormDetection'

      // Tracing submenu
      else if( senderName = 'ActionTracingOptions' ) then result := 'TFormTracingGlobal'
      else if( senderName = 'ActionTracing' ) then result := 'TFormTracing'
      else if( senderName = 'ActionTracingHerdExam' ) then result := 'TFormTracingHerdExam'
      else if( senderName = 'ActionTracingTesting' ) then result := 'TFormTracingTesting'

      // Zones submenu
      else if( senderName = 'ActionZonesOptions' ) then result := 'TFormYesNoZones'
      else if( senderName = 'ActionZonesDefinition' ) then result := 'TFormZoneCreation'
      else if( senderName = 'ActionZones' ) then result := 'TFormZone'

      // Destruction submenu
      else if( senderName = 'ActionDestrGlobal' ) then result := 'TFormDestrGlobal'
      else if( senderName = 'ActionDestrPriority' ) then result := 'TFormDestrPriority'
      else if( senderName = 'ActionDestruction' ) then result := 'TFormDestruction'

      // Vaccination submenu
      else if( senderName = 'ActionVaccGlobal' ) then result := 'TFormVaccGlobal'
      else if( senderName = 'ActionVaccPriority' ) then result := 'TFormVaccPriority'
      else if( senderName = 'ActionVaccination' ) then result := 'TFormVaccination'

      // Costs submenu
      else if( senderName = 'ActionCostOptions' ) then result := 'TFormCostOptions'
      else if( senderName = 'ActionCostsZones' ) then result := 'TFormCostsZones'
      else if( senderName = 'ActionCostsDestr' ) then result := 'TFormCostsDestr'
      else if( senderName = 'ActionCostsVacc' ) then result := 'TFormCostsVacc'
      
      // Output options
      else if( senderName = 'ActionOutputOptions' ) then result := 'TFormOutputOptions'

      // Custom outputs menu
      else if( senderName = 'ActionCustomOutputOptions' ) then result := 'TFormYesNoCustomOutputs'
      else if( senderName = 'ActionCustomOutputDefinitions' ) then result := 'TFormCustomOutputs'

      else result := ''
      ;
    end
  ;

  function TFormMain.paramFormIndex( const paramFormName: string ): integer;
    begin
      result := _paramFormList.IndexOf( paramFormName );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Output/Windows menu items and related functions
// ----------------------------------------------------------------------------
  procedure TFormMain.ActionOutputFormExecute( Sender: TObject );
    begin
      if( sender = ActionMap ) then
        frmMapToggle()
      else if( sender = ActionOutChart ) then
        frmDailyStatusByProdTypeToggle()
      else if( sender = ActionOutZoneChart ) then
        frmDailyZoneStatusByProdTypeToggle()
      else if( sender = ActionSummary ) then
        frmIterationSummaryToggle()
      else if( sender = ActionOutputStats ) then
        frmOutputStatsToggle()
      else if( sender = ActionCompareStats ) then
        frmCompareStatsToggle()
      else if( sender = ActionEpiCurve ) then
        frmEpiCurveToggle()
      else if( sender = ActionEvents ) then
        frmOutputEventsToggle()
      else if( sender = ActionExposures ) then
        frmOutputExposuresToggle()
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Run menu items and related functions
// ----------------------------------------------------------------------------
  procedure TFormMain.ActionRunExecute( Sender: TObject );
    var
      errorMessage: string;
      runResult: integer;
      {$IFNDEF EUREKALOG}
      dlgException: TDialogRunSimException;
      {$ENDIF}
      dlgLongMessage: TDialogLongMessage;
      days: integer;
      stopReason: TStopReason;
    begin
      dbcout( 'Prepping to run simulation...', DBSHOWMSG );

      // The RUN menu should be disabled if the sim library could not be loaded,
      // so this is probably an unnecessary check.
      if( not( naadsmLibLoaded ) ) then
        begin
          msgOK(
            tr( 'A required program library cannot be found or is out of date.' ) + ' '
              + tr( 'While you will be able to create a scenario, you will not be able to run it.' ) + ' '
              + endl + endl
              + tr( 'To solve this problem, please reinstall the application, or check with the developers.' ),
            SHORTERMASTERCAPTION,
            IMGWarning,
            frmMain
          );
          exit;
        end
      ;

    	errorMessage := '';

      stopReason := TStopReason( (Sender as TAction).tag );

      dbcout( 'stopReason: ' + stopReasonToString( stopReason ), DBSHOWMSG );

      if( ssStopAtSpecificDay = stopReason ) then
        begin
          dbcout( '### Stop at specified day', DBSHOWMSG );
          days := myStrToInt(
            msgInput(
              tr( 'Please specify the number of days for each iteration:' ),
              RE_INTEGER_INPUT,
              tr( 'Iteration length' ),
              IMGQuestion,
              self
            )
          );

          if( 0 = days ) then
            exit
          ;
        end
      else
        begin
          dbcout( 'Some other stop reason', DBSHOWMSG );
          days := 32767;
        end
      ;

      // Record the number of days specified and the reason for stopping the simulation
      _smdb.simDays := days;
      _smdb.simStopReason := stopReason;
      _smScenario.simInput.simDays := days;
      _smScenario.simInput.simStopReason := stopReason;

    	lblRunMessage.Caption := tr( 'Preparing to run simulation...' );

      // The map will be set to its initial state when the first iteration begins.
      // It isn't necessary or desirable to do so here.

      // Reset the progress indicators to the starting state
      lblIterationCounter.Caption := '';
      lblDayCounter.Caption := '';
      pbrIterations.Position := 0;
      pbrIterations.Max := _smScenario.simInput.simIterations;
      pnlRunStatus.Show();
      _simIsRunning := true;
      _userStop := NO_STOP;
      updateMenuItems( false );
      repaint(); // forces pnlRunStatus to be updated.

      dbcout( 'starting sim...', DBSHOWMSG );
      runResult := startSim(
        _smScenario.simInput,
        _smScenario.herdList,
        _smdb,
        errorMessage
      );

      case runResult of
        ERRRUNSIMEXCEPTION:
        	begin
            {$IFNDEF EUREKALOG}
              dlgException := TDialogRunSimException.create( self, errorMessage );
              dlgException.ShowModal();
              dlgException.Release();
            {$ENDIF}
          end
        ;
        ERRINVALIDSCENARIO:
        	begin
          	dlgLongMessage := TDialogLongMessage.create(
            	self,
              SHORTERMASTERCAPTION,
              tr( 'Problems were found with this scenario.  These problems must be corrected before this scenario can be run:' ),
              errorMessage
            );
            dlgLongMessage.showModal();
            dlgLongMessage.Release();
          end
        ;
        ERRCANNOTWRITEFILES:
        	begin
          	msgOK(
              tr( 'Temporary files required to launch this simulation were not written.' ) + '  ' 
                + tr( 'Please ensure that you have adequate hard disk space and sufficient permissions to write files to your hard disk.' ) + '  '  
                + tr( 'For further assistance, please contact the developers.' ),
              tr( 'Temporary files not written' ),
              IMGCritical,
              self
            );
          end
        ;
        ERRNONE:
        	begin
          	// FIX ME: ignore this?
          end
        ;
        ERRNOTSET:
        	begin
          	// ignore this.
          end
        ;
      end;


      pnlRunStatus.Hide();
      _simIsRunning := false;
      updateMenuItems( false );
    end
  ;


  procedure TFormMain.btnStopSimClick(Sender: TObject);
    begin
      _userStop := USER_STOP;
      lblRunMessage.Caption := tr( 'Simulation interrupted by user: please wait...' );
      lblRunMessage.Update();
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Display functions
// ----------------------------------------------------------------------------
  procedure TFormMain.frmMapToggle();
    begin
    	// Close the map if it showing.  If it isn't showing, then show it.
      if assigned( frmMap ) then
        begin
          frmMap.Close();
          uncheckWindowMenuItem( 'FormMap' );
        end
      else
        begin
          frmMap := TFormMap.Create( self, _smdb, _smScenario.simInput, _smScenario.herdList );
          ActionMap.Checked := True;
          setOpenWindows( _openWindows + 1 );
        end
      ;
    end
  ;


  procedure TFormMain.frmDailyStatusByProdTypeToggle();
    begin
    	// Close the form if it is showing.  If it isn't showing, then show it.
      if assigned( frmDailyStatusByProdType ) then
        begin
          frmDailyStatusByProdType.Close();
          uncheckWindowMenuItem( 'FormDailyStatusByProdType' );
        end
      else
        begin
        	frmDailyStatusByProdType := TFormDailyStatusByProdType.create( self, _smScenario.simInput, _smdb );
          ActionOutChart.Checked := True;
          setOpenWindows( _openWindows + 1 );
        end
      ;
    end
  ;

  procedure TFormMain.frmDailyZoneStatusByProdTypeToggle();
    begin
    	// Close the form if it is showing.  If it isn't showing, then show it.
      if assigned( frmDailyZoneStatusByProdType ) then
        begin
          frmDailyZoneStatusByProdType.Close();
          uncheckWindowMenuItem( 'FormDailyZoneStatusByProdType' );
        end
      else
        begin
        	frmDailyZoneStatusByProdType := TFormDailyZoneStatusByProdType.create( self, _smScenario.simInput, _smdb );
          ActionOutZoneChart.Checked := True;
          setOpenWindows( _openWindows + 1 );
        end
      ;
    end
  ;

	procedure TFormMain.frmIterationSummaryToggle();
		begin
      // Close the form if it is showing.  If it isn't showing, then show it.
			if Assigned( frmIterationSummary ) then
        begin
          frmIterationSummary.Close();
          uncheckWindowMenuItem( 'FormIterationSummary' );
        end
			else
			begin
				frmIterationSummary := TFormIterationSummary.Create( self, _smScenario.simInput, _smdb );
				ActionSummary.Checked := True;
        setOpenWindows( _openWindows + 1 );
			end;
		end
	;


  procedure TFormMain.frmOutputEventsToggle();
    begin
      // Close the form if it is showing.  If it isn't showing, then show it.
			if Assigned( frmOutputEvents ) then
        begin
          frmOutputEvents.Close();
          uncheckWindowMenuItem( 'FormOutputEvents' );
        end
			else
			begin
				frmOutputEvents := TFormOutputEvents.Create( self, _smScenario.simInput, _smdb );
				ActionEvents.Checked := True;
        setOpenWindows( _openWindows + 1 );
			end;
    end
  ;


  procedure TFormMain.frmOutputExposuresToggle();
    begin
      // Close the form if it is showing.  If it isn't showing, then show it.
			if Assigned( frmOutputExposures ) then
        begin
          frmOutputExposures.Close();
          uncheckWindowMenuItem( 'FormOutputExposures' );
        end
			else
			begin
				frmOutputExposures := TFormOutputExposures.Create( self, _smScenario.simInput, _smdb );
				ActionExposures.Checked := True;
        setOpenWindows( _openWindows + 1 );
			end;
    end
  ;


	procedure TFormMain.frmOutputStatsToggle();
		begin
      // Close the form if it is showing.  If it isn't showing, then show it.
			if Assigned( frmOutputStats ) then
        begin
          dbcout( 'Beginning to close output stats window...', DBSHOWMSG );
          frmOutputStats.Close();
          uncheckWindowMenuItem( 'FormOutputStats' );
          dbcout( 'Output stats window closed.', DBSHOWMSG );
        end
			else
			begin
        dbcout( 'Creating frmOutputStats', DBSHOWMSG );
				frmOutputStats := TFormOutputStats.Create( self, _smScenario.simInput, _smdb );
        ActionOutputStats.Checked := True;
        setOpenWindows( _openWindows + 1 );
			end;
		end
	;


  procedure TFormMain.frmCompareStatsToggle();
    var
      _smdbB: TSMDatabase;
      updateReason: TDBSchemaUpdateReason;
    begin
      // Close the form if it is showing.  If it isn't showing, then show it.
			if Assigned( frmCompareStats ) then
        begin
          dbcout( 'Beginning to close output stats window...', DBSHOWMSG );
          frmCompareStats.Close();
          uncheckWindowMenuItem( 'FormScenarioComparison' );
          dbcout( 'Output stats window closed.', DBSHOWMSG );
        end
			else
			begin
        updateReason := DBUpdateUnspecified;

        OpenDialog1.Title := 'Select a model scenario database to compare';

        if( _ini.hasKey( 'LastOpenDirectory' ) ) then
          OpenDialog1.initialDir := _ini.val( 'LastOpenDirectory' )
        ;

        if OpenDialog1.Execute() then
          begin
            _ini.update( 'LastOpenDirectory', directory( OpenDialog1.FileName ) );
            dbcout( 'Creating frmCompareStats', DBSHOWMSG );

            // Load the second database file, and
            // do the major error checking here.
            //-----------------------------------
            dbcout( 'Creating database B...', DBSHOWMSG );

            // OpenDialog1 checks for the existence of the file.
            // It isn't necessary to check again.

            _smdbB := TSMDatabase.create( OpenDialog1.FileName, DBOpen, nil, nil );
            // If there is an error, _smdbB is freed by this function.
            // Otherwise, it will be freed by frmCompareStats when that form closes.

            // Did the file open properly?
            if( not( _smdbB.isOpen ) ) then
              begin
                msgOK(
                  ansiReplaceStr( tr( 'The selected scenario file xyz' ), 'xyz', abbrevPath( OpenDialog1.FileName ) ) + ' ' + tr( 'cannot be opened.' ),
                  tr( 'Cannot compare scenarios' ),
                  IMGCritical,
                  self
                );

                freeAndNil( _smdbB );
                exit;
              end
            ;

            // Is the file the right version?
            case _smdb.checkVersion( updateReason ) of
              DBVersionUnrecognized:
                begin
                  // Nothing can be done.
                  msgOK(
                    ansiReplaceStr( tr( 'The selected file xyz' ), 'xyz', abbrevPath( OpenDialog1.FileName ) ) + ' ' + tr( 'does not appear to contain an NAADSM scenario' ) + ' '
                      + endl + tr( 'or may have been created with a newer version of NAADSM.' ) 
                      + tr( 'It cannot be displayed.' ),
                    tr( 'Cannot compare scenarios' ),
                    IMGCritical,
                    self
                  );

                  freeAndNil( _smdbB );
                  exit;
                end
              ;
              DBVersionObsolete:
                begin
                  // Nothing can be done.
                  msgOK(
                    tr( 'The format of the selected scenario file is out of date, and conversion to the new format requires a separate conversion utility.' ) + '  '  
                      + tr( 'For further assistance, please contact the programmers.' ),
                    tr( 'Obsolete scenario file' ),
                    IMGCritical,
                    self
                  );

                  freeAndNil( _smdbB );
                  exit;
                end
              ;
              DBVersionUpdated:
                begin
                  // Something could maybe be done, some day.
                  // But not today.
                  msgOK(
                    tr( 'The format of the selected scenario file is out of date.  You may wish to consider re-running the the scenario in this version of NAADSM.' ),
                    tr( 'Scenario file out of date' ),
                    IMGCritical,
                    self
                  );

                  freeAndNil( _smdbB );
                  exit;
                end
              ;
              DBVersionCurrent:
                begin
                  // Do nothing.
                  // All is right with the world.
                end
              ;
            end;


            // Does the other database contain output?
            if( not( _smdbB.containsOutput ) ) then
              begin
                msgOK(
                  ansiReplaceStr( tr( 'The selected scenario file xyz' ), 'xyz', abbrevPath( OpenDialog1.FileName ) ) + ' ' + tr( 'contains no output.' ),
                  tr( 'Cannot compare scenarios' ),
                  IMGCritical,
                  self
                );

                freeAndNil( _smdbB );
                exit;
              end
            ;

            // Does the other database contain a complete simulation?
            if( not( _smdbB.simulationComplete ) ) then
              begin
                msgOK(
                  ansiReplaceStr( tr( 'The simulation in file xyz' ), 'xyz', abbrevPath( OpenDialog1.FileName ) ) + ' ' + tr( 'did not run to completion.' ),
                  tr( 'Cannot compare scenarios' ),
                  IMGCritical,
                  self
                );

                freeAndNil( _smdbB );
                exit;
              end
            ;

            frmCompareStats := TFormScenarioComparison.create( self, _smScenario.simInput, _smdb, _smdbB );

            ActionCompareStats.Checked := True;
            setOpenWindows( _openWindows + 1 );
          end
        ;
			end;
    end
  ;


	procedure TFormMain.frmEpiCurveToggle();
		begin
      // Close the form if it is showing.  If it isn't showing, then show it.
			if Assigned( frmEpiCurve ) then
        begin
          frmEpiCurve.Close();
          uncheckWindowMenuItem( 'FormSummaryEpiCurves' );
        end
			else
			begin
        dbcout( 'Creating frmEpiCurve', DBSHOWMSG );
				frmEpiCurve := TFormSummaryEpiCurves.Create( self, _smScenario.simInput, _smdb );
				ActionEpiCurve.Checked := True;
        setOpenWindows( _openWindows + 1 );
			end;
		end
	;


  procedure TFormMain.setOpenWindows( val: integer );
    begin
      _openWindows := val;
      updateMenuItems( false );
    end
  ;


  procedure TFormMain.ActionCascadeExecute( Sender: TObject );
    var
      Spacing: integer;
      FNo: integer;
      baseh: integer;
      Ow: integer;
      StandardH: integer;
      StandardW: integer;

      procedure Calculate( Spacing, BaseH, Ow: integer; var StandardH: integer );
        begin
          StandardH := BaseH - ((Ow-1)*Spacing);
          StandardW := Self.ClientWidth - (((Ow-1)*Spacing) + 5);
        end
      ;

      procedure AdjustFormPosition( var FNo: integer; S: integer; F: TForm );
        begin
          F.Left := FNo*S;
          F.Top := FNo*S;
          F.Height := StandardH;
          F.Width := StandardW;
          Inc(Fno);
        end
      ;
    begin  // ActionCascadeExecute
      Ow := _openWindows;
      baseh := Self.ClientHeight - (pnlRunStatus.Height + ActionMainMenuBar1.Height);
      FNo := 0;
      Spacing := Self.Height - Self.ClientHeight;
      Calculate(Spacing, BaseH, Ow, StandardH);

      if Assigned(frmMap) then AdjustFormPosition( FNo, Spacing, frmMap );
      if Assigned(frmDailyStatusByProdType) then AdjustFormPosition( FNo, Spacing, frmDailyStatusByProdType );
      if Assigned(frmDailyZoneStatusByProdType) then AdjustFormPosition( FNo, Spacing, frmDailyZoneStatusByProdType );
      if Assigned(frmIterationSummary) then AdjustFormPosition( FNo, Spacing, frmIterationSummary );
      if Assigned(frmOutputStats) then AdjustFormPosition( FNo, Spacing, frmOutputStats );
      if Assigned(frmCompareStats) then AdjustFormPosition( FNo, Spacing, frmCompareStats );
      if Assigned(frmEpiCurve) then AdjustFormPosition( FNo, spacing, frmEpiCurve );
      if Assigned(frmOutputEvents) then AdjustFormPosition( FNo, spacing, frmOutputEvents );
      if Assigned(frmOutputExposures) then AdjustFormPosition( FNo, spacing, frmOutputExposures );
    end
  ;


  procedure TFormMain.ActionArrangeExecute( Sender: TObject );
    var
      Ow: integer;
      Cnt: byte;

      procedure AdjustAndPositionWindow(F: TForm; var Cnt: byte );
        var
          baseh, h: integer;
        begin
          baseh := Self.ClientHeight - (pnlRunStatus.Height + ActionMainMenuBar1.Height + 4);

          if( OW < 3 ) then
            h := baseh
          else if( OW < 5 ) then
            h := baseh div 2
          else
            h := baseh div 3
          ;

          F.Height := h;
          F.Width := (Self.ClientWidth div 2) - 2;

          if( Odd(Cnt) ) then
            F.Left := 0
          else
            F.Left := F.Width
          ;

          if( Cnt < 3 ) then
            F.Top := 0
          else if( Cnt < 5 ) then
            F.Top := h
          else
            F.Top := 2*h
          ;

          Inc(Cnt);
        end
      ;
    begin // ActionArrageExecute
      Ow := _openWindows;
      Cnt := 1;
      if Assigned( frmMap ) then AdjustAndPositionWindow( frmMap, Cnt );
      if Assigned( frmDailyStatusByProdType ) then AdjustAndPositionWindow( frmDailyStatusByProdType, Cnt );
      if Assigned( frmDailyZoneStatusByProdType ) then AdjustAndPositionWindow( frmDailyZoneStatusByProdType, Cnt );
      if Assigned( frmIterationSummary ) then adjustAndPositionWindow( frmIterationSummary, Cnt );
      if Assigned( frmOutputStats ) then adjustAndPositionWindow( frmOutputStats, Cnt );
      if Assigned( frmCompareStats ) then adjustAndPositionWindow( frmCompareStats, Cnt );
      if Assigned( frmEpiCurve ) then adjustAndPositionWindow( frmEpiCurve, Cnt );
      if Assigned( frmOutputEvents ) then adjustAndPositionWindow( frmOutputEvents, Cnt );
      if Assigned( frmOutputExposures ) then adjustAndPositionWindow( frmOutputExposures, Cnt );
    end
  ;


	procedure TFormMain.updateDisplay();
  	begin
    	updateCaption();
      updateMenuItems( false );
    end
  ;


	procedure TFormMain.updateCaption();
  	var
    	str: string;
  	begin
    	str := MASTERCAPTION;
      
      if( nil <> _smdb ) then
      	begin
        	str := str + ' [' + shortFileName( _smdb.permanentDBFileName );

          if( _smdb.isReadOnly ) then
            str := str + ' (Read only)]'
          else if( _smdb.workingDBHasChanged ) then
            str := str + '*]'
          else
            str := str + ']'
          ;
        end
      ;

      self.Caption := str;
    end
  ;


  procedure TFormMain.updateOutputWindows( const clearOutput: boolean );
    begin
    	dbcout( '-- TFormMain.updateOutputWindows()', DBSHOWMSG );

      if( nil = _smdb ) then
        closeOutputWindows()
      else
        begin
          // Deal with single-iteration output windows
          //------------------------------------------
          if( assigned( frmMap ) ) then
            frmMap.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList, clearOutput )
          ;

          if( assigned( frmDailyStatusByProdType ) ) then
            frmDailyStatusByProdType.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
          ;

          if( assigned( frmDailyZoneStatusByProdType ) ) then
            begin
              if( _smScenario.simInput.includeZonesGlobal ) then
                frmDailyZoneStatusByProdType.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
              else // close the window if zones aren't being used.
                frmDailyZoneStatusByProdTypeToggle()
              ;
            end
          ;

          if( assigned( frmIterationSummary ) ) then
            frmIterationSummary.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
          ;

          if( assigned( frmOutputEvents ) ) then
            frmOutputEvents.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
          ;

          if( assigned( frmOutputExposures ) ) then
            frmOutputExposures.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
          ;


          // Deal with cumulative output windows
          //------------------------------------
          if( 0 < _smdb.completedIterations ) then
            begin
              if( assigned( frmEpiCurve ) ) then
                frmEpiCurve.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
              ;

              if( assigned( frmOutputStats ) ) then
                frmOutputStats.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
              ;

              if( assigned( frmCompareStats ) ) then
                frmCompareStats.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList )
              ;
            end
          else
            closeCumulativeOutputWindows()
          ;
        end
      ;
      dbcout( '-- Done TFormMain.updateOutputWindows()', DBSHOWMSG );
    end
  ;


  procedure TFormMain.uncheckWindowMenuItem( frmName: string );
    begin
      try
        try
          if( 'FormMap' = frmName ) then
            begin
              ActionMap.Checked := false;
              frmMap := nil;
            end
          else if( 'FormIterationSummary' = frmName ) then
            begin
              ActionSummary.Checked := false;
              frmIterationSummary := nil;
            end
          else if( 'FormDailyStatusByProdType' = frmName ) then
            begin
              ActionOutChart.Checked := false;
              frmDailyStatusByProdType := nil;
            end
          else if( 'FormDailyZoneStatusByProdType' = frmName ) then
            begin
              ActionOutZoneChart.Checked := false;
              frmDailyZoneStatusByProdType := nil;
            end
          else if( 'FormOutputStats' = frmName ) then
            begin
              ActionOutputStats.Checked := false;
              frmOutputStats := nil;
            end
          else if( 'FormScenarioComparison' = frmname ) then
            begin
              ActionCompareStats.checked := false;
              frmCompareStats := nil;
            end
          else if( 'FormSummaryEpiCurves' = frmName ) then
            begin
              ActionEpiCurve.Checked := false;
              frmEpiCurve := nil;
            end
          else if( 'FormOutputEvents' = frmName ) then
            begin
              ActionEvents.checked := false;
              frmOutputEvents := nil;
            end
          else if( 'FormOutputExposures' = frmName ) then
            begin
              ActionExposures.checked := false;
              frmOutputExposures := nil;
            end
          else
            raise exception.create( 'Unrecognized form name (' + frmName + ') in TFormMain.closeWindow' )
          ;
        except
          {$IFDEF DEBUG}
          dbmsg( 'Exception occurred in TFormMain.closeWindow' );
          {$ENDIF}
        end;

      finally
        dec( _openWindows );
        updateMenuItems( false );
      end;
    end
  ;


  procedure TFormMain.closeCumulativeOutputWindows();
    begin
      if( assigned( frmOutputStats ) ) then
        begin
          frmOutputStats.Close();
          uncheckWindowMenuItem( 'FormOutputStats' );
        end
      ;

      if( assigned( frmEpiCurve ) ) then
        begin
          frmEpiCurve.Close();
          uncheckWindowMenuItem( 'FormSummaryEpiCurves' );
        end
      ;

      if( assigned( frmCompareStats ) ) then
        begin
          frmCompareStats.close();
          uncheckWindowMenuItem( 'FormScenarioComparison' );
        end
      ;
    end
  ;


  procedure TFormMain.closeIterationOutputWindows();
    begin
    	if( assigned( frmMap ) ) then
        begin
          frmMap.Close();
          uncheckWindowMenuItem( 'FormMap' );
        end
      ;

      if( assigned( frmDailyStatusByProdType ) ) then
        begin
          frmDailyStatusByProdType.Close();
          uncheckWindowMenuItem( 'FormDailyStatusByProdType' );
        end
      ;

      if( assigned( frmDailyZoneStatusByProdType ) ) then
        begin
          frmDailyZoneStatusByProdType.Close();
          uncheckWindowMenuItem( 'FormDailyZoneStatusByProdType' );
        end
      ;

      if( assigned( frmIterationSummary ) ) then
        begin
          frmIterationSummary.Close();
          uncheckWindowMenuItem( 'FormIterationSummary' );
        end
      ;

      if( assigned( frmOutputEvents ) ) then
        begin
          frmOutputEvents.Close();
          uncheckWindowMenuItem( 'FormOutputEvents' );
        end
      ;

      if( assigned( frmOutputExposures ) ) then
        begin
          frmOutputExposures.Close();
          uncheckWindowMenuItem( 'FormOutputExposures' );
        end
      ;
    end
  ;


  procedure TFormMain.closeOutputWindows();
  	begin
      closeIterationOutputWindows();
      closeCumulativeOutputWindows();
      setOpenWindows( 0 );
    end
  ;


  procedure TFormMain.ActionCloseWindowsExecute( Sender: TObject );
    begin
      closeOutputWindows();
    end
  ;


  procedure TFormMain.updateScenarioParamsMenuItems( const paramsEnabled: boolean );
    var
      useSpread: boolean;
      useContactSpread: boolean;
      useAirborneSpread: boolean;
      useDetection: boolean;
      useTracing: boolean;
      useTracingHerdExam: boolean;
      useTracingTesting: boolean;
      useZones: boolean;
      useDestruction: boolean;
      useVaccination: boolean;
      useCostsZones: boolean;
      useCostsDestr: boolean;
      useCostsVacc: boolean;
      useCustomOutputs: boolean;
    begin
      if( nil = _smScenario ) then
        begin
          useSpread := false;
          useContactSpread := false;
          useAirborneSpread := false;
          useDetection := false;
          useTracing := false;
          useTracingHerdExam := false;
          useTracingTesting := false;
          useZones := false;
          useDestruction := false;
          useVaccination := false;
          useCostsZones := false;
          useCostsDestr := false;
          useCostsVacc := false;
          useCustomOutputs := false;
        end
      else
        begin
          useSpread := _smScenario.simInput.includeAirborneSpreadGlobal or _smScenario.simInput.includeContactSpreadGlobal;
          useContactSpread := _smScenario.simInput.includeContactSpreadGlobal;
          useAirborneSpread := _smScenario.simInput.includeAirborneSpreadGlobal;
          useDetection := _smScenario.simInput.includeDetectionGlobal;
          useTracing := _smScenario.simInput.includeTracingGlobal;
          useTracingHerdExam := _smScenario.simInput.includeTracingGlobal and _smScenario.simInput.includeTracingHerdExamGlobal;
          useTracingTesting := _smScenario.simInput.includeTracingGlobal and _smScenario.simInput.includeTracingTestingGlobal;

          useZones := _smScenario.simInput.includeZonesGlobal;
          useDestruction := _smScenario.simInput.includeDestructionGlobal;
          useVaccination := _smScenario.simInput.includeVaccinationGlobal;
          useCostsZones := _smScenario.simInput.costTrackZoneSurveillance;
          useCostsDestr := _smScenario.simInput.costTrackDestruction;
          useCostsVacc := _smScenario.simInput.costTrackVaccination;
          useCustomOutputs := _smScenario.simInput.useCustomOutputs;
        end
      ;

      ActionGeneralParams.Enabled := paramsEnabled;
      ActionProdType.Enabled := paramsEnabled;

      ActionUnitsMenu.Enabled := paramsEnabled;
      ActionInitialUnitOptions.Enabled := paramsEnabled;
      ActionHerdListEditor.Enabled  := paramsEnabled;

      ActionDiseaseMenu.Enabled := paramsEnabled;
      ActionDiseaseOptions.Enabled := paramsEnabled;
      ActionDisease.Enabled := paramsEnabled;

      ActionSpreadMenu.Enabled := paramsEnabled;
      ActionSpreadOptions.Enabled := paramsEnabled;
      ActionProdTypePairs.Enabled := useSpread and paramsEnabled;
      ActionContactSpread.Enabled := useContactSpread and paramsEnabled;
      ActionAirborneSpread.Enabled := useAirborneSpread and paramsEnabled;

      ActionDetectionMenu.Enabled := paramsEnabled;
      ActionYesNoDetection.Enabled := paramsEnabled;
      ActionDetection.Enabled := useDetection and paramsEnabled;

      // if detection is not used then accessing all control measures is disabled
      //-----------------------------------------------------------------------
      ActionTracingMenu.Enabled := paramsEnabled and useDetection;
      ActionTracingOptions.Enabled := paramsEnabled;
      ActionTracing.Enabled := useTracing and paramsEnabled;
      ActionTracingHerdExam.Enabled := useTracing and useTracingHerdExam and paramsEnabled;
      ActionTracingTesting.Enabled := useTracing and useTracingTesting and paramsEnabled;

      ActionZonesMenu.Enabled := paramsEnabled and useDetection;
      ActionZonesOptions.enabled := paramsEnabled;
      ActionZonesDefinition.enabled := useZones and paramsEnabled;
      ActionZones.Enabled := useZones and paramsEnabled;

      ActionDestructionMenu.Enabled := paramsEnabled and useDetection;
      ActionDestrGlobal.Enabled := paramsEnabled;
      ActionDestrPriority.Enabled := useDestruction and paramsEnabled;
      ActionDestruction.Enabled := useDestruction and paramsEnabled;

      ActionVaccinationMenu.Enabled := paramsEnabled and useDetection;
      ActionVaccGlobal.Enabled := paramsEnabled;
      ActionVaccPriority.Enabled := useVaccination and paramsEnabled;
      ActionVaccination.Enabled := useVaccination and paramsEnabled;

      ActionCostMenu.Enabled := paramsEnabled and useDetection;
      // The option to include costs is only provided if one of the control measures is modeled
      ActionCostOptions.Enabled := paramsEnabled and ( useZones or useDestruction or useVaccination );
      ActionCostsZones.Enabled := useCostsZones and useZones and paramsEnabled;
      ActionCostsDestr.Enabled := useCostsDestr and useDestruction and paramsEnabled;
      ActionCostsVacc.Enabled := useCostsVacc and useVaccination and paramsEnabled;
      //-----------------------------------------------------------------------

      ActionOutputOptions.Enabled :=  paramsEnabled;

      ActionCustomOutputMenu.Enabled := paramsEnabled;
      ActionCustomOutputOptions.Enabled := paramsEnabled;
      ActionCustomOutputDefinitions.Enabled := useCustomOutputs and paramsEnabled;
    end
  ;


  procedure TFormMain.updateMenuItems( disableAll: boolean; temporarilyEnable: boolean = false );
    var
      useEvents: boolean;
      useExposures: boolean;
      useZones: boolean;
      readOnlyScenario: boolean;
      outputDataExists: boolean;
      paramsEnabled: boolean;
    begin
      if( disableAll ) then
        begin
          ActionMainMenuBar1.Enabled := false;
          exit;
        end
      else
        ActionMainMenuBar1.Enabled := true
      ;

      if( nil = _smScenario ) then
        begin
          useEvents := false;
          useExposures := false;
          useZones := false;
          readOnlyScenario := false;
          outputDataExists := false;
        end
      else
        begin
          useEvents := _smScenario.simInput.outputOptions.saveDailyEvents;
          useExposures := _smScenario.simInput.outputOptions.saveDailyExposuresAndTraces;
          useZones := _smScenario.simInput.includeZonesGlobal;
          readOnlyScenario := _smdb.isReadOnly;
          outputDataExists := _smdb.containsOutput;
        end
      ;

      if( temporarilyEnable ) then
        paramsEnabled := true
      else
        begin
          paramsEnabled := scenarioIsOpen
            and not simIsRunning
            and not _paramChangesInProgress
            and not readOnlyScenario
          ;
        end
      ;

      // File menu
      //-----------
      ActionNew.Enabled := not scenarioIsOpen and not simIsRunning;
      acnEmptyScenario.Enabled := not scenarioIsOpen and not simIsRunning;
      acnSampleScenario.Enabled := not scenarioIsOpen and not simIsRunning;
      ActionOpen.Enabled := not simIsRunning;
      ActionSave.Enabled := paramsEnabled;
      ActionSaveAs.Enabled := scenarioIsOpen and not simIsRunning and not _paramChangesInProgress;
      ActionImport.Enabled := not scenarioIsOpen and not simIsRunning;
      ActionExport.Enabled := scenarioIsOpen and not simIsRunning and not _paramChangesInProgress;
      ActionClose.Enabled := scenarioIsOpen and not simIsRunning and not _paramChangesInProgress;
      ActionExit.Enabled := not simIsRunning and not _paramChangesInProgress;

      // Scenario parameters menu
      //--------------------------
      dbcout( 'Updating setup menu items', DBSHOWMSG );
      updateScenarioParamsMenuItems( paramsEnabled );

      ActionVerifyScenario.Enabled := scenarioIsOpen and not simIsRunning and not _paramChangesInProgress;
      ActionVerifyHerds.Enabled := scenarioIsOpen and not simIsRunning and not _paramChangesInProgress;
      ActionClearOutput.enabled := paramsEnabled and outputDataExists;

      // Run menu (startup actions for runs)
      //------------------------------------
      dbcout( 'Updating run menu items', DBSHOWMSG );
      ActionRunUntilOutbreakEnd.Enabled := naadsmLibLoaded and paramsEnabled;
      ActionRunUntilDetection.Enabled := naadsmLibLoaded and paramsEnabled;
      ActionRunUntilDay.Enabled := naadsmLibLoaded and paramsEnabled;
      ActionRunUntilDiseaseEnd.Enabled := naadsmLibLoaded and paramsEnabled;

      ActionStop.Enabled := scenarioIsOpen and simIsRunning and not _paramChangesInProgress;

      // Output/windows menu
      //---------------------
      dbcout( 'Updating output menu items', DBSHOWMSG );
      ActionMap.Enabled := scenarioIsOpen and not _paramChangesInProgress; // TFormMap
      ActionOutChart.Enabled := scenarioIsOpen and not _paramChangesInProgress; // TFormDailyStatusByProdType
      ActionOutZoneChart.Enabled := scenarioIsOpen and useZones and not _paramChangesInProgress; // TFormDailyZoneStatusByProdType
      ActionSummary.Enabled := scenarioIsOpen and not _paramChangesInProgress; // TFormIterationSummary

      // These output forms are for a single iteration, but are not (yet?) updated dynamically.
      // They are available only when a simulation is complete.
      if( ( scenarioIsOpen ) and ( not _paramChangesInProgress ) ) then
        begin
          // TFormOutputEvents
          ActionEvents.Enabled := _smdb.containsOutput and useEvents;
          // TFormOutputExposures
          ActionExposures.Enabled := _smdb.containsOutput and useExposures;
        end
      else
        begin
          ActionEvents.Enabled := false;
          ActionExposures.Enabled := false;
        end
      ;

      // These output forms are for multiple iterations, and should only be
      // available when a simulation is complete.
      if( ( scenarioIsOpen ) and ( not _paramChangesInProgress ) and ( not simIsRunning ) ) then
        begin
          // TFormOutputStats
          ActionOutputStats.Enabled := _smdb.simulationComplete;
          // TFormSummaryEpiCurves
          ActionEpiCurve.Enabled := _smdb.simulationComplete;
          // TFormScenarioComparison
          ActionCompareStats.enabled := _smdb.simulationComplete;
        end
      else
        begin
          ActionOutputStats.Enabled := false;
          ActionEpiCurve.Enabled := false;
          ActionCompareStats.Enabled := false;
        end
      ;
      
      ActionCloseWindows.Enabled := ( 0 < _openWindows );
      ActionArrange.Enabled := ( 0 < _openWindows );
      ActionCascade.Enabled :=  ( 0 < _openWindows );

      // Other crap
      //------------
      dbcout( 'Updating everything else', DBSHOWMSG );

    end
  ;


  procedure TFormMain.ActionLanguageSettingsExecute( Sender: TObject );
    var
      frm: TFormLanguageSettings;
    begin
      frm := TFormLanguageSettings.Create( self, true, true );
      frm.ShowModal();
      frm.Release();
    end
  ;


  procedure TFormMain.acnRegionalSettingsExecute(Sender: TObject);
    var
      frm: TFormRegionalSettings;
    begin
      frm := TFormRegionalSettings.Create( self, _i88nSettings );
      frm.ShowModal();
      frm.Release();
    end
  ;


  procedure TFormMain.ActionAboutExecute(Sender: TObject);
    var
      frmExp: TFormAboutExperimental;
      frm: TFormAbout;
    begin
      if( IS_EXPERIMENTAL ) then
        begin
          frmExp := TFormAboutExperimental.create( self );
          frmExp.showModal();
          frmExp.Release();

          // This silliness prevents the display of a meaningless hint.
          frm := nil;
          freeAndNil( frm );
        end
      else
        begin
          frm := TFormAbout.create( self );
          frm.showModal();
          frm.Release();

          // Ditto.
          frmExp := nil;
          freeAndNil( frmExp );
        end
      ;
    end
  ;


  procedure TFormMain.ActionWebsiteExecute(Sender: TObject);
    begin
      ShellExecute(
        Application.Handle,
        PChar( 'open' ),
        PChar( WEBSITE ),
        PChar( 0 ),
        nil,
        SW_NORMAL
      );
    end
  ;


  procedure TFormMain.ActionSupportForumsExecute(Sender: TObject);
    begin
      ShellExecute(
        Application.Handle,
        PChar( 'open' ),
        PChar( WEBSITE_SUPPORT_FORUMS ),
        PChar( 0 ),
        nil,
        SW_NORMAL
      );
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Properties
// ----------------------------------------------------------------------------
	function TFormMain.getScenarioIsOpen(): boolean;
  	begin
      if( nil <> _smdb ) then
        result := true
      else
        result := false
      ;
  	end
  ;

  function TFormMain.getFirstOverallVaccination(): integer; begin result := _firstOverallVacc; end;
  function TFormMain.getFirstOverallDetection(): integer; begin result := _firstOverallDet; end;
  function TFormMain.getFirstOverallDestruction(): integer; begin result := _firstOverallDestr; end;

  function TFormMain.getDisplayedIteration():Integer; begin result := _displayedIteration; end;

  procedure TFormMain.setDisplayedIteration( _iteration: Integer );
    begin
      _displayedIteration := _iteration;

      if( assigned( frmDailyStatusByProdType ) ) then frmDailyStatusByProdType.resetIteration( _iteration );
      if( assigned( frmDailyZoneStatusByProdType ) ) then frmDailyZoneStatusByProdType.resetIteration( _iteration );
      if ( assigned( frmIterationSummary ) ) then frmIterationSummary.resetIteration( _iteration );
      if ( assigned( frmOutputEvents ) ) then frmOutputEvents.resetIteration( _iteration );
      if ( assigned( frmOutputExposures ) ) then frmOutputExposures.resetIteration( _iteration );
    end
  ;


  function TFormMain.getIterationInProgress(): integer;
    begin
      if( not _simIsRunning ) then
        _iterationInProgress := -1
      ;

      result := _iterationInProgress;
    end
  ;


  function TFormMain.getSimStatusStr(): string;
    begin
      result := tr( 'Unknown' );

      if( _simIsRunning ) then
        result := tr( 'Simulation in progress' )
      else if( nil = _smdb ) then
        // FIX ME: This is probably an error...
      else
        begin
          if( _smdb.containsOutput ) then
            begin
              if( _smdb.containsIncompleteIterations() ) then
                result := tr( 'Simulation aborted' )
              else
                result := tr( 'Simulation complete' )
              ;
            end
          else
            result := tr( 'Initial conditions' )
          ;
        end
      ;
    end
  ;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//  Functions for DLL interaction
// ----------------------------------------------------------------------------
	procedure TFormMain.simStart();
  	begin
   		lblRunMessage.caption := '';
      _displayedIteration := -1;
      _iterationInProgress := -1;

      if( assigned( frmMap ) ) then
        begin
          frmMap.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList, true );
          //frmMap.updateSimStarted(); // frmMap doesn't inherit from TFormSMOutputBase here.
        end
      ;

      if( assigned( frmDailyStatusByProdType ) ) then
        begin
          frmDailyStatusByProdType.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList );
          frmDailyStatusByProdType.updateSimStarted();
        end
      ;

      if( assigned( frmDailyZoneStatusByProdType ) ) then
        begin
          frmDailyZoneStatusByProdType.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList );
          frmDailyZoneStatusByProdType.updateSimStarted();
        end
      ;

      if( assigned( frmIterationSummary ) ) then
        begin
          frmIterationSummary.resetSim( _smdb, _smScenario.simInput, _smScenario.herdList );
          frmIterationSummary.updateSimStarted();
        end
      ;

      if( assigned( frmOutputEvents ) ) then
        begin
          frmOutputEvents.Close();
          uncheckWindowMenuItem( 'FormOutputEvents' );
        end
      ;

      if( assigned( frmOutputExposures ) ) then
        begin
          frmOutputExposures.Close();
          uncheckWindowMenuItem( 'FormOutputExposures' );
        end
      ;

      closeCumulativeOutputWindows();
    end
  ;


  procedure TFormMain.iterationStart( it: integer );
  	begin
      // FIX ME: if a herd's initial state is destroyed or vaccinated, one or more of these should be 0
      _firstOverallDet := -1;
      _firstOverallDestr := -1;
      _firstOverallVacc := -1;

      _iterationInProgress := it + 1;

      _userStop := NO_STOP;

      lblIterationCounter.Caption := tr( 'Iteration' ) + ' ' + intToStr( it + 1 ) + ' ' +  tr( 'of' ) + ' ' + intToStr( _smScenario.simInput.simIterations );
      lblDayCounter.Caption := tr( 'Day 1' );

    	if( assigned( frmMap ) ) then frmMap.drawAllUnits();
      if( assigned( frmDailyStatusByProdType ) ) then frmDailyStatusByProdType.reset();
      if( assigned( frmDailyZoneStatusByProdType ) ) then frmDailyZoneStatusByProdType.reset();
      if( assigned( frmIterationSummary ) ) then frmIterationSummary.reset();
    end
  ;


  procedure TFormMain.dayStart( day: integer );
  	begin
    	lblDayCounter.Caption := tr( 'Day' ) + ' ' + intToStr( day );
			_simDayStartTime := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
    end
  ;


  procedure TFormMain.changeHerdState( h: THerd );
  	begin
    	if( assigned( frmMap ) ) then frmMap.drawThisUnit( h );
    end
  ;


  procedure TFormMain.traceHerd( h: THerd );
    begin
      if( assigned( frmMap ) ) then frmMap.drawThisUnit( h );
    end
  ;


  procedure TFormMain.examineHerd( h: THerd );
    begin
      if( assigned( frmMap ) ) then frmMap.drawThisUnit( h );
    end
  ;


  procedure TFormMain.quarantineHerd( h: THerd );
    begin
      if( assigned( frmMap ) ) then frmMap.drawThisUnit( h );
    end
  ;
  

  procedure TFormMain.detectHerd( h: THerd; wasFirstEventForProdType: boolean; day: integer );
    begin
      if( assigned( frmMap ) ) then frmMap.drawThisUnit( h );

      if( -1 = _firstOverallDet ) then _firstOverallDet := day;

      if( wasFirstEventForProdType and assigned( frmDailyStatusByProdType ) ) then
        begin
          frmDailyStatusByProdType.firstDetection( day, h.prodType.productionTypeID );
        end
      ;
    end
  ;


  procedure TFormMain.destroyHerd( h: THerd; wasFirstEventForProdType: boolean; day: integer );
    begin
      // Map will be updated by changeState

      if( -1 = _firstOverallDestr ) then _firstOverallDestr := day;

      if( wasFirstEventForProdType and assigned( frmDailyStatusByProdType ) ) then
        frmDailyStatusByProdType.firstDestruction( day, h.prodType.productionTypeID )
      ;
    end
  ;


  procedure TFormMain.vaccinateHerd( h: THerd; wasFirstEventForProdType: boolean; day: integer );
    begin
      if( assigned( frmMap ) ) then frmMap.drawThisUnit( h );

      if( -1 = _firstOverallVacc ) then _firstOverallVacc := day;

      if( wasFirstEventForProdType and assigned( frmDailyStatusByProdType ) ) then
        frmDailyStatusByProdType.firstVaccination( day, h.prodType.productionTypeID )
      ;
    end
  ;


	procedure TFormMain.outbreakEnd( day: integer );
  	begin
			if( assigned( frmDailyStatusByProdType ) ) then frmDailyStatusByProdType.outbreakEnd( day );
 			if( assigned( frmDailyZoneStatusByProdType ) ) then frmDailyZoneStatusByProdType.outbreakEnd( day );
    end
  ;


  procedure TFormMain.dayComplete( day: integer );
  	const
    	delay: integer = 1000; // in milliseconds
  	begin
      //------------------------------------------------
      // This block can be used to cause a delay,
      // if the simulation is running too fast
      // (Delay could be dynamically set with a widget)
      //------------------------------------------------
    	(*
    	while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < _simDayStartTime + delay ) do
    		Application.ProcessMessages()
      ;
      *)
      //------------------------------------------------

      dbcout( '*** TFormMain.dayComplete...', DBSHOWMSG );
      if( assigned( frmDailyStatusByProdType ) ) then frmDailyStatusByProdType.updateForDay( day );
      if( assigned( frmDailyZoneStatusByProdType ) ) then frmDailyZoneStatusByProdType.updateForDay( day );
      if( assigned( frmIterationSummary ) ) then frmIterationSummary.updateForDay( day );

      dbcout( '*** TFormMain.dayComplete', DBSHOWMSG );
    end
  ;


  procedure TFormMain.iterationComplete( it: integer );
  	begin
      pbrIterations.Position := it + 1;
       if( assigned( frmDailyZoneStatusByProdType ) ) then frmDailyZoneStatusByProdType.iterationComplete();
    end
  ;


  procedure TFormMain.simComplete( success: boolean );
  	begin
    	pnlRunStatus.Hide();

      _simIsRunning := false;

      _iterationInProgress := -1;

      // These forms are updated dynamically, and should be informed when a simulation ends.
      if( assigned( frmMap ) ) then frmMap.updateSimComplete();
      if( assigned( frmDailyStatusByProdType ) ) then frmDailyStatusByProdType.updateSimComplete();
      if( assigned( frmDailyZoneStatusByProdType ) ) then frmDailyZoneStatusByProdType.updateSimComplete();
      if( assigned( frmIterationSummary ) ) then frmIterationSummary.updateSimComplete();

      // NOTE: Currently, these two forms should never be open when a simulation is in progress,
      // as they are not updated dynamically.
      // Bad things will happen if the updateSimComplete() events are called!
      if( assigned( frmOutputEvents ) ) then frmOutputEvents.updateSimComplete();
      if( assigned( frmOutputExposures ) ) then frmOutputExposures.updateSimComplete();

    	if( success ) then
        msgOK(
          tr( 'Simulation is complete!' ),
          SHORTMASTERCAPTION,
          IMGSuccess,
          self
        )
      else
      	msgOK(
        	tr( 'Simulation interrupted by user.' ),
          SHORTMASTERCAPTION,
          IMGInformation,
          self
        )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Trivial testing functions
//-----------------------------------------------------------------------------
  procedure TFormMain.BtnTestClick(Sender: TObject);
    begin
      _smScenario.herdList.debugProjected();
    end
  ;
//-----------------------------------------------------------------------------

  procedure TFormMain.WMNCActivate( var Msg: TWMNCActivate );
    begin
      if( not _windowsLocked ) then
        inherited
      else
        dbcout( 'Windows are locked: nonclient will not be activated. ***********', DBSHOWMSG )
      ;
    end
  ;


  procedure TFormMain.DatabaseActivityTimerTimer(Sender: TObject);
    begin
      // Ping the database every few minutes, in an ugly but hopefully
      // successful attempt to prevent the application from freezing up
      // if left unattended.
      dbcout( 'Timer has timed out.', true );
      
      if( ( nil <> _smdb ) and not( _simIsRunning ) ) then
        begin
          dbcout( 'Pinging the database at ' + TimeToStr( time() ), true );
          dbcout( _smdb.prodTypeCount, true );
        end
      ;
    end
  ;




end.
