program SpreadModel;
                        
(*
SpreadModel.dpr
----------------
Begin: 2004/07/15
Last revision: $Date: 2008/11/25 22:03:19 $ $Author: areeves $
Version: $Revision: 1.148 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2004 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{$INCLUDE Defs.inc}

{%ToDo 'SpreadModel.todo'}


(*
The Delphi interface overwrites user comments in the
"user" section.  Remember to copy this section back from a good version!!
(Also remember that comments in braces in the "uses" section have a
special purpose, so don't mess with them.)
*)


uses
  // Built-in units
  //---------------
  Windows,
  Forms,
  SysUtils,
  ComObj,
  Controls,

  // APHI General Purpose Delphi library
  //------------------------------------
  AppLog,
  ARMath,
  BasicGIS,
  CmdLine,
  ControlUtils,
  CStringList,
  CsvParser,
  DebugWindow,
  FunctionPointers,
  GuiStrUtils,
  I88n,
  ImageResources,
  IniHandler,
  MyDelphiArrayUtils,
  myDialogs,
  MyGraphicsUtils,
  MyStrUtils,
  Points,
  RegExpDefs,
  RegExpr,
  Resources,
  RoundToXReplacement_3c,
  SqlClasses,
  StringSuperList,
  UnicodeDev,
  USStrUtils,
  WindowsUtils,
  ZipFunctions,

  // APHI Delphi Library for Simulation Modeling
  //--------------------------------------------
  AphiRng,
  ChartFunction,
  RelFunction,
  ProbDensityFunctions,
  FunctionDictionary,
  Models,
  SimInput,

  // Simple Delphi Expat Wrapper
  //----------------------------
  Sdew,
  XMLReader,
  
  // QClasses: GPL-compatible data structures for use with Delphi
  //-------------------------------------------------------------
  QStringMaps,
  QIntegerMaps,
  QVectors,
  QLists,
  QOrderedDictionaries,

  // General-purpose forms and GUI components
  //-----------------------------------------
  //FIX ME put this guy in with the others
  FormFolderSelect,

  // Units for remote communication:
  // The path to RemoteMessenger should be specified in the Delphi Library path.
  //----------------------------------------------------------------------------
  RemoteMessenger,
  
  {$IFNDEF CONSOLEAPP}
    // General-purpose units (graphical)
    //----------------------------------
    DialogLongMessage in 'general_purpose_gui\DialogLongMessage.pas' {DialogLongMessage},
    FormProgress in 'general_purpose_gui\FormProgress.pas' {FormProgress},
    FrameAcceptCancel in 'general_purpose_gui\FrameAcceptCancel.pas' {FrameAcceptCancel: TFrame},
    FrameChartBase in 'general_purpose_gui\FrameChartBase.pas' {FrameChartBase: TFrame},
    FrameFileSelector in 'general_purpose_gui\FrameFileSelector.pas' {FrameFileSelector: TFrame},
    FrameStringGridBase in 'general_purpose_gui\FrameStringGridBase.pas' {FrameStringGridBase: TFrame},
    
    // FIX ME: Move these two forms to sm_forms: they are NOT general purpose!!
    FormAppUpdate in 'general_purpose_gui\FormAppUpdate.pas' {FormAppUpdate},
    FormRegistration in 'general_purpose_gui\FormRegistration.pas' {FormRegistration},

    // PDF/relational function editing, part of the APHI modeling library
    //-------------------------------------------------------------------
    FrameFunctionParams2 in 'libaphi_delphi_gui\function_editor\FrameFunctionParams2.pas' {FrameFunctionParams2: TFrame},
    FrameChartPointsEditor in 'libaphi_delphi_gui\function_editor\FrameChartPointsEditor.pas' {FrameChartPointsEditor: TFrame},
    FormChartPointsEditor2 in 'libaphi_delphi_gui\function_editor\FormChartPointsEditor2.pas' {FormChartPointsEditor2},
    FramePointEditorGrid in 'libaphi_delphi_gui\function_editor\FramePointEditorGrid.pas' {FramePointEditorGrid: TFrame},
    FrameFunctionEditor in 'libaphi_delphi_gui\function_editor\FrameFunctionEditor.pas' {FrameFunctionEditor: TFrame},
    
    // Model output displays from the APHI modeling library
    //-----------------------------------------------------
    FrameArrayConvergence in 'libaphi_delphi_gui\FrameArrayConvergence.pas' {FrameArrayConvergence: TFrame},
    FrameArrayHistogram in 'libaphi_delphi_gui\FrameArrayHistogram.pas' {FrameArrayHistogram: TFrame},
  {$ENDIF}

  RemoteDatabaseParams in 'sm_database\RemoteDatabaseParams.pas',

  {$IFNDEF CONSOLEAPP}  
    // The main form  
    //--------------
    FormMain in 'sm_forms\FormMain.pas' {FormMain},
  {$ENDIF}
  
  // Global enums and constants  
  //---------------------------
  FunctionEnums in 'sm_model_classes\FunctionEnums.pas',
  StringConsts in 'StringConsts.pas',
  StatusEnums in 'sm_model_classes\StatusEnums.pas',

  // NAADSM-specific model classes and data structures (non-graphical)  
  //------------------------------------------------------------------
  SMSimulationInput in 'sm_model_classes\SMSimulationInput.pas',
  SMDatabase in 'sm_database\SMDatabase.pas',
  OldDatabaseFns in 'sm_database\OldDatabaseFns.pas',
  VaccinationParams in 'sm_model_classes\VaccinationParams.pas',
  AirborneSpreadModel in 'sm_model_classes\AirborneSpreadModel.pas',
  ContactModel in 'sm_model_classes\ContactModel.pas',
  DestructionParams in 'sm_model_classes\DestructionParams.pas',
  Herd in 'sm_model_classes\Herd.pas',
  ProductionType in 'sm_model_classes\ProductionType.pas',
  ProductionTypeList in 'sm_model_classes\ProductionTypeList.pas',
  DetectionParams in 'sm_model_classes\DetectionParams.pas',
  RingVaccParams in 'sm_model_classes\RingVaccParams.pas',
  GlobalControlParams in 'sm_model_classes\GlobalControlParams.pas',
  ProductionTypePairList in 'sm_model_classes\ProductionTypePairList.pas',
  GlobalControlParamsList in 'sm_model_classes\GlobalControlParamsList.pas',
  ProductionTypePair in 'sm_model_classes\ProductionTypePair.pas',
  SMSimOutByProdType in 'sm_model_classes\SMSimOutByProdType.pas',
  SMOutputOptions in 'sm_model_classes\SMOutputOptions.pas',
  EventsAndExposures in 'sm_model_classes\EventsAndExposures.pas',
  SMScenario in 'sm_model_classes\SMScenario.pas',
  SMEpiCurves in 'sm_model_classes\SMEpiCurves.pas',
  CostParams in 'sm_model_classes\CostParams.pas',
  CustomOutputDefinitions in 'sm_model_classes\CustomOutputDefinitions.pas',
  Zone in 'sm_model_classes\Zone.pas',
  ZoneParams in 'sm_model_classes\ZoneParams.pas',
  SelectDailyOutputs in 'sm_model_classes\SelectDailyOutputs.pas',
  
  // Summary outputs
  //----------------
  SMSimulationStats in 'sm_model_classes\SMSimulationStats.pas',
  SMZoneStats in 'sm_model_classes\SMZoneStats.pas',
  OutputDescriptions in 'sm_model_classes\OutputDescriptions.pas',

  // XML parsing
  //------------
  ReadXMLInput in 'sm_xml_classes\ReadXMLInput.pas',
  xmlHerd in 'sm_xml_classes\xmlHerd.pas',
  DiseaseModelStatMethods in 'sm_xml_classes\DiseaseModelStatMethods.pas',
  Loc in 'sm_xml_classes\Loc.pas',  

  // Data structures for drawing zones
  //----------------------------------
  ZonePerimeter in 'ZonePerimeter.pas',
  
  // NAADSMap
  //---------
  NAADSMap in 'sm_naadsmap\NAADSMap.pas',

  {$IFNDEF CONSOLEAPP}
    // Scenario input forms and widgets  
    //---------------------------------
    FrameSMFunctionEditor in 'sm_widgets\FrameSMFunctionEditor.pas' {FrameSMFunctionEditor: TFrame},

    FormSMWizardBase in 'sm_forms\FormSMWizardBase.pas' {FormSMWizardBase},
    FormGeneralParams in 'sm_forms\FormGeneralParams.pas' {FormGeneralParams},
    FormProdType in 'sm_forms\FormProdType.pas' {FormProdType},
    FormProdTypeBase in 'sm_forms\FormProdTypeBase.pas' {FormProdTypeBase},
    FormSelectProdTypes in 'sm_forms\FormSelectProdTypes.pas' {FormSelectProdTypes},
    FormProdTypePairs in 'sm_forms\FormProdTypePairs.pas' {FormProdTypePairs},
    FormSelectProdTypePairs in 'sm_forms\FormSelectProdTypePairs.pas' {FormSelectProdTypePairs},
    
    FormDisease in 'sm_forms\FormDisease.pas' {FormDisease},
    FormDetection in 'sm_forms\FormDetection.pas' {FormDetection},
    FrameDetection in 'sm_forms\FrameDetection.pas' {FrameDetection: TFrame},
    FrameDestruction in 'sm_forms\FrameDestruction.pas' {FrameDestruction: TFrame},
    FormDestruction in 'sm_forms\FormDestruction.pas' {FormDestruction},
    
    FrameVaccination in 'sm_forms\FrameVaccination.pas' {FrameVaccination: TFrame},
    FormVaccination in 'sm_forms\FormVaccination.pas' {FormVaccination},
    FormPriorityBase in 'sm_forms\FormPriorityBase.pas' {FormPriorityBase},
    FormDestrPriority in 'sm_forms\FormDestrPriority.pas' {FormDestrPriority},
    FormVaccPriority in 'sm_forms\FormVaccPriority.pas' {FormVaccPriority},
    
    FrameDestrGlobal in 'sm_forms\FrameDestrGlobal.pas' {FrameDestrGlobal: TFrame},
    FormDestrGlobal in 'sm_forms\FormDestrGlobal.pas' {FormDestrGlobal},
    
    FrameVaccGlobal in 'sm_forms\FrameVaccGlobal.pas' {FrameVaccGlobal: TFrame},
    FormVaccGlobal in 'sm_forms\FormVaccGlobal.pas' {FormVaccGlobal},

    FrameTracing in 'sm_forms\FrameTracing.pas' {FrameTracing: TFrame},
    FormTracing in 'sm_forms\FormTracing.pas' {FormTracing},
    FrameTracingGlobal in 'sm_forms\FrameTracingGlobal.pas' {FrameTracingGlobal: TFrame},
    FormTracingGlobal in 'sm_forms\FormTracingGlobal.pas' {FormTracingGlobal},

    FormZoneCreation in 'sm_forms\FormZoneCreation.pas' {FormZoneCreation},
    FormZone in 'sm_forms\FormZone.pas' {FormZone},
    FrameZone in 'sm_forms\FrameZone.pas' {FrameZone: TFrame},
    FrameZoneProdTypeParams in 'sm_forms\FrameZoneProdTypeParams.pas' {FrameZoneProdTypeParams: TFrame},

    FormCostOptions in 'sm_forms\FormCostOptions.pas' {FormCostOptions},

    FrameCostsDestr in 'sm_forms\FrameCostsDestr.pas' {FrameCostsDestr: TFrame},
    FormCostsDestr in 'sm_forms\FormCostsDestr.pas' {FormCostsDestr},
    
    FrameCostsVacc in 'sm_forms\FrameCostsVacc.pas' {FrameCostsVacc: TFrame},
    FormCostsVacc in 'sm_forms\FormCostsVacc.pas' {FormCostsVacc},

    FrameCostsZoneProdTypeParams in 'sm_forms\FrameCostsZoneProdTypeParams.pas' {FrameCostsZoneProdTypeParams: TFrame},
    FrameCostsZones in 'sm_forms\FrameCostsZones.pas' {FrameCostsZones: TFrame},
    FormCostsZones in 'sm_forms\FormCostsZones.pas' {FormCostsZones},
    
    FormProdTypePairBase in 'sm_forms\FormProdTypePairBase.pas' {FormProdTypePairBase},
    
    FormAirborneSpread in 'sm_forms\FormAirborneSpread.pas' {FormAirborneSpread},
    FrameAirborneSpread in 'sm_forms\FrameAirborneSpread.pas' {FrameAirborneSpread: TFrame},
    FrameWindDirection in 'sm_widgets\FrameWindDirection.pas' {FrameWindDirection: TFrame},
    FormYesNoCollection in 'sm_forms\FormYesNoCollection.pas' {FormYesNoCollection},
    FrameDisease in 'sm_forms\FrameDisease.pas' {FrameDisease: TFrame},
    FormSpreadOptions in 'sm_forms\FormSpreadOptions.pas' {FormSpreadOptions},
    FormContactSpread in 'sm_forms\FormContactSpread.pas' {FormContactSpread},
    FrameContactSpread in 'sm_forms\FrameContactSpread.pas' {FrameContactSpread: TFrame},
    FrameContactSpreadMaster in 'sm_forms\FrameContactSpreadMaster.pas' {FrameContactSpreadMaster: TFrame},
    FrameHerdListEditor in 'sm_forms\FrameHerdListEditor.pas' {FrameHerdListEditor: TFrame},
    FormLatLonRange in 'sm_forms\FormLatLonRange.pas' {FormLatLonRange},
    FormHerdListEditor in 'sm_forms\FormHerdListEditor.pas' {FormHerdListEditor},
    FormOutputOptions in 'sm_forms\FormOutputOptions.pas' {FormOutputOptions},
    FormCustomOutputs in 'sm_forms\FormCustomOutputs.pas' {FormCustomOutputs},

    // Output forms and widgets
    //-------------------------
    FormIterationSummary in 'sm_forms\FormIterationSummary.pas' {FormIterationSummary},
    FormDailyStatusByProdType in 'sm_forms\FormDailyStatusByProdType.pas' {FormDailyStatusByProdType},
    FormDailyZoneStatusByProdType in 'sm_forms\FormDailyZoneStatusByProdType.pas' {FormDailyZoneStatusByProdType},
    FormMap in 'sm_forms\FormMap.pas' {FormMap},
    FrameOutputStatsTable in 'sm_forms\FrameOutputStatsTable.pas' {FrameOutputStatsTable: TFrame},
    FormOutputStats in 'sm_forms\FormOutputStats.pas' {FormOutputStats},
    FrameOutputStats in 'sm_forms\FrameOutputStats.pas' {FrameOutputStats: TFrame},
    FrameSummaryEpiCurves in 'sm_forms\FrameSummaryEpiCurves.pas' {FrameSummaryEpiCurves: TFrame},
    FormSummaryEpiCurves in 'sm_forms\FormSummaryEpiCurves.pas' {FormSummaryEpiCurves},
    FrameSummaryEpiCurveTable in 'sm_forms\FrameSummaryEpiCurveTable.pas' {FrameSummaryEpiCurveTable: TFrame},
    FrameDailyStatusByProdType in 'sm_forms\FrameDailyStatusByProdType.pas' {FrameDailyStatusByProdType: TFrame},
    FrameDailyZoneStatusByProdType in 'sm_forms\FrameDailyZoneStatusByProdType.pas' {FrameDailyZoneStatusByProdType: TFrame},
    FrameSingleEpiCurve in 'sm_forms\FrameSingleEpiCurve.pas' {FrameSingleEpiCurve: TFrame},
    FrameSingleCostCurve in 'sm_forms\FrameSingleCostCurve.pas' {FrameSingleCostCurve: TFrame},
    FrameSingleCostTable in 'sm_forms\FrameSingleCostTable.pas' {FrameSingleCostTable: TFrame},
    FrameEpiIterationSummary in 'sm_forms\FrameEpiIterationSummary.pas' {FrameEpiIterationSummary: TFrame},
    FrameCostIterationSummary in 'sm_forms\FrameCostIterationSummary.pas' {FrameCostIterationSummary: TFrame},
    FormSMOutputBase in 'sm_forms\FormSMOutputBase.pas' {FormSMOutputBase},
    DMOutputActionManager in 'sm_forms\DMOutputActionManager.pas' {DMOutputActionManager: TDataModule},
    FormOutputEvents in 'sm_forms\FormOutputEvents.pas' {FormOutputEvents},
    FormOutputExposures in 'sm_forms\FormOutputExposures.pas' {FormOutputExposures},
    FormScenarioComparison in 'sm_forms\FormScenarioComparison.pas' {FormScenarioComparison},

    // Miscellaneous forms and widgets  
    //--------------------------------
    // FIX ME: Move this guy
    DialogRunSimException in 'DialogRunSimException.pas' {DialogRunSimException},

    FormImport in 'sm_forms\FormImport.pas' {FormImport},
    FormExport in 'sm_forms\FormExport.pas' {FormExport},
    FormHerdExportOptions in 'sm_forms\FormHerdExportOptions.pas' {FormHerdExportOptions},
    FormLanguageSettings in 'sm_forms\FormLanguageSettings.pas' {FormLanguageSettings},
    FrameCredits in 'sm_forms\FrameCredits.pas' {FrameCredits: TFrame},

    {$IF Defined( CHEYENNE ) }
      FormAboutCheyenne in 'sm_forms\FormAboutCheyenne.pas' {FormAboutCheyenne},
      FormSplashCheyenne in 'sm_forms\FormSplashCheyenne.pas' {FormSplashCheyenne},
    {$ELSEIF Defined( LARAMIE ) }
      FormAboutLaramie in 'sm_forms\FormAboutLaramie.pas' {FormAboutLaramie},
      FormSplashLaramie in 'sm_forms\FormSplashLaramie.pas' {FormSplashLaramie},
    {$ELSE}
      FormAbout in 'sm_forms\FormAbout.pas' {FormAbout},
      FormSplash in 'sm_forms\FormSplash.pas' {FormSplash},
    {$IFEND}
{$ENDIF}

  // The unit that manages all of the heavy lifting
  ModelImplementation in 'ModelImplementation.pas'
;

{$R *.res}
{$R 'sm_translation\translation.res' 'sm_translation\translation.rc'}

var
  cmdParams: TCmdLine;
  {$IFDEF CONSOLEAPP}
    smdb: TSMDatabase;
    updateReason: TDBSchemaUpdateReason;
    dbUpdateResult: TDBCheckResult;
    sim: TSMSimulationInput;
    hList: THerdList;
    smScen: TSMScenario;
    runResult: integer;
    errorMessage: string;
    dbFileName: string;
    msg: string;
    stopReason: TStopReason;
    stopDay: integer;
    updateSuccess: boolean;
  {$ENDIF}


  {$IF Defined( CHEYENNE ) }
    {$IF Defined( LARAMIE ) }
      This block will lead to compiler errors if symbols for both
      experimental versions are defined. Note that these experimental
      versions are currently mutually exlusive, and only one symbol
      should be defined at a time.
    {$IFEND}
  {$IFEND}


  function requiredDllsOK( var msg: string ): boolean;
    var
      missingLibsMessage: string;
      missingLibs: integer;
      Sdew: TSdew;
      sdewLoaded: boolean;
      qClassesLoaded: boolean;

      rm: TRemoteMessenger;
      remoteLoaded: boolean;
    begin
      missingLibs := 0;
      result := true;

      Sdew := TSdew.createFromFile( nil );
      sdewLoaded := Sdew.LibLoaded;
      Sdew.free();

      rm := TRemoteMessenger.createAndTestDllOnly();
      remoteLoaded := rm.dllLoaded;
      rm.Free();

      qClassesLoaded := (
        qStringMapsDllLoaded( @missingLibsMessage )
      and
        qIntegerMapsDllLoaded( @missingLibsMessage )
      and
        qListsDllLoaded( @missingLibsMessage )
      and
        qVectorsDllLoaded( @missingLibsMessage )
      );

      dbcout( missingLibsMessage, true );

      // Check that the required DLLs are loaded
      //----------------------------------------
      if
        ( not( sssimLoaded ) )  // sssim.dll (cheyenne.dll, laramie.dll) cannot be found, or is the wrong version
      or
        ( not( gslLoaded ) ) // libgsl.dll cannot be found or is the wrong version
      or
        ( not( libAPHILoaded ) ) // libaphi.dll cannot be found or is the wrong version
      or
        ( not( gisFunctionsLoaded ) ) // libaphi.dll cannot be found or is the wrong version
      or
        ( not( sdewLoaded ) ) // sdew.dll cannot be found or is the wrong version
      or
        ( not( qClassesLoaded ) ) // qclasses.dll cannot be found or is the wrong version
      or
        ( not( remoteLoaded ) ) // remote.dll cannot be found or is the wrong version
      then
        begin
          result := false;

          if( not( sssimLoaded ) ) then inc( missingLibs ); // Count sssim as missing
          if( not( gslLoaded ) ) then inc( missingLibs ); // Count libgsl as missing
          if( not( libAPHILoaded ) or not( gisFunctionsLoaded ) ) then inc( missingLibs ); // count libaphi as missing
          if( not( sdewLoaded ) ) then inc( missingLibs ); // Count sdew.dll as missing
          if( not( qClassesLoaded ) ) then inc( missingLibs ); // count qclasses.dllHandle as missing

          {$IF Defined( CHEYENNE ) }
            if( not( sssimLoaded ) ) then msg := msg + 'cheyenne.dll';
          {$ELSEIF Defined( LARAMIE ) }
            if( not( sssimLoaded ) ) then msg := msg + 'laramie.dll';
          {$ELSE}
            if( not( sssimLoaded ) ) then msg := msg + 'sssim.dll';
          {$IFEND}

          if ( not( remoteLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'remote.dll'
              else
                msg := msg + ', remote.dll'
              ;
            end
          ;

          if( not( gslLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'libgsl.dll'
              else
                msg := msg + ', libgsl.dll'
              ;
            end
          ;

          if( not( libAPHILoaded ) or not( gisFunctionsLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'libaphi.dll'
              else
                msg := msg + ', libaphi.dll'
              ;
            end
          ;

          if ( not( sdewLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'sdew.dll'
              else
                msg := msg + ', sdew.dll'
              ;
            end
          ;

          if ( not( qClassesLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'qclasses.dll'
              else
                msg := msg + ', qclasses.dll'
              ;
            end
          ;

          if( 1 = missingLibs ) then
            msg := 'A required program library (' + msg + ') cannot be found or is out of date. '
          else
            msg := 'Required program libraries (' + msg + ') cannot be found or are out of date. '
          ;

          msg := msg + endl + 'To solve this problem, please reinstall the application, or check with the developers.';
        end
      ;
    end
  ;



  {$IFNDEF CONSOLEAPP}
  procedure launchGUI( cmdParams: TCmdLine {fileName: string = ''} );
    var
      msg: string;
      frm: TFormLanguageSettings;
    begin
      msg := '';

      Application.Initialize();

      // DON'T FORGET:  This value must be set manually.  It cannot be set to an
      // existing string constant.
      {$IF Defined( CHEYENNE ) }
        Application.Title := 'NAADSM-Cheyenne 3.1';
      {$ELSEIF Defined( LARAMIE ) }
        Application.Title := 'NAADSM-Laramie 3.1';
      {$ELSE}
        Application.Title := 'NAADSM 3.1';
      {$IFEND}

      // FIX ME: for now, only the GUI version of NAADSM/PC supports non-English language.
      if( TFormLanguageSettings.usePreferredLanguage() ) then
        begin
          dbcout( 'Using user-preferred language.', true );
          setLanguage( TFormLanguageSettings.preferredLanguage() );
        end
      else if( TFormLanguageSettings.useSystemLanguage() and languageSupported( systemLanguage() ) ) then
        begin
          dbcout( 'Using default system language.', true );
          setLanguage( systemLanguage() );
        end
      else // Use the form and ask the user
        begin
          frm := TFormLanguageSettings.create( nil, false, false );
          frm.ShowModal();
          setLanguage( frm.language );
          freeAndNil( frm );
        end
      ;

      // The Delphi IDE screws up this line once in a while.  It should read as follows:
      //Application.CreateForm( TFormMain, frmMain );
      Application.CreateForm( TFormMain, frmMain );

      {$IFDEF DEBUG}
        // Don't show the splash screen while debugging: it is annoying.
      {$ELSE}
        {$IF Defined( CHEYENNE ) }
          Application.CreateForm( TFormSplashCheyenne, frmSplash );
        {$ELSEIF Defined( LARAMIE ) }
          Application.CreateForm( TFormSplashLaramie, frmSplash );
        {$ELSE}
          Application.CreateForm( TFormSplash, frmSplash );
        {$IFEND}
      {$ENDIF}

       // Check for required libraries.
       // The application shouldn't/won't/can't run without them.
       if( not( requiredDllsOK( msg ) ) ) then
        begin
          if( nil <> frmSplash ) then
            begin
              if( frmSplash.Visible ) then frmSplash.Hide();
            end
          ;

          msgOK(
              msg,
              SHORTMASTERCAPTION,
              IMGWarning,
              frmMain
            );

          {$IFDEF DEBUG}
            dbcout( ssSimLoadErrors(), true );
            Application.Run();
          {$ELSE}
            exit;
          {$ENDIF}
        end
       else
        begin
          try
            frmMain.setCommandParams( cmdParams );
            Application.Run();
          except
            // do nothing
          end;
        end
      ;
    end
  ;
  {$ENDIF}


  procedure testUnits();
    begin
      ARMath.selfTest();
      BasicGIS.selfTest();
      MyStrUtils.selfTest();
    end
  ;

  procedure showVersion( extraMessage: string = '' );
    var
      vers: string;
    begin
      vers := 'Version ' + VERSIONNUMBER;
      if( 0 < length( BRANCHNAME ) ) then vers := vers + ' Branch ''' + BRANCHNAME + '''';
      cout( endl );
      cout( APPNAME );
      cout(  vers );
      cout( 'Copyright 2003 - 2008 Animal Population Health Institute at' );
      cout( 'Colorado State University and University of Guelph.' );
      cout( 'This is free software, released under the terms of the GNU General Public' );
      cout( 'License.  Please see the source or the following URL for copying conditions.' );
      cout( 'NAADSM home page: <http://www.naadsm.org>' + endl );

      if( '' <> extraMessage ) then
        begin
          cout( 'NOTE:' );
          cout( extraMessage + endl );
        end
      ;
    end
  ;

  procedure showHelp();
    begin
      cout( 'USAGE:' );
      cout( '--help, -h, -?:   Display this help message.' );
      cout( '--version, -v:    Display version information.' );
      cout( '--database <filename>, -d <filename>: ' );
      cout( '                  Run a simulation based on the scenario in the' );
      cout( '                  specified database file.  Required.' );
      cout( '--silent:         Run without writing any output to the console.' );
      cout( '                  Optional: if not present, output will be written' );
      cout( '                  to the console.' );
      cout( '--remote-database <ipaddress:port>, -rd <ipaddress:port>:' );
      cout( '                  Use the NAADSM Remote Database Server at the' );
      cout( '                  indicated address for output storage.  Optional:' );
      cout( '                  if not specified, the local database is used.  If' );
      cout( '                  this option is given, a scenario ID is required.' );
      cout( '--scenario-id <integer>, -si <integer>:' );
      cout( '                  Specify a scenario ID.  Required if using' );
      cout( '                  a remote database for output storage.' );
      cout( '--job-id <integer>, -ji <integer>:' );
      cout( '                  Specify a job ID.  Required if using' );
      cout( '                  a remote database for output storage.' );
      cout( '--random-seed <integer>, -rs <integer>:' );
      cout( '                  Optional.  Specify a random seed, overriding the' );
      cout( '                  option stored in the scenario database. If not' );
      cout( '                  given, the option specified in the scenario' );
      cout( '                  database is used.' );
      cout( '--stop <simulation end option>, -s <simulation end option>:' );
      cout( '                  End iterations when the specified condition is' );
      cout( '                  met.  Optional: if not specified, the default' );
      cout( '                  value is outbreakEnd (see below).' );
      cout( 'Simulation end options:' );
      cout( '  outbreakEnd:    End iterations when the outbreak is over and all' );
      cout( '                  controlmeasures are complete.' );
      cout( '  diseaseEnd:     End iterations when the active disease phase is' );
      cout( '                  over, regardless of pending control measures.' );
      cout( '  firstDetection: End iterations upon the first detection of any' );
      cout( '                  clinically infectious unit.' );
      cout( '  specDay x:      End iterations on day x, where x is a postiive' );
      cout( '                  integer value.' + endl );
    end
  ;


  {$IFDEF CONSOLEAPP}
  function handleCmdParams( cmdParams: TCmdLine; var dbFileName: string; extraMessage: string = '' ): boolean;
    var
      dbParam: string;
      stopParam: string;

      remoteDBParam: string;
      scenarioIDParam: string;
      jobIDParam: string;
      seedParam: string;
      temp: string;
    begin
      // Look for 'help' switch
      //------------------------
      if( cmdParams.hasSwitch( '--help' ) or cmdParams.hasSwitch( '-h' ) or cmdParams.hasSwitch( '-?' ) ) then
        begin
          showVersion( extraMessage );
          showHelp();
          result := false;
          exit;
        end
      ;

      // Look for 'version' switch
      //--------------------------
      if( cmdParams.hasSwitch( '--version' ) or cmdParams.hasSwitch( '-v' ) ) then
        begin
          showVersion( extraMessage );
          result := false;
          exit;
        end
      ;

      // Look for 'database' switch and its associated argument
      //-------------------------------------------------------
      if( cmdParams.hasSwitch( '--database' ) and cmdParams.hasSwitch( '-d' ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Ambiguous "--database" switch repetition. Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      ;

      if( cmdParams.hasSwitch( '--database' ) ) then
        dbParam := '--database'
      else if( cmdParams.hasSwitch( '-d' ) ) then
        dbParam := '-d'
      else
        dbParam := ''
      ;

      if( 0 = length( dbParam ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Required switch "--database" is missing.  Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      ;

      if(  1 <> cmdParams.getArgumentCount( dbParam ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Switch "' + dbParam + '" has the wrong number of arguments.  Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      ;

      dbFileName := cmdParams.getArgument( dbParam, 0 );


      // Look for (optional) 'stop' switch and its associated argument(s)
      //-----------------------------------------------------------------
      if( cmdParams.hasSwitch( '--stop' ) and (cmdParams.hasSwitch( '-s' ) ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Ambiguous "--stop" switch repetition. Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      ;

      if( cmdParams.hasSwitch( '--stop' ) ) then
        stopParam := '--stop'
      else if( cmdParams.hasSwitch( '-s' ) ) then
        stopParam := '-s'
      else
        stopParam := ''
      ;

      if( 0 = length( stopParam ) ) then
        stopReason := ssStartAndStopAtEndOfOutBreak
      else if( 0 = cmdParams.getArgumentCount( stopParam ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Switch "' + stopParam + '" has the wrong number of arguments.  Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      else
        begin
          if( 'outbreakend' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            stopReason := ssStartAndStopAtEndOfOutBreak
          else if( 'firstdetection' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            stopReason := ssStartAndStopAtFirstDetection
          else if( 'diseaseend' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            stopReason := ssStartAndStopAtDiseaseEnd
          else if( 'specday' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            begin
              stopReason := ssStartAndStopAtSpecificDay;
              
              if( -1 = strToInt( cmdParams.getSafeArgument( stopParam, 1, '-1' ) ) ) then
                begin
                  showVersion( extraMessage );
                  cout( 'Switch "' + stopParam + ' specDay" is missing an argument.  Use --help to view usage instructions.' + endl );
                  result := false;
                  exit;
                end
              else
                stopDay := strToInt( cmdParams.getArgument( stopParam, 1 ) )
              ;
            end
          else
            begin
              showVersion( extraMessage );
              cout( 'Unrecognized argument for the "' + stopParam + '" switch.  Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          ;
        end
      ;


      // Look for the optional random seed switch and its required value
      //----------------------------------------------------------------
      if( cmdParams.hasSwitch( '--random-seed' ) and cmdParams.hasSwitch( '-rs' ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Ambiguous "--random-seed" switch repetition. Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      ;

      if( cmdParams.hasSwitch( '--random-seed' ) ) then
        seedParam := '--random-seed'
      else if( cmdParams.hasSwitch( '-rs' ) ) then
        seedParam := '-rs'
      else
        seedParam := ''
      ;

      if( 0 = length( seedParam ) ) then
        remoteDBParams.randomSeed := -1
      else if( 1 <> cmdParams.getArgumentCount( seedParam ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Switch "' + seedParam + '" has the wrong number of arguments.  Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      else
        begin
          remoteDBParams.randomSeed := myStrToInt( cmdParams.getArgument( seedParam, 0 ), -1 );
          if( -1 = remoteDBParams.randomSeed ) then
            begin
              showVersion( extraMessage );
              cout( 'Switch "' + seedParam + '" does not have an integer argument.  Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          ;
        end
      ;


      // Look for the optional remote database switch and its required components
      //-------------------------------------------------------------------------
      if( cmdParams.hasSwitch( '--remote-database' ) and cmdParams.hasSwitch( '-rd' ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Ambiguous "--remote-database" switch repetition. Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      ;

      if( cmdParams.hasSwitch( '--remote-database' ) ) then
        remoteDBParam := '--remote-database'
      else if( cmdParams.hasSwitch( '-rd' ) ) then
        remoteDBParam := '-rd'
      else
        remoteDBParam := ''
      ;

      if( 0 = length( remoteDBParam ) ) then
        remoteDBParams.useRemoteDatabase := false
      else if( 1 <> cmdParams.getArgumentCount( remoteDBParam ) ) then
        begin
          showVersion( extraMessage );
          cout( 'Switch "' + remoteDBParam + '" has the wrong number of arguments.  Use --help to view usage instructions.' + endl );
          result := false;
          exit;
        end
      else
        begin
          remoteDBParams.useRemoteDatabase := true;
          // See if the argument has both an IP address and a port number
          //-------------------------------------------------------------
          temp := string( getElementStr( 0, ':', cmdParams.getArgument( remoteDBParam, 0 ) ) );

          // temp should be an IP address
          if( not( ExecRegExpr( RE_IP_ADDRESS, fixup( temp ) ) ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Switch "' + remoteDBParam + '" does not specify a valid IP address. Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          else
            remoteDBParams.remoteDatabaseHost := fixup( temp )
          ;

          temp := string( getElementStr( 1, ':', cmdParams.getArgument( remoteDBParam, 0 ) ) );

          // temp should be a port number
          if( 0 = myStrToInt( fixup( temp ) ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Switch "' + remoteDBParam + '" does not specify a valid port number. Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          else
            remoteDBParams.remoteDatabasePort := myStrToInt( fixup( temp ) )
          ;

          // See if the required job ID switch is present and correctly formatted
          //----------------------------------------------------------------------
          if( cmdParams.hasSwitch( '--job-id' ) and (cmdParams.hasSwitch( '-ji' ) ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Ambiguous "--job-id" switch repetition. Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          ;

          if( cmdParams.hasSwitch( '--job-id' ) ) then
            jobIDParam := '--job-id'
          else if( cmdParams.hasSwitch( '-ji' ) ) then
            jobIDParam := '-ji'
          else
            jobIDParam := ''
          ;

          if( 0 = length( jobIDParam ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Required switch --job-id is missing.  Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          else if( 1 <> cmdParams.getArgumentCount( jobIDParam ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Switch "' + jobIDParam + '" has the wrong number of arguments.  Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          else
            begin
              remoteDBParams.jobID := myStrToInt( cmdParams.getArgument( jobIDParam, 0 ) );
              if( 0 = remoteDBParams.jobID ) then
                begin
                  showVersion( extraMessage );
                  cout( 'Switch "' + jobIDParam + '" does not have an integer argument.  Use --help to view usage instructions.' + endl );
                  result := false;
                  exit;
                end
              ;
            end
          ;

          // See if the required scenario ID switch is present and correctly formatted
          //--------------------------------------------------------------------------
          if( cmdParams.hasSwitch( '--scenario-id' ) and (cmdParams.hasSwitch( '-si' ) ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Ambiguous "--scenario-id" switch repetition. Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          ;

          if( cmdParams.hasSwitch( '--scenario-id' ) ) then
            scenarioIDParam := '--scenario-id'
          else if( cmdParams.hasSwitch( '-si' ) ) then
            scenarioIDParam := '-si'
          else
            scenarioIDParam := ''
          ;

          if( 0 = length( scenarioIDParam ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Required switch --scenario-id is missing.  Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          else if( 1 <> cmdParams.getArgumentCount( scenarioIDParam ) ) then
            begin
              showVersion( extraMessage );
              cout( 'Switch "' + scenarioIDParam + '" has the wrong number of arguments.  Use --help to view usage instructions.' + endl );
              result := false;
              exit;
            end
          else
            begin
              remoteDBParams.scenarioID := myStrToInt( cmdParams.getArgument( scenarioIDParam, 0 ) );
              if( 0 = remoteDBParams.scenarioID ) then
                begin
                  showVersion( extraMessage );
                  cout( 'Switch "' + scenarioIDParam + '" does not have an integer argument.  Use --help to view usage instructions.' + endl );
                  result := false;
                  exit;
                end
              ;
            end
          ;
        end
      ;

      // If we make it this far, everything is OK.
      result := true;
    end
  ;
  {$ENDIF}

begin // MAIN
  {$IFDEF DEBUG}
    setDEBUGGING( true );
  {$ELSE}
    setDEBUGGING( false );
  {$ENDIF}

  {$IFDEF APPLOG}
    setUseAppLog( true );
    openAppLog( 'naadsm.log' );
  {$ELSE}
    setUseAppLog( false );
  {$endif}

  {$IFDEF TESTING}
    testUnits();
  {$ENDIF}

  {$IFDEF CONSOLEAPP}
    // Attempt to launch the app as a console application
    Application.Initialize(); // Needed here to initialize *something* for the TSql classes.

    // See longer comment below.
    consoleSilent := false;

    if( 0 = ParamCount ) then
      begin
        showVersion();
        cout( 'Not enough switches.  Use --help to view the usage instructions.' + endl );
        exit;
      end
    ;
    // Otherwise...

    cmdParams := TCmdLine.create();

    // If debugSilent is true, nothing will ever be displayed.
    // If the console version of NAADSM is run as a service
    // (e.g. by the Atriplex distributed computing system <http://www.atriplex.org>),
    // any attempts to write output to the console will result in very ugly, often unexpected error windows.
    // Use the --silent switch in these situations to prevent them.
    consoleSilent := cmdParams.hasSwitch( '--silent' );

    // Check for required libraries.  The program shouldn't/won't/can't run without them.
    //-----------------------------------------------------------------------------------
    if( not( requiredDllsOK( msg ) ) ) then
      begin
        handleCmdParams( cmdParams, dbFileName, msg );
        cmdParams.free();
        exit;
      end
    ;

    if( handleCmdParams( cmdParams, dbFileName ) ) then
      begin
        cmdParams.free();
        showVersion();

        // Check that the file exists.
        //----------------------------
        if( not( fileExists( dbFileName ) ) ) then
          begin
            cout( 'The scenario file "' + dbFileName + '" does not exist.' + endl );
            exit;
          end
        ;

        // Create the remote database manager, if necessary
        //-------------------------------------------------
        if( remoteDBParams.useRemoteDatabase ) then
          _rm := TRemoteMessenger.create( remoteDBParams.remoteDatabaseHost, remoteDBParams.remoteDatabasePort, true )
        else
          _rm := nil
        ;

        // Open the database file and create the SMDatabase object.
        //---------------------------------------------------------
        smdb := TSMDatabase.create( dbFileName, DBOpen, nil, nil );
        if( smdb.isReadOnly ) then
          begin
            cout( 'This scenario file is read-only.  NAADSM requires write access if you wish to modify or run this scenario.' + endl );
            smdb.close();
            deleteFile( smdb.workingDBFileName );
            freeAndNil( smdb );
            exit;
          end
        ;

        // Check the database schema.
        //---------------------------
        if( not( smdb.indexExists( 'dynHerd_PK', 'dynHerd' ) ) ) then
          begin
            cout( 'The schema of this scenario database has been altered.  NAADSM cannot run this scenario.' + endl );
            smdb.close();
            deleteFile( smdb.workingDBFileName );
            freeAndNil( smdb );
            exit;
          end
        ;

        // Check the database version...
        //------------------------------
        updateReason := DBUpdateUnspecified;
        dbUpdateResult := smdb.checkVersion( updateReason );
        if( not( dbUpdateResult in [DBVersionCurrent, DBVersionUpdated] ) ) then
          begin
            cout( 'The version of the scenario database is not recognized or cannot be updated.  This version of NAADSM cannot run this scenario.' + endl );
            smdb.close();
            deleteFile( smdb.workingDBFileName );
            freeAndNil( smdb );
            exit;
          end
        ;

        // ...and update if necessary.
        //----------------------------
        if( DBVersionUpdated = dbUpdateResult ) then
          begin
            smdb.updateSchema( updateSuccess );
            if( not( updateSuccess ) ) then
              begin
                cout( 'An error occurred, and this database was not updated.  This version of NAADSM cannot run this scenario.  If the problem persists, please contact the developers.' + endl );
                smdb.close();
                deleteFile( smdb.workingDBFileName );
                freeAndNil( smdb );
                exit;
              end
            ;
          end
        ;

        // Clear any output.
        //------------------
        smdb.initializeAllOutputRecords();

        // Set the remote seed, if necessary.
        //-----------------------------------
        if( -1 <> remoteDBParams.randomSeed ) then
          smdb.execute( 'UPDATE `inGeneral` SET `useFixedRandomSeed` = true, `randomSeed` = ' + intToStr( remoteDBParams.randomSeed ) )
        ;

        // Create the objects.
        //--------------------
        sim := TSMSimulationInput.create( smdb );
        hList := THerdList.create( smdb, sim );
        smScen := TSMScenario.create( sim, hList );

        // Record the number of days specified and the reason for stopping the simulation.
        //--------------------------------------------------------------------------------
        smScen.simInput.simStopReason := stopReason;
        smdb.simStopReason := stopReason;

        if( ssStartAndStopAtSpecificDay = stopReason ) then
          begin
            smScen.simInput.simDays := stopDay;
            smdb.simDays := stopDay;
          end
        ;

        // Launch the simulation.
        //-----------------------
        cout( 'Launching simulation...' );
        runResult := startSim(
          smScen.simInput,
          smScen.herdList,
          smdb,
          errorMessage
        );

        // Check the result.
        //------------------
        case runResult of
          ERRRUNSIMEXCEPTION:
            begin
              cout( 'An exception occurred:' + endl + errorMessage + endl );
              cout( 'This simulation did not run to completion.' + endl );
            end
          ;
          ERRINVALIDSCENARIO:
            begin
              cout( 'Problems were found with this scenario.  These problems must be corrected before this scenario can be run:' + endl );
              cout( errorMessage + endl );
            end
          ;
          ERRCANNOTWRITEFILES:
            begin
              cout(
                'Temporary files required to launch this simulation were not written.  Please ensure that you have'
                + ' adequate hard disk space and sufficient permissions to write files to your hard disk.  For'
                + ' further assistance, please contact the developers.' + endl
              );
            end
          ;
          ERRNONE:
            begin
              cout( 'Simulation complete.' + endl );
            end
          ;
          ERRNOTSET:
            begin
              // ignore this.
            end
          ;
        end;
        // Save the database file.
        //------------------------
        cout( 'Saving database...' + endl );
        smdb.save();

        // Clean up.
        //----------
        cout( 'Freeing database...' + endl );
        if( nil <> smdb ) then
          freeAndNil( smdb )
        ;

        cout( 'Freeing scenario....' + endl );
        if( nil <> smScen ) then
          freeAndNil( smScen )
        ;

        cout( 'Freeing _rm...' + endl );
        if( nil <> _rm ) then
          freeAndNil( _rm )
        ;

        // Go home.
        //---------
        cout( 'Exiting.' );
        exit;
      end
    else
      // Don't do anything: just let the app exit on its own.
    ;
  {$ELSE}
    // Launch the GUI
    cmdParams := TCmdLine.create();
    launchGUI( cmdParams );
    cmdParams.Free();
  {$ENDIF}

  {$IFDEF APPLOG}
    closeAppLog();
  {$endif}


end.
