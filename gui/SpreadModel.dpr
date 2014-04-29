program SpreadModel;
                        
(*
SpreadModel.dpr
----------------
Begin: 2004/07/15
Last revision: $Date: 2013-06-27 19:11:19 $ $Author: areeves $
Version: $Revision: 1.164.4.18 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2004 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{$INCLUDE Defs.inc}

(*
The Delphi interface overwrites user comments in the
"user" section.  Remember to copy this section back from a good version!!
(Also remember that comments in braces in the "uses" section have a
special purpose, so don't mess with them.)
*)


uses
  // Built-in units (including optional EurekaLog)
  //----------------------------------------------
  Windows,
  Forms,
  SysUtils,
  StrUtils,
  ComObj,
  Controls,

  // APHI General Purpose Delphi library
  // See http://www.naadsm.org/opensource/generalpurpose
  //----------------------------------------------------
  AppLog,
  ARMath,
  BasicGIS,
  CmdLine,
  ControlUtils,
  CStringList,
  CsvParser,
  DebugWindow,
  FunctionPointers,
  I88n,
  ImageResources,
  IniHandler,
  MyDelphiArrayUtils,
  MyDialogs,
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
  WindowsUtils,
  ZipFunctions,

  // APHI Delphi Library for Simulation Modeling
  // See http://www.naadsm.org/opensource/libaphi
  //---------------------------------------------
  AphiRng,
  ChartFunction,
  RelFunction,
  ProbDensityFunctions,
  FunctionDictionary,
  Models,
  SimInput,
  ModelDatabase,

  // Simple Delphi Expat Wrapper
  // See http://www.naadsm.org/opensource/sdew
  //------------------------------------------
  Sdew,
  XMLReader,

  // Proj.4 Delphi wrapper
  // See http://www.naadsm.org/opensource/proj4wrapper
  //--------------------------------------------------
  Proj4,

  // QClasses: GPL-compatible data structures for use with Delphi
  // See http://www.naadsm.org/opensource/qclasses
  //-------------------------------------------------------------
  QStringMaps,
  QIntegerMaps,
  QVectors,
  QLists,
  QOrderedDictionaries,

  // Units for remote communication:
  // The path to RemoteMessenger should be specified in the Delphi Library path.
  // See http://www.naadsm.org/opensource/remotemessenger
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
    FormFolderSelect in 'general_purpose_gui\FormFolderSelect.pas' {FormFolderSelect},

    // PDF/relational function editing, part of the APHI modeling library
    //-------------------------------------------------------------------
    FrameFunctionParams2 in 'libaphi_delphi_gui\function_editor\FrameFunctionParams2.pas' {FrameFunctionParams2: TFrame},
    FrameChartPointsEditor in 'libaphi_delphi_gui\function_editor\FrameChartPointsEditor.pas' {FrameChartPointsEditor: TFrame},
    FormFunctionEditor in 'libaphi_delphi_gui\function_editor\FormFunctionEditor.pas' {FormFunctionEditor},
    FramePointEditorGrid in 'libaphi_delphi_gui\function_editor\FramePointEditorGrid.pas' {FramePointEditorGrid: TFrame},
    FrameFunctionEditor in 'libaphi_delphi_gui\function_editor\FrameFunctionEditor.pas' {FrameFunctionEditor: TFrame},
    
    // Model output displays from the APHI modeling library
    //-----------------------------------------------------
    FrameArrayHistogram in 'libaphi_delphi_gui\FrameArrayHistogram.pas' {FrameArrayHistogram: TFrame},

    // The main form  
    //--------------
    FormMain in 'sm_forms\FormMain.pas' {FormMain},
  {$ENDIF}
  
  // Global enums and constants  
  //---------------------------
  FunctionEnums in 'sm_model_classes\FunctionEnums.pas',
  StringConsts in 'StringConsts.pas',
  StatusEnums in 'sm_model_classes\StatusEnums.pas',
  SMI88nSettings in 'SMI88nSettings.pas',

  // NAADSM-specific model classes and data structures (non-graphical)  
  //------------------------------------------------------------------
  SMSimulationInput in 'sm_model_classes\SMSimulationInput.pas',
  SMDatabase in 'sm_database\SMDatabase.pas',
  RemoteDatabaseParams in 'sm_database\RemoteDatabaseParams.pas',
  OldDatabaseFns in 'sm_database\OldDatabaseFns.pas',
  VaccinationParams in 'sm_model_classes\VaccinationParams.pas',
  AirborneSpreadParams in 'sm_model_classes\AirborneSpreadParams.pas',
  ContactSpreadParams in 'sm_model_classes\ContactSpreadParams.pas',
  DestructionParams in 'sm_model_classes\DestructionParams.pas',
  TracingParams in 'sm_model_classes\TracingParams.pas',
  TestingParams in 'sm_model_classes\TestingParams.pas',
  Herd in 'sm_model_classes\Herd.pas',
  HerdControlActivities in 'sm_model_classes\HerdControlActivities.pas',
  ProductionType in 'sm_model_classes\ProductionType.pas',
  ProductionTypeList in 'sm_model_classes\ProductionTypeList.pas',
  DetectionParams in 'sm_model_classes\DetectionParams.pas',
  RingVaccParams in 'sm_model_classes\RingVaccParams.pas',
  GlobalControlParams in 'sm_model_classes\GlobalControlParams.pas',
  ProductionTypePairList in 'sm_model_classes\ProductionTypePairList.pas',
  GlobalControlParamsList in 'sm_model_classes\GlobalControlParamsList.pas',
  ProductionTypePair in 'sm_model_classes\ProductionTypePair.pas',
  SMSimOutByProdType in 'sm_model_classes\SMSimOutByProdType.pas',
  HerdRandomizationOptions in 'sm_model_classes\HerdRandomizationOptions.pas',
  SMOutputOptions in 'sm_model_classes\SMOutputOptions.pas',
  EventsAndExposures in 'sm_model_classes\EventsAndExposures.pas',
  SMScenario in 'sm_model_classes\SMScenario.pas',
  SMEpiCurves in 'sm_model_classes\SMEpiCurves.pas',
  CostParams in 'sm_model_classes\CostParams.pas',
  CustomOutputDefinitions in 'sm_model_classes\CustomOutputDefinitions.pas',
  Zone in 'sm_model_classes\Zone.pas',
  ProdTypeZoneParams in 'sm_model_classes\ProdTypeZoneParams.pas',

  // Simulation outputs
  //-------------------
  IterationOutputs in 'sm_output_classes\IterationOutputs.pas',
  OutputDescriptions in 'sm_output_classes\OutputDescriptions.pas',
  SelectDailyOutputs in 'sm_output_classes\SelectDailyOutputs.pas',

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

    FormInitialUnitOptions in 'sm_forms\FormInitialUnitOptions.pas'  {FormInitialUnitOptions},

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
    FrameTracingHerdExam in 'sm_forms\FrameTracingHerdExam.pas' {FrameTracingHerdExam: TFrame},
    FormTracingHerdExam in 'sm_forms\FormTracingHerdExam.pas' {FormTracingHerdExam},
    FrameTracingTesting in 'sm_forms\FrameTracingTesting.pas' {FrameTracingTesting: TFrame},
    FormTracingTesting in 'sm_forms\FormTracingTesting.pas' {FormTracingTesting},

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
    FrameDailyStatusByProdTypeDiseaseStats in 'sm_forms\FrameDailyStatusByProdTypeDiseaseStats.pas' {FrameDailyStatusByProdTypeDiseaseStats: TFrame},
    FrameDailyStatusByProdTypeDetectionStats in 'sm_forms\FrameDailyStatusByProdTypeDetectionStats.pas' {FrameDailyStatusByProdTypeDetectionStats: TFrame},
    FrameDailyStatusByProdTypeControlStats in 'sm_forms\FrameDailyStatusByProdTypeControlStats.pas' {FrameDailyStatusByProdTypeControlStats: TFrame},    
    
    FormDailyZoneStatusByProdType in 'sm_forms\FormDailyZoneStatusByProdType.pas' {FormDailyZoneStatusByProdType},
    FrameDailyZoneStatusByProdType in 'sm_forms\FrameDailyZoneStatusByProdType.pas' {FrameDailyZoneStatusByProdType: TFrame},
    
    FormMap in 'sm_forms\FormMap.pas' {FormMap},
    FrameOutputStatsTable in 'sm_forms\FrameOutputStatsTable.pas' {FrameOutputStatsTable: TFrame},
    FormOutputStats in 'sm_forms\FormOutputStats.pas' {FormOutputStats},
    FrameOutputStats in 'sm_forms\FrameOutputStats.pas' {FrameOutputStats: TFrame},

    FormSummaryEpiCurves in 'sm_forms\FormSummaryEpiCurves.pas' {FormSummaryEpiCurves},
    FrameSummaryEpiCurves in 'sm_forms\FrameSummaryEpiCurves.pas' {FrameSummaryEpiCurves: TFrame},
    FrameSummaryEpiCurveTable in 'sm_forms\FrameSummaryEpiCurveTable.pas' {FrameSummaryEpiCurveTable: TFrame},

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
    DialogRunSimException in 'sm_forms\DialogRunSimException.pas' {DialogRunSimException},

    FormAppUpdate in 'sm_forms\FormAppUpdate.pas' {FormAppUpdate},
    FormRegistration in 'sm_forms\FormRegistration.pas' {FormRegistration},

    FormImport in 'sm_forms\FormImport.pas' {FormImport},
    FormExport in 'sm_forms\FormExport.pas' {FormExport},
    FormHerdExportOptions in 'sm_forms\FormHerdExportOptions.pas' {FormHerdExportOptions},
    FormLanguageSettings in 'sm_forms\FormLanguageSettings.pas' {FormLanguageSettings},
    FormRegionalSettings in 'sm_forms\FormRegionalSettings.pas' {FormRegionalSettings},

    FormAboutExperimental in 'sm_forms\FormAboutExperimental.pas' {FormAboutExperimental},
    FormSplashExperimental in 'sm_forms\FormSplashExperimental.pas' {FormSplashExperimental},
    FormAbout in 'sm_forms\FormAbout.pas' {FormAbout},
    FormSplash in 'sm_forms\FormSplash.pas' {FormSplash},
	
	  HerdKML in 'sm_model_classes\HerdKML.pas',
{$ENDIF}

  // Exception handling, with the (optional) EurekaLog
  SMExceptionHandler in 'SMExceptionHandler.pas',

  // The unit that manages all of the heavy lifting
  //-----------------------------------------------
  NAADSMLibrary in 'NAADSMLibrary.pas',
  NAADSMLibraryTypes in 'NAADSMLibraryTypes.pas'
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
    exportXmlOnly: boolean;
    updateSuccess: boolean;
  {$ENDIF}


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
        ( not( naadsmLibLoaded ) )  // naadsm.dll (cheyenne.dll, laramie.dll, riverton.dll) cannot be found, or is the wrong version
      or
        ( not( pdfGslFnsLoaded() ) ) // libgsl.dll cannot be found or is the wrong version
      or
        ( not( pdfAphiFnsLoaded() ) ) // libaphi.dll cannot be found or is the wrong version
      or
        ( not( projLibLoaded() ) ) // proj.dll cannot be found
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

          if( not( naadsmLibLoaded ) ) then inc( missingLibs ); // Count naadsm as missing
          if( not( pdfGslFnsLoaded() ) ) then inc( missingLibs ); // Count libgsl as missing
          if( not( pdfAphiFnsLoaded() ) or not( gisFunctionsLoaded ) ) then inc( missingLibs ); // count libaphi as missing
          if( not( projLibLoaded() ) ) then inc( missingLibs ); // count proj.dll as missing
          if( not( sdewLoaded ) ) then inc( missingLibs ); // Count sdew.dll as missing
          if( not( qClassesLoaded ) ) then inc( missingLibs ); // count qclasses.dllHandle as missing
          if( not( naadsmLibLoaded ) ) then msg := msg + SIM_DLL_NAME;

          if ( not( remoteLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'remote.dll'
              else
                msg := msg + ', remote.dll'
              ;
            end
          ;

          if( not( pdfGslFnsLoaded() ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'libgsl.dll'
              else
                msg := msg + ', libgsl.dll'
              ;
            end
          ;

          if( not( pdfAphiFnsLoaded() ) or not( gisFunctionsLoaded ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'libaphi.dll'
              else
                msg := msg + ', libaphi.dll'
              ;
            end
          ;

          if( not( projLibLoaded() ) ) then
            begin
              if( 0 = length( msg ) ) then
                msg := 'proj.dll'
              else
                msg := msg + ', proj.dll'
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
            msg := ansiReplaceStr( tr( 'A required program library (xyz) cannot be found or is out of date.' ), 'xyz', msg )
          else
            msg := ansiReplaceStr( tr( 'Required program libraries (xyz) cannot be found or are out of date.' ), 'xyz', msg )
          ;

          msg := msg + endl + tr( 'To solve this problem, please reinstall the application, or check with the developers.' );
        end
      ;
    end
  ;



  {$IFNDEF CONSOLEAPP}
  procedure launchGUI( cmdParams: TCmdLine {fileName: string = ''} );
    var
      msg: string;
      {$IFNDEF ENGLISHONLY}
        frm: TFormLanguageSettings;
      {$ENDIF}
    begin
      msg := '';

      Application.Initialize();

      {$IFDEF ENGLISHONLY}
        setLanguage( I88nEnglish );
      {$ELSE}
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
            frm.Free(); // parent is nil, so use Free() instead of Release()!
          end
        ;
      {$ENDIF}

      // Set up the main window
      // The Delphi IDE screws up this line once in a while.  It should read as follows:
      //Application.CreateForm( TFormMain, frmMain );
      Application.CreateForm( TFormMain, frmMain );

      // Set up the window for displaying warnings generated by the DLL (declared in NAADSMLibrary.pas).
      // With any luck, it will never be used.
      Application.CreateForm( TDialogLongMessage, frmDllWarnings );
      frmDllWarnings.caption := tr( 'Messages generated during simulation run' );
      frmDllWarnings.header :=
        tr( 'The following messages were generated by the running simulation.' )
          + ' ' + tr( 'These may simply indicate warnings to the user.' )
          + ' ' + tr( 'It is also possible that they indicate unanticipated error conditions, which might affect the validity of simulation output.' )
          + ' ' + tr( 'If you believe that these messages represent errors, please copy the information below and send it to the NAADSM Development Team.' )
          + ' ' + tr( 'Thank you for your assistance!' )
      ;
      //frmDllWarnings.BorderIcons := [ biSystemMenu, biMaximize ];
      frmDllWarnings.hide();


      {$IFDEF DEBUG}
        // Don't show the splash screen while debugging: it is annoying.
      {$ELSE}
        if( IS_EXPERIMENTAL ) then
          Application.CreateForm( TFormSplashExperimental, frmSplashExperimental )
        else
          Application.CreateForm( TFormSplash, frmSplash )
        ;
      {$ENDIF}

       // Check for experimental versions.
       // The application should not run if there's a problem.
       if( not( experimentalVersionDefinitionsOK( msg ) ) ) then
        begin
          if( nil <> frmSplash ) then
            begin
              if( frmSplash.Visible ) then frmSplash.Hide();
            end
          ;
          if( nil <> frmSplashExperimental ) then
            begin
              if( frmSplashExperimental.Visible ) then frmSplashExperimental.Hide();
            end
          ;

          msgOK(
            msg,
            SHORTMASTERCAPTION,
            IMGWarning,
            frmMain
          );

          {$IFDEF DEBUG}
            dbcout( naadsmLibLoadErrors(), true );
            Application.Run();
          {$ELSE}
            exit;
          {$ENDIF}
        end

       // Check for required libraries.
       // The application shouldn't/won't/can't run without them.
       else if( not( requiredDllsOK( msg ) ) ) then
        begin
          if( nil <> frmSplash ) then
            begin
              if( frmSplash.Visible ) then frmSplash.Hide();
            end
          ;
          if( nil <> frmSplashExperimental ) then
            begin
              if( frmSplashExperimental.Visible ) then frmSplashExperimental.Hide();
            end
          ;

          msgOK(
            msg,
            SHORTMASTERCAPTION,
            IMGWarning,
            frmMain
          );

          {$IFDEF DEBUG}
            dbcout( naadsmLibLoadErrors(), true );
            Application.Run();
          {$ELSE}
            exit;
          {$ENDIF}
        end
       else
        begin
          try
            outputDescriptionList := TOutputDescriptionList.create();

            frmMain.setCommandParams( cmdParams );
            Application.Run();

            freeAndNil( outputDescriptionList );
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
      cout( 'Copyright ' + COPYRIGHTDATES + ' NAADSM Development Team' );
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
      cout( '--export, -e:     Generate XML files from the specfied database.' );
      cout( '                  Optional: if specified, the simulation will not' );
      cout( '                  run, but scenario and unit fileswill be generated' );
      cout( '                  in the same directory and with the same file name' );
      cout( '                  as the specified database.  If scenario' );
      cout( '                  parameters are not valid, then no files will be' );
      cout( '                  produced, and an error message may be displayed.' );
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
      cout( '  specDay x:      End iterations on day x, where x is a positive' );
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


      // Look for the (optional) 'export' switch
      //----------------------------------------
      if( cmdParams.hasSwitch( '--export' ) or cmdParams.hasSwitch( '-e' ) ) then
        exportXmlOnly := true
      else
        exportXmlOnly := false
      ;

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
        stopReason := ssStopAtEndOfOutBreak
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
            stopReason := ssStopAtEndOfOutBreak
          else if( 'firstdetection' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            stopReason := ssStopAtFirstDetection
          else if( 'diseaseend' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            stopReason := ssStopAtDiseaseEnd
          else if( 'specday' = lower( cmdParams.getArgument( stopParam, 0 ) ) ) then
            begin
              stopReason := ssStopAtSpecificDay;
              
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


  procedure exportModelFromConsole();
    var
      sfn, hfn: string;
      errMsg: string;
      success: boolean;
    begin
      // Create the objects.
      //--------------------
      sim := TSMSimulationInput.create( smdb );
      hList := THerdList.create( smdb, sim );

      if
        sim.isValid( true, hList, @errMsg )
      and
        hList.isValid( @errMsg )
      then
        begin
          sfn := stripExtension( smdb.permanentDBFileName ) + '-scenario.xml';
          hfn := stripExtension( smdb.permanentDBFileName ) + '-units.xml';

          cout( 'Writing scenario XML file...' );
          success := sim.writeXMLFile( sfn, true, stopReason, stopDay, @errMsg );

          if( success ) then
            begin
              cout( 'Writing unit XML file...' );
              success := hList.writeXMLFile( hfn, sim.herdRandomizationOptions, false, @errMsg );
            end
          ;

          if ( not success ) then
            begin
              cout( 'XML files could not be written.  Please check permissions and available disk space:' + endl );
              cout( errMsg + endl );
              deleteFile( sfn );
              deleteFile( hfn );
            end
          ;
        end
      else
        begin
          cout( 'The scenario contained in this database is not valid and could not be exported:' + endl );
          cout( errMsg + endl );
        end
      ;

      // Clean up.
      //----------
      freeAndNil( hList );
      freeAndNil( sim );
      freeAndNil( smdb );

      cout( 'Done.' );
    end
  ;


  procedure runModelFromConsole();
    begin
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

      if( ssStopAtSpecificDay = stopReason ) then
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
      //cout( 'Freeing database...' + endl );
      freeAndNil( smdb );

      //cout( 'Freeing scenario....' + endl );
      freeAndNil( smScen );

      //cout( 'Freeing _rm...' + endl );
      if( nil <> _rm ) then
        freeAndNil( _rm )
      ;

      // Go home.
      //---------
      cout( 'Done.' );
    end
  ;
  {$ENDIF}

begin // MAIN
  {$IFDEF DEBUG}
    setDEBUGGING( true, true );
    {$IFDEF EUREKALOG}
      dbcout( 'Compiled with EurekaLog.', true );
    {$ELSE}
      dbcout( 'EurekaLog is not included.', true );
    {$ENDIF}
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

    // Check for experimental versions.  The application should not run if there's a problem.
    //---------------------------------------------------------------------------------------
    if( not( experimentalVersionDefinitionsOK( msg ) ) ) then
      begin
        handleCmdParams( cmdParams, dbFileName, msg );
        cmdParams.free();
        exit;
      end
    ;

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
            freeAndNil( smdb );
            exit;
          end
        ;

        // Check the database schema.
        //---------------------------
        if( not( smdb.indexExists( 'dynHerd_PK', 'dynHerd' ) ) ) then
          begin
            cout( 'The schema of this scenario database has been altered.  NAADSM cannot run this scenario.' + endl );
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
                freeAndNil( smdb );
                exit;
              end
            ;
          end
        ;

        if( exportXmlOnly ) then
          exportModelFromConsole()
        else
          runModelFromConsole()
        ;
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
