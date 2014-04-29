// Begin 3/5/08

program smcompare;

  uses
    Forms,

    I88n,
    DebugWindow,
    MyStrUtils,
    USStrUtils,
    GuiStrUtils,
    Processes,
    MyDialogs,
    WindowsUtils,
    ControlUtils,

    // qClasses: GPL-compatible data structures for use with Delphi
    //-------------------------------------------------------------
    QStringMaps,
    QIntegerMaps,
    QVectors,
    QLists,
    QOrderedDictionaries,

    // Global enums and constants  
    //---------------------------
    Enums in 'sm_model_classes\Enums.pas',
    StringConsts in 'StringConsts.pas',
    StatusEnums in 'sm_model_classes\StatusEnums.pas',

    // PDF/relational functions (nongraphical)
    Point, (* in 'general_purpose\Point.pas', *) // FIX ME: move to a different lib!
    ChartFunction, (* in 'general_purpose\ChartFunction.pas', *) // FIX ME: move to a different lib!
    RelFunction, (* in 'general_purpose\RelFunction.pas',*) // FIX ME: move to a different lib!
    ProbDensityFunctions, (* in 'general_purpose\ProbDensityFunctions.pas',*) // FIX ME: move to a different lib!

    // App-specific PDF/relational functions (nongraphical)
    //-----------------------------------------------------
    SMPdfs in 'sm_model_classes\SMPdfs.pas',
    SMRelFunction in 'sm_model_classes\SMRelFunction.pas',
  
    // Model classes and data structures (non-graphical)  
    //--------------------------------------------------
    Models in 'sm_model_classes\Models.pas',
    SMSimulationInput in 'sm_model_classes\SMSimulationInput.pas',
    SMDatabase in 'sm_database\SMDatabase.pas',
    OldDatabaseFns in 'sm_database\OldDatabaseFns.pas',
    VaccinationParams in 'sm_model_classes\VaccinationParams.pas',
    AirborneSpreadModel in 'sm_model_classes\AirborneSpreadModel.pas',
    ContactModel in 'sm_model_classes\ContactModel.pas',
    DestructionParams in 'sm_model_classes\DestructionParams.pas',
    Herd in 'sm_model_classes\Herd.pas',
    ProductionType in 'sm_model_classes\ProductionType.pas',
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
    SMFunctionList in 'sm_model_classes\SMFunctionList.pas',
    CustomOutputDefinitions in 'sm_model_classes\CustomOutputDefinitions.pas',
    Zone in 'sm_model_classes\Zone.pas',
    ZoneParams in 'sm_model_classes\ZoneParams.pas',
    SelectDailyOutputs in 'sm_model_classes\SelectDailyOutputs.pas',

    // Summary outputs
    //----------------
    //SMSimulationStats in 'sm_model_classes\SMSimulationStats.pas',
    //SMZoneStats in 'sm_model_classes\SMZoneStats.pas',
    OutputDescriptions in 'sm_model_classes\OutputDescriptions.pas',

    // Units for remote communication:
    // The path to RemoteMessenger should be specified in the Delphi Library path.
    //----------------------------------------------------------------------------
    RemoteMessenger,
    RemoteDatabaseParams in 'sm_database\RemoteDatabaseParams.pas',

    // XML parsing
    //------------
    ReadXMLInput in 'sm_xml_classes\ReadXMLInput.pas',
    xmlHerd in 'sm_xml_classes\xmlHerd.pas',
    DiseaseModelStatMethods in 'sm_xml_classes\DiseaseModelStatMethods.pas',
    Loc in 'sm_xml_classes\Loc.pas',

    FrameFileSelector in 'general_purpose_gui\FrameFileSelector.pas' {FrameFileSelector: TFrame},
    DialogLongMessage in 'general_purpose_gui\DialogLongMessage.pas' {DialogLongMessage},

    FormExport in 'sm_forms\FormExport.pas' {FormExport},
    FormMain in 'sm_compare\FormMain.pas' {FormMain}
  ;

{$R *.res}

begin
  setDEBUGGING( true );
  
  Application.Initialize;
  Application.CreateForm( TFormMain, frmMain );
  Application.Run;
end.
