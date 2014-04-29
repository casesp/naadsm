unit ProductionType;

(*
ProductionType.pas
-------------------
Begin: 2005/01/06
Last revision: $Date: 2013-06-27 19:11:35 $ $Author: areeves $
Version number: $Revision: 1.108.4.18 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)



{$INCLUDE ../Defs.inc}


interface

  uses
    Contnrs,
    Sysutils,

    Dialogs,

    Sdew,
    
    MyStrUtils,
    QLists,
    QStringMaps,
    SqlClasses,
    ChartFunction,

    FunctionDictionary,
    
    Models,
    ModelDatabase,
    SMDatabase,
    DetectionParams,
    ProdTypeZoneParams,
    DestructionParams,
    TracingParams,
    TestingParams,
    VaccinationParams,
    RingVaccParams,
    CostParams,
    ProbDensityFunctions,
    RelFunction,
    FunctionEnums,
    SMSimOutByProdType,
    StatusEnums,
    GlobalControlParams,
    NAADSMLibraryTypes,
    Zone
  ;


  const
    PRODTYPEIDNOTFOUND = -3;
    PRODTYPEUNASSIGNED = -1;

  type

  {type} TProdTypeOrder = (
    OrderUnspecified,
    OrderDestrPriority,
    OrderVaccPriority
  );


  {type} TProductionType = class( TModelWithFunctions )
    protected
      _myList: TObject {TProductionTypeList};

      _simulateTransition: boolean;
      _productionTypeID: integer;
      _productionTypeDescr: string;

      _pdfLatentName: string;
      _pdfSubclinicalName: string;
      _pdfClinicalName: string;
      _pdfImmuneName: string;
      _relPrevalenceName: string;

      _detection: TDetectionParams;

      _destr: TDestructionParams;

      _trace: TTracingParams;
      _testing: TTestingParams;

      _zoneParams: TProdTypeZoneParams;

      _vacc: TVaccinationParams;

      _ringVacc: TRingVaccParams;

      _costs: TCostParams;

      _outputs: TSMDailyOutput;
      _initialOutputs: TSMDailyOutput;

      _animalCount: longint;
      _unitCount: longint;

      _removed: boolean;

      // Functions for internal use
      //---------------------------
      procedure initialize();
      function validateDiseaseParams( err: PString = nil ): boolean;
      procedure updateDB( db: TSMDatabase );


      // Property getters and setters
      //-----------------------------
      // Overridden from TModel
      function getUpdated(): boolean; override;

      // Properties of production types
      function getSimulateTransition() : boolean;
      function getProdTypeDescr() : string;
      function getProdTypeID() : integer;
      procedure setSimulateTransition( val: boolean );
      procedure setProdTypeDescr( val: string );
      procedure setProdTypeID( val: integer );

      // Disease states
      procedure setPdfLatentName( val: string );
      procedure setPdfSubclinicalName( val: string );
      procedure setPdfClinicalName( val: string );
      procedure setPdfImmuneName( val: string );
      procedure setRelPrevalenceName( val: string );
      function getPdfLatentName(): string;
      function getPdfSubclinicalName(): string;
      function getPdfClinicalName(): string;
      function getPdfImmuneName(): string;
      function getRelPrevalenceName(): string;

      function getPdfDiseaseLatent(): TPdf;
      function getPdfDiseaseSubclinical(): TPdf;
      function getPdfDiseaseClinical(): TPdf;
      function getPdfDiseaseImmune(): TPdf;
      function getRelDiseasePrevalence(): TRelFunction;

      // Detection
      function getUseDetection(): boolean;
      function getDetectionParams(): TDetectionParams;
      procedure setDetectionParams( sv: TDetectionParams );

      // Zone parameters
      function getZoneParams(): TProdTypeZoneParams;
      procedure setZoneParams( z: TProdTypeZoneParams );
      function getIsZoneTrigger(): boolean;

      // Destruction
      function getUseBasicDestruction(): boolean;
      function getIsRingDestrTrigger(): boolean;
      function getIsRingDestrTarget(): boolean;
      function getIsDestrTarget(): boolean;
      function getUseTraceDestruction(): boolean;
      function getUseDestruction(): boolean;

      function getDestructionParams(): TDestructionParams;
      procedure setDestructionParams( dem: TDestructionParams );

      // Tracing
      function getUseTracing(): boolean;
      function getUseTracingExam(): boolean;
      function getTracingParams(): TTracingParams;
      procedure setTracingParams( dem: TTracingParams );

      // Testing
      function getUseTesting(): boolean;
      function getTestingParams(): TTestingParams;
      procedure setTestingParams( dem: TTestingParams );

      // Vaccination
      function getUseVaccination(): boolean;
      function getVaccinationParams(): TVaccinationParams;
      procedure setVaccinationParams( vp: TVaccinationParams );
      function getIsVaccTarget(): boolean;

      function getIsRingVaccTrigger(): boolean;
      function getRingVaccParams(): TRingVaccParams;
      procedure setRingVaccParams( rvp: TRingVaccParams );

      // Costs
      function getCostParams(): TCostParams;
      procedure setCostParams( cp: TCostParams );

      // Outputs
      function getCurrentOutputs(): TSMDailyOutput;
      function getInitialOutputs(): TSMDailyOutput;

      // Unit/animal counts
      function getUnitCount(): longint;
      function getAnimalCount(): longint;

      // Other
      procedure setRemoved( val: boolean );
      function getRemoved(): boolean;

      // XML import
      //-----------
      procedure importDiseaseXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

    public
      // Construction/initialization/destruction
      //----------------------------------------
      constructor create( prodTypeID: integer; prodTypeDescr: string; simTrans: boolean; sim: TObject ); overload;
      constructor create( const src: TProductionType; sim: TObject ); overload;

      destructor destroy(); override;

      // XML generation
      //---------------
      function ssDiseaseModelXml(): string;
      function ssDetectionXml(): string;
      function ssContactRecorderXml( const maxPeriod: integer ): string;
      function ssTracingXml(): string;
      function ssExamXml(): string;
      function ssTestXml(): string;
      function ssBasicDestrXml(): string;
      function ssTraceDestrXml(): string;
      function ssRingDestrXml(): string;
      function ssRingVaccXml(): string;
      function ssZoneXml(): string;
      function ssEconXml( const includeZonesGlobal: boolean; zoneList: TZoneList ): string;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

      // Functions for handling model outputs
      //-------------------------------------
      procedure clearAllRecords( db: TSMDatabase );
      procedure resetIterationRecords();
      procedure prepareForDay( day: integer );
      procedure processDailyRecords( db: TSMDatabase; iteration: integer; day: integer );
      procedure processIterationRecords( db: TSMDatabase; iteration: integer );

      procedure updateDailyRecordsProdType(
        const herdAnimalCount: integer;
        const oldState: TNAADSMDiseaseState;
        const newState: TNAADSMDiseaseState;
        const day: integer
      );

      procedure setInitialDailyRecords( const herdAnimalCount: integer; const herdDiseaseState: TNAADSMDiseaseState );

      procedure addInfectionEvent( const herdAnimalCount: integer; const r: THRDInfect; const day: integer );
      procedure addExposureEvent( const herdAnimalCount: integer; const e: THRDExpose );
      function addDetectionEvent( const herdAnimalCount: integer; const d: THRDDetect; const day: integer ): boolean;
      procedure addAttemptedTraceEvent( const herdAnimalCount: integer; const t: THRDTrace );
      procedure addTraceEvent( const herdAnimalCount: integer; const t: THRDTrace );
      procedure addTraceOrigin( const t: THRDTrace );
      procedure addHerdExamEvent( const herdAnimalCount: integer; const e: THRDExam );
      procedure addDiagnosticTestEvent( const herdAnimalCount: integer; const t: THRDTest );
      function addDestructionEvent( herd: TObject {THerd}; const c: THRDControl; const day: integer ): boolean;
      function addVaccinationEvent( herd: TObject {THerd}; const c: THRDControl; const day: integer ): boolean;
      procedure addZoneFocusEvent( const day: integer );
      procedure addDestructionQueueEvent( herd: TObject {THerd}; const day: integer );
      procedure addVaccinationQueueEvent( herd: TObject {THerd}; const day: integer );
      procedure subtractVaccinationQueueEvent( herd: TObject {THerd} );

      procedure decrementApparentInfectiousUnits();

      procedure recordInfectedAtFirstDetection();

      // Unit/animal counts
      //-------------------
      procedure clearCounts();
      procedure addToCounts( animals: longint );

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer; reintroduce;
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      function functionsAreValid(): boolean; override;
      procedure changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      ); override;


      // Other chart-related functions
      //------------------------------
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;

      // Used by ProductionTypeList, when the list is built from a database
      //-------------------------------------------------------------------
      procedure setUpdateFlag( const val: boolean );

      // Properties
      //-----------
      property productionTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property productionTypeID: integer read getProdTypeID write setProdTypeID;
      property ptList: TObject read _myList write _myList;

      property simulateTransition : boolean read getSimulateTransition write setSimulateTransition;

      property pdfLatentName: string read getPdfLatentName write setPdfLatentName;
      property pdfSubclinicalName: string read getPdfSubclinicalName write setPdfSubclinicalName;
      property pdfClinicalName: string read getPdfClinicalName write setPdfClinicalName;
      property pdfImmuneName: string read getPdfImmuneName write setPdfImmuneName;
      property relPrevalenceName: string read getRelPrevalenceName write setRelPrevalenceName;

      property pdfDiseaseLatent: TPdf read getPdfDiseaseLatent;
      property pdfDiseaseSubclinical: TPdf read getPdfDiseaseSubclinical;
      property pdfDiseaseClinical: TPdf read getPdfDiseaseClinical;
      property pdfDiseaseImmune: TPdf read getPdfDiseaseImmune;
      property relDiseasePrevalence: TRelFunction read getRelDiseasePrevalence;

      property useDetection: boolean read getUseDetection;
      property detectionParams: TDetectionParams read getDetectionParams write setDetectionParams;

      property useBasicDestruction: boolean read getUseBasicDestruction;
      property isRingDestrTrigger: boolean read getIsRingDestrTrigger;
      property isDestrRingTarget: boolean read getIsRingDestrTarget;
      property destructionParams: TDestructionParams read getDestructionParams write setDestructionParams;
      property isDestrTarget: boolean read getIsDestrTarget;
      property isRingDestrTarget: boolean read getisRingDestrTarget;
      property useTraceDestruction: boolean read getUseTraceDestruction;
      property useDestruction: boolean read getUseDestruction;

      property useTracing: boolean read getUseTracing;
      property useTracingExam: boolean read getUseTracingExam;
      property tracingParams: TTracingParams read getTracingParams write setTracingParams;

      property useTesting: boolean read getUseTesting;
      property testingParams: TTestingParams read getTestingParams write setTestingParams;

      property zoneParams: TProdTypeZoneParams read getZoneParams write setZoneParams;

      property useVaccination: boolean read getUseVaccination;
      property vaccinationParams: TVaccinationParams read getVaccinationParams write setVaccinationParams;
      property isVaccTarget: boolean read getIsVaccTarget;

      property isRingVaccTrigger: boolean read getIsRingVaccTrigger;
      property ringVaccParams: TRingVaccParams read getRingVaccParams write setRingVaccParams;

      property costParams: TCostParams read getCostParams write setCostParams;
      
      property currentOutputs: TSMDailyOutput read getCurrentOutputs;
      property initialOutputs: TSMDailyOutput read getInitialOutputs;

      property unitCount: longint read getUnitCount;
      property animalCount: longint read getAnimalCount;

      property removed: boolean read getRemoved write setRemoved;

      property isZoneTrigger: boolean read getIsZoneTrigger;
    end
  ;


implementation

  uses
    StrUtils,
    Math,
    TypInfo,
    Variants,
    Forms, // for Application.processMessages

    DebugWindow,
    QIntegerMaps,
    I88n,

    ProductionTypeList,

    SMSimulationInput,
    Herd,
    RemoteDatabaseParams
  ;


  const
    DBSHOWMSG: boolean = false; // set to true to enable debug messages for this unit.


//-----------------------------------------------------------------------------
// TProductionType Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TProductionType.Create( prodTypeID: integer; prodTypeDescr: string; simTrans: boolean; sim: TObject );
    begin
      inherited create();
      initialize();

      _sim := sim;
      _myList := nil; // This will be changed when the pt is added to a list
      
      _productionTypeID := prodTypeID;
      _productionTypeDescr := prodTypeDescr;
      
      _simulateTransition := simTrans;

      _destr := TDestructionParams.create( _sim, self.productionTypeDescr );
      _trace := TTracingParams.create( _sim, self.productionTypeDescr );
      _testing := TTestingParams.create( _sim, self.productionTypeDescr );
      _detection := TDetectionParams.create( _sim, self.productionTypeDescr );
      _vacc := TVaccinationParams.create( _sim, self.productionTypeDescr );
      _zoneParams := TProdTypeZoneParams.create( _sim, self.productionTypeDescr );
      _ringVacc := TRingVaccParams.create( _sim, self.productionTypeDescr );
      _costs := TCostParams.create( _sim, self.productionTypeDescr );

      _updated := true;
    end
  ;


  constructor TProductionType.create( const src: TProductionType; sim: TObject );
    begin
      inherited create( src );
      _sim := sim;
      _myList := nil; // This will be changed when the pt is added to a list

      _productionTypeID := src._productionTypeID;
      _productionTypeDescr := src._productionTypeDescr;

      _simulateTransition := src._simulateTransition;

      setPdfLatentName( src.pdfLatentName );
      setPdfSubclinicalName( src.pdfSubclinicalName );
      setPdfClinicalName( src.pdfClinicalName );
      setPdfImmuneName( src.pdfImmuneName );
      setRelPrevalenceName( src.relPrevalenceName );

      if( nil = src._detection ) then
        raise exception.Create( '_detection is nil in TProductionType.create()' )
      else
        _detection := TDetectionParams.create( src._detection, _sim )
      ;

      if( nil = src._trace ) then
        raise exception.create( '_trace is nil in TProductionType.create()' )
      else
        _trace := TTracingParams.create( src._trace, _sim )
      ;

      if( nil = src._testing ) then
        raise exception.create( '_testing is nil in TProductionType.create()' )
      else
        _testing := TTestingParams.create( src._testing, _sim )
      ;
      
      if( nil = src._destr ) then
        raise exception.Create( '_destr is nil in TProductionType.create()' )
      else
        _destr := TDestructionParams.create( src._destr, _sim )
      ;

      if( nil = src._vacc ) then
        raise exception.Create( '_vacc is nil in TProductionType.create()' )
      else
        _vacc := TVaccinationParams.create( src._vacc, _sim )
      ;

      if( nil = src._ringVacc ) then
        raise exception.Create( '_ringVacc is nil in TProductionType.create()' )
      else
        _ringVacc := TRingVaccParams.create( src._ringVacc, _sim )
      ;

      if( nil = src._zoneParams ) then
        raise exception.Create( '_zoneParams is nil in TProductionType.create()' )
      else
        _zoneParams := TProdTypeZoneParams.create( src._zoneParams, _sim )
      ;

      if( nil = src._costs ) then
        raise exception.Create( '_costs is nil in TProductionType.create()' )
      else
        _costs := TCostParams.create( src._costs, _sim )
      ;

      _animalCount := src._animalCount;
      _unitCount := src._unitCount;

      // It should never be necessary to copy outputs, but the structures still need to be created.
      _outputs := TSMDailyOutput.create();
      _initialOutputs := TSMDailyOutput.create();

      _removed := src._removed;
      _updated := src._updated;
    end
  ;


  procedure TProductionType.initialize();
    begin
      setPdfLatentName( '' );
      setPdfSubclinicalName( '' );
      setPdfClinicalName( '' );
      setPdfImmuneName( '' );
      setRelPrevalenceName( '' );

      _destr := nil;
      _trace := nil;
      _testing := nil;
      _detection := nil;
      _vacc := nil;
      _ringVacc := nil;
      _zoneParams := nil;
      _costs := nil;

      _unitCount := -1;
      _animalCount := -1;

      _outputs := TSMDailyOutput.create();
      _initialOutputs := TSMDailyOutput.create();

      _removed := false;
      _updated := false;
    end
  ;


  destructor TProductionType.destroy();
    begin
      // The function dictionary is freed elsewhere.
      // PDFs and RELs are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfLatentName( '' );
      setPdfSubclinicalName( '' );
      setPdfClinicalName( '' );
      setPdfImmuneName( '' );
      setRelPrevalenceName( '' );

      freeAndNil( _detection );
      freeAndNil( _destr );
      freeAndNil( _trace );
      freeAndNil( _testing );
      freeAndNil( _vacc );
      freeAndNil( _ringVacc );
      freeAndNil( _zoneParams );
      freeAndNil( _costs );

      freeAndNil( _outputs );
      freeAndNil( _initialOutputs );
      
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


  procedure TProductionType.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        DLatent: self.pdfLatentName := newName;
        DImmune: self.pdfImmuneName := newName;
        DSubclinical: self.pdfSubclinicalName := newName;
        DClinical: self.pdfClinicalName := newName;
        DPrevalence: self.relPrevalenceName := newName;

        DetProbReportVsFirstDetection: self.detectionParams.relReportVsFirstDetectionName := newName;
        DetProbObsVsTimeClinical: self.detectionParams.relObsVsTimeClinicalName := newName;

        TrDelay: self.tracingParams.pdfTraceDelayName := newName;

        TeDelay: self.testingParams.pdfTestDelayName := newName;

        VacImmunePeriod: self.vaccinationParams.pdfVaccImmuneName := newName;

        ZONMovementDirect: self.zoneParams.setChart( whichChart, fn, addlInfo );
        ZONMovementIndirect: self.zoneParams.setChart( whichChart, fn, addlInfo );
        else
          raise exception.create( 'Unrecognized whichChart (' + intToStr( integer( whichChart ) ) + ') in TProductionType.setChart' )
        ;
      end;

      _updated := true;
    end
  ;


  procedure TProductionType.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    var
      newName: string;
    begin
      if( nil = newChart ) then
        newName := ''
      else
        newName := newChart.name
      ;

      case whichChart of
        DLatent: if( oldChartName = self.pdfLatentName ) then  self.pdfLatentName := newName;
        DImmune: if( oldChartName = self.pdfImmuneName ) then self.pdfImmuneName := newName;
        DSubclinical: if( oldChartName = self.pdfSubclinicalName ) then self.pdfSubclinicalName := newName;
        DClinical: if( oldChartName = self.pdfClinicalName ) then self.pdfClinicalName := newName;
        DPrevalence: if( oldChartName = self.relPrevalenceName ) then self.relPrevalenceName := newName;
      end;

      detectionParams.changeChart( whichChart, oldChartName, newChart, addlInfo );
      tracingParams.changeChart( whichChart, oldChartName, newChart, addlInfo );
      testingParams.changeChart( whichChart, oldChartName, newChart, addlInfo );
      vaccinationParams.changeChart( whichChart, oldChartName, newChart, addlInfo );
      zoneParams.changeChart( whichChart, oldChartName, newChart, addlInfo );

      _updated := true;
    end
  ;


  function TProductionType.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      case whichChart of
        DLatent: result := ( chartName = self.pdfLatentName );
        DImmune: result := ( chartName = self.pdfImmuneName );
        DSubclinical: result := ( chartName = self.pdfSubclinicalName );
        DClinical: result := ( chartName = self.pdfClinicalName );
        DPrevalence: result := ( chartName = self.relPrevalenceName );

        DetProbReportVsFirstDetection: result := ( chartName = self.detectionParams.relReportVsFirstDetectionName );
        DetProbObsVsTimeClinical: result := ( chartName = self.detectionParams.relObsVsTimeClinicalName );

        TrDelay: result := ( chartName = self.tracingParams.pdfTraceDelayName );

        TeDelay: result := ( chartName = self.testingParams.pdfTestDelayName );

        VacImmunePeriod: result := ( chartName = self.vaccinationParams.pdfVaccImmuneName );

        ZONMovementDirect: result := (self.zoneParams.hasChartName( chartName, whichChart ) );
        ZONMovementIndirect: result := (self.zoneParams.hasChartName( chartName, whichChart ) );
        else
          begin
            raise exception.Create( 'Unrecognized whichChart in TProductionType.hasChartName' );
            result := false;
          end
        ;
      end;
    end
  ;


  function TProductionType.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    var
      ret_val: TChartFunction;
    begin
      ret_val := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            DLatent:
              if ( self.fnDictionary.contains( self.pdfLatentName ) ) then
                ret_val := self.fnDictionary.value( self.pdfLatentName ).fn
              ;
            DImmune:
              if ( self.fnDictionary.contains( self.pdfImmuneName ) ) then
                ret_val := self.fnDictionary.value( self.pdfImmuneName ).fn
              ;
            DSubclinical:
              if ( self.fnDictionary.contains( self.pdfSubclinicalName ) ) then
                ret_val := self.fnDictionary.value( self.pdfSubclinicalName ).fn
              ;
            DClinical:
              if ( self.fnDictionary.contains( self.pdfClinicalName ) ) then
                ret_val := self.fnDictionary.value( self.pdfClinicalName ).fn
              ;
            DPrevalence:
              if ( self.fnDictionary.contains( self.relPrevalenceName ) ) then
                ret_val := self.fnDictionary.value( self.relPrevalenceName ).fn
              ;

            TrDelay:
              if ( self.fnDictionary.contains( self.tracingParams.pdfTraceDelayName ) ) then
                ret_val := self.fnDictionary.value( self.tracingParams.pdfTraceDelayName ).fn
              ;

            TeDelay:
              if ( self.fnDictionary.contains( self.testingParams.pdfTestDelayName ) ) then
                ret_val := self.fnDictionary.value( self.testingParams.pdfTestDelayName ).fn
              ;

            DetProbReportVsFirstDetection:
              if ( self.fnDictionary.contains( self.detectionParams.relReportVsFirstDetectionName ) ) then
                ret_val := self.fnDictionary.value( self.detectionParams.relReportVsFirstDetectionName ).fn
              ;
            DetProbObsVsTimeClinical:
              if ( self.fnDictionary.contains( self.detectionParams.relObsVsTimeClinicalName ) ) then
                ret_val := self.fnDictionary.value( self.detectionParams.relObsVsTimeClinicalName ).fn
              ;


            VacImmunePeriod:
              if ( self.fnDictionary.contains( self.vaccinationParams.pdfVaccImmuneName ) ) then
                ret_val := self.fnDictionary.value( self.vaccinationParams.pdfVaccImmuneName ).fn
              ;
          end;
        end;
        
      result := ret_val;
    end
  ;


  procedure TProductionType.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfLatentName ) then self.pdfLatentName := '';
      if( chartName = self.pdfImmuneName ) then self.pdfImmuneName := '';
      if( chartName = self.pdfSubclinicalName ) then self.pdfSubclinicalName := '';
      if( chartName = self.pdfClinicalName ) then self.pdfClinicalName := '';
      if( chartName = self.relPrevalenceName ) then self.relPrevalenceName := '';

      if( chartName = self.tracingParams.pdfTraceDelayName ) then self.tracingParams.pdfTraceDelayName := '';

      if( chartName = self.testingParams.pdfTestDelayName ) then self.testingParams.pdfTestDelayName := '';

      if( chartName = self.detectionParams.relReportVsFirstDetectionName ) then self.detectionParams.relReportVsFirstDetectionName := '';
      if( chartName = self.detectionParams.relObsVsTimeClinicalName ) then self.detectionParams.relObsVsTimeClinicalName := '';

      if( chartName = self.vaccinationParams.pdfVaccImmuneName ) then self.vaccinationParams.pdfVaccImmuneName := '';

      // The _updated flag will be set by the properties above, if necessary
    end
  ;


//-----------------------------------------------------------------------------
// TProductionType Data validation
//-----------------------------------------------------------------------------
  function TProductionType.validateDiseaseParams( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;

      includePrevalence: boolean;
    begin
      result := true;
      msg := '';

      if( not simulateTransition ) then
        exit
      ;

      includePrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;

      submsg := '';
      if( nil = pdfDiseaseLatent ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Latent period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseLatent.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Latent period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseSubclinical ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Subclinical period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseSubclinical.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Subclinical period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseClinical) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Clinical period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseClinical.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Clinical period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseImmune ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Immune period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseImmune.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Immune period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( includePrevalence ) then
        begin
          if( nil = relDiseasePrevalence ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence REL function is not set.' ) + endl;
              result := false;
            end
          else if( not( relDiseasePrevalence.validate( @submsg) ) ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence REL function is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( err <> nil ) ) then
        err^ := err^ + msg
      ;
    end
  ;


  function TProductionType.validate( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;

      includeDetection: boolean;
      includeDestruction: boolean;
      includeTracing: boolean;
      includeTesting: boolean;
      includeVaccination: boolean;
      includeCosts: boolean;
      includeZones: boolean;
    begin
      result := true;
      msg := '';

      includeDetection := (_sim as TSMSimulationInput).includeDetectionGlobal;
      includeDestruction := (_sim as TSMSimulationInput).includeDestructionGlobal;
      includeTracing := (_sim as TSMSimulationInput).includeTracingGlobal;
      includeTesting := (_sim as TSMSimulationInput).includeTracingTestingGlobal;
      includeVaccination := (_sim as TSMSimulationInput).includeVaccinationGlobal;
      includeCosts := (_sim as TSMSimulationInput).includeCostsGlobal;
      includeZones := (_sim as TSMSimulationInput).includeZonesGlobal;
      
      dbcout( 'Validating production type ' + productionTypeDescr, DBSHOWMSG );

      if( 0 = unitCount ) then
        begin
          dbcout( 'Production type is not valid: there are no units.', DBSHOWMSG );
          if( nil <> err ) then msg := msg + tr( 'There are no units of this production type.' ) + endl;
          result := false;
        end
      ;

      submsg := '';
      if( not( validateDiseaseParams( @submsg ) ) ) then
        begin
          dbcout( 'Disease params are invalid: ' + submsg, DBSHOWMSG );
          if( nil <> err ) then msg := msg + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( includeDetection and useDetection ) then
        begin
          if( nil = detectionParams ) then
            begin
              dbcout( 'Detection is indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Detection is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( detectionParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Detection params are invalid: ' + endl + submsg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Detection parameters are not valid:' ) + ' ' + endl + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( includeDestruction and includeDetection ) then
        begin
          if( nil = destructionParams ) then
            begin
              dbcout( 'Destruction is indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( destructionParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Destruction params are invalid: ' + msg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
       if( includeTracing and includeDetection ) then
        begin
          if( nil = tracingParams ) then
            begin
              dbcout( 'Tracing is indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Tracing is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( tracingParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Tracing params are invalid: ' + msg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Tracing parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( includeTesting and includeDetection ) then
        begin
          if( nil = testingParams ) then
            begin
              dbcout( 'Diagnostic testing is indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Diagnostic testing is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( testingParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Diagnostic testing params are invalid: ' + msg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Diagnostic testing parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( ( includeVaccination and isRingVaccTrigger ) and includeDetection ) then
        begin
          if( nil = ringVaccParams ) then
            begin
              dbcout( 'Ring vacc is indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Ring vaccination is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( ringVaccParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Ring vacc params are invalid: ' + submsg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Ring vaccination parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( ( includeVaccination and useVaccination ) and includeDetection ) then
        begin
          if( nil = vaccinationParams ) then
            begin
              dbcout( 'Vacc indicated, not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( vaccinationParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Vacc params are invalid: ' + submsg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      // If there are any initially vaccinated units of the production type, then there must be a vaccine model.
      submsg := '';
      if
      ( ( not( includeVaccination ) ) or ( not( useVaccination ) ) )
      and
        ( (_sim as TSMSimulationInput).database.containsInitiallyVaccinatedUnits( self.productionTypeID ) )
      then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Units of this production type are initially vaccine immune, but parameters for vaccine immunity are not provided.' ) + endl;
          result := false;
        end
      ;

      submsg := '';
      if( includeCosts and includeDetection ) then
        begin
          if( nil = costParams ) then
            begin
              dbcout( 'Cost is indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Cost accounting is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else
            begin
              if (( includeTracing and useTracing ) and not( costParams.validateTracing( @submsg ) ) ) then
                begin
                  dbcout( 'Trace params are invalid: ' + submsg, DBSHOWMSG );
                  if( nil <> err ) then msg := msg + '  ' + tr( 'Tracing cost parameters are not valid:' ) + ' ' + submsg + endl;
                  result := false;
                end
              ;

              // Cost parameters currently include the cost of diagnostic
              // testing as a subcomponent of tracing cost parameters.

              if (( includeDestruction and isDestrTarget ) and not( costParams.validateDestr( @submsg ) ) ) then
                begin
                  dbcout( 'Cost params are invalid: ' + submsg, DBSHOWMSG );
                  if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction cost parameters are not valid:' ) + ' ' + submsg + endl;
                  result := false;
                end
              ;

              if (( includeVaccination and isVaccTarget ) and not( costParams.validateVacc( @submsg ) ) ) then
                begin
                  dbcout( 'Cost params are invalid: ' + submsg, DBSHOWMSG );
                  if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination cost parameters are not valid:' ) + ' ' + submsg + endl;
                  result := false;
                end
              ;
            end
          ;
        end
      ;

      submsg := '';
      if( includeZones and includeDetection ) then
        begin
          if( nil = zoneParams ) then
            begin
              dbcout( 'Zones are indicated but not set', DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Use of zones is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( zoneParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Zone params are invalid: ' + submsg, DBSHOWMSG );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Zone parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( err <> nil ) ) then
        begin
          msg := endl + ansiReplaceStr( tr( 'Production type xyz:' ), 'xyz', productionTypeDescr ) + endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType Database population
//-----------------------------------------------------------------------------
  function TProductionType.populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer;
    var
      q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      if( ( 0 < self.productionTypeID ) and ( updateAction <> MDBAForceInsert ) ) then
        updateDB( db )
      else
        begin
          self.productionTypeID := db.addProductionType( self.productionTypeDescr, self.simulateTransition, self.productionTypeID );
          //rbh 20110620 Added call to updateDB() below because disease parameter chart IDs were not being set for inProductionType on XML Import
          updateDB( db );
        end
      ;

      // Populate control measures in the same way, whether updating or creating a new record

      dict['useDiseaseTransition'] := usBoolToText( self._simulateTransition );

      if( nil <> detectionParams ) then
        begin
          if( detectionParams.updated ) then
            detectionParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> destructionParams ) then
        begin
          if( destructionParams.updated ) then
            destructionParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> tracingParams ) then
        begin
          if( tracingParams.updated ) then
            tracingParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> testingParams ) then
        begin
          if( testingParams.updated ) then
            testingParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> vaccinationParams ) then
        begin
          if( vaccinationParams.updated ) then
            vaccinationParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> ringVaccParams ) then
        begin
          if( ringVaccParams.updated ) then
            ringVaccParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> zoneParams ) then
        begin
          if( zoneParams.updated ) then
            zoneParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      if( nil <> costParams ) then
        begin
          if( costParams.updated ) then
            costParams.populateDatabase( db, self.productionTypeID )
          ;
        end
      ;

      q := writeQuery(
        'inProductionType',
        QUpdate,
        dict,
        'WHERE `productionTypeID` = ' + intToStr( self.productionTypeID )
      );

      result := integer( db.execute( q ) );
      //result := self.productionTypeID;
      
      dict.clear();
      dict.free();
      
      _updated := false;
    end
  ;


  procedure TProductionType.updateDB( db: TSMDatabase );
    var
      q: string;
      dict: TQueryDictionary;
      idstr: string;
    begin
      dict := TQueryDictionary.create();

      dict['descr'] := db.sqlQuote( self.productionTypeDescr );

      dict['useDiseaseTransition'] := usBoolToText( self.simulateTransition );

      if( nil <> pdfDiseaseLatent ) then
        idstr := intToStr( pdfDiseaseLatent.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disLatentPeriodPdfID'] := idstr;

      if( nil <> pdfDiseaseSubclinical ) then
        idstr := intToStr( pdfDiseaseSubclinical.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disSubclinicalPeriodPdfID'] := idstr;

      if( nil <> pdfDiseaseClinical ) then
        idstr := intToStr( pdfDiseaseClinical.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disClinicalPeriodPdfID'] := idstr;

      if( nil <> pdfDiseaseImmune ) then
        idstr := intToStr( pdfDiseaseImmune.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disImmunePeriodPdfID'] := idstr;

      if( nil <> relDiseasePrevalence ) then
        idstr := intToStr( relDiseasePrevalence.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disPrevalenceRelID'] := idstr;

      q := writeQuery(
        'inProductionType',
        QUpdate,
        dict,
        'WHERE [productionTypeID] = ' + intToStr( self.productionTypeID )
      );

      dict.Clear();
      dict.Free();

      db.execute( q );

      // Detection, destruction, tracing, testing, vaccination,
      // and cost parameters are updated by the original call to
      // populateDatabase. It isn't necessary to do anything
      // with them here.
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType XML export
//-----------------------------------------------------------------------------
  function TProductionType.ssDiseaseModelXml(): string;
    var
      useWithinHerdPrevalence: boolean;
    begin
      useWithinherdPrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;

      if( simulateTransition ) then
        begin
          result := '  <disease-model production-type="' + encodeXml( self.productionTypeDescr ) + '" production-type-id="' + intToStr( self.productionTypeID ) + '">' + endl;

          if( nil <> pdfDiseaseLatent ) then
            begin
              result := result + '    <latent-period>' + endl;
              result := result + pdfDiseaseLatent.ssXml( 3 );
              result := result + '    </latent-period>' + endl;
            end
          ;

          if( nil <> pdfDiseaseSubclinical ) then
            begin
            result := result + '    <infectious-subclinical-period>' + endl;
              result := result + pdfDiseaseSubclinical.ssXml( 3 );
              result := result + '    </infectious-subclinical-period>' + endl;
            end
          ;

          if( nil <> pdfDiseaseClinical ) then
            begin
              result := result + '    <infectious-clinical-period>' + endl;
              result := result + pdfDiseaseClinical.ssXml( 3 );
              result := result + '    </infectious-clinical-period>' + endl;
            end
          ;

          if( nil <> pdfDiseaseImmune ) then
            begin
              result := result + '    <immunity-period>' + endl;
              result := result + pdfDiseaseImmune.ssXml( 3 );
              result := result + '    </immunity-period>' + endl;
            end
          ;

          if( useWithinHerdPrevalence ) then
            begin
              result := result + '    <prevalence>' + endl;
              result := result + relDiseasePrevalence.ssXml( 3 );
              result := result + '    </prevalence>' + endl;
            end
          ;

          result := result + '  </disease-model>' + endl;
        end
      else
        result := ''
      ;

    end
  ;


  function TProductionType.ssDetectionXml(): string;
    begin
      if( useDetection ) then
        result := detectionParams.ssXML( self.productionTypeID )
      else
        result := ''
      ;
    end
  ;


  function TProductionType.ssContactRecorderXml( const maxPeriod: integer ): string;
    begin
      if( useTracing ) then
        result := tracingParams.ssContactRecorderXml( self.productionTypeID, maxPeriod )
      else
        result := ''
      ;
    end
  ;


  function TProductionType.ssTracingXml(): string;
    begin
      if( useTracing ) then
        result := tracingParams.ssXml( self.productionTypeID )
      else
        result := ''
      ;
    end
  ;


  function TProductionType.ssExamXml(): string;
    begin
      if( useTracingExam ) then
        result := tracingParams.ssExamXml( self.productionTypeID, useTesting, self.testingParams )
      else
        result := ''
      ;
    end
  ;


  function TProductionType.ssTestXml(): string;
    begin
      if( useTesting ) then
        result := testingParams.ssXml( self.productionTypeID )
      else
        result := ''
      ;
    end
  ;


  function TProductionType.ssBasicDestrXml(): string;
    var
      destrPriorityList: TQStringLongIntMap;
      priority: integer;
    begin
      if( useBasicDestruction ) then
        begin
          destrPriorityList := (_sim as TSMSimulationInput).controlParams.ssDestrPriorities;
          priority := destrPriorityList[ self.productionTypeDescr + '+' + 'basic' ];

          result := destructionParams.ssBasicDestrModelXml( productionTypeID, priority );
        end
      else
        result := ''
      ;
    end
  ;


  function TProductionType.ssTraceDestrXml(): string;
    var
      destrPriorityList: TQStringLongIntMap;
    begin
      if( useTraceDestruction ) then
        begin
          destrPriorityList := (_sim as TSMSimulationInput).controlParams.ssDestrPriorities;
          result := destructionParams.ssTraceDestrModelXml( productionTypeID, destrPriorityList );
        end
      else
        result := ''
      ;
    end
  ;


  {*
    The XML specification and core model code for ring destruction are more versatile than
    the current model specification and user interface.  According to the specification, if a
    particular production type is a ring trigger, then all units around it that are subject to ring
    destruction will be destroyed.

    Consider the following example:
      Cattle: not a ring trigger, but should be destroyed
      Swine: is a ring trigger, and should be destroyed
      Sheep: is a ring trigger, but should not be destroyed

      If a cattle herd is detected, nothing around it will be destroyed.
      If a swine herd is detected, then cattle and swine herds around it will be destroyed.
      If a sheep herd is detected, then cattle and swine herds around it will be destroyed.

    The XML is flexible enough to allow for more pairwise parameterization: detected swine herds could
    trigger ring destroyed of other swine herds, but not cattle or sheep herds; detected cattle
    herds could trigger destroyed of swine herds and other cattle herds but not sheep herds; detected
    sheep herds could trigger destroyed of only surrounding swine herds, etc.

    For now, the complication of this latter approach is not part of the model specification and is
    ignored by the graphical UI.
  }
  function TProductionType.ssRingDestrXml(): string;
    var
      fromType, toType: TProductionType;
      str: string;
      priority: integer;
      it: TProductionTypeListIterator;
      list: TProductionTypeList;
      destrPriorityList: TQStringLongIntMap;
    begin
      fromType := self;
      str := '';

      list := (_sim as TSMSimulationInput).ptList;
      destrPriorityList := (_sim as TSMSimulationInput).controlParams.ssDestrPriorities;

      it := TProductionTypeListIterator.create( list );

      while( nil <> it.current() ) do
        begin
          toType := it.current();

          dbcout( 'To type ' + toType.productionTypeDescr + ' should be destroyed: ' + usBoolToText( toType.isDestrRingTarget ), DBSHOWMSG );

          if( toType.isDestrRingTarget ) then
            begin
              priority := destrPriorityList[ toType.productionTypeDescr + '+' + 'ring' ];

              str := str + '  <ring-destruction-model to-production-type="' + encodeXml( toType.productionTypeDescr ) + '" from-production-type="' + encodeXml( fromType.productionTypeDescr ) + '">' + endl;
              str := str + '    <priority>' + intToStr( priority ) + '</priority> <!-- Based on the "to" type and destuction reason (ring) -->' + endl;
              str := str + '    <radius>' + endl;
              str := str + '      <value>' + usFloatToStr( fromType.destructionParams.ringRadius ) + '</value>' + endl;
              str := str + '      <units><xdf:unit>km</xdf:unit></units>' + endl;
              str := str + '    </radius>' + endl;
              str := str + '  </ring-destruction-model>' + endl;
              str := str + endl;
            end
          ;

          it.incr();
        end
      ;

      it.Free();

      result := str;
    end
  ;

  
  {*
    The XML specification and core model code for ring vaccination modules are more versatile than
    the current model specification and user interface.  According to the specification, if a
    particular production type is a ring trigger, then all units around it that should be vaccinated
    will be vaccinated.

    Consider the following example:
      Cattle: not a ring trigger, but should be vaccinated
      Swine: is a ring trigger, and should be vaccinated
      Sheep: is a ring trigger, but should not be vaccinated

      If a cattle herd is detected, nothing around it will be vaccinated.
      If a swine herd is detected, then cattle and swine herds around it will be vaccinated.
      If a sheep herd is detected, then cattle and swine herds around it will be vaccinated.

    The XML is flexible enough to allow for more pairwise parameterization: detected swine herds could
    trigger ring vaccination of other swine herds, but not cattle or sheep herds; detected cattle
    herds could trigger vaccination of swine herds and other cattle herds but not sheep herds; detected
    sheep herds could trigger vaccination of only surrounding swine herds, etc.

    For now, the complication of this latter approach is not part of the model specification and is
    ignored by the graphical UI.
  }
  function TProductionType.ssRingVaccXml(): string;
    var
      fromType, toType: TProductionType;
      str: string;
      priority: integer;
      it: TProductionTypeListIterator;
      list: TProductionTypeList;
      vaccPriorityList: TQStringLongIntMap;
    begin
      fromType := self;
      str := '';

      list := (_sim as TSMSimulationInput).ptList;
      vaccPriorityList := (_sim as TSMSimulationInput).controlParams.ssVaccPriorities;
      
      it := TProductionTypeListIterator.create( list );

      while( nil <> it.current() ) do
        begin
          toType := it.current();

          dbcout( 'From type ' + fromType.productionTypeDescr + ' is ring vacc trigger: ' + usBoolToText( fromType.isRingVaccTrigger ), DBSHOWMSG );
          dbcout( 'To type ' + toType.productionTypeDescr + ' should be vaccinated: ' + usBoolToText( toType.useVaccination ), DBSHOWMSG );

          if( toType.useVaccination ) then
            begin
              priority := vaccPriorityList[ toType.productionTypeDescr + '+' + 'ring' ];

              str := str + '  <ring-vaccination-model to-production-type="' + encodeXml( toType.productionTypeDescr ) + '" from-production-type="' + encodeXml( fromType.productionTypeDescr ) + '">' + endl;

              str := str + '    <priority>' + intToStr( priority ) + '</priority> <!-- Priority is based only on the "to" type -->' + endl;

              str := str + '    <radius>' + endl;
              str := str + '      <value>' + usFloatToStr( fromType.ringVaccParams.ringRadius ) + '</value>' + endl;
              str := str + '      <units><xdf:unit>km</xdf:unit></units>' + endl;
              str := str + '    </radius>' + endl;

              str := str + '    <min-time-between-vaccinations>' + endl;
              str := str + '      <value>' + intToStr( toType.ringVaccParams.minTimeBetweenVacc ) + '</value>' + endl;
              str := str + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
              str := str + '    </min-time-between-vaccinations>' + endl;

              str := str + '    <vaccinate-detected-units>' + usBoolToText( toType.ringVaccParams.vaccinateDetected ) + '</vaccinate-detected-units>' + endl;

              str := str + '  </ring-vaccination-model>' + endl;
              str := str + endl;
            end
          ;

          it.incr();
        end
      ;

      it.Free();

      result := str;
    end
  ;

  function TProductionType.ssZoneXml(): string;
    begin
      result := zoneParams.ssXml( self.productionTypeID );
    end
  ;


  function TProductionType.ssEconXml( const includeZonesGlobal: boolean; zoneList: TZoneList ): string;
    var
      it: TZoneListIterator;
    begin
      result := costParams.ssXml( isDestrTarget, isVaccTarget, self.productionTypeID );

      if( includeZonesGlobal ) then
        begin
          // FIX ME: Does the "background" zone need to be included here?
          // (I don't think so, but someone should check...)

          it := TZoneListIterator.create( zoneList );
          while( nil <> it.current() ) do
            begin
              result := result
                + '  <economic-model production-type="' + self.productionTypeDescr + '" production-type-id="' + intToStr( self.productionTypeID ) + '" zone="' + it.current().descr + '">' + endl
                + '    <surveillance>' + endl
                + '      <value>' + usFloatToStr( zoneParams.zonePtParamsList.paramsForZone( it.current().id ).costSurvPerAnimalDay ) + '</value>' + endl
                + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
                + '    </surveillance>' + endl
                + '  </economic-model>' + endl + endl
              ;
              it.incr();
            end
          ;
          it.Free();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType: XML import
//-----------------------------------------------------------------------------
  procedure TProductionType.importDiseaseXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      e: pointer;

      latentPeriod: TPdf;
      infectiousSubClinicalPeriod: TPdf;
      infectiousClinicalPeriod: TPdf;
      immunityPeriod: TPdf;
      prevalenceChart: TRelFunction;
    begin
      latentPeriod := nil;
      infectiousSubClinicalPeriod := nil;
      infectiousClinicalPeriod := nil;
      immunityPeriod := nil;
      prevalenceChart := nil;

      e := sdew.GetElementByName( model, 'latent-period' );
      if( nil <> e ) then
        latentPeriod := createPdfFromXml( e, sdew )
      ;

      e := sdew.GetElementByName( model, 'infectious-subclinical-period' );
      if( nil <> e ) then
        infectiousSubClinicalPeriod := createPdfFromXml( e, sdew )
      ;

      e := sdew.GetElementByName( model, 'infectious-clinical-period' );
      if( nil <> e ) then
        infectiousClinicalPeriod := createPdfFromXml( e, sdew )
      ;

      e := sdew.GetElementByName( model, 'immunity-period' );
      if( nil <> e ) then
        immunityPeriod := createPdfFromXml( e, sdew )
      ;

      e := sdew.GetElementByName( model, 'prevalence' );
      if( nil <> e ) then
        prevalenceChart := createRelFromXml( e, sdew )
      ;

      if( nil <> latentPeriod ) then
        begin
          if ( latentPeriod.name = '' ) then
            latentPeriod.name := self.productionTypeDescr + ' latent period'
          ;
          dbcout( latentPeriod.name, DBSHOWMSG );
          latentPeriod.dbField := word( DLatent );
          self.pdfLatentName := fnDictionary.checkAndInsert( latentPeriod );
          self.simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the latent period.' ) )
      ;

      if( nil <> infectiousSubClinicalPeriod ) then
        begin
          if ( infectiousSubClinicalPeriod.name = '' ) then
            infectiousSubClinicalPeriod.name := self.productionTypeDescr + ' subclinical period'
          ;
          dbcout( infectiousSubClinicalPeriod.name, DBSHOWMSG );
          infectiousSubClinicalPeriod.dbField := word( DSubclinical );
          self.pdfSubclinicalName := fnDictionary.checkAndInsert( infectiousSubClinicalPeriod );
          self.simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the subclinical period.' ) )
      ;

      if( nil <> infectiousClinicalPeriod ) then
        begin
          if ( infectiousClinicalPeriod.name = '' ) then
            infectiousClinicalPeriod.name := self.productionTypeDescr + ' clinical period'
          ;
          dbcout( infectiousClinicalPeriod.name, DBSHOWMSG );
          infectiousClinicalPeriod.dbField := word( DClinical );
          self.pdfClinicalName := fnDictionary.checkAndInsert( infectiousClinicalPeriod );
          self.simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the clinical period.' ) )
      ;

      if( nil <> immunityPeriod ) then
        begin
          if ( immunityPeriod.name = '' ) then
            immunityPeriod.name := self.productionTypeDescr + ' immune period'
          ;
          dbcout( immunityPeriod.name, DBSHOWMSG );
          immunityPeriod.dbField := word( DImmune );
          self.pdfImmuneName := fnDictionary.checkAndInsert( immunityPeriod );
          self.simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the natural immune period.' ) )
      ;

      if( nil <> prevalenceChart ) then
        begin
          if ( prevalenceChart.name = '' ) then
            prevalenceChart.name := self.productionTypeDescr + ' prevalence'
          ;
          dbcout( prevalenceChart.name, DBSHOWMSG );
          prevalenceChart.dbField := word( DPrevalence );
          self.relPrevalenceName := fnDictionary.checkAndInsert( prevalenceChart );
          self.simulateTransition := true;
          (self.sim as TSMSimulationInput).useWithinHerdPrevalence := true;
        end
      ;
    end
  ;


  procedure TProductionType.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      modelName: string;
    begin
      modelName := sdew.GetElementName( model );

      if( 'disease-model' = modelName ) then
        importDiseaseXml( model, sdew, errMsg )
      ;
      if( _detection.xmlModelList.contains( modelName ) ) then
        _detection.importXml( model, sdew, errMsg )
      ;
      if( _zoneParams.xmlModelList.contains( modelName ) ) then
        _zoneParams.importXml( model, sdew, errMsg )
      ;
      if( _vacc.xmlModelList.contains( modelName ) ) then
        _vacc.importXml( model, sdew, errMsg )
      ;
      if( _ringVacc.xmlModelList.contains( modelName ) ) then
        _ringVacc.importXml( model, sdew, errMsg )
      ;
      if( _destr.xmlModelList.contains( modelName ) ) then
        _destr.importXml( model, sdew, errMsg )
      ;
      if( _trace.xmlModelList.contains( modelName ) ) then
        _trace.importXml( model, sdew, errMsg )
      ;
      if( _testing.xmlModelList.contains( modelName ) ) then
        _testing.importXml( model, sdew, errMsg )
      ;
    end
  ;


  class function TProductionType.createXmlModelList(): TQStringList;
    var
      list: TQStringList;
    begin
      result := TQStringList.create();

      result.Append( 'disease-model' ); // TProductionType

      list := TDetectionParams.createXmlModelList();
      result.merge( list );
      list.Free();

      list := TProdTypeZoneParams.createXmlModelList();
      result.merge( list );
      list.Free();

      list := TVaccinationParams.createXmlModelList();
      result.merge( list );
      list.free();

      list := TRingVaccParams.createXmlModelList();
      result.merge( list );
      list.free();

      list := TDestructionParams.createXmlModelList();
      result.merge( list );
      list.free();

      list := TTracingParams.createXmlModelList();
      result.merge( list );
      list.free();

      list := TTestingParams.createXmlModelList();
      result.merge( list );
      list.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType: Unit/animal counts
//-----------------------------------------------------------------------------
  procedure TProductionType.clearCounts();
    begin
      _animalCount := 0;
      _unitCount := 0;
    end
  ;


  function TProductionType.getUnitCount(): longint;
    begin
      if( -1 = _unitCount ) then
        begin
          raise exception.create( 'TProductionType unitCount is not set for type ' + self.productionTypeDescr );
          result := -1;
        end
      else
        result := _unitCount
      ;
    end
  ;


  function TProductionType.getAnimalCount(): longint;
    begin
      if( -1 = _animalCount ) then
        begin
          raise exception.create( 'TProductionType animalCount is not setfor type ' + self.productionTypeDescr );
          result := -1;
        end
      else
        result := _animalCount
      ;
    end
  ;


  procedure TProductionType.addToCounts( animals: longint );
    begin
      // If both _animalCount and _unitCount are -1, this is OK.
      // It means that they haven't been set yet.
      // If neither are -1, that's fine, too.
      // If one but not the other is -1, however, this is a problem:
      // this situation should never occur.

      if( (-1 = _animalCount) xor (-1  = _unitCount) ) then
        raise exception.Create( 'Something is screwy in TProductionType.addToCounts' )
      else if( ( -1 = _animalCount ) and ( -1 = _unitCount ) ) then
        begin
          _animalCount := 0;
          _unitCount := 0;
        end
      ;

      inc( _unitCount );
      inc( _animalCount, animals );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType Debugging
//-----------------------------------------------------------------------------
  procedure TProductionType.debug();
    var
      msg: string;
    begin;
      msg := '++BEGIN TPRODUCTIONTYPE DEBUG' + endl;
      msg := msg + 'ProductionTypeID: ' + intToStr(self.productionTypeID) + endl;
      msg := msg + 'ProductionType: ' +  self.productionTypeDescr + endl;
      msg := msg + 'simulateTransition: ' + usBoolToText(self.simulateTransition) + endl;

      if( updated ) then msg := msg + '**UPDATED**' + endl;

      dbcout( msg, true );

      if( nil <> pdfDiseaseLatent ) then
        begin
          dbcout( 'diseaseLatent', true );
          pdfDiseaseLatent.debug();
        end
      else
        dbcout( 'NO LATENT PERIOD DEFINED', true )
      ;

      if( nil <> pdfDiseaseSubclinical ) then
        begin
          dbcout( endl + 'diseaseSubclinical', true );
          pdfDiseaseSubclinical.debug();
        end
      else
        dbcout( endl + 'NO SUBCLINICAL PERIOD DEFINED', true )
      ;

      if( nil <> pdfDiseaseClinical ) then
        begin
          dbcout( endl + 'diseaseClinical', true );
          pdfDiseaseClinical.debug();
        end
      else
        dbcout( endl + 'NO CLINICAL PERIOD DEFINED', true )
      ;

      if( nil <> pdfDiseaseImmune ) then
        begin
          dbcout( endl + 'diseaseImmune', true);
          pdfDiseaseImmune.debug();
        end
      else
        dbcout( endl + 'NO IMMUNE PERIOD DEFINED', true )
      ;

      if( (_sim as TSMSimulationInput).useWithinHerdPrevalence ) then
        begin
          if( nil <> relDiseasePrevalence ) then
            begin
              dbcout( endl + 'diseasePrevalence', true);
              relDiseasePrevalence.debug();
            end
          else
            dbcout( endl + 'NO PREVALENCE FUNCTION DEFINED', true )
          ;
        end
      else
        dbcout( endl + '(Within-herd prevalence is not used in this scenario)', true )
      ;

      if( useDetection ) then
        begin
          dbcout( endl + 'Detection will be conducted.', true );
          if( nil <> _detection ) then
            _detection.debug()
          else
            dbcout( endl + 'DETECTION IS UNDEFINED', true )
          ;
        end
      else
        dbcout( endl + 'Detection will not be conducted.', true )
      ;

      if( isDestrTarget or isRingDestrTrigger ) then
        begin
          dbcout( endl + 'Destruction will be used with or is triggered by this production type.', true );
          if( nil <> _destr ) then
            _destr.debug()
          else
            dbcout( endl + 'DESTRUCTION IS UNDEFINED', true )
          ;
        end
      else
        dbcout( endl + 'Destruction is not used with or triggered by this production type.', true );
      ;

      if( nil <> _trace ) then
        begin
          if( _trace.useTracing ) then
            _trace.debug()
          else
            dbcout( endl + 'Tracing will not be conducted.', true )
          ;
        end
      else
        dbcout( endl + 'TRACING IS UNDEFINED', true )
      ;

       if( nil <> _testing ) then
        begin
          if( _testing.useTesting ) then
            _testing.debug()
          else
            dbcout( endl + 'Diagnostic testing will not be conducted.', true )
          ;
        end
      else
        dbcout( endl + 'DIAGNOSTIC TESTING IS UNDEFINED', true )
      ;

      if( nil <> _vacc ) then
        begin
          if( _vacc.useVaccination ) then
            _vacc.debug()
          else
            dbcout( endl + 'Vaccination will not be conducted.', true )
          ;
        end
      else
        dbcout( endl + 'VACCINATION IS UNDEFINED', true )
      ;

      if( nil <> _costs ) then
        begin
          if( (_sim as TSMSimulationInput).includeCostsGlobal ) then
            _costs.debug()
          else
            dbcout( endl + 'Cost accounting will not be included.', true )
          ;
        end
      else
        dbcout( endl + 'COST PARAMS ARE UNDEFINED', true )
      ;

      if( -1 <> _unitCount ) then
        dbcout( 'Units of this type: ' + intToStr( _unitCount ), true )
      else
        dbcout( 'UNIT COUNT IS NOT SET', true )
      ;

      if( -1 <> _animalCount ) then
        dbcout( 'Animals of this type: ' + intToStr( _animalCount ), true )
      else
        dbcout( 'ANIMAL COUNT IS NOT SET', true )
      ;

      dbcout( '--END TPRODUCTIONTYPE DEBUG' + endl, true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType functions for handling model outputs
//-----------------------------------------------------------------------------
  procedure TProductionType.clearAllRecords( db: TSMDatabase );
    begin
      _outputs.clear();
      _initialOutputs.clear();

      // This function is called whenever a simulation is launched.
      // When this happens, the remote database needs to know about the production
      // types in the scenario.  That's what the following block of code is for
      // (even though it seems a little out of place here).
      if( remoteDBParams.useRemoteDatabase ) then
        db.remoteExecute(
          'INSERT INTO `inProductionType` ( `productionTypeID`, `descr`, `scenarioID` )'
          + ' VALUES('
          + ' ' + intToStr( productionTypeID )
          + ' , ''' + productionTypeDescr + ''''
          + ' , ' + intToStr( remoteDBParams.scenarioID )
          + ' )'
        )
      ;
    end
  ;


  procedure TProductionType.resetIterationRecords();
    begin
      // This restores current data to its original condition.
      _outputs.setAllRecordsFrom( _initialOutputs );
    end
  ;


  procedure TProductionType.prepareForDay( day: integer );
    begin
      _outputs.clearNewDailyCounts();
    end
  ;


  procedure TProductionType.processDailyRecords( db: TSMDatabase; iteration: integer; day: integer );
    begin
      _outputs.insertDatabaseOutputs( DRTDaily, db, productionTypeID, iteration, day );
    end
  ;


  // FIX ME: add tracing costs!
  procedure TProductionType.processIterationRecords( db: TSMDatabase; iteration: integer );
    var
      includeCostsDestr: boolean;
      includeCostsVacc: boolean;
    begin
      includeCostsDestr := (_sim as TSMSimulationInput).costTrackDestruction;
      includeCostsVacc := (_sim as TSMSimulationInput).costTrackVaccination;

      _outputs.insertDatabaseOutputs( DRTIteration, db, productionTypeID, iteration );

      if( includeCostsDestr or includeCostsVacc ) then
        begin
          includeCostsDestr := includeCostsDestr and self.isDestrTarget;
          includeCostsVacc := includeCostsVacc and self.isVaccTarget;
          _costs.insertDatabaseOutputs( _outputs, db, productionTypeID, includeCostsDestr, includeCostsVacc, iteration );
        end
      ;

    end
  ;


  function TProductionType.addDetectionEvent( const herdAnimalCount: integer; const d: THRDDetect; const day: integer ): boolean;
    begin
      ( _myList as TProductionTypeList ).detectHerd();
      
      case d.reason of
        NAADSMDetectionClinicalSigns:
          begin
            inc( _outputs.detnUClin );
            inc( _outputs.detnAClin, herdAnimalCount );
            inc( _outputs.detcUClin );
            inc( _outputs.detcAClin, herdAnimalCount );
            inc( _outputs.appdUInfectious );
          end
        ;
        NAADSMDetectionDiagnosticTest:
          begin
            inc( _outputs.detnUTest );
            inc( _outputs.detnATest, herdAnimalCount );
            inc( _outputs.detcUTest );
            inc( _outputs.detcATest, herdAnimalCount );
            inc( _outputs.appdUInfectious ); // FIX ME: Think about this...
          end
        ;
        else
          raise exception.Create( 'Unsupported detection reason (' + intToStr( cardinal( d.reason ) ) + ') in TProductionType.addDetectionEvent()' )
        ;
      end;

      _outputs.lastDetection := day;

      if( -1 = _outputs.firstDetection ) then
        begin
          _outputs.firstDetection := day;
          result := true;
        end
      else
        result := false
      ;
    end
  ;


  procedure TProductionType.addZoneFocusEvent( const day: integer );
    begin
      inc( _outputs.zonnFoci );
      inc( _outputs.zoncFoci );
    end
  ;


  procedure TProductionType.decrementApparentInfectiousUnits();
    begin
      dec( _outputs.appdUInfectious );
    end
  ;


  procedure TProductionType.recordInfectedAtFirstDetection();
    begin
      _outputs.firstDetUInf := _outputs.infcUAll + _outputs.infcUIni;
      _outputs.firstDetAInf := _outputs.infcAAll + _outputs.infcAIni;
    end
  ;


  procedure TProductionType.addDestructionQueueEvent( herd: TObject; const day: integer );
    begin
      _outputs.addToDestrQueue( ( herd as THerd ).initialSize, day );
      if( nil = _myList ) then
        raise exception.create( '_myList is nil in TProductionType.addDestructionQueueEvent()' )
      else
        ( _myList as TProductionTypeList ).addToDestrQueue( ( herd as THerd ).initialSize, day )
      ;
    end
  ;


  procedure TProductionType.addVaccinationQueueEvent( herd: TObject; const day: integer );
    begin
      _outputs.addToVaccQueue( ( herd as THerd ).initialSize, day );

      if( nil = _myList ) then
        raise exception.create( '_myList is nil in TProductionType.addVaccinationQueueEvent()' )
      else
        ( _myList as TProductionTypeList ).addToVaccQueue( ( herd as THerd ).initialSize, day )
      ;
    end
  ;


  procedure TProductionType.subtractVaccinationQueueEvent( herd: TObject {THerd} );
    begin
      _outputs.removeFromVaccQueue( ( herd as THerd ).initialSize );
      
      if( nil = _myList ) then
        raise exception.create( '_myList is nil in TProductionType.subtractVaccinationQueueEvent()' )
      else
        ( _myList as TProductionTypeList ).removeFromVaccQueue( ( herd as THerd ).initialSize )
      ;
    end
  ;


  function TProductionType.addDestructionEvent( herd: TObject; const c: THRDControl; const day: integer ): boolean;
    var
      h: THerd;
    begin
      h := herd as THerd;

      // Remember not to count units that start out as destroyed as the first destruction.
      if( ( -1 = _outputs.firstDestruction ) and ( NAADSMControlInitialState <> c.reason ) ) then
        begin
          _outputs.firstDestruction := day;
          result := true;
        end
      else
        result := false
      ;

      case c.reason of
        NAADSMControlTraceForwardDirect:
          begin
            inc( _outputs.descUDirFwd );
            inc( _outputs.descADirFwd, h.initialSize );
          end
        ;
        NAADSMControlTraceForwardIndirect:
          begin
            inc( _outputs.descUIndFwd );
            inc( _outputs.descAIndFwd, h.initialSize );
          end
        ;
        NAADSMControlTraceBackDirect:
          begin
            inc( _outputs.descUDirBack );
            inc( _outputs.descADirBack, h.initialSize );
          end
        ;
        NAADSMControlTraceBackIndirect:
          begin
            inc( _outputs.descUIndBack );
            inc( _outputs.descAIndBack, h.initialSize );
          end
        ;
        NAADSMControlRing:
          begin
            inc( _outputs.descURing );
            inc( _outputs.descARing, h.initialSize );
          end
        ;
        NAADSMControlDetection:
          begin
            inc( _outputs.descUDet );
            inc( _outputs.descADet, h.initialSize );
          end
        ;
        NAADSMControlInitialState:
          begin
            inc( _outputs.descUIni );
            inc( _outputs.descAIni, h.initialSize );
          end
        ;
        else
          raise exception.Create( 'Unrecognized destruction reason (' + intToStr( cardinal( c.reason ) ) + ') in TProductionType.addDestructionEvent' )
        ;
      end;

      
      // Deal with destruction queue
      //----------------------------
      if( NAADSMControlInitialState <> c.reason ) then
        begin
          _outputs.processDestruction( h.initialSize, day, c.dayCommitmentMade, h.ctrlActivities.isQueuedForVacc );
          if( nil = _myList ) then
            raise exception.Create( '_myList is nil in TProductionType.addDestructionEvent()' )
          else
            ( _myList as TProductionTypeList ).destroyHerd( h.initialSize, day, c.dayCommitmentMade )
          ;
        end
      ;

      // Deal with daily totals
      //-----------------------
      // Do this even for initially destroyed units.
      // "day" will have the special value -1 to initially destroyed units.
      inc( _outputs.desnUAll );
      inc( _outputs.desnAAll, h.initialSize );
    end
  ;


  function TProductionType.addVaccinationEvent( herd: TObject; const c: THRDControl; const day: integer ): boolean;
    var
      h: THerd;
    begin
      h := herd as THerd;

      // Remember not to count units that start out as vaccinated as the first vaccination.
      if( ( -1 = _outputs.firstVaccination ) and ( NAADSMControlInitialState <> c.reason ) ) then
        begin
          _outputs.firstVaccination := day;
          result := true;
        end
      else
        result := false
      ;

      case c.reason of
        NAADSMControlRing:
          begin
            inc( _outputs.vaccURing );
            inc( _outputs.vaccARing, h.initialSize );
          end
        ;
        NAADSMControlInitialState:
          begin
            inc( _outputs.vaccUIni );
            inc( _outputs.vaccAIni, h.initialSize );
          end
        ;
        else
          raise exception.Create( 'Unrecognized vaccination reason (' + intToStr( cardinal( c.reason ) ) + ') in TProductionType.addVaccinationEvent' )
        ;
      end;


      // Deal with vaccination queue
      //----------------------------
      if( NAADSMControlInitialState <> c.reason ) then
        begin
          _outputs.processVaccination( h.initialSize, day, c.dayCommitmentMade );
          if( nil = _myList ) then
            raise exception.create( '_myList is nil in TProductionType.addVaccinationEvent()' )
          else
            ( _myList as TProductionTypeList ).vaccinateHerd( h.initialSize, day, c.dayCommitmentMade )
          ;
        end
      ;

      // Deal with daily totals
      //-----------------------
      // Do this even for initially vaccinated units.
      // "day" will have the special value -1 to initially vaccinated units.
      inc( _outputs.vacnUAll );
      inc( _outputs.vacnAAll, h.initialSize );
    end
  ;


  procedure TProductionType.addAttemptedTraceEvent( const herdAnimalCount: integer; const t: THRDTrace );
    begin
      if( NAADSMDirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                inc( _outputs.trcUDirpFwd );
                inc( _outputs.trcADirpFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                inc( _outputs.trcUDirpBack );
                inc( _outputs.trcADirpBack, herdAnimalCount );
              end
            ;
            else
              raise exception.Create( 'Unrecognized trace type in TProductionType.addAttemptedTraceEvent' )
            ;
          end;
        end
      else if( NAADSMIndirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                inc( _outputs.trcUIndpFwd );
                inc( _outputs.trcAIndpFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                inc( _outputs.trcUIndpBack );
                inc( _outputs.trcAIndpBack, herdAnimalCount );
              end
            ;
            else
              raise exception.Create( 'Unrecognized trace direction (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addAttemptedTraceEvent' )
            ;
          end;
        end
      else
        raise exception.Create( 'Unrecognized contact type reason (' + intToStr( cardinal( t.contactType ) ) + ') in TProductionType.addAttemptedTraceEvent' )
      ;
    end
  ;


  procedure TProductionType.addTraceOrigin( const t: THRDTrace );
    begin
      if( NAADSMDirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                inc( _outputs.tonUDirFwd );
                inc( _outputs.tocUDirFwd );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                inc( _outputs.tonUDirBack );
                inc( _outputs.tocUDirBack );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addTraceOrigin' )
            ;
          end;
        end
      else if( NAADSMIndirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                inc( _outputs.tonUIndFwd );
                inc( _outputs.tocUIndFwd );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                inc( _outputs.tonUIndBack );
                inc( _outputs.tocUIndBack );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addTraceOrigin' )
            ;
          end;
        end
      else
        raise exception.Create( 'Unrecognized contact type (' + intToStr( cardinal( t.contactType ) ) + ') in TProductionType.addTraceOrigin' )
      ;
    end
  ;


  procedure TProductionType.addTraceEvent( const herdAnimalCount: integer; const t: THRDTrace );
    begin
      if( NAADSMDirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                inc( _outputs.trnUDirFwd );
                inc( _outputs.trnADirFwd, herdAnimalCount );
                inc( _outputs.trcUDirFwd );
                inc( _outputs.trcADirFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                inc( _outputs.trnUDirBack );
                inc( _outputs.trnADirBack, herdAnimalCount );
                inc( _outputs.trcUDirBack );
                inc( _outputs.trcADirBack, herdAnimalCount );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addTraceEvent' )
            ;
          end;
        end
      else if( NAADSMIndirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                inc( _outputs.trnUIndFwd );
                inc( _outputs.trnAIndFwd, herdAnimalCount );
                inc( _outputs.trcUIndFwd );
                inc( _outputs.trcAIndFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                inc( _outputs.trnUIndBack );
                inc( _outputs.trnAIndBack, herdAnimalCount );
                inc( _outputs.trcUIndBack );
                inc( _outputs.trcAIndBack, herdAnimalCount );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addTraceEvent' )
            ;
          end;
        end
      else
        raise exception.Create( 'Unrecognized contact type (' + intToStr( cardinal( t.contactType ) ) + ') in TProductionType.addTraceEvent' )
      ;
    end
  ;


  procedure TProductionType.addHerdExamEvent( const herdAnimalCount: integer; const e: THRDExam );
    begin
      // Regardless of contact type, increment exmnUAll and exmnAll
      //-----------------------------------------------------------
      inc( _outputs.exmnUAll );
      inc( _outputs.exmnAAll, herdAnimalCount );

      // Depending on contact type, increment the appropriate cumulative values
      // NOTE: Some day, it might be necessary to break down the new (incident)
      // counts by contact type, but we're not doing it yet.
      //-----------------------------------------------------------------------
      if( NAADSMDirectContact = e.contactType ) then
        begin
          case e.traceType of
            NAADSMTraceForwardOrOut:
              begin
                //inc( _outputs.exmnUDirFwd ); // Not yet(?) implemented
                //inc( _outputs.exmnADirFwd, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.exmcUDirFwd );
                inc( _outputs.exmcADirFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                //inc( _outputs.exmnUDirBack );  // Not yet(?) implemented
                //inc( _outputs.exmnADirBack, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.exmcUDirBack );
                inc( _outputs.exmcADirBack, herdAnimalCount );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( e.traceType ) ) + ') in TProductionType.addHerdExamEvent' )
            ;
          end;
        end
      else if( NAADSMIndirectContact = e.contactType ) then
        begin
          case e.traceType of
            NAADSMTraceForwardOrOut:
              begin
                //inc( _outputs.exmnUIndFwd );  // Not yet(?) implemented
                //inc( _outputs.exmnAIndFwd, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.exmcUIndFwd );
                inc( _outputs.exmcAIndFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                //inc( _outputs.exmnUIndBack );  // Not yet(?) implemented
                //inc( _outputs.exmnAIndBack, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.exmcUIndBack );
                inc( _outputs.exmcAIndBack, herdAnimalCount );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( e.traceType ) ) + ') in TProductionType.addHerdExamEvent' )
            ;
          end;
        end
      else
        raise exception.Create( 'Unrecognized contact type (' + intToStr( cardinal( e.contactType ) ) + ') in TProductionType.addHerdExamEvent' )
      ;
    end
  ;


  procedure TProductionType.addDiagnosticTestEvent( const herdAnimalCount: integer; const t: THRDTest );
    begin
      // Deal with contact type and direction
      //-------------------------------------
      if( NAADSMDirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                //inc( _outputs.tstnUDirFwd );  // Not yet(?) implemented
                //inc( _outputs.tstnADirFwd, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.tstcUDirFwd );
                inc( _outputs.tstcADirFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                //inc( _outputs.tstnUDirBack );  // Not yet(?) implemented
                //inc( _outputs.tstnADirBack, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.tstcUDirBack );
                inc( _outputs.tstcADirBack, herdAnimalCount );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace type (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addDiagnosticTestEvent' )
            ;
          end;
        end
      else if( NAADSMIndirectContact = t.contactType ) then
        begin
          case t.traceType of
            NAADSMTraceForwardOrOut:
              begin
                //inc( _outputs.tstnUIndFwd );  // Not yet(?) implemented
                //inc( _outputs.tstnAIndFwd, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.tstcUIndFwd );
                inc( _outputs.tstcAIndFwd, herdAnimalCount );
              end
            ;
            NAADSMTraceBackOrIn:
              begin
                //inc( _outputs.tstnUIndBack );  // Not yet(?) implemented
                //inc( _outputs.tstnAIndBack, herdAnimalCount );  // Not yet(?) implemented
                inc( _outputs.tstcUIndBack );
                inc( _outputs.tstcAIndBack, herdAnimalCount );
              end
            ;
            else
              raise exception.create( 'Unrecognized trace direction (' + intToStr( cardinal( t.traceType ) ) + ') in TProductionType.addDiagnosticTestEvent' )
            ;
          end;
        end
      else
        raise exception.Create( 'Unrecognized contactType (' + intToStr( cardinal( t.contactType ) ) + ') in TProductionType.addDiagnosticTestEvent' )
      ;

      // Deal with test result
      //----------------------
      case t.testResult of
        NAADSMTestTruePositive:
          begin
            inc( _outputs.tstnUTruePos );
            inc( _outputs.tstnATruePos, herdAnimalCount );
            inc( _outputs.tstcUTruePos );
            inc( _outputs.tstcATruePos, herdAnimalCount );
          end
        ;
        NAADSMTestTrueNegative:
          begin
            inc( _outputs.tstnUTrueNeg );
            inc( _outputs.tstnATrueNeg, herdAnimalCount );
            inc( _outputs.tstcUTrueNeg );
            inc( _outputs.tstcATrueNeg, herdAnimalCount );
          end
        ;
        NAADSMTestFalsePositive:
          begin
            inc( _outputs.tstnUFalsePos );
            inc( _outputs.tstnAFalsePos, herdAnimalCount );
            inc( _outputs.tstcUFalsePos );
            inc( _outputs.tstcAFalsePos, herdAnimalCount );
          end
        ;
        NAADSMTestFalseNegative:
          begin
            inc( _outputs.tstnUFalseNeg );
            inc( _outputs.tstnAFalseNeg, herdAnimalCount );
            inc( _outputs.tstcUFalseNeg );
            inc( _outputs.tstcAFalseNeg, herdAnimalCount );
          end
        ;
        else
          raise exception.Create( 'Unrecognized test result (' + intToStr( ord( t.testResult ) ) + ') in TProductionType.addDiagnosticTestEvent' )
        ;
      end;
    end
  ;


  procedure TProductionType.addExposureEvent( const herdAnimalCount: integer; const e: THRDExpose );
    begin
      case e.exposureMethod of
        NAADSMDirectContact:
          begin
            inc( _outputs.expcUDir );
            inc( _outputs.expcADir, herdAnimalCount );
          end
        ;
        NAADSMIndirectContact:
          begin
            inc( _outputs.expcUInd );
            inc( _outputs.expcAInd, herdAnimalCount );
          end
        ;
        NAADSMAirborneSpread:
          begin
            // Exposures by airborne spread are not (yet?) recorded,
            // but this function will still be called.
          end
        ;
        else
          raise exception.Create( 'Unrecognized exposure mechanism (' + intToStr( cardinal( e.exposureMethod ) ) + ') in TProductionType.addExposedByMechanism' )
        ;
      end;
    end
  ;


  procedure TProductionType.addInfectionEvent( const herdAnimalCount: integer; const r: THRDInfect; const day: integer );
    begin
      case r.infectionSourceType of
        NAADSMInitiallyInfected:
          begin
            if( 1 < day ) then
              raise exception.Create( '''Initial'' infection occurring after day 1.' )
            ;
            inc( _outputs.infcUIni );
            inc( _outputs.infcAIni, herdAnimalCount );
          end
        ;
        NAADSMAirborneSpread:
          begin
            inc( _outputs.infcUAir );
            inc( _outputs.infcAAir, herdAnimalCount );
            inc( _outputs.infnUAir );
            inc( _outputs.infnAAir, herdAnimalCount );
          end
        ;
        NAADSMDirectContact:
          begin
            inc( _outputs.infcUDir );
            inc( _outputs.infcADir, herdAnimalCount );
            inc( _outputs.infnUDir );
            inc( _outputs.infnADir, herdAnimalCount );
          end
        ;
        NAADSMIndirectContact:
          begin
            inc( _outputs.infcUInd );
            inc( _outputs.infcAInd, herdAnimalCount );
            inc( _outputs.infnUInd );
            inc( _outputs.infnAInd, herdAnimalCount );
          end
        ;
        else
          raise exception.Create( 'Unrecognized infection mechanism (' + intToStr( cardinal( r.infectionSourceType ) ) + ') in TProductionType.addInfectedByMechanism' )
        ;
      end;
    end
  ;

  {*
   Set daily and cumulative numbers of animals/herds in the various disease states
   according to their initial values in the database (dynHerd.initialStateCode)
  }
  procedure TProductionType.setInitialDailyRecords( const herdAnimalCount: integer; const herdDiseaseState: TNAADSMDiseaseState );
    begin

      dbcout( '*** TProductionType.setInitialDailyRecords', DBSHOWMSG );

      case herdDiseaseState of
        NAADSMStateSusceptible:
          begin
            inc( _initialOutputs.tsdUSusc );
            inc( _initialOutputs.tsdASusc, herdAnimalCount );
            inc( _initialOutputs.tscUSusc );
            inc( _initialOutputs.tscASusc, herdAnimalCount );
          end
        ;
        NAADSMStateLatent:
          begin
            inc( _initialOutputs.tsdULat );
            inc( _initialOutputs.tsdALat, herdAnimalCount );
            inc( _initialOutputs.tscULat );
            inc( _initialOutputs.tscALat, herdAnimalCount );
          end
        ;
        NAADSMStateSubclinical:
          begin
            inc( _initialOutputs.tsdUSubc );
            inc( _initialOutputs.tsdASubc, herdAnimalCount );
            inc( _initialOutputs.tscUSubc );
            inc( _initialOutputs.tscASubc, herdAnimalCount );
          end
        ;
        NAADSMStateClinical:
          begin
            inc( _initialOutputs.tsdUClin );
            inc( _initialOutputs.tsdAClin, herdAnimalCount );
            inc( _initialOutputs.tscUClin );
            inc( _initialOutputs.tscAClin, herdAnimalCount );
          end
        ;
        NAADSMStateNaturallyImmune:
          begin
            inc( _initialOutputs.tsdUNImm );
            inc( _initialOutputs.tsdANImm, herdAnimalCount );
            inc( _initialOutputs.tscUNImm );
            inc( _initialOutputs.tscANImm, herdAnimalCount );
          end
        ;
        NAADSMStateVaccineImmune:
          begin
            inc( _initialOutputs.tsdUVImm );
            inc( _initialOutputs.tsdAVImm, herdAnimalCount );
            inc( _initialOutputs.tscUVImm );
            inc( _initialOutputs.tscAVImm, herdAnimalCount );
          end
        ;
        NAADSMStateDestroyed:
          begin
            inc( _initialOutputs.tsdUDest );
            inc( _initialOutputs.tsdADest, herdAnimalCount );
            inc( _initialOutputs.tscUDest );
            inc( _initialOutputs.tscADest, herdAnimalCount );
          end
        ;
        else
          raise exception.Create( 'Unrecognized herd status in TProductionType.makeDailyRecord' )
        ;
      end;
    end
  ;


  procedure TProductionType.updateDailyRecordsProdType(
        const herdAnimalCount: integer;
        const oldState: TNAADSMDiseaseState;
        const newState: TNAADSMDiseaseState;
        const day: integer
      );
    begin
      dbcout( '*** TProductionType.updateDailyRecordsProdType', DBSHOWMSG );
(*
      if( 1 = day ) then
        begin
          // Do nothing: initial conditions are taken care of in resetIterationRecords
          dbcout2( 'TProductionType.updateDailyRecordsProdType on day 1' );
        end
      else
*)
      if( newState = oldState ) then
        // Do nothing: there is no change
      else
        _outputs.updateDailyCounts( herdAnimalCount, oldState, newState, day )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType properties
//-----------------------------------------------------------------------------

  // Disease states
  //---------------
  procedure TProductionType.setPdfLatentName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfLatentName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfLatentName := val;
      _updated := true;
    end
  ;


  procedure TProductionType.setPdfSubclinicalName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfSubclinicalName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfSubclinicalName := val;
      _updated := true;
    end
  ;


  procedure TProductionType.setPdfClinicalName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfClinicalName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfClinicalName := val;
      _updated := true;
    end
  ;


  procedure TProductionType.setPdfImmuneName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfImmuneName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfImmuneName := val;
      _updated := true;
    end
  ;


  procedure TProductionType.setRelPrevalenceName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relPrevalenceName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relPrevalenceName := val;
      _updated := true;
    end
  ;


  function TProductionType.getPdfDiseaseLatent(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfLatentName ) ) then
            begin
              if( fnDictionary.value( _pdfLatentName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfLatentName ).fn as TPdf
              else
                begin
                  setPdfLatentName( '' );
                  result := nil;
                end
              ;
            end
          else
            result := nil
          ;
        end
      ;
    end
  ;


  function TProductionType.getPdfDiseaseSubclinical(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfSubclinicalName ) ) then
            begin
              if( fnDictionary.value( _pdfSubclinicalName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfSubclinicalName ).fn as TPdf
              else
                begin
                  setPdfSubclinicalName( '' );
                  result := nil;
                end
              ;
            end
          else
            result := nil
          ;
        end
      ;
    end
  ;


  function TProductionType.getPdfDiseaseClinical(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfClinicalName ) ) then
            begin
              if( fnDictionary.value( _pdfClinicalName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfClinicalName ).fn as TPdf
              else
                begin
                  setPdfClinicalName( '' );
                  result := nil;
                end
              ;
            end
          else
            result := nil
          ;
        end
      ;
    end
  ;


  function TProductionType.getPdfDiseaseImmune(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfImmuneName ) ) then
            begin
              if( fnDictionary.value( _pdfImmuneName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfImmuneName ).fn as TPdf
              else
                begin
                  setPdfImmuneName( '' );
                  result := nil;
                end
              ;
            end
          else
            result := nil
          ;
        end
      ;
    end
  ;


  function TProductionType.getRelDiseasePrevalence(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relPrevalenceName ) ) then
            begin
              if( fnDictionary.value( _relPrevalenceName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relPrevalenceName ).fn as TRelFunction
              else
                begin
                  setRelPrevalenceName( '' );
                  result := nil;
                end
              ;
            end
          else
            result := nil
          ;
        end
      ;
    end
  ;


  function TProductionType.functionsAreValid(): boolean;
    var
      includePrevalence: boolean;
      includeZones: boolean;
      includeDetection: boolean;
      includeVaccination: boolean;
    begin
      result := true;

      includeDetection := (_sim as TSMSimulationInput).includeDetectionGlobal;
      if( includeDetection and useDetection and ( nil <> detectionParams ) ) then
        begin
          if( not detectionParams.functionsAreValid() ) then
            result := false
          ;
        end
      ;

      includeVaccination := (_sim as TSMSimulationInput).includeVaccinationGlobal;
      if( includeVaccination and useVaccination and ( nil <> vaccinationParams ) ) then
        begin
          if( not vaccinationparams.functionsAreValid() ) then
            result := false
          ;
        end
      ;

      includeZones := (_sim as TSMSimulationInput).includeZonesGlobal;
      if( includeZones  and ( nil <> zoneParams ) ) then
        begin
          if( not zoneParams.functionsAreValid() ) then
            result := false
          ;
        end
      ;

      if( fnDictionary.contains( _pdfLatentName ) ) then
        begin
          if( not( fnDictionary.value( _pdfLatentName ).fn is TPdf ) ) then
            begin
              setPdfLatentName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _pdfSubclinicalName ) ) then
        begin
          if( not( fnDictionary.value( _pdfSubclinicalName ).fn is TPdf ) ) then
            begin
              setPdfSubclinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _pdfClinicalName ) ) then
        begin
          if( not( fnDictionary.value( _pdfClinicalName ).fn is TPdf ) ) then
            begin
              setPdfClinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _pdfImmuneName ) ) then
        begin
          if( not( fnDictionary.value( _pdfImmuneName ).fn is TPdf ) ) then
            begin
              setPdfImmuneName( '' );
              result := false;
            end
          ;
        end
      ;

      includePrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;
      if( includePrevalence and fnDictionary.contains( _relPrevalenceName ) ) then
        begin
          if( not( fnDictionary.value( _relPrevalenceName ).fn is TRelFunction ) ) then
            begin
              setRelPrevalenceName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;


  function TProductionType.getPdfLatentName(): string; begin result := _pdfLatentName; end;
  function TProductionType.getPdfSubclinicalName(): string; begin result := _pdfSubclinicalName; end;
  function TProductionType.getPdfClinicalName(): string; begin result := _pdfClinicalName; end;
  function TProductionType.getPdfImmuneName(): string; begin result := _pdfImmuneName; end;
  function TProductionType.getRelPrevalenceName(): string; begin result := _relPrevalenceName; end;


  // Detection
  //----------
  procedure TProductionType.setDetectionParams( sv: TDetectionParams );
    begin
      if( nil <> _detection ) then freeAndNil( _detection );
      _detection := sv;
      _updated := true;
    end
  ;


  function TProductionType.getUseDetection(): boolean;
    begin
      if( nil = detectionParams ) then
        result := false
      else
        result := detectionParams.useDetection
      ;
    end
  ;

  function TProductionType.getDetectionParams(): TDetectionParams; begin result := _detection; end;


  // Destruction
  //------------
  procedure TProductionType.setDestructionParams( dem: TDestructionParams );
    begin
      if( nil <> _destr ) then freeAndNil( _destr );
      _destr := dem;
      _updated := true;
    end
  ;


  function TProductionType.getUseBasicDestruction(): boolean;
    begin
      if( nil = destructionParams ) then
        result := false
      else
        result := destructionParams.destroyDetectedUnits
      ;
    end
  ;


  function TProductionType.getIsRingDestrTrigger(): boolean;
    begin
      if( nil = destructionParams ) then
        result := false
      else
        result := destructionParams.isRingTrigger
      ;
    end
  ;


  function TProductionType.getIsRingDestrTarget(): boolean;
    begin
      if( nil = destructionParams ) then
        result := false
      else
        result := destructionParams.isRingTarget
      ;
    end
  ;
 

  function TProductionType.getDestructionParams(): TDestructionParams; begin result := _destr; end;

  function TProductionType.getIsDestrTarget(): boolean;
    begin
      if( nil = _destr ) then
        result := false
      else
        result := _destr.destroyForAnyReason
      ;
    end
  ;


  function TProductionType.getUseTraceDestruction(): boolean;
    begin
      if( nil = _destr ) then
        result := false
      else
        result := _destr.useTraceDestruction
      ;
    end
  ;


  function TProductionType.getUseDestruction(): boolean;
    begin
      result :=
        isRingDestrTarget
      or
        useBasicDestruction
      or
        useTraceDestruction
      ;
    end
  ;

  // Tracing
  //--------
  procedure TProductionType.setTracingParams( dem: TTracingParams );
    begin
      if( nil <> _trace ) then freeAndNil( _trace );
      _trace := dem;
      _updated := true;
    end
  ;

  function TProductionType.getTracingParams(): TTracingParams; begin result := _trace; end;

  function TProductionType.getUseTracing(): boolean;
    begin
      if( nil = tracingParams ) then
        result := false
      else
        result := tracingParams.useTracing
      ;
    end
  ;

  function TProductionType.getUseTracingExam(): boolean;
    begin
      if( nil = tracingParams ) then
        result := false
      else
        result := tracingParams.useTracingExam
      ;
    end
  ;

  // Testing
  //--------
  procedure TProductionType.setTestingParams( dem: TTestingParams );
    begin
      if( nil <> _testing ) then freeAndNil( _testing );
      _testing := dem;
      _updated := true;
    end
  ;

  function TProductionType.getTestingParams(): TTestingParams; begin result := _testing; end;

  function TProductionType.getUseTesting(): boolean;
    begin
      if( nil = testingParams ) then
        result := false
      else
        result := testingParams.useTesting
      ;
    end
  ;

  // Zones
  //-------
  procedure TProductionType.setZoneParams( z: TProdTypeZoneParams );
    begin
      if( nil <> _zoneParams ) then freeAndNil( _zoneParams );
      _zoneParams := z;
      _updated := true;
    end
  ;

  function TProductionType.getIsZoneTrigger(): boolean;
    begin
      if( nil <> _zoneParams ) then
        result := _zoneParams.isZoneTrigger
      else
        begin
          raise exception.create( '_zoneParams is nil in TProductionType.getIsZoneTrigger' );
          result := false;
        end
      ;
    end
  ;

  function TProductionType.getZoneParams(): TProdTypeZoneParams; begin result := _zoneParams; end;

  // Vaccination
  //------------
  procedure TProductionType.setVaccinationParams( vp: TVaccinationParams );
    begin
      if( nil <> _vacc ) then freeAndNil( _vacc );
      _vacc := vp;
      _updated := true;
    end
  ;


  procedure TProductionType.setRingVaccParams( rvp: TRingVaccParams );
    begin
      if( nil <> _ringVacc ) then freeAndNil( _ringVacc );
      _ringVacc := rvp;
      _updated := true;
    end
  ;


  function TProductionType.getIsRingVaccTrigger(): boolean;
    begin
      if( nil = ringVaccParams ) then
        result := false
      else
        result := ringVaccParams.useRing
      ;
    end
  ;



  function TProductionType.getUseVaccination(): boolean;
    begin
      if( nil = vaccinationParams ) then
        result := false
      else
        result := vaccinationParams.useVaccination
      ;
    end
  ;

  function TProductionType.getIsVaccTarget(): boolean;
    begin
      if( nil = _vacc ) then
        result := false
      else
        result := _vacc.useVaccination
      ;
    end
  ;

  function TProductionType.getVaccinationParams(): TVaccinationParams; begin result := _vacc; end;
  function TProductionType.getRingVaccParams(): TRingVaccParams; begin result := _ringVacc; end;

  // Costs
  //------
  procedure TProductionType.setCostParams( cp: TCostParams );
    begin
      if( nil <> _costs ) then freeAndNil( _costs );
      _costs := cp;
      _updated := true;
    end
  ;

  function TProductionType.getCostParams(): TCostParams; begin result := _costs; end;


  // Unsorted stuff
  //---------------
  function TProductionType.getUpdated(): boolean;
    begin
      if( _updated ) then
        result := true
      else
        begin
          result := false;

          if( nil <> _costs ) then
            begin
              if( _costs.updated ) then
                begin
                 result := true;
                 exit;
                end
              ;
            end
          ;

          if( nil <> _destr ) then
            begin
              if( _destr.updated ) then
                begin
                 result := true;
                 exit;
                end
              ;
            end
          ;

          if( nil <> _trace ) then
            begin
              if( _trace.updated ) then
                begin
                 result := true;
                 exit;
                end
              ;
            end
          ;

          if( nil <> _testing ) then
            begin
              if( _testing.updated ) then
                begin
                 result := true;
                 exit;
                end
              ;
            end
          ;

          if( nil <> _detection ) then
            begin
              if( _detection.updated ) then
                begin
                 result := true;
                 exit;
                end
              ;
            end
          ;

          if( nil <> _zoneParams ) then
            begin
              if( _zoneParams.updated ) then
                begin
                  result := true;
                  exit;
                end
              ;
            end
          ;

          if( nil <> _ringVacc ) then
            begin
              if( _ringVacc.updated ) then
                begin
                  result := true;
                  exit;
                end
              ;
            end
          ;

          if( nil <> _vacc ) then
            begin
              if( _vacc.updated ) then
                begin
                  result := true;
                  exit;
                end
              ;
            end
          ;
        end
      ;
    end
  ;

  function TProductionType.getSimulateTransition() : boolean; begin result := _simulateTransition; end;

  function TProductionType.getProdTypeDescr() : string;
    begin
      if( 0 = length( _productionTypeDescr ) ) then
        raise exception.Create( 'TProductionType._productionTypeDescr is not set' )
      ;
      result := _productionTypeDescr;
    end
  ;

  procedure TProductionType.setSimulateTransition( val: boolean );
    begin
      _simulateTransition := val;
      _updated := true;
    end
  ;


  procedure TProductionType.setProdTypeDescr( val: string );
    var
      oldProdTypeDescr: string;
      smSim: TSMSimulationInput;
    begin
      oldProdTypeDescr := _productionTypeDescr;

      _productionTypeDescr := val;

      _detection.prodTypeDescr := val;
      _ringVacc.prodTypeDescr := val;
      _zoneParams.prodTypeDescr := val;
      _destr.prodTypeDescr := val;
      _trace.prodTypeDescr := val;
      _testing.prodTypeDescr := val;
      _costs.prodTypeDescr := val;
      _vacc.prodTypeDescr := val;

      smSim := _sim as TSMSimulationInput;

      if( nil <> smSim.controlParams ) then
        smSim.controlParams.renameProductionType( oldProdTypeDescr, val )
      ;

      _updated := true;
    end
  ;

  procedure TProductionType.setProdTypeID( val: integer );
    begin
      _productionTypeID := val;
      _updated := true;
    end
  ;

  procedure TProductionType.setUpdateFlag( const val: boolean ); begin _updated := val; end;

  function TProductionType.getProdTypeID() : integer; begin result := _productionTypeID; end;

  function TProductionType.getCurrentOutputs(): TSMDailyOutput; begin result := _outputs; end;
  function TProductionType.getInitialOutputs(): TSMDailyOutput; begin result := _initialOutputs; end;

  procedure TProductionType.setRemoved( val: boolean ); begin _removed := val; _updated := true; end;
  function TProductionType.getRemoved(): boolean; begin result := _removed; end;
//-----------------------------------------------------------------------------



end.
