unit ProductionType;

(*
ProductionType.pas
-------------------
Begin: 2005/01/06
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.86 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

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

    MyStrUtils,
    USStrUtils,
    QStringMaps,
    SqlClasses,
    ChartFunction,

    FunctionDictionary,
    
  	Models,
    SMDatabase,
    DetectionParams,
    ZoneParams,
    DestructionParams,
    VaccinationParams,
    RingVaccParams,
    CostParams,
    ProbDensityFunctions,
    RelFunction,
    FunctionEnums,
    SMSimOutByProdType,
    StatusEnums,
    GlobalControlParams
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
      _simulateTransition: boolean;
      _productionTypeID: integer;
      _productionTypeDescr: string;
      _xmlProdTypeDescr: string;

      _latentName: string;
      _subclinicalName: string;
      _clinicalName: string;
      _immuneName: string;
      _prevalenceName: string;

      _useDetection: boolean;
      _detection: TDetectionParams;

      _destr: TDestructionParams;

      _zoneParams: TZoneParams;

      _vacc: TVaccinationParams;

      _ringVacc: TRingVaccParams;

      _costs: TCostParams;

      _outputs: TSMSimOutByProdType;
      _initialOutputs: TSMSimOutByProdType;

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
      procedure setLatentName( val: string );
      procedure setSubclinicalName( val: string );
      procedure setClinicalName( val: string );
      procedure setImmuneName( val: string );
      procedure setPrevalenceName( val: string );
      function getLatentName(): string;
      function getSubclinicalName(): string;
      function getClinicalName(): string;
      function getImmuneName(): string;
      function getPrevalenceName(): string;

      function getDiseaseLatent(): TPdf;
      function getDiseaseSubclinical(): TPdf;
      function getDiseaseClinical(): TPdf;
      function getDiseaseImmune(): TPdf;
      function getDiseasePrevalence(): TRelFunction;

      // Detection
      function getUseDetection(): boolean;
      procedure setUseDetection( val: boolean );
      function getDetectionParams(): TDetectionParams;
      procedure setDetectionParams( sv: TDetectionParams );

      // Zone parameters
      function getZoneParams(): TZoneParams;
      procedure setZoneParams( z: TZoneParams );
      function getIsZoneTrigger(): boolean;
      
      // Destruction
      function getIsRingDestrTrigger(): boolean;
      function getIsRingDestrTarget(): boolean;
      function getIsDestrTarget(): boolean;

      function getDestructionParams(): TDestructionParams;
      procedure setDestructionParams( dem: TDestructionParams );

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
      function getCurrentOutputs(): TSMSimOutByProdType;
      function getInitialOutputs(): TSMSimOutByProdType;

      // Unit/animal counts
      function getUnitCount(): longint;
      function getAnimalCount(): longint;

      // Other
      procedure setRemoved( val: boolean );
      function getRemoved(): boolean;

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
      function ssRingDestrXml(): string;
      function ssRingVaccXml(): string;
      function ssZoneXml(): string;

      // Functions for handling model outputs
      //-------------------------------------
      procedure clearAllRecords( db: TSMDatabase );
      procedure resetIterationRecords();
      procedure prepareForDay( day: integer );
      procedure processDailyRecords( db: TSMDatabase; iteration: integer; day: integer );
      procedure processIterationRecords( db: TSMDatabase; iteration: integer );

      procedure updateDailyRecordsProdType(
        herdAnimalCount: integer;
        oldState: TTransitionState;
        newState: TTransitionState
      );

      procedure setInitialDailyRecords( const herdAnimalCount: integer; const herdDiseaseState: TTransitionState );

      procedure addInfectedByMechanism( const herdAnimalCount: integer; infMech: string; const day: integer );
			procedure addExposedByMechanism( const herdAnimalCount: integer; mechanism: string );
      function addDetection( const herdAnimalCount: integer; const day: integer ): boolean;
      procedure addAttemptedTraceEvent( const herdAnimalCount: integer; mechanism: string );
      procedure addTraceEvent( const herdAnimalCount: integer; mechanism: string );
      function addDestructionEvent( const herdAnimalCount: integer; reason: string; const day: integer ): boolean;
      function addVaccinationEvent( const herdAnimalCount: integer; reason: string; const day: integer ): boolean;
      procedure addZoneFocus( const day: integer );
      procedure decrementApparentInfectiousUnits();

      // Unit/animal counts
      //-------------------
      procedure clearCounts();
      procedure addToCounts( animals: longint );

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function populateDatabase( db: TSMDatabase; const forceInsert: boolean = false ): integer; reintroduce;
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
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;

      // Used by ProductionTypeList, when the list is built from a database
      //-------------------------------------------------------------------
      procedure setUpdateFlag( const val: boolean );

      // Properties
      //-----------
      property productionTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property xmlProdTypeDescr: string read _xmlProdTypeDescr;
      property productionTypeID: integer read getProdTypeID write setProdTypeID;

      property simulateTransition : boolean read getSimulateTransition write setSimulateTransition;

      property latentName: string read getLatentName write setLatentName;
      property subclinicalName: string read getSubclinicalName write setSubclinicalName;
      property clinicalName: string read getClinicalName write setClinicalName;
      property immuneName: string read getImmuneName write setImmuneName;
      property prevalenceName: string read getPrevalenceName write setPrevalenceName;

      property diseaseLatent: TPdf read getDiseaseLatent;
      property diseaseSubclinical: TPdf read getDiseaseSubclinical;
      property diseaseClinical: TPdf read getDiseaseClinical;
      property diseaseImmune: TPdf read getDiseaseImmune;
      property diseasePrevalence: TRelFunction read getDiseasePrevalence;

      property useDetection: boolean read getUseDetection write setUseDetection;
      property detectionParams: TDetectionParams read getDetectionParams write setDetectionParams;

      property isRingDestrTrigger: boolean read getIsRingDestrTrigger;
      property isDestrRingTarget: boolean read getIsRingDestrTarget;
      property destructionParams: TDestructionParams read getDestructionParams write setDestructionParams;
      property isDestrTarget: boolean read getIsDestrTarget;
      property isRingDestrTarget: boolean read getisRingDestrTarget;

      property zoneParams: TZoneParams read getZoneParams write setZoneParams;

      property useVaccination: boolean read getUseVaccination;
      property vaccinationParams: TVaccinationParams read getVaccinationParams write setVaccinationParams;
      property isVaccTarget: boolean read getIsVaccTarget;

      property isRingVaccTrigger: boolean read getIsRingVaccTrigger;
      property ringVaccParams: TRingVaccParams read getRingVaccParams write setRingVaccParams;

      property costParams: TCostParams read getCostParams write setCostParams;
      
      property currentOutputs: TSMSimOutByProdType read getCurrentOutputs;
      property initialOutputs: TSmSimOutByProdType read getInitialOutputs;

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
    DBPRODUCTIONTYPE: boolean = false; // set to true to enable debug messages for this unit.
  	DBPRODUCTIONTYPELIST: boolean = false; // set to true to enable debugging messages for this unit


//-----------------------------------------------------------------------------
// TProductionType Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TProductionType.Create( prodTypeID: integer; prodTypeDescr: string; simTrans: boolean; sim: TObject );
    begin
    	inherited create();
      initialize();

      _sim := sim;

      _productionTypeID := prodTypeID;
      _productionTypeDescr := prodTypeDescr;
      _xmlProdTypeDescr := encodeXml( prodTypeDescr );
      
      _simulateTransition := simTrans;

      _destr := TDestructionParams.create( _sim, self.productionTypeDescr );
      _detection := TDetectionParams.create( _sim, self.productionTypeDescr );
      _vacc := TVaccinationParams.create( _sim, self.productionTypeDescr );
      _zoneParams := TZoneParams.create( _sim, self.productionTypeDescr );
      _ringVacc := TRingVaccParams.create( _sim, self.productionTypeDescr );
      _costs := TCostParams.create( _sim, self.productionTypeDescr );

      _updated := true;
    end
  ;


  constructor TProductionType.create( const src: TProductionType; sim: TObject );
    begin
      inherited create( src );
      _sim := sim;

      _productionTypeID := src._productionTypeID;
      _productionTypeDescr := src._productionTypeDescr;
      _xmlProdTypeDescr := encodeXml( _productionTypeDescr );
      
      _simulateTransition := src._simulateTransition;

      // Don't use the "set" functions here.
      // Otherwise, the function reference counters will get screwed up.
      _latentName := src.latentName;
      _subclinicalName := src.subclinicalName;
      _clinicalName := src.clinicalName;
      _immuneName := src.immuneName;
      _prevalenceName := src.prevalenceName;

      _useDetection := src._useDetection;

      if( nil <> src._detection ) then
        _detection := TDetectionParams.create( src._detection, _sim )
      else
        _detection := nil
      ;

      //_useDestruction := src._useDestruction;

      if( nil <> src._destr ) then
        _destr := TDestructionParams.create( src._destr, _sim )
      else
        _destr := nil
      ;

      if( nil <> src._vacc ) then
        _vacc := TVaccinationParams.create( src._vacc, _sim )
      else
        _vacc := nil
      ;

      if( nil <> src._ringVacc ) then
        _ringVacc := TRingVaccParams.create( src._ringVacc, _sim )
      else
        _ringVacc := nil
      ;

      if( nil <> src._zoneParams ) then
        _zoneParams := TZoneParams.create( src._zoneParams, _sim )
      else
        _zoneParams := nil
      ;

      if( nil <> src._costs ) then
        _costs := TCostParams.create( src._costs, _sim )
      else
        _costs := nil
      ;

      _animalCount := src._animalCount;
      _unitCount := src._unitCount;

      // It should never be necessary to copy outputs.
      // Leave _outputs and _initialOutputs alone.
      //    (But you still need to init them.....SPC:10/27/2006)
      _outputs := TSMSimOutByProdType.create();
      _initialOutputs := TSMSimOutByProdType.create();

      _removed := src._removed;
      _updated := src._updated;
    end
  ;


  procedure TProductionType.initialize();
  	begin
      setLatentName( '' );
      setSubclinicalName( '' );
      setClinicalName( '' );
      setImmuneName( '' );
      setPrevalenceName( '' );

      _destr := nil;
      _detection := nil;
      _vacc := nil;
      _ringVacc := nil;
      _zoneParams := nil;
      _costs := nil;

      _unitCount := -1;
      _animalCount := -1;

      _outputs := TSMSimOutByProdType.create();
      _initialOutputs := TSMSimOutByProdType.create();

      _removed := false;
      _updated := false;
    end
  ;


  destructor TProductionType.destroy();
    begin
      // The function dictionary is freed elsewhere.
      // Disease periods are handled by the function dictionary:
      // don't free them here, but do decrement their counters.

      if( nil <> fnDictionary ) then
        begin
          //dbcout2( 'Decrementing latent' );
          if( fnDictionary.contains( latentName ) ) then
            fnDictionary.value( latentName ).decrRefCounter()
          ;

          //dbcout2( 'Decrementing subclin' );
          if( fnDictionary.contains( subclinicalName ) ) then
            fnDictionary.value( subclinicalName ).decrRefCounter()
          ;

          //dbcout2( 'Decrementing clinical' );
          if( fnDictionary.contains( clinicalName ) ) then
            fnDictionary.value( clinicalName ).decrRefCounter()
          ;

          //dbcout2( 'Decrementing immune' );
          if( fnDictionary.contains( immuneName ) ) then
            fnDictionary.value( immuneName ).decrRefCounter()
          ;

          //dbcout2( 'Decrementing prevalence' );
          if( fnDictionary.contains( prevalenceName ) ) then
            fnDictionary.value( prevalenceName ).decrRefCounter()
          ;
        end
      ;

      //dbcout2( 'Freeing other stuff...' );
      freeAndNil( _detection );
      freeAndNil( _destr );
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
        DLatent: self.latentName := newName;
        DImmune: self.immuneName := newName;
        DSubclinical: self.subclinicalName := newName;
        DClinical: self.clinicalName := newName;
        DPrevalence: self.prevalenceName := newName;
        DetProbReportVsFirstDetection: self.detectionParams.relReportVsFirstDetectionName := newName;
        DetProbObsVsTimeClinical: self.detectionParams.relObsVsTimeClinicalName := newName;
        VacImmunePeriod: self.vaccinationParams.vaccImmunePdfName := newName;
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
        DLatent: self.latentName := newName;
        DImmune: self.immuneName := newName;
        DSubclinical: self.subclinicalName := newName;
        DClinical: self.clinicalName := newName;
        DPrevalence: self.prevalenceName := newName;
        DetProbReportVsFirstDetection: self.detectionParams.relReportVsFirstDetectionName := newName;
        DetProbObsVsTimeClinical: self.detectionParams.relObsVsTimeClinicalName := newName;
        VacImmunePeriod: self.vaccinationParams.vaccImmunePdfName := newName;
        ZONMovementDirect: self.zoneParams.changeChart( whichChart, oldChartName, newChart );
        ZONMovementIndirect: self.zoneParams.changeChart( whichChart, oldChartName, newChart );
      end;

      _updated := true;
    end
  ;


  function TProductionType.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      case whichChart of
        DLatent: result := ( chartName = self.latentName );
        DImmune: result := ( chartName = self.immuneName );
        DSubclinical: result := ( chartName = self.subclinicalName );
        DClinical: result := ( chartName = self.clinicalName );
        DPrevalence: result := ( chartName = self.prevalenceName );
        DetProbReportVsFirstDetection: result := ( chartName = self.detectionParams.relReportVsFirstDetectionName );
        DetProbObsVsTimeClinical: result := ( chartName = self.detectionParams.relObsVsTimeClinicalName );
        VacImmunePeriod: result := ( chartName = self.vaccinationParams.vaccImmunePdfName );
        ZONMovementDirect: result := (self.zoneParams.hasChartName( chartName ) );
        ZONMovementIndirect: result := (self.zoneParams.hasChartName( chartName ) );
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
              if ( self.fnDictionary.contains( self.latentName ) ) then
                ret_val := self.fnDictionary.value( self.latentName ).fn
              ;
            DImmune:
              if ( self.fnDictionary.contains( self.immuneName ) ) then
                ret_val := self.fnDictionary.value( self.immuneName ).fn
              ;
            DSubclinical:
              if ( self.fnDictionary.contains( self.subclinicalName ) ) then
                ret_val := self.fnDictionary.value( self.subclinicalName ).fn
              ;
            DClinical:
              if ( self.fnDictionary.contains( self.clinicalName ) ) then
                ret_val := self.fnDictionary.value( self.clinicalName ).fn
              ;
            DPrevalence:
              if ( self.fnDictionary.contains( self.prevalenceName ) ) then
                ret_val := self.fnDictionary.value( self.prevalenceName ).fn
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
              if ( self.fnDictionary.contains( self.vaccinationParams.vaccImmunePdfName ) ) then
                ret_val := self.fnDictionary.value( self.vaccinationParams.vaccImmunePdfName ).fn
              ;
          end;
        end;
        
      result := ret_val;
    end
  ;


  procedure TProductionType.removeChart( const chartName: string );
    begin
      if( chartName = self.latentName ) then self.latentName := '';
      if( chartName = self.immuneName ) then self.immuneName := '';
      if( chartName = self.subclinicalName ) then self.subclinicalName := '';
      if( chartName = self.clinicalName ) then self.clinicalName := '';
      if( chartName = self.prevalenceName ) then self.prevalenceName := '';
      if( chartName = self.detectionParams.relReportVsFirstDetectionName ) then self.detectionParams.relReportVsFirstDetectionName := '';
      if( chartName = self.detectionParams.relObsVsTimeClinicalName ) then self.detectionParams.relObsVsTimeClinicalName := '';
      if( chartName = self.vaccinationParams.vaccImmunePdfName ) then self.vaccinationParams.vaccImmunePdfName := '';

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
      if( nil = diseaseLatent ) then
      	begin
       		if( err <> nil ) then msg := msg + '  ' + tr( 'Latent period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( diseaseLatent.validate( @submsg) ) ) then
      	begin
					if( err <> nil ) then msg := msg + '  ' + tr( 'Latent period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = diseaseSubclinical ) then
      	begin
       		if( err <> nil ) then msg := msg + '  ' + tr( 'Subclinical period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( diseaseSubclinical.validate( @submsg) ) ) then
      	begin
					if( err <> nil ) then msg := msg + '  ' + tr( 'Subclinical period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = diseaseClinical) then
      	begin
       		if( err <> nil ) then msg := msg + '  ' + tr( 'Clinical period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( diseaseClinical.validate( @submsg) ) ) then
      	begin
					if( err <> nil ) then msg := msg + '  ' + tr( 'Clinical period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = diseaseImmune ) then
      	begin
       		if( err <> nil ) then msg := msg + '  ' + tr( 'Immune period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( diseaseImmune.validate( @submsg) ) ) then
      	begin
					if( err <> nil ) then msg := msg + '  ' + tr( 'Immune period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( includePrevalence ) then
        begin
          if( nil = diseasePrevalence ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence REL function is not set.' ) + endl;
              result := false;
            end
          else if( not( diseasePrevalence.validate( @submsg) ) ) then
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
      includeVaccination: boolean;
      includeCosts: boolean;
      includeZones: boolean;
  	begin
    	result := true;
			msg := '';

      includeDetection := (_sim as TSMSimulationInput).includeDetectionGlobal;
      includeDestruction := (_sim as TSMSimulationInput).includeDestructionGlobal;
      includeVaccination := (_sim as TSMSimulationInput).includeVaccinationGlobal;
      includeCosts := (_sim as TSMSimulationInput).includeCostsGlobal;
      includeZones := (_sim as TSMSimulationInput).includeZonesGlobal;
      
      dbcout( 'Validating production type ' + productionTypeDescr, DBPRODUCTIONTYPE );

			submsg := '';
      if( not( validateDiseaseParams( @submsg ) ) ) then
      	begin
        	dbcout( 'Disease params are invalid: ' + submsg, DBPRODUCTIONTYPE );
        	if( nil <> err ) then msg := msg + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( includeDetection and useDetection ) then
      	begin
          if( nil = detectionParams ) then
          	begin
            	dbcout( 'Detection is indicated but not set', DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Detection is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( detectionParams.validate( @submsg ) ) ) then
            begin
            	dbcout( 'Detection params are invalid: ' + endl + submsg, DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Detection parameters are not valid:' ) + ' ' + endl + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( includeDestruction {and useDestruction} ) then
      	begin
        	if( nil = destructionParams ) then
          	begin
            	dbcout( 'Destruction is indicated but not set', DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction is indicated, but parameters are not set.' ) + endl;
            	result := false;
            end
          else if( not( destructionParams.validate( @submsg ) ) ) then
          	begin
            	dbcout( 'Destruction params are invalid: ' + msg, DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction parameters are not valid:' ) + ' ' + submsg + endl;
            	result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( includeVaccination and isRingVaccTrigger ) then
      	begin
          if( nil = ringVaccParams ) then
            begin
            	dbcout( 'Ring vacc is indicated but not set', DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Ring vaccination is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( ringVaccParams.validate( @submsg ) ) ) then
            begin
            	dbcout( 'Ring vacc params are invalid: ' + submsg, DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Ring vaccination parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( includeVaccination and useVaccination ) then
      	begin
          if( nil = vaccinationParams ) then
            begin
            	dbcout( 'Vacc indicated, not set', DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( vaccinationParams.validate( @submsg ) ) ) then
            begin
            	dbcout( 'Vacc params are invalid: ' + submsg, DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination parameters are not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;
      	end
      ;

      submsg := '';
      if( includeCosts ) then
        begin
          if( nil = costParams ) then
            begin
              dbcout( 'Cost is indicated but not set', DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Cost accounting is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else
            begin
              if( isDestrTarget and not( costParams.validateDestr( @submsg ) ) ) then
                begin
                  dbcout( 'Cost params are invalid: ' + submsg, DBPRODUCTIONTYPE );
                  if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction cost parameters are not valid:' ) + ' ' + submsg + endl;
                  result := false;
                end
              ;

              if( isVaccTarget and not( costParams.validateVacc( @submsg ) ) ) then
                begin
                  dbcout( 'Cost params are invalid: ' + submsg, DBPRODUCTIONTYPE );
                  if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination cost parameters are not valid:' ) + ' ' + submsg + endl;
                  result := false;
                end
              ;
            end
          ;
        end
      ;

      submsg := '';
      if( includeZones ) then
        begin
          if( nil = zoneParams ) then
            begin
              dbcout( 'Zones are indicated but not set', DBPRODUCTIONTYPE );
              if( nil <> err ) then msg := msg + '  ' + tr( 'Use of zones is indicated, but parameters are not set.' ) + endl;
              result := false;
            end
          else if( not( zoneParams.validate( @submsg ) ) ) then
            begin
              dbcout( 'Zone params are invalid: ' + submsg, DBPRODUCTIONTYPE );
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
  function TProductionType.populateDatabase( db: TSMDatabase; const forceInsert: boolean = false ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      if( ( 0 < self.productionTypeID ) and ( not forceInsert ) ) then
        updateDB( db )
      else
      	self.productionTypeID := db.addProductionType( self.productionTypeDescr, self.simulateTransition, self.productionTypeID )
      ;

      // Populate control measures in the same way, whether updating or creating a new record
      
      // FIX ME: make 'detection' part of detectionParams.
//SPC:  10/06/2006 -- Moved this to the detectionParams populateDatebase routine,
//                    assuming that the presence of the object indications a "true"
//                    value for this field.
//
//SPC:  10/31/2006 -- Moved back here because the formDetection editing in this program can reset this
//                    value and still leave the detection object untouched.
//

      dict['useDetection'] := boolToStr( self.useDetection );

      dict['useDiseaseTransition'] := boolToStr( self._simulateTransition );

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

      dict['useDiseaseTransition'] := boolToStr( self.simulateTransition );

      if( nil <> diseaseLatent ) then
      	idstr := intToStr( diseaseLatent.id )
      else
      	idstr := DATABASE_NULL_VALUE
      ;
      dict['disLatentPeriodPdfID'] := idstr;

      if( nil <> diseaseSubclinical ) then
      	idstr := intToStr( diseaseSubclinical.id )
      else
      	idstr := DATABASE_NULL_VALUE
      ;
      dict['disSubclinicalPeriodPdfID'] := idstr;

      if( nil <> diseaseClinical ) then
      	idstr := intToStr( diseaseClinical.id )
      else
      	idstr := DATABASE_NULL_VALUE
      ;
      dict['disClinicalPeriodPdfID'] := idstr;

      if( nil <> diseaseImmune ) then
      	idstr := intToStr( diseaseImmune.id )
      else
      	idstr := DATABASE_NULL_VALUE
      ;
      dict['disImmunePeriodPdfID'] := idstr;

      if( nil <> diseasePrevalence ) then
      	idstr := intToStr( diseasePrevalence.id )
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

      // Detection, destruction, vaccination, and cost
      // parameters are updated by the original call to
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

          if( nil <> diseaseLatent ) then
            begin
              result := result + '    <latent-period>' + endl;
              result := result + diseaseLatent.ssXml( 3 );
              result := result + '    </latent-period>' + endl;
            end
          ;

          if( nil <> diseaseSubclinical ) then
            begin
              result := result + '    <infectious-subclinical-period>' + endl;
              result := result + diseaseSubclinical.ssXml( 3 );
              result := result + '    </infectious-subclinical-period>' + endl;
            end
          ;

          if( nil <> diseaseClinical ) then
            begin
              result := result + '    <infectious-clinical-period>' + endl;
              result := result + diseaseClinical.ssXml( 3 );
              result := result + '    </infectious-clinical-period>' + endl;
            end
          ;

          if( nil <> diseaseImmune ) then
            begin
              result := result + '    <immunity-period>' + endl;
              result := result + diseaseImmune.ssXml( 3 );
              result := result + '    </immunity-period>' + endl;
            end
          ;

          if( useWithinHerdPrevalence ) then
            begin
              result := result + '    <prevalence>' + endl;
              result := result + diseasePrevalence.ssXml( 3 );
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

          dbcout( 'To type ' + toType.productionTypeDescr + ' should be destroyed: ' + usBoolToText( toType.isDestrRingTarget ), DBPRODUCTIONTYPE );

          if( toType.isDestrRingTarget ) then
            begin
              priority := destrPriorityList[ toType.productionTypeDescr + '+' + 'ring' ];

              str := str + endl;
              str := str + '  <ring-destruction-model to-production-type="' + toType.xmlProdTypeDescr + '" from-production-type="' + fromType.xmlProdTypeDescr + '">' + endl;
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

          dbcout( 'From type ' + fromType.productionTypeDescr + ' is ring vacc trigger: ' + usBoolToText( fromType.isRingVaccTrigger ), DBPRODUCTIONTYPE );
          dbcout( 'To type ' + toType.productionTypeDescr + ' should be vaccinated: ' + usBoolToText( toType.useVaccination ), DBPRODUCTIONTYPE );

          if( toType.useVaccination ) then
            begin
              priority := vaccPriorityList[ toType.productionTypeDescr + '+' + 'ring' ];

              str := str + '  <ring-vaccination-model to-production-type="' + toType.xmlProdTypeDescr + '" from-production-type="' + fromType.xmlProdTypeDescr + '">' + endl;

              str := str + '    <priority>' + intToStr( priority ) + '</priority> <!-- Priority is based only on the "to" type -->' + endl;

              str := str + '    <radius>' + endl;
              str := str + '      <value>' + usFloatToStr( fromType.ringVaccParams.ringRadius ) + '</value>' + endl;
              str := str + '      <units><xdf:unit>km</xdf:unit></units>' + endl;
              str := str + '    </radius>' + endl;

              str := str + '    <min-time-between-vaccinations>' + endl;
              str := str + '      <value>' + intToStr( toType.ringVaccParams.minTimeBetweenVacc ) + '</value>' + endl;
              str := str + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
              str := str + '    </min-time-between-vaccinations>' + endl;

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
      msg := msg + 'simulateTransition: ' + boolToStr(self.simulateTransition) + endl;

      if( updated ) then msg := msg + '**UPDATED**' + endl;

      dbcout( msg, true );

      if( nil <> diseaseLatent ) then
      	begin
          dbcout( 'diseaseLatent', true );
          diseaseLatent.debug();
        end
      else
      	dbcout( 'NO LATENT PERIOD DEFINED', true )
      ;

      if( nil <> diseaseSubclinical ) then
      	begin
          dbcout( endl + 'diseaseSubclinical', true );
          diseaseSubclinical.debug();
        end
      else
      	dbcout( endl + 'NO SUBCLINICAL PERIOD DEFINED', true )
      ;

      if( nil <> diseaseClinical ) then
      	begin
          dbcout( endl + 'diseaseClinical', true );
          diseaseClinical.debug();
        end
      else
      	dbcout( endl + 'NO CLINICAL PERIOD DEFINED', true )
      ;

      if( nil <> diseaseImmune ) then
      	begin
          dbcout( endl + 'diseaseImmune', true);
          diseaseImmune.debug();
        end
      else
      	dbcout( endl + 'NO IMMUNE PERIOD DEFINED', true )
      ;

      if( (_sim as TSMSimulationInput).useWithinHerdPrevalence ) then
        begin
          if( nil <> diseasePrevalence ) then
            begin
              dbcout( endl + 'diseasePrevalence', true);
              diseasePrevalence.debug();
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

      // FIX ME: debug the control parameters!

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
      _outputs.setDailyRecordsFrom( _initialOutputs );

      // Units with an initial state other than susceptible
      // will be set by the core model code.  Initially susceptible
      // units, though, need to be set here.
      _outputs.tscUSusc := _initialOutputs.tscUSusc;
      _outputs.tscASusc := _initialOutputs.tscASusc;
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


  function TProductionType.addDetection( const herdAnimalCount: integer; const day: integer ): boolean;
  	begin
      inc( _outputs.detnUClin );
      inc( _outputs.detnAClin, herdAnimalCount );
      inc( _outputs.detcUClin );
      inc( _outputs.detcAClin, herdAnimalCount );
      inc( _outputs.appUInfectious );

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


  procedure TProductionType.addZoneFocus( const day: integer );
    begin
      inc( _outputs.zonnFoci );
      inc( _outputs.zoncFoci );
    end
  ;


  procedure TProductionType.decrementApparentInfectiousUnits();
    begin
      dec( _outputs.appUInfectious );
    end
  ;


  function TProductionType.addDestructionEvent( const herdAnimalCount: integer; reason: string; const day: integer ): boolean;
  	begin
    	reason := fixup( reason );

      if( -1 = _outputs.firstDestruction ) then
        begin
          _outputs.firstDestruction := day;
          result := true;
        end
      else
        result := false
      ;

      if( 'trace out-indirect contact' = reason ) then
      	begin
          inc( _outputs.descUInd );
          inc( _outputs.descAInd, herdAnimalCount );
        end
      else if( 'trace out-direct contact' = reason ) then
      	begin
          inc( _outputs.descUDir );
          inc( _outputs.descADir, herdAnimalCount );
        end
      else if( 'ring destruction' = reason ) then
      	begin
          inc( _outputs.descURing );
          inc( _outputs.descARing, herdAnimalCount );
        end
      else if( 'reported diseased' = reason ) then
      	begin
          inc( _outputs.descUDet );
          inc( _outputs.descADet, herdAnimalCount );
        end
      else if( 'initially destroyed' = reason ) then
        begin
          inc( _outputs.descUIni );
          inc( _outputs.descAIni, herdAnimalCount );
        end
      else
				raise exception.Create( 'Unrecognized destruction reason (' + reason + ') in TProductionType.addDestructionEvent' )
      ;

      inc( _outputs.desnUAll );
      inc( _outputs.desnAAll, herdAnimalCount );
    end
  ;


  function TProductionType.addVaccinationEvent( const herdAnimalCount: integer; reason: string; const day: integer ): boolean;
  	begin
    	reason := fixup( reason );

      if( -1 = _outputs.firstVaccination ) then
        begin
          _outputs.firstVaccination := day;
          result := true;
        end
      else
        result := false
      ;

      if( 'ring vaccination' = reason ) then
      	begin
          inc( _outputs.vaccURing );
          inc( _outputs.vaccARing, herdAnimalCount );
        end
      else if( 'initially immune' = reason ) then
        begin
          inc( _outputs.vaccUIni );
          inc( _outputs.vaccAIni, herdAnimalCount );
        end
      else
				raise exception.Create( 'Unrecognized vaccination reason (' + reason + ') in TProductionType.addVaccinationEvent' )
      ;

      inc( _outputs.vaccnUAll );
      inc( _outputs.vaccnAAll, herdAnimalCount );

    end
  ;


  procedure TProductionType.addAttemptedTraceEvent( const herdAnimalCount: integer; mechanism: string );
    begin
    	mechanism := fixup( mechanism );

      if( 'direct contact' = mechanism ) then
      	begin
          inc( _outputs.trcUDirp );
          inc( _outputs.trcADirp, herdAnimalCount );
        end
      else if( 'indirect contact' = mechanism ) then
      	begin
          inc( _outputs.trcUIndp );
          inc( _outputs.trcAIndp, herdAnimalCount );
        end
      else
      	raise exception.Create( 'Unrecognized trace reason (' + mechanism + ') in TProductionType.addAttemptedTraceEvent' )
      ;
    end
  ;


  procedure TProductionType.addTraceEvent( const herdAnimalCount: integer; mechanism: string );
  	begin
    	mechanism := fixup( mechanism );

      if( 'direct contact' = mechanism ) then
      	begin
          inc( _outputs.trnUDir );
          inc( _outputs.trnADir, herdAnimalCount );
          inc( _outputs.trcUDir );
          inc( _outputs.trcADir, herdAnimalCount );
        end
      else if( 'indirect contact' = mechanism ) then
      	begin
          inc( _outputs.trnUInd );
          inc( _outputs.trnAInd, herdAnimalCount );
          inc( _outputs.trcUInd );
          inc( _outputs.trcAInd, herdAnimalCount );
        end
      else
      	raise exception.Create( 'Unrecognized trace reason (' + mechanism + ') in TProductionType.addTraceEvent' )
      ;
    end
  ;


	procedure TProductionType.addExposedByMechanism( const herdAnimalCount: integer; mechanism: string );
  	begin
    	mechanism := fixup( mechanism );

      if( 'direct contact' = mechanism ) then
      	begin
          inc( _outputs.expcUDir );
          inc( _outputs.expcADir, herdAnimalCount );
        end
      else if( 'indirect contact' = mechanism ) then
      	begin
          inc( _outputs.expcUInd );
          inc( _outputs.expcAInd, herdAnimalCount );
        end
      else
      	raise exception.Create( 'Unrecognized exposure mechanism (' + mechanism + ') in TProductionType.addExposedByMechanism' )
      ;

    end
  ;


	procedure TProductionType.addInfectedByMechanism( const herdAnimalCount: integer; infMech: string; const day: integer );
  	begin
    	infMech := fixup( infMech );

   		if( 'initially infected' = infMech ) then
      	begin
          if( 1 < day ) then
            raise exception.Create( '''Initial'' infection occurring after day 1.' )
          ;
          inc( _outputs.infcUIni );
          inc( _outputs.infcAIni, herdAnimalCount );
        end
      else if( 'airborne spread' = infMech ) then
      	begin
          inc( _outputs.infcUAir );
          inc( _outputs.infcAAir, herdAnimalCount );
          inc( _outputs.infnUAir );
          inc( _outputs.infnAAir, herdAnimalCount );
        end
      else if( 'direct contact' = infMech ) then
      	begin
          inc( _outputs.infcUDir );
          inc( _outputs.infcADir, herdAnimalCount );
          inc( _outputs.infnUDir );
          inc( _outputs.infnADir, herdAnimalCount );
        end
      else if( 'indirect contact' = infMech ) then
      	begin
          inc( _outputs.infcUInd );
          inc( _outputs.infcAInd, herdAnimalCount );
          inc( _outputs.infnUInd );
          inc( _outputs.infnAInd, herdAnimalCount );
        end
      else
      	raise exception.Create( 'Unrecognized infection mechanism (' + infMech + ') in TProductionType.addInfectedByMechanism' )
      ;
    end
  ;

  {*
   Set daily and cumulative numbers of animals/herds in the various disease states
   according to their initial values in the database (dynHerd.initialStateCode)
  }
  procedure TProductionType.setInitialDailyRecords( const herdAnimalCount: integer; const herdDiseaseState: TTransitionState );
  	begin

      dbcout( '*** TProductionType.setInitialDailyRecords', DBPRODUCTIONTYPE );

    	case herdDiseaseState of
        tsSusceptible:
          begin
            inc( _initialOutputs.tsdUSusc );
            inc( _initialOutputs.tsdASusc, herdAnimalCount );
            inc( _initialOutputs.tscUSusc );
            inc( _initialOutputs.tscASusc, herdAnimalCount );
          end
        ;
        tsLatent:
          begin
            inc( _initialOutputs.tsdULat );
            inc( _initialOutputs.tsdALat, herdAnimalCount );
            inc( _initialOutputs.tscULat );
            inc( _initialOutputs.tscALat, herdAnimalCount );
          end
        ;
        tsSubClinical:
          begin
            inc( _initialOutputs.tsdUSubc );
            inc( _initialOutputs.tsdASubc, herdAnimalCount );
            inc( _initialOutputs.tscUSubc );
            inc( _initialOutputs.tscASubc, herdAnimalCount );
          end
        ;
        tsClinical:
          begin
            inc( _initialOutputs.tsdUClin );
            inc( _initialOutputs.tsdAClin, herdAnimalCount );
            inc( _initialOutputs.tscUClin );
            inc( _initialOutputs.tscAClin, herdAnimalCount );
          end
        ;
        tsNaturalImmune:
          begin
            inc( _initialOutputs.tsdUNImm );
            inc( _initialOutputs.tsdANImm, herdAnimalCount );
            inc( _initialOutputs.tscUNImm );
            inc( _initialOutputs.tscANImm, herdAnimalCount );
          end
        ;
        tsVaccineImmune:
          begin
            inc( _initialOutputs.tsdUVImm );
            inc( _initialOutputs.tsdAVImm, herdAnimalCount );
            inc( _initialOutputs.tscUVImm );
            inc( _initialOutputs.tscAVImm, herdAnimalCount );
          end
        ;
        tsDestroyed:
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
        herdAnimalCount: integer;
        oldState: TTransitionState;
        newState: TTransitionState
      );
  	begin
      // Decrement the daily number of units in oldState
      // Subtract herdAnimalCount from the daily number of animals in oldState
    	// Increment the daily number of units in newState
      // Add the herdAnimalCount to the daily number of animals in newState
    	// Increment the running number of units in newState
      // Add the herdAnimalCount to the running number of animals in newState

      dbcout( '*** TProductionType.updateDailyRecordsProdType', DBPRODUCTIONTYPE );

      if( newState = oldState ) then
        // do nothing
      else
        begin
          _outputs.decrementDailyCounts( herdAnimalCount, oldState );

          case newState of
            tsSusceptible:
              begin
                inc( _outputs.tsdUSusc );
                inc( _outputs.tsdASusc, herdAnimalCount );
                inc( _outputs.tscUSusc );
                inc( _outputs.tscASusc, herdAnimalCount );
              end
            ;
            tsLatent:
              begin
                inc( _outputs.tsdULat );
                inc( _outputs.tsdALat, herdAnimalCount );
                inc( _outputs.tscULat );
                inc( _outputs.tscALat, herdAnimalCount );
              end
            ;
            tsSubClinical:
              begin
                inc( _outputs.tsdUSubc );
                inc( _outputs.tsdASubc, herdAnimalCount );
                inc( _outputs.tscUSubc );
                inc( _outputs.tscASubc, herdAnimalCount );
              end
            ;
            tsClinical:
              begin
                inc( _outputs.tsdUClin );
                inc( _outputs.tsdAClin, herdAnimalCount );
                inc( _outputs.tscUClin );
                inc( _outputs.tscAClin, herdAnimalCount );
              end
            ;
            tsNaturalImmune:
              begin
                inc( _outputs.tsdUNImm );
                inc( _outputs.tsdANImm, herdAnimalCount );
                inc( _outputs.tscUNImm );
                inc( _outputs.tscANImm, herdAnimalCount );
              end
            ;
            tsVaccineImmune:
              begin
                inc( _outputs.tsdUVImm );
                inc( _outputs.tsdAVImm, herdAnimalCount );
                inc( _outputs.tscUVImm );
                inc( _outputs.tscAVImm, herdAnimalCount );
              end
            ;
            tsDestroyed:
              begin
                inc( _outputs.tsdUDest );
                inc( _outputs.tsdADest, herdAnimalCount );
                inc( _outputs.tscUDest );
                inc( _outputs.tscADest, herdAnimalCount );
              end
            ;
            else
              raise exception.Create( 'Unrecognized herd status in TProductionType.makeDailyRecord' )
            ;
          end;

        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType properties
//-----------------------------------------------------------------------------

	// Disease states
  //---------------
  procedure TProductionType.setLatentName( val: string );
    begin
      val := trim( val );
      _latentName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;

      _updated := true;
    end
  ;


  procedure TProductionType.setSubclinicalName( val: string );
    begin
      val := trim( val );
      _subclinicalName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      _updated := true;
    end
  ;


  procedure TProductionType.setClinicalName( val: string );
    begin
      val := trim( val );
      _clinicalName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      _updated := true;
    end
  ;


  procedure TProductionType.setImmuneName( val: string );
    begin
      val := trim( val );
      _immuneName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      
      _updated := true;
    end
  ;


  procedure TProductionType.setPrevalenceName( val: string );
    begin
      val := trim( val );
      _prevalenceName := val;

      if( '' <> val ) then
        begin
          if( fnDictionary.contains( val ) ) then
            fnDictionary.value( val ).incrRefCounter()
          ;
        end
      ;
      
      _updated := true;
    end
  ;


  function TProductionType.getDiseaseLatent(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _latentName ) ) then
            begin
              if( fnDictionary.value( _latentName ).fn is TPdf ) then
                result := fnDictionary.value( _latentName ).fn as TPdf
              else
                begin
                  setLatentName( '' );
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


  function TProductionType.getDiseaseSubclinical(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _subclinicalName ) ) then
            begin
              if( fnDictionary.value( _subclinicalName ).fn is TPdf ) then
                result := fnDictionary.value( _subclinicalName ).fn as TPdf
              else
                begin
                  setSubclinicalName( '' );
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


  function TProductionType.getDiseaseClinical(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _clinicalName ) ) then
            begin
              if( fnDictionary.value( _clinicalName ).fn is TPdf ) then
                result := fnDictionary.value( _clinicalName ).fn as TPdf
              else
                begin
                  setClinicalName( '' );
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


  function TProductionType.getDiseaseImmune(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _immuneName ) ) then
            begin
              if( fnDictionary.value( _immuneName ).fn is TPdf ) then
                result := fnDictionary.value( _immuneName ).fn as TPdf
              else
                begin
                  setImmuneName( '' );
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


  function TProductionType.getDiseasePrevalence(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _prevalenceName ) ) then
            begin
              if( fnDictionary.value( _prevalenceName ).fn is TRelFunction ) then
                result := fnDictionary.value( _prevalenceName ).fn as TRelFunction
              else
                begin
                  setPrevalenceName( '' );
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

      if( fnDictionary.contains( _latentName ) ) then
        begin
          if( not( fnDictionary.value( _latentName ).fn is TPdf ) ) then
            begin
              setLatentName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _subclinicalName ) ) then
        begin
          if( not( fnDictionary.value( _subclinicalName ).fn is TPdf ) ) then
            begin
              setSubclinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _clinicalName ) ) then
        begin
          if( not( fnDictionary.value( _clinicalName ).fn is TPdf ) ) then
            begin
              setClinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _immuneName ) ) then
        begin
          if( not( fnDictionary.value( _immuneName ).fn is TPdf ) ) then
            begin
              setImmuneName( '' );
              result := false;
            end
          ;
        end
      ;

      includePrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;
      if( includePrevalence and fnDictionary.contains( _prevalenceName ) ) then
        begin
          if( not( fnDictionary.value( _prevalenceName ).fn is TRelFunction ) ) then
            begin
              setPrevalenceName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;


  function TProductionType.getlatentName(): string; begin result := _latentName; end;
  function TProductionType.getsubclinicalName(): string; begin result := _subclinicalName; end;
  function TProductionType.getclinicalName(): string; begin result := _clinicalName; end;
  function TProductionType.getimmuneName(): string; begin result := _immuneName; end;
  function TProductionType.getPrevalenceName(): string; begin result := _prevalenceName; end;


  // Detection
  //----------
  procedure TProductionType.setUseDetection( val: boolean );
    begin
      _useDetection := val;
      _updated := true;
    end
  ;


  procedure TProductionType.setDetectionParams( sv: TDetectionParams );
    begin
      if( nil <> _detection ) then freeAndNil( _detection );
      _detection := sv;
      _updated := true;
    end
  ;


  function TProductionType.getUseDetection(): boolean; begin result := _useDetection; end;
  function TProductionType.getDetectionParams(): TDetectionParams; begin result := _detection; end;


  // Destruction
  //------------
  (*
  procedure TProductionType.setUseDestruction( val: boolean );
    begin
      _useDestruction := val;
      _updated := true;
    end
  ;
  *)

  procedure TProductionType.setDestructionParams( dem: TDestructionParams );
    begin
      if( nil <> _destr ) then freeAndNil( _destr );
      _destr := dem;
      _updated := true;
      //_useDestruction := true;
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

  // Zones
  //-------
  procedure TProductionType.setZoneParams( z: TZoneParams );
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

  function TProductionType.getZoneParams(): TZoneParams; begin result := _zoneParams; end;

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
      	result := vaccinationparams.useVaccination
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
                end;
            end
          ;

          if( nil <> _destr ) then
            begin
              if( _destr.updated ) then
                begin
                 result := true;
                 exit;
                end;
            end
          ;

          if( nil <> _detection ) then
            begin
              if( _detection.updated ) then
                begin
                 result := true;
                 exit;
                end;
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
                end;
            end
          ;

          if( nil <> _vacc ) then
            begin
              if( _vacc.updated ) then
                begin
                 result := true;
                 exit;
                end;              
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
      _xmlProdTypeDescr := encodeXml( val );

      _detection.prodTypeDescr := val;
      _ringVacc.prodTypeDescr := val;
      _zoneParams.prodTypeDescr := val;
      _destr.prodTypeDescr := val;
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

  function TProductionType.getCurrentOutputs(): TSMSimOutByProdType; begin result := _outputs; end;
  function TProductionType.getInitialOutputs(): TSMSimOutByProdType; begin result := _initialOutputs; end;

  procedure TProductionType.setRemoved( val: boolean ); begin _removed := val; _updated := true; end;
  function TProductionType.getRemoved(): boolean; begin result := _removed; end;
//-----------------------------------------------------------------------------



end.
