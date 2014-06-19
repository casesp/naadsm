unit GlobalControlParams;

(*
GlobalControlParams.pas
------------------------
Begin: 2005/06/03
Last revision: $Date: 2013-06-27 19:11:34 $ $Author: areeves $
Version number: $Revision: 1.49.4.6 $
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
    QLists,
    QStringMaps,

    Sdew,

    Models,
    ModelDatabase,
    ChartFunction,
    RelFunction,

    SMDatabase,
    FunctionEnums
  ;

  type TGlobalControlParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;

      _useDetectionGlobal: boolean;
      _useTracingGlobal: boolean;
      _useTracingHerdExamGlobal: boolean;
      _useTracingTestingGlobal: boolean;
      _useDestrGlobal: boolean;
      _useVaccGlobal: boolean;
      _useZonesGlobal: boolean;

      _destrProgramDelay: integer;
      _destrDelay: integer;

      _vaccDetectedUnitsBeforeStart: integer;

      _relDestrCapacityName: string;
      _relVaccCapacityName: string;

      _destrPriorityOrder: string;
      _destrReasonOrder: string;
      _vaccPriorityOrder: string;
      _vaccReasonOrder: string;

      // for determining priority order of destruction and vaccination for SharcSpread-compatible XML
      _destrOrderList: TQStringLongIntMap;
      _vaccOrderList: TQStringLongIntMap;

      procedure initialize();

      // XML import
      function getXmlModelList(): TQStringList;

			// property getters and setters
      //-----------------------------
      procedure setUseDetectionGlobal( val: boolean );
      procedure setUseTracingGlobal( val: boolean );
      procedure setUseTracingHerdExamGlobal( val: boolean );
      procedure setUseTracingTestingGlobal( val: boolean );
      procedure setUseDestrGlobal( val: boolean );
      procedure setUseVaccGlobal( val: boolean );
      procedure setUseZonesGlobal( val: boolean );
      procedure setDestrProgramDelay( val: integer );
      procedure setVaccDetectedUnitsBeforeStart( val: integer );

      function getUseDetectionGlobal(): boolean;
      function getUseTracingGlobal(): boolean;
      function getUseTracingHerdExamGlobal(): boolean;
      function getUseTracingTestingGlobal(): boolean;
      function getUseDestrGlobal(): boolean;
      function getUseVaccGlobal(): boolean;
      function getUseZonesGlobal(): boolean;

      function getDestrProgramDelay(): integer;
      function getVaccDetectedUnitsBeforeStart(): integer;

      procedure setDestrPriorityOrder( val: string );
      procedure setDestrReasonOrder( val: string );
      procedure setVaccPriorityOrder( val: string );
      function getDestrPriorityOrder(): string;
      function getDestrReasonOrder(): string;
      function getVaccPriorityOrder(): string;
      procedure setVaccReasonOrder( val: string );
      function getVaccReasonOrder(): string;

      function getSSDestrPriorities(): TQStringLongIntMap;
      procedure setSSDestrPriorities( newPriorities: TQStringLongIntMap );
      procedure buildSSDestructionPriorities();

      function getSSVaccPriorities(): TQStringLongIntMap;
      procedure setSSVaccPriorities( newPriorities: TQStringLongIntMap );
      procedure buildSSVaccPriorities();

      procedure setRelDestrCapacityName( val: string );
      procedure setRelVaccCapacityName( val: string );
      function getRelDestrCapacityName(): string;
      function getRelVaccCapacityName(): string;

      function getRelDestrCapacity(): TRelFunction;
      function getRelVaccCapacity(): TRelFunction;


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
      constructor create(); overload;
      constructor create( db: TSMDatabase; sim: TObject ); overload;
      constructor create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil ); overload;
      constructor create( const src: TGlobalControlParams; sim: TObject ); overload;
      destructor destroy(); override;

      procedure renameProductionType( const oldProdTypeDescr, newProdTypeDescr: string );

      // Overridden from TModel
      //-----------------------
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      function functionsAreValid(): boolean; override;

      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;

			function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer; reintroduce;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      procedure recalculatePriorities();

      property useDetectionGlobal: boolean read getUseDetectionGlobal write setUseDetectionGlobal;
      property useTracingGlobal: boolean read getuseTracingGlobal write setUseTracingGlobal;
      property useTracingHerdExamGlobal: boolean read getuseTracingHerdExamGlobal write setUseTracingHerdExamGlobal;
      property useTracingTestingGlobal: boolean read getuseTracingTestingGlobal write setUseTracingTestingGlobal;

      property useDestructionGlobal: boolean read getUseDestrGlobal write setUseDestrGlobal;
      property useVaccGlobal: boolean read getUseVaccGlobal write setUseVaccGlobal;
      property useZonesGlobal: boolean read getUseZonesGlobal write setUseZonesGlobal;

      property destrProgramDelay: integer read getDestrProgramDelay write setDestrProgramDelay;
      property vaccDetectedUnitsBeforeStart: integer read getVaccDetectedUnitsBeforeStart write setVaccDetectedUnitsBeforeStart;

      property destrPriorityOrder: string read getDestrPriorityOrder write setDestrPriorityOrder;
      property destrReasonOrder: string read getDestrReasonOrder write setDestrReasonOrder;
      property vaccPriorityOrder: string read getVaccPriorityOrder write setVaccPriorityOrder;
      property vaccReasonOrder: string read getVaccReasonOrder write setVaccReasonOrder;

      property ssDestrPriorities: TQStringLongIntMap read getSSDestrPriorities write setSSDestrPriorities;
      property ssVaccPriorities: TQStringLongIntMap read getSSVaccPriorities write setSSVaccPriorities;

      property relDestrCapacityName: string read getRelDestrCapacityName write setRelDestrCapacityName;
      property relVaccCapacityName: string read getRelVaccCapacityName write setRelVaccCapacityName;

      property relDestrCapacity: TRelFunction read getRelDestrCapacity;
      property relVaccCapacity: TRelFunction read getRelVaccCapacity;

    end
  ;


  const
    DBGLOBALCONTROLPARAMS: boolean = false; // Set to true to enable debugging messages for this unit.

implementation

  uses
    SysUtils,
    Variants,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    CStringList,
    I88n,
    Points,

    ProductionType,
    ProductionTypeList,
    FunctionDictionary,
    SMSimulationInput
  ;


//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TGlobalControlParams.create();
  	begin
    	inherited create();
			initialize();
    end
  ;


  constructor TGlobalControlParams.create( const src: TGlobalControlParams; sim: TObject );
    begin
      inherited create( src );
      initialize();
      _sim := sim;

      _useDetectionGlobal := src._useDetectionGlobal;
      _useTracingGlobal := src._useTracingGlobal;
      _useTracingHerdExamGlobal := src._useTracingHerdExamGlobal;
      _useTracingTestingGlobal := src._useTracingTestingGlobal;
      _useDestrGlobal := src._useDestrGlobal;
      _useVaccGlobal := src._useVaccGlobal;
      _useZonesGlobal := src._useZonesGlobal;

      _destrProgramDelay := src._destrProgramDelay;
      _destrDelay := src._destrDelay;

      _vaccDetectedUnitsBeforeStart := src._vaccDetectedUnitsBeforeStart;

      setRelDestrCapacityName( src._relDestrCapacityName );
      setRelVaccCapacityName( src._relVaccCapacityName );

      _destrPriorityOrder := src._destrPriorityOrder;
      _destrReasonOrder := src._destrReasonOrder;
      _vaccPriorityOrder := src._vaccPriorityOrder;
      _vaccReasonOrder := src._vaccReasonOrder;

      _destrOrderList.clear();
      _destrOrderList.assign( src._destrOrderList );

      _vaccOrderList.clear();
      _vaccOrderList.assign( src._vaccOrderList );

      _updated := src._updated;
    end
  ;


  constructor TGlobalControlParams.create( db: TSMDatabase; sim: TObject );
  	var
    	db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
      q: string;
  	begin
    	inherited create();
    	initialize();

      dbcout( '^^^ Creating TGlobalControlParams from database.', DBGLOBALCONTROLPARAMS );
      _sim := sim;

      db2 := db as TSqlDatabase;


      q := 'SELECT'
        //+ ' inControlsGlobal.controlsGlobalID,'
        + ' inControlsGlobal.includeDetection,'
        + ' inControlsGlobal.includeTracing,'
        + ' inControlsGlobal.includeTracingHerdExam,'
        + ' inControlsGlobal.includeTracingTesting,'
        + ' inControlsGlobal.includeDestruction,'
        + ' inControlsGlobal.destrProgramDelay,'
        + ' inControlsGlobal.destrCapacityRelID,'
        + ' destrCapacityChart.chartName AS destrCapacityChartName,'
        + ' inControlsGlobal.destrPriorityOrder,'
        + ' inControlsGlobal.destrReasonOrder,'
        + ' inControlsGlobal.includeVaccination,'
        + ' inControlsGlobal.vaccDetectedUnitsBeforeStart,'
        + ' inControlsGlobal.vaccCapacityRelID,'
        + ' vaccCapacityChart.chartName AS vaccCapacityChartName,'
        + ' inControlsGlobal.vaccPriorityOrder,'
        + ' inControlsGlobal.includeZones'
        + ' FROM'
        + ' ('
        + ' inControlsGlobal'
        + ' LEFT OUTER JOIN inChart destrCapacityChart'
        + ' ON destrCapacityChart.chartID = inControlsGlobal.destrCapacityRelID'
        + ' )'
        + ' LEFT OUTER JOIN inChart vaccCapacityChart'
        + ' ON vaccCapacityChart.chartID = inControlsGlobal.vaccCapacityRelID'
      ;


      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('includeDetection') ) then
        useDetectionGlobal := boolean (row.field('includeDetection') )
      else
        useDetectionGlobal := false
      ;

      if( null <> row.field('includeTracing') ) then
        useTracingGlobal := boolean( row.field('includeTracing') )
      else
        useTracingGlobal := false
      ;

      if( null <> row.field('includeTracingHerdExam') ) then
        useTracingHerdExamGlobal := boolean( row.field('includeTracingHerdExam') )
      else
        useTracingHerdExamGlobal := false
      ;

      if( null <> row.field('includeTracingTesting') ) then
        useTracingTestingGlobal := boolean( row.field('includeTracingTesting') )
      else
        useTracingTestingGlobal := false
      ;

      if( null <> row.field('includeDestruction') ) then
        useDestructionGlobal := boolean( row.field('includeDestruction') )
      else
        useDestructionGlobal := false
      ;
      if( null <> row.field('destrProgramDelay') ) then destrProgramDelay := integer( row.field('destrProgramDelay') );

      if( null <> row.field('destrCapacityRelID') ) then
        begin
      	  setRelDestrCapacityName( row.field( 'destrCapacityChartName' ) );
        end
      ;

      if( null <> row.field('includeVaccination') ) then
        useVaccGlobal := boolean( row.field('includeVaccination') )
      else
        useVaccGlobal := false
      ;
      if( null <> row.field('vaccDetectedUnitsBeforeStart') ) then vaccDetectedUnitsBeforeStart := integer( row.field('vaccDetectedUnitsBeforeStart') );

      if( null <> row.field('vaccCapacityRelID') ) then
        begin
      	  setRelVaccCapacityName( row.field( 'vaccCapacityChartName' ) );
        end
      ;

      if( null <> row.field('destrPriorityOrder') ) then
        destrPriorityOrder := row.field('destrPriorityOrder')
      ;

      if( null <> row.field('destrReasonOrder') ) then
        destrReasonOrder := row.field('destrReasonOrder')
      ;

      if( null <> row.field('vaccPriorityOrder') ) then
        vaccPriorityOrder := row.field('vaccPriorityOrder')
      ;

      _vaccReasonOrder := 'ring'; // This is currently the only reason for vaccination.

      if( null <> row.field('includeZones' ) ) then
        useZonesGlobal := boolean( row.field('includeZones') )
      else
        useZonesGlobal := false
      ;

      res.free();

      buildSSDestructionPriorities();
      buildSSVaccPriorities();

      _updated := false;
    end
  ;


  constructor TGlobalControlParams.create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil );
    var
      nModels: integer;
      modelName: string;
      i: integer;
      model: pointer;
    begin
    	inherited create();
    	initialize();
      _sim := sim;

      nModels := sdew.GetElementCount( models );

      for i := 0 to nModels - 1 do
        begin
          model := sdew.GetElementByIndex( models, i );
          modelName := sdew.GetElementName( model );

          if( xmlModelList.contains( modelName ) ) then
            importXml( model, sdew, errMsg )
          ;
        end
      ;
    end
  ;


	procedure TGlobalControlParams.initialize();
  	begin
      _xmlModelList := nil;

      _destrProgramDelay := 0;

      _useDetectionGlobal := false;
      _useTracingGlobal := false;
      _useTracingHerdExamGlobal := false;
      _useTracingTestingGlobal := false;
      _useDestrGlobal := false;
      _useVaccGlobal := false;
      _useZonesGlobal := false;

      _vaccDetectedUnitsBeforeStart := 0;

      _destrPriorityOrder := '';
      _destrReasonOrder  := '';
      _vaccPriorityOrder  := '';
      _vaccReasonOrder := '';

      _destrOrderList := TQStringLongIntMap.create();
      _vaccOrderList := TQStringLongIntMap.create();

      _updated := false;
    end
  ;


  destructor TGlobalControlParams.destroy();
  	begin
      freeAndNil( _xmlModelList );

      // Don't free functions, but decrement their reference counters.
      setRelDestrCapacityName( '' );
      setRelVaccCapacityName( '' );
      
    	freeAndNil( _destrOrderList );
      freeAndNil( _vaccOrderList );

    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

  procedure TGlobalControlParams.recalculatePriorities();
    begin
      buildSSDestructionPriorities();
      buildSSVaccPriorities();
    end
  ;


  procedure TGlobalControlParams.renameProductionType( const oldProdTypeDescr, newProdTypeDescr: string );
    begin
      if( nil <> _destrOrderList ) then
        begin
          if( _destrOrderList.HasKey( oldProdTypeDescr + '+basic' ) ) then
            _destrOrderList.rename( oldProdTypeDescr + '+basic', newProdTypeDescr + '+basic' );

          if( _destrOrderList.HasKey( oldProdTypeDescr + '+direct' ) ) then
            _destrOrderList.rename( oldProdTypeDescr + '+direct', newProdTypeDescr + '+direct' );

          if( _destrOrderList.HasKey( oldProdTypeDescr + '+indirect' ) ) then
            _destrOrderList.rename( oldProdTypeDescr + '+indirect', newProdTypeDescr + '+indirect' );

          if( _destrOrderList.HasKey( oldProdTypeDescr + '+ring' ) ) then
            _destrOrderList.rename( oldProdTypeDescr + '+ring', newProdTypeDescr + '+ring' );
        end
      ;

      if( nil <> _vaccOrderList ) then
        begin
          if( _vaccOrderList.HasKey( oldProdTypeDescr + '+ring' ) ) then
            _vaccOrderList.rename( oldProdTypeDescr + '+ring', newProdTypeDescr + '+ring' );
        end
      ;

      setUpdated( true );
    end
  ;


  procedure TGlobalControlParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        DestrCapacityGlobal: self.relDestrCapacityName := newName;
        vaccCapacityGlobal: self.relVaccCapacityName := newName;
      end;
    end
  ;


  function TGlobalControlParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      case whichChart of
        DestrCapacityGlobal: result := ( chartName = self.relDestrCapacityName );
        VaccCapacityGlobal: result := ( chartName = self.relVaccCapacityName );
        else
          begin
            raise exception.Create( 'Unrecognized whichChart in TGlobalControlParams.hasChartName' );
            result := false;
          end
        ;
      end;
    end
  ;


  function TGlobalControlParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    var
      Item:TFunctionDictionaryItem;
      ret_val:TChartFunction;
    begin
      ret_val := nil;

      case whichChart of
        DestrCapacityGlobal:
          begin
            Item := (_sim as TSMSimulationInput).functionDictionary.value( self.relDestrCapacityName );
            if ( nil <> Item ) then
              ret_val := Item.fn;
          end;
        VaccCapacityGlobal:
          begin
            Item := (_sim as TSMSimulationInput).functionDictionary.value( self.relVaccCapacityName );
            if ( nil <> Item ) then
              ret_val := Item.fn;
          end;
      end;

      result := ret_val;
    end
  ;


  procedure TGlobalControlParams.removeChart( const chartName: string );
    begin
      if( chartName = self.relVaccCapacityName ) then self.relVaccCapacityName :='';
      if( chartName = self.relDestrCapacityName ) then self.relDestrCapacityName := '';

      // The _updated flag will be set by the properties above, if necessary
    end
  ;

    
//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TGlobalControlParams.populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer;
  	var
      qDict: TQueryDictionary;
      q: string;
    begin
      qDict := TQueryDictionary.create();

      qDict['controlsGlobalID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );
      
      qDict['includeDetection'] := usBoolToText( useDetectionGlobal );
      qDict['includeTracing'] := usBoolToText( useTracingGlobal );
      qDict['includeTracingHerdExam'] := usBoolToText( useTracingHerdExamGlobal );
      qDict['includeTracingTesting'] := usBoolToText( useTracingTestingGlobal );
      qDict['includeDestruction'] := usBoolToText( useDestructionGlobal );
      qDict['includeVaccination'] := usBoolToText( useVaccGlobal );
      qDict['includeZones'] := usBoolToText( useZonesGlobal );

      qDict['destrProgramDelay'] := intToStr( destrProgramDelay );

      if( nil <> relDestrCapacity ) then
      	qDict['destrCapacityRelID'] := intToStr( relDestrCapacity.id )
      else
      	qDict['destrCapacityRelID'] := DATABASE_NULL_VALUE
      ;

      qDict['destrPriorityOrder'] := db.sqlQuote( self.destrPriorityOrder );
      if( 0 >= length( self.destrReasonOrder ) ) then
        self.destrReasonOrder := 'basic,direct-forward,indirect-forward,ring,direct-back,indirect-back'
      ;
      qDict['destrReasonOrder'] := db.sqlQuote( self.destrReasonOrder );

      qDict['vaccDetectedUnitsBeforeStart'] := intToStr( vaccDetectedUnitsBeforeStart );

      if( nil <> relVaccCapacity ) then
      	qDict['vaccCapacityRelID'] := intToStr( relVaccCapacity.id )
      else
      	qDict['vaccCapacityRelID'] := DATABASE_NULL_VALUE
      ;

      //  This was commented out...., uncommented on 10-10-2006 by SPC
      qDict['vaccPriorityOrder'] := db.sqlQuote( self.vaccPriorityOrder );

      if( MDBAForceUpdate = updateAction ) then
      	q := writeQuery( 'inControlsGlobal', QUpdate, qDict )
      else
      	q := writeQuery( 'inControlsGlobal', QInsert, qDict )
      ;

      result := integer( db.execute( q ) );

      qDict.Clear();
      qDict.Free();

      updated := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// chart handling (reimplemented from TModelWithFunctions)
//-----------------------------------------------------------------------------
  function TGlobalControlParams.ssXml(): string;
  	var
    	s: string;
      includeDestruction: boolean;
      includeVaccination: boolean;
      tmpRel: TRelFunction;
      tmpPnts: RPointArray;
    begin
    	s := endl;

      includeDestruction := (_sim as TSMSimulationInput).includeDestructionGlobal;
      includeVaccination := (_sim as TSMSimulationInput).includeVaccinationGlobal;

      // Model header
      //-------------
      s := s + '  <resources-and-implementation-of-controls-model>' + endl;

      // Global destruction options
      //---------------------------
      if( not( includeDestruction ) ) then
        begin
          s := s + '    <!-- This scenario does not actually use destruction! -->' + endl;
          s := s + '    <!-- This block must be written, however, to keep the core library happy. -->' + endl;
        end
      ;

      // Destruction delay
      //------------------
      s := s + '    <destruction-program-delay>' + endl;

      if( includeDestruction ) then
        s := s + '      <value>' + intToStr( destrProgramDelay ) + '</value>' + endl
      else // use default parameter
        s := s + '      <value>0</value>' + endl;
      ;

      s := s + '      ' + chartUnitTypeAsSSXml( UDays ) + endl;
      s := s + '    </destruction-program-delay>' + endl;


      // Destruction capacity
            //----------------------
      s := s + '    <destruction-capacity>' + endl;

      if( includeDestruction ) then
        s := s + relDestrCapacity.ssXml( 3 )
      else // use default capacity of 0
        begin
          {s := s + '      <value>1</value>   <value>0</value>' + endl;
          s := s + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
          s := s + '      <units><xdf:unit>herd</xdf:unit><xdf:unit power="-1">day</xdf:unit></units>' + endl;}

          SetLength(tmpPnts, 1);
          tmpPnts[0].x := 1;
          tmpPnts[0].y := 0;
          tmpRel := TRELFunction.create(tmpPnts, UDays, UHerdsPerDay);
          tmpRel.name := 'Default Destruction Capacity';
          s := s + tmpRel.ssXml( 3 );
          freeAndNil(tmpRel);
        end
      ;

      s := s + '    </destruction-capacity>' + endl;


      // Destruction priorities
      //------------------------
      s := s + '    <destruction-priority-order>' + endl;

      if( includeDestruction ) then
        s := s + '      ' + destrPriorityOrder + endl
      else // use default order
        s := s + '      reason,production type,time waiting' + endl
      ;

      s := s + '    </destruction-priority-order>' + endl;


      // Global vaccination options
      //---------------------------
      if( not( includeVaccination ) ) then
        begin
          s := s + '    <!-- This scenario does not actually use vaccination! -->' + endl;
          s := s + '    <!-- This block must be written, however, to keep the core library happy. -->' + endl;
        end
      ;

      // Vaccination capacity
      //----------------------
      s := s + '    <vaccination-capacity>' + endl;

      if( includeVaccination ) then
        s := s + relVaccCapacity.ssXml( 3 )
      else // use default capacity of 0
        begin
          {s := s + '      <value>1</value>   <value>0</value>' + endl;
          s := s + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
          s := s + '      <units><xdf:unit>herd</xdf:unit><xdf:unit power="-1">day</xdf:unit></units>' + endl;}

          SetLength(tmpPnts, 1);
          tmpPnts[0].x := 1;
          tmpPnts[0].y := 0;
          tmpRel := TRELFunction.create(tmpPnts, UDays, UHerdsPerDay);
          tmpRel.name := 'Default Vaccination Capacity';
          s := s + tmpRel.ssXml( 3 );
          freeAndNil(tmpRel);
          
        end
      ;
      s := s + '    </vaccination-capacity>' + endl;

      // Vaccination priority
      //----------------------
      s := s + '    <vaccination-priority-order>' + endl;

      if( includeVaccination ) then
        s := s + '      ' + vaccPriorityOrder + endl
      else // use default parameter
        s := s + '      reason,production type,time waiting' + endl
      ;

      s := s + '    </vaccination-priority-order>' + endl;


      // Vaccination program delay
      //--------------------------
      s := s + '    <vaccination-program-delay>' + endl;

      if( includeVaccination ) then
        s := s + '      ' + intToStr( vaccDetectedUnitsBeforeStart ) + endl
      else // use default parameter
        s := s + '      1' + endl;
      ;

      s := s + '    </vaccination-program-delay>' + endl;


      // Model footer
      //-------------
      s := s + '  </resources-and-implementation-of-controls-model>' + endl;
      s := s + endl;

    	result := s;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TGlobalControlParams.validate( err: PString = nil ): boolean;
  	var
    	msg: string;
    	submsg: string;
  	begin

    	// Charts must be valid
      // destrProgamDelay >= 0
      // vaccDetectedUnitsBeforeStart >= 0

    	result := true;
			msg := '';

      submsg := '';
      if( useDestructionGlobal ) then
      	begin
        	if( 0 > destrProgramDelay ) then
          	begin
              if( nil <> err ) then
              	msg := msg + '  ' + tr( 'Destruction program delay must be a greater than or equal to 0 days.' ) + endl
              ;
              result := false;
            end
          ;

          if( nil = relDestrCapacity ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction capacity is not set.' ) + endl;
              result := false;
            end
          else if( not( relDestrCapacity.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction capacity is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;

          if( 0 = length( trim( destrPriorityOrder ) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction priority order is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0 = length( trim( destrReasonOrder ) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction reason order is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      submsg := '';
      if( useVaccGlobal ) then
      	begin
        	if( 0 > vaccDetectedUnitsBeforeStart ) then
          	begin
              if( nil <> err ) then
              	msg := msg + '  ' + tr( 'Vaccination program delay (number of detected units) must be a greater than or equal to 0 days.' ) + endl
              ;
              result := false;
            end
          ;

          if( nil = relVaccCapacity ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination capacity is not set.' ) + endl;
              result := false;
            end
          else if( not( relVaccCapacity.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination capacity is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;

          if( 0 = length( trim( vaccPriorityOrder ) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination priority order is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + tr( 'Global control parameters:' ) + endl + msg;
          err^ := err^ + msg;
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Determining the SHARCSpread destruction and vaccination priorities
//-----------------------------------------------------------------------------
	procedure TGlobalControlParams.buildSSVaccPriorities();
  	var
			ptList: TProductionTypeList;
      it: TProductionTypeListIterator;
      reasonList: TCStringList;
      reason: pchar;
      i: integer;
      vaccReasonOrder: string;
      db: TSMDatabase;
    begin
    	if( nil = _sim  ) then
      	begin
      		raise exception.Create( 'Missing _sim in TGlobalControlParams.setSSPriorities' );
          exit;
        end
      else
        begin
          db := (_sim as TSMSimulationInput).database;

          if( nil = db  ) then
            begin
              raise exception.Create( 'Missing db in TGlobalControlParams.setSSPriorities' );
              exit;
            end
          ;
        end
      ;

      vaccReasonOrder := 'ring';
      (*
      if( length( 'ring' ) <> length( vaccReasonOrder ) ) then
      	vaccReasonOrder := 'ring'
      ;
      *)

      _vaccOrderList.clear();

      if( 0 < (_sim as TSMSimulationInput).ptList.Count ) then
        begin
          ptList := TProductionTypeList.create( (_sim as TSMSimulationInput).ptList, _sim );
          ptList.SortByVaccOrder();

          it := TProductionTypeListIterator.create( ptList );

          reasonList := TCStringList.create( vaccReasonOrder, ',' );

          i := 1;

          if( pos( 'production type', vaccPriorityOrder ) < pos( 'reason', vaccPriorityOrder ) ) then
            begin
              // Production type is higher priority than vaccination reason
              // So production type is the outer loop
              it.toFirst();
              while( nil <> it.current() ) do
                begin
                  if( it.current().isVaccTarget ) then
                    begin
                      reason := reasonList.first();
                      while( nil <> reason ) do
                        begin
                          _vaccOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                          inc( i );
                          reason := reasonList.next();
                        end
                      ;
                    end
                  ;
                  it.incr();
                end
              ;
            end
          else
            begin
              // Vaccination reason is higher priority than production type
              // So vaccination reason is the outer loop
              reason := reasonList.first();
              while( nil <> reason ) do
                begin
                  it.toFirst();

                  while( nil <> it.current() ) do
                    begin
                      if( it.current().isVaccTarget ) then
                        begin
                          _vaccOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                          inc( i );
                        end
                      ;
                      it.incr();
                    end
                  ;
                  reason := reasonList.next();
                end
              ;
            end
          ;

          freeAndNil( reasonList );
          freeAndNil( it );
          freeAndNil( ptList );
        end
      ;
    end
  ;


  procedure TGlobalControlParams.buildSSDestructionPriorities();
  	var
			ptList: TProductionTypeList;
      it: TProductionTypeListIterator;
      reasonList: TCStringList;
      reason: pchar;
      i: integer;
      db: TSMDatabase;
    begin
    	if( nil = _sim  ) then
      	begin
      		raise exception.Create( 'Missing _sim in TGlobalControlParams.setSSPriorities' );
          exit;
        end
      else
        begin
          db := (_sim as TSMSimulationInput).database;

          if( nil = db  ) then
            begin
              raise exception.Create( 'Missing db in TGlobalControlParams.setSSPriorities' );
              exit;
            end
          ;
        end
      ;

      if( length( 'basic,direct-forward,indirect-forward,ring,direct-back,indirect-back' ) <> length( destrReasonOrder ) ) then
      	destrReasonOrder := 'basic,direct-forward,indirect-forward,ring,direct-back,indirect-back'
      ;

      _destrOrderList.clear();

      ptList := TProductionTypeList.create( (_sim as TSMSimulationInput).ptList, _sim );
      ptList.SortByDestrOrder();

      it := TProductionTypeListIterator.create( ptList );

      reasonList := TCStringList.create( destrReasonOrder, ',' );

      i := 1;

      if( pos( 'production type', destrPriorityOrder ) < pos( 'reason', destrPriorityOrder ) ) then
      	begin
        	// Production type is higher priority than destruction reason
          // So production type is the outer loop
          it.toFirst();
          while( nil <> it.current() ) do
          	begin
              if( it.current().isDestrTarget ) then
                begin
                  reason := reasonList.first();
                  while( nil <> reason ) do
                    begin
                      _destrOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                      inc( i );
                      reason := reasonList.next();
                    end
                  ;
                end
              ;
           		it.incr();
            end
          ;
        end
      else
      	begin
        	// Destruction reason is higher priority than production type
          // So destruction reason is the outer loop
          reason := reasonList.first();
          while( nil <> reason ) do
          	begin
            	it.toFirst();
              while( nil <> it.current() ) do
              	begin
                  if( it.current().isDestrTarget ) then
                    begin
                	    _destrOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                	    inc( i );
                    end
                  ;
                	it.incr();
                end
              ;
            	reason := reasonList.next();
            end
          ;
        end
      ;

      freeAndNil( reasonList );
      freeAndNil( it );
      freeAndNil( ptList );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// debugging
//-----------------------------------------------------------------------------
	procedure TGlobalControlParams.debug();
  	begin
   		dbcout( '---------TGlobalControlParams', true );

      dbcout( 'Use detection: ' + usBoolToText( useDetectionGlobal ), true );

      dbcout( 'Use destruction: ' + usBoolToText( useDestructionGlobal ), true  );
      dbcout( 'destrProgramDelay: ' + intToStr( destrProgramDelay ), true  );

      if( nil <> relDestrCapacity ) then
        begin
          dbcout( 'Destruction capacity:', true  );
          relDestrCapacity.debug();
        end
      else
        dbcout( 'Destruction capacity is nil', true )
       ;

      dbcout( 'Use vaccination: ' + usBoolToText( useVaccGlobal ), true  );
      dbcout( 'Begin vaccination after x detections: ' + intToStr( vaccDetectedUnitsBeforeStart ), true  );

      if( nil <> relVaccCapacity ) then
        begin
          dbcout( 'Vaccination capacity:', true  );
          relVaccCapacity.debug();
        end
      else
        dbcout( 'Vaccination capacity is nil', true )
      ;

      dbcout( 'Use zones: ' + usBoolToText( useZonesGlobal ), true );

      dbcout( '---------End TGlobalControlParams', true  );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  procedure TGlobalControlParams.setUseDetectionGlobal( val: boolean ); begin _useDetectionGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseTracingGlobal( val: boolean ); begin _useTracingGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseTracingHerdExamGlobal( val: boolean ); begin _useTracingHerdExamGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseTracingTestingGlobal( val: boolean ); begin _useTracingTestingGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseDestrGlobal( val: boolean ); begin _useDestrGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseVaccGlobal( val: boolean ); begin _useVaccGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseZonesGlobal( val: boolean ); begin _useZonesGlobal := val; _updated := true; end;

  procedure TGlobalControlParams.setDestrProgramDelay( val: integer ); begin _destrProgramDelay := val; _updated := true; end;
  procedure TGlobalControlParams.setVaccDetectedUnitsBeforeStart( val: integer ); begin _vaccDetectedUnitsBeforeStart := val; _updated := true; end;

  function TGlobalControlParams.getUseDetectionGlobal(): boolean; begin result := _useDetectionGlobal; end;
  function TGlobalControlParams.getUseTracingGlobal(): boolean; begin result := _useTracingGlobal; end;

  function TGlobalControlParams.getUseTracingHerdExamGlobal(): boolean;
    begin
      result :=
        _useTracingGlobal
      and
        _useTracingHerdExamGlobal
      ;
    end
  ;

  function TGlobalControlParams.getUseTracingTestingGlobal(): boolean;
    begin
      result :=
        _useTracingGlobal
      and
        _useTracingHerdExamGlobal
      and
        _useTracingTestingGlobal
      ;
    end
  ;

  function TGlobalControlParams.getUseDestrGlobal(): boolean; begin result := _useDestrGlobal; end;
  function TGlobalControlParams.getUseVaccGlobal(): boolean; begin result := _useVaccGlobal; end;
  function TGlobalControlParams.getUseZonesGlobal(): boolean; begin result := _useZonesGlobal; end;

  function TGlobalControlParams.getDestrProgramDelay(): integer; begin Result := _destrProgramDelay; end;
  function TGlobalControlParams.getVaccDetectedUnitsBeforeStart(): integer; begin result := _vaccDetectedUnitsBeforeStart; end;

  procedure TGlobalControlParams.setDestrPriorityOrder( val: string ); begin _destrPriorityOrder := val; _updated := true;  end;
  procedure TGlobalControlParams.setDestrReasonOrder( val: string ); begin _destrReasonOrder := val; _updated := true;  end;
  procedure TGlobalControlParams.setVaccPriorityOrder( val: string ); begin _vaccPriorityOrder := val; _updated := true; end;
  procedure TGlobalControlParams.setVaccReasonOrder( val: string ); begin _vaccReasonorder := val; _updated := true; end;

  
  function TGlobalControlParams.getDestrPriorityOrder(): string;
    begin
      if( 0 < length( trim( _destrPriorityOrder ) ) ) then
        result := _destrPriorityOrder
      else
        result := ''
      ;
    end
  ;


  function TGlobalControlParams.getDestrReasonOrder(): string;
    begin
      if( 0 < length( trim( _destrReasonOrder ) ) ) then
        result := _destrReasonOrder
      else
        result := ''
      ;
    end
  ;


  function TGlobalControlParams.getVaccPriorityOrder(): string;
    begin
      if( 0 < length( trim( _vaccPriorityOrder ) ) ) then
        result := _vaccPriorityOrder
      else
        result := ''
      ;
    end
  ;

  
  function TGlobalControlParams.getVaccReasonOrder(): string;
    begin
      if( 0 < length( trim( _vaccReasonOrder ) ) ) then
        result := _vaccReasonOrder
      else
        result := ''
      ;
    end
  ;


  // FIX ME!
  function TGlobalControlParams.getUpdated(): boolean; begin result := _updated; end;


	function TGlobalControlParams.getSSDestrPriorities(): TQStringLongIntMap;
   	begin
   		if( ( nil <> ( _sim as TSMSimulationInput).ptList ) and ( _sim as TSMSimulationInput).ptList.updated ) then
      	buildSSDestructionPriorities()
      ;
      result := _destrOrderList;
    end
  ;


  procedure TGlobalControlParams.setSSDestrPriorities( newPriorities:TQStringLongIntMap );
    begin
      if ( nil <> _destrOrderList ) then
        begin
          freeAndNil( _destrOrderList );
        end
      ;

      _destrOrderList := newPriorities;
    end
  ;


  function TGlobalControlParams.getSSVaccPriorities(): TQStringLongIntMap;
  	begin
      if( ( nil <> ( _sim as TSMSimulationInput).ptList ) and ( _sim as TSMSimulationInput).ptList.updated ) then
      	buildSSVaccPriorities()
      ;
      result := _vaccOrderList;
    end
  ;


  procedure TGlobalControlParams.setSSVaccPriorities( newPriorities: TQStringLongIntMap );
    begin
      if ( nil <> _vaccOrderList ) then
        begin
          freeAndNil( _vaccOrderList );
        end
      ;

      _vaccOrderList := newPriorities;
    end
  ;


  procedure TGlobalControlParams.setRelDestrCapacityName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relDestrCapacityName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relDestrCapacityName := val;
      _updated := true;
    end
  ;


  procedure TGlobalControlParams.setRelVaccCapacityName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relVaccCapacityName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relVaccCapacityName := val;
      _updated := true;
    end
  ;


  function TGlobalControlParams.getRelDestrCapacityName(): string; begin result := _relDestrCapacityName; end;
  function TGlobalControlParams.getRelVaccCapacityName(): string; begin result := _relVaccCapacityName; end;

  function TGlobalControlParams.getRelDestrCapacity(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relDestrCapacityName ) ) then
            begin
              if( fnDictionary.value( _relDestrCapacityName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relDestrCapacityName ).fn as TRelFunction
              else
                begin
                  setRelDestrCapacityName( '' );
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


  function TGlobalControlParams.getRelVaccCapacity(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relVaccCapacityName ) ) then
            begin
              if( fnDictionary.value( _relVaccCapacityName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relVaccCapacityName ).fn as TRelFunction
              else
                begin
                  setRelVaccCapacityName( '' );
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


  function TGlobalControlParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( _useDestrGlobal and fnDictionary.contains( _relDestrCapacityName ) ) then
        begin
          if( not( fnDictionary.value( _relDestrCapacityName ).fn is TRelFunction ) ) then
            begin
              setRelDestrCapacityName( '' );
              result := false;
            end
          ;
        end
      ;

      if( _useVaccGlobal and fnDictionary.contains( _relVaccCapacityName ) ) then
        begin
          if( not( fnDictionary.value( _relVaccCapacityName ).fn is TRelFunction ) ) then
            begin
              setRelVaccCapacityName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TGlobalControlParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'resources-and-implementation-of-controls-model' );
    end
  ;


  function TGlobalControlParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TGlobalControlParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      delay: integer;
      priorityOrder: string;
      chart: TRelFunction;
      subElement: pointer;
      ssubElement: pointer;
    begin
      //  Get Destruction global settings
      subElement := sdew.GetElementByName( model, 'destruction-program-delay' );
      if ( nil <> subElement ) then
        begin
          // Ignoring Units here....always days.
          ssubElement := Sdew.GetElementByName( SubElement, 'value' );
          if ( nil <> ssubElement ) then
            begin
              delay := myStrToInt( Sdew.GetElementContents( ssubElement ), -1 );
              self.destrProgramDelay := delay;
            end
        else
          appendToPstring( errMsg, tr( 'No destruction-program-delay value found in this xml file.' )  )
        ;
        end
      else
        appendToPstring( errMsg, tr( 'No destruction-program-delay element found in this xml file.' ) )
      ;

      subElement := sdew.GetElementByName( model, 'destruction-priority-order');
      if ( nil <> subElement ) then
        begin
          priorityOrder := sdew.GetElementContents( subElement );
          self.destrPriorityOrder := PriorityOrder;
        end
      else
        appendToPstring( errMsg, tr('No destruction-priority-order element found in this xml file.' ) )
      ;

      subElement := Sdew.GetElementByName( model, 'destruction-capacity' );
      if ( nil <> subElement ) then
        begin
          chart := createRelFromXml( subElement, sdew );
        
          if ( strIsEmpty( Chart.name ) ) then
            chart.name := 'Destruction capacity'
          ;

          chart.xUnits :=  chartUnitTypeFromXml( 'days' );
          chart.yUnits := chartUnitTypeFromXml( 'units per day' );
          chart.dbField := word( DestrCapacityGlobal );
          self.relDestrCapacityName := fnDictionary.checkAndInsert( chart );
        end
      else
        appendToPString( errMsg, tr( 'No destruction-capacity element found in this xml file.' ) )
      ;

      // Get Vaccination global settings...
      subElement := Sdew.GetElementByName( model, 'vaccination-program-delay' );
      if ( nil <> subElement ) then
        begin
          Delay := StrToInt( Sdew.GetElementContents( subElement ) );
          self.vaccDetectedUnitsBeforeStart := Delay;
        end
      else
        appendToPString( errMsg, tr('No vaccination-program-delay element found in this xml file.' ) )
      ;

      subElement := Sdew.GetElementByName( model, 'vaccination-priority-order' );
      if ( nil <> subElement ) then
        begin
          priorityOrder := Sdew.GetElementContents( subElement );
          self.vaccPriorityOrder := priorityOrder;
        end
      else
        appendToPString( errMsg, tr( 'No vaccination-priority-order element found in this xml file.' ) )
      ;

      subElement := Sdew.GetElementByName( model, 'vaccination-capacity' );
      if ( nil <> subElement ) then
        begin
          chart := createRelFromXml( subElement, sdew );
        
          if ( strIsEmpty( chart.name ) ) then
            chart.name := 'Vaccination capacity'
          ;
          chart.xUnits :=  chartUnitTypeFromXml( 'days' );
          chart.yUnits := chartUnitTypeFromXml( 'units per day' );
          chart.dbField := word( VaccCapacityGlobal );
          self.relVaccCapacityName := fnDictionary.checkAndInsert( chart );
        end
      else
        appendToPString( errMsg, tr('Warning: No vaccination-capacity element found in this xml file' ) )
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
