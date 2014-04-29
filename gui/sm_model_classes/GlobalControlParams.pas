unit GlobalControlParams;

(*
GlobalControlParams.pas
------------------------
Begin: 2005/06/03
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.39 $
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
    RelFunction,
    SMDatabase,
    Models,
    FunctionEnums,
    ChartFunction,
    QStringMaps
  ;

  type TGlobalControlParams = class( TModelWithFunctions )
    protected
      _useDetectionGlobal: boolean;
      _useTracingGlobal: boolean;
      _useDestrGlobal: boolean;
      _useVaccGlobal: boolean;
      _useZonesGlobal: boolean;

      _destrProgramDelay: integer;
      _destrDelay: integer;

      _vaccDetectedUnitsBeforeStart: integer;

      _destrCapacityName: string;
      _vaccCapacityName: string;

      _destrPriorityOrder: string;
      _destrReasonOrder: string;
      _vaccPriorityOrder: string;

      // for determining priority order of destruction and vaccination for SharcSpread-compatible XML
      _destrOrderList: TQStringLongIntMap;
      _vaccOrderList: TQStringLongIntMap;


      procedure initialize();

			// property getters and setters
      //-----------------------------
      procedure setUseDetectionGlobal( val: boolean );
      procedure setUseTracingGlobal( val: boolean );
      procedure setUseDestrGlobal( val: boolean );
      procedure setUseVaccGlobal( val: boolean );
      procedure setUseZonesGlobal( val: boolean );
      procedure setDestrProgramDelay( val: integer );
      procedure setVaccDetectedUnitsBeforeStart( val: integer );

      function getUseDetectionGlobal(): boolean;
      function getUseTracingGlobal(): boolean;
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

      function getSSDestrPriorities(): TQStringLongIntMap;
      procedure setSSDestrPriorities( newPriorities:TQStringLongIntMap );
      procedure buildSSDestructionPriorities();

      function getSSVaccPriorities(): TQStringLongIntMap;
      procedure setSSVaccPriorities( newPriorities:TQStringLongIntMap );
      procedure buildSSVaccPriorities();

      procedure setdestrCapacityName( val: string );
      procedure setvaccCapacityName( val: string );
      function getdestrCapacityName(): string;
      function getvaccCapacityName(): string;

      function getDestrCapacity(): TRelFunction;
      function getVaccCapacity(): TRelFunction;


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
      constructor create(); overload;
      constructor create( db: TSMDatabase; sim: TObject ); overload;
      constructor create( const src: TGlobalControlParams; sim: TObject ); overload;
      destructor destroy(); override;

      procedure renameProductionType( const oldProdTypeDescr, newProdTypeDescr: string );

      // Overridden from TModel
      //-----------------------
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      function functionsAreValid(): boolean; override;

      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;

			function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; update: boolean = false ): integer; reintroduce;

      procedure recalculatePriorities();

      property useDetectionGlobal: boolean read getUseDetectionGlobal write setUseDetectionGlobal;
      property useTracingGlobal: boolean read getuseTracingGlobal write setUseTracingGlobal;
      property useDestructionGlobal: boolean read getUseDestrGlobal write setUseDestrGlobal;
      property useVaccGlobal: boolean read getUseVaccGlobal write setUseVaccGlobal;
      property useZonesGlobal: boolean read getUseZonesGlobal write setUseZonesGlobal;

      property destrProgramDelay: integer read getDestrProgramDelay write setDestrProgramDelay;
      property vaccDetectedUnitsBeforeStart: integer read getVaccDetectedUnitsBeforeStart write setVaccDetectedUnitsBeforeStart;

      property destrPriorityOrder: string read getDestrPriorityOrder write setDestrPriorityOrder;
      property destrReasonOrder: string read getDestrReasonOrder write setDestrReasonOrder;
      property vaccPriorityOrder: string read getVaccPriorityOrder write setVaccPriorityOrder;

      property ssDestrPriorities: TQStringLongIntMap read getSSDestrPriorities write setSSDestrPriorities;
      property ssVaccPriorities: TQStringLongIntMap read getSSVaccPriorities write setSSVaccPriorities;

      property destrCapacityName: string read getdestrCapacityName write setdestrCapacityName;
      property vaccCapacityName: string read getvaccCapacityName write setvaccCapacityName;

      property destrCapacity: TRelFunction read getDestrCapacity;
      property vaccCapacity: TRelFunction read getVaccCapacity;

    end
  ;


  const
    DBGLOBALCONTROLPARAMS: boolean = false; // Set to true to enable debugging messages for this unit.

implementation

  uses
    SysUtils,
    Variants,

    MyStrUtils,
    USStrUtils,
    DebugWindow,
    SqlClasses,
    CStringList,
    I88n,

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
      _sim := sim;

      _useDetectionGlobal := src._useDetectionGlobal;
      _useTracingGlobal := src._useTracingGlobal;
      _useDestrGlobal := src._useDestrGlobal;
      _useVaccGlobal := src._useVaccGlobal;
      _useZonesGlobal := src._useZonesGlobal;

      _destrProgramDelay := src._destrProgramDelay;
      _destrDelay := src._destrDelay;

      _vaccDetectedUnitsBeforeStart := src._vaccDetectedUnitsBeforeStart;

      // Don't use the "set" functions here.
      // Otherwise, the function reference counters will get screwed up.
      _destrCapacityName := src._destrCapacityName;
      _vaccCapacityName := src._vaccCapacityName;

      _destrPriorityOrder := src._destrPriorityOrder;
      _destrReasonOrder := src._destrReasonOrder;
      _vaccPriorityOrder := src._vaccPriorityOrder;

      if( nil <> src._destrOrderList ) then
        begin
          _destrOrderList := TQStringLongIntMap.create();
          _destrOrderList.assign( src._destrOrderList );
        end
      else
        _destrOrderList := nil
      ;

      if( nil <> src._vaccOrderList ) then
        begin
          _vaccOrderList := TQStringLongIntMap.create();
          _vaccOrderList.assign( src._vaccOrderList );
        end
      else
        _vaccOrderList := nil
      ;

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

      if( null <> row.field('includeDestruction') ) then
        useDestructionGlobal := boolean( row.field('includeDestruction') )
      else
        useDestructionGlobal := false
      ;
      if( null <> row.field('destrProgramDelay') ) then destrProgramDelay := integer( row.field('destrProgramDelay') );

      if( null <> row.field('destrCapacityRelID') ) then
        begin
      	  setdestrCapacityName( row.field( 'destrCapacityChartName' ) );
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
      	  setvaccCapacityName( row.field( 'vaccCapacityChartName' ) );
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


	procedure TGlobalControlParams.initialize();
  	begin
      _destrProgramDelay := 0;

      _useDetectionGlobal := false;
      _useTracingGlobal := false;
      _useDestrGlobal := false;
      _useVaccGlobal := false;
      _useZonesGlobal := false;

      _vaccDetectedUnitsBeforeStart := 0;

      _destrPriorityOrder := '';
      _destrReasonOrder  := '';
      _vaccPriorityOrder  := '';

      _destrOrderList := nil;
      _vaccOrderList := nil;

      _updated := false;
    end
  ;


  destructor TGlobalControlParams.destroy();
  	begin
    	freeAndNil( _destrOrderList );
      freeAndNil( _vaccOrderList );

    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

  procedure TGlobalControlParams.recalculatePriorities();
    begin
      freeAndNil( _destrOrderList );
      freeAndNil( _vaccOrderList );  
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
        DestrCapacityGlobal: self.destrCapacityName := newName;
        vaccCapacityGlobal: self.vaccCapacityName := newName;
      end;
    end
  ;


  function TGlobalControlParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      case whichChart of
        DestrCapacityGlobal: result := ( chartName = self.destrCapacityName );
        VaccCapacityGlobal: result := ( chartName = self.vaccCapacityName );
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
            Item := (_sim as TSMSimulationInput).functionDictionary.value( self.destrCapacityName );
            if ( nil <> Item ) then
              ret_val := Item.fn;
          end;
        VaccCapacityGlobal:
          begin
            Item := (_sim as TSMSimulationInput).functionDictionary.value( self.vaccCapacityName );
            if ( nil <> Item ) then
              ret_val := Item.fn;
          end;
      end;

      result := ret_val;
    end
  ;


  procedure TGlobalControlParams.removeChart( const chartName: string );
    begin
      if( chartName = self.vaccCapacityName ) then self.vaccCapacityName :='';
      if( chartName = self.destrCapacityName ) then self.destrCapacityName := '';

      // The _updated flag will be set by the properties above, if necessary
    end
  ;

    
//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TGlobalControlParams.populateDatabase( db: TSMDatabase; update: boolean = false ): integer;
  	var
      qDict: TQueryDictionary;
      q: string;
    begin
      qDict := TQueryDictionary.create();

      qDict['controlsGlobalID'] := db.sqlQuote( DB_SCHEMA_APPLICATION );
      
      qDict['includeDetection'] := boolToStr( useDetectionGlobal );
      qDict['includeTracing'] := boolToStr( useTracingGlobal );
      qDict['includeDestruction'] := boolToStr( useDestructionGlobal );
      qDict['includeVaccination'] := boolToStr( useVaccGlobal );
      qDict['includeZones'] := boolToStr( useZonesGlobal );

      qDict['destrProgramDelay'] := intToStr( destrProgramDelay );

      if( nil <> destrCapacity ) then
      	qDict['destrCapacityRelID'] := intToStr( destrCapacity.id )
      else
      	qDict['destrCapacityRelID'] := DATABASE_NULL_VALUE
      ;

      //  These were commented out...., uncommented on 10-10-2006 by SPC
      qDict['destrPriorityOrder'] := db.sqlQuote( self.destrPriorityOrder );
      //  Added this check...10-10-2006, SPC.
      if ( length( self.destrReasonOrder ) <= 0 ) then
        self.destrReasonOrder := 'basic,direct,indirect,ring';
      qDict['destrReasonOrder'] := db.sqlQuote( self.destrReasonOrder );

      qDict['vaccDetectedUnitsBeforeStart'] := intToStr( vaccDetectedUnitsBeforeStart );

      if( nil <> vaccCapacity ) then
      	qDict['vaccCapacityRelID'] := intToStr( vaccCapacity.id )
      else
      	qDict['vaccCapacityRelID'] := DATABASE_NULL_VALUE
      ;

      //  This was commented out...., uncommented on 10-10-2006 by SPC
      qDict['vaccPriorityOrder'] := db.sqlQuote( self.vaccPriorityOrder );

      if( update ) then
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

      s := s + '      ' + chartUnitTypeAsSSXml( UnitsDays ) + endl;
      s := s + '    </destruction-program-delay>' + endl;


      // Destruction capacity
      //----------------------
      s := s + '    <destruction-capacity>' + endl;

      if( includeDestruction ) then
        s := s + destrCapacity.ssXml( 3 )
      else // use default capacity of 0
        begin
          s := s + '      <value>1</value>   <value>0</value>' + endl;
          s := s + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
          s := s + '      <units><xdf:unit>herd</xdf:unit><xdf:unit power="-1">day</xdf:unit></units>' + endl;
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
        s := s + vaccCapacity.ssXml( 3 )
      else // use default capacity of 0
        begin
          s := s + '      <value>1</value>   <value>0</value>' + endl;
          s := s + '      <units><xdf:unit>day</xdf:unit></units>' + endl;
          s := s + '      <units><xdf:unit>herd</xdf:unit><xdf:unit power="-1">day</xdf:unit></units>' + endl;
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

          if( nil = destrCapacity ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction capacity is not set.' ) + endl;
              result := false;
            end
          else if( not( destrCapacity.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Destruction capacity is not valid:' ) + ' ' + submsg + endl;
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

          if( nil = vaccCapacity ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination capacity is not set.' ) + endl;
              result := false;
            end
          else if( not( vaccCapacity.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Vaccination capacity is not valid:' ) + ' ' + submsg + endl;
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

      if( nil <> _vaccOrderList ) then freeAndNil( _vaccOrderList );

      _vaccOrderList := TQStringLongIntMap.create();

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
             	reason := reasonList.first();
              while( nil <> reason ) do
              	begin
									_vaccOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                	inc( i );
               		reason := reasonList.next();
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
                	_vaccOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                	inc( i );
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

      if( length( 'basic,direct,indirect,ring' ) <> length( destrReasonOrder ) ) then
      	destrReasonOrder := 'basic,direct,indirect,ring'
      ;

      if( nil <> _destrOrderList ) then freeAndNil( _destrOrderList );

      _destrOrderList := TQStringLongIntMap.create();

      //ptList := TProductionTypeList.create( db, OrderDestrPriority );
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
             	reason := reasonList.first();
              while( nil <> reason ) do
              	begin
									_destrOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                	inc( i );
               		reason := reasonList.next();
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
                	_destrOrderList[ it.current().productionTypeDescr + '+' + reason ] := i;
                	inc( i );
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

      if( nil <> destrCapacity ) then
        begin
          dbcout( 'Destruction capacity:', true  );
          destrCapacity.debug();
        end
      else
        dbcout( 'Destruction capacity is nil', true )
       ;

      dbcout( 'Use vaccination: ' + usBoolToText( useVaccGlobal ), true  );
      dbcout( 'Begin vaccination after x detections: ' + intToStr( vaccDetectedUnitsBeforeStart ), true  );
      
      if( nil <> vaccCapacity ) then
        begin
          dbcout( 'Vaccination capacity:', true  );
          vaccCapacity.debug();
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
  procedure TGlobalControlParams.setUseDestrGlobal( val: boolean ); begin _useDestrGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseVaccGlobal( val: boolean ); begin _useVaccGlobal := val; _updated := true; end;
  procedure TGlobalControlParams.setUseZonesGlobal( val: boolean ); begin _useZonesGlobal := val; _updated := true; end;

  procedure TGlobalControlParams.setDestrProgramDelay( val: integer ); begin _destrProgramDelay := val; _updated := true; end;
  procedure TGlobalControlParams.setVaccDetectedUnitsBeforeStart( val: integer ); begin _vaccDetectedUnitsBeforeStart := val; _updated := true; end;

  function TGlobalControlParams.getUseDetectionGlobal(): boolean; begin result := _useDetectionGlobal; end;
  function TGlobalControlParams.getUseTracingGlobal(): boolean; begin result := _useTracingGlobal; end;
  function TGlobalControlParams.getUseDestrGlobal(): boolean; begin result := _useDestrGlobal; end;
  function TGlobalControlParams.getUseVaccGlobal(): boolean; begin result := _useVaccGlobal; end;
  function TGlobalControlParams.getUseZonesGlobal(): boolean; begin result := _useZonesGlobal; end;

  function TGlobalControlParams.getDestrProgramDelay(): integer; begin Result := _destrProgramDelay; end;
  function TGlobalControlParams.getVaccDetectedUnitsBeforeStart(): integer; begin result := _vaccDetectedUnitsBeforeStart; end;

  procedure TGlobalControlParams.setDestrPriorityOrder( val: string ); begin _destrPriorityOrder := val; _updated := true;  end;
  procedure TGlobalControlParams.setDestrReasonOrder( val: string ); begin _destrReasonOrder := val; _updated := true;  end;
  procedure TGlobalControlParams.setVaccPriorityOrder( val: string ); begin _vaccPriorityOrder := val; _updated := true; end;

  function TGlobalControlParams.getDestrPriorityOrder(): string; begin Result := _destrPriorityOrder; end;
  function TGlobalControlParams.getDestrReasonOrder(): string; begin Result := _destrReasonOrder; end;
  function TGlobalControlParams.getVaccPriorityOrder(): string; begin Result := _vaccPriorityOrder; end;

  // FIX ME!
  function TGlobalControlParams.getUpdated(): boolean; begin result := _updated; end;


	function TGlobalControlParams.getSSDestrPriorities(): TQStringLongIntMap;
   	begin
   		if( nil = _destrOrderList ) then
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
        end;

      _destrOrderList := newPriorities;
    end;


  function TGlobalControlParams.getSSVaccPriorities(): TQStringLongIntMap;
  	begin
    	if( nil = _vaccOrderList ) then
      	buildSSVaccPriorities()
      ;
      result := _vaccOrderList;
    end
  ;

  procedure TGlobalControlParams.setSSVaccPriorities( newPriorities:TQStringLongIntMap );
    begin
      if ( nil <> _vaccOrderList ) then
        begin
          freeAndNil( _vaccOrderList );
        end;

      _vaccOrderList := newPriorities;
    end;

  procedure TGlobalControlParams.setdestrCapacityName( val: string );
    begin
      val := trim( val );
      _destrCapacityName := val;

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


  procedure TGlobalControlParams.setvaccCapacityName( val: string );
    begin
      val := trim( val );
      _vaccCapacityName := val;

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


  function TGlobalControlParams.getdestrCapacityName(): string; begin result := _destrCapacityName; end;
  function TGlobalControlParams.getvaccCapacityName(): string; begin result := _vaccCapacityName; end;

  function TGlobalControlParams.getDestrCapacity(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _destrCapacityName ) ) then
            begin
              if( fnDictionary.value( _destrCapacityName ).fn is TRelFunction ) then
                result := fnDictionary.value( _destrCapacityName ).fn as TRelFunction
              else
                begin
                  setDestrCapacityName( '' );
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


  function TGlobalControlParams.getVaccCapacity(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _vaccCapacityName ) ) then
            begin
              if( fnDictionary.value( _vaccCapacityName ).fn is TRelFunction ) then
                result := fnDictionary.value( _vaccCapacityName ).fn as TRelFunction
              else
                begin
                  setVaccCapacityName( '' );
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

      if( _useDestrGlobal and fnDictionary.contains( _destrCapacityName ) ) then
        begin
          if( not( fnDictionary.value( _destrCapacityName ).fn is TRelFunction ) ) then
            begin
              setDestrCapacityName( '' );
              result := false;
            end
          ;
        end
      ;

      if( _useVaccGlobal and fnDictionary.contains( _vaccCapacityName ) ) then
        begin
          if( not( fnDictionary.value( _vaccCapacityName ).fn is TRelFunction ) ) then
            begin
              setVaccCapacityName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------
end.
