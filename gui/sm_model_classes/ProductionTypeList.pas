unit ProductionTypeList;

(*
ProductionTypeList.pas
----------------------
Begin: 2005/01/06
Last revision: $Date: 2013-06-27 19:11:35 $ $Author: areeves $
Version number: $Revision: 1.37.4.12 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QLists,
    QStringMaps,
    SqlClasses,

    Sdew,
    
    Models,
    ChartFunction,
    ModelDatabase,

    SMDatabase,
    FunctionEnums,
    ProductionType,
    Zone
  ;


  type TProductionTypeList = class( TModelList )
      protected
        _xmlModelList: TQStringList;

        // Variables for queue outputs
        // These outputs are not simple sums of the individual
        // production type values, so they require separate handling.
        _deswUMax: longint;
        _deswUMaxDay: integer;
        _deswUTimeMax: longint;
        _deswAMax: double;
        _deswAMaxDay: integer;
        _deswUDaysInQueue: double;
        _deswADaysInQueue: double;
        _destrQueueLengthUnits: longint;
        _destrQueueLengthAnimals: double;
        _unitsDestroyed: longint;

        _vacwUMax: longint;
        _vacwUMaxDay: integer;
        _vacwUTimeMax: longint;
        _vacwAMax: double;
        _vacwAMaxDay: integer;
        _vacwUDaysInQueue: double;
        _vacwADaysInQueue: double;
        _vaccQueueLengthUnits: longint;
        _vaccQueueLengthAnimals: double;
        _unitsVaccinated: longint;

        _firstDetectionHasOccurred: boolean;
        _firstDetectionProcessed: boolean;

        procedure initialize( sim: TObject );

        // XML import
        function getXmlModelList(): TQStringList;
        procedure importPtXml( ptName: string; ptID: integer; model: pointer; sdew: TSdew; errMsg: pstring = nil );
        procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

        function deswUTimeAvg(): double;
        function vacwUTimeAvg(): double;

        // Basic list functions
        procedure setObject( index: integer; item: TProductionType );
        function getObject( index: integer ): TProductionType;

        function getUpdated(): boolean;

        // Used to verify that production type control priorities are set appropriately.
        // Return true is priorites are faulty, false if they're OK.
        function badDestrPriorityOrder(): boolean;
        function badVaccPriorityOrder(): boolean;

      public
        constructor create( sim: TObject ); overload;
        constructor create( db: TSMDatabase; sim: TObject ); overload;
        constructor create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil ); overload;
        constructor create( const src: TProductionTypeList; sim: TObject ); overload;

        destructor destroy(); override;

        function equals( const otherList: TProductionTypeList ): boolean;

        procedure sortByDestrOrder();
        procedure sortByVaccOrder();

        // Typical list functions
        function append( dm: TProductionType ): integer; reintroduce;
        procedure insert( index: integer; dm: TProductionType );
        property objects[ index: integer]: TProductionType read getObject write setObject; default;

        function at( const i: integer ): TProductionType;

        function byID( id: integer ): TProductionType;
        function findProdTypeID( typeDescr: string ): integer;
        function findProdType( typeDescr: string ): TProductionType; overload;
        function findProdType( typeID: integer ): TProductionType; overload;
        function findProdTypeName( typeID: integer ): string;
        function prodTypeIDExists( typeID: integer ): boolean;

        procedure populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType );

        function ssDiseaseModelsXml(): string;
        function ssDetectionModelsXml(): string;
        function ssTracingModelsXml(): string;
        function ssExamModelsXml(): string;
        function ssTestingModelsXml(): string;
        function ssBasicDestructionModelsXml(): string;
        function ssTraceDestructionModelsXml(): string;
        function ssRingDestructionModelsXml(): string;
        function ssVaccineModelsXml(): string;
        function ssRingVaccModelsXml(): string;
        function ssZoneModelsXml(): string;
        function ssEconomicModelsXml( const includeZonesGlobal: boolean; zoneList: TZoneList ): string;

        procedure removeZone( const zoneID: integer );
        procedure addZone( const zoneID: integer );

        procedure recountUnits( hList: TObject );

        // For model outputs
        procedure clearAllRecords( db: TSMDatabase );
        procedure resetIterationRecords();
        procedure prepareForDay( day: integer );
        procedure detectHerd();
        procedure addToDestrQueue( const herdSize: integer; const day: integer );
        procedure destroyHerd( const herdSize, day, queueDay: integer );
        procedure addToVaccQueue( const herdSize: integer; const day: integer );
        procedure removeFromVaccQueue( const herdSize: integer );
        procedure vaccinateHerd( const herdSize, day, queueDay: integer );
        procedure processDailyRecords( db: TSMDatabase; const iteration: integer; const day: integer );
        procedure processIterationRecords( db: TSMDatabase; const iteration: integer );

        // Inherited functions for chart handling
        procedure removeChart( const chartName: string ); override;
        procedure changeChart(
          const whichChart: TSMChart;
          const oldChartName: string;
          newChart: TChartFunction;
          addlInfo: integer = -1
        ); override;

        // Inherited functions for debugging and validation
        procedure debug(); override;
        function validate( err: PString = nil ): boolean; override;
        function functionsAreValid(): boolean; override;

        // Properties
        property updated: boolean read getUpdated;
        property xmlModelList: TQStringList read getXmlModelList;
    end
  ;


  type TProductionTypeListIterator = class( TModelListIterator )
    public
      function toFirst(): TProductionType;
      function toLast(): TProductionType;
      function current(): TProductionType;
    end
  ;


implementation

  uses
    SysUtils,
    StrUtils,
    Math,
    TypInfo,
    Variants,
    Forms, // for Application.processMessages

    MyStrUtils,
    DebugWindow,
    QIntegerMaps,
    I88n,

    DetectionParams,
    TracingParams,
    TestingParams,
    ProdTypeZoneParams,
    DestructionParams,
    VaccinationParams,
    RingVaccParams,
    CostParams,
    SMSimulationInput,
    Herd,
    RemoteDatabaseParams
  ;


  const
    DBPRODUCTIONTYPE: boolean = false; // set to true to enable debug messages for this unit.
    DBPRODUCTIONTYPELIST: boolean = false; // set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// TProductionTypeList: Helper functions
//-----------------------------------------------------------------------------
  function compareProdTypesByDestrOrder( Item1, Item2: Pointer ): Integer;
    begin
      result := compareValue(
        TProductionType(Item1).destructionParams.destrPriority,
        TProductionType(Item2).destructionParams.destrPriority
      );
    end
  ;


  function compareProdTypesByVaccOrder( Item1, Item2: Pointer ): Integer;
    begin
      (*
      if( nil = TProductionType(Item1).ringVaccParams ) then
        raise exception.create( 'Item 1 is nil in compareProdTypesByVaccOrder: ' + TProductionType(Item1).productionTypeDescr )
      ;
      if( nil = TProductionType(Item2).ringVaccParams ) then
        raise exception.create( 'Item 2 is nil in compareProdTypesByVaccOrder: ' + TProductionType(Item2).productionTypeDescr )
      ;

      if ( 0 > TProductionType(Item1).ringVaccParams.vaccPriority )  then
        result :=  1
      else
        begin
          if ( 0 > TProductionType(Item2).ringVaccParams.vaccPriority ) then
            result := -1
          else
            begin
        *)
              result := compareValue(
                TProductionType(Item1).ringVaccParams.vaccPriority,
                TProductionType(Item2).ringVaccParams.vaccPriority
              );
        (*
            end
          ;
        end
      ;
      *)
    end
  ;
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// TProductionTypeList: Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TProductionTypeList.create( sim: TObject );
    begin
      inherited create();
      initialize( sim );
    end
  ;


  constructor TProductionTypeList.create( const src: TProductionTypeList; sim: TObject );
    var
      srcPT: TProductionType;
      newPT: TProductionType;
      it: TProductionTypeListIterator;
    begin
      inherited create( src );
      initialize( sim );

      it := TProductionTypeListIterator.create( src );

      if ( ( Assigned( src ) ) and ( Assigned( sim ) ) ) then
        begin
          it.toFirst();
          while( nil <> it.current() ) do
            begin
              srcPT := it.current();

              newPT := TProductionType.create( srcPT, _sim );
              self.append( newPT );

              it.incr();
            end
          ;
        end
      ;

      it.Free();
    end
  ;


  constructor TProductionTypeList.create( db: TSMDatabase; sim: TObject );
    var
      q: string;
      res: TSqlResult;
      row: TSqlRow;
      pt: TProductionType;
      db2: TSqlDatabase;
      
      det: TDetectionParams;
      destr: TDestructionParams;
      trace: TTracingParams;
      testing: TTestingParams;
      vacc: TVaccinationParams;
      ringv: TRingVaccParams;
      cost: TCostParams;
      zone: TProdTypeZoneParams;
    begin
      inherited create();
      initialize( sim );

      if ( ( Assigned( db ) ) and ( Assigned( sim ) ) ) then
        begin
          db2 := db as TSqlDatabase;

          q := 'SELECT'
            + ' inProductionType.productionTypeID, '
            + ' inProductionType.descr, '
            + ' inProductionType.useDiseaseTransition, '
            + ' inProductionType.disLatentPeriodPdfID, '
            + ' latentChart.chartName as latentChartName,'
            + ' inProductionType.disSubclinicalPeriodPdfID,'
            + ' subclinicalChart.chartName as subclinicalChartName,'
            + ' inProductionType.disClinicalPeriodPdfID, '
            + ' clinicalChart.chartName as clinicalChartName, '
            + ' inProductionType.disImmunePeriodPdfID, '
            + ' immuneChart.chartName as immuneChartName, '
            + ' inProductionType.disPrevalenceRelID, '
            + ' prevalenceChart.chartName as prevalenceChartName '
            + ' FROM '
            + ' ( ( ( ('
            + ' inProductionType'
            + ' LEFT OUTER JOIN inChart latentChart ON inProductionType.disLatentPeriodPdfID = latentChart.chartID )'
            + ' LEFT OUTER JOIN inChart subclinicalChart ON inProductionType.disSubclinicalPeriodPdfID = subclinicalChart.chartID )'
            + ' LEFT OUTER JOIN inChart clinicalChart ON inProductionType.disClinicalPeriodPdfID = clinicalChart.chartID )'
            + ' LEFT OUTER JOIN inChart immuneChart ON inProductionType.disImmunePeriodPdfID = immuneChart.chartID )'
            + ' LEFT OUTER JOIN inChart prevalenceChart ON inProductionType.disPrevalenceRelID = prevalenceChart.chartID'
            + ' ORDER BY inProductionType.productionTypeID'
          ;

          res := TSqlResult.create(q, db2 );

          row := res.fetchArrayFirst();

          while( nil <> row ) do
            begin
              Application.ProcessMessages();

              pt := TProductionType.Create(
                integer( row.field('productionTypeID') ),
                string( row.field('descr') ),
                boolean( row.field('useDiseaseTransition') ),
                sim
              );

              if( null <> row.field('disLatentPeriodPdfID') ) then
                pt.pdfLatentName := row.field( 'latentChartName' )
              ;

              if( null <> row.field('disSubclinicalPeriodPdfID') ) then
                pt.pdfSubclinicalName := row.field( 'subclinicalChartName' )
              ;

              if( null <> row.field('disClinicalPeriodPdfID') ) then
                pt.pdfClinicalName := row.field( 'clinicalChartName' )
              ;

              if( null <> row.field('disImmunePeriodPdfID') ) then
                pt.pdfImmuneName := row.field( 'immuneChartName' )
              ;

              if( null <> row.field('disPrevalenceRelID') ) then
                pt.relPrevalenceName := row.field( 'prevalenceChartName' )
              ;

              det := TDetectionParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.detectionParams := det;

              destr := TDestructionParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.destructionParams := destr;

              trace := TTracingParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.tracingParams := trace;

              testing := TTestingParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.testingParams := testing;

              vacc := TVaccinationParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.vaccinationParams := vacc;

              ringv := TRingVaccParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.ringVaccParams := ringv;

              zone := TProdTypeZoneParams.create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.zoneParams := zone;

              cost := TCostParams.Create( db, integer( row.field('productionTypeID') ), pt.productionTypeDescr, _sim );
              pt.costParams := cost;

              pt.setUpdateFlag( false );
              self.append( pt );

              row := res.fetchArrayNext();
            end
          ;

          freeAndNil( res );
        end
      ;
    end
  ;


  constructor TProductionTypeList.create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil );
    var
      i: integer;
      model: pointer;
      nModels: integer;
      modelName: string;
    begin
      inherited create();
      initialize( sim );

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


  procedure TProductionTypeList.initialize( sim: TObject );
    begin
      if ( Assigned( sim ) ) then
        _sim := sim
      else
        _sim := nil
      ;

      _xmlModelList:= nil;
    end
  ;


  destructor TProductionTypeList.destroy();
    begin
      freeAndNil( _xmlModelList );
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Sorting
//-----------------------------------------------------------------------------
  procedure TProductionTypeList.SortByDestrOrder();
    begin
      self.Sort( compareProdTypesByDestrOrder );
    end
  ;


  procedure TProductionTypeList.SortByVaccOrder();
    begin
      self.Sort( compareProdTypesByVaccOrder );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Comparing two lists
//-----------------------------------------------------------------------------
  function TProductionTypeList.equals( const otherList: TProductionTypeList ): boolean;
    var
      i: integer;
      pt, otherPT: TProductionType;
    begin
      result := true; // until proven otherwise.

      // Are the lists the same size?
      if( self.Count <> otherList.Count ) then
        begin
          result := false;
          exit;
        end
      ;

      // Do all production types match?
      for i := 0 to self.Count - 1 do
        begin
          pt := self.at( i );
          otherPT := otherList.at( i );

          if( pt.productionTypeID <> otherPT.productionTypeID ) then
            begin
              result := false;
              break;
            end
          ;

          if( pt.productionTypeDescr <> otherPT.productionTypeDescr ) then
            begin
              result := false;
              break;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Typical list functions
//-----------------------------------------------------------------------------
  function TProductionTypeList.append( dm: TProductionType ): integer;
    begin
      result := inherited Add( dm );
      dm.ptList := self;
    end
  ;


  procedure TProductionTypeList.setObject( index: integer; item: TProductionType );
    begin
      inherited SetItem( index, item );
      item.ptList := self;
    end
  ;


  function TProductionTypeList.getObject( index: integer ): TProductionType;
    begin
      result := inherited GetItem( index ) as TProductionType;
    end
  ;


  procedure TProductionTypeList.insert(index: integer; dm: TProductionType);
    begin
      inherited Insert(index, dm);
      dm.ptList := self;
    end
  ;


  function TProductionTypeList.at( const i: integer ): TProductionType;
    begin
      if( ( 0 = i ) and ( 0 = self.Count ) ) then
        result := nil
      else if( i > self.Count - 1 ) then
        raise exception.Create( 'Index (' + intToStr( i ) + ') out of bounds in TProductionTypeList' )
      else
        result := getObject( i )
      ;  
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Chart handling functions
//-----------------------------------------------------------------------------
  procedure TProductionTypeList.removeChart( const chartName: string );
    var
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );

      it.toFirst();

      while( nil <> it.current() ) do
        begin
          it.current().removeChart( chartName );
          it.incr();
        end
      ;
    end
  ;


  procedure TProductionTypeList.changeChart(
          const whichChart: TSMChart;
          const oldChartName: string;
          newChart: TChartFunction;
          addlInfo: integer = -1
      );
    var
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      it.toFirst();
      
      while( nil <> it.current() ) do
        begin
          it.current().changeChart( whichChart, oldChartName, newChart, addlInfo );
          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// TProductionTypeList: Functions for model outputs
//-----------------------------------------------------------------------------
  // called just before a simulation begins
  procedure TProductionTypeList.clearAllRecords( db: TSMDatabase );
    var
      lit: TProductionTypeListIterator;
    begin
       lit := TProductionTypeListIterator.create( self );

       while( nil <> lit.current() ) do
        begin
          lit.current().clearAllRecords( db );
          lit.incr();
        end
       ;

       lit.Free();
    end
  ;


  // called at the beginning of every iteration
  procedure TProductionTypeList.resetIterationRecords();
    var
      lit: TProductionTypeListIterator;
    begin
        // Reset records for individual production types
        //----------------------------------------------
       lit := TProductionTypeListIterator.create( self );

       while( nil <> lit.current() ) do
        begin
          lit.current().resetIterationRecords();
          lit.incr();
        end
       ;

       lit.Free();

      // Reset outputs for all production types
      // (These outputs are not simple sums of the individual production type values)
      //-----------------------------------------------------------------------------
      _deswUMax := 0;
      _deswUMaxDay := 0;
      _deswUTimeMax := 0;
      _deswAMax := 0.0;
      _deswAMaxDay := 0;
      _deswUDaysInQueue := 0.0;
      _deswADaysInQueue := 0.0;
      _destrQueueLengthUnits := 0;
      _destrQueueLengthAnimals := 0.0;
      _unitsDestroyed := 0;

      _vacwUMax := 0;
      _vacwUMaxDay := 0;
      _vacwUTimeMax := 0;;
      _vacwAMax := 0.0;
      _vacwAMaxDay := 0;
      _vacwUDaysInQueue := 0.0;
      _vacwADaysInQueue := 0.0;
      _vaccQueueLengthUnits := 0;
      _vaccQueueLengthAnimals := 0.0;
      _unitsVaccinated := 0;

      _firstDetectionHasOccurred := false;
      _firstDetectionProcessed := false;
    end
  ;


  procedure TProductionTypeList.prepareForDay( day: integer );
    var
      lit: TProductionTypeListIterator;
    begin
       lit := TProductionTypeListIterator.create( self );

       while( nil <> lit.current() ) do
        begin
          lit.current().prepareForDay( day );
          lit.incr();
        end
       ;

       lit.Free();
    end
  ;


  procedure TProductionTypeList.addToDestrQueue( const herdSize: integer; const day: integer );
    begin
      inc( _destrQueueLengthUnits );

      _destrQueueLengthAnimals := _destrQueueLengthAnimals + herdSize;

      if( _destrQueueLengthUnits > _deswUMax ) then
        begin
          _deswUMax := _destrQueueLengthUnits;
          _deswUMaxDay := day;
        end
      ;

      if( _destrQueueLengthAnimals > _deswAMax ) then
        begin
          _deswAMax := _destrQueueLengthAnimals;
          _deswAMaxDay := day;
        end
      ;
    end
  ;


  procedure TProductionTypeList.addToVaccQueue( const herdSize: integer; const day: integer );
    begin
      inc( _vaccQueueLengthUnits );
      _vaccQueueLengthAnimals := _vaccQueueLengthAnimals + herdSize;

      if( _vaccQueueLengthUnits > _vacwUMax ) then
        begin
          _vacwUMax := _vaccQueueLengthUnits;
          _vacwUMaxDay := day;
        end
      ;

      if( _vaccQueueLengthAnimals > _vacwAMax ) then
        begin
          _vacwAMax := _vaccQueueLengthAnimals;
          _vacwAMaxDay := day;
        end
      ;
    end
  ;


  procedure TProductionTypeList.removeFromVaccQueue( const herdSize: integer );
    begin
      dec( _vaccQueueLengthUnits );
      _vaccQueueLengthAnimals := _vaccQueueLengthAnimals - herdSize;

      if( 0 > _vaccQueueLengthUnits ) then
        raise exception.Create( 'Number of units in vaccination queue has dropped below 0 in TProductionTypeList.removeFromVaccQueue().' )
      ;
      if( 0.0 > _vaccQueueLengthAnimals ) then
        raise exception.Create( 'Number of animals in vaccination queue has dropped below 0 in TProductionTypeList.removeFromVaccQueue().' )
      ;
    end
  ;


  procedure TProductionTypeList.detectHerd();
    begin
        _firstDetectionHasOccurred := true;
    end
  ;


  procedure TProductionTypeList.vaccinateHerd( const herdSize, day, queueDay: integer );
    var
      daysInQueue: integer;
    begin
      daysInQueue := day - queueDay;

      inc( _unitsVaccinated );

      _vacwUTimeMax := max( _vacwUTimeMax, daysInQueue );

      _vacwUDaysInQueue := _vacwUDaysInQueue + daysInQueue;
      _vacwADaysInQueue := _vacwADaysInQueue + ( daysInQueue * herdSize );

      dec( _vaccQueueLengthUnits );
      _vaccQueueLengthAnimals := _vaccQueueLengthAnimals - herdSize;

      if( 0 > _vaccQueueLengthUnits ) then
        raise exception.Create( 'Number of units in vaccination queue has dropped below 0 in TProductionTypeList.vaccinateHerd().' )
      ;
      if( 0.0 > _vaccQueueLengthAnimals ) then
        raise exception.Create( 'Number of animals in vaccination queue has dropped below 0 in TProductionTypeList.vaccinateHerd().' )
      ;
    end
  ;


  procedure TProductionTypeList.destroyHerd( const herdSize, day, queueDay: integer );
    var
      daysInQueue: integer;
    begin
      daysInQueue := day - queueDay;

      inc( _unitsDestroyed );

      _deswUTimeMax := max( _deswUTimeMax, daysInQueue );

      _deswUDaysInQueue := _deswUDaysInQueue + daysInQueue;
      _deswADaysInQueue := _deswADaysInQueue + ( daysInQueue * herdSize );

      dec( _destrQueueLengthUnits );
      _destrQueueLengthAnimals := _destrQueueLengthAnimals - herdSize;

      // Vaccination queue length will be addressed by removeFromVaccQueue() when vaccination is explicitly canceled.

      // Do some error checking
      if( 0 > _destrQueueLengthUnits ) then
        raise exception.Create( 'Number of units in destruction queue has dropped below 0 in TProductionTypeList.destroyHerd().' )
      ;
      if( 0.0 > _destrQueueLengthAnimals ) then
        raise exception.Create( 'Number of animals in destruction queue has dropped below 0 in TProductionTypeList.destroyHerd().' )
      ;
    end
  ;


  procedure TProductionTypeList.processDailyRecords( db: TSMDatabase; const iteration: integer; const day: integer );
    var
      lit: TProductionTypeListIterator;
    begin
      lit := TProductionTypeListIterator.create( self );

      while( nil <> lit.current() ) do
        begin
          lit.current().processDailyRecords( db, iteration, day );
          lit.incr();
        end
      ;

      if( ( _firstDetectionHasOccurred ) and ( not( _firstDetectionProcessed ) ) ) then
        begin
          _firstDetectionProcessed := true;

          lit.toFirst();
          while( nil <> lit.current() ) do
            begin
              lit.current().recordInfectedAtFirstDetection();
              lit.incr();
            end
          ;
        end
      ;

      lit.Free();
    end
  ;


  procedure TProductionTypeList.processIterationRecords( db: TSMDatabase; const iteration: integer );
    var
      lit: TProductionTypeListIterator;

      qDict: TQueryDictionary;
      q: string;
    begin
      // Process production-type-specific records
      //-----------------------------------------
      lit := TProductionTypeListIterator.create( self );

      while( nil <> lit.current() ) do
      begin
        lit.current().processIterationRecords( db, iteration );
        lit.incr();
      end
      ;

      lit.Free();

      // Process overall records
      // (These outputs are not simple sums of the individual production type values)
      //-----------------------------------------------------------------------------
      qDict := TQueryDictionary.create();

      // Destruction queue outputs
      qDict['deswUMax'] := intToStr( _deswUMax );
      qDict['deswAMax'] := usFloatToStr( _deswAMax );
      qDict['deswUMaxDay'] := intToStr( _deswUMaxDay );
      qDict['deswAMaxDay'] := intToStr( _deswAMaxDay );
      qDict['deswUTimeMax'] := intToStr( _deswUTimeMax );
      qDict['deswUTimeAvg'] := usFloatToStr(  deswUTimeAvg() );

      // Vaccination queue outputs
      qDict['vacwUMax'] := intToStr( _vacwUMax );
      qDict['vacwAMax'] := usFloatToStr( _vacwAMax );
      qDict['vacwUMaxDay'] := intToStr( _vacwUMaxDay );
      qDict['vacwAMaxDay'] := intToStr( _vacwAMaxDay );
      qDict['vacwUTimeMax'] := intToStr( _vacwUTimeMax );
      qDict['vacwUTimeAvg'] := usFloatToStr( vacwUTimeAvg() );

      if( remoteDBParams.useRemoteDatabase ) then
        begin
          q := sqlClasses.writeQuery(
            'outIteration',
            QUpdate,
            qDict,
            'WHERE `iteration` = ' + intToStr( iteration ) + ' AND `jobID` = ' + intToStr( remoteDBParams.jobID )
          );
          db.remoteExecute( q )
        end
      else
        begin
          q := sqlClasses.writeQuery(
            'outIteration',
            QUpdate,
            qDict,
            'WHERE `iteration` = ' + intToStr( iteration )
          );
          db.execute( q );
        end
      ;

      qDict.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Unit/animal counts
//-----------------------------------------------------------------------------
  procedure TProductionTypeList.recountUnits( hList: TObject );
    var
      tmpHList: THerdList;
      h: THerd;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          it.current().clearCounts();
          it.incr();
        end
      ;

      tmpHList := hList as THerdList;
      h := tmpHList.first();
      while( nil <> h ) do
        begin
          h.prodType.addToCounts( h.initialSize );
          h := tmpHList.next();
        end
      ;

      it.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Production type lookups
//-----------------------------------------------------------------------------
  function TProductionTypeList.byID( id: integer ): TProductionType;
    var
      it: TProductionTypeListIterator;
    begin
      result := nil;

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( it.current().productionTypeID = id ) then
            begin
              result := it.current();
              break;
            end
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.findProdType( typeID: integer ): TProductionType;
    var
      res: TProductionType;
      it: TProductionTypeListIterator;
    begin
      res := nil;
      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( it.current().productionTypeID = typeID ) then
            begin
              res := it.current();
              break;
            end
          ;

          it.incr();
        end
      ;

      it.free();
      
      result := res;
    end
  ;


  function TProductionTypeList.findProdType( typeDescr: string ): TProductionType;
    var
      res: TProductionType;
      it: TProductionTypeListIterator;
    begin
      res := nil;

      typeDescr := fixup( typeDescr );

      it := TProductionTypeListIterator.create( self );
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          if( fixup( it.current().productionTypeDescr ) = typeDescr ) then
            begin
              res := it.current();
              break;
            end
          else
            it.incr();
          ;
        end
      ;

      it.Free();
      
      result := res;
    end
  ;


  function TProductionTypeList.prodTypeIDExists( typeID: integer ): boolean;
    var
      it: TProductionTypeListIterator;
    begin
      result := false;
      
      it := TProductionTypeListIterator.create( self );
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          if( it.current().productionTypeID = typeID ) then
            begin
              result := true;
              break;
            end
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;



  function TProductionTypeList.findProdTypeName( typeID: integer ): string;
    var
      it: TProductionTypeListIterator;
    begin
      result := '';

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin

          if( it.current().productionTypeID = typeID ) then
            begin
              //dbcout( 'Match found!', DBPRODUCTIONTYPELIST );
              result := it.current().productionTypeDescr;
              break;
            end
          ;

          it.incr();
        end
      ;

      if( length( result ) = 0 ) then
        raise exception.Create( 'TProductionTypeList.findProdTypeName: Cannot find production type name' )
      ;

      it.free();
    end
  ;



  function TProductionTypeList.findProdTypeID( typeDescr: string ): integer;
    var
      it: TProductionTypeListIterator;
      val: integer;
    begin
      val := PRODTYPEIDNOTFOUND;
      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      typeDescr := fixup( typeDescr );

      while( nil <> it.current() ) do
        begin

          if( fixup( it.current().productionTypeDescr ) = typeDescr ) then
            begin
              //dbcout( 'Match found!', DBPRODUCTIONTYPELIST );
              val := it.current().productionTypeID;
              break;
            end
          ;

          it.incr();
        end
      ;

      (*
      if( val = PRODTYPEUNASSIGNED ) then
        raise exception.create( 'Production type ID is unassigned' )
      ;
      *)
      it.Free();

      result := val;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Database handling
//-----------------------------------------------------------------------------
  procedure TProductionTypeList.populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType );
    var
      dm: TProductionType;
      sim: TSMSimulationInput;
      it: TProductionTypeListIterator;
    begin
      sim := _sim as TSMSimulationInput;

      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.removed ) then
            begin
              if( 0 < dm.productionTypeID ) then
                begin
                  dbcout( '######## pt will be removed: ' + intToStr( dm.productionTypeID ), DBPRODUCTIONTYPE );

                  // Remove this production type from the sim objects in memory
                  if( nil <> sim ) then sim.removeProductionType( dm.productionTypeID );

                  // Remove this production type, as well as herds
                  // and PTPs that use it, from the database.
                  db.removeProductionType( dm.productionTypeID );
                end
              ;

              // If this type is removed, herds of this type
              // must be removed from the herd list in memory
              // somewhere else.  There is no way for the pt
              // list to handle it.  Fortunately, this should
              // only be an issue for TFormProdType: see function
              // TFormProdType.updateScenarioData().

              // Likewise, production type pairs in the PTP list in memory
              // which use this production type must also be removed.  This
              // also occurs in TFormProdType.updateScenarioData().

              // Remove this production type from the pt list in memory
              remove( findProdType( dm.productionTypeID ) );

              // Don't increment the iterator here:
              // when an item is removed, everything else gets bumped up.
              it.current();
            end
          else
            begin
              if( dm.updated ) then
                dm.populateDatabase( db, updateAction )
              ;

              it.incr();
            end
          ;
        end
      ;

      it.free();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// TProductionTypeList: Data validation
//-----------------------------------------------------------------------------
  function TProductionTypeList.badDestrPriorityOrder(): boolean;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
      map: TQIntegerObjectMap;
      i: integer;
    begin
      result := false;

      map := TQIntegerObjectMap.create();
      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          // All production types should have destructionParams, whether units are destroyed or not.
          if( nil = dm.destructionParams ) then
            begin
              result := true;
              break;
            end
          else
            begin
              // If a production type is to be destroyed, then it must have the following:
              if( dm.isDestrTarget ) then
                begin
                  // It must have a priority value greater than 0.
                  if( 0 >= dm.destructionParams.destrPriority ) then
                    begin
                      result := true;
                      break;
                    end
                  // It must have a unique priority value.
                  // (The map is used to keep track of priorities as they are encountered.
                  // If the map already contains a value, then that value is not unique, and the problem must be corrected.)
                  else if( map.contains( dm.destructionParams.destrPriority ) ) then
                    begin
                      result := true;
                      break;
                    end
                  else
                    map.insert( dm.destructionParams.destrPriority, dm )
                  ;
                end
              // If a production type is NOT to be destroyed, then it should have a priority value of -1.
              else
                begin
                  if( -1 <> dm.destructionParams.destrPriority ) then
                    dm.destructionParams.destrPriority := -1
                  ;
                end
              ;
            end
          ;

          it.incr();
        end
      ;

      it.Free();

      if( false = result ) then
        begin
          // While we have the map, we can use it to ensure that priority values start at 1 and are consecutive.
          // Recall that items in a QMap are sorted by index: just read items out of the map in order of the index,
          // and reassign priority values in sequence.

          for i := 0 to map.count - 1 do
            begin
              dm := map.itemAtIndex( i ) as TProductionType;
              if( ( i + 1 ) <> dm.destructionParams.destrPriority ) then
                dm.destructionParams.destrPriority := i + 1
              ;
            end
          ;
        end
      ;

      map.Free();
    end
  ;


  function TProductionTypeList.badVaccPriorityOrder(): boolean;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
      map: TQIntegerObjectMap;
      i: integer;
    begin
      result := false;

      map := TQIntegerObjectMap.create();
      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          // All production types should have ringVaccParams, whether units are vaccinated or not.
          if( nil = dm.ringVaccParams ) then
            begin
              result := true;
              break;
            end
          else
            begin
              // If a production type is to be vaccinated, it must have the following:
              if( dm.isVaccTarget ) then
                begin
                  // It must have a priority value greater than 0.
                  if( 0 >= dm.ringVaccParams.vaccPriority ) then
                    begin
                      result := true;
                      break;
                    end
                  // It must have a unique priority value.
                  // (The map is used to keep track of priorities as they are encountered.
                  // If the map already contains a value, then that value is not unique, and the problem must be corrected.)
                  else if( map.contains( dm.ringVaccParams.vaccPriority ) ) then
                    begin
                      result := true;
                      break;
                    end
                  else
                    map.insert( dm.ringVaccParams.vaccPriority, dm )
                  ;
                end
              // If a production type is not vaccinated, then it should have a priority of -1.
              else
                begin
                  if( -1 <> dm.ringVaccParams.vaccPriority ) then
                    dm.ringVaccParams.vaccPriority := -1
                  ;
                end
              ;
            end
          ;

          it.incr();
        end
      ;

      it.free();

      if( false = result ) then
        begin
          // While we have the map, we can use it to ensure that priority values start at 1 and are consecutive.
          // Recall that items in a QMap are sorted by index: just read items out of the map in order of the index,
          // and reassign priority values in sequence.

          for i := 0 to map.count - 1 do
            begin
              dm := map.itemAtIndex( i ) as TProductionType;
              if( ( i + 1 ) <> dm.ringVaccParams.vaccPriority ) then
                dm.ringVaccParams.vaccPriority := i + 1
              ;
            end
          ;
        end
      ;

      //self.debug();

      map.Free();
    end
  ;


  function TProductionTypeList.functionsAreValid(): boolean;
    var
      it: TProductionTypeListIterator;
      dm: TProductionType;
    begin
      result := true;

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          dm := it.current();
          if( not dm.functionsAreValid() ) then
            result := false
          ;
          it.incr();
        end
      ;

      it.free();
    end
  ;


  function TProductionTypeList.validate( err: PString = nil ): boolean;
    var
      it: TProductionTypeListIterator;
      simulatingDisease: boolean;
      ringDestrTriggered: boolean;
      ringDestrIsUsed: boolean;
      destrIsUsed: boolean;
      tracingIsUsed: boolean;
      tracingExamIsUsed: boolean;
      testingIsUsed: boolean;
      vaccIsTriggered: boolean;
      vaccIsUsed: boolean;
      zoneIsTriggered: boolean;
      detectionIsUsed: boolean;
      dm: TProductionType;

      sim: TSMSimulationInput;
    begin
      sim := _sim as TSMSimulationInput;

      result := true;
      simulatingDisease := false;
      ringDestrTriggered := false;
      ringDestrIsUsed := false;
      destrIsUsed := false;
      tracingIsUsed := false;
      tracingExamIsUsed := false;
      testingIsUsed := false;
      vaccIsTriggered := false;
      vaccIsUsed := false;
      zoneIsTriggered := false;
      detectionIsUsed := false;

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.simulateTransition ) then simulatingDisease := true;
          if( dm.isDestrTarget ) then destrIsUsed := true;  // Destruction for any reason
          if( dm.isRingDestrTrigger ) then ringDestrTriggered := true;
          if( dm.isRingDestrTarget ) then ringDestrIsUsed := true;

          if( dm.useTracing ) then tracingIsUsed := true;
          if( dm.useTracingExam ) then tracingExamIsUsed := true;
          if( dm.useTesting ) then testingIsUsed := true;

          if( dm.isRingVaccTrigger ) then vaccIsTriggered := true;
          if( dm.isVaccTarget ) then vaccIsUsed := true;
          if( dm.isZoneTrigger ) then zoneIsTriggered := true;

          if( dm.useDetection ) then detectionIsUsed := true;

          if( not( dm.validate( err ) ) ) then
            result := false
          ;

          it.incr();
        end
      ;

      it.free();

      // Check if disease transition is not occuring in any production type.
      //--------------------------------------------------------------------
      if( not simulatingDisease ) then
        begin
          result := false;
          if( nil <> err ) then
            err^ := err^ + tr( 'Disease transition is not occurring in any production type.' ) + endl
          ;
        end
      ;

      // Check to see if tracing, tracing exams, testing, destruction, vaccination, or zones
      // are triggered by any type.
      //---------------------------------------------------------------------------------------
      // If tracing is used, are any production types set to be traced?

      if ( sim.includeDetectionGlobal ) then
        begin
          
          if (( sim.includeDetectionGlobal ) and ( not( detectionIsUsed ))) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Detection is specified but is not used for any production type.' ) + endl
              ;
            end
          ;

          if (( sim.includeTracingGlobal ) and ( not( tracingIsUsed ))) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Tracing is specified but is not used for any production type.' ) + endl
              ;
            end
          ;

          if( ( sim.includeTracingHerdExamGlobal ) and ( not ( tracingExamIsUsed ) ) ) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Examination of traced herds for clinical signs is specified but is not used for any production type.' ) + endl
              ;
            end
          ;

          if( ( sim.includeTracingTestingGlobal ) and ( not ( testingIsUsed ) ) ) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Diagnostic testing of traced herds is specified but is not used for any production type.' ) + endl
              ;
            end
          ;
        end
      ;


      if ( sim.includeDetectionGlobal ) then
        begin
          // If destruction is used, are any production types set to be destroyed?
          if( ( sim.includeDestructionGlobal ) and ( not( destrIsUsed ) ) ) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Destruction is specified but is not used for any production type.' ) + endl
              ;
            end
          ;

          // If ring destruction is triggered, are there any targets?
          if
            ( sim.includeDestructionGlobal )
          and
            ( ringDestrTriggered )
          and
            ( not( ringDestrIsUsed ) )
          then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Use of ring destruction is specified, but no production type is targeted for ring destruction.' ) + endl
              ;
            end
          ;

          // If there are ring destruction targets, is ring destruction triggered?
          if
            ( sim.includeDestructionGlobal )
          and
            ( ringDestrIsUsed )
          and
            ( not( ringDestrTriggered ) )
          then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Use of ring destruction is specified, but is not triggered by any production type.' ) + endl
              ;
            end
          ;

          // Are destruction priorities OK?
          if( ( sim.includeDestructionGlobal ) and ( self.badDestrPriorityOrder() ) ) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Production type destruction priorities are ambiguous: please double-check these parameters.' ) + endl
              ;
            end
          ;
        end
      ;

      // There is nothing here that needs to be validated for tracing.
      // The test above ensures that, if tracing is enabled, then it is conducted for at least one production type.
      // Other validation is handled by TTracingParams itself.

      // If vaccination is specified, are there any triggers?

      if ( sim.includeDetectionGlobal ) then
        begin
          if
            ( sim.includeVaccinationGlobal )
          and
            ( ( not( vaccIsTriggered ) ) and not( (sim as TSMSimulationInput).database.containsInitiallyVaccinatedUnits ) )
          then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Use of vaccination is specified, but is not triggered by any production type.' ) + endl
              ;
            end
          ;

          // If vaccination is specified, are there any targets?
          if( ( sim.includeVaccinationGlobal ) and ( not( vaccIsUsed ) ) ) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Vaccination is specified but is not used for any production type.' ) + endl
              ;
            end
          ;

          // Are vaccination priorities OK?
          if( ( sim.includeVaccinationGlobal ) and ( self.badVaccPriorityOrder() ) ) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Production type vaccination priorities are ambiguous: please double-check these parameters.' ) + endl
              ;
            end
          ;

          // If use of zones is specified, are any zones triggered?
          if( ( sim.includeZonesGlobal ) and ( not( zoneIsTriggered ) )) then
            begin
              result := false;
              if( nil <> err ) then
                err^ := err^ + tr( 'Use of zones is specified, but is not triggered by any production type.' ) + endl
              ;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionType: XML generation
//-----------------------------------------------------------------------------
  function TProductionTypeList.ssDiseaseModelsXml(): string;
    var
      it: TProductionTypeListIterator;
    begin
      result := '';
      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          result := result + it.current().ssDiseaseModelXml() + endl;
          it.incr();
        end
      ;

      it.free();
    end
  ;


  function TProductionTypeList.ssDetectionModelsXml(): string;
    var
      it: TProductionTypeListIterator;
    begin
      result := '';
      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          result := result + it.current().ssDetectionXml() + endl;
          it.incr();
        end
      ;

      it.free();
    end
  ;


  function TProductionTypeList.ssVaccineModelsXml(): string;
    var
      it: TProductionTypeListIterator;
    begin
      result := '';

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if ( nil <> it.current().vaccinationParams ) then
            begin
              if
                ( it.current().vaccinationParams.useVaccination )
              or
                ( (_sim as TSMSimulationInput).database.containsInitiallyVaccinatedUnits( it.current().productionTypeID ) )
              then
                result := endl + result + it.current().vaccinationParams.ssXml( it.current().productionTypeID ) + endl
              ;
            end
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssRingVaccModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          dbcout( dm.productionTypeDescr + ' is ring vacc trigger: ' + usBoolToText( dm.isRingVaccTrigger ), DBPRODUCTIONTYPE );
          if( dm.isRingVaccTrigger ) then
            result := result + dm.ssRingVaccXml()
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssBasicDestructionModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
    begin
      result := '';

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.useBasicDestruction ) then
            result := result + dm.ssBasicDestrXml()
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssTraceDestructionModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.useTraceDestruction ) then
            result := result + dm.ssTraceDestrXml()
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssRingDestructionModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.isRingDestrTrigger ) then
            result := result + dm.ssRingDestrXml()
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssTracingModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
      maxPeriod: integer;
    begin
      result := '';
      maxPeriod := 0;

      it := TProductionTypeListIterator.create( self );

      // First loop: write tracing models
      //---------------------------------
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.useTracing ) then
            begin
              result := result + dm.ssTracingXml();

              if( dm.tracingParams.useTracing ) then
                maxPeriod := max( maxPeriod, dm.tracingParams.maxTracePeriod )
              ;
            end
          ;

          it.incr();
        end
      ;

      // Second loop: write contact recorders
      //-------------------------------------
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.useTracing ) then
            result := result + dm.ssContactRecorderXml( maxPeriod )
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssExamModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.useTracingExam ) then
            result := result + dm.ssExamXml()
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssTestingModelsXml(): string;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.useTesting ) then
            result := result + dm.ssTestXml()
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssZoneModelsXml(): string;
    var
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          result := result + it.current().ssZoneXml();

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.ssEconomicModelsXml( const includeZonesGlobal: boolean; zoneList: TZoneList ): string;
    var
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      result := '';

      while( nil <> it.current() ) do
        begin
          result := result + it.current().ssEconXml( includeZonesGlobal, zoneList );

          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Zone handling
//-----------------------------------------------------------------------------
  procedure TProductionTypeList.removeZone( const zoneID: integer );
    var
      it: TProductionTypeListIterator;
      pt: TProductionType;
    begin
      it := TProductionTypeListIterator.create( self );

      while( nil <> it.current() ) do
        begin
          pt := it.current();

          if( pt.zoneParams.zonePtParamsList.contains( zoneID ) ) then
            pt.zoneParams.zonePtParamsList.delete( zoneID )
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;


  procedure TProductionTypeList.addZone( const zoneID: integer );
    var
      it: TProductionTypeListIterator;
      pt: TProductionType;
    begin
      it := TProductionTypeListIterator.create( self );

      while( nil <> it.current() ) do
        begin
          pt := it.current();

          pt.zoneParams.addZone( zoneID );

          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Debugging
//-----------------------------------------------------------------------------
  procedure TProductionTypeList.debug();
    var
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      dbcout( '-=-=-=-=-=-=-=-= Beginning TProductionTypeList.debug()...', true );
      while( nil <> it.current() ) do
        begin
          it.current().debug();
          dbcout( endl, true );
          it.incr();
        end
      ;
      dbcout( '-=-=-=-=-=-=-=-= Done TProductionTypeList.debug()', true );
      dbcout( endl, true );

      it.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: XML import
//-----------------------------------------------------------------------------
  function TProductionTypeList.getXmlModelList(): TQStringList;
    var
      list: TQStringList;
    begin
      if( nil = _xmlModelList ) then
        begin
          list := TProductionType.createXmlModelList();
          _xmlModelList := TQStringList.create( list );
          list.Free();
        end
      ;

      result := _xmlModelList;
    end
  ;


  procedure TProductionTypeList.importPtXml( ptName: string; ptID: integer; model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      pt: TProductionType;
    begin
      pt := self.findProdType( ptName );

      // If the production type is already in the list with a different ID number, that's a problem.
      if( nil <> pt ) then
        begin
          if( ( 0 < ptID ) and ( pt.productionTypeID <> ptID ) ) then
            begin
              appendToPstring( errMsg, tr( 'XML contains a mismatched production type name and ID number.' ) );
              exit;
            end
          ;
        end
      // If the production type is not in the list but another one with the same ID number is, that's also a problem.
      else if( nil <> self.findProdType( ptID ) ) then
        begin
          appendToPstring( errMsg, tr( 'XML contains a mismatched production type name and ID number.' ) );
          exit;
        end
      // If the production type is not in the list, that's not a problem.
      else
        begin
          pt := TProductionType.create( ptID, ptName, false, sim );
          self.append( pt );
        end
      ;

      // If we get this far, try to import the parameters
      pt.importXml( model, sdew, errMsg );
    end
  ;


  procedure TProductionTypeList.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      ptID: integer;
      ptName, toPtName, fromPtName: string;
    begin
      ptName := sdew.GetElementAttribute( model, 'production-type' );

      if( not strIsEmpty( ptName ) ) then
        begin
          ptID := myStrToInt( sdew.GetElementAttribute( model, 'production-type-id' ), -1 );
          importPtXml( ptName, ptID, model, sdew, errMsg );
        end
      else
        begin
          toPtName := sdew.GetElementAttribute( model, 'to-production-type' );
          if( not strIsEmpty( toPtName ) ) then
            importPtXml( toPtName, -1, model, sdew, errMsg )
          ;

          fromPtName := sdew.GetElementAttribute( model, 'from-production-type' );
          if( not strIsEmpty( fromPtName ) ) then
            importPtXml( fromPtName, -1, model, sdew, errMsg )
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypeList: Properties
//-----------------------------------------------------------------------------
  function TProductionTypeList.getUpdated(): boolean;
    var
      it: TProductionTypeListIterator;
    begin
      result := false;

      it := TProductionTypeListIterator.create( self );

      while( nil <> it.current() ) do
        begin
          if( it.current().updated ) then
            begin
              result := true;
              break;
            end
          ;
          
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.deswUTimeAvg(): double;
    begin
      if( 0 = _unitsDestroyed ) then
        result := 0.0
      else
        result := _deswUDaysInQueue / _unitsDestroyed
      ;
    end
  ;


  function TProductionTypeList.vacwUTimeAvg(): double;
    begin
      if( 0 = _unitsVaccinated ) then
        result := 0.0
      else
        result := _vacwUDaysInQueue / _unitsVaccinated
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// ProductionTypeListIterator
//-----------------------------------------------------------------------------
  function TProductionTypeListIterator.toFirst(): TProductionType;
    begin
      result := _toFirst() as TProductionType;
    end
  ;


  function TProductionTypeListIterator.toLast(): TProductionType;
    begin
      result := _toLast() as TProductionType;
    end
  ;


  function TProductionTypeListIterator.current(): TProductionType;
    begin
      result := _current() as TProductionType;
    end
  ;
//-----------------------------------------------------------------------------



end.