unit ProductionTypeList;

(*
ProductionTypeList.pas
----------------------
Begin: 2005/01/06
Last revision: $Date: 2008/11/25 22:04:42 $ $Author: areeves $
Version number: $Revision: 1.26 $
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
    QStringMaps,
    SqlClasses,
    Models,
    ChartFunction,
    SMDatabase,
    FunctionEnums,
    ProductionType
  ;


  type TProductionTypeList = class( TModelList )
      protected
        procedure setObject( index: integer; item: TProductionType );
        function getObject( index: integer ): TProductionType;

        function getUpdated(): boolean;

        function getMaxDestrPriority(): integer;
        function getMaxVaccPriority(): integer;

        function badDestrPriorityOrder(): boolean;
        function badVaccPriorityOrder(): boolean;

      public
        constructor create( sim: TObject ); overload;
        constructor create( db: TSMDatabase; sim: TObject ); overload;
        constructor create( const src: TProductionTypeList; sim: TObject ); overload;

        destructor destroy(); override;

        function equals( const otherList: TProductionTypeList ): boolean;

        procedure sortByDestrOrder();
        procedure sortByVaccOrder();

        // Typical list functions
        function append( dm: TProductionType ): integer; reintroduce;
        procedure insert( index: integer; dm: TProductionType );
        property objects[ index: integer]: TProductionType read getObject write setObject; default;

        function at( const i: word ): TProductionType;

        function byID( id: integer ): TProductionType;
        function findProdTypeID( typeDescr: string ): integer;
        function findProdType( typeDescr: string ): TProductionType; overload;
        function findProdType( typeID: integer ): TProductionType; overload;
        function findProdTypeName( typeID: integer ): string;
        function prodTypeIDExists( typeID: integer ): boolean;

        procedure populateDatabase( db: TSMDatabase );

        function ssDiseaseModelsXml(): string;
        function ssDetectionModelsXml(): string;
        function ssBasicDestructionModelsXml( destrPriorityList: TQStringLongIntMap ): string;

        function ssTracebackDestructionModelsXml(
          destrPriorityList: TQStringLongIntMap;
          const includeDestructionGlobal: boolean
        ): string;

        function ssRingDestructionModelsXml(): string;
        function ssVaccineModelsXml(): string;
        function ssRingVaccModelsXml(): string;
        function ssZoneModelsXml(): string;

        procedure removeZone( const zoneID: integer );
        procedure addZone( const zoneID: integer );

        procedure recountUnits( hList: TObject );

        // For model outputs
        procedure clearAllRecords( db: TSMDatabase );
        procedure resetIterationRecords();
        procedure prepareForDay( day: integer );
        procedure processDailyRecords( db: TSMDatabase; iteration: integer; day: integer );
        procedure processIterationRecords( db: TSMDatabase; iteration: integer );

        //Inherited functions for chart handling
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

        property maxDestrPriority: integer read getMaxDestrPriority;
        property maxVaccPriority: integer read getMaxVaccPriority;
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
    usStrUtils,
    DebugWindow,
    QIntegerMaps,
    I88n,

    DetectionParams,
    ZoneParams,
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
      if ( TProductionType(Item1).ringVaccParams.vaccPriority < 0 )  then
        result :=  1
      else
         if ( TProductionType(Item2).ringVaccParams.vaccPriority < 0 ) then
           result := -1
         else
          result := compareValue(
            TProductionType(Item1).ringVaccParams.vaccPriority,
            TProductionType(Item2).ringVaccParams.vaccPriority
      );
    end
  ;
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// TProductionTypeList: Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TProductionTypeList.create( sim: TObject );
    begin
      inherited create( true );

      if ( Assigned( sim ) ) then
        _sim := sim
      else
        _sim := nil;
    end
  ;


  constructor TProductionTypeList.create( const src: TProductionTypeList; sim: TObject );
    var
      srcPT: TProductionType;
      newPT: TProductionType;
      it: TProductionTypeListIterator;
    begin
      inherited create( src );

      it := TProductionTypeListIterator.create( src );

      if ( ( Assigned( src ) ) and ( Assigned( sim ) ) ) then
        begin
          _sim := sim;

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
      else
        _sim := nil
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
      vacc: TVaccinationParams;
      ringv: TRingVaccParams;
      cost: TCostParams;
      zone: TZoneParams;
    begin
      inherited create( true );

      if ( ( Assigned( db ) ) and ( Assigned( sim ) ) ) then
        begin 
          _sim := sim;

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
            + ' prevalenceChart.chartName as prevalenceChartName, '
            + ' inProductionType.useDetection '
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

              //pt.sim := sim;

              if( null <> row.field('disLatentPeriodPdfID') ) then
                pt.latentName := row.field( 'latentChartName' )
              ;

              if( null <> row.field('disSubclinicalPeriodPdfID') ) then
                pt.subclinicalName := row.field( 'subclinicalChartName' )
              ;

              if( null <> row.field('disClinicalPeriodPdfID') ) then
                pt.clinicalName := row.field( 'clinicalChartName' )
              ;

              if( null <> row.field('disImmunePeriodPdfID') ) then
                pt.immuneName :=row.field( 'immuneChartName' )
              ;

              if( null <> row.field('disPrevalenceRelID') ) then
                pt.prevalenceName := row.field( 'prevalenceChartName' )
              ;

              // FIX ME: this still needs to be fixed...
              if( null <> row.field('useDetection') ) then
                begin
                  pt.useDetection := row.field('useDetection');
                  det := TDetectionParams.create( _sim, row.field('descr') );
                  det.initializeFromDB( db, integer( row.field('productionTypeID') ), row.field('descr') );
                  pt.detectionParams := det;
                end
              else
                begin
                  det := TDetectionParams.create( string( row.field('descr') ) );
                  det.sim := _sim;
                  pt.detectionParams := det;
                end
              ;

              destr := TDestructionParams.create();
              destr.sim := _sim;
              destr.initializeFromDB(  db, integer( row.field('productionTypeID') ), row.field('descr') );
              pt.destructionParams := destr;

              vacc := TVaccinationParams.create();
              vacc.sim := _sim;
              vacc.initializeFromDB( db, integer( row.field('productionTypeID') ), row.field('descr') );
              pt.vaccinationParams := vacc;

              ringv := TRingVaccParams.create( db, integer( row.field('productionTypeID') ), row.field('descr') );
              ringv.sim := _sim;
              pt.ringVaccParams := ringv;

              zone := TZoneParams.create( db, integer( row.field('productionTypeID') ), row.field('descr'), (_sim as TSMSimulationInput).zoneList );
              zone.sim := _sim;
              pt.zoneParams := zone;

              cost := TCostParams.Create( db, integer( row.field('productionTypeID') ), row.field('descr'), _sim );
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


  destructor TProductionTypeList.destroy();
    begin
      // The base class takes care of this.
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
    end
  ;


  procedure TProductionTypeList.setObject( index: integer; item: TProductionType );
    begin
      inherited SetItem( index, item );
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
    end
  ;


  (*
  function TProductionTypeList.first() : TProductionType;
    begin
      _currentIndex := 0;
      if( self.Count = 0 ) then
        result := nil
      else
        result := getObject( _currentIndex )
      ;
    end
  ;


  function TProductionTypeList.last() : TProductionType;
    begin
      if( self.Count = 0 ) then
        result := nil
      else
        begin
          _currentIndex := self.Count - 1;
          result := getObject( _currentIndex );
        end
      ;
    end
  ;


  function TProductionTypeList.next() : TProductionType;
    begin
      _currentIndex := _currentIndex + 1;
      if( _currentIndex > (self.Count - 1) ) then
        result := nil
      else
        result := getObject( _currentIndex )
      ;
    end
  ;


  function TProductionTypeList.current() : TProductionType;
    begin
      if( _currentIndex > (self.Count - 1) ) then
        result := nil
      else
        result := getObject( _currentIndex )
      ;
    end
  ;
  *)

  function TProductionTypeList.at( const i: word ): TProductionType;
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
      tPT: TProductionType;
      it: TProductionTypeListIterator;
    begin
      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          tPT := it.current();

          if( tPT.hasChartName( oldChartName, whichChart ) ) then
            tPT.changeChart( whichChart, oldChartName, newChart, addlInfo )
          ;
          
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
       lit := TProductionTypeListIterator.create( self );

       while( nil <> lit.current() ) do
        begin
          lit.current().resetIterationRecords();
          lit.incr();
        end
       ;

       lit.Free();
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


  procedure TProductionTypeList.processDailyRecords( db: TSMDatabase; iteration: integer; day: integer );
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

       lit.Free();
    end
  ;


  procedure TProductionTypeList.processIterationRecords( db: TSMDatabase; iteration: integer );
    var
      lit: TProductionTypeListIterator;
    begin
      lit := TProductionTypeListIterator.create( self );

      while( nil <> lit.current() ) do
      begin
        lit.current().processIterationRecords( db, iteration );
        lit.incr();
      end
      ;

      lit.Free();
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
  procedure TProductionTypeList.populateDatabase( db: TSMDatabase );
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

                  // Remove this production type from the PTP list in memory
                  if( nil <> sim.ptpList ) then sim.ptpList.removeProductionType( dm.productionTypeID );

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
                dm.populateDatabase( db )
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
      map: TQIntegerStringMap;
    begin
      result := false;

      map := TQIntegerStringMap.create();
      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.isDestrTarget ) then
            begin
              if( map.contains( dm.destructionParams.destrPriority ) ) then
                begin
                  result := true;
                  break;
                end
              else
                map.insert( dm.destructionParams.destrPriority, dm.productionTypeDescr )
              ;
            end
          ;

          it.incr();
        end
      ;

      it.free();
      map.Free();
    end
  ;


  function TProductionTypeList.badVaccPriorityOrder(): boolean;
    var
      dm: TProductionType;
      it: TProductionTypeListIterator;
      map: TQIntegerStringMap;
    begin
      result := false;

      map := TQIntegerStringMap.create();
      it := TProductionTypeListIterator.create( self );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.isVaccTarget ) then
            begin
              if( nil = dm.ringVaccParams ) then
                begin
                  result := true;
                  break;
                end
              else if( map.contains( dm.ringVaccParams.vaccPriority ) ) then
                begin
                  result := true;
                  break;
                end
              else
                map.insert( dm.ringVaccParams.vaccPriority, dm.productionTypeDescr )
              ;
            end
          ;

          it.incr();
        end
      ;

      it.free();
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
      vaccIsTriggered: boolean;
      vaccIsUsed: boolean;
      zoneIsTriggered: boolean;
      dm: TProductionType;

      sim: TSMSimulationInput;
    begin
      sim := _sim as TSMSimulationInput;

      result := true;
      simulatingDisease := false;
      ringDestrTriggered := false;
      ringDestrIsUsed := false;
      destrIsUsed := false;
      vaccIsTriggered := false;
      vaccIsUsed := false;
      zoneIsTriggered := false;

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.simulateTransition ) then simulatingDisease := true;
          if( dm.isDestrTarget ) then destrIsUsed := true;  // Destruction for any reason
          if( dm.isRingDestrTrigger ) then ringDestrTriggered := true;
          if( dm.isRingDestrTarget ) then ringDestrIsUsed := true;

          if( dm.isRingVaccTrigger ) then vaccIsTriggered := true;
          if( dm.isVaccTarget ) then vaccIsUsed := true;
          if( dm.isZoneTrigger ) then zoneIsTriggered := true;

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

      // Check to see if destruction, vaccination, or zones are triggered by any type.
      //------------------------------------------------------------------------------
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

      // If vaccination is specified, are there any triggers?
      if( ( sim.includeVaccinationGlobal ) and ( not( vaccIsTriggered ) ) ) then
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
          if ( nil <> it.current().vaccinationParams  ) then
            begin
              if( it.current().vaccinationParams.useVaccination ) then
                result := result + it.current().vaccinationParams.ssXml( it.current().productionTypeID ) + endl
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


  function TProductionTypeList.ssBasicDestructionModelsXml( destrPriorityList: TQStringLongIntMap ): string;
    var
      it: TProductionTypeListIterator;
      priority: integer;
    begin
      result := '';

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( it.current().destructionParams.destroyDetectedUnits ) then
            begin
              priority := destrPriorityList[ it.current().productionTypeDescr + '+' + 'basic' ];
              result := result + it.current().destructionParams.ssBasicDestrModelXml( priority, it.current().productionTypeID );
            end
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


  function TProductionTypeList.ssTracebackDestructionModelsXml(
        destrPriorityList: TQStringLongIntMap;
        const includeDestructionGlobal: boolean
      ): string;
    var
      it: TProductionTypeListIterator;
      dm: TProductionType;
      priority: integer;
    begin

      result := '';

      it := TProductionTypeListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          dm := it.current();

          if( dm.destructionParams.traceDirectContact ) then
            begin
              priority := destrPriorityList[ dm.productionTypeDescr + '+' + 'direct' ];
              result := result + dm.destructionParams.ssTracebackDestrModelXml( priority, 'direct', includeDestructionGlobal, dm.productionTypeID );
            end
          ;

          if( dm.destructionParams.traceIndirectContact ) then
            begin
              priority := destrPriorityList[ dm.productionTypeDescr + '+' + 'indirect' ];
              result := result + dm.destructionParams.ssTracebackDestrModelXml( priority, 'indirect', includeDestructionGlobal, dm.productionTypeID );
            end
          ;

          it.incr();
        end
      ;

      it.free();
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

      while( nil <> it.current() ) do
        begin
          it.current().debug();
          it.incr();
        end
      ;

      it.free();
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


  function TProductionTypeList.getMaxDestrPriority(): integer;
    var
      it: TProductionTypeListIterator;
    begin
      result := -1;

      it := TProductionTypeListIterator.create( self );

      while( nil <> it.current() ) do
        begin
          if( result < it.current().destructionParams.destrPriority ) then
            result := it.current().destructionParams.destrPriority
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  function TProductionTypeList.getMaxVaccPriority(): integer;
    var
      it: TProductionTypeListIterator;
    begin
      result := -1;

      it := TProductionTypeListIterator.create( self );

      while( nil <> it.current() ) do
        begin
          if( result < it.current().ringVaccParams.vaccPriority ) then
            result := it.current().ringVaccParams.vaccPriority
          ;
          it.incr();
        end
      ;

      it.Free();
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