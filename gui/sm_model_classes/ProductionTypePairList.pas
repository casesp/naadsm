unit ProductionTypePairList;

(*
ProductionTypePairList.pas
---------------------------
Begin: 2005/04/02
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.36 $
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
  	Models,
  	ProductionType,
    ProductionTypeList,
    Contnrs,
    SMDatabase,
		ProbDensityFunctions,
    RelFunction,
    FunctionEnums,
    ChartFunction,
    ProductionTypePair,
    FunctionDictionary
  ;

  type TProductionTypePairList = class( TModelList )
    protected
      function getUpdated():boolean;

  	public
      constructor create( db: TSMDatabase; sim: TObject ); overload;
      constructor create( const src: TProductionTypePairList; const ptList: TProductionTypeList; sim: TObject ); overload;

      constructor create( ptList: TProductionTypeList ); overload;

      destructor destroy(); override;

      function ssXML(): string;

      function validate( err: PString = nil ): boolean; override;
      function functionsAreValid(): boolean; override;

      procedure debug(); override;

    	// Needed for chart editing
      procedure removeChart( const chartName: string ); override;
      procedure changeChart(
          const whichChart: TSMChart;
          const oldChartName: string;
          newChart: TChartFunction;
          addlInfo: integer = -1
      ); override;
      procedure removeProductionType( const ptID: integer );
      procedure logicAndAdd( ptpList: TProductionTypePairList );

      // Typical list functions
      function append( dm: TProductionTypePair ): integer; reintroduce;
      function at( i: integer ): TProductionTypePair;

      // Other stuff
      procedure populateDatabase( db: TSMDatabase; update: boolean = false );

      procedure subtract( ptp: TProductionTypePair ); overload;
      procedure subtract( ptpList: TProductionTypePairList ); overload;

      function pairPosition( src, dst: TProductionType ): integer;

      property updated:boolean read getUpdated;
    end
  ;


  {type} TProductionTypePairListIterator = class( TModelListIterator )
    public
      function toFirst(): TProductionTypePair;
      function toLast(): TProductionTypePair;
      function current(): TProductionTypePair;
    end
  ;

  
  const
    DBPRODUCTIONTYPEPAIRLIST: boolean = false; // set to true to enable debugging messages for this unit

implementation

	uses
  	SysUtils,
    SqlClasses,
    MyStrUtils,
    USStrUtils,
    DebugWindow,
    Variants,

    SMSimulationInput,
    ContactModel,
    AirborneSpreadModel
  ;


//-----------------------------------------------------------------------------
// List: Construction/destruction
//-----------------------------------------------------------------------------
  constructor TProductionTypePairList.create( ptList: TProductionTypeList );
  	var
    	itSrc, itDest: TProductionTypeListIterator;
      src, dest: TProductionType;
      ptPair: TProductionTypePair;
      c: integer;
    begin
      dbcout( 'TProductionTypePairList.create( ptList: TProductionTypeList )', DBPRODUCTIONTYPEPAIRLIST );
    	inherited create();
      _sim := ptList.sim;

    	itSrc := TProductionTypeListIterator.create( ptList );
      itDest := TProductionTypeListIterator.create( ptList );

      dbcout2( 'Counting!' );
      c := 0;

      while( itSrc.current() <> nil ) do
      	begin
        	src := itSrc.current();

          while( itDest.current() <> nil ) do
          	begin
              inc( c );
              dbcout2( c );
              dest := itDest.current();

              ptPair := TProductionTypePair.create( src, dest, _sim );
              self.append( ptPair );

              dbcout2( ptPair.pairDescr );

            	itDest.incr();
            end
          ;

        	itSrc.incr();
          itDest.toFirst();
        end
      ;

      dbcout2( '' );
      
      itSrc.free();
      itDest.free();

    end
  ;


  constructor TProductionTypePairList.create( const src: TProductionTypePairList; const ptList: TProductionTypeList; sim: TObject );
    var
      it: TProductionTypePairListIterator;
      srcPTP: TProductionTypePair;
      newPTP: TProductionTypePair;
    begin
      inherited create( src );

      _sim := sim;

      it := TProductionTypePairListIterator.create( src );
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          srcPTP := it.current();
          newPTP := TProductionTypePair.create( srcPTP, ptList, _sim );
          self.append( newPTP );
          it.incr();
        end
      ;

      it.Free();
    end
  ;


	constructor TProductionTypePairList.create( db: TSMDatabase; sim: TObject );
  	var
    	q: string;
      res: TSqlResult;
      row: TSqlRow;
      db2: TSqlDatabase;
      srcID, destID: integer;
      ptPair: TProductionTypePair;
      ptList: TProductionTypeList;

      cid: integer;
  	begin
      dbcout( 'TProductionTypePairList.create( ptList: TProductionTypeList; db: TSMDatabase )', DBPRODUCTIONTYPEPAIRLIST );
    	inherited create();

      _sim := sim;
      ptList := (sim as TSMSimulationInput).ptList;

    	db2 := db as TSqlDatabase;

      q := 'SELECT '
      		+ '`sourceProductionTypeID`, '
        	+ '`destProductionTypeID`, '
          + '`useDirectContact`, '
          + '`directContactSpreadID`, '
          + '`useIndirectContact`, '
          + '`indirectContactSpreadID`, '
          + '`useAirborneSpread`, '
          + '`airborneContactSpreadID` '
        + 'FROM `inProductionTypePair`'
      ;

      res := TSqlResult.create( q, db2 );

      row := res.fetchArrayFirst();
      while( row <> nil ) do
      	begin
          srcID := row.field( 'sourceProductionTypeID' );
          destID := row.field( 'destProductionTypeID' );

          // FIX ME: there are no checks for nil values here.
          // While nil values theoretically are OK, they probably pose a practical problem...
          ptPair := TProductionTypePair.create( ptList.findProdType( srcID ), ptList.findProdType( destID ), _sim );
          ptPair.isInDB := true;

          ptPair.includeDirect := row.field( 'useDirectContact' );

          // It is a bit awkward to create a TContactModel object even when contact
          // is not being used, but it is somewhat easier (for the time being) when
          // it comes to using TFormContactSpread to set/change parameters.
          if( null = row.field( 'directContactSpreadID' ) ) then
            cid := -1
          else
            cid := row.field( 'directContactSpreadID' )
          ;

          ptPair.direct := TContactModel.create(
            db,
            cid,
            CMDirect,
            _sim,
            srcID,
            destID,
            (null <> row.field( 'directContactSpreadID' ) ) // if not null, populate the object from the database
          );

          ptPair.includeIndirect := row.field( 'useIndirectContact' );


          // It is a bit awkward to create a TContactModel object even when contact
          // is not being used, but it is somewhat easier (for the time being) when
          // it comes to using TFormContactSpread to set/change parameters.
          if( null = row.field( 'indirectContactSpreadID' ) ) then
            cid := -1
          else
            cid := row.field( 'indirectContactSpreadID' )
          ;

          ptPair.indirect := TContactModel.create(
            db,
            cid,
            CMIndirect,
            _sim,
            srcID,
            destID,
            ( null <> row.field( 'indirectContactSpreadID' ) ) // if not null, populate the object from the database
          );


          // It is a bit awkward to create a TAirborneSpreadModel object even when
          // airborne spread is not being used, but it is somewhat easier (for the
          // time being) when it comes to using TFormAirborneSpread to set/change
          // parameters.
          if( null = row.field( 'airborneContactSpreadID' ) ) then
            cid := -1
          else
            cid := row.field( 'airborneContactSpreadID' )
          ;

          ptPair.airborne := TAirborneSpreadModel.create(
            db,
            cid,
            _sim,
            srcID,
            destID,
            ( null <> row.field( 'airborneContactSpreadID' ) ) // if not null, populate the object from the database
          );

          ptPair.updated := false;
          self.append( ptPair );
        	row := res.fetchArrayNext();
        end
      ;

      res.Free();
    end
  ;

  
  destructor TProductionTypePairList.destroy();
  	begin
      dbcout( '*** Destroying TProductionTypePairList', DBPRODUCTIONTYPEPAIRLIST );
      // By default, TObjectList owns its objects and will free them.
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


  function TProductionTypePairList.getUpdated():boolean;
    var
      it: TProductionTypePairListIterator;
    begin
      result := false;

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if ( it.current().updated ) then
            begin
              result := true;
              break;
            end
          ;
          it.incr();
        end
      ;

      it.free();
    end
  ;


  procedure TProductionTypePairList.populateDatabase( db: TSMDatabase; update: boolean = false );
    var
      it: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
    begin

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();
          ptp.populateDatabase( db, update );
          if ( ptp.removed ) then
            begin
              remove( ptp );
              it.current();
            end
          else
            it.incr();
        end
      ;

      it.Free();
    end
  ;

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
  function TProductionTypePairList.ssXML(): string;
    var
      it: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
      useContactModels: boolean;
      useAirborneModels: boolean;
    begin
      useContactModels := (_sim as TSMSimulationInput ).includeContactSpreadGlobal;
      useAirborneModels := (_sim as TSMSimulationInput ).includeAirborneSpreadGlobal;
      result := '';

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();
          if( useContactModels ) then
            begin
              dbcout( '*** Using contact models in TProductionTypePairList.ssXML', DBPRODUCTIONTYPEPAIRLIST );
              result := result + ptp.directContactXML();
              result := result + ptp.indirectContactXML();
            end
          else
            dbcout( 'Not using contact models in TProductionTypePairList.ssXML', DBPRODUCTIONTYPEPAIRLIST )
          ;

          if( useAirborneModels ) then
            begin
              dbcout( '*** Using airborne spread models in TProductionTypePairList.ssXML', DBPRODUCTIONTYPEPAIRLIST );
              result := result + ptp.airborneSpreadXML();
            end
          else
            dbcout( 'Not using airborne spread models in TProductionTypePairList.ssXML', DBPRODUCTIONTYPEPAIRLIST )
          ;

          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Validation
//-----------------------------------------------------------------------------
  function TProductionTypePairList.validate( err: PString = nil ): boolean;
    var
      it: TProductionTypePairListIterator;
    begin
      result := true;

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( not( it.current().validate( err ) ) ) then
            result := false
          ;

          it.incr();
        end
      ;

      it.free();
    end
  ;


  function TProductionTypePairList.functionsAreValid(): boolean;
    var
      it: TProductionTypePairListIterator;
    begin
      result := true;

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( not it.current().functionsAreValid() ) then
            result := false
          ;
          it.incr();
        end
      ;

      it.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart handling functions
//-----------------------------------------------------------------------------
  procedure TProductionTypePairList.removeChart( const chartName: string );
    var
      it: TProductionTypePairListIterator;
    begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          it.current().removeChart( chartName );
          it.incr();
        end
      ;

      it.free();
    end
  ;

  
  procedure TProductionTypePairList.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    var
      it: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
    begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();
          ptp.changeChart( whichChart, oldChartName, newChart );
          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TProductionTypePairList.debug();
    var
      it: TProductionTypePairListIterator;
    begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          it.current().debug();
          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// List: Typical list functions
//-----------------------------------------------------------------------------
  function TProductionTypePairList.append( dm: TProductionTypePair ): integer;
    begin
      result := inherited Add( dm );
      dm.added := true;
    end
  ;

  
  function TProductionTypePairList.at( i: integer ): TProductionTypePair;
  	begin
      if( i > self.Count-1 ) then
      	raise exception.Create( 'Index out of bounds (' + intToStr( i ) + ') in TProductionTypePairList with ' + intToStr( self.Count ) + ' items.' )
      else
      	result := getObject( i ) as TProductionTypePair
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// List: Other specialized functions
//-----------------------------------------------------------------------------

  procedure TProductionTypePairList.logicAndAdd( ptpList: TProductionTypePairList );
  	var
      it: TProductionTypePairListIterator;
      optIt: TProductionTypePairListIterator;
    	ptPair, optPair: TProductionTypePair;
      found: boolean;
    begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
          it.current().removed := true;
          it.current().updated := true;
          it.incr();
        end
      ;

      optIt := TProductionTypePairListIterator.create( ptpList );
      optIt.toFirst();

      while ( nil <> optIt.current() ) do
        begin
          optPair := optIt.current();

          it.toFirst();
          found := false;

          while ( nil <> it.current() ) do
            begin
              ptPair := it.current();
              if ( ptPair.source = optPair.source ) and ( ptPair.dest = optPair.dest ) then
                begin
                  ptPair.removed := false;
                  found := true;
                  break;
                end
              ;
              it.incr();
            end
          ;

          if ( not found ) then
            begin
              self.append( TProductionTypePair.create( optPair.source, optPair.dest, optPair.sim ) );
            end;
          optIt.incr();
        end
      ;

      it.Free();
      optIt.Free();
    end
  ;


  procedure TProductionTypePairList.removeProductionType( const ptID: integer );
    var
      it: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
    begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();
          if( ( ptID = ptp.source.productionTypeID ) or ( ptID = ptp.dest.productionTypeID ) ) then
            begin
              remove( ptp );
              it.current();
            end
          else
            it.incr()
          ;
        end
      ;
    end
  ;


	procedure TProductionTypePairList.subtract( ptp: TProductionTypePair );
  	var
      it: TProductionTypePairListIterator;
    	ptPair: TProductionTypePair;
  	begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
          ptPair := it.current();
       		if( ptPair.source = ptp.source ) then
          	begin
              if( ptPair.dest = ptp.dest ) then
                begin
                  self.remove( ptPair );
                  // FIX ME: should the function stop after removing
                  // a single instance, or should it remove all matches?
                  // (Right now, it's doing the former.)
                  break;
                end
              ;
            end
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;


  procedure TProductionTypePairList.subtract( ptpList: TProductionTypePairList );
  	var
      it: TProductionTypePairListIterator;
  	begin
      it := TProductionTypePairListIterator.create( ptpList );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
      		self.subtract( it.current() );
          it.incr();
        end
      ;

      it.Free();
    end
  ;


	function TProductionTypePairList.pairPosition( src, dst: TProductionType ): integer;
  	var
      it: TProductionTypePairListIterator;
    	ptp: TProductionTypePair;
  	begin
    	result := -1;

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
          ptp := it.current();
       		if( ptp.source = src ) then
          	begin
              if( ptp.dest = dst ) then
                begin
									result := it.currentIndex;
                  break;
                end
              ;
            end
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProductionTypePairListIterator
//-----------------------------------------------------------------------------
  function TProductionTypePairListIterator.toFirst(): TProductionTypePair;
    begin
      result := _toFirst() as TProductionTypePair;
    end
  ;


  function TProductionTypePairListIterator.toLast(): TProductionTypePair;
    begin
      result := _toLast() as TProductionTypePair;
    end
  ;


  function TProductionTypePairListIterator.current(): TProductionTypePair;
    begin
      result := _current() as TProductionTypePair;
    end
  ;
//-----------------------------------------------------------------------------

end.
