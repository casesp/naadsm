unit ProductionTypePairList;

(*
ProductionTypePairList.pas
---------------------------
Begin: 2005/04/02
Last revision: $Date: 2013-06-27 19:11:35 $ $Author: areeves $
Version number: $Revision: 1.42.4.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Contnrs,

    QLists,
    
    Sdew,

    ModelDatabase,
    Models,
    FunctionDictionary,
    ChartFunction,
    ProbDensityFunctions,
    RelFunction,

    ProductionType,
    ProductionTypeList,
    SMDatabase,
    FunctionEnums,
    ProductionTypePair
  ;

  type TProductionTypePairList = class( TModelList )
    protected
      _xmlModelList: TQStringList;

      function getUpdated():boolean;

      // XML import
      function getXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

    public
      constructor create( db: TSMDatabase; sim: TObject ); overload;
      constructor create( const src: TProductionTypePairList; const ptList: TProductionTypeList; sim: TObject ); overload;
      constructor create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil ); overload;
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
      procedure populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType );

      procedure subtract( ptp: TProductionTypePair ); overload;
      procedure subtract( ptpList: TProductionTypePairList ); overload;

      function pairPosition( src, dst: TProductionType ): integer;

      property updated:boolean read getUpdated;
      property xmlModelList: TQStringList read getXmlModelList;
    end
  ;


  {type} TProductionTypePairListIterator = class( TModelListIterator )
    public
      function toFirst(): TProductionTypePair;
      function toLast(): TProductionTypePair;
      function current(): TProductionTypePair;
    end
  ;


implementation

  uses
    StrUtils,
    SysUtils,
    Variants,

    SqlClasses,
    MyStrUtils,
    DebugWindow,
    I88n,
    
    SMSimulationInput,
    ContactSpreadParams,
    AirborneSpreadParams
  ;

  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit



//-----------------------------------------------------------------------------
// List: Construction/destruction
//-----------------------------------------------------------------------------
  constructor TProductionTypePairList.create( ptList: TProductionTypeList );
    var
      itSrc, itDest: TProductionTypeListIterator;
      src, dest: TProductionType;
      ptPair: TProductionTypePair;
    begin
      dbcout( 'TProductionTypePairList.create( ptList: TProductionTypeList )', DBSHOWMSG );
      inherited create();
      _xmlModelList := nil;
      _sim := ptList.sim;

      itSrc := TProductionTypeListIterator.create( ptList );
      itDest := TProductionTypeListIterator.create( ptList );

      while( itSrc.current() <> nil ) do
        begin
          src := itSrc.current();

          while( itDest.current() <> nil ) do
            begin
              dest := itDest.current();

              ptPair := TProductionTypePair.create( src, dest, _sim );
              self.append( ptPair );
              
              itDest.incr();
            end
          ;

          itSrc.incr();
          itDest.toFirst();
        end
      ;

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
      _xmlModelList := nil;
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
      ptSrc, ptDest: TProductionType;
    begin
      dbcout( 'TProductionTypePairList.create( ptList: TProductionTypeList; db: TSMDatabase )', DBSHOWMSG );
      inherited create();
      _xmlModelList := nil;
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

          ptSrc := ptList.findProdType( srcID );
          ptDest := ptList.findProdType( destID );

          if( ( nil = ptSrc ) or ( nil = ptDest ) ) then
            raise exception.create( 'A nil pt has been encountered in TProductionTypePairList.create()' )
          ;

          ptPair := TProductionTypePair.create( ptSrc, ptDest, _sim );
          ptPair.isInDB := true;

          ptPair.includeDirect := row.field( 'useDirectContact' );

          // It is a bit awkward to create a TContactSpreadParams object even when contact
          // is not being used, but it is somewhat easier (for the time being) when
          // it comes to using TFormContactSpread to set/change parameters.
          if( null = row.field( 'directContactSpreadID' ) ) then
            cid := -1
          else
            cid := row.field( 'directContactSpreadID' )
          ;

          ptPair.direct := TContactSpreadParams.create(
            db,
            cid,
            CMDirect,
            _sim,
            srcID,
            destID,
            (null <> row.field( 'directContactSpreadID' ) ) // if not null, populate the object from the database
          );

          ptPair.includeIndirect := row.field( 'useIndirectContact' );


          // It is a bit awkward to create a TContactSpreadParams object even when contact
          // is not being used, but it is somewhat easier (for the time being) when
          // it comes to using TFormContactSpread to set/change parameters.
          if( null = row.field( 'indirectContactSpreadID' ) ) then
            cid := -1
          else
            cid := row.field( 'indirectContactSpreadID' )
          ;

          ptPair.indirect := TContactSpreadParams.create(
            db,
            cid,
            CMIndirect,
            _sim,
            srcID,
            destID,
            ( null <> row.field( 'indirectContactSpreadID' ) ) // if not null, populate the object from the database
          );


          // It is a bit awkward to create a TAirborneSpreadParams object even when
          // airborne spread is not being used, but it is somewhat easier (for the
          // time being) when it comes to using TFormAirborneSpread to set/change
          // parameters.
          if( null = row.field( 'airborneContactSpreadID' ) ) then
            cid := -1
          else
            cid := row.field( 'airborneContactSpreadID' )
          ;

          ptPair.airborne := TAirborneSpreadParams.create(
            db,
            cid,
            _sim,
            srcID,
            destID,
            ( null <> row.field( 'airborneContactSpreadID' ) ) // if not null, populate the object from the database
          );

          self.append( ptPair );

          ptPair.updated := false;

          row := res.fetchArrayNext();
        end
      ;

      res.Free();
    end
  ;


  constructor TProductionTypePairList.create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil );
    var
      i: integer;
      model: pointer;
      nModels: integer;
      modelName: string;
    begin
      inherited create();
      _sim := sim;
      _xmlModelList := nil;

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

  
  destructor TProductionTypePairList.destroy();
    begin
      dbcout( '*** Destroying TProductionTypePairList', DBSHOWMSG );
      // By default, TObjectList owns its objects and will free them.
      
      freeAndNil( _xmlModelList );
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


  procedure TProductionTypePairList.populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType );
    var
      it: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
    begin

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();
          ptp.populateDatabase( db, updateAction );
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
      useContactSpread: boolean;
      useAirborneSpread: boolean;
    begin
      useContactSpread := (_sim as TSMSimulationInput ).includeContactSpreadGlobal;
      useAirborneSpread := (_sim as TSMSimulationInput ).includeAirborneSpreadGlobal;
      result := '';

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();
          if( useContactSpread ) then
            begin
              dbcout( '*** Using contact models in TProductionTypePairList.ssXML', DBSHOWMSG );
              result := result + ptp.directContactXML();
              result := result + ptp.indirectContactXML();
            end
          else
            dbcout( 'Not using contact models in TProductionTypePairList.ssXML', DBSHOWMSG )
          ;

          if( useAirborneSpread ) then
            begin
              dbcout( '*** Using airborne spread models in TProductionTypePairList.ssXML', DBSHOWMSG );
              result := result + ptp.airborneSpreadXML();
            end
          else
            dbcout( 'Not using airborne spread models in TProductionTypePairList.ssXML', DBSHOWMSG )
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
      includeAirborneSpread: boolean;
      includeDirectContactSpread: boolean;
      includeIndirectContactSpread: boolean;

    begin
      result := true;
      includeAirborneSpread := false;
      includeDirectContactSpread := false;
      includeIndirectContactSpread := false;

      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( not( it.current().validate( err ) ) ) then
            result := false
          ;

          if it.current().includeAirborne then includeAirborneSpread := true;
          if it.current().includeDirect then includeDirectContactSpread := true;
          if it.current().includeIndirect then includeIndirectContactSpread := true;

          it.incr();
        end
      ;
      it.free();

      //rbh The following checks were added to address issue 2404

      if (( (_sim as TSMSimulationInput).includeAirborneSpreadGlobal ) and ( not includeAirborneSpread )) then
        begin
          result := false;
          if( nil <> err ) then
            err^ := err^ + tr( 'Airborne disease spread is specified but is not used for any production type.' ) + endl
          ;
        end
      ;

      if (( (_sim as TSMSimulationInput).includeContactSpreadGlobal ) and not ( includeDirectContactSpread or includeIndirectContactSpread )) then
        begin
          result := false;
          if( nil <> err ) then
            err^ := err^ + tr( 'Contact disease spread is specified but is not used for any production type.' ) + endl
          ;
        end
      ;

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
    begin
      it := TProductionTypePairListIterator.create( self );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          it.current().changeChart( whichChart, oldChartName, newChart );
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
          if( ( ptPair.source = ptp.source ) and ( ptPair.dest = ptp.dest ) ) then
            begin
              self.remove( ptPair );
              // FIX ME: should the function stop after removing
              // a single instance, or should it remove all matches?
              // (Right now, it's doing the former.)
              break;
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
// TProductionTypePairList: XML import
//-----------------------------------------------------------------------------
  function TProductionTypePairList.getXmlModelList(): TQStringList;
    var
      list: TQStringList;
    begin
      if( nil = _xmlModelList ) then
        begin
          list := TProductionTypePair.createXmlModelList();
          _xmlModelList := TQStringList.create( list );
          list.Free();
        end
      ;

      result := _xmlModelList;
    end
  ;


  procedure TProductionTypePairList.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      prodTypeSrcName, prodTypeDestName, zoneName, modelName: string;
      prodTypeSrc, prodTypeDest: TProductionType;
      ptList: TProductionTypeList;
      ptp: TProductionTypePair;
      i: integer;
    begin
      // Do some preliminary error checking.
      //------------------------------------
      // Every model that this list will parse must have a "from" type.
      // Some contact models will have a "zone" instead of a "to" type.
      // These will be parsed elsewhere, but it might not hurt to flag the error condition here.

      modelName := sdew.getElementName( model );
      prodTypeSrcName :=  Sdew.GetElementAttribute( model, 'from-production-type' );
      prodTypeDestName := Sdew.GetElementAttribute( model, 'to-production-type');
      zoneName := Sdew.GetElementAttribute( model, 'zone' );

      if( strIsEmpty( prodTypeSrcName ) ) then
        begin
          appendToPString(
            errMsg,
            ansiReplaceStr( tr( 'XML for xyz is missing a source production type.' ), 'xyz', modelName )
          );
          exit;
        end
      ;

      if( ( not strIsEmpty( prodTypeDestName ) ) and ( not strIsEmpty( zoneName ) ) ) then
        begin
          appendToPString(
            errMsg,
            ansiReplaceStr( tr( 'XML for xyz is has both a destination production type and a zone.' ), 'xyz', modelName )
          );
          exit;
        end
      ;

      if( strIsEmpty( prodTypeDestName ) and strIsEmpty( zoneName ) ) then
        begin
          appendToPString(
            errMsg,
            ansiReplaceStr( tr( 'XML for xyz specifies neither a destination production type nor a zone.' ), 'xyz', modelName )
          );
          exit;
        end
      ;

      if( strIsEmpty( prodTypeDestName ) ) then
        begin
          // This is not an error, but this isn't a model that TProductionTypePairList should parse.
          exit;
        end
      ;

      // Find the production type pair to which this model belongs.
      // Create new production types if necessary.
      //-----------------------------------------------------------
      ptList := (sim as TSMSimulationInput).ptList;

      prodTypeSrc := ptList.findProdType( prodTypeSrcName );
      if( nil = prodTypeSrc ) then
        begin
          prodTypeSrc := TProductionType.create( -1, prodTypeSrcName, false, sim );
          ptList.append( prodTypeSrc );
        end
      ;

      prodTypeDest := ptList.findProdType( prodTypeDestName );
      if( nil = prodTypeDest ) then
        begin
          prodTypeDest := TProductionType.create( -1, prodTypeDestName, false, sim );
          ptList.append( prodTypeDest );
        end
      ;

      i := self.pairPosition( prodTypeSrc, prodTypeDest );

      if( -1 = i ) then
        begin
          ptp := TProductionTypePair.create( prodTypeSrc, prodTypeDest, sim );
          self.append( ptp );
        end
      else
        ptp := self.at( i )
      ;

      // If we get this far, we can actually do something about this model.
      //-------------------------------------------------------------------
      ptp.importXml( model, sdew, errMsg );
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
