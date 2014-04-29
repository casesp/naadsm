unit ProductionTypePair;

(*
ProductionTypePair.pas
----------------------
Begin: 2005/05/03
Last revision: $Date: 2011-10-19 01:30:22 $ $Author: areeves $
Version number: $Revision: 1.49.6.5 $
Project: NAADSM and related applications
Website:
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Contnrs,

    QLists,

    Sdew,

    Models,
    ModelDatabase,
    ProbDensityFunctions,
    ChartFunction,
    RelFunction,

    ProductionType,
    ProductionTypeList,
    SMDatabase,
    ContactSpreadParams,
    AirborneSpreadParams,
    LocalAreaSpreadParams,
    FunctionEnums
  ;

  type TProductionTypePair = class( TModelWithFunctions )
    protected
      _source: TProductionType;
      _dest: TProductionType;

      _includeDirect: boolean;
      _includeIndirect: boolean;

      _direct: TContactSpreadParams;
      _indirect: TContactSpreadParams;
      _airborne: TAirborneSpreadParams;
      _localArea: TLocalAreaSpreadParams;

      _isInDB: boolean;
      _added: boolean;
      _removed: boolean;

      function updateDB( db: TSMDatabase ): boolean;

      function getSource(): TProductionType;
      function getDest(): TProductionType;
      procedure setSource( pt: TProductionType );
      procedure setDest( pt: TProductionType );

      procedure setDirect( val: TContactSpreadParams );
      procedure setIndirect( val: TContactSpreadParams );
      procedure setAirborne( val: TAirborneSpreadParams );
      procedure setLocalArea( val: TLocalAreaSpreadParams );
      function getDirect(): TContactSpreadParams;
      function getIndirect(): TContactSpreadParams;
      function getAirborne(): TAirborneSpreadParams;
      function getLocalArea(): TLocalAreaSpreadParams;

      function getPairDescr(): string;

      procedure setIsInDB( val: boolean );
      procedure setAdded( val: boolean );
      procedure setRemoved( val: boolean );

      function getIsInDB(): boolean;
      function getAdded(): boolean;
      function getRemoved(): boolean;

      function getIncludeDirect(): boolean;
      function getIncludeIndirect(): boolean;
      function getIncludeAirborne(): boolean;
      function getIncludeLocalArea(): boolean;

      procedure setIncludeDirect( val: boolean );
      procedure setIncludeIndirect( val: boolean );


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      function allCharts(): TChartSet;

    public
      constructor create( src, dst: TProductionType; smSim: TObject ); overload;
      constructor create( const src: TProductionTypePair; const ptList: TProductionTypeList; sim: TObject ); overload;
      
      destructor destroy(); override;

      function directContactXML(): string;
      function indirectContactXML(): string;
      function airborneSpreadXML(): string;
      function localAreaSpreadXml(): string;

      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer; reintroduce;

      // Overridden from TModelWithFunctions
      //------------------------------------
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;
      function functionsAreValid(): boolean; override;      

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

      // Properties
      //-----------
      property pairDescr: string read getPairDescr;

      property source: TProductionType read getSource write setSource;
      property dest: TProductionType read getDest write setDest;

      property direct: TContactSpreadParams read getDirect write setDirect;
      property indirect: TContactSpreadParams read getIndirect write setIndirect;
      property airborne: TAirborneSpreadParams read getAirborne write setAirborne;
      property localArea: TLocalAreaSpreadParams read getLocalArea write setLocalArea;

      property isInDB: boolean read getIsInDB write setIsInDB;
      property added: boolean read getAdded write setAdded;
      property removed: boolean read getRemoved write setRemoved;

      property includeDirect: boolean read getIncludeDirect write setIncludeDirect;
      property includeIndirect: boolean read getIncludeIndirect write setIncludeIndirect;
      property includeAirborne: boolean read getIncludeAirborne;
      property includeLocalArea: boolean read getIncludeLocalArea;

      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      procedure changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      ); override;

    end
  ;


  const
    DBPRODUCTIONTYPEPAIR: boolean = false; // set to true to enable debugging messages for this unit.

implementation

  uses
    SysUtils,
    
    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    SMSimulationInput
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TProductionTypePair.create( src, dst: TProductionType; smSim: TObject );
    begin
      inherited create();


      if ( ( Assigned( src ) ) and ( Assigned( dst ) ) and ( Assigned( smSim ) ) )then
        begin
          _source := src;
          _dest := dst;
          _sim := smSim;
        end
      else
        begin
          _source := nil;
          _dest := nil;
          _sim := nil;
        end
      ;

     _direct := TContactSpreadParams.create( CMDirect, smSim, _dest.productionTypeID, _source.productionTypeID );
     _indirect := TContactSpreadParams.create( CMIndirect, smSim, _dest.productionTypeID, _source.productionTypeID );
     _airborne := TAirborneSpreadParams.create( smSim, _dest.productionTypeID, _source.productionTypeID );
     _localArea := TLocalAreaSpreadParams.create( smSim, _dest.productionTypeID, _source.productionTypeID );

      _includeDirect := false;
      _includeIndirect := false;

      _isInDB := false;
      _added := false;
      _removed := false;

      _updated := false;
    end
  ;


  constructor TProductionTypePair.create( const src: TProductionTypePair; const ptList: TProductionTypeList; sim: TObject );
    begin
      inherited create( src );

      if  ( ( Assigned( src ) ) and ( Assigned( ptList ) ) and ( Assigned( sim ) ) )  then
        begin
          _sim := sim;

          _source :=  ptList.findProdType( src._source.productionTypeID );
          _dest := ptList.findProdType( src._dest.productionTypeID );

          _isInDB := src._isInDB;
          _added := src._added;
          _removed := src._removed;

          if ( nil <> src._direct ) then
            _direct := TContactSpreadParams.create( src._direct, _sim )
          else
            _direct := nil
          ;

          if ( nil <> src._indirect ) then
            _indirect := TContactSpreadParams.create( src._indirect, _sim )
          else
            _indirect := nil
          ;

          if ( nil <> src._airborne ) then
            _airborne := TAirborneSpreadParams.create( src._airborne, _sim )
          else
            _airborne := nil
          ;

          if ( nil <> src._localArea ) then
            _localArea := TLocalAreaSpreadParams.create( src._localArea, _sim )
          else
            _localArea := nil
          ;

          _includeDirect := src._includeDirect;
          _includeIndirect := src._includeIndirect;

          _updated := src._updated;
        end
      else
        begin
          _source := nil;
          _dest := nil;
          _sim := nil;
          _direct := nil;
          _indirect := nil;
          _airborne := nil;
          _localArea := nil;
          _isInDB := false;
          _added := false;
          _removed := false;
          _updated := false;
        end;
    end
  ;


  destructor TProductionTypePair.destroy();
    begin
      _direct.Free();
      _indirect.Free();
      _airborne.Free();
      _localArea.Free();

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Overridden from TModelWithFunctions
//-----------------------------------------------------------------------------
  function TProductionTypePair.allCharts(): TChartSet;
    begin
      result := _direct.chartSet + _indirect.ChartSet;
    end
  ;


  procedure TProductionTypePair.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    begin
      if( not( whichChart in allCharts() ) ) then
        raise exception.Create( 'Unrecognized whichChart ' + intToStr( ord( whichChart ) ) + ' in TProductionTypePair.changeChart()' )
      ;

      _direct.changeChart( whichChart, oldChartName, newChart, addlInfo );
      _indirect.changeChart( whichChart, oldChartName, newChart, addlInfo );

      _updated := true;
    end
  ;

  procedure TProductionTypePair.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    begin
      if( not( whichChart in allCharts() ) ) then
        raise exception.Create( 'Unrecognized whichChart ' + intToStr( ord( whichChart ) ) + ' in TProductionTypePair.setChart()' )
      ;

      _direct.setChart( whichChart, fn, addlInfo );
      _indirect.setChart( whichChart, fn, addlInfo );

      _updated := true;
    end
  ;


  function TProductionTypePair.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      if( whichChart in _direct.chartSet ) then
        result := _direct.chart( whichChart, addlInfo )
      else if( whichChart in _indirect.chartSet ) then
        result := _indirect.chart( whichChart, addlInfo )
      else
        result := nil;
      ;
    end
  ;


  procedure TProductionTypePair.removeChart( const chartName: string );
    begin
      _direct.removeChart( chartName );
      _indirect.removeChart( chartName );

      // The _updated flag will be set by the properties above, if necessary
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Database handling
//-----------------------------------------------------------------------------
  function TProductionTypePair.updateDB( db: TSMDatabase ): boolean;
    var
      q: string;
      dict: TQueryDictionary;
    begin
      if ( not self.removed ) then
        begin
          if ( self.added ) then   //Brand new...
            db.makeProductionTypePair( self.source.productionTypeID, self.dest.productionTypeID )
          ;

          dict := TQueryDictionary.create();

          dict['useDirectContact'] := db.sqlBool( _includeDirect );

          if( _includeDirect and ( _direct <> nil ) ) then
            begin
              _direct.populateDatabase( db, true, QUpdate );
              dict['directContactSpreadID'] := intToStr( _direct.id );
            end
          else
            dict['directContactSpreadID'] := DATABASE_NULL_VALUE
          ;

          dict['useIndirectContact'] := db.sqlBool( _includeIndirect );

          if(_includeIndirect and ( _indirect <> nil ) ) then
            begin
              _indirect.populateDatabase( db, true, QUpdate );
              dict['indirectContactSpreadID'] := intToStr( _indirect.id );
            end
          else
            dict['indirectContactSpreadID'] := DATABASE_NULL_VALUE
          ;

          if( includeAirborne and ( _airborne <> nil ) ) then
            begin
              if( DBPRODUCTIONTYPEPAIR ) then _airborne.debug();
              dict['useAirborneSpread'] := db.sqlBool( true );
              _airborne.populateDatabase( db, true );
              dict['airborneContactSpreadID'] := intToStr( _airborne.id );
            end
          else
            begin
              dict['useAirborneSpread'] := db.sqlBool( false );
              dict['airborneContactSpreadID'] := DATABASE_NULL_VALUE;
            end
          ;

          if( includeLocalArea and ( _localArea <> nil ) ) then
            begin
              if( DBPRODUCTIONTYPEPAIR ) then _localArea.debug();
              dict['useLocalAreaSpread'] := db.sqlBool( true );
              _localArea.populateDatabase( db, true );
              dict['localAreaSpreadID'] := intToStr( _localArea.id );
            end
          else
            begin
              dict['useLocalAreaSpread'] := db.sqlBool( false );
              dict['localAreaSpreadID'] := DATABASE_NULL_VALUE;
            end
          ;


          q := writeQuery(
            'inProductionTypePair',
            QUpdate,
            dict,
            'WHERE `sourceProductionTypeID` = ' + intToStr( _source.productionTypeID ) + ' AND `destProductionTypeID` = ' + intToStr( _dest.productionTypeID )
          );

          dbcout( q, DBPRODUCTIONTYPEPAIR );

          dict.Clear();
          dict.Free();

          result := db.execute( q );
        end
      else
        begin
          // Remove this pair from the database.....
          q := 'DELETE from inProductionTypePair'
            + ' WHERE `sourceProductionTypeID` =' + intToStr( _source.productionTypeID )
            + ' AND `destProductionTypeID` =' + intToStr( _dest.productionTypeID )
          ;
          dbcout( q, DBPRODUCTIONTYPEPAIR );

          result := db.execute( q );
        end
      ;
    end
  ;



  function TProductionTypePair.populateDatabase( db: TSMDatabase; const updateAction: TDBUpdateActionType ): integer;
    begin
      if( MDBAForceInsert = updateAction ) then
        begin
          self.removed := false;
          self.added := true;
        end
      ;

      result := integer( updateDB( db ) );

      _updated := false;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Validation
//-----------------------------------------------------------------------------
  function TProductionTypePair.validate( err: PString = nil ): boolean;
    var
      includeContactSpread: boolean;
      includeAirborneSpread: boolean;
      includeLocalAreaSpread: boolean;
    begin
      result := true;

      includeContactSpread := (_sim as TSMSimulationInput).includeContactSpreadGlobal;
      includeAirborneSpread := (_sim as TSMSimulationInput).includeAirborneSpreadGlobal;
      includeLocalAreaSpread := (_sim as TSMSimulationInput).includeLocalAreaSpreadGlobal;

      if( includeContactSpread ) then
        begin
          if( includeDirect ) then
            begin
              if( nil = _direct ) then
                begin
                  result := false;
                  err^ := err^ + tr( 'Direct contact parameters are not set' );
                end
              else if( not( _direct.validate( err ) ) ) then
                result := false
              ;
            end
          ;

          if( includeIndirect ) then
            begin
              if( nil = _indirect ) then
                begin
                  result := false;
                  err^ := err^ + tr( 'Indirect contact parameters are not set' );
                end
              else if( not( _indirect.validate( err ) ) ) then
                result := false
              ;
            end
          ;
        end
      ;

      if( includeAirborneSpread ) then
        begin
          if( includeAirborne ) then
            begin
              if( nil = _airborne ) then
                begin
                  result := false;
                  err^ := err^ + tr( 'Airborne spread parameters are not set' );
                end
              else if( not( _airborne.validate( err ) ) ) then
                result := false
              ;
            end
          ;
        end
      ;


      if( includeLocalAreaSpread ) then
        begin
          if( includeLocalArea ) then
            begin
              if( nil = _localArea ) then
                begin
                  result := false;
                  err^ := err^ + tr( 'Local area spread parameters are not set' );
                end
              else if( not( _localArea.validate( err ) ) ) then
                result := false
              ;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TProductionTypePair.debug();
    begin
      dbcout( '== SOURCE:' + source.productionTypeDescr + ' DEST:' + dest.productionTypeDescr, true );

      if( includeDirect ) then
        begin
          if( nil <> _direct ) then
            _direct.debug()
          else
            dbcout( 'DIRECT CONTACT INCLUDED BUT NIL', true )
          ;
        end
      else
        dbcout( 'Direct contact is not included', true )
      ;


      if( includeIndirect ) then
        begin
          if( nil <> _indirect ) then
            _indirect.debug()
          else
            dbcout( 'INDIRECT CONTACT INCLUDED BUT NIL', true )
          ;
        end
      else
        dbcout( 'Indirect contact is not included', true )
      ;


      if( includeAirborne ) then
        begin
          if( nil <> _airborne ) then
            _airborne.debug()
          else
            dbcout( 'AIRBORNE SPREAD INCLUDED BUT NIL', true )
          ;
        end
      else
        dbcout( 'Airborne is not included', true )
      ;

      if( includeLocalArea ) then
        begin
          if( nil <> _localArea ) then
            _localArea.debug()
          else
            dbcout( 'LOCAL AREA SPREAD INCLUDED BUT NIL', true )
          ;
        end
      else
        dbcout( 'Local area spread is not included', true )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TProductionTypePair.getPairDescr(): string;
    begin
      result := _source.productionTypeDescr + ' > ' + _dest.productionTypeDescr;
    end
  ;

  function TProductionTypePair.getSource(): TProductionType; begin result := _source; end;
  function TProductionTypePair.getDest(): TProductionType; begin result := _dest; end;
  procedure TProductionTypePair.setSource( pt: TProductionType ); begin _source := pt; _updated := true;  end;
  procedure TProductionTypePair.setDest( pt: TProductionType ); begin _dest := pt; _updated := true;  end;

  procedure TProductionTypePair.setIncludeDirect( val: boolean ); begin _includeDirect := val; _updated := true; end;
  procedure TProductionTypePair.setIncludeIndirect( val: boolean ); begin _includeIndirect := val; _updated := true;  end;
  function TProductionTypePair.getIncludeDirect(): boolean; begin Result := _includeDirect; end;
  function TProductionTypePair.getIncludeIndirect(): boolean; begin Result := _includeIndirect; end;


  function TProductionTypePair.getIncludeAirborne(): boolean;
    begin
      if( nil = _airborne ) then
        result := false
      else
        result := _airborne.useAirborne
      ;
    end
  ;

  function TProductionTypePair.getIncludeLocalArea(): boolean;
    begin
      if( nil = _localArea ) then
        result := false
      else
        result := _localArea.useLocalArea
      ;
    end
  ;
  
  procedure TProductionTypePair.setDirect( val: TContactSpreadParams );
    begin
      if( nil <> _direct ) then freeAndNil( _direct );
      _direct := val;
      _updated := true;
    end
  ;


  procedure TProductionTypePair.setIndirect( val: TContactSpreadParams );
    begin
      if( nil <> _indirect ) then freeAndNil( _indirect );
      _indirect := val;
      _updated := true;
    end
  ;


  procedure TProductionTypePair.setAirborne( val: TAirborneSpreadParams );
    begin
      if( nil <> _airborne ) then freeAndNil( _airborne );
      _airborne := val;
      _updated := true;
    end
  ;

  procedure TProductionTypePair.setLocalArea( val: TLocalAreaSpreadParams );
    begin
      if( nil <> _localArea ) then freeAndNil( _localArea );
      _localArea := val;
      _updated := true;
    end
  ;


  function TProductionTypePair.getDirect(): TContactSpreadParams; begin Result := _direct; end;
  function TProductionTypePair.getIndirect(): TContactSpreadParams; begin Result := _indirect; end;
  function TProductionTypePair.getAirborne(): TAirborneSpreadParams; begin Result := _airborne; end;
  function TProductionTypePair.getLocalArea(): TLocalAreaSpreadParams; begin Result := _localArea; end;

  procedure TProductionTypePair.setIsInDB( val: boolean ); begin _isInDB := val; end;
  procedure TProductionTypePair.setAdded( val: boolean ); begin _added := val; _updated := true;  end;
  procedure TProductionTypePair.setRemoved( val: boolean ); begin _removed := val; _updated := true;  end;

  function TProductionTypePair.getIsInDB(): boolean; begin Result := _isInDB; end;
  function TProductionTypePair.getAdded(): boolean; begin Result := _added; end;
  function TProductionTypePair.getRemoved(): boolean; begin Result := _removed; end;

  
  function TProductionTypePair.getUpdated(): boolean;
    begin
      result := _updated;

      if( nil <> _airborne ) then
        if( _airborne.updated ) then result := true
      ;

      if( nil <> _localArea ) then
        if( _localArea.updated ) then result := true
      ;

      if( ( _includeDirect ) and ( nil <> _direct ) ) then
        if( _direct.updated ) then result := true
      ;

      if( (_includeDirect ) and ( nil <> _indirect ) ) then
        if( _indirect.updated ) then result := true
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Overridden from TModelWithFunctions
//-----------------------------------------------------------------------------
  function TProductionTypePair.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result :=
        direct.hasChartName( chartName, whichChart )
      or
        indirect.hasChartName( chartName, whichChart )
      ;
    end
  ;


  function TProductionTypePair.functionsAreValid(): boolean;
    begin
      result := true;

      if( includeDirect ) then
        begin
          if( not direct.functionsAreValid() ) then
            result := false
          ;
        end
      ;
      if( includeIndirect ) then
        begin
          if( not indirect.functionsAreValid() ) then
            result := false
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML generation
//-----------------------------------------------------------------------------
  // Note: this function is not currently used.
  // XML for individual components is written separately.
  function TProductionTypePair.ssXml(): string;
    begin
      result :=
        directContactXml()
        + indirectContactXml()
        + airborneSpreadXml()
        + localAreaSpreadXml()
      ;
    end
  ;


  function TProductionTypePair.directContactXML(): string;
    begin
      if( includeDirect and ( nil <> _direct ) ) then
        result := _direct.ssXml() + endl + endl
      else
        begin
          result := '  <!-- Not using direct contact for'
            + ' source="' + encodeXml( source.productionTypeDescr ) + '"'
            + ' dest="' + encodeXml( dest.productionTypeDescr ) + '" -->'
            + endl + endl
          ;
        end
      ;
    end
  ;


  function TProductionTypePair.indirectContactXML(): string;
    begin
      if( includeIndirect and ( nil <> _indirect ) ) then
        result := _indirect.ssXml() + endl + endl
      else
        begin
          result := '  <!-- Not using indirect contact for'
            + ' source="' + encodeXml( source.productionTypeDescr ) + '"'
            + ' dest="' + encodeXml( dest.productionTypeDescr ) + '" -->'
            + endl + endl
          ;
        end
      ;
    end
  ;


  function TProductionTypePair.airborneSpreadXml(): string;
    begin
      if( includeAirborne and ( nil <> _airborne ) ) then
        result := _airborne.ssXml() + endl + endl
      else
        begin
          result := '  <!-- Not using airborne spread for'
            + ' source="' + encodeXml( source.productionTypeDescr ) + '"'
            + ' dest="' + encodeXml( dest.productionTypeDescr ) + '" -->'
            + endl + endl
          ;
        end
      ;
    end
  ;


  function TProductionTypePair.localAreaSpreadXml(): string;
    begin
      if( includeLocalArea and ( nil <> _localArea ) ) then
        result := _localArea.ssXml() + endl + endl
      else
        begin
          result := '  <!-- Not using local area spread for'
            + ' source="' + encodeXml( source.productionTypeDescr ) + '"'
            + ' dest="' + encodeXml( dest.productionTypeDescr ) + '" -->'
            + endl + endl
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TProductionTypePair.createXmlModelList(): TQStringList;
    var
      list: TQStringList;
    begin
      result := TQStringList.create();

      list := TContactSpreadParams.createXmlModelList();
      result.merge( list );
      list.Free();

      list := TAirborneSpreadParams.createXmlModelList();
      result.merge( list );
      list.Free();
    end
  ;


  procedure TProductionTypePair.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      contact: string;
    begin
      if
        ( _direct.xmlModelList.contains( sdew.GetElementName( model ) ) )
      or
        ( _indirect.xmlModelList.contains( sdew.GetElementName( model ) ) )
      then
        begin
          contact := sdew.GetElementAttribute( model, 'contact-type' );

          if( 'direct' = contact ) then
            begin
              _direct.importXml( model, sdew, errMsg );
              self.includeDirect := true;
            end
          else if( 'indirect' = contact ) then
            begin
              _indirect.importXml( model, sdew, errMsg );
              self.includeIndirect := true;
            end
          else
            raise exception.create( 'Someone forgot something in TProductionTypePair.importXml' )
          ;
        end
      ;

      if( _airborne.xmlModelList.contains( sdew.GetElementName( model ) ) ) then
        begin
          _airborne.importXml( model, sdew, errMsg );
          _airborne.thisModelIsUsed := true;
        end
      ;

      if( _localArea.xmlModelList.contains( sdew.GetElementName( model ) ) ) then
        begin
          _localArea.importXml( model, sdew, errMsg );
          _localArea.thisModelIsUsed := true;
        end
      ;
      
    end
  ;
//-----------------------------------------------------------------------------
end.

