unit ProductionTypePair;

(*
ProductionTypePair.pas
-----------------------
Begin: 2005/05/03
Last revision: $Date: 2008/11/25 22:00:58 $ $Author: areeves $
Version number: $Revision: 1.39 $
Project: NAADSM and related applications
Website:
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
    ContactModel,
    AirborneSpreadModel,
    ProbDensityFunctions,
    ChartFunction,
    RelFunction,
    FunctionEnums
  ;

	type TProductionTypePair = class( TModelWithFunctions )
		protected
  		_source: TProductionType;
    	_dest: TProductionType;

      _includeDirect: boolean;
      _includeIndirect: boolean;

      _direct: TContactModel;
      _indirect: TContactModel;
      _airborne: TAirborneSpreadModel;

      _isInDB: boolean;
      _added: boolean;
      _removed: boolean;

      function updateDB( db: TSMDatabase ): boolean;

      function getSource(): TProductionType;
      function getDest(): TProductionType;
      procedure setSource( pt: TProductionType );
      procedure setDest( pt: TProductionType );

      procedure setDirect( val: TContactModel );
      procedure setIndirect( val: TContactModel );
      procedure setAirborne( val: TAirborneSpreadModel );
      function getDirect(): TContactModel;
      function getIndirect(): TContactModel;
      function getAirborne(): TAirborneSpreadModel;

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

      procedure setIncludeDirect( val: boolean );
      procedure setIncludeIndirect( val: boolean );


      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
    	constructor create( src, dst: TProductionType; smSim: TObject ); overload;
      constructor create( const src: TProductionTypePair; const ptList: TProductionTypeList; sim: TObject ); overload;
      
      destructor destroy(); override;

      function directContactXML(): string;
      function indirectContactXML(): string;
      function airborneSpreadXML(): string;


      // Overridden from TModel
      //-----------------------
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; update: boolean = false ): integer; reintroduce;

      function functionsAreValid(): boolean; override;      

      // Properties
      //-----------
    	property pairDescr: string read getPairDescr;

      property source: TProductionType read getSource write setSource;
      property dest: TProductionType read getDest write setDest;

      property direct: TContactModel read getDirect write setDirect;
      property indirect: TContactModel read getIndirect write setIndirect;
      property airborne: TAirborneSpreadModel read getAirborne write setAirborne;

      property isInDB: boolean read getIsInDB write setIsInDB;
      property added: boolean read getAdded write setAdded;
      property removed: boolean read getRemoved write setRemoved;

      property includeDirect: boolean read getIncludeDirect write setIncludeDirect;
      property includeIndirect: boolean read getIncludeIndirect write setIncludeIndirect;
      property includeAirborne: boolean read getIncludeAirborne;

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
    USStrUtils,
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
        end;

      (*
      _direct := nil;
      _indirect := nil;
      _airborne := nil;
     *)
     _direct := TContactModel.create( CMDirect, smSim, _dest.productionTypeID, _source.productionTypeID );
     _indirect := TContactModel.create( CMIndirect, smSim, _dest.productionTypeID, _source.productionTypeID );
     _airborne := TAirborneSpreadModel.create( smSim, _dest.productionTypeID, _source.productionTypeID );

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

          if ( src._direct <> nil ) then
            _direct := TContactModel.create( src._direct, _sim )
          else
            _direct := nil;

          if ( src._indirect <> nil ) then
            _indirect := TContactModel.create( src._indirect, _sim )
          else
            _indirect := nil;

          if ( src._airborne <> nil ) then
            _airborne := TAirborneSpreadModel.create( src._airborne, _sim )
          else
            _airborne := nil;

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
          _isInDB := false;
          _added := false;
          _removed := false;
          _updated := false;
        end;
    end
  ;


  destructor TProductionTypePair.destroy();
  	begin
			// Instances of TProductionTypePair don't own any objects: they just hold references.
      // Consequently, nothing should be freed here.
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

  procedure TProductionTypePair.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    begin
      if( nil <> newChart ) then
        begin
          if( oldChartName = _airborne.delayName ) then _airborne.delayName := newChart.name;

          if( oldChartName = _direct.distanceName ) then _direct.distanceName := newChart.name;
          if( oldChartName = _direct.delayName ) then _direct.delayName := newChart.name;
          if( oldChartName = _direct.MovementControlName ) then _direct.MovementControlName := newChart.name;

          if( oldChartName = _indirect.distanceName ) then _indirect.distanceName := newChart.name;
          if( oldChartName = _indirect.delayName ) then _indirect.delayName := newChart.name;
          if( oldChartName = _indirect.MovementControlName ) then _indirect.MovementControlName := newChart.name;
        end
      else
        raise exception.Create( 'fn should not be nil in TProductionTypePair.changeChart' )
      ;

      _updated := true;
    end
  ;

  procedure TProductionTypePair.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        AIRDelay: _airborne.DelayName := newName;
        CMDistanceDirect: _direct.DistanceName := newName;
        CMDelayDirect: _direct.DelayName := newName;
        CMMovementControlDirect: _direct.MovementControlName := newName;
        CMDistanceIndirect: _indirect.DistanceName := newName;
        CMDelayIndirect: _indirect.DelayName := newName;
        CMMovementControlIndirect: _indirect.MovementControlName := newName;
      end;

      _updated := true;
    end
  ;


  function TProductionTypePair.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    var
      ret_val:TChartFunction;
    begin
      ret_val := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            AIRDelay:
              begin
                if ( self.fnDictionary.contains( self._airborne.delayName ) ) then
                  ret_val := self.fnDictionary.value( self._airborne.delayName ).fn;
              end;
          CMDistanceDirect:
              begin
                if ( self.fnDictionary.contains( self._direct.distanceName ) ) then
                  ret_val := self.fnDictionary.value( self._direct.distanceName ).fn;
              end;
          CMDelayDirect:
              begin
                if ( self.fnDictionary.contains( self._direct.delayName ) ) then
                  ret_val := self.fnDictionary.value( self._direct.delayName ).fn;
              end;
          CMMovementControlDirect:
              begin
                if ( self.fnDictionary.contains( self._direct.movementControlName ) ) then
                  ret_val := self.fnDictionary.value( self._direct.movementControlName ).fn;
              end;
          CMDistanceIndirect:
              begin
                if ( self.fnDictionary.contains( self._indirect.distanceName ) ) then
                  ret_val := self.fnDictionary.value( self._indirect.distanceName ).fn;
              end;
          CMDelayIndirect:
              begin
                if ( self.fnDictionary.contains( self._indirect.delayName ) ) then
                  ret_val := self.fnDictionary.value( self._indirect.delayName ).fn;
              end;
          CMMovementControlIndirect:
              begin
                if ( self.fnDictionary.contains( self._indirect.movementControlName ) ) then
                  ret_val := self.fnDictionary.value( self._indirect.movementControlName ).fn;
              end;
          else
            raise exception.create( 'Unrecognized whichChart in TProductionTypePair.chart' )
          ;
          end;


        end
      ;

      result := ret_val;
    end
  ;


  procedure TProductionTypePair.removeChart( const chartName: string );
    begin
      if( chartName = _airborne.delayName ) then _airborne.delayName := '';

      if( chartName = _direct.distanceName ) then _direct.distanceName := '';
      if( chartName = _direct.delayName ) then _direct.delayName := '';
      if( chartName = _direct.MovementControlName ) then _direct.MovementControlName := '';

      if( chartName = _indirect.distanceName ) then _indirect.distanceName := '';
      if( chartName = _indirect.delayName ) then _indirect.delayName := '';
      if( chartName = _indirect.MovementControlName ) then _indirect.MovementControlName := '';

      // The _updated flag will be set by the properties above, if necessary
    end
  ;


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

          dict['useDirectContact'] := boolToStr( _includeDirect );

          if( _includeDirect and ( _direct <> nil ) ) then
          	begin
              _direct.populateDatabase( db, true, QUpdate );
          		dict['directContactSpreadID'] := intToStr( _direct.id );
            end
          else
          	dict['directContactSpreadID'] := 'NULL'
          ;

          dict['useIndirectContact'] := boolToStr( _includeIndirect );

          if(_includeIndirect and ( _indirect <> nil ) ) then
          	begin
            	_indirect.populateDatabase( db, true, QUpdate );
          		dict['indirectContactSpreadID'] := intToStr( _indirect.id );
            end
          else
          	dict['indirectContactSpreadID'] := 'NULL'
          ;

          if( includeAirborne and ( _airborne <> nil ) ) then
          	begin
            	if( DBPRODUCTIONTYPEPAIR ) then _airborne.debug();
          		dict['useAirborneSpread'] := boolToStr( true );
              _airborne.populateDatabase( db, true );
              dict['airborneContactSpreadID'] := intToStr( _airborne.id );
            end
          else
          	begin
          		dict['useAirborneSpread'] := boolToStr( false );
              dict['airborneContactSpreadID'] := DATABASE_NULL_VALUE;
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
          //Remove this pair from the database.....
          q := 'DELETE from inProductionTypePair WHERE `sourceProductionTypeID` =' + intToStr( _source.productionTypeID ) + ' AND `destProductionTypeID` =' + intToStr( _dest.productionTypeID ) + ';';
          dbcout( q, DBPRODUCTIONTYPEPAIR );

          result := db.execute( q );
        end;

    end
  ;



  function TProductionTypePair.populateDatabase( db: TSMDatabase; update: boolean = false ): integer;
  	begin
  		if( update ) then
      	result := integer( updateDB( db ) )
      else
        begin
      	  dbcout( 'FIX ME: Populate the database!!', true );
          result := -1;
        end
      ;

      _updated := false;
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
        directContactXML()
        + indirectContactXML()
        + airborneSpreadXML()
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
            + ' source="' + source.xmlProdTypeDescr + '"'
            + ' dest="' + dest.xmlProdTypeDescr + '" -->'
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
            + ' source="' + source.xmlProdTypeDescr + '"'
            + ' dest="' + dest.xmlProdTypeDescr + '" -->'
            + endl + endl
          ;
        end
      ;
    end
  ;


  function TProductionTypePair.airborneSpreadXML(): string;
    begin
      if( includeAirborne and ( nil <> _airborne ) ) then
        result := _airborne.ssXml() + endl + endl
      else
        begin
          result := '  <!-- Not using airborne spread for'
            + ' source="' + source.xmlProdTypeDescr + '"'
            + ' dest="' + dest.xmlProdTypeDescr + '" -->'
            + endl + endl
          ;
        end
      ;
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
    begin
      result := true;

      includeContactSpread := (_sim as TSMSimulationInput).includeContactSpreadGlobal;
      includeAirborneSpread := (_sim as TSMSimulationInput).includeAirborneSpreadGlobal;

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

  
  procedure TProductionTypePair.setDirect( val: TContactModel );
    begin
      if( nil <> _direct ) then freeAndNil( _direct );
      _direct := val;
      _updated := true;
    end
  ;


  procedure TProductionTypePair.setIndirect( val: TContactModel );
    begin
      if( nil <> _indirect ) then freeAndNil( _indirect );
      _indirect := val;
      _updated := true;
    end
  ;


  procedure TProductionTypePair.setAirborne( val: TAirborneSpreadModel );
    begin
      if( nil <> _airborne ) then freeAndNil( _airborne );
      _airborne := val;
      _updated := true;
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
      if( includeAirborne ) then
        begin
          if( not airborne.functionsAreValid() ) then
            result := false
          ;
        end
      ;
    end
  ;

  function TProductionTypePair.getDirect(): TContactModel; begin Result := _direct; end;
  function TProductionTypePair.getIndirect(): TContactModel; begin Result := _indirect; end;
  function TProductionTypePair.getAirborne(): TAirborneSpreadModel; begin Result := _airborne; end;

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
      	if( airborne.updated ) then result := true
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

end.
