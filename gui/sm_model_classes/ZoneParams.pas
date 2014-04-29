unit ZoneParams;

(*
ZoneParams.pas
--------------
Begin: 2006/12/19
Last revision: $Date: 2008/11/25 22:00:59 $ $Author: areeves $
Version number: $Revision: 1.12 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QIntegerMaps,

    FunctionDictionary,
    SMDatabase,
    Models,
    FunctionEnums,
    RelFunction,
    ChartFunction,
    Zone
  ;

  (**
    Every combination of zone and production type has two relational functions, a detection multiplier,
    and a cost associated with surveillance.
    Instances of this class store this information for each such combination.
  *)
  type TZoneProdTypeParams = class
    protected
      _directMovementName: string;
      _indirectMovementName: string;
      _detectionMultiplier: double;
      _useDirectMovementControl: boolean;
      _useIndirectMovementControl: boolean;
      _useDetectionMultiplier: boolean;
      _costSurvPerAnimalDay: double;

      _updated: boolean;

      _sim: TObject;

      procedure initialize();

      function getFnDictionary(): TFunctionDictionary;

      procedure setDirectMovementName( val: string );
      procedure setIndirectMovementName( val: string );
      procedure setDetectionMultiplier( val: double );
      procedure setUseDirectMovementControl( val: boolean );
      procedure setUseIndirectMovementControl( val: boolean );
      procedure setUseDetectionMultiplier( val: boolean );
      procedure setCostSurvPerAnimalDay( val: double );

      function getDirectMovementName(): string;
      function getIndirectMovementName(): string;
      function getDetectionMultiplier(): double;
      function getUseDirectMovementControl(): boolean;
      function getUseIndirectMovementControl(): boolean;
      function getUseDetectionMultiplier(): boolean;
      function getCostSurvPerAnimalDay(): double;

      function getDirectMovement(): TRelFunction;
      function getIndirectMovement(): TRelFunction;

      function getUpdated(): boolean;
      procedure setUpdated( val: boolean );

      property _fnDictionary: TFunctionDictionary read getFnDictionary;
    public
      constructor create(); overload;
      constructor create( const src: TZoneProdTypeParams ); overload;

      procedure setChart( whichChart: TSMChart; fn: TChartFunction );
      function chart( whichChart: TSMChart ): TChartFunction;

      function ssXml( const prodTypeDescr: string; const productionTypeID: integer; const zoneDescr: string ): string;

      function functionsAreValid(): boolean;

      function validate( err: PString = nil ): boolean;
      procedure debug();

      property directMovementName: string read getDirectMovementName write setDirectMovementName;
      property indirectMovementName: string read getIndirectMovementName write setIndirectMovementName;
      property detectionMultiplier: double read getDetectionMultiplier write setDetectionMultiplier;
      property useDirectMovementControl: boolean read getUseDirectMovementControl write setUseDirectMovementControl;
      property useIndirectMovementControl: boolean read getUseIndirectMovementControl write setUseIndirectMovementControl;
      property useDetectionMultiplier: boolean read getUseDetectionMultiplier write setUseDetectionMultiplier;
      property costSurvPerAnimalDay: double read getCostSurvPerAnimalDay write setCostSurvPerAnimalDay;

      property directMovement: TRelFunction read getDirectMovement;
      property indirectMovement: TRelFunction read getIndirectMovement;

      property updated: boolean read getUpdated write setUpdated;
    end
  ;


  (**
    Every production type can have many sets of zone parameters (see TZoneProdTypeParams above).
    All of these sets of zone parameters are stored in a dictionary, where the key is the
    zone ID and the value is the set of zone parameters associated with that zone.
  *)
  type TZPTList = class( TQIntegerObjectMap )
    protected
      _sim: TObject;
      _updated: boolean;
      
      function getUpdated(): boolean;

    public
      constructor create( const source: TZPTList ); overload;
      constructor create( db: TSMDatabase; ptID: integer; const zoneList: TZoneList ); reintroduce; overload;

      procedure assign( source: TObject ); override;

      procedure populateDatabase( db: TSMDatabase; ptID: integer );

      function paramsForZone( const zoneID: integer ): TZoneProdTypeParams;

      function ssXml( const prodTypeDescr: string; const productionTypeID: integer ): string;

      function validate( err: PString = nil ): boolean;
      procedure debug();

      property updated: boolean read getUpdated;
    end
  ;


  type TZPTListIterator = class( TQIntegerObjectMapIterator )
    end
  ;


  (**
    Each production type has some "top level" zone settings, as well as multiple sets of zone parameters.
    This class stores all of this stuff.
  *)
  type TZoneParams = class( TModelWithFunctions )
    protected
      _zptList: TZPTList;
      
      _prodTypeDescr: string;
      
      _detectionIsZoneTrigger: boolean;
      _directTraceIsZoneTrigger: boolean;
      _indirectTraceIsZoneTrigger: boolean;

      procedure initialize();

      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;

      // Properties overridden from base class
      function getUpdated(): boolean; override;
      procedure setSim( sim: TObject ); override;

      procedure setDetectionIsZoneTrigger( val: boolean );
      procedure setDirectTraceIsZoneTrigger( val: boolean );
      procedure setIndirectTraceIsZoneTrigger( val: boolean );

      function getDetectionIsZoneTrigger(): boolean;
      function getDirectTraceIsZoneTrigger(): boolean;
      function getIndirectTraceIsZoneTrigger(): boolean;

      function getIsZoneTrigger(): boolean;

    public
      constructor create( sim: TObject; ptDescr: string ); overload;
      constructor create( db: TSMDatabase; ptID: integer; ptDescr: string; const zoneList: TZoneList ); reintroduce; overload;
      constructor create( const src: TZoneParams; sim: TObject ); overload;
      destructor destroy(); override;

      function hasChartName( const chartName: string ): boolean;

      // Overridden from TModel
      //-----------------------
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXML( const productionTypeID: integer ): string; reintroduce;

      function functionsAreValid(): boolean; override;

    	// Needed for chart editing
			procedure removeChart( const chartName: string ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      procedure changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      ); override;

      procedure addZone( const zoneID: integer );

      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;

      property detectionIsZoneTrigger: boolean read getDetectionIsZoneTrigger write setDetectionIsZoneTrigger;
      property directTraceIsZoneTrigger: boolean read getDirectTraceIsZoneTrigger write setDirectTraceIsZoneTrigger;
      property indirectTraceIsZoneTrigger: boolean read getIndirectTraceIsZoneTrigger write setIndirectTraceIsZoneTrigger;

      property isZoneTrigger: boolean read getIsZoneTrigger;

      property zonePtParamsList: TZPTList read _zptList;
    end
  ;


implementation

  uses
    StrUtils,
    SysUtils,
    Variants,

    MyStrUtils,
    USStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    SMSimulationInput
  ;

//*******************************************************************************
// TZoneProdTypeParams
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TZoneProdTypeParams: construction/initialization/destruction
  //-----------------------------------------------------------------------------
    constructor TZoneProdTypeParams.create();
      begin
        inherited create();
        initialize();
      end
    ;


    constructor TZoneProdTypeParams.create( const src: TZoneProdTypeParams );
      begin
        inherited create();

        _directMovementName := src._directMovementName;
        _indirectMovementName := src._indirectMovementName;
        _detectionMultiplier := src._detectionMultiplier;
        _useDirectMovementControl := src._useDirectMovementControl;
        _useIndirectMovementControl := src._useIndirectMovementControl;
        _useDetectionMultiplier := src._useDetectionMultiplier;
        _costSurvPerAnimalDay := src._costSurvPerAnimalDay;
      end
    ;

    
    procedure TZoneProdTypeParams.initialize();
      begin
        _directMovementName := '';
        _indirectMovementName := '';
        _detectionMultiplier := 1.0;
        _useDirectMovementControl := false;
        _useIndirectMovementControl := false;
        _useDetectionMultiplier := false;

        _costSurvPerAnimalDay := -1.0;

        _updated := false;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeParams: Chart retrieval
  //-----------------------------------------------------------------------------
    function TZoneProdTypeParams.getFnDictionary(): TFunctionDictionary;
      begin
        if( nil = _sim ) then
          result := nil
        else
          result := (_sim as TSMSimulationInput).functionDictionary
        ;
      end
    ;


    function TZoneProdTypeParams.getDirectMovement(): TRelFunction;
      begin
        if( nil = _fnDictionary ) then
          result := nil
        else
          begin
            if( _fnDictionary.contains( _directMovementName ) ) then
              begin
                if( _fnDictionary.value( _directMovementName ).fn is TRelFunction ) then
                  result := _fnDictionary.value( _directMovementName ).fn as TRelFunction
                else
                  begin
                    setDirectMovementName( '' );
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


    function TZoneProdTypeParams.getIndirectMovement(): TRelFunction;
      begin
        if( nil = _fnDictionary ) then
          result := nil
        else
          begin
            if( _fnDictionary.contains( _indirectMovementName ) ) then
              begin
                if( _fnDictionary.value( _indirectMovementName ).fn is TRelFunction ) then
                  result := _fnDictionary.value( _indirectMovementName ).fn as TRelFunction
                else
                  begin
                    setIndirectMovementName( '' );
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


    function TZoneProdTypeParams.functionsAreValid(): boolean;
      begin
        result := true;

        if( useDirectMovementControl and _fnDictionary.contains( _directMovementName ) ) then
          begin
            if( not( _fnDictionary.value( _directMovementName ).fn is TRelFunction ) ) then
              begin
                setDirectMovementName( '' );
                result := false;
              end
            ;
          end
        ;

        if( useIndirectMovementControl and _fnDictionary.contains( _indirectMovementName ) ) then
          begin
            if( not( _fnDictionary.value( _indirectMovementName ).fn is TRelFunction ) ) then
              begin
                setIndirectMovementName( '' );
                result := false;
              end
            ;
          end
        ;
      end
    ;


    function TZoneProdTypeParams.chart( whichChart: TSMChart ): TChartFunction;
      begin
        case whichChart of
          ZONMovementDirect: result := getDirectMovement();
          ZONMovementIndirect: result := getIndirectMovement();
          else
            begin
              raise exception.create( 'Invalid whichChart (' + intToStr( integer( whichChart ) ) + ') in TZoneProdTypeParams.chart' );
              result := nil;
            end
          ;
        end;
      end
    ;


    procedure TZoneProdTypeParams.setChart( whichChart: TSMChart; fn: TChartFunction );
      var
        newName: string;
      begin
        if( nil = fn ) then
          newName := ''
        else
          newName := fn.name
        ;

        case whichChart of
          ZONMovementDirect: self.directMovementName := newName;
          ZONMovementIndirect: self.indirectMovementName := newName;
          else
            raise exception.create( 'Unrecognized whichChart (' + intToStr( integer( whichChart ) ) + ') in TZoneProdTypeParams.setChart' )
          ;
        end;

        _updated := true;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeParams: XML output
  //-----------------------------------------------------------------------------
    function TZoneProdTypeParams.ssXml( const prodTypeDescr: string; const productionTypeID: integer; const zoneDescr: string ): string;
      begin
        result := '';

        if( useDetectionMultiplier ) then
          begin
            result := result +
                '  <detection-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" zone="' + encodeXml( zoneDescr ) + '">' + endl
              + '    <zone-prob-multiplier>' + usFloatToStr( detectionMultiplier ) + '</zone-prob-multiplier>' + endl
              + '  </detection-model>' + endl + endl
            ;
          end
        else
          begin
            result := result +
              '  <!-- Production type "' + encodeXml( prodTypeDescr ) + '" does not use a detection multiplier for zone "' + encodeXml( zoneDescr ) + '" -->'  + endl + endl
            ;
          end
        ;

        if( useDirectMovementControl ) then
          begin
            result := result +
                '  <contact-spread-model from-production-type="' + encodeXml( prodTypeDescr ) + '" zone="' + encodeXml( zoneDescr ) + '" contact-type="direct">' + endl
              + '    <movement-control>' + endl
              + directMovement.ssXml( 3 )
              + '    </movement-control>' + endl
              + '  </contact-spread-model>' + endl + endl
            ;
          end
        else
          begin
            result := result +
              '  <!-- Production type "' + encodeXml( prodTypeDescr ) + '" does not use direct movement control for zone "' + encodeXml( zoneDescr ) + '" -->' + endl + endl
            ;
          end
        ;

        if( useIndirectMovementControl ) then
          begin
            result := result +
                '  <contact-spread-model from-production-type="' + encodeXml( prodTypeDescr ) + '" zone="' + encodeXml( zoneDescr ) + '" contact-type="indirect">' + endl
              + '    <movement-control>' + endl
              + indirectMovement.ssXml( 3 )
              + '    </movement-control>' + endl
              + '  </contact-spread-model>' + endl + endl
            ;
          end
        else
          begin
            result := result +
              '  <!-- Production type "' + encodeXml( prodTypeDescr ) + '" does not use indirect movement control for zone "' + encodeXml( zoneDescr ) + '" -->' + endl + endl
            ;
          end
        ;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeParams: Validation and debugging
  //-----------------------------------------------------------------------------
    function TZoneProdTypeParams.validate( err: PString = nil ): boolean;
      var
        msg, submsg: string;
        useSurvCosts: boolean;
      begin
        result := true;
        msg := '';

        useSurvCosts := ( _sim as TSMSimulationInput ).costTrackZoneSurveillance;

        if
          ( useDetectionMultiplier )
        and
        ( 0 >= detectionMultiplier )
        then
          begin
            if( nil <> err ) then msg := msg + '      ' + tr( 'Detection multiplier is unspecified.' ) + endl;
            result := false;
          end
        ;

        if( nil <> directMovement ) then
          directMovement.debug()
        ;

        submsg := '';
        if( useDirectMovementControl ) then
          begin
            if( nil = directMovement ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Effect of zone on direct movement rate is unspecified.' ) + endl;
                result := false;
              end
            else if( not( directMovement.validate( @submsg ) ) ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Direct movement rate function is not valid:' ) + submsg + endl;
                result := false;
              end
            ;
          end
        ;

        submsg := '';
        if( useIndirectMovementControl ) then
          begin
            if( nil = indirectMovement ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Effect of zone on indirect movement rate is unspecified.' ) + endl;
                result := false;
              end
            else if( not( indirectMovement.validate( @submsg ) ) ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Indirect movement rate function is not valid:' ) + submsg + endl;
                result := false;
              end
            ;
          end
        ;

        if( useSurvCosts ) then
          begin
            if( 0.0 > _costSurvPerAnimalDay ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Cost of surveillance is unspecified.' ) + endl;
                result := false;
              end
            ;
          end
        ;

        if( nil <> err ) then
          err^ := err^ + msg
        ;
      end
    ;


    procedure TZoneProdTypeParams.debug();
      begin
        dbcout( '  ================ TZoneProdTypeParams.debug', true );
        dbcout( '  Use direct movement control: ' + usBoolToText( useDirectMovementControl ), true );
        dbcout( '  Direct movement control chart: ' + directMovementName, true );
        dbcout( '  Use indirect movement control: ' + usBoolToText( useIndirectMovementControl ), true );
        dbcout( '  Indirect movement control chart: ' + indirectMovementName, true );
        dbcout( '  Use detection multiplier: ' + usBoolToText( useDetectionMultiplier ), true );
        dbcout( '  Detection probability multiplier: ' + usFloatToStr( detectionMultiplier ), true );
        dbcout( '  Cost of surveillance per animal day: ' + usFloatToStr( costSurvPerAnimalDay ), true );
        dbcout( '  ================ Done.', true );
        dbcout( '', true );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeParams: Properties
  //-----------------------------------------------------------------------------
    procedure TZoneProdTypeParams.setDirectMovementName( val: string ); begin _directMovementName := val; _updated := true; end;
    procedure TZoneProdTypeParams.setIndirectMovementName( val: string ); begin _indirectMovementName := val; _updated := true; end;
    procedure TZoneProdTypeParams.setDetectionMultiplier( val: double ); begin _detectionMultiplier := val; _useDetectionMultiplier := true; _updated := true; end;
    procedure TZoneProdTypeParams.setUseDirectMovementControl( val: boolean ); begin _useDirectMovementControl := val; _updated := true; end;
    procedure TZoneProdTypeParams.setUseIndirectMovementControl( val: boolean ); begin _useIndirectMovementControl := val; _updated := true; end;
    procedure TZoneProdTypeParams.setUseDetectionMultiplier( val: boolean ); begin _useDetectionMultiplier := val; _updated := true; end;
    procedure TZoneProdTypeParams.setCostSurvPerAnimalDay( val: double ); begin _costSurvPerAnimalDay := val; _updated := true; end;

    function TZoneProdTypeParams.getDirectMovementName(): string; begin result := _directMovementName; end;
    function TZoneProdTypeParams.getIndirectMovementName(): string; begin result := _indirectMovementName; end;
    function TZoneProdTypeParams.getDetectionMultiplier(): double; begin result := _detectionMultiplier; end;
    function TZoneProdTypeParams.getUseDirectMovementControl(): boolean; begin result := _useDirectMovementControl; end;
    function TZoneProdTypeParams.getUseIndirectMovementControl(): boolean; begin result := _useIndirectMovementControl; end;
    function TZoneProdTypeParams.getUseDetectionMultiplier(): boolean; begin result := _useDetectionMultiplier; end;
    function TZoneProdTypeParams.getCostSurvPerAnimalDay(): double; begin result := _costSurvPerAnimalDay; end;

    function TZoneProdTypeParams.getUpdated(): boolean; begin result := _updated; end;
    procedure TZoneProdTypeParams.setUpdated( val: boolean ); begin _updated := val; end;
  //-----------------------------------------------------------------------------
//*******************************************************************************



//*******************************************************************************
// TZPTList
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TZPTList: Construction
  //-----------------------------------------------------------------------------
    constructor TZPTList.create( const source: TZPTList );
      begin
        inherited create( self );
        assign( source );
      end
    ;


    constructor TZPTList.create( db: TSMDatabase; ptID: integer; const zoneList: TZoneList );
      var
        i: integer;

        q: string;
        db2: TSqlDatabase;
        res: TSqlResult;
        row: TSqlRow;

        zpt: TZoneProdTypeParams;
      begin
        inherited create();

        for i := 0 to zoneList.count - 1 do
          begin
            zpt := TZoneProdTypeParams.create();
            self.insert( zoneList.at( i ).id, zpt );
          end
        ;
        
        db2 := db as TSqlDatabase;

        q := 'SELECT'
          + ' `inZoneProductionTypePair`.`zoneID`,'
          + ' `inZoneProductionTypePair`.`useDirectMovementControl`,'
          + ' `inZoneProductionTypePair`.`zoneDirectMovementRelID`,'
          + ' `directChart`.`chartName` as `zoneDirectMovementName`,'
          + ' `inZoneProductionTypePair`.`useIndirectMovementControl`,'
          + ' `inZoneProductionTypePair`.`zoneIndirectMovementRelID`,'
          + ' `inZoneProductionTypePair`.`useDetectionMultiplier`,'
          + ' `indirectChart`.`chartName` as `zoneIndirectMovementName`,'
          + ' `inZoneProductionTypePair`.`zoneDetectionMultiplier`,'
          + ' `inZoneProductionTypePair`.`costSurvPerAnimalDay`'
          + ' FROM '
          + ' ( ('
          + ' `inZoneProductionTypePair`'
          + ' LEFT OUTER JOIN `inChart` `directChart` ON `inZoneProductionTypePair`.`zoneDirectMovementRelID` = `directChart`.`chartID` )'
          + ' LEFT OUTER JOIN `inChart` `indirectChart` ON `inZoneProductionTypePair`.`zoneIndirectMovementRelID` = `indirectChart`.`chartID` )'
          + ' WHERE `productionTypeID` = ' + intToStr( ptID )
        ;

        res := TSqlResult.create( q, db2 );
        row := res.fetchArrayFirst();

        while( nil <> row ) do
          begin
            zpt := self.value( row.field('zoneID') ) as TZoneProdTypeParams;

            if( null <> row.field('useDirectMovementControl') ) then
              zpt.useDirectMovementControl := boolean( row.field('useDirectMovementControl') )
            else
              zpt.useDirectMovementControl := false
            ;

            if( null <> row.field('zoneDirectMovementRelID') ) then
              zpt.directMovementName := row.field('zoneDirectMovementName')
            else
              zpt.directMovementName := ''
            ;

            if( null <> row.field('useIndirectMovementControl') ) then
              zpt.useIndirectMovementControl := boolean( row.field('useIndirectMovementControl') )
            else
              zpt.useIndirectMovementControl := false
            ;

            if( null <> row.field('zoneIndirectMovementRelID') ) then
              zpt.indirectMovementName := row.field('zoneIndirectMovementName')
            else
              zpt.indirectMovementName := ''
            ;

            if( null <> row.field('useDetectionMultiplier') ) then
              zpt.useDetectionMultiplier := boolean( row.field('useDetectionMultiplier') )
            else
              zpt.useDetectionMultiplier := false
            ;

            if( null <> row.field('zoneDetectionMultiplier') ) then
              zpt.detectionMultiplier := row.field('zoneDetectionMultiplier')
            else
              zpt.detectionMultiplier := 1.0
            ;

            if( null <> row.field('costSurvPerAnimalDay') ) then
              zpt.costSurvPerAnimalDay := row.field('costSurvPerAnimalDay')
            else
              zpt.costSurvPerAnimalDay := -1.0
            ;

            zpt.updated := false;

            row := res.fetchArrayNext();
          end
        ;

        res.free();

        _updated := false;
      end
    ;


    procedure TZPTList.assign( source: TObject );
      var
        src: TZPTList;
        it: TZPTListIterator;
        newObj: TZoneProdTypeParams;
      begin
        src := source as TZPTList;
        it := TZPTListIterator.create( src );

        repeat
          if( nil <> it.value() ) then
            begin
              newObj := TZoneProdTypeParams.Create( it.value() as TZoneProdTypeParams );

              self.insert( it.key(), newObj );

              it.incr();
            end
          ;
        until ( nil = it.value() );

        it.Free();

        _updated := src.updated;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZPTList: Database population
  //-----------------------------------------------------------------------------
    procedure TZPTList.populateDatabase( db: TSMDatabase; ptID: integer );
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeParams;

        q: string;
        dict: TQueryDictionary;
      begin
        q := 'DELETE FROM `inZoneProductionTypePair` WHERE `productionTypeID` = ' + intToStr( ptID );
        db.execute( q );

        dict := TQueryDictionary.create();
        it := TZPTListIterator.create( self );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := it.value() as TZoneProdTypeParams;

              dict['zoneID'] :=  IntToStr( it.key() );
              dict['productionTypeID'] := intToStr( ptID );

              if( nil <> zpt.directMovement ) then
                dict['zoneDirectMovementRelID'] := intToStr( zpt.directMovement.id )
              else
                dict['zoneDirectMovementRelID'] := DATABASE_NULL_VALUE
              ;

              if( nil <> zpt.indirectMovement ) then
                dict['zoneIndirectMovementRelID'] := intToStr( zpt.indirectMovement.id )
              else
                dict['zoneIndirectMovementRelID'] := DATABASE_NULL_VALUE
              ;

              dict['zoneDetectionMultiplier'] := usFloatToStr( zpt.detectionMultiplier );

              dict['useDirectMovementControl'] := usBoolToText( zpt.useDirectMovementControl );
              dict['useIndirectMovementControl'] := usBoolToText( zpt.useIndirectMovementControl );
              dict['useDetectionMultiplier'] := usBoolToText( zpt.useDetectionMultiplier );

              if( 0.0 > zpt.costSurvPerAnimalDay ) then
                dict['costSurvPerAnimalDay'] := DATABASE_NULL_VALUE
              else
                dict['costSurvPerAnimalDay'] := usFloatToStr( zpt.costSurvPerAnimalDay )
              ;

              q := writeQuery(
                'inZoneProductionTypePair',
                QInsert,
                dict
              );

              db.execute( q );

              dict.Clear();

              it.incr();
            end
          ;
        until ( nil = it.value() );

        it.Free();
        dict.Free();
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZPTList: Useful functions
    function TZPTList.paramsForZone( const zoneID: integer ): TZoneProdTypeParams;
      begin
        if( self.contains( zoneID ) ) then
          result := self.value( zoneID ) as TZoneProdTypeParams
        else
          result := nil
        ;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZPTList: XML output
  //-----------------------------------------------------------------------------
    function TZPTList.ssXml( const prodTypeDescr: string; const productionTypeID: integer ): string;
      var
        it: TZPTListIterator;

        smSim: TSMSimulationInput;
        zone: TZone;
      begin
        smSim := _sim as TSMSimulationInput;

        result := '';

        it := TZPTListIterator.create( self );

        while( nil <> it.value() ) do
          begin
            zone := smSim.zoneList.find( it.key() );
            if( nil = zone ) then
              raise exception.Create( 'zone is nil in TZPTList.ssXml' )
            ;

            result := result + (it.value()as TZoneProdTypeParams).ssXml( prodTypeDescr, productionTypeID, zone.descr );

            it.incr();
          end
        ;

        it.Free();
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZPTList: Validation and debugging
  //-----------------------------------------------------------------------------
    function TZPTList.validate( err: PString = nil ): boolean;
      var
        it: TZPTListIterator;
        msg, submsg: string;

        smSim: TSMSimulationInput;
        zone: TZone;
      begin
        smSim := _sim as TSMSimulationInput;

        result := true;
        msg := '';

        it := TZPTListIterator.create( self );

        while( nil <> it.value() ) do
          begin
            submsg := '';
            if( not( (it.value() as TZoneProdTypeParams).validate( @submsg ) ) ) then
              begin
                result := false;

                if( nil <> err ) then
                  begin
                    zone := smSim.zoneList.find( it.key() );
                    if( nil <> zone ) then
                      msg := msg + '    ' + ansiReplaceStr( tr( 'Parameters for zone "xyz":' ), 'xyz', zone.descr ) + endl + submsg
                    else
                      raise exception.Create( 'zone is nil in TZPTList.validate' )
                    ;
                  end
                ;
              end
            ;

            it.incr();
          end
        ;

        it.free();

        if( nil <> err ) then
          err^ := err^ + endl + msg
        ;
      end
    ;


    procedure TZPTList.debug();
      var
        it: TZPTListIterator;
      begin
        it := TZPTListIterator.create( self );

        while( nil <> it.value() ) do
          begin
            dbcout( 'Key: ' + intToStr( it.key() ), true );
            dbcout( 'Value: ' , true );
            (it.value() as TZoneProdTypeParams).debug();

            it.incr();
          end
        ;

        it.free();
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZPTList: Properties
  //-----------------------------------------------------------------------------
    function TZPTList.getUpdated(): boolean;
      var
        it: TZPTListIterator;
      begin
        if( not( _updated ) ) then
          begin
            it := TZPTListIterator.create( self );

            repeat
              if( nil <> it.value() ) then
                begin
                  if( ( it.value()as TZoneProdTypeParams ).updated ) then
                    begin
                      _updated := true;
                      break;
                    end
                  ;

                  it.incr();
                end
              ;
            until ( nil = it.value() );

            it.Free();
          end
        ;

        result := _updated;
      end
    ;
  //-----------------------------------------------------------------------------
//*******************************************************************************



//*******************************************************************************
// TZoneParams
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TZoneParams: Construction/initialization/destruction
  //-----------------------------------------------------------------------------
    constructor TZoneParams.create( sim: TObject; ptDescr: string );
      var
        smSim: TSMSimulationInput;
        i: integer;
        zpt: TZoneProdTypeParams;
      begin
        inherited create();
        initialize();

        setProdTypeDescr( ptDescr );

        if( nil <> sim ) then
          begin
            smSim := sim as TSMSimulationInput;

            if( nil <> smSim.zoneList ) then
              begin
                _zptList := TZPTList.create();

                for i := 0 to smSim.zoneList.Count - 1 do
                  begin
                    zpt := TZoneProdTypeParams.create();
                    _zptList.insert( smSim.zoneList.at(i).id, zpt );
                  end
                ;

                setSim( sim );
              end
            else
              begin
                _zptList := nil;
                _sim := sim;
              end
            ;
          end
        else
          begin
            _zptList := nil;
            _sim := sim;
          end
        ;

        _updated := true;
      end
    ;


    constructor TZoneParams.create( const src: TZoneParams; sim: TObject );
      begin
        inherited create( src );

        _detectionIsZoneTrigger := src._detectionIsZoneTrigger;
        _directTraceIsZoneTrigger := src._directTraceIsZoneTrigger;
        _indirectTraceIsZoneTrigger := src._indirectTraceIsZoneTrigger;

        _prodTypeDescr := src._prodTypeDescr;

        _zptList := TZPTList.create( src._zptList );

        setSim( sim );

        _updated := src.updated;
      end
    ;


    constructor TZoneParams.create( db: TSMDatabase; ptID: integer; ptDescr: string; const zoneList: TZoneList );
      var
        q: string;
        db2: TSqlDatabase;
        res: TSqlResult;
        row: TSqlRow;
      begin
        inherited create();
        initialize();

        db2 := db as TSqlDatabase;

        q := 'SELECT'
          + ' `zoneDetectionIsTrigger`,'
          + ' `zoneDirectTraceIsTrigger`,'
          + ' `zoneIndirectTraceIsTrigger`'
          + ' FROM `inProductionType`'
          + ' WHERE `productionTypeID` = ' + intToStr( ptID )
        ;

        res := TSqlResult.create( q, db2 );
        row := res.fetchArrayFirst();

        if( null <> row.field('zoneDetectionIsTrigger') ) then
          _detectionIsZoneTrigger := boolean( row.field('zoneDetectionIsTrigger') )
        else
          _detectionIsZoneTrigger := false
        ;

        if( null <> row.field('zoneDirectTraceIsTrigger') ) then
          _directTraceIsZoneTrigger := boolean( row.field('zoneDirectTraceIsTrigger') )
        else
          _directTraceIsZoneTrigger := false
        ;

        if( null <> row.field('zoneIndirectTraceIsTrigger') ) then
          _indirectTraceIsZoneTrigger := boolean( row.field('zoneIndirectTraceIsTrigger') )
        else
          _indirectTraceIsZoneTrigger := false
        ;

        _prodTypeDescr := ptDescr;

        res.Free();

        _zptList := TZPTList.create( db, ptID, zoneList );

        _updated := false;
      end
    ;


    procedure TZoneParams.initialize();
      begin
        _sim := nil;

        _detectionIsZoneTrigger := false;
        _directTraceIsZoneTrigger := false;
        _indirectTraceIsZoneTrigger := false;

        _updated := false;
      end
    ;


    destructor TZoneParams.destroy();
      begin
        inherited destroy();
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneParams: function handling
  //-----------------------------------------------------------------------------
    function TZoneParams.functionsAreValid(): boolean;
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeParams;
      begin
        result := true;

        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := (it.value() as TZoneProdTypeParams);
              result := zpt.functionsAreValid();
            end
          ;
          it.incr();
        until ( nil = it.value() );

        it.Free();
      end
    ;


    function TZoneParams.hasChartName( const chartName: string ): boolean;
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeParams;
      begin
        result := false;

        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := (it.value() as TZoneProdTypeParams);

              if
                ( chartName = zpt.directMovementName )
              or
                ( chartName = zpt.indirectMovementName )
              then
                begin
                  result := true;
                  break;
                end
              ;
            end
          ;
          it.incr();
        until ( nil = it.value() );

        it.Free();
      end
    ;


    procedure TZoneParams.setSim( sim: TObject );
      var
        it: TZPTListIterator;
      begin
        _sim := sim;

        _zptList._sim := _sim;

        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              (it.value() as TZoneProdTypeParams)._sim := _sim;
              it.incr();
            end
          ;
        until ( nil = it.value() );

        it.Free();
      end
    ;

    procedure TZoneParams.removeChart( const chartName: string );
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeParams;
      begin
        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := it.value() as TZoneProdTypeParams;

              if( chartName = zpt.directMovementName ) then zpt.directMovementName := '';
              if( chartName = zpt.indirectMovementName ) then zpt.indirectMovementName := '';

              it.incr();
            end
          ;
        until ( nil = it.value() );

        it.Free();

        _updated := true;
      end
    ;


    procedure TZoneParams.changeChart(
          const whichChart: TSMChart;
          const oldChartName: string;
          newChart: TChartFunction;
          addlInfo: integer = -1
        );
      var
        newName: string;

        it: TZPTListIterator;
        zpt: TZoneProdTypeParams;
      begin
        if( nil = newChart ) then
          newName := ''
        else
          newName := newChart.name
        ;

        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := it.value() as TZoneProdTypeParams;

              if( oldChartName = zpt.directMovementName ) then zpt.directMovementName := newName;
              if( oldChartName = zpt.indirectMovementName ) then zpt.indirectMovementName := newName;

              it.incr();
            end
          ;
        until ( nil = it.value() );

        it.Free();
      end
    ;



    procedure TZoneParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
      var
        newName: string;
      begin
        if( nil = fn ) then
          newName := ''
        else
          newName := fn.name
        ;

        if( nil <> _zptList[addlInfo] ) then
          begin
            case whichChart of
              ZONMovementDirect: (_zptList[addlInfo] as TZoneProdTypeParams).directMovementName := newName;
              ZONMovementIndirect: (_zptList[addlInfo] as TZoneProdTypeParams).indirectMovementName := newName;
            end;
          end
        else
          raise exception.create( 'Bad index (addlInfo = ' + intToStr( addlInfo ) + ') in TZoneParams.setChart' )
        ;
      end
    ;


    function TZoneParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
      begin
        if( nil <> _zptList[addlInfo] ) then
          result := (_zptList[addlInfo]as TZoneProdTypeParams).chart( whichChart )
        else
          begin
            raise exception.create( 'Bad index (addlInfoaddlInfo = ' + intToStr( addlInfo ) + ') in TZoneParams.chart' );
            result := nil;
          end
        ;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneParams: Zone changes
  //-----------------------------------------------------------------------------
  procedure TZoneParams.addZone( const zoneID: integer );
    var
      zpt: TZoneProdTypeParams;
    begin
      zpt := TZoneProdTypeParams.create();
      zpt._sim := _sim;

      _zptList.Insert( zoneID, zpt );
    end
  ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneParams: Database population
  //-----------------------------------------------------------------------------
    function TZoneParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
      var
        q: string;
        dict: TQueryDictionary;
      begin
        dict := TQueryDictionary.create();

        dict['zoneDetectionIsTrigger'] := usBoolToText( _detectionIsZoneTrigger );
        dict['zoneDirectTraceIsTrigger'] := usBoolToText( _directTraceIsZoneTrigger );
        dict['zoneIndirectTraceIsTrigger'] := usBoolToText( _indirectTraceIsZoneTrigger );

        q := writeQuery(
          'inProductionType',
          QUpdate,
          dict,
          'WHERE `productionTypeID` = ' + intToStr( ptID )
        );

        result := integer( db.execute( q ) );

        dict.Clear();
        dict.Free();

        _zptList.populateDatabase( db, ptID );

        _updated := false;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneParams: XML output
  //-----------------------------------------------------------------------------
    function TZoneParams.ssXML( const productionTypeID: integer ): string;
      begin
        result := '';

        // Start with the zone focus models
        //---------------------------------
        if( detectionIsZoneTrigger ) then
          begin
            result := result +
                '  <basic-zone-focus-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl
              + '  </basic-zone-focus-model>' + endl + endl
            ;
          end
        ;

        if( directTraceIsZoneTrigger ) then
          begin
            result := result +
                '  <trace-back-zone-focus-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="direct">' + endl
              + '  </trace-back-zone-focus-model>' + endl + endl
            ;
          end
        ;

        if( indirectTraceIsZoneTrigger ) then
          begin
            result := result +
                '  <trace-back-zone-focus-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="indirect">' + endl
              + '  </trace-back-zone-focus-model>' + endl + endl
            ;
          end
        ;

        // Now for each zone,
        // write the detection and movement control models
        //------------------------------------------------
        result := result + _zptList.ssXml( prodTypeDescr, productionTypeID );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneParams: Debugging and validation
  //-----------------------------------------------------------------------------
    function TZoneParams.validate( err: PString = nil ): boolean;
      begin
        // Only the list requires validation.
        result := _zptList.validate( err );
      end
    ;



    procedure TZoneParams.debug();
      begin
        dbcout( '========== TZoneParams DEBUG', true );
        dbcout( 'Detection is zone trigger: ' + usBoolToText( _detectionIsZoneTrigger ), true );
        dbcout( 'Direct trace is zone trigger: ' + usBoolToText( _directTraceIsZoneTrigger ), true );
        dbcout( 'Indirect trace is zone trigger: ' + usBoolToText( _indirectTraceIsZoneTrigger ), true );
        dbcout( 'Updated: ' + usBoolToText( _updated ), true );
        dbcout( '', true );
        dbcout( 'ZPTList items:', true );
        _zptList.debug();
        dbcout( '========== Done TZoneParams', true );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneParams: Properties
  //-----------------------------------------------------------------------------
    function TZoneParams.getUpdated(): boolean;
      begin
        result := _updated;

        if( not( result ) ) then
          if ( _zptList <> nil ) then
            result := _zptList.updated
        ;
      end
    ;

    procedure TZoneParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; _updated := true; end;

    function TZoneParams.getProdTypeDescr(): string;
      begin
        if( 0 = length( _prodTypeDescr ) ) then
          raise exception.Create( 'TZoneParams._prodTypeDescr is not set' )
          //dbcout( 'NOTE: TZoneParams._prodTypeDescr is not set', true )
        ;
        result := _prodTypeDescr;
      end
    ;

    procedure TZoneParams.setDetectionIsZoneTrigger( val: boolean ); begin _detectionIsZoneTrigger := val; _updated := true; end;
    procedure TZoneParams.setDirectTraceIsZoneTrigger( val: boolean ); begin _directTraceIsZoneTrigger := val; _updated := true; end;
    procedure TZoneparams.setIndirectTraceIsZoneTrigger( val: boolean ); begin _indirectTraceIsZoneTrigger := val; _updated := true; end;

    function TZoneParams.getDetectionIsZoneTrigger(): boolean; begin result := _detectionIsZoneTrigger; end;
    function TZoneParams.getDirectTraceIsZoneTrigger(): boolean; begin result := _directTraceIsZoneTrigger; end;
    function TZoneParams.getIndirectTraceIsZoneTrigger(): boolean; begin result := _indirectTraceIsZoneTrigger; end;

    function TZoneParams.getIsZoneTrigger(): boolean;
      begin
        result :=
          _detectionIsZoneTrigger
        or
          _directTraceIsZoneTrigger
        or
          _indirectTraceIsZoneTrigger
        ;
      end
    ;
  //-----------------------------------------------------------------------------
//*******************************************************************************

end.