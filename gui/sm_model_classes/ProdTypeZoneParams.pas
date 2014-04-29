unit ProdTypeZoneParams;

(*
ProdTypeZoneParams.pas
----------------------
Begin: 2006/12/19
Last revision: $Date: 2011-10-19 01:24:13 $ $Author: areeves $
Version number: $Revision: 1.5.6.12 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QLists,
    QIntegerMaps,

    Sdew,
    
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
  type TZoneProdTypeComboParams = class
    protected
      _relDirectMovementName: string;
      _relIndirectMovementName: string;
      _detectionMultiplier: double;
      _useDirectMovementControl: boolean;
      _useIndirectMovementControl: boolean;
      _useDetectionMultiplier: boolean;
      _costSurvPerAnimalDay: double;

      _updated: boolean;

      _sim: TObject;

      procedure initialize( sim: TObject );

      function getFnDictionary(): TFunctionDictionary;

      procedure setRelDirectMovementName( val: string );
      procedure setRelIndirectMovementName( val: string );
      procedure setDetectionMultiplier( val: double );
      procedure setUseDirectMovementControl( val: boolean );
      procedure setUseIndirectMovementControl( val: boolean );
      procedure setUseDetectionMultiplier( val: boolean );
      procedure setCostSurvPerAnimalDay( val: double );

      function getRelDirectMovementName(): string;
      function getRelIndirectMovementName(): string;
      function getDetectionMultiplier(): double;
      function getUseDirectMovementControl(): boolean;
      function getUseIndirectMovementControl(): boolean;
      function getUseDetectionMultiplier(): boolean;
      function getCostSurvPerAnimalDay(): double;

      function getRelDirectMovement(): TRelFunction;
      function getRelIndirectMovement(): TRelFunction;

      function getUpdated(): boolean;
      procedure setUpdated( val: boolean );

      property _fnDictionary: TFunctionDictionary read getFnDictionary;
    public
      constructor create( sim: TObject ); overload;
      constructor create( const src: TZoneProdTypeComboParams; sim: TObject ); overload;
      destructor destroy(); override;

      procedure setChart( whichChart: TSMChart; fn: TChartFunction );
      function chart( whichChart: TSMChart ): TChartFunction;

      function ssXml( const prodTypeDescr: string; const productionTypeID: integer; const zoneDescr: string ): string;

      function functionsAreValid(): boolean;

      function validate( err: PString = nil ): boolean;
      procedure debug();

      property relDirectMovementName: string read getRelDirectMovementName write setRelDirectMovementName;
      property relIndirectMovementName: string read getRelIndirectMovementName write setRelIndirectMovementName;
      property detectionMultiplier: double read getDetectionMultiplier write setDetectionMultiplier;
      property useDirectMovementControl: boolean read getUseDirectMovementControl write setUseDirectMovementControl;
      property useIndirectMovementControl: boolean read getUseIndirectMovementControl write setUseIndirectMovementControl;
      property useDetectionMultiplier: boolean read getUseDetectionMultiplier write setUseDetectionMultiplier;
      property costSurvPerAnimalDay: double read getCostSurvPerAnimalDay write setCostSurvPerAnimalDay;

      property relDirectMovement: TRelFunction read getRelDirectMovement;
      property relIndirectMovement: TRelFunction read getRelIndirectMovement;

      property updated: boolean read getUpdated write setUpdated;
    end
  ;


  (**
    Every production type can have many sets of zone parameters (see TZoneProdTypeComboParams above).
    All of these sets of zone parameters are stored in a dictionary, where the key is the
    zone ID and the value is the set of zone parameters associated with that zone.
  *)
  type TZPTList = class( TQIntegerObjectMap )
    protected
      _sim: TObject;
      _updated: boolean;
      
      function getUpdated(): boolean;

    public
      constructor create( sim: TObject ); overload;
      constructor create( const source: TZPTList; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; const zoneList: TZoneList; sim: TObject ); reintroduce; overload;
      destructor destroy(); override;

      procedure assign( source: TObject; sim: TObject ); reintroduce;
      
      function value( const key: integer ): TZoneProdTypeComboParams;
      procedure insert( const key: integer; const val: TZoneProdTypeComboParams );
      function itemAtIndex( const idx: integer ): TZoneProdTypeComboParams;

      procedure populateDatabase( db: TSMDatabase; ptID: integer );

      function paramsForZone( const zoneID: integer ): TZoneProdTypeComboParams;

      function ssXml( const prodTypeDescr: string; const productionTypeID: integer ): string;

      function validate( err: PString = nil ): boolean;
      procedure debug();

      property  Item[const Key: integer]: TZoneProdTypeComboParams read value write insert; default;

      property updated: boolean read getUpdated;
    end
  ;


  type TZPTListIterator = class( TQIntegerObjectMapIterator )
    public
      function value(): TZoneProdTypeComboParams;
    end
  ;


  (**
    Each production type has some "top level" zone settings, as well as multiple sets of zone parameters.
    This class stores all of this stuff.
  *)
  type TProdTypeZoneParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;
      _zptList: TZPTList;
      
      _prodTypeDescr: string;

      _detectionIsZoneTrigger: boolean;
      _directTraceIsZoneTrigger: boolean;
      _indirectTraceIsZoneTrigger: boolean;

      procedure initialize();

      // XML import
      function getXmlModelList(): TQStringList;
      procedure importDetectionModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      procedure importBasicZoneFocusModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      procedure importTraceBackZoneFocusModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      procedure importContactSpreadModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;

      procedure setDetectionIsZoneTrigger( val: boolean );
      procedure setDirectTraceIsZoneTrigger( val: boolean );
      procedure setIndirectTraceIsZoneTrigger( val: boolean );

      function getDetectionIsZoneTrigger(): boolean;
      function getDirectTraceIsZoneTrigger(): boolean;
      function getIndirectTraceIsZoneTrigger(): boolean;

      function getIsZoneTrigger(): boolean;

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      // Overridden from TModelWithFunctions
      //------------------------------------
      function getChartSet(): TChartSet; override;

    public
      constructor create( sim: TObject; ptDescr: string ); overload;
      //constructor create( db: TSMDatabase; ptID: integer; ptDescr: string; const zoneList: TZoneList ); reintroduce; overload;
      constructor create( db: TSMDatabase; ptID: integer; ptDescr: string; sim: TObject ); overload;
      constructor create( const src: TProdTypeZoneParams; sim: TObject ); overload;
      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXML( const productionTypeID: integer ): string; reintroduce;

      // Overridden from TModelWithFunctions
      //------------------------------------
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      procedure changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      ); override;
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;
      function functionsAreValid(): boolean; override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

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
    Math,
    StrUtils,
    SysUtils,
    Variants,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    SMSimulationInput
  ;

//*******************************************************************************
// TZoneProdTypeComboParams
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TZoneProdTypeComboParams: construction/initialization/destruction
  //-----------------------------------------------------------------------------
    constructor TZoneProdTypeComboParams.create( sim: TObject );
      begin
        inherited create();
        initialize( sim );
      end
    ;


    constructor TZoneProdTypeComboParams.create( const src: TZoneProdTypeComboParams; sim: TObject );
      begin
        inherited create();
        initialize( sim );

        setRelDirectMovementName( src._relDirectMovementName );
        setRelIndirectMovementName( src._relIndirectMovementName );

        _detectionMultiplier := src._detectionMultiplier;
        _useDirectMovementControl := src._useDirectMovementControl;
        _useIndirectMovementControl := src._useIndirectMovementControl;
        _useDetectionMultiplier := src._useDetectionMultiplier;
        _costSurvPerAnimalDay := src._costSurvPerAnimalDay;
      end
    ;

    
    procedure TZoneProdTypeComboParams.initialize( sim: TObject );
      begin
        _sim := sim;

        _detectionMultiplier := 1.0;
        _useDirectMovementControl := false;
        _useIndirectMovementControl := false;
        _useDetectionMultiplier := false;

        _costSurvPerAnimalDay := -1.0;

        _updated := false;
      end
    ;

    destructor TZoneProdTypeComboParams.destroy();
      begin
        // Don't delete the functions, but decrement their reference counters.
        setRelDirectMovementName( '' );
        setRelIndirectMovementName( '' );

        inherited destroy();
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeComboParams: Chart retrieval
  //-----------------------------------------------------------------------------
    function TZoneProdTypeComboParams.getFnDictionary(): TFunctionDictionary;
      begin
        if( nil = _sim ) then
          result := nil
        else
          result := (_sim as TSMSimulationInput).functionDictionary
        ;
      end
    ;


    function TZoneProdTypeComboParams.getRelDirectMovement(): TRelFunction;
      begin
        if( nil = _fnDictionary ) then
          result := nil
        else
          begin
            if( _fnDictionary.contains( _relDirectMovementName ) ) then
              begin
                if( _fnDictionary.value( _relDirectMovementName ).fn is TRelFunction ) then
                  result := _fnDictionary.value( _relDirectMovementName ).fn as TRelFunction
                else
                  begin
                    setRelDirectMovementName( '' );
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


    function TZoneProdTypeComboParams.getRelIndirectMovement(): TRelFunction;
      begin
        if( nil = _fnDictionary ) then
          result := nil
        else
          begin
            if( _fnDictionary.contains( _relIndirectMovementName ) ) then
              begin
                if( _fnDictionary.value( _relIndirectMovementName ).fn is TRelFunction ) then
                  result := _fnDictionary.value( _relIndirectMovementName ).fn as TRelFunction
                else
                  begin
                    setRelIndirectMovementName( '' );
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


    function TZoneProdTypeComboParams.functionsAreValid(): boolean;
      begin
        result := true;

        if( useDirectMovementControl and _fnDictionary.contains( _relDirectMovementName ) ) then
          begin
            if( not( _fnDictionary.value( _relDirectMovementName ).fn is TRelFunction ) ) then
              begin
                setRelDirectMovementName( '' );
                result := false;
              end
            ;
          end
        ;

        if( useIndirectMovementControl and _fnDictionary.contains( _relIndirectMovementName ) ) then
          begin
            if( not( _fnDictionary.value( _relIndirectMovementName ).fn is TRelFunction ) ) then
              begin
                setRelIndirectMovementName( '' );
                result := false;
              end
            ;
          end
        ;
      end
    ;


    function TZoneProdTypeComboParams.chart( whichChart: TSMChart ): TChartFunction;
      begin
        case whichChart of
          ZONMovementDirect: result := getRelDirectMovement();
          ZONMovementIndirect: result := getRelIndirectMovement();
          else
            begin
              raise exception.create( 'Invalid whichChart (' + intToStr( integer( whichChart ) ) + ') in TZoneProdTypeComboParams.chart' );
              result := nil;
            end
          ;
        end;
      end
    ;


    procedure TZoneProdTypeComboParams.setChart( whichChart: TSMChart; fn: TChartFunction );
      var
        newName: string;
      begin
        if( nil = fn ) then
          newName := ''
        else
          newName := fn.name
        ;

        case whichChart of
          ZONMovementDirect: self.relDirectMovementName := newName;
          ZONMovementIndirect: self.relIndirectMovementName := newName;
          else
            raise exception.create( 'Unrecognized whichChart (' + intToStr( integer( whichChart ) ) + ') in TZoneProdTypeComboParams.setChart' )
          ;
        end;

        _updated := true;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeComboParams: XML output
  //-----------------------------------------------------------------------------
    function TZoneProdTypeComboParams.ssXml( const prodTypeDescr: string; const productionTypeID: integer; const zoneDescr: string ): string;
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
              + relDirectMovement.ssXml( 3 )
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
              + relIndirectMovement.ssXml( 3 )
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
  // TZoneProdTypeComboParams: Validation and debugging
  //-----------------------------------------------------------------------------
    function TZoneProdTypeComboParams.validate( err: PString = nil ): boolean;
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
        ( 0.0 > detectionMultiplier )
        then
          begin
            if( nil <> err ) then msg := msg + '      ' + tr( 'Detection multiplier is unspecified.' ) + endl;
            result := false;
          end
        ;

        submsg := '';
        if( useDirectMovementControl ) then
          begin
            if( nil = relDirectMovement ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Effect of zone on direct movement rate is unspecified.' ) + endl;
                result := false;
              end
            else if( not( relDirectMovement.validate( @submsg ) ) ) then
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
            if( nil = relIndirectMovement ) then
              begin
                if( nil <> err ) then msg := msg + '      ' + tr( 'Effect of zone on indirect movement rate is unspecified.' ) + endl;
                result := false;
              end
            else if( not( relIndirectMovement.validate( @submsg ) ) ) then
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


    procedure TZoneProdTypeComboParams.debug();
      begin
        dbcout( '  ================ TZoneProdTypeComboParams.debug', true );
        dbcout( '  Use direct contact control: ' + usBoolToText( useDirectMovementControl ), true );
        dbcout( '  Direct contact control chart: ' + relDirectMovementName, true );
        dbcout( '  Use indirect contact control: ' + usBoolToText( useIndirectMovementControl ), true );
        dbcout( '  Indirect movement control chart: ' + relIndirectMovementName, true );
        dbcout( '  Use detection multiplier: ' + usBoolToText( useDetectionMultiplier ), true );
        dbcout( '  Detection probability multiplier: ' + usFloatToStr( detectionMultiplier ), true );
        dbcout( '  Cost of surveillance per animal day: ' + usFloatToStr( costSurvPerAnimalDay ), true );
        dbcout( '  ================ Done.', true );
        dbcout( '', true );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZoneProdTypeComboParams: Properties
  //-----------------------------------------------------------------------------
    procedure TZoneProdTypeComboParams.setRelDirectMovementName( val: string );
      begin
        val := trim( val );

        // Decrement the reference counter for the old function...
        if( '' <> _relDirectMovementName ) then
          begin
            if( _fnDictionary.contains( _relDirectMovementName ) ) then
              _fnDictionary.value( _relDirectMovementName ).decrRefCounter()
            ;
          end
        ;

        // ...and increment the reference counter for the new function.
        if( '' <> val ) then
          begin
            if( _fnDictionary.contains( val ) ) then
              _fnDictionary.value( val ).incrRefCounter()
            else
              raise exception.create( 'Missing function dictionary item (' + val + ') in TZoneProdTypeComboParams.setRelDirectMovementName' )
            ;
          end
        ;

        _relDirectMovementName := val;
        _updated := true;
      end
    ;


    procedure TZoneProdTypeComboParams.setRelIndirectMovementName( val: string );
      begin
         val := trim( val );

        // Decrement the reference counter for the old function...
        if( '' <> _relIndirectMovementName ) then
          begin
            if( _fnDictionary.contains( _relIndirectMovementName ) ) then
              _fnDictionary.value( _relIndirectMovementName ).decrRefCounter()
            ;
          end
        ;

        // ...and increment the reference counter for the new function.
        if( '' <> val ) then
          begin
            if( _fnDictionary.contains( val ) ) then
              _fnDictionary.value( val ).incrRefCounter()
            else
              raise exception.create( 'Missing function dictionary item (' + val + ') in TZoneProdTypeComboParams.setRelIndirectMovementName' )
            ;
          end
        ;

        _relIndirectMovementName := val;
        _updated := true;
      end
    ;


    procedure TZoneProdTypeComboParams.setDetectionMultiplier( val: double ); begin _detectionMultiplier := val; _updated := true; end;
    procedure TZoneProdTypeComboParams.setUseDirectMovementControl( val: boolean ); begin _useDirectMovementControl := val; _updated := true; end;
    procedure TZoneProdTypeComboParams.setUseIndirectMovementControl( val: boolean ); begin _useIndirectMovementControl := val; _updated := true; end;
    procedure TZoneProdTypeComboParams.setUseDetectionMultiplier( val: boolean ); begin _useDetectionMultiplier := val; _updated := true; end;
    procedure TZoneProdTypeComboParams.setCostSurvPerAnimalDay( val: double ); begin _costSurvPerAnimalDay := val; _updated := true; end;

    function TZoneProdTypeComboParams.getRelDirectMovementName(): string; begin result := _relDirectMovementName; end;
    function TZoneProdTypeComboParams.getRelIndirectMovementName(): string; begin result := _relIndirectMovementName; end;
    function TZoneProdTypeComboParams.getDetectionMultiplier(): double; begin result := _detectionMultiplier; end;
    function TZoneProdTypeComboParams.getUseDirectMovementControl(): boolean; begin result := _useDirectMovementControl; end;
    function TZoneProdTypeComboParams.getUseIndirectMovementControl(): boolean; begin result := _useIndirectMovementControl; end;
    function TZoneProdTypeComboParams.getUseDetectionMultiplier(): boolean; begin result := _useDetectionMultiplier; end;
    function TZoneProdTypeComboParams.getCostSurvPerAnimalDay(): double; begin result := _costSurvPerAnimalDay; end;

    function TZoneProdTypeComboParams.getUpdated(): boolean;
    begin
      result :=
        _updated
      or
        _fnDictionary.functionExistsAndIsUpdated( _relDirectMovementName )
      or
        _fnDictionary.functionExistsAndIsUpdated( _relIndirectMovementName )
      ;
    end
  ;



    procedure TZoneProdTypeComboParams.setUpdated( val: boolean ); begin _updated := val; end;
  //-----------------------------------------------------------------------------
//*******************************************************************************



//*******************************************************************************
// TZPTList
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TZPTList: Construction
  //-----------------------------------------------------------------------------
    constructor TZPTList.create( sim: TObject );
      begin
        inherited create();
        _sim := sim;
      end
    ;


    constructor TZPTList.create( const source: TZPTList; sim: TObject );
      begin
        inherited create();
        assign( source, sim );
        _sim := sim;
      end
    ;


    constructor TZPTList.create( db: TSMDatabase; ptID: integer; const zoneList: TZoneList; sim: TObject );
      var
        i: integer;

        q: string;
        db2: TSqlDatabase;
        res: TSqlResult;
        row: TSqlRow;

        zpt: TZoneProdTypeComboParams;
      begin
        inherited create();

        _sim := sim;

        for i := 0 to zoneList.count - 1 do
          begin
            zpt := TZoneProdTypeComboParams.create( _sim );
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
            zpt := self.value( row.field('zoneID') );

            if( null <> row.field('useDirectMovementControl') ) then
              zpt.useDirectMovementControl := boolean( row.field('useDirectMovementControl') )
            else
              zpt.useDirectMovementControl := false
            ;

            if( null <> row.field('zoneDirectMovementRelID') ) then
              zpt.relDirectMovementName := row.field('zoneDirectMovementName')
            else
              zpt.relDirectMovementName := ''
            ;

            if( null <> row.field('useIndirectMovementControl') ) then
              zpt.useIndirectMovementControl := boolean( row.field('useIndirectMovementControl') )
            else
              zpt.useIndirectMovementControl := false
            ;

            if( null <> row.field('zoneIndirectMovementRelID') ) then
              zpt.relIndirectMovementName := row.field('zoneIndirectMovementName')
            else
              zpt.relIndirectMovementName := ''
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


    destructor TZPTList.destroy();
      begin
        deleteValues();
        inherited destroy();
      end
    ;


    procedure TZPTList.assign( source: TObject; sim: TObject );
      var
        src: TZPTList;
        it: TZPTListIterator;
        newObj: TZoneProdTypeComboParams;
      begin
        src := source as TZPTList;
        it := TZPTListIterator.create( src );

        repeat
          if( nil <> it.value() ) then
            begin
              newObj := TZoneProdTypeComboParams.Create( it.value(), sim );

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
  // TZPTList: Reimplemented functions
  //-----------------------------------------------------------------------------
    function TZPTList.value( const key: integer ): TZoneProdTypeComboParams;
      begin
        result := inherited value( key ) as TZoneProdTypeComboParams;
      end
    ;

    procedure TZPTList.insert( const key: integer; const val: TZoneProdTypeComboParams );
      begin
        inherited insert( key, val );
      end
    ;

    function TZPTList.itemAtIndex( const idx: integer ): TZoneProdTypeComboParams;
      begin
        result := inherited itemAtIndex( idx ) as TZoneProdTypeComboParams;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TZPTList: Database population
  //-----------------------------------------------------------------------------
    procedure TZPTList.populateDatabase( db: TSMDatabase; ptID: integer );
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeComboParams;

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
              zpt := it.value();

              dict['zoneID'] :=  IntToStr( it.key() );
              dict['productionTypeID'] := intToStr( ptID );

              if( nil <> zpt.relDirectMovement ) then
                dict['zoneDirectMovementRelID'] := intToStr( zpt.relDirectMovement.id )
              else
                dict['zoneDirectMovementRelID'] := DATABASE_NULL_VALUE
              ;

              if( nil <> zpt.relIndirectMovement ) then
                dict['zoneIndirectMovementRelID'] := intToStr( zpt.relIndirectMovement.id )
              else
                dict['zoneIndirectMovementRelID'] := DATABASE_NULL_VALUE
              ;

              dict['zoneDetectionMultiplier'] := usFloatToStr( zpt.detectionMultiplier );

              dict['useDirectMovementControl'] := db.sqlBool( zpt.useDirectMovementControl );
              dict['useIndirectMovementControl'] := db.sqlBool( zpt.useIndirectMovementControl );
              dict['useDetectionMultiplier'] := db.sqlBool( zpt.useDetectionMultiplier );

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
    function TZPTList.paramsForZone( const zoneID: integer ): TZoneProdTypeComboParams;
      begin
        if( self.contains( zoneID ) ) then
          result := self.value( zoneID )
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

            result := result + it.value().ssXml( prodTypeDescr, productionTypeID, zone.descr );

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
            if( not( it.value().validate( @submsg ) ) ) then
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
            it.value().debug();

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
                  if( it.value().updated ) then
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
// TZPTListIterator
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TZPTListIterator: Reimplemented functions
  //-----------------------------------------------------------------------------
    function TZPTListIterator.value(): TZoneProdTypeComboParams;
      begin
        result := inherited value() as TZoneProdTypeComboParams;
      end
    ;
  //-----------------------------------------------------------------------------
//*******************************************************************************



//*******************************************************************************
// TProdTypeZoneParams
//*******************************************************************************
  //-----------------------------------------------------------------------------
  // TProdTypeZoneParams: Construction/initialization/destruction
  //-----------------------------------------------------------------------------
    constructor TProdTypeZoneParams.create( sim: TObject; ptDescr: string );
      var
        smSim: TSMSimulationInput;
        i: integer;
        zpt: TZoneProdTypeComboParams;
      begin
        inherited create();
        initialize();
        _sim := sim;

        setProdTypeDescr( ptDescr );

        if( nil <> sim ) then
          begin
            smSim := sim as TSMSimulationInput;

            if( nil <> smSim.zoneList ) then
              begin
                _zptList := TZPTList.create( sim );
                
                for i := 0 to smSim.zoneList.Count - 1 do
                  begin
                    zpt := TZoneProdTypeComboParams.create( sim );
                    _zptList.insert( smSim.zoneList.at(i).id, zpt );
                  end
                ;
              end
            else
              _zptList := nil
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


    constructor TProdTypeZoneParams.create( const src: TProdTypeZoneParams; sim: TObject );
      begin
        inherited create( src );
        initialize();
        _sim := sim;

        _detectionIsZoneTrigger := src._detectionIsZoneTrigger;
        _directTraceIsZoneTrigger := src._directTraceIsZoneTrigger;
        _indirectTraceIsZoneTrigger := src._indirectTraceIsZoneTrigger;

        _prodTypeDescr := src._prodTypeDescr;

        _zptList := TZPTList.create( src._zptList, _sim );

        _updated := src.updated;
      end
    ;


    constructor TProdTypeZoneParams.create( db: TSMDatabase; ptID: integer; ptDescr: string; sim: TObject );
      var
        q: string;
        db2: TSqlDatabase;
        res: TSqlResult;
        row: TSqlRow;

        zoneList: TZoneList;
      begin
        inherited create();
        initialize();

        _sim := sim;
        _prodTypeDescr := ptDescr;

        zoneList := (_sim as TSMSimulationInput).zoneList;

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

        res.Free();

        _zptList := TZPTList.create( db, ptID, zoneList, sim );

        _updated := false;
      end
    ;


    procedure TProdTypeZoneParams.initialize();
      begin
        _sim := nil;
        _xmlModelList := nil;

        _detectionIsZoneTrigger := false;
        _directTraceIsZoneTrigger := false;
        _indirectTraceIsZoneTrigger := false;

        _updated := false;
      end
    ;


    destructor TProdTypeZoneParams.destroy();
      begin
        _zptList.Free();
        freeAndNil( _xmlModelList );
        inherited destroy();
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TProdTypeZoneParams: function handling
  //-----------------------------------------------------------------------------
    function TProdTypeZoneParams.getChartSet(): TChartSet;
      begin
        result := [ ZONMovementDirect .. ZONMovementIndirect ];
      end
    ;

    function TProdTypeZoneParams.functionsAreValid(): boolean;
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeComboParams;
      begin
        result := true;

        it := TZPTListIterator.create( _zptList );

        while( nil <> it.value() ) do
          begin
            zpt := it.value();
            result := zpt.functionsAreValid();

            if( not result ) then
              break
            else
              it.incr()
            ;
          end
        ;

        it.Free();
      end
    ;


    function TProdTypeZoneParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeComboParams;
      begin
        result := false;

        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := it.value();

              if
                ( chartName = zpt.relDirectMovementName )
              or
                ( chartName = zpt.relIndirectMovementName )
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


    procedure TProdTypeZoneParams.removeChart( const chartName: string );
      var
        it: TZPTListIterator;
        zpt: TZoneProdTypeComboParams;
      begin
        it := TZPTListIterator.create( _zptList );

        repeat
          if( nil <> it.value() ) then
            begin
              zpt := it.value();

              if( chartName = zpt.relDirectMovementName ) then zpt.relDirectMovementName := '';
              if( chartName = zpt.relIndirectMovementName ) then zpt.relIndirectMovementName := '';

              it.incr();
            end
          ;
        until ( nil = it.value() );

        it.Free();

        _updated := true;
      end
    ;


    procedure TProdTypeZoneParams.changeChart(
          const whichChart: TSMChart;
          const oldChartName: string;
          newChart: TChartFunction;
          addlInfo: integer = -1
        );
      var
        newName: string;

        it: TZPTListIterator;
        zpt: TZoneProdTypeComboParams;
      begin
        if( nil = newChart ) then
          newName := ''
        else
          newName := newChart.name
        ;

        it := TZPTListIterator.create( _zptList );

        while( ( nil <> it.value() ) ) do
          begin
            zpt := it.value();

            if( oldChartName = zpt.relDirectMovementName ) then zpt.relDirectMovementName := newName;
            if( oldChartName = zpt.relIndirectMovementName ) then zpt.relIndirectMovementName := newName;

            it.incr();
          end
        ;

        it.Free();
      end
    ;



    procedure TProdTypeZoneParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
      var
        newName: string;
      begin
        if( not( whichChart in chartSet ) ) then
          exit
        ;

        if( nil = fn ) then
          newName := ''
        else
          newName := fn.name
        ;

        if( nil <> _zptList[addlInfo] ) then
          begin
            case whichChart of
              ZONMovementDirect: _zptList[addlInfo].relDirectMovementName := newName;
              ZONMovementIndirect: _zptList[addlInfo].relIndirectMovementName := newName;
            end;
          end
        else
          raise exception.create( 'Bad index (addlInfo = ' + intToStr( addlInfo ) + ') in TProdTypeZoneParams.setChart' )
        ;
      end
    ;


    function TProdTypeZoneParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
      begin
        if( nil <> _zptList[addlInfo] ) then
          result := _zptList[addlInfo].chart( whichChart )
        else
          begin
            raise exception.create( 'Bad index (addlInfoaddlInfo = ' + intToStr( addlInfo ) + ') in TProdTypeZoneParams.chart' );
            result := nil;
          end
        ;
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TProdTypeZoneParams: Zone changes
  //-----------------------------------------------------------------------------
  procedure TProdTypeZoneParams.addZone( const zoneID: integer );
    var
      zpt: TZoneProdTypeComboParams;
    begin
      zpt := TZoneProdTypeComboParams.create( _sim );

      _zptList.Insert( zoneID, zpt );
    end
  ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TProdTypeZoneParams: Database population
  //-----------------------------------------------------------------------------
    function TProdTypeZoneParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
      var
        q: string;
        dict: TQueryDictionary;
      begin
        dict := TQueryDictionary.create();

        dict['zoneDetectionIsTrigger'] := db.sqlBool( _detectionIsZoneTrigger );
        dict['zoneDirectTraceIsTrigger'] := db.sqlBool( _directTraceIsZoneTrigger );
        dict['zoneIndirectTraceIsTrigger'] := db.sqlBool( _indirectTraceIsZoneTrigger );

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
  // TProdTypeZoneParams: Debugging and validation
  //-----------------------------------------------------------------------------
    function TProdTypeZoneParams.validate( err: PString = nil ): boolean;
      begin
        // Only the list requires validation.
        result := _zptList.validate( err );
      end
    ;



    procedure TProdTypeZoneParams.debug();
      begin
        dbcout( '========== TProdTypeZoneParams DEBUG', true );
        dbcout( 'Detection is zone trigger: ' + usBoolToText( _detectionIsZoneTrigger ), true );
        dbcout( 'Direct trace is zone trigger: ' + usBoolToText( _directTraceIsZoneTrigger ), true );
        dbcout( 'Indirect trace is zone trigger: ' + usBoolToText( _indirectTraceIsZoneTrigger ), true );
        dbcout( 'Updated: ' + usBoolToText( _updated ), true );
        dbcout( '', true );
        dbcout( 'ZPTList items:', true );
        _zptList.debug();
        dbcout( '========== Done TProdTypeZoneParams', true );
      end
    ;
  //-----------------------------------------------------------------------------



  //-----------------------------------------------------------------------------
  // TProdTypeZoneParams: Properties
  //-----------------------------------------------------------------------------
    function TProdTypeZoneParams.getUpdated(): boolean;
      begin
        result := _updated;

        if( not( result ) ) then
          begin
            if ( _zptList <> nil ) then
              result := _zptList.updated
            ;
          end
        ;
      end
    ;

    procedure TProdTypeZoneParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; _updated := true; end;

    function TProdTypeZoneParams.getProdTypeDescr(): string;
      begin
        if( 0 = length( _prodTypeDescr ) ) then
          raise exception.Create( 'TProdTypeZoneParams._prodTypeDescr is not set' )
        ;
        result := _prodTypeDescr;
      end
    ;

    procedure TProdTypeZoneParams.setDetectionIsZoneTrigger( val: boolean ); begin _detectionIsZoneTrigger := val; _updated := true; end;
    procedure TProdTypeZoneParams.setDirectTraceIsZoneTrigger( val: boolean ); begin _directTraceIsZoneTrigger := val; _updated := true; end;
    procedure TProdTypeZoneParams.setIndirectTraceIsZoneTrigger( val: boolean ); begin _indirectTraceIsZoneTrigger := val; _updated := true; end;

    function TProdTypeZoneParams.getDetectionIsZoneTrigger(): boolean; begin result := _detectionIsZoneTrigger; end;
    function TProdTypeZoneParams.getDirectTraceIsZoneTrigger(): boolean; begin result := _directTraceIsZoneTrigger; end;
    function TProdTypeZoneParams.getIndirectTraceIsZoneTrigger(): boolean; begin result := _indirectTraceIsZoneTrigger; end;

    function TProdTypeZoneParams.getIsZoneTrigger(): boolean;
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

  
  //-----------------------------------------------------------------------------
  // TProdTypeZoneParams: XML output
  //-----------------------------------------------------------------------------
    function TProdTypeZoneParams.ssXML( const productionTypeID: integer ): string;
      begin
        result := '';

        // Start with the zone focus models
        //---------------------------------
        if( detectionIsZoneTrigger ) then
          begin
            result := result +
                '  <basic-zone-focus-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"></basic-zone-focus-model>' + endl + endl
            ;
          end
        ;

        if( directTraceIsZoneTrigger ) then
          begin
            result := result +
                '  <trace-zone-focus-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="direct"></trace-zone-focus-model>' + endl + endl
            ;
          end
        ;

        if( indirectTraceIsZoneTrigger ) then
          begin
            result := result +
                '  <trace-zone-focus-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="indirect"></trace-zone-focus-model>' + endl + endl
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
  // TProdTypeZoneParams: XML import
  //-----------------------------------------------------------------------------
    class function TProdTypeZoneParams.createXmlModelList(): TQStringList;
      begin
        result := TQStringList.create();
        result.Append( 'detection-model' );
        result.Append( 'basic-zone-focus-model' );
        result.Append( 'trace-back-zone-focus-model' );
        result.Append( 'contact-spread-model' );
      end
    ;


    function TProdTypeZoneParams.getXmlModelList(): TQStringList;
      begin
        if( nil = _xmlModelList ) then
          _xmlModelList := createXmlModelList()
        ;

        result := _xmlModelList;
      end
    ;


    procedure TProdTypeZoneParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      begin
        if( 'detection-model' = sdew.GetElementName( model ) ) then
          begin
            // If there is no 'zone' attribute, this model should be parsed by TDetectionParams instead.
            if( strIsEmpty( sdew.GetElementAttribute( model, 'zone' ) ) ) then
              exit
            else
              importDetectionModelXml( model, sdew, errMsg )
            ;
          end
        ;

        if( 'basic-zone-focus-model' = sdew.GetElementName( model ) ) then
          importBasicZoneFocusModelXml( model, sdew, errMsg )
        ;

        if( 'trace-back-zone-focus-model' = sdew.GetElementName( model ) ) then
          importTraceBackZoneFocusModelXml( model, sdew, errMsg )
        ;

        if( 'contact-spread-model' = sdew.GetElementName( model ) ) then
          begin
            // If there is no 'zone' attribute, this model should be parsed by TDetectionParams instead.
            if( strIsEmpty( sdew.GetElementAttribute( model, 'zone' ) ) ) then
              exit
            else
              importContactSpreadModelXml( model, sdew, errMsg )
            ;
          end
        ;
      end
    ;


    procedure TProdTypeZoneParams.importContactSpreadModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      var
        zoneName: string;
        contact: string;
        subElement: pointer;
        rel: TRelFunction;
        zList: TZoneList;
        zone: TZone;
      begin
        zList := (sim as TSMSimulationInput).zoneList;

        // zoneName can't be empty: it's already been checked above.
        zoneName := Sdew.GetElementAttribute( model, 'zone' );

        zone := zList.find( zoneName );
        if( nil = zone ) then
          begin
            appendToPstring( errMsg, tr( 'Contact spread model XML refers to a missing zone.' ) );
            exit;
          end
        ;

        if( nil = _zptList.value( zone.id ) ) then
          begin
            appendToPstring( errMsg, tr( 'Contact spread model XML refers to a missing zone.' ) );
            exit;
          end
        ;

        contact :=  Sdew.GetElementAttribute( model, 'contact-type' );
        if( strIsEmpty( contact ) ) then
          begin
            appendToPstring( errMsg, tr( 'Contact spread model XML does not specify a valid contact type.' ) );
            exit;
          end
        else if( ( 'direct' <> contact ) and ( 'indirect' <> contact ) ) then
          begin
            appendToPstring( errMsg, tr( 'Contact spread model XML does not specify a valid contact type.' ) );
            exit;
          end
        ;


        // Movement control function
        //--------------------------
        subElement := Sdew.GetElementByName( model, 'movement-control' );
        if( nil = subElement ) then
          appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid movement control function.' ) )
        else
          begin
            rel := createRelFromXml( subElement, sdew );

            if( nil = rel ) then
              appendToPString( errMsg, tr( 'Contact spread model XML does not include a valid movement control function.' ) )
            else
              begin
                if( strIsEmpty( rel.name ) ) then
                  rel.name := 'Zone movement control effect - ' + contact + ' - ' + self.prodTypeDescr + ' - ' + ZoneName
                ;

                if( 'direct' = contact ) then
                  begin
                    rel.dbField := word( ZONMovementDirect );
                    _zptList.value( zone.id ).relDirectMovementName := fnDictionary.checkAndInsert( rel );
                    _zptList.value( zone.id ).useDirectMovementControl := true;
                  end
                else if( 'indirect' = contact ) then
                  begin
                    rel.dbField := word( ZONMovementIndirect );
                    _zptList.value( zone.id ).relIndirectMovementName := fnDictionary.checkAndInsert( rel );
                    _zptList.value( zone.id ).useIndirectMovementControl := true;
                  end
                ;
              end
            ;
          end
        ;
      end
    ;


    procedure TProdTypeZoneParams.importTraceBackZoneFocusModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      var
        contact: string;
      begin
        contact :=  Sdew.GetElementAttribute( model, 'contact-type' );

        if( strIsEmpty( contact ) ) then
          appendToPString( errMsg, tr( 'Trace back zone focus XML does not contain a valid contact type.' ) )
        else if( ( 'direct' <> contact ) and ( 'indirect' <> contact ) ) then
          appendToPString( errMsg, tr( 'Trace back zone focus XML does not contain a valid contact type.' ) )
        else if( 'direct' = contact ) then
          self.directTraceIsZoneTrigger := true
        else if( 'indirect' = contact ) then
          self.indirectTraceIsZoneTrigger := true
        else
          raise exception.create( 'There is a problem in TProdTypeZoneParams.importTraceBackZoneFocusModelXml()' )
        ;

      end
    ;


    procedure TProdTypeZoneParams.importBasicZoneFocusModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      begin
        // The mere existence of this model means that detection will trigger a zone.
        self.detectionIsZoneTrigger := true;
      end
    ;

    procedure TProdTypeZoneParams.importDetectionModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      var
        zoneName: string;
        detectionMultiplier: double;
        zList: TZoneList;
        zone: TZone;
      begin
        zList := (sim as TSMSimulationInput).zoneList;

        zoneName := Sdew.GetElementAttribute( model, 'zone' );

        if ( nil = sdew.GetElementByName( model, 'zone-prob-multiplier') ) then
          begin
            appendToPString( errMsg, tr( 'Detection XML does not contain a valid zone multiplier.' ) );
            exit;
          end
        else
          detectionMultiplier := usStrToFloat( sdew.GetElementContents( sdew.GetElementByName( model, 'zone-prob-multiplier') ), NaN )
        ;

        zone := zList.find( zoneName );
        if( nil = zone ) then
          appendToPstring( errMsg, tr( 'Detection XML refers to a missing zone.' ) )
        else if( nil = _zptList.value( zone.id ) ) then
          appendToPstring( errMsg, tr( 'Detection XML refers to a missing zone.' ) )
        else if( isNaN( detectionMultiplier ) ) then
          appendToPString( errMsg, tr( 'Detection XML does not contain a valid zone multiplier.' ) )
        else
          begin
            _zptList.value( zone.id ).detectionMultiplier := detectionMultiplier;
            _zptList.value( zone.id ).useDetectionMultiplier := true;
          end
        ;
      end
    ;
  //-----------------------------------------------------------------------------
//*******************************************************************************

end.