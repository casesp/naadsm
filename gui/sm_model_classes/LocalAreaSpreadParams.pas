unit LocalAreaSpreadParams;

(*
LocalAreaSpreadParams.pas
-------------------------
Begin: 2009/11/08
Last revision: $Date: 2011-10-04 22:25:11 $ $Author: areeves $
Version number: $Revision: 1.1.6.10 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2009 - 2013 NAADSM Development Team

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    SysUtils,
    Dialogs,

    SMDatabase,
    SqlClasses,
    Models,
    QLists,
    Sdew
  ;


  type TModelType = (
    MTUnspecified,
    MTAirborne,
    MTLocalArea
  );

  function ModelTypeCode( const t: TModelType ): string;

  type TLocalAreaSpreadBase = class( TModel )
    protected
      _xmlModelList: TQStringList;
      _id: integer;
      _modelType: TModelType;
      _thisModelIsUsed: boolean;

      _includeLASSizeAdjustment: boolean;

      //-----------------------------------------------------------------------------
      // These may not be needed, now that I have a class for production type pair.
      //-----------------------------------------------------------------------------
      _fromProdType: string;
      _toProdType: string;
      _fromProdTypeID: integer;
      _toProdTypeID: integer;
      //-----------------------------------------------------------------------------

      _distBetwUnits: double;
      _nInfectiousInSource: integer;
      _nSusceptibleInReceipient: integer;
      _probSpread: double;

      procedure initialize();
      
      // XML import
      function getXmlModelList(): TQStringList;

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      // Properties
      //-----------
      procedure setID( val: integer );
      function getID(): integer;

      //-----------------------------------------------------------------------------
      // These may not be needed, now that I have a class for production type pair.
      //-----------------------------------------------------------------------------
      procedure setFromProdType( val: string );
      procedure setToProdType( val: string );
      procedure setFromProdTypeID( val: integer );
      procedure setToProdTypeID( val: integer );
      function getFromProdType(): string;
      function getToProdType(): string;
      function getFromProdTypeID(): integer;
      function getToProdTypeID(): integer;
      //-----------------------------------------------------------------------------

      function getThisModelIsUsed(): boolean;
      procedure setThisModelIsUsed( val: boolean );

      function getIncludeLASSizeAdjustment(): boolean;
      procedure setIncludeLASSizeAdjustment( val: boolean );

      procedure setDistBetwUnits( val: double );
      procedure setNInfectiousInSource( val: integer );
      procedure setNSusceptibleInReceipient( val: integer );
      procedure setProbSpread( val: double );

      function getDistBetwUnits(): double;
      function getNInfectiousInSource(): integer;
      function getNSusceptibleInReceipient(): integer;
      function getProbSpread(): double;

    public
      constructor create(); overload;
      constructor create( sim: TObject; const toProdTypeID, fromProdTypeID: integer ); overload;
      constructor create( const src: TLocalAreaSpreadBase; sim: TObject ); overload;

      constructor create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      ); overload;

      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      procedure debug(); override;
      function validate( err: PString = nil ): boolean; override;
      function ssXml(): string; override;
      function populateDatabase( db: TSMDatabase; fromPTP: boolean = false ): integer; reintroduce;
      
      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      property id: integer read getID write setID;

      property fromProdType: string read getFromProdType write setFromProdType;
      property toProdType: string read getToProdType write setToProdType;
      property fromProdTypeID: integer read getFromProdTypeID; // write setFromProdTypeID;
      property toProdTypeID: integer read getToProdTypeID; // write setToProdTypeID;

      property distBetwUnits: double read getDistBetwUnits write setDistBetwUnits;
      property nInfectiousInSource: integer read getNInfectiousInSource write setNInfectiousInSource;
      property nSusceptibleInReceipient: integer read getNSusceptibleInReceipient write setNSusceptibleInReceipient;
      property probSpread: double read getProbSpread write setProbSpread;

      property thisModelIsUsed: boolean read getThisModelIsUsed write setThisModelIsUsed;
      property includeLASSizeAdjustment: boolean read getIncludeLASSizeAdjustment write setIncludeLASSizeAdjustment;
      property modelType: TModelType read _modelType;

      property isValid: boolean read getIsValid;
    end
  ;

  
  type TLocalAreaSpreadParams = class( TLocalAreaSpreadBase )
    public
      constructor create(); overload;
      constructor create( sim: TObject; toProdTypeID, fromProdTypeID: integer ); overload;
      constructor create( const src: TLocalAreaSpreadBase; sim: TObject ); overload;

      constructor create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      ); overload;

      property useLocalArea: boolean read getThisModelIsUsed write setThisModelIsUsed;
    end
  ;

implementation

  uses
    StrUtils,
    Variants,
    Math,

    ARMath,
    DebugWindow,
    MyStrUtils,
    I88n,
    RoundToXReplacement_3c,

    SMSimulationInput,
    ProductionType
  ;

  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.


  function ModelTypeCode( const t: TModelType ): string;
    begin
      case t of
        MTUnspecified: result := 'U';
        MTAirborne: result := 'A';
        MTLocalArea: result := 'L';
      end;
    end
  ;

//-----------------------------------------------------------------------------
// TLocalAreaSpreadBase: Construction/initialization/destruction
//-----------------------------------------------------------------------------
  procedure TLocalAreaSpreadBase.initialize();
    begin
      _xmlModelList := nil;
      id := -1;
      _sim := nil;
      _modelType := MTUnspecified;
      _thisModelIsUsed := false;
      _includeLASSizeAdjustment := false;

      _distBetwUnits := -1.0;
      _nInfectiousInSource := -1;
      _nSusceptibleInReceipient := -1;
      _probSpread := -1.0;

      _updated := false;
    end
  ;


  constructor TLocalAreaSpreadBase.create( sim: TObject; const toProdTypeID, fromProdTypeID: integer );
    begin
      inherited create();
      initialize();

      _sim := sim;
      _fromProdTypeID := fromProdTypeID;
      _toProdTypeID := toProdTypeID;

      _updated := true;
    end
  ;


  constructor TLocalAreaSpreadBase.create();
    begin
      inherited create();
      initialize();
    end
  ;


  constructor TLocalAreaSpreadBase.create( const src: TLocalAreaSpreadBase; sim: TObject );
    begin
      inherited create( src );
      
      _xmlModelList := nil;
      
      _sim := sim;

      _id := src._id;

      _fromProdType := src._fromProdType;
      _toProdType := src._toProdType;
      _fromProdTypeID := src._fromProdTypeID;
      _toProdTypeID := src._toProdTypeID;

      _thisModelIsUsed := src._thisModelIsUsed;
      _includeLASSizeAdjustment := src._includeLASSizeAdjustment;

      _distBetwUnits := src._distBetwUnits;
      _nInfectiousInSource := src._nInfectiousInSource;
      _nSusceptibleInReceipient := src._nSusceptibleInReceipient;
      _probSpread := src._probSpread;

      _updated := src._updated;

    end
  ;


  constructor TLocalAreaSpreadBase.create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      );
    var
      q: string;
      res: TSqlResult;
      row: TSqlRow;
      db2: TSqlDatabase;
    begin
      inherited create();
      initialize();

      _sim := sim;

      setToProdTypeID( destID );
      setFromProdTypeID( srcID );

      if( populateFromDB ) then
        begin
          db2 := db as TSqlDatabase;

          q := 'SELECT '
            + ' inDiseaseSpread.lclAirDistanceBetweenUnits,'
            + ' inDiseaseSpread.lclAirSusceptibleInRecipientUnit,'
            + ' inDiseaseSpread.lclAirInfectiousInSourceUnit,'
            + ' inDiseaseSpread.lclAirProbSpread'
            + ' FROM inDiseaseSpread '
            + ' WHERE spreadID = ' + intToStr( modelID )
          ;

          res := TSqlResult.create( q, db2 );

          if( res.numRows = 1 ) then
            begin
              setID( modelID );
              _thisModelIsUsed := true;

              row := res.fetchArrayFirst();

              if( null <> row.field('lclAirDistanceBetweenUnits') ) then
                distBetwUnits := row.field( 'lclAirDistanceBetweenUnits' )
              ;
              if( null <> row.field('lclAirSusceptibleInRecipientUnit') ) then
              nSusceptibleInReceipient := row.field( 'lclAirSusceptibleInRecipientUnit' )
              ;
              if( null <> row.field('lclAirInfectiousInSourceUnit') ) then
                nInfectiousInSource := row.field( 'lclAirInfectiousInSourceUnit' )
              ;
              if( null <> row.field('lclAirProbSpread') ) then
                probSpread := row.field('lclAirProbSpread')
              ;
            end
          else if( 0 = res.numRows ) then
            // do nothing
          else
            raise exception.Create( 'Too many matching rows in TLocalAreaSpreadBase.create' )
          ;

          res.Free();
        end
      ;

      updated := false;
    end
  ;


  destructor TLocalAreaSpreadBase.destroy();
    begin
      freeAndNil( _xmlModelList );
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TLocalAreaSpreadBase: Database functions
//-----------------------------------------------------------------------------
  function TLocalAreaSpreadBase.populateDatabase( db: TSMDatabase; fromPTP: boolean = false ): integer;
    var
      q: string;
      dict: TQueryDictionary;
      ptpField: string;
      dbAction: TQueryType;
    begin
      case _modelType of
        MTUnspecified: raise exception.create( 'Model type is not set in TLocalAreaSpreadBase.populateDatabase' );
        MTAirborne: ptpField := 'airborneContactSpreadID';
        MTLocalArea: ptpField := 'localAreaSpreadID';
      end;

      if( -1 = id ) then
        dbAction := QInsert
      else
        dbAction := QUpdate
      ;

      // See if there's a record of the production type pair.
      // If not, create one.
      if( not fromPTP ) then
        db.makeProductionTypePair( self.fromProdTypeID, self.toProdTypeID )
      ;

      // once the chart is in, add the record for the spread model
      dict := TQueryDictionary.create();

      dict['spreadMethodCode'] := db.sqlQuote( modelTypeCode( _modelType ) );

      dict['lclAirDistanceBetweenUnits'] := usFloatToStr( distBetwUnits );
      dict['lclAirSusceptibleInRecipientUnit'] := intToStr( nSusceptibleInReceipient );
      dict['lclAirInfectiousInSourceUnit'] := intToStr( nInfectiousInSource );
      dict['lclAirProbSpread'] := usFloatToStr( probSpread, 10 ); // special case - calc of K may require a probability smaller than 6 decimals

      if( dbAction = QInsert ) then
        begin
          q := writeQuery( 'inDiseaseSpread', QInsert, dict );
          db.execute( q );
          id := db.lastInsertID();

          if( not fromPTP ) then
            begin
              q := 'UPDATE inProductionTypePair '
                + 'SET ' + ptpField + ' = ' + intToStr( id ) + ' '
                + 'WHERE sourceProductionTypeID = ' + intToStr( self.fromProdTypeID ) + ' '
                + 'AND destProductionTypeID = ' + intToStr( self.toProdTypeID )
              ;
              db.execute( q );
            end
          ;
        end
      else // Update the spread model record
        begin
          q := writeQuery( 'inDiseaseSpread', QUpdate, dict, 'WHERE [spreadID] = ' + intToStr( id ) );
          db.execute( q );
        end
      ;

      dict.Free();

      _updated := false;

      result := id;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TLocalAreaSpreadBase: Other functions overridden from TModel
//-----------------------------------------------------------------------------
  function TLocalAreaSpreadBase.getUpdated(): boolean; begin result := _updated; end;
  
  procedure TLocalAreaSpreadBase.debug();
    var
      msg: string;
    begin
      case _modelType of
        MTUnspecified: msg := '---Begin TLocalAreaSpreadBase debug';
        MTAirborne: msg := '---Begin Airborne (TLocalAreaSpreadBase) debug';
        MTLocalArea:  msg := '---Begin LocalArea (TLocalAreaSpreadBase) debug';
      end;
      
      msg := endl + msg;
      msg := msg + 'FromProdType: ' + fromProdType + ' (type ID ' + intToStr( fromProdTypeID ) + ') ';
      msg := msg + 'ToProdType: ' + toProdType + ' (type ID ' + intToStr( toProdTypeID ) + ')';
      dbcout( msg, true );

      dbcout( 'distBetwUnits: ' + dbStr( _distBetwUnits ), true );
      dbcout( 'nInfectiousInSource: ' + dbStr( _nInfectiousInSource ), true );
      dbcout( 'nSusceptibleInReceipient: ' + dbStr( _nSusceptibleInReceipient ), true );
      dbcout( 'probSpread: ' + dbStr( _probSpread ), true );

      dbcout( '---End debug' + endl, true );
    end
  ;


  function TLocalAreaSpreadBase.validate( err: PString = nil ): boolean;
    var
      modelTitle: string;
      msg: string;
    begin
      case _modelType of
        MTUnspecified:
          begin
            raise exception.create( 'Model type is not set in TLocalAreaSpreadBase.validate' );
            modelTitle := '';
          end
        ;
        MTAirborne: modelTitle := tr( 'Airborne spread from xyz to zyx:' );
        MTLocalArea: modelTitle := tr( 'Local area spread from xyz to zyx:' );
      end;

      // distBetwUnits must be positive
      // nInfectiousInSource and nSusceptibleInReceipient must be positive
      // probSpread must be a probability

      result := true; // until shown otherwise

      if( 0.0 >= distBetwUnits ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Distance between units must be greater than 0 to parameterize this mechanism of spread.' ) + endl;
          result := false;
        end
      ;

      if( 0 >= nInfectiousInSource ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'There must be at least 1 infectious animal in the source unit to parameterize this mechanism of spread.' ) + endl;
          result := false;
        end
      ;

      if( 0 >= nSusceptibleInReceipient ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'There must be at least 1 susceptible animal in the recipient unit to parameterize this mechanism of spread.' ) + endl;
          result := false;
        end
      ;

      {rbh: In the calculation of K, p = 1 results in a math error because ln(0) is undefined and
      p = 0 results in the formula's complete insensitivity to it's other parameters.}
      if( ( 0.0 >= probSpread ) or ( 1.0 <= probSpread ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of disease spread must be between 0 and 1, inclusive.' ) + endl;
          result := false;
        end
      ;

      // Deal with wind direction only for airborne models
      if( MTAirborne = _modelType ) then
        begin
          if( 0 > ( _sim as TSMSimulationInput ).windDirectionStart ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Wind direction start angle is invalid.'  )+ endl;
              result := false;
            end
          ;

          if( 0 > ( _sim as TSMSimulationInput ).windDirectionEnd ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Wind direction end angle is invalid.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( false = result ) and ( nil <> err ) ) then
        begin
          msg := endl + ansiReplaceStr( ansiReplaceStr( modelTitle, 'xyz', fromProdType ), 'zyx', toProdType ) + endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TLocalAreaSpreadBase: Properties
//-----------------------------------------------------------------------------
  procedure TLocalAreaSpreadBase.setID( val: integer ); begin _id := val; _updated := true; end;
  function TLocalAreaSpreadBase.getID(): integer; begin result := _id; _updated := true; end;

  procedure TLocalAreaSpreadBase.setThisModelIsUsed( val: boolean ); begin _thisModelIsUsed := val; _updated := true; end;
  function TLocalAreaSpreadBase.getThisModelIsUsed(): boolean; begin result := _thisModelIsUsed; end;

  procedure TLocalAreaSpreadBase.setIncludeLASSizeAdjustment( val: boolean ); begin _includeLASSizeAdjustment := val; _updated := true; end;
  function TLocalAreaSpreadBase.getIncludeLASSizeAdjustment(): boolean; begin result := _includeLASSizeAdjustment; end;

  procedure TLocalAreaSpreadBase.setDistBetwUnits( val: double ); begin _distBetwUnits := val; end;
  procedure TLocalAreaSpreadBase.setNInfectiousInSource( val: integer ); begin _nInfectiousInSource := val; end;
  procedure TLocalAreaSpreadBase.setNSusceptibleInReceipient( val: integer ); begin _nSusceptibleInReceipient := val; end;
  procedure TLocalAreaSpreadBase.setProbSpread( val: double ); begin _probSpread := val; end;

  function TLocalAreaSpreadBase.getDistBetwUnits(): double; begin Result := _distBetwUnits; end;
  function TLocalAreaSpreadBase.getNInfectiousInSource(): integer; begin Result := _nInfectiousInSource; end;
  function TLocalAreaSpreadBase.getNSusceptibleInReceipient(): integer; begin Result := _nSusceptibleInReceipient; end;
  function TLocalAreaSpreadBase.getProbSpread(): double; begin Result := _probSpread; end;


  //-----------------------------------------------------------------------------
  // These may not be needed, now that I have a class for production type pair.
  //-----------------------------------------------------------------------------
  procedure TLocalAreaSpreadBase.setFromProdType( val: string ); begin _fromProdType := val; _updated := true; end;
  procedure TLocalAreaSpreadBase.setToProdType( val: string ); begin _toProdType := val; _updated := true; end;
  procedure TLocalAreaSpreadBase.setFromProdTypeID( val: integer ); begin _fromProdTypeID := val; _updated := true; end;
  procedure TLocalAreaSpreadBase.setToProdTypeID( val: integer ); begin _toProdTypeID := val; _updated := true; end;


  function TLocalAreaSpreadBase.getFromProdType(): string;
    var
      sim: TSMSimulationInput;
    begin
      if( length( _fromProdType ) = 0 ) then
        begin
          if( nil = _sim ) then
              raise Exception.Create( 'TAirborneSpreadParams simulation is not set' )
          else
            begin
              sim := _sim as TSMSimulationInput;
              _fromProdType := sim.getProdTypeName( getFromProdTypeID() );
            end
          ;
        end
      ;

      result := _fromProdType;
    end
  ;



  function TLocalAreaSpreadBase.getToProdType(): string;
    var
      sim: TSMSimulationInput;
    begin
      if( length( _toProdType ) = 0 ) then
        begin
          if( _sim = nil ) then
              raise Exception.Create( 'TAirborneSpreadParams scenario is not set' )
          else
            begin
              sim := _sim as TSMSimulationInput;
              _toProdType := sim.getProdTypeName( getToProdTypeID() );
            end
          ;
        end
      ;

      result := _toProdType;
    end
  ;
  //-----------------------------------------------------------------------------

  function TLocalAreaSpreadBase.getFromProdTypeID(): integer;
    var
      val: integer;
      parentalUnit: TSMSimulationInput;
    begin
      if( PRODTYPEUNASSIGNED <> _fromProdTypeID ) then
        val := _fromProdTypeID
      else
        begin
          if( _sim = nil ) then
            begin
              raise Exception.Create( 'TAirborneSpreadParams parent is not set' );
              val := PRODTYPEUNASSIGNED;
            end
          else
            begin
              parentalUnit := _sim as TSMSimulationInput;
              val := parentalUnit.getProdTypeID( _fromProdType );
              setFromProdTypeID( val );
            end
          ;
        end
      ;

      if( PRODTYPEIDNOTFOUND = val ) then
        raise Exception.Create( 'Cannot find ptID for "' + _fromProdType +  '" in TAirborneSpreadParams' )
      ;

      Result := val;
    end
  ;


  function TLocalAreaSpreadBase.getToProdTypeID(): integer;
    var
      val: integer;
      parentalUnit: TSMSimulationInput;
    begin
      if( PRODTYPEUNASSIGNED <> _toProdTypeID ) then
        val := _toProdTypeID
      else
        begin
          if( _sim = nil ) then
            begin
              raise Exception.Create( 'TAirborneSpreadParams parent is not set' );
              val := PRODTYPEUNASSIGNED;
            end
          else
            begin
              parentalUnit := _sim as TSMSimulationInput;
              val := parentalUnit.getProdTypeID( _toProdType );
              setToProdTypeID( val );
            end
          ;
        end
      ;

      if( PRODTYPEIDNOTFOUND = val ) then
        raise Exception.Create( 'Cannot find ptID for "' + _toProdType +  '" in TAirborneSpreadParams' )
      ;

      Result := val;
    end
  ;
//-----------------------------------------------------------------------------


//------------------------------------------------------------------------------
// XML Export
//------------------------------------------------------------------------------

function TLocalAreaSpreadBase.ssXml(): string;
    var
      str: string;
      modelTitle: string;
    begin
      case _modelType of
        MTUnspecified:
          begin
            raise exception.create( 'Model type is not set in TLocalAreaSpreadBase.validate' );
            modelTitle := '';
          end
        ;
        MTAirborne: modelTitle := tr( 'airborne-spread-model' );
        MTLocalArea: modelTitle := tr( 'local-area-spread-model' );
      end;

      str := '';

      str := str + '  <' + modelTitle + ' from-production-type = "' + encodeXml( fromProdType )
        + '" to-production-type = "' + encodeXml( toProdType )
        + '">' + endl
      ;

      if( ( sim as TSMSimulationInput ).includeLASSizeAdjustmentGlobal ) then
        begin
          str := str + '      <herd-size-infectious>' + intToStr( nInfectiousInSource ) + '</herd-size-infectious>' + endl;
          str := str + '      <herd-size-susceptible>' + intToStr( nSusceptibleInReceipient ) + '</herd-size-susceptible>' + endl;
        end
      else
        str := str + '      <!-- Size adjustment for local-area and airborne spread is not used -->' + endl
      ;

      str := str + '      <herd-distance>' + endl;
      str := str + '        <value>' + usFloatToStr( distBetwUnits ) + '</value>' + endl;
      str := str + '        <units><xdf:unit>km</xdf:unit></units>' + endl;
      str := str + '      </herd-distance>' + endl;

      //rbh special case - calc of K may require a probability smaller than 6 decimals (if so, it will be in exponential format)
      str := str + '      <prob>' + usFloatToStr( probSpread, 10 ) + '</prob>' + endl;

      if( MTAirborne = _modelType ) then
        begin
          str := str + '      <wind-direction-start>' + endl;
          str := str + '        <value>' + intToStr( ( _sim as TSMSimulationInput ).windDirectionStart ) + '</value>' + endl;
          str := str + '        <units><xdf:unit>degree</xdf:unit></units>' + endl;
          str := str + '      </wind-direction-start>' + endl;
          str := str + '      <wind-direction-end>' + endl;
          str := str + '        <value>' + intToStr( ( _sim as TSMSimulationInput ).windDirectionEnd ) + '</value>' + endl;
          str := str + '        <units><xdf:unit>degree</xdf:unit></units>' + endl;
          str := str + '      </wind-direction-end>' + endl;
        end
      ;

      str := str + '  </' + modelTitle + '>' + endl;

      result := str;
    end
  ;

//------------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// TLocalAreaSpreadBase: XML import
//-----------------------------------------------------------------------------
  class function TLocalAreaSpreadBase.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'local-area-spread-model' );
      result.Append( 'airborne-spread-model' );
    end
  ;


  function TLocalAreaSpreadBase.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TLocalAreaSpreadBase.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      subElement: pointer;
      val: double;
      modelName: string;
      
    begin
      // from-production-type and to-production-type values are taken care of by ProductionTypePair

      modelName := 'Unidentified model';
      _modelType := MTUnspecified;

      // Model type and name
      //--------------------
      if( 'airborne-spread-model' = sdew.GetElementName( model ) ) then
        begin
          _modelType := MTAirborne;
          modelName := 'Airborne spread model';
        end
      else // local-area-spread-model
        begin
          _modelType := MTLocalArea;
          modelName := 'Local-area spread model';
        end
      ;

      // Herd size infectious
      //----------------------
      subElement := Sdew.GetElementByName( model, 'herd-size-infectious' );
      if ( nil = subElement ) then
        begin
          // This is not an error in NAADSM 4.1.
          // XML without herd-size-infectious or herd-size-susceptible do not use size to adjust probability of spread.

          //appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for N infectious in source herd.' ) );
        end
      else
        begin
          val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN);
          if( isNaN( val ) ) then
            appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for N infectious in source herd.' ) )
          else
            begin
              self.nInfectiousInSource := RoundDbl( val );
              _includeLASSizeAdjustment := true;
            end
          ;
        end
      ;

      // Herd size susceptible
      //----------------------
      subElement := Sdew.GetElementByName( model, 'herd-size-susceptible' );
      if ( nil = subElement ) then
        begin
          // This is not an error in NAADSM 4.1.
          // XML without herd-size-infectious or herd-size-susceptible do not use size to adjust probability of spread.

          //appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for N susceptible in recipient herd.' ) )
        end
      else
        begin
          val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN);
          if( isNaN( val ) ) then
            appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for N susceptible in recipient herd.' ) )
          else
            begin
              self.nSusceptibleInReceipient := RoundDbl( val );
              _includeLASSizeAdjustment := true;
            end
          ;
        end
      ;

      // Distance between infectious and susceptible herd
      //-------------------------------------------------
       subElement := Sdew.GetElementByName( model, 'herd-distance' );
      if ( nil = subElement ) then
        appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for herd source-recipient distance.' ) )
      else
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );
          if ( nil = subElement ) then
            appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for herd source-recipient distance.' ) )
          else
            begin
              val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
              if( isNaN( val ) ) then
                appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for herd source-recipient distance.' ) )
              else
                //  Units are present in the xml file, but not stored anywhere in the current schema.
                self.distBetwUnits := val
              ;
            end
          ;
        end
      ;

      // Probability of disease spread
      //-------------------------------
      subElement := Sdew.GetElementByName( model, 'prob' );
      if( nil = subElement ) then
          appendToPString( errMsg, tr( modelName + ' XML does not contain valid values for probability of spread between infectious and susceptible herd' ) )
      else
        begin
          val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN );
          if( not isProbability( val ) ) then
             appendToPString( errMsg, tr( modelName + ' XML does not contain valid value for probability of spread between infectious and susceptible herd' ) )
          else
            self.probSpread := val
          ;
        end
      ;

      // Wind directions
      //-----------------
      if( MTAirborne = _modelType ) then
        begin

          subElement := Sdew.GetElementByName( model, 'wind-direction-start' );
          if ( nil = subElement ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid value for wind direction.' ) )
          else
            begin
              subElement := Sdew.GetElementByName( subElement, 'value' );
              if ( nil = subElement ) then
                appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid value for wind direction.' ) )
              else
                begin
                  val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN);
                  if( isNaN( val ) ) then
                    appendToPString( errMsg, tr( modelName + ' Airborne spread model XML does not contain valid value for wind direction.' ) )
                  else
                    (_sim as TSMSimulationInput).windDirectionStart := RoundDbl( val )
                  ;
                end
              ;
            end
          ;

          subElement := Sdew.GetElementByName( model, 'wind-direction-end' );
          if ( nil = subElement ) then
            appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid value for wind direction.' ) )
          else
            begin
              subElement := Sdew.GetElementByName( subElement, 'value' );
              if ( nil = subElement ) then
                appendToPString( errMsg, tr( 'Airborne spread model XML does not contain valid value for wind direction.' ) )
              else
                begin
                  val := usStrToFloat( Sdew.GetElementContents( subElement ), NaN);
                  if( isNaN( val ) ) then
                    appendToPString( errMsg, tr( modelName + ' Airborne spread model XML does not contain valid value for wind direction.' ) )
                  else
                    (_sim as TSMSimulationInput).windDirectionEnd := RoundDbl( val )
                  ;
                end
              ;
            end
          ;
        end // if model is airborne
      ;

    end
  ;
//-----------------------------------------------------------------------------




//-----------------------------------------------------------------------------
// TLocalAreaSpreadParams: Construction
//-----------------------------------------------------------------------------
  constructor TLocalAreaSpreadParams.create();
    begin
      inherited create();
      _modelType := MTLocalArea;
    end
  ;


  constructor TLocalAreaSpreadParams.create( sim: TObject; toProdTypeID, fromProdTypeID: integer );
    begin
      inherited create( sim, toProdTypeID, fromProdTypeID );
      _modelType := MTLocalArea;
      //rbh20110614 Fix Me! This hack addresses a problem caused by TLocalAreaSpreadBase.createXmlModelList()
      // Having both local-area and airborne models in the list creates BOTH models when importing airborne only.
      self.xmlModelList.clear;
      self.xmlModelList.Append( 'local-area-spread-model' );
    end
  ;


  constructor TLocalAreaSpreadParams.create( const src: TLocalAreaSpreadBase; sim: TObject );
    begin
      inherited create( src, sim );
      _modelType := MTLocalArea;
    end
  ;


  constructor TLocalAreaSpreadParams.create(
        db: TSMDatabase;
        modelID: integer;
        sim: TObject;
        srcID: integer;
        destID: integer;
        populateFromDB: boolean
      );
    begin
      inherited create( db, modelID, sim, srcID, destID, populateFromDB );
      _modelType := MTLocalArea;
    end
  ;

//-----------------------------------------------------------------------------






end.