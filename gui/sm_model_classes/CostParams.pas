unit CostParams;

(*
CostParams.pas
--------------
Begin: 2005/12/15
Last revision: $Date: 2011-10-14 15:38:09 $ $Author: areeves $
Version number: $Revision: 1.30.2.17 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    SMDatabase,
    SMSimOutByProdType,
    Models
  ;

  type TCostApplicationCode =
    (
      caUnspecified,            // Error catcher
      caAlwaysApply,            // A - always applied
      caNeverApply,             // N - never applied
      caOnlyApplyAfterQueDestr  // D - applied only after queuing for destruction
    )
  ;

  function costApplicationCodeFromString( val: string ): TCostApplicationCode;
  function costApplicationCodeString( val: TCostApplicationCode ): string;

  type TCostParams = class( TModel )
    protected
      _prodTypeDescr: string;

      _destrAppraisalPerUnit: double;
      _destrCleaningPerUnit: double;
      _destrEuthanasiaPerAnimal: double;
      _destrIndemnificationPerAnimal: double;
      _destrDisposalPerAnimal: double;
      _vaccSetupPerUnit: double;
      _vaccThreshold: integer;
      _vaccBaselinePerAnimal: double;
      _vaccAdditionalPerAnimal: double;
      _destrAppraiseDeadUnitsCode: TCostApplicationCode;
      _destrCleanDeadUnitsCode: TCostApplicationCode;
      _destrIndemnifyDeadUnitsCode: TCostApplicationCode;
      _destrDisposeDeadUnitsCode: TCostApplicationCode;

      procedure initialize();

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;


      procedure setProdTypeDescr( val: string );
      procedure setDestrAppraisalPerUnit( val: double );
      procedure setDestrCleaningPerUnit( val: double );
      procedure setDestrEuthanasiaPerAnimal( val: double );
      procedure setDestrIndemnificationPerAnimal( val: double );
      procedure setDestrDisposalPerAnimal( val: double );
      procedure setVaccSetupPerUnit( val: double );
      procedure setVaccThreshold( val: integer );
      procedure setVaccBaselinePerAnimal( val: double );
      procedure setVaccAdditionalPerAnimal( val: double );
      procedure setDestrAppraiseDeadUnitsCode ( val: TCostApplicationCode );
      procedure setDestrCleanDeadUnitsCode ( val: TCostApplicationCode );
      procedure setDestrIndemnifyDeadUnitsCode ( val: TCostApplicationCode );
      procedure setDestrDisposeDeadUnitsCode ( val: TCostApplicationCode );

      function getProdTypeDescr(): string;
      function getDestrAppraisalPerUnit(): double;
      function getDestrCleaningPerUnit(): double;
      function getDestrEuthanasiaPerAnimal(): double;
      function getDestrIndemnificationPerAnimal(): double;
      function getDestrDisposalPerAnimal(): double;
      function getVaccSetupPerUnit(): double;
      function getVaccThreshold(): integer;
      function getVaccBaselinePerAnimal(): double;
      function getVaccAdditionalPerAnimal(): double;
      function getDestrAppraiseDeadUnitsCode(): TCostApplicationCode;
      function getDestrCleanDeadUnitsCode(): TCostApplicationCode;
      function getDestrIndemnifyDeadUnitsCode(): TCostApplicationCode;
      function getDestrDisposeDeadUnitsCode(): TCostApplicationCode;

    public
      constructor create( sim: TObject; prodTypeName: string ); overload;
      constructor create( db: TSMDatabase; ptID: integer; ptDescr: string; sim: TObject ); overload;
      constructor create( const src: TCostParams; sim: TObject ); overload;
      destructor destroy(); override;


      // Overridden from TModel
      //-----------------------
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
			function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;
      function ssXML( const useDestruction, useVaccination: boolean; const productionTypeID: integer ): string; reintroduce;

      function validateDestr( err: PString = nil ): boolean;
      function validateVacc( err: PString = nil ): boolean;
      function validateTracing( err: PString = nil ): boolean;

      procedure insertDatabaseOutputs(
        outputs: TSMDailyOutput;
      	db: TSMDatabase;
        ptID: integer;
        includeDestructionCosts: boolean;
        includeVaccinationCosts: boolean;
        iteration: integer
      );

      // Cost calculations
      //------------------
      function destrAppraisalCosts( const unitsDestroyed, unitsDcdOnly, unitsDcdInDestrQueue: longint ): double;
      function destrCleaningCosts( const unitsDestroyed, unitsDcdOnly, unitsDcdInDestrQueue: longint ): double;
      function destrEuthanasiaCosts( const animalsDestroyed, animalsDcdOnly: longint ): double;
      function destrIndemnificationCosts( const animalsDestroyed, animalsDcdOnly, animalsDcdInDestrQueue: longint ): double;
      function destrDisposalCosts( const animalsDestroyed, animalsDcdOnly, animalsDcdInDestrQueue: longint ): double;

      function destrTotalCosts(
        const unitsDestroyed, animalsDestroyed: longint;
        const unitsDcdOnly, animalsDcdOnly: longint;
        const unitsDcdInDestrQueue, animalsDcdInDestrQueue: longint
      ): double;  

      function vaccSetupCosts( const unitsVaccinated: longint ): double;

      function vaccVaccinationDailyCosts(
        const animalsToBeVaccinated: integer;
        var animalsAlreadyVaccinated: longint
      ): double;


      function vaccTotalVaccinationCosts( const animalsVaccinated: longint ): double;

      function vaccTotalCosts( const unitsVaccinated, animalsVaccinated: longint ): double;

      // Properties
      //-----------
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property destrAppraisalPerUnit: double read getDestrAppraisalPerUnit write setDestrAppraisalPerUnit;
      property destrCleaningPerUnit: double read getDestrCleaningPerUnit write setDestrCleaningPerUnit;
      property destrEuthanasiaPerAnimal: double read getDestrEuthanasiaPerAnimal write setDestrEuthanasiaPerAnimal;
      property destrIndemnificationPerAnimal: double read getDestrIndemnificationPerAnimal write setDestrIndemnificationPerAnimal;
      property destrDisposalPerAnimal: double read getDestrDisposalPerAnimal write setDestrDisposalPerAnimal;
      property vaccSetupPerUnit: double read getVaccSetupPerUnit write setVaccSetupPerUnit;
      property vaccThreshold: integer read getVaccThreshold write setVaccThreshold;
      property vaccBaselinePerAnimal: double read getVaccBaselinePerAnimal write setVaccBaselinePerAnimal;
      property vaccAdditionalPerAnimal: double read getVaccAdditionalPerAnimal write setVaccAdditionalPerAnimal;
      property destrAppraiseDeadUnitsCode: TCostApplicationCode read getDestrAppraiseDeadUnitsCode write setDestrAppraiseDeadUnitsCode;
      property destrCleanDeadUnitsCode: TCostApplicationCode read getDestrCleanDeadUnitsCode write setDestrCleanDeadUnitsCode;
      property destrIndemnificationDeadUnitsCode: TCostApplicationCode read getDestrIndemnifyDeadUnitsCode write setDestrIndemnifyDeadUnitsCode;
      property destrDisposeDeadUnitsCode: TCostApplicationCode read getDestrDisposeDeadUnitsCode write setDestrDisposeDeadUnitsCode;
    end
  ;

  const
    DBFORMCOSTPARAMS: boolean = false; // Set to true to enable debugging messages for this unit.

implementation

  uses
    Variants,
    SysUtils,
    StrUtils,
    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    SMSimulationInput,
    RemoteDatabaseParams
  ;

  const DBSHOWMSG = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Global helper functions
//-----------------------------------------------------------------------------
  function costApplicationCodeFromString( val: string ): TCostApplicationCode;
    var
      s: string;
    begin
      s := upper( charAt( val, 0 ) );

      if( 'A' = s ) then
        result := caAlwaysApply
      else if( 'N' = s ) then
        result := caNeverApply
      else if( 'D' = s ) then
        result := caOnlyApplyAfterQueDestr
      else
        begin
          raise exception.create( 'Unsupported string (' + s + ') in costApplicationCodeFromString()' );
          result := caUnspecified;
        end
      ;
    end
  ;


  function costApplicationCodeString( val: TCostApplicationCode ): string;
    begin
      case val of
        caAlwaysApply: result := 'A';
        caNeverApply: result := 'N';
        caOnlyApplyAfterQueDestr: result := 'D';
        else
          begin
            raise exception.create( 'Unsupported value (' + intToStr( integer( val ) ) + ') in costApplicationCodeString()' );
            result := '';
          end
        ;
      end;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TCostParams.create( sim: TObject; prodTypeName: string );
    begin
      inherited create();
      _sim := sim;

      _prodTypeDescr := prodTypeName;
    end
  ;


  constructor TCostParams.create( const src: TCostParams; sim: TObject );
    begin
      inherited create( src );
      _sim := sim;

      _destrAppraisalPerUnit := src._destrAppraisalPerUnit;
      _destrCleaningPerUnit := src._destrCleaningPerUnit;
      _destrEuthanasiaPerAnimal := src._destrEuthanasiaPerAnimal;
      _destrIndemnificationPerAnimal := src._destrIndemnificationPerAnimal;
      _destrDisposalPerAnimal := src._destrDisposalPerAnimal;
      _vaccSetupPerUnit := src._vaccSetupPerUnit;
      _vaccThreshold := src._vaccThreshold;
      _vaccBaselinePerAnimal := src._vaccBaselinePerAnimal;
      _vaccAdditionalPerAnimal := src._vaccAdditionalPerAnimal;
      _destrAppraiseDeadUnitsCode := src._destrAppraiseDeadUnitsCode;
      _destrCleanDeadUnitsCode := src._destrCleanDeadUnitsCode;
      _destrIndemnifyDeadUnitsCode := src._destrIndemnifyDeadUnitsCode;
      _destrDisposeDeadUnitsCode := src._destrDisposeDeadUnitsCode;

      _prodTypeDescr := src._prodTypeDescr;
      _updated := src._updated;
    end
  ;


  constructor TCostParams.create( db: TSMDatabase; ptID: integer; ptDescr: string; sim: TObject );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      inherited create();
      initialize();

      _sim := sim;
      _prodTypeDescr := ptDescr;

      db2 := db as TSqlDatabase;

      q := 'SELECT'
        +  '`costDestrAppraisalPerUnit`,'
        +  '`costDestrCleaningPerUnit`,'
        +  '`costDestrEuthanasiaPerAnimal`,'
        +  '`costDestrIndemnificationPerAnimal`,'
        +  '`costDestrDisposalPerAnimal`,'
        +  '`costVaccSetupPerUnit`,'
        +  '`costVaccThreshold`,'
        +  '`costVaccBaselinePerAnimal`,'
        +  '`costVaccAdditionalPerAnimal`,'
        +  '`costDestrAppraiseDeadUnitsCode`,'
        +  '`costDestrCleanDeadUnitsCode`,'
        +  '`costDestrIndemnifyDeadUnitsCode`,'
        +  '`costDestrDisposeDeadUnitsCode`'
        + ' FROM `inProductionType`'
        + ' WHERE `productionTypeID` = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('costDestrAppraisalPerUnit') ) then destrAppraisalPerUnit := row.field('costDestrAppraisalPerUnit');
      if( null <> row.field('costDestrCleaningPerUnit') ) then destrCleaningPerUnit := row.field('costDestrCleaningPerUnit');
      if( null <> row.field('costDestrEuthanasiaPerAnimal') ) then destrEuthanasiaPerAnimal := row.field('costDestrEuthanasiaPerAnimal');
      if( null <> row.field('costDestrIndemnificationPerAnimal') ) then destrIndemnificationPerAnimal := row.field('costDestrIndemnificationPerAnimal');
      if( null <> row.field('costDestrDisposalPerAnimal') ) then destrDisposalPerAnimal := row.field('costDestrDisposalPerAnimal');
      if( null <> row.field('costVaccSetupPerUnit') ) then vaccSetupPerUnit := row.field('costVaccSetupPerUnit');
      if( null <> row.field('costVaccThreshold') ) then vaccThreshold := row.field('costVaccThreshold');
      if( null <> row.field('costVaccBaselinePerAnimal') ) then vaccBaselinePerAnimal := row.field('costVaccBaselinePerAnimal');
      if( null <> row.field('costVaccAdditionalPerAnimal') ) then vaccAdditionalPerAnimal := row.field('costVaccAdditionalPerAnimal');

      if( null <> row.field('costDestrAppraiseDeadUnitsCode') ) then destrAppraiseDeadUnitsCode := costApplicationCodeFromString( row.field('costDestrAppraiseDeadUnitsCode') );
      if( null <> row.field('costDestrCleanDeadUnitsCode') ) then destrCleanDeadUnitsCode := costApplicationCodeFromString( row.field('costDestrCleanDeadUnitsCode') );
      if( null <> row.field('costDestrIndemnifyDeadUnitsCode') ) then destrIndemnificationDeadUnitsCode := costApplicationCodeFromString( row.field('costDestrIndemnifyDeadUnitsCode') );
      if( null <> row.field('costDestrDisposeDeadUnitsCode') ) then destrDisposeDeadUnitsCode := costApplicationCodeFromString( row.field('costDestrDisposeDeadUnitsCode') );

      res.Free();
      _updated := false;
    end
  ;


  procedure TCostParams.initialize();
    begin
      _destrAppraisalPerUnit := -1.0;
      _destrCleaningPerUnit := -1.0;
      _destrEuthanasiaPerAnimal := -1.0;
      _destrIndemnificationPerAnimal := -1.0;
      _destrDisposalPerAnimal := -1.0;
      _vaccSetupPerUnit := -1.0;
      _vaccThreshold := -1;
      _vaccBaselinePerAnimal := -1.0;
      _vaccAdditionalPerAnimal := -1.0;
      _destrAppraiseDeadUnitsCode := caUnspecified;
      _destrCleanDeadUnitsCode := caUnspecified;
      _destrIndemnifyDeadUnitsCode := caUnspecified;
      _destrDisposeDeadUnitsCode := caUnspecified;

    end
  ;


  destructor TCostParams.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Cost calculations
//-----------------------------------------------------------------------------
  function TCostParams.destrAppraisalCosts( const unitsDestroyed, unitsDcdOnly, unitsDcdInDestrQueue: longint ): double;
    begin
      // factor in the appraisal cost options for units dead from disease in addition to destroyed units
      case destrAppraiseDeadUnitsCode of
        caAlwaysApply: result := _destrAppraisalPerUnit * unitsDestroyed;
        caNeverApply: result := _destrAppraisalPerUnit * ( unitsDestroyed - unitsDcdOnly );
        caOnlyApplyAfterQueDestr: result := _destrAppraisalPerUnit * ( unitsDestroyed - unitsDcdOnly + unitsDcdInDestrQueue );
        else
          begin
            raise exception.Create( 'Unrecognized case in TCostParams.destrAppraisalCosts' );
            result := -1.0;
          end
        ;
      end; // case
    end
  ;


  function TCostParams.destrCleaningCosts( const unitsDestroyed, unitsDcdOnly, unitsDcdInDestrQueue: longint ): double;
    begin
      // factor in the cleanup cost options for units dead from disease in addition to destroyed units
      case destrCleanDeadUnitsCode of
        caAlwaysApply: result := _destrCleaningPerUnit * unitsDestroyed;
        caNeverApply: result := _destrCleaningPerUnit * ( unitsDestroyed - unitsDcdOnly );
        caOnlyApplyAfterQueDestr: result := _destrCleaningPerUnit * ( unitsDestroyed - unitsDcdOnly + unitsDcdInDestrQueue );
        else
          begin
            raise exception.Create( 'Unrecognized case in TCostParams.destrCleaningCosts' );
            result := -1.0;
          end
        ;
      end; // case
    end
  ;


  function TCostParams.destrEuthanasiaCosts( const animalsDestroyed, animalsDcdOnly: longint ): double;
    begin
      // no dead from disease costs associated with euthanasia ...
      result := _destrEuthanasiaPerAnimal * ( animalsDestroyed - animalsDcdOnly );
    end
  ;


  function TCostParams.destrIndemnificationCosts( const animalsDestroyed, animalsDcdOnly, animalsDcdInDestrQueue: longint ): double;
    begin
      // factor in the indemification cost options for animals dead from disease in addition to destroyed animals
      case destrIndemnificationDeadUnitsCode of
        caAlwaysApply: result := _destrIndemnificationPerAnimal * animalsDestroyed;
        caNeverApply: result := _destrIndemnificationPerAnimal * ( animalsDestroyed - animalsDcdOnly );
        caOnlyApplyAfterQueDestr: result := _destrIndemnificationPerAnimal * ( animalsDestroyed - animalsDcdOnly + animalsDcdInDestrQueue );
        else
          begin
            raise exception.Create( 'Unrecognized case in TCostParams.destrIndemnificationCosts' );
            result := -1.0;
          end
        ;
      end; // case
    end
  ;


  function TCostParams.destrDisposalCosts( const animalsDestroyed, animalsDcdOnly, animalsDcdInDestrQueue: longint ): double;
    begin
       // factor in the disposal cost options for animals dead from disease in addition to destroyed animals
      case destrDisposeDeadUnitsCode of
        caAlwaysApply: result := _destrDisposalPerAnimal * animalsDestroyed;
        caNeverApply: result := _destrDisposalPerAnimal * ( animalsDestroyed - animalsDcdOnly );
        caOnlyApplyAfterQueDestr: result := _destrDisposalPerAnimal * ( animalsDestroyed - animalsDcdOnly + animalsDcdInDestrQueue );
        else
          begin
            raise exception.Create( 'Unrecognized case in TCostParams.destrDisposalCosts' );
            result := -1.0;
          end
        ;
      end; // case
    end
  ;


  // This function is currently unused, but potentially useful
  function TCostParams.destrTotalCosts(
        const unitsDestroyed, animalsDestroyed: longint;
        const unitsDcdOnly, animalsDcdOnly: longint;
        const unitsDcdInDestrQueue, animalsDcdInDestrQueue: longint
      ): double;
    begin
      result :=
        destrAppraisalCosts( unitsDestroyed, unitsDcdOnly, unitsDcdInDestrQueue )
        + destrCleaningCosts( unitsDestroyed, unitsDcdOnly, unitsDcdInDestrQueue )
        + destrEuthanasiaCosts( animalsDestroyed, animalsDcdOnly )
        + destrIndemnificationCosts( animalsDestroyed, animalsDcdOnly, animalsDcdInDestrQueue )
        + destrDisposalCosts( animalsDestroyed, animalsDcdOnly, animalsDcdInDestrQueue )
      ;
    end
  ;


  function TCostParams.vaccSetupCosts( const unitsVaccinated: longint ): double;
    begin
      result := _vaccSetupPerUnit * unitsVaccinated;
    end
  ;


  function TCostParams.vaccTotalVaccinationCosts( const animalsVaccinated: longint ): double;
    begin
      if( animalsVaccinated <= _vaccThreshold ) then
        result := _vaccBaselinePerAnimal * animalsVaccinated
      else
        begin
          result :=
            ( _vaccThreshold * _vaccBaselinePerAnimal )
            + ( ( animalsVaccinated - vaccThreshold ) * (_vaccBaselinePerAnimal + _vaccAdditionalPerAnimal ) )
          ;
        end
      ;
    end
  ;


  function TCostParams.vaccVaccinationDailyCosts( const animalsToBeVaccinated: integer; var animalsAlreadyVaccinated: longint ): double;
    var
      animalsOverThreshold: integer;
      animalsUnderThreshold: integer;
    begin
      if( animalsAlreadyVaccinated > vaccThreshold ) then
        result := animalsToBeVaccinated * (_vaccBaselinePerAnimal + _vaccAdditionalPerAnimal )
      else if( ( animalsAlreadyVaccinated + animalsToBeVaccinated ) <= _vaccThreshold ) then
        result := animalsToBeVaccinated * _vaccBaselinePerAnimal
      else // The threshold is exceeded on this day.
        begin
          animalsOverThreshold :=
            animalsAlreadyVaccinated
            + animalsToBeVaccinated
            - _vaccThreshold
          ;

          animalsUnderThreshold := animalsToBeVaccinated - animalsOverThreshold;

          result :=
            ( animalsUnderThreshold * _vaccBaselinePerAnimal )
            + ( animalsOverThreshold * ( _vaccBaselinePerAnimal + _vaccAdditionalPerAnimal ) )
          ;
        end
      ;

      inc( animalsAlreadyVaccinated, animalsToBeVaccinated );
    end
  ;


  // This function is currently unused, but potentially useful
  function TCostParams.vaccTotalCosts( const unitsVaccinated, animalsVaccinated: longint ): double;
    begin
      result :=
        vaccSetupCosts( unitsVaccinated )
        + vaccTotalVaccinationCosts( animalsVaccinated )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// FIX ME
//-----------------------------------------------------------------------------
  function TCostParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['costDestrAppraisalPerUnit'] := DATABASE_NULL_VALUE;
      dict['costDestrCleaningPerUnit'] := DATABASE_NULL_VALUE;
      dict['costDestrEuthanasiaPerAnimal'] := DATABASE_NULL_VALUE;
      dict['costDestrIndemnificationPerAnimal'] := DATABASE_NULL_VALUE;
      dict['costDestrDisposalPerAnimal'] := DATABASE_NULL_VALUE;
      dict['costVaccSetupPerUnit'] := DATABASE_NULL_VALUE;
      dict['costVaccThreshold'] := DATABASE_NULL_VALUE;
      dict['costVaccBaselinePerAnimal'] := DATABASE_NULL_VALUE;
      dict['costVaccAdditionalPerAnimal'] := DATABASE_NULL_VALUE;
      dict['costDestrAppraiseDeadUnitsCode'] := DATABASE_NULL_VALUE;
      dict['costDestrCleanDeadUnitsCode'] := DATABASE_NULL_VALUE;
      dict['costDestrIndemnifyDeadUnitsCode'] := DATABASE_NULL_VALUE;
      dict['costDestrDisposeDeadUnitsCode'] := DATABASE_NULL_VALUE;


      if( 0.0 <= _destrAppraisalPerUnit ) then dict['costDestrAppraisalPerUnit'] := usFloatToStr( _destrAppraisalPerUnit );
      if( 0.0 <= _destrCleaningPerUnit ) then dict['costDestrCleaningPerUnit'] := usFloatToStr( _destrCleaningPerUnit );
      if( 0.0 <= _destrEuthanasiaPerAnimal ) then dict['costDestrEuthanasiaPerAnimal'] := usFloatToStr( _destrEuthanasiaPerAnimal );
      if( 0.0 <= _destrIndemnificationPerAnimal ) then dict['costDestrIndemnificationPerAnimal'] := usFloatToStr( _destrIndemnificationPerAnimal );
      if( 0.0 <= _destrDisposalPerAnimal ) then dict['costDestrDisposalPerAnimal'] := usFloatToStr( _destrDisposalPerAnimal );
      if( 0.0 <= _vaccSetupPerUnit ) then dict['costVaccSetupPerUnit'] := usFloatToStr( _vaccSetupPerUnit );
      if( 0 <= _vaccThreshold ) then dict['costVaccThreshold'] := intToStr( _vaccThreshold );
      if( 0.0 <= _vaccBaselinePerAnimal ) then dict['costVaccBaselinePerAnimal'] := usFloatToStr( _vaccBaselinePerAnimal );
      if( 0.0 <= _vaccAdditionalPerAnimal ) then dict['costVaccAdditionalPerAnimal'] := usFloatToStr( _vaccAdditionalPerAnimal );
      //AR:deadDV
      if( caUnspecified = _destrAppraiseDeadUnitsCode ) then
        dict['costDestrAppraiseDeadUnitsCode'] := DATABASE_NULL_VALUE
      else
        dict['costDestrAppraiseDeadUnitsCode'] := db.sqlQuote( costApplicationCodeString( _destrAppraiseDeadUnitsCode ) )
      ;

      if( caUnspecified = _destrCleanDeadUnitsCode ) then
        dict['costDestrCleanDeadUnitsCode'] := DATABASE_NULL_VALUE
      else
        dict['costDestrCleanDeadUnitsCode'] := db.sqlQuote( costApplicationCodeString( _destrCleanDeadUnitsCode ) )
      ;

      if( caUnspecified = _destrIndemnifyDeadUnitsCode ) then
        dict['costDestrIndemnifyDeadUnitsCode'] := DATABASE_NULL_VALUE
      else
        dict['costDestrIndemnifyDeadUnitsCode'] := db.sqlQuote( costApplicationCodeString( _destrIndemnifyDeadUnitsCode ) )
      ;

      if( caUnspecified = _destrDisposeDeadUnitsCode ) then
        dict['costDestrDisposeDeadUnitsCode'] := DATABASE_NULL_VALUE
      else
        dict['costDestrDisposeDeadUnitsCode'] := db.sqlQuote( costApplicationCodeString( _destrDisposeDeadUnitsCode ) )
      ;

      q := writeQuery(
      	'inProductionType',
        QUpdate,
        dict,
        'WHERE `productionTypeID` = ' + intToStr( ptID )
      );

      dbcout( '--- TCostParams.populateDatabase' + endl + q, DBFORMCOSTPARAMS );

      result := integer( db.execute( q ) );

      dict.Clear();
      dict.Free();

      _updated := false;
    end
  ;


  procedure TCostParams.debug();
    begin
      dbcout( '-------------BEGIN COST PARAMS', true );
      dbcout( 'destrAppraisalPerUnit: ' + usFloatToStr( _destrAppraisalPerUnit ), true );
      dbcout( 'destrCleaningPerUnit: ' + usFloatToStr( _destrCleaningPerUnit ), true );
      dbcout( 'destrEuthanasiaPerAnimal: ' + usFloatToStr( _destrEuthanasiaPerAnimal ), true );
      dbcout( 'destrIndemnificationPerAnimal: ' + usFloatToStr( _destrIndemnificationPerAnimal ), true );
      dbcout( 'destrDisposalPerAnimal: ' + usFloatToStr( _destrDisposalPerAnimal ), true );
      dbcout( 'vaccSetupPerUnit: ' + usFloatToStr( _vaccSetupPerUnit ), true );
      dbcout( 'vaccThreshold: ' + intToStr( _vaccThreshold ), true );
      dbcout( 'vaccBaselinePerAnimal: ' + usFloatToStr( _vaccBaselinePerAnimal ), true );
      dbcout( 'vaccAdditionalPerAnimal: ' + usFloatToStr( _vaccAdditionalPerAnimal ), true );

      if( caUnspecified  = _destrAppraiseDeadUnitsCode ) then
        dbcout( 'destrAppraiseDeadUnitsCode: UNSPECIFIED', true )
      else
        dbcout( 'destrAppraiseDeadUnitsCode: ' + costApplicationCodeString( _destrAppraiseDeadUnitsCode ) , true )
      ;

      if( caUnspecified  = _destrCleanDeadUnitsCode ) then
        dbcout( 'destrCleanDeadUnitsCode: UNSPECIFIED', true )
      else
        dbcout( 'destrCleanDeadUnitsCode: ' + costApplicationCodeString( _destrCleanDeadUnitsCode ) , true )
      ;

      if( caUnspecified  = _destrIndemnifyDeadUnitsCode ) then
        dbcout( 'destrIndemnifyDeadUnitsCode: UNSPECIFIED', true )
      else
        dbcout( 'destrIndemnifyDeadUnitsCode: ' + costApplicationCodeString( _destrIndemnifyDeadUnitsCode ) , true )
      ;

      if( caUnspecified  = _destrDisposeDeadUnitsCode ) then
        dbcout( 'destrDisposeDeadUnitsCode: UNSPECIFIED', true )
      else
        dbcout( 'destrDisposeDeadUnitsCode: ' + costApplicationCodeString( _destrDisposeDeadUnitsCode ) , true )
      ;

      dbcout( '-------------END COST PARAMS', true );
    end
  ;


  function TCostParams.validate( err: PString = nil ): boolean;
    begin
      raise exception.Create( 'TCostParams.validate() should not be called.  Use validateDestr() and validateVacc() instead.' );
      result := false;
    end
  ;


  function TCostParams.validateVacc( err: PString = nil ): boolean;
  	var
    	msg: string;
      costTrackVaccination: boolean;
    begin
    	result := true;
			msg := '';

      costTrackVaccination := (_sim as TSMSimulationInput).costTrackVaccination;

      if( costTrackVaccination ) then
        begin
          if( 0.0 > _vaccSetupPerUnit ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Vaccination setup cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0 > _vaccThreshold ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Vaccination threshold is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0.0 > _vaccBaselinePerAnimal ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Baseline vaccination cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0.0 > _vaccAdditionalPerAnimal ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Additional vaccination cost is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;


  function TCostParams.validateDestr( err: PString = nil ): boolean;
  	var
    	msg: string;
      costTrackDestruction: boolean;
    begin
    	result := true;
			msg := '';

      costTrackDestruction := (_sim as TSMSimulationInput).costTrackDestruction;

      if( costTrackDestruction ) then
        begin
          if( 0.0 > _destrAppraisalPerUnit ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Appraisal cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0.0 > _destrCleaningPerUnit ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Cleaning and disinfection cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0.0 > _destrEuthanasiaPerAnimal ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Euthanasia cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0.0 > _destrIndemnificationPerAnimal ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Indemnification cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( 0.0 > _destrDisposalPerAnimal ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Disposal cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( caUnspecified = _destrAppraiseDeadUnitsCode ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'How to evaluate dead unit appraisal cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( caUnspecified = _destrCleanDeadUnitsCode ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'How to evaluate dead unit cleaning cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( caUnspecified = _destrIndemnifyDeadUnitsCode ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'How to evaluate dead unit indemnity cost is not set.' ) + endl;
              result := false;
            end
          ;

          if( caUnspecified = _destrDisposeDeadUnitsCode ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'How to evaluate dead unit disposal cost is not set.' ) + endl;
              result := false;
            end
          ;

        end  //costTrackDestruction
      ;

      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;


  function TCostParams.validateTracing( err: PString = nil ): boolean;
    begin
      // FIX ME: write this function some day!
      result := true;
    end
  ;


  procedure TCostParams.insertDatabaseOutputs(
        outputs: TSMDailyOutput;
        db: TSMDatabase;
        ptID: integer;
        includeDestructionCosts: boolean;
        includeVaccinationCosts: boolean;
        iteration: integer
      );
    var
      qDict: TQueryDictionary;
      q: string;
    begin
      //self.debug();

      qDict := TQueryDictionary.create();

      qDict['iteration'] := intToStr( iteration );
      qDict['productionTypeID'] := intToStr( ptID );

      if( includeDestructionCosts and (_sim as TSMSimulationInput).controlParams.useDestructionGlobal ) then  // issue 2529
        begin
          qDict['destrAppraisal'] := usFloatToStr( destrAppraisalCosts( outputs.descUAll, outputs.descUDcd, outputs.descUDcdInDestrQueue ), 2, true );
          qDict['destrCleaning'] := usFloatToStr( destrCleaningCosts( outputs.descUAll, outputs.descUDcd, outputs.descUDcdInDestrQueue ), 2, true );
          qDict['destrEuthanasia'] := usFloatToStr( destrEuthanasiaCosts( outputs.descAAll, outputs.descADcd ), 2, true );
          qDict['destrIndemnification'] := usFloatToStr( destrIndemnificationCosts( outputs.descAAll, outputs.descADcd, outputs.descADcdInDestrQueue ), 2, true );
          qDict['destrDisposal'] := usFloatToStr( destrDisposalCosts( outputs.descAAll, outputs.descADcd, outputs.descADcdInDestrQueue ), 2, true );
        end
      else
        begin
          qDict['destrAppraisal'] := DATABASE_NULL_VALUE;
          qDict['destrCleaning'] := DATABASE_NULL_VALUE;
          qDict['destrEuthanasia'] := DATABASE_NULL_VALUE;
          qDict['destrIndemnification'] := DATABASE_NULL_VALUE;
          qDict['destrDisposal'] := DATABASE_NULL_VALUE;
        end
      ;

      if( includeVaccinationCosts and (_sim as TSMSimulationInput).controlParams.useVaccGlobal ) then // issue 2529
        begin
          qDict['vaccSetup'] := usFloatToStr( vaccSetupCosts( outputs.vaccUAll ), 2, true );
          qDict['vaccVaccination'] := usFloatToStr( vaccTotalVaccinationCosts( outputs.vaccAAll ), 2, true );
        end
      else
        begin
          qDict['vaccSetup'] := DATABASE_NULL_VALUE;
          qDict['vaccVaccination'] := DATABASE_NULL_VALUE;
        end
      ;

      if( remoteDBParams.useRemoteDatabase ) then
        qDict['jobID'] := intToStr( remoteDBParams.jobID )
      ;

      q := sqlclasses.writeQuery( 'outIterationCosts',  QInsert, qDict );

      dbcout( q, DBFORMCOSTPARAMS );

      if( remoteDBParams.useRemoteDatabase ) then
        db.remoteExecute( q )
      else
        db.execute( q )
      ;

      qDict.free();
    end
  ;


  function TCostParams.ssXML( const useDestruction, useVaccination: boolean; const productionTypeID: integer ): string;
    var
      DeadCodeAppraisal: string;
      DeadCodeCleaning: string;
      DeadCodeIndemification: string;
      DeadCodeDisposal: string;
    begin
      // Cost parameters have no XML if used with the GUI.
      // XML here is used for the supercomputer version.
      
      result := '  <economic-model production-type="' + prodTypeDescr + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;

      if( useVaccination ) then
        begin
          result := result
            + '    <vaccination-fixed>' + endl
            + '      <value>' + usFloatToStr( vaccSetupPerUnit ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </vaccination-fixed>' + endl
            + '    <vaccination>' + endl
            + '      <value>' + usFloatToStr( vaccBaselinePerAnimal ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </vaccination>' + endl
            + '    <baseline-vaccination-capacity>' + intToStr( vaccThreshold ) + '</baseline-vaccination-capacity>' + endl
            + '    <additional-vaccination>' + endl
            + '      <value>' + usFloatToStr( vaccAdditionalPerAnimal ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </additional-vaccination>' + endl
          ;
        end
      ;

      if( useDestruction ) then
        begin
          {
            Neil suggests: A value of "no" in all of the dead code fields will produce behavior consistent with older
            versions of the program, so his suggestion was to have all the tags filled in with "no" by default.
            - By doing so, caUnspecified defaults no and a decision does not have to be made whether or not to write
            the element to the file because it will always have a value.
          }
          DeadCodeAppraisal := 'no';
          DeadCodeCleaning := 'no';
          DeadCodeIndemification := 'no';
          DeadCodeDisposal := 'no';

          case _destrAppraiseDeadUnitsCode of
            caAlwaysApply: DeadCodeAppraisal := 'yes';
            caNeverApply:  DeadCodeAppraisal := 'no';
            caOnlyApplyAfterQueDestr: DeadCodeAppraisal := 'sometimes';
          end;
          case _destrCleanDeadUnitsCode of
            caAlwaysApply: DeadCodeCleaning := 'yes';
            caNeverApply:  DeadCodeCleaning := 'no';
            caOnlyApplyAfterQueDestr: DeadCodeCleaning := 'sometimes';
          end;
          case _destrIndemnifyDeadUnitsCode of
            caAlwaysApply: DeadCodeIndemification := 'yes';
            caNeverApply:  DeadCodeIndemification := 'no';
            caOnlyApplyAfterQueDestr: DeadCodeIndemification := 'sometimes';
          end;
          case _destrDisposeDeadUnitsCode of
            caAlwaysApply: DeadCodeDisposal := 'yes';
            caNeverApply:  DeadCodeDisposal := 'no';
            caOnlyApplyAfterQueDestr: DeadCodeDisposal := 'sometimes';
          end;


          result := result
            + '    <appraisal>' + endl
            + '      <value>' + usFloatToStr( destrAppraisalPerUnit ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </appraisal>' + endl
            + '    <euthanasia>' + endl
            + '      <value>' + usFloatToStr( destrEuthanasiaPerAnimal ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </euthanasia>' + endl
            + '    <indemnification>' + endl
            + '      <value>' + usFloatToStr( destrIndemnificationPerAnimal ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </indemnification>' + endl
            + '    <carcass-disposal>' + endl
            + '      <value>' + usFloatToStr( destrDisposalPerAnimal ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </carcass-disposal>' + endl
            + '    <cleaning-disinfecting>' + endl
            + '      <value>' + usFloatToStr( destrCleaningPerUnit ) + '</value>' + endl
            + '      <units><xdf:unit>USD</xdf:unit></units>' + endl
            + '    </cleaning-disinfecting>' + endl
            + '    <appraisal-applies-to-dead-units>' + DeadCodeAppraisal + '</appraisal-applies-to-dead-units>' + endl
            + '    <indemnification-applies-to-dead-units>' + DeadCodeIndemification + '</indemnification-applies-to-dead-units>' + endl
            + '    <carcass-disposal-applies-to-dead-units>' + DeadCodeDisposal + '</carcass-disposal-applies-to-dead-units>' + endl
            + '    <cleaning-disinfecting-applies-to-dead-units>' + DeadCodeCleaning + '</cleaning-disinfecting-applies-to-dead-units>' + endl
            + '    <output>' + endl
            + '      <variable-name>costsTotal</variable-name>' + endl
            + '      <frequency>daily</frequency>' + endl
            + '    </output>' + endl
          ;
        end
      ;

      result := result + '  </economic-model>' + endl;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TCostParams.getUpdated(): boolean; begin result := _updated; end;

  procedure TCostParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; _updated := true; end;

  function TCostParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TCostParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;
  procedure TCostParams.setDestrAppraisalPerUnit( val: double ); begin _destrAppraisalPerUnit := val; _updated := true; end;
  procedure TCostParams.setDestrCleaningPerUnit( val: double ); begin _destrCleaningPerUnit := val; _updated := true; end;
  procedure TCostParams.setDestrEuthanasiaPerAnimal( val: double ); begin _destrEuthanasiaPerAnimal := val; _updated := true; end;
  procedure TCostParams.setDestrIndemnificationPerAnimal( val: double ); begin _destrIndemnificationPerAnimal := val; _updated := true; end;
  procedure TCostParams.setDestrDisposalPerAnimal( val: double ); begin _destrDisposalPerAnimal := val; _updated := true; end;
  procedure TCostParams.setVaccSetupPerUnit( val: double ); begin _vaccSetupPerUnit := val; _updated := true; end;
  procedure TCostParams.setVaccThreshold( val: integer ); begin _vaccThreshold := val; _updated := true; end;
  procedure TCostParams.setVaccBaselinePerAnimal( val: double ); begin _vaccBaselinePerAnimal := val; _updated := true; end;
  procedure TCostParams.setVaccAdditionalPerAnimal( val: double ); begin _vaccAdditionalPerAnimal := val; _updated := true; end;

  procedure TCostParams.setDestrAppraiseDeadUnitsCode( val: TCostApplicationCode );
    begin
      case val of
        caAlwaysApply, caNeverApply, caOnlyApplyAfterQueDestr:
          begin _destrAppraiseDeadUnitsCode := val;  _updated := true; end;
      else
        begin _destrAppraiseDeadUnitsCode := caUnspecified; _updated := true; end;
      end;
    end
  ;

  procedure TCostParams.setDestrCleanDeadUnitsCode( val: TCostApplicationCode );
    begin
      case val of
        caAlwaysApply, caNeverApply, caOnlyApplyAfterQueDestr:
          begin _destrCleanDeadUnitsCode := val;  _updated := true; end;
      else
        begin _destrCleanDeadUnitsCode := caUnspecified; _updated := true; ;end
      end;
    end
  ;

  procedure TCostParams.setDestrDisposeDeadUnitsCode( val: TCostApplicationCode );
    begin
      case val of
        caAlwaysApply, caNeverApply, caOnlyApplyAfterQueDestr:
          begin _destrDisposeDeadUnitsCode := val;  _updated := true; end;
      else
        begin _destrDisposeDeadUnitsCode := caUnspecified; _updated := true; end;
      end;
    end
  ;

  procedure TCostParams.setDestrIndemnifyDeadUnitsCode( val: TCostApplicationCode );
    begin
      case val of
        caAlwaysApply, caNeverApply, caOnlyApplyAfterQueDestr:
          begin _destrIndemnifyDeadUnitsCode := val;  _updated := true; end;
      else
        begin _destrIndemnifyDeadUnitsCode := caUnspecified; _updated := true; end;
      end;
    end
  ;

  function TCostParams.getDestrAppraisalPerUnit(): double; begin result := _destrAppraisalPerUnit; end;
  function TCostParams.getDestrCleaningPerUnit(): double; begin result := _destrCleaningPerUnit; end;
  function TCostParams.getDestrEuthanasiaPerAnimal(): double; begin result := _destrEuthanasiaPerAnimal; end;
  function TCostParams.getDestrIndemnificationPerAnimal(): double; begin result := _destrIndemnificationPerAnimal; end;
  function TCostParams.getDestrDisposalPerAnimal(): double; begin result := _destrDisposalPerAnimal; end;
  function TCostParams.getVaccSetupPerUnit(): double; begin result := _vaccSetupPerUnit; end;
  function TCostParams.getVaccThreshold(): integer; begin result := _vaccThreshold; end;
  function TCostParams.getVaccBaselinePerAnimal(): double; begin result := _vaccBaselinePerAnimal; end;
  function TCostParams.getVaccAdditionalPerAnimal(): double; begin result := _vaccAdditionalPerAnimal; end;
  function TCostParams.getDestrAppraiseDeadUnitsCode(): TCostApplicationCode; begin result := _destrAppraiseDeadUnitsCode; end;
  function TCostParams.getDestrCleanDeadUnitsCode(): TCostApplicationCode; begin result := _destrCleanDeadUnitsCode; end;
  function TCostParams.getDestrDisposeDeadUnitsCode(): TCostApplicationCode; begin result := _destrDisposeDeadUnitsCode; end;
  function TCostParams.getDestrIndemnifyDeadUnitsCode(): TCostApplicationCode; begin result := _destrIndemnifyDeadUnitsCode; end;
//-----------------------------------------------------------------------------


end.
