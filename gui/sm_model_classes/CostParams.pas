unit CostParams;

(*
CostParams.pas
--------------
Begin: 2005/12/15
Last revision: $Date: 2008/10/15 16:23:09 $ $Author: areeves $
Version number: $Revision: 1.17 $
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
    SMDatabase,
    SMSimOutByProdType,
    Models
  ;

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
      function ssXML(): string; override;

      function validateDestr( err: PString = nil ): boolean;
      function validateVacc( err: PString = nil ): boolean;
      function validateTracing( err: PString = nil ): boolean;

      procedure insertDatabaseOutputs(
        outputs: TSMSimOutByProdType;
      	db: TSMDatabase;
        ptID: integer;
        includeDestructionCosts: boolean;
        includeVaccinationCosts: boolean;
        iteration: integer
      );

      // Cost calculations
      //------------------
      function destrAppraisalCosts( const unitsDestroyed: longint ): double;
      function destrCleaningCosts( const unitsDestroyed: longint ): double;
      function destrEuthanasiaCosts( const animalsDestroyed: longint ): double;
      function destrIndemnificationCosts( const animalsDestroyed: longint ): double;
      function destrDisposalCosts( const animalsDestroyed: longint ): double;

      function destrTotalCosts( const unitsDestroyed, animalsDestroyed: longint ): double;

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
    end
  ;

  const
    DBFORMCOSTPARAMS: boolean = false; // Set to true to enable debugging messages for this unit.

implementation

  uses
    Variants,
    SysUtils,

    MyStrUtils,
    USStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    SMSimulationInput,
    RemoteDatabaseParams
  ;

  const DBSHOWMSG = false; // Set to true to enable debugging messages for this unit

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
        +  '`costVaccAdditionalPerAnimal`'
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
  function TCostParams.destrAppraisalCosts( const unitsDestroyed: longint ): double;
    begin
      result := _destrAppraisalPerUnit * unitsDestroyed;
    end
  ;


  function TCostParams.destrCleaningCosts( const unitsDestroyed: longint ): double;
    begin
      result := _destrCleaningPerUnit * unitsDestroyed;
    end
  ;


  function TCostParams.destrEuthanasiaCosts( const animalsDestroyed: longint ): double;
    begin
      result := _destrEuthanasiaPerAnimal * animalsDestroyed;
    end
  ;


  function TCostParams.destrIndemnificationCosts( const animalsDestroyed: longint ): double;
    begin
      result := _destrIndemnificationPerAnimal * animalsDestroyed;
    end
  ;


  function TCostParams.destrDisposalCosts( const animalsDestroyed: longint ): double;
    begin
      result := _destrDisposalPerAnimal * animalsDestroyed;
    end
  ;


  function TCostParams.destrTotalCosts( const unitsDestroyed, animalsDestroyed: longint ): double;
    begin
      result :=
        destrAppraisalCosts( unitsDestroyed )
        + destrCleaningCosts( unitsDestroyed )
        + destrEuthanasiaCosts( animalsDestroyed )
        + destrIndemnificationCosts( animalsDestroyed )
        + destrDisposalCosts( animalsDestroyed )
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

      if( 0.0 <= _destrAppraisalPerUnit ) then dict['costDestrAppraisalPerUnit'] := usFloatToStr( _destrAppraisalPerUnit );
      if( 0.0 <= _destrCleaningPerUnit ) then dict['costDestrCleaningPerUnit'] := usFloatToStr( _destrCleaningPerUnit );
      if( 0.0 <= _destrEuthanasiaPerAnimal ) then dict['costDestrEuthanasiaPerAnimal'] := usFloatToStr( _destrEuthanasiaPerAnimal );
      if( 0.0 <= _destrIndemnificationPerAnimal ) then dict['costDestrIndemnificationPerAnimal'] := usFloatToStr( _destrIndemnificationPerAnimal );
      if( 0.0 <= _destrDisposalPerAnimal ) then dict['costDestrDisposalPerAnimal'] := usFloatToStr( _destrDisposalPerAnimal );
      if( 0.0 <= _vaccSetupPerUnit ) then dict['costVaccSetupPerUnit'] := usFloatToStr( _vaccSetupPerUnit );
      if( 0 <= _vaccThreshold ) then dict['costVaccThreshold'] := intToStr( _vaccThreshold );
      if( 0.0 <= _vaccBaselinePerAnimal ) then dict['costVaccBaselinePerAnimal'] := usFloatToStr( _vaccBaselinePerAnimal );
      if( 0.0 <= _vaccAdditionalPerAnimal ) then dict['costVaccAdditionalPerAnimal'] := usFloatToStr( _vaccAdditionalPerAnimal );

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
      dbcout( '-------------END COST PARAMS', true );
    end
  ;


  function TCostParams.validate( err: PString = nil ): boolean;
    begin
      raise exception.Create( 'TCostParams.validate() should not be called.  Use validateDescr() and validateVacc() instead.' );
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


  function TCostParams.validateTracing( err: PString = nil ): boolean;
    begin
      // FIX ME: write this function some day!
      result := true;
    end
  ;


  procedure TCostParams.insertDatabaseOutputs(
        outputs: TSMSimOutByProdType;
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
      self.debug();
      
      qDict := TQueryDictionary.create();

      qDict['iteration'] := intToStr( iteration );
      qDict['productionTypeID'] := intToStr( ptID );

      if( includeDestructionCosts ) then
        begin
          qDict['destrAppraisal'] := usFloatToStr( destrAppraisalCosts( outputs.descUTotal ), 2, true );
          qDict['destrCleaning'] := usFloatToStr( destrCleaningCosts( outputs.descUTotal ), 2, true );
          qDict['destrEuthanasia'] := usFloatToStr( destrEuthanasiaCosts( outputs.descATotal ), 2, true );
          qDict['destrIndemnification'] := usFloatToStr( destrIndemnificationCosts( outputs.descATotal ), 2, true );
          qDict['destrDisposal'] := usFloatToStr( destrDisposalCosts( outputs.descATotal ), 2, true );
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

      if( includeVaccinationCosts ) then
        begin
          qDict['vaccSetup'] := usFloatToStr( vaccSetupCosts( outputs.vaccUTotal ), 2, true );
          qDict['vaccVaccination'] := usFloatToStr( vaccTotalVaccinationCosts( outputs.vaccATotal ), 2, true );
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

  function TCostParams.ssXML(): string;
    begin
      // Cost parameters have no XML if used with the GUI
      result := '';
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

  function TCostParams.getDestrAppraisalPerUnit(): double; begin result := _destrAppraisalPerUnit; end;
  function TCostParams.getDestrCleaningPerUnit(): double; begin result := _destrCleaningPerUnit; end;
  function TCostParams.getDestrEuthanasiaPerAnimal(): double; begin result := _destrEuthanasiaPerAnimal; end;
  function TCostParams.getDestrIndemnificationPerAnimal(): double; begin result := _destrIndemnificationPerAnimal; end;
  function TCostParams.getDestrDisposalPerAnimal(): double; begin result := _destrDisposalPerAnimal; end;
  function TCostParams.getVaccSetupPerUnit(): double; begin result := _vaccSetupPerUnit; end;
  function TCostParams.getVaccThreshold(): integer; begin result := _vaccThreshold; end;
  function TCostParams.getVaccBaselinePerAnimal(): double; begin result := _vaccBaselinePerAnimal; end;
  function TCostParams.getVaccAdditionalPerAnimal(): double; begin result := _vaccAdditionalPerAnimal; end;
//-----------------------------------------------------------------------------


end.
