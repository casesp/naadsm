unit DiseaseParams;

(*
DiseaseParams.pas
------------------
Begin: 2009/11/09
Last revision: $Date: 2011-10-19 16:50:40 $ $Author: areeves $
Version number: $Revision: 1.5.2.15 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{$INCLUDE ../Defs.inc}

interface

  uses
    Contnrs,
    Sysutils,

    Dialogs,

    Sdew,

    QLists,
    MyStrUtils,
    QStringMaps,
    SqlClasses,
    ChartFunction,

    FunctionDictionary,

    Models,
    ModelDatabase,
    SMDatabase,
    ProbDensityFunctions,
    RelFunction,
    FunctionEnums,
    SMSimOutByProdType,
    StatusEnums,
    GlobalControlParams,
    NAADSMLibraryTypes,
    Zone
  ;

  type TDiseaseParams = class( TModelWithFunctions )
    protected
       _xmlModelList: TQStringList;

       _prodTypeDescr: string;
       
      _simulateTransition: boolean;

      _probMortality: double;

      _pdfLatentName: string;
      _pdfSubclinicalName: string;
      _pdfClinicalName: string;
      _pdfImmuneName: string;
      _relPrevInfectedName: string;
      _relPrevSheddingName: string;

      // Functions for internal use
      //---------------------------
      procedure initialize();

      procedure setSimulateTransition( val: boolean );
      function getSimulateTransition(): boolean;

      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;

      procedure setProbMortality( val: double );
      function getProbMortality(): double;

      procedure setPdfLatentName( val: string );
      procedure setPdfSubclinicalName( val: string );
      procedure setPdfClinicalName( val: string );
      procedure setPdfImmuneName( val: string );
      procedure setRelPrevInfectedName( val: string );
      procedure setRelPrevSheddingName( val: string );
      function getPdfLatentName(): string;
      function getPdfSubclinicalName(): string;
      function getPdfClinicalName(): string;
      function getPdfImmuneName(): string;
      function getRelPrevInfectedName(): string;
      function getRelPrevSheddingName(): string;

      function getPdfDiseaseLatent(): TPdf;
      function getPdfDiseaseSubclinical(): TPdf;
      function getPdfDiseaseClinical(): TPdf;
      function getPdfDiseaseImmune(): TPdf;
      function getRelDiseasePrevInfected(): TRelFunction;
      function getRelDiseasePrevShedding(): TRelFunction;

      // XML import
      //-----------
      function getXmlModelList(): TQStringList;

      // overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      // Overridden from TModelWithFunctions
      //------------------------------------
      function getChartSet(): TChartSet; override;

      // Helper function for validation
      //-------------------------------
      function prevCurvesOK(): boolean;

    public
      // Construction/initialization/destruction
      //----------------------------------------
      constructor create( sim: TObject; prodTypeName: string ); overload;
      constructor create( const src: TDiseaseParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;
      constructor create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil ); overload;
      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      procedure debug(); override;
      function validate( err: PString = nil ): boolean; override;
      function ssXML(  const productionTypeID: integer ): string; reintroduce;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;

      // Overridden from TModelWithFunctions
      //------------------------------------
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
      function hasChartName( const chartName: string; const whichChart: TSMChart ): boolean; override;
      function functionsAreValid(): boolean; override;

      // XML import
      //-----------
      class function createXmlModelList(): TQStringList;
      procedure importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      property xmlModelList: TQStringList read getXmlModelList;

      // Properties
      //-----------
      property useDisease: boolean read getSimulateTransition write setSimulateTransition;
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;

      property probMortality: double read getProbMortality write setProbMortality;

      property pdfLatentName: string read getPdfLatentName write setPdfLatentName;
      property pdfSubclinicalName: string read getPdfSubclinicalName write setPdfSubclinicalName;
      property pdfClinicalName: string read getPdfClinicalName write setPdfClinicalName;
      property pdfImmuneName: string read getPdfImmuneName write setPdfImmuneName;
      property relPrevInfectedName: string read getRelPrevInfectedName write setRelPrevInfectedName;
      property relPrevSheddingName: string read getRelPrevSheddingName write setRelPrevSheddingName;

      property pdfDiseaseLatent: TPdf read getPdfDiseaseLatent;
      property pdfDiseaseSubclinical: TPdf read getPdfDiseaseSubclinical;
      property pdfDiseaseClinical: TPdf read getPdfDiseaseClinical;
      property pdfDiseaseImmune: TPdf read getPdfDiseaseImmune;
      property relDiseasePrevInfected: TRelFunction read getRelDiseasePrevInfected;
      property relDiseasePrevShedding: TRelFunction read getRelDiseasePrevShedding;
    end
  ;

implementation

  uses
    Math,
    StrUtils,
    Variants,

    ARMath,
    DebugWindow,
    I88n,

    SMSimulationInput
  ;

  const DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TDiseaseParams.create( sim: TObject; prodTypeName: string );
    begin
      inherited create();
      initialize();

      _simulateTransition := useDisease;
      _prodTypeDescr := prodTypeName;
      _sim := sim;
    end
  ;


  constructor TDiseaseParams.create( const src: TDiseaseParams; sim: TObject );
    begin
      inherited create( src );
      initialize();
      
      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;
      _simulateTransition := src._simulateTransition;

      _probMortality := src._probMortality;

      setPdfLatentName( src.pdfLatentName );
      setPdfSubclinicalName( src.pdfSubclinicalName );
      setPdfClinicalName( src.pdfClinicalName );
      setPdfImmuneName( src.pdfImmuneName );
      setRelPrevInfectedName( src.relPrevInfectedName );
      setRelPrevSheddingName( src.relPrevSheddingName );

      _updated := src._updated;
    end
  ;


  constructor TDiseaseParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      inherited create();
      initialize();

      _prodTypeDescr := prodTypeName;
      _sim := sim;

      db2 := db as TSqlDatabase;

      q := 'SELECT'
        + ' inProductionType.useDiseaseTransition, '
        + ' inProductionType.disProbMortality, '
        + ' inProductionType.disLatentPeriodPdfID, '
        + ' latentChart.chartName as latentChartName,'
        + ' inProductionType.disSubclinicalPeriodPdfID,'
        + ' subclinicalChart.chartName as subclinicalChartName,'
        + ' inProductionType.disClinicalPeriodPdfID, '
        + ' clinicalChart.chartName as clinicalChartName, '
        + ' inProductionType.disImmunePeriodPdfID, '
        + ' immuneChart.chartName as immuneChartName, '
        + ' inProductionType.disPrevalenceInfectedRelID, '
        + ' prevInfectedChart.chartName as prevInfectedChartName, '
        + ' inProductionType.disPrevalenceSheddingRelID, '
        + ' prevSheddingChart.chartName as prevSheddingChartName '

        + ' FROM '
        + ' ( ( ( ( ('
        + ' inProductionType'
        + ' LEFT OUTER JOIN inChart latentChart ON inProductionType.disLatentPeriodPdfID = latentChart.chartID )'
        + ' LEFT OUTER JOIN inChart subclinicalChart ON inProductionType.disSubclinicalPeriodPdfID = subclinicalChart.chartID )'
        + ' LEFT OUTER JOIN inChart clinicalChart ON inProductionType.disClinicalPeriodPdfID = clinicalChart.chartID )'
        + ' LEFT OUTER JOIN inChart immuneChart ON inProductionType.disImmunePeriodPdfID = immuneChart.chartID )'
        + ' LEFT OUTER JOIN inChart prevInfectedChart ON inProductionType.disPrevalenceInfectedRelID = prevInfectedChart.chartID )'
        + ' LEFT OUTER JOIN inChart prevSheddingChart ON inProductionType.disPrevalenceSheddingRelID = prevSheddingChart.chartID'
        + ' WHERE inProductionType.productionTypeID = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('useDiseaseTransition') ) then
        self.useDisease := boolean( row.field( 'useDiseaseTransition' ) )
      ;

      if( null <> row.field('disProbMortality') ) then
        self.probMortality := double( row.field( 'disProbMortality' ) )
      ;

      if( null <> row.field('disLatentPeriodPdfID') ) then
        self.pdfLatentName := row.field( 'latentChartName' )
      ;

      if( null <> row.field('disSubclinicalPeriodPdfID') ) then
        self.pdfSubclinicalName := row.field( 'subclinicalChartName' )
      ;

      if( null <> row.field('disClinicalPeriodPdfID') ) then
        self.pdfClinicalName := row.field( 'clinicalChartName' )
      ;

      if( null <> row.field('disImmunePeriodPdfID') ) then
        self.pdfImmuneName := row.field( 'immuneChartName' )
      ;

      if( null <> row.field('disPrevalenceInfectedRelID') ) then
        self.relPrevInfectedName := row.field( 'prevInfectedChartName' )
      ;

      if( null <> row.field('disPrevalenceSheddingRelID') ) then
        self.relPrevSheddingName := row.field( 'prevSheddingChartName' )
      ;

      _prodTypeDescr := prodTypeName;

      _updated := false;

      freeAndNil( res );
    end
  ;

  constructor TDiseaseParams.create( db: TSMDatabase; sim: TObject; sdew: TSdew; models: pointer; errMsg: pstring = nil );
    var
      nModels: integer;
      modelName: string;
      i: integer;
      model: pointer;
    begin
    	inherited create();
    	initialize();
      _sim := sim;

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

  procedure TDiseaseParams.initialize();
    begin
      _xmlModelList := nil;
      _simulateTransition := false;

      _probMortality := -1.0;

      setPdfLatentName( '' );
      setPdfSubclinicalName( '' );
      setPdfClinicalName( '' );
      setPdfImmuneName( '' );
      setRelPrevInfectedName( '' );
      setRelPrevSheddingName( '' );

      _updated := false;
    end
  ;


  destructor TDiseaseParams.destroy();
    begin
      freeAndNil( _xmlModelList );
      
      // The function dictionary is freed elsewhere.
      // PDFs and RELs are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfLatentName( '' );
      setPdfSubclinicalName( '' );
      setPdfClinicalName( '' );
      setPdfImmuneName( '' );
      setRelPrevInfectedName( '' );
      setRelPrevSheddingName( '' );

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TDiseaseParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
    var
      q: string;
      dict: TQueryDictionary;
      idstr: string;
    begin
      dict := TQueryDictionary.create();

      dict['useDiseaseTransition'] := db.sqlBool( self._simulateTransition );

      dict['disProbMortality'] := usFloatToStr( self._probMortality );

      if( nil <> pdfDiseaseLatent ) then
        idstr := intToStr( pdfDiseaseLatent.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disLatentPeriodPdfID'] := idstr;

      if( nil <> pdfDiseaseSubclinical ) then
        idstr := intToStr( pdfDiseaseSubclinical.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disSubclinicalPeriodPdfID'] := idstr;

      if( nil <> pdfDiseaseClinical ) then
        idstr := intToStr( pdfDiseaseClinical.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disClinicalPeriodPdfID'] := idstr;

      if( nil <> pdfDiseaseImmune ) then
        idstr := intToStr( pdfDiseaseImmune.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disImmunePeriodPdfID'] := idstr;

      if( nil <> relDiseasePrevInfected ) then
        idstr := intToStr( relDiseasePrevInfected.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disPrevalenceInfectedRelID'] := idstr;

      if( nil <> relDiseasePrevShedding ) then
        idstr := intToStr( relDiseasePrevShedding.id )
      else
        idstr := DATABASE_NULL_VALUE
      ;
      dict['disPrevalenceSheddingRelID'] := idstr;

      q := writeQuery(
        'inProductionType',
        QUpdate,
        dict,
        'WHERE `productionTypeID` = ' + intToStr( ptID )
      );

      result := integer( db.execute( q ) );

      dict.Clear();
      dict.Free();

      _updated := false;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// TProductionType Debugging
//-----------------------------------------------------------------------------
  procedure TDiseaseParams.debug();
    var
      msg: string;
    begin
      dbcout( '--- TDiseaseParams.debug()', true );

      msg := msg + 'simulateTransition: ' + usBoolToText( useDisease ) + endl;
      if( updated ) then msg := msg + '**UPDATED**' + endl;

      dbcout( msg, true );

      if( nil <> pdfDiseaseLatent ) then
        begin
          dbcout( 'diseaseLatent', true );
          pdfDiseaseLatent.debug();
        end
      else
        dbcout( 'NO LATENT PERIOD DEFINED', true )
      ;

      if( nil <> pdfDiseaseSubclinical ) then
        begin
          dbcout( endl + 'diseaseSubclinical', true );
          pdfDiseaseSubclinical.debug();
        end
      else
        dbcout( endl + 'NO SUBCLINICAL PERIOD DEFINED', true )
      ;

      if( nil <> pdfDiseaseClinical ) then
        begin
          dbcout( endl + 'diseaseClinical', true );
          pdfDiseaseClinical.debug();
        end
      else
        dbcout( endl + 'NO CLINICAL PERIOD DEFINED', true )
      ;

      if( nil <> pdfDiseaseImmune ) then
        begin
          dbcout( endl + 'diseaseImmune', true);
          pdfDiseaseImmune.debug();
        end
      else
        dbcout( endl + 'NO IMMUNE PERIOD DEFINED', true )
      ;

      if( (_sim as TSMSimulationInput).useWithinHerdPrevalence ) then
        begin
          if( nil <> relDiseasePrevInfected ) then
            begin
              dbcout( endl + 'diseasePrevInfected', true);
              relDiseasePrevInfected.debug();
            end
          else
            dbcout( endl + 'NO PREVALENCE-INFECTED FUNCTION DEFINED', true )
          ;

          if( nil <> relDiseasePrevShedding ) then
            begin
              dbcout( endl + 'diseasePrevShedding', true);
              relDiseasePrevShedding.debug();
            end
          else
            dbcout( endl + 'NO PREVALENCE-SHEDDING FUNCTION DEFINED', true )
          ;
        end
      else
        dbcout( endl + '(Within-herd prevalence is not used in this scenario)', true )
      ;

      dbcout( 'Probability of mortality: ' + dbStr( probMortality ), true );

      dbcout( '--- Done TDiseaseParams.debug()', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------

  function TDiseaseParams.getProdTypeDescr: string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TDiseaseParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  procedure TDiseaseParams.setProdTypeDescr(val: string); begin _prodTypeDescr := val; end;


  // Disease states
  //---------------
  procedure TDiseaseParams.setPdfLatentName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfLatentName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfLatentName := val;
      _updated := true;
    end
  ;


  procedure TDiseaseParams.setPdfSubclinicalName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfSubclinicalName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfSubclinicalName := val;
      _updated := true;
    end
  ;


  procedure TDiseaseParams.setPdfClinicalName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfClinicalName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfClinicalName := val;
      _updated := true;
    end
  ;


  procedure TDiseaseParams.setPdfImmuneName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfImmuneName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfImmuneName := val;
      _updated := true;
    end
  ;


  procedure TDiseaseParams.setRelPrevInfectedName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relPrevInfectedName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relPrevInfectedName := val;
      _updated := true;
    end
  ;


  procedure TDiseaseParams.setRelPrevSheddingName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relPrevSheddingName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relPrevSheddingName := val;
      _updated := true;
    end
  ;

  function TDiseaseParams.getPdfDiseaseLatent(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfLatentName ) ) then
            begin
              if( fnDictionary.value( _pdfLatentName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfLatentName ).fn as TPdf
              else
                begin
                  setPdfLatentName( '' );
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


  function TDiseaseParams.getPdfDiseaseSubclinical(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfSubclinicalName ) ) then
            begin
              if( fnDictionary.value( _pdfSubclinicalName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfSubclinicalName ).fn as TPdf
              else
                begin
                  setPdfSubclinicalName( '' );
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


  function TDiseaseParams.getPdfDiseaseClinical(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfClinicalName ) ) then
            begin
              if( fnDictionary.value( _pdfClinicalName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfClinicalName ).fn as TPdf
              else
                begin
                  setPdfClinicalName( '' );
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


  function TDiseaseParams.getPdfDiseaseImmune(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfImmuneName ) ) then
            begin
              if( fnDictionary.value( _pdfImmuneName ).fn is TPdf ) then
                result := fnDictionary.value( _pdfImmuneName ).fn as TPdf
              else
                begin
                  setPdfImmuneName( '' );
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


  function TDiseaseParams.getRelDiseasePrevInfected(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relPrevInfectedName ) ) then
            begin
              if( fnDictionary.value( _relPrevInfectedName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relPrevInfectedName ).fn as TRelFunction
              else
                begin
                  setRelPrevInfectedName( '' );
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


  function TDiseaseParams.getRelDiseasePrevShedding(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relPrevSheddingName ) ) then
            begin
              if( fnDictionary.value( _relPrevSheddingName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relPrevSheddingName ).fn as TRelFunction
              else
                begin
                  setRelPrevSheddingName( '' );
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


  function TDiseaseParams.getPdfLatentName(): string; begin result := _pdfLatentName; end;
  function TDiseaseParams.getPdfSubclinicalName(): string; begin result := _pdfSubclinicalName; end;
  function TDiseaseParams.getPdfClinicalName(): string; begin result := _pdfClinicalName; end;
  function TDiseaseParams.getPdfImmuneName(): string; begin result := _pdfImmuneName; end;
  function TDiseaseParams.getRelPrevInfectedName(): string; begin result := _relPrevInfectedName; end;
  function TDiseaseParams.getRelPrevSheddingName(): string; begin result := _relPrevSheddingName; end;

  function TDiseaseParams.getSimulateTransition() : boolean; begin result := _simulateTransition; end;

  procedure TDiseaseParams.setSimulateTransition( val: boolean );
    begin
      _simulateTransition := val;
      _updated := true;
    end
  ;


  function TDiseaseParams.getProbMortality(): double; begin result := _probMortality; end;
  procedure TDiseaseParams.setProbMortality( val: double ); begin _probMortality := val; _updated := true; end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// overridden from TModel
//-----------------------------------------------------------------------------
  function TDiseaseParams.getUpdated(): boolean;
    begin
      result :=
        _updated
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfLatentName )
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfSubclinicalName )
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfClinicalName )
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfImmuneName )
      or
        fnDictionary.functionExistsAndIsUpdated( _relPrevInfectedName )
      or
        fnDictionary.functionExistsAndIsUpdated( _relPrevSheddingName )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Overridden from TModelWithFunctions
//-----------------------------------------------------------------------------
  function TDiseaseParams.getChartSet(): TChartSet;
    begin
      result := [ DLatent .. DPrevShedding ];
    end
  ;


  procedure TDiseaseParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        DLatent: self.pdfLatentName := newName;
        DImmune: self.pdfImmuneName := newName;
        DSubclinical: self.pdfSubclinicalName := newName;
        DClinical: self.pdfClinicalName := newName;
        DPrevInfected: self.relPrevInfectedName := newName;
        DPrevShedding: self.relPrevSheddingName := newName;
      end;
    end
  ;


  function TDiseaseParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            DLatent:
              if ( self.fnDictionary.contains( self.pdfLatentName ) ) then
                result := self.fnDictionary.value( self.pdfLatentName ).fn
              ;
            DImmune:
              if ( self.fnDictionary.contains( self.pdfImmuneName ) ) then
                result := self.fnDictionary.value( self.pdfImmuneName ).fn
              ;
            DSubclinical:
              if ( self.fnDictionary.contains( self.pdfSubclinicalName ) ) then
                result := self.fnDictionary.value( self.pdfSubclinicalName ).fn
              ;
            DClinical:
              if ( self.fnDictionary.contains( self.pdfClinicalName ) ) then
                result := self.fnDictionary.value( self.pdfClinicalName ).fn
              ;
            DPrevInfected:
              if ( self.fnDictionary.contains( self.relPrevInfectedName ) ) then
                result := self.fnDictionary.value( self.relPrevInfectedName ).fn
              ;
            DPrevShedding:
              if ( self.fnDictionary.contains( self.relPrevSheddingName ) ) then
                result := self.fnDictionary.value( self.relPrevSheddingName ).fn
              ;
          end;
        end
      ;
    end
  ;


  procedure TDiseaseParams.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfLatentName ) then self.pdfLatentName := '';
      if( chartName = self.pdfImmuneName ) then self.pdfImmuneName := '';
      if( chartName = self.pdfSubclinicalName ) then self.pdfSubclinicalName := '';
      if( chartName = self.pdfClinicalName ) then self.pdfClinicalName := '';
      if( chartName = self.relPrevInfectedName ) then self.relPrevInfectedName := '';
      if( chartName = self.relPrevSheddingName ) then self.relPrevSheddingName := '';
    end
  ;


  function TDiseaseParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;
      
      case whichChart of
        DLatent: result := ( chartName = self.pdfLatentName );
        DSubclinical: result := ( chartName = self.pdfSubclinicalName );
        DClinical: result := ( chartName = self.pdfClinicalName );
        DImmune: result := ( chartName = self.pdfImmuneName );
        DPrevInfected: result := ( chartName = self.relPrevInfectedName );
        DPrevShedding: result := ( chartName = self.relPrevSheddingName );
      end;
    end
  ;

  
  function TDiseaseParams.functionsAreValid(): boolean;
    var
      includePrevalence: boolean;
    begin
      result := true; // until shown otherwise
      
      if( fnDictionary.contains( _pdfLatentName ) ) then
        begin
          if( not( fnDictionary.value( _pdfLatentName ).fn is TPdf ) ) then
            begin
              setPdfLatentName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _pdfSubclinicalName ) ) then
        begin
          if( not( fnDictionary.value( _pdfSubclinicalName ).fn is TPdf ) ) then
            begin
              setPdfSubclinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _pdfClinicalName ) ) then
        begin
          if( not( fnDictionary.value( _pdfClinicalName ).fn is TPdf ) ) then
            begin
              setPdfClinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _pdfImmuneName ) ) then
        begin
          if( not( fnDictionary.value( _pdfImmuneName ).fn is TPdf ) ) then
            begin
              setPdfImmuneName( '' );
              result := false;
            end
          ;
        end
      ;

      includePrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;
      if( includePrevalence and fnDictionary.contains( _relPrevInfectedName ) ) then
        begin
          if( not( fnDictionary.value( _relPrevInfectedName ).fn is TRelFunction ) ) then
            begin
              setRelPrevInfectedName( '' );
              result := false;
            end
          ;
        end
      ;

      if( includePrevalence and fnDictionary.contains( _relPrevSheddingName ) ) then
        begin
          if( not( fnDictionary.value( _relPrevSheddingName ).fn is TRelFunction ) ) then
            begin
              setRelPrevSheddingName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TDiseaseParams.prevCurvesOK(): boolean;
    var
      idx, minIdx, maxIdx: integer;
    begin
      result := true;

      // Determine minimum and maximum x values
      minIdx := floor( min( relDiseasePrevInfected.points[0].x, relDiseasePrevShedding.points[0].x ) );
      maxIdx := ceil( max( relDiseasePrevInfected.points[ relDiseasePrevInfected.pointCount - 1 ].x, relDiseasePrevShedding.points[ relDiseasePrevShedding.pointCount - 1 ].x ) );

      // Are y values appropriate for every possible x value?
      for idx := minIdx to maxIdx do
        begin
          if( relDiseasePrevInfected.y( idx ) < relDiseasePrevShedding.y( idx ) ) then
            begin
              result := false;
              break;
            end
          ;
        end
      ;
    end
  ;


  function TDiseaseParams.validate( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;

      includePrevalence: boolean;
    begin
      result := true;
      msg := '';

      if( not useDisease ) then
        exit
      ;

      includePrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;

      submsg := '';
      if( ( 0.0 > probMortality ) or ( 1.0 < probMortality ) ) then
        begin
          if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of mortality is not valid.' ) + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseLatent ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Latent period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseLatent.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Latent period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseSubclinical ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Subclinical period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseSubclinical.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Subclinical period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseClinical) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Clinical period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseClinical.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Clinical period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      submsg := '';
      if( nil = pdfDiseaseImmune ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Immune period PDF is not set.' ) + endl;
          result := false;
        end
      else if( not( pdfDiseaseImmune.validate( @submsg) ) ) then
        begin
          if( err <> nil ) then msg := msg + '  ' + tr( 'Immune period PDF is not valid:' ) + ' ' + submsg + endl;
          result := false;
        end
      ;

      if( includePrevalence ) then
        begin
          submsg := '';
          if( nil = relDiseasePrevInfected ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence of infection is not set.' ) + endl;
              result := false;
            end
          else if( not( relDiseasePrevInfected.validate( @submsg) ) ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence of infection function is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;

          submsg := '';
          if( nil = relDiseasePrevShedding ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence of disease shedding is not set.' ) + endl;
              result := false;
            end
          else if( not( relDiseasePrevShedding.validate( @submsg) ) ) then
            begin
              if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence of disease shedding function is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;

          // If we're OK so far, check that the prevalence of infection is never lower than prevalence of shedding.
          if( result ) then
            begin
              if( not( prevCurvesOK() ) ) then
                begin
                  if( err <> nil ) then msg := msg + '  ' + tr( 'Prevalence of infection can never be less than prevalence of disease shedding.' ) + endl;
                  result := false;
                end
              ;
            end
          ;
        end
      ;

      if( ( result = false ) and ( err <> nil ) ) then
        begin
          msg := ansiReplaceStr( tr( 'Disease parameters for xyz are not valid:' ), 'xyz', _prodTypeDescr ) + endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
  function TDiseaseParams.ssXML( const productionTypeID: integer ): string;
    var
      useWithinHerdPrevalence: boolean;
    begin
      useWithinherdPrevalence := (_sim as TSMSimulationInput).useWithinHerdPrevalence;

      if( useDisease ) then
        begin
          result := '  <disease-model production-type="' + encodeXml( self._prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;

          if( nil <> pdfDiseaseLatent ) then
            begin
              result := result + '    <latent-period>' + endl;
              result := result + pdfDiseaseLatent.ssXml( 3 );
              result := result + '    </latent-period>' + endl;
            end
          ;

          if( nil <> pdfDiseaseSubclinical ) then
            begin
            result := result + '    <infectious-subclinical-period>' + endl;
              result := result + pdfDiseaseSubclinical.ssXml( 3 );
              result := result + '    </infectious-subclinical-period>' + endl;
            end
          ;

          if( nil <> pdfDiseaseClinical ) then
            begin
              result := result + '    <infectious-clinical-period>' + endl;
              result := result + pdfDiseaseClinical.ssXml( 3 );
              result := result + '    </infectious-clinical-period>' + endl;
            end
          ;

          if( nil <> pdfDiseaseImmune ) then
            begin
              result := result + '    <immunity-period>' + endl;
              result := result + pdfDiseaseImmune.ssXml( 3 );
              result := result + '    </immunity-period>' + endl;
            end
          ;

          if( useWithinHerdPrevalence ) then
            begin
              result := result + '    <prevalence>' + endl;
              result := result + relDiseasePrevInfected.ssXml( 3 );
              result := result + '    </prevalence>' + endl;

              result := result + '    <prevalence-infectious>' + endl;
              result := result + relDiseasePrevShedding.ssXml( 3 );
              result := result + '    </prevalence-infectious>' + endl;
            end
          ;

          result := result + '    <mortality>' + usFloatToStr( probMortality ) + '</mortality>' + endl;

          result := result + '  </disease-model>' + endl;
        end
      else
        result := ''
      ;
    end
  ;
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TDiseaseParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'disease-model' );
    end
  ;


  function TDiseaseParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TDiseaseParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      e: pointer;

      latentPeriod: TPdf;
      infectiousSubClinicalPeriod: TPdf;
      infectiousClinicalPeriod: TPdf;
      immunityPeriod: TPdf;
      prevalenceInfectedChart: TRelFunction;
      prevalenceSheddingChart: TRelFunction;

      val: double;
    begin
      latentPeriod := nil;
      infectiousSubClinicalPeriod := nil;
      infectiousClinicalPeriod := nil;
      immunityPeriod := nil;
      prevalenceInfectedChart := nil;
      prevalenceSheddingChart := nil;

      // Latent period
      //--------------
      e := sdew.GetElementByName( model, 'latent-period' );
      if( nil <> e ) then latentPeriod := createPdfFromXml( e, sdew );
      if( nil <> latentPeriod ) then
        begin
          if ( latentPeriod.name = '' ) then
            latentPeriod.name := self._prodTypeDescr + ' latent period'
          ;
          dbcout( latentPeriod.name, DBSHOWMSG );
          latentPeriod.dbField := word( DLatent );
          self.pdfLatentName := fnDictionary.checkAndInsert( latentPeriod );
          self._simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the latent period.' ) )
      ;

      // Subclinical period
      //--------------------
      e := sdew.GetElementByName( model, 'infectious-subclinical-period' );
      if( nil <> e ) then infectiousSubClinicalPeriod := createPdfFromXml( e, sdew );
      if( nil <> infectiousSubClinicalPeriod ) then
        begin
          if ( infectiousSubClinicalPeriod.name = '' ) then
            infectiousSubClinicalPeriod.name := self._prodTypeDescr + ' subclinical period'
          ;
          dbcout( infectiousSubClinicalPeriod.name, DBSHOWMSG );
          infectiousSubClinicalPeriod.dbField := word( DSubclinical );
          self.pdfSubclinicalName := fnDictionary.checkAndInsert( infectiousSubClinicalPeriod );
          self._simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the subclinical period.' ) )
      ;

      // Clinical period
      //----------------
      e := sdew.GetElementByName( model, 'infectious-clinical-period' );
      if( nil <> e ) then infectiousClinicalPeriod := createPdfFromXml( e, sdew );
      if( nil <> infectiousClinicalPeriod ) then
        begin
          if ( infectiousClinicalPeriod.name = '' ) then
            infectiousClinicalPeriod.name := self._prodTypeDescr + ' clinical period'
          ;
          dbcout( infectiousClinicalPeriod.name, DBSHOWMSG );
          infectiousClinicalPeriod.dbField := word( DClinical );
          self.pdfClinicalName := fnDictionary.checkAndInsert( infectiousClinicalPeriod );
          self._simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the clinical period.' ) )
      ;

      // Immune period
      //--------------
      e := sdew.GetElementByName( model, 'immunity-period' );
      if( nil <> e ) then immunityPeriod := createPdfFromXml( e, sdew );
      if( nil <> immunityPeriod ) then
        begin
          if ( immunityPeriod.name = '' ) then
            immunityPeriod.name := self._prodTypeDescr + ' immune period'
          ;
          dbcout( immunityPeriod.name, DBSHOWMSG );
          immunityPeriod.dbField := word( DImmune );
          self.pdfImmuneName := fnDictionary.checkAndInsert( immunityPeriod );
          self._simulateTransition := true;
        end
      else
        appendToPstring( errMsg, tr( 'Disease XML is missing a function for the natural immune period.' ) )
      ;

      // Prevalence of infected animals
      //--------------------------------
      e := sdew.GetElementByName( model, 'prevalence' );
      if( nil <> e ) then prevalenceInfectedChart := createRelFromXml( e, sdew );
      if( nil <> prevalenceInfectedChart ) then
        begin
          if ( prevalenceInfectedChart.name = '' ) then
            prevalenceInfectedChart.name := self._prodTypeDescr + ' prevalence infected'
          ;
          dbcout( prevalenceInfectedChart.name, DBSHOWMSG );
          prevalenceInfectedChart.dbField := word( DPrevInfected );
          self.relPrevInfectedName := fnDictionary.checkAndInsert( prevalenceInfectedChart );
          self._simulateTransition := true;
          (self.sim as TSMSimulationInput).useWithinHerdPrevalence := true;
        end
      ;

      // Prevalence of shedding animals
      //-------------------------------
       e := sdew.GetElementByName( model, 'prevalence-infectious' );
      if( nil <> e ) then prevalenceSheddingChart := createRelFromXml( e, sdew );
      if( nil <> prevalenceSheddingChart ) then
        begin
          if ( prevalenceSheddingChart.name = '' ) then
            prevalenceSheddingChart.name := self._prodTypeDescr + ' prevalence shedding'
          ;
          dbcout( prevalenceSheddingChart.name, DBSHOWMSG );
          prevalenceSheddingChart.dbField := word( DPrevShedding );
          self.relPrevSheddingName := fnDictionary.checkAndInsert( prevalenceSheddingChart );
          self._simulateTransition := true;
          (self.sim as TSMSimulationInput).useWithinHerdPrevalence := true;
        end
      ;

      // Probability of mortality
      //-------------------------
      e := Sdew.GetElementByName( model, 'mortality' );
      if ( nil = e ) then
        appendToPString( errMsg, tr( 'Disease model XML does not include a valid probability of mortality.' ) )
      else
        begin
          val := usStrToFloat( Sdew.GetElementContents( e ), NaN );
          if( not isProbability( val ) ) then
            appendToPString( errMsg, tr( 'Disease model XML does not include a valid probability of mortality.' ) )
          else
            self.probMortality := val
          ;
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------


end.