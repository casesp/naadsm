unit TracingParams;

(*
TracingParams.pas
------------------
Begin: 2008/04/23
Last revision: $Date: 2011-10-19 16:50:40 $ $Author: areeves $
Version number: $Revision: 1.11.6.9 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2008 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QLists,
    QStringMaps,

    Sdew,

    ChartFunction,
    ProbDensityFunctions,

    FunctionEnums,
    SMDatabase,
    Models,
    DestructionParams,
    TestingParams
  ;

  type TTracingParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;

      // Properties
      //-------------
      _prodTypeDescr: string;
      
      // Tracing parameters
      //-------------------
      _traceDirectContactForward: boolean;
      _traceIndirectContactForward: boolean;
      _traceDirectContactBack: boolean;
      _traceIndirectContactBack: boolean;

      _directTracePeriod: integer;
      _indirectTracePeriod: integer;
      _indirectTraceSuccess: double;
      _directTraceSuccess: double;

      _pdfDelayName: string;

      // Exam parameters
      //----------------
      _examDirectForward: boolean;
      _examDirectForwardMultiplier: double;
      _examIndirectForward: boolean;
      _examIndirectForwardMultiplier: double;
      _examDirectBack: boolean;
      _examDirectBackMultiplier: double;
      _examIndirectBack: boolean;
      _examIndirectBackMultiplier: double;

      // Unique functions for this class
      //--------------------------------
      procedure initialize();

      // XML import
      //-----------
      function getXmlModelList(): TQStringList;
      procedure importTraceModelXml( model: pointer; sdew: TSdew; errMsg: pstring );
      procedure importTraceExamModelXml( model: pointer; sdew: TSdew; errMsg: pstring );
      procedure importContactRecorderXml( model: pointer; sdew: TSdew; errMsg: pstring );

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      // Overridden from TModelWithFunctions
      //------------------------------------
      function getChartSet(): TChartSet; override;

      // Properties
      //-------------
      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;
      
      // For tracing
      //------------
      function getUseTracing(): boolean;

      procedure setTraceDirectContactForward( val: boolean );
      function getTraceDirectContactForward(): boolean;
      procedure setTraceIndirectContactForward( val: boolean );
      function getTraceIndirectContactForward(): boolean;
      procedure setTraceDirectContactBack( val: boolean );
      function getTraceDirectContactBack(): boolean;
      procedure setTraceIndirectContactBack( val: boolean );
      function getTraceIndirectContactBack(): boolean;

      procedure setDirectTracePeriod( val: integer );
      function getDirectTracePeriod(): integer;
      procedure setIndirectTracePeriod( val: integer );
      function getIndirectTracePeriod(): integer;

      function getMaxTracePeriod(): integer;

      procedure setIndirectTraceSuccess( val: double );
      procedure setDirectTraceSuccess( val: double );
      function getIndirectTraceSuccess(): double;
      function getDirectTraceSuccess(): double;

      procedure setPdfDelayName( val: string );
      function getPdfDelayName(): string;
      function getPdfTracingDelay(): TPdf;

      // For exams
      //----------
      function getUseExams(): boolean;

      function getExamDirectForward(): boolean;
      function getExamDirectForwardMultiplier(): double;
      function getExamIndirectForward(): boolean;
      function getExamIndirectForwardMultiplier(): double;
      function getExamDirectBack(): boolean;
      function getExamDirectBackMultiplier(): double;
      function getExamIndirectBack(): boolean;
      function getExamIndirectBackMultiplier(): double;

      procedure setExamDirectForward( val: boolean );
      procedure setExamDirectForwardMultiplier( val: double );
      procedure setExamIndirectForward( val: boolean );
      procedure setExamIndirectForwardMultiplier( val: double );
      procedure setExamDirectBack( val: boolean );
      procedure setExamDirectBackMultiplier( val: double );
      procedure setExamIndirectBack( val: boolean );
      procedure setExamIndirectBackMultiplier( val: double );

    public
      constructor create(sim: TObject; ptDescr: string ); overload;
      constructor create( const src: TTracingParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;
      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function ssXml( const productionTypeID: integer ): string; reintroduce;
      function ssContactRecorderXml( const productionTypeID, maxPeriod: integer ): string;
      function ssExamXml( const productionTypeID: integer; const useTesting: boolean; testingParams: TTestingParams ): string;

      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;

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
      //------------
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;

      // Tracing
      //--------
      property useTracing: boolean read getUseTracing;

      property traceDirectForward: boolean read getTraceDirectContactForward write setTraceDirectContactForward;
      property traceIndirectForward: boolean read getTraceIndirectContactForward write setTraceIndirectContactForward;
      property traceDirectBack: boolean read getTraceDirectContactBack write setTraceDirectContactBack;
      property traceIndirectBack: boolean read getTraceIndirectContactBack write setTraceIndirectContactBack;
      
      property directTracePeriod: integer read getDirectTracePeriod write setDirectTracePeriod;
      property indirectTracePeriod: integer read getIndirectTracePeriod write setIndirectTracePeriod;

      property maxTracePeriod: integer read getMaxTracePeriod;

      property indirectTraceSuccess: double read getIndirectTraceSuccess write setIndirectTraceSuccess;
      property directTraceSuccess: double read getDirectTraceSuccess write setDirectTraceSuccess;

      property pdfTraceDelayName: string read getPdfDelayName write setPdfDelayName;
      property pdfTraceDelay: TPdf read getPdfTracingDelay;

      // Exams
      //------
      property useTracingExam: boolean read getUseExams;

      property examDirectForward: boolean read getExamDirectForward write setExamDirectForward;
      property examDirectForwardMultiplier: double read getExamDirectForwardMultiplier write setExamDirectForwardMultiplier;
      property examIndirectForward: boolean read getExamIndirectForward write setExamIndirectForward;
      property examIndirectForwardMultiplier: double read getExamIndirectForwardMultiplier write setExamIndirectForwardMultiplier;
      property examDirectBack: boolean read getExamDirectBack write setExamDirectBack;
      property examDirectBackMultiplier: double read getExamDirectBackMultiplier write setExamDirectBackMultiplier;
      property examIndirectBack: boolean read getExamIndirectBack write setExamIndirectBack;
      property examIndirectBackMultiplier: double read getExamIndirectBackMultiplier write setExamIndirectBackMultiplier;
    end
  ;

implementation

  uses
    StrUtils,
    SysUtils,
    Variants,
    Math,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    FunctionDictionary,
    SMSimulationInput
  ;
  
  const DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit.
  
//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TTracingParams.create( sim: TObject; ptDescr: string );
    begin
      inherited create();
      initialize();
  
      _sim := sim;
      _prodTypeDescr := ptDescr;
    end
  ;
  
  
  constructor TTracingParams.create( const src: TTracingParams; sim: TObject );
    begin
      inherited create( src );
      _xmlModelList := nil;
      _sim := sim;
      prodTypeDescr := src.prodTypeDescr;

      // Tracing
      _traceDirectContactForward := src._traceDirectContactForward;
      _traceIndirectContactForward := src._traceIndirectContactForward;
      _traceDirectContactBack := src._traceDirectContactBack;
      _traceIndirectContactBack := src._traceIndirectContactBack;
        
      _directTracePeriod := src._directTracePeriod;
      _indirectTracePeriod := src._indirectTracePeriod;

      _directTraceSuccess := src._directTraceSuccess;
      _indirectTraceSuccess := src._indirectTraceSuccess;

      setPdfDelayName( src._pdfDelayName );

      // Exams
      _examDirectForward := src._examDirectForward;
      _examDirectForwardMultiplier := src._examDirectForwardMultiplier;
      _examIndirectForward := src._examIndirectForward;
      _examIndirectForwardMultiplier := src._examIndirectForwardMultiplier;
      _examDirectBack := src._examDirectBack;
      _examDirectBackMultiplier := src._examDirectBackMultiplier;
      _examIndirectBack := src._examIndirectBack;
      _examIndirectBackMultiplier := src._examIndirectBackMultiplier;

      _updated := src._updated;
    end
  ;
    
    
  constructor TTracingParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
    var
      q: string;
      db2: TSqlDatabase;
      res: TSqlResult;
      row: TSqlRow;
    begin
      inherited create();
      initialize();

      prodTypeDescr := prodTypeName;
      _sim := sim;

      db2 := db as TSqlDatabase;

      q := 'SELECT'
        // Tracing
        + ' `inProductionType`.`traceDirectForward`,'
        + ' `inProductionType`.`traceIndirectForward`,'
        + ' `inProductionType`.`traceDirectBack`,'
        + ' `inProductionType`.`traceIndirectBack`,'

        + ' `inProductionType`.`traceDirectSuccess`,'
        + ' `inProductionType`.`traceDirectTracePeriod`,'
        + ' `inProductionType`.`traceIndirectSuccess`,'
        + ' `inProductionType`.`traceIndirectTracePeriod`,'

        + ' `inProductionType`.`traceDelayPdfID`,'
        + ' `traceDelayChart`.`chartName` AS traceDelayChartName,'

        // Exams
        + ' `inProductionType`.`examDirectForward`,'
        + ' `inProductionType`.`examDirectForwardMultiplier`,'
        + ' `inProductionType`.`examIndirectForward`,'
        + ' `inProductionType`.`examIndirectForwardMultiplier`,'
        + ' `inProductionType`.`examDirectBack`,'
        + ' `inProductionType`.`examDirectBackMultiplier`,'
        + ' `inProductionType`.`examIndirectBack`,'
        + ' `inProductionType`.`examIndirectBackMultiplier`'

        + ' FROM inProductionType'
        + ' LEFT OUTER JOIN inChart traceDelayChart'
        + ' ON traceDelayChart.chartID = inProductionType.traceDelayPdfID'
        + ' WHERE productionTypeID = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      // Tracing
      if( null <> row.field('traceDirectForward') ) then
        traceDirectForward := boolean( row.field('traceDirectForward') )
      else
        traceDirectForward := false
      ;

      if( null <> row.field('traceIndirectForward') ) then
        traceIndirectForward := boolean( row.field('traceIndirectForward') )
      else
        traceIndirectForward := false
      ;

      if( null <> row.field('traceDirectBack') ) then
        traceDirectBack := boolean( row.field('traceDirectBack') )
      else
        traceDirectBack := false
      ;

      if( null <> row.field('traceIndirectBack') ) then
        traceIndirectBack := boolean( row.field('traceIndirectBack') )
      else
        traceIndirectBack := false
      ;

      if( null <> row.field('traceDirectSuccess') ) then
        directTraceSuccess := double( row.field('traceDirectSuccess') )
      ;
      if( null <> row.field('traceDirectTracePeriod') ) then
        directTracePeriod := integer( row.field('traceDirectTracePeriod') )
      ;
      if( null <> row.field('traceIndirectSuccess') ) then
        indirectTraceSuccess := double( row.field('traceIndirectSuccess') )
      ;
      if( null <> row.field('traceIndirectTracePeriod') ) then
        indirectTracePeriod := integer( row.field('traceIndirectTracePeriod') )
      ;

      if( null <> row.field('traceDelayPdfID') ) then
        begin
          setPdfDelayName( row.field( 'traceDelayChartName' ) );
        end
      ;

      // Exams
      if( null <> row.field('examDirectForward') ) then
        examDirectForward := boolean( row.field('examDirectForward') )
      else
        examDirectForward := false
      ;
      if( null <> row.field('examIndirectForward') ) then
        examIndirectForward := boolean( row.field('examIndirectForward') )
      else
        examIndirectForward := false
      ;
      if( null <> row.field('examDirectBack') ) then
        examDirectBack := boolean( row.field('examDirectBack') )
      else
        examDirectBack := false
      ;
      if( null <> row.field('examIndirectBack') ) then
        examIndirectBack := boolean( row.field('examIndirectBack') )
      else
        examIndirectBack := false
      ;

      if( null <> row.field('examDirectForwardMultiplier') ) then
        examDirectForwardMultiplier := double( row.field('examDirectForwardMultiplier') )
      ;
      if( null <> row.field('examIndirectForwardMultiplier') ) then
        examIndirectForwardMultiplier := double( row.field('examIndirectForwardMultiplier') )
      ;
      if( null <> row.field('examDirectBackMultiplier') ) then
        examDirectBackMultiplier := double( row.field('examDirectBackMultiplier') )
      ;
      if( null <> row.field('examIndirectBackMultiplier') ) then
        examIndirectBackMultiplier := double( row.field('examIndirectBackMultiplier') )
      ;

      freeAndNil( res );

      _updated := false;
    end
  ;


  procedure TTracingParams.initialize();
    begin
      _xmlModelList := nil;

      _updated := false;
      _prodTypeDescr := '';

      // Tracing
      //--------
      _traceDirectContactForward := false;
      _traceIndirectContactForward := false;
      _traceDirectContactBack := false;
      _traceIndirectContactBack := false;

      _directTracePeriod := 0;
      _directTraceSuccess := 0.0;

      _indirectTracePeriod := 0;
      _indirectTraceSuccess := 0.0;

      setPdfDelayName( '' );

      // Exams
      //------
      _examDirectForward := false;
      _examDirectForwardMultiplier := 1.0;
      _examIndirectForward := false;
      _examIndirectForwardMultiplier := 1.0;
      _examDirectBack := false;
      _examDirectBackMultiplier := 1.0;
      _examIndirectBack := false;
      _examIndirectBackMultiplier := 1.0;
    end
  ;


  destructor TTracingParams.destroy();
    begin
      freeAndNil( _xmlModelList );

      // The function dictionary is freed elsewhere.
      // PDFs are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfDelayName( '' );

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------  
  
  
//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TTracingParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
    var
      q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      // Tracing
      dict['traceDirectForward'] := db.sqlBool( traceDirectForward );
      dict['traceIndirectForward'] := db.sqlBool( traceIndirectForward );
      dict['traceDirectBack'] := db.sqlBool( traceDirectBack );
      dict['traceIndirectBack'] := db.sqlBool( traceIndirectBack );
      
      dict['traceDirectSuccess'] := usFloatToStr( directTraceSuccess );
      dict['traceDirectTracePeriod'] := intToStr( directTracePeriod );

      dict['traceIndirectSuccess'] := usFloatToStr( indirectTraceSuccess );
      dict['traceIndirectTracePeriod'] := intToStr( indirectTracePeriod );

      if( nil <> pdfTraceDelay ) then
      	dict['traceDelayPdfID'] := intToStr( pdfTraceDelay.id )
      else
     		dict['traceDelayPdfID'] := DATABASE_NULL_VALUE
      ;

    // Exams
    dict['examDirectForward'] := db.sqlBool( examDirectForward );
    dict['examDirectForwardMultiplier'] := usFloatToStr( examDirectForwardMultiplier );
    dict['examIndirectForward'] := db.sqlBool( examIndirectForward );
    dict['examIndirectForwardMultiplier'] := usFloatToStr( examIndirectForwardMultiplier );
    dict['examDirectBack'] := db.sqlBool( examDirectBack );
    dict['examDirectBackMultiplier'] := usFloatToStr( examDirectBackMultiplier );
    dict['examIndirectBack'] := db.sqlBool( examIndirectBack );
    dict['examIndirectBackMultiplier'] := usFloatToStr( examIndirectBackMultiplier );

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
// PDF properties
//-----------------------------------------------------------------------------
  procedure TTracingParams.setPdfDelayName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _pdfDelayName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _pdfDelayName := val;
      _updated := true;
    end
  ;

  function TTracingParams.getPdfDelayName(): string; begin result := _pdfDelayName; end;


  function TTracingParams.getPdfTracingDelay(): TPdf;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _pdfDelayName ) ) then
            begin
              if( ( fnDictionary.value( _pdfDelayName ) as TFunctionDictionaryItem ).fn is TPdf ) then
                result := ( fnDictionary.value( _pdfDelayName ) as TFunctionDictionaryItem ).fn as TPdf
              else
                begin
                  setPdfDelayName( '' );
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
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Overridden from TModelWithFunctions
//-----------------------------------------------------------------------------
  function TTracingParams.getChartSet(): TChartSet;
    begin
      result := [ TrDelay ];
    end
  ;


  procedure TTracingParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        TrDelay: self.pdfTraceDelayName := newName;
      end;
    end
  ;


  function TTracingParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            TrDelay:
              if ( self.fnDictionary.contains( self.pdfTraceDelayName ) ) then
                result := self.fnDictionary.value( self.pdfTraceDelayName ).fn
              ;
          end;
        end
      ;
    end
  ;


  procedure TTracingParams.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfTraceDelayName ) then self.pdfTraceDelayName := '';
    end
  ;


  function TTracingParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;

      case whichChart of
        TrDelay: result := ( chartName = self.pdfTraceDelayName );
      end;
    end
  ;


  function TTracingParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _pdfDelayName ) ) then
        begin
          if( not( ( fnDictionary.value( _pdfDelayName ) as TFunctionDictionaryItem ).fn is TPdf ) ) then
            begin
              setPdfDelayName( '' );
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
  function TTracingParams.validate( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;
    begin
      result := true;
      msg := '';
      submsg := '';

      // Tracing
      //--------
      // Validation of tracing parameters:
      // 0 <= probabilities of success <= 1
      // Trace period must be set
      if( traceDirectForward or traceDirectBack ) then
        begin
          if( ( 0.0 > directTraceSuccess ) or ( 1.0 < directTraceSuccess ) ) then
            begin
              if( nil <> err ) then
                msg := msg + '    ' + tr( 'Probability of direct trace success must be between 0 and 1.' ) + endl
              ;
              result := false;
            end
          ;

          if( 0 >= directTracePeriod ) then
            begin
              if( nil <> err ) then
                msg := msg + '  ' + tr( 'The critical period for traces of direct contact has not been set.' ) + endl
              ;
              result := false;
            end
          ;
        end
      ;

      if( traceIndirectForward or traceIndirectBack ) then
        begin
          if( ( 0.0 > indirectTraceSuccess ) or ( 1.0 < indirectTraceSuccess ) ) then
            begin
              if( nil <> err ) then
                msg := msg + '    ' + tr( 'Probability of indirect trace success must be between 0 and 1.' ) + endl
              ;
              result := false;
            end
          ;

          if( 0 >= indirectTracePeriod ) then
            begin
              if( nil <> err ) then
                msg := msg + '  ' + tr( 'The critical period for traces of indirect contact has not been set.' ) + endl
              ;
              result := false;
            end
          ;
        end
      ;

      // If any tracing is used, the delay PDF must be specified
      if( useTracing ) then
        begin
          if( nil = pdfTraceDelay ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Delay for tracing is not set.' ) + endl;
              result := false;
            end
          else if( not( pdfTraceDelay.validate( @submsg ) ) ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Delay for tracing is not valid:' ) + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      // Herd exams
      //-----------
      if( examDirectForward ) then
        begin
          if( 0.0 > examDirectForwardMultiplier ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Detection multiplier for forward traces of direct contact is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( examIndirectForward ) then
        begin
          if( 0.0 > examIndirectForwardMultiplier ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Detection multiplier for forward traces of indirect contact is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( examDirectBack ) then
        begin
          if( 0.0 > examDirectBackMultiplier ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Detection multiplier for trace back of direct contacts is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;

      if( examIndirectBack ) then
        begin
          if( 0.0 > examIndirectBackMultiplier ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Detection multiplier for trace back of indirect contacts is not set.' ) + endl;
              result := false;
            end
          ;
        end
      ;


      if( ( result = false ) and ( nil <> err ) ) then
        begin
          //msg := endl + ansiReplaceStr( tr( 'Tracing parameters for xyz:' ), 'xyz', prodTypeDescr ) + endl + msg;
          msg := endl + msg;
          err^ := err^ + msg;
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TTracingParams.debug();
    begin
      dbcout( '---------TRACING PARAMETERS FOR "' + prodTypeDescr + '"' + endl, true );

      // Tracing
      //--------
      if( traceDirectForward ) then
        dbcout( 'Direct forward trace destruction: period ' + intToStr( directTracePeriod )
          + ', success rate ' + usFloatToStr( directTraceSuccess )
          + endl, true )
      else
        dbcout( 'Direct forward trace destruction: FALSE', true )
      ;

      if( traceIndirectForward ) then
        dbcout( 'Indirect forward trace destruction: period ' + intToStr( indirectTracePeriod )
          + ', success rate ' + usFloatToStr( indirectTraceSuccess )
          + endl, true )
      else
        dbcout( 'Indirect forward trace destruction: FALSE', true )
      ;

      if( traceDirectBack ) then
        dbcout( 'Direct back trace destruction: period ' + intToStr( directTracePeriod )
          + ', success rate ' + usFloatToStr( directTraceSuccess )
          + endl, true )
      else
        dbcout( 'Direct back trace destruction: FALSE', true )
      ;

      if( traceIndirectBack ) then
        dbcout( 'Indirect back trace destruction: period ' + intToStr( indirectTracePeriod )
          + ', success rate ' + usFloatToStr( indirectTraceSuccess )
          + endl, true )
      else
        dbcout( 'Indirect back trace destruction: FALSE', true )
      ;

      if( nil <> pdfTraceDelay ) then
        begin
          dbcout( 'Period of trace result delay:', true );
          pdfTraceDelay.debug();
        end
      else
        dbcout( 'PERIOD OF TRACE RESULT DELAY IS UNSPECIFIED', true )
      ;

      // Exams
      //------
      if( examDirectForward ) then
        dbcout( 'Detection multiplier for trace forward of direct contacts: ' + usFloatToStr( examDirectForwardMultiplier ), true )
      else
        dbcout( 'Exams of trace forward of direct contacts NOT used, but multiplier would be ' + usFloatToStr( examDirectForwardMultiplier ), true )
      ;
      if( examIndirectForward ) then
        dbcout( 'Detection multiplier for trace forward of indirect contacts: ' + usFloatToStr( examIndirectForwardMultiplier ), true )
      else
        dbcout( 'Exams of trace forward of indirect contacts NOT used, but multiplier would be ' + usFloatToStr( examIndirectForwardMultiplier ), true )
      ;
      if( examDirectBack ) then
        dbcout( 'Detection multiplier for trace Back of direct contacts: ' + usFloatToStr( examDirectBackMultiplier ), true )
      else
        dbcout( 'Exams of trace Back of direct contacts NOT used, but multiplier would be ' + usFloatToStr( examDirectBackMultiplier ), true )
      ;
      if( examIndirectBack ) then
        dbcout( 'Detection multiplier for trace Back of indirect contacts: ' + usFloatToStr( examIndirectBackMultiplier ), true )
      else
        dbcout( 'Exams of trace Back of indirect contacts NOT used, but multiplier would be ' + usFloatToStr( examIndirectBackMultiplier ), true )
      ;

      dbcout( '---------END TRACING PARAMETERS' + endl, true );
    end
  ;
//-----------------------------------------------------------------------------  


//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TTracingParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TTracingParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  procedure TTracingParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;

  function TTracingParams.getUpdated(): boolean;
    begin
      result :=
        _updated
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfDelayName )
      ;
    end
  ;


  // Tracing
  //--------
  function TTracingParams.getUseTracing(): boolean;
    begin
      result :=
        traceDirectForward
      or
        traceIndirectForward
      or
        traceDirectBack
      or
        traceIndirectBack
      ;
    end
  ;

  function TTracingParams.getTraceDirectContactForward(): boolean; begin Result := _traceDirectContactForward; end;
  function TTracingParams.getTraceIndirectContactForward(): boolean; begin Result := _traceIndirectContactForward; end;
  function TTracingParams.getTraceDirectContactBack(): boolean; begin Result := _traceDirectContactBack; end;
  function TTracingParams.getTraceIndirectContactBack(): boolean; begin Result := _traceIndirectContactBack; end;

  function TTracingParams.getDirectTracePeriod(): integer; begin Result := _directTracePeriod; end;
  function TTracingParams.getIndirectTracePeriod(): integer; begin Result := _indirectTracePeriod; end;

  function TTracingParams.getMaxTracePeriod(): integer;
    begin
      result := max( directTracePeriod, indirectTracePeriod );
    end
  ;

  function TTracingParams.getIndirectTraceSuccess(): double; begin Result := _indirectTraceSuccess; end;
  function TTracingParams.getDirectTraceSuccess(): double; begin Result := _directTraceSuccess; end;

  procedure TTracingParams.setTraceDirectContactForward( val: boolean );
    begin
      _traceDirectContactForward := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setTraceIndirectContactForward( val: boolean );
    begin
      _traceIndirectContactForward := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setTraceDirectContactBack( val: boolean );
    begin
      _traceDirectContactBack := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setTraceIndirectContactBack( val: boolean );
    begin
      _traceIndirectContactBack := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setDirectTracePeriod( val: integer );
    begin
      _directTracePeriod := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setIndirectTracePeriod( val: integer );
    begin
      _indirectTracePeriod := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setIndirectTraceSuccess( val: double );
    begin
      _indirectTraceSuccess := val;
      _updated := true;
    end
  ;

  procedure TTracingParams.setDirectTraceSuccess( val: double );
    begin
      _directTraceSuccess := val;
      _updated := true;
    end
  ;


  // Exams
  //------
  function TTracingParams.getUseExams(): boolean;
    begin
      result :=
        examDirectForward
      or
        examIndirectForward
      or
        examDirectBack
      or
        examIndirectBack
      ;
    end
  ;

  procedure TTracingParams.setExamDirectForward( val: boolean ); begin _examDirectForward := val; _updated := true; end;
  procedure TTracingParams.setExamDirectForwardMultiplier( val: double ); begin _examDirectForwardMultiplier := val; _updated := true; end;
  procedure TTracingParams.setExamIndirectForward( val: boolean ); begin _examIndirectForward := val; _updated := true; end;
  procedure TTracingParams.setExamIndirectForwardMultiplier( val: double ); begin _examIndirectForwardMultiplier := val; _updated := true; end;
  procedure TTracingParams.setExamDirectBack( val: boolean ); begin _examDirectBack := val; _updated := true; end;
  procedure TTracingParams.setExamDirectBackMultiplier( val: double ); begin _examDirectBackMultiplier := val; _updated := true; end;
  procedure TTracingParams.setExamIndirectBack( val: boolean ); begin _examIndirectBack := val; _updated := true; end;
  procedure TTracingParams.setExamIndirectBackMultiplier( val: double ); begin _examIndirectBackMultiplier := val; _updated := true; end;

  function TTracingParams.getExamDirectForward(): boolean; begin Result := _examDirectForward; end;
  function TTracingParams.getExamDirectForwardMultiplier(): double; begin Result := _examDirectForwardMultiplier; end;
  function TTracingParams.getExamIndirectForward(): boolean; begin Result := _examIndirectForward; end;
  function TTracingParams.getExamIndirectForwardMultiplier(): double; begin Result := _examIndirectForwardMultiplier; end;
  function TTracingParams.getExamDirectBack(): boolean; begin Result := _examDirectBack; end;
  function TTracingParams.getExamDirectBackMultiplier(): double; begin Result := _examDirectBackMultiplier; end;
  function TTracingParams.getExamIndirectBack(): boolean; begin Result := _examIndirectBack; end;
  function TTracingParams.getExamIndirectBackMultiplier(): double; begin Result := _examIndirectBackMultiplier; end;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
  function TTracingParams.ssXml( const productionTypeID: integer ): string;
    begin
      result := '';

      if( traceDirectForward ) then
        begin
          result := result
            + '  <trace-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="out">' + endl
            + '    <trace-period>' + endl
            + '      <value>' + intToStr( directTracePeriod ) + '</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </trace-period>' + endl
            + '  </trace-model>' + endl + endl
          ;

          // In 3.2, quarantine is automatic.  Note that this might change in the future!
          result := result
            + '  <trace-quarantine-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="out"></trace-quarantine-model>' + endl + endl
          ;
        end
      ;

      if( traceIndirectForward ) then
        begin
          result := result
            + '  <trace-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="out">' + endl
            + '    <trace-period>' + endl
            + '      <value>' + intToStr( indirectTracePeriod ) + '</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </trace-period>' + endl
            + '  </trace-model>' + endl + endl
          ;

          // In 3.2, quarantine is automatic.  Note that this might change in the future!
          result := result
            + '  <trace-quarantine-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="out"></trace-quarantine-model>' + endl + endl
          ;
        end
      ;

      if( traceDirectBack ) then
        begin
          result := result
            + '  <trace-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="in">' + endl
            + '    <trace-period>' + endl
            + '      <value>' + intToStr( directTracePeriod ) + '</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </trace-period>' + endl
            + '  </trace-model>' + endl + endl
          ;

          // In 3.2, quarantine is automatic.  Note that this might change in the future!
          result := result
            + '  <trace-quarantine-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="in"></trace-quarantine-model>' + endl + endl
          ;
        end
      ;

      if( traceIndirectBack ) then
        begin
          result := result
            + '  <trace-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="in">' + endl
            + '    <trace-period>' + endl
            + '      <value>' + intToStr( indirectTracePeriod ) + '</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </trace-period>' + endl
            + '  </trace-model>' + endl + endl
          ;

          // In 3.2, quarantine is automatic.  Note that this might change in the future!
          result := result
            + '  <trace-quarantine-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="in"></trace-quarantine-model>' + endl + endl
          ;
        end
      ;
    end
  ;


  function TTracingParams.ssContactRecorderXml( const productionTypeID, maxPeriod: integer ): string;
    begin
      result := '';

      if( traceDirectForward or traceDirectBack ) then
        begin
          result := result
            + '  <contact-recorder-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="direct">' + endl
            + '    <trace-period>' + endl
            + '      <value>' + intToStr( maxPeriod ) + '</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </trace-period>' + endl
            + '    <trace-success>' + usFloatToStr( directTraceSuccess ) + '</trace-success>' + endl
            + '    <trace-delay>' + endl
          ;

          result := result + pdfTraceDelay.ssXml( 3 );

          result := result
            + '    </trace-delay>' + endl
            + '  </contact-recorder-model>' + endl + endl
          ;
        end
      ;

      if( traceIndirectForward or traceIndirectBack ) then
        begin
          result := result
            + '  <contact-recorder-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '" contact-type="indirect">' + endl
            + '    <trace-period>' + endl
            + '      <value>' + intToStr( maxPeriod ) + '</value>' + endl
            + '      <units><xdf:unit>day</xdf:unit></units>' + endl
            + '    </trace-period>' + endl
            + '    <trace-success>' + usFloatToStr( indirectTraceSuccess ) + '</trace-success>' + endl
            + '    <trace-delay>' + endl
          ;

          result := result + pdfTraceDelay.ssXml( 3 );

          result := result
            + '    </trace-delay>' + endl
            + '  </contact-recorder-model>' + endl + endl
          ;
        end
      ;
    end
  ;


  function TTracingParams.ssExamXml( const productionTypeID: integer; const useTesting: boolean; testingParams: TTestingParams ): string;
    begin
      result := '';

      if( examDirectForward ) then
        begin
          result := result
            + '  <trace-exam-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="out">' + endl
            + '    <detection-multiplier>' + usFloatToStr( examDirectForwardMultiplier ) + '</detection-multiplier>' + endl
          ;

          if( useTesting and testingParams.testDirectForward ) then
            result := result + '    <test-if-no-signs>true</test-if-no-signs>' + endl
          else
            result := result + '    <test-if-no-signs>false</test-if-no-signs>' + endl
          ;

          result := result  + '  </trace-exam-model>' + endl + endl;
        end
      ;

      if( examIndirectForward ) then
        begin
          result := result
            + '  <trace-exam-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="out">' + endl
            + '    <detection-multiplier>' + usFloatToStr( examIndirectForwardMultiplier ) + '</detection-multiplier>' + endl
          ;

          if( useTesting and testingParams.testIndirectForward ) then
            result := result + '    <test-if-no-signs>true</test-if-no-signs>' + endl
          else
            result := result + '    <test-if-no-signs>false</test-if-no-signs>' + endl
          ;

          result := result  + '  </trace-exam-model>' + endl + endl;
        end
      ;

      if( examDirectBack ) then
        begin
          result := result
            + '  <trace-exam-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="direct" direction="in">' + endl
            + '    <detection-multiplier>' + usFloatToStr( examDirectBackMultiplier ) + '</detection-multiplier>' + endl
          ;

          if( useTesting and testingParams.testDirectBack ) then
            result := result + '    <test-if-no-signs>true</test-if-no-signs>' + endl
          else
            result := result + '    <test-if-no-signs>false</test-if-no-signs>' + endl
          ;

          result := result  + '  </trace-exam-model>' + endl + endl;
        end
      ;

     if( examIndirectBack ) then
        begin
          result := result
            + '  <trace-exam-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '"'
            + ' contact-type="indirect" direction="in">' + endl
            + '    <detection-multiplier>' + usFloatToStr( examIndirectBackMultiplier ) + '</detection-multiplier>' + endl
          ;

          if( useTesting and testingParams.testIndirectBack ) then
            result := result + '    <test-if-no-signs>true</test-if-no-signs>' + endl
          else
            result := result + '    <test-if-no-signs>false</test-if-no-signs>' + endl
          ;

          result := result  + '  </trace-exam-model>' + endl + endl;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------  


//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TTracingParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'trace-model' );
      result.Append( 'trace-quarantine-model' );
      result.Append( 'trace-exam-model' );
      result.Append( 'contact-recorder-model' );
    end
  ;


  function TTracingParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TTracingParams.importTraceModelXml( model: pointer; sdew: TSdew; errMsg: pstring );
    var
      contactType: String;
      contactDirection: String;
      tracePeriod: Integer;
      subElement: Pointer;
    begin
      contactType :=  Sdew.GetElementAttribute( model, 'contact-type' );
      contactDirection :=  Sdew.GetElementAttribute( model, 'direction' );

      if( ( contactType <> 'direct' ) and ( contactType <> 'indirect' ) ) then
        begin
          appendToPstring( errMsg, tr( 'Tracing XML includes an invalid contact type.' ) );
          exit;
        end
      ;

      if ( contactDirection <> 'in' )  and ( contactDirection <> 'out') then
        begin
          appendToPstring( errMsg, tr( 'Tracing XML includes an invalid contact direction.' ) );
          exit;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'trace-period' );
      if ( nil <> subElement ) then
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );

          if ( nil <> subElement ) then
            begin
              tracePeriod := myStrToInt( Sdew.GetElementContents( subElement ), -1 );

              if( -1 = tracePeriod ) then
                appendToPstring( errMsg, tr( 'Tracing XML has an invalid trace period.' ) )
              else if ( contactType = 'direct' ) then
                begin
                  self.directTracePeriod := tracePeriod;

                  if ( contactDirection = 'in' ) then
                    self.traceDirectBack := true
                  else if( contactDirection = 'out' ) then
                    self.traceDirectForward := true
                  else
                    raise exception.create( 'Someone forgot something in TTracingParams.importTraceModelXml' )
                  ;
                end
              else if ( contactType = 'indirect') then
                begin
                  self.indirectTracePeriod := tracePeriod;

                  if( contactDirection = 'in' ) then
                    self.traceIndirectBack := true
                  else if( contactDirection = 'out' ) then
                    self.traceIndirectForward := true
                  else
                    raise exception.create( 'Someone forgot something in TTracingParams.importTraceModelXml' )
                  ;
                end
              else
                raise exception.Create( 'Someone forgot something in TTracingParams.importTraceModelXml' )
              ;
            end
          else
            appendToPstring( errMsg, tr( 'Tracing XML has an invalid trace period.' ) )
          ;
        end
      else
        appendToPstring( errMsg, tr( 'Tracing XML is missing a trace period.' ) )
      ;
    end
  ;


  procedure TTracingParams.importContactRecorderXml( model: pointer; sdew: TSdew; errMsg: pstring );
    var
      contactType: string;
      tracePeriod: integer;
      traceSuccess: double;
      subElement: pointer;
      traceDelay: TPdf;
    begin
      contactType :=  Sdew.GetElementAttribute( model, 'contact-type' );

      if( ( contactType <> 'direct' )  and ( contactType <> 'indirect' ) ) then
        begin
          appendToPstring( errMsg, tr( 'Contact recorder XML includes an invalid contact type.' ) );
          exit;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'trace-period' );
      if( nil = subElement ) then
        appendToPstring( errMsg, tr( 'Contact recorder XML is missing a trace period.' ) )
      else
        begin
          subElement := Sdew.GetElementByName( subElement, 'value' );

          if( nil = subElement ) then
            appendToPstring( errMsg, tr( 'Contact recorder XML has an invalid trace period.' ) )
          else
            begin
              tracePeriod := myStrToInt( Sdew.GetElementContents( subElement ), -1 );

              if( -1 = tracePeriod ) then
                appendToPstring( errMsg, tr( 'Contact recorder XML has an invalid trace period.' ) )
              else if ( contactType = 'direct' ) then
                self.directTracePeriod := tracePeriod
              else if ( contactType = 'indirect') then
                self.indirectTracePeriod := tracePeriod
              else
                raise exception.Create( 'Someone forgot something in TTracingParams.importContactRecorderXml()' );
              ;
            end
          ;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'trace-success' );
      if( nil = subElement ) then
        appendToPstring( errMsg, tr( 'Contact recorder XML is missing trace success.' ) )
      else
        begin
          traceSuccess := usStrToFloat( Sdew.GetElementContents( subElement ), -1.0 );

          if( ( traceSuccess < 0.0 ) or ( traceSuccess > 1.0 ) ) then
            appendToPstring( errMsg, tr( 'Contact recorder XML has an invalid value for trace success.' ) )
          else if ( contactType = 'direct' ) then
            self.directTraceSuccess := traceSuccess
          else if ( contactType = 'indirect') then
            self.indirectTraceSuccess := traceSuccess
          else
            raise exception.Create( 'Someone forgot something in TTracingParams.importContactRecorderXml()' )
          ;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'trace-delay' );
      if ( nil = subElement ) then
        appendToPstring( errMsg, tr( 'Contact recorder XML is missing a function for trace delay.' ) )
      else
        begin
          traceDelay := createPdfFromXml( subElement, Sdew );
          if( nil = traceDelay ) then
            appendToPstring( errMsg, tr( 'Contact recorder XML does not contain a valid function for trace delay.' ) )
          else
            begin
              if( '' = traceDelay.name ) then
                traceDelay.name := 'Tracing delay'
              ;
              traceDelay.dbField := word( TrDelay );
              self.pdfTraceDelayName := fnDictionary.checkAndInsert( traceDelay );
            end
          ;
        end
      ;
    end
  ;


  procedure TTracingParams.importTraceExamModelXml( model: pointer; sdew: TSdew; errMsg: pstring );
    var
      contactType: string;
      contactDirection: string;
      multiplier: double;
      subElement: pointer;
    begin
      contactType :=  Sdew.GetElementAttribute( model, 'contact-type' );
      contactDirection :=  Sdew.GetElementAttribute( model, 'direction' );

      if( ( 'direct' <> contactType )  and ( 'indirect' <> contactType ) ) then
        begin
          appendToPstring( errMsg, tr( 'Trace exam XML includes an invalid contact type.' ) );
          exit;
        end
      ;

      if( ( 'in' <> contactDirection )  and ( 'out' <> contactDirection ) ) then
        begin
          appendToPstring( errMsg, tr( 'Trace exam XML includes an invalid contact direction.' ) );
          exit;
        end
      ;

      subElement := sdew.GetElementByName( model, 'detection-multiplier' );

      if( nil = subElement ) then
        begin
          appendToPstring( errMsg, tr( 'Trace exam XML does not contain a valid detection multiplier.' ) );
          exit;
        end
      else
        begin
          multiplier := usStrToFloat( sdew.GetElementContents( subElement ), NaN );

          if( isNaN( multiplier ) ) then
            appendToPstring( errMsg, tr( 'Trace exam XML does not contain a valid detection multiplier.' ) )
          else
            begin
              if( 'direct' = contactType ) then
                begin
                  if( 'in' = contactDirection ) then
                    begin
                      self.examDirectBack := true;
                      self.examDirectBackMultiplier := multiplier;
                    end
                  else if( 'out' = contactDirection ) then
                    begin
                      self.examDirectForward := true;
                      self.examDirectForwardMultiplier := multiplier;
                    end
                  else
                    raise exception.Create( 'Someone forgot something in TTracingParams.importTraceExamModelXml()' )
                  ;
                end
              else if( 'indirect' = contactType ) then
                begin
                  if( 'in' = contactDirection ) then
                    begin
                      self.examIndirectBack := true;
                      self.examIndirectBackMultiplier := multiplier;
                    end
                  else if( 'out' = contactDirection ) then
                    begin
                      self.examIndirectForward := true;
                      self.examIndirectForwardMultiplier := multiplier;
                    end
                  else
                    raise exception.Create( 'Someone forgot something in TTracingParams.importTraceExamModelXml()' )
                  ;
                end
              else
                raise exception.Create( 'Someone forgot something in TTracingParams.importTraceExamModelXml()' )
              ;
            end
          ;
        end
      ;
    end
  ;


  procedure TTracingParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    begin
      if( 'trace-model' = sdew.GetElementName( model ) ) then
        importTraceModelXml( model, sdew, errMsg )
      ;
      if( 'trace-quarantine-model' <> sdew.GetElementName( model ) ) then
        // Quarantine is currently automatic, so there's nothing to import
      ;
      if( 'contact-recorder-model' = sdew.GetElementName( model ) ) then
        importContactRecorderXml( model, sdew, errMsg )
      ;
      if( 'trace-exam-model' = sdew.GetElementName( model ) ) then
        importTraceExamModelXml( model, sdew, errMsg )
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
