unit TestingParams;

(*
TestingParams.pas
-----------------
Begin: 2008/04/25
Last revision: $Date: 2011-10-19 01:30:22 $ $Author: areeves $
Version number: $Revision: 1.11.6.5 $
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
    Models
  ;

  type TTestingParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;

      // Properties
      //-------------
      _prodTypeDescr: string;

      // Properties of the test itself
      //------------------------------
      _specificity: double;
      _sensitivity: double;

      _pdfDelayName: string;

      // Properties of the testing strategy
      //-----------------------------------
      _testDirectForward: boolean;
      _testIndirectForward: boolean;
      _testDirectBack: boolean;
      _testIndirectBack: boolean;

      // Unique functions for this class
      //--------------------------------
      procedure initialize();

      // XML import
      //-----------
      function getXmlModelList(): TQStringList;
      procedure importTestModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
      procedure importTraceExamModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );

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

      function getUseTesting(): boolean;

      // Properties of the test itself
      //------------------------------
      procedure setSpecificity( val: double );
      procedure setSensitivity( val: double );
      procedure setPdfDelayName( val: string );

      function getSpecificity(): double;
      function getSensitivity(): double;
      function getPdfDelayName(): string;

      function getPdfTestingDelay(): TPdf;

      // Properties of the testing strategy
      //-----------------------------------
      procedure setTestDirectForward( val: boolean );
      procedure setTestIndirectForward( val: boolean );
      procedure setTestDirectBack( val: boolean );
      procedure setTestIndirectBack( val: boolean );

      function getTestDirectForward(): boolean;
      function getTestIndirectForward(): boolean;
      function getTestDirectBack(): boolean;
      function getTestIndirectBack(): boolean;

    public
      constructor create(sim: TObject; ptDescr: string ); overload;
      constructor create( const src: TTestingParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;

      destructor destroy(); override;

      // Overridden from TModel
      //-----------------------
      function ssXml( const productionTypeID: integer ): string; reintroduce;
      function populateDatabase( db: TSMDatabase; ptID: integer ): integer; reintroduce;
      function validate( err: PString = nil ): boolean; override;
      procedure debug(); override;

      // Overridden from TModelWithFunctions
      //------------------------------------
      procedure setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 ); override;
      function chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction; override;
      procedure removeChart( const chartName: string ); override;
(*
      procedure changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      ); override;
*)
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

      property useTesting: boolean read getUseTesting;

      // Properties of the test itself
      //------------------------------
      property specificity: double read getSpecificity write setSpecificity;
      property sensitivity: double read getSensitivity write setSensitivity;

      property pdfTestDelayName: string read getPdfDelayName write setPdfDelayName;
      property pdfTestDelay: TPdf read getPdfTestingDelay;

      // Properties of the testing strategy
      //-----------------------------------
      property testDirectForward: boolean read getTestDirectForward write setTestDirectForward;
      property testIndirectForward: boolean read getTestIndirectForward write setTestIndirectForward;
      property testDirectBack: boolean read getTestDirectBack write setTestDirectBack;
      property testIndirectBack: boolean read getTestIndirectBack write setTestIndirectBack;

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
  constructor TTestingParams.create( sim: TObject; ptDescr: string );
    begin
      inherited create();
      initialize();
  
      _sim := sim;
      _prodTypeDescr := ptDescr;
    end
  ;
  
  
  constructor TTestingParams.create( const src: TTestingParams; sim: TObject );
    begin
      inherited create( src );
      _xmlModelList := nil;
      _sim := sim;
      prodTypeDescr := src.prodTypeDescr;

      _specificity := src._specificity;
      _sensitivity := src._sensitivity;
      setPdfDelayName( src._pdfDelayName );

      _testDirectForward := src._testDirectForward;
      _testIndirectForward := src._testIndirectForward;
      _testDirectBack := src._testDirectBack;
      _testIndirectBack := src._testIndirectBack;

      _updated := src._updated;
    end
  ;


 constructor TTestingParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
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
        + ' `inProductionType`.`testSpecificity`,'
        + ' `inProductionType`.`testSensitivity`,'

        + ' `inProductionType`.`testDirectForward`,'
        + ' `inProductionType`.`testIndirectForward`,'
        + ' `inProductionType`.`testDirectBack`,'
        + ' `inProductionType`.`testIndirectBack`,'

        + ' `inProductionType`.`testDelayPdfID`,'
        + ' `testDelayChart`.`chartName` AS testDelayChartName'

        + ' FROM inProductionType'
        + ' LEFT OUTER JOIN inChart testDelayChart'
        + ' ON testDelayChart.chartID = inProductionType.testDelayPdfID'
        + ' WHERE productionTypeID = ' + intToStr( ptID )
      ;

      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('testDelayPdfID') ) then
        begin
          setPdfDelayName( row.field( 'testDelayChartName' ) );
        end
      ;

      if( null <> row.field('testSensitivity') ) then sensitivity := double( row.field('testSensitivity') );
      if( null <> row.field('testSpecificity') ) then specificity := double( row.field('testSpecificity') );

      if( null <> row.field('testDirectForward') ) then
        testDirectForward := boolean( row.field('testDirectForward') )
      else
        testDirectForward := false
      ;
      if( null <> row.field('testIndirectForward') ) then
        testIndirectForward := boolean( row.field('testIndirectForward') )
      else
        testIndirectForward := false
      ;
      if( null <> row.field('testDirectBack') ) then
        testDirectBack := boolean( row.field('testDirectBack') )
      else
        testDirectBack := false
      ;
      if( null <> row.field('testIndirectBack') ) then
        testIndirectBack := boolean( row.field('testIndirectBack') )
      else
        testIndirectBack := false
      ;

      freeAndNil( res );

      _updated := false;
    end
  ;


    procedure TTestingParams.initialize();
    begin

      _updated := false;
      _prodTypeDescr := '';

      _sensitivity := -1.0;
      _specificity := -1.0;

      setPdfDelayName( '' );

      _xmlModelList := nil;

      _testDirectForward := false;
      _testIndirectForward := false;
      _testDirectBack := false;
      _testIndirectBack := false;
    end
  ;

  destructor TTestingParams.destroy();
    begin
      // The function dictionary is freed elsewhere.
      // PDFs are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setPdfDelayName( '' );
      freeAndNil(_xmlModelList);
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TTestingParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
    var
      q: string;
      dict: TQueryDictionary;
    begin
      dict := TQueryDictionary.create();

      dict['testSensitivity'] := usFloatToStr( sensitivity );
      dict['testSpecificity'] := usFloatToStr( specificity );

      if( nil <> pdfTestDelay ) then
      	dict['testDelayPdfID'] := intToStr( pdfTestDelay.id )
      else
     		dict['testDelayPdfID'] := DATABASE_NULL_VALUE
      ;

      dict['testDirectForward'] := db.sqlBool( testDirectForward );
      dict['testIndirectForward'] := db.sqlBool( testIndirectForward );
      dict['testDirectBack'] := db.sqlBool( testDirectBack );
      dict['testIndirectBack'] := db.sqlBool( testIndirectBack );

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
  procedure TTestingParams.setPdfDelayName( val: string );
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

  function TTestingParams.getPdfDelayName(): string; begin result := _pdfDelayName; end;


  function TTestingParams.getPdfTestingDelay(): TPdf;
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
  function TTestingParams.getChartSet(): TChartSet;
    begin
      result := [ TeDelay ];
    end
  ;


  procedure TTestingParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        TeDelay: self.pdfTestDelayName := newName;
      end;
    end
  ;


  function TTestingParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            TeDelay:
              if ( self.fnDictionary.contains( self.pdfTestDelayName ) ) then
                result := self.fnDictionary.value( self.pdfTestDelayName ).fn
              ;
          end;
        end
      ;
    end
  ;


  procedure TTestingParams.removeChart( const chartName: string );
    begin
      if( chartName = self.pdfTestDelayName ) then self.pdfTestDelayName := '';
    end
  ;

(*
  procedure TTestingParams.changeChart(
        const whichChart: TSMChart;
        const oldChartName: string;
        newChart: TChartFunction;
        addlInfo: integer = -1
      );
    var
      newName: string;
    begin
      if( nil = newChart ) then
        newName := ''
      else
        newName := newChart.name
      ;

      case whichChart of
        TeDelay: self.pdfTestDelayName := newName;
      end;
    end
  ;
*)

  function TTestingParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;

      case whichChart of
        TeDelay: result := ( chartName = self.pdfTestDelayName );
      end;
    end
  ;


  function TTestingParams.functionsAreValid(): boolean;
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
  function TTestingParams.validate( err: PString = nil ): boolean;
    var
      msg: string;
      submsg: string;
    begin
      result := true;
      msg := '';
      submsg := '';

      // Validation of testing parameters
      //---------------------------------
      // 0 <= sensitivity and specificity <=1
      // The delay PDF must be specified
      
      if( useTesting ) then
        begin
          if( ( 0.0 > sensitivity ) or ( 1.0 < sensitivity ) ) then
            begin
              if( nil <> err ) then
                msg := msg + '    ' + tr( 'Test sensitivity must be between 0 and 1.' ) + endl
              ;
              result := false;
            end
          ;

          if( ( 0.0 > specificity ) or ( 1.0 < specificity ) ) then
            begin
              if( nil <> err ) then
                msg := msg + '    ' + tr( 'Test specificity must be between 0 and 1.' ) + endl
              ;
              result := false;
            end
          ;

          if( nil = pdfTestDelay ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Delay for test results is not set.' ) + endl;
              result := false;
            end
          else if( not( pdfTestDelay.validate( @submsg ) ) ) then
            begin
              if( nil <> err ) then msg := msg + '    ' + tr( 'Delay for test results is not valid:' ) + submsg + endl;
              result := false;
            end
          ;
        end
      ;

      if( ( result = false ) and ( nil <> err ) ) then
        begin
          //msg := endl + ansiReplaceStr( tr( 'Testing parameters for xyz:' ), 'xyz', prodTypeDescr ) + endl + msg;
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
  procedure TTestingParams.debug();
    begin
      dbcout( '---------TESTING PARAMETERS' + endl, true );

      dbcout( 'Testing is used: ' + usBoolToText( useTesting ), true );
      dbcout( 'Test sensitivity: ' + usFloatToStr( _sensitivity ), true );
      dbcout( 'Test specificitiy: ' + usFloatToStr( _specificity ), true );

      if( nil <> pdfTestDelay ) then
        begin
          dbcout( 'Period of test result delay:', true );
          pdfTestDelay.debug();
        end
      else
        dbcout( 'PERIOD OF TEST RESULT DELAY IS UNSPECIFIED', true )
      ;

      dbcout( 'testDirectForward: ' + usBoolToText( _testDirectForward ), true );
      dbcout( 'testIndirectForward: ' + usBoolToText( _testIndirectForward ), true );
      dbcout( 'testDirectBack: ' + usBoolToText( _testDirectBack ), true );
      dbcout( 'testIndirectBack: ' + usBoolToText( _testIndirectBack ), true );

      dbcout( '---------END TESTING PARAMETERS' + endl, true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TTestingParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TTestingParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  procedure TTestingParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;

  function TTestingParams.getUpdated(): boolean;
    begin
      result :=
        _updated
      or
        fnDictionary.functionExistsAndIsUpdated( _pdfDelayName )
      ;
    end
  ;

  function TTestingParams.getUseTesting(): boolean;
    begin
      result :=
        testDirectForward
      or
        testIndirectForward
      or
        testDirectBack
      or
        testIndirectBack
      ;
    end
  ;

  procedure TTestingParams.setSpecificity( val: double ); begin _specificity := val; _updated := true; end;
  procedure TTestingParams.setSensitivity( val: double ); begin _sensitivity := val; _updated := true; end;

  function TTestingParams.getSpecificity(): double; begin Result := _specificity; end;
  function TTestingParams.getSensitivity(): double; begin Result := _sensitivity; end;

  procedure TTestingParams.setTestDirectForward( val: boolean ); begin _testDirectForward := val; _updated := true; end;
  procedure TTestingParams.setTestIndirectForward( val: boolean ); begin _testIndirectForward := val; _updated := true; end;
  procedure TTestingParams.setTestDirectBack( val: boolean ); begin _testDirectBack := val; _updated := true; end;
  procedure TTestingParams.setTestIndirectBack( val: boolean ); begin _testIndirectBack := val; _updated := true; end;

  function TTestingParams.getTestDirectForward(): boolean; begin Result := _testDirectForward; end;
  function TTestingParams.getTestIndirectForward(): boolean; begin Result := _testIndirectForward; end;
  function TTestingParams.getTestDirectBack(): boolean; begin Result := _testDirectBack; end;
  function TTestingParams.getTestIndirectBack(): boolean; begin Result := _testIndirectBack; end;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
  function TTestingParams.ssXml( const productionTypeID: integer ): string;
    begin
      result := ''
        + '  <test-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl
        + '    <delay>' + endl
        ;

      result := result + pdfTestDelay.ssXml( 6 );

      result := result
        + '    </delay>' + endl
        + '    <sensitivity>' + usFloatToStr( sensitivity ) + '</sensitivity>' + endl
        + '    <specificity>' + usFloatToStr( specificity ) + '</specificity>' + endl
        + '  </test-model>' + endl + endl
      ;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TTestingParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'trace-exam-model' );
      result.append( 'test-model' );
    end
  ;


  function TTestingParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TTestingParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    begin
      if( 'trace-exam-model' = sdew.GetElementName( model ) ) then
        importTraceExamModelXml( model, sdew, errMsg )
      ;
      if( 'test-model' = sdew.GetElementName( model ) ) then
        importTestModelXml( model, sdew, errMsg )
      ;
    end
  ;


  procedure TTestingParams.importTestModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      val: double;
      subElement: pointer;
      delay: TPdf;
    begin
      subElement := sdew.GetElementByName( model, 'sensitivity' );
      if( nil = subElement ) then
        appendToPstring( errMsg, tr( 'Test model XML does not contain a valid value for test sensitivity.' ) )
      else
        begin
          val := usStrToFloat( sdew.GetElementContents( subElement ), NaN );
          if( isNaN( val ) ) then
            appendToPstring( errMsg, tr( 'Test model XML does not contain a valid value for test sensitivity.' ) )
          else
            self.sensitivity := val
          ;
        end
      ;

      subElement := sdew.GetElementByName( model, 'specificity' );
      if( nil = subElement ) then
        appendToPstring( errMsg, tr( 'Test model XML does not contain a valid value for test specificity.' ) )
      else
        begin
          val := usStrToFloat( sdew.GetElementContents( subElement ), NaN );
          if( isNaN( val ) ) then
            appendToPstring( errMsg, tr( 'Test model XML does not contain a valid value for test specificity.' ) )
          else
            self.specificity := val
          ;
        end
      ;

      subElement := Sdew.GetElementByName( model, 'delay' );
      if ( nil = subElement ) then
        appendToPstring( errMsg, tr( 'Test model XML does not contain a valid function for delay.' ) )
      else
        begin
          delay := createPdfFromXml( subElement, Sdew );
          if( nil = delay ) then
            appendToPstring( errMsg, tr( 'Test model XML does not contain a valid function for delay.' ) )
          else
            begin
              if( '' = delay.name ) then
                delay.name := 'Testing delay'
              ;
              delay.dbField := word( TeDelay );
              self.pdfTestDelayName := fnDictionary.checkAndInsert( delay );
            end
          ;
        end
      ;
    end
  ;


  procedure TTestingParams.importTraceExamModelXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      contactType: string;
      contactDirection: string;
      useTest: boolean;
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

      subElement := sdew.GetElementByName( model, 'test-if-no-signs' );

      if( nil = subElement ) then
        begin
          appendToPstring( errMsg, tr( 'Trace exam XML does not contain a valid parameter for diagnostic testing.' ) );
          exit;
        end
      else
        begin
          useTest := usTextToBool( sdew.GetElementContents( subElement ) );

          if( 'direct' = contactType ) then
            begin
              if( 'in' = contactDirection ) then
                self.testDirectBack := useTest
              else if( 'out' = contactDirection ) then
                self.testDirectForward := useTest
              else
                raise exception.Create( 'Someone forgot something in TTestingParams.importXml()' )
              ;
            end
          else if( 'indirect' = contactType ) then
            begin
              if( 'in' = contactDirection ) then
                self.testIndirectBack := useTest
              else if( 'out' = contactDirection ) then
                self.testIndirectForward := useTest
              else
                raise exception.Create( 'Someone forgot something in TTestingParams.importXml()' )
              ;
            end
          else
            raise exception.Create( 'Someone forgot something in TTestingParams.importXml()' )
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.