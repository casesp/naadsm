unit DetectionParams;

(*
DetectionParams.pas
--------------------
Begin: 2005/06/03
Last revision: $Date: 2013-06-27 19:11:34 $ $Author: areeves $
Version number: $Revision: 1.38.4.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    QLists,

    Sdew,
    
    ChartFunction,
    RelFunction,
    Models,
    SMDatabase,
    FunctionEnums
  ;

  type TDetectionParams = class( TModelWithFunctions )
    protected
      _xmlModelList: TQStringList;
      
    	// Properties
      _prodTypeDescr: string;
      _useDetection: boolean;
      
      _relObsVsTimeClinicalName: string;
      _relReportVsFirstDetectionName: string;

      // For internal use
      procedure initialize();

      // XML import
      function getXmlModelList(): TQStringList;

      // Properties
      procedure setProdTypeDescr( val: string );
      function getProdTypeDescr(): string;

      procedure setUseDetection( val: boolean );
      function getUseDetection(): boolean;

      procedure setRelObsVsTimeClinicalName( val: string );
      procedure setRelReportVsFirstDetectionName( val: string );

      function getRelObsVsTimeClinicalName(): string;
      function getRelReportVsFirstDetectionName(): string;

      function getRelObsVsTimeClinical(): TRelFunction;
      function getRelReportVsFirstDetection(): TRelFunction;

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

    public
      constructor create( sim: TObject; prodTypeName: string ); overload;
      constructor create( const src: TDetectionParams; sim: TObject ); overload;
      constructor create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject ); overload;
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
      property prodTypeDescr: string read getProdTypeDescr write setProdTypeDescr;
      property useDetection: boolean read getUseDetection write setUseDetection;

      property relObsVsTimeClinicalName: string read getRelObsVsTimeClinicalName write setRelObsVsTimeClinicalName;
      property relReportVsFirstDetectionName: string read getRelReportVsFirstDetectionName write setRelReportVsFirstDetectionName;

      property relObsVsTimeClinical: TRelFunction read getRelObsVsTimeClinical;
      property relReportVsFirstDetection: TRelFunction read getRelReportVsFirstDetection;
    end
  ;



implementation

  uses
    StrUtils,
    SysUtils,
    Variants,

    MyStrUtils,
    DebugWindow,
    SqlClasses,
    I88n,

    FunctionDictionary
  ;

  const DBSHOWMSG = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TDetectionParams.create( sim: TObject; prodTypeName: string );
    begin
      inherited create();
      initialize();

      _prodTypeDescr := prodTypeName;
      _sim := sim;
    end
  ;


  constructor TDetectionParams.create( db: TSMDatabase; ptID: integer; prodTypeName: string; sim: TObject );
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
        + ' inProductionType.useDetection,'
        + ' inProductionType.detProbObsVsTimeClinicalRelID,'
        + ' probVsTimeClinicalChart.chartName as probVsTimeClinicalName,'
        + ' inProductionType.detProbReportVsFirstDetectionRelID,'
        + ' probVsFirstDetectionChart.chartName as probVsFirstDetectionName'
        + ' FROM'
        + ' ( ('
        + ' inProductionType'
        + ' LEFT OUTER JOIN inChart probVsTimeClinicalChart'
        + ' ON probVsTimeClinicalChart.chartID = inProductionType.detProbObsVsTimeClinicalRelID'
        + ' )'
        + ' LEFT OUTER JOIN inChart probVsFirstDetectionChart'
        + ' ON probVsFirstDetectionChart.chartID = inProductionType.detProbReportVsFirstDetectionRelID'
        + ' ) '
        + ' WHERE inProductionType.productionTypeID = ' + intToStr( ptID )
      ;


      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('useDetection') ) then useDetection := row.field('useDetection');

      if( null <> row.field('detProbObsVsTimeClinicalRelID') ) then
        begin
      	  setrelObsVsTimeClinicalName( row.field( 'probVsTimeClinicalName' ) );
        end
      ;

      if( null <> row.field('detProbReportVsFirstDetectionRelID') ) then
        begin
      	  setrelReportVsFirstDetectionName( row.field( 'probVsFirstDetectionName' ) );
        end
      ;

      prodTypeDescr := prodTypeName;

      _updated := false;

      freeAndNil( res );
    end
  ;


  constructor TDetectionParams.create( const src: TDetectionParams; sim: TObject );
    begin
    	inherited create( src );
      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;
      _useDetection := src._useDetection;

      setRelObsVsTimeClinicalName( src._relObsVsTimeClinicalName );
      setRelReportVsFirstDetectionName( src._relReportVsFirstDetectionName );

      _updated := src._updated;
    end
  ;


  procedure TDetectionParams.initialize();
  	begin
      _xmlModelList := nil;

      _prodTypeDescr := '';
      _updated := false;
      _useDetection := false;

      setRelObsVsTimeClinicalName( '' );
      setRelReportVsFirstDetectionName( '' );
    end
  ;


  destructor TDetectionParams.destroy();
    begin
      dbcout( '*** TDetectionParams.destroy() for "' + _prodTypeDescr + '"', DBSHOWMSG );
      freeAndNil( _xmlModelList );

      // The function dictionary is freed elsewhere.
      // Functions are handled by the function dictionary:
      // don't free them here, but do decrement their counters.
      setRelObsVsTimeClinicalName( '' );
      setRelReportVsFirstDetectionName( '' );
      
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Database population
//-----------------------------------------------------------------------------
  function TDetectionParams.populateDatabase( db: TSMDatabase; ptID: integer ): integer;
  	var
    	q: string;
      dict: TQueryDictionary;
  	begin
      dict := TQueryDictionary.create();

      dict['useDetection'] := usBoolToText( useDetection );

      if( nil <> relObsVsTimeClinical ) then
      	dict['detProbObsVsTimeClinicalRelID'] := intToStr( relObsVsTimeClinical.id )
      else
      	dict['detProbObsVsTimeClinicalRelID'] := DATABASE_NULL_VALUE
      ;

      if( nil <> relReportVsFirstDetection ) then
      	dict['detProbReportVsFirstDetectionRelID'] := intToStr( relReportVsFirstDetection.id )
      else
     		dict['detProbReportVsFirstDetectionRelID'] := DATABASE_NULL_VALUE
      ;

      q := writeQuery(
        'inProductionType',
        QUpdate,
        dict,
        'WHERE [productionTypeID] = ' + intToStr( ptID )
      );

      result := integer( db.execute( q ) );

      dict.Clear();
      dict.Free();

      _updated := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML export
//-----------------------------------------------------------------------------
  function TDetectionParams.ssXML( const productionTypeID: integer ): string;
  	var
    	str: string;
    begin
      str := str + '  <detection-model production-type="' + encodeXml( prodTypeDescr ) + '" production-type-id="' + intToStr( productionTypeID ) + '">' + endl;

      str := str + '    <prob-report-vs-time-clinical>' + endl;
      str := str + relObsVsTimeClinical.ssXml( 3 );
      str := str + '    </prob-report-vs-time-clinical>' + endl;

      str := str + '    <prob-report-vs-time-since-outbreak>' + endl;
      str := str + relReportVsFirstDetection.ssXml( 3 );
      str := str + '    </prob-report-vs-time-since-outbreak>' + endl;

      str := str + '  </detection-model>' + endl;

      result := str;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TDetectionParams.validate( err: PString = nil ): boolean;
  	var
    	msg: string;
    	submsg: string;
  	begin
    	result := true;
			msg := '';

      submsg := '';

      if( useDetection ) then
        begin
          if( nil = relObsVsTimeClinical ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of observing clinical signs versus time clinical is not set.' ) + endl;
              result := false;
            end
          else if( not( relObsVsTimeClinical.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of observing clinical signs versus time clinical is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          (*
          // I think this one actually can start at 0...
          else if( relObsVsTimeClinical.yStartsAtZero ) then
            begin
              if( nil <> err ) then msg := msg + '  Probability of observing clinical signs versus time clinical is not valid: detection will not occur if the baseline probability is 0.' + endl;
              result := false;
            end
          *)
          ;

          submsg := '';
          if( nil = relReportVsFirstDetection ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting versus time since first detection is not set.' ) + endl;
              result := false;
            end
          else if( not( relReportVsFirstDetection.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting versus time since first detection is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          else if( relReportVsFirstDetection.yStartsAtZero ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting versus time since first detection is not valid: detection will not occur if the baseline probability is 0.' ) + endl;
              result := false;
            end
          ;
        end
      ;


      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := endl + ansiReplaceStr( tr( 'Detection parameters for xyz:' ), 'xyz', prodTypeDescr ) + endl + msg;
          err^ := err^ + msg;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Debugging
//-----------------------------------------------------------------------------
  procedure TDetectionParams.debug();
    begin
      dbcout(  endl + '##DETECTION PARAMS', true );

      if( nil <> relObsVsTimeClinical ) then
      	begin
          dbcout( 'ProbObsVsTimeClinical:', true );
          relObsVsTimeClinical.debug();
        end
      else
      	dbcout( 'ProbObsVsTimeClinical IS UNDEFINED' + endl, true )
      ;

      if( nil <> relReportVsFirstDetection ) then
      	begin
        	dbcout( 'ProbReportVsFirstDetection:', true );
      		relReportVsFirstDetection.debug();
        end
      else
      	dbcout( 'ProbReportVsFirstDetection IS UNDEFINED' + endl, true )
      ;
      
      dbcout( '##END DETECTION PARAMS', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TDetectionParams.setProdTypeDescr( val: string ); begin _prodTypeDescr := val; end;

  function TDetectionParams.getProdTypeDescr(): string;
    begin
      if( 0 = length( _prodTypeDescr ) ) then
        raise exception.Create( 'TDetectionParams._prodTypeDescr is not set' )
      ;
      result := _prodTypeDescr;
    end
  ;

  procedure TDetectionParams.setUseDetection( val: boolean );
    begin
      _useDetection := val;
      _updated := true;
    end
  ;

  function TDetectionParams.getUseDetection(): boolean; begin result := _useDetection; end;

  // FIX ME!
  function TDetectionParams.getUpdated(): boolean; begin result := _updated; end;


  procedure TDetectionParams.setRelObsVsTimeClinicalName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relObsVsTimeClinicalName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relObsVsTimeClinicalName := val;
      _updated := true;
    end
  ;


  procedure TDetectionParams.setRelReportVsFirstDetectionName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relReportVsFirstDetectionName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relReportVsFirstDetectionName := val;
      _updated := true;
    end
  ;


  function TDetectionParams.getRelObsVsTimeClinicalName(): string; begin result := _relObsVsTimeClinicalName; end;
  function TDetectionParams.getRelReportVsFirstDetectionName(): string; begin result := _relReportVsFirstDetectionName; end;


  function TDetectionParams.getRelObsVsTimeClinical(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relObsVsTimeClinicalName ) ) then
            begin
              if( fnDictionary.value( _relObsVsTimeClinicalName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relObsVsTimeClinicalName ).fn as TRelFunction
              else
                begin
                  setRelObsVsTimeClinicalName( '' );
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


  function TDetectionParams.getRelReportVsFirstDetection(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relReportVsFirstDetectionName ) ) then
            begin
              if( fnDictionary.value( _relReportVsFirstDetectionName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relReportVsFirstDetectionName ).fn as TRelFunction
              else
                begin
                  setRelReportVsFirstDetectionName( '' );
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
  procedure TDetectionParams.setChart( const whichChart: TSMChart; fn: TChartFunction; addlInfo: integer = -1 );
    var
      newName: string;
    begin
      if( nil = fn ) then
        newName := ''
      else
        newName := fn.name
      ;

      case whichChart of
        DetProbObsVsTimeClinical: self.relObsVsTimeClinicalName := newName;
        DetProbReportVsFirstDetection: self.relReportVsFirstDetectionName := newName;
      end;
    end
  ;


  function TDetectionParams.chart( const whichChart: TSMChart; addlInfo: integer = -1 ): TChartFunction;
    begin
      result := nil;

      if ( self.fnDictionary <> nil ) then
        begin
          case whichChart of
            DetProbObsVsTimeClinical:
              if ( self.fnDictionary.contains( self.relObsVsTimeClinicalName ) ) then
                result := self.fnDictionary.value( self.relObsVsTimeClinicalName ).fn
              ;
            DetProbReportVsFirstDetection:
              if ( self.fnDictionary.contains( self.relReportVsFirstDetectionName ) ) then
                result := self.fnDictionary.value( self.relReportVsFirstDetectionName ).fn
              ;
          end;
        end
      ;
    end
  ;


  procedure TDetectionParams.removeChart( const chartName: string );
    begin
      if( chartName = self.relObsVsTimeClinicalName ) then self.relObsVsTimeClinicalName := '';
      if( chartName = self.relReportVsFirstDetectionName ) then self.relReportVsFirstDetectionName := '';
    end
  ;


  function TDetectionParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;
      
      case whichChart of
        DetProbObsVsTimeClinical: result := ( chartName = self.relObsVsTimeClinicalName );
        DetProbReportVsFirstDetection: result := ( chartName = self.relReportVsFirstDetectionName );
      end;
    end
  ;

  
  function TDetectionParams.functionsAreValid(): boolean;
    begin
      result := true;

      if( fnDictionary.contains( _relObsVsTimeClinicalName ) ) then
        begin
          if( not( fnDictionary.value( _relObsVsTimeClinicalName ).fn is TRelFunction ) ) then
            begin
              setRelObsVsTimeClinicalName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _relReportVsFirstDetectionName ) ) then
        begin
          if( not( fnDictionary.value( _relReportVsFirstDetectionName ).fn is TRelFunction ) ) then
            begin
              setRelReportVsFirstDetectionName( '' );
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// XML import
//-----------------------------------------------------------------------------
  class function TDetectionParams.createXmlModelList(): TQStringList;
    begin
      result := TQStringList.create();
      result.Append( 'detection-model' );
    end
  ;


  function TDetectionParams.getXmlModelList(): TQStringList;
    begin
      if( nil = _xmlModelList ) then
        _xmlModelList := createXmlModelList()
      ;

      result := _xmlModelList;
    end
  ;


  procedure TDetectionParams.importXml( model: pointer; sdew: TSdew; errMsg: pstring = nil );
    var
      e: pointer;
      fn: TRelFunction;
    begin
      if( 'detection-model' <> sdew.GetElementName( model ) ) then
        raise exception.Create( 'There is a problem in TDetectionParams.importXml' )
      ;
      
      // If there is a 'zone' attribute, this model should be parsed by TProdTypeZoneComboParams instead.
      if( 0 < length( sdew.GetElementAttribute( model, 'zone' ) ) ) then
        exit
      ;

      e := sdew.GetElementByName( model, 'prob-report-vs-time-clinical' );
      if( nil <> e ) then
        begin
          fn := createRelFromXml( e, sdew );

          if( nil <> fn ) then
            begin
              fn.dbField := word( DetProbObsVsTimeClinical );
              if( strIsEmpty( fn.name ) ) then
                fn.name := 'Probability of observing clinical signs versus days clinical' + ' ' + self.prodTypeDescr
              ;
              self.relObsVsTimeClinicalName := fnDictionary.checkAndInsert( fn );
              self.useDetection := true;
            end
          else
            appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of observing clinical signs.' ) )
          ;
        end
      else
        appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of observing clinical signs.' ) )
      ;

      e := sdew.GetElementByName( model, 'prob-report-vs-time-since-outbreak' );
      if( nil <> e ) then
        begin
          fn := createRelFromXml( e, sdew );
          if( nil <> fn ) then
            begin
              fn.dbField := word( DetProbReportVsFirstDetection );
              if( strIsEmpty( fn.name ) ) then
                fn.name := 'Probability of reporting versus days of outbreak' + ' ' + self.prodTypeDescr
              ;
              self.relReportVsFirstDetectionName := fnDictionary.checkAndInsert( fn );
              self.useDetection := true;
            end
          else
            appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of reporting.' ) )
          ;
        end
      else
        appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of reporting.' ) )
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
