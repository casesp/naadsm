unit DetectionParams;

(*
DetectionParams.pas
--------------------
Begin: 2005/06/03
Last revision: $Date: 2011-10-19 01:24:12 $ $Author: areeves $
Version number: $Revision: 1.44.2.8 $
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
      _relObsVsTimeDeadName: string;
      _relReportClinVsFirstDetectionName: string;
      _relReportDeadVsFirstDetectionName: string;

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
      procedure setRelObsVsTimeDeadName( val: string );
      procedure setRelReportClinVsFirstDetectionName( val: string );
      procedure setRelReportDeadVsFirstDetectionName( val: string );

      function getRelObsVsTimeClinicalName(): string;
      function getRelObsVsTimeDeadName(): string;
      function getRelReportClinVsFirstDetectionName(): string;
      function getRelReportDeadVsFirstDetectionName(): string;

      function getRelObsVsTimeClinical(): TRelFunction;
      function getRelObsVsTimeDead(): TRelFunction;
      function getRelReportClinVsFirstDetection(): TRelFunction;
      function getRelReportDeadVsFirstDetection(): TRelFunction;

      // Overridden from TModel
      //-----------------------
      function getUpdated(): boolean; override;

      // Overridden from TModelWithFunctions
      //------------------------------------
      function getChartSet(): TChartSet; override;

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
      property relObsVsTimeDeadName: string read getRelObsVsTimeDeadName write setRelObsVsTimeDeadName;
      property relReportClinVsFirstDetectionName: string read getRelReportClinVsFirstDetectionName write setRelReportClinVsFirstDetectionName;
      property relReportDeadVsFirstDetectionName: string read getRelReportDeadVsFirstDetectionName write setRelReportDeadVsFirstDetectionName;

      property relObsVsTimeClinical: TRelFunction read getRelObsVsTimeClinical;
      property relObsVsTimeDead: TRelFunction read getRelObsVsTimeDead;
      property relReportClinVsFirstDetection: TRelFunction read getRelReportClinVsFirstDetection;
      property relReportDeadVsFirstDetection: TRelFunction read getRelReportDeadVsFirstDetection;
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
        + ' inProductionType.detProbObsVsTimeDeadRelID,'
        + ' inProductionType.detProbReportClinVsFirstDetectionRelID,'
        + ' inProductionType.detProbReportDeadVsFirstDetectionRelID,'
        + ' probVsTimeClinicalChart.chartName as probVsTimeClinicalName,'
        + ' probVsTimeDeadChart.chartName as probVsTimeDeadName,'            
        + ' probClinVsFirstDetectionChart.chartName as probClinVsFirstDetectionName,'
        + ' probDeadVsFirstDetectionChart.chartName as probDeadVsFirstDetectionName'
        + ' FROM'
        + ' ( ( ( ('
        + ' inProductionType'
        + ' LEFT OUTER JOIN inChart probVsTimeClinicalChart'
        + ' ON probVsTimeClinicalChart.chartID = inProductionType.detProbObsVsTimeClinicalRelID'
        + ' )'
         + ' LEFT OUTER JOIN inChart probVsTimeDeadChart'
        + ' ON probVsTimeDeadChart.chartID = inProductionType.detProbObsVsTimeDeadRelID'
        + ' )'
        + ' LEFT OUTER JOIN inChart probClinVsFirstDetectionChart'
        + ' ON probClinVsFirstDetectionChart.chartID = inProductionType.detProbReportClinVsFirstDetectionRelID'
        + ' ) '
        + ' LEFT OUTER JOIN inChart probDeadVsFirstDetectionChart'
        + ' ON probDeadVsFirstDetectionChart.chartID = inProductionType.detProbReportDeadVsFirstDetectionRelID'
        + ' ) '
        + ' WHERE inProductionType.productionTypeID = ' + intToStr( ptID )
      ;


      res := TSqlResult.create( q, db2 );
      row := res.fetchArrayFirst();

      if( null <> row.field('useDetection') ) then useDetection := row.field('useDetection');

      if( null <> row.field('detProbObsVsTimeClinicalRelID') ) then
        setRelObsVsTimeClinicalName( row.field( 'probVsTimeClinicalName' ) )
      ;

      if( null <> row.field('detProbObsVsTimeDeadRelID') ) then
        setRelObsVsTimeDeadName( row.field( 'probVsTimeDeadName' ) )
      ;

      if( null <> row.field('detProbReportClinVsFirstDetectionRelID') ) then
        setRelReportClinVsFirstDetectionName( row.field( 'probClinVsFirstDetectionName' ) )
      ;

      if( null <> row.field('detProbReportDeadVsFirstDetectionRelID') ) then
        setRelReportDeadVsFirstDetectionName( row.field( 'probDeadVsFirstDetectionName' ) )
      ;

      prodTypeDescr := prodTypeName;

      _updated := false;

      freeAndNil( res );
    end
  ;


  constructor TDetectionParams.create( const src: TDetectionParams; sim: TObject );
    begin
    	inherited create( src );
      
      _xmlModelList := nil;
      
      _sim := sim;

      _prodTypeDescr := src._prodTypeDescr;
      _useDetection := src._useDetection;

      setRelObsVsTimeClinicalName( src._relObsVsTimeClinicalName );
      setRelObsVsTimeDeadName( src._relObsVsTimeDeadName );
      setRelReportClinVsFirstDetectionName( src._relReportClinVsFirstDetectionName );
      setRelReportDeadVsFirstDetectionName( src._relReportDeadVsFirstDetectionName );

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
      setRelObsVsTimeDeadName( '' );
      setRelReportClinVsFirstDetectionName( '' );
      setRelReportDeadVsFirstDetectionName( '' );
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
      setRelObsVsTimeDeadName( '' );
      setRelReportClinVsFirstDetectionName( '' );
      setRelReportDeadVsFirstDetectionName( '' );

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

      dict['useDetection'] := db.sqlBool( useDetection );

      if( nil <> relObsVsTimeClinical ) then
      	dict['detProbObsVsTimeClinicalRelID'] := intToStr( relObsVsTimeClinical.id )
      else
      	dict['detProbObsVsTimeClinicalRelID'] := DATABASE_NULL_VALUE
      ;

      if( nil <> relObsVsTimeDead ) then
      	dict['detProbObsVsTimeDeadRelID'] := intToStr( relObsVsTimeDead.id )
      else
      	dict['detProbObsVsTimeDeadRelID'] := DATABASE_NULL_VALUE
      ;

      if( nil <> relReportClinVsFirstDetection ) then
      	dict['detProbReportClinVsFirstDetectionRelID'] := intToStr( relReportClinVsFirstDetection.id )
      else
     		dict['detProbReportClinVsFirstDetectionRelID'] := DATABASE_NULL_VALUE
      ;

      if( nil <> relReportDeadVsFirstDetection ) then
      	dict['detProbReportDeadVsFirstDetectionRelID'] := intToStr( relReportDeadVsFirstDetection.id )
      else
     		dict['detProbReportDeadVsFirstDetectionRelID'] := DATABASE_NULL_VALUE
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
          ;

          if( nil = relObsVsTimeDead ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of observing death from disease versus time dead from disease is not set.' ) + endl;
              result := false;
            end
          else if( not( relObsVsTimeDead.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of observing death from disease versus time dead from disease is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          ;


          submsg := '';
          if( nil = relReportClinVsFirstDetection ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting clinical signs versus time since first detection is not set.' ) + endl;
              result := false;
            end
          else if( not( relReportClinVsFirstDetection.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting clinical signs versus time since first detection is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          else if( relReportClinVsFirstDetection.yStartsAtZero ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting clinical signs versus time since first detection is not valid: detection will not occur if the baseline probability is 0.' ) + endl;
              result := false;
            end
          ;

          submsg := '';
          if( nil = relReportDeadVsFirstDetection ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting death from disease versus time since first detection is not set.' ) + endl;
              result := false;
            end
          else if( not( relReportDeadVsFirstDetection.validate( @submsg) ) ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting death from disease versus time since first detection is not valid:' ) + ' ' + submsg + endl;
              result := false;
            end
          else if( relReportDeadVsFirstDetection.yStartsAtZero ) then
            begin
              if( nil <> err ) then msg := msg + '  ' + tr( 'Probability of reporting death from disease versus time since first detection is not valid: detection will not occur if the baseline probability is 0.' ) + endl;
              result := false;
            end
          ;
        end
      ;


      if( ( result = false ) and ( nil <> err ) ) then
      	begin
					msg := ansiReplaceStr( tr( 'Detection parameters for xyz are not valid:' ), 'xyz', prodTypeDescr ) + endl + msg;
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
      dbcout( endl + '##DETECTION PARAMS', true );

      if( nil <> relObsVsTimeClinical ) then
      	begin
          dbcout( 'ProbObsVsTimeClinical:', true );
          relObsVsTimeClinical.debug();
        end
      else
      	dbcout( 'ProbObsVsTimeClinical IS UNDEFINED' + endl, true )
      ;

      if( nil <> relObsVsTimeDead ) then
      	begin
          dbcout( 'ProbObsVsTimeDead:', true );
          relObsVsTimeDead.debug();
        end
      else
      	dbcout( 'ProbObsVsTimeDead IS UNDEFINED' + endl, true )
      ;

      if( nil <> relReportClinVsFirstDetection ) then
      	begin
        	dbcout( 'ProbReportClinVsFirstDetection:', true );
      		relReportClinVsFirstDetection.debug();
        end
      else
      	dbcout( 'ProbReportClinVsFirstDetection IS UNDEFINED' + endl, true )
      ;

      if( nil <> relReportDeadVsFirstDetection ) then
      	begin
        	dbcout( 'ProbReportDeadVsFirstDetection:', true );
      		relReportDeadVsFirstDetection.debug();
        end
      else
      	dbcout( 'ProbReportDeadVsFirstDetection IS UNDEFINED' + endl, true )
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

  procedure TDetectionParams.setUseDetection( val: boolean ); begin _useDetection := val; _updated := true; end;
  function TDetectionParams.getUseDetection(): boolean; begin result := _useDetection; end;

  function TDetectionParams.getUpdated(): boolean;
    begin
      result :=
        _updated
      or
        fnDictionary.functionExistsAndIsUpdated( _relObsVsTimeClinicalName )
      or
        fnDictionary.functionExistsAndIsUpdated( _relObsVsTimeDeadName )
      or
        fnDictionary.functionExistsAndIsUpdated( _relReportClinVsFirstDetectionName )
      or
        fnDictionary.functionExistsAndIsUpdated( _relReportDeadVsFirstDetectionName )
      ;
    end
  ;


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


  procedure TDetectionParams.setRelObsVsTimeDeadName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relObsVsTimeDeadName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relObsVsTimeDeadName := val;
      _updated := true;
    end
  ;


  procedure TDetectionParams.setRelReportClinVsFirstDetectionName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relReportClinVsFirstDetectionName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relReportClinVsFirstDetectionName := val;
      _updated := true;
    end
  ;


  procedure TDetectionParams.setRelReportDeadVsFirstDetectionName( val: string );
    begin
      val := trim( val );

      // Decrement the reference counter for the old function...
      decrFnRefCounter( _relReportDeadVsFirstDetectionName );
      // ...and increment the reference counter for the new function.
      incrFnRefCounter( val );

      _relReportDeadVsFirstDetectionName := val;
      _updated := true;
    end
  ;


  function TDetectionParams.getRelObsVsTimeClinicalName(): string; begin result := _relObsVsTimeClinicalName; end;
  function TDetectionParams.getRelObsVsTimeDeadName(): string; begin result := _relObsVsTimeDeadName; end;
  function TDetectionParams.getRelReportClinVsFirstDetectionName(): string; begin result := _relReportClinVsFirstDetectionName; end;
  function TDetectionParams.getRelReportDeadVsFirstDetectionName(): string; begin result := _relReportDeadVsFirstDetectionName; end;

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


  function TDetectionParams.getRelObsVsTimeDead(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relObsVsTimeDeadName ) ) then
            begin
              if( fnDictionary.value( _relObsVsTimeDeadName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relObsVsTimeDeadName ).fn as TRelFunction
              else
                begin
                  setRelObsVsTimeDeadName( '' );
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


  function TDetectionParams.getRelReportClinVsFirstDetection(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relReportClinVsFirstDetectionName ) ) then
            begin
              if( fnDictionary.value( _relReportClinVsFirstDetectionName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relReportClinVsFirstDetectionName ).fn as TRelFunction
              else
                begin
                  setRelReportClinVsFirstDetectionName( '' );
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


  function TDetectionParams.getRelReportDeadVsFirstDetection(): TRelFunction;
    begin
      if( nil = fnDictionary ) then
        result := nil
      else
        begin
          if( fnDictionary.contains( _relReportDeadVsFirstDetectionName ) ) then
            begin
              if( fnDictionary.value( _relReportDeadVsFirstDetectionName ).fn is TRelFunction ) then
                result := fnDictionary.value( _relReportDeadVsFirstDetectionName ).fn as TRelFunction
              else
                begin
                  setRelReportDeadVsFirstDetectionName( '' );
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
  function TDetectionParams.getChartSet(): TChartSet;
    begin
      result := [ DetProbObsVsTimeClinical .. DetProbReportDeadVsFirstDetection ];
    end
  ;


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
        DetProbObsVsTimeDead: self.relObsVsTimeDeadName := newName;
        DetProbReportClinVsFirstDetection: self.relReportClinVsFirstDetectionName := newName;
        DetProbReportDeadVsFirstDetection: self.relReportDeadVsFirstDetectionName := newName;
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
            DetProbObsVsTimeDead:
              if ( self.fnDictionary.contains( self.relObsVsTimeDeadName ) ) then
                result := self.fnDictionary.value( self.relObsVsTimeDeadName ).fn
              ;
            DetProbReportClinVsFirstDetection:
              if ( self.fnDictionary.contains( self.relReportClinVsFirstDetectionName ) ) then
                result := self.fnDictionary.value( self.relReportClinVsFirstDetectionName ).fn
              ;
            DetProbReportDeadVsFirstDetection:
              if ( self.fnDictionary.contains( self.relReportDeadVsFirstDetectionName ) ) then
                result := self.fnDictionary.value( self.relReportDeadVsFirstDetectionName ).fn
              ;
          end;
        end
      ;
    end
  ;


  procedure TDetectionParams.removeChart( const chartName: string );
    begin
      if( chartName = self.relObsVsTimeClinicalName ) then self.relObsVsTimeClinicalName := '';
      if( chartName = self.relObsVsTimeDeadName ) then self.relObsVsTimeDeadName := '';
      if( chartName = self.relReportClinVsFirstDetectionName ) then self.relReportClinVsFirstDetectionName := '';
      if( chartName = self.relReportDeadVsFirstDetectionName ) then self.relReportDeadVsFirstDetectionName := '';
    end
  ;


  function TDetectionParams.hasChartName( const chartName: string; const whichChart: TSMChart ): boolean;
    begin
      result := false;

      case whichChart of
        DetProbObsVsTimeClinical: result := ( chartName = self.relObsVsTimeClinicalName );
        DetProbObsVsTimeDead: result := ( chartName = self.relObsVsTimeDeadName );
        DetProbReportClinVsFirstDetection: result := ( chartName = self.relReportClinVsFirstDetectionName );
        DetProbReportDeadVsFirstDetection: result := ( chartName = self.relReportDeadVsFirstDetectionName );
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

      if( fnDictionary.contains( _relObsVsTimeDeadName ) ) then
        begin
          if( not( fnDictionary.value( _relObsVsTimeDeadName ).fn is TRelFunction ) ) then
            begin
              setRelObsVsTimeDeadName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _relReportClinVsFirstDetectionName ) ) then
        begin
          if( not( fnDictionary.value( _relReportClinVsFirstDetectionName ).fn is TRelFunction ) ) then
            begin
              setRelReportClinVsFirstDetectionName( '' );
              result := false;
            end
          ;
        end
      ;

      if( fnDictionary.contains( _relReportDeadVsFirstDetectionName ) ) then
        begin
          if( not( fnDictionary.value( _relReportDeadVsFirstDetectionName ).fn is TRelFunction ) ) then
            begin
              setRelReportDeadVsFirstDetectionName( '' );
              result := false;
            end
          ;
        end
      ;
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

      str := str + '    <prob-detect-vs-time-clinical>' + endl;
      str := str + relObsVsTimeClinical.ssXml( 3 );
      str := str + '    </prob-detect-vs-time-clinical>' + endl;

      str := str + '    <prob-detect-vs-time-dead>' + endl;
      str := str + relObsVsTimeDead.ssXml( 3 );
      str := str + '    </prob-detect-vs-time-dead>' + endl;

      str := str + '    <prob-report-clinical-signs-vs-time-since-outbreak>' + endl;
      str := str + relReportClinVsFirstDetection.ssXml( 3 );
      str := str + '    </prob-report-clinical-signs-vs-time-since-outbreak>' + endl;

      str := str + '    <prob-report-death-from-disease-vs-time-since-outbreak>' + endl;
      str := str + relReportDeadVsFirstDetection.ssXml( 3 );
      str := str + '    </prob-report-death-from-disease-vs-time-since-outbreak>' + endl;

      str := str + '  </detection-model>' + endl;

      result := str;
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

      e := sdew.GetElementByName( model, 'prob-detect-vs-time-clinical' );
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

      e := sdew.GetElementByName( model, 'prob-report-clinical-signs-vs-time-since-outbreak' );
      if( nil <> e ) then
        begin
          fn := createRelFromXml( e, sdew );
          if( nil <> fn ) then
            begin
              fn.dbField := word( DetProbReportClinVsFirstDetection );
              if( strIsEmpty( fn.name ) ) then
                fn.name := 'Probability of reporting clinical signs versus days of outbreak' + ' ' + self.prodTypeDescr
              ;
              self.relReportClinVsFirstDetectionName := fnDictionary.checkAndInsert( fn );
              self.useDetection := true;
            end
          else
            appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of reporting clinical signs.' ) )
          ;
        end
      else
        appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of reporting clinical signs.' ) )
      ;

      e := sdew.GetElementByName( model, 'prob-detect-vs-time-dead' );
      if( nil <> e ) then
        begin
          fn := createRelFromXml( e, sdew );

          if( nil <> fn ) then
            begin
              fn.dbField := word( DetProbObsVsTimeDead );
              if( strIsEmpty( fn.name ) ) then
                fn.name := 'Probability of observing dead from disease versus days dead' + ' ' + self.prodTypeDescr
              ;
              self.relObsVsTimeDeadName := fnDictionary.checkAndInsert( fn );
              self.useDetection := true;
            end
          else
            appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of observing dead from disease.' ) )
          ;
        end
      else
        appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of observing dead from disease.' ) )
      ;

      e := sdew.GetElementByName( model, 'prob-report-death-from-disease-vs-time-since-outbreak' );
      if( nil <> e ) then
        begin
          fn := createRelFromXml( e, sdew );
          if( nil <> fn ) then
            begin
              fn.dbField := word( DetProbReportDeadVsFirstDetection );
              if( strIsEmpty( fn.name ) ) then
                fn.name := 'Probability of reporting death from disease versus days of outbreak' + ' ' + self.prodTypeDescr
              ;
              self.relReportDeadVsFirstDetectionName := fnDictionary.checkAndInsert( fn );
              self.useDetection := true;
            end
          else
            appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of reporting death from disease.' ) )
          ;
        end
      else
        appendToPstring( errMsg, tr( 'Detection XML does not contain a valid function for the probability of reporting clinical signs.' ) )
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
