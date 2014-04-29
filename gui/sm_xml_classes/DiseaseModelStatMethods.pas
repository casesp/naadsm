unit DiseaseModelStatMethods;
(*
DiseaseModelStatMethods.pas
---------------------------
Begin: 2006/09/01
Last revision: $Date: 2008/04/18 20:35:19 $ $Author: areeves $
Version number: $Revision: 1.9 $
Project: APHI General Purpose Delphi Library, XML datafile functions
Website: http://www.naadsm.org/opensource/delphi
Author: Shaun Case <Shaun.Case@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)
interface

  uses
    SysUtils,

    ChartFunction
  ;

  type statType = ( NONE, Uniform, Gamma, BetaPert, Piecewise, Triangular, Logistics, Weibull, sPoint, Gaussian, Loglogistics, Pearson5, Lognormal, sBeta, Exponential );

  type pieceList = record
    value:double;
    p:double;
  end;

  type TUnits = class(TObject)
    public
      constructor create();overload;
      constructor create( Units: String );overload;
      function GetUnits():String;
      procedure SetUnits( Units: String );
    protected
      _units: String;
  end;


  type TgenericStatMethod = class(TObject)
    public
      function GetType():statType;
      procedure WriteContents();virtual; abstract;
    protected
      _statType:statType;
    end;

  type TUniform = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( a:double; b:double; units:TUnits );overload;
      destructor destroy();override;
      function GetA():double;
      function GetB():double;
      function GetUnits():TUnits;
      procedure SetA( a:double );
      procedure SetB( b:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;        
    protected
      _a:double;
      _b:double;
      _units:TUnits;
  end;

  type TGamma = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( alpha:double; beta:double; units:TUnits );overload;
      destructor destroy();override;
      function GetAlpha():double;
      function GetBeta():double;
      function GetUnits():TUnits;
      procedure SetAlpha( alpha:double );
      procedure SetBeta( beta:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _alpha:double;
      _beta:double;
      _units:TUnits;
  end;

  type TBetaPert = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( min:double; max:double; mode:double; units:TUnits );overload;
      destructor destroy();override;
      function GetMin():double;
      function GetMax():double;
      function GetMode():double;
      function GetUnits():TUnits;
      procedure SetMin( min:double );
      procedure SetMax( max:double );
      procedure SetMode( mode:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _min:double;
      _mode:double;
      _max:double;
      _units:TUnits;
  end;


  type pieceListArray = array of pieceList;

  type TPiecewise = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( pieces:pieceListArray; units:TUnits );overload;
      destructor destroy();override;
      function GetPieces():pieceListArray;
      function GetUnits():TUnits;
      procedure SetPieces( pieces:pieceListArray );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _pieces:pieceListArray;
      _units:TUnits;
  end;

  type TTriangular = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( a:double; b:double; c:double; units:TUnits );overload;
      destructor destroy();override;
      function GetA():double;
      function GetB():double;
      function GetC():double;
      function GetUnits():TUnits;
      procedure SetA( a:double );
      procedure SetB( b:double );
      procedure SetC( c:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _a:double;
      _b:double;
      _c:double;
      _units:TUnits;
  end;

  type TLogistics = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( location:double; scale:double; units:TUnits );overload;
      destructor destroy();override;
      function GetLocation():double;
      function GetScale():double;
      function GetUnits():TUnits;
      procedure SetLocation( location:double );
      procedure SetScale( scale:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _location:double;
      _scale:double;
      _units:TUnits;
  end;

  type TWeibull = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( alpha:double; beta:double; units:TUnits );overload;
      destructor destroy();override;
      function GetAlpha():double;
      function GetBeta():double;
      function GetUnits():TUnits;
      procedure SetAlpha( alpha:double );
      procedure SetBeta( beta:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _alpha:double;
      _beta:double;
      _units:TUnits;
  end;

  type TPoint = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( point:double; units:TUnits );overload;
      destructor destroy();override;
      function GetPoint():double;
      function GetUnits():TUnits;
      procedure SetPoint( point:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _point:double;
      _units:TUnits;
  end;

  type TGaussian = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( mean:double; stddev:double; units:TUnits );overload;
      destructor destroy();override;
      function GetMean():double;
      function GetStddev():double;
      function GetUnits():TUnits;
      procedure SetMean( mean:double );
      procedure SetStddev( stddev:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _mean:double;
      _stddev:double;
      _units:TUnits;
  end;

  type TLogLogistics = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( location:double; scale:double; units:TUnits );overload;
      destructor destroy();override;
      function GetLocation():double;
      function GetScale():double;
      function GetUnits():TUnits;
      procedure SetLocation( location:double );
      procedure SetScale( scale:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _location:double;
      _scale:double;
      _units:TUnits;
  end;

  type TPearson5 = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( alpha:double; beta:double; units:TUnits );overload;
      destructor destroy();override;
      function GetAlpha():double;
      function GetBeta():double;
      function GetUnits():TUnits;
      procedure SetAlpha( alpha:double );
      procedure SetBeta( beta:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _alpha:double;
      _beta:double;
      _units:TUnits;
  end;

  type TLognormal = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( zeta:double; sigma:double; units:TUnits );overload;
      destructor destroy();override;
      function GetZeta():double;
      function GetSigma():double;
      function GetUnits():TUnits;
      procedure SetZeta( zeta:double );
      procedure SetSigma( sigma:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _zeta:double;
      _sigma:double;
      _units:TUnits;
  end;

  type TBeta = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( alpha:double; beta:double; location:double; scale:double; units:TUnits );overload;
      destructor destroy();override;
      function GetAlpha():double;
      function GetBeta():double;
      function GetLocation():double;
      function GetScale():double;
      function GetUnits():TUnits;
      procedure SetAlpha( alpha:double );
      procedure SetBeta( beta:double );
      procedure SetLocation( location:double );
      procedure SetScale( scale:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;
    protected
      _alpha:double;
      _beta:double;
      _location: double;
      _scale: double;
      _units:TUnits;
  end;

  type TExponential = class(TgenericStatMethod)
    public
      constructor create();overload;
      constructor create( mean:double; units:TUnits );overload;
      destructor destroy();override;
      function GetMean():double;
      function GetUnits():TUnits;
      procedure SetMean( mean:double );
      procedure SetUnits( units:TUnits );
      procedure WriteContents();override;        
    protected
      _mean:double;
      _units:TUnits;
  end;


  type TDiseasePeriod = class(TObject)
    public
    constructor create();overload;
    constructor create( statMethod:TChartFunction; StatType:statType; units: TUnits );overload;
    destructor destroy();override;

    function GetStatMethod():TChartFunction;
    function GetStatType():statType;
    function GetUnits():TUnits;

    procedure SetStatMethod( statMethod:TChartFunction; StatType:statType );

    protected
      _statMethod:TChartFunction;
      _statType:statType;  //Use this later to identify the object stored in statMethod
      _units: TUnits;    
  end;

  //  These exist, separately, just in case we need to add different functionality
  //  to them based on their type later.
  type TInfectiousClinicalPeriod = class(TDiseasePeriod)
  end;

  type TInfectiousSubClinicalPeriod = class(TDiseasePeriod)
  end;

  type TLatentPeriod = class(TDiseasePeriod)
  end;

  type TImmunityPeriod = class(TDiseasePeriod)
  end;

  type TDiseaseModel = class(TObject)
    public
    constructor create();overload;
    constructor create( immunityPeriod:TImmunityPeriod; latentPeriod:TLatentPeriod;
                        infectiousSubClinicalPeriod:TInfectiousSubClinicalPeriod;
                        infectiousClinicalPeriod:TInfectiousClinicalPeriod; prodType:String );overload;
    destructor destroy();override;

    function GetImmunityPeriod():TImmunityPeriod;
    function GetLatentPeriod():TLatentPeriod;
    function GetInfectiousSubClinicalPeriod():TInfectiousSubClinicalPeriod;
    function GetInfectiousClinicalPeriod():TInfectiousClinicalPeriod;
    function GetProductionType():String;

    procedure SetImmunityPeriod( immunityPeriod:TImmunityPeriod );
    procedure SetLatentPeriod( latentPeriod:TLatentPeriod );
    procedure SetInfectiousSubClinicalPeriod( infectiousSubClinicalPeriod:TInfectiousSubClinicalPeriod );
    procedure SetInfectiousClinicalPeriod( infectiousClinicalPeriod:TInfectiousClinicalPeriod );
    procedure SetProductionType( prodType:String );
    procedure WriteContents();

    protected
      _immunityPeriod:TImmunityPeriod;
      _latentPeriod:TLatentPeriod;
      _infectiousSubClinicalPeriod:TInfectiousSubClinicalPeriod;
      _infectiousClinicalPeriod:TInfectiousClinicalPeriod;
      _prodType:String;
  end;

implementation

  uses
    TypInfo,

    DebugWindow,
    USStrUtils
  ;


  function TgenericStatMethod.GetType():statType;
    begin
      result := _statType;
    end;


  constructor TDiseaseModel.create();
    begin
      inherited create();
      _immunityPeriod := nil;
      _latentPeriod := nil;
      _infectiousSubClinicalPeriod := nil;
      _infectiousClinicalPeriod := nil;
      _prodType := '';
    end;

  constructor TDiseaseModel.create( immunityPeriod:TImmunityPeriod; latentPeriod:TLatentPeriod;
                      infectiousSubClinicalPeriod:TInfectiousSubClinicalPeriod;
                      infectiousClinicalPeriod:TInfectiousClinicalPeriod; prodType:String );
    begin
      inherited create();
      _immunityPeriod := immunityPeriod;
      _latentPeriod := latentPeriod;
      _infectiousSubClinicalPeriod := infectiousSubClinicalPeriod;
      _infectiousClinicalPeriod := infectiousClinicalPeriod;
      _prodType := prodType;
    end;

  destructor TDiseaseModel.destroy();
    begin
    end;

  function TDiseaseModel.GetImmunityPeriod():TImmunityPeriod;
    begin
      result := _immunityPeriod;
    end;

  function TDiseaseModel.GetLatentPeriod():TLatentPeriod;
    begin
      result := _latentPeriod;
    end;

  function TDiseaseModel.GetInfectiousSubClinicalPeriod():TInfectiousSubClinicalPeriod;
    begin
      result := _infectiousSubClinicalPeriod;
    end;

  function TDiseaseModel.GetInfectiousClinicalPeriod():TInfectiousClinicalPeriod;
    begin
      result := _infectiousClinicalPeriod
    end;

  function TDiseaseModel.GetProductionType():String;
    begin
      result := _prodType;
    end;

  procedure TDiseaseModel.SetImmunityPeriod( immunityPeriod:TImmunityPeriod );
    begin
      if ( _immunityPeriod <> nil ) then _immunityPeriod.destroy();
      _immunityPeriod := immunityPeriod;
    end;

  procedure TDiseaseModel.SetLatentPeriod( latentPeriod:TLatentPeriod );
    begin
      if ( _latentPeriod <> nil ) then _latentPeriod.destroy();
      _latentPeriod := latentPeriod;
    end;

  procedure TDiseaseModel.SetInfectiousSubClinicalPeriod( infectiousSubClinicalPeriod:TInfectiousSubClinicalPeriod );
    begin
      if ( _infectiousSubClinicalPeriod <> nil ) then _infectiousSubClinicalPeriod.destroy();
      _infectiousSubClinicalPeriod := infectiousSubClinicalPeriod;
    end;

  procedure TDiseaseModel.SetInfectiousClinicalPeriod( infectiousClinicalPeriod:TInfectiousClinicalPeriod );
    begin
      if ( _infectiousClinicalPeriod <> nil ) then _infectiousClinicalPeriod.destroy();
      _infectiousClinicalPeriod := infectiousClinicalPeriod;
    end;
    
  procedure TDiseaseModel.SetProductionType( prodType:String );
    begin
      _prodType := prodType;
    end;

  procedure TDiseaseModel.WriteContents();
    begin
      if ( length( _prodType ) > 0 ) then
        begin
          dbcout( ' ', true );
          dbcout( '_________________________________________________________________', true );
          dbcout( 'Disease model present for: ' + _prodType, true );
          dbcout( ' ', true );

          dbcout( '  latent period, using', true );
          dbcout( '     ' + _latentPeriod.GetUnits().GetUnits() + ' units', true );
          _latentPeriod.GetStatMethod().debug();
          dbcout( ' ', true );

          dbcout( '  infectious subclinical using', true );
          dbcout( '     ' + _infectiousSubClinicalPeriod.GetUnits().GetUnits() + ' units', true );
          _infectiousSubClinicalPeriod.GetStatMethod().debug();
          dbcout( ' ', true );

          dbcout( '  infectious clinical using', true );
          dbcout( '     ' + _infectiousClinicalPeriod.GetUnits().GetUnits() + ' units', true );
          _infectiousClinicalPeriod.GetStatMethod().debug();
          dbcout( ' ', true );

          dbcout( '  immunity period using', true );
          dbcout( '     ' +  _immunityPeriod.GetUnits().GetUnits() + ' units', true );
         _immunityPeriod.GetStatMethod().debug();
         dbcout( '_________________________________________________________________', true );
          dbcout( ' ', true );
        end
      else
        dbcout( 'This model is empty....', true );
    end;
//
//
  constructor TDiseasePeriod.create();
    begin
      inherited create();
      _statMethod := nil;
      _statType := NONE;
      _units := nil;
    end;

  constructor TDiseasePeriod.create( statMethod:TChartFunction; StatType:statType; units: TUnits );
    begin
      inherited create();
      if ( statMethod <> nil ) then
        begin
          _statMethod := statMethod;
          _statType := StatType;
        end
      else
        begin
          _statMethod := nil;
          _statType := NONE;
        end;

      if units <> nil then
        begin
          _units := units;
          //_statMethod.xUnits := chartUnitTypeFromDatabase(_units.GetUnits());
        end
      else
        _units := nil;
    end;

  destructor TDiseasePeriod.destroy();
    begin
      inherited destroy();
      if ( _statMethod <> nil ) then _statMethod.destroy();
      _statType := NONE;
    end;

  function TDiseasePeriod.GetStatMethod():TChartFunction;
    begin
      result := _statMethod;
    end;

  function TDiseasePeriod.GetStatType():statType;
    begin
      result := _statType;
    end;

  function TDiseasePeriod.GetUnits():TUnits;
    begin
      result := _units;
    end;

  procedure TDiseasePeriod.SetStatMethod( statMethod:TChartFunction; StatType:statType );
    begin
      if ( statMethod <> nil ) then
        begin
          if ( _statMethod <> nil ) then _statMethod.destroy();
          _statMethod := statMethod;
          _statType := StatType;
        end;
    end;
//
//

  constructor TUnits.create();
    begin
      inherited create();    
      _units := ''
    end;

  constructor TUnits.create( Units: String );
    begin
      inherited create();    
      _units := Units
    end;

  function TUnits.GetUnits():String;
    begin
      result := _units
    end;

  procedure TUnits.SetUnits( Units:String );
    begin
      _units := Units
    end;
//
//

  constructor TUniform.create();
    begin
      inherited create();    
      _a := 0.0;
      _b := 0.0;
      _statType := Uniform;
    end;

  constructor TUniform.create( a:double; b:double; units:TUnits );
    begin
      inherited create();    
      _a := a;
      _b := b;
      _units := units;
      _statType := Uniform;
    end;

  destructor TUniform.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TUniform.GetA():double;
    begin
      result := _a
    end;

  function TUniform.GetB():double;
    begin
      result := _b
    end;

  function TUniform.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TUniform.SetA( a:double );
    begin
      _a := a
    end;

  procedure TUniform.SetB( b:double );
    begin
      _b := b
    end;

  procedure TUniform.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TUniform.WriteContents();
    begin
      dbcout( '     This uniform stat method is using', true);
      dbcout( '          a: ' + usFloatToStr(_a), true);
      dbcout( '          b: ' + usFloatToStr(_b), true);
    end;
//
//
  constructor TGamma.create();
    begin
      inherited create();
      _alpha := 0.0;
      _beta := 0.0;
      _units := nil;
      _statType := Gamma;      
    end;

  constructor TGamma.create( alpha:double; beta:double; units:TUnits );
    begin
      inherited create();
      _alpha := alpha;
      _beta := beta;
      _units := units;
      _statType := Gamma;
    end;

  destructor TGamma.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TGamma.GetAlpha():double;
    begin
      result := _alpha
    end;

  function TGamma.GetBeta():double;
    begin
      result := _beta
    end;

  function TGamma.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TGamma.SetAlpha( alpha:double );
    begin
      _alpha := alpha
    end;

  procedure TGamma.SetBeta( beta:double );
    begin
      _beta := beta
    end;

  procedure TGamma.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TGamma.WriteContents();
    begin
      dbcout( '     This gamma stat method is using', true);
      dbcout('           alpha: ' + usFloatToStr(_alpha), true);
      dbcout('           beta:  ' + usFloatToStr(_beta), true );
    end;
//
//
  constructor TBetaPert.create();
    begin
      inherited create();
      _min := 0.0;
      _max := 0.0;
      _mode := 0.0;
      _units := nil;
      _statType := BetaPert;
    end;

  constructor TBetaPert.create( min:double; max:double; mode:double; units:TUnits );
    begin
      inherited create();
      _min := min;
      _max := max;
      _mode := mode;
      _units := units;
      _statType := BetaPert;
    end;

  destructor TBetaPert.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TBetaPert.GetMin():double;
    begin
      result := _min
    end;

  function TBetaPert.GetMax():double;
    begin
      result := _max
    end;

  function TBetaPert.GetMode():double;
    begin
      result := _mode
    end;

  function TBetaPert.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TBetaPert.SetMin( min:double );
    begin
      _min := min
    end;

  procedure TBetaPert.SetMax( max:double );
    begin
      _max := max
    end;

  procedure TBetaPert.SetMode( mode:double );
    begin
      _mode := mode
    end;

  procedure TBetaPert.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TBetaPert.WriteContents();
    begin
      dbcout( '     This beta-pert stat method is using', true );
      dbcout( '          min:  ' + usFloatToStr(_min), true);
      dbcout( '          max:  ' + usFloatToStr(_max), true);
      dbcout( '          mode: ' + usFloatToStr(_mode), true);
    end;
//
//
  constructor TPieceWise.create();
    begin
      inherited create();
      SetLength(_pieces,0);
      _units := nil;
      _statType := Piecewise;
    end;

  constructor TPieceWise.create( pieces:pieceListArray; units:TUnits );
    begin
      inherited create();
      _pieces := Copy(pieces);
      _units := units;
      _statType := Piecewise;      
    end;

  destructor TPieceWise.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy();
      if ( length(_pieces) > 0 ) then SetLength(_pieces,0);
    end;

  function TPieceWise.GetPieces():pieceListArray;
    begin
      result := _pieces;
    end;

  function TPieceWise.GetUnits():TUnits;
    begin
      result := _units;
    end;

  procedure TPieceWise.SetPieces( pieces:pieceListArray );
    begin
      if ( length( _pieces ) > 0 ) then SetLength( _pieces, 0 );
      _pieces := Copy( pieces );
    end;

  procedure TPieceWise.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units;
    end;

  procedure TPiecewise.WriteContents();
    var
      I:Integer;
    begin
      dbcout( '     This piecewise stat method is using', true );
      for I := 0 to length( _pieces ) - 1 do
        begin
          dbcout( '          value: ' + usFloatToStr(_pieces[I].value) + ';     p: ' + usFloatToStr(_pieces[I].p ), true );
        end;
    end;

//
//
  constructor TTriangular.create();
    begin
      inherited create();
      _a := 0.0;
      _b := 0.0;
      _statType := Triangular;
    end;

  constructor TTriangular.create( a:double; b:double; c:double; units:TUnits );
    begin
      inherited create();
      _a := a;
      _b := b;
      _c := c;
      _units := units;
      _statType := Triangular;
    end;

  destructor TTriangular.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TTriangular.GetA():double;
    begin
      result := _a
    end;

  function TTriangular.GetB():double;
    begin
      result := _b
    end;

  function TTriangular.GetC():double;
    begin
      result := _c
    end;

  function TTriangular.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TTriangular.SetA( a:double );
    begin
      _a := a
    end;

  procedure TTriangular.SetB( b:double );
    begin
      _b := b
    end;

  procedure TTriangular.SetC( c:double );
    begin
      _c := c
    end;

  procedure TTriangular.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TTriangular.WriteContents();
    begin
      dbcout( '     This triangular stat method is using', true );
      dbcout( '          a: ' + usFloatToStr(_a), true );
      dbcout( '          b: ' + usFloatToStr(_b), true );
      dbcout( '          c: ' + usFloatToStr(_c), true );
    end;
//
//


  constructor TLogistics.create();
    begin
      inherited create();
      _location := 0.0;
      _scale := 0.0;
      _statType := Logistics;      
    end;

  constructor TLogistics.create( location:double; scale:double; units:TUnits );
    begin
      inherited create();    
      _location := location;
      _scale := scale;
      _units := units;
      _statType := Logistics;      
    end;

  destructor TLogistics.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TLogistics.GetLocation():double;
    begin
      result := _location
    end;

  function TLogistics.GetScale():double;
    begin
      result := _scale
    end;

  function TLogistics.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TLogistics.SetLocation( location:double );
    begin
      _location := location
    end;

  procedure TLogistics.SetScale( scale:double );
    begin
      _scale := scale
    end;

  procedure TLogistics.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TLogistics.WriteContents();
    begin
      dbcout( '     This logistic stat method is using', true );
      dbcout( '          location: ' + usFloatToStr(_location), true );
      dbcout( '          scale:    ' + usFloatToStr(_scale), true );
    end;
//
//
  constructor TWeibull.create();
    begin
      inherited create();    
      _alpha := 0.0;
      _beta := 0.0;
      _units := nil;
      _statType := Weibull;
    end;

  constructor TWeibull.create( alpha:double; beta:double; units:TUnits );
    begin
      inherited create();
      _alpha := alpha;
      _beta := beta;
      _units := units;
      _statType := Weibull;      
    end;

  destructor TWeibull.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TWeibull.GetAlpha():double;
    begin
      result := _alpha
    end;

  function TWeibull.GetBeta():double;
    begin
      result := _beta
    end;

  function TWeibull.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TWeibull.SetAlpha( alpha:double );
    begin
      _alpha := alpha
    end;

  procedure TWeibull.SetBeta( beta:double );
    begin
      _beta := beta
    end;

  procedure TWeibull.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TWeibull.WriteContents();
    begin
      dbcout( '     This weibull stat method is using', true );
      dbcout( '          alpha: ' + usFloatToStr(_alpha), true );
      dbcout( '          beta:  ' + usFloatToStr(_beta), true );
    end;
//
//

  constructor TPoint.create();
    begin
      inherited create();    
      _point := 0.0;
      _units := nil;
      _statType := sPoint;
    end;

  constructor TPoint.create( point:double; units:TUnits );
    begin
      inherited create();
      _point := point;
      _units := units;
      _statType := sPoint;      
    end;

  destructor TPoint.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy();
    end;

  function TPoint.GetPoint():double;
    begin
      result := _point;
    end;

  function TPoint.GetUnits():TUnits;
    begin
      result := _units;
    end;

  procedure TPoint.SetPoint( point:double );
    begin
      _point := point
    end;

  procedure TPoint.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units;
    end;

  procedure TPoint.WriteContents();
    begin
      dbcout( '     This point stat method is using', true );
      dbcout( '          point: ' + usFloatToStr(_point), true );
    end;
//
//
  constructor TGaussian.create();
    begin
      inherited create();    
      _mean := 0.0;
      _stddev := 0.0;
      _units := nil;
      _statType := Gaussian;
    end;

  constructor TGaussian.create( mean:double; stddev:double; units:TUnits );
    begin
      inherited create();    
      _mean := mean;
      _stddev := stddev;
      _units := units;
      _statType := Gaussian;
    end;

  destructor TGaussian.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TGaussian.GetMean():double;
    begin
      result := _mean
    end;

  function TGaussian.GetStddev():double;
    begin
      result := _stddev
    end;

  function TGaussian.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TGaussian.SetMean( mean:double );
    begin
      _mean := mean
    end;

  procedure TGaussian.SetStddev( stddev:double );
    begin
      _stddev := stddev
    end;

  procedure TGaussian.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TGaussian.WriteContents();
    begin
      dbcout( '     This gaussian stat method is using', true);
      dbcout( '          mean:   ' + usFloatToStr(_mean), true );
      dbcout( '          stddev: ' + usFloatToStr(_stddev), true );
    end;
//
//
  constructor TLogLogistics.create();
    begin
      inherited create();
      _location := 0.0;
      _scale := 0.0;
      _statType := Loglogistics;
    end;

  constructor TLogLogistics.create( location:double; scale:double; units:TUnits );
    begin
      inherited create();
      _location := location;
      _scale := scale;
      _units := units;
      _statType := Loglogistics;      
    end;

  destructor TLogLogistics.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TLogLogistics.GetLocation():double;
    begin
      result := _location
    end;

  function TLogLogistics.GetScale():double;
    begin
      result := _scale
    end;

  function TLogLogistics.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TLogLogistics.SetLocation( location:double );
    begin
      _location := location
    end;

  procedure TLogLogistics.SetScale( scale:double );
    begin
      _scale := scale
    end;

  procedure TLogLogistics.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TLogLogistics.WriteContents();
    begin
      dbcout( '     This loglogistics stat method is using', true );
      dbcout( '          location: ' + usFloatToStr(_location), true );
      dbcout( '          scale:    ' + usFloatToStr(_scale), true );
    end;
//
//
  constructor TPearson5.create();
    begin
      inherited create();
      _alpha := 0.0;
      _beta := 0.0;
      _units := nil;
      _statType := Pearson5;
    end;

  constructor TPearson5.create( alpha:double; beta:double; units:TUnits );
    begin
      _alpha := alpha;
      _beta := beta;
      _units := units;
      _statType := Pearson5;
    end;

  destructor TPearson5.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TPearson5.GetAlpha():double;
    begin
      result := _alpha
    end;

  function TPearson5.GetBeta():double;
    begin
      result := _beta
    end;

  function TPearson5.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TPearson5.SetAlpha( alpha:double );
    begin
      _alpha := alpha
    end;

  procedure TPearson5.SetBeta( beta:double );
    begin
      _beta := beta
    end;

  procedure TPearson5.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TPearson5.WriteContents();
    begin
      dbcout( '     This pearson5 stat method is using', true );
      dbcout( '          alpha: ' + usFloatToStr(_alpha), true );
      dbcout( '          beta:  ' + usFloatToStr(_beta), true );
    end;
//
//
  constructor TLognormal.create();
    begin
      inherited create();
      _zeta := 0.0;
      _sigma := 0.0;
      _units := nil;
      _statType := Lognormal;
    end;

  constructor TLognormal.create( zeta:double; sigma:double; units:TUnits );
    begin
      inherited create();
      _zeta := zeta;
      _sigma := sigma;
      _units := units;
      _statType := Lognormal;      
    end;

  destructor TLognormal.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TLognormal.GetZeta():double;
    begin
      result := _zeta
    end;

  function TLognormal.GetSigma():double;
    begin
      result := _sigma
    end;

  function TLognormal.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TLognormal.SetZeta( zeta:double );
    begin
      _zeta := zeta
    end;

  procedure TLognormal.SetSigma( sigma:double );
    begin
      _sigma := sigma
    end;

  procedure TLognormal.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TLognormal.WriteContents();
    begin
      dbcout( '     This lognormal stat method is using', true );
      dbcout( '          zeta:  ' + usFloatToStr(_zeta), true );
      dbcout( '          sigma: ' + usFloatToStr(_Sigma), true );
    end;
//
//
  constructor TBeta.create();
    begin
      inherited create();
      _alpha := 0.0;
      _beta := 0.0;
      _location := 0.0;
      _scale := 0.0;
      _units := nil;
      _statType := sBeta;
    end;

  constructor TBeta.create( alpha:double; beta:double; location:double; scale:double; units:TUnits );
    begin
      inherited create();
      _alpha := alpha;
      _beta := beta;
      _location := location;
      _scale := scale;
      _units := units;
      _statType := sBeta;
    end;

  destructor TBeta.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TBeta.GetAlpha():double;
    begin
      result := _alpha
    end;

  function TBeta.GetBeta():double;
    begin
      result := _beta
    end;

  function TBeta.GetLocation():double;
    begin
      result := _location
    end;

  function TBeta.GetScale():double;
    begin
      result := _scale
    end;

  function TBeta.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TBeta.SetAlpha( alpha:double );
    begin
      _alpha := alpha
    end;

  procedure TBeta.SetBeta( beta:double );
    begin
      _beta := beta
    end;

  procedure TBeta.SetLocation( location:double );
    begin
      _location := location
    end;

  procedure TBeta.SetScale( scale:double );
    begin
      _scale := scale
    end;

  procedure TBeta.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TBeta.WriteContents();
    begin
      dbcout( '     This beta stat mehod is using', true );
      dbcout( '          alpha:    ' + usFloatToStr(_alpha), true );
      dbcout( '          beta:     ' + usFloatToStr(_beta), true );
      dbcout( '          location: ' + usFloatToStr(_location), true );
      dbcout( '          scale:    ' + usFloatToStr(_scale), true );
    end;
//
//
  constructor TExponential.create();
    begin
      inherited create();
      _mean := 0.0;
      _units := nil;
      _statType := Exponential;
    end;

  constructor TExponential.create( mean:double; units:TUnits );
    begin
      inherited create();
      _mean := mean;
      _units := units;
      _statType := Exponential;      
    end;

  destructor TExponential.destroy();
    begin
      inherited destroy();
      if ( Assigned( _units ) ) then _units.destroy;
    end;

  function TExponential.GetMean():double;
    begin
      result := _mean
    end;

  function TExponential.GetUnits():TUnits;
    begin
      result := _units
    end;

  procedure TExponential.SetMean( mean:double );
    begin
      _mean := mean
    end;
    
  procedure TExponential.SetUnits( units:TUnits );
    begin
      if ( Assigned( _units ) ) then _units.destroy();
      _units := units
    end;

  procedure TExponential.WriteContents();
    begin
      dbcout( '     This exponential stat method is using', true );
      dbcout( '          mean: ' + usFloatToStr(_mean), true );
    end;
end.
