unit FrameCostsDestr;

(*
FrameCostsDestr.pas/dfm
------------------------
Begin: 2007/04/17
Last revision: $Date: 2013-06-27 19:11:30 $ $Author: areeves $
Version number: $Revision: 1.6.4.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2007 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

	uses
		Windows,
		Messages, 
		SysUtils, 
		Variants, 
		Classes, 
		Graphics, 
		Controls, 
		Forms,
		Dialogs,
    StdCtrls,
    ExtCtrls,
    REEdit,

    ProductionType
	;


  type TFrameCostsDestr = class( TFrame )
      pnlCostParams: TPanel;
      lblDestrAppraisalPerUnit: TLabel;
      rleDestrAppraisalPerUnit: TREEdit;
      lblDestrCleaningPerUnit: TLabel;
      rleDestrCleaningPerUnit: TREEdit;
      lblDestrIndemnificationPerAnimal: TLabel;
      rleDestrIndemnificationPerAnimal: TREEdit;
      lblDestrDisposalPerAnimal: TLabel;
      rleDestrEuthanasiaPerAnimal: TREEdit;
      lblDestrEuthanasiaPerAnimal: TLabel;
      rleDestrDisposalPerAnimal: TREEdit;
      lblDollars2: TLabel;
      lblDollars1: TLabel;
      lblDollars3: TLabel;
      lblDollars4: TLabel;
      lblDollars5: TLabel;

      pnlNoDestruction: TPanel;
      lblNoDestruction: TLabel;

      procedure processTextEntry( Sender: TObject );

		protected
    	// properties
      _prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      procedure translateUI();

      // properties
      procedure setProdType( val: TProductionType );
      function getProdType(): TProductionType;

		public
      constructor create( AOWner: TComponent ); override;

      function isValid(): boolean;

      // properties
      property prodType: TProductionType read getProdType write setProdType;
		end
	;

implementation

{$R *.dfm}

  uses
    RegExpDefs,
    MyStrUtils,
    ControlUtils,
    I88n
  ;

  constructor TFrameCostsDestr.create( AOWner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      _loading := false;

      pnlCostParams.BevelOuter := bvNone;

      pnlNoDestruction.BevelOuter := bvNone;
      pnlNoDestruction.Align := alClient;
      
      rleDestrAppraisalPerUnit.InputExpression := RE_DOLLAR_INPUT;
      rleDestrCleaningPerUnit.InputExpression := RE_DOLLAR_INPUT;
      rleDestrDisposalPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleDestrEuthanasiaPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleDestrIndemnificationPerAnimal.InputExpression := RE_DOLLAR_INPUT;
    end
   ;
      
   
  procedure TFrameCostsDestr.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameCostsDestr.dfm
      // File date: Wed Apr 25 11:56:53 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblDestrAppraisalPerUnit.Caption := tr( 'Cost of appraisal (per unit):' );
          lblDestrCleaningPerUnit.Caption := tr( 'Cost of cleaning and disinfection (per unit):' );
          lblDestrIndemnificationPerAnimal.Caption := tr( 'Indemnification (per animal):' );
          lblDestrDisposalPerAnimal.Caption := tr( 'Carcass disposal (per animal):' );
          lblDestrEuthanasiaPerAnimal.Caption := tr( 'Euthanasia (per animal):' );
          lblDollars2.Caption := tr( '$' );
          lblDollars1.Caption := tr( '$' );
          lblDollars3.Caption := tr( '$' );
          lblDollars4.Caption := tr( '$' );
          lblDollars5.Caption := tr( '$' );
          lblNoDestruction.Caption := tr( '(Destruction is not used with the selected production type.)' );
        end
      ;

    end
  ;   
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameCostsDestr.processTextEntry( Sender: TObject );
  	begin
      _prodType.costParams.destrAppraisalPerUnit := uiStrToFloat( rleDestrAppraisalPerUnit.text, -1.0 );
      _prodType.costParams.destrCleaningPerUnit := uiStrToFloat( rleDestrCleaningPerUnit.text, -1.0 );
      _prodType.costParams.destrEuthanasiaPerAnimal := uiStrToFloat( rleDestrEuthanasiaPerAnimal.text, -1.0 );
      _prodType.costParams.destrIndemnificationPerAnimal := uiStrToFloat( rleDestrIndemnificationPerAnimal.text, -1.0 );
      _prodType.costParams.destrDisposalPerAnimal := uiStrToFloat( rleDestrDisposalPerAnimal.text, -1.0 );

      _prodType.updated := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameCostsDestr.isValid(): boolean;
    begin
      // There isn't actually anything that needs to be validated here:
      // the regular expressions should take care of everything
      result := true;
    end
  ;
  

  function TFrameCostsDestr.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameCostsDestr.setProdType( val: TProductionType );
  	begin
    	_prodType := val;

      if( _prodType.isDestrTarget ) then
        begin
          pnlNoDestruction.Visible := false;
          pnlNoDestruction.Align := alNone;

          pnlCostParams.Visible := true;
          pnlCostParams.Align := alClient;

          _loading := true;

          if( 0.0 <= _prodType.costParams.DestrAppraisalPerUnit ) then
            rleDestrAppraisalPerUnit.text := uiFloatToStrZeroPadded( _prodType.costParams.DestrAppraisalPerUnit, 2, true )
          else
            rleDestrAppraisalPerUnit.text := ''
          ;

          if( 0.0 <= _prodType.costParams.DestrCleaningPerUnit ) then
            rleDestrCleaningPerUnit.text := uiFloatToStrZeroPadded( _prodType.costParams.DestrCleaningPerUnit, 2, true )
          else
            rleDestrCleaningPerUnit.text := ''
          ;

          if( 0.0 <= _prodType.costParams.DestrDisposalPerAnimal ) then
            rleDestrDisposalPerAnimal.text := uiFloatToStrZeroPadded( _prodType.costParams.DestrDisposalPerAnimal, 2, true )
          else
            rleDestrDisposalPerAnimal.text := ''
          ;


          if( 0.0 <= _prodType.costParams.DestrEuthanasiaPerAnimal ) then
            rleDestrEuthanasiaPerAnimal.text := uiFloatToStrZeroPadded( _prodType.costParams.DestrEuthanasiaPerAnimal, 2, true )
          else
            rleDestrEuthanasiaPerAnimal.text := ''
          ;


          if( 0.0 <= _prodType.costParams.DestrIndemnificationPerAnimal ) then
            rleDestrIndemnificationPerAnimal.text := uiFloatToStrZeroPadded( _prodType.costParams.DestrIndemnificationPerAnimal, 2, true )
          else
            rleDestrIndemnificationPerAnimal.text := ''
          ;

          _loading := false;
        end
      else
        begin
          pnlCostParams.Visible := false;
          pnlCostParams.Align := alNone;

          pnlNoDestruction.Align := alClient;
          horizCenterInside( lblNoDestruction, pnlNoDestruction );
          pnlNoDestruction.Visible := true;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------
end.
