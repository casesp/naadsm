unit FrameCostsVacc;

(*
FrameCostsVacc.pas/dfm
----------------------
Begin: 2007/04/17
Last revision: $Date: 2008/03/12 22:10:50 $ $Author: areeves $
Version number: $Revision: 1.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

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


  type TFrameCostsVacc = class( TFrame )
      pnlCostParams: TPanel;
      lblVaccAdditionalPerAnimal: TLabel;
      rleVaccAdditionalPerAnimal: TREEdit;
      lblVaccSetupPerUnit: TLabel;
      rleVaccSetupPerUnit: TREEdit;
      lblVaccBaselinePerAnimal: TLabel;
      rleVaccBaselinePerAnimal: TREEdit;
      lblVaccThreshold: TLabel;
      rleVaccThreshold: TREEdit;
      lblDollars6: TLabel;
      lblDollars7: TLabel;
      lblDollars8: TLabel;

      pnlNoDestruction: TPanel;
      lblNoDestruction: TLabel;

      procedure processTextEntry( Sender: TObject );

      { This function deals with a little bug in TREEdit.}
      procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

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
    GuiStrUtils,
    ControlUtils,
    I88n
  ;

  constructor TFrameCostsVacc.create( AOWner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      _loading := false;

      pnlCostParams.BevelOuter := bvNone;

      pnlNoDestruction.BevelOuter := bvNone;
      pnlNoDestruction.Align := alClient;

      rleVaccAdditionalPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleVaccBaselinePerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleVaccSetupPerUnit.InputExpression := RE_DOLLAR_INPUT;

      rleVaccThreshold.InputExpression := RE_INTEGER_INPUT;
    end
   ;
   
   
  procedure TFrameCostsVacc.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameCostsVacc.dfm
      // File date: Wed Apr 25 11:56:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblVaccAdditionalPerAnimal.Caption := tr( 'Additional cost for each animal vaccinated beyond the threshold (per animal):' );
          lblVaccSetupPerUnit.Caption := tr( 'Cost of site setup (per unit):' );
          lblVaccBaselinePerAnimal.Caption := tr( 'Baseline vaccination cost (per animal):' );
          lblVaccThreshold.Caption := tr( 'Number of animals that may be vaccinated before the cost increases:' );
          lblDollars6.Caption := tr( '$' );
          lblDollars7.Caption := tr( '$' );
          lblDollars8.Caption := tr( '$' );
          lblNoDestruction.Caption := tr( '(Vaccination is not used with the selected production type.)' );
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameCostsVacc.processTextEntry( Sender: TObject );
  	begin
      _prodType.costParams.vaccSetupPerUnit := myStrToFloat( rleVaccSetupPerUnit.text, -1.0 );
      _prodType.costParams.vaccThreshold := myStrToInt( rleVaccThreshold.text, -1 );
      _prodType.costParams.vaccBaselinePerAnimal := myStrToFloat( rleVaccBaselinePerAnimal.text, -1.0 );
      _prodType.costParams.vaccAdditionalPerAnimal := myStrToFloat( rleVaccAdditionalPerAnimal.text, -1.0 );

      _prodType.updated := true;
    end
  ;


  // This function deals with a little bug in TREEdit.
  procedure TFrameCostsVacc.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    var
      rle: TREEdit;
    begin
      if( sender is TREEdit ) then
        begin
          rle := sender as TREEdit;
          if( rle.SelLength = length( rle.Text ) ) then rle.Text := '';
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameCostsVacc.isValid(): boolean;
    begin
      // There isn't actually anything that needs to be validated here:
      // the regular expressions should take care of everything
      result := true;
    end
  ;
  

  function TFrameCostsVacc.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameCostsVacc.setProdType( val: TProductionType );
  	begin
    	_prodType := val;

      if( _prodType.isVaccTarget ) then
        begin
          pnlNoDestruction.Visible := false;
          pnlNoDestruction.Align := alNone;

          pnlCostParams.Visible := true;
          pnlCostParams.Align := alClient;

          _loading := true;

          if( 0.0 <= _prodType.costParams.VaccAdditionalPerAnimal ) then
            rleVaccAdditionalPerAnimal.text := uiFloatToStr( _prodType.costParams.VaccAdditionalPerAnimal, 2, true )
          else
            rleVaccAdditionalPerAnimal.text := ''
          ;


          if( 0.0 <= _prodType.costParams.VaccBaselinePerAnimal ) then
            rleVaccBaselinePerAnimal.text := uiFloatToStr( _prodType.costParams.VaccBaselinePerAnimal, 2, true )
          else
            rleVaccBaselinePerAnimal.text := ''
          ;


          if( 0.0 <= _prodType.costParams.VaccSetupPerUnit ) then
            rleVaccSetupPerUnit.text := uiFloatToStr( _prodType.costParams.VaccSetupPerUnit, 2, true )
          else
            rleVaccSetupPerUnit.text := ''
          ;


          if( 0 <= _prodType.costParams.VaccThreshold ) then
            rleVaccThreshold.text := intToStr( _prodType.costParams.VaccThreshold )
          else
            rleVaccThreshold.text := ''
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
