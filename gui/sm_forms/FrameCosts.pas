unit FrameCosts;

(*
FrameCosts.pas/dfm
------------------
Begin: 2005/12/15
Last revision: $Date: 2013-06-27 19:11:30 $ $Author: areeves $
Version number: $Revision: 1.12.16.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2008 Colorado State University

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


	type TFrameCosts = class( TFrame )
      cbxUseCosts: TCheckBox;

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
      lblVaccAdditionalPerAnimal: TLabel;
      rleVaccAdditionalPerAnimal: TREEdit;
      lblVaccSetupPerUnit: TLabel;
      rleVaccSetupPerUnit: TREEdit;
      lblVaccBaselinePerAnimal: TLabel;
      rleVaccBaselinePerAnimal: TREEdit;
      lblVaccThreshold: TLabel;
      rleVaccThreshold: TREEdit;
      lblDestructionCosts: TLabel;
      lblVaccinationCosts: TLabel;
      lblDollars2: TLabel;
      lblDollars1: TLabel;
      lblDollars3: TLabel;
      lblDollars4: TLabel;
      lblDollars5: TLabel;
      lblDollars6: TLabel;
      lblDollars7: TLabel;
      lblDollars8: TLabel;

      procedure processClick( Sender: TObject );
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

      procedure updateDisplay();

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
    MyStrUtils
  ;

  constructor TFrameCosts.create( AOWner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      _loading := false;

      rleDestrAppraisalPerUnit.InputExpression := RE_DOLLAR_INPUT;
      rleDestrCleaningPerUnit.InputExpression := RE_DOLLAR_INPUT;
      rleDestrDisposalPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleDestrEuthanasiaPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleDestrIndemnificationPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleVaccAdditionalPerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleVaccBaselinePerAnimal.InputExpression := RE_DOLLAR_INPUT;
      rleVaccSetupPerUnit.InputExpression := RE_DOLLAR_INPUT;

      rleVaccThreshold.InputExpression := RE_INTEGER_INPUT;

    end
   ;
   
 
  procedure TFrameCosts.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameCosts.dfm
      // File date: Thu Jan 12 15:39:05 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxUseCosts.Caption := tr( 'Track direct costs for this production type' );
          lblDestrAppraisalPerUnit.Caption := tr( 'Cost of appraisal (per unit):' );
          lblDestrCleaningPerUnit.Caption := tr( 'Cost of cleaning and disinfection (per unit):' );
          lblDestrIndemnificationPerAnimal.Caption := tr( 'Indemnification (per animal):' );
          lblDestrDisposalPerAnimal.Caption := tr( 'Carcass disposal (per animal):' );
          lblDestrEuthanasiaPerAnimal.Caption := tr( 'Euthanasia (per animal):' );
          lblVaccAdditionalPerAnimal.Caption := tr( 'Additional cost for each animal vaccinated beyond the threshold (per animal):' );
          lblVaccSetupPerUnit.Caption := tr( 'Cost of site setup (per unit):' );
          lblVaccBaselinePerAnimal.Caption := tr( 'Baseline vaccination cost (per animal):' );
          lblVaccThreshold.Caption := tr( 'Number of animals that may be vaccinated before the cost increases:' );
          lblDestructionCosts.Caption := tr( 'Destruction costs:' );
          lblVaccinationCosts.Caption := tr( 'Vaccination costs:' );
          lblDollars2.Caption := tr( '$' );
          lblDollars1.Caption := tr( '$' );
          lblDollars3.Caption := tr( '$' );
          lblDollars4.Caption := tr( '$' );
          lblDollars5.Caption := tr( '$' );
          lblDollars6.Caption := tr( '$' );
          lblDollars7.Caption := tr( '$' );
          lblDollars8.Caption := tr( '$' );
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameCosts.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
        	//_prodType.costParams.useCosts := cbxUseCosts.checked;
          _prodType.updated := true;
        end
      ;

      updateDisplay();
    end
  ;


  procedure TFrameCosts.processTextEntry( Sender: TObject );
  	begin
      _prodType.costParams.destrAppraisalPerUnit := myStrToFloat( rleDestrAppraisalPerUnit.text, -1.0 );
      _prodType.costParams.destrCleaningPerUnit := myStrToFloat( rleDestrCleaningPerUnit.text, -1.0 );
      _prodType.costParams.destrEuthanasiaPerAnimal := myStrToFloat( rleDestrEuthanasiaPerAnimal.text, -1.0 );
      _prodType.costParams.destrIndemnificationPerAnimal := myStrToFloat( rleDestrIndemnificationPerAnimal.text, -1.0 );
      _prodType.costParams.destrDisposalPerAnimal := myStrToFloat( rleDestrDisposalPerAnimal.text, -1.0 );

      _prodType.costParams.vaccSetupPerUnit := myStrToFloat( rleVaccSetupPerUnit.text, -1.0 );
      _prodType.costParams.vaccThreshold := myStrToInt( rleVaccThreshold.text, -1 );
      _prodType.costParams.vaccBaselinePerAnimal := myStrToFloat( rleVaccBaselinePerAnimal.text, -1.0 );
      _prodType.costParams.vaccAdditionalPerAnimal := myStrToFloat( rleVaccAdditionalPerAnimal.text, -1.0 );

      _prodType.updated := true;
    end
  ;


  // This function deals with a little bug in TREEdit.
  procedure TFrameCosts.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
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
// Display
//-----------------------------------------------------------------------------
	procedure TFrameCosts.updateDisplay();
  	begin
      pnlCostParams.Visible := cbxUseCosts.Checked;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameCosts.isValid(): boolean;
    begin
      // There isn't actually anything that needs to be validated here:
      // the regular expressions should take care of everything
      result := true;
    end
  ;
  

  function TFrameCosts.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameCosts.setProdType( val: TProductionType );
  	begin
    	_loading := true;

    	_prodType := val;

      //cbxUseCosts.Checked := _prodType.costParams.useCosts;

      if( 0.0 <= _prodType.costParams.DestrAppraisalPerUnit ) then
        rleDestrAppraisalPerUnit.text := myFloatToStr( _prodType.costParams.DestrAppraisalPerUnit, 2, true )
      else
        rleDestrAppraisalPerUnit.text := ''
      ;

      if( 0.0 <= _prodType.costParams.DestrCleaningPerUnit ) then
        rleDestrCleaningPerUnit.text := myFloatToStr( _prodType.costParams.DestrCleaningPerUnit, 2, true )
      else
        rleDestrCleaningPerUnit.text := ''
      ;

      if( 0.0 <= _prodType.costParams.DestrDisposalPerAnimal ) then
        rleDestrDisposalPerAnimal.text := myFloatToStr( _prodType.costParams.DestrDisposalPerAnimal, 2, true )
      else
        rleDestrDisposalPerAnimal.text := ''
      ;


      if( 0.0 <= _prodType.costParams.DestrEuthanasiaPerAnimal ) then
        rleDestrEuthanasiaPerAnimal.text := myFloatToStr( _prodType.costParams.DestrEuthanasiaPerAnimal, 2, true )
      else
        rleDestrEuthanasiaPerAnimal.text := ''
      ;


      if( 0.0 <= _prodType.costParams.DestrIndemnificationPerAnimal ) then
        rleDestrIndemnificationPerAnimal.text := myFloatToStr( _prodType.costParams.DestrIndemnificationPerAnimal, 2, true )
      else
        rleDestrIndemnificationPerAnimal.text := ''
      ;


      if( 0.0 <= _prodType.costParams.VaccAdditionalPerAnimal ) then
        rleVaccAdditionalPerAnimal.text := myFloatToStr( _prodType.costParams.VaccAdditionalPerAnimal, 2, true )
      else
        rleVaccAdditionalPerAnimal.text := ''
      ;


      if( 0.0 <= _prodType.costParams.VaccBaselinePerAnimal ) then
        rleVaccBaselinePerAnimal.text := myFloatToStr( _prodType.costParams.VaccBaselinePerAnimal, 2, true )
      else
        rleVaccBaselinePerAnimal.text := ''
      ;


      if( 0.0 <= _prodType.costParams.VaccSetupPerUnit ) then
        rleVaccSetupPerUnit.text := myFloatToStr( _prodType.costParams.VaccSetupPerUnit, 2, true )
      else
        rleVaccSetupPerUnit.text := ''
      ;


      if( 0 <= _prodType.costParams.VaccThreshold ) then
        rleVaccThreshold.text := intToStr( _prodType.costParams.VaccThreshold )
      else
        rleVaccThreshold.text := ''
      ;

      _loading := false;

      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------
end.
