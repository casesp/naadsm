unit FrameVaccination;

(*
FrameVaccination.pas/dfm
------------------------
Begin: 2005/06/08
Last revision: $Date: 2008/11/25 22:00:31 $ $Author: areeves $
Version: $Revision: 1.24 $
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
		Windows, 
		Messages, 
		SysUtils, 
		Variants, 
		Classes, 
		Graphics, 
		Controls, 
		Forms, 
		Dialogs,
		ExtCtrls, 
		StdCtrls,
		
		REEdit,
		
		ProductionType,

		FrameFunctionEditor,
		FrameSMFunctionEditor
	;

	type TFrameVaccination = class( TFrame )
      pnlVaccination: TPanel;

      pnlUseVacc: TPanel;
      cbxVaccinate: TCheckBox;

      pnlVaccParams: TPanel;
      lblVaccImmunePeriod: TLabel;
      lblDaysToImmunity: TLabel;
      lblMinTimeBetwVacc: TLabel;
      smcVaccImmunePeriod: TFrameSMFunctionEditor;
      rleDaysToImmunity: TREEdit;
      rleMinTimeBetwVacc: TREEdit;

      pnlRingVacc: TPanel;
      cbxRingVacc: TCheckBox;
      lblVaccRingRadius: TLabel;
      rleVaccRingRadius: TREEdit;
    imgPdf: TImage;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

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
    	constructor create( AOwner: TComponent ); override;

    	function isValid(): boolean;

      // properties
      property prodType: TProductionType read getProdType write setProdType;
		end
	 ;

implementation

{$R *.dfm}

	uses
  	ChartFunction,
    MyStrUtils,
    GuiStrUtils,
    MyDialogs,
    FormSMWizardBase,
    FunctionEnums,
    RegExpDefs,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFrameVaccination.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      _loading := false;

      rleDaysToImmunity.InputExpression := RE_INTEGER_INPUT;
      rleMinTimeBetwVacc.InputExpression := RE_INTEGER_INPUT;

      rleVaccRingRadius.InputExpression := RE_DECIMAL_INPUT;

      smcVaccImmunePeriod.setForm( AOwner as TFormSMWizardBase );
      smcVaccImmunePeriod.chartType := CTPdf;
	  	smcVaccImmunePeriod.xUnits := UnitsDays;
      smcVaccImmunePeriod.unitsLocked := true;
    end
  ;
  
  
  procedure TFrameVaccination.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameVaccination.dfm
      // File date: Thu Oct 12 14:33:58 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblVaccRingRadius.Caption := tr( 'Radius of vaccination ring (km):' );
          cbxRingVacc.Caption := tr( 'Trigger a vaccination ring upon disease detection in units of this production type' );
          cbxVaccinate.Caption := tr( 'Vaccinate units of this production type as part of disease control efforts' );
          lblVaccImmunePeriod.Caption := tr( 'Vaccine immune period:' );
          lblDaysToImmunity.Caption := tr( 'Delay in unit immunity following vaccination (days):' );
          lblMinTimeBetwVacc.Caption := tr( 'Minimum time between vaccinations (days):' );
          imgPdf.Hint := tr( 'This parameter is a probability density function' );
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
	procedure TFrameVaccination.updateDisplay();
  	begin
      if( cbxVaccinate.Checked ) then
      	begin
          pnlVaccination.Height := pnlUseVacc.Height + pnlVaccParams.Height + 2;
      		pnlVaccParams.Visible := true;
        end
      else
      	begin
        	pnlVaccParams.Visible := false;
          pnlVaccination.Height := pnlUseVacc.Height + 2;
        end
      ;

      lblVaccRingRadius.visible := cbxRingVacc.Checked;
      rleVaccRingRadius.visible := cbxRingVacc.Checked;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameVaccination.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
        	_prodType.vaccinationParams.useVaccination := cbxVaccinate.Checked;
          _prodType.ringVaccParams.useRing := cbxRingVacc.Checked;
          _prodType.updated := true;
        end
      ;

      updateDisplay();
    end
  ;


  procedure TFrameVaccination.processTextEntry( Sender: TObject );
  	begin
     	_prodType.vaccinationParams.daysToImmunity := myStrToInt( rleDaysToImmunity.text, -1 );
      _prodType.ringVaccParams.minTimeBetweenVacc := myStrToInt( rleMinTimeBetwVacc.Text, -1 );
      _prodType.ringVaccParams.ringRadius := myStrToFloat( rleVaccRingRadius.Text, -1.0 );

      _prodType.updated := true;
    end
  ;


  // This function deals with a little bug in TREEdit.
  procedure TFrameVaccination.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
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
// Data validation
//-----------------------------------------------------------------------------
	function TFrameVaccination.isValid(): boolean;
  	begin
      (*
    	if( smcVaccImmunePeriod.isOpen ) then
      	begin
          msgOK(
            'Please finish editing all probability density functions before continuing.',
            'Function editor is open',
            IMGInformation,
            _myParent
          );
          result := false;
        end
      else
      	result := true
      ;
      *)
      result := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameVaccination.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameVaccination.setProdType( val: TProductionType );
  	begin
    	_loading := true;

    	_prodType := val;

      cbxVaccinate.Checked := _prodType.vaccinationParams.useVaccination;

      if( 0 <= _prodType.vaccinationParams.daysToImmunity ) then
        rleDaysToImmunity.Text := intToStr( _prodType.vaccinationParams.daysToImmunity )
      else
        rleDaysToImmunity.Text := ''
      ;

      if( 0 <= _prodType.ringVaccParams.minTimeBetweenVacc ) then
        rleMinTimeBetwVacc.Text := intToStr( _prodType.ringVaccParams.minTimeBetweenVacc )
      else
        rleMinTimeBetwVacc.Text := ''
      ;

      cbxRingVacc.Checked := _prodType.ringVaccParams.useRing;

      if( 0.0 <= _prodType.ringVaccParams.ringRadius ) then
        rleVaccRingRadius.Text := uiFloatToStr( _prodType.ringVaccParams.ringRadius )
      else
        rleVaccRingRadius.Text := ''
      ;

      _loading := false;

      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------

end.
