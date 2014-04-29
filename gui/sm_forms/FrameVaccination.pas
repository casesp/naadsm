unit FrameVaccination;

(*
FrameVaccination.pas/dfm
------------------------
Begin: 2005/06/08
Last revision: $Date: 2013-06-27 19:11:33 $ $Author: areeves $
Version: $Revision: 1.34.4.2 $
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
      imgPdf: TImage;
      rleDaysToImmunity: TREEdit;
      rleMinTimeBetwVacc: TREEdit;
      cbxVaccinateDetected: TCheckBox;

      pnlRingVacc: TPanel;
      cbxRingVacc: TCheckBox;
      lblVaccRingRadius: TLabel;
      rleVaccRingRadius: TREEdit;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

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
	  	smcVaccImmunePeriod.xUnits := UDays;
      smcVaccImmunePeriod.unitsLocked := true;
      smcVaccImmunePeriod.setChartField( VacImmunePeriod );
    end
  ;
  
  
  procedure TFrameVaccination.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameVaccination.dfm
      // File date: Thu Oct 12 14:33:58 2006

      // MODIFIED BY HAND, 7/1/09 (A. Reeves)

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
          cbxVaccinateDetected.Caption := tr( 'Vaccinate detected, infected units of this production type' );
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
          _prodType.ringVaccParams.vaccinateDetected := cbxVaccinateDetected.checked;
          _prodType.ringVaccParams.useRing := cbxRingVacc.Checked;
          
          if ( not cbxVaccinate.Checked ) then
            _prodType.ringVaccParams.vaccPriority := -1
          ;
            
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
      _prodType.ringVaccParams.ringRadius := uiStrToFloat( rleVaccRingRadius.Text, -1.0 );

      _prodType.updated := true;
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

      cbxVaccinateDetected.Checked := _prodType.ringVaccParams.vaccinateDetected;

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
