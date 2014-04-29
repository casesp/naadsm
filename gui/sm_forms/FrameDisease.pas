unit FrameDisease;

(*
FrameDisease.pas/dfm
--------------------
Begin: 2005/03/19
Last revision: $Date: 2011-03-31 05:05:36 $ $Author: areeves $
Version: $Revision: 1.24.4.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

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

    FrameFunctionEditor,
		FrameSMFunctionEditor
  ;

  type TFrameDisease = class( TFrame )
      pnlParams: TPanel;
      pnlUseDiseaseTransition: TPanel;
      pnlDiseaseParams: TPanel;
      cbxTransition: TCheckBox;
      smcImmune: TFrameSMFunctionEditor;
      lblImmune: TLabel;
      smcClinical: TFrameSMFunctionEditor;
      lblClinical: TLabel;
      smcSubclinical: TFrameSMFunctionEditor;
      lblSubclinical: TLabel;
      smcLatent: TFrameSMFunctionEditor;
      lblLatent: TLabel;
      imgPdf1: TImage;
      imgPdf2: TImage;
      imgPdf3: TImage;
      imgPdf4: TImage;

      smrPrevalence: TFrameSMFunctionEditor;
      lblPrevalence: TLabel;
      imgPrevalence: TImage;

  	protected
    	myParent: TWinControl;
      
      procedure translateUI();
      
    public
      constructor create( AOwner: TComponent ); override;
      function isValid(): boolean;

    end
  ;

implementation

{$R *.dfm}

	uses
    MyStrUtils,
    MyDialogs,
    I88n,

    FunctionEnums,
    ChartFunction,

    FormSMWizardBase
  ;


	constructor TFrameDisease.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      myParent := AOwner as TWinControl;

      smcLatent.setForm( AOwner as TFormSMWizardBase );
      smcLatent.chartType := CTPdf;
	  	smcLatent.xUnits := UDays;
      smcLatent.setChartField( DLatent );

      smcSubclinical.setForm( AOwner as TFormSMWizardBase );
      smcSubclinical.chartType := CTPdf;
	  	smcSubclinical.xUnits := UDays;
      smcSubclinical.setChartField( DSubclinical );

      smcClinical.setForm( AOwner as TFormSMWizardBase );
      smcClinical.chartType := CTPdf;
	  	smcClinical.xUnits := UDays;
      smcClinical.setChartField( DClinical );

      smcImmune.setForm( AOwner as TFormSMWizardBase );
      smcImmune.chartType := CTPdf;
	  	smcImmune.xUnits := UDays;
      smcImmune.setChartField( DImmune );

      smrPrevalence.setForm( AOwner as TFormSMWizardBase );
      smrPrevalence.chartType := CTRel;
      smrPrevalence.minY := 0.0;
      smrPrevalence.maxY := 100.0; // Remember: this is a percent.
      smrPrevalence.xUnits := UDays;
      smrPrevalence.yUnits := UPercent;
      smrPrevalence.setChartField( DPrevalence );
    end
  ;


  procedure TFrameDisease.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameDisease.dfm
      // File date: Fri Jan 19 17:43:55 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxTransition.Caption := tr( 'Simulate disease progression in units of this production type' );
          lblImmune.Caption := tr( 'Immune period:' );
          lblClinical.Caption := tr( 'Infectious clinical period:' );
          lblSubclinical.Caption := tr( 'Infectious subclinical period:' );
          lblLatent.Caption := tr( 'Latent period:' );
          imgPdf1.Hint := tr( 'This parameter is a probability density function' );
          imgPdf2.Hint := tr( 'This parameter is a probability density function' );
          imgPdf3.Hint := tr( 'This parameter is a probability density function' );
          imgPdf4.Hint := tr( 'This parameter is a probability density function' );
          lblPrevalence.Caption := tr( 'Within-unit prevalence:' );
          imgPrevalence.Hint := tr( 'This parameter is a relational function' );
        end
      ;

    end
  ;


  function TFrameDisease.isValid(): boolean;
  	begin
      result := true;
    end
  ;


end.
