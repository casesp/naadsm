unit FrameDetection;

(*
FrameDetection.pas/dfm
-----------------------
Begin: 2005/06/08
Last revision: $Date: 2011-03-31 05:05:36 $ $Author: areeves $
Version: $Revision: 1.17.6.3 $
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

  type TFrameDetection = class( TFrame )
    pnlParams: TPanel;
    pnlUseDetection: TPanel;
    pnlDetectionParams: TPanel;
    cbxDetect: TCheckBox;
    lblReportVsDaysInfectious: TLabel;
    lblProbReportVsFirstDetection: TLabel;
    smrProbReportVsDaysInfectious: TFrameSMFunctionEditor;
    smrProbVsFirstDetection: TFrameSMFunctionEditor;
    imgRel1: TImage;
    imgRel2: TImage;

    protected
      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); override;

    end
  ;

  
implementation

{$R *.dfm}

	uses
    MyStrUtils,
    I88n,

    FunctionEnums,
    ChartFunction,

    FormSMWizardBase
  ;

	constructor TFrameDetection.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      smrProbReportVsDaysInfectious.setForm( AOwner as TFormSMWizardBase );
      smrProbReportVsDaysInfectious.chartType := CTRel;
      smrProbReportVsDaysInfectious.minY := 0.0;
      smrProbReportVsDaysInfectious.maxY := 0.0; // there is no maximum
      smrProbReportVsDaysInfectious.xUnits := UDays;
      smrProbReportVsDaysInfectious.yUnits := UPercentProbability;
      smrProbReportVsDaysInfectious.setChartField( DetProbObsVsTimeClinical );

      smrProbVsFirstDetection.setForm( AOwner as TFormSMWizardBase );
      smrProbVsFirstDetection.chartType := CTRel;
      smrProbVsFirstDetection.minY := 0.0;
      smrProbVsFirstDetection.maxY := 0.0; // there is no maximum
      smrProbVsFirstDetection.xUnits := UDays;
      smrProbVsFirstDetection.yUnits := UPercentProbability;
      smrProbVsFirstDetection.setChartField( DetProbReportVsFirstDetection );
    end
  ;
  
  
  procedure TFrameDetection.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameDetection.dfm
      // File date: Tue Sep 25 14:57:48 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxDetect.Caption := tr( 'Model disease detection in this production type' );
          lblReportVsDaysInfectious.Caption := tr( 'Probability of observing clinical signs, given the number of days that a unit is clinically infectious:' );
          lblProbReportVsFirstDetection.Caption := tr( 'Probability of reporting an observed clinical unit, given the number of days since disease was first detected in any unit:' );
          imgRel1.Hint := tr( 'This parameter is a relational function' );
          imgRel2.Hint := tr( 'This parameter is a relational function' );
        end
      ;

    end
  ;

end.
