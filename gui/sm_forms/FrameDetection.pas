unit FrameDetection;

(*
FrameDetection.pas/dfm
-----------------------
Begin: 2005/06/08
Last revision: $Date: 2011-03-31 22:06:48 $ $Author: areeves $
Version: $Revision: 1.19.2.6 $
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
      lblObsVsDaysInfectious: TLabel;
      lblProbReportClinVsFirstDetection: TLabel;
      smrProbObsVsDaysInfectious: TFrameSMFunctionEditor;
      smrProbReportClinVsFirstDetection: TFrameSMFunctionEditor;
      imgRel1: TImage;
      imgRel3: TImage;
      smrProbObsVsDaysDead: TFrameSMFunctionEditor;
      imgRel2: TImage;
      lblObsVsDaysDead: TLabel;
      imgRel4: TImage;
      lblProbReportDeadVsFirstDetection: TLabel;
      smrProbReportDeadVsFirstDetection: TFrameSMFunctionEditor;

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

    ChartFunction,

    FunctionEnums,
    FormSMWizardBase
  ;

	constructor TFrameDetection.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      smrProbObsVsDaysInfectious.setForm( AOwner as TFormSMWizardBase );
      smrProbObsVsDaysInfectious.chartType := CTRel;
      smrProbObsVsDaysInfectious.minY := 0.0;
      smrProbObsVsDaysInfectious.maxY := 0.0; // there is no maximum
      smrProbObsVsDaysInfectious.xUnits := UDays;
      smrProbObsVsDaysInfectious.yUnits := UPercentProbability;
      smrProbObsVsDaysInfectious.setChartField( DetProbObsVsTimeClinical );

      smrProbObsVsDaysDead.setForm( AOwner as TFormSMWizardBase );
      smrProbObsVsDaysDead.chartType := CTRel;
      smrProbObsVsDaysDead.minY := 0.0;
      smrProbObsVsDaysDead.maxY := 0.0; // there is no maximum
      smrProbObsVsDaysDead.xUnits := UDays;
      smrProbObsVsDaysDead.yUnits := UPercentProbability;
      smrProbObsVsDaysDead.setChartField( DetProbObsVsTimeDead );

      smrProbReportClinVsFirstDetection.setForm( AOwner as TFormSMWizardBase );
      smrProbReportClinVsFirstDetection.chartType := CTRel;
      smrProbReportClinVsFirstDetection.minY := 0.0;
      smrProbReportClinVsFirstDetection.maxY := 0.0; // there is no maximum
      smrProbReportClinVsFirstDetection.xUnits := UDays;
      smrProbReportClinVsFirstDetection.yUnits := UPercentProbability;
      smrProbReportClinVsFirstDetection.setChartField( DetProbReportClinVsFirstDetection );

      smrProbReportDeadVsFirstDetection.setForm( AOwner as TFormSMWizardBase );
      smrProbReportDeadVsFirstDetection.chartType := CTRel;
      smrProbReportDeadVsFirstDetection.minY := 0.0;
      smrProbReportDeadVsFirstDetection.maxY := 0.0; // there is no maximum
      smrProbReportDeadVsFirstDetection.xUnits := UDays;
      smrProbReportDeadVsFirstDetection.yUnits := UPercentProbability;
      smrProbReportDeadVsFirstDetection.setChartField( DetProbReportDeadVsFirstDetection );
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
          lblObsVsDaysInfectious.Caption := tr( 'Probability of observing clinical signs, given the number of days that a unit is clinically infectious:' );
          lblObsVsDaysDead.Caption := tr( 'Probability of observing death from disease, given the number of days that a unit has been dead from disease:' );
          lblProbReportClinVsFirstDetection.Caption := tr( 'Probability of reporting a unit with observed clinical signs, given the number of days since disease was first detected in any unit:' );
          lblProbReportDeadVsFirstDetection.Caption := tr( 'Probability of reporting a unit that is dead from disease, given the number of days since disease was first detected in any unit:' );
          imgRel1.Hint := tr( 'This parameter is a relational function' );
          imgRel2.Hint := tr( 'This parameter is a relational function' );
          imgRel3.Hint := tr( 'This parameter is a relational function' );
          imgRel4.Hint := tr( 'This parameter is a relational function' );
        end
      ;
    end
  ;

end.
