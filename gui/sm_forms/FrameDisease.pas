unit FrameDisease;

(*
FrameDisease.pas/dfm
--------------------
Begin: 2005/03/19
Last revision: $Date: 2011-10-04 23:58:12 $ $Author: areeves $
Version: $Revision: 1.25.6.5 $
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
    REEdit,

    ProductionType,

    FrameFunctionEditor,
    FrameSMFunctionEditor
  ;

  type TFrameDisease = class( TFrame )
      pnlParams: TPanel;
      pnlUseDiseaseTransition: TPanel;
      pnlDiseaseParams: TPanel;
      cbxTransition: TCheckBox;
      pnlDiseaseStates: TPanel;
      imgPdf1: TImage;
      lblLatent: TLabel;
      smcLatent: TFrameSMFunctionEditor;
      imgPdf2: TImage;
      lblSubclinical: TLabel;
      smcSubclinical: TFrameSMFunctionEditor;
      imgPdf3: TImage;
      lblClinical: TLabel;
      smcClinical: TFrameSMFunctionEditor;
      imgPdf4: TImage;
      lblImmune: TLabel;
      smcImmune: TFrameSMFunctionEditor;
      pnlPrevalence: TPanel;
      imgPrevInfected: TImage;
      lblPrevInfected: TLabel;
      smrPrevInfected: TFrameSMFunctionEditor;
      imgPrevShedding: TImage;
      lblPrevShedding: TLabel;
      smrPrevShedding: TFrameSMFunctionEditor;
      pnlMortality: TPanel;
      lblMortality: TLabel;
      reProbMortality: TREEdit;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

    protected
      // for internal use
      _myParent: TWinControl;
      _loading: boolean;
      _startingHeight: integer;

      // properties
      _prodType: TProductionType;
      
      procedure translateUI();
      procedure setScrollBarsVisible( const val: boolean );
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
    DebugWindow,
    MyStrUtils,
    MyDialogs,
    I88n,
    RegExpDefs,

    ChartFunction,

    FunctionEnums,
    SMSimulationInput,
    DiseaseParams,

    FormSMWizardBase,
    FormDisease
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameDisease.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _myParent := AOwner as TWinControl;

      dbcout2( _myParent.Name );

      _loading := false;
      _startingHeight := self.Height;

      reProbMortality.InputExpression := RE_DECIMAL_INPUT;

      pnlUseDiseaseTransition.BevelOuter := bvNone;
      pnlDiseaseParams.BevelOuter := bvNone;
      pnlDiseaseStates.BevelOuter := bvNone;
      pnlPrevalence.BevelOuter := bvNone;
      pnlMortality.BevelOuter := bvNone;

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

      smrPrevInfected.setForm( AOwner as TFormSMWizardBase );
      smrPrevInfected.chartType := CTRel;
      smrPrevInfected.minY := 0.0;
      smrPrevInfected.maxY := 100.0; // Remember: this is a percent.
      smrPrevInfected.xUnits := UDays;
      smrPrevInfected.yUnits := UPercent;
      smrPrevInfected.setChartField( DPrevInfected );

      smrPrevShedding.setForm( AOwner as TFormSMWizardBase );
      smrPrevShedding.chartType := CTRel;
      smrPrevShedding.minY := 0.0;
      smrPrevShedding.maxY := 100.0; // Remember: this is a percent.
      smrPrevShedding.xUnits := UDays;
      smrPrevShedding.yUnits := UPercent;
      smrPrevInfected.setChartField( DPrevShedding );
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
          lblPrevInfected.Caption := tr( 'Within-unit prevalence of infection (latent, subclinical, and clinical):' );
          imgPrevInfected.Hint := tr( 'This parameter is a relational function' );
          lblPrevShedding.Caption := tr( 'Within-unit prevalence of infectiousness (subclinical, and clinical):' );
          imgPrevShedding.Hint := tr( 'This parameter is a relational function' );
          lblMortality.Caption := tr( 'Probability that infected units will die from disease:' );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameDisease.setScrollBarsVisible( const val: boolean );
    begin
      ( _myParent as TFormDisease ).sbxAllParams.VertScrollBar.Visible := val;
    end
  ;


  procedure TFrameDisease.updateDisplay();
    var
      usePrevalence: boolean;
    begin
      if( cbxTransition.Checked ) then
        begin
          pnlDiseaseParams.Visible := true;

          usePrevalence := (_prodType.sim as TSMSimulationInput).useWithinHerdPrevalence;

          if( usePrevalence ) then
            begin
              pnlPrevalence.Height := 168;
              setScrollBarsVisible( true );
            end
          else
            begin
              pnlPrevalence.Height := 0;
              setScrollBarsVisible( false );
            end
          ;
        end
      else
        begin
          pnlDiseaseParams.Visible := false;
          setScrollBarsVisible( false );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameDisease.processClick( Sender: TObject );
    begin

      if( not _loading ) then
        begin
          _prodType.diseaseParams.useDisease := cbxTransition.Checked;
          _prodType.updated := true;
          setProdType( _prodType );
        end
      ;
      
      updateDisplay();
    end
  ;


  procedure TFrameDisease.processTextEntry( Sender: TObject );
    begin
      _prodType.diseaseParams.probMortality := uiStrToFloat( reProbMortality.text, -1.0 );

      _prodType.updated := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameDisease.getProdType(): TProductionType;
    begin
      result := _prodType;
    end
  ;


  procedure TFrameDisease.setProdType( val: TProductionType );
    begin
      _prodType := val;

      _loading := true;

      if( nil <> _prodType ) then
        begin
          cbxTransition.Checked := _prodType.useDisease;

          if( _prodType.useDisease ) then
            begin
              smcLatent.showChart( _prodType, _prodType.diseaseParams.pdfDiseaseLatent, DLatent );
              smcSubclinical.showChart( _prodType, _prodType.diseaseParams.pdfDiseaseSubclinical, DSubclinical );
              smcClinical.showChart( _prodType, _prodType.diseaseParams.pdfDiseaseClinical, DClinical );
              smcImmune.showChart( _prodType, _prodType.diseaseParams.pdfDiseaseImmune, DImmune );
              smrPrevInfected.showChart( _prodType, _prodType.diseaseParams.relDiseasePrevInfected, DPrevInfected );
              smrPrevShedding.showChart( _prodType, _prodType.diseaseParams.relDiseasePrevShedding, DPrevShedding );

              if( ( 0.0 <= _prodType.diseaseParams.probMortality ) and ( 1.0 >= _prodType.diseaseParams.probMortality ) ) then
                reProbMortality.Text := uiFloatToStr( _prodType.diseaseParams.probMortality )
              else
                reProbMortality.Text := ''
              ;

              updateDisplay();
            end
          else
            begin
              setScrollBarsVisible( false );
              pnlDiseaseParams.Visible := false;
            end
          ;
        end
      ;

      _loading := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  function TFrameDisease.isValid(): boolean;
    begin
      result := true;
    end
  ;
//-----------------------------------------------------------------------------


end.
