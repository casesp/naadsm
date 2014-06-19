unit FrameTracing;

(*
FrameTracing.pas
----------------
Begin: 2006/02/05
Last revision: $Date: 2013-06-27 19:11:32 $ $Author: areeves $
Version number: $Revision: 1.9.4.6 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2006 - 2011 Colorado State University

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

    FrameFunctionEditor,
    FrameSMFunctionEditor,

    ProductionType
  ;

  type TFrameTracing = class( TFrame )
      pnlTraceTypes: TPanel;
      cbxTraceDirectForward: TCheckBox;
      cbxTraceIndirectForward: TCheckBox;
      cbxTraceDirectBack: TCheckBox;
      cbxTraceIndirectBack: TCheckBox;

      pnlTraceDirectParams: TPanel;
      lblDirectContactParams: TLabel;
      rleSurvDirectTracePeriod: TREEdit;
      lblSurvDirectTracePeriod: TLabel;
      lblSurvDirectSuccess: TLabel;
      rleSurvDirectSuccess: TREEdit;

      pnlTraceIndirectParams: TPanel;
      lblIndirectContactParams: TLabel;
      lblSurvIndirectTracePeriod: TLabel;
      rleSurvIndirectTracePeriod: TREEdit;
      lblSurvIndirectSuccess: TLabel;
      rleSurvIndirectSuccess: TREEdit;

      pnlTracingDelayParams: TPanel;
      lblAllTraceParams: TLabel;
      smcTracingDelay: TFrameSMFunctionEditor;
      imgPdf: TImage;
      lblTracingDelay: TLabel;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

  	protected
    	// properties
    	_prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      procedure translateUI();
      procedure translateUIManual();

      // Display
      procedure setDirectEnabled();
      procedure setIndirectEnabled();

      procedure updateDisplay();

      // properties
      procedure setProdType( val: TProductionType );
      function getProdType(): TProductionType;

    public
    	// construction/initialization/destruction
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

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
    MyDialogs,
    I88n,

    FunctionEnums,
    ChartFunction,

    FormSMWizardBase
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameTracing.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      
      _prodType := nil;

      rleSurvDirectSuccess.InputExpression := RE_DECIMAL_INPUT;
      rleSurvDirectTracePeriod.InputExpression := RE_INTEGER_INPUT;
      rleSurvIndirectSuccess.InputExpression := RE_DECIMAL_INPUT;
      rleSurvIndirectTracePeriod.InputExpression := RE_INTEGER_INPUT;

      smcTracingDelay.setForm( AOwner as TFormSMWizardBase );
      smcTracingDelay.chartType := CTPdf;
	  	smcTracingDelay.xUnits := UDays;
      smcTracingDelay.unitsLocked := true;
      smcTracingDelay.setChartField( TrDelay );

      _loading := false;
    end
  ;


  procedure TFrameTracing.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FrameTracing.dfm
      // File date: Mon Apr 28 16:00:36 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblSurvDirectTracePeriod.Caption := tr( 'Days before detection (critical period):' );
          lblSurvDirectSuccess.Caption := tr( 'Probability of trace success (0 to 1):' );
          lblDirectContactParams.Caption := tr( 'For tracing of DIRECT contacts:' );
          lblIndirectContactParams.Caption := tr( 'For tracing of INDIRECT contacts:' );
          lblSurvIndirectTracePeriod.Caption := tr( 'Days before detection (critical period):' );
          lblSurvIndirectSuccess.Caption := tr( 'Probability of trace success (0 to 1):' );
          imgPdf.Hint := tr( 'This parameter is a probability density function' );
          lblTracingDelay.Caption := tr( 'Delay for carrying out trace investigation:' );
          lblAllTraceParams.Caption := tr( 'For ANY trace investigation:' );
          cbxTraceDirectForward.Caption := tr( 'Conduct TRACE FORWARD investigations to search for DIRECT contacts where the reported unit was the SOURCE of contact and was of this production type' );
          cbxTraceIndirectForward.Caption := tr( 'Conduct TRACE FORWARD investigations to search for INDIRECT contacts where the reported unit was the SOURCE of contact and was of this production type' );
          cbxTraceDirectBack.Caption := tr( 'Conduct TRACE BACK investigations to search for DIRECT contacts where the reported unit was the RECIPIENT of contact and was of this production type' );
          cbxTraceIndirectBack.Caption := tr( 'Conduct TRACE BACK investigations to search for INDIRECT contacts where the reported unit was the RECIPIENT of contact and was of this production type' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameTracing.translateUIManual();
    begin
    end
  ;
    

  destructor TFrameTracing.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameTracing.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
          _prodType.updated := true;

          _prodType.tracingParams.traceDirectForward := cbxTraceDirectForward.Checked;
          _prodType.tracingParams.traceIndirectForward := cbxTraceIndirectForward.Checked;
          _prodType.tracingParams.traceDirectBack := cbxTraceDirectBack.Checked;
          _prodType.tracingParams.traceIndirectBack := cbxTraceIndirectBack.Checked;

          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameTracing.processTextEntry( Sender: TObject );
  	begin
      _prodType.tracingParams.directTracePeriod := myStrToInt( rleSurvDirectTracePeriod.Text, -1 );
      _prodType.tracingParams.directTraceSuccess := uiStrToFloat( rleSurvDirectSuccess.Text, -1.0 );

      _prodType.tracingParams.indirectTracePeriod := myStrToInt( rleSurvIndirectTracePeriod.Text, -1 );
      _prodType.tracingParams.indirectTraceSuccess := uiStrToFloat( rleSurvIndirectSuccess.Text, -1.0 );

   		_prodType.updated := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameTracing.updateDisplay();
    var
      anyTracing: boolean;
  	begin
      anyTracing :=
        cbxTraceDirectForward.Checked
      or
        cbxTraceDirectBack.Checked
      or
        cbxTraceIndirectForward.Checked
      or
        cbxTraceIndirectBack.Checked
      ;

      setDirectEnabled();
      setIndirectEnabled();

      lblAllTraceParams.Enabled := anyTracing;
      imgPdf.Enabled := anyTracing;
      lblTracingDelay.Enabled := anyTracing;
      smcTracingDelay.enabled := anyTracing;
    end
  ;


  procedure TFrameTracing.setDirectEnabled();
    var
      enableControls: boolean;
  	begin
      enableControls := ( cbxTraceDirectForward.Checked or cbxTraceDirectBack.Checked );

      if( enableControls and ( 0 <= _prodType.tracingParams.directTracePeriod ) ) then
        rleSurvDirectTracePeriod.Text := intToStr( _prodType.tracingParams.directTracePeriod )
      else
        rleSurvDirectTracePeriod.Text := ''
      ;

      if( enableControls and ( 0.0 <= _prodType.tracingParams.directTraceSuccess ) ) then
        rleSurvDirectSuccess.Text := uiFloatToStr( _prodType.tracingParams.directTraceSuccess )
      else
        rleSurvDirectSuccess.Text := ''
      ;

      lblDirectContactParams.Enabled := enableControls;
      rleSurvDirectTracePeriod.Enabled := enableControls;
      rleSurvDirectSuccess.Enabled := enableControls;
      lblSurvDirectSuccess.enabled := enableControls;
      lblSurvDirectTracePeriod.enabled := enableControls;
    end
  ;


  procedure TFrameTracing.setIndirectEnabled();
    var
      enableControls: boolean;
  	begin
      enableControls := ( cbxTraceIndirectForward.checked or cbxTraceIndirectBack.Checked );

      if( enableControls and ( 0 <= _prodType.tracingParams.indirectTracePeriod ) ) then
        rleSurvIndirectTracePeriod.Text := intToStr( _prodType.tracingParams.indirectTracePeriod )
      else
        rleSurvIndirectTracePeriod.Text := ''
      ;

      if( enableControls and ( 0 <= _prodType.tracingParams.indirectTraceSuccess ) ) then
        rleSurvIndirectSuccess.Text := uiFloatToStr( _prodType.tracingParams.indirectTraceSuccess )
      else
        rleSurvIndirectSuccess.Text := ''
      ;

      lblIndirectContactParams.Enabled := enableControls;
      rleSurvIndirectSuccess.Enabled := enableControls;
      rleSurvIndirectTracePeriod.Enabled := enableControls;
      lblSurvIndirectSuccess.enabled := enableControls;
      lblSurvIndirectTracePeriod.enabled := enableControls;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameTracing.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameTracing.setProdType( val: TProductionType );
  	begin
    	_loading := true;

    	_prodType := val;

      cbxTraceDirectForward.Checked := _prodType.tracingParams.traceDirectForward;
      cbxTraceIndirectForward.checked := _prodType.tracingParams.traceIndirectForward;
      cbxTraceDirectBack.Checked := _prodType.tracingParams.traceDirectBack;
      cbxTraceIndirectBack.checked := _prodType.tracingParams.traceIndirectBack;

      // Text entries are updated by the updateDisplay() function, called below.

      _loading := false;

      updateDisplay();
    end
  ;


  function TFrameTracing.isValid(): boolean;
    begin
      result := true;

      if( cbxTraceDirectForward.Checked or cbxTraceDirectBack.Checked ) then
        begin
          if
            ( 0 > uiStrToFloat( rleSurvDirectSuccess.text ) )
          or
            ( 1 < uiStrToFloat( rleSurvDirectSuccess.text ) )
          then
            begin
              msgOK(
                tr( 'Probability of direct trace success must be between 0 and 1, inclusive.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleSurvDirectSuccess.SetFocus();
              result := false;
              exit;
            end
          ;
        end
      ;

      if( cbxTraceIndirectForward.Checked or cbxTraceIndirectBack.Checked ) then
        begin
          if
            ( 0 > uiStrToFloat( rleSurvIndirectSuccess.text ) )
          or
            ( 1 < uiStrToFloat( rleSurvIndirectSuccess.text ) )
          then
            begin
              msgOK(
                tr( 'Probability of indirect trace success must be between 0 and 1, inclusive.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleSurvIndirectSuccess.SetFocus();
              result := false;
              exit;
            end
          ;
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------


end.
