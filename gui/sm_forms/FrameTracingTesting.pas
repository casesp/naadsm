unit FrameTracingTesting;

(*
FrameTracingTesting.pas
-----------------------
Begin: 2008/04/22
Last revision: $Date: 2011-03-31 05:05:36 $ $Author: areeves $
Version number: $Revision: 1.8.4.3 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2008 - 2011 Animal Population Health Institute, Colorado State University

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

  type TFrameTracingTesting = class( TFrame )
      pnlTestingOptions: TPanel;
      cbxTestDirectForward: TCheckBox;
      cbxTestIndirectForward: TCheckBox;
      cbxTestDirectBack: TCheckBox;
      cbxTestIndirectBack: TCheckBox;

      pnlTestCharacteristics: TPanel;
      imgPdf: TImage;
      lblTestingDelay: TLabel;
      smcTestingDelay: TFrameSMFunctionEditor;
      lblSensitivity: TLabel;
      rleSensitivity: TREEdit;
      rleSpecificity: TREEdit;
      lblSpecificity: TLabel;
      lblTracingNote: TLabel;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

    protected
    	// properties
    	_prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;
      function testingIsUsed(): boolean;

      procedure translateUI();
      procedure translateUIManual();
      
      // Display
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
    ControlUtils,
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
  constructor TFrameTracingTesting.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      
      _prodType := nil;

      rleSensitivity.InputExpression := RE_DECIMAL_INPUT;
      rleSpecificity.InputExpression := RE_DECIMAL_INPUT;

      smcTestingDelay.setForm( AOwner as TFormSMWizardBase );
      smcTestingDelay.chartType := CTPdf;
	  	smcTestingDelay.xUnits := UDays;
      smcTestingDelay.unitsLocked := true;
      smcTestingDelay.setChartField( TeDelay );

      _loading := false;
    end
  ;


  procedure TFrameTracingTesting.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FrameTracingTesting.dfm
      // File date: Mon Apr 28 16:01:06 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblTracingNote.Caption := tr( '* Unit exams must be conducted for this type, or this option will be unavailable' );
          cbxTestDirectForward.Caption := tr( 'Perform diagnostic testing of units identified by TRACE-FORWARD of DIRECT contact*' );
          cbxTestIndirectForward.Caption := tr( 'Perform diagnostic testing of units identified by TRACE-FORWARD of INDIRECT contact*' );
          cbxTestDirectBack.Caption := tr( 'Perform diagnostic testing of units identified by TRACE-BACK of DIRECT contact*' );
          cbxTestIndirectBack.Caption := tr( 'Perform diagnostic testing of units identified by TRACE-BACK of INDIRECT contact*' );
          imgPdf.Hint := tr( 'This parameter is a probability density function' );
          lblTestingDelay.Caption := tr( 'Delay in obtaining test results' );
          lblSensitivity.Caption := tr( 'Unit-level test sensitivity:' );
          lblSpecificity.Caption := tr( 'Unit-level test specificity:' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameTracingTesting.translateUIManual();
    begin
    end
  ;
    

  destructor TFrameTracingTesting.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameTracingTesting.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
          _prodType.updated := true;

          if( cbxTestDirectForward.Enabled ) then
            _prodType.testingParams.testDirectForward := cbxTestDirectForward.Checked
          ;
          if( cbxTestIndirectForward.Enabled ) then
            _prodType.testingParams.testIndirectForward := cbxTestIndirectForward.Checked
          ;
          if( cbxTestDirectBack.Enabled ) then
            _prodType.testingParams.testDirectBack := cbxTestDirectBack.Checked
          ;
          if( cbxTestIndirectBack.Enabled ) then
            _prodType.testingParams.testIndirectBack := cbxTestIndirectBack.Checked
          ;

          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameTracingTesting.processTextEntry( Sender: TObject );
  	begin
      _prodType.testingParams.sensitivity := uiStrToFloat( rleSensitivity.Text, -1.0 );
      _prodType.testingParams.specificity := uiStrToFloat( rleSpecificity.Text, -1.0 );

   		_prodType.updated := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameTracingTesting.updateDisplay();
    var
      val: boolean;
  	begin
      val := testingIsUsed();

      if( val and ( 0.0 <= _prodType.testingParams.sensitivity ) ) then
        rleSensitivity.Text := uiFloatToStr( _prodType.testingParams.sensitivity )
      else
        rleSensitivity.Text := ''
      ;

      if( val and ( 0.0 <= _prodType.testingParams.specificity ) ) then
        rleSpecificity.Text := uiFloatToStr( _prodType.testingParams.specificity )
      else
        rleSpecificity.Text := ''
      ;

      setChildrenEnabled( pnlTestCharacteristics, val );
      smcTestingDelay.enabled := val;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameTracingTesting.testingIsUsed(): boolean;
    begin
      result :=
        cbxTestDirectForward.Checked
      or
        cbxTestDirectBack.Checked
      or
        cbxTestIndirectForward.Checked
      or
        cbxTestIndirectBack.Checked
      ;
    end
  ;

  function TFrameTracingTesting.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameTracingTesting.setProdType( val: TProductionType );
  	begin
    	_loading := true;

    	_prodType := val;

      cbxTestDirectForward.Checked := _prodType.testingParams.testDirectForward and _prodType.tracingParams.examDirectForward;
      cbxTestIndirectForward.Checked := _prodType.testingParams.testIndirectForward and _prodType.tracingParams.examIndirectForward;
      cbxTestDirectBack.Checked := _prodType.testingParams.testDirectBack and _prodType.tracingParams.examDirectBack;
      cbxTestIndirectBack.Checked := _prodType.testingParams.testIndirectBack and _prodType.tracingParams.examIndirectBack;

     cbxTestDirectForward.Enabled := _prodType.tracingParams.examDirectForward;
     cbxTestIndirectForward.Enabled := _prodType.tracingParams.examIndirectForward;
     cbxTestDirectBack.Enabled := _prodType.tracingParams.examDirectBack;
     cbxTestIndirectBack.Enabled := _prodType.tracingParams.examIndirectBack;

      if( 0.0 <= _prodType.testingParams.sensitivity ) then
        rleSensitivity.text := uiFloatToStr( _prodType.testingParams.sensitivity )
      else
        rleSensitivity.text := ''
      ;

      if( 0.0 <= _prodType.testingParams.specificity ) then
        rleSpecificity.text := uiFloatToStr( _prodType.testingParams.specificity )
      else
        rleSpecificity.text := ''
      ;

      _loading := false;

      updateDisplay();
    end
  ;


  function TFrameTracingTesting.isValid(): boolean;
    begin
      result := true;

      if( testingIsUsed() and ( 0 <> length( rleSensitivity.text ) ) ) then
        begin
          if
            ( 0.0 > uiStrToFloat( rleSensitivity.text ) )
          or
            ( 1.0 < uiStrToFloat( rleSensitivity.text ) )
          then
            begin
              msgOK(
                tr( 'Test sensitivity must be between 0 and 1.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleSensitivity.SetFocus();
              result := false;
              exit;
            end
          ;
        end
      ;

      if( testingIsUsed() and ( 0 <> length( rleSpecificity.text ) ) ) then
        begin
          if
            ( 0.0 > uiStrToFloat( rleSpecificity.text ) )
          or
            ( 1.0 < uiStrToFloat( rleSpecificity.text ) )
          then
            begin
              msgOK(
                tr( 'Test specificity must be between 0 and 1.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleSpecificity.SetFocus();
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
