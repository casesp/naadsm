unit FrameTracingGlobal;

(*
FrameTracingGlobal.pas/dfm
--------------------------
Begin: 2006/02/05
Last revision: $Date: 2010-02-11 18:57:59 $ $Author: areeves $
Version: $Revision: 1.6.6.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2010 Animal Population Health Institute, Colorado State University

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
    
    GlobalControlParams,

		FrameFunctionEditor,
		FrameSMFunctionEditor
	;

	type TFrameTracingGlobal = class( TFrame )
      pnlTracingGlobal: TPanel;
      pnlUseTracingGlobal: TPanel;
      cbxUseTracing: TCheckBox;
      cbxUseHerdExam: TCheckBox;
      cbxUseTesting: TCheckBox;

    	procedure cbxUseTracingClick(Sender: TObject);
      procedure cbxUseHerdExamClick(Sender: TObject);
      procedure cbxUseTestingClick(Sender: TObject);

		protected
      _loading: boolean;
      
      // properties
    	_ctrlParams: TGlobalControlParams;

      procedure translateUI();
      procedure translateUIManual();

      // properties
      procedure setCtrlParams( val: TGlobalControlParams );
      function getCtrlParams(): TGlobalControlParams;

    public
    	constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      // properties
			property ctrlParams: TGlobalControlParams read getCtrlParams write setCtrlParams;
		end
	;

implementation

{$R *.dfm}

	uses
  	RegExpDefs,
    MyStrUtils,
    FormSMWizardBase,
    ChartFunction,
    SMSimulationInput,
    FunctionEnums,
    I88n
  ;

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameTracingGlobal.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      _ctrlParams := nil;
        
    end
  ;


  procedure TFrameTracingGlobal.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FrameTracingGlobal.dfm
      // File date: Fri Apr 25 13:54:40 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxUseTracing.Caption := tr( 'Conduct tracing for some or all production types' );
          cbxUseHerdExam.Caption := tr( 'Examine some or all traced units for clinical signs of disease' );
          cbxUseTesting.Caption := tr( 'Perform diagnostic testing for some or all traced herds' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameTracingGlobal.translateUIManual();
    begin
    end
  ;


  destructor TFrameTracingGlobal.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFrameTracingGlobal.cbxUseTracingClick(Sender: TObject);
    begin
      if( not( _loading ) ) then
        begin
          _ctrlParams.useTracingGlobal := cbxUseTracing.Checked;
          _ctrlParams.updated := true;

          // Enable herd exams only if tracing is performed
          cbxUseHerdExam.Enabled := _ctrlParams.useTracingGlobal;

          if( cbxUseHerdExam.Enabled ) then
            cbxUseHerdExam.Checked := _ctrlParams.useTracingHerdExamGlobal
          else
            cbxUseHerdExam.Checked := false
          ;

          // Enable diagnostic testing only if herd exams are performed
          cbxUseTesting.Enabled := cbxUseHerdExam.Checked;

          if( cbxUseTesting.Enabled ) then
            cbxUseTesting.Checked := _ctrlParams.useTracingTestingGlobal
          else
            cbxUseTesting.Checked := false
          ;
        end
      ;
    end
  ;


  procedure TFrameTracingGlobal.cbxUseHerdExamClick(Sender: TObject);
    begin
      if( not( _loading ) ) then
        begin
          _ctrlParams.useTracingHerdExamGlobal := cbxUseHerdExam.Checked;
          _ctrlParams.updated := true;
          
          // Enable diagnostic testing only if herd exams are performed
          cbxUseTesting.Enabled := cbxUseHerdExam.Checked;

          if( cbxUseTesting.Enabled ) then
            cbxUseTesting.Checked := _ctrlParams.useTracingTestingGlobal
          else
            cbxUseTesting.Checked := false
          ;
        end
      ;
    end
  ;


  procedure TFrameTracingGlobal.cbxUseTestingClick(Sender: TObject);
    begin
      if ( not( _loading ) ) then
        begin
          _ctrlParams.useTracingTestingGlobal := cbxUseTesting.Checked;
          _ctrlParams.updated := true;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TFrameTracingGlobal.setCtrlParams( val: TGlobalControlParams );
    begin
      _loading := true;

      _ctrlParams := val;
      cbxUseTracing.Checked := _ctrlParams.useTracingGlobal;

      // Enable herd exams only if tracing is performed
      cbxUseHerdExam.Enabled := _ctrlParams.useTracingGlobal;

      // If herd exams are enabled, then set the right value.
      cbxUseHerdExam.Checked := _ctrlParams.useTracingGlobal and _ctrlParams.useTracingHerdExamGlobal;

      // if diagnostic testing is to occur it must be during a herd exam
      // this sets the initial conditions of cbxUseTesting when the form opens
      if( cbxUseHerdExam.Checked ) then
        begin
          cbxUseTesting.enabled := true;

          cbxUseTesting.Checked :=
            _ctrlParams.useTracingGlobal
          and
            _ctrlParams.useTracingHerdExamGlobal
          and
            _ctrlParams.useTracingTestingGlobal
          ;
        end
      else
        begin
          cbxUseTesting.Checked := false;
          cbxUseTesting.Enabled := false;
        end
      ;
      
      _loading := false;
    end
  ;


  function TFrameTracingGlobal.getCtrlParams(): TGlobalControlParams; begin Result := _ctrlParams; end;
//-----------------------------------------------------------------------------


end.
