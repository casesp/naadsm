unit FrameTracingGlobal;

(*
FrameTracingGlobal.pas/dfm
--------------------------
Begin: 2006/02/05
Last revision: $Date: 2008/11/25 22:00:31 $ $Author: areeves $
Version: $Revision: 1.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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

    	procedure cbxUseTracingClick(Sender: TObject);

		protected
      _loading: boolean;
      
      // properties
    	_ctrlParams: TGlobalControlParams;

      procedure translateUI();

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
    GuiStrUtils,
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
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameTracingGlobal.dfm
      // File date: Thu Feb 8 17:04:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxUseTracing.Caption := tr( 'Conduct trace-forward (trace-out) investigations for some or all production types' );
        end
      ;

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
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  procedure TFrameTracingGlobal.setCtrlParams( val: TGlobalControlParams );
    begin
      _loading := true;

      _ctrlParams := val;
      cbxUseTracing.Checked := _ctrlParams.useTracingGlobal or (_ctrlParams.sim as TSMSimulationInput).includeTracingGlobal;

      _loading := false;
    end
  ;


  function TFrameTracingGlobal.getCtrlParams(): TGlobalControlParams; begin Result := _ctrlParams; end;
//-----------------------------------------------------------------------------


end.
