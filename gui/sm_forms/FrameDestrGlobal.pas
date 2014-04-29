unit FrameDestrGlobal;

(*
FrameDestrGlobal.pas/dfm
------------------------
Begin: 2005/06/10
Last revision: $Date: 2011-03-31 03:55:38 $ $Author: areeves $
Version: $Revision: 1.20.10.1 $
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

    GlobalControlParams,
    GlobalControlParamsList,

		FrameFunctionEditor,
		FrameSMFunctionEditor
	;

	type TFrameDestrGlobal = class( TFrame )
      pnlDestrGlobal: TPanel;
      pnlUseDestrGlobal: TPanel;
      cbxUseDestruction: TCheckBox;
      pnlDestrParams: TPanel;
      lblDestrCapacity: TLabel;
      lblDestrProgramDelay: TLabel;
      smrDestrCapacity: TFrameSMFunctionEditor;
      rleDestrProgramDelay: TREEdit;
      imgRel1: TImage;

    	procedure cbxUseDestructionClick(Sender: TObject);
      procedure processText( sender: TObject );

		protected
      // properties
    	_ctrlParams: TGlobalControlParams;
      _ctrlList: TGlobalControlParamsList;
      
      procedure translateUI();
      
      // properties
      procedure setCtrlParams( val: TGlobalControlParams );
      function getCtrlParams(): TGlobalControlParams;
      function getEntityList(): TGlobalControlParamsList;
      procedure setEntityList( newList:TGlobalControlParamsList );

    public
    	constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure updateDisplay();

      // properties
			property ctrlParams: TGlobalControlParams read getCtrlParams write setCtrlParams;
      property entityList: TGlobalControlParamsList read getEntityList write setEntityList;
		end
	;

implementation

{$R *.dfm}

	uses
  	RegExpDefs,
    MyStrUtils,
    I88n,
    
    FormSMWizardBase,
    ChartFunction,
    FunctionEnums
  ;

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameDestrGlobal.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _ctrlParams := nil;
      smrDestrCapacity.setForm( AOwner as TFormSMWizardBase );
      smrDestrCapacity.chartType := CTRel;
      smrDestrCapacity.minY := 0.0;
      smrDestrCapacity.maxY := 0.0; // No maximum for this chart.
      smrDestrCapacity.xUnits := UDays;
      smrDestrCapacity.yUnits := UHerdsPerDay;
      smrDestrCapacity.setChartField( DestrCapacityGlobal );

      rleDestrProgramDelay.InputExpression := RE_INTEGER_INPUT;
    end
  ;


  procedure TFrameDestrGlobal.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameDestrGlobal.dfm
      // File date: Fri Jan 13 09:25:25 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxUseDestruction.Caption := tr( 'Use destruction for disease control for some or all production types' );
          lblDestrCapacity.Caption := tr( 'Destruction capacity (units per day over time):' );
          lblDestrProgramDelay.Caption := tr( 'Delay before implementing destruction program (days):' );
          imgRel1.Hint := tr( 'This parameter is a relational function' );
        end
      ;

    end
  ;


  destructor TFrameDestrGlobal.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



  function TFrameDestrGlobal.getEntityList(): TGlobalControlParamsList;
    begin
      result := _ctrlList;
    end;

  procedure TFrameDestrGlobal.setEntityList( newList:TGlobalControlParamsList );
    begin
      _ctrlList := newList;
      smrDestrCapacity.setModelList( _ctrlList );
    end;





//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	procedure TFrameDestrGlobal.updateDisplay();
		begin
    	if( _ctrlParams.useDestructionGlobal ) then
      	begin
       		cbxUseDestruction.checked := true;
          pnlDestrParams.visible := true;
        end
      else
      	begin
        	cbxUseDestruction.checked := false;
          pnlDestrParams.visible := false;
        end
      ;

      // Do this regardless of whether destruction will occur.
      // The chart editor can handle nil charts, but it needs the
      // reference to the model entity _ctrlParams.
      smrDestrCapacity.showChart( _ctrlParams, _ctrlParams.relDestrCapacity, DestrCapacityGlobal );

      // This may or may not be visible, but the text should be set regardless.
      rleDestrProgramDelay.Text := intToStr( ctrlParams.destrProgramDelay );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFrameDestrGlobal.cbxUseDestructionClick(Sender: TObject);
    begin
      _ctrlParams.useDestructionGlobal := cbxUseDestruction.Checked;
      pnlDestrParams.visible := cbxUseDestruction.Checked;
    end
  ;


  procedure TFrameDestrGlobal.processText( sender: TObject );
    begin
    	_ctrlParams.destrProgramDelay := myStrToInt( rleDestrProgramDelay.Text );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  procedure TFrameDestrGlobal.setCtrlParams( val: TGlobalControlParams ); begin _ctrlParams := val; end;
  function TFrameDestrGlobal.getCtrlParams(): TGlobalControlParams; begin Result := _ctrlParams; end;
//-----------------------------------------------------------------------------


end.
