unit FormTracingGlobal;

(*
FormTracingGlobal.pas/dfm
-------------------------
Begin: 2006/02/05
Last revision: $Date: 2013-06-27 19:11:29 $ $Author: areeves $
Version: $Revision: 1.8.6.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2010 Colorado State University

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
    Buttons,
    FormSMWizardBase,
    Menus,
    ActnPopupCtrl,
    ExtCtrls,
    FrameTracingGlobal,
    GlobalControlParams,
    FunctionDictionary,
    GlobalControlParamsList,
    SMScenario
  ;

  type TFormTracingGlobal = class( TFormSMWizardBase )
      pnlCaption: TPanel;
      fraParams: TFrameTracingGlobal;

    protected
      _ctrlParams: TGlobalControlParams;

      procedure translateUI();  
      procedure translateUIManual();
      
      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

      // property getters/setters
      procedure setCtrlParams( val: TGlobalControlParams );
      function getCtrlParams(): TGlobalControlParams;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

      property ctrlParams: TGlobalControlParams read getCtrlParams write setCtrlParams;
    end
  ;


implementation

{$R *.dfm}

  uses
    ControlUtils,
    MyStrUtils,
    DebugWindow,
    I88n,

    ChartFunction,

    FunctionEnums
  ;

  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.

//-----------------------------------------------------------------------------
// construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormTracingGlobal.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _ctrlParams := nil;
    end
  ;

  procedure TFormTracingGlobal.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FormTracingGlobal.dfm
      // File date: Fri Apr 25 11:58:50 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Global tracing options' );
          pnlCaption.Caption := tr( 'Global tracing options' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormTracingGlobal.translateUIManual();
    begin
    end
  ;


  destructor TFormTracingGlobal.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormTracingGlobal.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      // need detection in order to conduct tracing
    	if( _smScenarioCopy.simInput.includeDetectionGlobal ) then
    		result := inherited showModal( nextFormToShow, formDisplayed, currentFormIndex )
      else
      	begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------

  procedure TFormTracingGlobal.initializeFromSim();
    begin
      _ctrlParams := _smScenarioCopy.simInput.controlParams;
      fraParams.ctrlParams := _ctrlParams;

      updateMasterDisplay();
    end
  ;


  function TFormTracingGlobal.dataIsValid(): boolean;
    begin
      result := true;
    end
  ;


  function TFormTracingGlobal.getDataUpdated(): boolean;
    begin
      result := _ctrlParams.updated;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// properties
//-----------------------------------------------------------------------------
  procedure TFormTracingGlobal.setCtrlParams( val: TGlobalControlParams ); begin _ctrlParams := val; end;
  function TFormTracingGlobal.getCtrlParams(): TGlobalControlParams; begin Result := _ctrlParams; end;
//-----------------------------------------------------------------------------



initialization
  RegisterClass( TFormTracingGlobal );


end.
