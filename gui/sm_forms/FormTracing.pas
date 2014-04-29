unit FormTracing;

(*
FormTracing.pas/dfm
--------------------
Begin: 2006/02/05
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.5 $
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
    // Standard includes
    Forms,
    Controls,
    StdCtrls,
    Classes,
    ExtCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    // Widgets
    FormSMWizardBase,
    FormProdTypeBase,
    FrameTracing,

    // Data structures
    ProductionType,
    FunctionDictionary
  ;

  type TFormTracing = class( TFormProdTypeBase )
      fraParams: TFrameTracing;

    protected
      procedure translateUI();
    
  		procedure updateDisplay(); override;

      procedure giveListsToEditors(); override;
      procedure prepFunctionDicts(); override;

      function dataIsValid(): boolean; override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType ); override;

    public
    	constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

    end
  ;


  const
  	DBFORMTRACING: boolean = false; // Set to true to enable debugging messages for this unit.


implementation

{$R *.dfm}

  uses
    MyStrUtils,
    GuiStrUtils,
    ChartFunction,
    ControlUtils,
    I88n,

    FormMain,
    FunctionEnums
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormTracing.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
    end
  ;


  procedure TFormTracing.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormTracing.dfm
      // File date: Thu Feb 8 17:00:23 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Tracing' );
          pnlCaption.Caption := tr( 'Tracing' );
        end
      ;

    end
  ;


  destructor TFormTracing.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormTracing.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.includeTracingGlobal ) then
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


  procedure TFormTracing.updateDisplay();
  	begin
    	if( nil <> _selectedPT ) then
      	begin
          fraParams.Visible := true;
          fraParams.prodType := _selectedPT;
          lblProdType.Caption := ( _selectedPT.productionTypeDescr );
        end
      else
        begin
          fraParams.visible := false;
          lblProdType.Caption := '';
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormTracing.dataIsValid(): boolean;
		begin
      result:= fraParams.isValid();
    end
  ;


  procedure TFormTracing.prepFunctionDicts();
    begin
      // Nothing happens here in this class
    end
  ;

  
  procedure TFormTracing.giveListsToEditors();
    begin
      // Nothing happens here in this class
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormTracing.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.destructionParams.traceDirectContact := src.destructionParams.traceDirectContact;
      dest.destructionParams.traceIndirectContact := src.destructionParams.traceIndirectContact;

      dest.destructionParams.directTracePeriod := src.destructionParams.directTracePeriod;
      dest.destructionParams.directTraceSuccess := src.destructionParams.directTraceSuccess;

      dest.destructionParams.indirectTracePeriod := src.destructionParams.indirectTracePeriod;
      dest.destructionParams.indirectTraceSuccess := src.destructionParams.indirectTraceSuccess;

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
  RegisterClass( TFormTracing );


end.
