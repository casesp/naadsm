unit FormTracing;

(*
FormTracing.pas/dfm
--------------------
Begin: 2006/02/05
Last revision: $Date: 2010-09-09 14:29:37 $ $Author: rhupalo $
Version: $Revision: 1.9.4.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2009 Animal Population Health Institute, Colorado State University

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
      // need detection in order to conduct tracing
    	if (( _smScenarioCopy.simInput.includeTracingGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal)) then
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
          lblProdType.Caption := _selectedPT.productionTypeDescr;

          // show chart, even if no chart is used.  This sets the production type for the chart editor
          fraParams.smcTracingDelay.showChart(
            _selectedPT,
            _selectedPT.tracingParams.pdfTraceDelay,
            TrDelay
          );

					fraParams.prodType := _selectedPT;
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
  	var
      it: TFunctionDictionaryIterator;
  	begin
			fraParams.smcTracingDelay.cboChartList.clear();

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case( it.value().fn.dbField ) of
                  integer( TrDelay ): fraParams.smcTracingDelay.appendFunction( it.value().fn );
                end;
              end
            ;
          end
        ;

        it.incr();
      until ( nil = it.value() );

      it.Free();
    end
  ;

  
  procedure TFormTracing.giveListsToEditors();
    begin
   		with fraParams do
      	begin
        	smcTracingDelay.setFunctionDict( _fnDict );
          smcTracingDelay.setModelList( _ptList );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormTracing.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.tracingParams.traceDirectForward := src.tracingParams.traceDirectForward;
      dest.tracingParams.traceIndirectForward := src.tracingParams.traceIndirectForward;
      dest.tracingParams.traceDirectBack := src.tracingParams.traceDirectBack;
      dest.tracingParams.traceIndirectBack := src.tracingParams.traceIndirectBack;

      dest.tracingParams.directTracePeriod := src.tracingParams.directTracePeriod;
      dest.tracingParams.directTraceSuccess := src.tracingParams.directTraceSuccess;

      dest.tracingParams.indirectTracePeriod := src.tracingParams.indirectTracePeriod;
      dest.tracingParams.indirectTraceSuccess := src.tracingParams.indirectTraceSuccess;

      dest.setChart( TrDelay, src.chart( TrDelay ) );

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
  RegisterClass( TFormTracing );


end.
