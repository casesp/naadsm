unit FormTracingTesting;

(*
FormTracingTesting.pas
----------------------
Begin: 2008/04/22
Last revision: $Date: 2010-09-09 14:29:37 $ $Author: rhupalo $
Version number: $Revision: 1.5.4.1 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard includes
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    Menus,
    ActnPopupCtrl,
    StdCtrls,
    Buttons,
    ExtCtrls,

    // Widgets
    FormProdTypeBase,
    FrameTracingTesting,

    // Data structures
    ProductionType,
    FunctionDictionary
  ;

  type TFormTracingTesting = class( TFormProdTypeBase )
      fraParams: TFrameTracingTesting;

   protected
      procedure translateUI();
      procedure translateUIManual();

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

  const
  	DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit.


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormTracingTesting.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
    end
  ;


  procedure TFormTracingTesting.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FormTracingTesting.dfm
      // File date: Mon Apr 28 09:41:18 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Diagnostic testing' );
          pnlCaption.Caption := tr( 'Diagnostic testing' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormTracingTesting.translateUIManual();
    begin
    end
  ;


  destructor TFormTracingTesting.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormTracingTesting.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      // need detection in order to conduct tracing
    	if(( _smScenarioCopy.simInput.includeTracingTestingGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
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


  procedure TFormTracingTesting.updateDisplay();
  	begin
    	if( nil <> _selectedPT ) then
      	begin
          fraParams.Visible := true;
          fraParams.prodType := _selectedPT;
          lblProdType.Caption := _selectedPT.productionTypeDescr;

          // show chart, even if no chart is used.  This sets the production type for the chart editor
          fraParams.smcTestingDelay.showChart(
            _selectedPT,
            _selectedPT.testingParams.pdfTestDelay,
            TeDelay
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
  function TFormTracingTesting.dataIsValid(): boolean;
		begin
      result:= fraParams.isValid();
    end
  ;


  procedure TFormTracingTesting.prepFunctionDicts();
  	var
      it: TFunctionDictionaryIterator;
  	begin
			fraParams.smcTestingDelay.cboChartList.clear();

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case( it.value().fn.dbField ) of
                  integer( TeDelay ): fraParams.smcTestingDelay.appendFunction( it.value().fn );
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

  
  procedure TFormTracingTesting.giveListsToEditors();
    begin
   		with fraParams do
      	begin
        	smcTestingDelay.setFunctionDict( _fnDict );
          smcTestingDelay.setModelList( _ptList );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormTracingTesting.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.testingParams.specificity := src.testingParams.specificity;
      dest.testingParams.sensitivity := src.testingParams.sensitivity;

      dest.testingParams.testDirectForward := src.testingParams.testDirectForward;
      dest.testingParams.testIndirectForward := src.testingParams.testIndirectForward;
      dest.testingParams.testDirectBack := src.testingParams.testDirectBack;
      dest.testingParams.testIndirectBack := src.testingParams.testIndirectBack;

      dest.setChart( TeDelay, src.chart( TeDelay ) );

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
  RegisterClass( TFormTracingTesting );

end.
