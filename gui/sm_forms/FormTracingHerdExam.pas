unit FormTracingHerdExam;

(*
FormTracingHerdExam.pas
-----------------------
Begin: 2008/04/22
Last revision: $Date: 2010-09-09 14:29:37 $ $Author: rhupalo $
Version number: $Revision: 1.3.6.1 $
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

    ProductionType,

    FormProdTypeBase,
    FrameTracingHerdExam
  ;

  type TFormTracingHerdExam = class( TFormProdTypeBase )
      fraParams: TFrameTracingHerdExam;

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
    FormMain,
    MyStrUtils,
    ChartFunction,
    ControlUtils,
    FunctionEnums,
    I88n
  ;

  const
  	DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit.


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormTracingHerdExam.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
    end
  ;


  procedure TFormTracingHerdExam.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FormTracingHerdExam.dfm
      // File date: Mon Apr 28 14:15:53 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Unit examination for clinical signs' );
          pnlCaption.Caption := tr( 'Unit examination for clinical signs' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormTracingHerdExam.translateUIManual();
    begin
    end
  ;


  destructor TFormTracingHerdExam.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormTracingHerdExam.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
      //need detection in order to conduct tracing
    	if (( _smScenarioCopy.simInput.includeTracingHerdExamGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
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


  procedure TFormTracingHerdExam.updateDisplay();
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
  function TFormTracingHerdExam.dataIsValid(): boolean;
		begin
      result:= fraParams.isValid();
    end
  ;


  procedure TFormTracingHerdExam.prepFunctionDicts();
    begin
      // Nothing happens here in this class
    end
  ;

  
  procedure TFormTracingHerdExam.giveListsToEditors();
    begin
      // Nothing happens here in this class
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormTracingHerdExam.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.tracingParams.examDirectForward := src.tracingParams.examDirectForward;
      dest.tracingParams.examDirectForwardMultiplier := src.tracingParams.examDirectForwardMultiplier;
      dest.tracingParams.examIndirectForward := src.tracingParams.examIndirectForward;
      dest.tracingParams.examIndirectForwardMultiplier := src.tracingParams.examIndirectForwardMultiplier;

      dest.tracingParams.examDirectBack := src.tracingParams.examDirectBack;
      dest.tracingParams.examDirectBackMultiplier := src.tracingParams.examDirectBackMultiplier;
      dest.tracingParams.examIndirectBack := src.tracingParams.examIndirectBack;
      dest.tracingParams.examIndirectBackMultiplier := src.tracingParams.examIndirectBackMultiplier;

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
	RegisterClass( TFormTracingHerdExam );

end.
