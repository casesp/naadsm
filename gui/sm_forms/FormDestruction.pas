unit FormDestruction;

(*
FormDestruction.pas/dfm
------------------------
Begin: 2005/06/08
Last revision: $Date: 2011-05-24 16:02:04 $ $Author: rhupalo $
Version: $Revision: 1.28.12.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Animal Population Health Institute, Colorado State University

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
    FrameDestruction,

    // Data structures
    ProductionType,
    FunctionDictionary
  ;

  type TFormDestruction = class( TFormProdTypeBase )
      fraParams: TFrameDestruction;

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
  	DBFORMDESTRUCTION: boolean = false; // Set to true to enable debugging messages for this unit.


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

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormDestruction.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
    end
  ;


  procedure TFormDestruction.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDestruction.dfm
      // File date: Fri Jan 19 17:43:53 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Destruction' );
          pnlCaption.Caption := tr( 'Destruction' );
        end
      ;

    end
  ;


  destructor TFormDestruction.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormDestruction.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	// need detection of disease in order to conduct destruction campaign
    	if (( _smScenarioCopy.simInput.includeDestructionGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
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


  procedure TFormDestruction.updateDisplay();
  	begin
    	if( nil <> _selectedPT ) then
      	begin
          fraParams.Visible := true;
          fraParams.tracingEnabled := _smScenarioCopy.simInput.includeTracingGlobal or _smScenarioCopy.simInput.controlParams.useTracingGlobal ;

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
  function TFormDestruction.dataIsValid(): boolean;
		begin
      result:= fraParams.isValid();
    end
  ;


  procedure TFormDestruction.prepFunctionDicts();
    begin
      // Nothing happens here in this class
    end
  ;

  
  procedure TFormDestruction.giveListsToEditors();
    begin
      // Nothing happens here in this class
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormDestruction.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.destructionParams.isRingTarget := src.destructionParams.isRingTarget;
      dest.destructionParams.isRingTrigger := src.destructionParams.isRingTrigger;
      dest.destructionParams.ringRadius := src.destructionParams.ringRadius;
      dest.destructionParams.destroyDetectedUnits := src.destructionParams.destroyDetectedUnits;
      dest.destructionParams.destroyDirectForwardTraces := src.destructionParams.destroyDirectForwardTraces;
      dest.destructionParams.destroyIndirectForwardTraces := src.destructionParams.destroyIndirectForwardTraces;
	  dest.destructionParams.destroyDirectBackTraces := src.destructionParams.destroyDirectBackTraces;
      dest.destructionParams.destroyIndirectBackTraces := src.destructionParams.destroyIndirectBackTraces;

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------

initialization
	RegisterClass( TFormDestruction );


end.
