unit FormCostsDestr;

(*
FormCostsDestr.pas/dfm
----------------------
Begin: 2007/04/17
Last revision: $Date: 2008/11/25 22:00:29 $ $Author: areeves $
Version number: $Revision: 1.6 $
Project: (various)
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

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
    StdCtrls,
    Buttons,
    ExtCtrls,
    Dialogs,
    Menus,
    ActnPopupCtrl,
    
    ProductionType,
    ProductionTypeList,

    // Widgets
    FrameCostsDestr,
    FormProdTypeBase,
    FormSMWizardBase
  ;


  type TFormCostsDestr = class( TFormProdTypeBase )
      fraParams: TFrameCostsDestr;

    protected
      procedure translateUI();
    
  		procedure updateDisplay(); override;

      function dataIsValid(): boolean; override;

      // These two functions are abstract in the base class and must be
      // overridden, but since there are no PDFs/REL functions on this form,
      // they are empty.
  		procedure giveListsToEditors(); override;
      procedure prepFunctionDicts(); override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType );  override;
      procedure showApplyToAllMessage(); override;

    public
    	constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

    end
  ;


  const
  	DBFORMCOSTS: boolean = false; // set to true to enable debugging messages for this unit.


implementation

{$R *.dfm}

  uses
    MyDialogs,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormCostsDestr.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
    end
  ;


  procedure TFormCostsDestr.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormCostsDestr.dfm
      // File date: Wed Apr 25 11:56:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Costs of destruction' );
          pnlCaption.Caption := tr( 'Costs of destruction' );
        end
      ;

    end
  ;
    

  destructor TFormCostsDestr.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormCostsDestr.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.costTrackDestruction ) then
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


  procedure TFormCostsDestr.updateDisplay();
  	begin
    	if( nil <> _selectedPT ) then
      	begin
          lblProdType.Caption := _selectedPT.productionTypeDescr;
          fraParams.Visible := true;
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


  procedure TFormCostsDestr.giveListsToEditors();
    begin
      // Do nothing
    end
  ;


  procedure TFormCostsDestr.prepFunctionDicts();
    begin
      // Do nothing
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormCostsDestr.dataIsValid(): boolean;
		begin
   		result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormCostsDestr.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.costParams.destrAppraisalPerUnit := src.costParams.destrAppraisalPerUnit;
      dest.costParams.destrCleaningPerUnit := src.costParams.destrCleaningPerUnit;
      dest.costParams.destrEuthanasiaPerAnimal := src.costParams.destrEuthanasiaPerAnimal;
      dest.costParams.destrIndemnificationPerAnimal := src.costParams.destrIndemnificationPerAnimal;
      dest.costParams.destrDisposalPerAnimal := src.costParams.destrDisposalPerAnimal;

      dest.updated := true;
    end
  ;


  procedure TFormCostsDestr.showApplyToAllMessage();
    var
      allUseDestruction: boolean;
      it: TProductionTypeListIterator;
      pt: TProductionType;
    begin
      allUseDestruction := true;

      it := TProductionTypeListIterator.create( _ptList );

      while( nil <> it.current() ) do
        begin
          pt := it.current();
          if( not( pt.isDestrTarget ) ) then
            begin
              allUseDestruction := false;
              break;
            end
          ;

          it.incr();
        end
      ;

      it.Free();

      if( not( allUseDestruction ) ) then
        begin
          msgOK(
            tr( 'The current destruction cost parameters will be applied to all production types, but will have no effect on production types for which destruction is not used.' ),
            tr( 'Destruction is not used with all production types' ),
            IMGInformation,
            self
          );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------

initialization

  RegisterClass( TFormCostsDestr );

end.
