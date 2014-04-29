unit FormDisease;

(*
FormDisease.pas/dfm
-------------------
Begin: 2005/04/02
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.34 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard includes
    Forms,
    StdCtrls,
    Controls,
    Classes,
    ExtCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    // Data structures
    ProductionType,
    FunctionDictionary,

    // Widgets
    FormProdTypeBase,
    FrameDisease
  ;

  type TFormDisease = class( TFormProdTypeBase )
      fraParams: TFrameDisease;

    	procedure cbxTransitionClick(Sender: TObject);

    protected
      _ignoreClick: boolean;

      procedure translateUI();

  		procedure updateDisplay(); override;

  		procedure giveListsToEditors(); override;
      procedure prepFunctionDicts(); override;

      function dataIsValid(): boolean; override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType ); override;

    public
    	constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

    end
  ;

  const
  	DBFORMDISEASE: boolean = false; // set to true to enable debugging messages for this unit

implementation

{$R *.dfm}

  uses
    SysUtils,
    FormMain,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    ChartFunction,
    ControlUtils,
    FunctionEnums,
    QStringMaps,
    I88n,

    SMSimulationInput
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormDisease.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      fraParams.cbxTransition.visible := false;
      fraParams.pnlDiseaseParams.Visible := false;
      lblProdType.Caption := '';

      _ignoreClick := false;
    end
  ;


  procedure TFormDisease.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormDisease.dfm
      // File date: Thu Oct 12 14:33:57 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario Parameters: Disease' );
          pnlCaption.Caption := tr( 'Disease' );
        end
      ;

    end
  ;


  destructor TFormDisease.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  procedure TFormDisease.updateDisplay();
    var
      usePrevalence: boolean;
  	begin
    	if( _selectedPT <> nil ) then
      	begin
          usePrevalence := (_selectedPT.sim as TSMSimulationInput).useWithinHerdPrevalence;

          fraParams.lblPrevalence.Visible := usePrevalence;
          fraParams.imgPrevalence.Visible := usePrevalence;
          fraParams.smrPrevalence.Visible := usePrevalence;

          lblProdType.Caption := ( _selectedPT.productionTypeDescr );
          fraParams.pnlDiseaseParams.Visible := _selectedPT.simulateTransition;
          fraParams.cbxTransition.visible := true;

          // Setting the checkbox "checked" property triggers the checkbox "clicked" event, which is silly...
          _ignoreClick := true;
          fraParams.cbxTransition.Checked := _selectedPT.simulateTransition;
          _ignoreClick := false;

          if( _selectedPT.simulateTransition ) then
            begin
              fraParams.smcLatent.showChart( _selectedPT, _selectedPT.diseaseLatent, DLatent );
              fraParams.smcSubclinical.showChart( _selectedPT, _selectedPT.diseaseSubclinical, DSubclinical );
              fraParams.smcClinical.showChart( _selectedPT, _selectedPT.diseaseClinical, DClinical );
              fraParams.smcImmune.showChart( _selectedPT, _selectedPT.diseaseImmune, DImmune );
              fraParams.smrPrevalence.showChart( _selectedPT, _selectedPT.diseasePrevalence, DPrevalence );
            end
          ;
        end
      else
        begin
          fraParams.pnlDiseaseParams.visible := false;
      		fraParams.cbxTransition.visible := false;
          lblProdType.Caption := '';
        end
      ;
    end
  ;


  procedure TFormDisease.prepFunctionDicts();
  	var
    	pt: TProductionType;
      i: integer;
      it: TFunctionDictionaryIterator;
  	begin
    	dbcout( 'Updating master display.  Chart function editors should be cleared.', DBFORMDISEASE );

    	with fraParams do
      	begin
        	smcLatent.ClearList();
          smcSubclinical.ClearList();
          smcClinical.ClearList();
          smcImmune.ClearList();
          smrPrevalence.clearList();
        end
      ;

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case ( it.value().fn.dbField ) of
                  integer( DLatent ):
                    begin
                       it.value().refCounter := 0;
                       fraParams.smcLatent.appendFunction( it.value().fn );
                    end;

                  integer( DSubclinical ):
                    begin
                      it.value().RefCounter := 0;
                      fraParams.smcSubclinical.appendFunction( it.value().fn );
                    end;

                  integer( DClinical ):
                    begin
                      it.value().RefCounter := 0;
                      fraParams.smcClinical.appendFunction( it.value().fn );
                    end;

                  integer( DImmune ):
                    begin
                      it.value().RefCounter := 0;
                      fraParams.smcImmune.appendFunction( it.value().fn );
                    end;

                  integer( DPrevalence ):
                    begin
                      it.value().refCounter := 0;
                      fraParams.smrPrevalence.appendFunction( it.value().fn );
                    end;
                end;
              end;
          end
        ;

        it.incr();
      until ( nil = it.value() );

      it.Free();


      // Set the reference counters, based on how many times a function is actually used.
      for i := 0 to _ptList.Count-1 do
      	begin
        	pt := _ptList.at(i);
          if( nil <> pt.diseaseLatent ) then _fnDict.value( pt.latentName ).incrRefCounter();
          if( nil <> pt.diseaseSubclinical ) then _fnDict.value( pt.subclinicalName).incrRefCounter();
          if( nil <> pt.diseaseClinical ) then _fnDict.value( pt.clinicalName ).incrRefCounter();
          if( nil <> pt.diseaseImmune ) then _fnDict.value( pt.immuneName ).incrRefCounter();
          if( nil <> pt.diseasePrevalence ) then _fnDict.value( pt.prevalenceName ).incrRefCounter();
        end
      ;
    end
  ;


  procedure TFormDisease.giveListsToEditors();
  	begin
   		with fraParams do
      	begin
       		smcLatent.setFunctionDict( _fnDict );
          smcSubclinical.setFunctionDict( _fnDict );
          smcClinical.setFunctionDict( _fnDict );
          smcImmune.setFunctionDict( _fnDict );
          smrPrevalence.setFunctionDict( _fnDict );

       		smcLatent.setModelList( _ptList );
          smcSubclinical.setModelList( _ptList );
          smcClinical.setModelList( _ptList );
          smcImmune.setModelList( _ptList );
          smrPrevalence.setModelList( _ptList );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling functions
//-----------------------------------------------------------------------------
  procedure TFormDisease.cbxTransitionClick(Sender: TObject);
    begin
      if( not( _ignoreClick ) ) then
     	  _selectedPT.simulateTransition := fraParams.cbxTransition.Checked
      ;
     	updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormDisease.dataIsValid(): boolean;
		begin
   		result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormDisease.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.simulateTransition := src.simulateTransition;

      dest.setChart( DLatent, src.chart( DLatent ) );
      dest.setChart( DSubclinical, src.chart( DSubclinical ) );
      dest.setChart( DClinical, src.chart( DClinical ) );
      dest.setChart( DImmune, src.chart( DImmune ) );
      dest.setChart( DPrevalence, src.chart( DPrevalence ) );

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------

initialization
	RegisterClass( TFormDisease );


end.
