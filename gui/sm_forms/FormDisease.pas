unit FormDisease;

(*
FormDisease.pas/dfm
-------------------
Begin: 2005/04/02
Last revision: $Date: 2011-10-04 23:58:12 $ $Author: areeves $
Version: $Revision: 1.40.2.1 $
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
    sbxAllParams: TScrollBox;
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

      fraParams.visible := false;
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
  	begin
      if( 0 = _ptList.count ) then
        begin
          //sbxAllParams.Visible := false;
          exit;
        end
      else
        //sbxAllParams.Visible := true
      ;

    	if( nil <> _selectedPT ) then
      	begin
          lblProdType.Caption := _selectedPT.productionTypeDescr;
          fraParams.Visible := true;
					fraParams.prodType := _selectedPT;
        end
      else
        begin
          lblProdType.Caption := '';
          fraParams.visible := false;
        end
      ;
    end
  ;


  procedure TFormDisease.prepFunctionDicts();
  	var
      it: TFunctionDictionaryIterator;
  	begin
    	dbcout( 'Updating master display.  Chart function editors should be cleared.', DBFORMDISEASE );

    	with fraParams do
      	begin
        	smcLatent.ClearList();
          smcSubclinical.ClearList();
          smcClinical.ClearList();
          smcImmune.ClearList();
          smrPrevInfected.clearList();
          smrPrevShedding.clearList();
        end
      ;

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case( TSMChart( integer ( it.value().fn.dbField ) ) ) of
                  DLatent: fraParams.smcLatent.appendFunction( it.value().fn );
                  DSubclinical: fraParams.smcSubclinical.appendFunction( it.value().fn );
                  DClinical: fraParams.smcClinical.appendFunction( it.value().fn );
                  DImmune: fraParams.smcImmune.appendFunction( it.value().fn );
                  DPrevInfected: fraParams.smrPrevInfected.appendFunction( it.value().fn );
                  DPrevShedding: fraParams.smrPrevShedding.appendFunction( it.value().fn );
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


  procedure TFormDisease.giveListsToEditors();
  	begin
   		with fraParams do
      	begin
       		smcLatent.setFunctionDict( _fnDict );
          smcSubclinical.setFunctionDict( _fnDict );
          smcClinical.setFunctionDict( _fnDict );
          smcImmune.setFunctionDict( _fnDict );
          smrPrevInfected.setFunctionDict( _fnDict );
          smrPrevShedding.setFunctionDict( _fnDict );

       		smcLatent.setModelList( _ptList );
          smcSubclinical.setModelList( _ptList );
          smcClinical.setModelList( _ptList );
          smcImmune.setModelList( _ptList );
          smrPrevInfected.setModelList( _ptList );
          smrPrevShedding.setModelList( _ptList );

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
     	  _selectedPT.diseaseParams.useDisease := fraParams.cbxTransition.Checked
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
      dest.diseaseParams.useDisease := src.diseaseParams.useDisease;

      dest.setChart( DLatent, src.chart( DLatent ) );
      dest.setChart( DSubclinical, src.chart( DSubclinical ) );
      dest.setChart( DClinical, src.chart( DClinical ) );
      dest.setChart( DImmune, src.chart( DImmune ) );
      dest.setChart( DPrevInfected, src.chart( DPrevInfected ) );
      dest.setChart( DPrevShedding, src.chart( DPrevShedding ) );

      dest.diseaseParams.probMortality := src.diseaseParams.probMortality;

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------

initialization
	RegisterClass( TFormDisease );


end.
