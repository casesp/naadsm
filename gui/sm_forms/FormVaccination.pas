unit FormVaccination;

(*
FormVaccination.pas/dfm
-----------------------
Begin: 2005/06/08
Last revision: $Date: 2013-06-27 19:11:29 $ $Author: areeves $
Version: $Revision: 1.34.4.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2010 Colorado State University

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
    Buttons,
    Classes,
    ExtCtrls,
    Menus,
    ActnPopupCtrl,

    // Widgets
    FormProdTypeBase,
    FrameVaccination,
    FormSMWizardBase,

    // Data structures
    ProductionType,
    FunctionDictionary
  ;

  type TFormVaccination = class( TFormProdTypeBase )
      fraParams: TFrameVaccination;

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
    DBFORMVACCINATION: boolean = false; // set to true to enable debugging messages for this unit.


implementation

{$R *.dfm}

  uses
    SysUtils,
    FormMain,
    MyStrUtils,
    ChartFunction,
    ControlUtils,
    FunctionEnums,
    QStringMaps,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormVaccination.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
    end
  ;


  procedure TFormVaccination.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormVaccination.dfm
      // File date: Wed May 30 11:08:13 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Vaccination' );
          pnlCaption.Caption := tr( 'Vaccination' );
        end
      ;

    end
  ;
  

  destructor TFormVaccination.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormVaccination.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    begin
      // need detection of disease in order to conduct vaccination campaign
      if (( _smScenarioCopy.simInput.includeVaccinationGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
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


  procedure TFormVaccination.updateDisplay();
    begin
      if( nil <> _selectedPT ) then
        begin
          lblProdType.Caption := _selectedPT.productionTypeDescr;
          fraParams.Visible := true;

          // show chart, even if no chart is used.  This sets the production type for the chart editor
          fraParams.smcVaccImmunePeriod.showChart(
            _selectedPT,
            _selectedPT.vaccinationParams.pdfVaccImmune,
            VacImmunePeriod
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


  procedure TFormVaccination.prepFunctionDicts();
    var
      it: TFunctionDictionaryIterator;
    begin
      fraParams.smcVaccImmunePeriod.cboChartList.clear();

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case( it.value().fn.dbField ) of
                  integer( VacImmunePeriod ): fraParams.smcVaccImmunePeriod.appendFunction( it.value().fn );
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


  procedure TFormVaccination.giveListsToEditors();
    begin
      with fraParams do
        begin
          smcVaccImmunePeriod.setFunctionDict( _fnDict );
          smcVaccImmunePeriod.setModelList( _ptList );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormVaccination.dataIsValid(): boolean;
    begin
      result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormVaccination.copyParameters( const src: TProductionType; dest: TProductionType );
    begin
      dest.vaccinationParams.useVaccination := src.vaccinationParams.useVaccination;
      dest.vaccinationParams.daysToImmunity := src.vaccinationParams.daysToImmunity;
      dest.setChart( VacImmunePeriod, src.chart( VacImmunePeriod ) );

      dest.ringVaccParams.minTimeBetweenVacc := src.ringVaccParams.minTimeBetweenVacc;
      dest.ringVaccParams.ringRadius := src.ringVaccParams.ringRadius;
      dest.ringVaccParams.useRing := src.ringVaccParams.useRing;
      dest.ringVaccParams.vaccinateDetected := src.ringVaccParams.vaccinateDetected;

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
  RegisterClass( TFormVaccination );


end.
