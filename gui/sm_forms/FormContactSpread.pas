unit FormContactSpread;

(*
FormContactSpread.pas/dfm
-------------------------
Begin: 2005/06/17
Last revision: $Date: 2013-06-27 19:11:24 $ $Author: areeves $
Version number: $Revision: 1.40.4.2 $
Project: (various)
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2010 Colorado State University

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
    ExtCtrls,
    StdCtrls,
    Buttons,

    // Base class
    FormSMWizardBase,
    FormProdTypePairBase,

    // Data structures
    ProductionTypePair,
    ProductionTypePairList,
    FunctionDictionary,

    // Additional controls/widgets
    FrameContactSpreadMaster, Menus, ActnPopupCtrl
  ;

  type TFormContactSpread = class( TFormProdTypePairBase )
      sbxAllParams: TScrollBox;
      fraParams: TFrameContactSpreadMaster;

      procedure fraParamscbxIncludeDirectClick( Sender: TObject );
      procedure fraParamscbxIncludeIndirectClick( Sender: TObject );
      procedure cbxFixedContactRateClick( Sender: TObject );

      procedure fraDirectcbxFixedContactRateClick( Sender: TObject );
      procedure fraIndirectcbxFixedContactRateClick( Sender: TObject );

    protected
      _frameHeight: integer;

      _showFixedRateWarning: boolean;

      procedure translateUI();      

      procedure updateDisplay(); override;

      procedure prepFunctionDicts(); override;
      procedure giveListsToEditors(); override;

      function dataIsValid(): boolean; override;

      procedure copyParameters( const src: TProductionTypePair; dest: TProductionTypePair ); override;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

    end
   ;

   const
    DBFORMCONTACTSPREAD: boolean = false; // set to true to enable debugging messages for this unit.

implementation

{$R *.dfm}

  uses
    // Utility functions
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    I88n,

    // Data structures
    ContactSpreadParams,
    ChartFunction,
    QStringMaps,
    FunctionEnums
  ;

  constructor TFormContactSpread.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _showFixedRateWarning := true;

      _frameHeight := round( fraParams.pnlDirectParams.Height * screen.PixelsPerInch / 96 );

      fraParams.fraDirect.setForm( self );
      fraParams.fraIndirect.setForm( self );

      fraParams.fraDirect.smcDistanceDistr.setForm( self );
      fraParams.fraDirect.smcTransportDelay.setForm( self );
      fraParams.fraDirect.smrMovementControl.setForm( self );

      fraParams.fraDirect.smrMovementControl.minY := 0.0;
      fraParams.fraDirect.smrMovementControl.maxY := 100.0;
      fraParams.fraDirect.smrMovementControl.xUnits := UDays;
      fraParams.fraDirect.smrMovementControl.yUnits := UPercent;

      fraParams.fraIndirect.smcDistanceDistr.setForm( self );
      fraParams.fraIndirect.smcTransportDelay.setForm( self );
      fraParams.fraIndirect.smrMovementControl.setForm( self );

      fraParams.fraIndirect.smrMovementControl.minY := 0.0;
      fraParams.fraIndirect.smrMovementControl.maxY := 100.0;
      fraParams.fraIndirect.smrMovementControl.xUnits := UDays;
      fraParams.fraIndirect.smrMovementControl.yUnits := UPercent;

      //fraParams.fraDirect.cbxFixedContactRate.OnClick := cbxFixedContactRateClick;
      //fraParams.fraIndirect.cbxFixedContactRate.OnClick := cbxFixedContactRateClick;
    end
  ;


  procedure TFormContactSpread.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:28:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormContactSpread.dfm
      // File date: Thu Feb 8 17:00:22 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Contact spread' );
          pnlCaption.Caption := tr( 'Contact spread' );
        end
      ;

    end
  ;


  destructor TFormContactSpread.destroy();
    begin
      inherited destroy();
    end
  ;


  // As a general rule, users are allowed to store an incomplete scenario.
  // Consequently, most wizard froms don't need to do any real validation.
  // Somewhere, the interface needs to have another check on this, though.
  function TFormContactSpread.dataIsValid(): boolean;
    begin
      result := fraParams.isValid();
    end
  ;


  procedure TFormContactSpread.prepFunctionDicts();
    var
      it: TFunctionDictionaryIterator;
    begin
      fraParams.fraDirect.smcDistanceDistr.clearList();
      fraParams.fraDirect.smcTransportDelay.clearList();
      fraParams.fraDirect.smrMovementControl.clearList();

      fraParams.fraIndirect.smcDistanceDistr.clearList();
      fraParams.fraIndirect.smcTransportDelay.clearList();
      fraParams.fraIndirect.smrMovementControl.clearList();

      it := TFunctionDictionaryIterator.create( _fnList );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                case it.value().fn.dbField of
                  integer( CMDistanceDirect ):
                  if ( nil <> it.value().fn ) then
                    begin
                      fraParams.fraDirect.smcDistanceDistr.appendFunction( it.value().fn );
                      fraParams.fraIndirect.smcDistanceDistr.appendFunction( it.value().fn );
                    end
                  ;

                  integer( CMDistanceIndirect ):
                  if ( nil <> it.value().fn ) then
                    begin
                      fraParams.fraDirect.smcDistanceDistr.appendFunction( it.value().fn );
                      fraParams.fraIndirect.smcDistanceDistr.appendFunction( it.value().fn );
                    end
                  ;

                  integer( CMDelayDirect ):
                  if ( nil <> it.value().fn ) then
                    begin
                      fraParams.fraDirect.smcTransportDelay.appendFunction( it.value().fn );
                      fraParams.fraIndirect.smcTransportDelay.appendFunction( it.value().fn );
                    end
                  ;

                  integer( CMDelayIndirect ):
                  if ( nil <> it.value().fn ) then
                    begin
                      fraParams.fraDirect.smcTransportDelay.appendFunction( it.value().fn );
                      fraParams.fraIndirect.smcTransportDelay.appendFunction( it.value().fn );
                    end
                  ;

                  integer( CMMovementControlDirect ):
                  if ( nil <> it.value().fn ) then
                    begin
                      fraParams.fraDirect.smrMovementControl.appendFunction( it.value().fn );
                      fraParams.fraIndirect.smrMovementControl.appendFunction( it.value().fn );
                    end
                  ;

                  integer( CMMovementControlIndirect ):
                  if ( nil <> it.value().fn ) then
                    begin
                      fraParams.fraDirect.smrMovementControl.appendFunction( it.value().fn );
                      fraParams.fraIndirect.smrMovementControl.appendFunction( it.value().fn );
                    end
                  ;
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
  

  procedure TFormContactSpread.giveListsToEditors();
    begin
      // Each of the function editors needs to have access to the lists,
      // in order to know which have been updated.

      // _ptpList is used in editDone and removeChart.

      // _fnList is used in editDone, removeChart, and setChart.
      // pdfList is used in chartParamsBtnEditClick, and chartParamsBtnDoneClick (SMPdfEditor)

      // Remember that _fnList contains both PDFs and relational functions.
      // This seems to be an adequate solution...

      fraParams.fraDirect.smcDistanceDistr.setFunctionDict( _fnList );
      fraParams.fraDirect.smcTransportDelay.setFunctionDict( _fnList );
      fraParams.fraDirect.smrMovementControl.setFunctionDict( _fnList );

      fraParams.fraIndirect.smcDistanceDistr.setFunctionDict( _fnList );
      fraParams.fraIndirect.smcTransportDelay.setFunctionDict( _fnList );
      fraParams.fraIndirect.smrMovementControl.setFunctionDict( _fnList );

      fraParams.fraDirect.smcDistanceDistr.setModelList( _ptpList );
      fraParams.fraDirect.smcTransportDelay.setModelList( _ptpList );
      fraParams.fraDirect.smrMovementControl.setModelList( _ptpList );

      fraParams.fraIndirect.smcDistanceDistr.setModelList( _ptpList );
      fraParams.fraIndirect.smcTransportDelay.setModelList( _ptpList );
      fraParams.fraIndirect.smrMovementControl.setModelList( _ptpList );
    end
  ;


  function TFormContactSpread.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    begin
      if( _smScenarioCopy.simInput.includeContactSpreadGlobal ) then
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


  procedure TFormContactSpread.updateDisplay();
    begin
      if( 0 = _ptpList.count ) then
        begin
          sbxAllParams.Visible := false;
          exit;
        end
      else
        sbxAllParams.Visible := true
      ;

      if( nil <> _selectedPTP ) then
        begin
          _loadingForm := true;

          fraParams.Visible := true;

          fraParams.pnlIndirectContact.Align := alTop;
          (*
          // If both sets of parameters are visible,
          // make sure that the vertical scrollbar is visible.
          //--------------------------------------------------
          if
            ( fraParams.cbxIncludeDirect.checked )
          and
            ( fraParams.cbxIncludeIndirect.checked )
          then
            fraParams.pnlIndirectContact.Align := alTop
          else
            fraParams.pnlIndirectContact.Align := alTop //alClient
          ;
          *)
          
          // Set up the direct contact panel
          //--------------------------------
          fraParams.cbxIncludeDirect.checked := _selectedPTP.includeDirect;

          if( _selectedPTP.includeDirect ) then
            begin
              fraParams.pnlDirectParams.Visible := true;
              fraparams.pnlDirectContact.Height := _frameHeight + fraParams.pnlUseDirectContact.Height + 2;

              if( nil = _selectedPTP.direct ) then
                _selectedPTP.direct := TContactSpreadParams.create( CMDirect, _selectedPTP.sim, _selectedPTP.dest.productionTypeID, _selectedPTP.source.productionTypeID )
              ;

              fraParams.fraDirect.setContactSpreadParams( _selectedPTP.direct, _selectedPTP )
            end
          else
            begin
              fraParams.pnlDirectParams.Visible := false;
              fraparams.pnlDirectContact.Height := fraParams.pnlUseDirectContact.Height + 2;
            end
          ;


          // Set up the indirect contact panel
          //----------------------------------
          fraParams.cbxIncludeIndirect.checked := _selectedPTP.includeIndirect;

          if( _selectedPTP.includeIndirect ) then
            begin
              fraParams.pnlIndirectParams.visible := true;
              fraParams.pnlIndirectContact.Height := _frameHeight + fraParams.pnlUseIndirectContact.Height + 2;

              if( nil = _selectedPTP.indirect ) then
                _selectedPTP.indirect := TContactSpreadParams.create( CMIndirect, _selectedPTP.sim, _selectedPTP.dest.productionTypeID, _selectedPTP.source.productionTypeID )
              ;
              
              fraParams.fraIndirect.setContactSpreadParams( _selectedPTP.indirect, _selectedPTP );
            end
          else
            begin
              fraParams.pnlIndirectParams.visible := false;
              fraParams.pnlIndirectContact.Height := fraParams.pnlUseIndirectContact.Height + 2;
            end
          ;

          fraParams.pnlIndirectContact.Align := alTop;
          (*
          // If both sets of parameters are visible,
          // make sure that the vertical scrollbar is visible.
          //--------------------------------------------------
          if
            ( fraParams.cbxIncludeDirect.checked )
          and
            ( fraParams.cbxIncludeIndirect.checked )
          then
            fraParams.pnlIndirectContact.Align := alTop
          else
            fraParams.pnlIndirectContact.Align := alTop //alClient
          ;
          *)

          hideStar();
          _loadingForm := false;
        end
      else
        fraParams.Visible := false;
      ;
    end
  ;


  procedure TFormContactSpread.fraParamscbxIncludeDirectClick( Sender: TObject );
    begin
      inherited;

      if( nil <> _selectedPTP ) then
        begin
          _selectedPTP.includeDirect := fraParams.cbxIncludeDirect.Checked;
          updateDisplay();
          if ( not _loadingForm ) then
            showStar();
        end
      ;
    end
  ;


  procedure TFormContactSpread.fraParamscbxIncludeIndirectClick( Sender: TObject );
    begin
      inherited;

      if( nil <> _selectedPTP ) then
        begin
          _selectedPTP.includeIndirect := fraParams.cbxIncludeIndirect.checked;
          updateDisplay();
          if ( not _loadingForm ) then
            showStar();
        end
      ;
    end
  ;




  procedure TFormContactSpread.cbxFixedContactRateClick( Sender: TObject );
    var
      tmpBool: boolean;
    begin
      if( (sender as TCheckBox).Checked ) then
        begin
          msgOKCheckbox(
            tr( 'Fixed contact rates are primarily used for testing purposes.  For most analyses, please specify a mean contact rate.' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'Fixed contact rates' ),
            IMGInformation,
            self
          );
        end
      ;

      _showTestFeatureWarning := not( tmpBool );

      inherited;
    end
  ;


  procedure TFormContactSpread.fraDirectcbxFixedContactRateClick( Sender: TObject );
    var
      tmpBool: boolean;
    begin
      fraParams.fraDirect.cbxFixedContactRateClick( sender );

      if( (sender as TCheckBox).Checked and _showTestFeatureWarning and not( _loadingForm ) ) then
        begin
          msgOKCheckbox(
            tr( 'Fixed contact rates are primarily used for testing purposes.  For most analyses, please specify a mean contact rate.' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'Fixed contact rates' ),
            IMGInformation,
            self
          );

          _showTestFeatureWarning := not( tmpBool );
        end
      ;
    end
  ;


  procedure TFormContactSpread.fraIndirectcbxFixedContactRateClick( Sender: TObject );
    var
      tmpBool: boolean;
    begin
      fraParams.fraIndirect.cbxFixedContactRateClick( sender );

      if( (sender as TCheckBox).Checked and _showTestFeatureWarning and not( _loadingForm ) ) then
        begin
          msgOKCheckbox(
            tr( 'Fixed contact rates are primarily used for testing purposes.  For most analyses, please specify a mean contact rate.' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'Fixed contact rates' ),
            IMGInformation,
            self
          );

          _showTestFeatureWarning := not( tmpBool );
        end
      ;
    end
  ;


//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormContactSpread.copyParameters( const src: TProductionTypePair; dest: TProductionTypePair );
    begin
      // Direct contact
      //---------------
      dest.includeDirect := src.includeDirect;

      dest.direct.useFixedContactRate := src.direct.useFixedContactRate;
      dest.direct.fixedContactRate := src.direct.fixedContactRate;
      dest.direct.meanContactRate := src.direct.meanContactRate;
      dest.direct.probInfect := src.direct.probInfect;

      dest.direct.latentCanInfect := src.direct.latentCanInfect;
      dest.direct.subClinicalCanInfect := src.direct.subClinicalCanInfect;

      dest.setChart( CMDistanceDirect, src.chart( CMDistanceDirect ) );
      dest.setChart( CMDelayDirect, src.chart( CMDelayDirect ) );
      dest.setChart( CMMovementControlDirect, src.chart( CMMovementControlDirect ) );

      // Indirect contact
      //-----------------
      dest.includeIndirect := src.includeIndirect;

      dest.indirect.fixedContactRate := src.indirect.fixedContactRate;
      dest.indirect.useFixedContactRate := src.indirect.useFixedContactRate;
      dest.indirect.meanContactRate := src.indirect.meanContactRate;
      dest.indirect.probInfect := src.indirect.probInfect;

      // Remember that latent units cannot spread disease by indirect contact
      dest.indirect.latentCanInfect := false;
      dest.indirect.subClinicalCanInfect := src.indirect.subClinicalCanInfect;

      dest.setChart( CMDistanceIndirect, src.chart( CMDistanceIndirect ) );
      dest.setChart( CMDelayIndirect, src.chart( CMDelayIndirect ) );
      dest.setChart( CMMovementControlIndirect, src.chart( CMMovementControlIndirect ) );

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization
  RegisterClass( TFormContactSpread );

end.
