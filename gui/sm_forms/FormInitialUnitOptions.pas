unit FormInitialUnitOptions;

(*
FormInitialUnitOptions.pas/dfm
------------------------------
Begin: 2013/08/02
Last revision: 
Version: 
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2013 NAADSM Development Team

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
    ExtCtrls,
    StdCtrls,
    DBCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,
    REEdit,

    FormSMWizardBase,
    SMDatabase
  ;

  type TFormInitialUnitOptions = class( TFormSMWizardBase )
      pnlCaption: TPanel;
      pnlBody: TPanel;

      rdoAssignedUnits: TRadioButton;
      rdoRandomizedUnits: TRadioButton;
      gbxProdTypes: TGroupBox;
      rdoAnyProdType: TRadioButton;
      rdoSelectedProdTypes: TRadioButton;
      lstProdTypes: TListBox;
      gbxDzStates: TGroupBox;
      lblNInStates: TLabel;
      lblLatent: TLabel;
      lblSubclinical: TLabel;
      lblClinical: TLabel;
      lblNatImmune: TLabel;
      lblVacImmune: TLabel;
      lblNote: TLabel;
      gbxGeoRange: TGroupBox;
      lblNorthwest: TLabel;
      lblLatNW: TLabel;
      lblLonNW: TLabel;
      lblSoutheast: TLabel;
      lblLatSE: TLabel;
      lblLonSE: TLabel;
      rleLatNW: TREEdit;
      rleLonNW: TREEdit;
      rleLatSE: TREEdit;
      rleLonSE: TREEdit;
      rdoNoRange: TRadioButton;
      rdoRange: TRadioButton;
      rleLatent: TREEdit;
      rleSubclinical: TREEdit;
      rleClinical: TREEdit;
      rleNatImmune: TREEdit;
      rleVacImmune: TREEdit;

      procedure rdoAssignedUnitsClick(Sender: TObject);
    procedure lstProdTypesChanged(Sender: TObject);

    protected
      _valueChanged: boolean;
      _originalVal: boolean;
      _settingUp: boolean;

      procedure translateUI();

      procedure showControls();
      procedure setSelectedPTS();

      // For data validation
      function isValidLat( const val: double; rle: TREEdit = nil ): boolean;
      function isValidLon( const val: double; rle: TREEdit = nil ): boolean;

      // Parameter and database handling
      //---------------------------------
      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

    public
      constructor create( AOwner: TComponent ); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    Math,
    StrUtils,

    DebugWindow,
    MyDialogs,
    MyStrUtils,
    ControlUtils,
    I88n,
    RegExpDefs,
    BasicGIS,

    HerdRandomizationOptions,
    ProductionType,
    ProductionTypeList
  ;


  const DBSHOWMSG = true;

//------------------------------------------------------------------------------
// Construction, initialization, destruction
//------------------------------------------------------------------------------
	constructor TFormInitialUnitOptions.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();

      rleLatent.InputExpression := RE_INTEGER_INPUT;
      rleSubclinical.InputExpression := RE_INTEGER_INPUT;
      rleClinical.InputExpression := RE_INTEGER_INPUT;
      rleNatImmune.InputExpression := RE_INTEGER_INPUT;
      rleVacImmune.InputExpression := RE_INTEGER_INPUT;

      rleLatNW.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLonNW.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLatSE.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLonSE.InputExpression := RE_SIGNED_DECIMAL_INPUT;
    end
  ;


  procedure TFormInitialUnitOptions.translateUI();
    begin
      dbcout2( 'FIX ME: Write function TFormInitialUnitOptions.translateUI()' );
      
      with self do
        begin
          Caption := tr( 'Scenario parameters: Initial unit options' );
          pnlCaption.Caption := tr( 'Initial unit options' );
        end
      ;

    end
  ;
//------------------------------------------------------------------------------



//------------------------------------------------------------------------------
// Parameter and database handling
//------------------------------------------------------------------------------
  procedure TFormInitialUnitOptions.initializeFromSim();
    var
      params: THerdRandomizationOptions;
      it: TProductionTypeListIterator;
      pt: TProductionType;
      i: integer;
    begin
      params := _smScenarioCopy.simInput.herdRandomizationOptions;

      rdoRandomizedUnits.Checked := params.initInfectedRandomize;
      rdoAssignedUnits.Checked := not( rdoRandomizedUnits.Checked );

      rleLatent.Text := intToStr( params.initInfectedStateNumbers.at(0) );
      rleSubclinical.Text := intToStr( params.initInfectedStateNumbers.at(1) );
      rleClinical.Text := intToStr( params.initInfectedStateNumbers.at(2) );
      rleNatImmune.Text := intToStr( params.initInfectedStateNumbers.at(3) );
      rleVacImmune.Text := intToStr( params.initInfectedStateNumbers.at(4) );


      it := TProductionTypeListIterator.create( _smScenarioCopy.simInput.ptList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          lstProdTypes.Items.AddObject( it.current().productionTypeDescr, it.current() );
          it.incr();
        end
      ;

      it.Free();

      rdoSelectedProdTypes.Checked := params.initInfectedSelectedProdTypes;
      rdoAnyProdType.Checked := not( rdoSelectedProdTypes.Checked );

      for i := 0 to lstProdTypes.Items.Count - 1 do
        begin
          pt := lstProdTypes.Items.Objects[i] as TProductionType;

          if
            ( params.initInfectedSelectedProdTypes )
          and
          ( params.initInfectedProdTypes.contains( pt.productionTypeID ) )
          then
            lstProdTypes.Selected[i] := true
          else
            lstProdTypes.Selected[i] := false
          ;
        end
      ;

      rdoRange.Checked := params.initInfectedUseRange;
      rdoNoRange.Checked := not( rdoRange.Checked );

      if( gisValidLat( params.initInfectedLatNW ) ) then
        rleLatNW.Text := uiFloatToStr( params.initInfectedLatNW )
      ;

      if( gisValidLat( params.initInfectedLatSE ) ) then
        rleLatSE.Text := uiFloatToStr( params.initInfectedLatSE )
      ;

      if( gisValidLon( params.initInfectedLonNW ) ) then
        rleLonNW.Text := uiFloatToStr( params.initInfectedLonNW )
      ;

      if( gisValidLon( params.initInfectedLonSE ) ) then
        rleLonSE.Text := uiFloatToStr( params.initInfectedLonSE )
      ;

      showControls();
    end
  ;


  procedure TFormInitialUnitOptions.setSelectedPTS();
    var
      params: THerdRandomizationOptions;
      i: integer;
      str: string;
      lastChar: string;
    begin
      params := _smScenarioCopy.simInput.herdRandomizationOptions;

      str := '';
      for i := 0 to lstProdTypes.Items.Count - 1 do
        begin
          if( lstProdTypes.Selected[i] ) then
            str := str + intToStr( ( lstProdTypes.Items.Objects[i] as TProductionType ).productionTypeID ) + ','
          ;
        end
      ;

      lastChar := rightStr( str, 1 );
      if( ',' = lastChar ) then
        str := leftStr( str, length( str ) - 1 )
      ;
      params.setInitInfectedProdTypesFromString( str );
    end
  ;


  function TFormInitialUnitOptions.isValidLat( const val: double; rle: TREEdit = nil ): boolean;
    begin
      result := gisValidLat( val );

      if( not( result ) ) then
        begin
          result := false;
          msgOK( 
            tr( 'Latitudes must be between -90 and 90 degrees.' ), 
            tr( 'Invalid latitude' ), 
            IMGWarning, 
            self 
          );
          if( nil <> rle ) then rle.SetFocus();
        end
      ;
    end
  ;


  function TFormInitialUnitOptions.isValidLon( const val: double; rle: TREEdit = nil ): boolean;
    begin
      result := gisValidLon( val );

      if( not( result ) ) then
        begin
          result := false;
          msgOK( 
            tr( 'Longitudes must be between -180 and 180 degrees.' ), 
            tr( 'Invalid longitude' ), 
            IMGWarning, 
            self 
          );
          if( nil <> rle ) then rle.SetFocus();
        end
      ;
    end
  ;


  function TFormInitialUnitOptions.dataIsValid(): boolean;
    var
      params: THerdRandomizationOptions;
      str: string;

      latN, latS: double;
      lonW, lonE: double;

      totalInfected: integer;
  	begin
      result := true; // Until shown otherwise.

      totalInfected :=
        myStrToInt( rleLatent.Text )
        + myStrToInt( rleSubclinical.Text )
        + myStrToInt( rleClinical.Text )
      ;

      // There must be at least one infected unit.
      if( 0 = totalInfected ) then
        begin
          result := false;
          msgOK(
            tr( 'Please specify at least one unit initially in an infected disease state (latent, subclinical, or clinical).' ),
            tr( 'Invalid number of initially infected units' ),
            IMGWarning,
            self
          );
          exit;
        end
      ;

      // If selecting types, there must be at least one selected type.
      if( rdoSelectedProdTypes.checked ) then
        begin
          if( 0 = lstProdTypes.SelCount ) then
            begin
              result := false;
              msgOK(
                tr( 'No production types are selected.' ),
                tr( 'Invalid number of selected production types' ),
                IMGWarning,
                self
              );
              exit;
            end
          ;
        end
      ;


      // If using a geographic range:
      if( rdoRange.Checked ) then
        begin
          //  - Latitudes must be between -90 and 90.
          //  - Longitudes must be between -180 and 180.
          //  - LatN must be greater than or equal to latS.

          // FIX ME: more sophisticated validation might eventually be used:
          //  - Make sure that the specified region bounds at least part of the study area
          // See FormLatLonRange.validate()

          latN := uiStrToFloat( rleLatNW.Text, LAT_LON_UNDEFINED );
          latS := uiStrToFloat( rleLatSE.Text, LAT_LON_UNDEFINED );
          lonW := uiStrToFloat( rleLonNW.Text, LAT_LON_UNDEFINED );
          lonE := uiStrToFloat( rleLonSE.Text, LAT_LON_UNDEFINED );

          if( not( isValidLat( latN, rleLatNW ) ) ) then begin result := false; exit; end;
          if( not( isValidLon( lonW, rleLonNW ) ) ) then begin result := false; exit; end;

          if( not( isValidLat( latS, rleLatSE ) ) ) then begin result := false; exit; end;
          if( not( isValidLon( lonE, rleLonSE ) ) ) then begin result := false; exit; end;

          if( latS > latN ) then
            begin
              result := false;
              msgOK(
                tr( 'The southern latitude must be less than or equal to the northern latitude.' ),
                tr( 'Invalid latitude range' ),
                IMGWarning,
                self
              );
              rleLatSE.SetFocus();
              exit;
            end
          ;
        end
      ;


      // Handle assignment of new values to the parameter object in this function.
      // Why?  Because it's convenient to do so.
      //--------------------------------------------------------------------------
      if( result ) then
        begin
          params := _smScenarioCopy.simInput.herdRandomizationOptions;

          params.initInfectedRandomize := rdoRandomizedUnits.checked;

          str := intToStr( myStrToInt( rleLatent.Text ) ) + ','
            + intToStr( myStrToInt( rleSubclinical.Text ) ) + ','
            + intToStr( myStrToInt( rleClinical.Text ) ) + ','
            + intToStr( myStrToInt( rleNatImmune.Text ) ) + ','
            + intToStr( myStrToInt( rleVacImmune.Text ) )
          ;
          params.setInitInfectedStateNumbersFromString( str );

          params.initInfectedSelectedProdTypes := rdoSelectedProdTypes.Checked;

          setSelectedPTs();

          params.initInfectedUseRange := rdoRange.Checked;

          params.initInfectedLatNW := uiStrToFloat( rleLatNW.Text, NaN );
          params.initInfectedLonNW := uiStrToFloat( rleLonNW.Text, NaN );
          params.initInfectedLatSE := uiStrToFloat( rleLatSE.Text, NaN );
          params.initInfectedLonSE := uiStrToFloat( rleLonSE.Text, NaN );
        end
      ;
    end
  ;


  function TFormInitialUnitOptions.getDataUpdated(): boolean;
    begin
      result := _smScenarioCopy.simInput.herdRandomizationOptions.updated;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events and related functions
//-----------------------------------------------------------------------------
  procedure TFormInitialUnitOptions.showControls();
    var
      i: integer;
      pt: TProductionType;
      params: THerdRandomizationOptions;
    begin
      gbxDzStates.Visible := not( rdoAssignedUnits.Checked );
      gbxProdTypes.Visible := not( rdoAssignedUnits.Checked );
      gbxGeoRange.Visible := not( rdoAssignedUnits.Checked );

      if( gbxProdTypes.Visible ) then
        lstProdTypes.Enabled := rdoSelectedProdTypes.Checked
      ;

      params := _smScenarioCopy.simInput.herdRandomizationOptions;

      for i := 0 to lstProdTypes.Items.Count - 1 do
        begin
          pt := lstProdTypes.Items.Objects[i] as TProductionType;

          if
            ( rdoSelectedProdTypes.Checked )
          and
            ( params.initInfectedProdTypes.contains( pt.productionTypeID ) )
          then
            lstProdTypes.Selected[i] := true
          else
            lstProdTypes.Selected[i] := false
          ;
        end
      ;

      if( gbxGeoRange.Visible ) then
        begin
          rleLatNW.Enabled := rdoRange.Checked;
          rleLatSE.Enabled := rdoRange.Checked;
          rleLonNW.Enabled := rdoRange.Checked;
          rleLonSE.Enabled := rdoRange.Checked;

          lblNorthwest.Enabled := rdoRange.Checked;
          lblLatNW.Enabled := rdoRange.Checked;
          lblLonNW.Enabled := rdoRange.Checked;

          lblSoutheast.Enabled := rdoRange.Checked;
          lblLatSE.Enabled := rdoRange.Checked;
          lblLonSE.Enabled := rdoRange.Checked;
        end
      ;
    end
  ;


  procedure TFormInitialUnitOptions.rdoAssignedUnitsClick(Sender: TObject);
    begin
      showControls();
    end
  ;

  
  procedure TFormInitialUnitOptions.lstProdTypesChanged(Sender: TObject);
    begin
      dbcout( 'I''ve changed.', DBSHOWMSG );
      setSelectedPTS();
    end
  ;
//-----------------------------------------------------------------------------



initialization

  registerClass( TFormInitialUnitOptions );

end.

