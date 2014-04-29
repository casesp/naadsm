unit FormLatLonRange;

(*
FormLatLonRange.pas/dfm
-----------------------
Begin: 2006/02/27
Last revision: $Date: 2013-06-27 19:11:26 $ $Author: areeves $
Version number: $Revision: 1.10.4.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2009 Colorado State University

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
    StdCtrls,
    ExtCtrls,

    REEdit
	;

  type TRangeShape = (rsRectangle, rsCircle );

  type TFormLatLonRange = class( TForm )
      pnlTopSpacer: TPanel;

      pnlRadioControls: TPanel;
      gbxIncludeExclude: TGroupBox;
      rdoInclude: TRadioButton;
      rdoExclude: TRadioButton;

      gbxShape: TGroupBox;
      rdoRect: TRadioButton;
      rdoCircle: TRadioButton;

      pnlVertSpacer1: TPanel;
      pnlVertSpacer3: TPanel;
      pnlVertSpacer2: TPanel;

      pnlHorizSpacer1: TPanel;
      pnlHorizSpacer2: TPanel;

      gbxRect: TGroupBox;
      lblNorthwest: TLabel;
      lblLatNW: TLabel;
      lblLonNW: TLabel;
      rleLatNW: TREEdit;
      rleLonNW: TREEdit;
      lblSoutheast: TLabel;
      lblLatSE: TLabel;
      lblLonSE: TLabel;
      rleLonSE: TREEdit;
      rleLatSE: TREEdit;

      gbxCircle: TGroupBox;
      lblCenter: TLabel;
      lblLatCenter: TLabel;
      rleLatCenter: TREEdit;
      lblLonCenter: TLabel;
      rleLonCenter: TREEdit;
      lblRadius: TLabel;
      rleRadius: TREEdit;

      pnlButtonContainer: TPanel;
      pnlButtons: TPanel;
      btnOK: TButton;
      btnCancel: TButton;

			procedure FormCreate( Sender: TObject );

      procedure rdoShapeClick(Sender: TObject);
      procedure rdoIncludeClick(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      //procedure btnOKClick(Sender: TObject);
      //procedure btnCancelClick(Sender: TObject);

    protected
      // Properties
      _latNW: double;
      _lonNW: double;
      _latSE: double;
      _lonSE: double;
      _latCenter: double;
      _lonCenter: double;
      _radius: double;

      _oldLatNW: double;
      _oldLonNW: double;
      _oldLatSE: double;
      _oldLonSE: double;
      _oldLatCenter: double;
      _oldLonCenter: double;
      _oldRadius: double;

      _shape: TRangeShape;

      _exclude: boolean;

      procedure translateUI();

      procedure fixPanelColors( container: TWinControl );

      // Data entry validation
      function validate(): boolean;
      function isValidLat( const val: double; rle: TREEdit = nil ): boolean;
      function isValidLon( const val: double; rle: TREEdit = nil ): boolean;

      // Properties
      procedure setLatNW( val: double );
      procedure setLonNW( val: double );
      procedure setLatSE( val: double );
      procedure setLonSE( val: double );
      procedure setLatCenter( val: double );
      procedure setLonCenter( val: double );
      procedure setRadius( val: double );

      function getLatNW(): double;
      function getLonNW(): double;
      function getLatSE(): double;
      function getLonSE(): double;
      function getLatCenter(): double;
      function getLonCenter(): double;
      function getRadius(): double;


    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function showModal(): integer; override;

      // Properties
      property latNW: double read getLatNW write setLatNW;
      property lonNW: double read getLonNW write setLonNW;
      property latSE: double read getLatSE write setLatSE;
      property lonSE: double read getLonSE write setLonSE;
      property latCenter: double read getLatCenter write setLatCenter;
      property lonCenter: double read getLonCenter write setLonCenter;
      property radius: double read getRadius write setRadius;

      property shape: TRangeShape read _shape;
      property excludeRegion: boolean read _exclude;

    end
  ;


implementation

{$R *.dfm}

  uses
    RegExpDefs,
    MyStrUtils,
    BasicGIS,
    I88n,

    ControlUtils,
    MyDialogs
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormLatLonRange.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest.   

      fixPanelColors( self );

      rleLatNW.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLonNW.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLatSE.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLonSE.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLatCenter.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLonCenter.InputExpression := RE_SIGNED_DECIMAL_INPUT;

      rleRadius.InputExpression := RE_DECIMAL_INPUT;

      horizVertCenterInside( pnlButtons, pnlButtonContainer );

      // Set some default values
      rleLatNW.Text := '41';
      rleLonNW.Text := '-109';

      rleLatSE.Text := '37';
      rleLonSE.Text := '-102';

      rleLatCenter.Text := '40.41';
      rleLonCenter.Text := '-105.08';

      rleRadius.Text := '100';

      rdoIncludeClick( nil );
      rdoShapeClick( nil );
    end
  ;


  procedure TFormLatLonRange.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormLatLonRange.dfm
      // File date: Mon Oct 30 19:41:36 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Lat/lon range selection' );
          gbxIncludeExclude.Caption := tr( 'Inclusion/exclusion' );
          rdoInclude.Caption := tr( 'Select units within the specified area' );
          rdoExclude.Caption := tr( 'Select units that are NOT within the specified area' );
          gbxShape.Caption := tr( 'Area shape' );
          rdoRect.Caption := tr( 'Rectangular area' );
          rdoCircle.Caption := tr( 'Circular area' );
          btnOK.Caption := tr( '&OK' );
          btnCancel.Caption := tr( '&Cancel' );
          gbxRect.Caption := tr( 'Rectangular area' );
          lblNorthwest.Caption := tr( 'Northwest corner:' );
          lblLatNW.Caption := tr( 'Latitude:' );
          lblLonNW.Caption := tr( 'Longitude:' );
          lblSoutheast.Caption := tr( 'Southeast corner:' );
          lblLatSE.Caption := tr( 'Latitude:' );
          lblLonSE.Caption := tr( 'Longitude:' );
          gbxCircle.Caption := tr( 'Circular area' );
          lblCenter.Caption := tr( 'Center coordinates:' );
          lblLatCenter.Caption := tr( 'Latitude:' );
          lblLonCenter.Caption := tr( 'Longitude:' );
          lblRadius.Caption := tr( 'Circle radius (km):' );
        end
      ;

    end
  ;
  

	procedure TFormLatLonRange.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;
	

  procedure TFormLatLonRange.fixPanelColors( container: TWinControl );
    var
      i: integer;
      ctrl: TWinControl;
    begin
      for i := 0 to container.ControlCount - 1 do
        begin
          if( container.Controls[i] is TWinControl ) then
            begin
              ctrl := container.Controls[i] as TWinControl;

              if( ctrl is TPanel ) then
                (ctrl as TPanel).Color := clBtnFace
              ;

              fixPanelColors( ctrl );
            end
          ;
        end
      ;
    end
  ;



  destructor TFormLatLonRange.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormLatLonRange.showModal(): integer;
    begin
      _oldLatNW := _latNW;
      _oldLonNW := _lonNW;
      _oldLatSE := _latSE;
      _oldLonSE := _lonSE;
      _oldLatCenter := _latCenter;
      _oldLonCenter := _lonCenter;
      _oldRadius := _radius;

      result := inherited showModal();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data entry validation
//-----------------------------------------------------------------------------
  function TFormLatLonRange.validate(): boolean;
    var
      latN, latS: double;
      lonW, lonE: double;
      latC, lonC: double;
    begin
      // For all shapes:
      //  - Latitudes must be between -90 and 90.
      //  - Longitudes must be between -180 and 180.
      // For rectangles:
      //  - LatN must be greater than or equal to latS.
      // For circles:
      //  - Radius must be greater than 0.
      //
      // FIX ME: more sophisticated validation might eventually be used:
      //  - Make sure that the specified region bounds at least part of the study area
      // See TFormInitialUnitOptions.dataIsValid()

      result := true;

      latN := uiStrToFloat( rleLatNW.Text, LAT_LON_UNDEFINED );
      latS := uiStrToFloat( rleLatSE.Text, LAT_LON_UNDEFINED );
      lonW := uiStrToFloat( rleLonNW.Text, LAT_LON_UNDEFINED );
      lonE := uiStrToFloat( rleLonSE.Text, LAT_LON_UNDEFINED );
      latC := uiStrToFloat( rleLatCenter.Text, LAT_LON_UNDEFINED );
      lonC := uiStrToFloat( rleLonCenter.Text, LAT_LON_UNDEFINED );

      case _shape of
        rsRectangle:
          begin
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
        rsCircle:
          begin
            if( not( isValidLat( latC, rleLatCenter ) ) ) then begin result := false; exit; end;
            if( not( isValidLon( lonC, rleLonCenter ) ) ) then begin result := false; exit; end;

            if( not( 0.0 < uiStrToFloat( rleRadius.Text, -1 ) ) ) then
              begin
                result := false;
                msgOK( 
                  tr( 'Radius must be greater than 0.' ), 
                  tr( 'Invalid radius' ), 
                  IMGWarning, 
                  self 
                );
                rleRadius.SetFocus();
                exit;
              end
            ;
          end
        ;
      end;
    end
  ;


  function TFormLatLonRange.isValidLat( const val: double; rle: TREEdit = nil ): boolean;
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


  function TFormLatLonRange.isValidLon( const val: double; rle: TREEdit = nil ): boolean;
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
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormLatLonRange.rdoShapeClick(Sender: TObject);
    begin
      if( rdoRect.Checked ) then
        begin
          _shape := rsRectangle;

          gbxCircle.Align := alNone;
          gbxCircle.Visible := false;

          gbxRect.Align := alClient;
          gbxRect.visible := true;
        end
      else
        begin
          _shape := rsCircle;

          gbxRect.Align := alNone;
          gbxRect.Visible := false;

          gbxCircle.Align := alClient;
          gbxCircle.visible := true;
        end
      ;
    end
  ;


  procedure TFormLatLonRange.rdoIncludeClick(Sender: TObject);
    begin
      _exclude := not( rdoInclude.Checked );
    end
  ;

  
  procedure TFormLatLonRange.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      if( mrOK = ModalResult ) then
        begin
          if( not( validate() ) ) then
            action := caNone
          else // Validation was successful.  Store values and move on.
            begin
              _oldLatNW := _latNW;
              _oldLonNW := _lonNW;
              _oldLatSE := _latSE;
              _oldLonSE := _lonSE;
              _oldLatCenter := _latCenter;
              _oldLonCenter := _lonCenter;
              _oldRadius := _radius;

              action := caHide;
            end
          ;
        end
      else // User cancelled.  Restore old values and move one.
        begin
          latNW := _oldLatNW;
          lonNW := _oldLonNW;
          latSE := _oldLatSE;
          lonSE := _oldLonSE;
          latCenter := _oldLatCenter;
          lonCenter := _oldLonCenter;
          radius := _oldRadius;

          action := caHide;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TFormLatLonRange.setLatNW( val: double ); begin _latNW := val; rleLatNW.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;
  procedure TFormLatLonRange.setLonNW( val: double ); begin _lonNW := val; rleLonNW.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;
  procedure TFormLatLonRange.setLatSE( val: double ); begin _latSE := val; rleLatSE.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;
  procedure TFormLatLonRange.setLonSE( val: double ); begin _lonSE := val; rleLonSE.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;
  procedure TFormLatLonRange.setLatCenter( val: double ); begin _latCenter := val; rleLatCenter.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;
  procedure TFormLatLonRange.setLonCenter( val: double ); begin _lonCenter := val; rleLonCenter.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;
  procedure TFormLatLonRange.setRadius( val: double ); begin _radius := val; rleRadius.Text := uiFloatToStr( val, LAT_LON_PRECISION ); end;

  function TFormLatLonRange.getLatNW(): double; begin result := uiStrToFloat( rleLatNW.Text, LAT_LON_UNDEFINED ); end;
  function TFormLatLonRange.getLonNW(): double; begin result := uiStrToFloat( rleLonNW.Text, LAT_LON_UNDEFINED ); end;
  function TFormLatLonRange.getLatSE(): double; begin result := uiStrToFloat( rleLatSE.Text, LAT_LON_UNDEFINED ); end;
  function TFormLatLonRange.getLonSE(): double; begin result := uiStrToFloat( rleLonSE.Text, LAT_LON_UNDEFINED ); end;
  function TFormLatLonRange.getLatCenter(): double; begin result := uiStrToFloat( rleLatCenter.Text, LAT_LON_UNDEFINED ); end;
  function TFormLatLonRange.getLonCenter(): double; begin result := uiStrToFloat( rleLonCenter.Text, LAT_LON_UNDEFINED ); end;
  function TFormLatLonRange.getRadius(): double; begin result := uiStrToFloat( rleRadius.Text, LAT_LON_UNDEFINED ); end;
//-----------------------------------------------------------------------------


end.
