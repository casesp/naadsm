unit FrameAirborneSpread;

(*
FrameAirborneSpread.pas/dfm
---------------------------
Begin: 2005/06/10
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.31 $
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
    REEdit,
    ExtCtrls,
    
    AirborneSpreadModel,

		FrameFunctionEditor,
		FrameSMFunctionEditor,    
    
    FrameWindDirection
	;

  type TFrameAirborneSpread = class( TFrame )    
      pnlParams: TPanel;
      lblMaxSpread: TLabel;
      lblProbSpread1km: TLabel;
      rleProbSpread1km: TREEdit;
      rleMaxSpread: TREEdit;
      
      fraWindDir: TFrameWindDirection;
      
    	smcTransportDelay: TFrameSMFunctionEditor;
      lblTransportDelay: TLabel;
      pnlUseAirborneSpread: TPanel;
      cbxUseAirborneSpread: TCheckBox;
      imgPDF: TImage;
      lblUseExponential: TLabel;

      procedure processText( sender: TObject );
      procedure cbxUseAirborneSpreadClick(Sender: TObject);
  		procedure processWindDirText( sender: TObject );

    procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

    protected
      // properties
      _am: TAirborneSpreadModel;

      _myParent: TWinControl;

      _useExp: boolean; // use exponential (TRUE) or linear (FALSE) decay for airborne spread

      procedure translateUI();

      // properties
      procedure setASM( val: TAirborneSpreadModel );
      function getASM(): TAirborneSpreadModel;

      procedure setUseExp( val: boolean );
      function getUseExp(): boolean;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure updateDisplay();

      function isValid(): boolean;

      property airborneSpreadModel: TAirborneSpreadModel read getASM write setASM;
      property useAirborneExponentialDecay: boolean read getUseExp write setUseExp;
    end
  ;

implementation

{$R *.dfm}


	uses
  	RegExpDefs,
    FormSMWizardBase,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    MyDialogs,
    ChartFunction,
    I88n
  ;


  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit



//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
	constructor TFrameAirborneSpread.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;

      lblUseExponential.Top := lblMaxSpread.Top;
      lblUseExponential.left := lblMaxSpread.left;

      smcTransportDelay.setForm( AOwner as TFormSMWizardBase );
      smcTransportDelay.chartType := CTPdf;
      smcTransportDelay.xUnits := UnitsDays;

      rleMaxSpread.InputExpression := RE_DECIMAL_INPUT;
      rleProbSpread1km.InputExpression := RE_DECIMAL_INPUT;

      _useExp := false;
    end
  ;


  procedure TFrameAirborneSpread.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameAirborneSpread.dfm
      // File date: Fri Jan 19 17:43:56 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblMaxSpread.Caption := tr( 'Maximum distance spread under these conditions (in km, minimum 1 km):' );
          lblProbSpread1km.Caption := tr( 'Probability of spread/contg. day, at 1 km, average unit sizes (0 to 1):' );
          lblTransportDelay.Caption := tr( 'Airborne transport delay:' );
          imgPDF.Hint := tr( 'This parameter is a probability density function' );
          lblUseExponential.Caption := tr( '(The rate of disease declines exponentially from the source: a maximum distance of spread is not required.) ' );
          cbxUseAirborneSpread.Caption := tr( 'Model airborne spread between these production types' );
        end
      ;

    end
  ;


  destructor TFrameAirborneSpread.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameAirborneSpread.updateDisplay();
    begin
    	if( nil <> _am ) then
      	begin
          cbxUseAirborneSpread.Checked := _am.useAirborne;

          if( 0 <= _am.maxSpread ) then
            rleMaxSpread.text := uiFloatToStr( _am.maxSpread )
          else
            rleMaxSpread.text := ''
          ;

          if( 0 <= _am.probSpread1km ) then
            rleProbSpread1km.text := uiFloatToStr( _am.probSpread1km )
          else
            rleProbSpread1km.text := ''
          ;

          fraWindDir.setAngles( _am.windStart, _am.windEnd );

          pnlParams.Visible := _am.useAirborne;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameAirborneSpread.processText( sender: TObject );
    begin
      dbcout( 'Processing text in TFrameAirborneSpread', DBSHOWMSG );

    	if( nil <> _am ) then
      	begin
          _am.maxSpread := myStrToFloat( rleMaxSpread.text, -1.0 );
          _am.probSpread1km := myStrToFloat( rleProbSpread1km.text, -1.0 );
          _am.updated := true;
        end
      ;
    end
  ;


  procedure TFrameAirborneSpread.cbxUseAirborneSpreadClick(Sender: TObject);
    begin
    	if( nil <> _am ) then
      	begin
          _am.useAirborne := cbxUseAirborneSpread.checked;
          _am.updated := true;
          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameAirborneSpread.processWindDirText( sender: TObject );
  	begin
      fraWindDir.processText( sender );

      _am.windStart := fraWindDir.startAngle;
      _am.windEnd := fraWindDir.endAngle;
      _am.updated := true;      
     end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TFrameAirborneSpread.setUseExp( val: boolean );
    begin
      _useExp := val;
      lblMaxSpread.Visible := not( val );
      rleMaxSpread.Visible := not( val );

      lblUseExponential.Visible := val;
    end
  ;


  function TFrameAirborneSpread.getUseExp(): boolean;
    begin
      result := _useExp;
    end
  ;

  function TFrameAirborneSpread.getASM(): TAirborneSpreadModel;
  	begin
    	if( nil <> _am ) then
      	begin
          _am.windStart := fraWindDir.startAngle;
          _am.windEnd := fraWindDir.endAngle;
          _am.updated := true;
        end
      ;

    	result := _am;
    end
  ;


  procedure TFrameAirborneSpread.setASM( val: TAirborneSpreadModel );
  	begin
    	_am := val;
      updateDisplay();
    end
  ;

  function TFrameAirborneSpread.isValid(): boolean;
  	begin
      result := true;

      if( cbxUseAirborneSpread.Checked ) then
        begin
          if
            ( 0 > myStrToFloat( rleProbSpread1km.text, -1.0 ) )
          or
            ( 1 < myStrToFloat( rleProbSpread1km.text, -1.0 ) )
          then
            begin
              msgOK(
                tr( 'Probability must be between 0 and 1, inclusive.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleProbSpread1km.SetFocus();
              result := false;
              exit;
            end
          ;

          if( useAirborneExponentialDecay ) then
            begin
              if( 1.0 = myStrToFloat( rleProbSpread1km.Text, -1.0 ) ) then
                begin
                  msgOK(
                    tr( 'Probability of spread at 1 km must be less than 1 for exponential decay.' ),
                    tr( 'Parameter out of range' ),
                    IMGCritical,
                    _myParent
                  );

                  rleProbSpread1km.SetFocus();
                  result := false;
                  exit;
                end
              ;
            end
          ;

          if( not( useAirborneExponentialDecay ) ) then
            begin
              if( 1 > myStrToFloat( rleMaxSpread.Text, -1.0 ) ) then
                begin
                  msgOK(
                    tr( 'Maximum distance of spread must be at least 1 km.' ),
                    tr( 'Parameter out of range' ),
                    IMGCritical,
                    _myParent
                  );

                  rleMaxSpread.SetFocus();
                  result := false;
                  exit;
                end
              ;
            end
          ;

        end
      ;

    end
  ;
//-----------------------------------------------------------------------------


  // This function deals with a little bug in TREEdit.
  procedure TFrameAirborneSpread.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    var
      rle: TREEdit;
    begin
      if( sender is TREEdit ) then
        begin
          rle := sender as TREEdit;
          if( rle.SelLength = length( rle.Text ) ) then rle.Text := '';
        end
      ;
    end
  ;

end.
