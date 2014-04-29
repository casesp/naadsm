unit FrameAirborneOrLocalAreaSpread;

(*
FrameAirborneOrLocalAreaSpread.pas/dfm
--------------------------------------
Begin: 2005/06/10
Last revision: $Date: 2011-06-14 14:46:44 $ $Author: rhupalo $
Version: $Revision: 1.1.6.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 NAADSM Development Team

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
    
    AirborneSpreadParams,
    LocalAreaSpreadParams
  ;

  type TFrameAirborneOrLocalAreaSpread = class( TFrame )    
      pnlParams: TPanel;
      pnlUseAirborneSpread: TPanel;
      cbxUseSpread: TCheckBox;
    pnlSizeParams: TPanel;
    lblNInfectiousInSource: TLabel;
    reNInfectiousInSource: TREEdit;
    lblNSusceptibleInRecipient: TLabel;
    reNSusceptibleInRecipient: TREEdit;
    pnlOtherParams: TPanel;
    lblDistBetwUnits: TLabel;
    reDistBetwUnits: TREEdit;
    lblProbSpread: TLabel;
    reProbSpread: TREEdit;
      procedure processText( sender: TObject );
      procedure cbxUseSpreadClick(Sender: TObject);

    protected
      // properties
      _am: TLocalAreaSpreadBase;

      _myParent: TWinControl;

      _useExp: boolean; // use exponential (TRUE) or linear (FALSE) decay for airborne spread

      procedure translateUI();

      // properties
      procedure setASM( val: TLocalAreaSpreadBase );
      function getASM(): TLocalAreaSpreadBase;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure setCaptions( t: TModelType );

      procedure updateDisplay();

      function isValid(): boolean;

      property spreadParams: TLocalAreaSpreadBase read getASM write setASM;
    end
  ;

implementation

{$R *.dfm}


  uses
    RegExpDefs,
    FormSMWizardBase,
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    I88n
  ;


  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit



//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameAirborneOrLocalAreaSpread.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;

      pnlSizeParams.BevelOuter := bvNone;
      pnlOtherParams.BevelOuter := bvNone;

      reNInfectiousInSource.InputExpression := RE_INTEGER_INPUT;
      reNSusceptibleInRecipient.InputExpression := RE_INTEGER_INPUT;
      reDistBetwUnits.InputExpression := RE_DECIMAL_INPUT;
      reProbSpread.InputExpression := RE_DECIMAL_INPUT;
    end
  ;


  procedure TFrameAirborneOrLocalAreaSpread.setCaptions( t: TModelType );
    begin
      case t of
        MTUnspecified:
          begin
            raise exception.Create( 'Model type is not set in TFrameAirborneOrLocalAreaSpread.setASM()' );
          end
        ;
        MTAirborne:
          begin
            cbxUseSpread.Caption := tr( 'Model airborne spread between these production types' );
            lblProbSpread.Caption := tr( 'Daily probability of airborne spread of disease between these two units:' );
          end
        ;
        MTLocalArea:
          begin
            cbxUseSpread.Caption := tr( 'Model local area spread between these production types' );
            lblProbSpread.Caption := tr( 'Daily probability of local area spread of disease between these two units:' );
          end
        ;
      end;
    end
  ;


  procedure TFrameAirborneOrLocalAreaSpread.translateUI();
    begin
      dbcout( 'Rewrite TFrameAirborneOrLocalAreaSpread.translateUI()', true );
    end
  ;


  destructor TFrameAirborneOrLocalAreaSpread.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameAirborneOrLocalAreaSpread.updateDisplay();
    begin
      if( nil <> _am ) then
        begin
          cbxUseSpread.Checked := _am.thisModelIsUsed;

          if( _am.includeLASSizeAdjustment ) then
            pnlSizeParams.Height :=  73
          else
            pnlSizeParams.Height :=  0
          ;

          if( 0.0 < _am.probSpread ) then
            {Original code, if the value has >6 decimals then it rounded to 0 (and for some reason not reloaded to the edit box on re-opening scenario)!
              reProbSpread.text := uiFloatToStr( _am.probSpread)
            Allow 10 decimals rather than the default 6, but it displays in exponential format - but can not be edited because of reg exp!
              reProbSpread.text := uiFloatToStr( _am.probSpread, 10 )
            }

            // This probability may need to be smaller than the default allowance of six decimal places
            // Displays as decimal value out to 10 decimals if they occur
            reProbSpread.text := FormatFloat('0.##########', _am.probSpread )
          else
            reProbSpread.text := ''
          ;

          if( 0.0 < _am.distBetwUnits ) then
            reDistBetwUnits.text := uiFloatToStr( _am.distBetwUnits )
          else
            reDistBetwUnits.text := ''
          ;

          if( 0 < _am.nInfectiousInSource ) then
            reNInfectiousInSource.text := uiFloatToStr( _am.nInfectiousInSource )
          else
            reNInfectiousInSource.text := ''
          ;

          if( 0 < _am.nSusceptibleInReceipient ) then
            reNSusceptibleInRecipient.text := uiFloatToStr( _am.nSusceptibleInReceipient )
          else
            reNSusceptibleInRecipient.text := ''
          ;

          pnlParams.Visible := _am.thisModelIsUsed;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameAirborneOrLocalAreaSpread.processText( sender: TObject );
    begin
      dbcout( 'Processing text in TFrameAirborneOrLocalAreaSpread', DBSHOWMSG );

      if( nil <> _am ) then
        begin
          _am.probSpread := uiStrToFloat( reProbSpread.Text, -1.0 );
          _am.distBetwUnits := uiStrToFloat( reDistBetwUnits.Text, -1.0 );
          _am.nInfectiousInSource := myStrToInt( reNInfectiousInSource.Text, - 1 );
          _am.nSusceptibleInReceipient := myStrToInt( reNSusceptibleInRecipient.Text, - 1 );
          _am.updated := true;

          self.isValid(); //rbh: catch invalid entries immediately, issue 2329
        end
      ;
    end
  ;


  procedure TFrameAirborneOrLocalAreaSpread.cbxUseSpreadClick(Sender: TObject);
    begin
      if( nil <> _am ) then
        begin
          _am.thisModelIsUsed := cbxUseSpread.checked;
          _am.updated := true;
          updateDisplay();
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameAirborneOrLocalAreaSpread.getASM(): TLocalAreaSpreadBase;
    begin
      result := _am;
    end
  ;


  procedure TFrameAirborneOrLocalAreaSpread.setASM( val: TLocalAreaSpreadBase );
    begin
      _am := val;

      updateDisplay();
    end
  ;

  function TFrameAirborneOrLocalAreaSpread.isValid(): boolean;
    begin
      result := true;

      if( cbxUseSpread.Checked ) then
        begin
          if( not( strIsEmpty( trim( reProbSpread.text ) ) ) ) then
            begin
              if
                ( 0.0 >= uiStrToFloat( reProbSpread.text, -1.0 ) )
              or
                ( 1.0 <= uiStrToFloat( reProbSpread.text, -1.0 ) )
              then
                begin
                  msgOK(
                    tr( 'Daily probability of spread must be greater than 0 and less than 1.' ),
                    tr( 'Parameter out of range' ),
                    IMGCritical,
                    _myParent
                  );

                  reProbSpread.SetFocus();
                  result := false;
                  exit;
                end
              ;

              { rbh: if block added related to issue 2329.
                Entering a value smaller than 6 decimal places was rounded to 0. Zero is no longer allowed as a valid input value
                due to it's influence in the calculation of K. Probabilities to just the default 6
                decimal places may not be enough for this parameter. So updateDisplay() truncates
                to 10 decimal places, rather than the default 6. Analysis of the influence
                of this probability on the calculation of K showed that a value of 1E-11 results
                in a zero probability of infection transfer over a large range of herd sizes and distances.
              }
              if ( 0.00000000009 > uiStrToFloat( reProbSpread.text, -1.0 ) ) then
                begin
                  msgOK(
                    tr( 'Daily probability of spread can not be smaller than 10 decimals.' ),
                    tr( 'Parameter out of range' ),
                    IMGCritical,
                    _myParent
                  );

                  reProbSpread.SetFocus();
                  result := false;
                  exit;
                end
              ;

            end
          ;

          if( _am.includeLASSizeAdjustment ) then
            begin
              if( not( strIsEmpty( trim( reNInfectiousInSource.text ) ) ) ) then
                begin
                  if ( 0 >= myStrToInt( reNInfectiousInSource.text, -1 ) ) then
                    begin
                      msgOK(
                        tr( 'The number of infectious animals in the source unit must be greater than 0.' ),
                        tr( 'Parameter out of range' ),
                        IMGCritical,
                        _myParent
                      );

                      reNInfectiousInSource.SetFocus();
                      result := false;
                      exit;
                    end
                  ;
                end
              ;

              if( not( strIsEmpty( trim( reNSusceptibleInRecipient.text ) ) ) ) then
                begin
                  if ( 0 >= myStrToInt( reNSusceptibleInRecipient.text, -1 ) ) then
                    begin
                      msgOK(
                        tr( 'The number of susceptible animals in the recipient unit must be greater than 0.' ),
                        tr( 'Parameter out of range' ),
                        IMGCritical,
                        _myParent
                      );

                      reNSusceptibleInRecipient.SetFocus();
                      result := false;
                      exit;
                    end
                  ;
                end
              ;
            end
          ;

          if( not( strIsEmpty( trim( reDistBetwUnits.text ) ) ) ) then
            begin
              if ( 0.0 >= uiStrToFloat( reDistBetwUnits.text, -1.0 ) ) then
                begin
                  msgOK(
                    tr( 'The distance between these two units must be greater than 0.' ),
                    tr( 'Parameter out of range' ),
                    IMGCritical,
                    _myParent
                  );

                  reDistBetwUnits.SetFocus();
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


end.
