unit FrameTracing;

(*
FrameTracing.pas
----------------
Begin: 2006/02/05
Last revision: $Date: 2008/04/18 20:35:18 $ $Author: areeves $
Version number: $Revision: 1.3 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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

    ProductionType
  ;

  type TFrameTracing = class( TFrame )
    	rleSurvDirectTracePeriod: TREEdit;
    	lblSurvDirectTracePeriod: TLabel;
    	rleSurvIndirectTracePeriod: TREEdit;
    	lblSurvIndirectTracePeriod: TLabel;
    	lblSurvDirectSuccess: TLabel;
    	rleSurvDirectSuccess: TREEdit;
    	rleSurvIndirectSuccess: TREEdit;
    	lblSurvIndirectSuccess: TLabel;
      cbxDirectTrace: TCheckBox;
      cbxIndirectTrace: TCheckBox;
      lblTraceForward: TLabel;
    	
      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

      procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

  	protected
    	// properties
    	_prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      procedure translateUI();

      // Display
      procedure setDirectEnabled();
      procedure setIndirectEnabled();

      procedure updateDisplay();

      // properties
      procedure setProdType( val: TProductionType );
      function getProdType(): TProductionType;

    public
    	// construction/initialization/destruction
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function isValid(): boolean;

      // properties
      property prodType: TProductionType read getProdType write setProdType;
    end
  ;

implementation

{$R *.dfm}

	uses
  	RegExpDefs,
    MyStrUtils,
    GuiStrUtils,
    MyDialogs,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameTracing.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      
      _prodType := nil;

      rleSurvDirectSuccess.InputExpression := RE_DECIMAL_INPUT;
      rleSurvDirectTracePeriod.InputExpression := RE_INTEGER_INPUT;
      rleSurvIndirectSuccess.InputExpression := RE_DECIMAL_INPUT;
      rleSurvIndirectTracePeriod.InputExpression := RE_INTEGER_INPUT;

      _loading := false;
    end
  ;


  procedure TFrameTracing.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameTracing.dfm
      // File date: Thu Feb 8 17:04:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblSurvDirectTracePeriod.Caption := tr( 'Days before detection:' );
          lblSurvIndirectTracePeriod.Caption := tr( 'Days before detection:' );
          lblSurvDirectSuccess.Caption := tr( 'Probability of trace success (0 to 1):' );
          lblSurvIndirectSuccess.Caption := tr( 'Probability of trace success (0 to 1):' );
          lblTraceForward.Caption := tr( 'Trace-forward (trace-out) investigations' );
          cbxDirectTrace.Caption := tr( 'Trace direct contacts' );
          cbxIndirectTrace.Caption := tr( 'Trace indirect contacts' );
        end
      ;

    end
  ;
    

  destructor TFrameTracing.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameTracing.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
          _prodType.updated := true;

          _prodType.destructionParams.traceDirectContact := cbxDirectTrace.Checked;
          _prodType.destructionParams.traceIndirectContact := cbxIndirectTrace.Checked;

          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameTracing.processTextEntry( Sender: TObject );
  	begin
      _prodType.destructionParams.directTracePeriod := myStrToInt( rleSurvDirectTracePeriod.Text, -1 );
      _prodType.destructionParams.directTraceSuccess := myStrToFloat( rleSurvDirectSuccess.Text, -1.0 );

      _prodType.destructionParams.indirectTracePeriod := myStrToInt( rleSurvIndirectTracePeriod.Text, -1 );
      _prodType.destructionParams.indirectTraceSuccess := myStrToFloat( rleSurvIndirectSuccess.Text, -1.0 );

   		_prodType.updated := true;
    end
  ;


  // This function deals with a little bug in TREEdit.
  procedure TFrameTracing.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
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
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameTracing.updateDisplay();
  	begin
      setDirectEnabled();
      setIndirectEnabled();
    end
  ;


  procedure TFrameTracing.setDirectEnabled();
  	begin
      if( 0 <= _prodType.destructionParams.directTracePeriod ) then
        rleSurvDirectTracePeriod.Text := intToStr( _prodType.destructionParams.directTracePeriod )
      else
        rleSurvDirectTracePeriod.Text := ''
      ;

      if( 0.0 <= _prodType.destructionParams.directTraceSuccess ) then
        rleSurvDirectSuccess.Text := uiFloatToStr( _prodType.destructionParams.directTraceSuccess )
      else
        rleSurvDirectSuccess.Text := ''
      ;

      if( ( cbxDirectTrace.Checked ) ) then
      	begin
       		rleSurvDirectTracePeriod.Enabled := true;
          rleSurvDirectSuccess.Enabled := true;
          lblSurvDirectSuccess.enabled := true;
          lblSurvDirectTracePeriod.enabled := true;
        end
      else
      	begin
          rleSurvDirectSuccess.Enabled := false;
          rleSurvDirectTracePeriod.Enabled := false;
          lblSurvDirectSuccess.enabled := false;
          lblSurvDirectTracePeriod.enabled := false;
        end
      ;
    end
  ;


  procedure TFrameTracing.setIndirectEnabled();
  	begin
      if( 0 <= _prodType.destructionParams.indirectTracePeriod ) then
        rleSurvIndirectTracePeriod.Text := intToStr( _prodType.destructionParams.indirectTracePeriod )
      else
        rleSurvIndirectTracePeriod.Text := ''
      ;

      if( 0 <= _prodType.destructionParams.indirectTraceSuccess ) then
        rleSurvIndirectSuccess.Text := uiFloatToStr( _prodType.destructionParams.indirectTraceSuccess )
      else
        rleSurvIndirectSuccess.Text := ''
      ;

      if( cbxIndirectTrace.checked ) then
      	begin
       		rleSurvIndirectTracePeriod.Enabled := true;
          rleSurvIndirectSuccess.Enabled := true;
          lblSurvIndirectSuccess.enabled := true;
          lblSurvIndirectTracePeriod.enabled := true;
        end
      else
      	begin
          rleSurvIndirectSuccess.Enabled := false;
          rleSurvIndirectTracePeriod.Enabled := false;
          lblSurvIndirectSuccess.enabled := false;
          lblSurvIndirectTracePeriod.enabled := false;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameTracing.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameTracing.setProdType( val: TProductionType );
  	begin
    	_loading := true;

    	_prodType := val;

      cbxDirectTrace.Checked := _prodType.destructionParams.traceDirectContact;
      cbxIndirectTrace.checked := _prodType.destructionparams.traceIndirectContact;

      _loading := false;

      updateDisplay();
    end
  ;


  function TFrameTracing.isValid(): boolean;
    begin
      result := true;

      if( cbxDirectTrace.Checked ) then
        begin
          if
            ( 0 > myStrToFloat( rleSurvDirectSuccess.text ) )
          or
            ( 1 < myStrToFloat( rleSurvDirectSuccess.text ) )
          then
            begin
              msgOK(
                tr( 'Probability of direct trace success must be between 0 and 1, inclusive.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleSurvDirectSuccess.SetFocus();
              result := false;
              exit;
            end
          ;
        end
      ;

      if( cbxIndirectTrace.Checked ) then
        begin
          if
            ( 0 > myStrToFloat( rleSurvIndirectSuccess.text ) )
          or
            ( 1 < myStrToFloat( rleSurvIndirectSuccess.text ) )
          then
            begin
              msgOK(
                tr( 'Probability of indirect trace success must be between 0 and 1, inclusive.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleSurvIndirectSuccess.SetFocus();
              result := false;
              exit;
            end
          ;
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------


end.
