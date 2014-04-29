unit FrameDestruction;

(*
FrameDestruction.pas
---------------------
Begin: 2005/06/08
Last revision: $Date: 2008/03/12 22:10:50 $ $Author: areeves $
Version number: $Revision: 1.19 $
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

    ProductionType
  ;

  type TFrameDestruction = class( TFrame )
      cbxDestrBasic: TCheckBox;
      cbxDestrPreempt: TCheckBox;
      cbxDestrDirect: TCheckBox;
      cbxDestrIndirect: TCheckBox;
      cbxDestrRingTarget: TCheckBox;
      rleDestrRingRadius: TREEdit;
      lblDestrRingRadius: TLabel;
      cbxDestrRingTrigger: TCheckBox;

      lblBasicDestrNote: TLabel;
      lblTriggerNote: TLabel;
      lblTracingNote: TLabel;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

      procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );

  	protected
    	// properties
    	_prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      _tracingEnabled: boolean;

      procedure translateUI();

      // Display
      procedure updateDisplay();

      // properties
      procedure setProdType( val: TProductionType );
      function getProdType(): TProductionType;

      procedure setTracingEnabled( const val: boolean );

    public
    	// construction/initialization/destruction
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function isValid(): boolean;

      // properties
      property prodType: TProductionType read getProdType write setProdType;
      property tracingEnabled: boolean write setTracingEnabled;
    end
  ;

implementation

{$R *.dfm}

	uses
  	RegExpDefs,
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    MyDialogs,
    I88n
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameDestruction.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      
      _prodType := nil;
      _tracingEnabled := false;

      rleDestrRingRadius.InputExpression := RE_DECIMAL_INPUT;

      _loading := false;
    end
  ;


  procedure TFrameDestruction.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameDestruction.dfm
      // File date: Wed Oct 25 14:50:18 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblDestrRingRadius.Caption := tr( 'Ring radius (km):' );
          lblBasicDestrNote.Caption := tr( '(Diseased units of this production type will be destroyed if detected) ' );
          lblTriggerNote.Caption := tr( '(Units of this and/or other types may be pre-emptively destroyed if they are within the specified ring) ' );
          lblTracingNote.Caption := tr( '* Tracing must be conducted for this type, or this option will be unavailable' );
          cbxDestrBasic.Caption := tr( 'Destroy detected diseased units of this production type' );
          cbxDestrPreempt.Caption := tr( 'Pre-emptively destroy units of this production type' );
          cbxDestrDirect.Caption := tr( 'Destroy units of this production type that have had DIRECT contact with a detected unit as identified by tracing*' );
          cbxDestrIndirect.Caption := tr( 'Destroy units of this production type that have had INDIRECT contact with a detected unit as identified by tracing*' );
          cbxDestrRingTarget.Caption := tr( 'Destroy units of this type when they are within a destruction ring around any unit that is a ring trigger' );
          cbxDestrRingTrigger.Caption := tr( 'Trigger ring destruction around detected units of this production type' );
        end
      ;

    end
  ;


  destructor TFrameDestruction.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameDestruction.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
          _prodType.updated := true;

          _prodType.destructionParams.destroyDetectedUnits := cbxDestrBasic.Checked;

          _prodType.destructionParams.isRingTrigger := cbxDestrRingTrigger.Checked;

          if( cbxDestrPreempt.Checked ) then
            begin
              _prodType.destructionParams.isRingTarget := cbxDestrRingTarget.Checked;
              _prodType.destructionParams.destroyDirectTraces := cbxDestrDirect.Checked;
              _prodType.destructionParams.destroyIndirectTraces := cbxDestrIndirect.Checked;
            end
          else
            begin
              _prodType.destructionParams.isRingTarget := false;
              _prodType.destructionParams.destroyDirectTraces := false;
              _prodType.destructionParams.destroyIndirectTraces := false;
            end
          ;

          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameDestruction.processTextEntry( Sender: TObject );
  	begin
    	_prodType.destructionParams.ringRadius := myStrToFloat( rleDestrRingRadius.Text, -1.0 );

   		_prodType.updated := true;
    end
  ;


  // This function deals with a little bug in TREEdit.
  procedure TFrameDestruction.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
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
	procedure TFrameDestruction.updateDisplay();
  	begin
    	_loading := true;

      cbxDestrBasic.checked := _prodType.destructionParams.destroyDetectedUnits;

      if( _tracingEnabled ) then
        begin
          if( cbxDestrPreempt.Checked ) then
            begin
              cbxDestrDirect.Enabled := _prodType.destructionParams.traceDirectContact;

              if( cbxDestrDirect.Enabled ) then
                cbxDestrDirect.Checked := _prodType.destructionParams.destroyDirectTraces
              else
                cbxDestrDirect.Checked := false
              ;

              cbxDestrIndirect.Enabled := _prodType.destructionParams.traceIndirectContact;

              if( cbxDestrIndirect.Enabled ) then
                cbxDestrIndirect.Checked := _prodType.destructionParams.destroyIndirectTraces
              else
                cbxDestrIndirect.Checked := false
              ;
            end
          else
            begin
              cbxDestrDirect.Checked := false;
              cbxDestrDirect.Enabled := false;
              cbxDestrIndirect.Checked := false;
              cbxDestrIndirect.Enabled := false;
            end
        end
      else
        begin
          cbxDestrDirect.Checked := false;
          cbxDestrDirect.Enabled := false;
          cbxDestrIndirect.Checked := false;
          cbxDestrIndirect.Enabled := false;
        end
      ;

      cbxDestrRingTrigger.Checked := _prodType.destructionParams.isRingTrigger;

      cbxDestrRingTarget.Checked := _prodType.destructionParams.isRingTarget;
      cbxDestrRingTarget.Enabled := cbxDestrPreempt.Checked;

      if( 0 <= _prodType.destructionParams.ringRadius ) then
        rleDestrRingRadius.Text := uiFloatToStr( _prodType.destructionParams.ringRadius )
      else
        rleDestrRingRadius.Text := ''
      ;

      rleDestrRingRadius.Visible := cbxDestrRingTrigger.Checked;
      lblDestrRingRadius.Visible := cbxDestrRingTrigger.Checked;

      _loading := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  procedure TFrameDestruction.setTracingEnabled( const val: boolean );
    begin
      _tracingEnabled := val;
    end
  ;

  function TFrameDestruction.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameDestruction.setProdType( val: TProductionType );
  	begin
      _loading := true;

    	_prodType := val;
      cbxDestrPreempt.checked := _prodType.destructionParams.isRingTarget or _prodType.destructionParams.destroyDirectTraces or _prodType.destructionParams.destroyIndirectTraces;

      _loading := false;
      
      updateDisplay();
    end
  ;


  function TFrameDestruction.isValid(): boolean;
    begin
      result := true;

      if( cbxDestrRingTrigger.Checked ) then
        begin
          if( 0 >= myStrToFloat( rleDestrRingRadius.Text ) ) then
            begin
              msgOK(
                tr( 'Destruction ring radius must be greater than 0.' ),
                tr( 'Parameter out of range' ),
                IMGCritical,
                _myParent
              );

              rleDestrRingRadius.SetFocus();
              result := false;
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
