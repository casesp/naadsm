unit FrameDestruction;

(*
FrameDestruction.pas
---------------------
Begin: 2005/06/08
Last revision: $Date: 2013-05-14 17:04:59 $ $Author: areeves $
Version number: $Revision: 1.24.10.5 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2013 Colorado State University

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
      cbxDestrDirectForward: TCheckBox;
      cbxDestrIndirectForward: TCheckBox;
      cbxDestrRingTarget: TCheckBox;
      rleDestrRingRadius: TREEdit;
      lblDestrRingRadius: TLabel;
      cbxDestrRingTrigger: TCheckBox;

      lblBasicDestrNote: TLabel;
      lblTriggerNote: TLabel;
      lblTracingNote: TLabel;
      cbxDestrDirectBack: TCheckBox;
      cbxDestrIndirectBack: TCheckBox;
      lblPreempt: TLabel;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );
      
  	protected
    	// properties
    	_prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      _tracingEnabled: boolean;

      procedure translateUI();
      procedure translateUIManual();

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
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FrameDestruction.dfm
      // File date: Thu Apr 24 09:10:22 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblDestrRingRadius.Caption := tr( 'Ring radius (km):' );
          lblBasicDestrNote.Caption := tr( '(Units of this production type will be destroyed if detected) ' );
          lblTriggerNote.Caption := tr( '(Units of this and/or other types may be pre-emptively destroyed if they are within the specified ring) ' );
          lblTracingNote.Caption := tr( '* Tracing must be conducted, or this option will be unavailable' );
          cbxDestrBasic.Caption := tr( 'Destroy detected units of this production type' );
          lblPreempt.Caption := tr( 'Pre-emptive destuction of units of this production type' );
          cbxDestrDirectForward.Caption := tr( 'Destroy units of this production type that have had DIRECT contact with a detected unit as identified by TRACE FORWARD*' );
          cbxDestrIndirectForward.Caption := tr( 'Destroy units of this production type that have had INDIRECT contact with a detected unit as identified by TRACE FORWARD*' );
          cbxDestrRingTarget.Caption := tr( 'Destroy units of this type when they are within a destruction ring around any unit that is a ring trigger' );
          cbxDestrRingTrigger.Caption := tr( 'Trigger ring destruction around detected units of this production type' );
          cbxDestrDirectBack.Caption := tr( 'Destroy units of this production type that have had DIRECT contact with a detected unit as identified by TRACE BACK*' );
          cbxDestrIndirectBack.Caption := tr( 'Destroy units of this production type that have had INDIRECT contact with a detected unit as identified by TRACE BACK*' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameDestruction.translateUIManual();
    begin

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

          // Preemptive destruction
          //-----------------------
          _prodType.destructionParams.isRingTarget := cbxDestrRingTarget.Checked;
          _prodType.destructionParams.destroyDirectForwardTraces := cbxDestrDirectForward.Checked;
          _prodType.destructionParams.destroyIndirectForwardTraces := cbxDestrIndirectForward.Checked;
          _prodType.destructionParams.destroyDirectBackTraces := cbxDestrDirectBack.Checked;
          _prodType.destructionParams.destroyIndirectBackTraces := cbxDestrIndirectBack.Checked;

          if( not _prodType.isDestrTarget ) then
            _prodType.destructionParams.destrPriority := -1
          ;

          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameDestruction.processTextEntry( Sender: TObject );
  	begin
    	_prodType.destructionParams.ringRadius := uiStrToFloat( rleDestrRingRadius.Text, -1.0 );

   		_prodType.updated := true;
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
          cbxDestrDirectForward.Enabled := true;
          cbxDestrDirectForward.Checked := _prodType.destructionParams.destroyDirectForwardTraces;
          cbxDestrIndirectForward.Enabled := true;
          cbxDestrIndirectForward.Checked := _prodType.destructionParams.destroyIndirectForwardTraces;
          cbxDestrDirectBack.Enabled := true;
          cbxDestrDirectBack.Checked := _prodType.destructionParams.destroyDirectBackTraces;
          cbxDestrIndirectBack.Enabled := true;
          cbxDestrIndirectBack.Checked := _prodType.destructionParams.destroyIndirectBackTraces;
        end
      else
        begin
          cbxDestrDirectForward.Checked := false;
          cbxDestrDirectForward.Enabled := false;
          cbxDestrIndirectForward.Checked := false;
          cbxDestrIndirectForward.Enabled := false;
          cbxDestrDirectBack.Checked := false;
          cbxDestrDirectBack.Enabled := false;
          cbxDestrIndirectBack.Checked := false;
          cbxDestrIndirectBack.Enabled := false;
        end
      ;

      cbxDestrRingTrigger.Checked := _prodType.destructionParams.isRingTrigger;

      cbxDestrRingTarget.Checked := _prodType.destructionParams.isRingTarget;
      cbxDestrRingTarget.Enabled := true;

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

      _loading := false;
      
      updateDisplay();
    end
  ;


  function TFrameDestruction.isValid(): boolean;
    begin
      result := true;

      if( cbxDestrRingTrigger.Checked ) then
        begin
          if( 0 >= uiStrToFloat( rleDestrRingRadius.Text ) ) then
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
