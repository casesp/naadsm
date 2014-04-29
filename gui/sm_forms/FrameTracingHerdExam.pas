unit FrameTracingHerdExam;

(*
FrameTracingHerdExam.pas
------------------------
Begin: 2008/04/22
Last revision: $Date: 2011-03-28 22:01:52 $ $Author: areeves $
Version number: $Revision: 1.5.4.3 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2008 - 2011 Animal Population Health Institute, Colorado State University

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


  type TFrameTracingHerdExam = class( TFrame )
      cbxForwardDirectExam: TCheckBox;
      cbxForwardIndirectExam: TCheckBox;
      cbxBackDirectExam: TCheckBox;
      cbxBackIndirectExam: TCheckBox;
      rleForwardDirectMultiplier: TREEdit;
      lblForwardDirectMultiplier: TLabel;
      lblForwardIndirectMultiplier: TLabel;
      rleForwardIndirectMultiplier: TREEdit;
      lblBackDirectMultiplier: TLabel;
      rleBackDirectMultiplier: TREEdit;
      lblBackIndirectMultiplier: TLabel;
      rleBackIndirectMultiplier: TREEdit;

      procedure processClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );

    protected
    	// properties
    	_prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      procedure translateUI();
      procedure translateUIManual();
      
      // Display
      procedure updateDisplay();
      procedure updateParamDisplay( cbx: TCheckBox; lbl: TLabel; rle: TREEdit; const val: double );

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
    MyDialogs,
    I88n,

    ChartFunction,
    ProductionTypeList,

    FormSMWizardBase
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameTracingHerdExam.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      
      _prodType := nil;

      rleForwardDirectMultiplier.InputExpression := RE_DECIMAL_INPUT;
      rleForwardIndirectMultiplier.InputExpression := RE_DECIMAL_INPUT;
      rleBackDirectMultiplier.InputExpression := RE_DECIMAL_INPUT;
      rleBackIndirectMultiplier.InputExpression := RE_DECIMAL_INPUT;

      _loading := false;
    end
  ;


  procedure TFrameTracingHerdExam.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FrameTracingHerdExam.dfm
      // File date: Mon Apr 28 16:02:18 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblForwardDirectMultiplier.Caption := tr( 'Multiplier for the probability of observing clinical signs:' );
          lblForwardIndirectMultiplier.Caption := tr( 'Multiplier for the probability of observing clinical signs:' );
          lblBackDirectMultiplier.Caption := tr( 'Multiplier for the probability of observing clinical signs:' );
          lblBackIndirectMultiplier.Caption := tr( 'Multiplier for the probability of observing clinical signs:' );
          cbxForwardDirectExam.Caption := tr( 'Examine units identified by TRACE-FORWARD of DIRECT contact for clinical signs of disease' );
          cbxForwardIndirectExam.Caption := tr( 'Examine units identified by TRACE-FORWARD of INDIRECT contact for clinical signs of disease' );
          cbxBackDirectExam.Caption := tr( 'Examine units identified by TRACE-BACK of DIRECT contact for clinical signs of disease' );
          cbxBackIndirectExam.Caption := tr( 'Examine units identified by TRACE-BACK of INDIRECT contact for clinical signs of disease' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameTracingHerdExam.translateUIManual();
    begin
    end
  ;
    

  destructor TFrameTracingHerdExam.destroy();
  	begin
    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameTracingHerdExam.processClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
          _prodType.updated := true;

          if( cbxForwardDirectExam.Enabled ) then
            _prodType.tracingParams.examDirectForward := cbxForwardDirectExam.Checked
          ;
          if( cbxForwardIndirectExam.Enabled ) then
            _prodType.tracingParams.examIndirectForward := cbxForwardIndirectExam.Checked
          ;
          if( cbxBackDirectExam.Enabled ) then
            _prodType.tracingParams.examDirectBack := cbxBackDirectExam.Checked
          ;
          if( cbxBackIndirectExam.Enabled ) then
            _prodType.tracingParams.examIndirectBack := cbxBackIndirectExam.Checked
          ;

          updateDisplay();
        end
      ;
    end
  ;


  procedure TFrameTracingHerdExam.processTextEntry( Sender: TObject );
  	begin
      _prodType.tracingParams.examDirectForwardMultiplier := uiStrToFloat( rleForwardDirectMultiplier.Text, -1.0 );
      _prodType.tracingParams.examIndirectForwardMultiplier := uiStrToFloat( rleForwardIndirectMultiplier.Text, -1.0 );
      _prodType.tracingParams.examDirectBackMultiplier := uiStrToFloat( rleBackDirectMultiplier.Text, -1.0 );
      _prodType.tracingParams.examIndirectBackMultiplier := uiStrToFloat( rleBackIndirectMultiplier.Text, -1.0 );

   		_prodType.updated := true;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameTracingHerdExam.updateParamDisplay( cbx: TCheckBox; lbl: TLabel; rle: TREEdit; const val: double );
    begin
      lbl.Enabled := cbx.Checked;
      rle.Enabled := cbx.Checked;

      if( cbx.Checked and ( 0.0 <= val ) ) then
        rle.Text := uiFloatToStr( val )
      else
        rle.Text := ''
      ;
    end
  ;


  procedure TFrameTracingHerdExam.updateDisplay();
  	begin
      updateParamDisplay( cbxForwardDirectExam, lblForwardDirectMultiplier, rleForwardDirectMultiplier, _prodType.tracingParams.examDirectForwardMultiplier );
      updateParamDisplay( cbxForwardIndirectExam, lblForwardIndirectMultiplier, rleForwardIndirectMultiplier, _prodType.tracingParams.examIndirectForwardMultiplier );
      updateParamDisplay( cbxBackDirectExam, lblBackDirectMultiplier, rleBackDirectMultiplier, _prodType.tracingParams.examDirectBackMultiplier );
      updateParamDisplay( cbxBackIndirectExam, lblBackIndirectMultiplier, rleBackIndirectMultiplier, _prodType.tracingParams.examIndirectBackMultiplier );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameTracingHerdExam.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameTracingHerdExam.setProdType( val: TProductionType );
  	begin
    	_loading := true;

    	_prodType := val;

      cbxForwardDirectExam.Checked := _prodType.tracingParams.examDirectForward;
      cbxForwardIndirectExam.Checked := _prodType.tracingParams.examIndirectForward;
      cbxBackDirectExam.Checked := _prodType.tracingParams.examDirectBack;
      cbxBackIndirectExam.Checked := _prodType.tracingParams.examIndirectBack;

      // Text entries are updated by the updateDisplay() function, called below.

      _loading := false;

      updateDisplay();
    end
  ;

  function TFrameTracingHerdExam.isValid(): boolean;
    var
      pt: TProductionType;
      ptIt: TProductionTypeListIterator;
      showDlg: boolean;
      response: integer;
    begin
      showDlg := false;

      ptIt := TProductionTypeListIterator.create( _prodType.ptList as TProductionTypeList );

      ptIt.toFirst();
      while( nil <> ptIt.current() ) do
        begin
          pt := ptIt.current();

          if( pt.tracingParams.useTracingExam ) then
            begin
              showDlg := (
                ( pt.tracingParams.examDirectForward and ( 1.0 > pt.tracingParams.examDirectForwardMultiplier ) )
              or
                ( pt.tracingParams.examIndirectForward and ( 1.0 > pt.tracingParams.examIndirectForwardMultiplier ) )
              or
                ( pt.tracingParams.examDirectBack and ( 1.0 > pt.tracingParams.examDirectBackMultiplier ) )
              or
                ( pt.tracingParams.examIndirectBack and ( 1.0 > pt.tracingParams.examIndirectBackMultiplier ) )
              );

              if( showDlg ) then
                break
              ;
            end
          ;

          ptIt.incr();
        end
      ;
      ptIt.Free();

      if( showDlg ) then
        begin
          response := msgYesNo(
            tr( 'The herd exam multiplier for one or more production types is set to a value less than 1.' )
              + '  ' + tr( 'This will reduce the probability of detection compared to detection without a herd exam.' )
              + '  ' + tr( 'Is this what you want?' ),
            tr( 'Unusual detection mulitiplier found' ),
            IMGQuestion,
            _myParent
          );

          if( mrYes = response ) then
            result := true
          else
            result := false
          ;
        end
      else
        result := true
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
