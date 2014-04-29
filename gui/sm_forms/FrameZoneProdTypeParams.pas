unit FrameZoneProdTypeParams;

(*
FrameZoneProdTypeParams.pas/dfm
-------------------------------
Begin: 2007/01/08
Last revision: $Date: 2008/11/25 22:00:31 $ $Author: areeves $
Version number: $Revision: 1.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

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

    REEdit,
    FrameFunctionEditor,
    FrameSMFunctionEditor
  ;

  type TFrameZoneProdTypeParams = class( TFrame )
      pnlDirectMovement: TPanel;
      pnlUseDirectMovementControl: TPanel;
      cbxUseDirectMovementControl: TCheckBox;
      pnlHeader: TPanel;
      lblZoneDescr: TLabel;
      pnlIndirectMovement: TPanel;
      pnlUseIndirectMovementControl: TPanel;
      cbxUseIndirectMovementControl: TCheckBox;
      pnlIndirectMovementParams: TPanel;
      smrIndirectMovement: TFrameSMFunctionEditor;
      lblIndirectMovement: TLabel;
      imgRel2: TImage;
      pnlDirectMovementParams: TPanel;
      lblDirectMovement: TLabel;
      imgRel1: TImage;
      smrDirectMovement: TFrameSMFunctionEditor;
      pnlDetection: TPanel;
      pnlUseDetectionMultiplier: TPanel;
      cbxUseDetectionMultiplier: TCheckBox;
      pnlDetectionParams: TPanel;
      lblDetectionMultiplier: TLabel;
      rleDetectionMultiplier: TREEdit;
      pnlFooter: TPanel;

      procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
      procedure cbxClick(Sender: TObject);

    protected
      _directMovementHeight: integer;
      _indirectMovementHeight: integer;
      _detectionHeight: integer;
      
      procedure translateUI();
      
    public
      cbxProcessClick: procedure( Sender: TObject ) of object;

      constructor create( AOwner: TComponent ); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    DebugWindow,
    RegExpDefs,

    ChartFunction,
    FunctionEnums,
    I88n
  ;

  constructor TFrameZoneProdTypeParams.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      cbxProcessClick := nil;

      smrDirectMovement.chartType := CTRel;
      smrDirectMovement.minY := 0.0;
      smrDirectMovement.maxY := 0.0; // there is no maximum
      smrDirectMovement.xUnits := UnitsDays;
      smrDirectMovement.yUnits := UnitsPercent;
      smrDirectMovement.setChartField( ZONMovementDirect );

      smrIndirectMovement.chartType := CTRel;
      smrIndirectMovement.minY := 0.0;
      smrIndirectMovement.maxY := 0.0; // there is no maximum
      smrIndirectMovement.xUnits := UnitsDays;
      smrIndirectMovement.yUnits := UnitsPercent;
      smrIndirectMovement.setChartField( ZONMovementIndirect );

      rleDetectionMultiplier.InputExpression := RE_DECIMAL_INPUT;

      pnlDirectMovement.BevelOuter := bvNone;
      pnlUseDirectMovementControl.BevelOuter := bvNone;
      pnlHeader.BevelOuter := bvNone;
      pnlIndirectMovement.BevelOuter := bvNone;
      pnlUseIndirectMovementControl.BevelOuter := bvNone;
      pnlIndirectMovementParams.BevelOuter := bvNone;
      pnlDirectMovementParams.BevelOuter := bvNone;
      pnlDetection.BevelOuter := bvNone;
      pnlUseDetectionMultiplier.BevelOuter := bvNone;
      pnlDetectionParams.BevelOuter := bvNone;

      _directMovementHeight := pnlDirectMovement.Height;
      _indirectMovementHeight := pnlIndirectMovement.Height;
      _detectionHeight := pnlDetection.Height;
    end
  ;
  

  procedure TFrameZoneProdTypeParams.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.5.0.
      // Generation date: Thu Feb 21 20:53:29 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/DialogRunSimException.dfm
      // File date: Thu Oct 12 14:33:46 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxUseDirectMovementControl.Caption := tr( 'Alter direct movement rate for this production type in this zone' );
          lblDirectMovement.Caption := tr( 'Effect on baseline direct movement rate from units of this type:' );
          imgRel1.Hint := tr( 'This parameter is a relational function' );
          lblZoneDescr.Caption := tr( 'Zone description' );
          cbxUseIndirectMovementControl.Caption := tr( 'Alter indirect movement rate for this production type in this zone' );
          lblIndirectMovement.Caption := tr( 'Effect on baseline indirect movement rate from units of this type:' );
          imgRel2.Hint := tr( 'This parameter is a relational function' );
          cbxUseDetectionMultiplier.Caption := tr( 'Alter the probability of detection for this production type in this zone' );
          lblDetectionMultiplier.Caption := tr( 'Multiplier for the probability of observing clinical signs: (Note: probability of reporting an observed clinical unit within a zone is 1.)' );
        end
      ;

    end
  ;


  // This function deals with a little bug in TREEdit.
  procedure TFrameZoneProdTypeParams.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
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

  procedure TFrameZoneProdTypeParams.cbxClick( Sender: TObject );
    begin
      if( cbxUseDirectMovementControl.Checked ) then
        pnlDirectMovement.Height := _directMovementHeight
      else
        pnlDirectMovement.Height := pnlUseDirectMovementControl.Height
      ;

      if( cbxUseIndirectMovementControl.Checked ) then
        pnlIndirectMovement.Height := _indirectMovementHeight
      else
        pnlIndirectMovement.Height := pnlUseIndirectMovementControl.Height
      ;

      if( cbxUseDetectionMultiplier.Checked ) then
        pnlDetection.Height := _detectionHeight
      else
        pnlDetection.Height := pnlUseDetectionMultiplier.Height
      ;

      self.Height := pnlHeader.Height + pnlDirectMovement.Height + pnlIndirectMovement.Height + pnlDetection.Height + pnlFooter.height;

      if( nil <> @cbxProcessClick ) then
        cbxProcessClick( sender )
      else
        dbcout( 'cbxProcessClick is nil in TFrameZoneProdTypeParams.cbxClick', true )
      ;
    end
  ;

end.
