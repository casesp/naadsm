unit FrameZone;

(*
FrameZone.pas/dfm
-----------------
Begin: 2006/12/19
Last revision: $Date: 2008/11/25 22:00:31 $ $Author: areeves $
Version number: $Revision: 1.7 $
Project: NAADSM
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
    ExtCtrls,

    QIntegerMaps,

    ProductionType
	;


	type TFrameZone = class( TFrame )
      pnlZoneTrigger: TPanel;
      lblZoneCreation: TLabel;
      cbxIndirectTraceTrigger: TCheckBox;
      cbxDirectTraceTrigger: TCheckBox;
      cbxDetectionTrigger: TCheckBox;
      pnlZoneEffects: TPanel;
      pnlZoneEffectsBody: TPanel;

      procedure processTriggerClick( Sender: TObject );
      procedure processTextEntry( Sender: TObject );
      procedure processEffectClick( Sender: TObject );

		protected
    	// properties
      _prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

      procedure translateUI();

      procedure updateDisplay();

      // properties
      procedure setProdType( val: TProductionType );
      function getProdType(): TProductionType;

		public
      paramFrameList: TQIntegerObjectMap;

      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function isValid(): boolean;

      procedure initializeFrameDisplay();

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
    DebugWindow,
    I88n,

    FunctionEnums,
    Zone,
    ZoneParams,

    FormSMWizardBase,
    FormZone,
    FrameZoneProdTypeParams
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFrameZone.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _myParent := AOwner as TWinControl;
      
      paramFrameList := TQIntegerObjectMap.Create();

      pnlZoneEffectsBody.BevelOuter := bvNone;

      _loading := false;
    end
   ;


  procedure TFrameZone.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameZone.dfm
      // File date: Wed Apr 25 11:56:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblZoneCreation.Caption := tr( 'Creating (triggering) zones' );
          cbxIndirectTraceTrigger.Caption := tr( 'Indirect tracing of units of this type triggers a zone focus' );
          cbxDirectTraceTrigger.Caption := tr( 'Direct tracing of units of this type triggers a zone focus' );
          cbxDetectionTrigger.Caption := tr( 'Detection of infected units of this type triggers a  zone focus' );
        end
      ;

    end
  ;

   
   destructor TFrameZone.destroy();
    begin
      // DO NOT delete the items stored in paramFrameList: the list does not own the objects.
      // They will be freed when the form is closed.
      freeAndNil( paramFrameList );
      inherited destroy();
    end
   ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI events
//-----------------------------------------------------------------------------
  procedure TFrameZone.processTriggerClick( Sender: TObject );
    begin
      if( not _loading ) then
        begin
          _prodType.zoneParams.detectionIsZoneTrigger := cbxDetectionTrigger.Checked;
          _prodType.zoneParams.directTraceIsZoneTrigger := cbxDirectTraceTrigger.Checked;
          _prodType.zoneParams.indirectTraceIsZoneTrigger := cbxIndirectTraceTrigger.checked;
          _prodType.updated := true;

          (_myParent as TFormSMWizardBase).showStar();
        end
      ;

      updateDisplay();
    end
  ;


  procedure TFrameZone.processTextEntry( Sender: TObject );
    var
      zpt: TZoneProdTypeParams;
      zptFrame: TFrameZoneProdTypeParams;
      it: TZPTListIterator;
    begin
      if( not( _loading ) ) then
        begin
          it := TZPTListIterator.create( _prodType.zoneParams.zonePtParamsList );

          repeat
            if( nil <> it.value() ) then
              begin
                zptFrame := paramFrameList.value( it.key() ) as TFrameZoneProdTypeParams;
                zpt := it.value() as TZoneProdTypeParams;

                zpt.detectionMultiplier := myStrToFloat( zptFrame.rleDetectionMultiplier.Text, -1.0 );

                (_myParent as TFormSMWizardBase).showStar();
              end
            ;
            it.incr();
          until ( nil = it.value() );

          it.Free();
        end
      ;
    end
  ;


  procedure TFrameZone.processEffectClick( Sender: TObject );
    var
      zpt: TZoneProdTypeParams;
      zptFrame: TFrameZoneProdTypeParams;
      it: TZPTListIterator;

      newHt: integer;
    begin
      it := TZPTListIterator.create( _prodType.zoneParams.zonePtParamsList );
      newHt := 0;

      repeat
        if( nil <> it.value() ) then
          begin
            zptFrame := paramFrameList.value( it.key() ) as TFrameZoneProdTypeParams;

            if( not( _loading ) ) then
              begin
                zpt := it.value() as TZoneProdTypeParams;

                zpt.useDirectMovementControl := zptFrame.cbxUseDirectMovementControl.Checked;
                zpt.useIndirectMovementControl := zptFrame.cbxUseIndirectMovementControl.Checked;
                zpt.useDetectionMultiplier := zptFrame.cbxUseDetectionMultiplier.Checked;

                (_myParent as TFormSMWizardBase).showStar();
              end
            ;

            inc( newHt, zptFrame.Height );
          end
        ;
        it.incr();
      until ( nil = it.value() );

      pnlZoneEffects.Height := newHt;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameZone.initializeFrameDisplay();
    var
      frm: TFormZone;
      i: integer;
      zptFrame: TFrameZoneProdTypeParams;
    begin
      frm := _myParent as TFormZone;

      if( nil = frm.zoneList ) then
        raise exception.Create( 'frm.zoneList is nil in TFrameZone.initializeFrameDisplay' )
      ;

      paramFrameList.Clear();

      for i := 0 to frm.zoneList.count - 1 do
        begin
          zptFrame := TFrameZoneProdTypeParams.create( self );
          zptFrame.Name := 'paramFrame' + intToStr( i );

          zptFrame.smrDirectMovement.setForm( frm );
          zptFrame.smrDirectMovement.additionalInfo := frm.zoneList.at(i).id;

          zptFrame.smrIndirectMovement.setForm( frm );
          zptFrame.smrIndirectMovement.additionalInfo := frm.zoneList.at(i).id;

          zptFrame.lblZoneDescr.Caption := tr( 'Effect of zone' ) + ' "' +  frm.zoneList.at(i).descr + '"';
          zptFrame.Parent := pnlZoneEffectsBody;
          zptFrame.Align := alTop;

          // Set the appropriate function pointers.
          zptFrame.rleDetectionMultiplier.OnExit := processTextEntry;
          zptFrame.cbxProcessClick := processEffectClick;


          paramFrameList.insert( frm.zoneList.at(i).id, zptFrame );
        end
      ;

      self.Height := pnlZoneEffects.Height + pnlZoneTrigger.Height;
    end
  ;


	procedure TFrameZone.updateDisplay();
  	begin
      //pnlCostParams.Visible := cbxUseCosts.Checked;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameZone.isValid(): boolean;
    begin
      // There isn't actually anything that needs to be validated here.
      result := true;
    end
  ;
  

  function TFrameZone.getProdType(): TProductionType;
  	begin
    	result := _prodType;
    end
  ;


  procedure TFrameZone.setProdType( val: TProductionType );
    var
      zpt: TZoneProdTypeParams;
      zptFrame: TFrameZoneProdTypeParams;
      it: TZPTListIterator;
  	begin
    	_loading := true;

    	_prodType := val;

      cbxDetectionTrigger.Checked := _prodType.zoneParams.detectionIsZoneTrigger;
      cbxDirectTraceTrigger.Checked := _prodType.zoneParams.directTraceIsZoneTrigger;
      cbxIndirectTraceTrigger.Checked := _prodType.zoneParams.indirectTraceIsZoneTrigger;

      it := TZPTListIterator.create( _prodType.zoneParams.zonePtParamsList );

      repeat
        if( nil <> it.value() ) then
          begin
            zptFrame := paramFrameList.value( it.key() ) as TFrameZoneProdTypeParams;

            zpt := it.value() as TZoneProdTypeParams;

            zptFrame.cbxUseDirectMovementControl.Checked := zpt.useDirectMovementControl;
            zptFrame.cbxUseIndirectMovementControl.Checked := zpt.useIndirectMovementControl;
            zptFrame.cbxUseDetectionMultiplier.Checked := zpt.useDetectionMultiplier;

            zptFrame.cbxClick( nil ); // This function call forces the frame to recalculate and report its height.

            zptFrame.smrDirectMovement.showChart( _prodType.zoneParams, zpt.directMovement, ZONMovementDirect );
            zptFrame.smrIndirectMovement.showChart( _prodType.zoneParams, zpt.indirectMovement, ZONMovementIndirect );

            if( 0.0 <= zpt.detectionMultiplier ) then
              zptFrame.rleDetectionMultiplier.Text := uiFloatToStr( zpt.detectionMultiplier )
            else
              zptFrame.rleDetectionMultiplier.Text := ''
            ;
          end
        ;
        it.incr();
      until ( nil = it.value() );

      it.Free();


      _loading := false;

      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------
end.
