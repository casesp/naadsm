unit FormZone;

(*
FormZone.pas/dfm
----------------
Begin: 2006/12/19
Last revision: $Date: 2010-09-09 14:34:02 $ $Author: rhupalo $
Version number: $Revision: 1.12.10.1 $
Project: (various)
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard includes
    Windows,
    Messages,
    SysUtils,
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    StdCtrls,
    Buttons,
    ExtCtrls,
    Dialogs,
    Menus,
    ActnPopupCtrl,

    Zone,
    ProductionType,

    // Widgets
    FrameZone,
    FormProdTypeBase,
    FormSMWizardBase
  ;


  type TFormZone = class( TFormProdTypeBase )
      fraParams: TFrameZone;

    protected
      _frameSetupComplete: boolean;

      procedure translateUI();

      function getZoneList(): TZoneList;

      procedure updateDisplay(); override;

      function dataIsValid(): boolean; override;

      procedure giveListsToEditors(); override;
      procedure prepFunctionDicts(); override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType ); override;
      
    public
      constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

      property zoneList: TZoneList read getZoneList;
    end
  ;


  const
    DBFORMZONE: boolean = false; // set to true to enable debugging messages for this unit.


implementation

{$R *.dfm}

  uses
    QStringMaps,
    I88n,

    FunctionEnums,
    FunctionDictionary,
    ProdTypeZoneParams,

    FrameZoneProdTypeParams
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormZone.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      fraParams.Visible := false;
      lblProdType.Caption := '';
      _frameSetupComplete := false;
    end
  ;


  procedure TFormZone.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormZone.dfm
      // File date: Wed Apr 25 11:56:52 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Zone parameters' );
          pnlCaption.Caption := tr( 'Zone parameters' );
        end
      ;

    end
  ;


  destructor TFormZone.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  function TFormZone.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    begin
      // must first detect disease in order to implement zone restrictions
      if (( _smScenarioCopy.simInput.includeZonesGlobal ) and ( _smScenarioCopy.simInput.includeDetectionGlobal )) then
        result := inherited showModal( nextFormToShow, formDisplayed, currentFormIndex )
      else
        begin
          formDisplayed := false;
          nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;


  procedure TFormZone.updateDisplay();
    begin
      if( nil <> _selectedPT ) then
        begin
          lblProdType.Caption := _selectedPT.productionTypeDescr;
          fraParams.Visible := true;
          fraParams.prodType := _selectedPT;
        end
      else
        begin
          fraParams.visible := false;
          lblProdType.Caption := '';
        end
      ;
    end
  ;


  procedure TFormZone.prepFunctionDicts();
    var
      i: integer;
      zptFrame: TFrameZoneProdTypeParams;
      it: TFunctionDictionaryIterator;
    begin
      if( not( _frameSetupComplete ) ) then
        begin
          fraParams.initializeFrameDisplay();
          _frameSetupComplete := true;
        end
      ;

      for i := 0 to fraParams.paramFrameList.Count - 1 do
        begin
          zptFrame := fraParams.paramFrameList.itemAtIndex( i ) as TFrameZoneProdTypeParams;
          zptFrame.smrDirectMovement.clearList();
          zptFrame.smrIndirectMovement.clearList();
        end
      ;

      it := TFunctionDictionaryIterator.create( _fnDict );

      repeat
        if( nil <> it.value() ) then
          begin
            if ( not it.value().removed ) then
              begin
                if
                  ( integer( ZONMovementDirect ) = it.value().fn.dbField )
                or
                  ( integer( ZONMovementIndirect ) = it.value().fn.dbField )
                then
                  begin
                    for i := 0 to fraParams.paramFrameList.Count - 1 do
                      begin
                        zptFrame := fraParams.paramFrameList.itemAtIndex( i ) as TFrameZoneProdTypeParams;
                        zptFrame.smrDirectMovement.appendFunction( it.value().fn );
                        zptFrame.smrIndirectMovement.appendFunction( it.value().fn );
                      end
                    ;
                  end
                ;
              end
            ;
          end
        ;
        it.incr();
      until ( nil = it.value() );

      it.Free();
    end
  ;

  
  procedure TFormZone.giveListsToEditors();
    var
      i: integer;
      zptFrame: TFrameZoneProdTypeParams;
    begin
      for i := 0 to fraParams.paramFrameList.Count - 1 do
        begin
          zptFrame := fraParams.paramFrameList.itemAtIndex( i ) as TFrameZoneProdTypeParams;

          zptFrame.smrDirectMovement.setFunctionDict( _fnDict );
          zptFrame.smrIndirectMovement.setFunctionDict( _fnDict );

          zptFrame.smrDirectMovement.setModelList( _ptList );
          zptFrame.smrIndirectMovement.setModelList( _ptList );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  function TFormZone.dataIsValid(): boolean;
    begin
      result := fraParams.isValid();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFormZone.getZoneList(): TZoneList;
    begin
      if( nil = self._smScenarioCopy ) then
        result := nil
      else
        result := self._smScenarioCopy.simInput.zoneList
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Copying parameters
//-----------------------------------------------------------------------------
  procedure TFormZone.copyParameters( const src: TProductionType; dest: TProductionType );
    var
      it: TZPTListIterator;
      zptDest, zptSrc: TZoneProdTypeComboParams;
    begin
      dest.zoneParams.detectionIsZoneTrigger := src.zoneParams.detectionIsZoneTrigger;
      dest.zoneParams.directTraceIsZoneTrigger := src.zoneParams.directTraceIsZoneTrigger;
      dest.zoneParams.indirectTraceIsZoneTrigger := src.zoneParams.indirectTraceIsZoneTrigger;

      it := TZPTListIterator.create( src.zoneParams.zonePtParamsList );

      while( nil <> it.value() ) do
        begin
          zptSrc := it.value();
          zptDest := dest.zoneParams.zonePtParamsList[ it.key()] ;

          zptDest.useDetectionMultiplier := zptSrc.useDetectionMultiplier;
          zptDest.detectionMultiplier := zptSrc.detectionMultiplier;

          zptDest.useDirectMovementControl := zptSrc.useDirectMovementControl;
          zptDest.setChart( ZONMovementDirect, zptSrc.chart( ZONMovementDirect ) );
          zptDest.useIndirectMovementControl := zptSrc.useIndirectMovementControl;
          zptDest.setChart( ZONMovementIndirect, zptSrc.chart( ZONMovementIndirect ) );

          it.incr();
        end
      ;

      it.free();

      dest.updated := true;
    end
  ;
//-----------------------------------------------------------------------------


initialization

  RegisterClass( TFormZone );

end.
