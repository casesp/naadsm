unit FrameCostsZones;

(*
FrameCostsZones.pas/dfm
-----------------------
Begin: 2007/04/18
Last revision: $Date: 2009-08-17 17:38:17 $ $Author: areeves $
Version number: $Revision: 1.9 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2007 - 2009 Animal Population Health Institute, Colorado State University

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


  type TFrameCostsZones = class( TFrame )
      pnlZoneEffects: TPanel;
      pnlZoneEffectsBody: TPanel;

      procedure processTextEntry( Sender: TObject );

    protected
      // properties
      _prodType: TProductionType;

      // for internal use
      _loading: boolean;
      _myParent: TWinControl;

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
    DebugWindow,
    I88n,

    FunctionEnums,
    Zone,
    ProdTypeZoneParams,

    FormSMWizardBase,
    FormCostsZones,
    FrameCostsZoneProdTypeParams
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFrameCostsZones.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      
      _myParent := AOwner as TWinControl;
      
      paramFrameList := TQIntegerObjectMap.Create();

      pnlZoneEffectsBody.BevelOuter := bvNone;

      _loading := false;
    end
   ;


   destructor TFrameCostsZones.destroy();
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
  procedure TFrameCostsZones.processTextEntry( Sender: TObject );
    var
      zpt: TZoneProdTypeComboParams;
      zptFrame: TFrameCostsZoneProdTypeParams;
      it: TZPTListIterator;
    begin
      if( not( _loading ) ) then
        begin
          it := TZPTListIterator.create( _prodType.zoneParams.zonePtParamsList );

          repeat
            if( nil <> it.value() ) then
              begin
                zptFrame := paramFrameList.value( it.key() ) as TFrameCostsZoneProdTypeParams;
                zpt := it.value();

                zpt.costSurvPerAnimalDay := uiStrToFloat( zptFrame.rleCostPerAnimalDay.Text, -1.0 );

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
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display
//-----------------------------------------------------------------------------
  procedure TFrameCostsZones.initializeFrameDisplay();
    var
      frm: TFormCostsZones;
      i: integer;
      zptFrameHeight: integer;
      zptFrame: TFrameCostsZoneProdTypeParams;
    begin
      // How high is a frame?
      zptFrame := TFrameCostsZoneProdTypeParams.create( nil );
      zptFrameHeight := zptFrame.Height;
      freeAndNil( zptFrame );

      frm := _myParent as TFormCostsZones;

      if( nil = frm.zoneList ) then
        raise exception.Create( 'frm.zoneList is nil in TFrameCostsZones.initializeFrameDisplay' )
      ;

      paramFrameList.Clear();

      for i := 0 to frm.zoneList.count - 1 do
        begin
          zptFrame := TFrameCostsZoneProdTypeParams.create( self );
          zptFrame.Name := 'paramFrame' + intToStr( i );

          zptFrame.lblZoneDescr.Caption := tr( 'Costs associated with zone' ) + ' "' +  frm.zoneList.at(i).descr + '"';
          zptFrame.Parent := pnlZoneEffectsBody;
          zptFrame.Align := alTop;

          // Set the appropriate function pointers.
          zptFrame.rleCostPerAnimalDay.OnExit := processTextEntry;

          paramFrameList.insert( frm.zoneList.at(i).id, zptFrame );
        end
      ;

      pnlZoneEffects.Height := frm.zoneList.count * zptFrameHeight;
      self.Height := pnlZoneEffects.Height + 10;
    end
  ;


  procedure TFrameCostsZones.updateDisplay();
    begin
      // Do nothing, for now
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFrameCostsZones.isValid(): boolean;
    begin
      // There isn't actually anything that needs to be validated here.
      result := true;
    end
  ;
  

  function TFrameCostsZones.getProdType(): TProductionType;
    begin
      result := _prodType;
    end
  ;


  procedure TFrameCostsZones.setProdType( val: TProductionType );
    var
      zpt: TZoneProdTypeComboParams;
      zptFrame: TFrameCostsZoneProdTypeParams;
      it: TZPTListIterator;
    begin
      _loading := true;

      _prodType := val;

      it := TZPTListIterator.create( _prodType.zoneParams.zonePtParamsList );

      repeat
        if( nil <> it.value() ) then
          begin
            zptFrame := paramFrameList.value( it.key() ) as TFrameCostsZoneProdTypeParams;

            zpt := it.value();

            if( 0.0 <= zpt.costSurvPerAnimalDay ) then
              zptFrame.rleCostPerAnimalDay.Text := uiFloatToStrZeroPadded( zpt.costSurvPerAnimalDay, 2 )
            else
              zptFrame.rleCostPerAnimalDay.Text := ''
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
