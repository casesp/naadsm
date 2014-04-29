unit FrameContactSpreadMaster;

(*
FrameContactSpreadMaster.pas/dfm
--------------------------------
Begin: 2005/06/17
Last revision: $Date: 2009-07-11 00:44:23 $ $Author: areeves $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2009 Animal Population Health Institute, Colorado State University

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

    FrameContactSpread
  ;

  type TFrameContactSpreadMaster = class( TFrame )
      pnlDirectContact: TPanel;
      pnlDirect: TPanel;
      pnlUseDirectContact: TPanel;
      cbxIncludeDirect: TCheckBox;
      pnlIndirectContact: TPanel;
      pnlUseIndirectContact: TPanel;
      cbxIncludeIndirect: TCheckBox;
      pnlIndirect: TPanel;
      pnlDirectParams: TPanel;
      fraDirect: TFrameContactSpread;
      pnlSpacerDirect: TPanel;
      pnlIndirectParams: TPanel;
      pnlSpacerIndirect: TPanel;
      fraIndirect: TFrameContactSpread;

    protected
      procedure translateUI();  
      
    public
      constructor create( AOwner: TComponent ); override;
      function isValid(): boolean;

    end
  ;



implementation

{$R *.dfm}


  uses
    MyStrUtils,
    DebugWindow,
    I88n,

    ContactSpreadParams,

    FormSMWizardBase
  ;


  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit


  constructor TFrameContactSpreadMaster.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    
      fraDirect.setContactType( CMDirect );
      fraIndirect.setContactType( CMIndirect );

      fraDirect.smcDistanceDistr.setForm( AOwner as TFormSMWizardBase );
      fraDirect.smcTransportDelay.setForm( AOwner as TFormSMWizardBase );

      fraIndirect.smcDistanceDistr.setForm( AOwner as TFormSMWizardBase );
      fraIndirect.smcTransportDelay.setForm( AOwner as TFormSMWizardBase );
    end
  ;

  
  procedure TFrameContactSpreadMaster.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameContactSpreadMaster.dfm
      // File date: Fri Jan 19 17:43:55 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxIncludeDirect.Caption := tr( 'Model direct contact spread' );
          cbxIncludeIndirect.Caption := tr( 'Model indirect contact spread' );
        end
      ;

    end
  ;
  
  
  function TFrameContactSpreadMaster.isValid(): boolean;
    begin
      if( pnlDirectParams.Visible ) and ( pnlIndirectParams.Visible ) then
        result := ( fraDirect.isValid() ) and ( fraIndirect.isValid() )
      else if( pnlDirectParams.Visible ) and ( not( pnlIndirectParams.Visible ) ) then
        result := fraDirect.isValid()
      else if( not( pnlDirectParams.Visible ) ) and ( pnlIndirectParams.Visible ) then
        result := fraIndirect.isValid()
      else
        result := true
      ;
    end
  ;

end.
