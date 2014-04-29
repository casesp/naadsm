unit FrameCostsZoneProdTypeParams;

(*
FrameCostsZoneProdTypeParams.pas/dfm
------------------------------------
Begin: 2007/04/18
Last revision: $Date: 2008/03/12 22:10:50 $ $Author: areeves $
Version number: $Revision: 1.3 $
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

    REEdit
  ;

  type TFrameCostsZoneProdTypeParams = class( TFrame )
      pnlHeader: TPanel;
      lblZoneDescr: TLabel;
      pnlCostParams: TPanel;
      lblCostPerAnimalDay: TLabel;
      lblDollar: TLabel;
      rleCostPerAnimalDay: TREEdit;

      procedure rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
    
    protected
      procedure translateUI();
      
    public
      constructor create( AOwner: TComponent ); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    RegExpDefs,
    I88n
  ;

  constructor TFrameCostsZoneProdTypeParams.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      rleCostPerAnimalDay.InputExpression := RE_DOLLAR_INPUT;
    end
  ;


  procedure TFrameCostsZoneProdTypeParams.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:53 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameCostsZoneProdTypeParams.dfm
      // File date: Wed Apr 25 11:56:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblZoneDescr.Caption := tr( 'Zone description' );
          lblCostPerAnimalDay.Caption := tr( 'Cost of surveillance per animal per day:' );
          lblDollar.Caption := tr( '$' );
        end
      ;

    end
  ;



  // This function deals with a little bug in TREEdit.
  procedure TFrameCostsZoneProdTypeParams.rleKeyDown( Sender: TObject; var Key: Word; Shift: TShiftState );
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

end.
