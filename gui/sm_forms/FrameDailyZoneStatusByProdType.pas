unit FrameDailyZoneStatusByProdType;

(*
FrameDailyZoneStatusByProdType.pas/dfm
--------------------------------------
Begin: 2005/12/13
Last revision: $Date: 2008-03-12 22:10:50 $ $Author: areeves $
Version number: $Revision: 1.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Shaun Case <Shaun.Case@colostate.edu>
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
    Series,
    TeEngine,
    TeeProcs,
    Chart,
    StdCtrls,
    ExtCtrls,
    TeeFunci,

    FrameChartBase
  ;


  type TFrameDailyZoneStatusByProdType = class(TFrameChartBase)
      pnlChartOptions: TPanel;
      cbxThreeD: TCheckBox;
      chtOutputs: TChart;
      
    protected
      procedure translateUI();
      procedure translateUIManual();
      
    public
      constructor create( AOwner: TComponent ); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    I88n
  ;


  constructor TFrameDailyZoneStatusByProdType.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    end
   ;


  procedure TFrameDailyZoneStatusByProdType.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 17:07:14 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameDailyZoneStatusByProdType.dfm
      // File date: Wed Apr 25 11:56:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          cbxThreeD.Caption := tr( '3-D View' );
        end
      ;

      // Set TChart properties
      with self do
        begin
          chtOutputs.BottomAxis.Title.Caption := tr( 'Day of simulation' );
          chtOutputs.LeftAxis.Title.Caption := tr( 'Y axis' );
          chtOutputs.Title.Text.Strings[0] := tr( '' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      // Otherwise, this function will be empty:
      translateUIManual();
    end
  ;


  procedure TFrameDailyZoneStatusByProdType.translateUIManual();
    begin
    end
  ;

end.
