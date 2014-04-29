unit FrameSingleEpiCurve;

(*
FrameSingleEpiCurve.pas/dfm
---------------------------
Begin: 2005/12/13
Last revision: $Date: 2008-03-12 22:10:51 $ $Author: areeves $
Version number: $Revision: 1.6 $
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
    TeEngine,
    Series,
    ExtCtrls,
    TeeProcs,
    Chart,

    FrameChartBase
  ;

  
  type TFrameSingleEpiCurve = class( TFrameChartBase )
      chtCurve: TChart;
      series: TBarSeries;

    protected
      procedure translateUI();
      procedure translateUIManual();

      procedure adjustForChartBug();

    public
      constructor create( AOwner: TComponent ); override;
    
      procedure clearSeries();
      procedure addXY( const x, y: double );

    end
  ;


implementation

{$R *.dfm}

  uses
    I88n
  ;

  constructor TFrameSingleEpiCurve.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    end
  ;


  procedure TFrameSingleEpiCurve.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 17:08:32 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameSingleEpiCurve.dfm
      // File date: Tue Dec 13 13:24:28 2005

      // Caption Collector found no text properties in this form.

      // Set TChart properties
      with self do
        begin
          chtCurve.BottomAxis.Title.Caption := tr( 'Days since start of iteration' );
          chtCurve.LeftAxis.Title.Caption := tr( 'Number of case-units' );
          chtCurve.Title.Text.Strings[0] := tr( 'Apparent Epidemic Curve' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      // Otherwise, this function will be empty:
      translateUIManual();
    end
  ;


  procedure TFrameSingleEpiCurve.translateUIManual();
    begin
    end
  ;


  procedure TFrameSingleEpiCurve.clearSeries();
    begin
      series.Clear();
    end
  ;


  procedure TFrameSingleEpiCurve.addXY( const x, y: double );
    begin
      series.AddXY( x, y );

      adjustForChartBug();
    end
  ;


  procedure TFrameSingleEpiCurve.adjustForChartBug();
    begin
      // The following IF statements correct for a known bug in TChart
      // See http://www.teechart.net/support/modules.php?name=Newsgroups&file=article&id=924&group=steema.public.teechart6.delphi
      if( series.YValues.MaxValue = series.YValues.MinValue ) then
        series.GetVertAxis.SetMinMax( series.YValues.MinValue - 1.0, series.YValues.MinValue + 1.0 )
      else
        series.GetVertAxis.Automatic := true
      ;

      if( series.XValues.MaxValue = series.XValues.MinValue ) then
        series.GetHorizAxis.SetMinMax( series.XValues.MinValue - 1.0, series.XValues.MinValue + 1.0 )
      else
        series.GetHorizAxis.Automatic := true
      ;
    end
  ;


end.
