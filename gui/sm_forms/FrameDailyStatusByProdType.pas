unit FrameDailyStatusByProdType;

(*
FrameDailyStatusByProdType.pas/dfm
----------------------------------
Begin: 2005/12/13
Last revision: $Date: 2008/10/17 19:20:47 $ $Author: areeves $
Version number: $Revision: 1.10 $
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
    Series, 
    TeEngine, 
    TeeProcs, 
    Chart, 
    StdCtrls,
    ExtCtrls,

    FrameChartBase
    ;

  type TFrameDailyStatusByProdType = class(TFrameChartBase)
      pnlChartOptions: TPanel;
      cbxThreeD: TCheckBox;
      chtOutputs: TChart;
      SeriesPDetection: TPointSeries;
      SeriesPDestroy: TPointSeries;
      SeriesPVaccination: TPointSeries;
      SeriesPOver: TPointSeries;
      SeriesSusc: TLineSeries;
      SeriesLatent: TLineSeries;
      SeriesSubClinical: TLineSeries;
      SeriesClinical: TLineSeries;
      SeriesNatImmune: TLineSeries;
      SeriesVacImmune: TLineSeries;
      SeriesDestroyed: TLineSeries;
      SeriesDetected: TLineSeries;
      SeriesTraceDir: TLineSeries;
      SeriesTraceInd: TLineSeries;
      SeriesVaccinated: TLineSeries;
    
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
  
  constructor TFrameDailyStatusByProdType.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    end
  ;


  procedure TFrameDailyStatusByProdType.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 20:53:19 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FrameDailyStatusByProdType.dfm
      // File date: Mon Jan 23 16:02:40 2006

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
          chtOutputs.LeftAxis.Title.Caption := tr( 'Proportion of units X 100%' );
          chtOutputs.Title.Text.Strings[0] := tr( '' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFrameDailyStatusByProdType.translateUIManual();
    begin
      with self.chtOutputs do
        begin
          Series[0].Title := tr( 'Susceptible' );
          Series[1].Title := tr( 'Latent' );
          Series[2].Title := tr( 'Subclinical' );
          Series[3].Title := tr( 'Clinical' );
          Series[4].Title := tr( 'NatImmune' );
          Series[5].Title := tr( 'VacImmune' );
          Series[6].Title := tr( 'Destroyed' );
          Series[7].Title := tr( 'Detected' );
          Series[8].Title := tr( 'Traced - Direct' );
          Series[9].Title := tr( 'Traced - Indirect' );
          Series[10].Title := tr( 'Vaccinated' );
          Series[11].Title := tr( 'First detection' );
          Series[12].Title := tr( 'First destruction' );
          Series[13].Title := tr( 'First vaccination' );
          Series[14].Title := tr( 'Outbreak over' );
        end
      ;
    end
  ;

end.
