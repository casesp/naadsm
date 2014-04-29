unit FrameSummaryEpiCurvesDensity;

(*
FrameSummaryEpiCurvesDensity.pas/dfm
-----------------------------
Begin: 2008/01/15
Last revision: $Date: 2008/03/12 21:19:15 $ $Author: areeves $
Version number: $Revision: 1.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Shaun Case <Shaun.Case@colostate.edu>
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
    TeeProcs,
    Chart,
    StdCtrls,
    ExtCtrls,

    SMEpiCurves,
    ProductionType,

    FrameSummaryEpiCurvesDensityPlot,
    FrameChartBase
  ;


  type TFrameSummaryEpiCurvesDensity = class( TFrameChartBase )
      _DensityPlot: TFrameSummaryEpiCurvesDensityPlot;

    protected
      _graph: TFrameSummaryEpiCurvesDensityPlot;

      function getChartWidth(): integer; override;
      function getChartHeight(): integer; override;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

    	function createMetafile(): TMetaFile; override;
      function printChart(): boolean; override;
  end
;


implementation

{$R *.dfm}

  uses
    I88n
  ;

  constructor TFrameSummaryEpiCurvesDensity.create( AOwner: TComponent );
    var
      chartCount: integer;

      procedure lookForGraph( cntnr: TWinControl );
        var
          i: integer;
        begin
          for i := 0 to cntnr.ControlCount - 1 do
            begin
              if( cntnr.Controls[i] is TFrameSummaryEpiCurvesDensityPlot ) then
                begin
                  inc( chartCount );
                  _graph := cntnr.Controls[i] as TFrameSummaryEpiCurvesDensityPlot;
                end
              ;

              if( cntnr.Controls[i] is TWinControl ) then
                begin
                  if( 0 < (cntnr.Controls[i] as TWinControl).controlCount ) then
                    lookForGraph( cntnr.Controls[i] as TWinControl )
                  ;
                end
              ;
            end
          ;
        end
      ;
		begin
      _ignoreCharts := true;

      inherited create( AOwner );

      with _DensityPlot do
        begin
          bgColor := clBtnFace;
          fitToWindow := True;
          graphColor := clWhite;
          keepAspect := False;
          outlineColor := 8421568;
          textColor := clBlack;
          tickSpacing := 0;
          useSeparatorLines := False;
          useTooltips := False;
          XAxisText := ' ' + tr( 'X-Axis' ) + ' ';
          YAxisText := ' ' + tr( 'Y-Axis' ) + ' ';
          XDivisor := 1;
          YDivisor := 1;
          useColorPlot := True;
        end
      ;

      chartCount := 0;
			_graph := nil;

      lookForGraph( self );

      if( 1 <> chartCount ) then
        begin
          raise exception.Create( 'Wrong number of main graphs (' + intToStr(chartCount) + ') in TFrameDensityBase' );
          _graph := nil;
        end
      ;
    end
  ;

  destructor TFrameSummaryEpiCurvesDensity.destroy();
    begin
      inherited destroy();
    end
  ;


 	function TFrameSummaryEpiCurvesDensity.createMetafile(): TMetaFile;
    var
      ret_val: TMetaFile;
      MFCanvas: TMetaFileCanvas;
      BMP: TBitmap;
    begin
      ret_val := nil;

//      dbcout( 'Not implemented createMetafile() in TFrameDensityBase', true );
      if ( _graph <> nil ) then
        begin
          ret_val := TMetaFile.Create();
          BMP := _graph.pbxDraw.Picture.Bitmap;

          ret_val.Height := BMP.Height;
          ret_val.Width  := BMP.Width;

          MFCanvas := TMetafileCanvas.Create(ret_val, 0);
          MFCanvas.Draw(0, 0, BMP);
          MFCanvas.Free;
        end;
      result := ret_val;
    end
  ;

  function TFrameSummaryEpiCurvesDensity.getChartWidth(): integer;
    begin
      if( nil  <> _graph ) then
        result := _graph.Width
      else
        result := -1
      ;
    end
  ;

  function TFrameSummaryEpiCurvesDensity.getChartHeight(): integer;
    begin
      if( nil  <> _graph ) then
        result := _graph.Height
      else
        result := -1
      ;
    end
  ;

    function TFrameSummaryEpiCurvesDensity.printChart(): boolean;
      begin
      try
        try
          Screen.Cursor := crHourGlass;
//          _graph.PrintLandscape();
          result := true;
        except
          result := false;
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end
  ;

end.
