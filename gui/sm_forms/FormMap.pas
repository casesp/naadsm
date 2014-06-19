unit FormMap;

(*
FormMap.pas/dfm
---------------
Begin: 2005/05/25
Last revision: $Date: 2013-06-27 19:11:27 $ $Author: areeves $
Version: $Revision: 1.58.4.13 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
Author: Snehal Shetye <snehal@goku.engr.colostate.edu>
------------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    Windows,
    Messages,
    SysUtils,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    StdCtrls,
    Buttons,
    ToolWin,
    ActnMan,
    ActnCtrls,
    ActnMenus,
    ActnList,
    XPStyleActnCtrls,

    Points,
    SparseArrays,

    Herd,
    StatusEnums,
    NAADSMLibraryTypes,
    ZonePerimeter,
    SMSimulationInput,
    SMDatabase,

    MdHintWn,
    Zone
  ;


  type TMapDisplay = (
    MAPUnspecified,
    MAPDiseaseStatus,
    MAPControlStatus,
    MAPDetectionStatus
  );


  type TFormMap = class( TForm )
      pnlContainer: TPanel;

      pnlSize: TPanel;
      pnlSizeLegend: TPanel;
      pbxSizeLegend: TPaintBox;
      pnlStatus: TPanel;
      pnlStatusLegend: TPanel;

      Panel1: TPanel;
      Panel2: TPanel;
      Panel3: TPanel;
      pbxMapL: TPaintBox;
      Panel4: TPanel;
      pbxMap: TImage;

      pnlLegend: TPanel;
      pbxLegend: TPaintBox;
      pnlProdType: TPanel;
      sbxMap: TPanel;
      sbStatus: TSpeedButton;
      sbSize: TSpeedButton;

      pnlScrollBarH: TPanel;
      ScrollBarH: TScrollBar;

      ScrollBarV: TScrollBar;
      pbxLeftBackground: TPaintBox;
      Panel5: TPanel;
      pbxMapAxis: TPaintBox;
      Panel6: TPanel;
      pbxMapT: TPaintBox;
      pbxTopBackground: TPaintBox;
      pnlMapInfo: TPanel;
      pbxMapInfo: TPaintBox;
      pbxInv: TImage;
      pbxZoneLegend: TPaintBox;

      pnlControls: TPanel;
      pnlProdTypes: TPanel;
      pnlDecorator3: TPanel;
      pnlDecorator4: TPanel;
      Panel7: TPanel;
      btnSaveData: TBitBtn;
      btnPrintData: TBitBtn;
      btnCopyData: TBitBtn;
      btnSaveCharts: TBitBtn;
      btnCopyCharts: TBitBtn;
      btnPrintCharts: TBitBtn;
      pnlDecorator1: TPanel;
      pnlDecorator2: TPanel;
      pnlMenu: TPanel;
      mainMenuBar: TActionMainMenuBar;
      cboProdTypes: TComboBox;
      sbZoomIn: TSpeedButton;
      sbZoomOut: TSpeedButton;
      sbFitToWindow: TSpeedButton;
      ActionManager1: TActionManager;

      mnuFile: TAction;
      actKMZExport: TAction;

      mnuEdit: TAction;
      mnuView: TAction;

      SaveDialog1: TSaveDialog;

      procedure FormClose(Sender: TObject; var Action: TCloseAction);

      procedure pbxLegendPaint(Sender: TObject);

      procedure cboProdTypes3Change(Sender: TObject);

      //procedure pbxMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      //procedure pbxMapMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

      procedure ScrollBarHScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
      procedure ScrollBarVScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);

      procedure sbStatusClick(Sender: TObject);
      procedure sbSizeClick(Sender: TObject);

      procedure sbZoomInClick(Sender: TObject);
      procedure sbZoomOutClick(Sender: TObject);

      procedure pbxMapTPaint(Sender: TObject);
      procedure pbxMapLPaint(Sender: TObject);
      procedure FormResize(Sender: TObject);
      procedure pbxTopBackgroundPaint(Sender: TObject);
      procedure pbxLeftBackgroundPaint(Sender: TObject);
      procedure sbFitToWindowClick(Sender: TObject);
      procedure pbxMapAxisPaint(Sender: TObject);
      procedure pbxMapInfoPaint(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure pbxMapMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure pbxMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure actKMZExportExecute(Sender: TObject);

    protected
      _mouseIsDown: boolean;
      _displayType: TMapDisplay;
      _sizeDisplaySmall: boolean;

      _smdb: TSMDatabase;
      _sim: TSMSimulationInput;
      _herds: THerdList;
      _myMainForm: TForm;

      _minX, _maxX, _minY, _maxY: double;

      _zoomCount, _maxZoom: integer;

      _scaleFactor: double;

      _perimeterListC: THRD_PerimeterList;
      _usingZonesCopy: boolean;
      _perimeterList: TZonePerimeterList;

      _lastHintPos: TPoint;
      _updateHint: boolean;
      _herdLocations: TTwoDPlusSparseArray;

      _borderDisabled: boolean;

      procedure translateUI();

      procedure WMNCPaint( var Msg: TWMNCPaint ); message WM_NCPAINT;

      procedure TextOutAngle(Canvas: TCanvas; x,y: integer; s: string; angle: integer);

      // Primitive drawing functions, used by drawScreen
      //------------------------------------------------
      procedure DrawBasicRectangle();
      procedure DrawXScaleBar(); /// Draws the scale bar to illustrate approximate distances on the map.
      procedure DrawAxisLabels(); /// Writes "Lat" and "Lon" on the appropriate axes.
      procedure DrawYAxisScale(); /// Draws a scale down the left edge indicating approximate latitudes.
      procedure DrawXAxisScale(); /// Draws a scale across the top edge indicating approximate longitudes.

      procedure MakeZoneLegend();
      procedure MakeHerdLegend();
      procedure DrawLegendPoint( pbx: TPaintbox; sz: byte; x,y: integer );
      procedure makeDiseaseStateLegend( newLeft, nTop, vSpace, halfVs: integer );
      procedure makeControlStatusLegend( newLeft, nTop, vSpace, halfVs: integer );
      procedure makeDetectionStatusLegend( newLeft, nTop, vSpace, halfVs: integer );

      function getPtSize( hs: word ): integer;

      procedure setScrollProperties();
      function ScrX( x: double ): integer; /// Transforms the map x coordinate into the screen coordinate system
      function ScrY( y: double ): integer; /// Transforms the map y coordinate into the screen coordinate system
      procedure DrawScreen( const lockWindow: boolean = true );
      procedure clearMap();
      procedure clearInfoMap();
      procedure updateProdTypeList();

      function getAvailableMapHeight(): integer;
      function getAvailableMapWidth(): integer;

      procedure fitMapToWindow();

      procedure drawZonesInternal();

      procedure displayHerdInfo();

    public
      constructor create( AOwner: TComponent; db: TSMDatabase; smsim: TSMSimulationInput; smherds: THerdList ); reintroduce;

      destructor destroy(); override;

      procedure resetSim( db: TSMDatabase; smsim: TSMSimulationInput; smherds: THerdList; const clearOutput: boolean );

      procedure DrawAllUnits();
      procedure drawThisUnit( h: THerd; const hIdx: integer = -1 );

      procedure clearZones();
      procedure drawZones( p: THRD_PerimeterList );
      procedure copyZones();
      procedure updateCaption();

      procedure updateSimComplete();

      property borderDisabled: boolean read _borderDisabled write _borderDisabled;
      property availableMapHeight: integer read getAvailableMapHeight;
      property availableMapWidth: integer read getAvailableMapWidth;
    end
  ;

  var
    frmMap: TFormMap;

implementation
  {$R *.DFM}

  uses
    Math,

    MyStrUtils,
    DebugWindow,
    BasicGIS,
    I88n,

    FormMain,
    ProductionType,
    ProductionTypeList,
    HerdKML
  ;

  const
    DBSHOWMSG: boolean = false; // set to true to enable debugging messages for this unit.  

  // From http://www.experts-exchange.com/Programming/Programming_Languages/Delphi/Q_21488485.html
  procedure TFormMap.TextOutAngle(Canvas: TCanvas; x,y: integer; s: string; angle: integer);
    var
      Fnt, FntPrev: HFONT;
      lMyLogFont : TLogFont;
    begin
      SetBkMode(Canvas.Handle, TRANSPARENT);
      GetObject(Canvas.Font.Handle, SizeOf(TLogFont), @lMyLogFont);
      lMyLogFont.lfEscapement:= Angle;
      lMyLogFont.lfOutPrecision:= OUT_TT_ONLY_PRECIS;

      Fnt:= CreateFontIndirect(lMyLogFont);
      FntPrev := SelectObject(Canvas.Handle, Fnt);
      Canvas.TextOut(x,y, s);
      SelectObject(Canvas.Handle, FntPrev);
      DeleteObject(Fnt);
      SetBkMode(Canvas.Handle, OPAQUE);
    end
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormMap.create( AOwner: TComponent; db: TSMDatabase; smSim: TSMSimulationInput; smherds: THerdList );
    var
      mStream: TMemoryStream;
    begin
      inherited create( AOwner );
      translateUI();

      _borderDisabled := false;

      _smdb := db;

      _perimeterListC := nil;

      if( _smdb.containsZonePerimeters ) then
        begin
          _perimeterList := TZonePerimeterList.create();

          mStream := _smdb.createStreamFromBlob( 'dynBlob', 'zonePerimeters' );

          _perimeterList.loadFromStream( mStream );
          mStream.Free();
          _usingZonesCopy := true;
        end
      else
        begin
          _perimeterList := nil;
          _usingZonesCopy := false;
        end
      ;

      _scaleFactor := screen.PixelsPerInch / 96;

      //LockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      _smdb := db;
      _sim := smSim;
      _herds := smherds;

      _myMainForm := AOwner as TForm;

      pnlSize.BevelOuter := bvNone;
      pnlSizeLegend.BevelOuter := bvNone;
      pnlStatus.BevelOuter := bvNone;
      pnlStatusLegend.BevelOuter := bvNone;
      pnlProdType.BevelOuter := bvNone;
      pnlMapInfo.BevelOuter := bvNone;

      pbxMapT.Height := Panel6.ClientHeight;
      pbxMapT.Left := 0;
      pbxTopBackground.Align := alClient;

      pbxMapL.Width := Panel3.ClientWidth;
      pbxMapL.top := 0;
      pbxLeftBackground.Align := alClient;

      pbxInv.Height := pbxMap.Height;
      pbxInv.Width := pbxMap.Width;

      //Allows creation of very large Bitmaps!!!!
      pbxMap.Picture.Bitmap.PixelFormat := pf24bit;
      pbxInv.Picture.Bitmap.PixelFormat := pf24bit;

      _maxZoom := 10;       //Sets the Maximum Zoom of the Map.
      _zoomCount := 1;

      fitMapToWindow();

      _mouseIsDown := False;

      _herdLocations := TTwoDPlusSparseArray.create();
      _lastHintPos.x := -1;
      _lastHintPos.y := -1;

      // setup
      updateProdTypeList();

      // The FormCreate event draws the map.
      // That way, the map is scaled properly.

      _displayType := MAPDiseaseStatus;

      //LockWindowUpdate( 0 );
      self.Perform( WM_SETREDRAW, 1, 0 );
      RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );

      //updateCaption(); // Don't do this here.  It causes the caption to appear as a "ghost".
    end
  ;


  procedure TFormMap.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormMap.dfm
      // File date: Tue May 8 08:46:52 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Control status of units, current/final day' );
          sbSize.Caption := tr( 'Size' );
          sbStatus.Caption := tr( 'Status' );
          sbZoomIn.Hint := tr( 'Zoom in' );
          sbZoomOut.Hint := tr( 'Zoom out' );
          sbFitToWindow.Hint := tr( 'Fit to window' );
          actKMZExport.Caption := tr( '&Export KMZ file...' );
        end
      ;

    end
  ;


  procedure TFormMap.FormCreate(Sender: TObject);
    begin
      //LockWindowUpdate( self.Handle );
      self.Perform( WM_SETREDRAW, 0, 0 );

      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );

          if( 0 < self.Constraints.MinWidth ) then
            self.Constraints.MinWidth := round( self.Constraints.MinWidth * screen.PixelsPerInch / 96 )
          ;
          if( 0 < self.Constraints.MinHeight ) then
            self.Constraints.MinHeight := round( self.Constraints.MinHeight * screen.PixelsPerInch / 96 )
          ;
          if( 0 < self.Constraints.MaxHeight ) then
            self.Constraints.MaxHeight := round( self.Constraints.MaxHeight * screen.PixelsPerInch / 96 )
          ;
          if( 0 < self.Constraints.MaxWidth ) then
            self.Constraints.MaxWidth := round( self.Constraints.MaxWidth * screen.PixelsPerInch / 96 )
          ;
        end
      ;

      sbZoomOutClick( nil );

      //LockWindowUpdate( 0 );
      self.Perform( WM_SETREDRAW, 1, 0 );
      RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
    end
  ;


  destructor TFormMap.destroy();
    begin
      freeAndNil( _perimeterList );
      
      // Don't try to free _perimeterListC: it is owned by the DLL

      freeAndNil( _herdLocations );

      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


  procedure TFormMap.resetSim( db: TSMDatabase; smsim: TSMSimulationInput; smherds: THerdList; const clearOutput: boolean );
    begin
      _smdb := db;
      _sim := smSim;
      _herds := smHerds;

      if( clearOutput ) then
        begin
          freeAndNil( _perimeterList );
          _smdb.clearZonePerimeters();
        end
      ;

      // New //
      _maxZoom := 10;       //Sets the Maximum Zoom of the Map.
      _zoomCount := 1;

      fitMapToWindow();

      _mouseIsDown := False;

      _lastHintPos.x := -1;
      _lastHintPos.y := -1;

      updateProdTypeList();

      self.Perform( WM_SETREDRAW, 0, 0 );

      sbZoomOutClick( nil );

      //LockWindowUpdate( 0 );
      self.Perform( WM_SETREDRAW, 1, 0 );
      RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );

      updateCaption();

      // old //
      (*
      updateProdTypeList();
      drawScreen();
      updateCaption();
      *)
      // end //
    end
  ;


  procedure TFormMap.updateProdTypeList();
    var
      it: TProductionTypeListIterator;
    begin
      it := nil;

      // Update the production type combo box
      cboProdTypes.Clear();

      if( assigned( _sim ) ) then
        begin
          cboProdTypes.AddItem( tr( 'All production types' ), nil );
          it := TProductionTypeListIterator.create( _sim.ptList );
          it.toFirst();

          while( nil <> it.current() ) do
            begin
              cboProdTypes.addItem( it.current().productionTypeDescr, it.current() );
              it.incr();
            end
          ;
        end
      ;

      freeAndNil( it );

      cboProdTypes.ItemIndex := 0;
    end
  ;



  //Clears the main paintbox where the actual Map is drawn
  procedure TFormMap.clearMap();
    begin
      pbxMap.Canvas.Brush.Color := clBtnFace;
      pbxMap.Canvas.pen.Color := clWhite;
      pbxMap.Canvas.Pen.Width := 1;
      pbxMap.Canvas.Pen.Style := psSolid;
      pbxMap.Canvas.Rectangle( 0, 0, pbxMap.Width,pbxMap.Height );
      pbxMap.Canvas.FillRect(Rect (0, 0, pbxMap.Width, pbxMap.Height));

      pbxInv.Canvas.Brush.Color := clBtnFace;
      pbxInv.Canvas.pen.Color := clWhite;
      pbxInv.Canvas.Pen.Width := 1;
      pbxInv.Canvas.Pen.Style := psSolid;
      pbxInv.Canvas.Rectangle( 0, 0, pbxInv.Width,pbxInv.Height );
      pbxInv.Canvas.FillRect(Rect (0, 0, pbxInv.Width, pbxInv.Height));
    end
  ;

  //Clears the Info Bar paintbox.
  procedure TFormMap.clearInfoMap();
    begin
      pbxMapInfo.Canvas.Brush.Color := clBtnFace;
      pbxMapInfo.Canvas.pen.Color := clBlack;
      pbxMapInfo.Canvas.Pen.Width := 1;
      pbxMapInfo.Canvas.Pen.Style := psSolid;
      pbxMapInfo.Canvas.Rectangle( 0, 0, pbxMapInfo.Width,pbxMapInfo.Height );
      pbxMapInfo.Canvas.FillRect(Rect (0, 0, pbxMapInfo.Width, pbxMapInfo.Height));
    end
  ;

  procedure TFormMap.DrawScreen( const lockWindow: boolean = true );
    var
      xBuffer, yBuffer: double;
      xDiff, yDiff: double;
    begin
      if( lockWindow ) then
        //LockWindowUpdate( self.Handle )
        self.Perform( WM_SETREDRAW, 0, 0 )
      ;

      // clear any existing display
      clearMap();

      if( assigned( _herds ) ) then
        begin
          if( 0 < _herds.Count ) then
            begin
              // These values ensure that the map is 2.5% larger in each direction
              // than is actually used by the units in the simulation.  The point
              // is to keep all points drawn on the map inside the map boundaries.
              xDiff := ( _herds.maxX - _herds.minX );
              yDiff := ( _herds.maxY - _herds.minY );

              if( 10.0 > xDiff ) then
                xDiff := 10.0
              ;

              if( 10.0 > yDiff ) then
                yDiff := 10.0
              ;

              xBuffer := abs( 0.025 * xDiff );
              yBuffer := abs( 0.025 * yDiff );

              _minX := ( _herds.minX - xBuffer );
              _maxX := ( _herds.maxX + xBuffer );
              _minY := ( _herds.minY - yBuffer );
              _maxY := ( _herds.maxY + yBuffer );

              // Draw zones first, so that the zone boundaries don't obscure any herds.
              drawZonesInternal();
              DrawAllUnits();
            end
          ;
        end
      ;

      if( lockWindow ) then
        begin
          // see comment above
          //LockWindowUpdate( 0 );
          self.Perform( WM_SETREDRAW, 1, 0 );
          RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
        end
      ;

      DrawXScaleBar();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Primitive drawing/helper functions
//-----------------------------------------------------------------------------
  function TFormMap.getPtSize( hs: word ): integer;
    var
      mult: integer;
    begin
      case _zoomCount of
        1: mult := 1;
        2..5: mult := 2;
        6..10: mult := 3;
      else
        mult := 1;
      end;


      if( _sizeDisplaySmall ) then
        Result := 1 * mult
      else
        begin
          case hs of
            0..1000 : result := 1 * mult;
            1001..10000 : result := 2 * mult;
          else
            result := 3 * mult;
          end;
        end
      ;
    end
  ;


  procedure TFormMap.DrawBasicRectangle();
    begin
      pbxMap.Canvas.Pen.Color := clBlack;
      pbxMap.Canvas.Pen.Width := 1;
      pbxMap.Canvas.Pen.Style := psSolid;
      pbxMap.Canvas.Brush.Style := bsSolid;
      pbxMap.Canvas.Brush.Color := clBtnFace;

      pbxMap.Canvas.Rectangle(
        ScrX( _minX ),
        ScrY( _minY ),
        ScrX( _maxX ),
        ScrY( _maxY )
      );

      pbxInv.Canvas.Pen.Color := clBlack;
      pbxInv.Canvas.Pen.Width := 1;
      pbxInv.Canvas.Pen.Style := psSolid;
      pbxInv.Canvas.Brush.Style := bsSolid;
      pbxInv.Canvas.Brush.Color := clBtnFace;

      pbxInv.Canvas.Rectangle(
        ScrX( _minX ),
        ScrY( _minY ),
        ScrX( _maxX ),
        ScrY( _maxY )
      );
    end
  ;


  procedure TFormMap.DrawYAxisScale();
    var
      s: string;
      w: integer;
      nTicks: integer;
      i: integer;
      yIncrement: double;
      llr: RLatLon;
      textHeight: integer;
      nextTextY: integer;
      y: double;
      tickY, textY: integer;
      llMin, llMax: RLatLon;
      llDiff: double;
    begin
      pbxMapL.Canvas.Pen.Color := clBlack;
      pbxMapL.Canvas.Pen.Style := psSolid;
      pbxMapL.Canvas.Pen.Width := 1;
      pbxMapL.Canvas.Brush.Color := clWhite;

      if( 3 >= _zoomCount ) then
        nTicks := 6
      else
        nTicks := 11
      ;

      yIncrement := ( _maxY - _minY ) / nTicks;

      textHeight := pbxMapL.Canvas.TextHeight( '0' ) + 2;
      nextTextY := 0;

      //rbh: Issue 2464
      // If the area modeled is small, around a half degree latitude (~35 mi) or smaller,
      // or if the zoom is increased such that the tick interval is less than 0.1 degrees
      // then show the tick mark latitudes at two decimal precision (otherwise adjacent ticks can have the same value).
      llMin := _herds.projection.pjInv( _minX, _minY );
      llMax := _herds.projection.pjInv( _minX, _maxY );
      llDiff := Abs(llMax.lat - llMin.lat) / nTicks;

      for i := nTicks - 1 downto 1 do
        begin
          y := _minY + ( i * yIncrement );
          tickY := scrY( y );
          textY := tickY - 5;

          // Be a little careful to ensure that new text isn't being
          // drawn on top of existing text.
          if( textY > nextTextY ) then
            begin
              // Draw a tickmark
              pbxMapL.canvas.MoveTo( pbxMapL.Width, tickY );
              pbxMapL.canvas.LineTo( pbxMapL.Width - 5, tickY );

              // Figure out what the latitude is at this projected y value
              llr := _herds.projection.pjInv( _minX, y );

              // If the tick interval is less than 0.1 degrees then want to show 2 decimals, else 1
              if (0.1 >= llDiff) then
                s := uiFloatToStrZeroPadded( llr.lat, 2 )
              else
                s := uiFloatToStrZeroPadded( llr.lat, 1 );
              // Draw the text to the image.
              w := pbxMapL.canvas.TextWidth(s);

              pbxMapL.canvas.TextOut( pbxMapL.Width - (w + 6), textY, s );
              nextTextY := textY + textHeight + 2;
            end
          ;
        end
      ;
    end
  ;


  procedure TFormMap.DrawXAxisScale();
    var
      s: string;
      halfTextHeight: integer;
      nTicks: integer;
      xIncrement: double;
      i: integer;
      llr: RLatLon;
      textHeight: integer;
      nextTextX: integer;
      x: double;
      tickX, textX: integer;
      llMin, llMax: RLatLon;
      llDiff: double;
    begin
      pbxMapT.Canvas.Pen.Color := clBlack;
      pbxMapT.Canvas.Pen.Style := psSolid;
      pbxMapT.Canvas.Pen.Width := 1;
      pbxMapT.Canvas.Brush.Color := clWhite;

      halfTextHeight := pbxMapT.Canvas.TextHeight( '0' ) div 2;

      if( 3 >= _zoomCount ) then
        nTicks := 6
      else
        nTicks := 11
      ;

      xIncrement := ( _maxX - _minX ) / nTicks;

      textHeight := pbxMapL.Canvas.TextHeight( '0' ) + 2;
      nextTextX := 0;

      //rbh: Issue 2464
      // If the area modeled is small, around a half degree longitude (~35 mi) or smaller,
      // or if the zoom is increased such that the tick interval is less than 0.1 degrees
      // then show the tick mark longitudes at two decimal precision (otherwise adjacent ticks can have the same value).
      // Fix Me! There is not enough room for longitudes of 7 characters (-nnn.nn), the trailing decimal is partially hidden.
      llMin := _herds.projection.pjInv( _minX, _minY );
      llMax := _herds.projection.pjInv( _maxX, _minY );
      llDiff := Abs(llMax.lon - llMin.lon) / nTicks;

      //dbcout2( scrX( _maxX ) );
      for i := 1 to nTicks do
        begin
          x := _minX + ( i * xIncrement );
          tickX := scrX( x );
          textX := tickX - halfTextHeight;

          // Be a little careful to ensure that new text isn't being
          // drawn on top of existing text.
          if( textX > nextTextX ) then
            begin
              // Draw a tickmark
              //dbcout2( usFloatToStr( x ) + ': ' + intToStr( tickX ) );
              pbxMapT.Canvas.MoveTo( tickX, pbxMapT.Height );
              pbxMapT.Canvas.LineTo( tickX, pbxMapT.Height - 5 );

              // Figure out what the longitude is at this projected x value
              llr := _herds.projection.pjInv( x, ( _minY + _maxY ) / 2 );

              // If the tick interval is less than 0.1 degrees then want to show 2 decimals, else 1
              if (0.1 >= llDiff) then
                s := uiFloatToStrZeroPadded( llr.lon, 2 )
              else
                s := uiFloatToStrZeroPadded( llr.lon, 1 );
              // Draw the text to the image
              textOutAngle( pbxMapT.Canvas, textX, pbxMapT.Height - 7, s, 900 );
              nextTextX := textX + textHeight + 2;
            end
          ;
        end
      ;
    end
  ;


  procedure TFormMap.DrawXScaleBar();
    var
      LRDist, Prop: double;
      t: integer;

      x, y: integer;
      distScreenCoords, lineLen: integer;
    begin
      // Erase old information
      clearInfoMap();

      // Write the zoom number
      pbxMapInfo.Canvas.Brush.Color := clBtnFace;
      pbxMapInfo.Canvas.Pen.Color := clBlack;
      pbxMapInfo.Canvas.Font.Name := 'Arial';
      pbxMapInfo.Canvas.Font.Size := round( 8 / _scaleFactor );
      pbxMapInfo.Canvas.TextOut(pbxMapInfo.Left+20,pbxMapInfo.Height-15, tr( 'Zoom:' ) + ' ' + intToStr( _zoomCount ) + 'x');


      // Determine the distance between two lines of longitude at the mean latitude of the map.
      // (Remember that this unit makes adjustments for different distances between lines of longitude
      // at various latitudes.)
      LRDist := abs( _maxX - _minX );

      if LRDist = 0 then LRDist := 1;


      // Calculate 1/10th of this distance, to roughly the nearest 10 km.
      // Store this calculation in Prop.
      t := 0;
      while( t / LRDist < 0.1 ) do
        Inc(t, 10)
      ;

      try
        Prop := t / LRDist;
      except
        dbcout( 'Map exception!', true );
        dbcout( 'Prop := t / LRDist', true );
        dbcout( 't = ' + dbFloatToStr( t ), true );
        dbcout( 'LRDist = ' + dbFloatToStr( LRDist ), true );
        Prop := 1.0;
      end;


      // Convert the 1/10 distance into screen coordinates.
      distScreenCoords := Round( Prop * ( ScrX( _maxX ) - ScrX( _minX ) ) );

      // Further divide distScreenCoords by the zoom number, to determine
      // how long the scale bar will actually be.
      lineLen := round( distScreenCoords / _zoomCount );

      // Start drawing the scalebar.
      // The left end of the scalebar should be at x = 200.
      // The right end of the scalebar will be at 200 + lineLen.
      y := ( pbxMapInfo.height div 3 ) * 2;
      x := 200;

      pbxMapInfo.Canvas.MoveTo( x, y );
      pbxMapInfo.Canvas.LineTo( x + lineLen, y );

      // Draw the tick lines on the scalebar
      pbxMapInfo.Canvas.MoveTo( x, y - 3 );
      pbxMapInfo.Canvas.LineTo( x, y + 3 );
      pbxMapInfo.Canvas.MoveTo( x + lineLen, y - 3 );
      pbxMapInfo.Canvas.LineTo( x + lineLen, y + 3 );

      // Finally, write the text that indicates how long the scalebar is.
      // It should be centered over the scalebar.
      pbxMapInfo.Canvas.Font.Name := 'MS Serif';
      pbxMapInfo.Canvas.Font.Size := round( 6 / _scaleFactor );
      t := t div _zoomCount;
      pbxMapInfo.Canvas.TextOut(
        (x + lineLen div 2) - ( pbxMapInfo.Canvas.TextWidth(IntToStr(t) + ' km') div 2),
        y - 1 - pbxMapInfo.Canvas.TextHeight('k'),
        IntToStr(t) + ' km'
      );
    end
  ;


  procedure TFormMap.DrawAxisLabels();
    var
      yPos, xPos: integer;
    begin
      pbxMapAxis.Canvas.Brush.Color := clWhite;
      pbxMapAxis.Canvas.Pen.Color := clBlack;
      pbxMapAxis.Canvas.Font := pbxMapT.Canvas.Font;
      pbxMapAxis.Canvas.MoveTo(pbxMapAxis.Left,pbxMapAxis.Height-1);
      pbxMapAxis.Canvas.LineTo(pbxMapAxis.Left+pbxMapAxis.Width,pbxMapAxis.Height-1);
      pbxMapAxis.Canvas.MoveTo(pbxMapAxis.Left+pbxMapAxis.Width-1,pbxMapAxis.Height-1);
      pbxMapAxis.Canvas.LineTo(pbxMapAxis.Left+pbxMapAxis.Width-1,0);

      // Draw the "Lat" label
      xPos := 1;
      yPos := pbxMapAxis.Height - pbxMapAxis.Canvas.TextHeight( 'Lat' ) - 1;
      pbxMapAxis.Canvas.TextOut( xPos, yPos, 'Lat');

      // Draw the "Lon" label
      // Remember that it's sideways, so the xPos calculation looks a little odd.
      xPos := pbxMapAxis.Width - pbxMapAxis.Canvas.TextHeight( 'Lon' ) - 1;
      yPos := pbxMapAxis.Canvas.TextWidth( 'Lonn'); // The extra character ensures a little padding.
      textOutAngle( pbxMapAxis.Canvas, xPos, yPos, 'Lon', 900 );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Legend creation
//-----------------------------------------------------------------------------
  procedure TFormMap.DrawLegendPoint( pbx: TPaintbox; sz: byte; x,y: integer );
    begin
      pbx.Canvas.Rectangle( x-sz, y-sz, x+sz, y+sz );
    end
  ;

  procedure TFormMap.MakeZoneLegend();
    var
      vspace, halfVS : byte;
      NewLeft,NTop : integer;
      zList: TZoneList;
      i: integer;
      maxWidth: integer;
    begin
      NewLeft := 3;
      zList := frmMain.Scenario.simInput.zoneList;
      zlist.ssXml();  // Need this to force level creation in zones!

      pbxZoneLegend.Canvas.Font.Name := 'Arial Narrow';
      pbxZoneLegend.Canvas.Font.Size := 8; //round( 8 / _scaleFactor ) ;
      vspace := pbxZoneLegend.Canvas.TextHeight('N');
      NTop := (pbxZoneLegend.Height - (3*vSpace)) div 2;
      HalfVS := (vSpace div 2) + 1;
      maxWidth := 0;

      for i := 0 to zList.Count -1 do
        begin
          if ( pbxZoneLegend.Canvas.TextWidth( zList.at(i).descr ) > maxWidth )  then
            maxWidth := pbxZoneLegend.Canvas.TextWidth( zList.at(i).descr );
        end;

      maxWidth := maxWidth + round( 10 * _scaleFactor ) * 2;

      for i := 0 to zList.Count - 1  do
        begin
          pbxZoneLegend.Canvas.Pen.Color := zoneColor( zList.at(i).level );
          pbxZoneLegend.Canvas.Brush.Color := pbxZoneLegend.Canvas.Pen.Color;

          DrawLegendPoint( pbxZoneLegend, 2,NewLeft + trunc(i/3)* maxWidth, NTop + ((i mod 3) * vspace) + HalfVS);
          DrawLegendPoint( pbxZoneLegend, 2,NewLeft + trunc(i/3)* maxWidth + 2, NTop + ((i mod 3) * vspace) + HalfVS);

          pbxZoneLegend.Canvas.Pen.Color := clBlack;
          pbxZoneLegend.Canvas.Brush.Color := clBtnFace;

          pbxZoneLegend.Canvas.TextOut(NewLeft + trunc(i/3)* maxWidth + round( 10 * _scaleFactor ),NTop + ((i mod 3 ) * vspace),zList.at(i).descr );
        end;
    end
  ;


  procedure TFormMap.makeDiseaseStateLegend( newLeft, nTop, vSpace, halfVs: integer );
    begin
      // First column
      //-------------
      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateSusceptible );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateSusceptible );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + HalfVS );

      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateLatent );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateLatent );
      DrawLegendPoint( pbxLegend, 2, NewLeft, Ntop + (1*vspace) + HalfVS );

      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateSubclinical );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateSubclinical );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + (2*vspace) + HalfVS );

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft,7);
      pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2, tr( 'Susceptible' ) );
      pbxLegend.Canvas.TextOut( NewLeft, NTop + 2 + vspace, tr( 'Latent' ) );
      pbxLegend.Canvas.TextOut( NewLeft, NTop + 2 + (2*vspace), tr( 'Subclinical' ) );

      // Second column
      //--------------
      Inc(NewLeft, pbxLegend.Canvas.TextWidth( tr( 'Susceptible' ) ) + round( 10 * _scaleFactor ) );

      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateClinical );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateClinical );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + HalfVS );

      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateNaturallyImmune );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateNaturallyImmune );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + vspace + HalfVS );

      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateVaccineImmune );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateVaccineImmune );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + 2 * vspace + HalfVS );

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;

      Inc(NewLeft,7);
      pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2, tr( 'Clinical' ) );
      pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2 + vspace,tr( 'Nat immune' ) );
      pbxLegend.Canvas.TextOut( NewLeft, NTop + 2 + (2*vspace), tr( 'Vaccine immune' ) );

      // Third column
      //-------------
      inc( NewLeft, pbxLegend.Canvas.TextWidth( tr( 'Vaccine immune') ) + round( 10 * _scaleFactor ) );

      pbxLegend.Canvas.Pen.Color := naadsmDiseaseStateColor( NAADSMStateDestroyed );
      pbxLegend.Canvas.Brush.Color := naadsmDiseaseStateColor( NAADSMStateDestroyed );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + HalfVS );

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;

      Inc(NewLeft, round( 7 * _scaleFactor ) );
      pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2, tr( 'Destroyed' ) );
    end
  ;


  procedure TFormMap.makeControlStatusLegend( newLeft, nTop, vSpace, halfVs: integer );
    var
      maxTextWidth: integer;
    begin
      // First column
      //--------------
      pbxLegend.Canvas.Pen.Color := controlStatusColor( asNoControl );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asNoControl );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + HalfVS);

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asDetected );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asDetected );
      DrawLegendPoint( pbxLegend, 2, NewLeft, Ntop + (1*vspace) + HalfVS);

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asDestroyed );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asDestroyed );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + (2*vspace) + HalfVS);

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft, round( 7 * _scaleFactor ) );
      pbxLegend.Canvas.TextOut(NewLeft, Ntop + 2, tr( 'No control' ) );
      pbxLegend.Canvas.TextOut(NewLeft, NTop + 2 + vspace, tr( 'Detected' ) );
      pbxLegend.Canvas.TextOut(NewLeft, NTop + 2 + (2*vspace),tr( 'Destroyed' ) );

      // Second column
      //--------------
      maxTextWidth := max(
        pbxLegend.Canvas.TextWidth( tr( 'No control' ) ),
        pbxLegend.Canvas.TextWidth( tr( 'Detected' ) )
      );
      maxTextWidth := max( maxTextWidth, pbxLegend.Canvas.TextWidth( tr( 'Destroyed' ) ) );

      Inc(NewLeft, maxTextWidth + round( 10 * _scaleFactor ) );

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asVaccinated );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asVaccinated );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asTracedDirectFwd );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asTracedDirectFwd );
      DrawLegendPoint( pbxLegend, 2,NewLeft, Ntop + (1*vspace) + HalfVS);

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asTracedIndirectFwd );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asTracedIndirectFwd );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + (2*vspace) + HalfVS);

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft,7);
      pbxLegend.Canvas.TextOut(NewLeft,Ntop + 2, tr( 'Vaccinated' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + vspace, tr( 'Traced fwd-Direct' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + (2*vspace), tr( 'Traced fwd-Indirect' ) );

      // Third column
      //-------------
      maxTextWidth := max(
        pbxLegend.Canvas.TextWidth( tr( 'Vaccinated' ) ),
        pbxLegend.Canvas.TextWidth( tr( 'Traced fwd-Direct' ) )
      );
      maxTextWidth := max( maxTextWidth, pbxLegend.Canvas.TextWidth( tr( 'Traced fwd-Indirect' ) ) );

      Inc(NewLeft, maxTextWidth + round( 10 * _scaleFactor ) );

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asTracedDirectBack );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asTracedDirectBack );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asTracedIndirectBack );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asTracedIndirectBack );
      DrawLegendPoint( pbxLegend, 2,NewLeft, Ntop + (1*vspace) + HalfVS);

      pbxLegend.Canvas.Pen.Color := controlStatusColor( asInDestructionQueue );
      pbxLegend.Canvas.Brush.Color := controlStatusColor( asInDestructionQueue );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + (2*vspace) + HalfVS);

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft,7);
      pbxLegend.Canvas.TextOut(NewLeft,Ntop + 2, tr( 'Traced back-Direct' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + vspace, tr( 'Traced back-Indirect' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + (2*vspace), tr( 'Quarantined' ) );
    end
  ;


  procedure TFormMap.makeDetectionStatusLegend( newLeft, nTop, vSpace, halfVs: integer );
    var
      maxTextWidth: integer;
    begin
      // First column
      //--------------
      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsNoStatus );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsNoStatus );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + HalfVS);

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsInfectedUndetected );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsInfectedUndetected );
      DrawLegendPoint( pbxLegend, 2, NewLeft, Ntop + (1*vspace) + HalfVS);

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsExamined );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsExamined );
      DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + (2*vspace) + HalfVS);

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft, round( 7 * _scaleFactor ) );
      pbxLegend.Canvas.TextOut(NewLeft, Ntop + 2, tr( 'No status' ) );
      pbxLegend.Canvas.TextOut(NewLeft, NTop + 2 + vspace, tr( 'Infected undetected' ) );
      pbxLegend.Canvas.TextOut(NewLeft, NTop + 2 + (2*vspace),tr( 'Examined' ) );

      // Second column
      //--------------
      maxTextWidth := max(
        pbxLegend.Canvas.TextWidth( tr( 'No status' ) ),
        pbxLegend.Canvas.TextWidth( tr( 'Infected undetected' ) )
      );
      maxTextWidth := max( maxTextWidth, pbxLegend.Canvas.TextWidth( tr( 'Examined' ) ) );

      Inc(NewLeft, maxTextWidth + round( 10 * _scaleFactor ) );

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsTestTrueNeg );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsTestTrueNeg );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsTestFalseNeg );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsTestFalseNeg );
      DrawLegendPoint( pbxLegend, 2,NewLeft, Ntop + (1*vspace) + HalfVS);

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsTestFalsePos );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsTestFalsePos );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + (2*vspace) + HalfVS);

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft,7);
      pbxLegend.Canvas.TextOut(NewLeft,Ntop + 2, tr( 'Test true neg' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + vspace, tr( 'Test false neg' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + (2*vspace), tr( 'Test false pos' ) );

      // Third column
      //-------------
      maxTextWidth := max(
        pbxLegend.Canvas.TextWidth( tr( 'Test true neg' ) ),
        pbxLegend.Canvas.TextWidth( tr( 'Test false neg' ) )
      );
      maxTextWidth := max( maxTextWidth, pbxLegend.Canvas.TextWidth( tr( 'Test false pos' ) ) );

      Inc(NewLeft, maxTextWidth + round( 10 * _scaleFactor ) );

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsTestTruePos );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsTestTruePos );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsDetectedClinical );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsDetectedClinical );
      DrawLegendPoint( pbxLegend, 2,NewLeft, Ntop + (1*vspace) + HalfVS);

      pbxLegend.Canvas.Pen.Color := detectionStatusColor( dsDestroyed );
      pbxLegend.Canvas.Brush.Color := detectionStatusColor( dsDestroyed );
      DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + (2*vspace) + HalfVS);

      // corresponding text
      pbxLegend.Canvas.Pen.Color := clBlack;
      pbxLegend.Canvas.Brush.Color := clBtnFace;
      Inc(NewLeft,7);
      pbxLegend.Canvas.TextOut(NewLeft,Ntop + 2, tr( 'Test true pos' ) );
      pbxLegend.Canvas.TextOut(NewLeft,NTop + 2 + vspace, tr( 'Detected clinical' ) );
      pbxLegend.Canvas.TextOut(NewLeft, NTop + 2 + (2*vspace),tr( 'Destroyed' ) );
    end
  ;


  procedure TFormMap.MakeHerdLegend();
    var
      vspace, halfVS : byte;
      NewLeft,NTop : integer;
    begin
      NewLeft := 8;

      pbxSizeLegend.Canvas.Font.Name := 'Arial Narrow';
      pbxSizeLegend.Canvas.Font.Size := 11;
      vspace := pbxSizeLegend.Canvas.TextHeight('N');
      pbxSizeLegend.Canvas.Font.Size := 8; //round( 8 / _scaleFactor ) ;
      NTop := (pbxSizeLegend.Height - (3*vSpace)) div 2;
      HalfVS := (vSpace div 2) + 1;

      pbxSizeLegend.Canvas.Brush.Color := clBlack;
      pbxSizeLegend.Canvas.pen.Color := clBlack;

      if( _sizeDisplaySmall ) then
        begin
          pbxSizeLegend.Canvas.Brush.Color := clBtnFace;
          pbxSizeLegend.Canvas.pen.Color := clBtnFace;
          DrawLegendPoint( pbxSizeLegend, 12, NewLeft, NTop + HalfVS );
          pbxSizeLegend.Canvas.pen.Color := clBlack;
          pbxSizeLegend.Canvas.Brush.Color := clBlack;
          DrawLegendPoint( pbxSizeLegend, getPtSize( 100 ), NewLeft, NTop + HalfVS );
          Inc( NewLeft, round( 10 * _scaleFactor ) );
          pbxSizeLegend.Canvas.Brush.Color := clBtnFace;
          pbxSizeLegend.Canvas.TextOut( NewLeft + 2, NTop + 4, tr( 'All units' ) );
        end
      else
        begin
          // size marks
          pbxSizeLegend.Canvas.pen.Color := clBtnFace;
          pbxSizeLegend.Canvas.Brush.Color := clBtnFace;
          DrawLegendPoint( pbxSizeLegend, 12, NewLeft, NTop + HalfVS );
          DrawLegendPoint( pbxSizeLegend, 12, NewLeft, NTop + vspace + HalfVS );
          DrawLegendPoint( pbxSizeLegend, 12, NewLeft, NTop + (2*vspace) + HalfVS );
          pbxSizeLegend.Canvas.pen.Color := clBlack;
          pbxSizeLegend.Canvas.Brush.Color := clBlack;
          DrawLegendPoint( pbxSizeLegend, getPtSize( 100 ), NewLeft, NTop + HalfVS );
          DrawLegendPoint( pbxSizeLegend, getPtSize( 2000 ), NewLeft, NTop + vspace + HalfVS );
          DrawLegendPoint( pbxSizeLegend, getPtSize( 20000 ), NewLeft, NTop + (2*vspace) + HalfVS );
          pbxSizeLegend.Canvas.Brush.Color := clBtnFace;
          Inc(NewLeft, round( 10 * _scaleFactor ) );
          pbxSizeLegend.Canvas.TextOut( NewLeft+2, NTop, '1-1000');
          pbxSizeLegend.Canvas.TextOut( NewLeft+2, NTop + vspace + 4, '1001-10000' );
          pbxSizeLegend.Canvas.TextOut( NewLeft+2, Ntop + (vspace*2) + 4, '> 10000' );
        end
      ;

      pbxLegend.Canvas.Font.Name := 'Arial Narrow';
      pbxLegend.Canvas.Font.Size := 8; //round( 8 / _scaleFactor ) ;
      pbxLegend.Canvas.Brush.Color := clBlack;
      NewLeft := 3;

      case _displayType of
        MAPDiseaseStatus: makeDiseaseStateLegend( newLeft, nTop, vSpace, halfVs );
        MAPControlStatus: makeControlStatusLegend( newLeft, nTop, vSpace, halfVs );
        MAPDetectionStatus: makeDetectionStatusLegend( newLeft, nTop, vSpace, halfVs );
      end;

      updateCaption();
    end
  ;

  
  procedure TFormMap.updateCaption();
    begin
      case _displayType of
        MAPDiseaseStatus: Self.Caption := tr( 'Unit disease state map:' ) + ' ' + ( _myMainForm as TFormMain ).simStatusStr;
        MAPControlStatus: Self.Caption := tr( 'Unit control status map:' ) + ' ' + ( _myMainForm as TFormMain ).simStatusStr;
        MAPDetectionStatus: self.Caption := tr( 'Unit detection status map:' ) + ' ' + ( _myMainForm as TFormMain ).simStatusStr;
      end;
    end
  ;


  procedure TFormMap.updateSimComplete();
    begin
      updateCaption();
    end
  ;
//-----------------------------------------------------------------------------


  function TFormMap.ScrY( y: double ): integer;
    var
      yRange : single;
      ScrYRange : integer;
    begin
      yRange := _maxY - _minY;
      ScrYRange := pbxMap.Height;
      if yRange = 0 then yRange := 0.00001;
      Result := Round((1-((y - _minY)/ yRange))*ScrYRange);
    end
  ;


   function TFormMap.ScrX( x: double ): integer;
    var
      xRange : single;
      ScrXRange : integer;
    begin
      xRange := _maxX - _minX;
      ScrXRange := pbxMap.Width;
      if xRange = 0 then xRange := 0.00001;
      Result := Round( ( ( x - _minX ) / xRange) * ScrXRange );
    end
  ;


  {*
    Used to update the displayed status of individual herds during a simulation run.
  }
  procedure TFormMap.drawThisUnit( h: THerd; const hIdx: integer );
    var
      pointSize : byte;
      pointColor : TColor;
      pt: TProductionType;
      x, y: integer;
    begin
      // If cboProdTypes.ItemIndex is 0, draw herds of all production types.
      // Otherwise, check to see if this production type should be drawn before proceeding.
      if( 0 <> cboProdTypes.ItemIndex ) then
        begin
          pt := cboProdTypes.Items.Objects[cboProdTypes.ItemIndex] as TProductionType;

          if( h.prodTypeID <> pt.productionTypeID ) then
            exit
          ;
        end
      ;
      
      // Set point size
      pointSize := GetPtSize( h.initialSize );

      // Set point color
      case _displayType of
        MAPDiseaseStatus: pointColor := naadsmDiseaseStateColor( h.diseaseStatus );
        MAPControlStatus: pointColor := controlStatusColor( h.controlStatus );
        MAPDetectionStatus: pointColor := detectionStatusColor( h.detectionStatus );
      else
        begin
          raise exception.Create('Bad display type in TFormMain.drawThisUnit()');
          pointColor := clLime;
        end
      ;
      end;

      pbxMap.Canvas.Brush.Color := pointColor;
      pbxMap.Canvas.Pen.Color := pointColor;

      pbxMap.Canvas.Rectangle(
        ScrX( h.x ) - pointSize,
        ScrY( h.y ) - pointSize,
        ScrX( h.x ) + pointSize,
        ScrY( h.y ) + pointSize
      );

      pbxInv.Canvas.Brush.Color := pointColor;
      pbxInv.Canvas.Pen.Color := pointColor;

      pbxInv.Canvas.Rectangle(
        ScrX( h.x ) - pointSize,
        ScrY( h.y ) - pointSize,
        ScrX( h.x ) + pointSize,
        ScrY( h.y ) + pointSize
      );

      if( -1 = hIdx ) then
        begin
          // The herd location array has already been populated.
          // Just refresh the hint, if one is showing.
          displayHerdInfo();
        end
      else
        begin
          // The herd array needs to be built.
          for x := ( ScrX( h.x ) - pointSize ) to ( ScrX( h.x ) + pointSize ) do
            begin
              for y := ( ScrY( h.y ) - pointSize ) to ( ScrY( h.y ) + pointSize ) do
                _herdLocations.appendZValue( x, y, hIdx )
              ;
            end
          ;
        end
      ;
    end
  ;


  procedure TFormMap.drawAllUnits();
    var
      hIdx: integer;
    begin
      _herdLocations.deleteValues();
      _updateHint := false;

      for hIdx := 0 to _herds.Count - 1 do
        drawThisUnit( _herds.at( hIdx ), hIdx )
      ;

      _updateHint := true;

      displayHerdInfo();
    end
  ;


  procedure TFormMap.FormClose(Sender: TObject; var Action: TCloseAction);
    begin
      frmMain.uncheckWindowMenuItem( self.name );
      Action := caFree;
    end
  ;


  procedure TFormMap.FormResize(Sender: TObject);
    begin
      setScrollProperties();
    end
  ;


  procedure TFormMap.pbxLegendPaint(Sender: TObject);
    begin
      MakeHerdLegend();
      MakeZoneLegend();
    end
  ;


  procedure TFormMap.sbStatusClick(Sender: TObject);
    begin
      case _displayType of
        MAPDiseaseStatus: _displayType := MAPControlStatus;
        MAPControlStatus: _displayType := MAPDetectionStatus;
        MAPDetectionStatus: _displayType := MAPDiseaseStatus;
      end;

      Screen.Cursor := crHourglass;

      pbxLegend.Repaint();
      DrawScreen(false);

      Screen.Cursor := crDefault;
    end
  ;


  procedure TFormMap.sbSizeClick(Sender: TObject);
    begin
      _sizeDisplaySmall := not _sizeDisplaySmall;

      Screen.Cursor := crHourglass;

      pbxSizeLegend.Repaint();
      DrawScreen(false);

      Screen.Cursor := crDefault;
    end
  ;


  procedure TFormMap.cboProdTypes3Change( Sender: TObject );
    begin
      dbcout( 'TFormMap.cboProdTypesChange', DBSHOWMSG );

      drawScreen(false);
    end
  ;


  procedure TFormMap.setScrollProperties();
    begin
      sbZoomOut.Enabled := ( _zoomCount > 1 );
      sbZoomIn.Enabled := ( _zoomCount < _maxZoom );

      if( availableMapHeight < pbxMap.Height ) then
        begin
          ScrollBarV.Enabled := true;
          ScrollBarV.Max := pbxMap.Height - availableMapHeight;
        end
      ;

      if( availableMapWidth < pbxMap.Width ) then
        begin
          ScrollBarH.Enabled := true;
          ScrollBarH.Max := pbxMap.Width - availableMapWidth;
        end
      ;

      ScrollBarH.LargeChange := ScrollBarH.Max div 10;
      ScrollBarV.LargeChange := ScrollBarV.Max div 10;
      ScrollBarH.SmallChange := ScrollBarH.Max div 100;
      ScrollBarV.SmallChange := ScrollBarV.Max div 100;

      if( 1 > ScrollBarH.LargeChange ) then ScrollBarH.LargeChange := 1;
      if( 1 > ScrollBarV.LargeChange ) then ScrollBarV.LargeChange := 1;
      if( 1 > ScrollBarH.SmallChange ) then ScrollBarH.SmallChange := 1;
      if( 1 > ScrollBarV.SmallChange ) then ScrollBarV.SmallChange := 1;
    end
  ;


  procedure TFormMap.sbZoomInClick(Sender: TObject);
    var
      PanelCenterX,PanelCenterY,ImageCenterX,ImageCenterY: Double;
    begin
      try
        Screen.Cursor := crHourglass;

        if( _zoomCount < _maxZoom ) then
          begin
            pbxMap.Width :=  pbxMap.Width + pbxMap.Width div _zoomCount;
            pbxMap.Height := pbxMap.Height + pbxMap.Height div _zoomCount;
            pbxMap.Picture.Bitmap.Width := pbxMap.Picture.Bitmap.Width + pbxMap.Picture.Bitmap.Width div _zoomCount;
            pbxMap.Picture.Bitmap.Height := pbxMap.Picture.Bitmap.Height + pbxMap.Picture.Bitmap.Height div _zoomCount;
            pbxMapT.Width := pbxMapT.Width + pbxMapT.Width div _zoomCount;
            pbxMapL.Height := pbxMapL.Height + pbxMapL.Height div _zoomCount;
            inc( _zoomCount );
          end
        else
          begin
            pbxMap.Width :=  pbxMap.Width;
            pbxMap.Height :=  pbxMap.Height;
            pbxMap.Picture.Bitmap.Width := pbxMap.Picture.Bitmap.Width ;
            pbxMap.Picture.Bitmap.Height := pbxMap.Picture.Bitmap.Height ;
          end
        ;

        if(Panel1.ClientWidth >= (pbxMap.Width)) then
          begin
            pbxMap.Left := 0;
            ScrollBarH.Max := 0;
            ScrollBarH.position := -pbxMap.Left;
            pbxMapT.Left := pbxMap.Left;
            ScrollBarH.Enabled := false;
          end
        else
          begin
            if( _zoomCount < _maxZoom ) then
              begin
                ScrollBarH.Enabled := true;
                PanelCenterX := ((Panel1.ClientWidth) div 2);
                ImageCenterX := (pbxMap.Left + (pbxMap.Width div 2)*(1-(1/_zoomCount)));
                pbxMap.Left := Round(pbxMap.Left - pbxMap.Width/(2*_zoomCount) + (ImageCenterX-PanelCenterX)*(1+(1/(_zoomCount-1))));
                ScrollBarH.Max := pbxMap.Width - Panel1.ClientWidth;
                ScrollBarH.position := -pbxMap.Left;
                pbxMap.Left := -ScrollBarH.Position;
                pbxMapT.Left := pbxMap.Left;
              end
            ;
          end
        ;

        if(Panel1.ClientHeight >= (pbxMap.Height)) then
          begin
            pbxMap.Top := 0;
            ScrollBarV.Max := 0;
            ScrollBarV.Position := -pbxMap.Top;
            pbxMapL.Top := pbxMap.Top;
            ScrollBarV.Enabled := false;
          end
        else
          begin
            if( _zoomCount < _maxZoom ) then
              begin
                ScrollBarV.Enabled := true;
                PanelCenterY := ((Panel1.ClientHeight) div 2);
                ImageCenterY := (pbxMap.Top + (pbxMap.Height div 2)*(1-(1/_zoomCount)));
                pbxMap.Top := Round(pbxMap.Top - pbxMap.height/(2*_zoomCount) + (ImageCenterY-PanelCenterY)*(1+(1/(_zoomCount-1))));
                ScrollBarV.Max := pbxMap.Height - Panel1.ClientHeight;
                ScrollBarV.Position := -pbxMap.Top;
                pbxMap.Top := -ScrollBarV.Position;
                pbxMapL.Top := pbxMap.Top;
              end
            ;
          end
        ;

        pbxMap.Picture.Bitmap.Width := 0;
        pbxMap.Picture.Bitmap.Height := 0;
        pbxMap.Picture.Bitmap.Width :=  pbxMap.Width;
        pbxMap.Picture.Bitmap.Height := pbxMap.Height;

        pbxInv.Height := pbxMap.Height;
        pbxInv.Width := pbxMap.Width;
        pbxInv.Picture.Bitmap.Width := 0;
        pbxInv.Picture.Bitmap.Height := 0;
        pbxInv.Picture.Bitmap.Width :=  pbxMap.Width;
        pbxInv.Picture.Bitmap.Height := pbxMap.Height;

        DrawScreen(False);

        MakeHerdLegend();

        setScrollProperties();
      finally
        screen.Cursor := crDefault;
      end;
    end
  ;



  procedure TFormMap.sbZoomOutClick(Sender: TObject);
    var
    PanelCenterX,PanelCenterY,ImageCenterX,ImageCenterY: Double;
    begin
      try
        screen.Cursor := crHourglass;

        if( _zoomCount > 1 ) then
          begin
            pbxMap.Width :=  pbxMap.Width - (pbxMap.Width div _zoomCount);
            pbxMap.Height := pbxMap.Height -  (pbxMap.Height div _zoomCount);
            pbxMap.Picture.Bitmap.Width := pbxMap.Picture.Bitmap.Width + pbxMap.Picture.Bitmap.Width div _zoomCount;
            pbxMap.Picture.Bitmap.Height := pbxMap.Picture.Bitmap.Height + pbxMap.Picture.Bitmap.Height div _zoomCount;
            pbxMapT.Width :=  pbxMapT.Width - (pbxMapT.Width div _zoomCount);
            pbxMapL.Height := pbxMapL.Height - (pbxMapL.Height div _zoomCount);
            dec( _zoomCount );
          end
        else
          begin
            pbxMap.Width :=  pbxMap.Width;
            pbxMap.Height :=  pbxMap.Height;
            pbxMap.Picture.Bitmap.Width := pbxMap.Picture.Bitmap.Width ;
            pbxMap.Picture.Bitmap.Height := pbxMap.Picture.Bitmap.Height ;
          end
        ;
          if(Panel1.ClientWidth >= (pbxMap.Width)) then
            begin
              pbxMap.Left := 0;
              pbxMapT.Left := pbxMap.Left;
              ScrollBarH.Max := 0;
              ScrollBarH.position := -pbxMap.Left;
              ScrollBarH.Enabled := false;
            end
          else
            begin
              ScrollBarH.Enabled := true;
              PanelCenterX := ((Panel1.ClientWidth) div 2);
              ImageCenterX := (pbxMap.Left + (pbxMap.Width div 2)*((_zoomCount+1)/_zoomCount));
              pbxMap.Left := Round(pbxMap.Left + pbxMap.Width/(2*_zoomCount) - (ImageCenterX-PanelCenterX)*(1- (1/(_zoomCount + 1)))/2);
              ScrollBarH.Max := pbxMap.Width - Panel1.ClientWidth;
              ScrollBarH.position := -pbxMap.Left;
              pbxMapT.Left := pbxMap.Left;
              if(ScrollBarH.Position <= 0) then
                begin
                  pbxMap.Left := 0;
                  pbxMapT.Left := 0;
                end
              ;
              if(-pbxMap.Left > ScrollBarH.Max) then
              begin
              pbxMap.Left := -ScrollBarH.Position;
              pbxMapT.Left := -ScrollBarH.Position;
              end;
            end
          ;

        if(Panel1.ClientHeight >= (pbxMap.Height)) then
          begin
            pbxMap.Top := 0;
            pbxMapL.Top := pbxMap.Top;
            ScrollBarV.Max := 0;
            ScrollBarV.Position := -pbxMap.Top;
            ScrollBarV.Enabled := false;
          end
        else
          begin
            ScrollBarV.Enabled := true;
            PanelCenterY := ((Panel1.ClientHeight) div 2);
            ImageCenterY := (pbxMap.Top + (pbxMap.Height div 2)*((_zoomCount+1)/_zoomCount));
            pbxMap.Top := Round(pbxMap.Top + pbxMap.Height/(2*_zoomCount) - (ImageCenterY-PanelCenterY)*(1- (1/(_zoomCount + 1)))/2);
            ScrollBarV.Max := pbxMap.Height - Panel1.ClientHeight;
            ScrollBarV.Position := -pbxMap.Top;
            pbxMapL.Top := pbxMap.Top;
            if(ScrollBarV.Position <= 0) then
              begin
                pbxMap.Top := 0;
                pbxMapL.Top := 0;
              end
            ;
            if(-pbxMap.Top > ScrollBarV.Max) then
            begin
            pbxMap.Top := -ScrollBarV.Position;
            pbxMapL.Top := -ScrollBarV.Position;
            end;
          end
        ;
        pbxMap.Picture.Bitmap.Width := 0;
        pbxMap.Picture.Bitmap.Height := 0;
        pbxMap.Picture.Bitmap.Width :=  pbxMap.Width;
        pbxMap.Picture.Bitmap.Height := pbxMap.Height;

        pbxInv.Height := pbxMap.Height;
        pbxInv.Width := pbxMap.Width;
        pbxInv.Picture.Bitmap.Width := 0;
        pbxInv.Picture.Bitmap.Height := 0;
        pbxInv.Picture.Bitmap.Width :=  pbxMap.Width;
        pbxInv.Picture.Bitmap.Height := pbxMap.Height;

        DrawScreen(False);

        MakeHerdLegend();

        setScrollProperties();
      finally
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormMap.ScrollBarHScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    begin
      pbxMap.Left := -ScrollbarH.Position;
      pbxMapT.Left := -ScrollBarH.Position;
    end
  ;


  procedure TFormMap.ScrollBarVScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    begin
      pbxMap.Top := -ScrollBarV.Position;
      pbxMapL.Top := -ScrollBarV.Position;
    end
  ;


  procedure TFormMap.pbxMapTPaint(Sender: TObject);
    begin
      DrawXAxisScale();
    end
  ;

  procedure TFormMap.pbxMapLPaint(Sender: TObject);
    begin
      DrawYAxisScale();
    end
  ;


  procedure TFormMap.pbxTopBackgroundPaint(Sender: TObject);
    begin
      pbxTopBackground.Canvas.Pen.Color := clBlack;
      pbxTopBackground.Canvas.Pen.Style := psSolid;
      pbxTopBackground.Canvas.Pen.Width := 1;
      pbxTopBackground.Canvas.Brush.Color := clWhite;
      pbxTopBackground.Canvas.MoveTo(0,pbxTopBackground.Height-1);
      pbxTopBackground.Canvas.LineTo(pbxTopBackground.Width,pbxTopBackground.Height-1);
    end
  ;


  procedure TFormMap.pbxLeftBackgroundPaint(Sender: TObject);
    begin
      pbxLeftBackground.Canvas.Pen.Color := clBlack;
      pbxLeftBackground.Canvas.Pen.Style := psSolid;
      pbxLeftBackground.Canvas.Pen.Width := 1;
      pbxLeftBackground.Canvas.Brush.Color := clWhite;

      pbxLeftBackground.Canvas.MoveTo(pbxLeftBackground.Left+pbxLeftBackground.Width-1,0);
      pbxLeftBackground.Canvas.LineTo(pbxLeftBackground.Left+pbxLeftBackground.Width-1,pbxLeftBackground.Height);
    end
  ;


  function TFormMap.getAvailableMapHeight(): integer;
    begin
      result := Panel1.ClientHeight;
    end
  ;


  function TFormMap.getAvailableMapWidth(): integer;
    begin
      result := Panel1.ClientWidth;
    end
  ;


  procedure TFormMap.fitMapToWindow();
    var
      rangeH, rangeW: double;
      hRatio, wRatio: double;
      adjRatio: double;
    begin
      // Get the latitude/vertical/height distance covered by the population
      if( _herds.maxY <> _herds.minY ) then
        rangeH := abs( _herds.maxY - _herds.minY )
      else
        rangeH := 1.0
      ;

      // Get the longitude/horizontal/width distance covered by the population
      if( _herds.maxX <> _herds.minX ) then
        rangeW := abs( _herds.maxX - _herds.minX )
      else
        rangeW := 1.0
      ;

      // If 1 >= hRatio, the available map is large enough to encompass the entire vertical space.
      // If 1 < hRatio, the available map is too small in the vertical dimension.
      hRatio := rangeH / availableMapHeight;

      // If 1 >= wRatio, the available map is large enough to encompass the entire horizontal space.
      // If 1 < wRatio, the available map is too small in the horizontal dimension.
      wRatio := rangeW /availableMapWidth;


      dbcout( rangeH, DBSHOWMSG );
      dbcout( availableMapHeight, DBSHOWMSG );
      dbcout( hRatio, DBSHOWMSG );
      dbcout( endl, DBSHOWMSG );
      dbcout( rangeW, DBSHOWMSG );
      dbcout( availableMapWidth, DBSHOWMSG );
      dbcout( wRatio, DBSHOWMSG );
      

      // Scale the area based on whichever dimension has the largest ratio.
      adjRatio := max( hRatio, wRatio );
      pbxMap.Height := round( rangeH / adjRatio );
      pbxMap.Width := round( rangeW / adjRatio );

      // Reset the position of the map in the upper left corner
      pbxMap.Left := 0;
      pbxMap.Top := 0;

      pbxInv.Height := pbxMap.Height;
      pbxInv.Width := pbxMap.Width;

      // Reset the paintboxes for the scales
      pbxMapT.Width := pbxMap.Width + 5;
      pbxMapL.Height := pbxMap.Height + 10;
      pbxMapT.Left := 0;
      pbxMapL.Top := 0;

      // Reset the scrollbars
      ScrollBarH.Position := 0;
      ScrollBarV.Position := 0;

      ScrollBarH.Max := 0;
      ScrollBarV.Max := 0;

      ScrollBarH.Enabled := false;
      ScrollBarV.Enabled := false;

      // Reset zoom count
      _zoomCount := 1;
    end
  ;


  procedure TFormMap.sbFitToWindowClick(Sender: TObject);
    begin
      try
        screen.Cursor := crHourglass;

        fitMapToWindow();

        pbxMap.Picture.Bitmap.Width := 0;
        pbxMap.Picture.Bitmap.Height := 0;
        pbxMap.Picture.Bitmap.Width :=  pbxMap.Width;
        pbxMap.Picture.Bitmap.Height := pbxMap.Height;

        pbxInv.Picture.Bitmap.Width := 0;
        pbxInv.Picture.Bitmap.Height := 0;
        pbxInv.Picture.Bitmap.Width :=  pbxInv.Width;
        pbxInv.Picture.Bitmap.Height := pbxInv.Height;

        setScrollProperties();

        DrawScreen(False);

        MakeHerdLegend();
      finally
        screen.Cursor := crDefault;
      end;
    end
  ;


  procedure TFormMap.pbxMapAxisPaint(Sender: TObject);
    begin
      DrawAxisLabels();
    end
  ;


  procedure TFormMap.pbxMapInfoPaint(Sender: TObject);
    begin
      drawXScaleBar();
    end
  ;

  procedure TFormMap.clearZones();
    begin
      _perimeterListC := nil;

      // Delete the copy of the old perimeter list data structure.
      if( nil <> _perimeterList ) then
        freeAndNil( _perimeterList )
      ;

      _usingZonesCopy := false;

      updateCaption();
    end
  ;

  procedure TFormMap.drawZones( p: THRD_PerimeterList );
    begin
      _perimeterListC := p;
      _usingZonesCopy := false;

      drawZonesInternal();
      updateCaption();
    end
  ;

  procedure TFormMap.copyZones();
    var
      mStream: TMemoryStream;
    begin
      // Create a copy of the perimeter list data structure.
      if( nil <> _perimeterList ) then
        freeAndNil( _perimeterList )
      ;
      _perimeterList := TZonePerimeterList.create( _perimeterListC );

      _smdb.clearZonePerimeters();

      mStream := TMemoryStream.Create();
      mStream.Position := 0;

      _perimeterList.saveToStream( mStream );

      _smdb.writeBlobFromStream( mStream, 'dynBlob', 'zonePerimeters' );

      mStream.Free();

      _perimeterListC := nil;

      _usingZonesCopy := true;

      updateCaption();
    end
  ;


  procedure TFormMap.drawZonesInternal();
    var
      I, J, K: integer;
      MyVertices: ^gpc_vertex_list;
      ZoneSize: Integer;
      Zone: ZON_zone_t_ptr;
      penPoint: TPoint;
      perim: TZonePerimeter;
    begin
      pbxMap.Picture := pbxInv.Picture;

      if( nil = _perimeterListC ) then
        begin
          if( _usingZonesCopy ) then
            begin
              // Draw the zones from the copied data structure
              //----------------------------------------------

              // Are there any zones to draw?
              if( nil = _perimeterList ) then
                exit
              ;

              if( 0 = _perimeterList.count ) then
                exit
              ;

              // If so, draw all of the zones
              for j := 0 to _perimeterList.count - 1 do
                begin
                  perim := _perimeterList[j] as TZonePerimeter;

                  pbxMap.Canvas.Pen.Color := zoneColor( perim.zoneLevel );
                  pbxMap.Canvas.Pen.Width := 1;
                  pbxMap.Canvas.Pen.Style := psSolid;

                  //dbcout2( endl + '------ Arbitrary polgon with ' + intToStr( perim.count ) + ' vertices.' );

                  if ( 0 < perim.count ) then
                    begin
                      // Set the first vertex position
                      penPoint.X := ScrX( perim[0].x );
                      penPoint.Y := ScrY( perim[0].y );
                      pbxMap.Canvas.PenPos := penPoint;

                      //dbcout2( '  ' + dbFloatToStr( perim[0].x ) + tab + dbFloatToStr( perim[0].y ) );

                      // Draw the line segments
                      for i := 1 to perim.count - 1 do
                        begin
                          pbxMap.Canvas.lineTo( scrX( perim[i].x ), scrY( perim[i].y ) );
                          //dbcout2( '  ' + dbFloatToStr( perim[i].x ) + tab + dbFloatToStr( perim[i].y ) );
                        end
                      ;

                      // Connect the last vertex to the first one
                      pbxMap.Canvas.lineTo( scrX( perim[0].x ), scrY( perim[0].y ) );
                    end
                  ;
                end
              ;
            end
          else
            begin
              // If _perimeterListC is nil and _usingZonesCopy is false, don't draw anything.
              // This is normal, and should NOT raise an exception. (I think...)
              //raise exception.create( 'I can''t draw any zones!' );
              exit;
            end
          ;
        end
      else
        begin
          // Draw the zones from the data structure in the DLL
          //--------------------------------------------------

          // Are there any zones to draw?
          if ( assigned( _perimeterListC ) )  then
            ZoneSize := Get_zone_list_size( _perimeterListC )
          else
            ZoneSize := 0
          ;

          if ( 0 < ZoneSize ) then
            begin
              // If so, draw all of the zones
              for j := 0 to (ZoneSize - 1) do
                begin
                  Zone := Get_zone_list_zone( _perimeterListC, j );

                  pbxMap.Canvas.Pen.Color := zoneColor( Zone^.level );
                  pbxMap.Canvas.Pen.Width := 1;
                  pbxMap.Canvas.Pen.Style := psSolid;

                  if ( Zone^.poly^.num_contours > 0 ) then
                    begin
                      for k := 0 to Zone^.poly^.num_contours - 1 do
                        begin

                          MyVertices := @Zone^.poly^.contour[k];
                          if( 0 < MyVertices^.num_vertices ) then
                            begin
                              // FIX ME: consider using TCanvas.Polyline

                              //dbcout2( 'vertex 0: ' + usFloatToStr( MyVertices^.vertex[0].x ) + ', ' + usFloatToStr( MyVertices^.vertex[0].y ) );

                              // Set the first vertex position
                              penPoint.X := ScrX( MyVertices^.vertex[0].x );
                              penPoint.Y := ScrY( MyVertices^.vertex[0].y );
                              pbxMap.Canvas.PenPos := penPoint;

                              // Draw the line segments
                              for i := 1 to MyVertices.num_vertices - 1 do
                                pbxMap.Canvas.lineTo( scrX( MyVertices^.vertex[i].x ), scrY( MyVertices^.vertex[i].y ) )
                              ;

                              // Connect the last vertex to the first one
                              pbxMap.Canvas.lineTo( scrX( MyVertices^.vertex[0].x ), scrY( MyVertices^.vertex[0].y ) );
                            end
                          ;
                        end
                      ;
                    end
                  ;
                end
              ;
            end
          ;
        end
      ;
    end
  ;


  procedure TFormMap.displayHerdInfo();
    var
      i, maxi: integer;
      theHint: string;
      pt: TPoint;
    begin
      if( 0 < _lastHintPos.x ) then
        begin
          theHint := '';

          HintManager.HintHidePause := 10 * 60 * 1000; // 10 minutes
          HintManager.HintPause := 10;
          HintManager.CursorTolerance := 3;
          SetHintWindowClass( TMdHintWindow );

          maxi := _herdLocations.depth( _lastHintPos.x, _lastHintPos.y );
          if( 0 = maxi ) then
            theHint := '(There are no units at this location)'
          else if( 5 < maxi ) then
            begin
              theHint :=
                  '(There are too many units at these' + endl
                + ' coordinates to display unit information)'
              ;
            end
          else
            begin
              for i := 0 to maxi - 1 do
                begin
                  theHint := theHint + _herds.at( _herdLocations.arrayValue( _lastHintPos.x, _lastHintPos.y, i ) ).briefInfo();
                  if( i < maxi - 1 ) then
                    theHint := theHint + endl + endl
                  ;
                end
              ;
            end
          ;

          self.hint := theHint;

          pt := pbxMap.ClientToScreen( _lastHintPos );

          Application.ActivateHint(pt);
        end
      ;
    end
  ;


  procedure TFormMap.pbxMapMouseDown(
        Sender: TObject;
        Button: TMouseButton;
        Shift: TShiftState;
        X, Y: Integer
      );
    begin
      _lastHintPos.x := x;
      _lastHintPos.y := y;

      displayHerdInfo();
    end
  ;


  procedure TFormMap.pbxMapMouseMove(
        Sender: TObject;
        Shift: TShiftState;
        X, Y: Integer
      );
    begin
    // For reasons that I don't understand, a mouseMove event is triggered immediately after
    // a mouseDown event, even if the cursor hasn't moved.  The approach used here will hide
    // a hint if the cursor is  moved too far from the point where the initial click occurred.
    if( ( 3 < abs( x - _lastHintPos.x ) ) or ( 3 < abs( y - _lastHintPos.y ) ) ) then
      begin
        _lastHintPos.x := -1;
        _lastHintPos.y := -1;

        application.CancelHint();

        // Restore defaults
        SetHintWindowClass( THintWindow );
        HintManager.CursorTolerance := 1;
        HintManager.HintHidePause := 2500;
        HintManager.HintPause := 500;
      end
    ;
    end
  ;


  procedure TFormMap.WMNCPaint( var Msg: TWMNCPaint );
    begin
      if( not _borderDisabled ) then
        inherited
      ;
    end
  ;

  procedure TFormMap.actKMZExportExecute(Sender: TObject);
    var
      KMLGen: TKMLFileGenerator;
      currentPTID, i : integer;

    begin
      i := cboProdTypes.ItemIndex;

      if (0 > i)
      then currentPTID := (cboProdTypes.Items.Objects[i] as TProductionType).productionTypeID
      else currentPTID := -1; //all production types has no assoc object

      if Assigned( _herds ) then
        begin
          saveDialog1.Title := tr( 'Choose a filename for your KMZ map file' );
          saveDialog1.InitialDir := GetCurrentDir;
          saveDialog1.DefaultExt := 'kmz';
          saveDialog1.Filter := tr( 'KMZ file (*.kmz)|*.kmz' );
          saveDialog1.FilterIndex := 1;

          if saveDialog1.Execute then
            begin
              try
                if ( 0 > currentPTID )
                then KMLGen := TKMLFileGenerator.create( _herds )  // all prod types
                else KMLGen := TKMLFileGenerator.create( _herds, currentPTID ); // a particular prod type

                KMLGen.generateHerdKMZFile(saveDialog1.FileName);
              finally
                FreeAndNil(KMLGen);
              end;
            end
          ;  //execute
        end
      ; //assigned
    end
  ;

end.
