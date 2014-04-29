unit FormMap;

(*
FormMap.pas/dfm
---------------
Begin: 2005/05/25
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version: $Revision: 1.45 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
Author: Snehal Shetye <snehal@goku.engr.colostate.edu>
------------------------------------------------------
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
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    ExtCtrls,
    StdCtrls,
    Buttons,

    Points,
    SparseArrays,
    
    Herd,
    StatusEnums,
    ZonePerimeter,
    SMSimulationInput,
    SMDatabase,

    MdHintWn,
    Zone
  ;


  type TFormMap = class( TForm )
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
      ScrollBarH: TScrollBar;
      ScrollBarV: TScrollBar;
      pbxLeftBackground: TPaintBox;
      Panel5: TPanel;
      pbxMapAxis: TPaintBox;
      Panel6: TPanel;
      pbxMapT: TPaintBox;
      pbxTopBackground: TPaintBox;
      pnlButtons: TPanel;
      sbZoomIn: TSpeedButton;
      sbZoomOut: TSpeedButton;
      sbFitToWindow: TSpeedButton;
      pnlMapInfo: TPanel;
      pbxMapInfo: TPaintBox;
      pbxInv: TImage;
      cboProdTypes: TComboBox;
      pbxZoneLegend: TPaintBox;

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

    protected
      _mouseIsDown: boolean;
      _displayApparent: boolean;
      _sizeDisplaySmall: boolean;

      _smdb: TSMDatabase;
      _sim: TSMSimulationInput;
      _herds: THerdList;
      _myMainForm: TForm;

      _minLat, _minLon, _maxLat, _maxLon: double;

      _zoomCount, _maxZoom: integer;

      _scaleFactor: double;

      _perimeterListC: THRD_PerimeterList;
      _usingZonesCopy: boolean;
      _perimeterColors: TColorArray;
      _perimeterList: TZonePerimeterList;

      _lastHintPos: TPoint;
      _updateHint: boolean;
      _herdLocations: TTwoDPlusSparseArray;

      _borderDisabled: boolean;

      procedure translateUI();

      procedure WMNCPaint( var Msg: TWMNCPaint ); message WM_NCPAINT;

      procedure TextOutAngle(Canvas: TCanvas; x,y: integer; s: string; angle: integer);

      // Primitive drawing functions, used by drawScreen
      procedure DrawBasicRectangle();
      procedure DrawYAxisLabel();
			procedure DrawXAxisLabel();
      procedure DrawXScale();
      procedure DrawAxisLabels();

      function getPtSize( hs: word ): integer;
			function getPtColor( s: TTransitionState ): TColor; overload;
      function getPtColor( s: TApparentStatus ): TColor; overload;

      procedure MakeZoneLegend();
			procedure MakeHerdLegend();
      procedure DrawLegendPoint( pbx: TPaintbox; sz: byte; x,y: integer );

      procedure setScrollProperties();
      function ScrX( Lon: double ): integer;
      function ScrY( Lat: double ): integer;
      function ScrXInfo( Lon: double ): integer;
      function ScrYLeft( Lat: double ): integer;
      function ScrXTop( Lon: double ): integer;
      function ScrXLeft( Lon: double ) : integer;
      function ScrYTop( Lat: double ): integer;
      //procedure DisplayTheHerdData( pb1, pb2 : TPaintBox; x, y : integer);
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

      property borderDisabled: boolean read _borderDisabled write _borderDisabled;
      property availableMapHeight: integer read getAvailableMapHeight;
      property availableMapWidth: integer read getAvailableMapWidth;

      property zoneColors: TColorArray read _perimeterColors;
    end
  ;

  // FIX ME: switch over to the C library version of this function??
  // FIX ME: Why is this declaration suddenly causing a compile error? 
  //function DistInKM( r1, r2: RLatLon ): double;

  var
    frmMap: TFormMap;


  const
  	DBFORMMAP: boolean = false; // set to true to enable debugging messages for this unit.

implementation
  {$R *.DFM}

  uses
    MyStrUtils,
    GuiStrUtils,
    DebugWindow,
    BasicGIS,
    I88n,

    FormMain,
    ProductionType,
    ProductionTypeList,
    ModelImplementation
  ;

  
//-----------------------------------------------------------------------------
// Global helper functions
//-----------------------------------------------------------------------------
  {
  Calculates an estimated distance in km between two points specified by lat/lon.
  AR 6/23/04

  @param R1 RLatLon containing one of the two points
  @param R2 RLatLon containing the other of the two points

  @return A real value for the distance in km
  }

  function DistInKM( r1, r2: RLatLon ): double;
    const
      CONVERSIONLATITUDE = 111.07455;
    var
      x, y: real; // distance in km, east/west, and north/south
      MeanLat, LonC: real;
      tResult: real;
      lon1, lon2: real;

    {
    Determines the latitude 1/2-way between two specified latitudes.
    AR 6/23/04

    @param lat1 real One of the two specified latitudes
    @param lat2 real The other specified latitude

    @return A real value indicating the "average" latitude in decimal degree units
    }
    function GetAverageLat( lat1, lat2: real): real;
      var
        diff, halfDiff, av: real;
      begin
        diff := lat1 - lat2;
        halfDiff := diff/2;
        av := lat1 - halfDiff;
        Result := av;
      end
    ;


    {
    /**
    Returns the number of kilometers per decimal degree of longitude at the specified latitude.

    WARNING: This function was written for the northern hemisphere, and expects a positive value
    for the latitude.  It is easy to deal with the southern hemisphere by passing the absolute value
    of the latitude, but this is not currently enforced by the function.
    AR 6/23/04

    @param Lat real value indicating the latitude in question, in decimal degrees

    @return real value indicating km per degree longitude at the specified latitude
    */
    }
    function GetConversionLongitude(Lat : real) : real;
      const
        LonConvFactor : array[0..9] of single =
          (116, 109.662, 104.196,
          95.675, 83.671, 69.369,
          51.856, 35, 19, 0); { km / dec degree at latitudes of 0, 10, 20, 30..90 }
      var
        lLat, uLat : integer;  // lower /upper latitude
        r : real;

      function Interpolate( x : real; x1, y1 : real; x2, y2  : real) : real; { return y }
        var
          b, gx, gy, m : real;
        begin
          gy := y2 - y1;
          gx := x2 - x1;
          { gy is rise }
          { gx is run }
          { m is slope }
          if gy = 0 then
            Result := y1
          else
          if gx = 0 then
            Result := gy / 2 { undefined, use average }
          else
          begin
            m := gy/gx;
           { y = mx + b }
            b := y1 - (m*x1);
            Result := (x*m) + b;
          end;
        end
      ;

      begin // GetConversionLongitude
        // get the next lowest latitude divisible by 10
        lLat := (Trunc(Lat) div 10) * 10;
        if lLat = 90 then // upper latitude not possible
          r := 0.0000001
        else
        begin
          uLat := lLat + 10;
          r := Interpolate(Lat,
            lLat, LonConvFactor[lLat div 10],
            uLat, LonConvFactor[uLat div 10]);
          if r = 0 then
            r := 1; {must give a non-zero result to prevent / by 0}
        end;
        Result := r;
      end
    ;

    begin  // DistInKM
      y := ( r1.Lat - r2.Lat ) * CONVERSIONLATITUDE;
      meanLat := GetAverageLat( r1.Lat, r2.Lat );
      LonC := GetConversionLongitude( Abs( meanLat ) ); // AR: use Abs here b/c GetConversionLongitude expects a positive number!

      lon1 := r1.Lon;
      lon2 := r2.Lon;

      if( lon1 > 180 ) then lon1 := lon1 - 360;
      if( lon2 > 180 ) then lon2 := lon2 - 360;

      x := ( lon1 - lon2 ) * LonC;
      tResult := Sqrt( Sqr( x ) + Sqr( y ) );
      Result := tResult;
    end
  ;
//-----------------------------------------------------------------------------

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
end;


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

      setLength( _perimeterColors, 4 );
      _perimeterColors[0] := clMaroon;
      _perimeterColors[1] := clTeal;
      _perimeterColors[2] := clNavy;
      _perimeterColors[3] := clOlive;

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
      pnlButtons.BevelOuter := bvNone;
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
      
      _displayApparent := False;
      //LockWindowUpdate( 0 );
      self.Perform( WM_SETREDRAW, 1, 0 );
      RedrawWindow( self.Handle, nil, 0, RDW_ERASE or RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
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
          Caption := tr( 'Apparent map of units, current/final day' );
          sbSize.Caption := tr( 'Size' );
          sbStatus.Caption := tr( 'Status' );
          sbZoomIn.Hint := tr( 'Zoom in' );
          sbZoomOut.Hint := tr( 'Zoom out' );
          sbFitToWindow.Hint := tr( 'Fit to window' );
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
      setLength( _perimeterColors, 0 );

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

      updateProdTypeList();
      drawScreen();
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
      latBuffer, lonBuffer: double;
      latDiff, lonDiff: double;
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
              latDiff := ( _herds.maxLat - _herds.minLat );
              lonDiff := ( _herds.maxLon - _herds.minLon );

              if( 10.0 > latDiff ) then
                latDiff := 10.0
              ;

              if( 10.0 > lonDiff ) then
                lonDiff := 10.0
              ;

              latBuffer := abs( 0.025 * latDiff );
              lonBuffer := abs( 0.025 * lonDiff );

              _minLat := (_herds.minLat - latBuffer);
              _maxLat := (_herds.maxLat + latBuffer);
              _minLon := (_herds.minLon - lonBuffer);
              _maxLon := (_herds.maxLon + lonBuffer);

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

      DrawXScale();
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


  function TFormMap.getPtColor( s: TTransitionState ): TColor;
    begin
      case s of
        tsSusceptible: 		result := clBlack;
        tsLatent: 				result := clYellow;
        tsSubclinical:	  result := clFuchsia;
        tsClinical: 			result := clRed;
        tsNaturalImmune:  result := clLime;
        tsVaccineImmune:  result := clBlue;
        tsDestroyed: 			result := clWhite;
        else
        	raise exception.Create( 'Unrecognized transition state (' + intToStr(ord(s)) + ') in TFormMap.getPtColor' )
        ;
      end;
    end
  ;


  function TFormMap.getPtColor( s: TApparentStatus ): TColor;
  	begin
   		case s of
        asUnknown: result := clBlack;
        asDetected: result := clGreen;
        asTracedDirect: result := clNavy;
        asTracedIndirect: result := clPurple;
        asVaccinated: result := clAqua;
        asDestroyed: result := clWhite;
        else
        	raise exception.Create( 'Unrecognized apparent status (' + intToStr(ord(s)) + ') in TFormMap.getPtColor' )
        ;
      end;
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
      	ScrX( _minLon ),
        ScrY( _minLat ),
        ScrX( _maxLon ),
        ScrY( _maxLat )
      );

      pbxInv.Canvas.Pen.Color := clBlack;
      pbxInv.Canvas.Pen.Width := 1;
      pbxInv.Canvas.Pen.Style := psSolid;
      pbxInv.Canvas.Brush.Style := bsSolid;
      pbxInv.Canvas.Brush.Color := clBtnFace;

      pbxInv.Canvas.Rectangle(
      	ScrX( _minLon ),
        ScrY( _minLat ),
        ScrX( _maxLon ),
        ScrY( _maxLat )
      );
    end
  ;


  procedure TFormMap.DrawYAxisLabel();
    var
      n : integer;
      s : string;
      scale : integer;
      w : integer;
      xp, yp : integer;
    begin
      pbxMapL.Canvas.Pen.Color := clBlack;
      pbxMapL.Canvas.Pen.Style := psSolid;
      pbxMapL.Canvas.Pen.Width := 1;
      pbxMapL.Canvas.Brush.Color := clWhite;

      // Draw the vertical line at the right edge of the paintbox
      pbxMapL.Canvas.MoveTo(pbxMapL.Left+pbxMapL.Width-1,0);
      pbxMapL.Canvas.LineTo(pbxMapL.Left+pbxMapL.Width-1,pbxMapL.Height);

      n := Trunc( _minLat * 10);
      if( _zoomCount > 3 ) then
        scale := 5
      else
        scale := 10
      ;
      while n < ( _maxLat * 10) do
        begin
          if (n div scale) = (n/scale) then
            begin
              Str( n/10 : 5 : 1, s);
              s := Trim(s);
              w := pbxMapL.canvas.TextWidth(s);
              //xp := ScrXLeft( _minLon );
              xp := pbxMapL.Width;
              yp := ScrYLeft( n/10 );

              // Draw a tickmark
              pbxMapL.canvas.MoveTo( xp, yp );
              pbxMapL.canvas.LineTo( xp - 5, yp );

              // Write the text
              pbxMapL.canvas.TextOut( xp - (w + 5), yp - 5, s );
            end
          ;
          n := n + 1;
        end
      ;
    end
  ;


  procedure TFormMap.DrawXAxisLabel();
    var
      n: integer;
      s: string;
      scale: integer;
      xp, yp: integer;
      halfTextHeight: integer;
    begin
      pbxMapT.Canvas.Pen.Color := clBlack;
      pbxMapT.Canvas.Pen.Style := psSolid;
      pbxMapT.Canvas.Pen.Width := 1;
      pbxMapT.Canvas.Brush.Color := clWhite;
      pbxMapT.Canvas.MoveTo(0,pbxMapT.Height-1);
      pbxMapT.Canvas.LineTo(pbxMapT.Width,pbxMapT.Height-1);

      halfTextHeight := pbxMapT.Canvas.TextHeight( 'A' ) div 2;

      // counter
      n := Trunc( _minLon * 10);
      if( _zoomCount > 3 ) then
        scale := 5
      else
        scale := 10
      ;
      while n < ( _maxLon * 10) do
        begin
          if (n div scale) = (n/scale) then
            begin
              Str(n/10 : 5 : 1, s);
              s := Trim(s);
              xp := ScrXTop( n/10 );
              //yp := ScrYTop( _minLat );
              yp := pbxMapT.Height;
              pbxMapT.Canvas.MoveTo(xp, yp);
              pbxMapT.Canvas.LineTo(xp, yp - 5);

              textOutAngle( pbxMapT.Canvas, xp - halfTextHeight, yp - 7, s, 900 );
            end
          ;
          n := n + 1;
        end
      ;
    end
  ;


  procedure TFormMap.DrawXScale();
    var
      LRDist, Prop, MeanLat : real;
      LL1, LL2 : RLatLon;
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
      MeanLat := ( _minLat + _maxLat )/2;
    	LL1.Lat := MeanLat;
      LL1.Lon := _minLon;

      LL2.Lat := MeanLat;
      LL2.lon := _maxLon;

      LRDist := DistInKM( LL1, LL2 );

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
        dbcout( 't = ' + uiFloatToStr( t ), true );
        dbcout( 'LRDist = ' + uiFloatToStr( LRDist ), true );
        Prop := 1.0;
      end;


      // Convert the 1/10 distance into screen coordinates.
      distScreenCoords := Round( Prop * ( ScrXTop( _maxLon ) - ScrXTop( _minLon ) ) );

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


  //Draws the axes Labels at the top left corner.
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
          pbxZoneLegend.Canvas.Pen.Color := _perimeterColors[ (zList.at(i).level - 1) mod length( _perimeterColors ) ];
          pbxZoneLegend.Canvas.Brush.Color := pbxZoneLegend.Canvas.Pen.Color;

          DrawLegendPoint( pbxZoneLegend, 2,NewLeft + trunc(i/3)* maxWidth, NTop + ((i mod 3) * vspace) + HalfVS);
          DrawLegendPoint( pbxZoneLegend, 2,NewLeft + trunc(i/3)* maxWidth + 2, NTop + ((i mod 3) * vspace) + HalfVS);

          pbxZoneLegend.Canvas.Pen.Color := clBlack;
          pbxZoneLegend.Canvas.Brush.Color := clBtnFace;

          pbxZoneLegend.Canvas.TextOut(NewLeft + trunc(i/3)* maxWidth + round( 10 * _scaleFactor ),NTop + ((i mod 3 ) * vspace),zList.at(i).descr );
        end;
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

      if( _displayApparent ) then
        begin
          // First column
          //--------------
          pbxLegend.Canvas.Pen.Color := GetPtColor( asUnknown );
          pbxLegend.Canvas.Brush.Color := GetPtColor( asUnknown );
          DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( asDetected );
          pbxLegend.Canvas.Brush.Color := GetPtColor( asDetected );
          DrawLegendPoint( pbxLegend, 2, NewLeft, Ntop + (1*vspace) + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( asDestroyed );
          pbxLegend.Canvas.Brush.Color := GetPtColor( asDestroyed );
          DrawLegendPoint( pbxLegend, 2, NewLeft, NTop + (2*vspace) + HalfVS);

          // corresponding text
          pbxLegend.Canvas.Pen.Color := clBlack;
          pbxLegend.Canvas.Brush.Color := clBtnFace;
          Inc(NewLeft, round( 7 * _scaleFactor ) );
          pbxLegend.Canvas.TextOut(NewLeft, Ntop, tr( 'Unknown' ) );
          pbxLegend.Canvas.TextOut(NewLeft, NTop + vspace, tr( 'Detected' ) );
          pbxLegend.Canvas.TextOut(NewLeft, NTop + (2*vspace),tr( 'Destroyed' ) );

          // Second column
          //--------------
          Inc(NewLeft, pbxLegend.Canvas.TextWidth( tr( 'Destroyed' ) ) + round( 10 * _scaleFactor ) );

          pbxLegend.Canvas.Pen.Color := GetPtColor( asVaccinated );
          pbxLegend.Canvas.Brush.Color := GetPtColor( asVaccinated );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( asTracedDirect );
          pbxLegend.Canvas.Brush.Color := GetPtColor( asTracedDirect );
          DrawLegendPoint( pbxLegend, 2,NewLeft, Ntop + (1*vspace) + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( asTracedIndirect );
          pbxLegend.Canvas.Brush.Color := GetPtColor( asTracedIndirect );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + (2*vspace) + HalfVS);

          // corresponding text
          pbxLegend.Canvas.Pen.Color := clBlack;
          pbxLegend.Canvas.Brush.Color := clBtnFace;
          Inc(NewLeft,7);
          pbxLegend.Canvas.TextOut(NewLeft,Ntop, tr( 'Vaccinated' ) );
          pbxLegend.Canvas.TextOut(NewLeft,NTop + vspace, tr( 'Traced - Direct' ) );
          pbxLegend.Canvas.TextOut(NewLeft,NTop + (2*vspace), tr( 'Traced - Indirect' ) );
        end
      else
        begin
        	// First column
          //-------------
          pbxLegend.Canvas.Pen.Color := GetPtColor( tsSusceptible );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsSusceptible );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( tsLatent );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsLatent );
          DrawLegendPoint( pbxLegend, 2,NewLeft, Ntop + (1*vspace) + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( tsSubclinical );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsSubclinical );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + (2*vspace) + HalfVS);

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

          pbxLegend.Canvas.Pen.Color := GetPtColor( tsClinical );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsClinical );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( tsNaturalImmune );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsNaturalImmune );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + vspace + HalfVS);

          pbxLegend.Canvas.Pen.Color := GetPtColor( tsVaccineImmune );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsVaccineImmune );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + 2 * vspace + HalfVS);

          // corresponding text
          pbxLegend.Canvas.Pen.Color := clBlack;
          pbxLegend.Canvas.Brush.Color := clBtnFace;

          Inc(NewLeft,7);
          pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2, tr( 'Clinical' ) );
          pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2 + vspace,tr( 'Natural immune' ) );
          pbxLegend.Canvas.TextOut( NewLeft, NTop + 2 + (2*vspace), tr( 'Vaccine immune' ) );

          // Third column
          //-------------
          inc( NewLeft, pbxLegend.Canvas.TextWidth( tr( 'Vaccine immune') ) + round( 10 * _scaleFactor ) );

          pbxLegend.Canvas.Pen.Color := GetPtColor( tsDestroyed );
          pbxLegend.Canvas.Brush.Color := GetPtColor( tsDestroyed );
          DrawLegendPoint( pbxLegend, 2,NewLeft, NTop + HalfVS);

          // corresponding text
          pbxLegend.Canvas.Pen.Color := clBlack;
          pbxLegend.Canvas.Brush.Color := clBtnFace;

          Inc(NewLeft, round( 7 * _scaleFactor ) );
          pbxLegend.Canvas.TextOut( NewLeft, Ntop + 2, tr( 'Destroyed' ) );
        end
      ;

      if( _displayApparent ) then
        Self.Caption := tr( 'Apparent map of units, current/final day' )
      else
        Self.Caption := tr( 'Map of units, current/final day' )
      ;
    end
  ;
//-----------------------------------------------------------------------------




  //Used to draw the X Axis labels at the correct Lat and Lon for the Top PaintBox.
  function TFormMap.ScrXTop( Lon: double ): integer;
    var
      LonRange : single;
      ScrXRange : integer;
    begin
      LonRange := _maxLon - _minLon;
      ScrXRange := pbxMapT.Width;
      if LonRange = 0 then LonRange := 0.00001;
      Result := Round( ( ( Lon - _minLon ) / LonRange) * ScrXRange );
    end
  ;

  //Used to draw the Y Axis labels at the correct Lat and Lon for the Top PaintBox.
  function TFormMap.ScrYTop( Lat: double ): integer;
    var
      LatRange : single;
      ScrYRange : integer;
    begin
      LatRange := _maxLat - _minLat;
      ScrYRange := pbxMapT.Height;
      if LatRange = 0 then LatRange := 0.00001;
      Result := Round((1-((Lat - _minLat)/ LatRange))*ScrYRange);
    end
  ;

  //Used to draw the X Axis labels at the correct Lat and Lon for the Left PaintBox.
   function TFormMap.ScrXLeft( Lon: double ): integer;
    var
      LonRange : single;
      ScrXRange : integer;
    begin
      LonRange := _maxLon - _minLon;
      ScrXRange := pbxMapL.Width;
      if LonRange = 0 then LonRange := 0.00001;
      Result := 30 + Round( ( ( Lon - _minLon ) / LonRange) * ScrXRange );
    end
  ;
  //Used to draw the Y Axis labels at the correct Lat and Lon for the Left PaintBox.
  function TFormMap.ScrYLeft( Lat: double ): integer;
    var
      LatRange : single;
      ScrYRange : integer;
    begin
      LatRange := _maxLat - _minLat;
      ScrYRange := pbxMapL.Height;
      if LatRange = 0 then LatRange := 0.00001;
      Result := Round((1-((Lat - _minLat)/ LatRange))*ScrYRange);
    end
  ;

  //Used to draw the X Axis labels at the correct Lat and Lon for the Info PaintBox.
  function TFormMap.ScrXInfo( Lon: double ): integer;
    var
      LonRange : single;
      ScrXRange : integer;
    begin
      LonRange := _maxLon - _minLon;
      ScrXRange := pbxMapInfo.Width;
      if LonRange = 0 then LonRange := 0.00001;
      Result := Round( ( ( Lon - _minLon ) / LonRange) * ScrXRange );
    end
  ;

  function TFormMap.ScrY( Lat: double ): integer;
    var
      LatRange : single;
      ScrYRange : integer;
    begin
      LatRange := _maxLat - _minLat;
      ScrYRange := pbxMap.Height;
      if LatRange = 0 then LatRange := 0.00001;
      Result := Round((1-((Lat - _minLat)/ LatRange))*ScrYRange);
    end
  ;

   function TFormMap.ScrX( Lon: double ): integer;
    var
      LonRange : single;
      ScrXRange : integer;
    begin
      LonRange := _maxLon - _minLon;
      ScrXRange := pbxMap.Width;
      if LonRange = 0 then LonRange := 0.00001;
      Result := Round( ( ( Lon - _minLon ) / LonRange) * ScrXRange );
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
      if( _displayApparent ) then
        begin
          //dbcout( 'Setting color based on apparent status' );
      	  pointColor := getPtColor( h.apparentStatus );
        end
      else
        begin
          //dbcout( 'Setting color based on simulated status ' + transitionStateCode( h.simulatedStatus ) );
      	  pointColor := getPtColor( h.simulatedStatus );
        end
      ;

      pbxMap.Canvas.Brush.Color := pointColor;
      pbxMap.Canvas.Pen.Color := pointColor;

      pbxMap.Canvas.Rectangle(
        ScrX( h.Lon ) - pointSize,
        ScrY( h.Lat ) - pointSize,
        ScrX( h.Lon ) + pointSize,
        ScrY( h.Lat ) + pointSize
      );

      pbxInv.Canvas.Brush.Color := pointColor;
      pbxInv.Canvas.Pen.Color := pointColor;

      pbxInv.Canvas.Rectangle(
        ScrX( h.Lon ) - pointSize,
        ScrY( h.Lat ) - pointSize,
        ScrX( h.Lon ) + pointSize,
        ScrY( h.Lat ) + pointSize
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
          for x := ( ScrX( h.Lon ) - pointSize ) to ( ScrX( h.Lon ) + pointSize ) do
            begin
              for y := ( ScrY( h.Lat ) - pointSize ) to ( ScrY( h.Lat ) + pointSize ) do
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
      h: THerd;
      hIdx: integer;
    begin
      _herdLocations.deleteValues();
      _updateHint := false;

      for hIdx := 0 to _herds.Count - 1 do
        begin
          h := _herds.at( hIdx );
          //h.debug();
          drawThisUnit( h, hIdx );
        end
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
      _displayApparent := not _displayApparent;
      pbxLegend.Repaint();

      DrawScreen(false);
    end
  ;


  procedure TFormMap.sbSizeClick(Sender: TObject);
    begin
      _sizeDisplaySmall := not _sizeDisplaySmall;
      pbxSizeLegend.Repaint();

      DrawScreen(false);
    end
  ;


  procedure TFormMap.cboProdTypes3Change( Sender: TObject );
    begin
      dbcout( 'TFormMap.cboProdTypesChange', DBFORMMAP );

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
      DrawXAxisLabel();
    end
  ;

  procedure TFormMap.pbxMapLPaint(Sender: TObject);
    begin
      DrawYAxisLabel();
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
      llRangeH, llRangeW: double;
      latLon1, latLon2: RLatLon;
      aspect: double;
    begin
      // Get the latitude/vertical/height distance covered by the population
      latLon1.Lat := _herds.minLat;
      latLon1.Lon := _herds.maxLon;
      latLon2.Lat := _herds.maxLat;
      latLon2.Lon := _herds.maxLon;
      llRangeH := DistInKM( latLon1, latLon2 );

      // Get the longitude/horizontal/width distance covered by the population
      latLon1.Lat := _herds.maxLat;
      latLon1.Lon := _herds.minLon;
      latLon2.Lat := _herds.maxLat;
      latLon2.Lon := _herds.maxLon;
      llRangeW := DistInKM( latLon1, latLon2 );

      // Calculate aspect ratio as h/w
      // if aspect = 1, the map will be square
      // if aspect > 1, the map will be taller than it is wide
      // if aspect < 1, the map will be wider than it is tall

      // These adjustments hopefully keep the map from getting ridiculously narrow
      if( 1 > llRangeH ) then llRangeH := 1;
      if( 1 > llRangeW ) then llRangeW := 1;
      aspect := llRangeH / llRangeW;

      if( aspect > 1 ) then
        begin
          // Set the height of the map image control to the maximum available space
          pbxMap.Height := availableMapHeight;

          // Set the width based on the aspect ratio
          pbxMap.Width := round( availableMapHeight / aspect );

          // See if width is a limiting factor, and adjust if necessary
          if( pbxMap.Width > availableMapWidth ) then
            begin
              pbxMap.Width := availableMapWidth;
              pbxMap.Height := round( availableMapWidth * aspect );
            end
          ;
        end
      else
        begin
          // Set the width of the map image control to the maximum available space
          pbxMap.Width := availableMapWidth;

          // Set the height based on the aspect ratio
          pbxMap.Height := round( availableMapWidth * aspect );

          // See if height is a limiting factor, and adjust if necessary
          if( pbxMap.Height > availableMapHeight ) then
            begin
              pbxMap.Height := availableMapHeight;
              pbxMap.Width := round( availableMapWidth * aspect );
            end
          ;
        end
      ;

      // Reset the position of the map in the upper left corner
      pbxMap.Left := 0;
      pbxMap.Top := 0;

      pbxInv.Height := pbxMap.Height;
      pbxInv.Width := pbxMap.Width;

      // Reset the paintboxes for the scales
      pbxMapT.Width := pbxMap.Width;
      pbxMapL.Height := pbxMap.Height;
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
      drawXScale();
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
    end
  ;

  procedure TFormMap.drawZones( p: THRD_PerimeterList );
    begin
      _perimeterListC := p;
      _usingZonesCopy := false;

      drawZonesInternal();
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

                  pbxMap.Canvas.Pen.Color := _perimeterColors[ (perim.zoneLevel - 1) mod length( _perimeterColors ) ];
                  pbxMap.Canvas.Pen.Width := 1;
                  pbxMap.Canvas.Pen.Style := psSolid;

                  //dbcout2( endl + '------ Arbitrary polgon with ' + intToStr( perim.count ) + ' vertices.' );

                  if ( 0 < perim.count ) then
                    begin
                      // Set the first vertex position
                      penPoint.X := ScrX( perim[0].x );
                      penPoint.Y := ScrY( perim[0].y );
                      pbxMap.Canvas.PenPos := penPoint;

                      //dbcout2( '  ' + uiFloatToStr( perim[0].x ) + tab + uiFloatToStr( perim[0].y ) );

                      // Draw the line segments
                      for i := 1 to perim.count - 1 do
                        begin
                          pbxMap.Canvas.lineTo( scrX( perim[i].x ), scrY( perim[i].y ) );
                          //dbcout2( '  ' + uiFloatToStr( perim[i].x ) + tab + uiFloatToStr( perim[i].y ) );
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
            ZoneSize := 0;
            
          if ( 0 < ZoneSize ) then
            begin

          // If so, draw all of the zones
          for j := 0 to (ZoneSize - 1) do
            begin
              Zone := Get_zone_list_zone( _perimeterListC, j );

              pbxMap.Canvas.Pen.Color := _perimeterColors[ ( Zone^.level - 1 ) mod length( _perimeterColors ) ];
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

end.
