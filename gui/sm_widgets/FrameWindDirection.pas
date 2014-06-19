unit FrameWindDirection;

(*
FrameWindDirection.pas/dfm
--------------------------
Begin: 2005/06/10
Last revision: $Date: 2013-06-27 19:11:37 $ $Author: areeves $
Version number: $Revision: 1.19.6.3 $
Project: NAADSM and related applications
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
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
    Variants,
    Classes,
    Graphics,
    Controls,
    Forms,
    Dialogs,
    StdCtrls,
    REEdit,
    ExtCtrls
  ;

  type TFrameWindDirection = class( TFrame )
      lblWindRange: TLabel;
      lblWindStart: TLabel;
      lblWindEnd: TLabel;
      lblWindDir: TLabel;
      pbxWindDir: TPaintBox;
      rleWindStart: TREEdit;
      rleWindEnd: TREEdit;

      procedure pbxWindDirPaint( sender: TObject );
      procedure processText( sender: TObject );

      procedure startAngleDataEntry( Sender: TObject; var Key: Word; Shift: TShiftState );
      procedure endAngleDataEntry( Sender: TObject; var Key: Word; Shift: TShiftState );


    protected
    	_startAngle: integer;
      _endAngle: integer;
      _doNotDraw: boolean;

      procedure translateUI();

      procedure drawWindDirection( startAngle, endAngle: integer );

      function getStartAngle(): integer;
      function getEndAngle(): integer;

      procedure setStartAngle( val: integer );
      procedure setEndAngle( val: integer );

    public
    	constructor create( AOwner: TComponent ); override;

      procedure setAngles( angleStart, angleEnd: integer );

      property startAngle: integer read getStartAngle write setStartAngle;
      property endAngle: integer read getEndAngle write setEndAngle;

    end
  ;

  const
    DBFRAMEWINDDIRECTION: boolean = false; // Set to true to enable debugging messages for this unit.

implementation

{$R *.dfm}

 	uses
		Math,
    MyStrUtils,
    DebugWindow,
    RegExpDefs,
    I88n
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameWindDirection.create( AOwner: TComponent );
  	begin
    	inherited create( AOwner );
      translateUI();
      
      rleWindEnd.InputExpression := RE_INTEGER_INPUT;
      rleWindStart.InputExpression := RE_INTEGER_INPUT;
      startAngle := 0;
      endAngle := 0;
      _doNotDraw := false;
    end
  ;
  
  
  procedure TFrameWindDirection.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:59:46 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_widgets/FrameWindDirection.dfm
      // File date: Thu Oct 12 14:33:58 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblWindRange.Caption := tr( 'Area at risk of exposure around an infectious unit located at the center of the circle (0-360 degrees, 0/360 degrees = north, proceeding clockwise):' );
          lblWindStart.Caption := capitalize( tr( 'Start' ) );
          lblWindEnd.Caption := capitalize( tr( 'End' ) );
          lblWindDir.Caption := tr( 'Area at risk of exposure (blue indicates affected area):' );
        end
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties (wind start and end angles)
//-----------------------------------------------------------------------------
  procedure TFrameWindDirection.setStartAngle( val: integer );
  	begin
      dbcout( 'Setting startAngle to ' + intToStr( val ), DBSHOWMSG );
      rleWindStart.Text := intToStr( val );
      _startAngle := val;

      if( not _doNotDraw ) then pbxWindDirPaint( nil );
    end
  ;


  procedure TFrameWindDirection.setEndAngle( val: integer );
  	begin
      rleWindEnd.Text := intToStr( val );
      _endAngle := val;

      if( not _doNotDraw ) then pbxWindDirPaint( nil );
    end
  ;


  function TFrameWindDirection.getStartAngle(): integer;
  	begin
    	if( _startAngle < 0 ) then
        while( 0 > _startAngle ) do inc( _startAngle, 360 )
      else if( _startAngle > 360 ) then
        while( 360 < _startAngle ) do dec( _startAngle, 360 )
      ;

    	result := _startAngle;
    end
  ;


  function TFrameWindDirection.getEndAngle(): integer;
  	begin
    	if( _endAngle < 0 ) then
        while( 0 > _endAngle ) do inc( _endAngle, 360 )
      else if( _endAngle > 360 ) then
        while( 360 < _endAngle ) do dec( _endAngle, 360 )
      ;

  		result := _endAngle;
    end
  ;


	procedure TFrameWindDirection.setAngles( angleStart, angleEnd: integer );
    begin
    	_doNotDraw := true;

    	if( angleStart < 0 ) then
        while( 0 > angleStart ) do inc( angleStart, 360 )
      else if( angleStart > 360 ) then
        while( 360 < angleStart ) do dec( angleStart, 360 )
      ;

    	if( angleEnd < 0 ) then
        while( 0 > angleEnd ) do inc( angleEnd, 360 )
      else if( angleEnd > 360 ) then
        while( 360 < angleEnd ) do dec( angleEnd, 360 )
      ;

      setStartAngle( angleStart );
      setEndAngle( angleEnd );

      _doNotDraw := false;

      pbxWindDirPaint( nil );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// display update functions
//-----------------------------------------------------------------------------
  procedure TFrameWindDirection.drawWindDirection( startAngle, endAngle: integer );
    var
      p: integer;
      x3,y3,x4,y4: integer;
      radius: integer;
      startAngleRad, endAngleRad: double;
      drawColor: TColor;
    begin

    	if( startAngle = endAngle ) then
      	drawColor := clBtnFace
      else
      	drawColor := clHotLight
      ;

    	// Draw the circle and label North
      //--------------------------------
      pbxWindDir.Canvas.Pen.Color := clBlack;
      pbxWindDir.Canvas.Ellipse( 0, 0, pbxWindDir.Width, pbxWindDir.Height );
      p := pbxWindDir.Canvas.TextWidth( 'N' );
      pbxWindDir.Canvas.TextOut( pbxWindDir.Width - p - 9, 1,'N' );

      // line
      //-----
      pbxWindDir.Canvas.MoveTo( pbxWindDir.Width - 3, 1 );
      pbxWindDir.Canvas.LineTo( pbxWindDir.Width - 3, 15 );

      // left arrow
      //-----------
      pbxWindDir.Canvas.MoveTo( pbxWindDir.Width - 3, 1 );
      pbxWindDir.Canvas.LineTo( pbxWindDir.Width - 6, 4 );

      // right arrow
      //------------
      pbxWindDir.Canvas.MoveTo( pbxWindDir.Width - 3, 1 );
      pbxWindDir.Canvas.LineTo( pbxWindDir.Width - 0, 4 );

      // pie
      //----
      // convert degrees to radians
      // Remember to adjust by 90 degrees counterclockwise for the "0 degrees = North" convention
      startAngleRad := (Pi/180) * ( startAngle - 90 );
      endAngleRad := (Pi/180) * ( endAngle - 90 );

      radius := pbxWindDir.Height div 2;

      x3 := round( ( cos( startAngleRad ) * radius ) + radius );
      y3 := round( ( sin( startAngleRad ) * radius ) + radius );

      x4 := round( ( cos( endAngleRad ) * radius ) + radius );
      y4 := round( ( sin( endAngleRad ) * radius ) + radius );

      pbxWindDir.Canvas.Brush.Color := drawColor;

      pbxWindDir.Canvas.Pie( 0, 0, pbxWindDir.Width, pbxWindDir.Height, x4, y4, x3, y3 );

      pbxWindDir.Canvas.MoveTo( pbxWindDir.Width div 2, pbxWindDir.Height div 2 );
      pbxWindDir.Canvas.LineTo( x3, y3 );
      pbxWindDir.Canvas.MoveTo( pbxWindDir.Width div 2, pbxWindDir.Height div 2 );
      pbxWindDir.Canvas.LineTo( x4, y4 );

      pbxWindDir.Canvas.Brush.Color := clBtnFace;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFrameWindDirection.processText( sender: TObject );
  	var
      tmpStart, tmpEnd: integer;
    begin
      dbcout( 'Processing text!', DBSHOWMSG );
      tmpStart := myStrToInt( rleWindStart.Text, 0 );
      tmpEnd := myStrToInt( rleWindEnd.Text, 0 );

      setStartAngle( tmpStart );
      setEndAngle( tmpEnd );

      //rleWindStart.Text := intToStr( tmpStart );
      //rleWindEnd.Text := intToStr( tmpEnd );

      rleWindStart.Text := intToStr( startAngle );
      rleWindEnd.Text := intToStr( endAngle );

      pbxWindDirPaint( nil );
    end
  ;



  procedure TFrameWindDirection.startAngleDataEntry( Sender: TObject; var Key: Word; Shift: TShiftState );
  	var
    	val: integer;
  	begin
      if( 0 < length( trim( rleWindStart.Text ) ) ) then
        begin
    	    val := myStrToInt( rleWindStart.Text, 0 );
          setStartAngle( val );
        end
      ;

      dbcout( key, DBSHOWMSG );
      if( 13 = key ) then
        self.SetFocus()
      ;
    end
  ;


  procedure TFrameWindDirection.endAngleDataEntry( Sender: TObject; var Key: Word; Shift: TShiftState );
  	var
    	val: integer;
  	begin
      if( 0 < length( trim( rleWindEnd.Text ) ) ) then
        begin
          val := myStrToInt( rleWindEnd.Text, 0 );
          setEndAngle( val );
        end
      ;

      dbcout( key, DBSHOWMSG );
      if( 13 = key ) then
        self.SetFocus()
      ;
    end
  ;


  procedure TFrameWindDirection.pbxWindDirPaint(Sender: TObject);
    begin
    	drawWindDirection( _startAngle, _endAngle );
    end
  ;
//-----------------------------------------------------------------------------



end.
