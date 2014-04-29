unit ZonePerimeter;

(*
ZonePerimeter.pas
-----------------
Begin: 2007/01/11
Last revision: $Date: 2013-06-27 19:11:19 $ $Author: areeves $
Version: $Revision: 1.14.4.3 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Shaun Case <Shaun.Case@colostate.edu>
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2007 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

{$INCLUDE Defs.inc}

interface

  uses
    Classes,
    Contnrs
  ;

  // Types, etc. for interacting with the DLL
  //-----------------------------------------
  type gpc_vertex = record
    x: double;
    y: double;
  end;

  type gpc_vertex_ptr = Array of gpc_vertex;

  type gpc_vertex_list = record
    num_vertices:   integer;
    vertex:         gpc_vertex_ptr;
  end;

  type vertexListPtr = Array of gpc_vertex_list;

  type gpc_polygon = record
     num_contours:integer;
     hole:Pointer;
     contour:vertexListPtr;
   end;

  type gpc_polygon_ptr = ^gpc_polygon;

  type _GFociArray = record
    data: gpc_vertex_ptr;
    len: Integer;
  end;

  type g_foci_list = Array of _GFociArray;

  //  A zone.
  type ZON_zone_t = record
    name: pchar;
    level: integer;
    radius: double;
    radius_sq: double;
    epsilon_sq: double;
    foci: g_foci_list;  // (GArray*)  /**< Unordered array of foci.  Each focus is a gpc_vertex structure. */
    poly: gpc_polygon_ptr; // (gpc_polygon*) /**< the (possibly multi-contour) polygon */
    fragments: Pointer; // (GQueue*)
    area: double;
    perimeter: double;
    nholes_filled: integer;  //Unsigned
  end;

  type ZON_zone_t_ptr = ^ZON_zone_t;

  type ZON_zone_list_t = Pointer;

  type THRD_PerimeterList = ZON_zone_list_t;

  function Get_zone_list_size( z:Pointer ):integer;

  function get_zone_list_zone( z:Pointer; idx:Integer ): ZON_zone_t_ptr;

  procedure ZonePerimeterDebug( p: THRD_PerimeterList; const showDebuggingMessage: boolean );


  // Delphi types for dealing with zone perimeters (used only when necessary)
  //-------------------------------------------------------------------------
  type VertexArray = array of gpc_vertex;

  type TZonePerimeter = class( TPersistent )
    protected
      _zoneLevel: integer;
      _array: VertexArray;

      procedure setAt( const idx: integer; vertex: gpc_vertex );
      function getAt( const idx: integer ): gpc_vertex;

      function getCount(): integer;
      procedure setLength( val: integer );

    public
      constructor create(); overload;
      constructor create( length: integer ); overload;

      destructor destroy(); override;

      procedure clear();

      procedure debug();

      procedure readData( reader: TReader ); dynamic;
      procedure writeData( writer: TWriter ); dynamic;

      property at[ const idx: integer ]: gpc_vertex read getAt write setAt; default;
      property count: integer read getCount;
      property length: integer read getCount write setLength;
      property zoneLevel: integer read _zoneLevel write _zoneLevel;
    end
  ;


  // Some code adapted from http://www.mindspring.com/~cityzoo/tips/tstream2.txt
  type TZonePerimeterList = class( TObjectList )
    public
      constructor create(); overload;
      constructor create( p: THRD_PerimeterList ); reintroduce; overload; 
      destructor destroy(); override;

      procedure saveToStream( stream: TStream );
      procedure loadFromStream( stream: TStream );
      procedure saveToFile( const filename: string );
      procedure loadFromFile( const filename: string );

      procedure debug();
    end
  ;


implementation

  uses
    SysUtils,

    MyStrUtils,
    DebugWindow,

    NAADSMLibrary
  ;


  function Get_zone_list_size( z:Pointer ):integer;
    var
      ret_val: integer;
    begin
      ret_val := 0;

      if ( assigned( @get_zone_list_length ) AND (z <> nil) ) then
        ret_val := get_zone_list_length( z );

      result := ret_val;
    end
  ;


  function get_zone_list_zone( z:Pointer; idx:Integer ): ZON_zone_t_ptr;
    var
      ret_val: ZON_zone_t_ptr;
    begin
      ret_val := nil;

      if ( assigned( @get_zone_from_list ) AND ( z <> nil ) AND ( idx >=0 )) then
        ret_val := get_zone_from_list( z, idx )
      ;

      result := ret_val;
    end
  ;

  procedure ZonePerimeterDebug( p: THRD_PerimeterList; const showDebuggingMessage: boolean );
    var
      I, J, K: integer;
      MyVertices: ^gpc_vertex_list;
      ZoneSize: Integer;
      Zone: ZON_zone_t_ptr;
    begin
      if( showDebuggingMessage ) then
        begin
          dbcout( endl, true );
          dbcout('=================================================', true );
          dbcout('        Perimeter Callback function data'         , true);
          dbcout('=================================================', true );
          dbcout('Data set by the gui lib via the set perimeters callback function.', true );

          ZoneSize := Get_zone_list_size( p );
          if ( ZoneSize > 0 ) then
            begin
              dbcout( 'We have (' + IntToStr( ZoneSize ) + ') Zones defined by the simulation library', true );

              for j:= 0 to (ZoneSize - 1) do
                begin
                  Zone := Get_zone_list_zone( p, j );

                  dbcout( 'Zone number ' + IntToStr(j) + ' is named: ' + Zone^.name, true );
                  dbcout( '  Level: ' + IntToStr( Zone^.level ), true );
                  dbcout( '  Holes filled: ' + IntToStr( Zone^.nholes_filled ), true );
                  dbcout( '  Radius: ' + usFloatToStr( Zone^.radius ), true );
                  dbcout( '  Radius SQ: ' + usFloatToStr( Zone^.radius_sq ), true );
                  //dbcout( '  Radius as degrees: ' + usFloatToStr( Zone^.radius_as_degrees ), true );
                  dbcout( '  Area: ' + usFloatToStr( Zone^.area ), true );
                  dbcout( '  Perimeter: ' + usFloatToStr( Zone^.perimeter ), true );
                  dbcout( '  ' +  IntToStr( Zone^.poly^.num_contours ) + ' contours in its structure', true );

                  if ( Zone^.poly^.num_contours > 0 ) then
                    begin
                      for k := 1 to Zone^.poly^.num_contours do
                        begin
                          MyVertices := @Zone^.poly^.contour[k-1];

                          dbcout( 'Contour: ' + IntToStr(k) + ' has ' + IntToStr( MyVertices.num_vertices ) + ' vertices.', true );

                          for i := 0 to MyVertices^.num_vertices - 1 do
                            begin
                              dbcout( '     Vertex number ' + IntToStr( i + 1 )+ ':', true );
                              dbcout( '          x: ' + usFloatToStr( MyVertices^.vertex[i].x), true );
                              dbcout( '          y: ' + usFloatToStr( MyVertices^.vertex[i].y), true );
                            end
                          ;

                          dbcout('    -----------------------------', true );
                        end
                      ;
                    end
                  ;
                  dbcout( endl, true );

                end
              ;
            end
          ;
          dbcout('=================================================', true );
          dbcout('       END Perimeter Callback function data'      , true);
          dbcout('=================================================', true );
          dbcout( endl, true );
        end
      ;
    end
  ;

  constructor TZonePerimeter.create();
    begin
      inherited create();
      clear();
    end
  ;

  constructor TZonePerimeter.create( length: integer );
    begin
      inherited create();
      clear();
      System.setLength( _array, length );
    end
  ;

  destructor TZonePerimeter.destroy();
    begin
      clear();
      inherited destroy();
    end
  ;

  procedure TZonePerimeter.clear();
    begin
      System.setLength( _array, 0 );
    end
  ;


  procedure TZonePerimeter.debug();
    var
      i: integer;
    begin
      dbcout( '  Perimeter:', true );
      for i := 0 to system.length( _array ) - 1 do
        dbcout( '    ' + usFloatToStr( _array[i].x ) + ', ' + usFloatToStr( _array[i].y ), true )
      ;
    end
  ;


  procedure TZonePerimeter.readData( reader: TReader );
    var
      i: integer;
      vertex: gpc_vertex;
    begin
      zoneLevel := reader.ReadInteger;
      length := reader.readInteger;

      for i := 0 to length - 1 do
        begin
          vertex.x := reader.ReadFloat;
          vertex.y := reader.ReadFloat;
          self[i] := vertex;
        end
      ;
    end
  ;


  procedure TZonePerimeter.writeData( writer: TWriter );
    var
      i: integer;
    begin
      writer.writeInteger( zoneLevel );
      writer.writeInteger( length );

      for i := 0 to length - 1 do
        begin
          writer.WriteFloat( self[i].x );
          writer.WriteFloat( self[i].y );
        end
      ;
    end
  ;

  procedure TZonePerimeter.setLength( val: integer );
    begin
      System.setLength( _array, val );
    end
  ;

  procedure TZonePerimeter.setAt( const idx: integer; vertex: gpc_vertex );
    begin
      _array[idx] := vertex;
    end
  ;

  function TZonePerimeter.getAt( const idx: integer ): gpc_vertex;
    begin
      result := _array[idx];
    end
  ;

  function TZonePerimeter.getCount(): integer;
    begin
      result := system.length( _array );
    end
  ;

  constructor TZonePerimeterList.create();
    begin
      inherited create();
      OwnsObjects := true;
    end
  ;
  
  constructor TZonePerimeterList.create( p: THRD_PerimeterList );
    var
      I, J, K: integer;
      MyVertices: ^gpc_vertex_list;
      ZoneSize: Integer;
      Zone: ZON_zone_t_ptr;

      perim: TZonePerimeter;
      vert: gpc_vertex;
    begin
      inherited create();

      OwnsObjects := true;

      // Are there any zones to copy?
      ZoneSize := Get_zone_list_size( p );
      if ( 0 = ZoneSize ) then
        exit
      ;

      // Otherwise, copy each contour in each zone
      for j := 0 to ZoneSize - 1 do
        begin
          Zone := Get_zone_list_zone( p, j );

          if ( 0 < Zone^.poly^.num_contours ) then
            begin
              for k := 0 to Zone^.poly^.num_contours - 1 do
                begin
                  perim := TZonePerimeter.create();
                  perim.zoneLevel := zone^.level;

                  MyVertices := @Zone^.poly^.contour[k];

                  if( 0 < MyVertices^.num_vertices ) then
                    begin
                      perim.length := MyVertices^.num_vertices;

                      // Record the vertices
                      for i := 0 to MyVertices^.num_vertices - 1 do
                        begin
                          vert.x := MyVertices^.vertex[i].x;
                          vert.y := MyVertices^.vertex[i].y;
                          perim[i] := vert;
                        end
                      ;
                    end
                  ;
                  
                  self.Add( perim );
                end
              ;
            end
          ;
        end
      ;

    end
  ;


  destructor TZonePerimeterList.destroy();
    begin
      inherited destroy();
    end
  ;


  procedure TZonePerimeterList.saveToStream( stream: TStream );
    var
      writer : TWriter;
      i      : integer;
    begin
      writer := TWriter.Create( stream, $ff );
      try
        with writer do
          begin
          {mark beginning of file and beginning of object list}
          WriteSignature;
          WriteListBegin;

          {loop through this list}
          for i := 0 to Count - 1 do
            begin
              {Store any TPersistent objects}
              if TObject(Items[i]) is TPersistent then
                begin
                WriteString(TPersistent(Items[i]).ClassName);
                {Call WriteData() for TPlayer objects}
                if (TPersistent(Items[i]) is TZonePerimeter) then
                TZonePerimeter(Items[i]).WriteData(writer);
                end
              ;
            end
          ;
          {mark end of object list}
          WriteListEnd;
          end
        ;
      finally
        writer.Free;
      end;
    end
  ;


  procedure TZonePerimeterList.loadFromStream( stream: TStream );
    var
      reader : TReader;
      obj    : TPersistent;
      ctype  : TPersistentClass;
      cname  : string;
    begin
      stream.Position := 0;

      reader:=TReader.Create(stream,$ff);
      try
        with reader do
          begin
            {read beginning of file and beginning of object list markers}
            ReadSignature;
            ReadListBegin;
            {loop through file list of objects}
            while not EndOfList do
              begin
                {Load ClassName and use it to get ClassType}
                cname := ReadString;
                ctype := GetClass(cname);
                if Assigned(ctype) then
                  begin
                    {If a ClassType was found, create an instance}
                    obj := ctype.Create;
                    try
                      {if obj is a TZonePerimeter, call its ReadData() method}
                      if obj is TZonePerimeter then
                      TZonePerimeter(obj).ReadData(reader);
                    except
                      obj.free;
                      raise;
                    end;
                    {add object to this list}
                    Add(obj);
                  end
                ;
              end
            ;
            ReadListEnd;
          end
        ;
      finally
        reader.Free;
      end;
    end
  ;


  procedure TZonePerimeterList.saveToFile( const filename: string );
    var
      stream : TFileStream;
    begin
      stream := TFileStream.Create(filename, fmCreate or fmOpenWrite);

      try
        SaveToStream(stream);
      finally
        stream.Free;
      end;
    end
  ;



  procedure TZonePerimeterList.loadFromFile( const filename: string );
    var
      stream : TFileStream;
    begin
      stream := TFileStream.Create(filename, fmOpenRead);

      try
        Clear;
        LoadFromStream(stream);
      finally
        stream.Free;
      end;
    end
  ;

  procedure TZonePerimeterList.debug();
    var
      i: integer;
    begin
      dbcout( endl + '------------ TZonePerimeterList.debug()', true );
      for i := 0 to self.count - 1 do
        ( self[i] as TZonePerimeter ).debug()
      ;
      dbcout( '------------ TZonePerimeterList.debug done.', true );
    end
  ;

initialization
  RegisterClass( TZonePerimeter );

end.