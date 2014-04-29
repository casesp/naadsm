unit HerdKML;

(*
HerdKML.pas
-----------
Begin: 2009/10/30
Last revision: $Date: 2010-01-27 00:28:00 $ $Author: areeves $
Version number: $Revision: 1.2.6.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Ric Hupalo <Ric.Hupalo@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{*
  The unit produces a kmz file from a HerdList object. The file is
  organized by folders for each production type, wherein each herd is a map
  element called a placemark. The symbol size of each herd is based on the herd size.

  The kmz file is a zip file, so opening the kmz file with a zip utility will
  reveal the kml file. The kmz file can be loaded into GoogleEarth, available
  for download at: http://earth.google.com/intl/en/download-earth.html
  Google Maps will also read the file, but has some file size limitations.

  KML files are in XML format and their terminology can be found at:
  http://code.google.com/apis/kml/documentation/kmlreference.html

  To use this unit create an instance of TKMLFileGenerator, passing in a
  herd list and optionally a ProductionTypeID. Then call method
  generateHerdKMZFile.
}


interface

  uses
    //Delphi Units
    Graphics,
    Windows,
    SysUtils,
    Contnrs,
    Classes,
    Dialogs,
    StrUtils,

    //NAADSM
    Herd,
    NAADSMLibraryTypes
  ;

  Type
    /// Use for differentiating the mapping symbol size (value  / 10)
    TSymbolSize =
      (
        sUndefined = 1,   // need to test what Google Map does if the symbol size is 0
        sVerySmall = 3,  //      1 - 10 animals
        sSmall = 4,      //     11 - 100 animals
        sMedium = 6,     //    101 - 500 animals
        sLarge = 8,      //  > 501 - 1000 animals
        sVeryLarge = 10  // > 1000 animals
      )
    ;

  //Type
    {*
      This class maintains the information needed to build the Style id elements
      of the KML Document for each combination of production type and herd size.
    }
    THerdTypeMapSymbol = Class
      protected
        _styleID: string;
        _prodTypeID: integer;
        _prodTypeName: string;
        _symbolHexColor: string;
        _symbolURL: string;
        _symbolScale: double;
        _symbolSize: TSymbolSize;

      public
        constructor create( prodTypeID: integer; prodTypeName: string; herdSize: Integer; symbolColor: string );
        function generateKMLPointStyle(): string;

        property styleID: string read _styleID;
        property productionTypeID: integer read _prodTypeID;
        property productionTypeName: string read _prodTypeName;
        property symbolSize: TSymbolSize read _symbolSize;
        property symbolHexColor: string read _symbolHexColor;
      end
    ;

  //Type
    {*
     This class must:
     1. let each herd know it's KML Style ID (for the styleURL tag element of the Placemark sections).
     2. After iterating through all the herds, provide TKMLFileGenerator the info to build the KML Style id sections of the Document
    }
    TMapSymbolProvider = Class (TObjectList)
      protected
        _nextColorIndex : integer;
        function at( index: integer ): THerdTypeMapSymbol;
      public
        constructor create();
        function getStyleID( prodTypeID: integer; prodTypeName: string; herdSize: integer ): string;
      end
    ;

    //Type
    {*
      Holds the herd KML placemarks for a particular production type and writes
      the KML document section for the folder. It also keeps track of the max and min
      lat lon coordinates of it's herds. For large simulations, each production type
      may have to be written to an individual kml file, needing it's own LookAt coordiantes.
    }
    TPlacemarkFolder = Class
      protected
        _prodTypeID: integer;
        _folderName: string;
        _placemarks: TStringList;
        _minLat, _minLon, _maxLat, _maxLon: double;

      public
        constructor create( productionTypeID: integer; folderName: string );
        destructor destroy(); override;
        procedure addPlacemark( herd: THerd; styleID: string );
        function generateKMLContent(): string;

        //properties
        property FolderName: string read _folderName;
        property ProductionTypeID: integer read _prodTypeID;
        property minLat: double read _minLat;
        property minLon: double read _minLon;
        property maxLat: double read _maxLat;
        property maxLon: double read _maxLon;
      end
    ;


    {*
      Directs incoming herd objects to the correct placemark folder based production type.
      If a folder does not exist for the prod type it adds one.  Also keeps track of the overall
      point coordinate range, providing the formated LookAt elements for all production types
      or any particular one.
    }
    //Type
    TPlacemarkOrganizer = Class (TObjectList)
      protected
        procedure getMinMaxCoordinates( out minLat: double; out minLon: double; out maxLat: double; out maxLon: double; const pmFolderName: string = '' );

      public
        constructor create( herd: THerd );
        procedure addHerd ( herd: THerd; styleID: string );
        function generateKMLLookAtContent( const placeMarkFolderName: string = '' ): string; // coordinate range for one or all production type
      end
    ;

    //Type
    {*
      Class that oversees the clases that build the KML document elements and
      uses the strings they return to make the KML output file and to zip it into a KMZ file.
      Making the file in two steps is nice to look at the text file before it is compressed.
    }
    TKMLFileGenerator = Class
      protected
        _it: THerdListIterator;
        _prodTypeID: integer;

        procedure writeHerdKMLFileToDisk( OutputFileFullPath, kmlDocument: string );
      public
        constructor create( srcHerdList: THerdList ); overload;
        constructor create( srcHerdList: THerdList; productionTypeID: integer ); overload;
        destructor destroy(); override;
        procedure generateHerdKMZFile( OutputFileFullPath: string );

      end
    ;

    //Functions for all Unit Members
    function getHerdSymbolSize( herdSize: integer ): TSymbolSize; // defines a mapping symbol size category
    //function RGBColor2Hexadecimal( const RGB: TColor ): string;  //  converts Delphi/Windows type colors to web html type colors
   
implementation

  uses
    I88n,
    ZipMstr,
    MyDialogs,

    StringConsts,
    FormMain
  ;

const
  nl = #13 + #10; //new line (carrige return + line feed - to make KML file readable)
  dq = '"';

  iconUrlWhtCir = 'http://maps.google.com/mapfiles/kml/paddle/wht-circle.png';  // where to get the mapping symbol
  //http://maps.google.com/mapfiles/kml/shapes/open-diamond.png


  {*
    Colors for production types, iterate through and repeat colors if necessary.
    For now users will not be able to set there own mapping symbol colors

    The colors begin with an additional FF because google uses a convention where
    this 'alpha' term indicates color transparancey 00 to opacity FF
    The order of expression is aaBBGGRR, not RGB ...
    where aa=alpha (00 to ff); BB=blue (00 to ff); GG=green (00 to ff); RR=red (00 to ff). 
  }

   aHexColor: array [0..16] of string =
  (

    'FF00FF00',    //Lime
    'FF00FFFF',    //Yellow
    'FF00A5FF',    //Orange
    'FF0000FF',    //Red
    'FF008000',    //Green
    'FFFF901E',    //DodgerBlue
    'FF82004B',    //Indigo
    'FFEE82EE',    //Violet
    'FF9314FF',    //DeepPink
    'FF008CFF',    //DarkOrange
    'FF00D7FF',    //Gold
    'FFFF0000',    //Blue
    'FFCC3299',    //DarkOrchid
    'FFD670DA',    //Orchid
    'FF2A2AA5',    //Brown
    'FF000000',    //Black
    'FFFFFFFF'     //White
  );


  {*
    Assigns the herd a map symbol size based on herd size.
    Currently five categories: 1-10, 11-100, 101-500, 501-1000, >1000 animals
  }
  function getHerdSymbolSize( herdSize: integer): TSymbolSize;
    var
      symbolSize: TSymbolSize;
    begin
      case herdSize of
        1..10:                symbolSize := sVerySmall;
        11..100:              symbolSize := sSmall;
        101..500:             symbolSize := sMedium;
        501..1000:            symbolSize := sLarge;
        1001..High(Integer):  symbolSize := sVeryLarge;
        else                  symbolSize := sUndefined;
       end;
      result := symbolSize;
    end
  ;


// -----------------------------------------------------------------------------

  { THerdTypeMapSymbol }

  {*
    Each herd must have a mapping symbol to be plotted as a Placemark. This symbol is
    specified as a Style id element and referenced in the Placemark element as a
    styleUrl element. Symbol types are created around the input parameters.

    @param prodTypeID ProductionTypeID
    @param prodTypeName Production Type Name
    @param herdSize The(inital) size of the herd
    @param symbolColor The color of the symbol
  }
  constructor THerdTypeMapSymbol.create(prodTypeID: integer; prodTypeName: string; herdSize: integer; symbolColor: string);
    var
      herdSizeCategory: string;
    begin
      _prodTypeID := prodTypeID;
      _prodTypeName := prodTypeName;
      _symbolURL := iconUrlWhtCir;
      _symbolHexColor := symbolColor;
      _symbolSize := getHerdSymbolSize(herdSize);
      _symbolScale := Ord(_symbolSize) / 10;  // don't use div, want 0.1 to 1.5

      case _symbolSize of
        sVerySmall: herdSizeCategory := 'VSM';
        sSmall:     herdSizeCategory := 'SM' ;
        sMedium:    herdSizeCategory := 'MED';
        sLarge:     herdSizeCategory := 'LG';
        sVeryLarge: herdSizeCategory := 'VLG';
        sUndefined: herdSizeCategory := 'UND';
        else        herdSizeCategory := '?';
      end;

      _styleID :=  'sID_' + IntToStr(prodTypeID) + '_' + herdSizeCategory;
    end
  ;


  {*
    Creates the KML Style id element for each mapping symbol,
    the Style id is then referenced by each placemark.

    @return the KML Style id element content for this symbol
  }
  function THerdTypeMapSymbol.generateKMLPointStyle: string;
    var
      kml: string;
    begin
      // single quote characters ' are escaped by using two in a row ''
      // so the output text 'Hello World' is coded as ''Hello World''

      if ((2 < _symbolScale) or (0 > _symbolScale)) then  _symbolScale := 0.9;

      kml := ' <Style id=' + dq + _styleID + dq + '>' + nl;
      kml := kml + '  <IconStyle>' + nl;
      kml := kml + '    <color>'+ _symbolHexColor + '</color>' + nl;
      kml := kml + '    <colorMode>normal</colorMode>' + nl;
      kml := kml + '    <scale>'+ Format('%1.1f', [_symbolScale]) + '</scale>' + nl;
      kml := kml + '    <Icon>' + nl;
      kml := kml + '      <href>'+ _symbolURL + '</href>' + nl;
      kml := kml + '    </Icon>' + nl;
      kml := kml + '  </IconStyle>' + nl;
      kml := kml + '  <LabelStyle>' + nl;
                        // a color that stands out nice against earth aerial color imagery
      kml := kml + '    <color>'+ 'FFFFFFFF' + '</color>' + nl;
      kml := kml + '    <colorMode>normal</colorMode>' + nl;
      kml := kml + '    <scale>'+ Format('%1.1f', [_symbolScale]) + '</scale>' + nl;

      kml := kml + '  </LabelStyle>' + nl;
      kml := kml + '  <BalloonStyle>' + nl;
      //kml := kml + '    <bgColor>'+ '7fff0000' + '</bgColor>' + nl;
      kml := kml + '    <Text><![CDATA[$[description]<br/>$[geDirections]]]></Text>' + nl;
      kml := kml + '  </BalloonStyle>' + nl;
      kml := kml + ' </Style>' + nl;

      result := kml;
    end
  ;


  { TMapSymbolProvider }

  {*
    The provider keeps a list of mapping symbols and lets each herd
    placemark know it's mapping symbol style based on production type and
    herd size. As new production types are discovered when iterating through the
    herd list new mapping symbols are created for combinations of herd size and
    production type.
  }
  constructor TMapSymbolProvider.create;
    begin
      inherited create( true ); // list owns the map symbol objects
      _nextColorIndex := 0;
    end
  ;

  {*
     Returns the mapping symbol object at index
  }
  function TMapSymbolProvider.at(index: integer): THerdTypeMapSymbol;
    begin
      result := GetItem( index ) as THerdTypeMapSymbol;
    end
  ;


  {*
    Each Placemark (herd) element of the KML document needs a StyleID value for
    it's styleURL element. This symbol style is based prodType, herd size scale symbol size.
    
    @param prodTypeID The production type of the herd, used in the style id name.
    @param prodTypeName Production Type Name, used in the KML extended data tag
    @param herdSize Number of animals in the herd, used to formulate part of the name.
  }
  function TMapSymbolProvider.getStyleID(prodTypeID: integer; prodTypeName: string; herdSize: integer): string;
    var
      ms: THerdTypeMapSymbol;
      i: integer;
      foundSymbol: boolean;
      myHexColor: string;

    begin
      foundSymbol := false;
      myHexColor := '?';

      if( self.Count = 0 ) then
        begin
          // The list is empty, add the first map symbol to the list
          myHexColor := aHexColor[_nextColorIndex]; //use the first color
          ms := THerdTypeMapSymbol.create(prodTypeID, prodTypeName, herdSize, myHexColor);
          self.Add(ms);
          result := ms.styleID;
        end
      else
        begin
          // The list holds map symbols, look for the symbol having the key parameters
          for i := 0 to self.count - 1 do
            begin
              ms := self.at(i);
              if ( ms.productionTypeID = prodTypeID ) then
                begin
                  myHexColor := ms.symbolHexColor;
                  if ( ms.symbolSize = getHerdSymbolSize( herdSize )) then
                    begin
                      //Found it !
                      result := ms.styleID;
                      foundSymbol := true;
                      break;
                    end
                  ;  //herd size match
                end  //prodTypID match
              ;
            end // for list object count
          ;

          //if a symbol object was not found for these parameters create one and add it to the list
          if not foundSymbol then
            begin
              { The goal is to have symbol size vary by herd size, but all herds
              of the same production type should be of the same color.
              Choosing a color is left to the symbol provider, not the individual symbols }

              if (myHexColor = '?') then
                begin
                  if (_nextColorIndex < high(aHexColor)) then  inc(_nextColorIndex)
                  else _nextColorIndex := 0;
                  myHexColor := aHexColor[_nextColorIndex];
                end
              ;

              ms := THerdTypeMapSymbol.create(prodTypeID, prodTypeName, herdSize, myHexColor);
              self.Add(ms);
              result := ms.styleID;
            end
          ;

        end // if the list is not empty
      ;
    end
  ;


  { TPlacemarkFolder }


  {*
    Placemark folders are to organize placemarks (herds) in the KML file by
    Production Type. The folder must also keep track of the geographic extent
    of the placemarks it holds.

    @param productionTypeID The Production Type ID of the placemarks in this folder
    @param folderName The name of the folder, named after the Production Type
  }
  constructor TPlacemarkFolder.create(productionTypeID: integer; folderName: string);
    begin
      _prodTypeID := productionTypeID;
      _folderName :=  folderName;
      _maxLat := 0.0;
      _maxLon := 0.0;
      _maxLon := 0.0;
      _minLon := 0.0;
      _placemarks := TStringlist.Create;
    end
  ;


  destructor TPlacemarkFolder.destroy;
    begin
      inherited;
      FreeAndNil(_placemarks);
    end
  ;


  {*
     Takes herd information makes a formated Placemark data element and adds
     this to its body string. Placemarks are for 2D point coordinates. Also
     keeps track of the coordinate range of the herds (placemarks).

     @param herd Contains data needed to make a KML Placemark element
  }
  procedure TPlacemarkFolder.addPlacemark(herd: THerd; styleID: string);
    var
      pm: string;
      desc: string;
    begin

      // Description element with associated html tables and data
      desc:= '<font size=5>' + tr('NAADSM initial condition') + '</font><br>' + nl;
      desc := desc + '<table>' + nl;
      desc := desc + '  <tr><td colspan=' + dq + '6' + dq + '>' + '<font size=5>' + herd.actualProdTypeName + '</font></td></tr>' + nl;
      desc := desc + '</table>' + nl;
      desc := desc + '<table border=' + dq + '1' + dq + '>' + nl;
      desc := desc + '  <tr><th align=' + dq+ 'right' + dq + '>'
                    + tr('Initial size') + '</th><th>'
                    + tr('Initial state') + '</th><th align=' + dq + 'right' + dq + '>'
                    + tr('Zone') + '</th></tr>' + nl;
      desc := desc + '  <tr><td align=' + dq + 'right' + dq + '>' + intToStr(herd.initialSize) + '</td>' +
                       '<td>' + tr(naadsmDiseaseStateStr(herd.initialStatus)) + '</td>'  +
                       '<td align=' + dq+ 'right' + dq+ '>' + intToStr(herd.zoneLevel) + '</td>' + '</tr>' + nl;
      desc := desc + '</table>';

      // Placemark element
      pm := ' <Placemark>' + nl;
      pm := pm + '  <name>' + 'Unit ID ' + IntToStr(herd.id) + '</name>' + nl;
      pm := pm + '  <Snippet><![CDATA[' + herd.actualProdTypeName + ', ' + intToStr(herd.initialSize) + ' Units' + ']]></Snippet>' + nl;
      pm := pm + '  <styleUrl>#' + styleID + '</styleUrl>' + nl;
      pm := pm + '  <description><![CDATA[' + desc + ']]></description>' + nl;
      // not doing  <ExtendedData></ExtendedData>, but this is where this element  would be inserted
      pm := pm + '  <Point>' + nl;
      pm := pm + '    <coordinates>' + Format('%3.5f', [herd.lon]) + ',' + Format('%3.5f', [herd.lat]) + ',0</coordinates>' + nl;
      pm := pm + '  </Point>' + nl;
      pm := pm + ' </Placemark>' + nl;

      _placemarks.Add(pm);

      { Manage the folder point coordinate range:
        The _min values were initialized to 0.0, but using 0 for comparisons
        will not work in southern hemisphere, so the first herd should reinitialize these
      }
      if (_placemarks.Count = 1) then _minLat := herd.lat
      else if (herd.lat < _minLat) then _minLat := herd.lat;

      if (_placemarks.Count = 1) then _minLon := herd.lon
      else if (herd.lon < _minLon) then _minLon := herd.lon;

      if (_placemarks.Count = 1) then _maxLat := herd.lat
      else if (herd.lat > _maxLat) then _maxLat := herd.lat;

      if (_placemarks.Count = 1) then _maxLon := herd.lon
      else if (herd.lon > _maxLon) then _maxLon := herd.lon;

    end
  ;

  {*
    Generates the KML placemark folder element body for this folder
    @return String containg the folder tags and children placemark elements
  }
  function TPlacemarkFolder.generateKMLContent: string;
    var
      body: string;
      i: integer;
    begin
      body := '<Folder><name>' + self.FolderName + '</name>' + nl;

      for i:= 0 to (_placeMarks.Count-1) do
        begin
          body := body + _placeMarks.Strings[i];
        end
      ;

      body := body + '</Folder>' + nl;
      result := body;
    end
  ;


  { TPlacemarkOrganizer }


  {*
    The placemark organizer accepts herds and organizes them into a series
    of KML placemark folders. Create a folder for the first herd and productionType

    @param herd First herd to create a map placemark for.
  }
  constructor TPlacemarkOrganizer.create(herd: THerd );
    var
      newPMF: TPlacemarkFolder;

    begin
      inherited create( true ); // list manages the placemark folder objects

      newPMF := TPlacemarkFolder.create(herd.actualProdTypeID,herd.prodTypeName);
      self.Add(newPMF);
    end
  ;


  /// For all production types then pmFolderName should be empty
  {*
     The KML file may being built for all the production types or just one. In
     either situation the LookAt element of the file zooms Google Earth to the
     vicinity of the palcemarks. This function is used to support doing so.

     @param minLat the outputted minimum Latitude
     @param minLon the outputted minimum Longitude
     @param maxLat the outputted maximum Latitude
     @param maxLon the outputted maximum Longitude
     @param pmFolderName Input: A particular placemark folder name. If it is left empty
     then the geographic extent will be for all production types. Otherwise,
     only the geographic extent of herds in pmFolderName are in the result.
     @comment Note you read the output parameters in the calling method
  }
  procedure TPlacemarkOrganizer.getMinMaxCoordinates(out minLat, minLon, maxLat, maxLon: double; Const pmFolderName: string = '' );
    var
      i: integer;
      initialized: boolean;
    begin
      minLat := 0.0;
      minLon := 0.0;
      maxLat := 0.0;
      maxLon := 0.0;
      if (1 > self.Count) then exit;

      if ('' = pmFolderName) then // examine all production types
        begin
          // Initialize to relative coordinates rather than 0, to work in both hemispheres
          maxLat := (self.Items[0] as TPlacemarkFolder).maxLat;
          maxLon := (self.Items[0] as TPlacemarkFolder).maxLon;
          minLat := (self.Items[0] as TPlacemarkFolder).minLat;
          minLon := (self.Items[0] as TPlacemarkFolder).minLon;

          for i := 0 to (self.Count -1) do
            begin
              if (( self.Items[i] as TPlacemarkFolder ).maxLat > maxLat )
              then maxLat := ( self.Items[i] as TPlacemarkFolder ).maxLat;

              if (( self.Items[i] as TPlacemarkFolder ).maxLon > maxLon )
              then maxLon := ( self.Items[i] as TPlacemarkFolder ).maxLon;

              if (( self.Items[i] as TPlacemarkFolder ).minLat < minLat )
              then minLat := ( self.Items[i] as TPlacemarkFolder ).minLat;

              if (( self.Items[i] as TPlacemarkFolder ).minLon < minLon )
              then minLon := ( self.Items[i] as TPlacemarkFolder ).minLon;
            end
           ; // for i folders
        end
      else // limit examining max and mins to the input production type
        begin

          initialized := false;
          for i := 0 to (self.Count -1) do
            begin

              if (( self.Items[i] as TPlacemarkFolder ).FolderName = pmFolderName ) then
                begin

                  if not initialized then
                    begin
                      // Initialize to relative coordinates rather than 0, to work in both hemispheres
                      maxLat := (self.Items[i] as TPlacemarkFolder).maxLat;
                      maxLon := (self.Items[i] as TPlacemarkFolder).maxLon;
                      minLat := (self.Items[i] as TPlacemarkFolder).minLat;
                      minLon := (self.Items[i] as TPlacemarkFolder).minLon;
                      initialized := true;
                    end
                  ;

                  if (( self.Items[i] as TPlacemarkFolder ).maxLat > maxLat )
                  then maxLat := ( self.Items[i] as TPlacemarkFolder ).maxLat;

                  if (( self.Items[i] as TPlacemarkFolder ).maxLon > maxLon )
                  then maxLon := ( self.Items[i] as TPlacemarkFolder ).maxLon;

                  if (( self.Items[i] as TPlacemarkFolder ).minLat < minLat )
                  then minLat := ( self.Items[i] as TPlacemarkFolder ).minLat;

                  if (( self.Items[i] as TPlacemarkFolder ).minLon < minLon )
                  then minLon := ( self.Items[i] as TPlacemarkFolder ).minLon;
                end
              ; // if production types match

            end
          ; // for i folders
        end
      ; // if prodTypeID < 0
    end
  ;


  {*
    The KML LookAt element establishes an initial viewing location, altitude, and aspect.
    Function uses all the herd point coordinates or just those in a specific folder to do so.

    @param placeMarkFolderName The name of one of the placemark folders (one production type)
    @comment If placeMarkFolderName is empty then the geographic extent encompases all production types
  }
  function TPlacemarkOrganizer.generateKMLLookAtContent( const placeMarkFolderName: string = '' ): string;
    var
      laBody: string;
      minLat, minLon, maxLat, maxLon, kmlLat, kmlLon: double;
      
    begin
      getMinMaxCoordinates( minLat, minLon, maxLat, maxLon, placeMarkFolderName);
      if ((0 > minLat) And (0 > maxLat)) then
        begin
          //Cases in the S Hemi
          kmlLon := (minLon + maxLon)/2;
          kmlLat := (maxLat - minLat)/2;
          kmlLat := kmlLat + minLat;
        end
      else
        begin
          // cases in N Hemi or where the equator is straddled
          kmlLon := (minLon + maxLon)/2;
          kmlLat := (minLat + maxLat)/2;
        end
      ;

      laBody := '<LookAt>' + nl;
      laBody := laBody + ' <longitude>' + Format('%3.5f', [kmlLon]) + '</longitude>' + nl;
      laBody := laBody + ' <latitude>' + Format('%3.5f', [kmlLat]) + '</latitude>' + nl;
      laBody := laBody + ' <altitude>' + '0' + '</altitude>' + nl;
      laBody := laBody + ' <heading>' + '0' + '</heading>' + nl;
      laBody := laBody + ' <tilt>' + '0' + '</tilt>' + nl;
      laBody := laBody + ' <range>' + '1600000' + '</range>' + nl;
      laBody := laBody + ' <altitudeMode>' + 'clampToGround' + '</altitudeMode>' + nl;
      laBody := laBody + '</LookAt>' + nl;

      result := laBody;
    end
  ;


  {*
     Examine herd then go through the list of folders and add herd to the
     correct folder or if the herd production type is not found then
     create a new folder and add herd there.

     @param herd The herd to use to create a mapping placemark in its product type folder
     @param styleID The name of the map symbol style to be associated through the styleURL element
  }
  procedure TPlacemarkOrganizer.addHerd(herd: THerd; styleID: string);
    var
      i: integer;
      foundIt: boolean;
      pmf: TPlacemarkFolder;
      folderProdTypeID: integer;

    begin

      foundIt := false;
      for i := 0 to (self.Count -1) do
        begin
          folderProdTypeID := (self.Items[i] as TPlacemarkFolder).ProductionTypeID;
          if ( folderProdTypeID = herd.actualProdTypeID ) then
            begin
              (self.Items[i] as TPlacemarkFolder).addPlacemark(herd, styleID);
              foundIt := true;
              break;
            end // if correct folder is located
          ;
        end // for i folders
      ;

      if not foundIt then
        begin
          //Must be a new production type,
          //a folder must be created the herd added, and the folder added to the collection
          pmf := TPlacemarkFolder.create( herd.actualProdTypeID, herd.prodTypeName );
          pmf.addPlacemark(herd, styleID);
          self.Add(pmf);
        end
      ;

    end
  ;


{ TKMLFileGenerator }

  {*
    The entry point into HerdKML when you want to create a KML file for all production types

    @param srcHerdList The source herd list to use to generate the KML file.
  }
  constructor TKMLFileGenerator.create(srcHerdList: THerdList);
    begin
      _it := THerdListIterator.create(srcHerdList);
      _prodTypeID := -1; // no production Type ID specified, placemarks for all types will be generated
    end
  ;


  {*
    The entry point into HerdKML when you want to create a KML file for one production type

    @param srcHerdList The source herd list to use to generate the KML file.
    @param productionTypeID The production type ID of the herds to include in the KML file
  }
  constructor TKMLFileGenerator.create(srcHerdList: THerdList; productionTypeID: integer);
    begin
      _it := THerdListIterator.create(srcHerdList);
      _prodTypeID := productionTypeID;
    end
  ;

  destructor TKMLFileGenerator.destroy;
    begin
      FreeAndNil(_it);
      inherited destroy();
    end
  ;

  // --- Implementing the objects


  {*
     The method to invoke after creating the TKMLFileGenerator object.

     @param OutputFileFullPath Path and file name for the KMZ file (compressed KML file).
     @comment If you want to inspect the actual KML file, just unzip the KMZ file.
  }
  procedure TKMLFileGenerator.generateHerdKMZFile(OutputFileFullPath: string );
    var
      styleID: string;
      msp: TMapSymbolProvider;
      pmo: TPlacemarkOrganizer;
      i: integer;
      styleIDBody: string;
      folderBody: string;
      kmlDocument: string;
      LookAtElement: string;

    begin

      try
        msp := TMapSymbolProvider.create();

         //Create a Placemark Organizer and submit the first herd
          _it.toFirst();
          pmo := TPlacemarkOrganizer.create( _it.current() );

         if (0 > _prodTypeID) then  // Build file for all Production Types
           begin
            styleID := msp.getStyleID( _it.current().actualProdTypeID, _it.current().actualProdTypeName, _it.current().initialSize );
            pmo.addHerd( _it.current(), styleID );
            _it.incr();
          end
         else  // build file for one particular production type
          begin
            while( nil <> _it.current() ) do
              begin
                if (_it.current().prodTypeID = _prodTypeID) then
                  begin
                    styleID := msp.getStyleID( _it.current().actualProdTypeID, _it.current().actualProdTypeName, _it.current().initialSize );
                    pmo.addHerd( _it.current(), styleID );
                    break;  // we only want the first one !
                  end
                ;
                _it.incr();
              end
            ; // while looking for the first herd of correct production type

            // Make sure a herd was found with the specified production type
            if not assigned(pmo) then
              begin
                raise exception.create(tr( 'ProductionTypeID not associated with this herd, output canceled.' ) );
                exit;
              end
            ;
          end
         ;

        //Create placemark elements for the rest of the herds/units

        while( nil <> _it.current() ) do
          begin
            if (0 > _prodTypeID) then  // add all  herds
              begin
                // get the current herd, locate a styleid and add a placemark
                styleID := msp.getStyleID( _it.current().actualProdTypeID, _it.current().actualProdTypeName, _it.current().initialSize );
                pmo.addHerd( _it.current(), styleID );
              end
            else   // add only herds of the specified production type
              begin
                if (_it.current().prodTypeID = _prodTypeID) then
                  begin
                    // get the current herd, locate a styleid and add a placemark
                    styleID := msp.getStyleID( _it.current().actualProdTypeID, _it.current().actualProdTypeName, _it.current().initialSize );
                    pmo.addHerd( _it.current(), styleID );
                  end
                ;
              end  // adding herds of only _prodTypeID
            ;
            _it.incr();
          end
        ; // while iterating through the herd list

        {
          The objects now have everything needed for building the KML file
          Get the content for each document element and put it all in one string
        }

        // Symbology Style id elements
        styleIDBody := '';
        for i := 0 to (msp.Count - 1) do
          begin
            styleIDBody := styleIDBody + (msp.Items[i] as THerdTypeMapSymbol).generateKMLPointStyle;
          end
        ;
        //LookAt element   rbh: revisit - what about a single production type, need to pass in the pm folder name
        LookAtElement := pmo.generateKMLLookAtContent();

        folderBody := '';
        for i := 0 to (pmo.Count - 1) do
          begin
            folderBody := folderBody + ( pmo.Items[i] as TPlacemarkFolder ).generateKMLContent;
          end
        ;

      finally
        //done with these, free memory for repository objects
        if Assigned(msp) then FreeAndNil(msp);
        if Assigned(pmo) then FreeAndNil(pmo);
      end;

      try  // Could this string ever exceed 2 GB ??
        kmlDocument := '<?xml version=' + dq + '1.0' + dq + ' encoding=' + dq + 'UTF-8' + dq + '?>' + nl;
        kmlDocument := kmlDocument + '<kml xmlns=' + dq + 'http://www.opengis.net/kml/2.2' + dq + '>' + nl;
        kmlDocument := kmlDocument + '<Document>' + nl;
        kmlDocument := kmlDocument + styleIDBody;
        kmlDocument := kmlDocument + LookAtElement;
        kmlDocument := kmlDocument + folderBody;
        kmlDocument := kmlDocument + '</Document>' + nl;
        kmlDocument := kmlDocument + '</kml>';
      except
        raise exception.create(tr( 'Merging body elements failed, output canceled.' ) );
        kmlDocument := '';
        exit;
      end;

      //Write the KML xml file to disk
      writeHerdKMLFileToDisk( OutputFileFullPath, kmlDocument );

    end
  ;

  {*
     Protected method that writes the KMZ file to disk. Called by generateHerdKMZFile.

     @param OutputFileFullPath Full path and file name of the KMZ file
     @param kmlDocument The text of the KML file, an XML type formating
  }
  procedure TKMLFileGenerator.writeHerdKMLFileToDisk(OutputFileFullPath, kmlDocument: string);
    var
      kmlTextFile: TextFile;
      kmlFileFullPath: string;
      kmzFileFullPath: string;
      zipper: TZipMaster;

    begin

      // if there is an old file here with same name delete it
      if FileExists(kmlFileFullPath) then
      DeleteFile(kmlFileFullPath);

      //Write the KML xml file to disk
      try
        //Make sure the output file will have a .kml extension
        kmlFileFullPath := OutputFileFullPath;
        if (AnsiRightStr(kmlFileFullPath, 4) <> AnsiLowerCase('.kml') )
        then kmlFileFullPath := AnsiLeftStr(kmlFileFullPath, (Length(kmlFileFullPath) - 4 )) + '.kml';

        assignfile(kmlTextFile, kmlFileFullPath) ;
        rewrite(kmlTextFile) ;
        write(kmlTextFile, kmlDocument) ;
        flush(kmlTextFile);
      finally
        closefile(kmlTextFile) ;
      end;

      //Compress the KML to create a KMZ file and then delete the KML
       if FileExists(kmlFileFullPath) then
          begin
            // Create the KMZ Full Path Name
            kmzFileFullPath := kmlFileFullPath;
            kmzFileFullPath := StringReplace(kmzFileFullPath, '.kml', '.kmz', [rfReplaceAll, rfIgnoreCase]);
            if (AnsiRightStr(kmzFileFullPath, 4) <>  LowerCase('.kmz'))
            then kmzFileFullPath := kmzFileFullPath + '.kmz';

            try
              //Zip up the kml file
              zipper := TZipMaster.Create( nil );
              zipper.ZipFileName := kmzFileFullPath;
              zipper.FSpecArgs.Add( kmlFileFullPath );
              zipper.Add();
              //delete the kml file
              DeleteFile(kmlFileFullPath);

              msgOK(
                tr( 'The KMZ file was sucessfully created.' ),
                SHORTMASTERCAPTION,
                IMGSuccess,
                frmMain
              );

            finally
              FreeAndNil(zipper);
            end;
          end
        ;
    end
  ;


end.



