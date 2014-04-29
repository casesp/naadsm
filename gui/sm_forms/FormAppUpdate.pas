unit FormAppUpdate;

(*
FormAppUpdate.pas/dfm
----------------------
Begin: 2006/06/05
Last revision: $Date: 2011-07-12 16:38:17 $ $Author: rhupalo $
Version number: $Revision: 1.1.10.1 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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

    QOrderedDictionaries
  ;


  type TAppUpdateInfo = class
    protected
      _major: string;
      _minor: string;
      _build: string;
      _priority: integer;
      _description: string;
      _url: string;
      _timestamp: string;

      function getMajor(): string;
      function getMinor(): string;
      function getBuild(): string;
      function getPriority(): integer;
      function getDescription(): string;
      function getUrl(): string;
      function getTimestamp(): string;

      function getMajorAsFloat(): double;
      function getMinorAsInt(): integer;
      function getBuildAsInt(): integer;

    public
      constructor create(
        xMajor: string;
        xMinor: string;
        xBuild: string;
        xPriority: integer;
        xDescription: string;
        xURL: string;
        xTimestamp: string
      );

      procedure debug();

      property major: string read getMajor;
      property majorAsFloat: double read getMajorAsFloat;

      property minor: string read getMinor;
      property minorAsInt: integer read getMinorAsInt;

      property build: string read getBuild;
      property buildAsInt: integer read getBuildAsInt;

      property priority: integer read getPriority;
      property description: string read getDescription;
      property url: string read getUrl;
      property timestamp: string read getTimestamp;
    end
  ;


  type TAppUpdateInfoDictionary = class( TQOrderedStringObjectDictionary )
    protected
      function GetItem(const Key: String): TAppUpdateInfo; reintroduce;
    public
      function GetItemByIndex(const Idx: Integer): TAppUpdateInfo; reintroduce;

      procedure debug();
    end
  ;


  type TFormAppUpdate = class( TForm )
      pnlHeader: TPanel;
      lblVersionIDs: TLabel;
      mmoNewFeatures: TMemo;
      pnlInstructions: TPanel;
      pnlButtons: TPanel;
      btnURL: TButton;
      Button2: TButton;
      btnNoncritical: TButton;
      lblInstructions1: TLabel;
      lblURL: TLabel;
      lblInstructions2: TLabel;
      lblNoncritical: TLabel;
      lblPriority: TLabel;
      Label3: TLabel;
      imgIcon: TImage;

			procedure FormCreate( Sender: TObject );
      procedure urlClick( Sender: TObject );
      procedure btnClick( Sender: TObject );

    public
      constructor create(
        AOwner: TComponent;
        const updateXML: string;
        const majorVersionNumber: string;
        const minorVersionNumber: string;
        formToHide: TForm = nil
      ); reintroduce;

    protected
      _updateVersMajor: string;
      _updateVersMinor: string;

      procedure translateUI();

      function getUpdateRequired(
        const updateXML: string;
        const majorVersionNumber: string;
        const minorVersionNumber: string;
        strings: TStringList = nil
      ): boolean;
    end
  ;


implementation

{$R *.dfm}

  uses
    ShellAPI,
    Registry,

    Sdew,

    MyStrUtils,
    DebugWindow,
    WindowsUtils,
    I88n,

    ImageResources,
    ControlUtils
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
  constructor TAppUpdateInfo.create(
        xMajor: string;
        xMinor: string;
        xBuild: string;
        xPriority: integer;
        xDescription: string;
        xURL: string;
        xTimestamp: string
      );
    begin
      _major := trim( xMajor );
      _minor := trim( xMinor );
      _build := trim( xBuild );
      _priority := xPriority;
      _description := trim( xDescription );
      _url := trim( xURL );
      _timestamp := trim( xTimestamp );
    end
  ;


  procedure TAppUpdateInfo.debug();
    begin
      dbcout( '--------- TAppUpdateInfo.debug()...', true );
      dbcout( 'Major: ' + major, true );
      dbcout( 'Minor: ' + minor, true );
      dbcout( 'Build: ' + build, true );
      dbcout( 'Priority: ' + intToStr( priority ), true );
      dbcout( 'Description: ' + description, true );
      dbcout( 'Url: ' + url, true );
      dbcout( 'Timestamp: ' + timestamp, true );
      dbcout( '--------- done.', true );
      dbcout( endl, true );
    end
  ;


  function TAppUpdateInfo.getMajor(): string; begin result := _major; end;
  function TAppUpdateInfo.getMinor(): string; begin result := _minor; end;
  function TAppUpdateInfo.getBuild(): string; begin result := _build; end;
  function TAppUpdateInfo.getPriority(): integer; begin result := _priority; end;
  function TAppUpdateInfo.getDescription(): string; begin result := _description; end;
  function TAppUpdateInfo.getUrl(): string; begin result := _url; end;
  function TAppUpdateInfo.getTimestamp(): string; begin result := _timestamp; end;

  function TAppUpdateInfo.getMajorAsFloat(): double; begin result := usStrToFloat( major, -1.0 ); end;
  function TAppUpdateInfo.getMinorAsInt(): integer; begin result := myStrToInt( minor, -1 ); end;
  function TAppUpdateInfo.getBuildAsInt(): integer; begin result := myStrToInt( minor, -1 ); end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
  function TAppUpdateInfoDictionary.GetItem(const Key: String): TAppUpdateInfo;
    begin
      //result := inherited getItem( key ) as TAppUpdateInfo;
      result := inherited value( key ) as TAppUpdateInfo;
    end
  ;


  function TAppUpdateInfoDictionary.GetItemByIndex(const Idx: Integer): TAppUpdateInfo;
    begin
      result := inherited getItemByIndex( Idx ) as TAppUpdateInfo;
    end
  ;


  procedure TAppUpdateInfoDictionary.debug();
    var
      app: TAppUpdateInfo;
      key: string;
      i: integer;
    begin
      dbcout( '====================== TAppUpdateInfoDictionary.debug()', true );

      for i := 0 to self.count - 1 do
        begin
          key := self.keyByIndex( i );
          app := self.GetItemByIndex( i );

          dbcout( endl + key, true );
          app.debug();
        end
      ;

      dbcout( '====================== done', true );
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
  constructor TFormAppUpdate.create(
        AOwner: TComponent;
        const updateXML: string;
        const majorVersionNumber: string;
        const minorVersionNumber: string;
        formToHide: TForm = nil
      );
    var
      updateRequired: boolean;
      strings: TStringList;

      skipVersMajor: double;
      skipVersMinor: integer;
    begin
      inherited create( AOwner );
      translateUI();

      strings := TStringList.Create();

      updateRequired := getUpdateRequired( updateXML, majorVersionNumber, minorVersionNumber, strings );
      
      if( updateRequired ) then
        begin
          // Is there anything in the update message?
          //-----------------------------------------
          dbcout( 'Update XML is:', DBSHOWMSG );
          dbcout( updateXML, DBSHOWMSG );

          if( 6 <> strings.Count ) then
            begin
              dbcout( 'XML Update string is incorrect: there are ' + intToStr( strings.Count ) + ' elements in the array.', DBSHOWMSG );
              exit;
            end
          ;

          // Check to see if (a) the user clicked "skip noncritical update"
          // the last time, and (b) there are no new updates since that click.
          //------------------------------------------------------------------
          try
            skipVersMajor := double( getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','SkipUpdateMajor' ) );
          except
            skipVersMajor := -1.0;
          end;

          try
            skipVersMinor := integer( getRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','SkipUpdateMinor' ) );
          except
            skipVersMinor := -1;
          end;

          _updateVersMajor := strings[4];
          _updateVersMinor := strings[5];

          if
            ( usStrToFloat( strings[4] ) <= skipVersMajor )
          and
            ( myStrToInt( strings[5] ) <= skipVersMinor )
          then
            begin
              dbcout( 'No updated is needed.', DBSHOWMSG );
              exit;
            end
          ;

          // If we get this far, alert the user about the update.
          //-----------------------------------------------------
          if( nil <> formtoHide ) then formToHide.Hide();

          lblVersionIDs.Caption := tr( 'An updated version' ) 
            + ' (' + strings[0] + ') ' 
            + tr( 'of NAADSM is available: you are currently using version' )
            + ' ' + majorVersionNumber + '.' + minorVersionNumber + '.'
          ;

          lblURL.Caption := strings[1] + '&&fromVersion=' + majorVersionNumber + '.' + minorVersionNumber;

          mmoNewFeatures.Lines.Text := strings[2];

          case strToInt( strings[3] ) of
            1:
              begin
                lblPriority.Caption := tr( 'This is a critical update: it corrects programming bugs or model implementation errors that may result in incorrect calculations or data loss.' );
                imgIcon.Picture.Bitmap := bitmapStyle( BITCritical );
                lblNoncritical.Visible := false;
                btnNoncritical.visible := false;
              end
            ;
            2:
              begin
                lblPriority.Caption := tr( 'This is an important update: it corrects problems that may cause NAADSM to crash unexpectedly.' );
                imgIcon.Picture.Bitmap := bitmapStyle( BITWarning );
                lblNoncritical.Visible := true;
                btnNoncritical.visible := true;
              end
            ;
            3, 4, 5:
              begin
                lblPriority.Caption := tr( 'This is not a critical update, but it corrects one or more minor problems or introduces useful new features.' );
                imgIcon.Picture.Bitmap := bitmapStyle( BITInformation );
                lblNoncritical.Visible := true;
                btnNoncritical.visible := true;
              end
            ;
            else
              raise exception.Create( 'Unrecognized priority in TFormAppUpdate.create().' )
            ;
          end;

          self.ShowModal();
        end
      else
        begin
          // Do nothing (of consequence, anyway)
          dbcout( 'An update is not needed', DBSHOWMSG );
        end
      ;
      strings.Free;
    end
  ;


  procedure TFormAppUpdate.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FormAppUpdate.dfm
      // File date: Thu Jan 4 11:16:16 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'NAADSM updates' );
          lblVersionIDs.Caption := tr( 'An updated version (xxx) of NAADSM is available: you are currently using version yyy.' );
          lblPriority.Caption := tr( 'This update is critical, and highly recommended.  This line should wrap this line should wrap.' );
          Label3.Caption := tr( 'A description of the enhancements in the new version is shown below:' );
          lblInstructions1.Caption := tr( 'More information about this update is available at:' );
          lblURL.Caption := tr( 'http://www.naadsm.org/updates.php?version=3.0.79&&fromVersion=3.0.78' );
          lblInstructions2.Caption := tr( 'You may go to the NAADSM website now to download the update, if you wish.' );
          lblNoncritical.Caption := tr( 'If you do not want to install this noncritical update, click ''Skip this update'' below, and you will not be prompted again until the next update becomes available.' );
          btnURL.Caption := tr( '&Go to the NAADSM website' );
          Button2.Caption := tr( '&Do not update now' );
          btnNoncritical.Caption := tr( '&Skip this update' );
        end
      ;

    end
  ;


	procedure TFormAppUpdate.FormCreate(Sender: TObject);
		begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if( Screen.PixelsPerInch <> 96 ) then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;


  function TFormAppUpdate.getUpdateRequired(
        const updateXML: string;
        const majorVersionNumber: string;
        const minorVersionNumber: string;
        strings: TStringList = nil
      ): boolean;
    var
      doc: TSdew;
      versionsElement: pointer;
      versionElement: pointer;

      i: integer;

      updateInfo: TAppUpdateInfo;
      dict: TAppUpdateInfoDictionary;

      currentMajor: double;
      currentMinor: integer;

      overallPriority: integer;
      updateDescr: string;
      updateURL: string;
      newestVersion: string;
      newMajor, newMinor: string;
      newestVersionMajor: string;
      newestVersionMinor: string;
    begin
      dict := nil;
      
      try
        try
          // Parse the update message
          doc := TSdew.createFromString( updateXML );

          versionsElement := doc.getElementByName( doc.getRootTree(), 'versions' );

          if( nil <> versionsElement ) then
            begin
              currentMajor := usStrToFloat( majorVersionNumber );
              currentMinor := myStrToInt( minorVersionNumber );

              dict := TAppUpdateInfoDictionary.Create();

              for i := 0 to doc.getElementCount( versionsElement ) - 1 do
                begin
                  versionElement := doc.GetElementByIndex( versionsElement, i );
                  if( 'version' = doc.getElementName( versionElement ) ) then
                    begin
                      // Insert update information into the dictionary if it is for a newer version than the current one.
                      // Otherwise, ignore it.
                      newMajor := trim( doc.getAttributeValue( versionElement, 'major' ) );
                      newMinor := trim( doc.getAttributeValue( versionElement, 'minor' ) );

                      if
                        ( usStrToFloat( newMajor ) >= currentMajor )
                      and
                        ( usStrToFloat( newMinor ) > currentMinor )
                      then
                        begin
                          updateInfo := TAppUpdateInfo.create(
                            newMajor,
                            newMinor,
                            doc.getAttributeValue( versionElement, 'build' ),
                            myStrToInt( doc.GetElementContents( versionElement, 'priority' ) ),
                            doc.GetElementContents( versionElement, 'description' ),
                            doc.GetElementContents( versionElement, 'url' ),
                            doc.GetElementContents( versionElement, 'timestamp' )
                          );

                          dict.insert( updateInfo.major + '.' + updateInfo.minor, updateInfo );
                        end
                      ;
                    end
                  ;
                end
              ;

              //dict.debug();

              updateDescr := '';
              updateURL := '';
              newestVersion := '';

              if( 0 < dict.Count ) then
                begin
                  // Get some information about the latest (newest) version...
                  newestVersion := dict.keyAtIndex( 0 );

                  updateInfo := dict.GetItemByIndex( 0 );
                  updateURL := updateInfo.url;
                  newestVersionMajor := updateInfo.major;
                  newestVersionMinor := updateInfo.minor;
                  overallPriority := updateInfo.priority;
                  updateDescr := updateInfo.description + endl;

                  // ... then see if we need to know anything else about older releases.
                  for i := 1 to dict.Count - 1 do
                    begin
                      updateInfo := dict.GetItemByIndex( i );
                      if( updateInfo.priority < overallPriority ) then
                        overAllPriority := updateInfo.priority
                      ;
                      if( 0 < length( updateInfo.description ) ) then
                        updateDescr := updateDescr + updateInfo.description + endl
                      ;
                    end
                  ;

                  if( nil <> strings ) then
                    begin
                      strings.Append( newestVersion );
                      strings.Append( updateURL );
                      strings.Append( updateDescr );
                      strings.Append( intToStr( overallPriority ) );
                      strings.Append( newestVersionMajor );
                      strings.Append( newestVersionMinor );
                    end
                  ;

                  // Since the dictionary contains items, an update must be necessary.
                  result := true;
                end
              else // If the dictionary is empty, no update is needed.
                result := false
              ;
            end
          else // VersionsElement was nil: there was a problem with the XML
            begin
              dbcout( 'XML problem in TFormAppUpdate.getUpdateRequired()', true );
              result := false;
            end
          ;
        except
          dbcout( 'Exception raised while parsing XML update message in TFormAppUpdate.getUpdateRequired()', true );
          result := false;
        end;
      finally
        if( nil <> dict ) then
          begin
            dict.deleteValues();
            dict.Free();
          end
        ;
        freeAndNil( doc );
      end;
    end
  ;


  procedure TFormAppUpdate.urlClick(Sender: TObject);
    begin
      ShellExecute(
        Application.Handle,
        PChar( 'open' ),
        PChar( lblURL.Caption ),
        PChar( 0 ),
        nil,
        SW_NORMAL
      );

      if( btnURL = sender ) then self.Close();
    end
  ;


  procedure TFormAppUpdate.btnClick( Sender: TObject );
    begin
      if( sender = btnNoncritical ) then
        begin
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','SkipUpdateMajor', rdString, _updateVersMajor );
          setRegistryData( HKEY_CURRENT_USER, 'Software\NAADSM','SkipUpdateMinor', rdString, _updateVersMinor )
        end
      ;

      self.Close();
    end
  ;
//-----------------------------------------------------------------------------


end.
