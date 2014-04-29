unit FormHerdExportOptions;

(*
FormHerdExportOptions.pas/dfm
-----------------------------
Begin: 2005/09/01
Last revision: $Date: 2013-06-27 19:11:26 $ $Author: areeves $
Version number: $Revision: 1.11.6.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

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
    ExtCtrls,
    FrameFileSelector
  ;


  type TFormHerdExportOptions = class( TForm )
      fraExportFile: TFrameFileSelector;
      rdgFileFormat: TRadioGroup;
      gbxCSVOptions: TGroupBox;
      rdgProdTypes: TRadioGroup;
      rdgInitialStates: TRadioGroup;
      btnOK: TButton;
      btnCancel: TButton;
      pnlXMLOptions: TPanel;

			procedure FormCreate(Sender: TObject );

      procedure rdgFileFormatClick(Sender: TObject);

      procedure btnOKClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);

      function getFileFormat(): integer;
      function getProdTypeByName(): boolean;
      function getInitialStateAsCharacter(): boolean;
      function getFileName(): string;

    protected
      _execute: boolean;

      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      function execute(): boolean;

      property fileFormat: integer read getFileFormat;
      property productionTypeByName: boolean read getProdTypeByName;
      property initialStateAsCharacter: boolean read getInitialStateAsCharacter;
      property fileName: string read getFileName;
    end
  ;

implementation

{$R *.dfm}

  uses
    MyStrUtils,
    MyDialogs,
    ControlUtils,
    I88n,
    Herd
  ;


  constructor TFormHerdExportOptions.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest.  

      with pnlXmlOptions do
        begin
          top := gbxCsvOptions.Top;
          left := gbxCsvOptions.Left;
          width := gbxCsvOptions.Width;
          height := gbxCsvOptions.Height;
          visible := false;
        end
      ;

      rdgFileFormat.ItemIndex := 0; // CSV format
      rdgFileFormatClick( nil );
    end
  ;


  procedure TFormHerdExportOptions.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:55 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormHerdExportOptions.dfm
      // File date: Thu Oct 12 14:33:57 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Unit list export options' );
          rdgFileFormat.Caption := tr( 'File format' );
          gbxCSVOptions.Caption := tr( 'CSV options' );
          rdgProdTypes.Caption := tr( 'Production types' );
          rdgInitialStates.Caption := tr( 'Initial disease states' );
          btnOK.Caption := tr( 'OK' );
          btnCancel.Caption := tr( 'Cancel' );
          pnlXMLOptions.Caption := tr( '(There are no XML options)' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          rdgFileFormat.Items[0] := tr( 'Export as CSV' );
          rdgFileFormat.Items[1] := tr( 'Export as XML' );

          rdgProdTypes.Items[0] := tr( 'Export production types by name' );
          rdgProdTypes.Items[1] := tr( 'Export production types by number' );

          rdgInitialStates.Items[0] := tr( 'Export initial states as character codes' );
          rdgInitialStates.Items[1] := tr( 'Export initial states as numeric codes' );
        end
      ;

    end
  ;


	procedure TFormHerdExportOptions.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> PixelsPerInch then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;

  destructor TFormHerdExportOptions.destroy();
    begin
      inherited destroy();
    end
  ;


  function TFormHerdExportOptions.execute(): boolean;
    begin
      showModal();
      result := _execute;
    end
  ;

  procedure TFormHerdExportOptions.rdgFileFormatClick( Sender: TObject );
    begin
      case rdgFileFormat.ItemIndex of
        1: // XML format
          begin
           setChildrenEnabled( gbxCSVOptions, false, true );
           gbxCSVOptions.Visible := false;
           pnlXMLOptions.Visible := true;
           fraExportFile.fileNameExtension := '.xml';
           fraExportFile.filter := tr( 'NAADSM XML (*.xml)|*.xml' );
          end
        ;
        0: // CSV format
          begin
            pnlXMLOptions.Visible := false;
            gbxCSVOptions.Visible := true;
            setChildrenEnabled( gbxCSVOptions, true, true );
            fraExportFile.fileNameExtension := '.csv';
            fraExportFile.filter := tr( 'Comma separated values (*.csv)|*.csv' );
          end
        ;
      end;
    end
  ;


  procedure TFormHerdExportOptions.btnOKClick(Sender: TObject);
    begin
      if( 0 = length( fraExportFile.fileName ) ) then
        begin
          msgOK(
            tr( 'Please enter a valid file name.' ),
            tr( 'File name is missing' ),
            IMGWarning,
            self
          );
          fraExportFile.btnBrowse.SetFocus();
        end
      else
        begin
          _execute := true;
          close();
        end
      ;
    end
  ;


  procedure TFormHerdExportOptions.btnCancelClick(Sender: TObject);
    begin
      _execute := false;
      close();
    end
  ;


  function TFormHerdExportOptions.getFileFormat(): integer;
    begin
      result := rdgFileFormat.ItemIndex + 1;
    end
  ;


  function TFormHerdExportOptions.getProdTypeByName(): boolean;
    begin
      result := ( 0 = rdgProdTypes.ItemIndex );
    end
  ;


  function TFormHerdExportOptions.getInitialStateAsCharacter(): boolean;
    begin
      result := ( 0 = rdgInitialStates.ItemIndex );
    end
  ;


  function TFormHerdExportOptions.getFileName(): string;
    begin
      result := fraExportFile.fileName;
    end
  ;

end.
