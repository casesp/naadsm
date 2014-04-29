unit DMOutputActionManager;

(*
DMOutputActionManager.pas
-------------------------
Begin: 2005/12/07
Last revision: $Date: 2010-06-28 18:29:03 $ $Author: areeves $
Version number: $Revision: 1.8.12.1 $
Project: (various)
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

	uses
		SysUtils, 
		Classes, 
		ImgList, 
		Controls, 
		ActnList, 
		XPStyleActnCtrls, 
		ActnMan
	;

	type TDMOutputActionManager = class(TDataModule)
			ActionManager1: TActionManager;

			acnSaveData: TAction;
			acnSaveCharts: TAction;
			acnPrintData: TAction;
			acnPrintCharts: TAction;
			acnCopyData: TAction;
			acnCopyCharts: TAction;
			acnClose: TAction;

      acnCopyRawData: TAction;
      acnExportRawData: TAction;

			ImageList1: TImageList;

    protected
      procedure translateUI();
      procedure translateUIManual();

		public
      constructor create( AOwner: TComponent ); override;
			destructor destroy(); override;
		end
	;


implementation

{$R *.dfm}

  uses
    MyStrUtils,
    DebugWindow,
    I88n
  ;

	
	const
		DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit.



  constructor TDMOutputActionManager.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      acnCopyRawData.visible := false;
      acnExportRawData.visible := false;
    end
  ;


  procedure TDMOutputActionManager.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 20:20:29 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/DMOutputActionManager.dfm
      // File date: Mon Feb 25 15:25:29 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          acnSaveData.Caption := tr( 'Save data to file...' );
          acnSaveCharts.Caption := tr( 'Save chart(s) to file...' );
          acnPrintData.Caption := tr( 'Print data...' );
          acnPrintCharts.Caption := tr( 'Print chart(s)...' );
          acnCopyData.Caption := tr( 'Copy data to clipboard' );
          acnCopyCharts.Caption := tr( 'Copy chart(s) to clipboard' );
          acnExportRawData.Caption := tr( 'Export &raw data for selected output to file...' );
          acnClose.Caption := tr( 'Close' );
        end
      ;

      // Set action Caption properties
      with self do
        begin
          acnSaveData.Caption := tr( '&Save data to file...' );
          acnSaveCharts.Caption := tr( 'S&ave chart(s) to file...' );
          acnPrintData.Caption := tr( '&Print data...' );
          acnPrintCharts.Caption := tr( 'P&rint chart(s)...' );
          acnClose.Caption := tr( '&Close' );
          acnCopyData.Caption := tr( '&Copy data to clipboard' );
          acnCopyCharts.Caption := tr( 'C&opy chart(s) to clipboard' );
          acnCopyRawData.Caption := tr( 'Copy &raw data for selected output to clipboard' );
        end
      ;

      translateUIManual();
    end
  ;


  procedure TDMOutputActionManager.translateUIManual();
    begin
      with self do
        begin
          ActionManager1.ActionBars[0].Items[0].caption := tr( '&File' );
          ActionManager1.ActionBars[0].Items[1].caption := tr( '&Edit' );
        end
      ;
    end
  ;


  destructor TDMOutputActionManager.destroy();
    begin
      dbcout( 'DATA MODULE IS BEING DESTROYED', DBSHOWMSG );
      inherited destroy();
    end
  ;


end.
