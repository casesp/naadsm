unit DialogRunSimException;

(*
DialogRunSimException.pas/dfm
-----------------------------
Begin: 2004/08/20
Last revision: $Date: 2009-09-02 15:50:31 $ $Author: areeves $
Version: $Revision: 1.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2004 - 2008 Animal Population Health Institute, Colorado State University

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
    StdCtrls
  ;

  type TDialogRunSimException = class( TForm )
      lblErrorOccurred: TLabel;
      lblSendError: TLabel;
      lblErrorFileNameLabel: TLabel;
      btnOK: TButton;
    	lblFileName: TLabel;

    	procedure btnOKClick(Sender: TObject);
    	procedure FormCreate(Sender: TObject);

    protected
      procedure translateUI();

    public
      constructor create( AOwner: TComponent; errFileName: string ); reintroduce;
      
    end
  ;


implementation

{$R *.dfm}

	uses
  	ControlUtils,
    I88n
  ;

	constructor TDialogRunSimException.create( AOwner: TComponent; errFileName: string );
  	begin
    	inherited create( AOwner );
      translateUI();
      
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest.
    	
      lblFileName.Caption := errFileName;
    end
  ;


  procedure TDialogRunSimException.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.5.0.
      // Generation date: Thu Feb 21 20:53:29 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/DialogRunSimException.dfm
      // File date: Thu Oct 12 14:33:46 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Simulation error' );
          lblErrorOccurred.Caption := tr( 'An error occurred while this simulation was in progress, and an error file has been created.' );
          lblSendError.Caption := tr( 'We would appreciate your help in solving this problem: please send the error file (named below) to the NAADSM Development Team for analysis.  Thank you!' );
          lblErrorFileNameLabel.Caption := tr( 'Error file name:' );
          btnOK.Caption := tr( 'OK' );
        end
      ;

    end
  ;

	procedure TDialogRunSimException.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;


  procedure TDialogRunSimException.btnOKClick(Sender: TObject);
    begin
    	close();
    end
  ;

end.
