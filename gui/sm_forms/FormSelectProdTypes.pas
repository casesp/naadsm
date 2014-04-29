unit FormSelectProdTypes;

(*
FormSelectProdTypes.pas/dfm
---------------------------
Begin: 2008/04/21
Last revision: $Date: 2008/11/25 22:00:30 $ $Author: areeves $
Version number: $Revision: 1.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2008 Animal Population Health Institute, Colorado State University

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

    QLists,

    ProductionType,
    ProductionTypeList
  ;

  type TFormSelectProdTypes = class(TForm)
    pnlProdTypes: TPanel;
    pnlProdTypeCaption: TPanel;
    lbxProdTypes: TListBox;
    pnlInstructions: TPanel;
    lblInstructions: TLabel;
    pnlButtons: TPanel;
    btnCopy: TButton;
    btnCancel: TButton;
    procedure btnCopyClick(Sender: TObject);

    protected
      _selectedTypes: TQObjectList;

      procedure translateUI();
      procedure translateUIManual();

    public
      constructor Create( AOwner: TForm; const ptList: TProductionTypeList; const currentPT: TProductionType ); reintroduce;
      destructor Destroy(); override;

      property selectedTypes: TQObjectList read _selectedTypes;
    end
  ;


implementation

{$R *.dfm}

  uses
    DebugWindow,
    I88n
  ;

  constructor TFormSelectProdTypes.Create( AOwner: TForm; const ptList: TProductionTypeList; const currentPT: TProductionType );
    var
      it: TProductionTypeListIterator;
    begin
      inherited create( AOwner );
      translateUI();

      lbxProdTypes.Clear();

      it := TProductionTypeListIterator.create( ptList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( currentPT <> it.current() ) then
            lbxProdTypes.AddItem( it.current().productionTypeDescr + ' (#' + intToStr( it.current().productionTypeID ) + ')', it.current() );
          ;
          it.incr();
        end
      ;

      it.Free();

      _selectedTypes := TQObjectList.create();
    end
  ;


  procedure TFormSelectProdTypes.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FormSelectProdTypes.dfm
      // File date: Mon Apr 21 17:11:29 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Select production types' );
          pnlProdTypeCaption.Caption := tr( 'Production types' );
          lbxProdTypes.Hint := tr( 'Press CTRL + left mouse button to select multiple production types' );
          lblInstructions.Caption := tr( 'To copy the parameters for the current production type to other production types, select one or more from the list below and click ''Copy'':' );
          btnCopy.Caption := tr( '&Copy' );
          btnCancel.Caption := tr( 'Cancel' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
           lbxProdTypes.Items[0] := tr( 'Cattle - beef' );
           lbxProdTypes.Items[1] := tr( 'Cattle - dairy' );
           lbxProdTypes.Items[2] := tr( 'Mixed - beef/dairy' );
           lbxProdTypes.Items[3] := tr( 'Swine - confinement' );
           lbxProdTypes.Items[4] := tr( 'Swine - outside' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormSelectProdTypes.translateUIManual();
    begin
    end
  ;


  destructor TFormSelectProdTypes.Destroy();
    begin
      _selectedTypes.Free(); // DO NOT free the values in the list: they are used elsewhere.

      inherited destroy();
    end
  ;

  
  procedure TFormSelectProdTypes.btnCopyClick(Sender: TObject);
    var
      i: integer;
    begin
      _selectedTypes.clear();

      for i := 0 to lbxProdTypes.Items.Count - 1 do
        begin
          if( lbxProdTypes.Selected[i] ) then
            _selectedTypes.append( lbxProdTypes.Items.Objects[i] )
          ;
        end
      ;
    end
  ;

end.
