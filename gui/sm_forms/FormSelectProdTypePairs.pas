unit FormSelectProdTypePairs;

(*
FormSelectProdTypePairs.pas/dfm
-------------------------------
Begin: 2008/04/21
Last revision: $Date: 2008-11-25 22:00:30 $ $Author: areeves $
Version number: $Revision: 1.4 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
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

    ProductionTypePair,
    ProductionTypePairList 
    
  ;

  type TFormSelectProdTypePairs = class(TForm)
    pnlInstructions: TPanel;
    lblInstructions: TLabel;
    pnlButtons: TPanel;
    btnCopy: TButton;
    btnCancel: TButton;
    pnlSelectedPairs: TPanel;
    pnlSelectedPairsCaption: TPanel;
    lbxSelectedPairs: TListBox;
    procedure btnCopyClick(Sender: TObject);

    protected
      _selectedPairs: TQObjectList;

      procedure translateUI();
      procedure translateUIManual();
      
    public
      constructor Create( AOwner: TForm; const ptpList: TProductionTypePairList; const currentPTP: TProductionTypePair ); reintroduce;
      destructor Destroy(); override;

      property selectedPairs: TQObjectList read _selectedPairs;
    end
  ;


implementation

{$R *.dfm}

  uses
    DebugWindow,
    I88n,

    Models
  ;

  constructor TFormSelectProdTypePairs.Create( AOwner: TForm; const ptpList: TProductionTypePairList; const currentPTP: TProductionTypePair );
    var
      it: TProductionTypePairListIterator;
    begin
      inherited create( AOwner );
      translateUI();

      lbxSelectedPairs.Clear();

      it := TProductionTypePairListIterator.create( ptpList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( currentPTP <> it.current() ) then
            lbxSelectedPairs.AddItem( it.current().pairDescr, it.current() )
          ;
          it.incr();
        end
      ;

      it.Free();

      _selectedPairs := TQObjectList.create();
    end
  ;


  procedure TFormSelectProdTypePairs.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Mon Apr 28 16:42:33 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Gilpin/sm_forms/FormSelectProdTypePairs.dfm
      // File date: Mon Apr 21 17:11:29 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Select production type combinations' );
          lblInstructions.Caption := tr( 'To copy the parameters for the current production type combination to other combinations, select one or more from the list below and click ''Copy'':' );
          btnCopy.Caption := tr( '&Copy' );
          btnCancel.Caption := tr( 'Cancel' );
          pnlSelectedPairsCaption.Caption := tr( 'Production type combinations' );
          lbxSelectedPairs.Hint := tr( 'Press CTRL + left mouse button to select multiple combinations' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
           lbxSelectedPairs.Items[0] := tr( 'Cattle - beef' );
           lbxSelectedPairs.Items[1] := tr( 'Cattle - dairy' );
           lbxSelectedPairs.Items[2] := tr( 'Mixed - beef/dairy' );
           lbxSelectedPairs.Items[3] := tr( 'Swine - confinement' );
           lbxSelectedPairs.Items[4] := tr( 'Swine - outside' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      translateUIManual();
    end
  ;


  procedure TFormSelectProdTypePairs.translateUIManual();
    begin
    end
  ;


  destructor TFormSelectProdTypePairs.Destroy();
    begin
      _selectedPairs.Free(); // DO NOT free the values in the list: they are used elsewhere.

      inherited destroy();
    end
  ;

  
  procedure TFormSelectProdTypePairs.btnCopyClick(Sender: TObject);
    var
      i: integer;
    begin
      _selectedPairs.clear();

      for i := 0 to lbxSelectedPairs.Items.Count - 1 do
        begin
          if( lbxSelectedPairs.Selected[i] ) then
            _selectedPairs.append( lbxSelectedPairs.Items.Objects[i] )
          ;
        end
      ;
    end
  ;
end.
