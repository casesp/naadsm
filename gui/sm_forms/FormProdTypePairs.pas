unit FormProdTypePairs;

(*
FormProdTypePairs.pas/dfm
-------------------------
Begin: 2005/04/02
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version: $Revision: 1.28.4.1 $
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
		ToolWin, 
		ActnMan, 
		ActnCtrls, 
		ActnList, 
		XPStyleActnCtrls,
		ExtCtrls, 
		StdCtrls, 
		Buttons,
		ActnMenus,
    Menus,
    ActnPopupCtrl,
    
    ProductionType,
    ProductionTypeList,
    ProductionTypePair,
    ProductionTypePairList,
    FormSMWizardBase
	 ;

	type TFormProdTypePairs = class( TFormSMWizardBase )
      pnlSelectedPairs: TPanel;
      lbxSelectedPairs: TListBox;
      pnlCenter: TPanel;
      pnlPairsAndProdTypes: TPanel;
      lbxAllPairs: TListBox;
      pnlProdTypes: TPanel;
      lbxSource: TListBox;
      lbxDest: TListBox;
      pnlCaption: TPanel;

			BitBtnAdd: TBitBtn;
			BitBtnRemove: TBitBtn;

			Panel9: TPanel;
			Panel2: TPanel;
 			Panel5: TPanel;
			Panel7: TPanel;

			procedure lbxSelectedPairsClick(Sender: TObject);
			procedure lbxAllPairsClick(Sender: TObject);
			procedure BitBtnAddClick(Sender: TObject);
			procedure BitBtnRemoveClick(Sender: TObject);
			procedure lbxSourceClick(Sender: TObject);
			procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean);

		protected
      ptList: TProductionTypeList;
      selectedPairsList: TProductionTypePairList;
      allPairsList: TProductionTypePairList;

			DataHasChanged: boolean;

      procedure translateUI();

//      procedure initializeFromDatabase(); override;
//      procedure updateDatabase(); override;
      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;

      procedure updateDisplay();
			procedure UpdateButtons();
			function MatchingSelected(): boolean;
      procedure updateScenarioData(); override;

      // This function is overridden from TFormSMWizardBase
      function getSelectedProdTypePairIndex(): integer; override;
      function getDataUpdated(): boolean; override;

    public
      constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

    	function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

		end
	;

  const
  	DBFORMPRODTYPEPAIRS: boolean = false; // set to true to enable debugging messages for this unit
  
implementation

{$R *.dfm}

	uses
    MyStrUtils,
    DebugWindow,
    MyDialogs,
    I88n,

    Models
	;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormProdTypePairs.create( aOwner: TComponent );
  	begin
      inherited create( aOwner );
      translateUI();

			pnlSelectedPairs.Width :=  pnlCaption.Width div 3;
			pnlCenter.Width := pnlSelectedPairs.Width;
			BitBtnAdd.Left := (pnlCenter.Width - BitBtnAdd.Width) div 2;
			BitBtnRemove.Left := BitBtnAdd.Left;
			BitBtnAdd.Top := (pnlCenter.Height div 2) - (BitBtnAdd.Height + 5);
			BitBtnRemove.Top := (pnlCenter.Height div 2) + 5;

      ptList := nil;
      selectedPairsList := nil;
      allPairsList := nil;

			DataHasChanged := False;
    end
  ;


  procedure TFormProdTypePairs.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormProdTypePairs.dfm
      // File date: Mon Mar 19 11:05:32 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Production type combinations' );
          pnlSelectedPairs.Hint := tr( 'Press CTRL key to select more than one production type' );
          Panel2.Caption := tr( 'Production type combinations' );
          lbxSelectedPairs.Hint := tr( 'Press CTRL + left mouse button to select multiple production types' );
          BitBtnAdd.Caption := tr( 'Add' );
          BitBtnAdd.Hint := tr( 'Removes selected combinations from the model' );
          BitBtnRemove.Caption := tr( 'Remove' );
          BitBtnRemove.Hint := tr( 'Includes selected combinations in the model' );
          Panel5.Caption := tr( 'Choose from all combinations' );
          Panel7.Caption := tr( 'Match production types below' );
          pnlCaption.Caption := tr( 'Production type combinations' );
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

    end
  ;


  procedure TFormProdTypePairs.initializeFromSim();
  	var
    	it: TProductionTypeListIterator;
      ptpIt: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
    begin
    	ptList := _smScenarioCopy.simInput.ptList;

      lbxSource.clear();
      lbxDest.clear();

      // These list boxes are populated exactly once: they don't change.
      it := TProductionTypeListIterator.create( ptList );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
        	lbxSource.AddItem( it.current().productionTypeDescr, it.current() );
          lbxDest.addItem( it.current().productionTypeDescr, it.current() );
          it.incr();
        end
      ;

      selectedPairsList := TProductionTypePairList.create( ptList );
      selectedPairsList.Clear();

      ptpIt := TProductionTypePairListIterator.create( _smScenarioCopy.simInput.ptpList );
      ptpIt.toFirst();

      while( nil <> ptpIt.current() ) do
        begin
          ptp := ptpIt.current();
          if ( not ptp.removed ) then
            begin
              selectedPairsList.append( TProductionTypePair.create( ptp.source, ptp.dest, ptp.sim ) );
            end
          ;

          ptpit.incr();
        end
      ;

      allPairsList := TProductionTypePairList.create( ptList );
			allPairsList.subtract( selectedPairsList );

      it.Free();
      ptpIt.Free();

      // Remaining listboxes require updating periodically, so they're handled separately.
			UpdateDisplay();
    end
  ;


  destructor TFormProdTypePairs.destroy();
  	begin
      allPairsList.free();
      selectedPairsList.free();
//      ptList.free();

    	inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormProdTypePairs.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.includeContactSpreadGlobal or _smScenarioCopy.simInput.includeAirborneSpreadGlobal ) then
    		result := inherited showModal( nextFormToShow, formDisplayed, currentFormIndex )
      else
      	begin
          formDisplayed := false;
      		nextForm := nextFormToShow;
          result := 0;
        end
      ;
    end
  ;

	function TFormProdTypePairs.MatchingSelected(): boolean;
		begin
			Result := (lbxSource.ItemIndex > -1) and (lbxDest.ItemIndex > -1);
		end
	;


  procedure TFormProdTypePairs.updateDisplay();
  	var
      it: TProductionTypePairListIterator;
  	begin

      lbxSelectedPairs.clear();

      it := TProductionTypePairListIterator.create( selectedPairsList );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
          if ( not it.current().removed ) then
            lbxSelectedPairs.addItem( it.current().pairDescr, it.current() )
          ;

          it.incr();
        end
      ;

      it.Free();

      lbxAllPairs.clear();

      it := TProductionTypePairListIterator.create( allPairsList );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
       		lbxAllPairs.addItem( it.current().pairDescr, it.current() );
          it.incr();
        end
      ;

      it.Free();

      if( dataHasChanged ) then
        showStar()
      else
        hideStar()
      ;

    	updateButtons();
    end
  ;


	procedure TFormProdTypePairs.UpdateButtons();
		begin

			BitBtnAdd.Enabled := (lbxAllPairs.SelCount > 0) or( MatchingSelected() );

			if MatchingSelected() then
				begin
					BitBtnAdd.Enabled := lbxSelectedPairs.Items.IndexOf(
						lbxSource.Items[lbxSource.ItemIndex]
						+ ' > ' 
						+ lbxDest.Items[lbxDest.ItemIndex]) = -1
					;

					if( not BitBtnAdd.Enabled ) then
            ShowMessage( tr( 'This combination of production types is already selected' ) )
          ;
				end
			;

			BitBtnRemove.Enabled := lbxSelectedPairs.SelCount > 0;
		end
	;


	procedure TFormProdTypePairs.lbxSelectedPairsClick( Sender: TObject );
		begin
			lbxAllPairs.ClearSelection();
			lbxSource.ClearSelection();
			lbxDest.ClearSelection();
			UpdateButtons();
		end
	;


	procedure TFormProdTypePairs.lbxAllPairsClick( Sender: TObject );
		begin
			lbxSelectedPairs.ClearSelection();
			lbxDest.ClearSelection();
			lbxSource.ClearSelection();
			UpdateButtons();
		end
	;


	procedure TFormProdTypePairs.lbxSourceClick(Sender: TObject);
		begin
			lbxAllPairs.ClearSelection();
			lbxSelectedPairs.ClearSelection();
			UpdateButtons();
		end
	;


	procedure TFormProdTypePairs.FormCanResize( Sender: TObject; var NewWidth, NewHeight: Integer; var Resize: Boolean );
		begin
			lbxAllPairs.Height := pnlPairsAndProdTypes.Height div 2;
			lbxSource.Width := pnlPairsAndProdTypes.Width div 2;
		end
	;
//-----------------------------------------------------------------------------





//-----------------------------------------------------------------------------
// GUI event handling
//-----------------------------------------------------------------------------
	procedure TFormProdTypePairs.BitBtnAddClick( Sender: TObject );
		var
			i : longint;
      ptp: TProductionTypePair;
		begin
			if( MatchingSelected() ) then // The lists of PTs were used.
				begin
        	i := allPairsList.pairPosition( lbxSource.Items.Objects[lbxSource.ItemIndex] as TProductionType, lbxDest.Items.Objects[lbxDest.ItemIndex] as TProductionType );
          ptp := allPairsList.at( i );
          allPairsList.Extract( ptp );
          ptp.removed := false;
          ptp.added := true;

          selectedPairsList.append( ptp );

          lbxSource.ClearSelection();
          lbxDest.ClearSelection();
          dataHasChanged := True;
          updateDisplay();
				end
			else // The list of pre-made pairs was used.
				begin
					i := 0;

					while i < lbxAllPairs.Items.Count do
						begin
							if lbxAllPairs.Selected[i] then
								begin
                	ptp := allPairsList.at(i);
                  allPairsList.Extract( ptp );
                  lbxAllPairs.Items.Delete(i);
                  ptp.removed := false;
                  ptp.added := true;
                  dataHasChanged := True;
                  selectedPairsList.append( ptp );
								end
							else
								Inc(i)
							;
						end
					;
          lbxAllPairs.ClearSelection();
          updateDisplay();
				end
			;

		end
  ;


	procedure TFormProdTypePairs.BitBtnRemoveClick(Sender: TObject);
		var
			i : longint;
      ptp: TProductionTypePair;
		begin
			i := 0;

			while( i < lbxSelectedPairs.Items.Count ) do
				begin
					if( lbxSelectedPairs.Selected[i] ) then
						begin
              ptp := selectedPairsList.at(i);
              selectedPairsList.Extract( ptp );
              lbxSelectedPairs.Items.Delete(i);
              ptp.removed := true;
              ptp.added := false;

              allPairsList.append( ptp );
						end
					else
						Inc(i)
					;
				end
			;

			DataHasChanged := True;
			updateDisplay();
		end
	;
//-----------------------------------------------------------------------------

  procedure TFormProdTypePairs.updateScenarioData();
    begin
      //Copy over ptList here....
      _smScenarioCopy.simInput.ptpList.logicAndAdd( selectedPairsList );

      inherited updateScenarioData();
    end;
    
(*
//-----------------------------------------------------------------------------
// Data and database functions
//-----------------------------------------------------------------------------
  procedure TFormProdTypePairs.updateDatabase();
    var
      ptp: TProductionTypePair;
      q: string;
    begin

      ptp := selectedPairsList.first();
      while( ptp <> nil ) do
        begin
          if( ptp.added ) and ( not ptp.isInDB ) then
            begin
              dbcout( 'PAIR ADDED: ' + ptp.pairDescr, DBFORMPRODTYPEPAIRS );
              q := 'INSERT INTO inProductionTypePair ( sourceProductionTypeID, destProductionTypeID ) '
                + 'VALUES ( ' + intToStr( ptp.source.productionTypeID ) + ', ' + intToStr( ptp.dest.productionTypeID ) + ' )'
              ;
              _smdb.execute(q);
            end
          ;
          ptp := selectedPairsList.next();
        end
      ;

      ptp := allPairsList.first();
      while( ptp <> nil ) do
        begin
          if( ptp.removed ) then
            begin
              dbcout( 'PAIR REMOVED: ' + ptp.pairDescr, DBFORMPRODTYPEPAIRS );
              q := 'DELETE FROM inProductionTypePair WHERE '
                + 'sourceProductionTypeID = ' +  intToStr( ptp.source.productionTypeID ) + ' '
                + 'AND destProductionTypeID = ' + intToStr( ptp.dest.productionTypeID )
              ;
              _smdb.execute( q );
            end
          ;
          ptp := allPairsList.next();
        end
      ;

      if( selectedPairsList.Count = 0 ) then
        begin
          q := 'UPDATE inGeneral SET includeContactSpread = 0, includeAirborneSpread = 0';
          _smdb.execute( q );
        end
      ;
    end
  ;
*)

  // FIX ME: I'm not crazy about the latter 1/2 of this function (it strikes me
  // as a bit of a hack), but it will do for now.
  function TFormProdTypePairs.dataIsValid(): boolean;
    var
      i : longint;
      MainComb : string;
      MissingComb : string;
      test: integer;
    begin
      MissingComb := '';

      if( selectedPairsList.Count = 0 ) then
        begin
          test := msgYesNo(
            tr( 'There are no selected production type pairs.  Disease spread will be disabled.  Continue?' ),
            tr( 'No disease spread' ),
            IMGQuestion,
            self
          );

          if( test = mrNo ) then
            result := false
          else
            result := true
          ;

          exit;
        end
      ;

      for i := 0 to lbxSource.Items.Count - 1 do
        begin
          MainComb := lbxSource.Items[i] + ' > ' + lbxSource.Items[i];

          if lbxSelectedPairs.Items.IndexOf(MainComb) = -1 then
            MissingComb := MissingComb + endl + MainComb
          ;
        end
      ;

      if( MissingComb <> '' ) then
        begin
          test := msgYesNo(
            tr( 'The following primary combinations may be critical to simulations:' )
              + endl + MissingComb + endl + endl
              + tr( 'Proceed to save without these combinations?' ),
            tr( 'Missing combinations' ),
            IMGQuestion,
            self
          );

          if( test  = mrNo )
          then
            result := false
          else
            result := true
          ;
        end
      else
        Result := true
      ;
    end
  ;


  function TFormProdTypePairs.getDataUpdated(): boolean;
    var
      it: TProductionTypePairListIterator;
      ptp: TProductionTypePair;
    begin
      result := false;

      it := TProductionTypePairListIterator.create( selectedPairsList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          ptp := it.current();

          if
            ( ptp.added )
          or
            ( ptp.removed )
          then
            begin
              result := true;
              break;
            end
          ;
          it.incr();
        end
      ;

      it.Free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFormProdTypePairs.getSelectedProdTypePairIndex(): integer;
    begin
      if( dataHasChanged ) then
        result := -1
      else
        result := _selectedProdTypeIndex
      ;
    end
  ;
//-----------------------------------------------------------------------------

initialization
	RegisterClass( TFormProdTypePairs );


end.
