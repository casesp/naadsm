unit FormProdTypePairBase;

(*
FormProdTypePairBase.pas/dfm
----------------------------
Begin: 2005/06/10
Last revision: $Date: 2013-06-27 19:11:28 $ $Author: areeves $
Version: $Revision: 1.37.6.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <aaron.reeves@naadsm.org>
--------------------------------------------------
Copyright (C) 2005 - 2009 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
  	// Standard includes
    Controls,
    StdCtrls,
    ExtCtrls,
    Classes,
    Forms,
    Buttons,
    Menus,
    ActnPopupCtrl,

    // Base class
    FormSMWizardBase,

    // Data structures
    ProductionType,
    ProductionTypeList,
    ProductionTypePair,
    ProductionTypePairList,
    FunctionDictionary
  ;

  type TFormProdTypePairBase = class( TFormSMWizardBase )
    	// All forms for prod type pairs have these controls
      pnlBody: TPanel;
      pnlHeader: TPanel;
      lblProdTypePair: TLabel;
      btnApplyToAll: TButton;
      pnlCaption: TPanel;
      pnlSelectedPairs: TPanel;
    	pnlSelectedPairsCaption: TPanel;
      lbxSelectedPairs: TListBox;
      btnCopy: TButton;

      // All forms for prod type pairs respond to these events
      procedure lbxSelectedPairsClick(Sender: TObject);
      procedure btnApplyToAllClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);

    protected
      // Not all derived classes will necessarily use
      // _fnList, but if it's nil, there is no problem.
      _fnList: TFunctionDictionary;

    	// These are for the prod type pair list
      _ptList: TProductionTypeList;
      _ptpList: TProductionTypePairList;
      _selectedPTP: TProductionTypePair;

      _loadingForm: boolean;

      procedure translateUI();

      // The following functions override TFormSMWizardBase, but apply to all
      // derived classes. These is no need to override them further.
      procedure initializeFromSim(); override;

      // These functions need to be reimplemented in every derived class.
      procedure updateDisplay(); virtual; abstract;
  		procedure prepFunctionDicts(); virtual; abstract;
      procedure giveListsToEditors(); virtual; abstract;

      procedure copyParameters( const src: TProductionTypePair; dest: TProductionTypePair ); virtual; abstract;

      // This function may optionally be overridden in derived classes
    	function dataIsValid(): boolean; override;

      // This function is overridden from TFormSMWizardBase
      function getSelectedProdTypePairIndex(): integer; override;
      function getDataUpdated(): boolean; override;

    public
      // Override the constructor in all derived classes.
      // The destructor is optional.
    	constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      // The following functions override TFormSMWizardBase, but apply to all
      // derived classes. These is no need to override it further.
      procedure updateMasterDisplay(); override;
      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;
      property LoadingForm: Boolean read _loadingForm;

    end
   ;


   const
   	DBFORMPRODTYPEPAIRBASE: boolean = false; // set to true to enable debugging messages for this unit.

implementation

{$R *.dfm}

	uses
    SysUtils,
    
		// Utility functions
    MyDialogs,
    MyStrUtils,
    DebugWindow,
    I88n,

    // Data structures
    ChartFunction,
    FunctionEnums,
    Models,

    FormSelectProdTypePairs
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
 	constructor TFormProdTypePairBase.create( AOwner: TComponent );
  	begin
      inherited create( AOwner );
      translateUI();
      
      _ptpList := nil;
      _fnList := nil;

      btnApplyToAll.Enabled := false;
    end
  ;


  procedure TFormProdTypePairBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormProdTypePairBase.dfm
      // File date: Mon Jan 29 15:43:18 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters' );
          btnApplyToAll.Caption := tr( 'Apply to all' );
          btnCopy.Caption := tr( 'C&opy...' );
          pnlCaption.Caption := tr( 'CHANGE THIS IN DERIVED CLASSES' );
          pnlSelectedPairsCaption.Caption := tr( 'Production type combinations' );
          lblProdTypePair.Caption := tr( 'lblProdTypePair' );
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


  destructor TFormProdTypePairBase.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


  procedure TFormProdTypePairBase.initializeFromSim();
    var
      it: TProductionTypePairListIterator;
    begin
      _loadingForm := true;

      _fnList := _smScenarioCopy.simInput.functionDictionary;
     	_ptList := _smScenarioCopy.simInput.ptList;
    	_ptpList := _smScenarioCopy.simInput.ptpList;

      lbxSelectedPairs.clear();

      it := TProductionTypePairListIterator.create( _ptpList );
      it.toFirst();

      while( nil <> it.current() ) do
      	begin
        	lbxSelectedPairs.AddItem( it.current().pairDescr, it.current() );
          it.incr();
        end
      ;

      it.Free();

      updateMasterDisplay();

      _loadingForm := false;
    end
  ;



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  // As a general rule, users are allowed to store an incomplete scenario.
  // Consequently, most wizard froms don't need to do any real validation.
  // Somewhere, the interface needs to have another check on this, though.
  function TFormProdTypePairBase.dataIsValid(): boolean;
  	begin
    	result := true;
    end
  ;


  function TFormProdTypePairBase.getDataUpdated(): boolean;
    var
      i: integer;
    begin
      result := false;
      
      for i := 0 to _ptpList.Count-1 do
      	begin
       		if( _ptpList.at(i).updated ) then
            begin
              result := true;
              exit;
            end
          ;
        end
      ;

      if ( _fnList.updated ) then
        result := true;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormProdTypePairBase.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
    frm: TForm;
  	begin
      frm := self as TForm;
      initializeFromSim();
      _currentFormIndex := currentFormIndex;

      formDisplayed := true;

      if( lbxSelectedPairs.Count > _selectedProdTypePairIndex ) then
        begin
          if( -1 = _selectedProdTypePairIndex ) then _selectedProdTypePairIndex := 0;
          dbcout( 'Should be setting prod type pair to %d', [ _selectedProdTypePairIndex ], DBFORMPRODTYPEPAIRBASE );
          lbxSelectedPairs.ItemIndex := _selectedProdTypePairIndex;
          if( lbxSelectedPairs.itemIndex > -1 ) then lbxSelectedPairsClick( nil );
        end
      ;
    	result := frm.showModal();
    end
  ;


  procedure TFormProdTypePairBase.updateMasterDisplay();
  	begin
      prepFunctionDicts();
      giveListsToEditors();
      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormProdTypePairBase.lbxSelectedPairsClick(Sender: TObject);
    begin
      inherited;
    	_selectedPTP := lbxSelectedPairs.items.objects[lbxSelectedPairs.itemIndex]  as TProductionTypePair;

      lblProdTypePair.Caption := ( _selectedPTP.pairDescr );

      btnApplyToAll.Enabled := ( ( nil <> _selectedPTP ) and ( 1 < _ptpList.Count ) );

      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Apply to all
//-----------------------------------------------------------------------------
  procedure TFormProdTypePairBase.btnApplyToAllClick(Sender: TObject);
    var
      response: integer;
      tmpBool: boolean;

      it: TProductionTypePairListIterator;
      dest: TProductionTypePair;
    begin
      if( showApplyToAllWarning ) then
        begin
          response := msgYesNoCheckbox(
            tr( 'Parameters for the selected production type combination will be applied to all other combinations.  Continue?' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'All combinations will be changed' ),
            IMGQuestion,
            self
          );

          showApplyToAllWarning := not( tmpBool );
        end
      else
        response := mrYes
      ;

      if( mrYes = response ) then
        begin
          it := TProductionTypePairListIterator.create( _ptpList );

          while( nil <> it.current() ) do
            begin
              dest := it.current();

              if( _selectedPTP <> dest ) then
                copyParameters( _selectedPTP, dest )
              ;
              showStar();
              
              it.incr();
            end
          ;

          it.Free();
        end
      ;
    end
  ;


  procedure TFormProdTypePairBase.btnCopyClick(Sender: TObject);
    var
      frm: TFormSelectProdTypePairs;
      i: integer;
    begin
      frm := TFormSelectProdTypePairs.create( self, _ptpList, _selectedPTP );

      if( mrOK = frm.ShowModal() ) then
        begin
          for i := 0 to (frm.selectedPairs.count - 1) do
            copyParameters( _selectedPTP, ( frm.selectedPairs.at(i)  as TProductionTypePair ) )
          ;
        end
      ;

      frm.Release();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  function TFormProdTypePairBase.getSelectedProdTypePairIndex(): integer;
    begin
      result := lbxSelectedPairs.ItemIndex;
    end
  ;
//-----------------------------------------------------------------------------


initialization
	RegisterClass( TFormProdTypePairBase );

  
end.
