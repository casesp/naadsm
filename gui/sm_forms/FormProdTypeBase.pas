unit FormProdTypeBase;

(*
FormProdTypeBase.pas/dfm
------------------------
Begin: 2005/06/17
Last revision: $Date: 2010-06-15 01:08:07 $ $Author: areeves $
Version number: $Revision: 1.35.12.1 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard includes
    Forms,
    Controls,
    StdCtrls,
    Classes,
    ExtCtrls,
    Buttons,
    Menus,
    ActnPopupCtrl,

    // Widgets
    FormSMWizardBase,

    // Data structures
    ProductionType,
    ProductionTypeList,
    FunctionDictionary
  ;

  type TFormProdTypeBase = class( TFormSMWizardBase )
      pnlProdTypes: TPanel;
      pnlProdTypeCaption: TPanel;
      lbxProdTypes: TListBox;

      pnlCaption: TPanel;
      pnlBody: TPanel;
      pnlHeader: TPanel;
      lblProdType: TLabel;

      btnApplyToAll: TButton;
      btnCopy: TButton;

      // All derived classes have these events
      procedure lbxProdTypesClick(Sender: TObject);
      procedure btnApplyToAllClick(Sender: TObject);
      procedure btnCopyClick(Sender: TObject);

    protected
      _ptList: TProductionTypeList;
      _fnDict: TFunctionDictionary;
      _selectedPT: TProductionType;

      procedure translateUI();

      // The following functions override TFormSMWizardBase, but apply to all
      // derived classes. These is no need to override them further.
      procedure initializeFromSim(); override;

      // These functions need to be reimplemented in every derived class.
      procedure updateDisplay(); virtual; abstract;
  		procedure prepFunctionDicts(); virtual; abstract;
      procedure giveListsToEditors(); virtual; abstract;

      // This function may optionally be overridden in derived classes
    	function dataIsValid(): boolean; override;

      // This function is overridden from TFormSMWizardBase
      function getSelectedProdTypeIndex(): integer; override;
      function getDataUpdated(): boolean; override;

      procedure copyParameters( const src: TProductionType; dest: TProductionType ); virtual; abstract;

      procedure showApplyToAllMessage(); virtual;

    public
      // Override the constructor in all derived classes.
      // The destructor is optional.
    	constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

      // The following functions override TFormSMWizardBase, but apply to all
      // derived classes. These is no need to override it further.      
      procedure updateMasterDisplay(); override;
      function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

    end
  ;

  const
  	DBFORMPRODUCTIONTYPEBASE: boolean = false; // set to true to enable debugging messages for this unit

implementation

{$R *.dfm}

  uses
    SysUtils,
    FormMain,
    MyStrUtils,
    MyDialogs,
    DebugWindow,
    ChartFunction,
    ControlUtils,
    I88n,

    FormSelectProdTypes,

    FunctionEnums,
    SMSimulationInput
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormProdTypeBase.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _ptList := nil;
      _fnDict := nil;

      btnApplyToAll.Enabled := false;
    end
  ;


  procedure TFormProdTypeBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormProdTypeBase.dfm
      // File date: Fri Jan 19 17:43:54 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters' );
          btnApplyToAll.Caption := tr( 'Apply to all' );
          btnCopy.Caption := tr( 'C&opy...' );
          pnlProdTypeCaption.Caption := tr( 'Production types' );
          pnlCaption.Caption := tr( 'CHANGE THIS IN DERIVED CLASSES' );
          lblProdType.Caption := tr( 'lblProdType' );
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

    end
  ;


  destructor TFormProdTypeBase.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Data handling
//-----------------------------------------------------------------------------
  procedure TFormProdTypeBase.initializeFromSim();
    var
      sim: TSMSimulationInput;
      it: TProductionTypeListIterator;
    begin
       sim := _smScenarioCopy.simInput;
      _fnDict := sim.functionDictionary;

      _ptList := _smScenarioCopy.simInput.ptList;

      lbxProdTypes.clear();

      it := TProductionTypeListIterator.create( _ptList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          lbxProdTypes.AddItem( it.current().productionTypeDescr + ' (#' + intToStr( it.current().productionTypeID ) + ')', it.current() );
          it.incr();
        end
      ;

      it.Free();

      updateMasterDisplay();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data validation
//-----------------------------------------------------------------------------
  // As a general rule, users are allowed to store an incomplete scenario.
  // Consequently, most wizard froms don't need to do any real validation.
  // Somewhere, the interface needs to have another check on this, though.
  function TFormProdTypeBase.dataIsValid(): boolean;
  	begin
    	result := true;
    end
  ;


  function TFormProdTypeBase.getDataUpdated(): boolean;
    var
      it: TProductionTypeListIterator;
    begin
      result := false;

      it := TProductionTypeListIterator.create( _ptList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
       		if( it.current().updated ) then
            begin
              result := true;
              break;
            end
          ;

          it.incr();
        end
      ;

      if( ( not result ) and _fnDict.updated ) then
        result := true
      ;

      it.free();
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormProdTypeBase.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
    var
      frm: TForm;
  	begin
      frm := self as TForm;
      initializeFromSim();
      _currentFormIndex := currentFormIndex;

      formDisplayed := true;

      dbcout( 'Selected prod type index: ' + intToStr( _selectedProdTypeIndex ), DBFORMPRODUCTIONTYPEBASE );

      if( lbxProdTypes.Count > _selectedProdTypeIndex ) then
        begin
          if( -1 = _selectedProdTypeIndex ) then _selectedProdTypeIndex := 0;
          dbcout( 'Should be setting selected prod type', DBFORMPRODUCTIONTYPEBASE );
          lbxProdTypes.ItemIndex := _selectedProdTypeIndex;
          if( lbxProdTypes.itemIndex > -1 ) then lbxProdTypesClick( nil );
        end
      ;
    	result := frm.showModal();
    end
  ;


  procedure TFormProdTypeBase.updateMasterDisplay();
  	begin
      prepFunctionDicts();
      giveListsToEditors();
      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handling functions
//-----------------------------------------------------------------------------
  procedure TFormProdTypeBase.lbxProdTypesClick(Sender: TObject);
    begin
      dbcout( 'I''ve been clicked!', DBFORMPRODUCTIONTYPEBASE );
      _selectedPT := lbxProdTypes.items.objects[lbxProdTypes.ItemIndex] as TProductionType;
      btnApplyToAll.Enabled := ( ( nil <> _selectedPT ) and ( 1 < _ptList.Count ) );
      updateDisplay();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Apply to all
//-----------------------------------------------------------------------------
  procedure TFormProdTypeBase.showApplyToAllMessage();
    begin
      // Do nothing in the base class.
      // This function is reimplemented in a few derived classes.
    end
  ;


  procedure TFormProdTypeBase.btnApplyToAllClick(Sender: TObject);
    var
      response: integer;
      tmpBool: boolean;

      it: TProductionTypeListIterator;
      dest: TProductionType;
    begin
      if( showApplyToAllWarning ) then
        begin
          response := msgYesNoCheckbox(
            tr( 'Parameters for the selected production type will be applied to all other production types.  Continue?' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'All production types will be changed' ),
            IMGQuestion,
            self
          );

          showApplyToAllWarning := not( tmpBool );
        end
      else
        response := mrYes
      ;

      showApplyToAllMessage();

      if( mrYes = response ) then
        begin
          it := TProductionTypeListIterator.create( _ptList );

          while( nil <> it.current() ) do
            begin
              dest := it.current();

              if( _selectedPT <> dest ) then
                copyParameters( _selectedPT, dest )
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


  procedure TFormProdTypeBase.btnCopyClick(Sender: TObject);
    var
      frm: TFormSelectProdTypes;
      i: integer;
    begin
      frm := TFormSelectProdTypes.create( self, _ptList, _selectedPT );

      if( mrOK = frm.ShowModal() ) then
        begin
          for i := 0 to (frm.selectedTypes.count - 1) do
            copyParameters( _selectedPT, (frm.selectedTypes.at(i) as TProductionType) )
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
  function TFormProdTypeBase.getSelectedProdTypeIndex(): integer;
    begin
      dbcout( 'Getting selected index', DBFORMPRODUCTIONTYPEBASE );
      result := lbxProdTypes.ItemIndex;
    end
  ;
//-----------------------------------------------------------------------------


initialization
	RegisterClass( TFormProdTypeBase );


end.
