unit FormCustomOutputs;

(*
FormCustomOutputs.pas/dfm
-------------------------
Begin: 2006/10/14
Last revision: $Date: 2013-06-27 19:11:25 $ $Author: areeves $
Version: $Revision: 1.12.6.2 $
Project: NAADSM
Website: http://www.naadsm.org
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2006 - 2009 Colorado State University

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
    Buttons,
    Menus,
    ActnPopupCtrl,
    
		REEdit,

    CustomOutputDefinitions,

    FormSMWizardBase
	;

  type TSelectedItem = (
    SELUndefined,
    SELFirst,
    SELLast
  );

  type TFormCustomOutputs = class( TFormSMWizardBase )
			pnlCaption: TPanel;
			pnlCustomOutputs: TPanel;
			pnlCustomOutputsCaption: TPanel;
			lbxCustomOutputs: TListBox;

			pnlControls: TPanel;

      pnlDefEditor: TPanel;

      gbxOutputFrequency: TGroupBox;
      rdoIterationOutput: TRadioButton;
      rdoProdTypeOutput: TRadioButton;

      gbxOutputType: TGroupBox;
      rdoInteger: TRadioButton;
      rdoDouble: TRadioButton;
      rdoString: TRadioButton;

      lblSql: TLabel;
      mmoSql: TMemo;

      pnlDefController: TPanel;
      btnNew: TButton;
      btnRemove: TButton;
      btnTest: TButton;
      btnRename: TButton;
      pnlWarning: TPanel;
      lblWarning: TLabel;
      WarningImage: TImage;
      rdoZoneOutput: TRadioButton;
      rdoZonePTOutput: TRadioButton;

      procedure lbxCustomOutputsClick(Sender: TObject);
      procedure rdoBtnClick(Sender: TObject);
      procedure mmoSqlChange(Sender: TObject);
      procedure btnNewClick(Sender: TObject);
      procedure btnRemoveClick(Sender: TObject);
      procedure btnRenameClick(Sender: TObject);

		protected
      _dList: TCustomOutputList;
      _selectedDefinition: TCustomOutputDefinition;
      _nextAddID: integer;
      _loading: boolean;

      procedure translateUI();

      procedure initializeFromSim(); override;
      function dataIsValid(): boolean; override;
      function getDataUpdated(): boolean; override;

      procedure updateListBox( selectedItem: TSelectedItem = SELFirst );
      procedure updateDisplay();
      procedure clearDisplay();
      procedure setControlsEnabled( val: boolean );

		public
      constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

			function showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer; override;

		end
	;


implementation

{$R *.dfm}

  uses
    StrUtils,
    
    MyDialogs,
    DebugWindow,
    MyStrUtils,
    ControlUtils,
    I88n
  ;

//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFormCustomOutputs.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
      
      _dList := nil;
      _nextAddID := -1;

      btnTest.enabled := false; // FIX ME until the test actually works.
    end
  ;


  procedure TFormCustomOutputs.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:56 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormCustomOutputs.dfm
      // File date: Mon Sep 24 12:04:52 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Custom outputs' );
          pnlCaption.Caption := tr( 'Custom output definitions' );
          pnlCustomOutputsCaption.Caption := tr( 'Custom outputs' );
          lbxCustomOutputs.Hint := tr( 'Press CTRL + left mouse button to select multiple definitions' );
          lblSql.Caption := tr( 'SQL statement:' );
          gbxOutputFrequency.Caption := tr( 'Output frequency:' );
          rdoIterationOutput.Caption := tr( 'Generate once per iteration' );
          rdoProdTypeOutput.Caption := tr( 'Generate for every production type for every iteration' );
          rdoZoneOutput.Caption := tr( 'Generate for every zone for every iteration' );
          rdoZonePTOutput.Caption := tr( 'Generate for every zone and every production type for every iteration' );
          gbxOutputType.Caption := tr( 'Output type:' );
          rdoInteger.Caption := tr( 'Number - integer' );
          rdoDouble.Caption := tr( 'Number - floating point' );
          rdoString.Caption := tr( 'String' );
          btnNew.Caption := tr( 'Add definition' );
          btnRemove.Caption := tr( 'Remove selected definition' );
          btnTest.Caption := tr( 'Test current/selected definition' );
          btnRename.Caption := tr( 'Rename current/selected definition' );
          lblWarning.Caption := tr( 'This feature of NAADSM/PC is not well documented and may not be fully implemented.  It is intended for experts and others who don''t mind living dangerously...' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          lbxCustomOutputs.Items[0] := tr( 'unitsVacc5DaysPostDetection' );
          lbxCustomOutputs.Items[1] := tr( 'animalsVacc5DaysPostDetection' );
          lbxCustomOutputs.Items[2] := tr( 'unitsDestr5DaysPostDetection' );
          lbxCustomOutputs.Items[3] := tr( 'unitsVacc5DaysPostDetection' );
        end
      ;

    end
  ;


  destructor TFormCustomOutputs.destroy();
  	begin
   		inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Data handling
//-----------------------------------------------------------------------------
  procedure TFormCustomOutputs.initializeFromSim();
    begin
        _dList := _smScenarioCopy.simInput.customOutputDefinitions;

        updateListBox();
    end
  ;


  function TFormCustomOutputs.dataIsValid(): boolean;
    var
      d: TCustomOutputDefinition;
    begin
      result := true;

      d := _dList.first();
      while( nil <> d ) do
        begin
          if( ( OFUndefined = d.frequency ) or ( VTUndefined = d.variableType ) ) then
            begin
              msgOK(
                tr( 'Please make sure that every definition includes an output frequency and type.' ),
                tr( 'Problem with definition(s)' ),
                IMGWarning,
                self
              );

              result := false;
              exit;
            end
          ;

          if( 0 = length( trim( d.sql ) ) ) then
            begin
              msgOK(
                tr( 'Please make sure that every definition includes an SQL statement.' ),
                tr( 'Problem with definition(s)' ),
                IMGWarning,
                self
              );

              result := false;
              exit;
            end
          ;

          d := _dList.next();
        end
      ;
    end
  ;

  
  function TFormCustomOutputs.getDataUpdated(): boolean;
    begin
      // If anything on this form has been altered, stored output will no longer be valid.
      result := _dList.updated;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
	function TFormCustomOutputs.showModal( const nextFormToShow: integer; var formDisplayed: boolean; const currentFormIndex: integer ): integer;
  	begin
    	if( _smScenarioCopy.simInput.useCustomOutputs ) then
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


  procedure TFormCustomOutputs.updateListBox( selectedItem: TSelectedItem = SELFirst );
    var
      d: TCustomOutputDefinition;
    begin
      lbxCustomOutputs.Clear();

      d := _dList.first();

      while( nil <> d ) do
        begin
          if( not( d.removed ) ) then
            lbxCustomOutputs.Items.AddObject( d.outputName, d );
          ;

          d := _dList.next();
        end
      ;

      if( 0 < _dList.Count ) then
        begin
          if( SELFirst = selectedItem ) then
            lbxCustomOutputs.ItemIndex := 0
          else if( SELLast = selectedItem ) then
            lbxCustomOutputs.ItemIndex := lbxCustomOutputs.Count - 1
          ;
        end
      ;
      updateDisplay();
    end
  ;


  procedure TFormCustomOutputs.setControlsEnabled( val: boolean );
    begin
      setChildrenEnabled( pnlDefEditor, val, true );

      btnRemove.enabled := val;
      btnRename.enabled := val;
      btnTest.enabled := false; // FIX ME until the test actually works.
    end
  ;


  procedure TFormCustomOutputs.clearDisplay();
    begin
      rdoIterationOutput.Checked := false;
      rdoProdTypeOutput.Checked := false;

      rdoString.Checked := false;
      rdoInteger.Checked := false;
      rdoDouble.checked := false;

      mmoSql.Lines.Text := '';

      setControlsEnabled( false );
    end
  ;


  procedure TFormCustomOutputs.updateDisplay();
  	begin
      _loading := true;

      if( ( -1 = lbxCustomOutputs.ItemIndex ) or ( 0 = _dList.Count ) ) then
        clearDisplay()
      else
        begin
          _selectedDefinition := lbxCustomOutputs.items.objects[lbxCustomOutputs.ItemIndex] as TCustomOutputDefinition;

          if(  nil <> _selectedDefinition ) then
            begin
              setControlsEnabled( true );

              case _selectedDefinition.frequency of
                OFUndefined:
                  begin
                    dbcout( 'Invalid output frequency in TFormCustomOutputs.updateDisplay', true );
                    rdoIterationOutput.Checked := false;
                    rdoProdTypeOutput.Checked := false;
                  end
                ;
                OFIteration: rdoIterationOutput.Checked := true;
                OFProductionTypeIteration: rdoProdTypeOutput.Checked := true;
                OFZoneIteration: rdoZoneOutput.Checked := true;
                OFZoneProductionTypeIteration: rdoZonePTOutput.Checked := true;
              end;

              case _selectedDefinition.variableType of
                VTUndefined:
                  begin
                    dbcout( 'Invalid variable type in TFormCustomOutputs.updateDisplay', true );
                    rdoString.Checked := false;
                    rdoInteger.Checked := false;
                    rdoDouble.checked := false;
                  end
                ;
                VTString: rdoString.Checked := true;
                VTInteger: rdoInteger.Checked := true;
                VTDouble: rdoDouble.checked := true;
              end;

              mmoSql.Lines.Text := _selectedDefinition.sql;
            end
          else
            clearDisplay()
          ;
        end
      ;

      _loading := false;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormCustomOutputs.lbxCustomOutputsClick(Sender: TObject);
    begin
      updateDisplay();
    end
  ;


  procedure TFormCustomOutputs.rdoBtnClick(Sender: TObject);
    begin
      if( not( _loading ) ) then
        begin
          if( nil <> _selectedDefinition ) then
            begin
              if( rdoIterationOutput.Checked ) then
                _selectedDefinition.frequency := OFIteration
              else if( rdoProdTypeOutput.Checked ) then
                _selectedDefinition.frequency := OFProductionTypeIteration
              else if( rdoZoneOutput.Checked ) then
                _selectedDefinition.frequency := OFZoneIteration
              else if( rdoZonePTOutput.Checked ) then
                _selectedDefinition.frequency := OFZoneProductionTypeIteration
              else
                _selectedDefinition.frequency := OFUndefined
              ;

              if( rdoInteger.Checked ) then
                _selectedDefinition.variableType :=  VTInteger
              else if( rdoDouble.Checked ) then
                _selectedDefinition.variableType := VTDouble
              else if( rdoString.Checked ) then
                _selectedDefinition.variableType := VTString
              else
                _selectedDefinition.variableType := VTUndefined
              ;

              // The asterisk is a visible indicator that something on the form has changed, and that the database will be updated
              showStar();
            end
          else
            dbcout( '_selectedDefinition is nil!!', true )
          ;
        end
      ;
    end
  ;


  procedure TFormCustomOutputs.mmoSqlChange(Sender: TObject);
    begin
      if( not( _loading ) ) then
        begin
          if( nil <> _selectedDefinition ) then
            begin
              _selectedDefinition.sql := mmoSql.Lines.Text;

              // The asterisk is a visible indicator that something on the form has changed, and that the database will be updated
              showStar();
            end
          else
            dbcout( '_selectedDefinition is nil!!', true )
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------






  procedure TFormCustomOutputs.btnNewClick(Sender: TObject);
    var
      newDesc: string;
      d : TCustomOutputDefinition;
    begin
      newDesc := msgInput(
        tr( 'Name of new output definition:' ),
        '', // regexp
        '', // caption
        IMGQuestion,
        self
      );
      newDesc := trim( newDesc );

      if( newDesc = '' ) then
        exit
      else
        begin
          d := _dlist.findDefinition( newDesc );
          if( nil <> d ) then
            begin
              if( not( d.removed ) ) then
                begin
                  msgOK( 
                    tr( 'A definition with this name is already present.' ),
                    tr( 'Cannot add output definition' ),
                    IMGCritical,
                    self )
                  ;
                  exit;
                end
              ;
            end
          ;
        end
      ;

      showStar();
      d := TCustomOutputDefinition.create( _nextAddID, newDesc );
      _dList.append( d );
      dec( _nextAddID );
      updateListBox( SELLast );
    end
  ;


  procedure TFormCustomOutputs.btnRemoveClick(Sender: TObject);
    var
      answer: integer;
    begin
      if( nil <> _selectedDefinition ) then
        begin
          answer := msgYesNo(
            ansiReplaceStr( tr( 'Are you sure you want to remove the custom output definition ''xyz''?' ), 'xyz', _selectedDefinition.outputName ),
            tr( 'Remove custom output definition' ),
            IMGQuestion,
            self )
          ;

          if( answer = mrYes ) then
            begin
              showStar();
              _selectedDefinition.removed := true;
              updateListBox();
            end
          ;
        end
      ;
    end
  ;


  procedure TFormCustomOutputs.btnRenameClick(Sender: TObject);
    var
      newDefName, oldDefName: string;
    begin
      if( nil <> _selectedDefinition ) then
        begin
          oldDefName := _selectedDefinition.outputName;

          newDefName := msgInput(
            ansiReplaceStr( tr( 'Modify custom output name for xyz.' ), 'xyz', oldDefName ),
            '', // regexp
            '', // caption
            IMGQuestion,
            self,
            oldDefName
          );
          newDefName := trim( newDefName );

          // Make sure name is unique
          if( '' = newDefName ) then
            // do nothing
          else if( nil <> _dList.findDefinition( newDefName ) ) then
            begin
              msgOK( 
                tr( 'Another output definition with this name is already present.' ),
                tr( 'Cannot modify output definition' ),
                IMGCritical,
                self )
              ;
            end
          else // everything is OK.  Modify the name.
            begin
              _selectedDefinition.outputName := newDefName;
              updateListBox();
              showStar();
            end
          ;
        end
      ;
    end
  ;

initialization

  registerClass( TFormCustomOutputs );

end.
