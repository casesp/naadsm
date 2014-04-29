unit FormProdType;

(*
FormProdType.pas/dfm
---------------------
Begin: 2005/04/02
Last revision: $Date: 2013-06-27 19:11:27 $ $Author: areeves $
Version: $Revision: 1.28.6.5 $
Project: NAADSM
Website:
Author: Aaron Reeves <Aaron.Reeves@ucalgary.ca>
--------------------------------------------------
Copyright (C) 2005 - 2011 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


interface

  uses
    SysUtils,
    ExtCtrls,
    StdCtrls,
    Buttons,
    Controls,
    Classes,
    Dialogs,
    Forms,
    Menus,
    ActnPopupCtrl,

    FormSMWizardBase,
    SMScenario
  ;


  {*
    This form handles removal or renaming of existing production types and the
    creation of new production types.

    New production types are assigned a temporary negative ID number. The negative
    PTID number is just a placeholder until the database generates an appropriate ID.

    To prevent referential integrity errors, update the database with removed
    PTs first, then with modified PTs, and finally with newly created PTs.
  }
  type TFormProdType = class( TFormSMWizardBase )
      pnlProdTypeList: TPanel;
      pnlProdTypeListHeader: TPanel;
      pnlCaption: TPanel;
      pnlBody: TPanel;

      lstProdTypes: TListBox;
      BitBtnAdd: TBitBtn;
      BitBtnModify: TBitBtn;
      BitBtnRemove: TBitBtn;


      // Main GUI event handlers
      //------------------------
      procedure BitBtnAddClick(Sender: TObject);
      procedure BitBtnModifyClick(Sender: TObject);
      procedure BitBtnRemoveClick(Sender: TObject);


      // Simple GUI event handlers
      //--------------------------
      procedure lstProdTypesChange(Sender: TObject);
      procedure lstProdTypesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure lstProdTypesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure lstProdTypesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
      procedure lstProdTypesEnter(Sender: TObject);
      procedure lstProdTypesExit(Sender: TObject);
      procedure lstProdTypesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      procedure lstProdTypesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    protected
      _nextAddID: integer; // Used to keep track of the temporary (negative) ID number to assign to new pts
      _warningShown: boolean;
      _removedCount: integer;

      procedure translateUI();

      function extractRealName( val: string ): string;

      // Reimplemented functions
      //-------------------------
      { Properties }
      function getSelectedProdTypeIndex(): integer; override;

      { Load production type names from database and populate the list box }
      procedure initializeFromSim(); override;

      { Data entry validation }
      function dataIsValid(): boolean; override;
      procedure updateScenarioData(); override;
      procedure updateGlobalControlSettings();
      function getDataUpdated(): boolean; override;

      // GUI display updates
      //--------------------
      procedure UpdateButtons();
      procedure updateListBox();


    public
      // Construction/destruction
      //-------------------------
      constructor create( Aowner: TComponent ); override;
      destructor destroy(); override;

    end
  ;

  const
  	DBFORMPRODTYPE: boolean = false; // set to true to display debugging messages for this form.

implementation

  {$R *.dfm}

  uses
    StrUtils,
    
    SMDatabase,
    SqlClasses,
    MyDialogs,
    MyStrUtils,
    DebugWindow,
    I88n,

    ProductionType,
    ProductionTypeList
  ;

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFormProdType.create( Aowner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _nextAddID := -1;
      _warningShown := false;
      _removedCount := 0;
    end
  ;


  procedure TFormProdType.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 12:56:54 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/sm_forms/FormProdType.dfm
      // File date: Thu Jan 4 11:16:18 2007

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Scenario parameters: Production type(s)' );
          pnlProdTypeListHeader.Caption := tr( 'Production types' );
          lstProdTypes.Hint := tr( 'Press CTRL + left mouse button to select multiple production types' );
          pnlCaption.Caption := tr( 'Production type(s)' );
          BitBtnAdd.Caption := tr( 'Add production type' );
          BitBtnModify.Caption := tr( 'Modify selected production type' );
          BitBtnRemove.Caption := tr( 'Remove selected production type(s)' );
        end
      ;

      // Set TStrings properties
      with self do
        begin
          lstProdTypes.Items[0] := tr( 'Cattle - beef' );
          lstProdTypes.Items[1] := tr( 'Cattle - dairy' );
          lstProdTypes.Items[2] := tr( 'Mixed - beef/dairy' );
          lstProdTypes.Items[3] := tr( 'Swine - confinement' );
          lstProdTypes.Items[4] := tr( 'Swine - outside' );
        end
      ;

    end
  ;


  destructor TFormProdType.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Parameter and database functions
//-----------------------------------------------------------------------------
  procedure TFormProdType.initializeFromSim();
    begin
      updateListBox();
      updateButtons();
    end
  ;


  function TFormProdType.dataIsValid(): boolean;
    begin
      // There is nothing to validate on this form: validation of
      // added/modified names occurs immediately upon input.
      // Always return true.
      result := true;
    end
  ;



  function TFormProdType.getDataUpdated(): boolean;
    begin
      // Adding new types or renaming existing types doesn't require that output be deleted.
      // Removing an existing type does.

      result := ( _removedCount > 0 );
    end
  ;


  procedure TFormProdType.updateScenarioData();
    var
      it: TProductionTypeListIterator;
    begin
      // When a production type is removed, several things need to happen:
      // - Herds of that type need to be removed from the database.
      // - Production type pairs (PTPs) using the removed type need to be removed from the database.
      // - Zone/production type combinations using the removed type need to be removed from the database.
      //     This is handled when the production type itself is removed.
      // - Herds need to be removed from the herd list in memory.
      //     Since the only herd list in memory is the original herd list,
      //     (the only form that gets a copy of the herd list is the herd list editor)
      //     herds of the removed type need to be removed from the original herd list in memory.
      // - PTPs need to be removed from the PTP list in memory.
      //     In the case of PTPs, the copy of simInput can be used: when it is copied back to the "original",
      //     everything will be fine.
      // - Zone/production type combinations need to be removed from the zone list in memory.
      //     In this case, like PTPs, the copy of simInput can be used: when it is copied back to the "original",
      //     everything will be fine.
      // - The removed production type itself will be removed from memory during the database update process.
      // - Update the global control options so the user does have to do it manually in the cases where
      //     the production type(s) removed results in global control options on but not used by remaining production types.
      //
      // Creating or modifying production types is pretty simple by comparison, and is all taken care of
      //   during the database update process.

      it := TProductionTypeListIterator.create( _smScenarioCopy.simInput.ptList );

      it.toFirst();
      while( nil <> it.current() ) do
        begin
          if( ( it.current().removed ) and ( 0 < it.current().productionTypeID ) ) then
            begin
              _smScenarioOriginal.herdList.removeProductionType( it.current().productionTypeID );
              _smScenarioCopy.simInput.ptpList.removeProductionType( it.current().productionTypeID );
            end
          ;

          it.incr();
        end
      ;

      it.free();

      // rbh20110928 Now update the global control options for cases where a global control
      // is specified, but now no longer used by any production type, Issue 2308.
      updateGlobalControlSettings();

      inherited updateScenarioData();
    end
  ;


  // Helper method to updateScenarioData() to address issue 2308
  procedure TFormProdType.updateGlobalControlSettings();
    var
      it: TProductionTypeListIterator;
      dm: TProductionType;
      
      detectionIsUsed: boolean;
      tracingIsUsed: boolean;
      tracingExamIsUsed: boolean;
      testingIsUsed: boolean;
      zoneIsTriggered: boolean;
      destrIsUsed: boolean;
      vaccIsUsed: boolean;

    begin
      detectionIsUsed := false;
      tracingIsUsed := false;
      tracingExamIsUsed := false;
      testingIsUsed := false;
      zoneIsTriggered := false;
      destrIsUsed := false;
      vaccIsUsed := false;

      it := TProductionTypeListIterator.create( _smScenarioCopy.simInput.ptList );
      it.toFirst();
      while( nil <> it.current() ) do
        begin
          dm := it.current();
          if( (not dm.removed ) and ( 0 < dm.productionTypeID ) ) then
            begin
              if( dm.useDetection ) then detectionIsUsed := true;
              if( dm.useTracing ) then tracingIsUsed := true;
              if( dm.useTracingExam ) then tracingExamIsUsed := true;
              if( dm.useTesting ) then testingIsUsed := true;
              if( dm.isZoneTrigger ) then zoneIsTriggered := true;
              if( dm.isDestrTarget ) then destrIsUsed := true;  // Destruction for any reason
              if( dm.isVaccTarget ) then vaccIsUsed := true;
            end
          ;
          it.incr();
        end
      ;
      it.free();

      if( not detectionIsUsed ) then
          _smScenarioCopy.simInput.controlParams.useDetectionGlobal := false
      ;

      if( not tracingExamIsUsed ) then
          _smScenarioCopy.simInput.controlParams.useTracingHerdExamGlobal := false
      ;

      if( not testingIsUsed ) then
          _smScenarioCopy.simInput.controlParams.useTracingTestingGlobal := false
      ;

      if( not tracingIsUsed ) then
          _smScenarioCopy.simInput.controlParams.useTracingGlobal := false
      ;

       if( not zoneIsTriggered ) then
          _smScenarioCopy.simInput.controlParams.useZonesGlobal := false
      ;

      // User may still get validity comments if removing a production type causes the
      // destruction priorities or ring destruction targets-triggers to be wonky.
      if( not destrIsUsed ) then
          _smScenarioCopy.simInput.controlParams.useDestructionGlobal := false
      ;

      if( not vaccIsUsed ) then
          _smScenarioCopy.simInput.controlParams.useVaccGlobal := false
      ;

    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Main GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormProdType.BitBtnAddClick(Sender: TObject);
    var
      NewProdTypeDesc: string;
      answer: integer;
      ptList: TProductionTypeList;
      pt : TProductionType;
    begin
      ptList := _smScenarioCopy.simInput.ptList;

      newProdTypeDesc := msgInput(
        tr( 'Please enter a name for the new production type.  NOTE: Do not enter an ID number.  It will be handled automatically.' ),
        '', // regexp
        '', // caption
        IMGQuestion,
        self
      );
      newProdTypeDesc := trim( newProdTypeDesc );

      if( '' = NewProdTypeDesc ) then
        exit
      else
        begin
          pt := ptList.findProdType( newProdTypeDesc );
          if( nil <> pt ) then
            begin
              if( not( pt.removed ) ) then
                begin
                  msgOK( 
                    tr( 'A production type with this name is already present.' ),
                    tr( 'Cannot add production type' ),
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

      // If we get this far...
      if( not _warningShown ) then
        answer := msgYesNo(
          tr( 'To add a new production type it will be necessary to delete all current output records, if present.  Continue?' ),
          tr( 'Add production type' ),
          IMGQuestion,
          self )
      else
        answer := mrYes
      ;

      if( answer = mrYes) then
        begin
          showStar();
          _warningShown := true;
          
          pt := TProductionType.create( _nextAddID, newProdTypeDesc, false, ptList.sim );

          // Newly created production types should not have destruction and vaccination priorities set.
          // Make the user set them!
          pt.destructionParams.destrPriority := -1;
          pt.ringVaccParams.vaccPriority := -1;
          //pt.destructionParams.destrPriority := ptList.maxDestrPriority + 1;
          //pt.ringVaccParams.vaccPriority := ptList.maxVaccPriority + 1;

          ptList.append( pt );

          _smScenarioCopy.simInput.controlParams.recalculatePriorities();
          dec( _nextAddID );
          updateListBox();
        end
      ;
    end
  ;


  procedure TFormProdType.BitBtnModifyClick(Sender: TObject);
    var
      newPTName, oldPTName: string;
      ptList: TProductionTypeList;
      pt : TProductionType;
    begin
      ptList := _smScenarioCopy.simInput.ptList;

      oldPTName := extractRealName( lstProdTypes.Items.Strings[lstProdTypes.ItemIndex] );

      newPTName := msgInput(
        ansiReplaceStr( tr( 'Modify production type description for xyz.' ), 'xyz', oldPTName )
          + tr( 'NOTE: Do not enter an ID number.  It will be handled automatically.' ),
        '', // regexp
        '', // caption
        IMGQuestion,
        self,
        oldPTName
      );

      newPTName := trim( newPTName );

      // If the new name is the same as the old name, don't mess with anything
      if( oldPTName = newPTName ) then
        exit
      ;

      // Make sure newPT is unique
      if( '' = newPTName ) then
        // do nothing
      else if( nil <> ptList.findProdType( newPTName ) ) then
        begin
          msgOK( 
            tr( 'A production type with this name is already present.' ),
            tr( 'Cannot modify production type' ),
            IMGCritical,
            self )
          ;
        end
      else // everything is OK.  Modify the production type name.
      	begin
          pt := lstProdTypes.Items.Objects[lstProdTypes.ItemIndex] as TProductionType;
          pt.productionTypeDescr := newPTName;
          updateListBox();
          showStar();
        end
      ;
    end
  ;

    // The "fake" name will look liked "Cattle (#27)".  The "real" name is "cattle".
  function TFormProdType.extractRealName( val: string ): string;
    var
      len: integer;
      i: integer;
    begin
      len := length( val );
      for i := length( val ) downto 1 do
        begin
          dec( len );
          if( '(' = charAt( val, i ) ) then
            break
          ;
        end
      ;

      result := trim( leftStr( val, len ) );
    end
  ;

  procedure TFormProdType.BitBtnRemoveClick(Sender: TObject);
    var
      messageText: string;
      answer: integer;
      i: integer;

      pt : TProductionType;
    begin
      if lstProdTypes.selCount = 1 then
        messageText := lstProdTypes.Items[lstProdTypes.ItemIndex]
      else
        messageText := tr( 'these selected production types' )
      ;

      if( not _warningShown ) then
        answer := msgYesNo(
          tr( 'By continuing, units of this production type(s) and all output records, if present, will be deleted.' )
            + '  ' + ansiReplaceStr( tr( 'Are you sure you wish to remove xyz?' ), 'xyz', MessageText ),
          tr( 'Remove production type(s)' ),
          IMGQuestion,
          self )
      else
      	answer := mrYes
      ;

      if( answer = mrYes ) then
        begin
          showStar();
        	_warningShown := true;

          for i := 0 to (lstProdTypes.Count-1) do
            begin
              if( lstProdTypes.Selected[i] ) then
                begin
                  pt := lstProdTypes.Items.Objects[i] as TProductionType;
                  pt.removed := true;
                end
              ;
            end
          ;

        end
      ;

      updateListBox();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Display functions
//-----------------------------------------------------------------------------
  procedure TFormProdType.updateButtons();
    begin
      BitBtnModify.Enabled := (lstProdTypes.SelCount = 1);
      BitBtnRemove.Enabled := (lstProdTypes.SelCount > 0);
    end
  ;


  procedure TFormProdType.updateListBox();
    var
      result: string;
      id: integer;
      it: TProductionTypeListIterator;
  	begin
      lstProdTypes.Clear();
      result := '';

      it := TProductionTypeListIterator.create( _smScenarioCopy.simInput.ptList );
      it.toFirst();

      while( nil <> it.current() ) do
        begin
          if( not( it.current().removed ) ) then
            begin
              result := it.current().productionTypeDescr;
              id := it.current().productionTypeID;

              if( 0 < id ) then
                result := result + ' (#' + intToStr( id ) + ')'
              else
                result := result + ' (New)'
              ;

              lstProdTypes.Items.AddObject( result, it.current() );
            end
          ;

          it.incr();
        end
      ;

      it.free();

      lstProdTypes.ClearSelection();
      updateButtons();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Simple GUI event handlers
//-----------------------------------------------------------------------------
  procedure TFormProdType.lstProdTypesChange(Sender: TObject); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesEnter(Sender: TObject); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesExit(Sender: TObject); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin updateButtons(); end;
  procedure TFormProdType.lstProdTypesKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); begin updateButtons(); end;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties reimplemented from TFormSMWizardBase
//-----------------------------------------------------------------------------
  function TFormProdType.getSelectedProdTypeIndex(): integer;
    begin
      if( 0 = _removedCount ) then
        result := _selectedProdTypeIndex
      else
        result := -1
      ;
    end
  ;
//-----------------------------------------------------------------------------



initialization

	RegisterClass( TFormProdType );

end.

