unit FrameSMFunctionEditor;

(*
FrameSMFunctionEditor.pas/dfm
-----------------------------
Begin: 2006/01/04
Last revision: $Date: 2013-06-27 19:11:37 $ $Author: areeves $
Version number: $Revision: 1.29.4.1 $
Project: NAADSM and related applications
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

    ChartFunction,

    FrameFunctionEditor
  ;

  {*
    This class implements an NAADSM-specific version of FrameFunctionEditor.  The primary
    responsibility of this class is to interact with several specialized GUI components in the application.

    See FrameFunctionEditor for more information.
  }
  type TFrameSMFunctionEditor = class( TFrameFunctionEditor )
  	protected
      { Overridden from the base class. }
      function multipleEffectsOK( const idx: integer ): boolean; override;
      function removalEffectsOK( const idx: integer ): boolean; override;
      procedure updateAppDisplayForChartChange( fn: TChartFunction ); override;
      procedure updateAppDisplay(); override;
      
    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;
    end
  ;

implementation

{$R *.dfm}

  uses
    MyDialogs,
    MyStrUtils,
    DebugWindow,
    I88n,

    FunctionDictionary,
    
    FormSMWizardBase
  ;


  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages for this unit


//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
  constructor TFrameSMFunctionEditor.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
    end
  ;


  destructor TFrameSMFunctionEditor.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Initial setup (property-like functions)
//-----------------------------------------------------------------------------
  procedure TFrameSMFunctionEditor.updateAppDisplayForChartChange( fn: TChartFunction );
    begin
      // Update the form to show that a change was made
      if ( _chartFn <> _functionDict.value(fn.name).fn ) then
        (_myForm as TFormSMWizardBase).showStar()
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Functions reimplemented from the base class
//-----------------------------------------------------------------------------
  function TFrameSMFunctionEditor.multipleEffectsOK( const idx: integer ): boolean;
    var
    	listItem: TFunctionDictionaryItem;
      response: integer;
      tmpBool: boolean;
    begin
      result := true;

    	listItem := _functionDict.value( cboChartList.items.strings[idx] );

      // Make sure that the user knows that there will be multiple effects
      //------------------------------------------------------------------
      if( ( 1 < listItem.refCounter ) and (_myForm as TFormSMWizardBase).showMultiUseWarning ) then
      	begin
          response := msgYesNoCheckbox(
            tr( 'This function is used in several places.  Altering it will affect all of them.  Continue?' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'Multiple function instances' ),
            IMGQuestion,
            _myForm
          );

          (_myForm as TFormSMWizardBase).showMultiUseWarning := not( tmpBool );

          if( response = mrNo ) then result := false;
        end
      ;
    end
  ;


  procedure TFrameSMFunctionEditor.updateAppDisplay();
    begin
      (_myForm as TFormSMWizardBase).updateMasterDisplay();
      (_myForm as TFormSMWizardBase).showStar();
    end
  ;

  
  function TFrameSMFunctionEditor.removalEffectsOK( const idx: integer ): boolean;
    var
    	listItem: TFunctionDictionaryItem;
      response: integer;
      tmpBool: boolean;
  	begin
      result := true;

      listItem := _functionDict.value( cboChartList.items.strings[idx] );

      // Make sure that the user knows that the function will be blown away.
      //--------------------------------------------------------------------
      if( (_myForm as TFormSMWizardBase).showFunctionRemovalWarning ) then
        begin
          response := msgYesNoCheckbox(
            tr( 'Removing this function will permanently delete it from the scenario file.' )
            + '  ' + tr( 'If you want to keep this function in the scenario file but not apply it here, select ''Clear'' instead.' )
              + endl + endl
              + tr( 'Continue?' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'Function will be permanently removed' ),
            IMGQuestion,
            _myForm
          );

         (_myForm as TFormSMWizardBase).showFunctionRemovalWarning := not( tmpBool );

         if( response = mrNo ) then result := false;
        end
      ;

      // Make sure that the user knows that there will be multiple effects
      //------------------------------------------------------------------
      if( ( listItem.refCounter > 1 ) and (_myForm as TFormSMWizardBase).showMultiUseWarning ) then
      	begin
          response := msgYesNoCheckbox(
            tr( 'This function is used in several places.  Removing it will affect all of them.  Continue?' ),
            tr( 'Do not show this message again' ),
            tmpBool,
            tr( 'Multiple function instances' ),
            IMGQuestion,
            _myForm
          );

          (_myForm as TFormSMWizardBase).showMultiUseWarning := not( tmpBool );

          if( response = mrNo ) then result := false;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------


end.
 