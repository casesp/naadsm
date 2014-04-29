// Begin 3/5/08

unit FormMain;

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

    Processes,

    SMSimulationInput,
    Herd,

    FrameFileSelector,
    FormExport
  ;


  type TFormMain = class(TForm)
      fraScenario1: TFrameFileSelector;
      fraScenario2: TFrameFileSelector;
      btnCompareScenario: TButton;
      btnComparePopulation: TButton;
      btnCompareAll: TButton;

      procedure btnCompareClick(Sender: TObject);

    protected
      _fn: TLineOutputFunc;

      procedure setFormEnabled( const val: boolean );

      procedure showProcessOutput( line: string );

      function diffResult( const file1, file2: string ): string;

      function exportScenario( smSim: TSMSimulationInput; scenarioFileName: string ): TExportResult;
      function exportHerdList( herdList: THerdList; smSim: TSMSimulationInput; herdFileName: string ): TExportResult;
    public
      constructor create( AOwner: TComponent ); override;

      procedure updateCaption();

    end
  ;


  var
    frmMain: TFormMain;

implementation

{$R *.dfm}

  uses
    StrUtils,
    
    I88n,
    DebugWindow,
    WindowsUtils,
    MyStrUtils,
    SqlClasses,
    MyDialogs,
    ControlUtils,

    DialogLongMessage,

    SMDatabase
  ;

  const
    SHORTERMASTERCAPTION = 'NAADSMCompare';

  procedure foo( line: string );
    begin
      dbcout2( line );
    end
  ;


  
  constructor TFormMain.create( AOwner: TComponent );
    begin
      inherited create( AOwner );

      fraScenario1.filter := tr( 'NAADSM scenario databases (*.mdb)|*.mdb|All files (*.*)|*.*' );
      fraScenario2.filter := tr( 'NAADSM scenario databases (*.mdb)|*.mdb|All files (*.*)|*.*' );

      _fn := foo;
    end
  ;



  procedure TFormMain.showProcessOutput( line: string );
    begin
      dbcout2( line );
    end
  ;



  procedure TFormMain.updateCaption();
    begin
      // Do nothing
    end
  ;



  function TFormMain.diffResult( const file1, file2: string ): string;
    var
      cmd: string;
    begin
      cmd := 'diff.exe ''' + file1 + ''' ''' + file2 + '''';

      diffResult := runAndGetOutput(
        cmd,           // Cmd: string;
        currentDir(),  // WorkDir: string;
        nil,           // ErrFunc: TErrFunc;
        nil (*_fn*),   // LineOutputFunc: TLineOutputFunc;
        nil,           // CheckAbortFunc: TCheckAbortFunc;
        false          // ShowReturnValue: Boolean
      );

      // Deal with end-of-line characters
      result := ansiReplaceStr( result, #13#10, #10 );
      result := ansiReplaceStr( result, #10, #13#10 );
    end
  ;



  procedure TFormMain.btnCompareClick(Sender: TObject);
    var
      smdb1: TSMDatabase;
      sim1: TSMSimulationInput;
      hList1: THerdList;

      smdb2: TSMDatabase;
      sim2: TSMSimulationInput;
      hList2: THerdList;

      xmlFilePath1: string;
      xmlFilePath2: string;
      exportResult1: TExportResult;
      exportResult2: TExportResult;

      val: string;
    begin
      dbClear();
      setFormEnabled( false );

      smdb1 := nil;
      sim1 := nil;
      hList1 := nil;

      smdb2 := nil;
      sim2 := nil;
      hList2 := nil;

      // Create the objects.
      //--------------------
      smdb1 := TSMDatabase.create( fraScenario1.fileName, DBOpen, nil, nil );
      sim1 := TSMSimulationInput.create( smdb1 );

      smdb2 := TSMDatabase.create( fraScenario2.fileName, DBOpen, nil, nil );
      sim2 := TSMSimulationInput.create( smdb2 );

      if( ( btnComparePopulation = sender ) or ( btnCompareAll = sender ) ) then
        begin
          hList1 := THerdList.create( smdb1, sim1 );
          hList2 := THerdList.create( smdb2, sim2 );
        end
      ;


      // Get some temporary file names for XML output
      //---------------------------------------------
      xmlFilePath1 := tempFileName( currentDir() );// 'C:\Documents and Settings\apreeves\Desktop\SampleScenario\194.xml';
      xmlFilePath2 := tempFileName( currentDir() );// 'C:\Documents and Settings\apreeves\Desktop\SampleScenario\195.xml';


      // Compare scenarios, if requested
      //--------------------------------
      if( ( btnCompareScenario = sender ) or ( btnCompareAll = sender ) ) then
        begin
          exportResult1 := exportScenario( sim1, xmlFilePath1 );
          exportResult2 := exportScenario( sim2, xmlFilePath2 );

          if( ( ExpSuccess = exportResult1 ) and ( ExpSuccess = exportResult2 ) ) then
            begin
              val := diffResult( xmlFilePath1, xmlFilePath2 );
              if( 0 = length( val ) ) then
                begin
                  dbcout2( 'Scenarios are the same.' );
                  msgOK(
                    'Scenarios are the same.',
                    'NAADSMDiff',
                    IMGSuccess,
                    self
                  );
                end
              else
                begin
                  dbcout2( 'Scenarios are different:' );
                  msgOK(
                    'Scenarios are different.',
                    'NAADSMDiff',
                    IMGWarning,
                    self
                  );
                  dbcout2( val );
                end
              ;
            end
          ;
          // It isn't necessary to show any warnings here: they would have been displayed elsewhere.

          if( fileExists( xmlFilePath2 ) ) then deleteFile( xmlFilePath1 );
          if( fileExists( xmlFilePath2 ) ) then deleteFile( xmlFilePath2 );
        end
      ;


      // Compare populations, if requested
      //----------------------------------
      if( ( btnComparePopulation = sender ) or ( btnCompareAll = sender ) ) then
        begin
          exportResult1 := exportHerdList( hList1, sim1, xmlFilePath1 );
          exportResult2 := exportHerdList( hList2, sim2, xmlFilePath2 );

          if( ( ExpSuccess = exportResult1 ) and ( ExpSuccess = exportResult2 ) ) then
            begin
              val := diffResult( xmlFilePath1, xmlFilePath2 );
              if( 0 = length( val ) ) then
                begin
                  dbcout2( 'Populations are the same.' );
                  msgOK(
                    'Populations are the same.',
                    'NAADSMDiff',
                    IMGSuccess,
                    self
                  );
                end
              else
                begin
                  dbcout2( 'Populations are different:' );
                  msgOK(
                    'Populations are different.',
                    'NAADSMDiff',
                    IMGWarning,
                    self
                  );
                  dbcout2( val );
                end
              ;
            end
          ;
          // It isn't necessary to show any warnings here: they would have been displayed elsewhere.

          if( fileExists( xmlFilePath2 ) ) then deleteFile( xmlFilePath1 );
          if( fileExists( xmlFilePath2 ) ) then deleteFile( xmlFilePath2 );
        end
      ;


      // Clean up
      //---------
      freeAndNil( hList1 );
      freeAndNil( sim1 );
      smdb1.close();
      deleteFile( smdb1.workingDBFileName );
      freeAndNil( smdb1 );

      freeAndNil( hList2 );
      freeAndNil( sim2 );
      smdb2.close();
      deleteFile( smdb2.workingDBFileName );
      freeAndNil( smdb2 );

      setFormEnabled( true );
    end
  ;



  function TFormMain.exportScenario( smSim: TSMSimulationInput; scenarioFileName: string ): TExportResult;
    var
      simIsValid: boolean;
      dlgLongMessage: TDialogLongMessage;
      err: string;
      tmpCursor: TCursor;
    begin
      err := '';

      simIsValid := smSim.isValid( true, @err );

      if( simIsValid ) then
        begin
          if( smSim.writeXMLFile( scenarioFileName, false (*cbxWriteOutputs.Checked*) ) ) then
            result := ExpSuccess
          else
            begin
              tmpCursor := screen.Cursor;
              screen.Cursor := crDefault;

              msgOK(
                tr( 'This scenario file could not be written.  Please check your file name and available disk space.' ),
                tr( 'Cannot write scenario file' ),
                IMGCritical,
                self
              );

              screen.Cursor := tmpCursor;

              result := ExpFileFailure;
            end
          ;
        end
      else
        begin
          tmpCursor := screen.Cursor;
          screen.Cursor := crDefault;

          dlgLongMessage := TDialogLongMessage.create(
            self,
            SHORTERMASTERCAPTION,
            'Problems were found with this scenario.  These problems'
              + ' must be corrected before this scenario can be exported:',
            err
          );
          dlgLongMessage.showModal();
          dlgLongMessage.free();

          screen.Cursor := tmpCursor;

          result := ExpValidationFailure;
        end
      ;

    end
  ;



  function TFormMain.exportHerdList( herdList: THerdList; smSim: TSMSimulationInput; herdFileName: string ): TExportResult;
    var
      herdsAreValid: boolean;
      dlgLongMessage: TDialogLongMessage;
      err: string;

      tmpCursor: TCursor;
    begin
      err := '';

      herdsAreValid := herdList.isValid( @err );

      if( herdsAreValid ) then
        begin
          //dbcout( 'Writing herd list to file ' + herdFileName );

          if( herdList.writeXMLFile( herdFileName ) ) then
            result := ExpSuccess
          else
            begin
              tmpCursor := screen.Cursor;
              screen.Cursor := crDefault;

              msgOK(
                tr( 'This herd file could not be written.  Please check your file name and available disk space.' ),
                tr( 'Cannot write herd file' ),
                IMGCritical,
                self
              );

              screen.Cursor := tmpCursor;

              result := ExpFileFailure;
            end
          ;

        end
      else
        begin
          tmpCursor := screen.Cursor;
          screen.Cursor := crDefault;

          dlgLongMessage := TDialogLongMessage.create(
            self,
            SHORTERMASTERCAPTION,
            'Problems were found with this herd list.  These problems' + endl
              + ' must be corrected before this herd list can be exported:',
            err
          );
          dlgLongMessage.showModal();
          dlgLongMessage.free();

          screen.Cursor := tmpCursor;

          result := ExpValidationFailure;
        end
      ;
    end
  ;


  procedure TFormMain.setFormEnabled( const val: boolean );
    begin
      setChildrenEnabled( self, val );
      fraScenario1.enabled := val;
      fraScenario2.enabled := val;
    end
  ;

end.
