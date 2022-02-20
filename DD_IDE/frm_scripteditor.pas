unit frm_scripteditor;

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, Graphics,
  SynEdit, SynHighlighterMulti, SynHighlighterDDScript, SynHighlighterActorDef,
  SynHighlighterDDDisasm, SynEditSearch, SynEditRegexSearch, SynEditPrint,
  SynEditPrintTypes, SynCompletionProposal, SynUnicode,
  ide_baseframe, ide_project, Buttons, Dialogs, Menus, Messages;

type

  { TFrame_ScriptEditor }

  TFrame_ScriptEditor = class(TFrame)
    Panel7: TPanel;
    Timer1: TTimer;
    Panel1: TPanel;
    CloseButton1: TSpeedButton;
    CompileButton1: TSpeedButton;
    SaveButton1: TSpeedButton;
    PrintDialog: TPrintDialog;
    CompileAndSaveButton1: TSpeedButton;
    PrintButton1: TSpeedButton;
    PrintPreviewButton1: TSpeedButton;
    PageSetupButton1: TSpeedButton;
    SavePCodeDialog1: TSaveDialog;
    EventsPopupMenu: TPopupMenu;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    N91: TMenuItem;
    N101: TMenuItem;
    N111: TMenuItem;
    N121: TMenuItem;
    N131: TMenuItem;
    N141: TMenuItem;
    N151: TMenuItem;
    N161: TMenuItem;
    N171: TMenuItem;
    N181: TMenuItem;
    N191: TMenuItem;
    N201: TMenuItem;
    EventsSpeedButton: TSpeedButton;
    procedure Timer1Timer(Sender: TObject);
    procedure CloseButton1Click(Sender: TObject);
    procedure CompileButton1Click(Sender: TObject);
    procedure SaveButton1Click(Sender: TObject);
    procedure PrintButton1Click(Sender: TObject);
    procedure PrintPreviewButton1Click(Sender: TObject);
    procedure PageSetupButton1Click(Sender: TObject);
    procedure CompileAndSaveButton1Click(Sender: TObject);
    procedure EventsSpeedButtonClick(Sender: TObject);
    procedure OnEventsPopupMenuClick(Sender: TObject);
  private
    { private declarations }
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynDDScriptSyn;
    SynEditRegexSearch1: TSynEditRegexSearch;
    SynEditSearch1: TSynEditSearch;
    SynActordefSyn1: TSynActordefSyn;
    SynDDDisasmSyn1: TSynDDDisasmSyn;
    scpParams: TSynCompletionProposal;
    scpParamsHintList: TStringList;
    fSearchFromCaret: boolean;
    fEventsList: TStringList;
    lastgame: string;
    procedure CompileCmd(const dosave: Boolean);
    procedure SavePCodeCmd(const pcode: string);
    procedure ShowSearchReplaceDialog(AReplace: boolean);
    procedure DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
    procedure OnEditorChange(Sender: TObject);
    procedure scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
      var AString: UnicodeString; var x, y: Integer; var CanExecute: Boolean);
    procedure GenerateParameterHintList(const gamestr: string);
    procedure UpdateEventsMenuItems;
    procedure UpdateControls;
  public
    { public declarations }
    obj: TBaseEditorObject;
    pcode: string;
    SynEditPrint: TSynEditPrint;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustLayout(const adata: AnsiString);
    procedure ShowSearchDialog;
    procedure SearchAgain;
    procedure ShowReplaceDialog;
    procedure GoToLineNumber;
  end;

implementation

uses
  Windows,
  SynEditTypes,
  ide_utils,
  ddc_base,
  frm_GotoLine, frm_SearchText, frm_ReplaceText, frm_ConfirmReplace,
  frm_PrintPreview, frm_PageSetup;

{$R *.dfm}

var
  gbSearchBackwards: boolean;
  gbSearchCaseSensitive: boolean;
  gbSearchFromCaret: boolean;
  gbSearchSelectionOnly: boolean;
  gbSearchTextAtCaret: boolean;
  gbSearchWholeWords: boolean;
  gbSearchRegex: boolean;
  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

//==============================================================================
//
// TFrame_ScriptEditor.AdjustLayout
//
//==============================================================================
procedure TFrame_ScriptEditor.AdjustLayout(const adata: AnsiString);
var
  it: TIDEProjectItem;
  prj: TIDEProject;
begin
  Screen.Cursor := crHourGlass;
  try
    if obj <> nil then
      if obj is TIDEProjectItem then
      begin
        it := obj as TIDEProjectItem;
        if it.ItemType in [pi_script, pi_actor] then
          SynEdit1.Lines.Text := adata
        else if it.ItemType = pi_compiledscript then
        begin
          prj := it.Parent as TIDEProject;
          LogOutput(LOG_NORMAL, 'Disassembling %s compiled script "%s"', [prj.Game, it.FileName]);
          SynEdit1.Lines.Text := dll_getdisassembly(prj.Game, adata);
        end;
      end;
    UpdateControls;
  finally
    Screen.Cursor := crDefault;
  end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.Create
//
//==============================================================================
constructor TFrame_ScriptEditor.Create(AOwner: TComponent);
var
  AFont: TFont;
begin
  inherited;
  obj := nil;
  pcode := '';

  SynEditRegexSearch1 := TSynEditRegexSearch.Create(Self);
  SynEditSearch1 := TSynEditSearch.Create(Self);

  SynEdit1 := TSynEdit.Create(Self);
  SynEdit1.Parent := Self;
  SynEdit1.Align := alClient;
  SynEdit1.Highlighter := nil;
  SynEdit1.OnChange := OnEditorChange;
  SynEdit1.Gutter.ShowLineNumbers := True;
  SynEdit1.Gutter.AutoSize := True;
  SynEdit1.MaxScrollWidth := 255;
  SynEdit1.WantTabs := True;
  SynEdit1.ReadOnly := True;

  SynPasSyn1 := TSynDDScriptSyn.Create(Self);

  SynActordefSyn1 := TSynActordefSyn.Create(self);
  SynActordefSyn1.CommentAttri.Foreground := RGB(0, 128, 0);
  SynActordefSyn1.IdentifierAttri.Foreground := RGB(0, 0, 0);
  SynActordefSyn1.KeyAttri.Foreground := RGB(0, 0, 128);
  SynActordefSyn1.NumberAttri.Foreground := RGB(0, 0, 255);
  SynActordefSyn1.StringAttri.Foreground := RGB(0, 0, 255);

  SynDDDisasmSyn1 := TSynDDDisasmSyn.Create(self);
  SynDDDisasmSyn1.CommentAttri.Foreground := RGB(0, 128, 0);
  SynDDDisasmSyn1.IdentifierAttri.Foreground := RGB(0, 0, 0);
  SynDDDisasmSyn1.KeyAttri.Foreground := RGB(0, 0, 128);
  SynDDDisasmSyn1.NumberAttri.Foreground := RGB(0, 0, 255);
  SynDDDisasmSyn1.StringAttri.Foreground := RGB(0, 0, 255);

  SynEditPrint := TSynEditPrint.Create(self);
  SynEditPrint.Copies := 1;
  SynEditPrint.Header.FrameTypes := [ftBox, ftShaded];
  SynEditPrint.Header.DefaultFont.Charset := DEFAULT_CHARSET;
  SynEditPrint.Header.DefaultFont.Color := clBlack;
  SynEditPrint.Header.DefaultFont.Height := -13;
  SynEditPrint.Header.DefaultFont.Name := 'Arial';
  SynEditPrint.Header.DefaultFont.Style := [];
  SynEditPrint.Footer.DefaultFont.Charset := DEFAULT_CHARSET;
  SynEditPrint.Footer.DefaultFont.Color := clBlack;
  SynEditPrint.Footer.DefaultFont.Height := -13;
  SynEditPrint.Footer.DefaultFont.Name := 'Arial';
  SynEditPrint.Footer.DefaultFont.Style := [];
  SynEditPrint.Margins.Left := 25.000000000000000000;
  SynEditPrint.Margins.Right := 15.000000000000000000;
  SynEditPrint.Margins.Top := 25.000000000000000000;
  SynEditPrint.Margins.Bottom := 25.000000000000000000;
  SynEditPrint.Margins.Header := 15.000000000000000000;
  SynEditPrint.Margins.Footer := 15.000000000000000000;
  SynEditPrint.Margins.LeftHFTextIndent := 2.000000000000000000;
  SynEditPrint.Margins.RightHFTextIndent := 2.000000000000000000;
  SynEditPrint.Margins.HFInternalMargin := 0.500000000000000000;
  SynEditPrint.Margins.MirrorMargins := False;
  SynEditPrint.Font.Charset := DEFAULT_CHARSET;
  SynEditPrint.Font.Color := clWindowText;
  SynEditPrint.Font.Height := -11;
  SynEditPrint.Font.Name := 'MS Sans Serif';
  SynEditPrint.Font.Style := [];
  SynEditPrint.Colors := True;
  SynEditPrint.Highlighter := SynPasSyn1;
  SynEditPrint.TabWidth := 2;
  SynEditPrint.Color := clWhite;

  SynEditPrint.Header.Add('$TITLE$', nil, taLeftJustify, 1);
  SynEditPrint.Footer.Add('$PAGENUM$/$PAGECOUNT$', nil, taRightJustify, 1);
  AFont := TFont.Create;
  AFont.Assign(SynEditPrint.Footer.DefaultFont);
  AFont.Size := 6;
  SynEditPrint.Footer.Add('Print Date: $DATE$. Time: $TIME$', AFont, taRightJustify, 2);
  AFont.Free;

  lastgame := '';

  scpParamsHintList := TStringList.Create;

  scpParams := TSynCompletionProposal.Create(self);
  scpParams.DefaultType := ctParams;
  scpParams.Options := [scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer];
  scpParams.ClBackground := clInfoBk;
  scpParams.Width := 262;
  scpParams.EndOfTokenChr := '()[]. ';
  scpParams.TriggerChars := '(';
  scpParams.Font.Charset := DEFAULT_CHARSET;
  scpParams.Font.Color := clWindowText;
  scpParams.Font.Height := -11;
  scpParams.Font.Name := 'MS Sans Serif';
  scpParams.Font.Style := [];
  scpParams.TitleFont.Charset := DEFAULT_CHARSET;
  scpParams.TitleFont.Color := clBtnText;
  scpParams.TitleFont.Height := -11;
  scpParams.TitleFont.Name := 'MS Sans Serif';
  scpParams.TitleFont.Style := [fsBold];
  scpParams.OnExecute := scpParamsExecute;
  scpParams.ShortCut := 24608;
  scpParams.Editor := SynEdit1;
  scpParams.TimerInterval := 1000;
  scpParams.EndOfTokenChr := '';

  fEventsList := TStringList.Create;
end;

//==============================================================================
//
// TFrame_ScriptEditor.Destroy
//
//==============================================================================
destructor TFrame_ScriptEditor.Destroy;
begin
  if obj <> nil then
    obj.Editor := nil;

  FreeStringList(scpParamsHintList);

  fEventsList.Free;

  inherited;
end;

//==============================================================================
//
// TFrame_ScriptEditor.GoToLineNumber
//
//==============================================================================
procedure TFrame_ScriptEditor.GoToLineNumber;
begin
  with TfrmGotoLine.Create(self) do
  try
    Char := SynEdit1.CaretX;
    Line := SynEdit1.CaretY;
    ShowModal;
    if ModalResult = mrOK then
      SynEdit1.CaretXY := CaretXY;
  finally
    Free;
  end;
  try
    SynEdit1.SetFocus;
  except
  end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.Timer1Timer
//
//==============================================================================
procedure TFrame_ScriptEditor.Timer1Timer(Sender: TObject);
begin
  UpdateControls;
end;

//==============================================================================
//
// TFrame_ScriptEditor.UpdateControls
//
//==============================================================================
procedure TFrame_ScriptEditor.UpdateControls;
begin
  fEventsList.Text := '';
  if obj = nil then
    SynEdit1.Highlighter := nil
  else if obj is TIDEProjectItem then
  begin
    if (obj as TIDEProjectItem).ItemType = pi_script then
    begin
      SaveButton1.Visible := True;
      SynEdit1.Highlighter := SynPasSyn1;
      SynEditPrint.Highlighter := SynPasSyn1;
      CompileButton1.Visible := True;
      CompileAndSaveButton1.Visible := True;
      EventsSpeedButton.Visible := True;
      fEventsList.Text := dll_getevents((obj as TIDEProjectItem).Parent.Game);
      SynEdit1.ReadOnly := False;
    end
    else if (obj as TIDEProjectItem).ItemType = pi_actor then
    begin
      SaveButton1.Visible := True;
      SynEdit1.Highlighter := SynActordefSyn1;
      SynEditPrint.Highlighter := SynActordefSyn1;
      SynEdit1.ReadOnly := False;
    end
    else if (obj as TIDEProjectItem).ItemType = pi_compiledscript then
    begin
      SaveButton1.Visible := False;
      SynEdit1.Highlighter := SynDDDisasmSyn1;
      SynEditPrint.Highlighter := SynDDDisasmSyn1;
      SynEdit1.ReadOnly := True;
    end
    else
    begin
      SynEdit1.Highlighter := nil;
      SynEditPrint.Highlighter := nil;
    end;
    GenerateParameterHintList((obj as TIDEProjectItem).Parent.Game);
    Timer1.Enabled := False;
  end
  else
    SynEdit1.Highlighter := nil;
end;

//==============================================================================
//
// TFrame_ScriptEditor.CloseButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.CloseButton1Click(Sender: TObject);
begin
  if Parent <> nil then
    if Parent is TTabSheet then
      (Parent as TTabSheet).Enabled := False;
end;

//==============================================================================
//
// TFrame_ScriptEditor.OnEditorChange
//
//==============================================================================
procedure TFrame_ScriptEditor.OnEditorChange(Sender: TObject);
var
  sw: WideString;
begin
  if SynEdit1.Modified then
    if obj <> nil then
      if obj is TIDEProjectItem then
        if (obj as TIDEProjectItem).ItemType in [pi_script, pi_actor] then
        begin
          sw := SynEdit1.Text;
          obj.data := AnsiString(sw);
        end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.ShowSearchReplaceDialog
//
//==============================================================================
procedure TFrame_ScriptEditor.ShowSearchReplaceDialog(AReplace: boolean);
var
  dlg: TTextSearchDialog;
begin
  if AReplace then
    dlg := TTextReplaceDialog.Create(Self)
  else
    dlg := TTextSearchDialog.Create(Self);
  with dlg do
  try
    // assign search options
    SearchBackwards := gbSearchBackwards;
    SearchCaseSensitive := gbSearchCaseSensitive;
    SearchFromCursor := gbSearchFromCaret;
    SearchInSelectionOnly := gbSearchSelectionOnly;
    // start with last search text
    SearchText := gsSearchText;
    if gbSearchTextAtCaret then
    begin
      // if something is selected search for that text
      if SynEdit1.SelAvail and (SynEdit1.BlockBegin.Line = SynEdit1.BlockEnd.Line) //Birb (fix at SynEdit's SearchReplaceDemo)
      then
        SearchText := SynEdit1.SelText
      else
        SearchText := SynEdit1.GetWordAtRowCol(SynEdit1.CaretXY);
    end;
    SearchTextHistory := gsSearchTextHistory;
    if AReplace then with dlg as TTextReplaceDialog do
    begin
      ReplaceText := gsReplaceText;
      ReplaceTextHistory := gsReplaceTextHistory;
    end;
    SearchWholeWords := gbSearchWholeWords;
    if ShowModal = mrOK then
    begin
      gbSearchBackwards := SearchBackwards;
      gbSearchCaseSensitive := SearchCaseSensitive;
      gbSearchFromCaret := SearchFromCursor;
      gbSearchSelectionOnly := SearchInSelectionOnly;
      gbSearchWholeWords := SearchWholeWords;
      gbSearchRegex := SearchRegularExpression;
      gsSearchText := SearchText;
      gsSearchTextHistory := SearchTextHistory;
      if AReplace then
        with dlg as TTextReplaceDialog do
        begin
          gsReplaceText := ReplaceText;
          gsReplaceTextHistory := ReplaceTextHistory;
        end;
      fSearchFromCaret := gbSearchFromCaret;
      if gsSearchText <> '' then
      begin
        DoSearchReplaceText(AReplace, gbSearchBackwards);
        fSearchFromCaret := True;
      end;
    end;
  finally
    dlg.Free;
  end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.DoSearchReplaceText
//
//==============================================================================
procedure TFrame_ScriptEditor.DoSearchReplaceText(AReplace: boolean; ABackwards: boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not fSearchFromCaret then
    Include(Options, ssoEntireScope);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if gbSearchRegex then
    SynEdit1.SearchEngine := SynEditRegexSearch1
  else
    SynEdit1.SearchEngine := SynEditSearch1;
  if SynEdit1.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    LogOutput(LOG_INFO, '"%s" not found'#13#10, [gsSearchText]);
    if ssoBackwards in Options then
      SynEdit1.BlockEnd := SynEdit1.BlockBegin
    else
      SynEdit1.BlockBegin := SynEdit1.BlockEnd;
    SynEdit1.CaretXY := SynEdit1.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

//==============================================================================
//
// TFrame_ScriptEditor.ShowSearchDialog
//
//==============================================================================
procedure TFrame_ScriptEditor.ShowSearchDialog;
begin
  ShowSearchReplaceDialog(False);
end;

//==============================================================================
//
// TFrame_ScriptEditor.SearchAgain
//
//==============================================================================
procedure TFrame_ScriptEditor.SearchAgain;
var
  Options: TSynSearchOptions;
begin
  if gsSearchText = '' then
  begin
    ShowSearchDialog;
    Exit;
  end;

  Options := [];
  if gbSearchBackwards then
    Include(Options, ssoBackwards);
  if gbSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if gbSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if gbSearchWholeWords then
    Include(Options, ssoWholeWord);
  if gbSearchRegex then
    SynEdit1.SearchEngine := SynEditRegexSearch1
  else
    SynEdit1.SearchEngine := SynEditSearch1;
  if SynEdit1.SearchReplace(gsSearchText, '', Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    LogOutput(LOG_NORMAL, '"%s" not found'#13#10, [gsSearchText]);
    if ssoBackwards in Options then
      SynEdit1.BlockEnd := SynEdit1.BlockBegin
    else
      SynEdit1.BlockBegin := SynEdit1.BlockEnd;
    SynEdit1.CaretXY := SynEdit1.BlockBegin;
  end;

  if ConfirmReplaceDialog <> nil then
    ConfirmReplaceDialog.Free;
end;

//==============================================================================
//
// TFrame_ScriptEditor.ShowReplaceDialog
//
//==============================================================================
procedure TFrame_ScriptEditor.ShowReplaceDialog;
begin
  ShowSearchReplaceDialog(True);
end;

//==============================================================================
//
// TFrame_ScriptEditor.CompileButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.CompileButton1Click(Sender: TObject);
begin
  CompileCmd(False);
end;

//==============================================================================
//
// TFrame_ScriptEditor.CompileCmd
//
//==============================================================================
procedure TFrame_ScriptEditor.CompileCmd(const dosave: Boolean);
var
  it: TIDEProjectItem;
  prj: TIDEProject;
  code, msgs: string;
  i: integer;
  mList: TStringList;
  ret: boolean;
  p1, p2: integer;
  location: string;
  sx, sy: string;
begin
  Screen.Cursor := crHourGlass;
  try
    if obj <> nil then
      if obj is TIDEProjectItem then
      begin
        it := obj as TIDEProjectItem;
        if it.ItemType = pi_script then
        begin
          prj := it.Parent as TIDEProject;
          LogOutput(LOG_NORMAL, 'Compiling %s script "%s"', [prj.Game, it.RelativePath]);
          code := AnsiString(SynEdit1.Text);
          msgs := '';
          pcode := '';
          ret := dll_compile(prj.Game, code, pcode, msgs);
          mList := TStringList.Create;
          try
            mList.Text := msgs;
            if ret then
            begin
              for i := 0 to mList.Count - 1 do
                LogOutput(LOG_WARNING, mList.Strings[i]);
              LogOutput(LOG_NORMAL, 'Compile successful');
              if dosave then
                SavePCodeCmd(pcode);
            end
            else
            begin
              for i := 0 to mList.Count - 1 do
                LogOutput(LOG_ERROR, mList.Strings[i]);
            end;
            if msgs <> '' then
            begin
              p1 := Pos('[Error]', msgs);
              p2 := Pos('):', msgs);
              if p2 > p1 then
              begin
                location := '';
                for i := p1 to p2 do
                  if msgs[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
                    location := location + msgs[i]
                  else
                    location := location + ' ';
                 splitstring(Trim(location), sx, sy, ' ');
                 p1 := StrToIntDef(sx, -1);
                 p2 := StrToIntDef(sy, -1);
                 if (p1 >= 0) and (p2 >= 0) then
                 begin
                  SynEdit1.CaretX := p2;
                  SynEdit1.CaretY := p1;
                  SynEdit1.SetFocus;
                 end;
              end;
            end;
          finally
            mList.Free;
          end;
        end;
      end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.SavePCodeCmd
//
//==============================================================================
procedure TFrame_ScriptEditor.SavePCodeCmd(const pcode: string);
var
  i: integer;
  fs: TFileStream;
  it: TIDEProjectItem;
begin
  if obj <> nil then
    if obj is TIDEProjectItem then
    begin
      it := obj as TIDEProjectItem;
      SavePCodeDialog1.FileName := ChangeFileExt(it.FileName, '.ddout');
      if SavePCodeDialog1.Execute then
      begin
        fs := TFileStream.Create(SavePCodeDialog1.FileName, fmCreate or fmShareExclusive);
        try
          for i := 1 to Length(pcode) do
            fs.Write(pcode[i], SizeOf(Char));
        finally
          fs.Free;
        end;
      end;
    end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.SaveButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.SaveButton1Click(Sender: TObject);
var
  it: TIDEProjectItem;
begin
  Screen.Cursor := crHourGlass;
  try
    if obj <> nil then
      if obj is TIDEProjectItem then
      begin
        it := obj as TIDEProjectItem;
        if it.ItemType in [pi_script, pi_actor] then
          it.SaveToFile(it.FileName);
      end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.PrintButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.PrintButton1Click(Sender: TObject);
var
  it: TIDEProjectItem;
begin
  if obj <> nil then
    if obj is TIDEProjectItem then
    begin
      it := obj as TIDEProjectItem;
      if PrintDialog.Execute then
      begin
        SynEditPrint.SynEdit := SynEdit1;
        SynEditPrint.Title := it.FileName;
        SynEditPrint.Print;
      end;
    end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.PrintPreviewButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.PrintPreviewButton1Click(Sender: TObject);
var
  it: TIDEProjectItem;
begin
  if obj <> nil then
    if obj is TIDEProjectItem then
    begin
      it := obj as TIDEProjectItem;
      SynEditPrint.SynEdit := SynEdit1;
      SynEditPrint.Title := it.FileName;
      ScriptPrintPreviewDlg.Caption := 'Print Preview - ' + it.FileName;
      ScriptPrintPreviewDlg.SynEditPrintPreview.SynEditPrint := SynEditPrint;
      ScriptPrintPreviewDlg.ShowModal;
    end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.PageSetupButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.PageSetupButton1Click(Sender: TObject);
begin
  PageSetupDlg.SetValues(SynEditPrint);
  if PageSetupDlg.ShowModal = mrOk then
    PageSetupDlg.GetValues(SynEditPrint);
end;

//==============================================================================
//
// TFrame_ScriptEditor.scpParamsExecute
//
//==============================================================================
procedure TFrame_ScriptEditor.scpParamsExecute(Kind: SynCompletionType; Sender: TObject;
  var AString: UnicodeString; var x, y: Integer; var CanExecute: Boolean);
var
  locline, lookup: UnicodeString;
  TmpX, savepos, StartX,
  ParenCounter,
  TmpLocation: Integer;
  FoundMatch: Boolean;
  idx: integer;
begin
  idx := -1;
  with TSynCompletionProposal(Sender).Editor do
  begin
    locLine := LineText;

    //go back from the cursor and find the first open paren
    TmpX := CaretX;
    if TmpX > Length(locLine) then
      TmpX := Length(locLine)
    else
      Dec(TmpX);
    FoundMatch := False;
    TmpLocation := 0;
    while (TmpX > 0) and not FoundMatch do
    begin
      if LocLine[TmpX] = ',' then
      begin
        Inc(TmpLocation);
        Dec(TmpX);
      end
      else if LocLine[TmpX] = ')' then
      begin
        //We found a close, go till it's opening paren
        ParenCounter := 1;
        Dec(TmpX);
        while (TmpX > 0) and (ParenCounter > 0) do
        begin
          if LocLine[TmpX] = ')' then
            Inc(ParenCounter)
          else if LocLine[TmpX] = '(' then
            Dec(ParenCounter);
          Dec(TmpX);
        end;
        if TmpX > 0 then
          Dec(TmpX);  //eat the open paren
      end
      else
      if locLine[TmpX] = '(' then
      begin
        //we have a valid open paren, lets see what the word before it is
        StartX := TmpX;
        while (TmpX > 0) and not SynEdit1.IsIdentChar(locLine[TmpX]) do
          Dec(TmpX);
        if TmpX > 0 then
        begin
          SavePos := TmpX;
          while (TmpX > 0) and SynEdit1.IsIdentChar(locLine[TmpX]) do
            Dec(TmpX);
          Inc(TmpX);
          lookup := UpperCase(Copy(LocLine, TmpX, SavePos - TmpX + 1));
          idx := scpParamsHintList.IndexOf(Lookup);
          FoundMatch := idx > -1;
          if not FoundMatch then
          begin
            TmpX := StartX;
            Dec(TmpX);
          end;
        end;
      end
      else
        Dec(TmpX)
    end;
  end;

  CanExecute := FoundMatch;

  if CanExecute then
  begin
    TSynCompletionProposal(Sender).Form.CurrentIndex := TmpLocation;
    if Lookup <> TSynCompletionProposal(Sender).PreviousToken then
    begin
      if idx > -1 then
      begin
        TSynCompletionProposal(Sender).ItemList.Clear;
        TSynCompletionProposal(Sender).ItemList.Add((scpParamsHintList.Objects[idx] as TSimpleString).str);
      end;
    end;
  end
  else
    TSynCompletionProposal(Sender).ItemList.Clear;
end;

//==============================================================================
//
// TFrame_ScriptEditor.GenerateParameterHintList
//
//==============================================================================
procedure TFrame_ScriptEditor.GenerateParameterHintList(const gamestr: string);
var
  i, j, k: integer;
  units: TStringList;
  actordeffuncs: TStringList;
  lst: TStringList;
  decl, stmp, params: string;
  afuncname: string;
  pos1, pos2: integer;
  it: TIDEProjectItem;
begin
  if LowerCase(lastgame) = LowerCase(gamestr) then
    Exit;

  lastgame := LowerCase(gamestr);

  units := dll_getuntisfuncdeclarations(lastgame);

  if units = nil then
    Exit;

  FreeStringList(scpParamsHintList);
  scpParamsHintList := TStringList.Create;

  for i := 0 to units.Count - 1 do
  begin
    lst := units.Objects[i] as TStringList;
    for j := 0 to lst.Count - 1 do
    begin
      decl := lst.Strings[j];
      stmp := decl;
      for k := 1 to Length(decl) do
        if stmp[k] in ['(', ';', ':'] then
          stmp[k] := ' ';
      pos1 := Pos('(', decl);
      pos2 := Pos(')', decl);
      params := '';
      if (pos1 > 1) and (pos2 > pos1) then
      begin
        params := '"' + Trim(Copy(decl, pos1 + 1, pos2 - pos1 - 1)) + '"';
        if params = '""' then
          params := ''
        else
        begin
          params := StringReplace(params, ',', ',","', [rfReplaceAll]);
          params := StringReplace(params, ';', ',","', [rfReplaceAll]);
        end;
      end;
      if params = '' then
        params := '** No parameters expected';
      scpParamsHintList.AddObject(UpperCase(firstword(secondword(stmp, ' '), ' ')), TSimpleString.Create(params));
    end;
  end;

  actordeffuncs := nil;
  if obj <> nil then
    if obj is TIDEProjectItem then
    begin
      it := obj as TIDEProjectItem;
      if it.ItemType = pi_actor then
        actordeffuncs := dll_getactordeffunctions(lastgame);
    end;

  if actordeffuncs <> nil then
  begin
    for j := 0 to actordeffuncs.Count - 1 do
    begin
      decl := actordeffuncs.Strings[j];
      stmp := decl;
      for k := 1 to Length(decl) do
        if stmp[k] in ['(', ';', ':'] then
          stmp[k] := ' ';
      pos1 := Pos('(', decl);
      pos2 := Pos(')', decl);
      params := '';
      if (pos1 > 1) and (pos2 > pos1) then
      begin
        params := '"' + Trim(Copy(decl, pos1 + 1, pos2 - pos1 - 1)) + '"';
        if params = '""' then
          params := ''
        else
        begin
          params := StringReplace(params, ',', ',","', [rfReplaceAll]);
          params := StringReplace(params, ';', ',","', [rfReplaceAll]);
        end;
      end;
      if params = '' then
        params := '** No parameters expected';
      afuncname := Trim(UpperCase(firstword(stmp, ' ')));
      if afuncname <> '' then
      begin
        scpParamsHintList.AddObject(afuncname, TSimpleString.Create(params));
        if Pos('A_', afuncname) = 1 then
        begin
          afuncname[1] := ' ';
          afuncname[2] := ' ';
          afuncname := Trim(afuncname);
          if afuncname <> '' then
            scpParamsHintList.AddObject(afuncname, TSimpleString.Create(params));
        end;
      end;
    end;
    actordeffuncs.Free;
  end;

  scpParamsHintList.Sorted := True;

  FreeStringList(units);
end;

//==============================================================================
//
// TFrame_ScriptEditor.CompileAndSaveButton1Click
//
//==============================================================================
procedure TFrame_ScriptEditor.CompileAndSaveButton1Click(Sender: TObject);
begin
  CompileCmd(True);
end;

//==============================================================================
//
// TFrame_ScriptEditor.EventsSpeedButtonClick
//
//==============================================================================
procedure TFrame_ScriptEditor.EventsSpeedButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  UpdateEventsMenuItems;
  p := EventsSpeedButton.ClientToScreen(Point(0, EventsSpeedButton.Height));
  EventsPopupMenu.Popup(p.X, p.Y);
  EventsSpeedButton.Down := False;
end;

//==============================================================================
//
// TFrame_ScriptEditor.UpdateEventsMenuItems
//
//==============================================================================
procedure TFrame_ScriptEditor.UpdateEventsMenuItems;
var
  i: integer;
  cnt: integer;
  s1, s2: string;
begin
  cnt := fEventsList.Count;
  if cnt > EventsPopupMenu.Items.Count then
    cnt := EventsPopupMenu.Items.Count;
  for i := 0 to cnt - 1 do
  begin
    splitstring(fEventsList.Strings[i], s1, s2, '|');
    EventsPopupMenu.Items[i].Visible := True;
    EventsPopupMenu.Items[i].Caption := s1;
    EventsPopupMenu.Items[i].Tag := i;
  end;
  for i := cnt to EventsPopupMenu.Items.Count - 1 do
  begin
    EventsPopupMenu.Items[i].Visible := False;
    EventsPopupMenu.Items[i].Tag := i;
  end;
end;

//==============================================================================
//
// TFrame_ScriptEditor.OnEventsPopupMenuClick
//
//==============================================================================
procedure TFrame_ScriptEditor.OnEventsPopupMenuClick(Sender: TObject);
var
  mitem: TMenuItem;
  id: integer;
  skey, sevent, decl: string;
  fHighlighter: TSynDDScriptSyn;
  p: integer;
  found: boolean;
  len: integer;
begin
  mitem := Sender as TMenuItem;
  id := mitem.tag;
  if mitem.tag < fEventsList.Count then
  begin
    splitstring(fEventsList.Strings[id], sevent, decl, '|');
    sevent := UpperCase(sevent);
    fHighlighter := TSynDDScriptSyn.Create(nil);
    fHighlighter.ResetRange;
    fHighlighter.SetLine(SynEdit1.Lines.Text, 1);
    p := -1;
    found := False;
    while not fHighlighter.GetEol do
    begin
      if fHighlighter.GetTokenKind = Ord(SynHighlighterDDScript.tkKey) then
      begin
        skey := UpperCase(fHighlighter.GetToken);
        if skey = 'EVENT' then
        begin
          p := fHighlighter.GetTokenPos;
          while not fHighlighter.GetEol do
          begin
            fHighlighter.Next;
            if fHighlighter.GetTokenKind = Ord(SynHighlighterDDScript.tkIdentifier) then
            begin
              if UpperCase(fHighlighter.GetToken) = sevent then
                found := True;
              Break;
            end;
            if (fHighlighter.GetTokenKind = Ord(SynHighlighterDDScript.tkComment)) or
               (fHighlighter.GetTokenKind = Ord(SynHighlighterDDScript.tkPreprocessor)) or
               (fHighlighter.GetTokenKind = Ord(SynHighlighterDDScript.tkSpace)) or
               (fHighlighter.GetTokenKind = Ord(SynHighlighterDDScript.tkNull)) then
              Continue;
            Break;
          end;
          if found then
            Break
          else
            p := -1;
        end;
      end;
      fHighlighter.Next;
    end;

    fHighlighter.Free;

    if found then
    begin
      SynEdit1.SelStart := p;
      SynEdit1.SelEnd := p;
      SynEdit1.SetFocus;
      Exit;
    end;

    len := Length(SynEdit1.Lines.Text);

    decl := #13#10 + #13#10 + StringReplace(decl, '|', #13#10, [rfReplaceAll]);
    if Trim(SynEdit1.Lines.Text) = '' then
      decl := 'uses'#13#10'  all;'#13#10 + decl;
    p := Pos('end;', decl);
    if p > 3 then
      p := p - 3
    else
      p := 0;
    SynEdit1.Lines.Text := SynEdit1.Lines.Text + decl;
    SynEdit1.SelStart := len + p;
    SynEdit1.SelEnd := len + p;
    SynEdit1.SetFocus;
  end;
end;

end.

