unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, ComCtrls, Menus, ActnList, ImgList, ide_project,
  frm_projectmanager, AppEvnts, StdActns, StdCtrls, Clipbrd,
  frm_unitfunctions, frm_constants, frm_variables, frm_classes, frm_types,
  frm_actordef, frm_mobjinfo, frm_states, frm_sprites;

type
  TForm1 = class(TForm)
    AboutButton1: TSpeedButton;
    ActionQuit: TAction;
    ActionAbout: TAction;
    ActionRedo: TAction;
    ActionUndo: TAction;
    ActionSaveAs: TAction;
    ActionSave: TAction;
    ActionOpenProject: TAction;
    ActionNewProject: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    ExitButton1: TSpeedButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    NewButton1: TSpeedButton;
    OpenButton1: TSpeedButton;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    LibraryPanel: TPanel;
    RedoButton1: TSpeedButton;
    SaveAsButton1: TSpeedButton;
    SaveButton1: TSpeedButton;
    SaveProjectDialog: TSaveDialog;
    LibrarySplitter: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    UndoButton1: TSpeedButton;
    PageControl2: TPageControl;
    TabSheet1: TTabSheet;
    Frame_ProjectManager1: TFrame_ProjectManager;
    ActionGoToLineNumber: TAction;
    OpenProjectDialog: TOpenDialog;
    StatusBar1: TStatusBar;
    ApplicationEvents1: TApplicationEvents;
    AddNewActorDialog1: TOpenDialog;
    AddNewScriptDialog1: TOpenDialog;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditDelete1: TEditDelete;
    SelectAll1: TMenuItem;
    EditUndo1: TEditUndo;
    NewActor1: TMenuItem;
    NewScript1: TMenuItem;
    NewCompiledScript1: TMenuItem;
    AddNewCompiledScriptDialog1: TOpenDialog;
    BottomPageControl: TPageControl;
    OutputTabSheet: TTabSheet;
    MessagesListBox: TListBox;
    ActionFind: TAction;
    ActionReplace: TAction;
    ActionSearchAgain: TAction;
    ActionProjectDeleteItem: TAction;
    N1: TMenuItem;
    GeneratePK31: TMenuItem;
    GeneratePK3SaveDialog: TSaveDialog;
    PopupMenu1: TPopupMenu;
    Clearmessages1: TMenuItem;
    SaveMessages1: TMenuItem;
    N2: TMenuItem;
    Copy1: TMenuItem;
    SaveMessagesDialog: TSaveDialog;
    Panel2: TPanel;
    LibraryButton1: TSpeedButton;
    LibraryPageControl: TPageControl;
    FunctionsTabSheet: TTabSheet;
    TabSheet3: TTabSheet;
    Frame_UnitFunctions1: TFrame_UnitFunctions;
    Frame_Constants1: TFrame_Constants;
    TabSheet2: TTabSheet;
    Frame_Variables1: TFrame_Variables;
    TabSheet4: TTabSheet;
    Frame_Classes1: TFrame_Classes;
    TabSheet5: TTabSheet;
    Frame_Types1: TFrame_Types;
    TabSheet6: TTabSheet;
    Frame_Actordef1: TFrame_Actordef;
    TabSheet7: TTabSheet;
    Frame_Mobjinfo1: TFrame_Mobjinfo;
    TabSheet8: TTabSheet;
    Frame_States1: TFrame_States;
    TabSheet9: TTabSheet;
    Frame_Sprites1: TFrame_Sprites;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionNewProjectExecute(Sender: TObject);
    procedure ActionOpenProjectExecute(Sender: TObject);
    procedure ActionQuitExecute(Sender: TObject);
    procedure ActionRedoExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionUndoExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure ActionGoToLineNumberExecute(Sender: TObject);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure Frame_ProjectManager1AddActorButton1Click(Sender: TObject);
    procedure Frame_ProjectManager1TreeView1DblClick(Sender: TObject);
    procedure Frame_ProjectManager1AddScriptButton1Click(Sender: TObject);
    procedure Frame_ProjectManager1AddCompiledScriptButton1Click(
      Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure ActionFindExecute(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);
    procedure ActionSearchAgainExecute(Sender: TObject);
    procedure Frame_ProjectManager1TreeView1Editing(Sender: TObject;
      Node: TTreeNode; var AllowEdit: Boolean);
    procedure Frame_ProjectManager1TreeView1Edited(Sender: TObject;
      Node: TTreeNode; var S: String);
    procedure MessagesListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure MenuItem3Click(Sender: TObject);
    procedure ActionProjectDeleteItemExecute(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure GeneratePK31Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Clearmessages1Click(Sender: TObject);
    procedure SaveMessages1Click(Sender: TObject);
    procedure LibraryButton1Click(Sender: TObject);
    procedure LibraryPanelResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PageControl1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PageControl1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Frame_ProjectManager1GameTypeComboBoxChange(Sender: TObject);
  private
    { private declarations }
    project: TIDEProject;
    procedure CreateProjectObject(const fname: string);
    procedure DoNewProject;
    procedure DoOpenProject(const fname: string);
    procedure NewProject;
    procedure OpenProject;
    procedure DoSave(const fname: string);
    procedure Save;
    procedure SaveAs;
    procedure Quit;
    procedure About;
    procedure Undo;
    procedure Redo;
    function CheckCanClose: boolean;
    function CheckCanCloseProjectItem(const x: TIDEProjectItem): boolean;
    procedure ChangeGame(const newgame: string);
    function ActiveEditorFrame: TFrame;
    function ProjectEditorFrame: TFrame;
    procedure UpdateCaption;
    procedure CloseAllProjectEditors;
    procedure DoEditProjectItem(const it: TIDEProjectItem);
    procedure DoDeleteProjectItem(const it: TIDEProjectItem);
    function FindSelectedProjectItem: TIDEProjectItem;
    function FindItemEditor(const pctrl: TPageControl; const obj: TIDEProjectItem): TFrame;
    procedure LogOutput(const c: TColor; const s: string);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

uses
  frm_scripteditor, ide_version, ide_utils, frm_message, ddc_base;

resourcestring
  rsTitle = 'DelphiDOOM Script IDE';

procedure TForm1.ActionNewProjectExecute(Sender: TObject);
begin
  NewProject;
end;

procedure TForm1.ActionAboutExecute(Sender: TObject);
begin
  About;
end;

procedure TForm1.ActionOpenProjectExecute(Sender: TObject);
begin
  OpenProject;
end;

procedure TForm1.ActionQuitExecute(Sender: TObject);
begin
  Quit;
end;

procedure TForm1.ActionRedoExecute(Sender: TObject);
begin
  Redo;
end;

procedure TForm1.ActionSaveAsExecute(Sender: TObject);
begin
  SaveAs;
end;

procedure TForm1.ActionSaveExecute(Sender: TObject);
begin
  Save;
end;

procedure TForm1.ActionUndoExecute(Sender: TObject);
begin
  Undo;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := CheckCanClose;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scaled := False;
  LibraryPanel.Width := 16;
  LibraryPageControl.ActivePageIndex := 0;
  MessagesListBox.Style := lbOwnerDrawFixed;
  outproc := LogOutput;
  project := nil;

  if ParamCount > 1 then
    DoOpenProject(ParamStr(1))
  else
    DoNewProject;
end;

procedure TForm1.CreateProjectObject(const fname: string);
begin
  if project <> nil then
    project.Free;

  project := TIDEProject.Create(fname);
  project.projectquerymodifieditem := CheckCanCloseProjectItem;
  project.projectonchangegame := ChangeGame;
  project.Game := 'doom';
  project.Editor := ProjectEditorFrame;
end;

procedure TForm1.DoNewProject;
var
  TabSheet: TTabSheet;
begin
  CloseAllProjectEditors;

  TabSheet := PageControl2.ActivePage;
  if TabSheet = nil then
    Exit;

  CreateProjectObject('');
  UpdateCaption;
end;

procedure TForm1.NewProject;
begin
  if not CheckCanClose then
    Exit;

  DoNewProject;
end;

procedure TForm1.DoOpenProject(const fname: string);
var
  TabSheet: TTabSheet;
begin
  CloseAllProjectEditors;

  TabSheet := PageControl2.ActivePage;
  if TabSheet = nil then
    Exit;

  CreateProjectObject(fname);
  UpdateCaption;
end;

procedure TForm1.OpenProject;
begin
  if not CheckCanClose then
    Exit;

  if OpenProjectDialog.Execute then
    DoOpenProject(OpenProjectDialog.FileName);
end;

procedure TForm1.DoSave(const fname: string);
begin
  if project = nil then
    Exit;

  Screen.Cursor := crHourGlass;
  try
    project.SaveToFile(fname);
    UpdateCaption;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.Save;
begin
  if project = nil then
    Exit;

  if project.FileName = '' then
  begin
    SaveAs;
    Exit;
  end;

  DoSave(project.FileName);
end;

procedure TForm1.SaveAs;
begin
  if project = nil then
    Exit;

  if SaveProjectDialog.Execute then
    DoSave(SaveProjectDialog.FileName);
end;

procedure TForm1.Undo;
begin
  if project = nil then
    Exit;

  if project.CanUndo then
    project.Undo;
end;

procedure TForm1.Redo;
begin
  if project = nil then
    Exit;

  if project.CanRedo then
    project.Redo;
end;

procedure TForm1.Quit;
begin
  Close;
end;

procedure TForm1.About;
begin
  IDEMessage(
    Format(
      '%s'#13#10'Version ' + I_VersionBuilt +
      #13#10#13#10'A tool for creating SCRIPTS for the DelphiDoom engine.'#13#10'© 2016 - 2021, jvalavanis@gmail.com',
      [rsTitle]));
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if project <> nil then
    if project.Modified then
    begin
      ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
      if ret = IDCANCEL then
      begin
        Result := false;
        Exit;
      end;
      if ret = IDNO then
      begin
        Result := True;
        Exit;
      end;
      if ret = IDYES then
      begin
        ActionSaveExecute(self);
        Result := not project.Modified;
        Exit;
      end;
    end;
  Result := True;
end;

function TForm1.CheckCanCloseProjectItem(const x: TIDEProjectItem): boolean;
var
  ret: integer;
begin
  if project <> nil then
  begin
    ret := MessageBox(Handle,
      PChar('Do you want to save changes to "' + x.RelativePath + '"?'),
      PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL then
    begin
      Result := false;
      Exit;
    end;
    if ret = IDNO then
    begin
      Result := True;
      Exit;
    end;
    if ret = IDYES then
    begin
      x.SaveToFile(x.Filename);
      Result := not x.Modified;
      Exit;
    end;
  end;
  Result := True;
end;

function TForm1.ActiveEditorFrame: TFrame;
var
  i: integer;
  iTab: TTabSheet;
begin
  iTab := PageControl1.ActivePage;
  if iTab = nil then
  begin
    Result := nil;
    Exit;
  end;

  for i := 0 to iTab.ComponentCount - 1 do
    if iTab.Components[i].InheritsFrom(TFrame) then
    begin
      Result := iTab.Components[i] as TFrame;
      Exit;
    end;

  Result := nil;
end;

function TForm1.ProjectEditorFrame: TFrame;
begin
  Result := Frame_ProjectManager1;
end;


procedure TForm1.ActionGoToLineNumberExecute(Sender: TObject);
var
  frm: TFrame;
begin
  frm := ActiveEditorFrame;
  if frm = nil then
    Exit;

  if frm is TFrame_ScriptEditor then
    (frm as TFrame_ScriptEditor).GoToLineNumber;
end;

procedure TForm1.UpdateCaption;
var
  TabSheet: TTabSheet;
begin
  TabSheet := PageControl2.ActivePage;
  if TabSheet = nil then
    Exit;

  if project = nil then
    Exit;

  TabSheet.Caption := DefItemName(project.ShortFileName);

  Caption := rsTitle + ' - ' + TabSheet.Caption;
end;

procedure TForm1.ApplicationEvents1Hint(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := Application.Hint;
end;

procedure TForm1.Frame_ProjectManager1AddActorButton1Click(
  Sender: TObject);
var
  i: integer;
begin
  if AddNewActorDialog1.Execute then
  begin
    for i := 0 to AddNewActorDialog1.Files.Count - 1 do
      project.Add(AddNewActorDialog1.Files[i], pi_actor);
    project.AdjustLayout(project.data);
  end;
end;

procedure TForm1.CloseAllProjectEditors;
var
  i: integer;
begin
  for i := PageControl1.PageCount - 1 downto 0 do
    PageControl1.Pages[i].Free;
end;

procedure TForm1.DoEditProjectItem(const it: TIDEProjectItem);
var
  frm: TFrame;
  TabSheet: TTabSheet;
begin
  if not (it.ItemType in [pi_script, pi_compiledscript, pi_actor, pi_project]) then
    Exit;

  frm := FindItemEditor(PageControl1, it);
  if frm <> nil then
  begin
    if frm.Parent is TTabSheet then
    begin
      PageControl1.ActivePage := (frm.Parent as TTabSheet);
      Exit;
    end;
  end;

  TabSheet := TTabSheet.Create(self);

  TabSheet.PageControl := PageControl1;
  TabSheet.Align := alClient;
  TabSheet.Caption := it.RelativePath;
  PageControl1.ActivePage := TabSheet;

  frm := TFrame_ScriptEditor.Create(TabSheet);
  frm.Parent := TabSheet;
  frm.Align := alClient;
  (frm as TFrame_ScriptEditor).obj := it;
  it.Edit(frm);
end;

procedure TForm1.DoDeleteProjectItem(const it: TIDEProjectItem);
var
  frm: TFrame;
begin
  frm := FindItemEditor(PageControl1, it);
  if frm <> nil then
    if frm.Parent is TTabSheet then
      (frm.Parent as TTabSheet).Free;

  if project <> nil then
    project.Delete(it);
end;

function TForm1.FindItemEditor(const pctrl: TPageControl; const obj: TIDEProjectItem): TFrame;
var
  TabSheet: TTabSheet;
  i, j: integer;
  frm: TFrame;
begin
  for i := pctrl.PageCount - 1 downto 0 do
  begin
    TabSheet := pctrl.Pages[i];
    for j := 0 to TabSheet.ComponentCount - 1 do
      if TabSheet.Components[j] is TFrame then
      begin
        frm := TabSheet.Components[j] as TFrame;
        if frm.Parent = TabSheet then
        begin
          if frm is TFrame_ScriptEditor then
            if (frm as TFrame_ScriptEditor).obj = obj then
            begin
              Result := frm;
              Exit;
            end;
        end;
      end;
  end;

  Result := nil;
end;

function TForm1.FindSelectedProjectItem: TIDEProjectItem;
var
  node: TTreeNode;
begin
  Result := nil;

  if project = nil then
    Exit;

  node := Frame_ProjectManager1.TreeView1.Selected;
  if node = nil then
    Exit;

  Result := project.Find(node.Text);
end;

procedure TForm1.Frame_ProjectManager1TreeView1DblClick(Sender: TObject);
var
  it: TIDEProjectItem;
begin
  it := FindSelectedProjectItem;
  if it = nil then
    Exit;

  DoEditProjectItem(it);
end;

procedure TForm1.Frame_ProjectManager1AddScriptButton1Click(
  Sender: TObject);
var
  i: integer;
begin
  if AddNewScriptDialog1.Execute then
  begin
    for i := 0 to AddNewScriptDialog1.Files.Count - 1 do
      project.Add(AddNewScriptDialog1.Files[i], pi_script);
    project.AdjustLayout(project.data);
  end;
end;

procedure TForm1.Frame_ProjectManager1AddCompiledScriptButton1Click(
  Sender: TObject);
var
  i: integer;
begin
  if AddNewCompiledScriptDialog1.Execute then
  begin
    for i := 0 to AddNewCompiledScriptDialog1.Files.Count - 1 do
      project.Add(AddNewCompiledScriptDialog1.Files[i], pi_compiledscript);
    project.AdjustLayout(project.data);
  end;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
var
  i: integer;
begin
  for i := PageControl1.PageCount - 1 downto 0 do
    if not PageControl1.Pages[i].Enabled then
      PageControl1.Pages[i].Free;
  Done := True;
end;

procedure TForm1.LogOutput(const c: TColor; const s: string);
var
  i: integer;
  l: TStringList;
begin
  l := TStringList.Create;
  try
    l.Text := s;

    while MessagesListBox.Items.Count >= 256 do
      MessagesListBox.Items.Delete(0);

    for i := 0 to l.Count - 1 do
      MessagesListBox.Items.AddObject(l.Strings[i], Pointer(c));

    MessagesListBox.ItemIndex := MessagesListBox.Items.Count - 1;
  finally
    l.Free;
  end;
end;

procedure TForm1.ActionFindExecute(Sender: TObject);
var
  frm: TFrame;
begin
  frm := ActiveEditorFrame;
  if frm = nil then
    Exit;

  if frm is TFrame_ScriptEditor then
    (frm as TFrame_ScriptEditor).ShowSearchDialog;
end;

procedure TForm1.ActionReplaceExecute(Sender: TObject);
var
  frm: TFrame;
begin
  frm := ActiveEditorFrame;
  if frm = nil then
    Exit;

  if frm is TFrame_ScriptEditor then
    (frm as TFrame_ScriptEditor).ShowReplaceDialog;
end;

procedure TForm1.ActionSearchAgainExecute(Sender: TObject);
var
  frm: TFrame;
begin
  frm := ActiveEditorFrame;
  if frm = nil then
    Exit;

  if frm is TFrame_ScriptEditor then
    (frm as TFrame_ScriptEditor).SearchAgain;
end;

procedure TForm1.Frame_ProjectManager1TreeView1Editing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  if project = nil then
  begin
    AllowEdit := False;
    Exit;
  end;

  if Node.Parent = nil then
  begin
    AllowEdit := False;
    Exit;
  end;

  AllowEdit := project.CanRename(Node.Text);
end;

procedure TForm1.Frame_ProjectManager1TreeView1Edited(Sender: TObject;
  Node: TTreeNode; var S: String);
var
  it: TIDEProjectItem;
  frm: TFrame;
begin
  if project = nil then
    Exit;

  if S = Node.Text then
    Exit;

  if Node.Parent = nil then
    Exit;

  it := project.Find(Node.Text);
  if it = nil then
    Exit;

  frm := FindItemEditor(PageControl1, it);

  if project.Rename(Node.Text, S) then
  begin
    if frm <> nil then
    begin
      if frm.Parent is TTabSheet then
      begin
        PageControl1.ActivePage := (frm.Parent as TTabSheet);
        PageControl1.ActivePage.Caption := it.RelativePath;
        Exit;
      end;
    end;
    S := it.RelativePath;
  end
  else
    S := Node.Text;
end;

procedure TForm1.MessagesListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  log_color: TColor;
begin
  with Control as TListBox do
  begin
    log_color := TColor(Items.Objects[Index]);
    if log_color = LOG_ERROR then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];

    Canvas.FillRect(Rect);
    Canvas.Font.Color := log_color;
    Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]);
  end;
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
var
  b: Boolean;
begin
  b := ActiveEditorFrame <> nil;
  ActionFind.Enabled := b;
  ActionGoToLineNumber.Enabled := b;
  ActionReplace.Enabled := b;
  ActionSearchAgain.Enabled := b;
end;

procedure TForm1.ActionProjectDeleteItemExecute(Sender: TObject);
var
  it: TIDEProjectItem;
begin
  if project = nil then
    Exit;

  it := FindSelectedProjectItem;
  if it = nil then
    Exit;

  DoDeleteProjectItem(it);

  project.AdjustLayout(project.data);
end;

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  GeneratePK31.Enabled := (project <> nil) and (project.Count > 0);
end;

procedure TForm1.GeneratePK31Click(Sender: TObject);
begin
  if project = nil then
    Exit;

  if project.Count = 0 then
    Exit;

  if GeneratePK3SaveDialog.Execute then
    project.GeneratePK3(GeneratePK3SaveDialog.FileName);
end;

procedure TForm1.Copy1Click(Sender: TObject);
var
  idx: integer;
begin
  idx := MessagesListBox.ItemIndex;
  if idx >= 0 then
    Clipboard.AsText := MessagesListBox.Items[idx];
end;

procedure TForm1.Clearmessages1Click(Sender: TObject);
begin
  MessagesListBox.Items.Clear;
end;

procedure TForm1.SaveMessages1Click(Sender: TObject);
begin
  if SaveMessagesDialog.Execute then
    MessagesListBox.Items.SaveToFile(SaveMessagesDialog.FileName);
end;

procedure TForm1.ChangeGame(const newgame: string);
begin
  Screen.Cursor := crHourGlass;
  try
    Frame_UnitFunctions1.UpdateGameControls(LowerCase(newgame));
    Frame_Constants1.UpdateGameControls(LowerCase(newgame));
    Frame_Variables1.UpdateGameControls(LowerCase(newgame));
    Frame_Classes1.UpdateGameControls(LowerCase(newgame));
    Frame_Types1.UpdateGameControls(LowerCase(newgame));
    Frame_Actordef1.UpdateGameControls(LowerCase(newgame));
    Frame_Mobjinfo1.UpdateGameControls(LowerCase(newgame));
    Frame_States1.UpdateGameControls(LowerCase(newgame));
    Frame_Sprites1.UpdateGameControls(LowerCase(newgame));
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.LibraryButton1Click(Sender: TObject);
var
  lWidth: integer;
begin
  if LibraryButton1.Down then
  begin
    if LibraryPanel.Width < 128 then
    begin
      if (Screen.WorkAreaWidth > 2000) and (ClientWidth > 1500) then
        lWidth := 768
      else if (Screen.WorkAreaWidth > 1500) and (ClientWidth > 1200) then
        lWidth := 512
      else if ClientWidth > 799 then
        lWidth := 296
      else
        lWidth := ClientWidth div 4;
      LibraryPanel.Width := lWidth;
    end;
  end
  else
    LibraryPanel.Width := 16;
end;

procedure TForm1.LibraryPanelResize(Sender: TObject);
begin
  LibraryButton1.Down := LibraryPanel.Width >= 128;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  CloseAllProjectEditors;

  if project <> nil then
    project.Free;
end;

procedure TForm1.PageControl1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   PageControl1.BeginDrag(False);
end;

procedure TForm1.PageControl1DragDrop(Sender, Source: TObject; X,
  Y: Integer);
const
  TCM_GETITEMRECT = $130A;
var
  i: Integer;
  r: TRect;
begin
  if Sender <> PageControl1 then
    Exit;
  for i := 0 to PageControl1.PageCount - 1 do
  begin
    PageControl1.Perform(TCM_GETITEMRECT, i, lParam(@r));
    if PtInRect(r, Point(X, Y)) then
    begin
      if i <> PageControl1.ActivePage.PageIndex then
        PageControl1.ActivePage.PageIndex := i;
      Exit;
    end;
  end;
end;

procedure TForm1.PageControl1DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Sender = PageControl1;
end;

procedure TForm1.Frame_ProjectManager1GameTypeComboBoxChange(
  Sender: TObject);
begin
  Frame_ProjectManager1.GameTypeComboBoxChange(Sender);
end;

end.

