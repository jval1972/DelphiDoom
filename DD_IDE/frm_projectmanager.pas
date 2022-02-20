unit frm_projectmanager;

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls,
  ide_baseframe, ide_project, ImgList, Buttons, StdCtrls, Graphics;

type

  { TFrame_ProjectManager }

  TFrame_ProjectManager = class(TFrame)
    Panel7: TPanel;
    TreeView1: TTreeView;
    ImageList1: TImageList;
    AddActorButton1: TSpeedButton;
    AddScriptButton1: TSpeedButton;
    AddCompiledScriptButton1: TSpeedButton;
    DeleteItemSpeedButton1: TSpeedButton;
    GameTypeImageList: TImageList;
    GameTypeComboBox: TComboBox;
    procedure GameTypeComboBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure GameTypeComboBoxChange(Sender: TObject);
  protected
    tscripts, tactors, tcompiledscripts: TTreeNode;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { public declarations }
    obj: TBaseEditorObject;
    destructor Destroy; override;
    procedure AdjustLayout(const adata: AnsiString);
  end;

implementation

{$R *.dfm}

//==============================================================================
//
// TFrame_ProjectManager.AdjustLayout
//
//==============================================================================
procedure TFrame_ProjectManager.AdjustLayout(const adata: AnsiString);
var
  i: integer;
  prj: TIDEProject;
  item: TIDEProjectItem;
  tscript, tactor, tcompiledscript: TTreeNode;
begin
  TreeView1.Items.BeginUpdate;
  TreeView1.Items.Clear;
  tactors := TreeView1.Items.AddFirst(nil, 'Actors');
  tscripts := TreeView1.Items.AddChild(nil, 'Scripts');
  tcompiledscripts := TreeView1.Items.AddChild(nil, 'Compiled Scripts');
  tactors.ImageIndex := 6;
  tactors.SelectedIndex := 7; //tactors.ImageIndex;
  tscripts.ImageIndex := 8;
  tscripts.SelectedIndex := 9; //tscripts.ImageIndex;
  tcompiledscripts.ImageIndex := 10;
  tcompiledscripts.SelectedIndex := 11; //tcompiledscripts.ImageIndex;

  if obj <> nil then
  begin
    if obj is TIDEProject then
    begin
      prj := obj as TIDEProject;
      GameTypeComboBox.ItemIndex := 0;
      if prj.Game = 'heretic' then
        GameTypeComboBox.ItemIndex := 1
      else if prj.Game = 'hexen' then
        GameTypeComboBox.ItemIndex := 2
      else if prj.Game = 'strife' then
        GameTypeComboBox.ItemIndex := 3;
      for i := 0 to prj.Count - 1 do
      begin
        item := prj.Items[i];
        if item <> nil then
          case item.ItemType of
            pi_actor:
              begin
                tactor := TreeView1.Items.AddChild(tactors, item.RelativePath);
                tactor.ImageIndex := 0;
                tactor.SelectedIndex := 1;
              end;
            pi_script:
              begin
                tscript := TreeView1.Items.AddChild(tscripts, item.RelativePath);
                tscript.ImageIndex := 2;
                tscript.SelectedIndex := 3;
              end;
            pi_compiledscript:
              begin
                tcompiledscript := TreeView1.Items.AddChild(tcompiledscripts, item.RelativePath);
                tcompiledscript.ImageIndex := 4;
                tcompiledscript.SelectedIndex := 5;
              end;
          end;
      end;
    end;
  end;

  TreeView1.Items.EndUpdate;
end;

//==============================================================================
//
// TFrame_ProjectManager.CreateParams
//
//==============================================================================
procedure TFrame_ProjectManager.CreateParams(var Params: TCreateParams);
begin
  obj := nil;
  tscripts := nil;
  tactors := nil;
  tcompiledscripts := nil;
  Inherited;
end;

//==============================================================================
//
// TFrame_ProjectManager.Destroy
//
//==============================================================================
destructor TFrame_ProjectManager.Destroy;
begin
  if obj <> nil then
    obj.Editor := nil;
  inherited;
end;

//==============================================================================
//
// TFrame_ProjectManager.GameTypeComboBoxDrawItem
//
//==============================================================================
procedure TFrame_ProjectManager.GameTypeComboBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ComboBox: TComboBox;
  bitmap: TBitmap;
  s: string;
begin
  ComboBox := (Control as TComboBox);
  Bitmap := TBitmap.Create;
  try
    GameTypeImageList.GetBitmap(Index, Bitmap);
    with ComboBox.Canvas do
    begin
      FillRect(Rect);
      if Bitmap.Handle <> 0 then
        Draw(Rect.Left + 2, Rect.Top, Bitmap);
      Rect := Bounds(
        Rect.Left + ComboBox.ItemHeight + 2,
        Rect.Top,
        Rect.Right - Rect.Left,
        Rect.Bottom - Rect.Top
      );
      s := '  ' + ComboBox.Items[Index];
      DrawText(
        handle,
        PChar(s),
        Length(s),
        Rect,
        DT_VCENTER + DT_SINGLELINE
      );
    end;
  finally
    Bitmap.Free;
  end;
end;

//==============================================================================
//
// TFrame_ProjectManager.GameTypeComboBoxChange
//
//==============================================================================
procedure TFrame_ProjectManager.GameTypeComboBoxChange(Sender: TObject);
begin
  if obj <> nil then
    if obj is TIDEProject then
      if GameTypeComboBox.ItemIndex >= 0 then
        (obj as TIDEProject).Game := GameTypeComboBox.Items.Strings[GameTypeComboBox.ItemIndex];
end;

end.

