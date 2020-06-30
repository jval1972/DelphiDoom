unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Run1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure ClearListView;
    procedure FillListView;
    procedure RunCmd(const cmd: string);
    function GetCmd: string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  MMSystem;

type
  TCardinal = class(TObject)
  public
    value: Cardinal;
    constructor Create(const aval: Cardinal);
  end;

constructor TCardinal.Create(const aval: Cardinal);
begin
  value := aval;
end;

procedure TForm1.ClearListView;
var
  i: integer;
begin
  for i := 0 to ListBox1.Items.Count - 1 do
    ListBox1.Items.Objects[i].Free;
  ListBox1.Items.Clear;
end;

procedure TForm1.FillListView;
var
  i, j: integer;
  numdev: integer;
  midicaps: MIDIOUTCAPS;
  devname: string;
begin
  ClearListView;

  numdev := midiOutGetNumDevs;

  for i := -1 to numdev - 1 do
  begin
    if midiOutGetDevCaps(i, @midicaps, SizeOf(midicaps)) = MMSYSERR_NOERROR then
      if midicaps.dwSupport and MIDICAPS_STREAM <> 0 then
      begin
        devname := '';
        for j := 0 to MAXPNAMELEN - 1 do
        begin
          if midicaps.szPname[j] = #0 then
            break
          else
            devname := devname + midicaps.szPname[j];
        end;
        if devname <> '' then
          ListBox1.Items.AddObject(devname, TCardinal.Create(i));
      end;
  end;

  if ListBox1.Items.Count > 0 then
    ListBox1.ItemIndex := 0;
end;

procedure TForm1.RunCmd(const cmd: string);
var
  weret: integer;
  errmsg: string;
begin
  weret := WinExec(PChar(cmd), SW_SHOWNORMAL);
  if weret > 31 then
//    Close
  else
  begin
    errmsg := Format('Can not execute DelphiDoom, error code = %d', [weret]);
    ShowMessage(errmsg);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FillListView;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FillListView;
end;

function TForm1.GetCmd: string;
var
  valtmp: int64;
begin
  if (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
  begin
    valtmp := (ListBox1.Items.Objects[ListBox1.ItemIndex] as TCardinal).value;
    result := 'doom32.exe -mididevice ' + IntToStr(valtmp);
    exit;
  end;

  result := '';
end;

procedure TForm1.Run1Click(Sender: TObject);
var
  cmd: string;
begin
  if (ListBox1.ItemIndex >= 0) and (ListBox1.ItemIndex < ListBox1.Items.Count) then
  begin
    cmd := GetCmd;
    if cmd <> '' then
      RunCmd(cmd);
  end
  else
    ShowMessage('Please select midi device');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  cmd: string;
begin
  cmd := GetCmd;
  if Edit1.Text <> cmd then
    Edit1.Text := cmd;
end;

end.

