unit i_windowframe;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TWindowFrameForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure I_CreateWindowFrame;

procedure I_DestroyWindowFrame;

function I_WindowFrameParent: HWND;

procedure I_WindowFrameShow(const w, h: Integer; out x, y: Integer);

procedure I_WindowFrameHide;

implementation

{$R *.dfm}

uses
  d_main;

var
  WindowFrameForm: TWindowFrameForm;

procedure I_CreateWindowFrame;
begin
  WindowFrameForm := TWindowFrameForm.Create(nil);
end;

procedure I_DestroyWindowFrame;
begin
  WindowFrameForm.Free;
end;

function I_WindowFrameParent: HWND;
begin
  result := WindowFrameForm.Handle;
end;

procedure I_WindowFrameShow(const w, h: Integer; out x, y: Integer);
var
  p: TPoint;
begin
  WindowFrameForm.Visible := True;
  WindowFrameForm.ClientWidth := w;
  WindowFrameForm.ClientHeight := h;
  p.X := 0;
  p.Y := 0;
  ClientToScreen(WindowFrameForm.Handle, p);
  x := p.X;
  y := p.Y;
end;

procedure I_WindowFrameHide;
begin
  WindowFrameForm.Visible := False;
end;

procedure TWindowFrameForm.FormCreate(Sender: TObject);
begin
  Caption := D_Version + ' - ' + D_VersionBuilt;
  DoubleBuffered := True;
end;

end.
