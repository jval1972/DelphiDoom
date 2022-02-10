unit frm_ReplaceText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, frm_SearchText;

type
  TTextReplaceDialog = class(TTextSearchDialog)
    Label2: TLabel;
    cbReplaceText: TComboBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetReplaceText: string;
    function GetReplaceTextHistory: string;
    procedure SetReplaceText(Value: string);
    procedure SetReplaceTextHistory(Value: string);
  public
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property ReplaceTextHistory: string read GetReplaceTextHistory
      write SetReplaceTextHistory;
  end;

implementation

{$R *.DFM}

{ TTextReplaceDialog }

//==============================================================================
//
// TTextReplaceDialog.GetReplaceText
//
//==============================================================================
function TTextReplaceDialog.GetReplaceText: string;
begin
  Result := cbReplaceText.Text;
end;

//==============================================================================
//
// TTextReplaceDialog.GetReplaceTextHistory
//
//==============================================================================
function TTextReplaceDialog.GetReplaceTextHistory: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to cbReplaceText.Items.Count - 1 do begin
    if i >= 10 then
      break;
    if i > 0 then
      Result := Result + #13#10;
    Result := Result + cbReplaceText.Items[i];
  end;
end;

//==============================================================================
//
// TTextReplaceDialog.SetReplaceText
//
//==============================================================================
procedure TTextReplaceDialog.SetReplaceText(Value: string);
begin
  cbReplaceText.Text := Value;
end;

//==============================================================================
//
// TTextReplaceDialog.SetReplaceTextHistory
//
//==============================================================================
procedure TTextReplaceDialog.SetReplaceTextHistory(Value: string);
begin
  cbReplaceText.Items.Text := Value;
end;

//==============================================================================
//
// TTextReplaceDialog.FormCloseQuery
//
//==============================================================================
procedure TTextReplaceDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  s: string;
  i: integer;
begin
  inherited;
  if ModalResult = mrOK then begin
    s := cbReplaceText.Text;
    if s <> '' then begin
      i := cbReplaceText.Items.IndexOf(s);
      if i > -1 then begin
        cbReplaceText.Items.Delete(i);
        cbReplaceText.Items.Insert(0, s);
        cbReplaceText.Text := s;
      end else
        cbReplaceText.Items.Insert(0, s);
    end;
  end;
end;

end.

