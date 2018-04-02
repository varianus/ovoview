unit uInfo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit;

type

  { TfrmInfo }

  TfrmInfo = class(TForm)
    ValueListEditor1: TValueListEditor;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private

  public

  end;

var
  frmInfo: TfrmInfo;

implementation

{$R *.lfm}

{ TfrmInfo }

procedure TfrmInfo.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

end.

