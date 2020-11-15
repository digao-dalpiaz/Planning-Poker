unit UVersionCheck;

interface

procedure CheckMyVersion;

implementation

uses Vars, System.Net.HttpClient, System.JSON, Vcl.Dialogs,
  ULanguage, System.SysUtils, System.UITypes, System.Classes,
  Winapi.ShellAPI, Winapi.Windows;

const URL_GITHUB = 'https://api.github.com/repos/digao-dalpiaz/Planning-Poker/releases/latest';

type
  TThCheck = class(TThread)
  protected
    procedure Execute; override;
  private
    procedure Check;
  end;

procedure TThCheck.Check;
var
  H: THTTPClient;
  Res, tag_versao, tag_url: string;
  data: TJSONObject;
begin
  H := THTTPClient.Create;
  try
    Res := H.Get(URL_GITHUB).ContentAsString;
  finally
    H.Free;
  end;

  data := TJSONObject.ParseJSONValue(Res) as TJSONObject;
  try
    tag_versao := data.GetValue('tag_name').Value;
    tag_url := data.GetValue('html_url').Value;
  finally
    data.Free;
  end;

  if 'v'+STR_VERSION<>tag_versao then
  begin
    Synchronize(procedure
    begin
      if MessageDlg(Format(Lang.Get('NEW_VERSION_INFO'), [tag_versao]), mtInformation, mbYesNo, 0) = mrYes then
        ShellExecute(0, '', PChar(tag_url), '', '', SW_SHOWNORMAL);
    end);
  end;
end;

procedure TThCheck.Execute;
begin
  inherited;
  FreeOnTerminate := True;

  try
    Check;
  except
    on E: Exception do
      Synchronize(procedure
      begin
        MessageDlg('Error checking version updates: '+E.Message, mtError, [mbOK], 0);
      end);
  end;
end;

procedure CheckMyVersion;
begin
  TThCheck.Create;
end;

end.
