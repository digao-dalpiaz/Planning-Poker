unit ULanguage;

interface

uses System.IniFiles;

type
  TLanguageName = (langEnglish, langPortugueseBrazil);

  TLang = class
  private
    Section: string;
    Ini: TMemIniFile;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetLanguage(aLanguageName: TLanguageName);

    function Get(const Ident: string): string;
  end;

var Lang: TLang;

implementation

uses System.Classes, System.SysUtils, Winapi.Windows;

constructor TLang.Create;
var
  R: TResourceStream;
  S: TStringStream;
  Lst: TStringList;
begin
  inherited;
  Ini := TMemIniFile.Create('');

  Lst := TStringList.Create;
  try
    S := TStringStream.Create('', TEncoding.UTF8);
    try
      R := TResourceStream.Create(HInstance, 'LANG', RT_RCDATA);
      try
        S.LoadFromStream(R);
      finally
        R.Free;
      end;

      Lst.Text := S.DataString;
    finally
      S.Free;
    end;

    Ini.SetStrings(Lst);
  finally
    Lst.Free;
  end;
end;

destructor TLang.Destroy;
begin
  Ini.Free;
  inherited;
end;

procedure TLang.SetLanguage(aLanguageName: TLanguageName);
begin
  case aLanguageName of
    langEnglish: Section := 'English';
    langPortugueseBrazil: Section := 'Portuguese-Brazil';
  end;
end;

function TLang.Get(const Ident: string): string;
begin
  Result := Ini.ReadString(Section, Ident, '');

  if Result='' then
    raise Exception.CreateFmt('Cannot retrieve language ident "%s"', [Ident]);
end;

initialization
  Lang := TLang.Create;

finalization
  Lang.Free;

end.
