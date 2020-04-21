unit Utils;

interface

type TMsgArray = TArray<Variant>;
function MsgToArray(const A: String): TMsgArray;
function ArrayToMsg(const Data: TMsgArray): String;

procedure Log(const A: String);

procedure PlaySound;

implementation

uses UFrm, System.SysUtils,
 System.Classes, Winapi.Windows, Winapi.MMSystem;

const MSG_SEPARATOR = #9;

function MsgToArray(const A: String): TMsgArray;
var
  StrArray: TArray<String>;
  I: Integer;
begin
  StrArray := A.Split([MSG_SEPARATOR]);

  SetLength(Result, Length(StrArray));
  for I := Low(StrArray) to High(StrArray) do
    Result[I] := StrArray[I];
end;

function ArrayToMsg(const Data: TMsgArray): String;
var A: String;
begin
  Result := EmptyStr;

  for A in Data do
    Result := Result + A + MSG_SEPARATOR;
end;

procedure Log(const A: String);
begin
  Frm.EdLog.Lines.Add(DateTimeToStr(Now) + ' - ' + A);
end;

procedure PlaySound;
var R: TResourceStream;
begin
  R := TResourceStream.Create(Hinstance, 'BEEP', RT_RCDATA);
  try
    sndPlaySound(R.Memory, SND_MEMORY or SND_ASYNC);
  finally
    R.Free;
  end;
end;

end.
