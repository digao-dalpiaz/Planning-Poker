unit Utils;

interface

procedure Log(const A: String);

procedure PlaySound;

implementation

uses UFrm, System.SysUtils,
 System.Classes, Winapi.Windows, Winapi.MMSystem;

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
