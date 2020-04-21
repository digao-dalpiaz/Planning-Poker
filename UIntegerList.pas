unit UIntegerList;

interface

uses System.Generics.Collections;

type
  TIntegerList = class(TList<Integer>)
    max, min: Integer;
    avg: Extended;

    procedure CalcStatistics;
  end;

implementation

procedure TIntegerList.CalcStatistics;
var I, sum: Integer;
begin
  max := 0;
  min := 0;
  avg := 0;

  sum := 0;

  for I in Self do
  begin
    if I>max then max := I;

    if (min=0) or (I<min) then min := I;

    Inc(sum, I);
  end;

  if Count>0 then
    avg := sum / Count;
end;

end.
