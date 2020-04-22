unit UIntegerList;

interface

uses System.Generics.Collections;

type
  TIntegerList = class(TList<Integer>)
    max, min: Integer;
    avg, median, proximity: Extended;

    procedure CalcStatistics;
  end;

implementation

uses System.Generics.Defaults;

procedure TIntegerList.CalcStatistics;
var I, sum: Integer;
begin
  max := 0;
  min := 0;
  avg := 0;
  median := 0;
  proximity := 0;

  sum := 0;

  Sort(TComparer<Integer>.Construct(
    function(const Left, Right: Integer): Integer
    begin
      Result := Left-Right;
    end)
  );

  for I in Self do
  begin
    if I>max then max := I;

    if (min=0) or (I<min) then min := I;

    Inc(sum, I);
  end;

  if Count>0 then
  begin
    avg := sum / Count;

    if Odd(Count) then
      median := Items[((Count+1) div 2)-1] //central value
    else
      median := ( Items[(Count div 2)-1] + Items[Count div 2] ) / 2; //avg of two central values

    proximity := min / max * 100;
  end;
end;

end.
