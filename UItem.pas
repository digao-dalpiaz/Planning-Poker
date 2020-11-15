unit UItem;

interface

//Item class for screen listbox

type TItem = class
  ID: Integer;
  User: string;

  Estimated: Boolean;
  Number: Integer;
end;

function GetItemByIndex(Index: Integer): TItem;
function FindItemIndexByID(ID: Integer): Integer;

implementation

uses UFrm;

function GetItemByIndex(Index: Integer): TItem;
begin
  Result := TItem(Frm.L.Items.Objects[Index]);
end;

function FindItemIndexByID(ID: Integer): Integer;
var
  I: Integer;
begin
  for I := 0 to Frm.L.Count-1 do
    if GetItemByIndex(I).ID = ID then Exit(I);

  Exit(-1);
end;

end.
