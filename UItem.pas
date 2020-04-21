unit UItem;

interface

//Item class for screen listbox

type TItem = class
  ID: Integer;
  User: String;

  Estimated: Boolean;
  Number: Integer;
end;

function FindItemIndexByID(ID: Integer): Integer;

implementation

uses UFrm;

function FindItemIndexByID(ID: Integer): Integer;
var
  I: Integer;
  Item: TItem;
begin
  Result := -1;

  for I := Frm.L.Count-1 downto 0 do
  begin
    Item := TItem(Frm.L.Items.Objects[I]);
    if Item.ID = ID then
    begin
      Result := I;
      Break;
    end;
  end;
end;

end.
