unit UDMClient;

interface

uses
  System.Classes, DzSocket;

type
  TDMClient = class(TDataModule)
    C: TDzTCPClient;
    procedure CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure CConnect(Sender: TObject; Socket: TDzSocket);
    procedure CDisconnect(Sender: TObject; Socket: TDzSocket;
      const WasConnected: Boolean);
    procedure CConnectionLost(Sender: TObject; Socket: TDzSocket);
    procedure CError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure ReceivedAccepted(const A: String);
    procedure OtherClientConnected(const A: String);
    procedure OtherClientDisconnected(const A: String);
    procedure ReceivedListOfClients(const A: String);
    procedure ReceivedOpenRound(const A: String);
    procedure ClientPointed(const A: String);
    procedure ReceivedCloseRound(const A: String);
    procedure ReceivedWrongVersion;
  end;

var
  DMClient: TDMClient;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses Vars, UFrm, UItem, System.SysUtils, Utils, ULanguage;

procedure TDMClient.DataModuleCreate(Sender: TObject);
begin
  C.Port := INT_PORT;
end;

procedure TDMClient.CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
  const A: string);
begin
  case Cmd of
    'E': ReceivedAccepted(A);
    'C': OtherClientConnected(A);
    'D': OtherClientDisconnected(A);
    'L': ReceivedListOfClients(A);
    'A': ReceivedOpenRound(A);
    'P': ClientPointed(A);
    'X': ReceivedCloseRound(A);
    'V': ReceivedWrongVersion;
  end;
end;

procedure TDMClient.CConnect(Sender: TObject; Socket: TDzSocket);
begin
  //send version and user name to the server
  C.Send('C', ArrayToMsg([STR_VERSION, pubUser]));
end;

procedure TDMClient.CDisconnect(Sender: TObject; Socket: TDzSocket;
  const WasConnected: Boolean);
begin
  if WasConnected then
    Log(Lang.Get('LOG_DISCONNECTED'));

  Frm.ClientDisconnected;
end;

procedure TDMClient.CConnectionLost(Sender: TObject; Socket: TDzSocket);
begin
  Log(Lang.Get('LOG_CONNECTION_LOST'));
end;

procedure TDMClient.CError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  Log(Format(Lang.Get('LOG_SOCKET_ERROR'), [ErrorMsg]));
end;

procedure TDMClient.ReceivedAccepted(const A: String);
begin
  //server accepted client connection
  Log(Lang.Get('LOG_CONNECTED'));
  Frm.ClientConnected;
  Frm.FillClientsList(A);
end;

procedure TDMClient.OtherClientConnected(const A: String);
begin
  //a client connected - add into list
  Frm.AddItem(A);
  Frm.AtCountConnections;
end;

procedure TDMClient.OtherClientDisconnected(const A: String);
var Idx: Integer;
begin
  //a client disconnected - remove from list
  Idx := FindItemIndexByID(A.ToInteger);
  if Idx<>-1 then
  begin
    Frm.L.Items.Objects[Idx].Free;
    Frm.L.Items.Delete(Idx);

    Frm.AtCountConnections;
  end;
end;

procedure TDMClient.ClientPointed(const A: String);
var
  Idx: Integer;
  Item: TItem;
begin
  //a client made an estimate
  Idx := FindItemIndexByID(A.ToInteger);
  if Idx<>-1 then
  begin
    Item := TItem(Frm.L.Items.Objects[Idx]);
    Item.Estimated := True;
    Frm.L.Invalidate;
  end;
end;

procedure TDMClient.ReceivedOpenRound(const A: String);
begin
  Log(Lang.Get('LOG_ROUND_OPENED'));
  //server opened estimate round
  Frm.FillClientsList(A);

  PlaySound;
end;

procedure TDMClient.ReceivedCloseRound(const A: String);
begin
  Log(Lang.Get('LOG_ROUND_CLOSED'));
  //server closed estimate round
  Frm.FillClientsList(A);
end;

procedure TDMClient.ReceivedListOfClients(const A: String);
begin
  Log(Lang.Get('LOG_LIST_RECEIVED'));
  //clients list received
  Frm.FillClientsList(A);
end;

procedure TDMClient.ReceivedWrongVersion;
begin
  Log(Lang.Get('LOG_WRONG_VERSION'));
end;

end.
