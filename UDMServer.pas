unit UDMServer;

interface

uses
  System.Classes, DzSocket;

type
  TDMServer = class(TDataModule)
    S: TDzTCPServer;
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure DataModuleCreate(Sender: TObject);
    procedure SClientDisconnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
      var Accept: Boolean; const RequestData: string; var ResponseData: string);
    procedure SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
  private
    procedure ClientNumberPointed(Socket: TDzSocket; const A: String);
    function ClientToArray(Socket: TDzSocket): String;
    procedure ClientRefreshRequest(Socket: TDzSocket);
    procedure ClearEstimatives;
    procedure CheckForAllClientsEstimateDone;
    function ClientNameExists(const UserName: String): Boolean;
  public
    OpenedRound: Boolean;
    function GetClientsList: String;

     procedure OpenRound;
    procedure CloseRound;
  end;

var
  DMServer: TDMServer;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses UClient, UFrm, System.SysUtils, System.Math, Vars, Utils,
  UIntegerList;

procedure TDMServer.DataModuleCreate(Sender: TObject);
begin
  S.Port := INT_PORT;
  S.AutoFreeObjs := True;
  S.EnumeratorOnlyAuth := True;
end;

procedure TDMServer.SClientRead(Sender: TObject; Socket: TDzSocket;
  const Cmd: Char; const A: string);
begin
  case Cmd of
    'N': ClientNumberPointed(Socket, A);
    'R': ClientRefreshRequest(Socket);
  end;
end;

procedure TDMServer.SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
  var Accept: Boolean; const RequestData: string; var ResponseData: string);
var
  MA: TMsgArray;
  C: TClient;
begin
  MA := DataToArray(RequestData);

  if MA[0]<>STR_VERSION then
  begin
    Accept := False;
    ResponseData := 'V'+STR_VERSION;
    Exit;
  end;

  if ClientNameExists(MA[1]) then
  begin
    Accept := False;
    ResponseData := 'U';
    Exit;
  end;

  C := TClient.Create;
  C.User := MA[1];
  Socket.Data := C;
end;

procedure TDMServer.SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
begin
  //send clients list to the client
  S.Send(Socket, 'L', GetClientsList);
  //send to other clients that a client connected
  S.SendAllEx(Socket, 'C', ClientToArray(Socket));
end;

procedure TDMServer.SClientDisconnect(Sender: TObject; Socket: TDzSocket);
begin
  //client disconnected - send to others
  S.SendAllEx(Socket, 'D', Socket.ID.ToString);
end;

procedure TDMServer.SClientError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  Log(Format('SERVER ERROR: (Socket %d) %s', [Socket.ID, ErrorMsg]));
end;

function TDMServer.ClientToArray(Socket: TDzSocket): String;
var C: TClient;
begin
   C := Socket.Data;

   Result := ArrayToData([Socket.ID, C.User, C.Estimated,
     IfThen(OpenedRound, 0, C.Number)]);
end;

function TDMServer.GetClientsList: String;
var
  lst: TStringList;
  l_nums: TIntegerList;
  k: TDzSocket;
  C: TClient;
begin
  lst := TStringList.Create;
  l_nums := TIntegerList.Create;
  try
    S.Lock;
    try
      for k in S do
      begin
        lst.Add(ClientToArray(k));

        C := k.Data;
        if C.Estimated and (C.Number>0) then
          l_nums.Add(C.Number);
      end;
    finally
      S.Unlock;
    end;

    if not OpenedRound then l_nums.CalcStatistics;
    //general properties
    lst.Insert(0, ArrayToData([OpenedRound, l_nums.min, l_nums.max,
      l_nums.votes, Format('%d < %d', [l_nums.min, l_nums.max]),
      FormatFloat('0.0', l_nums.avg), FormatFloat('0.0', l_nums.median),
      FormatFloat('0 %', l_nums.proximity)+' '+Format('(%d/%d)', [l_nums.min, l_nums.max])]));

    Result := lst.Text;
  finally
    lst.Free;
    l_nums.Free;
  end;
end;

procedure TDMServer.ClientNumberPointed(Socket: TDzSocket; const A: String);
var C: TClient;
begin
  //a client made an estimate
  if not OpenedRound then Exit;

  C := Socket.Data;
  C.Estimated := True;
  C.Number := A.ToInteger;

  //send to all that a client made an estimate
  S.SendAll('P', Socket.ID.ToString);

  CheckForAllClientsEstimateDone;
end;

procedure TDMServer.ClientRefreshRequest(Socket: TDzSocket);
begin
  //client request a list refresh
  S.Send(Socket, 'L', GetClientsList);
end;

procedure TDMServer.ClearEstimatives;
var
  k: TDzSocket;
  C: TClient;
begin
  S.Lock;
  try
    for k in S do
    begin
      C := k.Data;
      C.Estimated := False;
      C.Number := 0;
    end;
  finally
    S.Unlock;
  end;
end;

procedure TDMServer.OpenRound;
begin
  ClearEstimatives;

  OpenedRound := True;
  S.SendAll('A', GetClientsList);

  Frm.SetRoundButtons(True);
end;

procedure TDMServer.CloseRound;
begin
  OpenedRound := False;
  S.SendAll('X', GetClientsList);

  Frm.SetRoundButtons(False);
end;

procedure TDMServer.CheckForAllClientsEstimateDone;
var
  k: TDzSocket;
  C: TClient;
begin
  S.Lock;
  try
    for k in S do
    begin
      C := k.Data;
      if not C.Estimated then Exit;
    end;
  finally
    S.Unlock;
  end;

  //when all clients have estimate done, then close the round
  CloseRound;
end;

function TDMServer.ClientNameExists(const UserName: String): Boolean;
var
  k: TDzSocket;
  C: TClient;
begin
  S.Lock;
  try
    for k in S do
    begin
      C := k.Data;
      if SameText(C.User, UserName) then Exit(True);
    end;
  finally
    S.Unlock;
  end;

  Exit(False);
end;

end.
