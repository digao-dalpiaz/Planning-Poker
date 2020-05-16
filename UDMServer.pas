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
  private
    procedure ClientConnect(Socket: TDzSocket; const A: String);
    procedure ClientNumberPointed(Socket: TDzSocket; const A: String);
    function ClientToArray(Socket: TDzSocket): String;
    procedure ClientRefreshRequest(Socket: TDzSocket);
    procedure ClearEstimatives;
    procedure CheckForAllClientsEstimateDone;
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
  S.SendAllOnlyWithData := True;
end;

function InvalidSocket(Socket: TDzSocket): Boolean;
begin
  Result := (Socket.Data=nil);
end;

procedure TDMServer.SClientRead(Sender: TObject; Socket: TDzSocket;
  const Cmd: Char; const A: string);
begin
  case Cmd of
    'C': ClientConnect(Socket, A);
    'N': ClientNumberPointed(Socket, A);
    'R': ClientRefreshRequest(Socket);
  end;
end;

procedure TDMServer.ClientConnect(Socket: TDzSocket; const A: String);
var
  Data: TMsgArray;
  C: TClient;
begin
  Data := MsgToArray(A);

  if Data[0]<>STR_VERSION then
  begin
    Socket.Send('V'); //send to client your version is not supported
    Socket.Close; //drop client
    Exit;
  end;

  C := TClient.Create;
  C.User := Data[1];
  Socket.Data := C;

  //send accepted with clients list to the client
  S.Send(Socket, 'E', GetClientsList);
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

   Result := ArrayToMsg([Socket.ID, C.User, C.Estimated,
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
        if InvalidSocket(k) then Continue;

        lst.Add(ClientToArray(k));

        C := k.Data;
        if C.Estimated and (C.Number>0) then
          l_nums.Add(C.Number);
      end;
    finally
      S.Unlock;
    end;

    l_nums.CalcStatistics;
    //general properties
    lst.Insert(0, ArrayToMsg([OpenedRound,
      l_nums.Count, Format('%d < %d', [l_nums.min, l_nums.max]),
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
  if InvalidSocket(Socket) then Exit;

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
  if InvalidSocket(Socket) then Exit;

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
      if InvalidSocket(k) then Continue;

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
      if InvalidSocket(k) then Continue;

      C := k.Data;
      if not C.Estimated then Exit;
    end;
  finally
    S.Unlock;
  end;

  //when all clients have estimate done, then close the round
  CloseRound;
end;

end.
