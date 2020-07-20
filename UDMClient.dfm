object DMClient: TDMClient
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 237
  Width = 372
  object C: TDzTCPClient
    KeepAlive = True
    OnLoginRequest = CLoginRequest
    OnLoginResponse = CLoginResponse
    OnConnect = CConnect
    OnDisconnect = CDisconnect
    OnRead = CRead
    OnError = CError
    OnConnectionLost = CConnectionLost
    Left = 204
    Top = 80
  end
end
