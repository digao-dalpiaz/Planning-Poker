object DMServer: TDMServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Height = 213
  Width = 334
  object S: TDzTCPServer
    OnClientLoginCheck = SClientLoginCheck
    OnClientLoginSuccess = SClientLoginSuccess
    OnClientDisconnect = SClientDisconnect
    OnClientRead = SClientRead
    OnClientError = SClientError
    KeepAlive = True
    Left = 111
    Top = 48
  end
end
