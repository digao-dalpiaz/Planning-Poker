unit USyncClient;

interface

uses System.SyncObjs;

var SyncClient: TCriticalSection;

implementation

initialization
  SyncClient := TCriticalSection.Create;

finalization
  SyncClient.Free;

end.
