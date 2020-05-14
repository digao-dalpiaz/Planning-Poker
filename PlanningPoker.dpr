program PlanningPoker;

{$R *.dres}

uses
  Vcl.Forms,
  UFrm in 'UFrm.pas' {Frm},
  UClient in 'UClient.pas',
  UItem in 'UItem.pas',
  Vars in 'Vars.pas',
  UDMClient in 'UDMClient.pas' {DMClient: TDataModule},
  UDMServer in 'UDMServer.pas' {DMServer: TDataModule},
  Utils in 'Utils.pas',
  UIntegerList in 'UIntegerList.pas',
  ULanguage in 'ULanguage.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrm, Frm);
  Application.CreateForm(TDMClient, DMClient);
  Application.CreateForm(TDMServer, DMServer);
  Application.Run;
end.
