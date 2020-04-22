unit UFrm;

interface

uses Vcl.Forms, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Buttons, Vcl.ExtCtrls, System.Classes,
  //
  System.Types;

type
  TFrm = class(TForm)
    Pages: TPageControl;
    TabStart: TTabSheet;
    TabPoker: TTabSheet;
    LbWelcome: TLabel;
    L: TListBox;
    LbVersion: TLabel;
    LbParticipants: TLabel;
    EdLog: TMemo;
    DividerBar: TBevel;
    BoxEstimate: TGroupBox;
    LbYourEstimate: TLabel;
    BtnSend: TBitBtn;
    BoxCmdServer: TGroupBox;
    BtnOpenRound: TBitBtn;
    BtnCloseRound: TBitBtn;
    Stats: TListView;
    LbStatistics: TLabel;
    BtnRefresh: TSpeedButton;
    LbLabelConnections: TLabel;
    LbCountConnections: TLabel;
    LbLabelUser: TLabel;
    LbUser: TLabel;
    BoxConnection: TPanel;
    EdUser: TEdit;
    BtnStart: TBitBtn;
    BoxCfgClient: TPanel;
    LbServerAddress: TLabel;
    EdHost: TEdit;
    BoxMode: TRadioGroup;
    LbUserName: TLabel;
    LbGitHub: TLinkLabel;
    BtnSpin: TUpDown;
    EdNumber: TEdit;
    IL: TImageList;
    LbHelpPort: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BoxModeClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure BtnOpenRoundClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnCloseRoundClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LbGitHubLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    procedure SetRoundButtons(hab: Boolean);
    procedure SetBoxEstimate(hab: Boolean);
    procedure FillStatistics(const votes, max, min, avg, median: String);
    procedure ResetAllControls;
    procedure ClearAllClients;
    procedure LoadRegistry;
    procedure SaveRegistry;
    procedure SetConnectionControlsEnabled(hab: Boolean);
  public
    procedure ClientConnected;
    procedure ClientDisconnected;
    procedure FillClientsList(const A: String);
    procedure AddItem(const A: String);
    procedure AtCountConnections;
  end;

var
  Frm: TFrm;

implementation

{$R *.dfm}

uses Vars, UItem, UDMServer, UDMClient, Utils,
  System.SysUtils, Vcl.Graphics, Winapi.Windows, Vcl.Dialogs, System.UITypes,
  Winapi.ShellAPI, System.Win.Registry;

procedure TFrm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  //--Hide PageControl Tabs
  TabStart.TabVisible := False;
  TabPoker.TabVisible := False;
  //--
  Pages.ActivePage := TabStart;

  LbVersion.Caption := 'Version '+STR_VERSION;
  LbHelpPort.Caption := Format('TCP Port %d', [INT_PORT]);

  LoadRegistry;
end;

procedure TFrm.FormDestroy(Sender: TObject);
begin
  ClearAllClients;

  SaveRegistry;
end;

procedure TFrm.LoadRegistry;
var R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if not R.OpenKey('SOFTWARE\PlanningPoker', True) then
      raise Exception.Create('Error opening registry key');

    EdUser.Text := R.ReadString('User');
    EdHost.Text := R.ReadString('Host');
  finally
    R.Free;
  end;
end;

procedure TFrm.SaveRegistry;
var R: TRegistry;
begin
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if not R.OpenKey('SOFTWARE\PlanningPoker', True) then
      raise Exception.Create('Error opening registry key');

    R.WriteString('User', EdUser.Text);
    R.WriteString('Host', EdHost.Text);
  finally
    R.Free;
  end;
end;

procedure TFrm.LbGitHubLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, '', PChar(Link), '', '', 0);
end;

procedure TFrm.BoxModeClick(Sender: TObject);
begin
  BoxCfgClient.Visible := (BoxMode.ItemIndex=0);
end;

procedure TFrm.ClearAllClients;
var I: Integer;
begin
  for I := 0 to L.Count-1 do
    L.Items.Objects[I].Free;

  L.Clear;
end;

procedure TFrm.ResetAllControls;
begin
  ClearAllClients;
  Stats.Clear;
  BoxEstimate.Visible := False;
  LbCountConnections.Caption := String.Empty;
  LbUser.Caption := pubUser;

  DMServer.OpenedRound := False;
  BoxCmdServer.Visible := pubServerMode;
  SetRoundButtons(False);
end;

procedure TFrm.SetConnectionControlsEnabled(hab: Boolean);
begin
  EdUser.Enabled := hab;
  EdHost.Enabled := hab;
  BoxMode.Enabled := hab;
  BtnStart.Enabled := hab;
end;

procedure TFrm.BtnStartClick(Sender: TObject);
begin
  EdUser.Text := Trim(EdUser.Text);
  if EdUser.Text=String.Empty then
  begin
    MessageDlg('Please, type your name.', mtError, [mbOK], 0);
    EdUser.SetFocus;
    Exit;
  end;

  if BoxCfgClient.Visible then
  begin
    EdHost.Text := Trim(EdHost.Text);
    if EdHost.Text=String.Empty then
    begin
      MessageDlg('Plase, type the server address.', mtError, [mbOK], 0);
      EdHost.SetFocus;
      Exit;
    end;
  end;

  //

  pubUser := EdUser.Text;
  pubServerMode := (BoxMode.ItemIndex=1);

  ResetAllControls;

  if pubServerMode then
  begin
    DMServer.S.Open;

    DMClient.C.Host := 'localhost';
  end else
  begin
    DMClient.C.Host := EdHost.Text;
  end;

  SetConnectionControlsEnabled(False);
  Log('Connecting...');
  DMClient.C.Connect;
end;

procedure TFrm.ClientConnected;
begin
  Pages.ActivePage := TabPoker;
end;

procedure TFrm.ClientDisconnected;
begin
  SetConnectionControlsEnabled(True);
  Pages.ActivePage := TabStart;

  //ensure stop server when back to start page
  if pubServerMode then
    DMServer.S.Close;
end;

procedure TFrm.LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  Item: TItem;
  Res: String;
  Cor: TColor;
begin
  L.Canvas.Font.Color := clBlack;
  if odSelected in State  then L.Canvas.Brush.Color := clYellow;
  L.Canvas.FillRect(Rect);

  IL.Draw(L.Canvas, Rect.Left+3, Rect.Top+2, 0);

  Item := TItem(L.Items.Objects[Index]);

  L.Canvas.TextOut(Rect.Left+30, Rect.Top+4, Item.User);

  Cor := clBlack;
  if BoxEstimate.Visible then
  begin
    if Item.Estimated then
    begin
      Res := 'Estimated!';
      Cor := clGreen;
    end else
    begin
      Res := 'Thinking...';
      Cor := clGray;
    end;
  end else
  begin
    if Item.Estimated then
    begin
      if Item.Number<>0 then
      begin
        Res := 'Value: '+Item.Number.ToString;
        Cor := clBlack;
      end else
      begin
        Res := 'Abstained';
        Cor := clRed;
      end;
    end;
    //else
    //  Res := 'Not estimated';
  end;
  L.Canvas.Font.Color := Cor;
  L.Canvas.TextOut(Rect.Left+300, Rect.Top+4, Res);
end;

procedure TFrm.AddItem(const A: String);
var
  Data: TMsgArray;
  Item: TItem;
begin
  Data := MsgToArray(A);

  Item := TItem.Create;
  Item.ID := Data[0];
  Item.User := Data[1];
  Item.Estimated := Data[2];
  Item.Number := Data[3];
  L.Items.AddObject('', Item);
end;

procedure TFrm.SetRoundButtons(hab: Boolean);
begin
  BtnOpenRound.Enabled := not hab;
  BtnCloseRound.Enabled := hab;
end;

procedure TFrm.BtnOpenRoundClick(Sender: TObject);
begin
  DMServer.OpenRound;
  SetRoundButtons(True);
end;

procedure TFrm.BtnCloseRoundClick(Sender: TObject);
begin
  DMServer.CloseRound;
  SetRoundButtons(False);
end;

procedure TFrm.BtnSendClick(Sender: TObject);
var Val: Integer;
begin
   Val := StrToIntDef(EdNumber.Text, -1);

   if Val<0 then
   begin
     MessageDlg('Invalid value', mtError, [mbOK], 0);
     EdNumber.SetFocus;
     Exit;
   end;

  //send estimate to server
  DMClient.C.Send('N', Val.ToString);

  Log('You have sent an estimate with value: '+Val.ToString);
end;

procedure TFrm.BtnRefreshClick(Sender: TObject);
begin
  //request a list refresh
  DMClient.C.Send('R');
end;

procedure TFrm.SetBoxEstimate(hab: Boolean);
begin
  BoxEstimate.Visible := hab;
  if hab then EdNumber.Clear;
end;

procedure TFrm.FillClientsList(const A: String);
var
  Data: TMsgArray;
  lst: TStringList;
  props: String;
  TopIdx, IDSel, Idx: Integer;
  data_client: String;
begin
  lst := TStringList.Create;
  try
    lst.Text := A;

    props := lst[0]; //first line contains general properties
    Data := MsgToArray(props);
    SetBoxEstimate(Data[0]);
    FillStatistics(Data[1], Data[2], Data[3], Data[4], Data[5]);
    lst.Delete(0);
    //**keep box loading before list because drawing depends on this

    L.Items.BeginUpdate;
    try
      TopIdx := L.TopIndex;
      IDSel := -1;
      if L.ItemIndex<>-1 then
        IDSel := TItem(L.Items.Objects[L.ItemIndex]).ID;

      ClearAllClients;

      for data_client in lst do
      begin
        AddItem(data_client);
      end;

      L.TopIndex := TopIdx;
      if IDSel<>-1 then
      begin
        Idx := FindItemIndexByID(IDSel);
        if Idx<>-1 then L.ItemIndex := Idx;
      end;
    finally
      L.Items.EndUpdate;
    end;

  finally
    lst.Free;
  end;

  AtCountConnections;
end;

procedure TFrm.FillStatistics(const votes, max, min, avg, median: String);

  procedure Add(Title, Value: String);
  begin
    with Stats.Items.Add do
    begin
      Caption := Title;
      SubItems.Add(Value);
    end;
  end;

begin
  Stats.Items.BeginUpdate;
  try
    Stats.Clear;

    Add('Votes', votes);
    Add('Bigger', max);
    Add('Smaller', min);
    Add('Average', avg);
    Add('Median', median);
  finally
    Stats.Items.EndUpdate;
  end;
end;

procedure TFrm.AtCountConnections;
begin
  LbCountConnections.Caption := L.Count.ToString;
end;

end.
