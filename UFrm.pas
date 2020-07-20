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
    BtnSendValue: TBitBtn;
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
    LbLanguage: TLabel;
    EdLanguage: TComboBox;
    BtnSendAbstain: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure BoxModeClick(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure LDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure BtnOpenRoundClick(Sender: TObject);
    procedure BtnSendValueClick(Sender: TObject);
    procedure BtnCloseRoundClick(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LbGitHubLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure EdLanguageChange(Sender: TObject);
    procedure BtnSendAbstainClick(Sender: TObject);
  private
    RoundMinValue, RoundMaxValue: Integer;

    procedure SetBoxEstimate(hab: Boolean);
    procedure FillStatistics(const votes, bounds, avg, median, proximity: String);
    procedure ResetAllControls;
    procedure ClearAllClients;
    procedure LoadRegistry;
    procedure SaveRegistry;
    procedure SetConnectionControlsEnabled(hab: Boolean);
    procedure ReloadLanguage;
    procedure SendValue(Value: Integer);
  public
    procedure ClientAccepted;
    procedure ClientDisconnected;
    procedure FillClientsList(const A: String);
    procedure AddItem(const A: String);
    procedure AtCountConnections;
    procedure SetRoundButtons(hab: Boolean);
  end;

var
  Frm: TFrm;

implementation

{$R *.dfm}

uses Vars, UItem, UDMServer, UDMClient,
  Utils, ULanguage, UVersionCheck, DzSocket,
  System.SysUtils, Vcl.Graphics, Winapi.Windows, Vcl.Dialogs, System.UITypes,
  Winapi.ShellAPI, System.Win.Registry, System.Math;

procedure TFrm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  //--Hide PageControl Tabs
  TabStart.TabVisible := False;
  TabPoker.TabVisible := False;
  //--
  Pages.ActivePage := TabStart;

  LoadRegistry;

  if EdLanguage.ItemIndex=-1 then EdLanguage.ItemIndex := 0;
  EdLanguageChange(nil);

  CheckMyVersion;
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

    EdLanguage.ItemIndex := EdLanguage.Items.IndexOf(R.ReadString('Language'));
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

    R.WriteString('Language', EdLanguage.Text);
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
    GetItemByIndex(I).Free;

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
    MessageDlg(Lang.Get('ERROR_NAME_BLANK'), mtError, [mbOK], 0);
    EdUser.SetFocus;
    Exit;
  end;

  if BoxCfgClient.Visible then
  begin
    EdHost.Text := Trim(EdHost.Text);
    if EdHost.Text=String.Empty then
    begin
      MessageDlg(Lang.Get('ERROR_SERVER_BLANK'), mtError, [mbOK], 0);
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
  Log(Lang.Get('LOG_CONNECTING'));
  DMClient.C.Connect;
end;

procedure TFrm.ClientAccepted;
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
  IdxBoundIcon: Integer;
begin
  L.Canvas.Font.Color := clBlack;
  if odSelected in State  then L.Canvas.Brush.Color := clYellow;
  L.Canvas.FillRect(Rect);

  IL.Draw(L.Canvas, Rect.Left+3, Rect.Top+2, 0);

  Item := GetItemByIndex(Index);

  L.Canvas.TextOut(Rect.Left+30, Rect.Top+4, Item.User);

  Cor := clBlack;
  if BoxEstimate.Visible then
  begin
    if Item.Estimated then
    begin
      Res := Lang.Get('LIST_ITEM_ESTIMATED');
      Cor := clGreen;
    end else
    begin
      Res := Lang.Get('LIST_ITEM_THINKING');
      Cor := clGray;
    end;
  end else
  begin
    if Item.Estimated then
    begin
      if Item.Number<>0 then
      begin
        Res := Format(Lang.Get('LIST_ITEM_VALUE'), [Item.Number]);
        Cor := clBlack;

        if RoundMinValue<>RoundMaxValue then
        begin
          IdxBoundIcon := -1;
          if Item.Number=RoundMinValue then IdxBoundIcon := 1 else
          if Item.Number=RoundMaxValue then IdxBoundIcon := 2;

          if IdxBoundIcon<>-1 then
            IL.Draw(L.Canvas, Rect.Left+275, Rect.Top+2, IdxBoundIcon);
        end;
      end else
      begin
        Res := Lang.Get('LIST_ITEM_ABSTAINED');
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
  MA: TMsgArray;
  Item: TItem;
begin
  MA := DataToArray(A);

  Item := TItem.Create;
  Item.ID := MA[0];
  Item.User := MA[1];
  Item.Estimated := MA[2];
  Item.Number := MA[3];
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
end;

procedure TFrm.BtnCloseRoundClick(Sender: TObject);
begin
  DMServer.CloseRound;
end;

procedure TFrm.BtnSendValueClick(Sender: TObject);
var Val: Integer;
begin
   Val := StrToIntDef(EdNumber.Text, 0);

   if Val<=0 then
   begin
     MessageDlg(Lang.Get('ERROR_INVALID_VALUE'), mtError, [mbOK], 0);
     EdNumber.SetFocus;
     Exit;
   end;

  SendValue(Val);
end;

procedure TFrm.BtnSendAbstainClick(Sender: TObject);
begin
  SendValue(0);
end;

procedure TFrm.SendValue(Value: Integer);
begin
  //send estimate to server
  DMClient.C.Send('N', Value.ToString);

  Log(Format(Lang.Get('LOG_SENT_VALUE'), [Value]));
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

function CompareItems(List: TStringList; Index1, Index2: Integer): Integer;
var Item1, Item2: TItem;
begin
   Item1 := TItem(List.Objects[Index1]);
   Item2 := TItem(List.Objects[Index2]);

   Result := CompareValue(Item2.Number, Item1.Number);
end;

procedure TFrm.FillClientsList(const A: String);
var
  MA: TMsgArray;
  lst: TStringList;
  props: String;
  TopIdx, IDSel, Idx: Integer;
  data_client: String;
  TempStrings: TStringList;
begin
  lst := TStringList.Create;
  try
    lst.Text := A;

    props := lst[0]; //first line contains general properties
    MA := DataToArray(props);
    SetBoxEstimate(MA[0]);
    RoundMinValue := MA[1];
    RoundMaxValue := MA[2];
    FillStatistics(MA[3], MA[4], MA[5], MA[6], MA[7]);
    lst.Delete(0);
    //**keep box loading before list because drawing depends on this

    L.Items.BeginUpdate;
    try
      TopIdx := L.TopIndex;
      IDSel := -1;
      if L.ItemIndex<>-1 then
        IDSel := GetItemByIndex(L.ItemIndex).ID;

      ClearAllClients;

      for data_client in lst do
      begin
        AddItem(data_client);
      end;

      TempStrings := TStringList.Create;
      try
        TempStrings.Assign(L.Items);
        TempStrings.CustomSort(CompareItems);
        L.Items.Assign(TempStrings);
      finally
        TempStrings.Free;
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

procedure TFrm.FillStatistics(const votes, bounds, avg, median, proximity: String);

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

    Add(Lang.Get('INFO_STATS_VOTES'), votes);
    Add(Lang.Get('INFO_STATS_BOUNDS'), bounds);
    Add(Lang.Get('INFO_STATS_AVERAGE'), avg);
    Add(Lang.Get('INFO_STATS_MEDIAN'), median);
    Add(Lang.Get('INFO_STATS_PROXIMITY'), proximity);
  finally
    Stats.Items.EndUpdate;
  end;
end;

procedure TFrm.AtCountConnections;
begin
  LbCountConnections.Caption := L.Count.ToString;
end;

procedure TFrm.EdLanguageChange(Sender: TObject);
begin
  Lang.SetLanguage(TLanguageName(EdLanguage.ItemIndex));
  ReloadLanguage;
end;

procedure TFrm.ReloadLanguage;
begin
  LbWelcome.Caption := Lang.Get('WELCOME');
  LbGitHub.Caption := Format('<a href="https://github.com/digao-dalpiaz">%s</a>', [Lang.Get('GITHUB_LINK_LABEL')]);
  LbUserName.Caption := Lang.Get('LABEL_YOUR_NAME');
  BoxMode.Caption := Lang.Get('LABEL_OP_MODE');
  BoxMode.Items[0] := Lang.Get('MODE_CLIENT');
  BoxMode.Items[1] := Lang.Get('MODE_SERVER');
  LbServerAddress.Caption := Lang.Get('LABEL_SERVER_ADDR');
  LbVersion.Caption := Format(Lang.Get('LABEL_VERSION'), [STR_VERSION]);
  LbHelpPort.Caption := Format(Lang.Get('LABEL_SOCKET_PORT'), [INT_PORT]);
  BtnStart.Caption := Lang.Get('BTN_START');
  LbParticipants.Caption := Lang.Get('LABEL_PARTICIPANTS');
  LbLabelConnections.Caption := Lang.Get('LABEL_CONNECTIONS');
  LbLabelUser.Caption := Lang.Get('LABEL_USER');
  BoxEstimate.Caption := Lang.Get('LABEL_ESTIMATE_BOX');
  LbYourEstimate.Caption := Lang.Get('LABEL_YOUR_ESTIMATE');
  BtnSendValue.Caption := Lang.Get('BTN_SEND_VALUE');
  BtnSendAbstain.Caption := Lang.Get('BTN_SEND_ABSTAIN');
  BoxCmdServer.Caption := Lang.Get('LABEL_ROUND_BOX');
  BtnOpenRound.Caption := Lang.Get('BTN_OPEN_ROUND');
  BtnCloseRound.Caption := Lang.Get('BTN_CLOSE_ROUND');
  LbStatistics.Caption := Lang.Get('LABEL_STATISTICS');
  BtnRefresh.Hint := Lang.Get('HINT_REFRESH_LIST');
end;

end.
