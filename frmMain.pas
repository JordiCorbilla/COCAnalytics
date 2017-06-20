// Copyright (c) 2017, Jordi Corbilla
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// - Redistributions of source code must retain the above copyright notice,
// this list of conditions and the following disclaimer.
// - Redistributions in binary form must reproduce the above copyright notice,
// this list of conditions and the following disclaimer in the documentation
// and/or other materials provided with the distribution.
// - Neither the name of this library nor the names of its contributors may be
// used to endorse or promote products derived from this software without
// specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

unit frmMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation, lib.coc.api.rest, FMX.ListBox,
  FMX.Layouts, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.MSSQL, FireDAC.Phys.MSSQLDef,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FMX.ScrollBox, FMX.Memo, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, System.Rtti, FMX.Grid.Style, FMX.Grid,
  FMX.ListView, FMX.TabControl;

type
  TForm1 = class(TForm)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    StyleBook1: TStyleBook;
    TabControl2: TTabControl;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Label2: TLabel;
    ListHeroes: TListBox;
    Label3: TLabel;
    ListTroops: TListBox;
    Label4: TLabel;
    ListSpells: TListBox;
    ListAchievements: TListBox;
    TabItem2: TTabItem;
    Grid1: TGrid;
    Column1: TColumn;
    Column2: TColumn;
    Column3: TColumn;
    Button5: TButton;
    Label5: TLabel;
    Panel1: TPanel;
    Button6: TButton;
    Label6: TLabel;
    edtuser1: TEdit;
    Label7: TLabel;
    edtUser2: TEdit;
    Panel2: TPanel;
    Button7: TButton;
    Label8: TLabel;
    edtUser: TEdit;
    Button4: TButton;
    Button3: TButton;
    TabControl3: TTabControl;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    ListBoxUser1: TListBox;
    ListBoxBuilder: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadDocument(jsonDocument1 : string; jsonDocument2 : string; list: TListBox; list2 : TListBox);
    procedure AddSideBySide(list: TListBox; mainLabel, achievementValue1: string; level1, maxLevel1: integer; achievementValue2: string; level2, maxLevel2: integer);
  public
    document : string;
    items : array of array of TValue;
    procedure AddItem(list: TListBox; name : string; level : integer; maxLevel : integer);
  end;

var
  Form1: TForm1;

implementation

uses
  System.JSON, Data.DBXJSONCommon, lib.coc.json.parse, lib.coc.basic, System.UIConsts, lib.coc.achievement, lib.coc.detail, lib.coc.view;

{$R *.fmx}

procedure TForm1.AddItem(list: TListBox; name: string; level, maxLevel: integer);
var
  l1: TListBoxItem;
  p1: TProgressBar;
begin
  l1 := TListBoxItem.Create(list);
  l1.Parent := list;
  l1.Text := name;
  p1 := TProgressBar.Create(l1);
  p1.Parent := l1;
  if (level > maxlevel) then
  begin
    p1.Max := level;
    p1.Value := level;
  end
  else
  begin
    p1.Max := maxLevel;
    p1.Value := level;
  end;
  p1.Align := TAlignLayout.Right;
//  p1 := TProgressBar.Create(l1);
//  p1.Parent := l1;
//  if (level > maxlevel) then
//  begin
//    p1.Max := level;
//    p1.Value := level;
//  end
//  else
//  begin
//    p1.Max := maxLevel;
//    p1.Value := level;
//  end;
//  p1.Align := TAlignLayout.Right;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  jsonResponse : string;
  json: TJSONObject;
  achievements: TJSONArray;
  troops: TJSONArray;
  heroes: TJSONArray;
  spells: TJSONArray;
  versusBattleWinCount : TJSONString;
begin
  //Get the JSON value from COC API.
  if edtuser.Text = '' then
  begin
    showMessage('Please enter your COC User Hash tag -> #AAABBBCCC');
    exit;
  end;
  jsonResponse := TCOCApiRest.New.GetUserInfo(edtuser.Text);
  document := jsonResponse;
  //Parse the JSON and get the whole list of objects.
  json := TJSONObject.ParseJSONValue(jsonResponse) as TJSONObject;
  try
    versusBattleWinCount := json.Get('versusBattleWinCount').JsonValue as TJSONString;
    achievements := json.Get('achievements').JsonValue as TJSONArray;
    troops := json.Get('troops').JsonValue as TJSONArray;
    heroes := json.Get('heroes').JsonValue as TJSONArray;
    spells := json.Get('spells').JsonValue as TJSONArray;
  finally
    json.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FDConnection1.Open();
  FDQuery1.SQL.Text := 'INSERT INTO Analytics (Document) VALUES (:Document)';
  FDQuery1.ParamByName('Document').AsWideMemo := document;
  FDQuery1.ExecSQL;
  FDConnection1.Close();
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  jsonDocument : string;
  COC : TCOC;
  i: Integer;
begin
  FDConnection1.Open();
  //Get two items to compare.
  FDQuery1.SQL.Text := 'select top 2 Document, Created from Analytics order by Created desc'; //Just get the latest
  FDQuery1.Open;

  while not FdQuery1.Eof do
  begin
    jsonDocument :=  FdQuery1.FieldByName('Document').AsString;
    FdQuery1.Next;
  end;

  COC := TCOC.Create();
  COC.Load(jsonDocument);

  for i := 0 to COC.Heroes.count-1 do
  begin
    if COC.Heroes[i].Village='home' then
      AddItem(ListHeroes, COC.Heroes[i].GetLabel(), COC.Heroes[i].Level, COC.Heroes[i].MaxLevel);
  end;

  for i := 0 to COC.Troops.count-1 do
  begin
    if COC.Troops[i].Village='home' then
      AddItem(ListTroops, COC.Troops[i].GetLabel(), COC.Troops[i].Level, COC.Troops[i].MaxLevel);
  end;

  for i := 0 to COC.Spells.count-1 do
  begin
    if COC.Spells[i].Village='home' then
      AddItem(ListSpells, COC.Spells[i].GetLabel(), COC.Spells[i].Level, COC.Spells[i].MaxLevel);
  end;

  for i := 0 to COC.Achievements.count-1 do
  begin
    if COC.Achievements[i].Village='home' then
      AddItem(ListAchievements, COC.Achievements[i].GetLabel(), COC.Achievements[i].Value, COC.Achievements[i].Target);
  end;

  FDConnection1.Close();
end;

procedure TForm1.Button5Click(Sender: TObject);
var
  l1: TListBoxItem;
  p1: TProgressBar;
  lab1 : TLabel;
begin
//  l1 := TListBoxItem.Create(listbox1);
//  l1.Parent := listbox1;
//
//  lab1 := TLabel.Create(l1);
//  lab1.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
//  lab1.TextSettings.FontColor := claLawngreen;
//  lab1.Parent := l1;
//  lab1.Width := 200;
//  lab1.TextSettings.FontColor := claLawngreen;
//  lab1.Text := 'this is a long text';
//  lab1.Align := TAlignLayout.Left;
//
////  l1.Text := name;
//  p1 := TProgressBar.Create(l1);
//  p1.Parent := l1;
//  p1.Max := 100;
//  p1.Value := 50;
//  p1.Align := TAlignLayout.Right;
//
//  lab1 := TLabel.Create(l1);
//  lab1.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
//  lab1.TextSettings.FontColor := claLawngreen;
//  lab1.Parent := l1;
//  lab1.Width := 200;
//  lab1.TextSettings.FontColor := claRed;
//  lab1.Text := 'this is a long text2';
//  lab1.Align := TAlignLayout.Right;
//
//  p1 := TProgressBar.Create(l1);
//  p1.Parent := l1;
//  p1.Max := 100;
//  p1.Value := 50;
//  p1.Align := TAlignLayout.Right;
//
//
//  lab1 := TLabel.Create(l1);
//  lab1.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
//  lab1.TextSettings.FontColor := claLawngreen;
//  lab1.Parent := l1;
//  lab1.Width := 200;
//  lab1.TextSettings.FontColor := claRed;
//  lab1.Text := 'this is a long text2';
//  lab1.Align := TAlignLayout.Right;


end;

procedure TForm1.Button6Click(Sender: TObject);
var
  jsonResponse1, jsonResponse2 : string;
begin
  //Get the JSON value from COC API.
  if (edtuser1.Text = '') or (edtUser2.Text = '') then
  begin
    showMessage('Please enter your COC User Hash tag -> #AAABBBCCC');
    exit;
  end;
  jsonResponse1 := TCOCApiRest.New.GetUserInfo(edtuser1.Text);
  jsonResponse2 := TCOCApiRest.New.GetUserInfo(edtuser2.Text);

  ListBoxUser1.Clear;
  ListBoxBuilder.Clear;
  LoadDocument(jsonResponse1, jsonResponse2, ListBoxUser1, ListBoxBuilder);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TabItem5.IsSelected := true;
end;

procedure TForm1.LoadDocument(jsonDocument1 : string; jsonDocument2 : string; list: TListBox; list2 : TListBox);
var
  COC1, COC2 : TCOC;
  i: Integer;
  achievement1, achievement2 : IAchievement;
  detail1, detail2 : IDetail;
begin
  COC1 := TCOC.Create();
  COC2 := TCOC.Create();
  COC1.Load(jsonDocument1);
  COC2.Load(jsonDocument2);

  //Check if left side has more than right side

  for i := 0 to COC1.Achievements.count-1 do
  begin
    if COC1.Achievements[i].Village='home' then
    begin
      achievement1 := COC1.Achievements[i];
      achievement2 := COC2.LookUpAchievement(achievement1.Name);
      if (achievement2 <> nil) then
        AddSideBySide(list, achievement1.Name, achievement1.GetAchievementValue, achievement1.Value, achievement1.Target, achievement2.GetAchievementValue, achievement2.Value, achievement2.Target)
      else
        AddSideBySide(list, achievement1.Name, achievement1.GetAchievementValue, achievement1.Value, achievement1.Target, '', 0, 1);
    end
    else
    begin
      achievement1 := COC1.Achievements[i];
      achievement2 := COC2.LookUpAchievement(achievement1.Name);
      if (achievement2 <> nil) then
        AddSideBySide(list2, achievement1.Name, achievement1.GetAchievementValue, achievement1.Value, achievement1.Target, achievement2.GetAchievementValue, achievement2.Value, achievement2.Target)
      else
        AddSideBySide(list2, achievement1.Name, achievement1.GetAchievementValue, achievement1.Value, achievement1.Target, '', 0, 1);
    end;
  end;

  for i := 0 to COC1.Troops.count-1 do
  begin
    if COC1.Troops[i].Village='home' then
    begin
      detail1 := COC1.Troops[i];
      detail2 := COC2.LookUpTroop(detail1.Name);
      if (detail2 <> nil) then
        AddSideBySide(list, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, detail2.GetAchievementValue, detail2.Level, detail2.MaxLevel)
      else
        AddSideBySide(list, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, '', 0, 1);
    end
    else
    begin
      detail1 := COC1.Troops[i];
      detail2 := COC2.LookUpTroop(detail1.Name);
      if (detail2 <> nil) then
        AddSideBySide(list2, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, detail2.GetAchievementValue, detail2.Level, detail2.MaxLevel)
      else
        AddSideBySide(list2, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, '', 0, 1);
    end;
  end;

  for i := 0 to COC1.Spells.count-1 do
  begin
    if COC1.Spells[i].Village='home' then
    begin
      detail1 := COC1.Spells[i];
      detail2 := COC2.LookUpSpell(detail1.Name);
      if (detail2 <> nil) then
        AddSideBySide(list, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, detail2.GetAchievementValue, detail2.Level, detail2.MaxLevel)
      else
        AddSideBySide(list, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, '', 0, 1);
    end
    else
    begin
      detail1 := COC1.Spells[i];
      detail2 := COC2.LookUpSpell(detail1.Name);
      if (detail2 <> nil) then
        AddSideBySide(list2, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, detail2.GetAchievementValue, detail2.Level, detail2.MaxLevel)
      else
        AddSideBySide(list2, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, '', 0, 1);
    end;
  end;

  for i := 0 to COC1.Heroes.count-1 do
  begin
    if COC1.Heroes[i].Village='home' then
    begin
      detail1 := COC1.Heroes[i];
      detail2 := COC2.LookUpHeroe(detail1.Name);
      if (detail2 <> nil) then
        AddSideBySide(list, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, detail2.GetAchievementValue, detail2.Level, detail2.MaxLevel)
      else
        AddSideBySide(list, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, '', 0, 1);
    end
    else
    begin
      detail1 := COC1.Heroes[i];
      detail2 := COC2.LookUpHeroe(detail1.Name);
      if (detail2 <> nil) then
        AddSideBySide(list2, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, detail2.GetAchievementValue, detail2.Level, detail2.MaxLevel)
      else
        AddSideBySide(list2, detail1.Name, detail1.GetAchievementValue, detail1.Level, detail1.MaxLevel, '', 0, 1);
    end;
  end;
end;

procedure TForm1.AddSideBySide(list: TListBox; mainLabel : string; achievementValue1: string; level1, maxLevel1: integer; achievementValue2: string; level2, maxLevel2: integer);
var
  l1: TListBoxItem;
  p1: TProgressBar;
  lab1 : TLabel;
begin
  l1 := TListBoxItem.Create(list);
  l1.Parent := list;

  lab1 := TLabel.Create(l1);
  lab1.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
  lab1.TextSettings.FontColor := claWhite;
  lab1.Parent := l1;
  lab1.Width := 400;
  //lab1.TextSettings.FontColor := claLawngreen;
  lab1.Text := mainLabel;
  lab1.Align := TAlignLayout.Left;

  p1 := TProgressBar.Create(l1);
  p1.Parent := l1;
  p1.Width := 300;
  if (level2 > maxlevel2) then
  begin
    p1.Max := level2;
    p1.Value := level2;
  end
  else
  begin
    p1.Max := maxLevel2;
    p1.Value := level2;
  end;
  p1.Align := TAlignLayout.Right;

  lab1 := TLabel.Create(l1);
  lab1.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
  lab1.TextSettings.FontColor := claWhite;
  lab1.Parent := l1;
  lab1.Width := 300;
//  lab1.TextSettings.FontColor := claRed;
  lab1.Text := '   ' + achievementValue2;
  lab1.Align := TAlignLayout.Right;

  p1 := TProgressBar.Create(l1);
  p1.Parent := l1;
  p1.Width := 300;
  if (level1 > maxlevel1) then
  begin
    p1.Max := level1;
    p1.Value := level1;
  end
  else
  begin
    p1.Max := maxLevel1;
    p1.Value := level1;
  end;
  p1.Align := TAlignLayout.Right;

  lab1 := TLabel.Create(l1);
  lab1.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style];
  lab1.TextSettings.FontColor := claWhite;
  lab1.Parent := l1;
  lab1.Width := 300;
  //lab1.TextSettings.FontColor := claRed;
  lab1.Text := '   ' + achievementValue1;
  lab1.Align := TAlignLayout.Right;
end;

procedure TForm1.Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
begin
  //Get the value
end;

procedure TForm1.Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
begin
  //Set the value
end;

end.
