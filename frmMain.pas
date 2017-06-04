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
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Grid1: TGrid;
    Column1: TColumn;
    Column2: TColumn;
    Column3: TColumn;
    StyleBook1: TStyleBook;
    Button2: TButton;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Label2: TLabel;
    ListHeroes: TListBox;
    Label3: TLabel;
    ListTroops: TListBox;
    Label4: TLabel;
    ListSpells: TListBox;
    ListAchievements: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const ACol, ARow: Integer; var Value: TValue);
    procedure Grid1SetValue(Sender: TObject; const ACol, ARow: Integer; const Value: TValue);
  private
    { Private declarations }
  public
    document : string;
    items : array of array of TValue;
    procedure AddItem(list: TListBox; name : string; level : integer; maxLevel : integer);
  end;

var
  Form1: TForm1;

implementation

uses
  System.JSON, Data.DBXJSONCommon, lib.coc.json.parse, lib.coc.basic;

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
  if edit1.Text = '' then
  begin
    showMessage('Please enter your COC User Hash tag -> #AAABBBCCC');
    exit;
  end;
  jsonResponse := TCOCApiRest.New.GetUserInfo(edit1.Text);
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

procedure TForm1.Button2Click(Sender: TObject);
var
  rttiContext: TRttiContext;
  rttiType: TRttiType;
  rttiFields: TArray<TRttiField>;
  myvar : integer;
  basic : TBasic;
begin
  basic := Tbasic.Create();
  basic.LoadValues(basic);
//  rttiContext := TRttiContext.Create;
//  rttiType := rttiContext.GetType(myvar);
//  memo1.Lines.Add(rttitype.ToString);
//  rttiType := rttiContext.GetType(document^);
//  memo1.Lines.Add(rttitype.ToString);
end;

//procedure TForm1.Button2Click(Sender: TObject);
//var
//  l1: TListBoxItem;
//  p1: TProgressBar;
//begin
//  l1 := TListBoxItem.Create(ListBox1);
//  l1.Parent := ListBox1;
//  l1.Text := 'potato';
//  p1 := TProgressBar.Create(l1);
//  p1.Parent := l1;
//  p1.Align := TAlignLayout.Right;
//end;

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
    AddItem(ListHeroes, COC.Heroes[i].GetLabel(), COC.Heroes[i].Level, COC.Heroes[i].MaxLevel);
  end;

  for i := 0 to COC.Troops.count-1 do
  begin
    AddItem(ListTroops, COC.Troops[i].GetLabel(), COC.Troops[i].Level, COC.Troops[i].MaxLevel);
  end;

  for i := 0 to COC.Spells.count-1 do
  begin
    AddItem(ListSpells, COC.Spells[i].GetLabel(), COC.Spells[i].Level, COC.Spells[i].MaxLevel);
  end;

  //setlength(items, 3, 10);

  for i := 0 to COC.Achievements.count-1 do
  begin
    AddItem(ListAchievements, COC.Achievements[i].GetLabel(), COC.Achievements[i].Value, COC.Achievements[i].Target);
  end;


  memo1.Lines.Add(jsonDocument);
  FDConnection1.Close();
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
