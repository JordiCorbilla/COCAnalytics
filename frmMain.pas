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
  FMX.Layouts;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ProgressBar2: TProgressBar;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.JSON, Data.DBXJSONCommon;

{$R *.fmx}

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
  l1: TListBoxItem;
  p1: TProgressBar;
begin
  l1 := TListBoxItem.Create(ListBox1);
  l1.Parent := ListBox1;
  l1.Text := 'potato';
  p1 := TProgressBar.Create(l1);
  p1.Parent := l1;
  p1.Align := TAlignLayout.Right;
end;

end.
