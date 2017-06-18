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

unit lib.coc.json.parse;

interface

uses
  System.classes, SysUtils, lib.coc.detail, lib.coc.achievement, generics.collections, System.JSON, Data.DBXJSONCommon,
  lib.coc.comparer;

type
  TCOC = Class(TObject)
  private
    FSpells: TList<IDetail>;
    FTroops: TList<IDetail>;
    FHeroes: TList<IDetail>;
    IDetailComparer : TIDetailComparer;
    IAchievementComparer : TIAchievementComparer;
    FAchievements: TList<IAchievement>;
    procedure SetAchievements(const Value: TList<IAchievement>);
    procedure SetHeroes(const Value: TList<IDetail>);
    procedure SetSpells(const Value: TList<IDetail>);
    procedure SetTroops(const Value: TList<IDetail>);
  public
    property Achievements : TList<IAchievement> read FAchievements write SetAchievements;
    property Troops : TList<IDetail> read FTroops write SetTroops;
    property Heroes : TList<IDetail> read FHeroes write SetHeroes;
    property Spells : TList<IDetail> read FSpells write SetSpells;
    Constructor Create();
    Destructor Destroy(); override;
    procedure Load(json : string);
    procedure LoadDetail(list : TList<IDetail>; json: TJSONArray);
    function LookUpAchievement(name : string) : IAchievement;
    function LookUpTroop(name : string) : IDetail;
    function LookUpHeroe(name : string) : IDetail;
    function LookUpSpell(name : string) : IDetail;
  End;


implementation

uses
  lib.coc.json.mapper;

{ TCOC }

constructor TCOC.Create;
begin
  IDetailComparer := TIDetailComparer.Create;
  IAchievementComparer := TIAchievementComparer.Create;
  FSpells := TList<IDetail>.Create(IDetailComparer);
  FTroops := TList<IDetail>.Create(IDetailComparer);
  FHeroes := TList<IDetail>.Create(IDetailComparer);
  FAchievements := TList<IAchievement>.Create(IAchievementComparer);
end;

destructor TCOC.Destroy;
begin
  FSpells.Free;
  FTroops.Free;
  FHeroes.Free;
  FAchievements.Free;
  inherited;
end;

function TCOC.LookUpAchievement(name: string): IAchievement;
var
  i: Integer;
  pointer : IAChievement;
begin
  pointer := nil;
  for i := 0 to FAchievements.count-1 do
  begin
    if (FAchievements[i].Name = name) then
    begin
      pointer := FAchievements[i];
      break;
    end;
  end;
  result := pointer;
end;

function TCOC.LookUpHeroe(name: string): IDetail;
var
  i: Integer;
  pointer : IDetail;
begin
  pointer := nil;
  for i := 0 to FHeroes.count-1 do
  begin
    if (FHeroes[i].Name = name) then
    begin
      pointer := FHeroes[i];
      break;
    end;
  end;
  result := pointer;
end;

function TCOC.LookUpSpell(name: string): IDetail;
var
  i: Integer;
  pointer : IDetail;
begin
  pointer := nil;
  for i := 0 to FSpells.count-1 do
  begin
    if (FSpells[i].Name = name) then
    begin
      pointer := FSpells[i];
      break;
    end;
  end;
  result := pointer;
end;

function TCOC.LookUpTroop(name: string): IDetail;
var
  i: Integer;
  pointer : IDetail;
begin
  pointer := nil;
  for i := 0 to FTroops.count-1 do
  begin
    if (FTroops[i].Name = name) then
    begin
      pointer := FTroops[i];
      break;
    end;
  end;
  result := pointer;
end;

procedure TCOC.Load(json: string);
var
  stream: TJSONObject;
  achievements: TJSONArray;
  troops: TJSONArray;
  heroes: TJSONArray;
  spells: TJSONArray;
  size : integer;
  i : integer;
  temp: TJSONObject;
  AchievementDetail : TAchievement;
  jsonMapper : IJSONMapper;
begin
  FSpells.Clear;
  FTroops.Clear;
  FHeroes.Clear;
  FAchievements.Clear;

  stream := TJSONObject.ParseJSONValue(json) as TJSONObject;
  try
    //VersusBattleWinCount := (stream.Get('versusBattleWinCount').JsonValue as TJSONNumber).AsInt;
    achievements := stream.Get('achievements').JsonValue as TJSONArray;
    size := achievements.Count;
    for i := 0 to size - 1 do
    begin
      temp := achievements.Items[i] as TJSONObject;
      AchievementDetail := TAchievement.Create();

      //Use new JSON Mapper
      jsonMapper := TJsonMapper.New();
      jsonMapper.Map(AchievementDetail, temp);

//      AchievementDetail.Name := (temp.Get('name').JsonValue as TJSONString).Value;
//      AchievementDetail.Stars := (temp.Get('stars').JsonValue as TJSONNumber).AsInt;
//      AchievementDetail.Value := (temp.Get('value').JsonValue as TJSONNumber).AsInt64;
//      AchievementDetail.Target := (temp.Get('target').JsonValue as TJSONNumber).AsInt64;
//      AchievementDetail.Info := (temp.Get('info').JsonValue as TJSONString).Value;
//      if (temp.Get('completionInfo') <> nil) then
//        AchievementDetail.CompletionInfo := (temp.Get('completionInfo').JsonValue as TJSONString).Value;
//      AchievementDetail.Village := (temp.Get('village').JsonValue as TJSONString).Value;
      FAchievements.Add(AchievementDetail);
    end;
    FAchievements.Sort;

    troops := stream.Get('troops').JsonValue as TJSONArray;
    LoadDetail(FTroops, troops);
    FTroops.Sort;

    heroes := stream.Get('heroes').JsonValue as TJSONArray;
    LoadDetail(FHeroes, heroes);
    FHeroes.Sort;

    spells := stream.Get('spells').JsonValue as TJSONArray;
    LoadDetail(FSpells, spells);
    FSpells.Sort;

  finally
    stream.Free;
  end;
end;

procedure TCOC.LoadDetail(list: TList<IDetail>; json: TJSONArray);
var
  size : integer;
  i : integer;
  temp: TJSONObject;
  Detail : IDetail;
begin
    size := json.Count;
    for i := 0 to size - 1 do
    begin
      temp := json.Items[i] as TJSONObject;
      Detail := TDetail.New();
      Detail.Name := (temp.Get('name').JsonValue as TJSONString).Value;
      Detail.Level := (temp.Get('level').JsonValue as TJSONNumber).AsInt64;
      Detail.MaxLevel := (temp.Get('maxLevel').JsonValue as TJSONNumber).AsInt64;
      Detail.Village := (temp.Get('village').JsonValue as TJSONString).Value;
      list.Add(Detail);
    end;
end;

procedure TCOC.SetAchievements(const Value: TList<IAchievement>);
begin
  FAchievements := Value;
end;

procedure TCOC.SetHeroes(const Value: TList<IDetail>);
begin
  FHeroes := Value;
end;

procedure TCOC.SetSpells(const Value: TList<IDetail>);
begin
  FSpells := Value;
end;

procedure TCOC.SetTroops(const Value: TList<IDetail>);
begin
  FTroops := Value;
end;

end.
