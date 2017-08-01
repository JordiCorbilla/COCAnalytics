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
  lib.coc.comparer, lib.coc.basic, lib.coc.json.mapper;

type
  TCOC = Class(TObject)
  private
    FSpells: TList<TDetail>;
    FTroops: TList<TDetail>;
    FHeroes: TList<TDetail>;
    IDetailComparer : TIDetailComparer;
    IAchievementComparer : TIAchievementComparer;
    FAchievements: TList<TAchievement>;
    FBasic: TBasic;
    procedure SetAchievements(const Value: TList<TAchievement>);
    procedure SetHeroes(const Value: TList<TDetail>);
    procedure SetSpells(const Value: TList<TDetail>);
    procedure SetTroops(const Value: TList<TDetail>);
    procedure SetBasic(const Value: TBasic);
  public
    property Achievements : TList<TAchievement> read FAchievements write SetAchievements;
    property Troops : TList<TDetail> read FTroops write SetTroops;
    property Heroes : TList<TDetail> read FHeroes write SetHeroes;
    property Spells : TList<TDetail> read FSpells write SetSpells;
    property Basic : TBasic read FBasic write SetBasic;
    Constructor Create();
    Destructor Destroy(); override;
    procedure Load(json : string);
    function LookUpAchievement(name : string) : TAchievement;
    function LookUpTroop(name : string) : TDetail;
    function LookUpHeroe(name : string) : TDetail;
    function LookUpSpell(name : string) : TDetail;
  End;


implementation

{ TCOC }

constructor TCOC.Create;
begin
  IDetailComparer := TIDetailComparer.Create;
  IAchievementComparer := TIAchievementComparer.Create;
  FSpells := TList<TDetail>.Create(IDetailComparer);
  FTroops := TList<TDetail>.Create(IDetailComparer);
  FHeroes := TList<TDetail>.Create(IDetailComparer);
  FAchievements := TList<TAchievement>.Create(IAchievementComparer);
  FBasic := TBasic.Create;
end;

destructor TCOC.Destroy;
begin
  FSpells.Free;
  FTroops.Free;
  FHeroes.Free;
  FAchievements.Free;
  inherited;
end;

function TCOC.LookUpAchievement(name: string): TAchievement;
var
  i: Integer;
  pointer : TAChievement;
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

function TCOC.LookUpHeroe(name: string): TDetail;
var
  i: Integer;
  pointer : TDetail;
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

function TCOC.LookUpSpell(name: string): TDetail;
var
  i: Integer;
  pointer : TDetail;
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

function TCOC.LookUpTroop(name: string): TDetail;
var
  i: Integer;
  pointer : TDetail;
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
  jsonBasic : IJSONMapper<TBasic>;
  jsonAchievement : IJSONMapper<TAchievement>;
  jsonDetail : IJSONMapper<TDetail>;
  detail : TDetail;
begin
  FSpells.Clear;
  FTroops.Clear;
  FHeroes.Clear;
  FAchievements.Clear;

  stream := TJSONObject.ParseJSONValue(json) as TJSONObject;
  try
    jsonBasic := TJsonMapper<TBasic>.New();
    jsonBasic.Map(FBasic, stream);

    achievements := stream.Get('achievements').JsonValue as TJSONArray;
    jsonAchievement := TJsonMapper<TAchievement>.New();
    jsonAchievement.MapArray(FAchievements, achievements);

    troops := stream.Get('troops').JsonValue as TJSONArray;
    jsonDetail := TJSONMapper<TDetail>.New();
    jsonDetail.MapArray(FTroops, troops);

    heroes := stream.Get('heroes').JsonValue as TJSONArray;
    jsonDetail := TJSONMapper<TDetail>.New();
    jsonDetail.MapArray(FHeroes, heroes);

    spells := stream.Get('spells').JsonValue as TJSONArray;
    jsonDetail := TJSONMapper<TDetail>.New();
    jsonDetail.MapArray(FSpells, spells);

  finally
    stream.Free;
  end;
end;

procedure TCOC.SetAchievements(const Value: TList<TAchievement>);
begin
  FAchievements := Value;
end;

procedure TCOC.SetBasic(const Value: TBasic);
begin
  FBasic := Value;
end;

procedure TCOC.SetHeroes(const Value: TList<TDetail>);
begin
  FHeroes := Value;
end;

procedure TCOC.SetSpells(const Value: TList<TDetail>);
begin
  FSpells := Value;
end;

procedure TCOC.SetTroops(const Value: TList<TDetail>);
begin
  FTroops := Value;
end;

end.
