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

unit lib.coc.basic;

interface

type
  TBasic = class(TObject)
  private
    FName: string;
    FBestTrophies: Integer;
    FVersusTrophies: integer;
    FWarStars: integer;
    FRole: string;
    FDonationsReceived: integer;
    FAttacksWin: integer;
    FTag: string;
    FBuilderHallLevel: integer;
    FBesVersusTrophies: integer;
    FTownHallLevel: integer;
    FExpLevel: integer;
    FTrophies: integer;
    FDonations: integer;
    FDefenseWin: integer;
    FVersusBattleWinCount: integer;
    procedure SetAttacksWin(const Value: integer);
    procedure SetBestTrophies(const Value: Integer);
    procedure SetBesVersusTrophies(const Value: integer);
    procedure SetBuilderHallLevel(const Value: integer);
    procedure SetDefenseWin(const Value: integer);
    procedure SetDonations(const Value: integer);
    procedure SetDonationsReceived(const Value: integer);
    procedure SetExpLevel(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetRole(const Value: string);
    procedure SetTag(const Value: string);
    procedure SetTownHallLevel(const Value: integer);
    procedure SetTrophies(const Value: integer);
    procedure SetVersusTrophies(const Value: integer);
    procedure SetWarStars(const Value: integer);
    procedure SetVersusBattleWinCount(const Value: integer);
  published
    property Tag : string read FTag write SetTag;
    property Name : string read FName write SetName;
    property TownHallLevel : integer read FTownHallLevel write SetTownHallLevel;
    property ExpLevel : integer read FExpLevel write SetExpLevel;
    property Trophies : integer read FTrophies write SetTrophies;
    property BestTrophies : Integer read FBestTrophies write SetBestTrophies;
    property WarStars : integer read FWarStars write SetWarStars;
    property AttacksWin : integer read FAttacksWin write SetAttacksWin;
    property DefenseWin : integer read FDefenseWin write SetDefenseWin;
    property BuilderHallLevel :integer read FBuilderHallLevel write SetBuilderHallLevel;
    property VersusTrophies : integer read FVersusTrophies write SetVersusTrophies;
    property BesVersusTrophies : integer read FBesVersusTrophies write SetBesVersusTrophies;
    property Role : string read FRole write SetRole;
    property Donations: integer read FDonations write SetDonations;
    property DonationsReceived: integer read FDonationsReceived write SetDonationsReceived;
    property VersusBattleWinCount : integer read FVersusBattleWinCount write SetVersusBattleWinCount;
    procedure LoadValues(const AObject : TObject);
  end;

implementation

uses
  typInfo;

{ TBasic }

procedure TBasic.LoadValues(const AObject : TObject);
var
  PropIndex: Integer;
  PropCount: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
const
  TypeKinds: TTypeKinds = [tkEnumeration, tkString, tkLString, tkWString, tkUString, tkInteger];
begin
  PropCount := GetPropList(AObject.ClassInfo, TypeKinds, nil);
  GetMem(PropList, PropCount * SizeOf(PPropInfo));
  try
    GetPropList(AObject.ClassInfo, TypeKinds, PropList);
    for PropIndex := 0 to PropCount - 1 do
    begin
      PropInfo := PropList^[PropIndex];
      if Assigned(PropInfo^.SetProc) then
      case PropInfo^.PropType^.Kind of
        tkString, tkLString, tkUString, tkWString:
          SetStrProp(AObject, PropInfo, '');
        tkEnumeration:
          if GetTypeData(PropInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
            SetOrdProp(AObject, PropInfo, 0);
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure TBasic.SetAttacksWin(const Value: integer);
begin
  FAttacksWin := Value;
end;

procedure TBasic.SetBestTrophies(const Value: Integer);
begin
  FBestTrophies := Value;
end;

procedure TBasic.SetBesVersusTrophies(const Value: integer);
begin
  FBesVersusTrophies := Value;
end;

procedure TBasic.SetBuilderHallLevel(const Value: integer);
begin
  FBuilderHallLevel := Value;
end;

procedure TBasic.SetDefenseWin(const Value: integer);
begin
  FDefenseWin := Value;
end;

procedure TBasic.SetDonations(const Value: integer);
begin
  FDonations := Value;
end;

procedure TBasic.SetDonationsReceived(const Value: integer);
begin
  FDonationsReceived := Value;
end;

procedure TBasic.SetExpLevel(const Value: integer);
begin
  FExpLevel := Value;
end;

procedure TBasic.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TBasic.SetRole(const Value: string);
begin
  FRole := Value;
end;

procedure TBasic.SetTag(const Value: string);
begin
  FTag := Value;
end;

procedure TBasic.SetTownHallLevel(const Value: integer);
begin
  FTownHallLevel := Value;
end;

procedure TBasic.SetTrophies(const Value: integer);
begin
  FTrophies := Value;
end;

procedure TBasic.SetVersusBattleWinCount(const Value: integer);
begin
  FVersusBattleWinCount := Value;
end;

procedure TBasic.SetVersusTrophies(const Value: integer);
begin
  FVersusTrophies := Value;
end;

procedure TBasic.SetWarStars(const Value: integer);
begin
  FWarStars := Value;
end;

end.
