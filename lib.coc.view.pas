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

unit lib.coc.view;

interface

uses
  FMX.ListBox, lib.coc.achievement, lib.coc.json.parse, lib.coc.detail;

type
  TDisplayAchievement = reference to procedure(list: TListBox; leftPlayer: TAchievement; rightPlayer : TAchievement);
  TDisplayDetail = reference to procedure(list: TListBox; leftPlayer: TDetail; rightPlayer : TDetail);
  TDisplayBasic = reference to procedure(list: TListBox; mainLabel : string; textLeft: string; valueLeft, maxLeft: integer; textRight: string; valueRight, maxRight: integer);

  TView = class(TObject)
  private
    FLeft : TListBox;
    FRight : TListBox;
    FLeftPlayer : TCOC;
    FRightPlayer : TCOC;
  public
    procedure DisplayAchievements(village : string; display: TDisplayAchievement);
    procedure DisplayTroops(village : string; display: TDisplayDetail);
    procedure DisplayHeroes(village : string; display: TDisplayDetail);
    procedure DisplaySpells(village : string; display: TDisplayDetail);
    procedure DisplayBasic(display : TDisplayBasic);
    Constructor Create(left, right: TListBox; leftPlayer, rightPlayer: TCOC);
  end;

implementation

uses
  SysUtils;

{ TView }

constructor TView.Create(left, right: TListBox; leftPlayer, rightPlayer: TCOC);
begin
  FLeft := left;
  FRight := right;

  //Show on the left the player with more achievements
  //this will ensure that the left side has always more items to display
  if (leftPlayer.Achievements.Count+leftPlayer.Troops.Count+leftPlayer.Heroes.Count+leftPlayer.Spells.Count) >=
    (rightPlayer.Achievements.count+rightPlayer.Troops.Count+rightPlayer.Heroes.Count+rightPlayer.Spells.Count) then
  begin
    FLeftPlayer := leftPlayer;
    FRightPlayer := rightPlayer;
  end
  else
  begin
    FLeftPlayer := rightPlayer;
    FRightPlayer := leftPlayer;
  end;
end;

procedure TView.DisplayAchievements(village : string; display: TDisplayAchievement);
var
  i : integer;
  achievementLeft : TAchievement;
  achievementRight : TAchievement;
begin
  for i := 0 to FLeftPlayer.Achievements.count-1 do
  begin
    if FLeftPlayer.Achievements[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Achievements[i];
      achievementRight := FRightPlayer.LookUpAchievement(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end
    else
    begin
      achievementLeft := FLeftPlayer.Achievements[i];
      achievementRight := FRightPlayer.LookUpAchievement(achievementLeft.Name);
      display(FRight, achievementLeft, achievementRight);
    end;
  end;
end;

procedure TView.DisplayBasic(display: TDisplayBasic);
begin
  display(FLeft, 'Tag', FLeftPlayer.Basic.Tag, 0, 1, FRightPlayer.Basic.Tag, 0, 1);
  display(FLeft, 'Name', FLeftPlayer.Basic.Name, 0, 1, FRightPlayer.Basic.Name, 0, 1);
  display(FLeft, 'TownHallLevel', FLeftPlayer.Basic.TownHallLevel.ToString, FLeftPlayer.Basic.TownHallLevel, FLeftPlayer.Basic.TownHallLevel, FRightPlayer.Basic.TownHallLevel.ToString, FRightPlayer.Basic.TownHallLevel, FRightPlayer.Basic.TownHallLevel);
  display(FLeft, 'ExpLevel', FLeftPlayer.Basic.ExpLevel.ToString, FLeftPlayer.Basic.ExpLevel, FLeftPlayer.Basic.ExpLevel, FRightPlayer.Basic.ExpLevel.ToString, FRightPlayer.Basic.ExpLevel, FRightPlayer.Basic.ExpLevel);
  display(FLeft, 'Trophies', FLeftPlayer.Basic.Trophies.ToString, FLeftPlayer.Basic.Trophies, FLeftPlayer.Basic.Trophies, FRightPlayer.Basic.Trophies.ToString, FRightPlayer.Basic.Trophies, FRightPlayer.Basic.Trophies);
  display(FLeft, 'BestTrophies', FLeftPlayer.Basic.BestTrophies.ToString, FLeftPlayer.Basic.Trophies, FLeftPlayer.Basic.BestTrophies, FRightPlayer.Basic.BestTrophies.ToString, FRightPlayer.Basic.Trophies, FRightPlayer.Basic.BestTrophies);
  display(FLeft, 'WarStars', FLeftPlayer.Basic.WarStars.ToString, FLeftPlayer.Basic.WarStars, FLeftPlayer.Basic.WarStars, FRightPlayer.Basic.WarStars.ToString, FRightPlayer.Basic.WarStars, FRightPlayer.Basic.WarStars);
  display(FLeft, 'AttackWins', FLeftPlayer.Basic.AttackWins.ToString, FLeftPlayer.Basic.AttackWins, FLeftPlayer.Basic.AttackWins, FRightPlayer.Basic.AttackWins.ToString, FRightPlayer.Basic.AttackWins, FRightPlayer.Basic.AttackWins);
  display(FLeft, 'DefenseWins', FLeftPlayer.Basic.DefenseWins.ToString, FLeftPlayer.Basic.DefenseWins, FLeftPlayer.Basic.DefenseWins, FRightPlayer.Basic.DefenseWins.ToString, FRightPlayer.Basic.DefenseWins, FRightPlayer.Basic.DefenseWins);
  display(FLeft, 'BuilderHallLevel', FLeftPlayer.Basic.BuilderHallLevel.ToString, FLeftPlayer.Basic.BuilderHallLevel, FLeftPlayer.Basic.BuilderHallLevel, FRightPlayer.Basic.BuilderHallLevel.ToString, FLeftPlayer.Basic.BuilderHallLevel, FLeftPlayer.Basic.BuilderHallLevel);
  display(FLeft, 'VersusTrophies', FLeftPlayer.Basic.VersusTrophies.ToString, FLeftPlayer.Basic.VersusTrophies, FLeftPlayer.Basic.VersusTrophies, FRightPlayer.Basic.VersusTrophies.ToString, FRightPlayer.Basic.VersusTrophies, FRightPlayer.Basic.VersusTrophies);
  display(FLeft, 'BestVersusTrophies', FLeftPlayer.Basic.BestVersusTrophies.ToString, FLeftPlayer.Basic.BestVersusTrophies, FLeftPlayer.Basic.BestVersusTrophies, FRightPlayer.Basic.BestVersusTrophies.ToString, FRightPlayer.Basic.BestVersusTrophies, FRightPlayer.Basic.BestVersusTrophies);
  display(FLeft, 'Role', FLeftPlayer.Basic.Role, 0, 1, FRightPlayer.Basic.Role, 0, 1);
  display(FLeft, 'Donations', FLeftPlayer.Basic.Donations.ToString, FLeftPlayer.Basic.Donations, FLeftPlayer.Basic.Donations, FRightPlayer.Basic.Donations.ToString, FRightPlayer.Basic.Donations, FRightPlayer.Basic.Donations);
  display(FLeft, 'DonationsReceived', FLeftPlayer.Basic.DonationsReceived.ToString, FLeftPlayer.Basic.DonationsReceived, FLeftPlayer.Basic.DonationsReceived, FRightPlayer.Basic.DonationsReceived.ToString, FRightPlayer.Basic.DonationsReceived, FRightPlayer.Basic.DonationsReceived);
  display(FLeft, 'VersusBattleWinCount', FLeftPlayer.Basic.VersusBattleWinCount.ToString, FLeftPlayer.Basic.VersusBattleWinCount, FLeftPlayer.Basic.VersusBattleWinCount, FRightPlayer.Basic.VersusBattleWinCount.ToString, FLeftPlayer.Basic.VersusBattleWinCount, FLeftPlayer.Basic.VersusBattleWinCount);
end;

procedure TView.DisplayHeroes(village: string; display: TDisplayDetail);
var
  i : integer;
  achievementLeft : TDetail;
  achievementRight : TDetail;
begin
  for i := 0 to FLeftPlayer.Heroes.count-1 do
  begin
    if FLeftPlayer.Heroes[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Heroes[i];
      achievementRight := FRightPlayer.LookUpHeroe(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end
    else
    begin
      achievementLeft := FLeftPlayer.Heroes[i];
      achievementRight := FRightPlayer.LookUpHeroe(achievementLeft.Name);
      display(FRight, achievementLeft, achievementRight);
    end;
  end;
end;

procedure TView.DisplaySpells(village: string; display: TDisplayDetail);
var
  i : integer;
  achievementLeft : TDetail;
  achievementRight : TDetail;
begin
  for i := 0 to FLeftPlayer.Spells.count-1 do
  begin
    if FLeftPlayer.Spells[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Spells[i];
      achievementRight := FRightPlayer.LookUpSpell(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end
    else
    begin
      achievementLeft := FLeftPlayer.Spells[i];
      achievementRight := FRightPlayer.LookUpSpell(achievementLeft.Name);
      display(FRight, achievementLeft, achievementRight);
    end;
  end;
end;

procedure TView.DisplayTroops(village: string; display: TDisplayDetail);
var
  i : integer;
  achievementLeft : TDetail;
  achievementRight : TDetail;
begin
  for i := 0 to FLeftPlayer.Troops.count-1 do
  begin
    if FLeftPlayer.Troops[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Troops[i];
      achievementRight := FRightPlayer.LookUpTroop(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end
    else
    begin
      achievementLeft := FLeftPlayer.Troops[i];
      achievementRight := FRightPlayer.LookUpTroop(achievementLeft.Name);
      display(FRight, achievementLeft, achievementRight);
    end;
  end;
end;

end.
