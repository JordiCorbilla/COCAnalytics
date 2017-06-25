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
  TDisplayAchievement = reference to procedure(list: TListBox; leftPlayer: IAchievement; rightPlayer : IAchievement);
  TDisplayDetail = reference to procedure(list: TListBox; leftPlayer: IDetail; rightPlayer : IDetail);
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
  achievementLeft : IAchievement;
  achievementRight : IAchievement;
begin
  for i := 0 to FLeftPlayer.Achievements.count-1 do
  begin
    if FLeftPlayer.Achievements[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Achievements[i];
      achievementRight := FRightPlayer.LookUpAchievement(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end;
  end;
end;

procedure TView.DisplayBasic(display: TDisplayBasic);
begin
  display(FLeft, 'Tag', FLeftPlayer.Basic.Tag, 0, 1, FRightPlayer.Basic.Tag, 0, 1);
end;

procedure TView.DisplayHeroes(village: string; display: TDisplayDetail);
var
  i : integer;
  achievementLeft : IDetail;
  achievementRight : IDetail;
begin
  for i := 0 to FLeftPlayer.Heroes.count-1 do
  begin
    if FLeftPlayer.Heroes[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Heroes[i];
      achievementRight := FRightPlayer.LookUpHeroe(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end;
  end;
end;

procedure TView.DisplaySpells(village: string; display: TDisplayDetail);
var
  i : integer;
  achievementLeft : IDetail;
  achievementRight : IDetail;
begin
  for i := 0 to FLeftPlayer.Spells.count-1 do
  begin
    if FLeftPlayer.Spells[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Spells[i];
      achievementRight := FRightPlayer.LookUpSpell(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end;
  end;
end;

procedure TView.DisplayTroops(village: string; display: TDisplayDetail);
var
  i : integer;
  achievementLeft : IDetail;
  achievementRight : IDetail;
begin
  for i := 0 to FLeftPlayer.Troops.count-1 do
  begin
    if FLeftPlayer.Troops[i].Village = village then
    begin
      achievementLeft := FLeftPlayer.Troops[i];
      achievementRight := FRightPlayer.LookUpTroop(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end;
  end;
end;

end.
