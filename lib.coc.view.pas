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
  FMX.ListBox, lib.coc.achievement, lib.coc.json.parse;

type
  TDisplayAchievement = reference to procedure(list: TListBox; leftPlayer: IAchievement; rightPlayer : IAchievement);

  TView = class(TObject)
  private
    FLeft : TListBox;
    FRight : TListBox;
  public
    procedure DisplayAchievements(village : string; leftPlayer : TCOC; rightPlayer : TCOC; display : TDisplayAchievement);
    Constructor Create(left : TListBox; right : TListBox);
  end;

implementation

{ TView }

constructor TView.Create(left, right: TListBox);
begin
  FLeft := left;
  FRight := right;
end;

procedure TView.DisplayAchievements(village : string; leftPlayer, rightPlayer: TCOC; display: TDisplayAchievement);
var
  left : TCOC;
  right : TCOC;
  i : integer;
  achievementLeft : IAchievement;
  achievementRight : IAchievement;
begin
  //Show on the left the player with more achievements
  //this will ensure that the left side has always more items to display
  if leftPlayer.Achievements.Count >= rightPlayer.Achievements.count then
  begin
    left := leftPlayer;
    right := rightPlayer;
  end
  else
  begin
    left := rightPlayer;
    right := leftPlayer;
  end;

  for i := 0 to left.Achievements.count-1 do
  begin
    if left.Achievements[i].Village = village then
    begin
      achievementLeft := left.Achievements[i];
      achievementRight := right.LookUpAchievement(achievementLeft.Name);
      display(FLeft, achievementLeft, achievementRight);
    end;
  end;
end;

end.
