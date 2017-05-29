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

unit lib.coc.achievement;

interface

type
  IAchievement = interface
    procedure SetCompletionInfo(const Value: string);
    procedure SetInfo(const Value: string);
    procedure SetName(const Value: string);
    procedure SetStars(const Value: integer);
    procedure SetTarget(const Value: int64);
    procedure SetValue(const Value: int64);
    procedure SetVillage(const Value: string);
    function GetCompletionInfo() : string;
    function GetInfo(): string;
    function GetName(): string;
    function GetStars() : integer;
    function GetTarget() : int64;
    function GetValue() : int64;
    function GetVillage() : string;
    property Name : string read GetName write SetName;
    property Stars : integer read GetStars write SetStars;
    property Value : int64 read GetValue write SetValue;
    property Target : int64 read GetTarget write SetTarget;
    property Info : string read GetInfo write SetInfo;
    property CompletionInfo : string read GetCompletionInfo write SetCompletionInfo;
    property Village : string read GetVillage write SetVillage;
    function Add(Name : string; Stars : integer; Value : int64; Target : int64; Info : string; CompletionInfo : string; Village : string) : IAchievement;
  end;

  TAchievement = class(TInterfacedObject, IAchievement)
  private
    FName: string;
    FVillage: string;
    FStars: integer;
    FInfo: string;
    FTarget: int64;
    FValue: int64;
    FCompletionInfo: string;
    procedure SetCompletionInfo(const Value: string);
    procedure SetInfo(const Value: string);
    procedure SetName(const Value: string);
    procedure SetStars(const Value: integer);
    procedure SetTarget(const Value: int64);
    procedure SetValue(const Value: int64);
    procedure SetVillage(const Value: string);
    function GetCompletionInfo() : string;
    function GetInfo(): string;
    function GetName(): string;
    function GetStars() : integer;
    function GetTarget() : int64;
    function GetValue() : int64;
    function GetVillage() : string;
  public
    property Name : string read GetName write SetName;
    property Stars : integer read GetStars write SetStars;
    property Value : int64 read GetValue write SetValue;
    property Target : int64 read GetTarget write SetTarget;
    property Info : string read GetInfo write SetInfo;
    property CompletionInfo : string read GetCompletionInfo write SetCompletionInfo;
    property Village : string read GetVillage write SetVillage;
    constructor Create();
    class function New() : IAchievement;
    function Add(Name : string; Stars : integer; Value : int64; Target : int64; Info : string; CompletionInfo : string; Village : string) : IAchievement;
  end;

implementation

{ TAchievement }

function TAchievement.Add(Name: string; Stars: integer; Value, Target: int64; Info, CompletionInfo, Village: string): IAchievement;
begin
  SetName(Name);
  SetStars(Stars);
  SetValue(Value);
  SetTarget(Target);
  SetInfo(Info);
  SetCompletionInfo(CompletionInfo);
  SetVillage(Village);
  result := self;
end;

constructor TAchievement.Create;
begin

end;

function TAchievement.GetCompletionInfo: string;
begin
  result := FCompletionInfo;
end;

function TAchievement.GetInfo: string;
begin
  result := FInfo;
end;

function TAchievement.GetName: string;
begin
  result := FName;
end;

function TAchievement.GetStars: integer;
begin
  result := FStars;
end;

function TAchievement.GetTarget: int64;
begin
  result := FTarget;
end;

function TAchievement.GetValue: int64;
begin
  result := FValue;
end;

function TAchievement.GetVillage: string;
begin
  result := FVillage;
end;

class function TAchievement.New: IAchievement;
begin
  result := Create;
end;

procedure TAchievement.SetCompletionInfo(const Value: string);
begin
  FCompletionInfo := Value;
end;

procedure TAchievement.SetInfo(const Value: string);
begin
  FInfo := Value;
end;

procedure TAchievement.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TAchievement.SetStars(const Value: integer);
begin
  FStars := Value;
end;

procedure TAchievement.SetTarget(const Value: int64);
begin
  FTarget := Value;
end;

procedure TAchievement.SetValue(const Value: int64);
begin
  FValue := Value;
end;

procedure TAchievement.SetVillage(const Value: string);
begin
  FVillage := Value;
end;

end.
