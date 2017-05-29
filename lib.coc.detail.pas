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

unit lib.coc.detail;

interface

type
  IDetail = interface
    procedure SetLevel(const Value: integer);
    procedure SetMaxLevel(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetVillage(const Value: string);
    function GetLevel() : integer;
    function GetMaxLevel() : integer;
    function GetName() : string;
    function GetVillage() : string;
    property Name : string read GetName write SetName;
    property Level : integer read GetLevel write SetLevel;
    property MaxLevel : integer read GetMaxLevel write SetMaxLevel;
    property Village : string read GetVillage write SetVillage;
    function Add(Name : string; Level: integer; MaxLevel : integer; Village : string) : IDetail;
    function GetLabel() : string;
  end;

  TDetail = class(TInterfacedObject, IDetail)
  private
    FLevel: integer;
    FName: string;
    FMaxLevel: integer;
    FVillage: string;
    procedure SetLevel(const Value: integer);
    procedure SetMaxLevel(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetVillage(const Value: string);
    function GetLevel() : integer;
    function GetMaxLevel() : integer;
    function GetName() : string;
    function GetVillage() : string;
  public
    property Name : string read GetName write SetName;
    property Level : integer read GetLevel write SetLevel;
    property MaxLevel : integer read GetMaxLevel write SetMaxLevel;
    property Village : string read GetVillage write SetVillage;
    constructor Create();
    class function New() : IDetail;
    function Add(Name : string; Level: integer; MaxLevel : integer; Village : string) : IDetail;
    function GetLabel() : string;
  end;

implementation

uses
  System.SysUtils;

{ TDetail }

function TDetail.Add(Name: string; Level, MaxLevel: integer; Village: string): IDetail;
begin
  SetName(Name);
  SetLevel(Level);
  SetMaxLevel(MaxLevel);
  SetVillage(Village);
  result := self;
end;

constructor TDetail.Create;
begin

end;

function TDetail.GetLabel: string;
var
  calc : double;
  calcRes : string;
begin
  calc := (FLevel*100) / FMaxLevel;
  str(calc:10:2, calcRes);
  result := FName + '. Level ' + Flevel.ToString() + ' out of '+FMaxLevel.ToString()+' (' + trim(calcRes) + '%)';
end;

function TDetail.GetLevel: integer;
begin
  result := FLevel;
end;

function TDetail.GetMaxLevel: integer;
begin
  result := FMaxLevel;
end;

function TDetail.GetName: string;
begin
  result := FName;
end;

function TDetail.GetVillage: string;
begin
  result := FVillage;
end;

class function TDetail.New: IDetail;
begin
  result := Create;
end;

procedure TDetail.SetLevel(const Value: integer);
begin
  FLevel := Value;
end;

procedure TDetail.SetMaxLevel(const Value: integer);
begin
  FMaxLevel := Value;
end;

procedure TDetail.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TDetail.SetVillage(const Value: string);
begin
  FVillage := Value;
end;

end.
