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

unit lib.coc.json.mapper;

interface

uses
  System.JSON, Data.DBXJSONCommon;

type
  IJsonMapper = interface
    procedure Map(AObject : TObject; json: TJSONObject);
  end;

  TJsonMapper = class(TInterfacedObject, IJsonMapper)
    procedure Map(AObject : TObject; json: TJSONObject);
    class function New() : IJsonMapper;
    function GetPropertyName(value : string) : string;
  end;

implementation

uses
  typInfo, system.strUtils, system.SysUtils;

{ TJsonMapper }

function TJsonMapper.GetPropertyName(value: string): string;
var
  lowerText : string;
  restText : string;
begin
  if length(value) > 0 then
  begin
    lowerText := AnsiLeftStr(value, 1);
    lowerText := AnsiLowerCase(lowerText);
    restText := AnsiRightStr(value, length(value)-1);
    result := lowerText + restText;
  end
  else
    result := value;
end;

procedure TJsonMapper.Map(AObject: TObject; json: TJSONObject);
var
  propertyIndex: Integer;
  propertyCount: Integer;
  propertyList: PPropList;
  propertyInfo: PPropInfo;
  value : string;
  valueInt : integer;
  valueInt64 : int64;
const
  TypeKinds: TTypeKinds = [tkEnumeration, tkString, tkLString, tkWString, tkUString, tkInteger, tkInt64];
begin
  propertyCount := GetPropList(AObject.ClassInfo, TypeKinds, nil);
  GetMem(propertyList, propertyCount * SizeOf(PPropInfo));
  try
    GetPropList(AObject.ClassInfo, TypeKinds, propertyList);
    for propertyIndex := 0 to propertyCount - 1 do
    begin
      propertyInfo := propertyList^[propertyIndex];
      if Assigned(propertyInfo^.SetProc) then
      case propertyInfo^.PropType^.Kind of
        tkString, tkLString, tkUString, tkWString:
          begin
            value := GetPropertyName(propertyInfo.Name);
            if (json.Get(value) <> nil) then
            begin
              value := (json.Get(value).JsonValue as TJSONString).Value;
              SetStrProp(AObject, propertyInfo, value);
            end;
          end;
        tkInteger:
        begin
          value := GetPropertyName(propertyInfo.Name);
          if (json.Get(value) <> nil) then
          begin
            valueInt := (json.Get(value).JsonValue as TJSONNumber).AsInt;
            SetOrdProp(AObject, propertyInfo, valueInt);
          end;
        end;
        tkInt64:
        begin
          value := GetPropertyName(propertyInfo.Name);
          if (json.Get(value) <> nil) then
          begin
            valueInt64 := (json.Get(value).JsonValue as TJSONNumber).AsInt64;
            SetInt64Prop(AObject, propertyInfo, valueInt);
          end;
        end;
        tkEnumeration:
          if GetTypeData(propertyInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
            SetOrdProp(AObject, propertyInfo, 0);
      end;
    end;
  finally
    FreeMem(propertyList);
  end;
end;

class function TJsonMapper.New: IJsonMapper;
begin
  result := Create;
end;

end.
