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
  System.JSON, Data.DBXJSONCommon, generics.collections, RTTI;

type
  TObjectHelper = class helper for TObject
  public
    procedure Map(const refJson: TJSONObject);
  end;

//  TListHelper<T> = class helper for TList<T>
//  public
//    procedure MapArray(const refJson: TJSONArray);
//  end;

  TContainerList<T:class, constructor> = class(TList<T>)
  public
    function New: T;
  end;

  IJsonMapper<T: class> = interface
    procedure Map(const refObject: TObject; const refJson: TJSONObject);
    procedure MapArray(const refList: TList<T>; const refJson: TJSONArray);
  end;

  TJsonMapper<T: class> = class(TInterfacedObject, IJsonMapper<T>)
  private
    function GetPropertyName(value: string): string;
  public
    procedure Map(const refObject: TObject; const refJson: TJSONObject);
    procedure MapArray(const refList: TList<T>; const refJson: TJSONArray);
    class function New() : IJsonMapper<T>;
  end;

implementation

uses
  typInfo, system.strUtils, system.SysUtils;

{ TJsonMapper }

function TJsonMapper<T>.GetPropertyName(value: string): string;
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

procedure TJsonMapper<T>.Map(const refObject: TObject; const refJson: TJSONObject);
var
  propertyIndex: Integer;
  propertyCount: Integer;
  propertyList: PPropList;
  propertyInfo: PPropInfo;
  value : string;
  valueInt : integer;
  valueInt64 : int64;
  refPair : TJSONPair;
const
  TypeKinds: TTypeKinds = [tkEnumeration, tkString, tkLString, tkWString, tkUString, tkInteger, tkInt64];
begin
  propertyCount := GetPropList(refObject.ClassInfo, TypeKinds, nil);
  GetMem(propertyList, propertyCount * SizeOf(PPropInfo));
  try
    GetPropList(refObject.ClassInfo, TypeKinds, propertyList);
    for propertyIndex := 0 to propertyCount - 1 do
    begin
      propertyInfo := propertyList^[propertyIndex];
      if Assigned(propertyInfo^.SetProc) then
      begin
        value := GetPropertyName(propertyInfo.Name);
        refPair := refJson.Get(value);
        if (refPair <> nil) then
        begin
          case propertyInfo^.PropType^.Kind of
            tkString, tkLString, tkUString, tkWString:
              begin
                  value := (refPair.JsonValue as TJSONString).Value;
                  SetStrProp(refObject, propertyInfo, value);
              end;
            tkInteger:
            begin
                valueInt := (refPair.JsonValue as TJSONNumber).AsInt;
                SetOrdProp(refObject, propertyInfo, valueInt);
            end;
            tkInt64:
            begin
                valueInt64 := (refPair.JsonValue as TJSONNumber).AsInt64;
                SetInt64Prop(refObject, propertyInfo, valueInt64);
            end;
            tkEnumeration: //TODO
            begin
              if GetTypeData(propertyInfo^.PropType^)^.BaseType^ = TypeInfo(Boolean) then
                SetOrdProp(refObject, propertyInfo, 0);
            end;
          end;
        end;
      end;
    end;
  finally
    FreeMem(propertyList);
  end;
end;

procedure TJsonMapper<T>.MapArray(const refList: TList<T>; const refJson: TJSONArray);
var
  size : integer;
  i : integer;
  detail : T;
  temp: TJSONObject;
  context: TRttiContext;
begin
  size := refJson.Count;
  for i := 0 to size - 1 do
  begin
    temp := refJson.Items[i] as TJSONObject;
    detail := context.GetType(TClass(T)).GetMethod('create').Invoke(TClass(T),[]).AsType<T>;
    Map(detail, temp);
    refList.Add(detail);
  end;
  refList.Sort;
end;

class function TJsonMapper<T>.New: IJsonMapper<T>;
begin
  result := Create;
end;

{ TContainerList<T> }

function TContainerList<T>.New: T;
begin
  result := T.Create;
end;

{ TObjectHelper }

procedure TObjectHelper.Map(const refJson: TJSONObject);
var
  mapper : IJsonMapper<TObject>;
begin
  mapper := TJsonMapper<TObject>.New();
  mapper.Map(self, refJson);
end;

end.
