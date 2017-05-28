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

unit lib.options;

interface

uses
  System.classes, SysUtils;

type
  IOptions = interface
    function Load() : IOptions;
    procedure SetCOCToken(const Value: string);
    function GetCOCToken() : string;
    property COCToken : string read GetCOCToken write SetCOCToken;
  end;

  TOptions = class(TInterfacedObject, IOptions)
  private
    FCOCToken: string;
    procedure SetCOCToken(const Value: string);
    function GetCOCToken() : string;
  public
    property COCToken : string read GetCOCToken write SetCOCToken;
    class function New(): IOptions;
    function Load() : IOptions;
  end;

implementation

uses
  inifiles;

{ TOptions }

function TOptions.GetCOCToken: string;
begin
  result := FCOCToken;
end;

function TOptions.Load : IOptions;
var
  inifile : Tinifile;
begin
  inifile := TInifile.Create(ExtractFilePath(ParamStr(0)) + 'COCAnalytics.ini');
  try
    FCOCToken := inifile.ReadString('COC', 'token', '');
  finally
    inifile.Free;
  end;
  result := self;
end;

class function TOptions.New: IOptions;
begin
  result := Create;
end;

procedure TOptions.SetCOCToken(const Value: string);
begin
  FCOCToken := Value;
end;

end.
