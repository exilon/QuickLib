{ ***************************************************************************

  Copyright (c) 2015-2019 Kike Pérez

  Unit        : Quick.Options.Serializer.Yaml
  Description : Configuration groups Yaml Serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 28/11/2019

  This file is part of QuickLib: https://github.com/exilon/QuickLib

 ***************************************************************************

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

 *************************************************************************** }

unit Quick.Options.Serializer.Yaml;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  Quick.YAML,
  Quick.Commons,
  Quick.YAML.Serializer,
  Quick.Options;

type

  TYamlOptionsSerializer = class(TOptionsSerializer)
  private
    fSerializer : TRTTIYaml;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean; override;
    procedure Save(const aFilename : string; aSections : TSectionList); override;
  end;

implementation

{ TYamlOptionsSerializer }

constructor TYamlOptionsSerializer.Create;
begin
  fSerializer := TRTTIYaml.Create(TSerializeLevel.slPublishedProperty,True);
end;

destructor TYamlOptionsSerializer.Destroy;
begin
  fSerializer.Free;
  inherited;
end;

function TYamlOptionsSerializer.Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean;
var
  option : TOptions;
  fileoptions : string;
  yaml : TYamlObject;
  ypair : TYamlPair;
begin
  Result := False;
  if FileExists(aFilename) then
  begin
    //read option file
    fileoptions := TFile.ReadAllText(aFilename,TEncoding.UTF8);
    yaml := TYamlObject.ParseYAMLValue(fileoptions) as TYamlObject;
    for option in aSections do
    begin
      ypair := fSerializer.GetYamlPairByName(yaml,option.Name);
      if ypair = nil then
      begin
        if aFailOnSectionNotExists then raise Exception.CreateFmt('Config section "%s" not found',[option.Name])
          else Continue;
      end;
      if ypair.Value <> nil then
      begin
        //deserialize option
        fSerializer.DeserializeObject(option,ypair.Value as TYamlObject);
        //validate loaded configuration
        option.ValidateOptions;
      end;
    end;
  end;
end;

procedure TYamlOptionsSerializer.Save(const aFilename : string; aSections : TSectionList);
var
  option : TOptions;
  fileoptions : string;
  yaml : TYamlObject;
  jpair : TYamlPair;
begin
  yaml := TYamlObject.Create;
  try
    for option in aSections do
    begin
      //validate configuration before save
      option.ValidateOptions;
      //serialize option
      jpair := fSerializer.Serialize(option.Name,option);
      yaml.AddPair(jpair);
    end;
    fileoptions := yaml.ToYaml;
    TFile.WriteAllText(aFilename,fileoptions);
  finally
    yaml.Free;
  end;
end;

end.

