{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.Options.Serializer.Yaml
  Description : Configuration groups Yaml Serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 05/04/2020

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
    function ParseFile(const aFilename : string; out aYamlObj : TYamlObject) : Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean; override;
    function LoadSection(const aFilename : string; aSections : TSectionList; aOptions: TOptions) : Boolean; override;
    procedure Save(const aFilename : string; aSections : TSectionList); override;
    function GetFileSectionNames(const aFilename : string; out oSections : TArray<string>) : Boolean; override;
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

function TYamlOptionsSerializer.GetFileSectionNames(const aFilename : string; out oSections : TArray<string>) : Boolean;
var
  yaml : TYamlObject;
  i : Integer;
begin
  Result := False;
  yaml := nil;
  if ParseFile(aFilename,yaml) then
  begin
    try
      for i := 0 to yaml.Count - 1 do
      begin
        oSections := oSections + [yaml.Pairs[i].Name];
      end;
      Result := True;
    finally
      yaml.Free;
    end;
  end;
end;

function TYamlOptionsSerializer.ParseFile(const aFilename : string; out aYamlObj : TYamlObject) : Boolean;
var
  fileoptions : string;
begin
  aYamlObj := nil;
  if FileExists(aFilename) then
  begin
    fileoptions := TFile.ReadAllText(aFilename,TEncoding.UTF8);
    aYamlObj := TYamlObject.ParseYAMLValue(fileoptions) as TYamlObject;
    if aYamlObj = nil then raise EOptionLoadError.CreateFmt('Config file "%s" is damaged or not well-formed Yaml format!',[ExtractFileName(aFilename)]);
  end;
  Result := aYamlObj <> nil;
end;

function TYamlOptionsSerializer.Load(const aFilename : string; aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean;
var
  option : TOptions;
  yaml : TYamlObject;
  ypair : TYamlPair;
  found : Integer;
begin
  Result := False;
  //read option file
  if ParseFile(aFilename,yaml) then
  begin
    found := 0;
    try
      for option in aSections do
      begin
        ypair := fSerializer.GetYamlPairByName(yaml,option.Name);
        if ypair = nil then
        begin
          if aFailOnSectionNotExists then raise Exception.CreateFmt('Config section "%s" not found',[option.Name])
          else
          begin
            //count as found if hidden
            if option.HideOptions then Inc(found);
            Continue;
          end;
        end;
        if ypair.Value <> nil then
        begin
          //deserialize option
          fSerializer.DeserializeObject(option,ypair.Value as TYamlObject);
          //validate loaded configuration
          option.ValidateOptions;
          Inc(found);
        end;
      end;
    finally
      yaml.Free;
    end;
    //returns true if all sections located into file
    Result := found = aSections.Count;
  end;
end;

function TYamlOptionsSerializer.LoadSection(const aFilename : string; aSections : TSectionList; aOptions: TOptions) : Boolean;
var
  yaml : TYamlObject;
  ypair : TYamlPair;
begin
  Result := False;
  //read option file
  if ParseFile(aFilename,yaml) then
  begin
    try
      ypair := fSerializer.GetYamlPairByName(yaml,aOptions.Name);
      if (ypair <> nil) and (ypair.Value <> nil) then
      begin
        //deserialize option
        fSerializer.DeserializeObject(aOptions,ypair.Value as TYamlObject);
        //validate loaded configuration
        aOptions.ValidateOptions;
        Result := True;
      end
    finally
      yaml.Free;
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
      if not option.HideOptions then
      begin
        //validate configuration before save
        option.ValidateOptions;
        //serialize option
        jpair := fSerializer.Serialize(option.Name,option);
        yaml.AddPair(jpair);
      end;
    end;
    fileoptions := yaml.ToYaml;
    if not fileoptions.IsEmpty then TFile.WriteAllText(aFilename,fileoptions);
  finally
    yaml.Free;
  end;
end;

end.

