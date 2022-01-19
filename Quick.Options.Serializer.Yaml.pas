{ ***************************************************************************

  Copyright (c) 2015-2020 Kike Pérez

  Unit        : Quick.Options.Serializer.Yaml
  Description : Configuration groups Yaml Serializer
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 18/10/2019
  Modified    : 15/12/2021

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

  TYamlOptionsSerializer = class(TOptionsFileSerializer)
  private
    fSerializer : TRTTIYaml;
    function ParseFile(out aYamlObj : TYamlObject) : Boolean;
  public
    constructor Create(const aFilename : string);
    destructor Destroy; override;
    function Load(aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean; override;
    function LoadSection(aSections : TSectionList; aOptions: TOptions) : Boolean; override;
    procedure Save(aSections : TSectionList); override;
    function GetFileSectionNames(out oSections : TArray<string>) : Boolean; override;
    function ConfigExists : Boolean; override;
  end;

implementation

{ TYamlOptionsSerializer }

function TYamlOptionsSerializer.ConfigExists: Boolean;
begin
  Result := FileExists(Filename);
end;

constructor TYamlOptionsSerializer.Create(const aFilename : string);
begin
  Filename := aFilename;
  fSerializer := TRTTIYaml.Create(TSerializeLevel.slPublishedProperty,True);
end;

destructor TYamlOptionsSerializer.Destroy;
begin
  fSerializer.Free;
  inherited;
end;

function TYamlOptionsSerializer.GetFileSectionNames(out oSections : TArray<string>) : Boolean;
var
  yaml : TYamlObject;
  i : Integer;
begin
  Result := False;
  yaml := nil;
  if ParseFile(yaml) then
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

function TYamlOptionsSerializer.ParseFile(out aYamlObj : TYamlObject) : Boolean;
var
  fileoptions : string;
begin
  aYamlObj := nil;
  if FileExists(Filename) then
  begin
    fileoptions := TFile.ReadAllText(Filename,TEncoding.UTF8);
    if fileoptions.IsEmpty then EOptionLoadError.CreateFmt('Config file "%s" is empty!',[ExtractFileName(Filename)]);

    aYamlObj := TYamlObject.ParseYAMLValue(fileoptions) as TYamlObject;
    if aYamlObj = nil then raise EOptionLoadError.CreateFmt('Config file "%s" is damaged or not well-formed Yaml format!',[ExtractFileName(Filename)]);
  end;
  Result := aYamlObj <> nil;
end;

function TYamlOptionsSerializer.Load(aSections : TSectionList; aFailOnSectionNotExists : Boolean) : Boolean;
var
  option : TOptions;
  yaml : TYamlObject;
  ypair : TYamlPair;
  found : Integer;
begin
  Result := False;
  //read option file
  if ParseFile(yaml) then
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

function TYamlOptionsSerializer.LoadSection(aSections : TSectionList; aOptions: TOptions) : Boolean;
var
  yaml : TYamlObject;
  ypair : TYamlPair;
begin
  Result := False;
  //read option file
  if ParseFile(yaml) then
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

procedure TYamlOptionsSerializer.Save(aSections : TSectionList);
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
    if not fileoptions.IsEmpty then TFile.WriteAllText(Filename,fileoptions);
  finally
    yaml.Free;
  end;
end;

end.

