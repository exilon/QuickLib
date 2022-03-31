{ ***************************************************************************

  Copyright (c) 2016-2021 Kike Pérez

  Unit        : Quick.RegEx.Utils
  Description : Common string validations
  Author      : Kike Pérez
  Version     : 2.0
  Created     : 07/04/2021
  Modified    : 07/04/2021

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

unit Quick.RegEx.Utils;

{$i QuickLib.inc}

interface

uses
  System.SysUtils,
  System.RegularExpressions;

type
  TRegExUtils = class
    /// <summary> Validates if value is Integer or Float number </summary>
    class function IsNumber(const aValue : string) : Boolean;
    /// <summary> Validates if value is Integer number </summary>
    class function IsInteger(const aValue : string) : Boolean;
    /// <summary> Validates if value is Float number </summary>
    class function IsFloat(const aValue : string) : Boolean;
    /// <summary> Validates if value is alphanumeric with optional space char as valid char </summary>
    class function IsAlphanumeric(const aValue : string; aSpaceCharAsValid : Boolean = True) : Boolean;
    /// <summary> Validates email address </summary>
    class function IsValidEmail(const aEmail : string) : Boolean;
    /// <summary> Validates password complexity (Should have 1 lowercase letter, 1 uppercase letter, 1 number,
    /// 1 special character and be at least 8 characters long) </summary>
    class function IsPasswordComplex(const aPassword : string) : Boolean;
    /// <summary> Validate username (may include _ and – with min and max length restriction) </summary>
    class function IsValidUsername(const aUsername: string; aMinLength : Integer = 3; aMaxLength : Integer = 18): Boolean;
    // <summary> Validates Url with optional protocol </summary>
    class function IsValidUrl(const aUrl: string; aProtocolOptional : Boolean): Boolean;
    // <summary> Validates Ip v4 </summary>
    class function IsValidIpv4(const aIp : string) : Boolean;
    // <summary> Validates Ip v6 </summary>
    class function IsValidIpv6(const aIp : string) : Boolean;
    // <summary> Validates date format YYYMMdd with - or . or / </summary>
    class function IsValidDate_YYYYMMdd(const aDate : string) : Boolean;
    // <summary> Validates date format ddMMYYYY with - or . or / </summary>
    class function IsValidDate_ddMMYYY(const aDate : string) : Boolean;
    // <summary> Validates Httml tag </summary>
    class function IsValidHtmlTag(const aValue : string) : Boolean;
    // <summary> Validates for duplicates in a string </summary>
    class function HasDuplicates(const aValue : string) : Boolean;
    /// <summary> Validates international number with optional country code/extension </summary>
    class function IsValidPhoneNumber(const aPhoneNumber : string) : Boolean;
    /// <summary> Validates Path, filename and extension </summary>
    class function IsValidFilePath(const aFilePath : string) : Boolean;
    /// <summary> Validates Visa card number </summary>
    class function IsValidVisaCard(const aCardNumber : string) : Boolean;
    /// <summary> Validates Master Card number </summary>
    class function IsValidMasterCard(const aCardNumber : string) : Boolean;
    /// <summary> Validates American Express card number /summary>
    class function IsValidAmericanExpressCard(const aCardNumber : string) : Boolean;
    /// <summary> Validates Passport number</summary>
    class function IsValidPassport(const aPassport : string) : Boolean;
    /// <summary> Validates Spanish Documento Nacional de Identidad </summary>
    class function IsValidDNI_ES(const aDNI : string) : Boolean;
    /// <summary> Validates USA Social Security Number document </summary>
    class function IsValidSSN_US(const aSSN : string) : Boolean;
  end;

implementation

{ TRegExUtils }

class function TRegExUtils.IsNumber(const aValue: string): Boolean;
begin
  Result := TRegEx.IsMatch(aValue,'^(-|)\d*(\.\d+)?$');
end;

class function TRegExUtils.IsFloat(const aValue: string): Boolean;
begin
  Result := TRegEx.IsMatch(aValue,'^(-|)\d*\.\d+$');
end;

class function TRegExUtils.IsInteger(const aValue: string): Boolean;
begin
  Result := TRegEx.IsMatch(aValue,'^\d+$');
end;

class function TRegExUtils.IsAlphanumeric(const aValue : string; aSpaceCharAsValid : Boolean) : Boolean;
begin
  if aSpaceCharAsValid then Result := TRegEx.IsMatch(aValue,'^[a-zA-Z0-9 ]*$')
    else Result := TRegEx.IsMatch(aValue,'^[a-zA-Z0-9]*$');
end;

class function TRegExUtils.IsPasswordComplex(const aPassword : string) : Boolean;
begin
  Result := TRegEx.IsMatch(aPassword,'(?=(.*[0-9]))(?=.*[\!@#$%^&*()\\[\]{}\-_+=~`|:;"''<>,./?])(?=.*[a-z])(?=(.*[A-Z]))(?=(.*)).{8,}');
end;

class function TRegExUtils.IsValidEmail(const aEmail: string): Boolean;
begin
  Result := TRegEx.IsMatch(aEmail,'^([a-zA-Z0-9._%-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,6})*$');
end;

class function TRegExUtils.IsValidFilePath(const aFilePath: string): Boolean;
begin
  Result := TRegEx.IsMatch(aFilePath,'((\/|\\|\/\/|https?:\\\\|https?:\/\/)[a-z0-9_@\-^!#$%&+={}.\/\\\[\]]+)+\.[a-z]+$');
end;

class function TRegExUtils.IsValidUsername(const aUsername: string; aMinLength : Integer; aMaxLength : Integer): Boolean;
begin
  Result := TRegEx.IsMatch(aUsername,Format('^[A-Za-z0-9_-]{%d,%d}$',[aMinLength,aMaxLength]));
end;


class function TRegExUtils.IsValidUrl(const aUrl: string; aProtocolOptional : Boolean): Boolean;
begin
  if aProtocolOptional then Result := TRegEx.IsMatch(aUrl,'(https?:\/\/)?(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)')
    else Result := TRegEx.IsMatch(aUrl,'https?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#()?&//=]*)');
end;

class function TRegExUtils.IsValidIpv4(const aIp: string): Boolean;
begin
  Result := TRegEx.IsMatch(aIp,'^(([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])\.){3}([0-9]|[1-9][0-9]|1[0-9]{2}|2[0-4][0-9]|25[0-5])$');
end;

class function TRegExUtils.IsValidIpv6(const aIp: string): Boolean;
begin
  Result := TRegEx.IsMatch(aIp,'(([0-9a-fA-F]{1,4}:){7,7}[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,7}:|([0-9a-fA-F]'
                              +'{1,4}:){1,6}:[0-9a-fA-F]{1,4}|([0-9a-fA-F]{1,4}:){1,5}(:[0-9a-fA-F]{1,4}){1,2}|'
                              +'([0-9a-fA-F]{1,4}:){1,4}(:[0-9a-fA-F]{1,4}){1,3}|([0-9a-fA-F]{1,4}:){1,3}'
                              +'(:[0-9a-fA-F]{1,4}){1,4}|([0-9a-fA-F]{1,4}:){1,2}(:[0-9a-fA-F]{1,4}){1,5}'
                              +'|[0-9a-fA-F]{1,4}:((:[0-9a-fA-F]{1,4}){1,6})|:((:[0-9a-fA-F]{1,4}){1,7}|:)'
                              +'|fe80:(:[0-9a-fA-F]{0,4}){0,4}%[0-9a-zA-Z]{1,}|::(ffff(:0{1,4}){0,1}:){0,1}'
                              +'((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.){3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9])'
                              +'{0,1}[0-9])|([0-9a-fA-F]{1,4}:){1,4}:((25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])\.)'
                              +'{3,3}(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9]))');
end;

class function TRegExUtils.IsValidDate_ddMMYYY(const aDate: string): Boolean;
begin
  Result := TRegEx.IsMatch(aDate,'^(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)'
                                 + '(?:0?[13-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-'
                                 +'|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|'
                                 +'(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)'
                                 +'(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$');
end;

class function TRegExUtils.IsValidDate_YYYYMMdd(const aDate: string): Boolean;
begin
  Result := TRegEx.IsMatch(aDate,'([12]\d{3}(\/|-|\.)(0[1-9]|1[0-2])(\/|-|\.)(0[1-9]|[12]\d|3[01]))');
end;

class function TRegExUtils.IsValidHtmlTag(const aValue: string): Boolean;
begin
  Result := TRegEx.IsMatch(aValue,'<\/?[\w\s]*>|<.+[\W]>');
end;

class function TRegExUtils.HasDuplicates(const aValue: string): Boolean;
begin
  Result := TRegEx.IsMatch(aValue,'(\b\w+\b)(?=.*\b\1\b)');
end;

class function TRegExUtils.IsValidPhoneNumber(const aPhoneNumber: string): Boolean;
begin
  Result := TRegEx.IsMatch(aPhoneNumber,'^(?:(?:\(?(?:00|\+)([1-4]\d\d|[1-9]\d?)\)?)?[\-\.\ \\\/]?)?((?:\(?\d{1,}\)?[\-\.\ \\\/]?){0,})(?:[\-\.\ \\\/]?(?:#|ext\.?|extension|x)[\-\.\ \\\/]?(\d+))?$');
end;

class function TRegExUtils.IsValidVisaCard(const aCardNumber: string): Boolean;
begin
  Result := TRegEx.IsMatch(aCardNumber,'^4[0-9]{12}(?:[0-9]{3})?$');
end;

class function TRegExUtils.IsValidMasterCard(const aCardNumber: string): Boolean;
begin
  Result := TRegEx.IsMatch(aCardNumber,'^(?:5[1-5][0-9]{2}|222[1-9]|22[3-9][0-9]|2[3-6][0-9]{2}|27[01][0-9]|2720)[0-9]{12}$');
end;

class function TRegExUtils.IsValidAmericanExpressCard(const aCardNumber: string): Boolean;
begin
  Result := TRegEx.IsMatch(aCardNumber,'^3[47][0-9]{13}$');
end;

class function TRegExUtils.IsValidPassport(const aPassport: string): Boolean;
begin
  Result := TRegEx.IsMatch(aPassport,'^[A-PR-WY][1-9]\d\s?\d{4}[1-9]$');
end;

class function TRegExUtils.IsValidDNI_ES(const aDNI: string): Boolean;
begin
  Result := TRegEx.IsMatch(aDNI,'((([X-Z])|([LM])){1}([-]?)((\d){7})([-]?)([A-Z]{1}))|((\d{8})([-]?)([A-Z]))');
end;

class function TRegExUtils.IsValidSSN_US(const aSSN: string): Boolean;
begin
  Result := TRegEx.IsMatch(aSSN,'^((?!219-09-9999|078-05-1120)(?!666|000|9\d{2})\d{3}-(?!00)\d{2}-(?!0{4})\d{4})|((?!219 09 9999|078 05 1120)(?!666|000|9\d{2})\d{3} (?!00)\d{2} (?!0{4})\d{4})|((?!219099999|078051120)(?!666|000|9\d{2})\d{3}(?!00)\d{2}(?!0{4})\d{4})$');
end;

end.
