{
    This file is part of QuickLib: https://github.com/exilon/QuickLib

    QuickLibs. Copyright (C) 2022 Kike Pérez
      Exilon - https://www.exilon.es

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

     ***************************************************************************
}

{$ifdef FPC}
  {$MODE DELPHI}

  {$INLINE ON}
  {$define HASINLINE}

  {$ifdef LINUX}
    {$define FPCLINUX}
  {$endif}

  {$ifdef ANDROID}
    {$define LINUX}
  {$endif}

  {$ifdef VER2_7}
    {$define ISFPC27}
  {$endif}

  {$ifdef VER3_0}
    {$define ISFPC27}
    {$define ISFPC30}
  {$endif}

  {$ifdef VER3_1}
    {$define ISFPC27}
    {$define ISFPC30}
  {$endif}

  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    {$define FPC_ENUMHASINNER}
  {$endif}

  {$ifdef FPC_HAS_MANAGEMENT_OPERATORS}
    {$define ISDELPHI2006ANDUP}
  {$endif FPC_HAS_MANAGEMENT_OPERATORS}

{$else FPC}

  {$ifdef CONDITIONALEXPRESSIONS} //Delphi 6 or newer
    {$define HASINTERFACERTTI} //interface RTTI (not FPC)
    {$ifndef LINUX}
      {$ifdef VER140}
        {$define DELPHI6_UP} //Delphi 6 or newer
      {$else}
        {$define DELPHI7_UP} //Delphi 7 or newer
      {$endif}
    {$endif LINUX}
    {$if CompilerVersion >= 17}
      {$define DELPHI2005_UP} //Delphi 2005 or newer
    {$endif}
    {$if CompilerVersion >= 18}
      {$define DELPHI2006_UP} //Delphi 2006 or newer
      {$define HASINLINE}
    {$endif}
    {$if CompilerVersion > 18}
      {$define DELPHI2007_UP} //Delphi 2007 or newer
    {$endif}
    {$if CompilerVersion = 20}
      {$define DELPHI2009_UP} //Delphi 2009
    {$endif}
    {$if CompilerVersion >= 21.0} //Delphi 2010
      {$define DELPHI2010_UP}
      {$define FPC_OR_UNICODE}
    {$endif}
    {$if CompilerVersion >= 22.0} //Delphi XE
      {$define DELPHIXE_UP}
    {$endif}
    {$if CompilerVersion >= 23.0} //Delphi XE2
      {$define DELPHIXE2_UP}
    {$endif}
    {$if CompilerVersion >= 24.0} //Delphi XE3
      {$define DELPHIXE3_UP}
    {$endif}
    {$if CompilerVersion >= 25.0} //Delphi XE4
      {$define DELPHIXE4_UP}
    {$endif}
    {$if CompilerVersion >= 26.0} //Delphi XE5
      {$define DELPHIXE5_UP}
    {$endif}
    {$if CompilerVersion >= 27.0} //Delphi XE6
      {$define DELPHIXE6_UP}
    {$endif}
    {$if CompilerVersion >= 28.0} //Delphi XE7
      {$define DELPHIXE7_UP}
    {$endif}
    {$if CompilerVersion >= 29.0} //Delphi XE8
      {$define DELPHIXE8_UP}
    {$endif}
    {$if CompilerVersion >= 30.0} //Delphi XE10 Seattle
      {$define DELPHIRX10_UP}
	  {$define DELPHISEATTLE_UP}
    {$endif}
    {$if CompilerVersion >= 31.0} //Delphi RX10.1 Berlin
      {$define DELPHIRX101_UP}
	    {$define DELPHIBERLIN_UP}
    {$endif}
    {$if CompilerVersion >= 32.0} //Delphi RX10.2 Tokyo
      {$define DELPHIRX102_UP}
	    {$define DELPHITOKYO_UP}
      {$ifdef LINUX}
        {$define DELPHILINUX}
      {$endif}
    {$endif}
    {$if CompilerVersion >= 33.0} //Delphi RX10.3 Rio
      {$define DELPHIRX103_UP}
	    {$define DELPHIRIO_UP}
    {$endif}
    {$if CompilerVersion >= 34.0} //Delphi RX10.4 Sydney
      {$define DELPHIRX104_UP}
	    {$define DELPHISYDNEY_UP}
      {$UNDEF DELPHILINUX}
      {$if defined(POSIX)}
        {$if defined(CPUARM)}
          {$if defined(MACOS)}

          {$ELSE}
            {$DEFINE ANDROID}
          {$ENDIF}
        {$ELSE}
          {$DEFINE LINUX}
          {$DEFINE DELPHILINUX}
        {$ENDIF}
      {$ENDIF}
      {$if defined(ANDROID) OR defined(IOS)}
        {$define NEXTGEN} //compatibility with older delphis
      {$endif}
    {$endif}
    {$if CompilerVersion >= 35.0} //Delphi RX11 Alexandria
      {$define DELPHIRX11_UP}
	    {$define DELPHIALEXANDRIA_UP}
      {$UNDEF DELPHILINUX}
      {$if defined(POSIX)}
        {$DEFINE LINUX}
        {$if defined(CPUARM)}
          {$if defined(MACOS)}

          {$ELSE}
            {$DEFINE ANDROID}
          {$ENDIF}
        {$ELSE}
          {$DEFINE LINUX}
          {$DEFINE DELPHILINUX}
        {$ENDIF}
      {$ENDIF}
      {$if defined(ANDROID) OR defined(IOS)}
        {$define NEXTGEN} //compatibility with older delphis
      {$endif}
    {$endif}
  {$else}
    //Delphi 5 or older
    {$define DELPHI6OROLDER}
    {$define DELPHI5OROLDER}
    {$define DELPHI5ORFPC}
    {$define MSWINDOWS}
  {$endif}

{$endif FPC}

{$ifdef VER150}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$ifdef CONDITIONALEXPRESSIONS}  //Delphi 6 or newer
  {.$WARN SYMBOL_PLATFORM OFF}
  {.$WARN UNIT_PLATFORM OFF}
{$endif}

{.$define SHOW_ENVIRONMENTPATH_ERRORS}

//Debug library
{$IFDEF DEBUG}
  {.$DEFINE DEBUG_SERIALIZER}
  {.$DEFINE DEBUG_HTTPSERVER}
  {.$DEFINE DEBUG_OBJPOOL}
  {.$DEFINE DEBUG_PARAMETERS}
  {.$DEFINE DEBUG_REDIS}
  {.$DEFINE DEBUG_IOC}
{$ENDIF}

{.$DEFINE QUICK_HINTSOFF}
{.$DEFINE QUICK_WARNOFF}

{$ifdef QUICK_HINTSOFF}
	{$HINTS OFF}
{$endif}

{$ifdef QUICK_WARNOFF}
	{$WARNINGS OFF}
{$endif}


