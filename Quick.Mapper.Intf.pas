{ ***************************************************************************

  Copyright (c) 2016-2020 Kike Pérez

  Unit        : Quick.Mapper.Intf
  Description : Quick Mapper Interface
  Author      : Kike Pérez
  Version     : 1.8
  Created     : 07/02/2020
  Modified    : 14/02/2020

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

unit Quick.Mapper.Intf;

{$i QuickLib.inc}

interface

type
  TMapTarget = record
  private
    fSrcObj : TObject;
  public
    constructor Create(aSrcObj : TObject);
    function AsType<T : class, constructor> : T;
  end;

  IMapper = interface
  ['{A4EBC4BC-94D0-4F32-98FD-4888E1EF199A}']
    procedure Map(aSrcObj, aTgtObj : TObject); overload;
    function Map(aSrcObj : TObject) : TMapTarget; overload;
  end;

implementation

end.
