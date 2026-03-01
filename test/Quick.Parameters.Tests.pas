{ ***************************************************************************

  Copyright (c) 2016-2026 Kike Pérez

  Unit        : Quick.Parameters.Tests
  Description : Tests for Quick.Parameters
  Author      : Kike Pérez
  Version     : 1.0
  Created     : 01/03/2026
  Modified    : 01/03/2026

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

unit Quick.Parameters.Tests;

{$i QuickLib.inc}

interface

uses
  DUnitX.TestFramework,
  Quick.Parameters;

type
  // Minimal TParameters subclass with no published properties,
  // so ParseParams does not raise ERequiredParameterNotFound.
  TTestParameters = class(TParameters)
  published
  end;

  [TestFixture]
  TQuickParametersTests = class
  private
    fParams : TTestParameters;
  public
    [Setup]
    procedure SetUp;
    [TearDown]
    procedure TearDown;

    // ExistsParam single-arg overload (default '=' separator)
    [Test]
    procedure Test_ExistsParam_NonExistentParam_ReturnsFalse;
    [Test]
    procedure Test_ExistsParam_EmptyParam_ReturnsFalse;
    [Test]
    procedure Test_ExistsParam_DoesNotRaise;

    // ExistsParam two-arg overload (custom separator — issue #135)
    [Test]
    procedure Test_ExistsParam_WithCustomSeparator_NonExistentParam_ReturnsFalse;
    [Test]
    procedure Test_ExistsParam_WithColonSeparator_DoesNotRaise;
    [Test]
    procedure Test_ExistsParam_WithSpaceSeparator_DoesNotRaise;
    [Test]
    procedure Test_ExistsParam_DefaultAndCustomSeparator_SameResultForAbsentParam;
  end;

implementation

procedure TQuickParametersTests.SetUp;
begin
  fParams := TTestParameters.Create(False);
end;

procedure TQuickParametersTests.TearDown;
begin
  fParams.Free;
end;

procedure TQuickParametersTests.Test_ExistsParam_NonExistentParam_ReturnsFalse;
begin
  Assert.IsFalse(fParams.ExistsParam('__nonexistent_param_xyz__'),
    'A param that is not on the command line must return False');
end;

procedure TQuickParametersTests.Test_ExistsParam_EmptyParam_ReturnsFalse;
begin
  Assert.IsFalse(fParams.ExistsParam(''),
    'An empty param name must return False');
end;

procedure TQuickParametersTests.Test_ExistsParam_DoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure begin fParams.ExistsParam('someParam'); end,
    nil,
    'ExistsParam with a valid param name must not raise an exception');
end;

procedure TQuickParametersTests.Test_ExistsParam_WithCustomSeparator_NonExistentParam_ReturnsFalse;
begin
  // Issue #135: the two-arg overload must honour the custom separator.
  // For a param that does not exist, the result must still be False.
  Assert.IsFalse(fParams.ExistsParam('__nonexistent_param_xyz__', ':'),
    'A param that is not on the command line must return False even with a custom separator');
end;

procedure TQuickParametersTests.Test_ExistsParam_WithColonSeparator_DoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure begin fParams.ExistsParam('someParam', ':'); end,
    nil,
    'ExistsParam with colon separator must not raise an exception');
end;

procedure TQuickParametersTests.Test_ExistsParam_WithSpaceSeparator_DoesNotRaise;
begin
  Assert.WillNotRaise(
    procedure begin fParams.ExistsParam('someParam', ' '); end,
    nil,
    'ExistsParam with space separator must not raise an exception');
end;

procedure TQuickParametersTests.Test_ExistsParam_DefaultAndCustomSeparator_SameResultForAbsentParam;
var
  resultDefault : Boolean;
  resultCustom  : Boolean;
begin
  // For a param that is not present, both overloads should agree.
  resultDefault := fParams.ExistsParam('__absent__');
  resultCustom  := fParams.ExistsParam('__absent__', ':');
  Assert.AreEqual(resultDefault, resultCustom,
    'Both overloads must return the same result when the param is absent');
end;

initialization
  TDUnitX.RegisterTestFixture(TQuickParametersTests);

end.
