unit WbemScripting_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 52393 $
// File generated on 03/04/2019 16:27:07 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\SysWOW64\wbem\wbemdisp.TLB (1)
// LIBID: {565783C6-CB41-11D1-8B02-00600806D9B6}
// LCID: 0
// Helpfile: 
// HelpString: Microsoft WMI Scripting V1.2 Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// SYS_KIND: SYS_WIN32
// Errors:
//   Hint: Member 'Class' of 'ISWbemObjectPath' changed to 'Class_'
//   Hint: Member 'Object' of 'ISWbemRefreshableItem' changed to 'Object_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}

interface

uses Winapi.Windows, System.Classes, System.Variants, System.Win.StdVCL, Vcl.Graphics, Vcl.OleServer, Winapi.ActiveX;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WbemScriptingMajorVersion = 1;
  WbemScriptingMinorVersion = 2;

  LIBID_WbemScripting: TGUID = '{565783C6-CB41-11D1-8B02-00600806D9B6}';

  IID_ISWbemServices: TGUID = '{76A6415C-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemObject: TGUID = '{76A6415A-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemObjectPath: TGUID = '{5791BC27-CE9C-11D1-97BF-0000F81E849C}';
  IID_ISWbemNamedValueSet: TGUID = '{CF2376EA-CE8C-11D1-8B05-00600806D9B6}';
  IID_ISWbemNamedValue: TGUID = '{76A64164-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemSecurity: TGUID = '{B54D66E6-2287-11D2-8B33-00600806D9B6}';
  IID_ISWbemPrivilegeSet: TGUID = '{26EE67BF-5804-11D2-8B4A-00600806D9B6}';
  IID_ISWbemPrivilege: TGUID = '{26EE67BD-5804-11D2-8B4A-00600806D9B6}';
  IID_ISWbemObjectSet: TGUID = '{76A6415F-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemQualifierSet: TGUID = '{9B16ED16-D3DF-11D1-8B08-00600806D9B6}';
  IID_ISWbemQualifier: TGUID = '{79B05932-D3B7-11D1-8B06-00600806D9B6}';
  IID_ISWbemPropertySet: TGUID = '{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}';
  IID_ISWbemProperty: TGUID = '{1A388F98-D4BA-11D1-8B09-00600806D9B6}';
  IID_ISWbemMethodSet: TGUID = '{C93BA292-D955-11D1-8B09-00600806D9B6}';
  IID_ISWbemMethod: TGUID = '{422E8E90-D955-11D1-8B09-00600806D9B6}';
  IID_ISWbemEventSource: TGUID = '{27D54D92-0EBE-11D2-8B22-00600806D9B6}';
  IID_ISWbemLocator: TGUID = '{76A6415B-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemLastError: TGUID = '{D962DB84-D4BB-11D1-8B09-00600806D9B6}';
  DIID_ISWbemSinkEvents: TGUID = '{75718CA0-F029-11D1-A1AC-00C04FB6C223}';
  IID_ISWbemSink: TGUID = '{75718C9F-F029-11D1-A1AC-00C04FB6C223}';
  IID_ISWbemServicesEx: TGUID = '{D2F68443-85DC-427E-91D8-366554CC754C}';
  IID_ISWbemObjectEx: TGUID = '{269AD56A-8A67-4129-BC8C-0506DCFE9880}';
  IID_ISWbemDateTime: TGUID = '{5E97458A-CF77-11D3-B38F-00105A1F473A}';
  IID_ISWbemRefresher: TGUID = '{14D8250E-D9C2-11D3-B38F-00105A1F473A}';
  IID_ISWbemRefreshableItem: TGUID = '{5AD4BF92-DAAB-11D3-B38F-00105A1F473A}';
  CLASS_SWbemLocator: TGUID = '{76A64158-CB41-11D1-8B02-00600806D9B6}';
  CLASS_SWbemNamedValueSet: TGUID = '{9AED384E-CE8B-11D1-8B05-00600806D9B6}';
  CLASS_SWbemObjectPath: TGUID = '{5791BC26-CE9C-11D1-97BF-0000F81E849C}';
  CLASS_SWbemLastError: TGUID = '{C2FEEEAC-CFCD-11D1-8B05-00600806D9B6}';
  CLASS_SWbemSink: TGUID = '{75718C9A-F029-11D1-A1AC-00C04FB6C223}';
  CLASS_SWbemDateTime: TGUID = '{47DFBE54-CF76-11D3-B38F-00105A1F473A}';
  CLASS_SWbemRefresher: TGUID = '{D269BF5C-D9C1-11D3-B38F-00105A1F473A}';
  CLASS_SWbemServices: TGUID = '{04B83D63-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemServicesEx: TGUID = '{62E522DC-8CF3-40A8-8B2E-37D595651E40}';
  CLASS_SWbemObject: TGUID = '{04B83D62-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemObjectEx: TGUID = '{D6BDAFB2-9435-491F-BB87-6AA0F0BC31A2}';
  CLASS_SWbemObjectSet: TGUID = '{04B83D61-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemNamedValue: TGUID = '{04B83D60-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemQualifier: TGUID = '{04B83D5F-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemQualifierSet: TGUID = '{04B83D5E-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemProperty: TGUID = '{04B83D5D-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemPropertySet: TGUID = '{04B83D5C-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemMethod: TGUID = '{04B83D5B-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemMethodSet: TGUID = '{04B83D5A-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemEventSource: TGUID = '{04B83D58-21AE-11D2-8B33-00600806D9B6}';
  CLASS_SWbemSecurity: TGUID = '{B54D66E9-2287-11D2-8B33-00600806D9B6}';
  CLASS_SWbemPrivilege: TGUID = '{26EE67BC-5804-11D2-8B4A-00600806D9B6}';
  CLASS_SWbemPrivilegeSet: TGUID = '{26EE67BE-5804-11D2-8B4A-00600806D9B6}';
  CLASS_SWbemRefreshableItem: TGUID = '{8C6854BC-DE4B-11D3-B390-00105A1F473A}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum WbemImpersonationLevelEnum
type
  WbemImpersonationLevelEnum = TOleEnum;
const
  wbemImpersonationLevelAnonymous = $00000001;
  wbemImpersonationLevelIdentify = $00000002;
  wbemImpersonationLevelImpersonate = $00000003;
  wbemImpersonationLevelDelegate = $00000004;

// Constants for enum WbemAuthenticationLevelEnum
type
  WbemAuthenticationLevelEnum = TOleEnum;
const
  wbemAuthenticationLevelDefault = $00000000;
  wbemAuthenticationLevelNone = $00000001;
  wbemAuthenticationLevelConnect = $00000002;
  wbemAuthenticationLevelCall = $00000003;
  wbemAuthenticationLevelPkt = $00000004;
  wbemAuthenticationLevelPktIntegrity = $00000005;
  wbemAuthenticationLevelPktPrivacy = $00000006;

// Constants for enum WbemPrivilegeEnum
type
  WbemPrivilegeEnum = TOleEnum;
const
  wbemPrivilegeCreateToken = $00000001;
  wbemPrivilegePrimaryToken = $00000002;
  wbemPrivilegeLockMemory = $00000003;
  wbemPrivilegeIncreaseQuota = $00000004;
  wbemPrivilegeMachineAccount = $00000005;
  wbemPrivilegeTcb = $00000006;
  wbemPrivilegeSecurity = $00000007;
  wbemPrivilegeTakeOwnership = $00000008;
  wbemPrivilegeLoadDriver = $00000009;
  wbemPrivilegeSystemProfile = $0000000A;
  wbemPrivilegeSystemtime = $0000000B;
  wbemPrivilegeProfileSingleProcess = $0000000C;
  wbemPrivilegeIncreaseBasePriority = $0000000D;
  wbemPrivilegeCreatePagefile = $0000000E;
  wbemPrivilegeCreatePermanent = $0000000F;
  wbemPrivilegeBackup = $00000010;
  wbemPrivilegeRestore = $00000011;
  wbemPrivilegeShutdown = $00000012;
  wbemPrivilegeDebug = $00000013;
  wbemPrivilegeAudit = $00000014;
  wbemPrivilegeSystemEnvironment = $00000015;
  wbemPrivilegeChangeNotify = $00000016;
  wbemPrivilegeRemoteShutdown = $00000017;
  wbemPrivilegeUndock = $00000018;
  wbemPrivilegeSyncAgent = $00000019;
  wbemPrivilegeEnableDelegation = $0000001A;
  wbemPrivilegeManageVolume = $0000001B;

// Constants for enum WbemCimtypeEnum
type
  WbemCimtypeEnum = TOleEnum;
const
  wbemCimtypeSint8 = $00000010;
  wbemCimtypeUint8 = $00000011;
  wbemCimtypeSint16 = $00000002;
  wbemCimtypeUint16 = $00000012;
  wbemCimtypeSint32 = $00000003;
  wbemCimtypeUint32 = $00000013;
  wbemCimtypeSint64 = $00000014;
  wbemCimtypeUint64 = $00000015;
  wbemCimtypeReal32 = $00000004;
  wbemCimtypeReal64 = $00000005;
  wbemCimtypeBoolean = $0000000B;
  wbemCimtypeString = $00000008;
  wbemCimtypeDatetime = $00000065;
  wbemCimtypeReference = $00000066;
  wbemCimtypeChar16 = $00000067;
  wbemCimtypeObject = $0000000D;

// Constants for enum WbemErrorEnum
type
  WbemErrorEnum = TOleEnum;
const
  wbemNoErr = $00000000;
  wbemErrFailed = $80041001;
  wbemErrNotFound = $80041002;
  wbemErrAccessDenied = $80041003;
  wbemErrProviderFailure = $80041004;
  wbemErrTypeMismatch = $80041005;
  wbemErrOutOfMemory = $80041006;
  wbemErrInvalidContext = $80041007;
  wbemErrInvalidParameter = $80041008;
  wbemErrNotAvailable = $80041009;
  wbemErrCriticalError = $8004100A;
  wbemErrInvalidStream = $8004100B;
  wbemErrNotSupported = $8004100C;
  wbemErrInvalidSuperclass = $8004100D;
  wbemErrInvalidNamespace = $8004100E;
  wbemErrInvalidObject = $8004100F;
  wbemErrInvalidClass = $80041010;
  wbemErrProviderNotFound = $80041011;
  wbemErrInvalidProviderRegistration = $80041012;
  wbemErrProviderLoadFailure = $80041013;
  wbemErrInitializationFailure = $80041014;
  wbemErrTransportFailure = $80041015;
  wbemErrInvalidOperation = $80041016;
  wbemErrInvalidQuery = $80041017;
  wbemErrInvalidQueryType = $80041018;
  wbemErrAlreadyExists = $80041019;
  wbemErrOverrideNotAllowed = $8004101A;
  wbemErrPropagatedQualifier = $8004101B;
  wbemErrPropagatedProperty = $8004101C;
  wbemErrUnexpected = $8004101D;
  wbemErrIllegalOperation = $8004101E;
  wbemErrCannotBeKey = $8004101F;
  wbemErrIncompleteClass = $80041020;
  wbemErrInvalidSyntax = $80041021;
  wbemErrNondecoratedObject = $80041022;
  wbemErrReadOnly = $80041023;
  wbemErrProviderNotCapable = $80041024;
  wbemErrClassHasChildren = $80041025;
  wbemErrClassHasInstances = $80041026;
  wbemErrQueryNotImplemented = $80041027;
  wbemErrIllegalNull = $80041028;
  wbemErrInvalidQualifierType = $80041029;
  wbemErrInvalidPropertyType = $8004102A;
  wbemErrValueOutOfRange = $8004102B;
  wbemErrCannotBeSingleton = $8004102C;
  wbemErrInvalidCimType = $8004102D;
  wbemErrInvalidMethod = $8004102E;
  wbemErrInvalidMethodParameters = $8004102F;
  wbemErrSystemProperty = $80041030;
  wbemErrInvalidProperty = $80041031;
  wbemErrCallCancelled = $80041032;
  wbemErrShuttingDown = $80041033;
  wbemErrPropagatedMethod = $80041034;
  wbemErrUnsupportedParameter = $80041035;
  wbemErrMissingParameter = $80041036;
  wbemErrInvalidParameterId = $80041037;
  wbemErrNonConsecutiveParameterIds = $80041038;
  wbemErrParameterIdOnRetval = $80041039;
  wbemErrInvalidObjectPath = $8004103A;
  wbemErrOutOfDiskSpace = $8004103B;
  wbemErrBufferTooSmall = $8004103C;
  wbemErrUnsupportedPutExtension = $8004103D;
  wbemErrUnknownObjectType = $8004103E;
  wbemErrUnknownPacketType = $8004103F;
  wbemErrMarshalVersionMismatch = $80041040;
  wbemErrMarshalInvalidSignature = $80041041;
  wbemErrInvalidQualifier = $80041042;
  wbemErrInvalidDuplicateParameter = $80041043;
  wbemErrTooMuchData = $80041044;
  wbemErrServerTooBusy = $80041045;
  wbemErrInvalidFlavor = $80041046;
  wbemErrCircularReference = $80041047;
  wbemErrUnsupportedClassUpdate = $80041048;
  wbemErrCannotChangeKeyInheritance = $80041049;
  wbemErrCannotChangeIndexInheritance = $80041050;
  wbemErrTooManyProperties = $80041051;
  wbemErrUpdateTypeMismatch = $80041052;
  wbemErrUpdateOverrideNotAllowed = $80041053;
  wbemErrUpdatePropagatedMethod = $80041054;
  wbemErrMethodNotImplemented = $80041055;
  wbemErrMethodDisabled = $80041056;
  wbemErrRefresherBusy = $80041057;
  wbemErrUnparsableQuery = $80041058;
  wbemErrNotEventClass = $80041059;
  wbemErrMissingGroupWithin = $8004105A;
  wbemErrMissingAggregationList = $8004105B;
  wbemErrPropertyNotAnObject = $8004105C;
  wbemErrAggregatingByObject = $8004105D;
  wbemErrUninterpretableProviderQuery = $8004105F;
  wbemErrBackupRestoreWinmgmtRunning = $80041060;
  wbemErrQueueOverflow = $80041061;
  wbemErrPrivilegeNotHeld = $80041062;
  wbemErrInvalidOperator = $80041063;
  wbemErrLocalCredentials = $80041064;
  wbemErrCannotBeAbstract = $80041065;
  wbemErrAmendedObject = $80041066;
  wbemErrClientTooSlow = $80041067;
  wbemErrNullSecurityDescriptor = $80041068;
  wbemErrTimeout = $80041069;
  wbemErrInvalidAssociation = $8004106A;
  wbemErrAmbiguousOperation = $8004106B;
  wbemErrQuotaViolation = $8004106C;
  wbemErrTransactionConflict = $8004106D;
  wbemErrForcedRollback = $8004106E;
  wbemErrUnsupportedLocale = $8004106F;
  wbemErrHandleOutOfDate = $80041070;
  wbemErrConnectionFailed = $80041071;
  wbemErrInvalidHandleRequest = $80041072;
  wbemErrPropertyNameTooWide = $80041073;
  wbemErrClassNameTooWide = $80041074;
  wbemErrMethodNameTooWide = $80041075;
  wbemErrQualifierNameTooWide = $80041076;
  wbemErrRerunCommand = $80041077;
  wbemErrDatabaseVerMismatch = $80041078;
  wbemErrVetoPut = $80041079;
  wbemErrVetoDelete = $8004107A;
  wbemErrInvalidLocale = $80041080;
  wbemErrProviderSuspended = $80041081;
  wbemErrSynchronizationRequired = $80041082;
  wbemErrNoSchema = $80041083;
  wbemErrProviderAlreadyRegistered = $80041084;
  wbemErrProviderNotRegistered = $80041085;
  wbemErrFatalTransportError = $80041086;
  wbemErrEncryptedConnectionRequired = $80041087;
  wbemErrRegistrationTooBroad = $80042001;
  wbemErrRegistrationTooPrecise = $80042002;
  wbemErrTimedout = $80043001;
  wbemErrResetToDefault = $80043002;

// Constants for enum WbemObjectTextFormatEnum
type
  WbemObjectTextFormatEnum = TOleEnum;
const
  wbemObjectTextFormatCIMDTD20 = $00000001;
  wbemObjectTextFormatWMIDTD20 = $00000002;

// Constants for enum WbemChangeFlagEnum
type
  WbemChangeFlagEnum = TOleEnum;
const
  wbemChangeFlagCreateOrUpdate = $00000000;
  wbemChangeFlagUpdateOnly = $00000001;
  wbemChangeFlagCreateOnly = $00000002;
  wbemChangeFlagUpdateCompatible = $00000000;
  wbemChangeFlagUpdateSafeMode = $00000020;
  wbemChangeFlagUpdateForceMode = $00000040;
  wbemChangeFlagStrongValidation = $00000080;
  wbemChangeFlagAdvisory = $00010000;

// Constants for enum WbemFlagEnum
type
  WbemFlagEnum = TOleEnum;
const
  wbemFlagReturnImmediately = $00000010;
  wbemFlagReturnWhenComplete = $00000000;
  wbemFlagBidirectional = $00000000;
  wbemFlagForwardOnly = $00000020;
  wbemFlagNoErrorObject = $00000040;
  wbemFlagReturnErrorObject = $00000000;
  wbemFlagSendStatus = $00000080;
  wbemFlagDontSendStatus = $00000000;
  wbemFlagEnsureLocatable = $00000100;
  wbemFlagDirectRead = $00000200;
  wbemFlagSendOnlySelected = $00000000;
  wbemFlagUseAmendedQualifiers = $00020000;
  wbemFlagGetDefault = $00000000;
  wbemFlagSpawnInstance = $00000001;
  wbemFlagUseCurrentTime = $00000001;

// Constants for enum WbemQueryFlagEnum
type
  WbemQueryFlagEnum = TOleEnum;
const
  wbemQueryFlagDeep = $00000000;
  wbemQueryFlagShallow = $00000001;
  wbemQueryFlagPrototype = $00000002;

// Constants for enum WbemTextFlagEnum
type
  WbemTextFlagEnum = TOleEnum;
const
  wbemTextFlagNoFlavors = $00000001;

// Constants for enum WbemTimeout
type
  WbemTimeout = TOleEnum;
const
  wbemTimeoutInfinite = $FFFFFFFF;

// Constants for enum WbemComparisonFlagEnum
type
  WbemComparisonFlagEnum = TOleEnum;
const
  wbemComparisonFlagIncludeAll = $00000000;
  wbemComparisonFlagIgnoreQualifiers = $00000001;
  wbemComparisonFlagIgnoreObjectSource = $00000002;
  wbemComparisonFlagIgnoreDefaultValues = $00000004;
  wbemComparisonFlagIgnoreClass = $00000008;
  wbemComparisonFlagIgnoreCase = $00000010;
  wbemComparisonFlagIgnoreFlavor = $00000020;

// Constants for enum WbemConnectOptionsEnum
type
  WbemConnectOptionsEnum = TOleEnum;
const
  wbemConnectFlagUseMaxWait = $00000080;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  ISWbemServices = interface;
  ISWbemServicesDisp = dispinterface;
  ISWbemObject = interface;
  ISWbemObjectDisp = dispinterface;
  ISWbemObjectPath = interface;
  ISWbemObjectPathDisp = dispinterface;
  ISWbemNamedValueSet = interface;
  ISWbemNamedValueSetDisp = dispinterface;
  ISWbemNamedValue = interface;
  ISWbemNamedValueDisp = dispinterface;
  ISWbemSecurity = interface;
  ISWbemSecurityDisp = dispinterface;
  ISWbemPrivilegeSet = interface;
  ISWbemPrivilegeSetDisp = dispinterface;
  ISWbemPrivilege = interface;
  ISWbemPrivilegeDisp = dispinterface;
  ISWbemObjectSet = interface;
  ISWbemObjectSetDisp = dispinterface;
  ISWbemQualifierSet = interface;
  ISWbemQualifierSetDisp = dispinterface;
  ISWbemQualifier = interface;
  ISWbemQualifierDisp = dispinterface;
  ISWbemPropertySet = interface;
  ISWbemPropertySetDisp = dispinterface;
  ISWbemProperty = interface;
  ISWbemPropertyDisp = dispinterface;
  ISWbemMethodSet = interface;
  ISWbemMethodSetDisp = dispinterface;
  ISWbemMethod = interface;
  ISWbemMethodDisp = dispinterface;
  ISWbemEventSource = interface;
  ISWbemEventSourceDisp = dispinterface;
  ISWbemLocator = interface;
  ISWbemLocatorDisp = dispinterface;
  ISWbemLastError = interface;
  ISWbemLastErrorDisp = dispinterface;
  ISWbemSinkEvents = dispinterface;
  ISWbemSink = interface;
  ISWbemSinkDisp = dispinterface;
  ISWbemServicesEx = interface;
  ISWbemServicesExDisp = dispinterface;
  ISWbemObjectEx = interface;
  ISWbemObjectExDisp = dispinterface;
  ISWbemDateTime = interface;
  ISWbemDateTimeDisp = dispinterface;
  ISWbemRefresher = interface;
  ISWbemRefresherDisp = dispinterface;
  ISWbemRefreshableItem = interface;
  ISWbemRefreshableItemDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  SWbemLocator = ISWbemLocator;
  SWbemNamedValueSet = ISWbemNamedValueSet;
  SWbemObjectPath = ISWbemObjectPath;
  SWbemLastError = ISWbemLastError;
  SWbemSink = ISWbemSink;
  SWbemDateTime = ISWbemDateTime;
  SWbemRefresher = ISWbemRefresher;
  SWbemServices = ISWbemServices;
  SWbemServicesEx = ISWbemServicesEx;
  SWbemObject = ISWbemObject;
  SWbemObjectEx = ISWbemObjectEx;
  SWbemObjectSet = ISWbemObjectSet;
  SWbemNamedValue = ISWbemNamedValue;
  SWbemQualifier = ISWbemQualifier;
  SWbemQualifierSet = ISWbemQualifierSet;
  SWbemProperty = ISWbemProperty;
  SWbemPropertySet = ISWbemPropertySet;
  SWbemMethod = ISWbemMethod;
  SWbemMethodSet = ISWbemMethodSet;
  SWbemEventSource = ISWbemEventSource;
  SWbemSecurity = ISWbemSecurity;
  SWbemPrivilege = ISWbemPrivilege;
  SWbemPrivilegeSet = ISWbemPrivilegeSet;
  SWbemRefreshableItem = ISWbemRefreshableItem;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}


// *********************************************************************//
// Interface: ISWbemServices
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415C-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemServices = interface(IDispatch)
    ['{76A6415C-CB41-11D1-8B02-00600806D9B6}']
    function Get(const strObjectPath: WideString; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObject; safecall;
    procedure GetAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); safecall;
    procedure Delete(const strObjectPath: WideString; iFlags: Integer; 
                     const objWbemNamedValueSet: IDispatch); safecall;
    procedure DeleteAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                          iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                          const objWbemAsyncContext: IDispatch); safecall;
    function InstancesOf(const strClass: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure InstancesOfAsync(const objWbemSink: IDispatch; const strClass: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function SubclassesOf(const strSuperclass: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure SubclassesOfAsync(const objWbemSink: IDispatch; const strSuperclass: WideString; 
                                iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); safecall;
    function ExecQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure ExecQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                             const strQueryLanguage: WideString; lFlags: Integer; 
                             const objWbemNamedValueSet: IDispatch; 
                             const objWbemAsyncContext: IDispatch); safecall;
    function AssociatorsOf(const strObjectPath: WideString; const strAssocClass: WideString; 
                           const strResultClass: WideString; const strResultRole: WideString; 
                           const strRole: WideString; bClassesOnly: WordBool; 
                           bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                           const strRequiredQualifier: WideString; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure AssociatorsOfAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                 const strAssocClass: WideString; const strResultClass: WideString; 
                                 const strResultRole: WideString; const strRole: WideString; 
                                 bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                 const strRequiredAssocQualifier: WideString; 
                                 const strRequiredQualifier: WideString; iFlags: Integer; 
                                 const objWbemNamedValueSet: IDispatch; 
                                 const objWbemAsyncContext: IDispatch); safecall;
    function ReferencesTo(const strObjectPath: WideString; const strResultClass: WideString; 
                          const strRole: WideString; bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure ReferencesToAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                const strResultClass: WideString; const strRole: WideString; 
                                bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); safecall;
    function ExecNotificationQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                                   iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemEventSource; safecall;
    procedure ExecNotificationQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                                         const strQueryLanguage: WideString; iFlags: Integer; 
                                         const objWbemNamedValueSet: IDispatch; 
                                         const objWbemAsyncContext: IDispatch); safecall;
    function ExecMethod(const strObjectPath: WideString; const strMethodName: WideString; 
                        const objWbemInParameters: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch): ISWbemObject; safecall;
    procedure ExecMethodAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                              const strMethodName: WideString; 
                              const objWbemInParameters: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemServicesDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415C-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemServicesDisp = dispinterface
    ['{76A6415C-CB41-11D1-8B02-00600806D9B6}']
    function Get(const strObjectPath: WideString; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 1;
    procedure GetAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete(const strObjectPath: WideString; iFlags: Integer; 
                     const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                          iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                          const objWbemAsyncContext: IDispatch); dispid 4;
    function InstancesOf(const strClass: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesOfAsync(const objWbemSink: IDispatch; const strClass: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 6;
    function SubclassesOf(const strSuperclass: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesOfAsync(const objWbemSink: IDispatch; const strSuperclass: WideString; 
                                iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 8;
    function ExecQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure ExecQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                             const strQueryLanguage: WideString; lFlags: Integer; 
                             const objWbemNamedValueSet: IDispatch; 
                             const objWbemAsyncContext: IDispatch); dispid 10;
    function AssociatorsOf(const strObjectPath: WideString; const strAssocClass: WideString; 
                           const strResultClass: WideString; const strResultRole: WideString; 
                           const strRole: WideString; bClassesOnly: WordBool; 
                           bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                           const strRequiredQualifier: WideString; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure AssociatorsOfAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                 const strAssocClass: WideString; const strResultClass: WideString; 
                                 const strResultRole: WideString; const strRole: WideString; 
                                 bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                 const strRequiredAssocQualifier: WideString; 
                                 const strRequiredQualifier: WideString; iFlags: Integer; 
                                 const objWbemNamedValueSet: IDispatch; 
                                 const objWbemAsyncContext: IDispatch); dispid 12;
    function ReferencesTo(const strObjectPath: WideString; const strResultClass: WideString; 
                          const strRole: WideString; bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 13;
    procedure ReferencesToAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                const strResultClass: WideString; const strRole: WideString; 
                                bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 14;
    function ExecNotificationQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                                   iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemEventSource; dispid 15;
    procedure ExecNotificationQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                                         const strQueryLanguage: WideString; iFlags: Integer; 
                                         const objWbemNamedValueSet: IDispatch; 
                                         const objWbemAsyncContext: IDispatch); dispid 16;
    function ExecMethod(const strObjectPath: WideString; const strMethodName: WideString; 
                        const objWbemInParameters: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 17;
    procedure ExecMethodAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                              const strMethodName: WideString; 
                              const objWbemInParameters: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 18;
    property Security_: ISWbemSecurity readonly dispid 19;
  end;

// *********************************************************************//
// Interface: ISWbemObject
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415A-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObject = interface(IDispatch)
    ['{76A6415A-CB41-11D1-8B02-00600806D9B6}']
    function Put_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; safecall;
    procedure PutAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch; const objWbemAsyncContext: IDispatch); safecall;
    procedure Delete_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); safecall;
    procedure DeleteAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch; 
                           const objWbemAsyncContext: IDispatch); safecall;
    function Instances_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure InstancesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); safecall;
    function Subclasses_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure SubclassesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function Associators_(const strAssocClass: WideString; const strResultClass: WideString; 
                          const strResultRole: WideString; const strRole: WideString; 
                          bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredAssocQualifier: WideString; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure AssociatorsAsync_(const objWbemSink: IDispatch; const strAssocClass: WideString; 
                                const strResultClass: WideString; const strResultRole: WideString; 
                                const strRole: WideString; bClassesOnly: WordBool; 
                                bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); safecall;
    function References_(const strResultClass: WideString; const strRole: WideString; 
                         bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                         const strRequiredQualifier: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure ReferencesAsync_(const objWbemSink: IDispatch; const strResultClass: WideString; 
                               const strRole: WideString; bClassesOnly: WordBool; 
                               bSchemaOnly: WordBool; const strRequiredQualifier: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function ExecMethod_(const strMethodName: WideString; const objWbemInParameters: IDispatch; 
                         iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObject; safecall;
    procedure ExecMethodAsync_(const objWbemSink: IDispatch; const strMethodName: WideString; 
                               const objWbemInParameters: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function Clone_: ISWbemObject; safecall;
    function GetObjectText_(iFlags: Integer): WideString; safecall;
    function SpawnDerivedClass_(iFlags: Integer): ISWbemObject; safecall;
    function SpawnInstance_(iFlags: Integer): ISWbemObject; safecall;
    function CompareTo_(const objWbemObject: IDispatch; iFlags: Integer): WordBool; safecall;
    function Get_Qualifiers_: ISWbemQualifierSet; safecall;
    function Get_Properties_: ISWbemPropertySet; safecall;
    function Get_Methods_: ISWbemMethodSet; safecall;
    function Get_Derivation_: OleVariant; safecall;
    function Get_Path_: ISWbemObjectPath; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Qualifiers_: ISWbemQualifierSet read Get_Qualifiers_;
    property Properties_: ISWbemPropertySet read Get_Properties_;
    property Methods_: ISWbemMethodSet read Get_Methods_;
    property Derivation_: OleVariant read Get_Derivation_;
    property Path_: ISWbemObjectPath read Get_Path_;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415A-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObjectDisp = dispinterface
    ['{76A6415A-CB41-11D1-8B02-00600806D9B6}']
    function Put_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; dispid 1;
    procedure PutAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch; const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch; 
                           const objWbemAsyncContext: IDispatch); dispid 4;
    function Instances_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 6;
    function Subclasses_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 8;
    function Associators_(const strAssocClass: WideString; const strResultClass: WideString; 
                          const strResultRole: WideString; const strRole: WideString; 
                          bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredAssocQualifier: WideString; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure AssociatorsAsync_(const objWbemSink: IDispatch; const strAssocClass: WideString; 
                                const strResultClass: WideString; const strResultRole: WideString; 
                                const strRole: WideString; bClassesOnly: WordBool; 
                                bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 10;
    function References_(const strResultClass: WideString; const strRole: WideString; 
                         bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                         const strRequiredQualifier: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure ReferencesAsync_(const objWbemSink: IDispatch; const strResultClass: WideString; 
                               const strRole: WideString; bClassesOnly: WordBool; 
                               bSchemaOnly: WordBool; const strRequiredQualifier: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 12;
    function ExecMethod_(const strMethodName: WideString; const objWbemInParameters: IDispatch; 
                         iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 13;
    procedure ExecMethodAsync_(const objWbemSink: IDispatch; const strMethodName: WideString; 
                               const objWbemInParameters: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 14;
    function Clone_: ISWbemObject; dispid 15;
    function GetObjectText_(iFlags: Integer): WideString; dispid 16;
    function SpawnDerivedClass_(iFlags: Integer): ISWbemObject; dispid 17;
    function SpawnInstance_(iFlags: Integer): ISWbemObject; dispid 18;
    function CompareTo_(const objWbemObject: IDispatch; iFlags: Integer): WordBool; dispid 19;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 20;
    property Properties_: ISWbemPropertySet readonly dispid 21;
    property Methods_: ISWbemMethodSet readonly dispid 22;
    property Derivation_: OleVariant readonly dispid 23;
    property Path_: ISWbemObjectPath readonly dispid 24;
    property Security_: ISWbemSecurity readonly dispid 25;
  end;

// *********************************************************************//
// Interface: ISWbemObjectPath
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {5791BC27-CE9C-11D1-97BF-0000F81E849C}
// *********************************************************************//
  ISWbemObjectPath = interface(IDispatch)
    ['{5791BC27-CE9C-11D1-97BF-0000F81E849C}']
    function Get_Path: WideString; safecall;
    procedure Set_Path(const strPath: WideString); safecall;
    function Get_RelPath: WideString; safecall;
    procedure Set_RelPath(const strRelPath: WideString); safecall;
    function Get_Server: WideString; safecall;
    procedure Set_Server(const strServer: WideString); safecall;
    function Get_Namespace: WideString; safecall;
    procedure Set_Namespace(const strNamespace: WideString); safecall;
    function Get_ParentNamespace: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    procedure Set_DisplayName(const strDisplayName: WideString); safecall;
    function Get_Class_: WideString; safecall;
    procedure Set_Class_(const strClass: WideString); safecall;
    function Get_IsClass: WordBool; safecall;
    procedure SetAsClass; safecall;
    function Get_IsSingleton: WordBool; safecall;
    procedure SetAsSingleton; safecall;
    function Get_Keys: ISWbemNamedValueSet; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    function Get_Locale: WideString; safecall;
    procedure Set_Locale(const strLocale: WideString); safecall;
    function Get_Authority: WideString; safecall;
    procedure Set_Authority(const strAuthority: WideString); safecall;
    property Path: WideString read Get_Path write Set_Path;
    property RelPath: WideString read Get_RelPath write Set_RelPath;
    property Server: WideString read Get_Server write Set_Server;
    property Namespace: WideString read Get_Namespace write Set_Namespace;
    property ParentNamespace: WideString read Get_ParentNamespace;
    property DisplayName: WideString read Get_DisplayName write Set_DisplayName;
    property Class_: WideString read Get_Class_ write Set_Class_;
    property IsClass: WordBool read Get_IsClass;
    property IsSingleton: WordBool read Get_IsSingleton;
    property Keys: ISWbemNamedValueSet read Get_Keys;
    property Security_: ISWbemSecurity read Get_Security_;
    property Locale: WideString read Get_Locale write Set_Locale;
    property Authority: WideString read Get_Authority write Set_Authority;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectPathDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {5791BC27-CE9C-11D1-97BF-0000F81E849C}
// *********************************************************************//
  ISWbemObjectPathDisp = dispinterface
    ['{5791BC27-CE9C-11D1-97BF-0000F81E849C}']
    property Path: WideString dispid 0;
    property RelPath: WideString dispid 1;
    property Server: WideString dispid 2;
    property Namespace: WideString dispid 3;
    property ParentNamespace: WideString readonly dispid 4;
    property DisplayName: WideString dispid 5;
    property Class_: WideString dispid 6;
    property IsClass: WordBool readonly dispid 7;
    procedure SetAsClass; dispid 8;
    property IsSingleton: WordBool readonly dispid 9;
    procedure SetAsSingleton; dispid 10;
    property Keys: ISWbemNamedValueSet readonly dispid 11;
    property Security_: ISWbemSecurity readonly dispid 12;
    property Locale: WideString dispid 13;
    property Authority: WideString dispid 14;
  end;

// *********************************************************************//
// Interface: ISWbemNamedValueSet
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {CF2376EA-CE8C-11D1-8B05-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValueSet = interface(IDispatch)
    ['{CF2376EA-CE8C-11D1-8B05-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strName: WideString; iFlags: Integer): ISWbemNamedValue; safecall;
    function Get_Count: Integer; safecall;
    function Add(const strName: WideString; const varValue: OleVariant; iFlags: Integer): ISWbemNamedValue; safecall;
    procedure Remove(const strName: WideString; iFlags: Integer); safecall;
    function Clone: ISWbemNamedValueSet; safecall;
    procedure DeleteAll; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemNamedValueSetDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {CF2376EA-CE8C-11D1-8B05-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValueSetDisp = dispinterface
    ['{CF2376EA-CE8C-11D1-8B05-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strName: WideString; iFlags: Integer): ISWbemNamedValue; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const strName: WideString; const varValue: OleVariant; iFlags: Integer): ISWbemNamedValue; dispid 2;
    procedure Remove(const strName: WideString; iFlags: Integer); dispid 3;
    function Clone: ISWbemNamedValueSet; dispid 4;
    procedure DeleteAll; dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemNamedValue
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A64164-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValue = interface(IDispatch)
    ['{76A64164-CB41-11D1-8B02-00600806D9B6}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(const varValue: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    // Skipped Property "Value"
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  ISWbemNamedValueDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A64164-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValueDisp = dispinterface
    ['{76A64164-CB41-11D1-8B02-00600806D9B6}']
    function Value: OleVariant; dispid 0;
    property Name: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ISWbemSecurity
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B54D66E6-2287-11D2-8B33-00600806D9B6}
// *********************************************************************//
  ISWbemSecurity = interface(IDispatch)
    ['{B54D66E6-2287-11D2-8B33-00600806D9B6}']
    function Get_ImpersonationLevel: WbemImpersonationLevelEnum; safecall;
    procedure Set_ImpersonationLevel(iImpersonationLevel: WbemImpersonationLevelEnum); safecall;
    function Get_AuthenticationLevel: WbemAuthenticationLevelEnum; safecall;
    procedure Set_AuthenticationLevel(iAuthenticationLevel: WbemAuthenticationLevelEnum); safecall;
    function Get_Privileges: ISWbemPrivilegeSet; safecall;
    property ImpersonationLevel: WbemImpersonationLevelEnum read Get_ImpersonationLevel write Set_ImpersonationLevel;
    property AuthenticationLevel: WbemAuthenticationLevelEnum read Get_AuthenticationLevel write Set_AuthenticationLevel;
    property Privileges: ISWbemPrivilegeSet read Get_Privileges;
  end;

// *********************************************************************//
// DispIntf:  ISWbemSecurityDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B54D66E6-2287-11D2-8B33-00600806D9B6}
// *********************************************************************//
  ISWbemSecurityDisp = dispinterface
    ['{B54D66E6-2287-11D2-8B33-00600806D9B6}']
    property ImpersonationLevel: WbemImpersonationLevelEnum dispid 1;
    property AuthenticationLevel: WbemAuthenticationLevelEnum dispid 2;
    property Privileges: ISWbemPrivilegeSet readonly dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemPrivilegeSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BF-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilegeSet = interface(IDispatch)
    ['{26EE67BF-5804-11D2-8B4A-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(iPrivilege: WbemPrivilegeEnum): ISWbemPrivilege; safecall;
    function Get_Count: Integer; safecall;
    function Add(iPrivilege: WbemPrivilegeEnum; bIsEnabled: WordBool): ISWbemPrivilege; safecall;
    procedure Remove(iPrivilege: WbemPrivilegeEnum); safecall;
    procedure DeleteAll; safecall;
    function AddAsString(const strPrivilege: WideString; bIsEnabled: WordBool): ISWbemPrivilege; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPrivilegeSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BF-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilegeSetDisp = dispinterface
    ['{26EE67BF-5804-11D2-8B4A-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(iPrivilege: WbemPrivilegeEnum): ISWbemPrivilege; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(iPrivilege: WbemPrivilegeEnum; bIsEnabled: WordBool): ISWbemPrivilege; dispid 2;
    procedure Remove(iPrivilege: WbemPrivilegeEnum); dispid 3;
    procedure DeleteAll; dispid 4;
    function AddAsString(const strPrivilege: WideString; bIsEnabled: WordBool): ISWbemPrivilege; dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemPrivilege
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BD-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilege = interface(IDispatch)
    ['{26EE67BD-5804-11D2-8B4A-00600806D9B6}']
    function Get_IsEnabled: WordBool; safecall;
    procedure Set_IsEnabled(bIsEnabled: WordBool); safecall;
    function Get_Name: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_Identifier: WbemPrivilegeEnum; safecall;
    property IsEnabled: WordBool read Get_IsEnabled write Set_IsEnabled;
    property Name: WideString read Get_Name;
    property DisplayName: WideString read Get_DisplayName;
    property Identifier: WbemPrivilegeEnum read Get_Identifier;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPrivilegeDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BD-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilegeDisp = dispinterface
    ['{26EE67BD-5804-11D2-8B4A-00600806D9B6}']
    property IsEnabled: WordBool dispid 0;
    property Name: WideString readonly dispid 1;
    property DisplayName: WideString readonly dispid 2;
    property Identifier: WbemPrivilegeEnum readonly dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemObjectSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76A6415F-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObjectSet = interface(IDispatch)
    ['{76A6415F-CB41-11D1-8B02-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strObjectPath: WideString; iFlags: Integer): ISWbemObject; safecall;
    function Get_Count: Integer; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    function ItemIndex(lIndex: Integer): ISWbemObject; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76A6415F-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObjectSetDisp = dispinterface
    ['{76A6415F-CB41-11D1-8B02-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strObjectPath: WideString; iFlags: Integer): ISWbemObject; dispid 0;
    property Count: Integer readonly dispid 1;
    property Security_: ISWbemSecurity readonly dispid 4;
    function ItemIndex(lIndex: Integer): ISWbemObject; dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemQualifierSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9B16ED16-D3DF-11D1-8B08-00600806D9B6}
// *********************************************************************//
  ISWbemQualifierSet = interface(IDispatch)
    ['{9B16ED16-D3DF-11D1-8B08-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const Name: WideString; iFlags: Integer): ISWbemQualifier; safecall;
    function Get_Count: Integer; safecall;
    function Add(const strName: WideString; const varVal: OleVariant; 
                 bPropagatesToSubclass: WordBool; bPropagatesToInstance: WordBool; 
                 bIsOverridable: WordBool; iFlags: Integer): ISWbemQualifier; safecall;
    procedure Remove(const strName: WideString; iFlags: Integer); safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemQualifierSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9B16ED16-D3DF-11D1-8B08-00600806D9B6}
// *********************************************************************//
  ISWbemQualifierSetDisp = dispinterface
    ['{9B16ED16-D3DF-11D1-8B08-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const Name: WideString; iFlags: Integer): ISWbemQualifier; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const strName: WideString; const varVal: OleVariant; 
                 bPropagatesToSubclass: WordBool; bPropagatesToInstance: WordBool; 
                 bIsOverridable: WordBool; iFlags: Integer): ISWbemQualifier; dispid 2;
    procedure Remove(const strName: WideString; iFlags: Integer); dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemQualifier
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79B05932-D3B7-11D1-8B06-00600806D9B6}
// *********************************************************************//
  ISWbemQualifier = interface(IDispatch)
    ['{79B05932-D3B7-11D1-8B06-00600806D9B6}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(const varValue: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_IsLocal: WordBool; safecall;
    function Get_PropagatesToSubclass: WordBool; safecall;
    procedure Set_PropagatesToSubclass(bPropagatesToSubclass: WordBool); safecall;
    function Get_PropagatesToInstance: WordBool; safecall;
    procedure Set_PropagatesToInstance(bPropagatesToInstance: WordBool); safecall;
    function Get_IsOverridable: WordBool; safecall;
    procedure Set_IsOverridable(bIsOverridable: WordBool); safecall;
    function Get_IsAmended: WordBool; safecall;
    // Skipped Property "Value"
    property Name: WideString read Get_Name;
    property IsLocal: WordBool read Get_IsLocal;
    property PropagatesToSubclass: WordBool read Get_PropagatesToSubclass write Set_PropagatesToSubclass;
    property PropagatesToInstance: WordBool read Get_PropagatesToInstance write Set_PropagatesToInstance;
    property IsOverridable: WordBool read Get_IsOverridable write Set_IsOverridable;
    property IsAmended: WordBool read Get_IsAmended;
  end;

// *********************************************************************//
// DispIntf:  ISWbemQualifierDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79B05932-D3B7-11D1-8B06-00600806D9B6}
// *********************************************************************//
  ISWbemQualifierDisp = dispinterface
    ['{79B05932-D3B7-11D1-8B06-00600806D9B6}']
    function Value: OleVariant; dispid 0;
    property Name: WideString readonly dispid 1;
    property IsLocal: WordBool readonly dispid 2;
    property PropagatesToSubclass: WordBool dispid 3;
    property PropagatesToInstance: WordBool dispid 4;
    property IsOverridable: WordBool dispid 5;
    property IsAmended: WordBool readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ISWbemPropertySet
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemPropertySet = interface(IDispatch)
    ['{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strName: WideString; iFlags: Integer): ISWbemProperty; safecall;
    function Get_Count: Integer; safecall;
    function Add(const strName: WideString; iCimType: WbemCimtypeEnum; bIsArray: WordBool; 
                 iFlags: Integer): ISWbemProperty; safecall;
    procedure Remove(const strName: WideString; iFlags: Integer); safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPropertySetDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemPropertySetDisp = dispinterface
    ['{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strName: WideString; iFlags: Integer): ISWbemProperty; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const strName: WideString; iCimType: WbemCimtypeEnum; bIsArray: WordBool; 
                 iFlags: Integer): ISWbemProperty; dispid 2;
    procedure Remove(const strName: WideString; iFlags: Integer); dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemProperty
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1A388F98-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemProperty = interface(IDispatch)
    ['{1A388F98-D4BA-11D1-8B09-00600806D9B6}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(const varValue: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_IsLocal: WordBool; safecall;
    function Get_Origin: WideString; safecall;
    function Get_CIMType: WbemCimtypeEnum; safecall;
    function Get_Qualifiers_: ISWbemQualifierSet; safecall;
    function Get_IsArray: WordBool; safecall;
    // Skipped Property "Value"
    property Name: WideString read Get_Name;
    property IsLocal: WordBool read Get_IsLocal;
    property Origin: WideString read Get_Origin;
    property CIMType: WbemCimtypeEnum read Get_CIMType;
    property Qualifiers_: ISWbemQualifierSet read Get_Qualifiers_;
    property IsArray: WordBool read Get_IsArray;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPropertyDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1A388F98-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemPropertyDisp = dispinterface
    ['{1A388F98-D4BA-11D1-8B09-00600806D9B6}']
    function Value: OleVariant; dispid 0;
    property Name: WideString readonly dispid 1;
    property IsLocal: WordBool readonly dispid 2;
    property Origin: WideString readonly dispid 3;
    property CIMType: WbemCimtypeEnum readonly dispid 4;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 5;
    property IsArray: WordBool readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ISWbemMethodSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C93BA292-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethodSet = interface(IDispatch)
    ['{C93BA292-D955-11D1-8B09-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strName: WideString; iFlags: Integer): ISWbemMethod; safecall;
    function Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemMethodSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C93BA292-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethodSetDisp = dispinterface
    ['{C93BA292-D955-11D1-8B09-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strName: WideString; iFlags: Integer): ISWbemMethod; dispid 0;
    property Count: Integer readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ISWbemMethod
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {422E8E90-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethod = interface(IDispatch)
    ['{422E8E90-D955-11D1-8B09-00600806D9B6}']
    function Get_Name: WideString; safecall;
    function Get_Origin: WideString; safecall;
    function Get_InParameters: ISWbemObject; safecall;
    function Get_OutParameters: ISWbemObject; safecall;
    function Get_Qualifiers_: ISWbemQualifierSet; safecall;
    property Name: WideString read Get_Name;
    property Origin: WideString read Get_Origin;
    property InParameters: ISWbemObject read Get_InParameters;
    property OutParameters: ISWbemObject read Get_OutParameters;
    property Qualifiers_: ISWbemQualifierSet read Get_Qualifiers_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemMethodDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {422E8E90-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethodDisp = dispinterface
    ['{422E8E90-D955-11D1-8B09-00600806D9B6}']
    property Name: WideString readonly dispid 1;
    property Origin: WideString readonly dispid 2;
    property InParameters: ISWbemObject readonly dispid 3;
    property OutParameters: ISWbemObject readonly dispid 4;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemEventSource
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {27D54D92-0EBE-11D2-8B22-00600806D9B6}
// *********************************************************************//
  ISWbemEventSource = interface(IDispatch)
    ['{27D54D92-0EBE-11D2-8B22-00600806D9B6}']
    function NextEvent(iTimeoutMs: Integer): ISWbemObject; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemEventSourceDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {27D54D92-0EBE-11D2-8B22-00600806D9B6}
// *********************************************************************//
  ISWbemEventSourceDisp = dispinterface
    ['{27D54D92-0EBE-11D2-8B22-00600806D9B6}']
    function NextEvent(iTimeoutMs: Integer): ISWbemObject; dispid 1;
    property Security_: ISWbemSecurity readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ISWbemLocator
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415B-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemLocator = interface(IDispatch)
    ['{76A6415B-CB41-11D1-8B02-00600806D9B6}']
    function ConnectServer(const strServer: WideString; const strNamespace: WideString; 
                           const strUser: WideString; const strPassword: WideString; 
                           const strLocale: WideString; const strAuthority: WideString; 
                           iSecurityFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemServices; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemLocatorDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415B-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemLocatorDisp = dispinterface
    ['{76A6415B-CB41-11D1-8B02-00600806D9B6}']
    function ConnectServer(const strServer: WideString; const strNamespace: WideString; 
                           const strUser: WideString; const strPassword: WideString; 
                           const strLocale: WideString; const strAuthority: WideString; 
                           iSecurityFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemServices; dispid 1;
    property Security_: ISWbemSecurity readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ISWbemLastError
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D962DB84-D4BB-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemLastError = interface(ISWbemObject)
    ['{D962DB84-D4BB-11D1-8B09-00600806D9B6}']
  end;

// *********************************************************************//
// DispIntf:  ISWbemLastErrorDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {D962DB84-D4BB-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemLastErrorDisp = dispinterface
    ['{D962DB84-D4BB-11D1-8B09-00600806D9B6}']
    function Put_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; dispid 1;
    procedure PutAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch; const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch; 
                           const objWbemAsyncContext: IDispatch); dispid 4;
    function Instances_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 6;
    function Subclasses_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 8;
    function Associators_(const strAssocClass: WideString; const strResultClass: WideString; 
                          const strResultRole: WideString; const strRole: WideString; 
                          bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredAssocQualifier: WideString; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure AssociatorsAsync_(const objWbemSink: IDispatch; const strAssocClass: WideString; 
                                const strResultClass: WideString; const strResultRole: WideString; 
                                const strRole: WideString; bClassesOnly: WordBool; 
                                bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 10;
    function References_(const strResultClass: WideString; const strRole: WideString; 
                         bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                         const strRequiredQualifier: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure ReferencesAsync_(const objWbemSink: IDispatch; const strResultClass: WideString; 
                               const strRole: WideString; bClassesOnly: WordBool; 
                               bSchemaOnly: WordBool; const strRequiredQualifier: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 12;
    function ExecMethod_(const strMethodName: WideString; const objWbemInParameters: IDispatch; 
                         iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 13;
    procedure ExecMethodAsync_(const objWbemSink: IDispatch; const strMethodName: WideString; 
                               const objWbemInParameters: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 14;
    function Clone_: ISWbemObject; dispid 15;
    function GetObjectText_(iFlags: Integer): WideString; dispid 16;
    function SpawnDerivedClass_(iFlags: Integer): ISWbemObject; dispid 17;
    function SpawnInstance_(iFlags: Integer): ISWbemObject; dispid 18;
    function CompareTo_(const objWbemObject: IDispatch; iFlags: Integer): WordBool; dispid 19;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 20;
    property Properties_: ISWbemPropertySet readonly dispid 21;
    property Methods_: ISWbemMethodSet readonly dispid 22;
    property Derivation_: OleVariant readonly dispid 23;
    property Path_: ISWbemObjectPath readonly dispid 24;
    property Security_: ISWbemSecurity readonly dispid 25;
  end;

// *********************************************************************//
// DispIntf:  ISWbemSinkEvents
// Flags:     (4240) Hidden NonExtensible Dispatchable
// GUID:      {75718CA0-F029-11D1-A1AC-00C04FB6C223}
// *********************************************************************//
  ISWbemSinkEvents = dispinterface
    ['{75718CA0-F029-11D1-A1AC-00C04FB6C223}']
    procedure OnObjectReady(const objWbemObject: ISWbemObject; 
                            const objWbemAsyncContext: ISWbemNamedValueSet); dispid 1;
    procedure OnCompleted(iHResult: WbemErrorEnum; const objWbemErrorObject: ISWbemObject; 
                          const objWbemAsyncContext: ISWbemNamedValueSet); dispid 2;
    procedure OnProgress(iUpperBound: Integer; iCurrent: Integer; const strMessage: WideString; 
                         const objWbemAsyncContext: ISWbemNamedValueSet); dispid 3;
    procedure OnObjectPut(const objWbemObjectPath: ISWbemObjectPath; 
                          const objWbemAsyncContext: ISWbemNamedValueSet); dispid 4;
  end;

// *********************************************************************//
// Interface: ISWbemSink
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {75718C9F-F029-11D1-A1AC-00C04FB6C223}
// *********************************************************************//
  ISWbemSink = interface(IDispatch)
    ['{75718C9F-F029-11D1-A1AC-00C04FB6C223}']
    procedure Cancel; safecall;
  end;

// *********************************************************************//
// DispIntf:  ISWbemSinkDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {75718C9F-F029-11D1-A1AC-00C04FB6C223}
// *********************************************************************//
  ISWbemSinkDisp = dispinterface
    ['{75718C9F-F029-11D1-A1AC-00C04FB6C223}']
    procedure Cancel; dispid 1;
  end;

// *********************************************************************//
// Interface: ISWbemServicesEx
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D2F68443-85DC-427E-91D8-366554CC754C}
// *********************************************************************//
  ISWbemServicesEx = interface(ISWbemServices)
    ['{D2F68443-85DC-427E-91D8-366554CC754C}']
    function Put(const objWbemObject: ISWbemObjectEx; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; safecall;
    procedure PutAsync(const objWbemSink: ISWbemSink; const objWbemObject: ISWbemObjectEx; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); safecall;
  end;

// *********************************************************************//
// DispIntf:  ISWbemServicesExDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {D2F68443-85DC-427E-91D8-366554CC754C}
// *********************************************************************//
  ISWbemServicesExDisp = dispinterface
    ['{D2F68443-85DC-427E-91D8-366554CC754C}']
    function Put(const objWbemObject: ISWbemObjectEx; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; dispid 20;
    procedure PutAsync(const objWbemSink: ISWbemSink; const objWbemObject: ISWbemObjectEx; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); dispid 21;
    function Get(const strObjectPath: WideString; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 1;
    procedure GetAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete(const strObjectPath: WideString; iFlags: Integer; 
                     const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                          iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                          const objWbemAsyncContext: IDispatch); dispid 4;
    function InstancesOf(const strClass: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesOfAsync(const objWbemSink: IDispatch; const strClass: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 6;
    function SubclassesOf(const strSuperclass: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesOfAsync(const objWbemSink: IDispatch; const strSuperclass: WideString; 
                                iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 8;
    function ExecQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure ExecQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                             const strQueryLanguage: WideString; lFlags: Integer; 
                             const objWbemNamedValueSet: IDispatch; 
                             const objWbemAsyncContext: IDispatch); dispid 10;
    function AssociatorsOf(const strObjectPath: WideString; const strAssocClass: WideString; 
                           const strResultClass: WideString; const strResultRole: WideString; 
                           const strRole: WideString; bClassesOnly: WordBool; 
                           bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                           const strRequiredQualifier: WideString; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure AssociatorsOfAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                 const strAssocClass: WideString; const strResultClass: WideString; 
                                 const strResultRole: WideString; const strRole: WideString; 
                                 bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                 const strRequiredAssocQualifier: WideString; 
                                 const strRequiredQualifier: WideString; iFlags: Integer; 
                                 const objWbemNamedValueSet: IDispatch; 
                                 const objWbemAsyncContext: IDispatch); dispid 12;
    function ReferencesTo(const strObjectPath: WideString; const strResultClass: WideString; 
                          const strRole: WideString; bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 13;
    procedure ReferencesToAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                const strResultClass: WideString; const strRole: WideString; 
                                bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 14;
    function ExecNotificationQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                                   iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemEventSource; dispid 15;
    procedure ExecNotificationQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                                         const strQueryLanguage: WideString; iFlags: Integer; 
                                         const objWbemNamedValueSet: IDispatch; 
                                         const objWbemAsyncContext: IDispatch); dispid 16;
    function ExecMethod(const strObjectPath: WideString; const strMethodName: WideString; 
                        const objWbemInParameters: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 17;
    procedure ExecMethodAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                              const strMethodName: WideString; 
                              const objWbemInParameters: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 18;
    property Security_: ISWbemSecurity readonly dispid 19;
  end;

// *********************************************************************//
// Interface: ISWbemObjectEx
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {269AD56A-8A67-4129-BC8C-0506DCFE9880}
// *********************************************************************//
  ISWbemObjectEx = interface(ISWbemObject)
    ['{269AD56A-8A67-4129-BC8C-0506DCFE9880}']
    procedure Refresh_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); safecall;
    function Get_SystemProperties_: ISWbemPropertySet; safecall;
    function GetText_(iObjectTextFormat: WbemObjectTextFormatEnum; iFlags: Integer; 
                      const objWbemNamedValueSet: IDispatch): WideString; safecall;
    procedure SetFromText_(const bsText: WideString; iObjectTextFormat: WbemObjectTextFormatEnum; 
                           iFlags: Integer; const objWbemNamedValueSet: IDispatch); safecall;
    property SystemProperties_: ISWbemPropertySet read Get_SystemProperties_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectExDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {269AD56A-8A67-4129-BC8C-0506DCFE9880}
// *********************************************************************//
  ISWbemObjectExDisp = dispinterface
    ['{269AD56A-8A67-4129-BC8C-0506DCFE9880}']
    procedure Refresh_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); dispid 26;
    property SystemProperties_: ISWbemPropertySet readonly dispid 27;
    function GetText_(iObjectTextFormat: WbemObjectTextFormatEnum; iFlags: Integer; 
                      const objWbemNamedValueSet: IDispatch): WideString; dispid 28;
    procedure SetFromText_(const bsText: WideString; iObjectTextFormat: WbemObjectTextFormatEnum; 
                           iFlags: Integer; const objWbemNamedValueSet: IDispatch); dispid 29;
    function Put_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; dispid 1;
    procedure PutAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch; const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch; 
                           const objWbemAsyncContext: IDispatch); dispid 4;
    function Instances_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 6;
    function Subclasses_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 8;
    function Associators_(const strAssocClass: WideString; const strResultClass: WideString; 
                          const strResultRole: WideString; const strRole: WideString; 
                          bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredAssocQualifier: WideString; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure AssociatorsAsync_(const objWbemSink: IDispatch; const strAssocClass: WideString; 
                                const strResultClass: WideString; const strResultRole: WideString; 
                                const strRole: WideString; bClassesOnly: WordBool; 
                                bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 10;
    function References_(const strResultClass: WideString; const strRole: WideString; 
                         bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                         const strRequiredQualifier: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure ReferencesAsync_(const objWbemSink: IDispatch; const strResultClass: WideString; 
                               const strRole: WideString; bClassesOnly: WordBool; 
                               bSchemaOnly: WordBool; const strRequiredQualifier: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 12;
    function ExecMethod_(const strMethodName: WideString; const objWbemInParameters: IDispatch; 
                         iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 13;
    procedure ExecMethodAsync_(const objWbemSink: IDispatch; const strMethodName: WideString; 
                               const objWbemInParameters: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 14;
    function Clone_: ISWbemObject; dispid 15;
    function GetObjectText_(iFlags: Integer): WideString; dispid 16;
    function SpawnDerivedClass_(iFlags: Integer): ISWbemObject; dispid 17;
    function SpawnInstance_(iFlags: Integer): ISWbemObject; dispid 18;
    function CompareTo_(const objWbemObject: IDispatch; iFlags: Integer): WordBool; dispid 19;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 20;
    property Properties_: ISWbemPropertySet readonly dispid 21;
    property Methods_: ISWbemMethodSet readonly dispid 22;
    property Derivation_: OleVariant readonly dispid 23;
    property Path_: ISWbemObjectPath readonly dispid 24;
    property Security_: ISWbemSecurity readonly dispid 25;
  end;

// *********************************************************************//
// Interface: ISWbemDateTime
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5E97458A-CF77-11D3-B38F-00105A1F473A}
// *********************************************************************//
  ISWbemDateTime = interface(IDispatch)
    ['{5E97458A-CF77-11D3-B38F-00105A1F473A}']
    function Get_Value: WideString; safecall;
    procedure Set_Value(const strValue: WideString); safecall;
    function Get_Year: Integer; safecall;
    procedure Set_Year(iYear: Integer); safecall;
    function Get_YearSpecified: WordBool; safecall;
    procedure Set_YearSpecified(bYearSpecified: WordBool); safecall;
    function Get_Month: Integer; safecall;
    procedure Set_Month(iMonth: Integer); safecall;
    function Get_MonthSpecified: WordBool; safecall;
    procedure Set_MonthSpecified(bMonthSpecified: WordBool); safecall;
    function Get_Day: Integer; safecall;
    procedure Set_Day(iDay: Integer); safecall;
    function Get_DaySpecified: WordBool; safecall;
    procedure Set_DaySpecified(bDaySpecified: WordBool); safecall;
    function Get_Hours: Integer; safecall;
    procedure Set_Hours(iHours: Integer); safecall;
    function Get_HoursSpecified: WordBool; safecall;
    procedure Set_HoursSpecified(bHoursSpecified: WordBool); safecall;
    function Get_Minutes: Integer; safecall;
    procedure Set_Minutes(iMinutes: Integer); safecall;
    function Get_MinutesSpecified: WordBool; safecall;
    procedure Set_MinutesSpecified(bMinutesSpecified: WordBool); safecall;
    function Get_Seconds: Integer; safecall;
    procedure Set_Seconds(iSeconds: Integer); safecall;
    function Get_SecondsSpecified: WordBool; safecall;
    procedure Set_SecondsSpecified(bSecondsSpecified: WordBool); safecall;
    function Get_Microseconds: Integer; safecall;
    procedure Set_Microseconds(iMicroseconds: Integer); safecall;
    function Get_MicrosecondsSpecified: WordBool; safecall;
    procedure Set_MicrosecondsSpecified(bMicrosecondsSpecified: WordBool); safecall;
    function Get_UTC: Integer; safecall;
    procedure Set_UTC(iUTC: Integer); safecall;
    function Get_UTCSpecified: WordBool; safecall;
    procedure Set_UTCSpecified(bUTCSpecified: WordBool); safecall;
    function Get_IsInterval: WordBool; safecall;
    procedure Set_IsInterval(bIsInterval: WordBool); safecall;
    function GetVarDate(bIsLocal: WordBool): TDateTime; safecall;
    procedure SetVarDate(dVarDate: TDateTime; bIsLocal: WordBool); safecall;
    function GetFileTime(bIsLocal: WordBool): WideString; safecall;
    procedure SetFileTime(const strFileTime: WideString; bIsLocal: WordBool); safecall;
    property Value: WideString read Get_Value write Set_Value;
    property Year: Integer read Get_Year write Set_Year;
    property YearSpecified: WordBool read Get_YearSpecified write Set_YearSpecified;
    property Month: Integer read Get_Month write Set_Month;
    property MonthSpecified: WordBool read Get_MonthSpecified write Set_MonthSpecified;
    property Day: Integer read Get_Day write Set_Day;
    property DaySpecified: WordBool read Get_DaySpecified write Set_DaySpecified;
    property Hours: Integer read Get_Hours write Set_Hours;
    property HoursSpecified: WordBool read Get_HoursSpecified write Set_HoursSpecified;
    property Minutes: Integer read Get_Minutes write Set_Minutes;
    property MinutesSpecified: WordBool read Get_MinutesSpecified write Set_MinutesSpecified;
    property Seconds: Integer read Get_Seconds write Set_Seconds;
    property SecondsSpecified: WordBool read Get_SecondsSpecified write Set_SecondsSpecified;
    property Microseconds: Integer read Get_Microseconds write Set_Microseconds;
    property MicrosecondsSpecified: WordBool read Get_MicrosecondsSpecified write Set_MicrosecondsSpecified;
    property UTC: Integer read Get_UTC write Set_UTC;
    property UTCSpecified: WordBool read Get_UTCSpecified write Set_UTCSpecified;
    property IsInterval: WordBool read Get_IsInterval write Set_IsInterval;
  end;

// *********************************************************************//
// DispIntf:  ISWbemDateTimeDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5E97458A-CF77-11D3-B38F-00105A1F473A}
// *********************************************************************//
  ISWbemDateTimeDisp = dispinterface
    ['{5E97458A-CF77-11D3-B38F-00105A1F473A}']
    property Value: WideString dispid 0;
    property Year: Integer dispid 1;
    property YearSpecified: WordBool dispid 2;
    property Month: Integer dispid 3;
    property MonthSpecified: WordBool dispid 4;
    property Day: Integer dispid 5;
    property DaySpecified: WordBool dispid 6;
    property Hours: Integer dispid 7;
    property HoursSpecified: WordBool dispid 8;
    property Minutes: Integer dispid 9;
    property MinutesSpecified: WordBool dispid 10;
    property Seconds: Integer dispid 11;
    property SecondsSpecified: WordBool dispid 12;
    property Microseconds: Integer dispid 13;
    property MicrosecondsSpecified: WordBool dispid 14;
    property UTC: Integer dispid 15;
    property UTCSpecified: WordBool dispid 16;
    property IsInterval: WordBool dispid 17;
    function GetVarDate(bIsLocal: WordBool): TDateTime; dispid 18;
    procedure SetVarDate(dVarDate: TDateTime; bIsLocal: WordBool); dispid 19;
    function GetFileTime(bIsLocal: WordBool): WideString; dispid 20;
    procedure SetFileTime(const strFileTime: WideString; bIsLocal: WordBool); dispid 21;
  end;

// *********************************************************************//
// Interface: ISWbemRefresher
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {14D8250E-D9C2-11D3-B38F-00105A1F473A}
// *********************************************************************//
  ISWbemRefresher = interface(IDispatch)
    ['{14D8250E-D9C2-11D3-B38F-00105A1F473A}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(iIndex: Integer): ISWbemRefreshableItem; safecall;
    function Get_Count: Integer; safecall;
    function Add(const objWbemServices: ISWbemServicesEx; const bsInstancePath: WideString; 
                 iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemRefreshableItem; safecall;
    function AddEnum(const objWbemServices: ISWbemServicesEx; const bsClassName: WideString; 
                     iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemRefreshableItem; safecall;
    procedure Remove(iIndex: Integer; iFlags: Integer); safecall;
    procedure Refresh(iFlags: Integer); safecall;
    function Get_AutoReconnect: WordBool; safecall;
    procedure Set_AutoReconnect(bCount: WordBool); safecall;
    procedure DeleteAll; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property AutoReconnect: WordBool read Get_AutoReconnect write Set_AutoReconnect;
  end;

// *********************************************************************//
// DispIntf:  ISWbemRefresherDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {14D8250E-D9C2-11D3-B38F-00105A1F473A}
// *********************************************************************//
  ISWbemRefresherDisp = dispinterface
    ['{14D8250E-D9C2-11D3-B38F-00105A1F473A}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(iIndex: Integer): ISWbemRefreshableItem; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const objWbemServices: ISWbemServicesEx; const bsInstancePath: WideString; 
                 iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemRefreshableItem; dispid 2;
    function AddEnum(const objWbemServices: ISWbemServicesEx; const bsClassName: WideString; 
                     iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemRefreshableItem; dispid 3;
    procedure Remove(iIndex: Integer; iFlags: Integer); dispid 4;
    procedure Refresh(iFlags: Integer); dispid 5;
    property AutoReconnect: WordBool dispid 6;
    procedure DeleteAll; dispid 7;
  end;

// *********************************************************************//
// Interface: ISWbemRefreshableItem
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5AD4BF92-DAAB-11D3-B38F-00105A1F473A}
// *********************************************************************//
  ISWbemRefreshableItem = interface(IDispatch)
    ['{5AD4BF92-DAAB-11D3-B38F-00105A1F473A}']
    function Get_Index: Integer; safecall;
    function Get_Refresher: ISWbemRefresher; safecall;
    function Get_IsSet: WordBool; safecall;
    function Get_Object_: ISWbemObjectEx; safecall;
    function Get_ObjectSet: ISWbemObjectSet; safecall;
    procedure Remove(iFlags: Integer); safecall;
    property Index: Integer read Get_Index;
    property Refresher: ISWbemRefresher read Get_Refresher;
    property IsSet: WordBool read Get_IsSet;
    property Object_: ISWbemObjectEx read Get_Object_;
    property ObjectSet: ISWbemObjectSet read Get_ObjectSet;
  end;

// *********************************************************************//
// DispIntf:  ISWbemRefreshableItemDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {5AD4BF92-DAAB-11D3-B38F-00105A1F473A}
// *********************************************************************//
  ISWbemRefreshableItemDisp = dispinterface
    ['{5AD4BF92-DAAB-11D3-B38F-00105A1F473A}']
    property Index: Integer readonly dispid 1;
    property Refresher: ISWbemRefresher readonly dispid 2;
    property IsSet: WordBool readonly dispid 3;
    property Object_: ISWbemObjectEx readonly dispid 4;
    property ObjectSet: ISWbemObjectSet readonly dispid 5;
    procedure Remove(iFlags: Integer); dispid 6;
  end;

// *********************************************************************//
// The Class CoSWbemLocator provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemLocator exposed by              
// the CoClass SWbemLocator. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemLocator = class
    class function Create: ISWbemLocator;
    class function CreateRemote(const MachineName: string): ISWbemLocator;
  end;

// *********************************************************************//
// The Class CoSWbemNamedValueSet provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemNamedValueSet exposed by              
// the CoClass SWbemNamedValueSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemNamedValueSet = class
    class function Create: ISWbemNamedValueSet;
    class function CreateRemote(const MachineName: string): ISWbemNamedValueSet;
  end;

// *********************************************************************//
// The Class CoSWbemObjectPath provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemObjectPath exposed by              
// the CoClass SWbemObjectPath. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemObjectPath = class
    class function Create: ISWbemObjectPath;
    class function CreateRemote(const MachineName: string): ISWbemObjectPath;
  end;

// *********************************************************************//
// The Class CoSWbemLastError provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemLastError exposed by              
// the CoClass SWbemLastError. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemLastError = class
    class function Create: ISWbemLastError;
    class function CreateRemote(const MachineName: string): ISWbemLastError;
  end;

// *********************************************************************//
// The Class CoSWbemSink provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemSink exposed by              
// the CoClass SWbemSink. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemSink = class
    class function Create: ISWbemSink;
    class function CreateRemote(const MachineName: string): ISWbemSink;
  end;

// *********************************************************************//
// The Class CoSWbemDateTime provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemDateTime exposed by              
// the CoClass SWbemDateTime. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemDateTime = class
    class function Create: ISWbemDateTime;
    class function CreateRemote(const MachineName: string): ISWbemDateTime;
  end;

// *********************************************************************//
// The Class CoSWbemRefresher provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemRefresher exposed by              
// the CoClass SWbemRefresher. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemRefresher = class
    class function Create: ISWbemRefresher;
    class function CreateRemote(const MachineName: string): ISWbemRefresher;
  end;

// *********************************************************************//
// The Class CoSWbemServices provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemServices exposed by              
// the CoClass SWbemServices. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemServices = class
    class function Create: ISWbemServices;
    class function CreateRemote(const MachineName: string): ISWbemServices;
  end;

// *********************************************************************//
// The Class CoSWbemServicesEx provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemServicesEx exposed by              
// the CoClass SWbemServicesEx. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemServicesEx = class
    class function Create: ISWbemServicesEx;
    class function CreateRemote(const MachineName: string): ISWbemServicesEx;
  end;

// *********************************************************************//
// The Class CoSWbemObject provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemObject exposed by              
// the CoClass SWbemObject. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemObject = class
    class function Create: ISWbemObject;
    class function CreateRemote(const MachineName: string): ISWbemObject;
  end;

// *********************************************************************//
// The Class CoSWbemObjectEx provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemObjectEx exposed by              
// the CoClass SWbemObjectEx. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemObjectEx = class
    class function Create: ISWbemObjectEx;
    class function CreateRemote(const MachineName: string): ISWbemObjectEx;
  end;

// *********************************************************************//
// The Class CoSWbemObjectSet provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemObjectSet exposed by              
// the CoClass SWbemObjectSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemObjectSet = class
    class function Create: ISWbemObjectSet;
    class function CreateRemote(const MachineName: string): ISWbemObjectSet;
  end;

// *********************************************************************//
// The Class CoSWbemNamedValue provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemNamedValue exposed by              
// the CoClass SWbemNamedValue. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemNamedValue = class
    class function Create: ISWbemNamedValue;
    class function CreateRemote(const MachineName: string): ISWbemNamedValue;
  end;

// *********************************************************************//
// The Class CoSWbemQualifier provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemQualifier exposed by              
// the CoClass SWbemQualifier. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemQualifier = class
    class function Create: ISWbemQualifier;
    class function CreateRemote(const MachineName: string): ISWbemQualifier;
  end;

// *********************************************************************//
// The Class CoSWbemQualifierSet provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemQualifierSet exposed by              
// the CoClass SWbemQualifierSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemQualifierSet = class
    class function Create: ISWbemQualifierSet;
    class function CreateRemote(const MachineName: string): ISWbemQualifierSet;
  end;

// *********************************************************************//
// The Class CoSWbemProperty provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemProperty exposed by              
// the CoClass SWbemProperty. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemProperty = class
    class function Create: ISWbemProperty;
    class function CreateRemote(const MachineName: string): ISWbemProperty;
  end;

// *********************************************************************//
// The Class CoSWbemPropertySet provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemPropertySet exposed by              
// the CoClass SWbemPropertySet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemPropertySet = class
    class function Create: ISWbemPropertySet;
    class function CreateRemote(const MachineName: string): ISWbemPropertySet;
  end;

// *********************************************************************//
// The Class CoSWbemMethod provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemMethod exposed by              
// the CoClass SWbemMethod. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemMethod = class
    class function Create: ISWbemMethod;
    class function CreateRemote(const MachineName: string): ISWbemMethod;
  end;

// *********************************************************************//
// The Class CoSWbemMethodSet provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemMethodSet exposed by              
// the CoClass SWbemMethodSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemMethodSet = class
    class function Create: ISWbemMethodSet;
    class function CreateRemote(const MachineName: string): ISWbemMethodSet;
  end;

// *********************************************************************//
// The Class CoSWbemEventSource provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemEventSource exposed by              
// the CoClass SWbemEventSource. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemEventSource = class
    class function Create: ISWbemEventSource;
    class function CreateRemote(const MachineName: string): ISWbemEventSource;
  end;

// *********************************************************************//
// The Class CoSWbemSecurity provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemSecurity exposed by              
// the CoClass SWbemSecurity. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemSecurity = class
    class function Create: ISWbemSecurity;
    class function CreateRemote(const MachineName: string): ISWbemSecurity;
  end;

// *********************************************************************//
// The Class CoSWbemPrivilege provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemPrivilege exposed by              
// the CoClass SWbemPrivilege. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemPrivilege = class
    class function Create: ISWbemPrivilege;
    class function CreateRemote(const MachineName: string): ISWbemPrivilege;
  end;

// *********************************************************************//
// The Class CoSWbemPrivilegeSet provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemPrivilegeSet exposed by              
// the CoClass SWbemPrivilegeSet. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemPrivilegeSet = class
    class function Create: ISWbemPrivilegeSet;
    class function CreateRemote(const MachineName: string): ISWbemPrivilegeSet;
  end;

// *********************************************************************//
// The Class CoSWbemRefreshableItem provides a Create and CreateRemote method to          
// create instances of the default interface ISWbemRefreshableItem exposed by              
// the CoClass SWbemRefreshableItem. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoSWbemRefreshableItem = class
    class function Create: ISWbemRefreshableItem;
    class function CreateRemote(const MachineName: string): ISWbemRefreshableItem;
  end;

implementation

uses System.Win.ComObj;

class function CoSWbemLocator.Create: ISWbemLocator;
begin
  Result := CreateComObject(CLASS_SWbemLocator) as ISWbemLocator;
end;

class function CoSWbemLocator.CreateRemote(const MachineName: string): ISWbemLocator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemLocator) as ISWbemLocator;
end;

class function CoSWbemNamedValueSet.Create: ISWbemNamedValueSet;
begin
  Result := CreateComObject(CLASS_SWbemNamedValueSet) as ISWbemNamedValueSet;
end;

class function CoSWbemNamedValueSet.CreateRemote(const MachineName: string): ISWbemNamedValueSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemNamedValueSet) as ISWbemNamedValueSet;
end;

class function CoSWbemObjectPath.Create: ISWbemObjectPath;
begin
  Result := CreateComObject(CLASS_SWbemObjectPath) as ISWbemObjectPath;
end;

class function CoSWbemObjectPath.CreateRemote(const MachineName: string): ISWbemObjectPath;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemObjectPath) as ISWbemObjectPath;
end;

class function CoSWbemLastError.Create: ISWbemLastError;
begin
  Result := CreateComObject(CLASS_SWbemLastError) as ISWbemLastError;
end;

class function CoSWbemLastError.CreateRemote(const MachineName: string): ISWbemLastError;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemLastError) as ISWbemLastError;
end;

class function CoSWbemSink.Create: ISWbemSink;
begin
  Result := CreateComObject(CLASS_SWbemSink) as ISWbemSink;
end;

class function CoSWbemSink.CreateRemote(const MachineName: string): ISWbemSink;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemSink) as ISWbemSink;
end;

class function CoSWbemDateTime.Create: ISWbemDateTime;
begin
  Result := CreateComObject(CLASS_SWbemDateTime) as ISWbemDateTime;
end;

class function CoSWbemDateTime.CreateRemote(const MachineName: string): ISWbemDateTime;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemDateTime) as ISWbemDateTime;
end;

class function CoSWbemRefresher.Create: ISWbemRefresher;
begin
  Result := CreateComObject(CLASS_SWbemRefresher) as ISWbemRefresher;
end;

class function CoSWbemRefresher.CreateRemote(const MachineName: string): ISWbemRefresher;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemRefresher) as ISWbemRefresher;
end;

class function CoSWbemServices.Create: ISWbemServices;
begin
  Result := CreateComObject(CLASS_SWbemServices) as ISWbemServices;
end;

class function CoSWbemServices.CreateRemote(const MachineName: string): ISWbemServices;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemServices) as ISWbemServices;
end;

class function CoSWbemServicesEx.Create: ISWbemServicesEx;
begin
  Result := CreateComObject(CLASS_SWbemServicesEx) as ISWbemServicesEx;
end;

class function CoSWbemServicesEx.CreateRemote(const MachineName: string): ISWbemServicesEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemServicesEx) as ISWbemServicesEx;
end;

class function CoSWbemObject.Create: ISWbemObject;
begin
  Result := CreateComObject(CLASS_SWbemObject) as ISWbemObject;
end;

class function CoSWbemObject.CreateRemote(const MachineName: string): ISWbemObject;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemObject) as ISWbemObject;
end;

class function CoSWbemObjectEx.Create: ISWbemObjectEx;
begin
  Result := CreateComObject(CLASS_SWbemObjectEx) as ISWbemObjectEx;
end;

class function CoSWbemObjectEx.CreateRemote(const MachineName: string): ISWbemObjectEx;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemObjectEx) as ISWbemObjectEx;
end;

class function CoSWbemObjectSet.Create: ISWbemObjectSet;
begin
  Result := CreateComObject(CLASS_SWbemObjectSet) as ISWbemObjectSet;
end;

class function CoSWbemObjectSet.CreateRemote(const MachineName: string): ISWbemObjectSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemObjectSet) as ISWbemObjectSet;
end;

class function CoSWbemNamedValue.Create: ISWbemNamedValue;
begin
  Result := CreateComObject(CLASS_SWbemNamedValue) as ISWbemNamedValue;
end;

class function CoSWbemNamedValue.CreateRemote(const MachineName: string): ISWbemNamedValue;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemNamedValue) as ISWbemNamedValue;
end;

class function CoSWbemQualifier.Create: ISWbemQualifier;
begin
  Result := CreateComObject(CLASS_SWbemQualifier) as ISWbemQualifier;
end;

class function CoSWbemQualifier.CreateRemote(const MachineName: string): ISWbemQualifier;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemQualifier) as ISWbemQualifier;
end;

class function CoSWbemQualifierSet.Create: ISWbemQualifierSet;
begin
  Result := CreateComObject(CLASS_SWbemQualifierSet) as ISWbemQualifierSet;
end;

class function CoSWbemQualifierSet.CreateRemote(const MachineName: string): ISWbemQualifierSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemQualifierSet) as ISWbemQualifierSet;
end;

class function CoSWbemProperty.Create: ISWbemProperty;
begin
  Result := CreateComObject(CLASS_SWbemProperty) as ISWbemProperty;
end;

class function CoSWbemProperty.CreateRemote(const MachineName: string): ISWbemProperty;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemProperty) as ISWbemProperty;
end;

class function CoSWbemPropertySet.Create: ISWbemPropertySet;
begin
  Result := CreateComObject(CLASS_SWbemPropertySet) as ISWbemPropertySet;
end;

class function CoSWbemPropertySet.CreateRemote(const MachineName: string): ISWbemPropertySet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemPropertySet) as ISWbemPropertySet;
end;

class function CoSWbemMethod.Create: ISWbemMethod;
begin
  Result := CreateComObject(CLASS_SWbemMethod) as ISWbemMethod;
end;

class function CoSWbemMethod.CreateRemote(const MachineName: string): ISWbemMethod;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemMethod) as ISWbemMethod;
end;

class function CoSWbemMethodSet.Create: ISWbemMethodSet;
begin
  Result := CreateComObject(CLASS_SWbemMethodSet) as ISWbemMethodSet;
end;

class function CoSWbemMethodSet.CreateRemote(const MachineName: string): ISWbemMethodSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemMethodSet) as ISWbemMethodSet;
end;

class function CoSWbemEventSource.Create: ISWbemEventSource;
begin
  Result := CreateComObject(CLASS_SWbemEventSource) as ISWbemEventSource;
end;

class function CoSWbemEventSource.CreateRemote(const MachineName: string): ISWbemEventSource;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemEventSource) as ISWbemEventSource;
end;

class function CoSWbemSecurity.Create: ISWbemSecurity;
begin
  Result := CreateComObject(CLASS_SWbemSecurity) as ISWbemSecurity;
end;

class function CoSWbemSecurity.CreateRemote(const MachineName: string): ISWbemSecurity;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemSecurity) as ISWbemSecurity;
end;

class function CoSWbemPrivilege.Create: ISWbemPrivilege;
begin
  Result := CreateComObject(CLASS_SWbemPrivilege) as ISWbemPrivilege;
end;

class function CoSWbemPrivilege.CreateRemote(const MachineName: string): ISWbemPrivilege;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemPrivilege) as ISWbemPrivilege;
end;

class function CoSWbemPrivilegeSet.Create: ISWbemPrivilegeSet;
begin
  Result := CreateComObject(CLASS_SWbemPrivilegeSet) as ISWbemPrivilegeSet;
end;

class function CoSWbemPrivilegeSet.CreateRemote(const MachineName: string): ISWbemPrivilegeSet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemPrivilegeSet) as ISWbemPrivilegeSet;
end;

class function CoSWbemRefreshableItem.Create: ISWbemRefreshableItem;
begin
  Result := CreateComObject(CLASS_SWbemRefreshableItem) as ISWbemRefreshableItem;
end;

class function CoSWbemRefreshableItem.CreateRemote(const MachineName: string): ISWbemRefreshableItem;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_SWbemRefreshableItem) as ISWbemRefreshableItem;
end;

end.
