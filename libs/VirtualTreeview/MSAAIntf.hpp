// CodeGear C++ Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Msaaintf.pas' rev: 11.00

#ifndef MsaaintfHPP
#define MsaaintfHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <Activex.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------
#include <oleacc.h>
typedef DelphiInterface<IAccessible> _di_IAccessible;
#pragma link "oleacc.lib"

namespace Msaaintf
{
//-- type declarations -------------------------------------------------------
typedef Byte *PByte1;

typedef GUID *PUserType1;

struct __MIDL_IWinTypes_0009
{
	
	union
	{
		struct 
		{
			int hRemote;
			
		};
		struct 
		{
			int hInproc;
			
		};
		
	};
} ;

#pragma pack(push,1)
struct _RemotableHandle
{
	
public:
	int fContext;
	__MIDL_IWinTypes_0009 u;
} ;
#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
static const Shortint AccessibilityMajorVersion = 0x1;
static const Shortint AccessibilityMinorVersion = 0x1;
extern PACKAGE bool __fastcall InitAccLibrary(void);
extern PACKAGE void __fastcall FreeAccLibrary(void);

}	/* namespace Msaaintf */
using namespace Msaaintf;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Msaaintf
