// CodeGear C++ Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Vtaccessibilityfactory.pas' rev: 11.00

#ifndef VtaccessibilityfactoryHPP
#define VtaccessibilityfactoryHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Oleacc.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Virtualtrees.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vtaccessibilityfactory
{
//-- type declarations -------------------------------------------------------
__interface IVTAccessibleProvider;
typedef System::DelphiInterface<IVTAccessibleProvider> _di_IVTAccessibleProvider;
__interface IVTAccessibleProvider  : public IInterface 
{
	
public:
	virtual _di_IAccessible __fastcall CreateIAccessible(Virtualtrees::TBaseVirtualTree* ATree) = 0 ;
};

class DELPHICLASS TVTAccessibilityFactory;
class PASCALIMPLEMENTATION TVTAccessibilityFactory : public System::TObject 
{
	typedef System::TObject inherited;
	
private:
	Classes::TInterfaceList* FAccessibleProviders;
	
public:
	__fastcall TVTAccessibilityFactory(void);
	__fastcall virtual ~TVTAccessibilityFactory(void);
	_di_IAccessible __fastcall CreateIAccessible(Virtualtrees::TBaseVirtualTree* ATree);
	void __fastcall RegisterAccessibleProvider(_di_IVTAccessibleProvider AProvider);
	void __fastcall UnRegisterAccessibleProvider(_di_IVTAccessibleProvider AProvider);
};


//-- var, const, procedure ---------------------------------------------------
extern PACKAGE TVTAccessibilityFactory* __fastcall GetAccessibilityFactory(void);

}	/* namespace Vtaccessibilityfactory */
using namespace Vtaccessibilityfactory;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vtaccessibilityfactory
