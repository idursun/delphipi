// CodeGear C++ Builder
// Copyright (c) 1995, 2007 by CodeGear
// All rights reserved

// (DO NOT EDIT: machine generated header) 'Vtheaderpopup.pas' rev: 11.00

#ifndef VtheaderpopupHPP
#define VtheaderpopupHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member functions
#pragma pack(push,8)
#include <System.hpp>	// Pascal unit
#include <Sysinit.hpp>	// Pascal unit
#include <Menus.hpp>	// Pascal unit
#include <Virtualtrees.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Vtheaderpopup
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TVTHeaderPopupOption { poOriginalOrder, poAllowHideAll };
#pragma option pop

typedef Set<TVTHeaderPopupOption, poOriginalOrder, poAllowHideAll>  TVTHeaderPopupOptions;

#pragma option push -b-
enum TAddPopupItemType { apNormal, apDisabled, apHidden };
#pragma option pop

typedef void __fastcall (__closure *TAddHeaderPopupItemEvent)(const Virtualtrees::TBaseVirtualTree* Sender, const Virtualtrees::TColumnIndex Column, TAddPopupItemType &Cmd);

typedef void __fastcall (__closure *TColumnChangeEvent)(const Virtualtrees::TBaseVirtualTree* Sender, const Virtualtrees::TColumnIndex Column, bool Visible);

typedef TMenuItem TVTMenuItem;
;

class DELPHICLASS TVTHeaderPopupMenu;
class PASCALIMPLEMENTATION TVTHeaderPopupMenu : public Menus::TPopupMenu 
{
	typedef Menus::TPopupMenu inherited;
	
private:
	TVTHeaderPopupOptions FOptions;
	TAddHeaderPopupItemEvent FOnAddHeaderPopupItem;
	TColumnChangeEvent FOnColumnChange;
	
protected:
	virtual void __fastcall DoAddHeaderPopupItem(const Virtualtrees::TColumnIndex Column, /* out */ TAddPopupItemType &Cmd);
	virtual void __fastcall DoColumnChange(Virtualtrees::TColumnIndex Column, bool Visible);
	void __fastcall OnMenuItemClick(System::TObject* Sender);
	
public:
	virtual void __fastcall Popup(int x, int y);
	
__published:
	__property TVTHeaderPopupOptions Options = {read=FOptions, write=FOptions, default=0};
	__property TAddHeaderPopupItemEvent OnAddHeaderPopupItem = {read=FOnAddHeaderPopupItem, write=FOnAddHeaderPopupItem};
	__property TColumnChangeEvent OnColumnChange = {read=FOnColumnChange, write=FOnColumnChange};
public:
	#pragma option push -w-inl
	/* TPopupMenu.Create */ inline __fastcall virtual TVTHeaderPopupMenu(Classes::TComponent* AOwner) : Menus::TPopupMenu(AOwner) { }
	#pragma option pop
	#pragma option push -w-inl
	/* TPopupMenu.Destroy */ inline __fastcall virtual ~TVTHeaderPopupMenu(void) { }
	#pragma option pop
	
};


//-- var, const, procedure ---------------------------------------------------

}	/* namespace Vtheaderpopup */
using namespace Vtheaderpopup;
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Vtheaderpopup
