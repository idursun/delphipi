{*------------------------------------------------------------------------------
  GNU gettext translation system for Delphi, Kylix, C++ Builder and others.
  All parts of the translation system are kept in this unit.

  @author Lars B. Dybdahl and others
  @version $LastChangedRevision: 163 $
  @see http://dybdahl.dk/dxgettext/
-------------------------------------------------------------------------------}
unit gnugettext;
(**************************************************************)
(*                                                            *)
(*  (C) Copyright by Lars B. Dybdahl and others               *)
(*  E-mail: Lars@dybdahl.dk, phone +45 70201241               *)
(*                                                            *)
(*  Contributors: Peter Thornqvist, Troy Wolbrink,            *)
(*                Frank Andreas de Groot, Igor Siticov,       *)
(*                Jacques Garcia Vazquez, Igor Gitman         *)
(*                                                            *)
(*  See http://dybdahl.dk/dxgettext/ for more information     *)
(*                                                            *)
(**************************************************************)

// Information about this file:
// $LastChangedDate: 2008-09-29 22:45:20 +0200 (Mon, 29 Sep 2008) $
// $LastChangedRevision: 163 $
// $HeadURL: file:///svnroot/repos/dxgettext/trunk/dxgettext/sample/gnugettext.pas $

// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// The names of any contributor may not be used to endorse or promote
// products derived from this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

interface

// If the conditional define DXGETTEXTDEBUG is defined, debugging log is activated.
// Use DefaultInstance.DebugLogToFile() to write the log to a file.
{ $define DXGETTEXTDEBUG}

{$ifdef VER100}
  // Delphi 3
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER110}
  // C++ Builder 3
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER120}
  // Delphi 4
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER125}
  // C++ Builder 4
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER130}
  // Delphi 5
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI2007OROLDER}
  {$ifdef WIN32}
  {$DEFINE MSWINDOWS}
  {$endif}
{$endif}
{$ifdef VER135}
  // C++ Builder 5
  {$DEFINE DELPHI5OROLDER}
  {$DEFINE DELPHI6OROLDER}
  {$DEFINE DELPHI2007OROLDER}
  {$ifdef WIN32}
  {$DEFINE MSWINDOWS}
  {$endif}
{$endif}
{$ifdef VER140}
  // Delphi 6
  {$DEFINE DELPHI2007OROLDER}
{$ifdef MSWINDOWS}
  {$DEFINE DELPHI6OROLDER}
{$endif}
{$endif}
{$ifdef VER150}
  // Delphi 7
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER160}
  // Delphi 8
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER170}
  // Delphi 2005
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER180}
  // Delphi 2006
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER190}
  // Delphi 2007
  {$DEFINE DELPHI2007OROLDER}
{$endif}
{$ifdef VER200}
  // Delphi 2009 with Unicode
{$endif}

uses
{$ifdef DELPHI5OROLDER}
  gnugettextD5,
{$endif}
{$ifdef MSWINDOWS}
  Windows,
{$else}
  Libc,
{$ifdef FPC}
  CWString,
{$endif}
{$endif}
  Classes, StrUtils, SysUtils, TypInfo;

(*****************************************************************************)
(*                                                                           *)
(*  MAIN API                                                                 *)
(*                                                                           *)
(*****************************************************************************)

type
  {$IFNDEF UNICODE}
  UnicodeString=WideString;
  RawUtf8String=AnsiString;
  {$ELSE}
  RawUtf8String=RawByteString;
  {$ENDIF}
  DomainString=string;
  LanguageString=string;
  ComponentNameString=string;
  FilenameString=string;
  MsgIdString=UnicodeString;
  TranslatedUnicodeString=UnicodeString;

// Main GNU gettext functions. See documentation for instructions on how to use them.
function _(const szMsgId: MsgIdString): TranslatedUnicodeString;
procedure TranslateComponent(const comp:TComponent);
implementation

procedure TranslateComponent(const comp:TComponent);
begin

end;


function _(const szMsgId: string): string;
begin
  Result := szMsgId;
end;


end.
