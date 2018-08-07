(* kate: default-dictionary en; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
 * 
 *	Slackware packages database
 * 
 * This file is part of 'slap' project.
 * 'Slap' is an Automated packaging tool for Slackware Linux.
 * Copyright (C) 2018 Nicholas Christopoulos
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Project Page: https://github.com/nereusx/slap
 * Nicholas Christopoulos (nereus@freemail.gr)
 *)
{$MODE OBJFPC}
{$CODEPAGE UTF8}

Unit Slackpack;

Interface
Uses SysUtils, SBTree, SList, RegExpr;

Type LongString = UTF8String;

Const
	PKGDataDir  = '/var/log/packages';
	PKGDataFile = '/var/lib/slackpkg/PACKAGES.TXT';
	REPDataFile = '/var/lib/slackpkg/pkglist';
	SPP_SIGN    = 'SLACKPKGPLUS_';
	SPP_SIGN_L  = 13;

Type
	PKGPtr = ^PKG;
	PKG = Object
	public
		Name  : String;
		FName : String;
		Desc  : StrList;
		Vars  : StrList;
		Repos : StrList;
		Vers  : StrList;
		bInst : Boolean;
		
		Constructor Init(key, filename : String; txt, opts : StrList);
		Destructor  Free; virtual;
	End;

Type
	SlackwarePDB = Object
	private
		verbose		: Boolean;

	public
		packs		: BTree;	{ package db }
		reposlist	: StrList;	{ repository list }
		instlist	: StrList;	{ installed packages }

	private
		Procedure	GetFileList(var lst : StrList; dirx : String; pattern : String = '*');
		Procedure	Split(str : String; delim : String; Var v : StrList);
		Function	LoadDataFile : Boolean;
		Function	LoadRepoDataFile : Boolean;
		Function	IsDigit(ch : Char) : Boolean;

	public
		Constructor Init(debmsgs : Boolean = false);
		Destructor  Free; virtual;		
	End;

(* --- *)
Implementation

(*
 * Initialize package information object
 *)
Constructor PKG.Init(key, filename : String; txt, opts : StrList);
Begin
	bInst := false;
	Name  := key;
	FName := filename;
	Desc.Copy(txt);
	Vars.Copy(opts);
	Repos.Init;
	Vers.Init;
End;

(*
 * Cleanup package object
 *)
Destructor PKG.Free;
Begin
	Desc.Free;
	Vars.Free;
	Repos.Free;
	Vers.Free;
End;

(*
 *)
Function SlackwarePDB.IsDigit(ch : Char) : Boolean;
Begin
	IsDigit := ((ch >= #48) and (ch <= #57))
End;

(*
 * Initialize package database
 *)
Constructor SlackwarePDB.Init(debmsgs : Boolean);
Var	cur		: StrListNodePtr;
	node	: BTreeNodePtr;
	data	: PKGPtr;
	name, buf : String;
	lastidx, idx  : Integer;
Begin
	verbose := debmsgs;

	reposlist.Init;
	instlist.Init;
	packs.Init;
	
	if verbose then
		WriteLn('Loading ', PKGDataFile, ' ...');
	If LoadDataFile then Begin
		if verbose then
			WriteLn('Loading ', REPDataFile, ' ...');
		If LoadRepoDataFile then Begin
			{ mark installed packages }
			GetFileList(instlist, PKGDataDir);
			cur := instlist.Head;
			while cur <> NIL do begin

				buf  := cur^.Key;
				name := buf;
				lastidx := 0;

				Repeat
					idx  := Pos(#45, buf);
					if (IsDigit(buf[1])) and (lastidx > 0) then
						break;
					lastidx := lastidx + idx;
					buf := Copy(buf, idx + 1, 255);
				Until idx = 0;
				
				if lastidx > 0 then
					name := Copy(name, 1, lastidx - 1)
				else
					name := buf;
				
				node := packs.Find(name);
				if node = NIL then
					WriteLn('Installed package ', name, ' not found in PDB.')
					{ add this package }
				else Begin
					data := node^.Ptr;
					data^.bInst := true;
				End;
				cur := cur^.Next;
			end;
			{ done }
			if verbose then
				WriteLn('* DONE *');
		End	Else
			WriteLn(REPDataFile, ' FAILED!');
	End	Else
		WriteLn(PKGDataFile, ' FAILED!');
End;

(*
 * Cleanup package database 
 *)
Destructor SlackwarePDB.Free;
Begin
	packs.Free;
	instlist.Free;
	reposlist.Free;
	inherited;
End;

(*
 *  Load packages information into the memory
 *)
Function SlackwarePDB.LoadDataFile : Boolean;
Var
	tf : TextFile;
	buf, key, txt, wrd, pkg_name, pkg_fname : String;
	pkg_desc, pkg_opts : StrList;
	idx : Integer;
	recBlock, hasKey : Boolean;
	node : BTreeNodePtr;
	data : PKGPtr;

Begin
	pkg_desc.Init;
	pkg_opts.Init;
	LoadDataFile := False;
	If FileExists(PKGDataFile) then Begin
		Assign(tf, PKGDataFile);
		{$I-}Reset(tf);{$I+}
		If IOResult = 0 then Begin
			recBlock := false;
			While not EOF(tf) do Begin
				ReadLn(tf, buf);

				idx := Pos(Char(':'), buf);
				hasKey := (idx <> 0); (* the line starts with keyword *)

				{ enable recording }
				If NOT recBlock then Begin
					If hasKey then Begin
						key := Copy(buf, 1, idx - 1);
						If key = 'PACKAGE NAME' then Begin
							recBlock := true;
							pkg_opts.Clear;
							pkg_name := '';
							pkg_fname := Trim(Copy(buf, idx + 1, 255));
							pkg_desc.Clear;
						End
					End
					Else
						Continue
					End;

				{ if recording is enabled }
				If recBlock then Begin
					If hasKey then Begin
						key := Copy(buf, 1, idx - 1);
						txt := Trim(Copy(buf, idx + 1, 255));
						
						If Copy(buf, 1, 7) = 'PACKAGE' then Begin
							if Length(txt) > 0 then Begin
								wrd := Copy(key, 9, 255);
								if wrd = 'SIZE (compressed)' then
									wrd := 'CSIZE'
								else if wrd = 'SIZE (uncompressed)' then
									wrd := 'USIZE';
								pkg_opts.Add(Concat(wrd, '=', txt));
							End
						End
						Else Begin
							pkg_name := key;
							pkg_desc.Add(txt);
						End
					End
					Else { line has no keyword }
					Begin
						recBlock := false;
						{ store data }
						node := packs.Find(pkg_name);
						If node = NIL then
							packs.Insert(pkg_name, New(PKGPtr, Init(pkg_name, pkg_fname, pkg_desc, pkg_opts)))
						Else Begin
							data := node^.Ptr;
							if ( verbose ) then
								WriteLn('WARNING: Package ', data^.FName, ' found again as ', pkg_fname);
						End 
					End
				End
			End;
			Close(tf);			
			LoadDataFile := True
		End
	End
End;

(*
 * Split text line 'str' to words and fills the array of strings 'v'
 *)
Procedure SlackwarePDB.Split(str : String; delim : String; Var v : StrList);
Var	idx, l : Integer;
	left : String;
Begin
	l := Length(str);
	Repeat
		idx := Pos(delim, str);
		if idx <> 0 then begin
			left := Copy(str, 1, idx - 1);
			str  := Trim(Copy(str, idx + 1, l));
			v.push(left);
		End
	Until idx = 0;
	if Length(str) > 0 then
		v.push(str);
End;

(*
 *  Load additional packages information into the memory (pkglist)
 *)
Function SlackwarePDB.LoadRepoDataFile : Boolean;
Var
	tf	: TextFile;
	buf, repo, name, vers : String;
{	fname, ext : String; }
	v	: StrList;
	cur	: StrListNodePtr;
	node : BTreeNodePtr;
	data : PKGPtr;

Begin
	v.Init;
	LoadRepoDataFile := False;
	If FileExists(REPDataFile) then Begin
		Assign(tf, REPDataFile);
		{$I-}Reset(tf);{$I+}
		If IOResult = 0 then Begin
			While not EOF(tf) do Begin
				ReadLn(tf, buf);
				Split(buf, #32, v);
				if v.Count > 5 then Begin
					cur := v.Head;		repo  := v.Head^.Key;
					cur := cur^.Next;	name  := cur^.Key;
					cur := cur^.Next;	vers  := cur^.Key;
(*					cur := cur^.Next;	{arch  := cur^.Key; }
					cur := cur^.Next;	{???   := cur^.Key; }
					cur := cur^.Next;	fname  := cur^.Key;
					cur := cur^.Next;	{???   := cur^.Key; }
					cur := cur^.Next;	ext    := cur^.Key; *)
					node := packs.Find(name);
					if node = NIL then Begin
						if verbose then
							WriteLn('WARNING: The package ', name, ' not found.')
					End
					Else Begin
						if Copy(repo, 1, SPP_SIGN_L) = SPP_SIGN then
							repo := Copy(repo, SPP_SIGN_L + 1, 255);
						data := node^.Ptr;
						data^.Repos.Add(repo);
						data^.Vers.Add(vers);
						if NOT (repo IN reposlist) then
							reposlist.Add(repo);
					End
				End;
				v.Clear;
			End;
			Close(tf);
			LoadRepoDataFile := True
		End
	End
End;

(*
 * Get all filenames of directory
 *)
Procedure SlackwarePDB.GetFileList(var lst : StrList; dirx : String; pattern : String);
Var	Info : TSearchRec;
	prevDir : String;
Begin
	prevDir := GetCurrentDir;
	SetCurrentDir(dirx);
	If FindFirst(pattern, faAnyFile AND faReadOnly, Info) = 0 then
	Begin
		Repeat
			lst.push(Info.Name);
		Until FindNext(info)<>0;
	End;
	FindClose(Info);
	SetCurrentDir(prevDir);
End;

End.

(* --- *)
END.
