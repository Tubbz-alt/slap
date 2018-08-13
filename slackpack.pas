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

	(*
	 * PACKAGE information
	 *)
	PKG = Object
	public
		Name  : String;		{ Name }
		FName : String;		{ File-name }
		Desc  : StrList;	{ multi-line description }
		Vars  : StrList;	{ other variables }
		Repos : StrList;	{ list of repositories }
		Vers  : StrList;	{ list of versions on repositories }
		bInst : Boolean;	{ true if it is installed }
		
		Constructor Init(key, filename : String);
		Constructor Init2(key, filename : String; txt, opts : StrList);
		Destructor  Free; virtual;
	End;

Type
	(*
	 * Slackware Package DataBase 
	 *)
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
		Procedure	LoadPackageList; { /var/log/packages }
		Procedure	LoadPackageDataFile(fname : String; var data : PKGPtr);
		Function	LoadPACKAGESFile : Boolean;
		Function	LoadRepoDataFile : Boolean;
		Function	IsDigit(ch : Char) : Boolean;
		Function	IsVerStr(s : String) : Boolean;
		Function	GetShortName(fileName : String) : String;

	public
		Constructor Init(debmsgs : Boolean = false);
		Destructor  Free; virtual;		
	End;

(* --- *)
Implementation

(*
 * Initialize package information object
 *)
Constructor PKG.Init2(key, filename : String; txt, opts : StrList);
Begin
	bInst := false;
	Name  := key;
	FName := filename;
	Desc.Copy(txt);
	Vars.Copy(opts);
	Repos.Init;
	Vers.Init;
End;

Constructor PKG.Init(key, filename : String);
Begin
	bInst := false;
	Name  := key;
	FName := filename;
	Desc.Init;
	Vars.Init;
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
 * Returns true if the ch is a decimal digit
 *)
Function SlackwarePDB.IsDigit(ch : Char) : Boolean;
Begin
	IsDigit := ((ch >= #48) and (ch <= #57))
End;

(*
 * Returns true if the string (until the end or '-') its seems to be version string
 *)
Function SlackwarePDB.IsVerStr(s : String) : Boolean;
Var	i, l : Integer;
Begin
	IsVerStr := true;
	l := Length(s);
	FOR i := 1 TO l DO Begin
		If s[i] = '-' THEN
			Break;
		If s[i] = '.' THEN
			Continue;
		If NOT IsDigit(s[i]) then Begin
			IsVerStr := false;
			Break;
		End;
	End
End;

(*
 * Returns the package name from a package fileName
 *)
Function SlackwarePDB.GetShortName(fileName : String) : String;
Var	buf, name : String;
	idx, lastidx : Integer;
Begin
	buf  := fileName;
	name := buf;
	lastidx := 0;

	Repeat
		idx  := Pos(#45, buf);
		if (IsDigit(buf[1])) and (lastidx > 0) then Begin
			if IsVerStr(buf) then
				break;
			End;
		lastidx := lastidx + idx;
		buf := Copy(buf, idx + 1, 255);
	Until idx = 0;
				
	if lastidx > 0 then
		name := Copy(name, 1, lastidx - 1)
	else
		name := buf;
	
	GetShortName := name;
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

(*
 * Load data from 'package file' and store them inside the 'data' node
 *)
Procedure SlackwarePDB.LoadPackageDataFile(fname : String; var data : PKGPtr);
Var	tf : TextFile;
	buf, key, txt	: String;
	fullname		: String;
	idx				: Integer;
Begin
	fullname := Concat(PKGDataDir, '/', fname);
	If FileExists(fullname) then Begin
		Assign(tf, fullname);
		{$I-}Reset(tf);{$I+}
		If IOResult = 0 then Begin
			While not EOF(tf) do Begin
				ReadLn(tf, buf);
				idx := Pos(Char(':'), buf);
				if idx > 0 then Begin
					key := Copy(buf, 1, idx - 1);
					txt := Trim(Copy(buf, idx + 1, 255));
					IF key = 'PACKAGE NAME' THEN
						Continue
					ELSE IF key = 'COMPRESSED PACKAGE SIZE' THEN
						data^.Vars.Add(Concat('CSIZE=', txt))
					ELSE IF key = 'UNCOMPRESSED PACKAGE SIZE' THEN
						data^.Vars.Add(Concat('USIZE=', txt))
					ELSE IF key = 'PACKAGE LOCATION' THEN
						data^.Vars.Add(Concat('LOCATION=', txt))
					ELSE IF key = 'PACKAGE DESCRIPTION' THEN
						Continue
					ELSE IF key = 'FILE LIST' THEN
						BREAK
					ELSE BEGIN
						data^.Desc.Add(txt);
					END
				END
				Else
					BREAK
			End; { While !EOF }
			
			Close(tf)
		End Else Begin
			if verbose then
				WriteLn(fullname, ': cannot open file.');
		End
	End Else Begin
		if verbose then
			WriteLn(fullname, ': file does not exists.');
		End
End;

(*
 * Load every package from /var/log/packages
 *)
Procedure SlackwarePDB.LoadPackageList;
Var	fileList : StrList;
	name	: String;
	cur		: StrListNodePtr;
	node	: BTreeNodePtr;
Begin
	fileList.Init;

	IF verbose THEN
		WriteLn('Processing ', PKGDataDir, ' ...');
	
	GetFileList(fileList, PKGDataDir);

	cur := fileList.Head;
	While cur <> NIL do Begin
		name := GetShortName(cur^.Key);
		node := packs.Find(name);
		if node <> NIL then Begin
			if verbose then
				WriteLn(PKGDataDir, ': Installed package ', name, ' already found in PDB!');
		End Else Begin
			node := packs.Insert(name, New(PKGPtr, Init(name, cur^.Key)));
			PKGPtr(node^.Ptr)^.bInst := true;
			LoadPackageDataFile(PKGPtr(node^.Ptr)^.FName, PKGPtr(node^.Ptr))
		End;
		cur := cur^.Next;
	End; { While }
	fileList.Free;
End;

(*
 * Initialize package database
 *)
Constructor SlackwarePDB.Init(debmsgs : Boolean);
Begin
	verbose := debmsgs;

	reposlist.Init;
	instlist.Init;
	packs.Init;
	
	LoadPackageList;
	If LoadPACKAGESFile then Begin
		If LoadRepoDataFile then Begin
			if verbose then
				WriteLn('* DONE *');
		End	Else
			WriteLn(REPDataFile, ': FAILED!');
	End	Else
		WriteLn(PKGDataFile, ': FAILED!');
End;

(*
 * Cleanup package database 
 *)
Destructor SlackwarePDB.Free;
Begin
	packs.Free;
	instlist.Free;
	reposlist.Free;
End;

(*
 *  Load packages information into the memory
 *)
Function SlackwarePDB.LoadPACKAGESFile : Boolean;
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

	if verbose then
		WriteLn('Loading ', PKGDataFile, ' ...');

	LoadPACKAGESFile := False;
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
						End;
					End Else Begin
						Continue
					End
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
						End Else Begin
							pkg_name := key;
							pkg_desc.Add(txt);
						End
					End Else Begin
						recBlock := false;
						{ store data }
						node := packs.Find(pkg_name);
						If node = NIL then
							packs.Insert(pkg_name, New(PKGPtr, Init2(pkg_name, pkg_fname, pkg_desc, pkg_opts)))
						Else Begin
							data := node^.Ptr;
							data^.Desc.Assign(pkg_desc);
							data^.Vars.Assign(pkg_opts);
						End
					End
				End
			End; { While }
			Close(tf);
			LoadPACKAGESFile := True;
		End
	End
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

	if verbose then
		WriteLn('Loading ', REPDataFile, ' ...');

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

(* --- *)
END.
