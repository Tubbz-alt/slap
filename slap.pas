(* kate: default-dictionary en; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
 * 
 * Slap main CLI
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
 * Nicholas Christopoulos nereus@freemail.gr
 * 
 *)
{$MODE OBJFPC}
{$CODEPAGE UTF8}

Program Slap;

Uses SysUtils, SBTree, SList, RegExpr;

Const
	PKGDataDir  = '/var/lib/slackpkg';
	PKGDataFile = '/var/lib/slackpkg/PACKAGES.TXT';
	REPDataFile = '/var/lib/slackpkg/pkglist';
	SPP_SIGN    = 'SLACKPKGPLUS_';
	SPP_SIGN_L  = 13;
	AppVersion  = '1.00';

Type LongString = UTF8String;
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
		
		Constructor Init(key, filename : String; txt, opts : StrList);
		Destructor  Free; virtual;
	End;

Var
	packs		: BTree;
	reposlist	: StrList;

	{ CLI parameters }
	opt_verbose, opt_bsearch, opt_desc_bsearch, opt_brepo : Boolean;
	opt_names, opt_repolist : Boolean;
	opt_search, opt_desc_search, opt_repo : String;

Constructor PKG.Init(key, filename : String; txt, opts : StrList);
Begin
	Name  := key;
	FName := filename;
	Desc.Copy(txt);
	Vars.Copy(opts);
	Repos.Init;
	Vers.Init;
End;

Destructor PKG.Free;
Begin
	Desc.Free;
	Vars.Free;
	Repos.Free;
	Vers.Free;
End;

(*
 *  Load packages information into the memory
 *)
Function LoadDataFile : Boolean;
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
				hasKey := (idx <> 0); { the line starts with keyword }

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
							packs.Insert(pkg_name, New(PKGPtr, init(pkg_name, pkg_fname, pkg_desc, pkg_opts)) )
						Else Begin
							data := node^.Ptr;
							if ( opt_verbose ) then
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
Procedure Split(str : String; delim : String; Var v : StrList);
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
Function LoadRepoDataFile : Boolean;
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
						if opt_verbose then
							WriteLn('WARNING: The package ', name, ' not found.')
					End
					Else Begin
						if Copy(repo, 1, SPP_SIGN_L) = SPP_SIGN then
							repo := Copy(repo, SPP_SIGN_L + 1, 255);
						data := node^.Ptr;
						data^.Repos.Add(repo);
						data^.Vers.Add(vers);
						if NOT reposlist.Contains(repo) then
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
 * Parse command-line parameters
 *)
Procedure ParseCLIParams;
Var	i : Integer;
	opt : String;

	Procedure PrintHelp;
	Begin
		WriteLn('slap, Slackware Package Explorer');
		WriteLn;
		WriteLn('Usage: slap [options]...');
		WriteLn;
		WriteLn('  -h, --help		Prints this screen and exits.');
		WriteLn('  -v, --version		Prints the version information and exits.');
		WriteLn('  -d, --debug		Prints verbose messages.');
		WriteLn('  -s, --search pattern	Search information about a package. (1)');
		WriteLn('  -sd, --search-desc pattern');
		WriteLn('			Search in descriptions. (1)');
		WriteLn('  -n, --names		Display package names only instead of full information.');
		WriteLn('  -rl, --repo-list	Display a list of repositories.');
		WriteLn('  -rp, --repo-pkg-list repo');
		WriteLn('			Display all packages from the specified repository.');
		WriteLn;
		WriteLn('(1): Regular expressions are supported.');
		Halt(0);
	End;

	Procedure PrintVersion;
	Begin
		WriteLn('slap v', AppVersion);
		WriteLn('Copyright (C) 2018 Free Software Foundation, Inc.');
		WriteLn('License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>.');
		WriteLn('This is free software: you are free to change and redistribute it.');
		WriteLn('There is NO WARRANTY, to the extent permitted by law.');
		WriteLn;
		WriteLn('Written by Nicholas Christopoulos <mailto:nereus@freemail.gr>.');
		Halt(0);
	End;

Begin
	{ WriteLn('Executable: ', ParamStr(0)); }
	For i := 1 to ParamCount do begin
		{WriteLn('Param ', i, ': ', ParamStr(i));}
		opt := ParamStr(i);
		if Copy(opt, 1, 1) = '-' then begin
			if (opt = '-h') OR (opt = '--help') then
				PrintHelp
			Else If (opt = '-v') OR (opt = '--version') then
				PrintVersion
			Else If (opt = '-d') OR (opt = '--debug') then
				opt_verbose := true
			Else If (opt = '-n') OR (opt = '--names') then
				opt_names := true
			Else If (opt = '-s') OR (opt = '--search') then Begin
				if ParamCount >= i + 1 then Begin
					opt_bsearch := true;
					opt_search := ParamStr(i+1);
				End
				Else Begin
					WriteLn('Error: Missing search pattern');
					Halt(1);
				End
			End
			Else If (opt = '-sd') OR (opt = '--search-desc') then Begin
				if ParamCount >= i + 1 then Begin
					opt_desc_bsearch := true;
					opt_desc_search := ParamStr(i+1);
				End
				Else Begin
					WriteLn('Error: Missing search pattern');
					Halt(1);
				End
			End
			Else If (opt = '-rl') OR (opt = '--repo-list' ) then
				opt_repolist := true
			Else If (opt = '-rp') OR (opt = '--repo-pkg-list') then Begin
				if ParamCount >= i + 1 then Begin
					opt_brepo := true;
					opt_repo := ParamStr(i+1);
				End
				Else Begin
					WriteLn('Error: Missing repository name');
					Halt(1);
				End
			End
			Else Begin
				WriteLn('Error: Unknown option ', opt);
				Halt(1)
			End
		End
	End
End;

(*
 * Displays package information
 *)
Procedure PrintPackage(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	if opt_names then
		WriteLn(data^.Name)
	else begin
		WriteLn('PACKAGE   : ', data^.Name);
{		WriteLn('Filename     : ', data^.FName); }
		Write('Repositories : ');
		data^.Repos.Print(#32);
		WriteLn;
		Write('Versions     : ');
		data^.Vers.Print(#32);
		WriteLn;
{		WriteLn('Variables    : ', data^.Vars); }
		WriteLn('Variables    : ');
		data^.Vars.Print;
		WriteLn('Description  :');
		data^.Desc.Print(#10, #9, true);
	End
End;

(*
 * search for package callback
 *)
Var re : TRegExpr;
	desc : LongString;

Function BuildDescString(node : StrListNodePtr) : StrListWalkResult;
Begin
	desc := Concat(desc, node^.Key);
	BuildDescString := slContinue;
End;

Procedure SearchProc(node : BTreeNodePtr);
Begin
	if node^.Key = opt_search then
		PrintPackage(node)
	else if re.Exec(node^.Key) then
		PrintPackage(node)
End;

Procedure SearchDescProc(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	desc := '';
	data^.Repos.Walk(@BuildDescString);
	if re.Exec(node^.Key) then
		PrintPackage(node)
	else if re.Exec(desc) then
		PrintPackage(node)
End;

(*
 * print report-list callback
 *)
Procedure RepoListProc(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	if data^.Repos.Contains(opt_repo) then
		PrintPackage(node)
End;

(*
 * main
 *)
Begin
	packs.Init;
	reposlist.Init;
	
	ParseCLIParams;	
	if ( opt_verbose ) then
		WriteLn('Loading ', PKGDataFile, ' ...');
	If LoadDataFile then Begin
		if ( opt_verbose ) then
			WriteLn('Loading ', REPDataFile, ' ...');
		If LoadRepoDataFile then Begin
			if ( opt_verbose ) then
				WriteLn('* DONE *');

			if ( opt_bsearch ) then Begin
				re := TRegExpr.Create(Concat('^', opt_search));
				packs.Walk(@SearchProc);
				re.Free;
			End ELSE if ( opt_desc_bsearch ) then Begin
				re := TRegExpr.Create(Concat('^', opt_desc_search));
				packs.Walk(@SearchDescProc);
				re.Free;
			End ELSE if ( opt_brepo ) then Begin
				packs.Walk(@RepoListProc);
			End ELSE if ( opt_repolist ) then Begin
				reposlist.Print(#10);
			End;
		End
		Else
			WriteLn('FAILED!');
	End
	Else
		WriteLn('FAILED!');

	reposlist.Free;
	packs.Free
End.
