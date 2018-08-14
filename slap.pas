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
 * Nicholas Christopoulos (nereus@freemail.gr)
 *)
{$MODE OBJFPC}
{$CODEPAGE UTF8}

Program Slap;

Uses SysUtils, SBTree, SList, Slackpack, RegExpr;

Const
	AppVersion  = '1.00';

Type
	LongString = UTF8String;

Var
	pdb : SlackwarePDB;

	opt_verbose, opt_bsearch, opt_desc_bsearch, opt_brepo : Boolean;
	opt_names, opt_repolist, opt_list_inst, opt_list_uninst : Boolean;
	opt_search, opt_desc_search, opt_repo : String;

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
		WriteLn('  -r, --repos		Display a list of repositories.');
		WriteLn('  -lr, --list-repo repo');
		WriteLn('			Display or searches the packages of the specified repository.');
		WriteLn('  -li, --list-installed	Display or searches the list of installed packages.');
		WriteLn('  -lu, --list-uninstalled');
		WriteLn('			Display or searches the list of uninstalled packages.');
		WriteLn;
		WriteLn(' (1): Regular expressions are supported.');
		WriteLn('Note: The list (-l*) options can combined with search (-s*) options.');
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
			Else If (opt = '-li') OR (opt = '--list-installed' ) then
				opt_list_inst := true
			Else If (opt = '-lu') OR (opt = '--list-uninstalled' ) then
				opt_list_uninst := true
			Else If (opt = '-r') OR (opt = '--repos' ) then
				opt_repolist := true
			Else If (opt = '-lr') OR (opt = '--list-repo') then Begin
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
Var pd_last : String;
Var pd_n : Integer;
Function PrintDesc(nd : StrListNodePtr) : StrListWalkResult;
Var ll, ls : Integer;
Begin
	Inc(pd_n);
	ll := Length(pd_last);
	pd_last := Trim(nd^.Key);
	ls := Length(pd_last);
	if (ll <> 0) OR (ls <> 0) then begin
		if (pd_n = 2) and (ls <> 0) then
			WriteLn;
		WriteLn(#9, nd^.Key);
	end;
	PrintDesc := slContinue;
End;
Procedure PrintPackage(node : BTreeNodePtr);
Var	data	: PKGPtr;
Begin
	data := node^.Ptr;
	if opt_names then
		WriteLn(data^.Name)
	else begin
		Write('PACKAGE      : ', data^.Name);
		if data^.bInst then
			WriteLn(#9, '(installed)')
		else
			WriteLn(#9, '(uninstalled)');
{		WriteLn('Filename     : ', data^.FName); }
		Write('Repositories : ');
		data^.Repos.Print(#32);
		WriteLn;
		Write('Versions     : ');
		data^.Vers.Print(#32);
		WriteLn;
		WriteLn('Size         : ', data^.USize, '; ', data^.CSize, ' compressed. ');
		WriteLn('Variables    : ');
		data^.Vars.Print;
		WriteLn('DESCRIPTION');
		pd_last := '';
		pd_n := 0;
		data^.Desc.Walk(@PrintDesc);
		if Length(pd_last) > 0 then
			WriteLn;
	End
End;

(*
 * search for package callback
 *)
Var re : TRegExpr;
	ls_desc : LongString;

Function BuildDescString(slnode : StrListNodePtr) : StrListWalkResult;
Begin
	ls_desc := Concat(ls_desc, slnode^.Key);
	BuildDescString := slContinue
End;

Procedure PrintProc(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	if (opt_list_inst) and (NOT data^.bInst) then exit;
	if (opt_list_uninst) and (data^.bInst) then exit;
	PrintPackage(node)
End;

Procedure SearchProc(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	if (opt_list_inst) and (NOT data^.bInst) then exit;
	if (opt_list_uninst) and (data^.bInst) then exit;
	if node^.Key = opt_search then
		PrintPackage(node)
	else if (re<>NIL) AND (re.Exec(node^.Key)) then
		PrintPackage(node)
End;

Procedure SearchDescProc(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	if (opt_list_inst) and (NOT data^.bInst) then exit;
	if (opt_list_uninst) and (data^.bInst) then exit;
	ls_desc := '';
	data^.desc.Walk(@BuildDescString);
	if (re<>NIL) AND (re.Exec(node^.Key)) then
		PrintPackage(node)
	else if (re<>NIL) AND (re.Exec(ls_desc)) then
		PrintPackage(node);
End;

(*
 * print report-list callback
 *)
Procedure RepoListProc(node : BTreeNodePtr);
Var	data : PKGPtr;
Begin
	data := node^.Ptr;
	if (opt_list_inst) and (NOT data^.bInst) then exit;
	if (opt_list_uninst) and (data^.bInst) then exit;
	if opt_repo in data^.Repos then
		PrintPackage(node)
End;

(*
 * main
 *)
Begin
	ParseCLIParams;
	
	pdb.Init(opt_verbose);
	IF opt_bsearch THEN
	BEGIN
		re := TRegExpr.Create(Concat('^', opt_search));
		pdb.packs.Walk(@SearchProc);
		re.Free;
	END 
	ELSE
		IF opt_desc_bsearch THEN
		BEGIN
			re := TRegExpr.Create(opt_desc_search);
			pdb.packs.Walk(@SearchDescProc);
			re.Free;
		END
		ELSE
			IF opt_brepo THEN
				pdb.packs.Walk(@RepoListProc)
			ELSE
				IF opt_repolist THEN
					pdb.reposlist.Print(#10)
				ELSE
					IF opt_list_inst THEN
						pdb.packs.Walk(@PrintProc)
					ELSE
						IF opt_list_uninst THEN
							pdb.packs.Walk(@PrintProc);

	pdb.Free;
End.
