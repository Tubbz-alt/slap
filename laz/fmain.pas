(* kate: default-dictionary en; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
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
 *)

unit FMain;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
	StdCtrls, ComCtrls,
    SBTree, SList, SlackPack;

type

	{ TFMain }

 TFMain = class(TForm)
		btnInstall: TButton;
		btnRemove: TButton;
		chkDescr: TCheckBox;
		cmbList: TComboBox;
		Image1: TImage;
		Memo1: TMemo;
		Panel2: TPanel;
		Panel5: TPanel;
		Panel6: TPanel;
		txtSearchFor: TEdit;
		Label3: TLabel;
		lstRepos: TComboBox;
		Label1: TLabel;
		Label2: TLabel;
		ListBox1: TListBox;
		Panel1: TPanel;
		Panel3: TPanel;
		Panel4: TPanel;
		Splitter1: TSplitter;
		StatusBar1: TStatusBar;
		procedure cmbListChange(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure ListBox1Click(Sender: TObject);
		procedure lstReposChange(Sender: TObject);
	private
		{ private declarations }
       pdb : SlackwarePDB;
       opt_installed : Boolean;
       opt_uninstalled : Boolean;
       opt_repo : String;
       Procedure PopulateListBox(node : BTreeNodePtr);
       Procedure RebuildPackageList;
       Function	StoreRepo(node : StrListNodePtr) : StrListWalkResult;
       Procedure ShowPackage(node : BTreeNodePtr);
	public
		{ public declarations }
	end;

var
	FMain1 : TFMain;


implementation

{$R *.lfm}

{ TFMain }

Procedure TFMain.PopulateListBox(node : BTreeNodePtr);
Var data : PKGPtr;
    badd : Boolean;
Begin
    data := node^.Ptr;
    badd := true;
    if opt_installed and (NOT data^.bInst) then
    	badd := false;
	if opt_uninstalled and data^.bInst then
        badd := false;
    if opt_repo <> 'ALL' then
        Begin
        if NOT (opt_repo in data^.repos) then
			badd := false;
        End;

    if badd then
		ListBox1.AddItem(node^.Key, NIL);
end;

Function TFMain.StoreRepo(node : StrListNodePtr) : StrListWalkResult;
Begin
	lstRepos.AddItem(node^.Key, NIL);
    StoreRepo := slContinue;
end;

Procedure TFMain.RebuildPackageList;
Begin
    ListBox1.Clear;
	pdb.packs.Walk(@PopulateListBox);
//    if ListBox1.Items.Count > 0 then
//	    ListBox1.ItemIndex := 0
//    else
//    	Memo1.Text := '';
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
    opt_installed := false;
    opt_uninstalled := false;
    opt_repo := 'ALL';
    StatusBar1.SimpleText := 'Slackware Package DB ... Loading';
	pdb.Init;
	RebuildPackageList;
    lstRepos.Items.Clear;
    lstRepos.Items.Add('ALL');
    pdb.reposlist.Walk(@StoreRepo);
    lstRepos.ItemIndex := 0;
    StatusBar1.SimpleText := 'Slackware Package DB ... ';
    Memo1.Text := '';
end;

Procedure TFMain.ShowPackage(node : BTreeNodePtr);
Var	s : String;
	data : PKGPtr;
Begin
    data := node^.Ptr;
    s := '';
    if data^.bInst then
		s := Concat(s, 'PACKAGE: ', data^.Name, ' INSTALLED', #10#10)
    else
   		s := Concat(s, 'PACKAGE: ', data^.Name, ' UNINSTALLED', #10#10);
	s := Concat(s, 'FILE: ', data^.FName, #10);
	s := Concat(s, 'REPO: ', data^.Repos.toLongString('; '), #10);
	s := Concat(s, 'VERS: ', data^.Vers.toLongString('; '), #10);
	s := Concat(s, 'SIZE: ', data^.USize, '; ', data^.CSize, ' compressed. ', #10);
{			s := Concat(s, 'Variables    : ');
     data^.Vars.Print;}
    s := Concat(s, #10, data^.desc.ToLongString, #10);
    Memo1.Text := s;
end;

procedure TFMain.ListBox1Click(Sender: TObject);
Var	s : String;
	node : BTreeNodePtr;
begin
	if ListBox1.ItemIndex <> -1 then
	begin
	 	s := ListBox1.Items[ListBox1.ItemIndex];
        StatusBar1.SimpleText := Concat('Package: ', s);
		node := pdb.packs.Find(s);
        if node <> NIL then
        	ShowPackage(node);
	end
end;

procedure TFMain.lstReposChange(Sender: TObject);
begin
    opt_repo := lstRepos.Items[lstRepos.ItemIndex];
    RebuildPackageList;
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	pdb.Free;
end;

procedure TFMain.cmbListChange(Sender: TObject);
begin
	case cmbList.ItemIndex of
    0 : Begin
        opt_installed := false;
    	opt_uninstalled := false;
        End;
    1 : Begin
        opt_installed := true;
    	opt_uninstalled := false;
        End;
    2 :	Begin
        opt_installed := false;
    	opt_uninstalled := true;
        End;
	end;
    RebuildPackageList;
end;

end.

