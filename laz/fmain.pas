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
		ComboBox1: TComboBox;
		ComboBox2: TComboBox;
		Label1: TLabel;
		Label2: TLabel;
		txtSearchFor: TLabeledEdit;
		ListBox1: TListBox;
		Memo1: TMemo;
		Panel1: TPanel;
		Panel2: TPanel;
		Panel3: TPanel;
		Panel4: TPanel;
		Splitter1: TSplitter;
		StatusBar1: TStatusBar;
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure ListBox1Click(Sender: TObject);
	private
		{ private declarations }
       pdb : SlackwarePDB;
       Procedure PopulateListBox(node : BTreeNodePtr);
	public
		{ public declarations }
	end;

var
	FMain1 : TFMain;


implementation

{$R *.lfm}

{ TFMain }

Procedure TFMain.PopulateListBox(node : BTreeNodePtr);
Begin
	ListBox1.AddItem(node^.Key, NIL);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
    StatusBar1.SimpleText := 'Slackware Package DB ... Loading';
	pdb.Init;
	pdb.packs.Walk(@PopulateListBox);
    StatusBar1.SimpleText := 'Slackware Package DB ... done';
    Memo1.Text := '';
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
			Memo1.Text := PKGPtr(node^.Ptr)^.Desc.ToLongString;
	end
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	pdb.Free;
end;

end.

