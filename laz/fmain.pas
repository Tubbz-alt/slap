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
	StdCtrls, fmsg,
    SBTree, SList, SlackPack;

type

	{ TFMain }

 TFMain = class(TForm)
		ListBox1: TListBox;
		Memo1: TMemo;
		Panel1: TPanel;
		Panel2: TPanel;
		procedure FormActivate(Sender: TObject);
		procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure ListBox1Click(Sender: TObject);
	private
		{ private declarations }
       pdb : SlackwarePDB;
       Procedure PopLB(node : BTreeNodePtr);
	public
		{ public declarations }
	end;

var
	FMainF: TFMain;


implementation

{$R *.lfm}

{ TFMain }

Procedure TFMain.PopLB(node : BTreeNodePtr);
Begin
	 FMainF.ListBox1.AddItem(node^.Key, NIL);
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
//	 FMesg.setmsg('Building package database...');
	 pdb.Init;
	 pdb.packs.Walk(@PopLB);
end;

procedure TFMain.ListBox1Click(Sender: TObject);
Var		  s : String;
    node : BTreeNodePtr;
begin
	 if ListBox1.ItemIndex <> -1 then
     begin
	 	s := ListBox1.Items[ListBox1.ItemIndex];
		node := pdb.packs.Find(s);
        if node <> NIL then begin
		   Memo1.Text := PKGPtr(node^.Ptr)^.Desc.ToLongString;
           end
     end
end;

procedure TFMain.FormActivate(Sender: TObject);
begin
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    pdb.Free;
end;

end.

