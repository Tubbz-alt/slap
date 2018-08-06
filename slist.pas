(* kate: default-dictionary en; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
 * 
 * Single linked list with string-key
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
{$MODE OBJFPC}

Unit slist;

Interface

Uses SysUtils;

Type
	StrListNodePtr = ^StrListNode;
	StrListNode = Record
		Key  : String;		{ the string }
		Data : Pointer;		{ optional, additional data }
		Next : StrListNodePtr;
	End;
	
	StrListWalkResult = (slStop, slContinue);
	StrListWalkProc = Function(node : StrListNodePtr) : StrListWalkResult;

	StrList = Object
	private
		pHead, pTail : StrListNodePtr;
		nCount : Integer;

	private
		Function	FindParent(node : StrListNodePtr) : StrListNodePtr;

	public
		Constructor	Init;
		Destructor 	Free; virtual;
		Constructor	Copy(src : StrList);
		Procedure	Assign(src : StrList);
		Procedure	Append(src : StrList);
		Procedure	Add(key : String; pData : Pointer = NIL);
		Procedure	Push(str : String; pData : Pointer = NIL); inline;
		Procedure	Clear;
		Procedure	Delete(node : StrListNodePtr);
		Function	Find(key : String) : StrListNodePtr;
		Procedure	Walk(UDF : StrListWalkProc);
		Procedure	Print(delim : String = #10; prefix : String = '');
		Function	Contains(str : String) : Boolean;
		Function	Count : Integer; inline;
		Function	Head  : StrListNodePtr; inline;
	End;

Operator in (const A: String; const B: StrList) : Boolean;

Implementation

Operator IN (const A : String; const B : StrList) : Boolean;  
Begin  
	Result := (B.Find(A) <> NIL)
End;
  
(*
 * Returns the number of nodes in the list.
 *)
Function StrList.Count : Integer; inline;
Begin
	Count := nCount;
End;

(*
 * Returns the head pointer
 *)
Function StrList.Head  : StrListNodePtr; inline;
Begin
	Head := pHead;
End;

(*
 * Initialize the list.
 *)
Constructor StrList.Init;
Begin
	pHead  := NIL;
	pTail  := NIL;
	nCount := 0;
End;

(*
 * Initialize the list with the data of 'src' list.
 *)
Constructor StrList.Copy(src : StrList);
Begin
	Init;
	Append(src);
End;

(*
 * Deletes all nodes of the list.
 *)
Procedure StrList.Clear;
Var	cur, prev : StrListNodePtr;
Begin
	cur := pHead;
	while cur <> NIL do
	Begin
		prev := cur;
		cur  := cur^.Next;
		Dispose(prev);
	End;
	pHead := NIL;
	pTail := NIL;
	nCount := 0;
End;

(*
 * Deletes all nodes of the list.
 *)
Destructor StrList.Free;
Begin
	Clear;
End;

(*
 * Adds a new node at the end of the list.
 *)
Procedure StrList.Add(key : String; pData : Pointer);
Var	node : StrListNodePtr;
Begin
	node := New(StrListNodePtr);
	node^.Next := NIL;
	node^.Key  := key;
	node^.Data := pData;

	If pHead = NIL then	Begin
		pHead := node;
		pTail := node
	End
	Else Begin
		pTail^.Next := node;
		pTail := node
	End;
	Inc(nCount);
End;

Procedure StrList.Push(str : String; pData : Pointer); inline;
Begin
	Add(str, pData);
End;

(*
 * Find the parent node of node
 *)
Function StrList.FindParent(node : StrListNodePtr) : StrListNodePtr;
Var	cur, pre : StrListNodePtr;
Begin
	cur := pHead;
	pre := NIL;
	While cur <> NIL do	Begin
		if cur = node then
			break;
		pre := cur;
		cur := cur^.Next;
		Assert(cur <> NIL);
	End;
	FindParent := pre
End;

(*
 * Delete node from list
 *)
Procedure StrList.Delete(node : StrListNodePtr);
Var	parent : StrListNodePtr;
Begin
	if node <> NIL then begin
		if pHead = node then Begin
			if pHead = pTail then
				pTail := NIL;
			pHead := node^.Next
		End
		else Begin
			parent := FindParent(node);
			Assert(parent <> NIL);
			if pTail = node then
				pTail := parent;
			parent^.Next := node^.Next
		End;
		Dispose(node);
		Dec(nCount)
	End
End;

(*
 * Adds the src list to current list.
 *)
Procedure StrList.Append(src : StrList);
Var cur : StrListNodePtr;
Begin
	Clear;
	cur := src.pHead;
	while cur <> NIL do Begin
		Add(cur^.Key, cur^.Data);
		cur := cur^.Next
	End
End;

(*
 * Copies the src list to current list.
 *)
Procedure StrList.Assign(src : StrList);
Begin
	Clear;
	Append(src);
End;

(*
 * Returns the node with key as key or NIL if not found.
 *)
Function StrList.Find(key : String) : StrListNodePtr;
Var cur : StrListNodePtr;
Begin
	cur := pHead;
	While cur <> NIL do	Begin
		If cur^.Key = key then
			Break;
		cur := cur^.Next;
	End;
	Find := cur
End;

(*
 * Prints the list of the strings.
 *)
Procedure StrList.Print(delim : String; prefix : String);
Var cur	: StrListNodePtr;
Begin
	cur := pHead;
	While cur <> NIL do	Begin
		Write(prefix, cur^.Key, delim);
		cur := cur^.Next
	End
End;

(*
 * Call UDF for each node; UDF should return slStop to stop the process.
 *)
Procedure StrList.Walk(UDF : StrListWalkProc);
Var cur	: StrListNodePtr;
Begin
	cur := pHead;
	While cur <> NIL do	Begin
		if UDF(cur) = slStop then
			break;
		cur := cur^.Next
	End
End;

(*
 * returns true if the list contains the 'str' key
 *)
Function StrList.Contains(str : String) : Boolean;
Begin
	if Find(str) <> NIL then
		Contains := true
	else
		Contains := false
End;

(* --- end --- *)
END.

