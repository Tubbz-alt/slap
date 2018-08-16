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
{$MODESWITCH NESTEDPROCVARS} 
 
Unit slist;

Interface

Uses SysUtils;

Type
    LongString = UTF8String;

Type
	StrListNodePtr = ^StrListNode;
	StrListNode = Record
		Key  : String;		{ the string }
		Data : Pointer;		{ optional, additional data }
		Next : StrListNodePtr;
	End;
	
	StrListWalkResult = (slStop, slContinue);
	StrListWalkProc = Function(node : StrListNodePtr) : StrListWalkResult;
	StrListWalkNestProc = Function(node : StrListNodePtr) : StrListWalkResult is Nested;
	StrListWalkMethod = Function(node : StrListNodePtr) : StrListWalkResult of Object;

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
		Function	Add(key : String; pData : Pointer = NIL) : StrListNodePtr;
		Function	Push(str : String; pData : Pointer = NIL) : StrListNodePtr; inline;
		Procedure	Clear;
		Procedure	Delete(node : StrListNodePtr);
		Function	Find(key : String) : StrListNodePtr;
		Procedure	Walk(u1 : StrListWalkProc; u2 : StrListWalkNestProc; u3 : StrListWalkMethod);
		Procedure	Walk(UDF : StrListWalkProc); inline;
		Procedure	Walk(UDF : StrListWalkNestProc); inline;
		Procedure	Walk(UDF : StrListWalkMethod); inline;
		Procedure	Print(delim : String = #10; prefix : String = '');
		Function	Count : Integer; inline;
		Function	Head  : StrListNodePtr; inline;
		Function	ToLongString(sep : String = #10) : LongString;
	End;

Operator in (const A: String; const B: StrList) : Boolean;

Implementation

(*
 * returns true if the list contains the 'A' key
 *)
Operator in (const A : String; const B : StrList) : Boolean;  
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
Function StrList.Add(key : String; pData : Pointer) : StrListNodePtr;
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
	Add := node;
End;

Function StrList.Push(str : String; pData : Pointer) : StrListNodePtr; inline;
Begin
	Push := Add(str, pData);
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
Procedure StrList.Walk(u1 : StrListWalkProc; u2 : StrListWalkNestProc; u3 : StrListWalkMethod);
Var cur	: StrListNodePtr;
Begin
	cur := pHead;
	While cur <> NIL do	Begin
		IF (u1 <> NIL) AND (u1(cur) = slStop) THEN BREAK;
		IF (u2 <> NIL) AND (u2(cur) = slStop) THEN BREAK;
		IF (u3 <> NIL) AND (u3(cur) = slStop) THEN BREAK;
		cur := cur^.Next
	End
End;

Procedure StrList.Walk(UDF : StrListWalkProc); inline;
BEGIN Walk(UDF,NIL,NIL) END;

Procedure StrList.Walk(UDF : StrListWalkNestProc); inline;
BEGIN Walk(NIL,UDF,NIL) END;

Procedure StrList.Walk(UDF : StrListWalkMethod); inline;
BEGIN Walk(NIL,NIL,UDF) END;

(*
 *	Returns variable string (ansistring) version of the Texts
 *)
Function StrList.ToLongString(sep : String) : LongString;
Var cur : StrListNodePtr;
  	s	: LongString;
Begin
	cur := pHead;
	s   := '';
	while cur <> NIL do Begin
		s := Concat(s, cur^.Key, sep);
		cur := cur^.Next
	End;
    ToLongString := s
End;

(* --- end --- *)
END.

