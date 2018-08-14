(* kate: default-dictionary en; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
 * 
 * Binary Tree Implementation
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

Unit SBTree;

Interface

Type
	BTreeTraversal  = (btPreorder, btInorder, btPostorder);

Type
	BTreeNodePtr = ^BTreeNode;
	BTreeNode = Record
		Key   : String;
		Ptr   : Pointer;
		Left  : BTreeNodePtr;
		Right : BTreeNodePtr;
	End;

	BTreeWalkProc = Procedure(node : BTreeNodePtr);
	BTreeWalkNestProc = Procedure(node : BTreeNodePtr) is nested;
    BTreeWalkMethod = Procedure(node : BTreeNodePtr) of Object;

	BTree = Object
	private
		root : BTreeNodePtr;

	private
		Procedure	Clear(Var aRoot : BTreeNodePtr);

	public
		Constructor	Init;
		Destructor	Free; virtual;
		Procedure	Clear;
		Function	Insert(key : String; data : Pointer) : BTreeNodePtr;
		Function	Find(key : String) : BTreeNodePtr;
		Procedure	Walk(u1 : BTreeWalkProc; u2 : BTreeWalkNestProc; u3 : BTreeWalkMethod; order : BTreeTraversal = btInorder);
		Procedure	Walk(UDP : BTreeWalkProc; order : BTreeTraversal = btInorder); inline;
		Procedure	Walk(UDP : BTreeWalkNestProc; order : BTreeTraversal = btInorder); inline;
		Procedure	Walk(UDP : BTreeWalkMethod; order : BTreeTraversal = btInorder); inline;
	End;

Implementation

(*
 * Initialise a tree root node
 *)
Constructor BTree.Init;
Begin
	root := NIL
End;

(*
 * Delete a whole tree
 *)
Procedure BTree.Clear(Var aRoot : BTreeNodePtr);
Begin
	If aRoot <> NIL then Begin
		Clear(aRoot^.Left);
		Clear(aRoot^.Right);
		Dispose(aRoot);
	End;
	aRoot := NIL
End;

Procedure BTree.Clear;
Begin
	Clear(root)
End;

Destructor BTree.Free;
Begin
	Clear
End;

(*
 * Add a new node in the tree
 *)
Function BTree.Insert(key : String; data : Pointer) : BTreeNodePtr;
Var	cur, pre, node : BTreeNodePtr;
Begin
	{ create a new node }
	node := New(BTreeNodePtr);
	node^.Left  := NIL;
	node^.Right := NIL;
	node^.Key   := key;
	node^.Ptr   := data;

	{ find the correct position to insert the new node }
	cur := root;
	pre := NIL;
	While cur <> NIL do	Begin
		pre := cur;
		If cur^.Key > key then
			cur := cur^.Left
		Else
			cur := cur^.Right
	End;

	{ now: pre = parent, cur = NIL; }
	{ connect the new node }
	If pre <> NIL then Begin
		If pre^.Key > key then
			pre^.Left := node
		Else
			pre^.Right := node
	End
	Else
		root := node;
	Insert := node;
End;

(*
 * Find and return the node with 'key' key; otherwise return NIL
 *)
Function BTree.Find(key : String) : BTreeNodePtr;
Var cur : BTreeNodePtr;
Begin
	cur := root;
	While cur <> NIL do	Begin
		If cur^.Key = key then
			Break
		Else Begin
			If cur^.Key > key then
				cur := cur^.Left
			else
				cur := cur^.Right
		End
	End;
	Find := cur
End;

(*
 * For each node of the tree
 *)
Procedure BTree.Walk(u1 : BTreeWalkProc; u2 : BTreeWalkNestProc; u3 : BTreeWalkMethod; order : BTreeTraversal);

	Procedure PrintInOrder(root : BTreeNodePtr);
	Begin
		If root <> NIL then	Begin
			PrintInOrder(root^.Left);
			IF u1 <> NIL THEN U1(root);
			IF u2 <> NIL THEN U2(root);
			IF u3 <> NIL THEN U3(root);
			PrintInOrder(root^.Right)
		End
	End;

	Procedure PrintPreOrder(root : BTreeNodePtr);
	Begin
		If root <> NIL then Begin
			IF u1 <> NIL THEN U1(root);
			IF u2 <> NIL THEN U2(root);
			IF u3 <> NIL THEN U3(root);
			PrintPreOrder(root^.Left);
			PrintPreOrder(root^.Right)
		End
	End;

	Procedure PrintPostOrder(root : BTreeNodePtr);
	Begin
		If root <> NIL then Begin
			PrintPostOrder(root^.Left);
			PrintPostOrder(root^.Right);
			IF u1 <> NIL THEN U1(root);
			IF u2 <> NIL THEN U2(root);
			IF u3 <> NIL THEN U3(root);
		End
	End;

Begin
	Case order of
	btPreorder  : PrintPreOrder(root);
	btInorder   : PrintInOrder(root);
	btPostorder : PrintPostOrder(root)
	End
end;

Procedure BTree.Walk(UDP : BTreeWalkProc; order : BTreeTraversal); inline;
BEGIN Walk(UDP, NIL, NIL, order) END;

Procedure BTree.Walk(UDP : BTreeWalkNestProc; order : BTreeTraversal); inline;
BEGIN Walk(NIL, UDP, NIL, order) END;

Procedure BTree.Walk(UDP : BTreeWalkMethod; order : BTreeTraversal); inline;
BEGIN Walk(NIL, NIL, UDP, order) END;

(* --- end --- *)
END.
