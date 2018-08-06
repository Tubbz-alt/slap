(* kate: default-dictionary en; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
 * 
 * slap - An Automated packaging tool for Slackware Linux
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
 * Project Page: http://...
 * Nicholas Christopoulos nereus@freemail.gr
 * 
 *)
{$MODE OBJFPC}

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

	BTree = Object
	private
		root : BTreeNodePtr;

	private
		Procedure	Clear(Var aRoot : BTreeNodePtr);

	public
		Constructor	Init;
		Destructor	Free; virtual;
		Procedure	Clear;
		Procedure	Insert(key : String; data : Pointer);
		Function	Find(key : String) : BTreeNodePtr;
		Procedure	Walk(UDP : BTreeWalkProc; order : BTreeTraversal = btInorder);
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
Procedure BTree.Insert(key : String; data : Pointer);
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
		root := node
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
Procedure BTree.Walk(UDP : BTreeWalkProc; order : BTreeTraversal);

	Procedure PrintInOrder(root : BTreeNodePtr);
	Begin
		If root <> NIL then	Begin
			PrintInOrder(root^.Left);
			UDP(root);
			PrintInOrder(root^.Right)
		End
	End;

	Procedure PrintPreOrder(root : BTreeNodePtr);
	Begin
		If root <> NIL then Begin
			UDP(root);
			PrintPreOrder(root^.Left);
			PrintPreOrder(root^.Right)
		End
	End;

	Procedure PrintPostOrder(root : BTreeNodePtr);
	Begin
		If root <> NIL then Begin
			PrintPostOrder(root^.Left);
			PrintPostOrder(root^.Right);
			UDP(root)
		End
	End;

Begin
	Case order of
	btPreorder  : PrintPreOrder(root);
	btInorder   : PrintInOrder(root);
	btPostorder : PrintPostOrder(root)
	End
End;

(* --- end --- *)
END.
