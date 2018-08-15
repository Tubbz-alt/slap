program lazSlap;

{$mode objfpc}{$H+}

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads}
	cthreads,
	{$ENDIF}{$ENDIF}
	Interfaces, // this includes the LCL widgetset
	Forms, fmain
	{ you can add units after this }, SBTree, SList, SlackPack;

{$R *.res}

begin
	RequireDerivedFormResource:=True;
	Application.Initialize;
	Application.CreateForm(TFMain, FMain1);
	Application.Run;
end.

