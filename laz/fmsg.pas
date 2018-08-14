unit fmsg;

{$mode objfpc}{$H+}

interface

uses
	Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
	ExtCtrls;

type

	{ TFMesg }

 TFMesg = class(TForm)
		Panel1: TPanel;
	private
		{ private declarations }
	public
		{ public declarations }
		Procedure setmsg(s : String);
	end;

var
	FMesg: TFMesg;

implementation

{$R *.lfm}

Procedure TFMesg.setmsg(s : String);
Begin
  FMesg.Caption := s;
  FMesg.Panel1.Caption := s;
end;

end.

