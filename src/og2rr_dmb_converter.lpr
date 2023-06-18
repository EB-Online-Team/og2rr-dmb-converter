program og2rr_dmb_converter;

{$mode objfpc}{$H+}{$J-}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces,
  uDarkStyleParams,
  uMetaDarkStyle,
  uDarkStyleSchemes,
  Forms,
  Forms.Main,
  DMB;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='OG2RR DMB Converter';
  Application.Scaled:=True;
  PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
