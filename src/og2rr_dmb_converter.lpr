{
This program converts `descr_model_battle.txt` (DMB) from _Rome: Total War_ (OG) to _Total War: Rome Remastered_ (RR).

Copyright (C) 2022-2024 Vartan Haghverdi

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
}

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
  {$IFDEF WINDOWS}
  uDarkStyleParams,
  uMetaDarkStyle,
  uDarkStyleSchemes,
  {$ENDIF}
  Forms,
  Forms.Main,
  DMB;

  {$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title:='OG2RR DMB Converter';
  Application.Scaled:=True;
  {$IFDEF WINDOWS}
  PreferredAppMode := pamAllowDark;
  uMetaDarkStyle.ApplyMetaDarkStyle(DefaultDark);
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
