unit Forms.Main;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Menus, regexpr, LazFileUtils, DMB;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnOG: TButton;
    btnRR: TButton;
    btnConvert: TButton;
    cgOptions: TCheckGroup;
    edtOG: TEdit;
    edtRR: TEdit;
    gbMessages: TGroupBox;
    lblOG: TLabel;
    lblRR: TLabel;
    dlgOG: TOpenDialog;
    dlgRR: TSaveDialog;
    mnuMainAbout: TMenuItem;
    mnuMain: TMainMenu;
    memMessages: TMemo;
    pnlMessages: TPanel;
    pnlMain: TPanel;
    procedure btnConvertClick(Sender: TObject);
    procedure btnOGClick(Sender: TObject);
    procedure btnRRClick(Sender: TObject);
    procedure edtOGChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure mnuMainAboutClick(Sender: TObject);
  end;

  TCheckBoxOption = (cboTextureFallback, cboOptimalDistance);

var
  frmMain: TfrmMain;

const
  TITLE: string = 'OG2RR DMB Converter';
  VERSION: string = 'v1.0.0';
  AUTHOR: string = 'Vartan Haghverdi';
  COPYRIGHT: string = 'Copyright 2023';
  NOTE: string = 'Brought to you by the EB Online Team';

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnOGClick(Sender: TObject);
begin
  if dlgOG.Execute then
    edtOG.Text := dlgOG.FileName;
end;

procedure TfrmMain.btnRRClick(Sender: TObject);
begin
  if dlgRR.Execute then
    edtRR.Text := dlgRR.FileName;
end;

procedure TfrmMain.edtOGChange(Sender: TObject);
begin
  // when user selects OG DMB, set default RR DMB save path as suffix to OG path
  edtRR.Text := ExtractFileNameWithoutExt(edtOG.Text) + '_RR.txt';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  frmMain.Caption := TITLE + ' ' + VERSION;
end;

procedure TfrmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  edtOG.Text := FileNames[0];
end;

procedure TfrmMain.mnuMainAboutClick(Sender: TObject);
begin
  ShowMessage(TITLE + ' ' + VERSION + LineEnding + NOTE + LineEnding +
    COPYRIGHT + ' ' + AUTHOR);
end;

procedure TfrmMain.btnConvertClick(Sender: TObject);
var
  sl: TStringList;
  MissingTextureCount: integer;
  DMBEntry: TDMBEntry;
  DMBEntryList: TDMBEntryList;
begin
  if (edtOG.Text = '') or (edtRR.Text = '') then
  begin
    memMessages.Clear;
    memMessages.Lines.Add('Error: Missing OG and/or RR DMB filepaths.');
    MessageDlg('Error', 'Missing OG and/or RR DMB filepaths.', mtError, [mbOK], 0);
    Exit;
  end;
  sl := TStringList.Create;
  DMBEntryList := TDMBEntryList.Create;
  try
    sl.LoadFromFile(edtOG.Text);
    sl.Text := FormatDMB(sl.Text);
    DMBEntryList.LoadFromString(sl.Text,
      cgOptions.Checked[Ord(cboTextureFallback)],
      cgOptions.Checked[Ord(cboOptimalDistance)]);
    sl.Text := DMBEntryList.Text;
    sl.SaveToFile(edtRR.Text);
    memMessages.Clear;
    memMessages.Lines.Add(Format('Converted %d units.', [DMBEntryList.Count]));
    MessageDlg(Format('Converted %d units.', [DMBEntryList.Count]),
      mtInformation, [mbOK], 0);

    { count and report entries with missing textures }
    MissingTextureCount := 0;
    for DMBEntry in DMBEntryList do
      if DMBEntry.Textures.Count = 0 then
      begin
        memMessages.Lines.Add(DMBEntry.TypeID);
        MissingTextureCount := MissingTextureCount + 1;
      end;
    if MissingTextureCount > 0 then
    begin
      memMessages.Lines.Insert(1, '');
      memMessages.Lines.Insert(2, '----------------------------------');
      memMessages.Lines.Insert(3,
        Format('%d entries are missing textures', [MissingTextureCount]));
      memMessages.Lines.Insert(4, '----------------------------------');
      memMessages.Lines.Insert(5, '');
    end;
  finally
    FreeAndNil(sl);
    FreeAndNil(DMBEntryList);
  end;
end;

end.
