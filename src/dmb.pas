unit DMB;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes,
  SysUtils,
  Generics.Collections,
  regexpr;

type

  { TDMBTexture }

  TDMBTexture = class
  private
    FFaction: string;
    FPath: string;
    function FText: string;
  public
    constructor Create(Path: string);
    constructor Create(Faction, Path: string);
    property Faction: string read FFaction write FFaction;
    property Path: string read FPath write FPath;
    property Text: string read FText;
  end;

  TDMBTextureList = specialize TObjectList<TDMBTexture>;

  { TDMBModel }

  TDMBModel = class
  private
    FSuffix: string;
    FPath: string;
    FDistance: string;
    function FText: string;
  public
    constructor Create(Suffix, Path, Distance: string);
    property Suffix: string read FSuffix write FSuffix;
    property Path: string read FPath write FPath;
    property Distance: string read FDistance write FDistance;
    property Text: string read FText;
  end;

  TDMBModelList = specialize TObjectList<TDMBModel>;

  { TDMBEntry }

  TDMBEntry = class
  private
    FTypeID: string;
    FSkeletonPrimary: string;
    FSkeletonSecondary: string;
    FSkeletonHorse: string;
    FSkeletonElephant: string;
    FSkeletonChariot: string;
    FSkeletonCamel: string;
    FSkeletonScorpion: string;
    FTextures: TDMBTextureList;
    FModels: TDMBModelList;
    FTextureFallback: boolean;
    FOptimalDistance: boolean;
    function FText: string;
  public
    constructor Create(TextureFallback: boolean = False;
      OptimalDistance: boolean = False);
    destructor Destroy; override;
    property TypeID: string read FTypeID write FTypeID;
    property SkeletonPrimary: string read FSkeletonPrimary write FSkeletonPrimary;
    property SkeletonSecondary: string read FSkeletonSecondary write FSkeletonSecondary;
    property SkeletonHorse: string read FSkeletonHorse write FSkeletonHorse;
    property SkeletonElephant: string read FSkeletonElephant write FSkeletonElephant;
    property SkeletonChariot: string read FSkeletonChariot write FSkeletonChariot;
    property SkeletonCamel: string read FSkeletonCamel write FSkeletonCamel;
    property SkeletonScorpion: string read FSkeletonScorpion write FSkeletonScorpion;
    property Textures: TDMBTextureList read FTextures write FTextures;
    property Models: TDMBModelList read FModels write FModels;
    property TextureFallback: boolean read FTextureFallback write FTextureFallback;
    property OptimalDistance: boolean read FOptimalDistance write FOptimalDistance;
    property Text: string read FText;
    procedure Clear;
    procedure LoadFromString(Input: string);
  end;

  TDMBEntryList = specialize TObjectList<TDMBEntry>;

  { TDMBEntryListHelper }

  TDMBEntryListHelper = class helper for TDMBEntryList
  private
    function FText: string;
  public
    property Text: string read FText;
    procedure LoadFromString(Input: string; TextureFallback: boolean = False;
      OptimalDistance: boolean = False);
  end;

function FormatDMB(DMB: string): string;




implementation

const
  HorseSkeletons: array of string =
    ('fs_horse', 'fs_fast_horse', 'fs_medium_horse', 'fs_cataphract_horse');
  ElephantSkeletons: array of string =
    ('fs_african_elephant', 'fs_forest_elephant', 'fs_indian_elephant',
    'fs_indian_giant_elephant');
  CamelSkeletons: array of string = ('fs_camel');
  OptimalDistanceInfantry: array of string = ('15', '30', '40', 'max');
  OptimalDistanceHorse: array of string = ('25', '55', '80', 'max');
  OptimalDistanceElephant: array of string = ('30', '60', '120', 'max');
  OptimalDistanceCamel: array of string = ('15', '30', '60', 'max');

function FormatDMB(DMB: string): string;
var
  i: integer;
  sLine: string;
  slOriginalDMB, slFormattedDMB: TStringList;
begin
  slOriginalDMB := TStringList.Create;
  slFormattedDMB := TStringList.Create;
  try
    slOriginalDMB.Text := DMB;
    for i := 0 to Pred(slOriginalDMB.Count) do
    begin
      sLine := Trim(slOriginalDMB.Strings[i]);
      // ignore empty lines and comment-only lines
      if (not sLine.IsEmpty) and (sLine[1] <> ';') then
      begin
        // add empty line between DMB entries
        if 'type' = sLine.Substring(0, 4) then
          slFormattedDMB.Add('');
        slFormattedDMB.Add(sLine);
      end;
    end;
    // remove initial empty line
    if slFormattedDMB.Strings[0].IsEmpty then
      slFormattedDMB.Delete(0);
    Result := slFormattedDMB.Text;
  finally
    FreeAndNil(slOriginalDMB);
    FreeAndNil(slFormattedDMB);
  end;
end;

constructor TDMBTexture.Create(Path: string);
begin
  Self.Path := Path;
end;

constructor TDMBTexture.Create(Faction, Path: string);
begin
  Self.Faction := Faction;
  Self.Path := Path;
end;

function TDMBTexture.FText: string;
begin
  Result := Format('%-28s', ['texture']);
  if Faction.Length > 0 then
    Result := Result + Format('%s, %s', [Faction, Path])
  else
    Result := Result + Path;
end;

constructor TDMBModel.Create(Suffix, Path, Distance: string);
begin
  Self.Suffix := Suffix;
  Self.Path := Path;
  Self.Distance := Distance;
end;

function TDMBModel.FText: string;
begin
  Result := Format('%-28s%s, %s', ['model_flexi' + Suffix, Path, Distance]) + LineEnding;
  Result := Result + Format('%-28s%s, %s', ['no_variation model_flexi' +
    Suffix, Path, Distance]) + LineEnding;
end;

constructor TDMBEntry.Create(TextureFallback: boolean; OptimalDistance: boolean);
begin
  Self.TextureFallback := TextureFallback;
  Self.OptimalDistance := OptimalDistance;
  Textures := TDMBTextureList.Create;
  Models := TDMBModelList.Create;
end;

destructor TDMBEntry.Destroy;
begin
  FreeAndNil(FTextures);
  FreeAndNil(FModels);
  inherited;
end;

function TDMBEntry.FText: string;
var
  Texture: TDMBTexture;
  Model: TDMBModel;
begin
  Result := Format('%-28s%s', ['type', TypeID]) + LineEnding;
  Result := Result + ';---------------------------------------' + LineEnding;
  Result := Format('%s%-28s%s', [Result, 'skeleton', SkeletonPrimary]);
  if SkeletonSecondary <> '' then
    Result := Format('%s, %s', [Result, SkeletonSecondary]);
  Result := Result + LineEnding;
  if SkeletonHorse <> '' then
    Result := Format('%s%-28s%s', [Result, 'skeleton_horse', SkeletonHorse]) +
      LineEnding;
  if SkeletonElephant <> '' then
    Result := Format('%s%-28s%s', [Result, 'skeleton_elephant', SkeletonElephant]) +
      LineEnding;
  if SkeletonChariot <> '' then
    Result := Format('%s%-28s%s', [Result, 'skeleton_chariot', SkeletonChariot]) +
      LineEnding;
  if SkeletonCamel <> '' then
    Result := Format('%s%-28s%s', [Result, 'skeleton_camel', SkeletonCamel]) +
      LineEnding;
  if SkeletonScorpion <> '' then
    Result := Format('%s%-28s%s', [Result, 'skeleton_scorpion_cart',
      SkeletonScorpion]) + LineEnding;
  Result := Format('%s%-28s%s', [Result, 'pbr_texture',
    'data/characters/textures/generic_pbr.tga']) + LineEnding;
  for Texture in Textures do
    if Texture.Faction.IsEmpty then
      Result := Format('%s%-28s%s', [Result, 'texture', Texture.Path]) + LineEnding
    else
      Result := Format('%s%-28s%s, %s', [Result, 'texture', Texture.Faction,
        Texture.Path]) + LineEnding;
  for Model in Models do
    Result := Result + Model.Text;
end;

procedure TDMBEntry.Clear;
begin
  TypeID := '';
  SkeletonPrimary := '';
  SkeletonSecondary := '';
  SkeletonHorse := '';
  SkeletonElephant := '';
  SkeletonChariot := '';
  SkeletonCamel := '';
  SkeletonScorpion := '';
  Textures.Clear;
  Models.Clear;
end;

procedure TDMBEntry.LoadFromString(Input: string);
var
  i: integer;
  re: TRegExpr;
  SkeletonLine: array of string;
  Texture: TDMBTexture;
  TextureFallbackFound: boolean = False;
  ModelType: string;
begin
  re := TRegExpr.Create;
  try
    re.ModifierM := True;
    re.ModifierS := False;
    re.InputString := Input;

    { type }
    re.Expression := '^type\s([^;\n]+)';
    re.Exec;
    TypeID := re.Match[1].Trim;

    { skeletons }
    re.Expression := '^skeleton\s([^;\n]+)';
    re.Exec;
    SkeletonLine := re.Match[1].Split(',');
    SkeletonPrimary := SkeletonLine[0].Trim;
    if Length(SkeletonLine) > 1 then
      SkeletonSecondary := SkeletonLine[1].Trim;
    re.Expression := '^skeleton_horse([^;\n]+)';
    re.Exec;
    SkeletonHorse := re.Match[1].Trim;
    re.Expression := '^skeleton_elephant([^;\n]+)';
    re.Exec;
    SkeletonElephant := re.Match[1].Trim;
    re.Expression := '^skeleton_chariot([^;\n]+)';
    re.Exec;
    SkeletonChariot := re.Match[1].Trim;
    re.Expression := '^skeleton_camel([^;\n]+)';
    re.Exec;
    SkeletonCamel := re.Match[1].Trim;
    re.Expression := '^skeleton_scorpion_cart([^;\n]+)';
    re.Exec;
    SkeletonScorpion := re.Match[1].Trim;

    { textures }
    Textures.Clear;
    re.Expression := '^texture\s+([^;\s,]+)?.+(data[^;\s]+).*$';
    if re.Exec then
    begin
      Textures.Add(TDMBTexture.Create(re.Match[1].Trim, re.Match[2].Trim));
      while re.ExecNext do
        Textures.Add(TDMBTexture.Create(re.Match[1].Trim, re.Match[2].Trim));
    end;

    { default texture }
    if TextureFallback then
    begin
      { check for a default texture }
      for Texture in Textures do
        if Texture.Faction.IsEmpty then
          TextureFallbackFound := True;

      { use slave texture if default texture is missing }
      if not TextureFallbackFound then
        for Texture in Textures do
          if CompareStr(Texture.Faction, 'slave') = 0 then
          begin
            TextureFallbackFound := True;
            Textures.Insert(0, TDMBTexture.Create(Texture.Path));
            Break;
          end;

      { use merc texture if slave texture is missing }
      if not TextureFallbackFound then
        for Texture in Textures do
          if CompareStr(Texture.Faction, 'merc') = 0 then
          begin
            TextureFallbackFound := True;
            Textures.Insert(0, TDMBTexture.Create(Texture.Path));
            Break;
          end;

      { use first texture as last resort }
      if (not TextureFallbackFound) and (Textures.Count > 0) then
        Textures.Insert(0, TDMBTexture.Create(Textures[0].Path));
    end;

    { models }
    i := 0;
    re.Expression := '^model_flexi(_m|_c)?.+(data.+),\s*([^;\s]+)';
    if re.Exec then
    begin
      i := i + 1;
      Models.Add(TDMBModel.Create(re.Match[1].Trim, re.Match[2].Trim,
        re.Match[3].Trim));
      while re.ExecNext and (i < 4) do
      begin
        i := i + 1;
        Models.Add(TDMBModel.Create(re.Match[1].Trim, re.Match[2].Trim,
          re.Match[3].Trim));
      end;
    end;

    { fit to four models }
    case Models.Count of
      { duplicate single model three more times }
      1: for i := 1 to 3 do
          Models.Add(TDMBModel.Create(Models[0].Suffix, Models[0].Path,
            Models[0].Distance));
      { duplicate each model }
      2: begin
        Models.Insert(1, TDMBModel.Create(Models[0].Suffix, Models[0].Path,
          Models[0].Distance));
        Models.Add(TDMBModel.Create(Models[2].Suffix, Models[2].Path,
          Models[2].Distance));
      end;
      { duplicate first model }
      3: Models.Insert(1, TDMBModel.Create(Models[0].Suffix, Models[0].Path,
          Models[0].Distance));
    end;
    Models[3].Distance := 'max'; // final model must have max distance

    if OptimalDistance then
    begin
      ModelType := 'infantry';
      for i := 0 to High(HorseSkeletons) do
        if (SkeletonPrimary = HorseSkeletons[i]) or
          (SkeletonSecondary = HorseSkeletons[i]) then
          ModelType := 'horse';
      for i := 0 to High(ElephantSkeletons) do
        if (SkeletonPrimary = ElephantSkeletons[i]) or
          (SkeletonSecondary = ElephantSkeletons[i]) then
          ModelType := 'elephant';
      for i := 0 to High(CamelSkeletons) do
        if (SkeletonPrimary = CamelSkeletons[i]) or
          (SkeletonSecondary = CamelSkeletons[i]) then
          ModelType := 'camel';
      case ModelType of
        'infantry': for i := 0 to 3 do
            Models[i].Distance := OptimalDistanceInfantry[i];
        'horse': for i := 0 to 3 do
            Models[i].Distance := OptimalDistanceHorse[i];
        'elephant': for i := 0 to 3 do
            Models[i].Distance := OptimalDistanceElephant[i];
        'camel': for i := 0 to 3 do
            Models[i].Distance := OptimalDistanceCamel[i];
      end;
    end;
  finally
    FreeAndNil(re);
  end;
end;

function TDMBEntryListHelper.FText: string;
var
  i: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    for i := 0 to Pred(Count) do
      sl.Add(Items[i].Text);
    Result := sl.Text;
  finally
    FreeAndNil(sl);
  end;
end;

procedure TDMBEntryListHelper.LoadFromString(Input: string;
  TextureFallback: boolean; OptimalDistance: boolean);
var
  re: TRegExpr;
  DMBEntry: TDMBEntry;
begin
  Clear;
  re := TRegExpr.Create('^type(?:.+\n)+(?:model_sprite|model_tri).+$');
  try
    re.ModifierM := True;
    re.ModifierG := False;
    re.InputString := Input;
    if re.Exec then
    begin
      DMBEntry := TDMBEntry.Create(TextureFallback, OptimalDistance);
      DMBEntry.LoadFromString(re.Match[0]);
      Add(DMBEntry);
      while re.ExecNext do
      begin
        DMBEntry := TDMBEntry.Create(TextureFallback, OptimalDistance);
        DMBEntry.LoadFromString(re.Match[0]);
        Add(DMBEntry);
      end;
    end;
  finally
    FreeAndNil(re);
  end;
end;

end.
