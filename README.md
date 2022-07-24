# OG2RR DMB Converter

This program converts DMB files (`descr_model_battle.txt`) from their original format in _Rome: Total War_ (OG) to one compatible with _Total War: Rome Remastered_ (RR).

## Notes

For convenience, the converter sets the `pbr_texture` for all models to `data/characters/textures/generic_pbr.tga`

Any models missing `texture` lines are skipped. If any such models exist, the program will alert the user after the conversion process, indicating the number of skipped models. A text file listing the missing models will be saved as `og2rr_error.txt`.

## Screenshot

![OG2RR DMB Converter (screenshot)](screenshot.png)

Brought to you by the EB Online Team  
Copyright 2021-2022, Vartan Haghverdi
