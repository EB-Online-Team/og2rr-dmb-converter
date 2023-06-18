# Changelog

## [1.0.0] - 2023-06-17

### Changed

- Ported GUI app from Python to Object Pascal (FPC/Lazarus), reducing release size.
- Entries with missing textures are now reported in the Messages memo box instead of outputting a log file.
- Default output filename is the OG path with the suffix `_RR.txt`.

### Added

- Option to add a default texture line to any units missing one. The default texture chosen will be (in decreasing order of priority) the slave texture, merc texture, or first texture.

## [0.3.0] - 2023-02-17

### Changed

- Optimized draw distance depending on whether the skeleton is of a human (15/30/40/max), horse (25/55/80/max), elephant (30/60/120/max), or camel (15/30/60/max). The algorithm determines skeleton type based on the game's built-in skeletons. Custom skeletons default to human distances.

### Fixed

- Limit output to 4 `model` lines (LODs). If there were originally fewer, copy lines as needed; if there were more, keep the top 4. The remaster crashes if a unit has more than 4 LODs.

## [0.2.1] - 2022-09-13

### Changed

- Format DMB in memory before converting.
- No longer skips models that lack textures.

### Fixed

- Formatted DMB now properly finds all entries, including those previously missed due to empty lines within entries.

## [0.2.0] - 2022-09-13

### Added

- MIT License.

### Changed

- Account for initial whitespace in DMB lines.
- Keep all model lines instead of only the first.
- Simplify instructions.
- Update screenshot.
- Start following [SemVer](https://semver.org/).

## [0.1] - 2022-07-23

### Added

- GUI app written in Python using the PySimpleGUI package.
- Ability to convert `descr_model_battle.txt` from _Rome: Total War_ (OG) to _Total War: Rome Remastered_ (RR).

[1.0.0]: https://github.com/vhaghverdi/og2rr-dmb-converter/compare/v0.3.0...v1.0.0
[0.3.0]: https://github.com/vhaghverdi/og2rr-dmb-converter/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/vhaghverdi/og2rr-dmb-converter/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/vhaghverdi/og2rr-dmb-converter/compare/v0.1...v0.2.0
[0.1]: https://github.com/vhaghverdi/og2rr-dmb-converter/releases/tag/v0.1
