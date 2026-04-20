# YS CAD Tools

This folder contains a small AutoLISP command pack for AutoCAD-compatible CAD tools.

Files:
- `ys_tools.lsp`: main command file

Commands:
- `Q`: restyles selected furniture geometry to yellow outer lines + red dashed inner lines
- `P`: removes `ACAD_DGNLINESTYLECOMP`, then runs full drawing purge
- `B`: quick-blocks the selected objects with an auto-generated block name
- `R`: draws a straight cabinet layout with X braces

How to load:
1. In CAD, run `APPLOAD`
2. Load `ys_tools.lsp`
3. If you want it every time, add it to Startup Suite

Current behavior:
- Loading this file overrides only the single-letter commands `Q`, `P`, `B`, and `R`
- `R` only supports closed straight cabinet outlines
- `R` supports ordinary right-angle rectangles
- `R` also supports straight rounded-end / half-arc cabinet outlines
- `R` does not support L-shaped cabinets
- `R` uses a target panel width of `600.0`, then evenly splits the full span to the nearest panel count
- `R` offsets inward by `30.0` by default
- `Q` uses grouped bounding-box classification to keep open outer perimeter lines yellow
- `Q` styles direct geometry and editable block definitions, but does not explode selected blocks
- `Q` still uses a lightweight geometry heuristic and may need another round for very irregular outer outlines
- `B` uses the lower-left corner of the selected objects as the block base point
- `B` creates a block name like `YS_BLK_20260420153045`
- Default outer color is yellow (`2`)
- Default inner color is red (`1`)
- Default inner linetype is `DASHED`

If you want a different cabinet split rule later, change this line in `ys_tools.lsp`:

`(setq *ys-cabinet-target-width* 600.0)`
