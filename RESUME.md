# Lexis Plot — Resume File for Next Agent

**Branch**: `lexis`
**Last commit**: `fcb4c9c` — "WIP: Lexis plot implementation"
**Date**: 2026-03-30
**R CMD check**: 0 errors, 0 warnings (as of last run)

This file captures the full context an agent needs to resume work on the
Lexis plot feature.  Read `AGENTS.md` for package-wide conventions and
architecture; this file focuses on **unresolved problems and design
decisions**.

---

## What exists

All code is in `R/Lexis.R` (~1112 lines).  Tests in
`tests/testthat/test-Lexis.R` (60 tests + 7 vdiffr snapshots, all
passing).  Vignette in `vignettes/Lexis.Rmd`.  Docs regenerated and
`NAMESPACE` updated.

### Implemented and working

| Component | Status |
|-----------|--------|
| `LexisPlot()` | Draws parallelogram with grid, axes, ticks, labels, titles, corner tips |
| `AddToLexis()` / `LexisPoints/Lines/Text/Polygon()` | All working |
| `LexisToXY()` / `XYToLexis()` | Public coordinate transforms (named-arg API) |
| `LexisPointValues()` / `ColourLexis()` / `ColorLexis()` | Tile colour fill |
| `LexisContour()` | Contour lines on parallelogram |
| Generalised axis system | Any two of {aRange, bRange, cRange}; third is derived |
| Free-axis ticks/labels on edges 2→3 and 3→4 | Working, with `free.fun` support |
| `free.fun` parameter | Allows user-supplied label transform for derived axis |
| `TernaryPoints()` etc. compatibility | Standard ternary functions work on Lexis plots |
| Four orientations | `point = "up"/"down"/"left"/"right"` all work |

---

## Critical unresolved issues

### Issue 1: Diagonal grid lines ≠ constant Cohort

**The fundamental problem.**  In a standard ternary plot, the three
families of grid lines represent constant a, constant b, and constant c
where a + b + c = k.  In the Lexis parallelogram:

- Family 1: constant coord1 (e.g. constant Age) ✓
- Family 2: constant coord2 (e.g. constant Period) ✓
- Family 3 (diagonal): constant (coord1′ + coord2′), i.e. constant
  (Age′ + Period′) where primes denote range-shifted values ✗

In demography, the third family should represent **constant Cohort =
Period − Age**.  But:

- constant (Age + Period) ≠ constant (Period − Age)
- These are geometrically different families of lines: in (Age, Period)
  Cartesian space, constant sum has slope −1 while constant difference
  has slope +1
- In the equilateral ternary grid, only the constant-sum family aligns
  with the third natural grid direction

**Consequence**: The `free.fun` feature (which labels the diagonal grid
lines) produces **inconsistent labels at opposite ends of the same
diagonal**.  A diagonal at shifted-sum d crosses edge 2→3 at one point
and edge 3→4 at another; `free.fun(coord1, coord2)` gives different
values at those two points because the diagonal represents constant
*sum*, not constant *difference*.

Concrete example with `aRange = c(0, 100)`, `bRange = c(1922, 2022)`:
- Diagonal at d = 50 crosses edge 2→3 at (Age=100, Period=1972) →
  Cohort = 1972 − 100 = 1872
- Same diagonal crosses edge 3→4 at (Age=50, Period=2022) →
  Cohort = 2022 − 50 = 1972
- The labels "1872" and "1972" appear on the same line, which is wrong

**Options considered**:

A. **Accept the limitation**: Diagonals represent constant (Age + Period),
   not Cohort.  Remove or document `free.fun` as a local labelling
   convenience only.  Users who need true constant-Cohort lines can draw
   them manually.

B. **Remap ternary axes** so that one ternary axis captures Cohort:
   e.g. set `a = Age′`, `b = Cohort′`, `c = k − Period′`.  Then the
   three grid families would be constant Age, constant Cohort, and
   constant Period.  This might work but changes the meaning of the
   ternary coordinates fundamentally and may break the parallelogram
   geometry (coordinates could go negative, pushing points outside the
   simplex).  **Not yet investigated in code.**

C. **Abandon the ternary framework** for Lexis plots entirely and build
   a custom parallelogram system from scratch.  Higher effort but avoids
   inheriting ternary constraints.

### Issue 2: Parallelogram orientation is reversed

**Current**: The parallelogram slants **top-to-the-right** (as Age
increases vertically, Period shifts right).

**Expected** (per demographic convention and reference images from Ian
Timaeus): The parallelogram should slant **top-to-the-left** (as Age
increases vertically, the diagonal edge goes left).

Current corner layout (for `aRange = c(0, 100)`, `bRange = c(1922, 2022)`,
c-free, direction = "up"):

```
C2 (Age=100, Period=1922) ---- C3 (Age=100, Period=2022)
       \                              \
        \                              \
C1 (Age=0, Period=1922) ----- C4 (Age=0, Period=2022)
```

The slant is top-right.  To get top-left, the parallelogram needs to
be mirrored.  Possible fixes:

- Swap the two bounded axes in `.ResolveLexisAxes()` (i.e. which one
  becomes axis1 vs axis2)
- Change the direction parameter or clockwise parameter
- Negate or reflect in `.LexisCoordsToXY()`

**This has not been diagnosed precisely** — it needs investigation of
how the ternary direction and axis ordering interact to produce the XY
layout.

---

## How the coordinate system works

Understanding this is critical for fixing the issues above.

### `.ResolveLexisAxes(aRange, bRange, cRange)`

Given two non-NULL ranges, determines:
- `freeAxis`: the NULL one (e.g. "c")
- `boundedAxes`: the two provided ones in order (e.g. c("a", "b"))
- `axisMap`: named integer `c(axis1=1, axis2=2, free=3)` mapping roles
  to triplicate indices

### `.LexisCoordsToXY(coord1, coord2, range1, range2, freeAxis, direction)`

1. Shifts coordinates to zero: `c1 = coord1 − range1[1]`, `c2 = coord2 − range2[1]`
2. Computes `k = diff(range1) + diff(range2)` and `free = k − c1 − c2`
3. Assembles a 3-row ternary matrix `abc` based on `freeAxis`:
   - If a is free: `abc = rbind(free, c1, c2)`
   - If b is free: `abc = rbind(c1, free, c2)`
   - If c is free: `abc = rbind(c1, c2, free)`
4. Passes to `TernaryCoords(abc, direction, region = ternRegionDefault)`

The key insight: the ternary a/b/c coordinates here are **not** the same
as the user's conceptual a/b/c axes.  They are shifted and the free axis
holds `k − c1′ − c2′`.

### Parallelogram corners

For c-free with `aRange = c(0, 100)`, `bRange = c(1922, 2022)`:

```
Corner  coord1(a)  coord2(b)  → c1′  c2′  free=k−c1′−c2′  → ternary (a,b,c)
C1      0          1922          0     0     200              (0, 0, 200)
C2      100        1922          100   0     100              (100, 0, 100)
C3      100        2022          100   100   0                (100, 100, 0)
C4      0          2022          0     100   100              (0, 100, 100)
```

These ternary triples are passed to `TernaryCoords()` which maps them
to XY using the equilateral triangle geometry.

### `TernaryCoords()` direction mapping

The `direction` parameter (1=up, 2=right, 3=down, 4=left) controls how
the equilateral triangle is oriented.  With `direction = 1` (up):
- a increases from bottom to top
- b increases towards the bottom-right
- c increases towards the bottom-left

The interaction between which axis is "free" (and thus placed where in
the ternary triple) and the direction parameter determines the
parallelogram orientation.  **This is what needs investigation to fix
Issue 2.**

---

## Architecture notes for the next agent

### Edge and corner conventions

Corners are numbered 1–4 and correspond to:
- C1: both bounded coords at minimum (coord1=min, coord2=min)
- C2: coord1 at max, coord2 at min
- C3: both at max
- C4: coord1 at min, coord2 at max

Edges:
- Edge 1→2: axis1 ticks/labels (coord2 constant at min)
- Edge 4→1: axis2 ticks/labels (coord1 constant at min)
- Edge 2→3: free-axis ticks/labels
- Edge 3→4: free-axis ticks/labels
- Corner 1: free axis tip
- Corner 2: axis1 tip
- Corner 4: axis2 tip

### `.LexisEdgeNormals(corners)`

Computes outward-pointing unit normals for each of the 4 edges.
Auto-corrects for winding direction using signed area.

### `.LexisTickDirs(lex)`

Computes tick direction vectors **parallel to each grid-line family**
(not perpendicular to the edge), so it's clear which grid family a tick
belongs to.  Then orients each outward using the edge normals.

### `.LexisGridFamily(lex, vals, family, col, lty, lwd)`

Draws one family of grid lines.  `family = "1"` for constant-axis1
lines, `"2"` for constant-axis2, `"d"` for diagonals.

### `.LexisDiagEndpoints(d, span1, span2)`

Finds the two points where the diagonal `c1′ + c2′ = d` intersects the
parallelogram boundary `[0, span1] × [0, span2]`.  Used by the diagonal
grid drawer.

### Tile system

`.LexisTriangleCentres(resolution, lex)` generates up- and down-triangle
centres across the parallelogram.  `LexisPointValues()` evaluates a
user function at those centres.  `ColourLexis()` delegates to
`ColourTernary()` for the actual tile painting.

---

## Relationship to existing Ternary infrastructure

### What is reused from the existing package

- `TernaryCoords()` / `XYToTernary()` — the core coordinate math
- `ColourTernary()` — tile painting (called by `ColourLexis()`)
- `.StartPlot()` — sets up the graphics device
- `.Triplicate()`, `.ValidateGridLines()`, `.GridExists()` — small helpers
- `ternRegionDefault` — the standard triangle region
- All standard `Ternary*()` plotting functions work on Lexis plots because
  they read from `options(".Last.triangle")`

### What is NOT reused (Lexis has its own)

- Grid line drawing (`.LexisGridFamily` — parallelogram boundary, not triangle)
- Axis ticks and labels (4 edges, not 3; free-axis ticks on diagonal crossings)
- Corner labels (4 corners, 3 labelled, not 3 corners)
- Edge normals (4-sided polygon, not 3-sided)
- Triangle centre generation (full parallelogram, not single triangle)
- Plot state structure (class `"LexisPlot"`, stores ranges, axisMap, etc.)

---

## Files to modify when making changes

1. `R/Lexis.R` — all Lexis source code
2. `tests/testthat/test-Lexis.R` — unit tests
3. `tests/testthat/_snaps/Lexis/` — vdiffr SVG snapshots (will need
   updating if visual output changes; run `testthat::snapshot_review()`)
4. `vignettes/Lexis.Rmd` — user-facing documentation
5. `AGENTS.md` — update the implementation progress section
6. `NEWS.md` — update changelog
7. `NAMESPACE` — regenerate via `devtools::document()`

---

## Suggested approach for fixing the two critical issues

### Fixing the orientation (Issue 2)

Start here — it's more tractable and may inform Issue 1.

1. Study how `TernaryCoords()` maps (a, b, c) → (x, y) for direction=1
   (up).  The key reference is `R/Coordinates.R`.
2. For the c-free APC case with `aRange = c(0, 100)`, `bRange =
   c(1922, 2022)`, compute the four corners by hand and verify what XY
   positions `TernaryCoords()` produces.
3. Determine what change flips the slant.  Candidates:
   - Swap which bounded axis is axis1 vs axis2
   - Change the ternary row ordering in `.LexisCoordsToXY()`
   - Change the direction parameter
4. Implement and verify with a reference plot.

### Fixing the diagonal semantics (Issue 1)

This is the harder, more fundamental issue.

The core question: **Can the ternary coordinate mapping be adjusted so
that the three natural grid families represent Age, Period, and Cohort?**

One idea (not yet tested): if we map `a = Age′`, `b = Period′ − Age′`
(= Cohort′ in shifted space), `c = k − Period′`, then:
- Constant a ⇒ constant Age ✓
- Constant b ⇒ constant Cohort ✓
- Constant c ⇒ constant Period ✓

But this means `b` can be negative (e.g. if Period < Age in shifted
coords), which pushes points outside the ternary simplex.  The ternary
coordinate functions may still work (they're just linear algebra), but
the parallelogram shape and grid lines need verification.

An alternative: build the parallelogram with a **non-equilateral**
geometry (a custom 2D affine coordinate system) rather than reusing the
ternary triangle.  This would be more work but avoids fighting the
ternary constraints.

A pragmatic option: **accept that the ternary diagonals represent
constant (Age + Period)** and document this clearly.  The diagonal grid
lines are still useful for reading off a + b values.  Users who need
true Cohort isolines can draw them as regular line segments using
`LexisLines()`.

### Ian Timaeus's reference

The feature was requested by Ian Timaeus (LSHTM) for demographic APC
plots.  See the email thread saved as `Wrapper functions for
Ternary.eml` and the external package https://github.com/BugBunny/APCplot.
The reference images from that package show left-leaning parallelograms
with true Cohort diagonals.  Understanding exactly what that package does
geometrically would help clarify the target.

---

## Quick-start for the next agent

```r
# Load the package from source
devtools::load_all(".")

# Run tests
devtools::test()

# Check a basic Lexis plot
par(mar = rep(0.2, 4))
LexisPlot()

# Check the APC case (shows both issues)
par(mar = rep(2, 4))
LexisPlot(
  aRange = c(0, 100), bRange = c(1922, 2022),
  alab = "Age", blab = "Period", clab = "Cohort",
  free.fun = function(a, b) b - a,
  grid.lines = 10, grid.minor.lines = 0
)

# Verify: does the parallelogram slant right or left?
# Verify: are the Cohort labels on opposite edges consistent?
```
