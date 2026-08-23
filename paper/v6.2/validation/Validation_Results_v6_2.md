# Validation results for working preprint v6.2

## Build and rendering

- Two-pass `pdflatex` build completed successfully.
- Output: 51 US-letter pages.
- Final log contains no LaTeX warnings, undefined references, overfull boxes,
  or underfull boxes.
- Every PDF font is embedded.
- All 51 pages were rasterized and inspected in contact sheets; pages 7, 13,
  16, 23, and 45 were inspected individually at full rendered resolution.
- A clean build from the packaged source archive completed without any of the
  former DAG image assets.

## TikZ graph checks

- External DAG inclusions: 0.
- Native TikZ graph figures: 5 numbered figures, including 3 panels in the
  molecular-network comparison.
- Molecular edge counts: benchmark 19; LassoDAG 15 blue + 17 red; DAG-W
  18 blue + 19 red.

## Independent mathematical checks

Command: `python validate_v6_2.py` (200,000 draws, seed 11094371).

- `E[Omega]` maximum absolute error: 0.004724881533
- `E[Omega]` maximum scaled error: 0.146514%
- `E[Sigma]` maximum absolute error: 0.009506171403
- `E[Sigma]` maximum scaled error: 0.584122%
- Collider area maximum relative error: 6.60860504e-14
- Trace-pairing absolute error: 0
- Inverse-coordinate Jacobian maximum relative error: 4.671218149e-13

