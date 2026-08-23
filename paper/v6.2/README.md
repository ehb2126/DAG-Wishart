# Corrected working preprint v6.2

This folder is a self-contained release of the corrected DAG-Wishart working
preprint.

## Folder structure

```text
paper/v6.2/
├── README.md
├── pdf/
│   └── High_dimensional_Bayesian_DAG_v6_2.pdf
├── source/
│   ├── High_dimensional_Bayesian_DAG_v6_2.tex
│   └── figures/
│       ├── CallCenterError-2002-Final.pdf
│       └── Explanation.pdf
└── validation/
    ├── TikZ_DAG_Audit_v6_2.md
    ├── Validation_Results_v6_2.md
    └── validate_v6_2.py
```

All DAG figures are embedded in the `.tex` file as native TikZ. The two files
in `source/figures/` are historical numerical plots and are the only external
graphics required.

## Compile

From `paper/v6.2/source/`, run:

```sh
pdflatex -interaction=nonstopmode -halt-on-error High_dimensional_Bayesian_DAG_v6_2.tex
pdflatex -interaction=nonstopmode -halt-on-error High_dimensional_Bayesian_DAG_v6_2.tex
```

A standard TeX Live installation with TikZ/PGF is sufficient.

## arXiv submission

Upload `arxiv/High_dimensional_Bayesian_DAG_v6_2_arXiv_source.zip` directly
to arXiv. The archive places the main TeX file at its root and preserves the
`figures/` subdirectory expected by the `\includegraphics` commands.

## Validate

From `paper/v6.2/validation/`, run:

```sh
python validate_v6_2.py
```

The script requires Python and NumPy. Recorded results are in
`Validation_Results_v6_2.md`.
