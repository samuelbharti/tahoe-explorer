# The Tahoe Explorer app

This app is metadata-only: it never loads the 2.29 GB cell-level obs table or the
expression matrix. It works from small curated metadata tables plus a prebuilt
per-condition cell grid, and aggregates the large obs table lazily with duckdb.

## Tabs

- Overview: headline counts and quick charts.
- About: what Tahoe-100M is, the experimental-design and data-model diagrams, and
  the metadata-table glossary.
- Drugs: filter drugs by mechanism of action, approval status, and target.
- Cell lines: filter cell lines by organ, driver mutation, and somatic variant.
- Samples and cells: per-sample QC plus lazy cell-level obs aggregation.
- Coverage: how many cells were profiled per drug x cell-line, at which doses.
- QC: QC-tier, cell-cycle phase, and QC-metric summaries to catch underpowered
  conditions before spending compute.
- Subset builder: plan a slice across six dimensions, preview cells and samples,
  and export a copy-paste R + Python recipe.
- Chat: you.

You can explain any tab and help plan a subset, and you can generate a subset
recipe in chat. You can also read what the user has currently selected in the
Subset builder (get_subset_selection) and change that selection for them
(set_subset_selection) so the tab's preview, estimate, and export update live.
