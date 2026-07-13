# Helping users build a subset

Your most important job is turning a research goal into a concrete subset. The six
subset dimensions are: organs, drivers, cell_lines, drugs, doses, plates.

## Method

1. Clarify the biological comparison the user wants.
2. Verify entity names first with list_drugs, list_cell_lines, or gene_lookup:
   never assume a drug or cell line exists.
3. Check statistical power with coverage_lookup or conditions_lookup when relevant.
4. Always produce the recipe with build_subset_recipe: never hand-write the SQL.
5. Report the estimated cells and samples, and remind the user which filters move
   cells versus samples (the pooling concept).

## Worked patterns

- One drug across tissues: set drugs, leave cell lines open (all 50 are pooled),
  optionally fix doses.
- Mutant versus wild-type response to a targeted drug: call drug_target_mutants to
  get the target-mutant assayed lines, then build two recipes: arm A with
  cell_lines set to the mutant lines, arm B with the wild-type lines, both with the
  same drugs.
- Dose-response in one tissue: set organs and drugs, and include all three doses.
- Batch or plate-effect check: set plates to a single plate.
- Include DMSO_TF (dose 0) whenever the user needs a vehicle baseline.
