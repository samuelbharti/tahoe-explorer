# Generate small, fully synthetic metadata fixtures committed under
# data/fixtures/. These let the app, the shinytest2 smoke test, and the module
# tests run with no network and no real data. The values are invented (never a
# slice of the real Tahoe-100M dataset) and the schema mirrors the real tables.
#
# Run from the app root:  Rscript dev/make_fixtures.R

suppressWarnings(suppressMessages({
  library(DBI)
  library(duckdb)
}))

set.seed(100)

out_dir <- file.path("data", "fixtures")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

n_drug <- 24L
n_cell <- 18L
n_sample <- 60L
n_gene <- 120L
n_obs <- 4000L

organs <- c("Lung", "Bowel", "Breast", "Skin", "Pancreas", "Ovary")
moa_broad <- c(
  "inhibitor/antagonist",
  "agonist",
  "modulator",
  "other"
)
moa_fine <- c(
  "Kinase inhibitor",
  "RAS inhibitor",
  "HDAC inhibitor",
  "Cyclooxygenase inhibitor",
  "Tubulin inhibitor",
  "unclear"
)
driver_genes <- c("TP53", "KRAS", "EGFR", "BRAF", "PIK3CA", "MYC")
var_types <- c("Missense", "Gain", "Stopgain", "Frameshift")
phases <- c("G1", "S", "G2M")
doses <- c(0.05, 0.5, 5.0)

pad <- function(prefix, i, width = 3) {
  sprintf("%s%0*d", prefix, width, i)
}

drug_names <- paste0("Synthdrug-", pad("", seq_len(n_drug)))

drug_metadata <- data.frame(
  drug = drug_names,
  targets = vapply(
    seq_len(n_drug),
    function(i) {
      paste(sample(driver_genes, sample(1:3, 1)), collapse = ", ")
    },
    character(1)
  ),
  "moa-broad" = sample(moa_broad, n_drug, replace = TRUE),
  "moa-fine" = sample(moa_fine, n_drug, replace = TRUE),
  "human-approved" = sample(c("yes", "no"), n_drug, replace = TRUE),
  "clinical-trials" = sample(c("yes", "no"), n_drug, replace = TRUE),
  "gpt-notes-approval" = "Synthetic record for demo purposes.",
  canonical_smiles = sample(
    c("CCO", "CC(=O)O", "c1ccccc1", "CCN(CC)CC"),
    n_drug,
    replace = TRUE
  ),
  pubchem_cid = sample(1000:99999, n_drug),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

cell_metadata <- data.frame(
  cell_name = paste0("SYNTH-CL", pad("", seq_len(n_cell), 2)),
  Cell_ID_DepMap = paste0("ACH-", pad("", seq_len(n_cell), 6)),
  Cell_ID_Cellosaur = paste0("CVCL_", pad("", seq_len(n_cell), 4)),
  Organ = sample(organs, n_cell, replace = TRUE),
  Driver_Gene_Symbol = sample(driver_genes, n_cell, replace = TRUE),
  Driver_VarZyg = sample(c("Het", "Hom"), n_cell, replace = TRUE),
  Driver_VarType = sample(var_types, n_cell, replace = TRUE),
  Driver_ProtEffect_or_CdnaEffect = "p.X000X",
  Driver_Mech_InferDM = sample(
    c("LoF", "GoF", "unknown"),
    n_cell,
    replace = TRUE
  ),
  Driver_GeneType_DM = sample(
    c("oncogene", "TSG"),
    n_cell,
    replace = TRUE
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

sample_drug <- sample(drug_names, n_sample, replace = TRUE)
sample_dose <- sample(doses, n_sample, replace = TRUE)
sample_metadata <- data.frame(
  sample = paste0("smp_", pad("", seq_len(n_sample), 4)),
  plate = paste0("plate", sample(1:4, n_sample, replace = TRUE)),
  mean_gene_count = round(runif(n_sample, 1500, 6000), 1),
  mean_tscp_count = round(runif(n_sample, 5000, 40000), 1),
  mean_mread_count = round(runif(n_sample, 8000, 60000), 1),
  mean_pcnt_mito = round(runif(n_sample, 0.01, 0.15), 4),
  drug = sample_drug,
  drugname_drugconc = sprintf(
    "[('%s', %s, 'uM')]",
    sample_drug,
    sample_dose
  ),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

gene_metadata <- data.frame(
  gene_symbol = paste0("GENE", pad("", seq_len(n_gene), 4)),
  ensembl_id = paste0("ENSG", pad("", seq_len(n_gene), 11)),
  token_id = seq_len(n_gene),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

obs_idx <- sample(seq_len(n_sample), n_obs, replace = TRUE)
obs_drug <- sample_metadata$drug[obs_idx]
obs_dose <- sample_dose[obs_idx]
obs_metadata <- data.frame(
  plate = sample_metadata$plate[obs_idx],
  BARCODE_SUB_LIB_ID = paste0("bc_", pad("", seq_len(n_obs), 6)),
  sample = sample_metadata$sample[obs_idx],
  gene_count = as.integer(round(runif(n_obs, 800, 8000))),
  tscp_count = as.integer(round(runif(n_obs, 2000, 50000))),
  mread_count = as.integer(round(runif(n_obs, 4000, 80000))),
  drugname_drugconc = sprintf("[('%s', %s, 'uM')]", obs_drug, obs_dose),
  drug = obs_drug,
  cell_line = sample(cell_metadata$cell_name, n_obs, replace = TRUE),
  sublibrary = sample(c("sublib_A", "sublib_B"), n_obs, replace = TRUE),
  BARCODE = paste0("AAAC", pad("", seq_len(n_obs), 8)),
  pcnt_mito = round(runif(n_obs, 0, 0.2), 4),
  S_score = round(rnorm(n_obs), 3),
  G2M_score = round(rnorm(n_obs), 3),
  phase = sample(phases, n_obs, replace = TRUE),
  # QC tier, matching the real obs vocabulary: "full" = passed the full-
  # stringency filter (high quality), "minimal" = passed only minimal QC.
  pass_filter = sample(
    c("full", "minimal"),
    n_obs,
    replace = TRUE,
    prob = c(0.9, 0.1)
  ),
  cell_name = sample(cell_metadata$cell_name, n_obs, replace = TRUE),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# The real cell_line_metadata table is driver-level: many rows per cell line,
# one per driver mutation. Expand the 18 distinct lines into 2-6 driver rows
# each so the fixture mirrors that shape (nrow > distinct cell_name). Built
# after obs so the other fixtures' RNG stream is unchanged.
cell_line_metadata <- do.call(
  rbind,
  lapply(seq_len(n_cell), function(i) {
    base <- cell_metadata[i, , drop = FALSE]
    k <- sample(2:6, 1)
    genes_i <- sample(driver_genes, min(k, length(driver_genes)))
    data.frame(
      cell_name = base$cell_name,
      Cell_ID_DepMap = base$Cell_ID_DepMap,
      Cell_ID_Cellosaur = base$Cell_ID_Cellosaur,
      Organ = base$Organ,
      Driver_Gene_Symbol = genes_i,
      Driver_VarZyg = sample(c("Het", "Hom"), length(genes_i), replace = TRUE),
      Driver_VarType = sample(var_types, length(genes_i), replace = TRUE),
      Driver_ProtEffect_or_CdnaEffect = "p.X000X",
      Driver_Mech_InferDM = sample(
        c("LoF", "GoF", "unknown"),
        length(genes_i),
        replace = TRUE
      ),
      Driver_GeneType_DM = sample(
        c("oncogene", "TSG"),
        length(genes_i),
        replace = TRUE
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
)

# Synthetic somatic variants keyed on Cell_ID_DepMap, mirroring the shape of a
# DepMap OmicsSomaticMutations export (one row per variant per cell line). Built
# after cell_line_metadata so the earlier fixtures' RNG stream is unchanged.
variant_genes <- c(driver_genes, "PTEN", "RB1", "APC", "CTNNB1")
v_types <- c("SNV", "insertion", "deletion")
consequences <- c(
  "missense_variant",
  "stop_gained",
  "frameshift_variant",
  "splice_region_variant",
  "synonymous_variant"
)
zygosities <- c("Homozygous", "Heterozygous")
aa <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]
cell_variants <- do.call(
  rbind,
  lapply(seq_len(n_cell), function(i) {
    k <- sample(2:8, 1)
    pos <- sample(20:900, k, replace = TRUE)
    # A few lines mimic the Cellosaurus curated-driver fallback (with zygosity,
    # no hotspot/LoF/dbSNP flags); the rest mimic the DepMap full profile.
    cello <- i %% 6 == 1
    data.frame(
      cell_name = cell_metadata$cell_name[i],
      source = if (cello) "Cellosaurus" else "DepMap",
      gene = sample(variant_genes, k, replace = TRUE),
      protein_change = sprintf(
        "p.%s%d%s",
        sample(aa, k, replace = TRUE),
        pos,
        sample(aa, k, replace = TRUE)
      ),
      dna_change = sprintf(
        "c.%d%s>%s",
        pos * 3L,
        sample(c("A", "C", "G", "T"), k, replace = TRUE),
        sample(c("A", "C", "G", "T"), k, replace = TRUE)
      ),
      variant_type = if (cello) {
        "Mutation"
      } else {
        sample(v_types, k, replace = TRUE)
      },
      consequence = if (cello) {
        "Simple"
      } else {
        sample(consequences, k, replace = TRUE)
      },
      hotspot = if (cello) FALSE else runif(k) < 0.1,
      likely_lof = if (cello) FALSE else runif(k) < 0.25,
      dbsnp = if (cello) {
        NA_character_
      } else {
        ifelse(
          runif(k) < 0.5,
          sprintf("rs%d", sample(1000:9999999, k, replace = TRUE)),
          NA_character_
        )
      },
      zygosity = if (cello) {
        sample(zygosities, k, replace = TRUE)
      } else {
        NA_character_
      },
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })
)

# Vehicle control. The real obs table carries a DMSO_TF arm on every plate, for
# every cell line, at no dose. It is deliberately absent from drug_metadata, so
# the real data has 379 drugs but 380 distinct drugs in obs. Without it the QC
# tab's control breakdown has no data to render, and its test can only skip.
# Built last so the earlier fixtures' RNG stream is unchanged.
control_drug <- "DMSO_TF"
control_plates <- sort(unique(sample_metadata$plate))
control_lines <- cell_metadata$cell_name
n_control <- length(control_plates) * length(control_lines)

control_samples <- data.frame(
  sample = paste0("smp_ctrl_", pad("", seq_along(control_plates), 2)),
  plate = control_plates,
  mean_gene_count = round(runif(length(control_plates), 1500, 6000), 1),
  mean_tscp_count = round(runif(length(control_plates), 5000, 40000), 1),
  mean_mread_count = round(runif(length(control_plates), 8000, 60000), 1),
  mean_pcnt_mito = round(runif(length(control_plates), 0.01, 0.15), 4),
  drug = control_drug,
  drugname_drugconc = sprintf("[('%s', 0.0, 'uM')]", control_drug),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
sample_metadata <- rbind(sample_metadata, control_samples)

# One control well for each plate and cell line, so the control is complete
# across plates and lines exactly as it is in the real data.
control_grid <- expand.grid(
  plate = control_plates,
  cell_name = control_lines,
  KEEP.OUT.ATTRS = FALSE,
  stringsAsFactors = FALSE
)
control_obs <- data.frame(
  plate = control_grid$plate,
  BARCODE_SUB_LIB_ID = paste0("bc_ctrl_", pad("", seq_len(n_control), 6)),
  sample = control_samples$sample[
    match(control_grid$plate, control_samples$plate)
  ],
  gene_count = as.integer(round(runif(n_control, 800, 8000))),
  tscp_count = as.integer(round(runif(n_control, 2000, 50000))),
  mread_count = as.integer(round(runif(n_control, 4000, 80000))),
  drugname_drugconc = sprintf("[('%s', 0.0, 'uM')]", control_drug),
  drug = control_drug,
  cell_line = control_grid$cell_name,
  sublibrary = sample(c("sublib_A", "sublib_B"), n_control, replace = TRUE),
  BARCODE = paste0("CCCC", pad("", seq_len(n_control), 8)),
  pcnt_mito = round(runif(n_control, 0, 0.2), 4),
  S_score = round(rnorm(n_control), 3),
  G2M_score = round(rnorm(n_control), 3),
  phase = sample(phases, n_control, replace = TRUE),
  pass_filter = sample(
    c("full", "minimal"),
    n_control,
    replace = TRUE,
    prob = c(0.9, 0.1)
  ),
  cell_name = control_grid$cell_name,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
stopifnot(identical(names(control_obs), names(obs_metadata)))
obs_metadata <- rbind(obs_metadata, control_obs)

con <- dbConnect(duckdb())
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

write_fixture <- function(df, name) {
  path <- file.path(out_dir, paste0(name, ".parquet"))
  duckdb_register(con, "tmp_fixture", df)
  dbExecute(
    con,
    sprintf(
      "COPY tmp_fixture TO '%s' (FORMAT PARQUET)",
      path
    )
  )
  duckdb_unregister(con, "tmp_fixture")
  cat(sprintf("wrote %s (%d rows)\n", path, nrow(df)))
}

write_fixture(drug_metadata, "drug_metadata")
write_fixture(cell_line_metadata, "cell_line_metadata")
write_fixture(sample_metadata, "sample_metadata")
write_fixture(gene_metadata, "gene_metadata")
write_fixture(obs_metadata, "obs_metadata")
write_fixture(cell_variants, "cell_line_variants")

cat("Synthetic fixtures written to", out_dir, "\n")
