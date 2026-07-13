# Optional LLM assistant ("Chat" tab): configuration, gating, system prompt, and
# the ellmer client builder.
#
# Like R/async.R this is an OPT-IN, gracefully-degrading layer that must never
# break the default offline / CI-green app:
#   * The agent packages (ellmer, shinychat) are NOT attached in global.R; they
#     are used only via `requireNamespace()` + `pkg::` inside the enabled path.
#   * `tahoe_agent_available()` is FALSE unless the packages are installed AND a
#     Vertex project is configured AND the kill switch is off. When FALSE the Chat
#     page renders a setup panel instead of the chat widget (see modules/chat_mod.R).
#   * No credential/network check happens here: Vertex ADC is resolved lazily by
#     ellmer on the first turn, so a misconfigured host degrades to a clean
#     turn-time error, never a load-time crash.
#
# Backend: ellmer::chat_google_vertex() with Gemini (gemini-2.5-flash by default).
# Auth = gcloud Application Default Credentials (no API key). Config lives in the
# gitignored .Renviron (see .Renviron.example).

# First non-empty env var among `vars`, else `default`. Mirrors tahoe_hf_token().
.tahoe_env_first <- function(vars, default = "") {
  for (v in vars) {
    val <- Sys.getenv(v, unset = "")
    if (nzchar(val)) {
      return(val)
    }
  }
  default
}

#' GCP project id for Vertex AI. Required to enable the assistant.
.tahoe_vertex_project <- function() {
  .tahoe_env_first(c(
    "TAHOE_VERTEX_PROJECT",
    "GOOGLE_CLOUD_PROJECT",
    "GCLOUD_PROJECT"
  ))
}

#' Vertex region. Defaults to us-central1 (Gemini flash is broadly available).
.tahoe_vertex_location <- function() {
  .tahoe_env_first(
    c("TAHOE_VERTEX_LOCATION", "GOOGLE_CLOUD_LOCATION", "GOOGLE_CLOUD_REGION"),
    default = "us-central1"
  )
}

#' Gemini model id (env-overridable; default per the locked decision).
tahoe_agent_model <- function() {
  .tahoe_env_first(c("TAHOE_VERTEX_MODEL", "GEMINI_MODEL"), "gemini-2.5-flash")
}

#' Sampling temperature (0-2). Low default for factual, grounded answers.
tahoe_agent_temperature <- function() {
  raw <- .tahoe_env_first(
    c("TAHOE_AGENT_TEMPERATURE", "GEMINI_TEMPERATURE"),
    "0.2"
  )
  val <- suppressWarnings(as.numeric(raw))
  if (is.na(val) || val < 0) 0.2 else val
}

# Bounds that keep cost/latency in check (also enforced tool-side).
.tahoe_agent_max_tokens <- function() 1024L
.tahoe_agent_row_cap <- function() 50L
.tahoe_agent_max_turns <- function() 25L

#' Whether the assistant can run. TRUE only when NOT force-disabled, ellmer +
#' shinychat are installed, and a Vertex project is configured. Intentionally
#' UNCACHED (the inputs are cheap Sys.getenv/requireNamespace) so env-toggling
#' tests work, and it NEVER touches the network.
tahoe_agent_available <- function() {
  if (.tahoe_env_truthy(Sys.getenv("TAHOE_AGENT_DISABLE", unset = ""))) {
    return(FALSE)
  }
  # gargle is needed for Vertex Application Default Credentials (it is a Suggests
  # of ellmer, so requiring it here also pins it into renv.lock).
  requireNamespace("ellmer", quietly = TRUE) &&
    requireNamespace("shinychat", quietly = TRUE) &&
    requireNamespace("gargle", quietly = TRUE) &&
    nzchar(.tahoe_vertex_project())
}

# --- System prompt -----------------------------------------------------------

# Markdown context files, assembled in order into the system prompt.
.tahoe_agent_prompt_files <- c(
  "00-persona.md",
  "10-dataset.md",
  "20-app-features.md",
  "30-data-schema.md",
  "40-subset-playbook.md",
  "50-guardrails.md"
)

# Where the prompt markdown lives: inst/agent/prompts under the app root. The app
# is sourced (not installed as a package), so this is always the on-disk path.
.tahoe_agent_prompt_dir <- function() {
  file.path(.tahoe_root, "inst", "agent", "prompts")
}

# Format a headline count for the live context block.
.tahoe_agent_num <- function(x) {
  if (length(x) != 1 || is.na(x)) "unknown" else format(x, big.mark = ",")
}

# A live "current session" block so the model uses real headline numbers and can
# caveat fixture vs real provenance. Degrades to "" if the data layer is
# unreachable, so prompt assembly never fails.
.tahoe_agent_live_context <- function() {
  cc <- tryCatch(tahoe_summary_counts(), error = function(e) NULL)
  if (is.null(cc)) {
    return("")
  }
  pin <- tryCatch(
    tahoe_dataset_pin(),
    error = function(e) list(repo = "tahoebio/Tahoe-100M", revision = "unknown")
  )
  paste0(
    "## Current session (live numbers -- prefer these over guessing)\n\n",
    sprintf("- Dataset: %s @ %s\n", pin$repo, substr(pin$revision, 1, 7)),
    sprintf(
      paste0(
        "- Live counts: drugs %s; assayed cell lines %s; samples %s; ",
        "plates %s; genes %s; cells %s.\n"
      ),
      .tahoe_agent_num(cc$drugs),
      .tahoe_agent_num(cc$cell_lines),
      .tahoe_agent_num(cc$samples),
      .tahoe_agent_num(cc$plates),
      .tahoe_agent_num(cc$genes),
      .tahoe_agent_num(cc$cells)
    ),
    sprintf(
      paste0(
        "- Provenance: small tables = %s, obs = %s. If 'fixture', these are ",
        "ILLUSTRATIVE synthetic numbers -- say so, and note real numbers need ",
        "the full dataset loaded.\n"
      ),
      cc$data_source %||% "unknown",
      cc$obs_source %||% "unknown"
    )
  )
}

#' Assemble the assistant's system prompt from the markdown context files plus a
#' live headline-counts block. Returns a single string.
tahoe_agent_system_prompt <- function() {
  dir <- .tahoe_agent_prompt_dir()
  parts <- vapply(
    .tahoe_agent_prompt_files,
    function(f) {
      p <- file.path(dir, f)
      if (file.exists(p)) {
        paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      } else {
        ""
      }
    },
    character(1)
  )
  parts <- parts[nzchar(parts)]
  live <- .tahoe_agent_live_context()
  pieces <- c(parts, if (nzchar(live)) live else NULL)
  paste(pieces, collapse = "\n\n---\n\n")
}

# --- ellmer client -----------------------------------------------------------

#' Build a per-session ellmer Chat backed by Vertex/Gemini, with the Tahoe tool
#' suite registered. Constructed once per Shiny session (at chat_agent_server
#' init) so each user gets an isolated conversation. Only called on the enabled
#' path, so ellmer is guaranteed present here.
tahoe_agent_client <- function(
  system_prompt = tahoe_agent_system_prompt(),
  tools = tahoe_agent_tools()
) {
  chat <- ellmer::chat_google_vertex(
    location = .tahoe_vertex_location(),
    project_id = .tahoe_vertex_project(),
    model = tahoe_agent_model(),
    system_prompt = system_prompt,
    params = ellmer::params(
      temperature = tahoe_agent_temperature(),
      max_tokens = .tahoe_agent_max_tokens()
    ),
    echo = "none"
  )
  for (spec in tools) {
    chat$register_tool(.tahoe_agent_ellmer_tool(spec))
  }
  chat
}
