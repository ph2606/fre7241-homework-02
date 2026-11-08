# Record the package closure and immutable submission data.
# Run: Rscript --vanilla reproducibility/record_environment.R
suppressPackageStartupMessages(library(rutils))
record_hw2_environment <- function() {
  script_files <- c(
    sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)),
    unlist(lapply(sys.frames(), function(frame) frame$ofile)))
  script_files <- script_files[basename(script_files) == "record_environment.R"]
  root <- if (length(script_files)) {
    dirname(dirname(normalizePath(tail(script_files, 1L), mustWork = TRUE)))
  } else if (file.exists("data/hw2_inputs.rds")) {
    normalizePath(".", mustWork = TRUE)
  } else if (file.exists("../data/hw2_inputs.rds")) {
    normalizePath("..", mustWork = TRUE)
  } else stop("Cannot locate the homework input snapshot.")
  out_dir <- file.path(root, "reproducibility")
  dir.create(out_dir, showWarnings = FALSE)
  roots <- c("rutils", "HighFreq", "htmlwidgets", "dygraphs", "parallel", "digest")
  installed <- installed.packages()
  missing <- setdiff(roots, rownames(installed))
  if (length(missing)) stop("Run setup.R first. Missing: ", paste(missing, collapse = ", "))
  dependencies <- tools::package_dependencies(roots, db = installed,
    which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)
  packages <- sort(unique(c("base", roots, unlist(dependencies, use.names = FALSE))))
  packages <- packages[packages %in% rownames(installed)]
  pins <- c(rutils = "131a2b9b950c35a9ad48aee05444f76244ab0616",
            HighFreq = "505c9b74f96bbf1b703279772b191f1fa81743ab")
  package_rows <- lapply(packages, function(package) {
    description <- packageDescription(package)
    field <- function(name) {
      value <- description[[name]]
      if (is.null(value)) "" else paste(value, collapse = "; ")
    }
    sha <- field("RemoteSha")
    if (!nzchar(sha)) sha <- field("GithubSHA1")
    data.frame(package = package, version = field("Version"),
      direct_dependency = package %in% roots, priority = field("Priority"),
      repository = field("Repository"), github_owner = field("RemoteUsername"),
      github_repository = field("RemoteRepo"), github_sha = sha,
      expected_sha = if (package %in% names(pins)) pins[[package]] else "",
      compatibility_patch = field("FRE7241CompatibilityPatch"),
      stringsAsFactors = FALSE)
  })
  package_table <- do.call(rbind, package_rows)
  write.csv(package_table, file.path(out_dir, "package-versions.csv"), row.names = FALSE)

  input_file <- file.path(root, "data", "hw2_inputs.rds")
  inputs <- readRDS(input_file)
  stopifnot(xts::is.xts(inputs$spy_returns), xts::is.xts(inputs$stock_prices))
  provenance <- inputs$provenance
  if (is.null(provenance)) provenance <- list()
  value_or_empty <- function(x) if (is.null(x)) "" else as.character(x)
  snapshot_hash <- digest::digest(file = input_file, algo = "sha256")
  object_row <- function(name, source, source_hash = "") {
    x <- inputs[[name]]
    data.frame(dataset = name, path = "data/hw2_inputs.rds", source = source,
      rows = NROW(x), columns = NCOL(x),
      start_date = as.character(min(zoo::index(x))),
      end_date = as.character(max(zoo::index(x))),
      missing_values = sum(is.na(x)), duplicate_dates = sum(duplicated(zoo::index(x))),
      source_updated_utc = if (is.null(attr(x, "updated"))) "" else
        format(attr(x, "updated"), tz = "UTC", usetz = TRUE),
      sha256 = digest::digest(x, algo = "sha256", serialize = TRUE, serializeVersion = 2),
      hash_method = "SHA-256 of digest R serialization (version 2)",
      source_file_sha256 = source_hash,
      snapshot_rutils_commit = value_or_empty(provenance$rutils_commit),
      stringsAsFactors = FALSE)
  }
  manifest <- rbind(
    data.frame(dataset = "input_file", path = "data/hw2_inputs.rds",
      source = "Frozen homework input list: SPY returns, stock prices, provenance",
      rows = NA_integer_, columns = NA_integer_, start_date = "", end_date = "",
      missing_values = NA_integer_, duplicate_dates = NA_integer_, source_updated_utc = "",
      sha256 = snapshot_hash, hash_method = "SHA-256 of exact file bytes",
      source_file_sha256 = "", snapshot_rutils_commit = value_or_empty(provenance$rutils_commit),
      stringsAsFactors = FALSE),
    object_row("spy_returns", value_or_empty(provenance$spy)),
    object_row("stock_prices", value_or_empty(provenance$stock),
               value_or_empty(provenance$stock_file_sha256)))
  write.csv(manifest, file.path(out_dir, "data-manifest.csv"), row.names = FALSE, na = "")

  git_sha <- "Not recorded: the homework folder is not a Git repository."
  if (dir.exists(file.path(root, ".git")) && nzchar(Sys.which("git"))) {
    revision <- suppressWarnings(system2("git", c("-C", shQuote(root), "rev-parse", "HEAD"),
                                        stdout = TRUE, stderr = TRUE))
    if (is.null(attr(revision, "status")) && length(revision)) git_sha <- revision[[1L]]
  }
  lines <- c(
    paste("Recorded UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste("Homework Git revision:", git_sha),
    paste("Input file SHA-256:", snapshot_hash),
    "Package scope: six direct analysis/export/verification dependencies plus",
    "their recursive Depends, Imports, and LinkingTo closure (including base R).",
    "Object hashes use digest's R serialization version 2; file hashes use exact bytes.",
    "HighFreq lambda= works by partial matching to the formal lambdaf argument.",
    "", capture.output(sessionInfo()))
  writeLines(lines, file.path(out_dir, "session-info.txt"), useBytes = TRUE)
  message("Recorded ", NROW(package_table), " packages and ", NROW(manifest), " data hashes.")
  invisible(list(packages = package_table, data = manifest))
}

record_hw2_environment()
