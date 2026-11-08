# Reproduce the R dependencies without upgrading unrelated installations.
# Run in a fresh process: Rscript --vanilla setup.R
setup_hw2 <- function() {
  options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 900,
          install.packages.check.source = "no")
  if (.Platform$OS.type == "windows") options(pkgType = "binary")
  install_library <- .libPaths()[1L]
  pins <- c(rutils = "131a2b9b950c35a9ad48aee05444f76244ab0616",
            HighFreq = "505c9b74f96bbf1b703279772b191f1fa81743ab")
  cran <- c("htmlwidgets", "dygraphs", "digest", "remotes", "callr")
  missing <- setdiff(cran, rownames(installed.packages()))
  if (length(missing)) {
    install.packages(missing, lib = install_library, dependencies = NA,
                     type = if (.Platform$OS.type == "windows") "binary" else "source")
  }

  # Child builds must not load a course .Rprofile or hold package DLLs open.
  empty_profile <- tempfile("hw2-install-profile-", fileext = ".R")
  stopifnot(file.create(empty_profile))
  previous_profile <- Sys.getenv("R_PROFILE_USER", unset = NA_character_)
  Sys.setenv(R_PROFILE_USER = empty_profile)
  on.exit({
    if (is.na(previous_profile)) Sys.unsetenv("R_PROFILE_USER") else
      Sys.setenv(R_PROFILE_USER = previous_profile)
    unlink(empty_profile)
  }, add = TRUE)

  installed_sha <- function(package) {
    if (!package %in% rownames(installed.packages())) return("")
    description <- packageDescription(package)
    sha <- description$RemoteSha
    if (is.null(sha)) sha <- description$GithubSHA1
    if (is.null(sha)) "" else sha
  }
  install_remote <- function(package, sha) {
    diagnostics <- character()
    result <- tryCatch(callr::r(
      function(package, sha, lib) {
        options(repos = c(CRAN = "https://cloud.r-project.org"),
                timeout = 900, install.packages.check.source = "no")
        if (.Platform$OS.type == "windows") options(pkgType = "binary")
        Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "false")
        remotes::install_github(paste0("algoquant/", package, "@", sha),
          lib = lib, dependencies = NA, upgrade = "never",
          build_vignettes = FALSE, force = TRUE)
        if (!identical(packageDescription(package, lib.loc = lib)$RemoteSha, sha)) {
          stop("Pinned package installation did not complete: ", package)
        }
      }, args = list(package = package, sha = sha, lib = install_library),
      libpath = .libPaths(), user_profile = FALSE,
      stdout = "|", stderr = "2>&1", show = TRUE,
      callback = function(line, ...) diagnostics <<- c(diagnostics, line)),
      error = identity)
    list(result = result, diagnostics = c(diagnostics,
      if (inherits(result, "error")) conditionMessage(result) else character()))
  }

  install_highfreq_compat <- function(sha) {
    # The pinned release requests C++11. R >= 4.6 on Windows removed CXX11;
    # recent RcppArmadillo also requires a later standard. Only these two
    # build declarations are changed; the numerical source is unchanged.
    build_dir <- tempfile("hw2-HighFreq-compat-")
    dir.create(build_dir)
    build_dir <- normalizePath(build_dir, winslash = "/", mustWork = TRUE)
    temp_root <- normalizePath(tempdir(), winslash = "/", mustWork = TRUE)
    if (!startsWith(tolower(build_dir), paste0(tolower(temp_root), "/"))) {
      stop("Refusing cleanup outside the R session temporary directory.")
    }
    on.exit(unlink(build_dir, recursive = TRUE), add = TRUE)
    archive <- file.path(build_dir, "HighFreq.tar.gz")
    download.file(paste0("https://github.com/algoquant/HighFreq/archive/", sha,
                         ".tar.gz"), archive, mode = "wb")
    untar(archive, exdir = build_dir)
    source_dirs <- list.dirs(build_dir, recursive = FALSE, full.names = TRUE)
    source_dirs <- source_dirs[file.exists(file.path(source_dirs, "DESCRIPTION"))]
    stopifnot(length(source_dirs) == 1L)
    source_dir <- source_dirs[[1L]]
    makevars <- file.path(source_dir, "src", "Makevars.win")
    lines <- readLines(makevars, warn = FALSE)
    setting <- grep("^[[:space:]]*CXX_STD[[:space:]]*=[[:space:]]*CXX11[[:space:]]*$", lines)
    if (length(setting) != 1L) stop("Unexpected pinned HighFreq build settings.")
    lines[setting] <- "CXX_STD = CXX17"
    writeLines(lines, makevars, useBytes = TRUE)
    description_file <- file.path(source_dir, "DESCRIPTION")
    description <- as.list(read.dcf(description_file)[1L, ])
    if (!identical(description$SystemRequirements, "GNU make, C++11")) {
      stop("Unexpected pinned HighFreq SystemRequirements.")
    }
    provenance <- list(
      RemoteType = "github", RemoteHost = "api.github.com",
      RemoteUsername = "algoquant", RemoteRepo = "HighFreq", RemoteRef = sha,
      RemoteSha = sha, GithubUsername = "algoquant", GithubRepo = "HighFreq",
      GithubRef = sha, GithubSHA1 = sha,
      FRE7241CompatibilityPatch = paste(
        "src/Makevars.win: CXX_STD = CXX11 -> CXX_STD = CXX17;",
        "DESCRIPTION: SystemRequirements = GNU make, C++11 -> GNU make, C++17"))
    description$SystemRequirements <- "GNU make, C++17"
    description[names(provenance)] <- provenance
    write.dcf(as.data.frame(description, check.names = FALSE), description_file)
    message("Installing pinned HighFreq with the Windows C++17 build declarations.")
    callr::r(function(path, lib, provenance) {
      options(repos = c(CRAN = "https://cloud.r-project.org"),
              timeout = 900, pkgType = "binary",
              install.packages.check.source = "no")
      Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "false")
      remotes::install_local(path, lib = lib, dependencies = NA,
        upgrade = "never", build_vignettes = FALSE, force = TRUE)
      installed_dir <- find.package("HighFreq", lib.loc = lib)
      # Restore GitHub provenance overwritten by the local installation.
      remotes::add_metadata(installed_dir, c(provenance, list(RemoteUrl = NULL)))
      md5_file <- file.path(installed_dir, "MD5")
      if (file.exists(md5_file)) {
        checksums <- readLines(md5_file, warn = FALSE)
        writeLines(checksums[!grepl("[ *](DESCRIPTION|Meta/package[.]rds)$", checksums)],
                   md5_file)
      }
      if (!identical(packageDescription("HighFreq", lib.loc = lib)$RemoteSha,
                     provenance$RemoteSha)) stop("HighFreq provenance check failed.")
    }, args = list(path = source_dir, lib = install_library, provenance = provenance),
    libpath = .libPaths(), user_profile = FALSE, show = TRUE)
  }

  for (package in names(pins)) {
    sha <- pins[[package]]
    if (identical(installed_sha(package), sha)) {
      message("Retaining ", package, " at ", sha)
      next
    }
    if (package == "HighFreq" && .Platform$OS.type == "windows" &&
        getRversion() >= "4.6.0") {
      install_highfreq_compat(sha)
      next
    }
    attempted <- install_remote(package, sha)
    if (!inherits(attempted$result, "error")) next
    standard_mismatch <- any(grepl(
      "C\\+\\+14 compiler required|C\\+\\+ compilation standard is too old.*C\\+\\+14",
      attempted$diagnostics)) &&
      any(grepl("RcppArmadillo|armadillo_bits", attempted$diagnostics))
    if (package == "HighFreq" && .Platform$OS.type == "windows" && standard_mismatch) {
      install_highfreq_compat(sha)
    } else stop(attempted$result)
  }
  for (package in names(pins)) {
    stopifnot(identical(installed_sha(package), pins[[package]]))
  }
  message("Homework 2 dependencies are ready. Source builds require an R toolchain.")
  invisible(pins)
}

setup_hw2()
