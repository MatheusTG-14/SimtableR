#' Package Initialization Hooks
#' @param libname Library name path.
#' @param pkgname Package name string.
#' @keywords internal

.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion(pkgname)
  msg <- cli::rule(
    left = paste0("SimtablR ", ver),
    right = "Simple R functions for usefull tables"
  )
  packageStartupMessage(cli::col_cyan(msg))
  packageStartupMessage(
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue("tb()"),
    cli::col_white(": Descriptive tables (One-Table)\n"),
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue("diag_test()"),
    cli::col_white(": Confusion matrix and Diagnostic Values\n"),
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue("regtab()"),
    cli::col_white(": Regression Tables by Outcome (GLM)")
  )
  packageStartupMessage(
    cli::col_silver("Use suppressPackageStartupMessages() to silence.")
  )
}

.onLoad <- function(libname, pkgname) {
  # RStudio Variable Autocomplete Hook
  if (Sys.getenv("RSTUDIO") == "1") {
    tryCatch(
      {
        rstudio_env <- as.environment("tools:rstudio")
        if (
          exists(
            ".rs.registerAutocompleteReplacement",
            envir = rstudio_env,
            inherits = FALSE
          )
        ) {
          register_autocomplete <- get(
            ".rs.registerAutocompleteReplacement",
            envir = rstudio_env
          )
          register_autocomplete(
            pkgname,
            "tb",
            function(token, start, end, data) {
              if (!missing(data) && is.data.frame(data)) {
                return(names(data))
              }
              return(character(0))
            }
          )
        }
      },
      error = function(e) NULL
    )
  }

  # Register base generic S3 methods explicitly to guarantee interactive environment dispatch
  tryCatch(
    {
      namespace_env <- asNamespace(pkgname)
      base_env <- asNamespace("base")

      # rbind method
      registerS3method(
        "rbind",
        "tb",
        get("rbind.tb", envir = namespace_env),
        envir = base_env
      )

      # print methods
      registerS3method(
        "print",
        "tb",
        get("print.tb", envir = namespace_env),
        envir = base_env
      )
      registerS3method(
        "print",
        "rbind_tb",
        get("print.rbind_tb", envir = namespace_env),
        envir = base_env
      )

      # as.data.frame methods (critical due to double-dot generic ambiguity)
      registerS3method(
        "as.data.frame",
        "tb",
        get("as.data.frame.tb", envir = namespace_env),
        envir = base_env
      )
      registerS3method(
        "as.data.frame",
        "rbind_tb",
        get("as.data.frame.rbind_tb", envir = namespace_env),
        envir = base_env
      )
    },
    error = function(e) NULL
  )

  # External S3 Method Registration for Soft Dependencies (flextable)
  if (requireNamespace("flextable", quietly = TRUE)) {
    tryCatch(
      {
        flextable_env <- asNamespace("flextable")
        namespace_env <- asNamespace(pkgname)

        registerS3method(
          genname = "as_flextable",
          class = "tb",
          method = get("as_flextable.tb", envir = namespace_env),
          envir = flextable_env
        )
        registerS3method(
          genname = "as_flextable",
          class = "rbind_tb",
          method = get("as_flextable.rbind_tb", envir = namespace_env),
          envir = flextable_env
        )
      },
      error = function(e) NULL
    )
  }
}
