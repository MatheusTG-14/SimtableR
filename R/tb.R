#' Frequency and Summary Tables
#'
#' Creates comprehensive tables for categorical or continuous variables with formatting,
#' statistical tests, prevalence ratios (PR), odds ratios (OR), and column stratification.
#'
#' @param data A data.frame or atomic vector.
#' @param ... Variables to be tabulated. Accepts variable names and/or flags
#'   (`m`, `p`, `row`, `col`, `rp`, `or`) for controlling output format.
#' @param m Logical. Include missing values (NA) in the table. Default: `FALSE`.
#' @param d Integer. Decimal places for percentages and statistics. Default: `1`.
#' @param format Logical. Render a formatted grid output. Default: `TRUE`.
#' @param style Character. Format for displaying counts and percentages.
#'   Options: `"n_pct"` (default), `"pct_n"`, or a custom template with `{n}` and
#'   `{p}` placeholders, e.g. `"{n} [{p}%]"`.
#' @param style.rp Character. Format string for Prevalence Ratio.
#'   Default: `"{rp} ({lower} - {upper})"`.
#' @param style.or Character. Format string for Odds Ratio.
#'   Default: `"{or} ({lower} - {upper})"`.
#' @param test Logical or Character. Performs statistical test on 2x2+ tables.
#'   `TRUE` for automatic selection, or one of `"chisq"`, `"fisher"`, `"mcnemar"`.
#' @param subset Logical expression for row filtering.
#' @param strat Variable for column stratification. Disables PR/OR calculations.
#' @param rp Logical. Calculate Prevalence Ratios (PR). Default: `FALSE`.
#' @param or Logical. Calculate Odds Ratios (OR). Default: `FALSE`.
#' @param ref Character or numeric. Reference level for PR/OR calculations.
#' @param conf.level Numeric. Confidence level for intervals (0-1). Default: `0.95`.
#' @param var.type Named character vector specifying variable types, e.g.
#'   `c(age = "continuous")` or a scalar string `"continuous"`.
#' @param stat.cont Character. `"mean"` (Mean/SD) or `"median"` (Median/IQR).
#'   Default: `"median"`.
#' @param flags Character vector. Programmatic alternative for specifying formatting flags.
#' @param labels Named character vector. Custom display labels for variables.
#'
#' @return An object of class `c("tb", "simtab")`.
#' @export
tb <- function(
  data,
  ...,
  m = FALSE,
  d = 1,
  format = TRUE,
  style = "n_pct",
  style.rp = "{rp} ({lower} - {upper})",
  style.or = "{or} ({lower} - {upper})",
  test = FALSE,
  subset = NULL,
  strat = NULL,
  rp = FALSE,
  or = FALSE,
  ref = NULL,
  conf.level = 0.95,
  var.type = NULL,
  stat.cont = "median",
  flags = NULL,
  labels = NULL
) {
  if (missing(data)) {
    stop(
      "No data provided. Please supply a data.frame or vector.",
      call. = FALSE
    )
  }
  if (is.matrix(data) || (!is.data.frame(data) && !is.atomic(data))) {
    stop("'data' must be a data.frame or atomic vector.", call. = FALSE)
  }
  if (!is.numeric(d) || d < 0 || d > 10) {
    stop("'d' must be a number between 0 and 10.", call. = FALSE)
  }
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1) {
    stop("'conf.level' must be between 0 and 1.", call. = FALSE)
  }
  if (is.character(test)) {
    test <- tolower(test)
    valid_tests <- c("chisq", "fisher", "mcnemar")
    if (!test %in% valid_tests) {
      stop(
        "Invalid test method. Use one of: chisq, fisher, mcnemar.",
        call. = FALSE
      )
    }
  }

  d <- as.integer(d)
  call_matched <- match.call()

  subset_expr <- substitute(subset)
  strat_expr <- substitute(strat)
  dots <- as.list(substitute(list(...)))[-1]

  if (is.data.frame(data)) {
    env_data <- data
    env_parent <- parent.frame()
    arg_expr <- dots
  } else {
    env_data <- NULL
    env_parent <- parent.frame()
    data_sym <- substitute(data)
    arg_expr <- c(list(data_sym), dots)
  }

  flag_names <- c("m", "p", "row", "col", "rp", "or")
  flags_list <- list(missing = m, percent = FALSE, by = "total")

  if (!is.null(flags)) {
    for (f in flags) {
      if (f == "m") {
        flags_list$missing <- TRUE
      }
      if (f == "rp") {
        rp <- TRUE
      }
      if (f == "or") {
        or <- TRUE
      }
      if (f == "p") {
        flags_list$percent <- TRUE
        flags_list$by <- "total"
      }
      if (f == "row") {
        flags_list$percent <- TRUE
        flags_list$by <- "row"
      }
      if (f == "col") {
        flags_list$percent <- TRUE
        flags_list$by <- "col"
      }
    }
  }

  clean_arg_expr <- list()
  for (expr in arg_expr) {
    if (is.symbol(expr)) {
      sym_name <- as.character(expr)
      if (sym_name %in% flag_names) {
        if (is.data.frame(data) && sym_name %in% names(data)) {
          warning(
            sprintf(
              "Ambiguity detected: '%s' matches a formatting flag but is also a column name in the dataset. Treating '%s' as a variable. Use the 'flags' argument to safely pass formatting options.",
              sym_name,
              sym_name
            ),
            call. = FALSE,
            immediate. = TRUE
          )
          clean_arg_expr <- c(clean_arg_expr, expr)
        } else {
          if (sym_name == "m") {
            flags_list$missing <- TRUE
          }
          if (sym_name == "rp") {
            rp <- TRUE
          }
          if (sym_name == "or") {
            or <- TRUE
          }
          if (sym_name == "p") {
            flags_list$percent <- TRUE
            flags_list$by <- "total"
          }
          if (sym_name == "row") {
            flags_list$percent <- TRUE
            flags_list$by <- "row"
          }
          if (sym_name == "col") {
            flags_list$percent <- TRUE
            flags_list$by <- "col"
          }
        }
      } else {
        clean_arg_expr <- c(clean_arg_expr, expr)
      }
    } else {
      clean_arg_expr <- c(clean_arg_expr, expr)
    }
  }

  vars <- list()
  var_names <- character()
  var_labels <- character()

  for (i in seq_along(clean_arg_expr)) {
    expr <- clean_arg_expr[[i]]
    nm <- deparse(expr, width.cutoff = 500L)[1]
    val <- tryCatch(
      eval(expr, env_data, env_parent),
      error = function(e) {
        stop(sprintf("Variable '%s' not found.", nm), call. = FALSE)
      }
    )
    if (!is.atomic(val) && !is.factor(val)) {
      stop(
        sprintf("Variable '%s' must be atomic or factor.", nm),
        call. = FALSE
      )
    }

    lbl <- .resolve_label(nm, data, labels)
    vars[[i]] <- val
    var_names[i] <- nm
    var_labels[i] <- lbl
  }

  if (length(vars) == 0) {
    stop("No variables specified.", call. = FALSE)
  }
  if (length(vars) > 2) {
    stop("Maximum of 2 variables allowed.", call. = FALSE)
  }

  row_var_name <- var_names[1]
  row_label <- var_labels[1]
  col_var_name <- if (length(vars) == 2) var_names[2] else NULL
  col_label <- if (length(vars) == 2) var_labels[2] else NULL

  strat_val <- NULL
  if (!is.null(strat_expr)) {
    strat_val <- tryCatch(
      eval(strat_expr, env_data, env_parent),
      error = function(e) {
        stop("Stratification variable not found.", call. = FALSE)
      }
    )
    if (length(strat_val) != length(vars[[1]])) {
      stop("Stratification variable length mismatch.", call. = FALSE)
    }
    if (rp || or) {
      warning(
        "PR/OR calculations are disabled when stratification is used because crude stratified ratios can be misleading due to confounding. Use regtab() to perform multivariable or stratified regression for adjusted effect measures.",
        call. = FALSE,
        immediate. = TRUE
      )
      rp <- FALSE
      or <- FALSE
    }
  }

  if (!is.null(subset_expr)) {
    subset_val <- tryCatch(
      eval(subset_expr, env_data, env_parent),
      error = function(e) stop("Error evaluating subset.", call. = FALSE)
    )
    if (!is.logical(subset_val)) {
      stop("Subset must be logical.", call. = FALSE)
    }

    keep <- subset_val & !is.na(subset_val)
    if (sum(keep) == 0) {
      stop("Subset removed all observations.", call. = FALSE)
    }

    vars <- lapply(vars, `[`, keep)
    if (!is.null(strat_val)) strat_val <- strat_val[keep]
  }

  if (!is.null(strat_val)) {
    if (length(vars) == 2) {
      if (!flags_list$missing) {
        ok <- !is.na(strat_val)
        vars <- lapply(vars, `[`, ok)
        strat_val <- strat_val[ok]
      }
      vars[[2]] <- interaction(
        strat_val,
        vars[[2]],
        sep = " : ",
        drop = TRUE,
        lex.order = TRUE
      )
    } else {
      vars[[2]] <- factor(strat_val)
    }
    col_var_name <- if (is.null(col_var_name)) {
      "Stratum"
    } else {
      paste0(col_var_name, " (Stratified)")
    }
    col_label <- col_var_name
  }

  is_continuous <- FALSE
  if (!is.null(var.type)) {
    if (
      length(var.type) == 1 &&
        (is.null(names(var.type)) || all(names(var.type) == ""))
    ) {
      var.type <- setNames(var.type, row_var_name)
    }
    if (row_var_name %in% names(var.type)) {
      type_spec <- tolower(var.type[[row_var_name]])

      if (type_spec %in% c("continuous", "cont", "numeric", "num")) {
        is_continuous <- TRUE
      }
    }
  } else if (is.numeric(vars[[1]]) && !is.factor(vars[[1]])) {
    is_continuous <- TRUE
    message(sprintf(
      "Variable '%s' automatically treated as continuous because it is numeric. Use 'var.type' to override.",
      row_var_name
    ))
  }

  res_data <- list()
  res_meta <- list(
    is_continuous = is_continuous,
    stat.cont = stat.cont,
    d = d,
    style = style,
    style.rp = style.rp,
    style.or = style.or,
    conf.level = conf.level,
    labels = labels,
    row_var_name = row_var_name,
    row_label = row_label,
    col_var_name = col_var_name,
    col_label = col_label,
    flags = flags_list,
    stats = NULL
  )

  if (is_continuous) {
    y <- vars[[1]]
    x <- if (length(vars) > 1) vars[[2]] else NULL

    if (!is.numeric(y)) {
      stop(
        sprintf("Variable '%s' is not numeric.", row_var_name),
        call. = FALSE
      )
    }

    if (!is.null(x)) {
      if (is.factor(x)) {
        x <- droplevels(x)
      }
      if (!flags_list$missing) {
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
      }
    } else {
      y <- y[!is.na(y)]
    }

    if (!is.null(test) && !(is.logical(test) && !test)) {
      message(sprintf(
        "Note: Statistical test choice for continuous variables is driven by 'stat.cont' ('%s'), not by empirical normality testing. Ensure this assumption aligns with your data's true distribution.",
        stat.cont
      ))
    }

    calc_raw_stats <- function(val) {
      if (length(val) == 0) {
        return(numeric(0))
      }
      if (stat.cont == "mean") {
        return(c(mean = mean(val, na.rm = TRUE), sd = sd(val, na.rm = TRUE)))
      } else {
        return(as.numeric(quantile(
          val,
          probs = c(0.5, 0.25, 0.75),
          na.rm = TRUE
        )))
      }
    }

    summary_list <- list()
    if (is.null(x)) {
      summary_list[["Total"]] <- calc_raw_stats(y)
    } else {
      levs <- if (is.factor(x)) levels(x) else sort(unique(x))
      if (flags_list$missing && any(is.na(x))) {
        levs <- c(levs, NA)
      }

      for (l in levs) {
        sub_y <- if (is.na(l)) y[is.na(x)] else y[x == l & !is.na(x)]
        lbl_l <- if (is.na(l)) "<NA>" else as.character(l)
        summary_list[[lbl_l]] <- calc_raw_stats(sub_y)
      }
      summary_list[["Total"]] <- calc_raw_stats(y)
    }

    res_data$summary <- summary_list

    stats_res <- NULL
    if ((isTRUE(test) || is.character(test)) && !is.null(x)) {
      tryCatch(
        {
          n_groups <- length(unique(x[!is.na(x)]))
          if (n_groups >= 2) {
            if (stat.cont == "mean") {
              stats_res <- if (n_groups == 2) {
                t.test(y ~ x)
              } else {
                fit <- lm(y ~ x)
                list(p.value = anova(fit)$`Pr(>F)`[1], method = "One-way ANOVA")
              }
            } else {
              stats_res <- if (n_groups == 2) {
                wilcox.test(y ~ x, exact = FALSE)
              } else {
                kruskal.test(y ~ x)
              }
            }
          }
        },
        error = function(e) NULL
      )
    }
    res_meta$stats <- stats_res
  } else {
    if (!is.null(ref)) {
      row_var <- if (is.factor(vars[[1]])) vars[[1]] else factor(vars[[1]])
      ref_str <- as.character(ref)
      if (ref_str %in% levels(row_var)) {
        vars[[1]] <- relevel(row_var, ref = ref_str)
      } else if (is.numeric(ref) && ref %in% seq_along(levels(row_var))) {
        vars[[1]] <- relevel(row_var, ref = levels(row_var)[ref])
      } else {
        stop(
          sprintf(
            "Reference level '%s' not found in row variable levels.",
            ref_str
          ),
          call. = FALSE
        )
      }
    } else if (rp || or) {
      levs <- if (is.factor(vars[[1]])) {
        levels(vars[[1]])
      } else {
        levels(factor(vars[[1]]))
      }
      ref <- levs[1]
      message(sprintf(
        "Note: No reference level specified for PR/OR calculation. Defaulting to the first level: '%s'.",
        ref
      ))
    }

    vars <- lapply(vars, function(v) if (is.factor(v)) droplevels(v) else v)
    useNA <- if (flags_list$missing) "always" else "no"
    tab <- table(vars, useNA = useNA)

    if (sum(tab) == 0) {
      stop("Table is empty.", call. = FALSE)
    }

    pct_full <- NULL
    if (flags_list$percent) {
      if (length(dim(tab)) == 1) {
        pct_full <- as.vector(tab / sum(tab) * 100)
      } else {
        nr <- nrow(tab)
        nc <- ncol(tab)
        pct_full <- matrix(NA_real_, nr, nc)
        pct_calc <- switch(
          flags_list$by,
          total = tab / sum(tab),
          row = tab / rowSums(tab),
          col = sweep(tab, 2, colSums(tab), "/")
        )
        pct_full[seq_len(nr), seq_len(nc)] <- pct_calc * 100
      }
    }

    ratios_df <- NULL
    if ((rp || or) && length(dim(tab)) == 2 && ncol(tab) >= 2) {
      event_col_idx <- ncol(tab)
      total_events <- sum(tab[, event_col_idx])
      total_n <- sum(tab)
      prev <- total_events / total_n

      if (or && prev > 0.10) {
        message(sprintf(
          "Note: Outcome prevalence is %.1f%%. Odds ratios overestimate the Prevalence Ratio in common outcomes (>10%%). Consider using rp = TRUE (Prevalence Ratio) for cross-sectional data, or Poisson regression via regtab() for adjusted estimates.",
          prev * 100
        ))
      }

      events <- tab[, event_col_idx]
      totals <- rowSums(tab)
      risks <- events / totals

      n_rows <- nrow(tab)
      row_levs <- rownames(tab)

      ratios_df <- data.frame(
        variable = rep(row_var_name, n_rows),
        level = row_levs,
        estimate = rep(NA_real_, n_rows),
        lower_ci = rep(NA_real_, n_rows),
        upper_ci = rep(NA_real_, n_rows),
        p_value = rep(NA_real_, n_rows),
        ref = rep(FALSE, n_rows),
        type = rep(if (rp) "PR" else "OR", n_rows),
        stringsAsFactors = FALSE
      )
      ratios_df$ref[1] <- TRUE
      ratios_df$estimate[1] <- 1.0

      z_crit <- qnorm(1 - (1 - conf.level) / 2)

      for (i in seq_len(n_rows)[-1]) {
        if (rp) {
          if (events[i] > 0 && events[1] > 0 && risks[1] > 0 && risks[i] > 0) {
            est <- risks[i] / risks[1]
            se_log <- sqrt(
              (1 / events[i] - 1 / totals[i]) + (1 / events[1] - 1 / totals[1])
            )
            if (!is.na(se_log) && se_log > 0) {
              ratios_df$estimate[i] <- est
              ratios_df$lower_ci[i] <- exp(log(est) - z_crit * se_log)
              ratios_df$upper_ci[i] <- exp(log(est) + z_crit * se_log)
              z_stat <- log(est) / se_log
              ratios_df$p_value[i] <- 2 * (1 - pnorm(abs(z_stat)))
            }
          }
        } else if (or) {
          a_i <- events[i]
          b_i <- totals[i] - a_i
          a_1 <- events[1]
          b_1 <- totals[1] - a_1

          if (a_i > 0 && b_i > 0 && a_1 > 0 && b_1 > 0) {
            est <- (a_i * b_1) / (b_i * a_1)
            se_log <- sqrt(1 / a_i + 1 / b_i + 1 / a_1 + 1 / b_1)
            if (!is.na(se_log) && se_log > 0) {
              ratios_df$estimate[i] <- est
              ratios_df$lower_ci[i] <- exp(log(est) - z_crit * se_log)
              ratios_df$upper_ci[i] <- exp(log(est) + z_crit * se_log)
              z_stat <- log(est) / se_log
              ratios_df$p_value[i] <- 2 * (1 - pnorm(abs(z_stat)))
            }
          }
        }
      }
    }

    res_data$frequencies <- tab
    res_data$percentages <- pct_full
    res_data$ratios <- ratios_df

    stats_res <- NULL
    if ((isTRUE(test) || is.character(test)) && length(dim(tab)) == 2) {
      type <- if (is.character(test)) test else "chisq"
      tryCatch(
        {
          stats_res <- switch(
            type,
            chisq = {
              cc <- chisq.test(tab)
              if (any(cc$expected < 5)) {
                warning(
                  sprintf(
                    "Chi-squared test requirement violated: expected counts < 5. Results may be unreliable. Consider setting test = 'fisher'."
                  ),
                  call. = FALSE,
                  immediate. = TRUE
                )
              }
              cc
            },
            fisher = fisher.test(tab, workspace = 2e7),
            mcnemar = mcnemar.test(tab)
          )
        },
        error = function(e) NULL
      )
    }
    res_meta$stats <- stats_res
  }

  out_obj <- list(
    data = res_data,
    meta = res_meta,
    call = call_matched
  )
  class(out_obj) <- c("tb", "simtab")
  return(out_obj)
}

#' Combine Objects by Rows
#'
#' @param ... Objects to be combined.
#' @param deparse.level Integer controlling label deparsing.
#' @return A combined object.
#' @export
rbind <- function(..., deparse.level = 1) {
  dots <- list(...)
  if (length(dots) == 0) {
    stop("No objects provided to rbind.", call. = FALSE)
  }
  if (inherits(dots[[1]], "tb") || inherits(dots[[1]], "rbind_tb")) {
    return(rbind.tb(..., deparse.level = deparse.level))
  }
  return(base::rbind(..., deparse.level = deparse.level))
}

#' Combine tb Objects by Rows
#'
#' Vertical stacking of `tb` objects to create multi-variable tables (foundations for Table 1).
#'
#' @param ... Objects of class `tb` to be combined.
#' @param deparse.level Integer controlling label deparsing (unused).
#' @return A combined object of class `c("rbind_tb", "simtab")`.
#' @method rbind tb
#' @export
rbind.tb <- function(..., deparse.level = 1) {
  dots <- list(...)
  if (length(dots) == 0) {
    stop("No objects provided to rbind.", call. = FALSE)
  }
  for (i in seq_along(dots)) {
    if (!inherits(dots[[i]], "tb")) {
      stop(sprintf("Argument %d is not a 'tb' object.", i), call. = FALSE)
    }
  }
  ref_meta <- dots[[1]]$meta
  for (i in seq_along(dots)[-1]) {
    if (dots[[i]]$meta$col_var_name != ref_meta$col_var_name) {
      stop(
        sprintf(
          "Column variable mismatch at argument %d: '%s' vs '%s'. All objects must share the same column structure.",
          i,
          dots[[i]]$meta$col_var_name,
          ref_meta$col_var_name
        ),
        call. = FALSE
      )
    }
  }
  out_obj <- list(
    data = lapply(dots, function(x) x$data),
    meta = list(
      tables = lapply(dots, function(x) x$meta),
      row_label = "Variable",
      col_label = ref_meta$col_label,
      col_var_name = ref_meta$col_var_name
    ),
    call = match.call()
  )
  class(out_obj) <- c("rbind_tb", "simtab")
  return(out_obj)
}

#' Helper function for internal label resolution
#' @keywords internal
.resolve_label <- function(var_name, data, labels_arg) {
  if (!is.null(labels_arg) && var_name %in% names(labels_arg)) {
    return(labels_arg[[var_name]])
  }
  if (is.data.frame(data) && var_name %in% names(data)) {
    lbl <- attr(data[[var_name]], "label", exact = TRUE)
    if (!is.null(lbl)) return(lbl)
  }
  return(var_name)
}

#' Shared Display Matrix Builder
#' @keywords internal
.build_display_matrix <- function(x) {
  if (inherits(x, "rbind_tb")) {
    mats <- list()
    all_cols <- character()
    for (i in seq_along(x$data)) {
      tb_i <- list(data = x$data[[i]], meta = x$meta$tables[[i]], call = NULL)
      class(tb_i) <- c("tb", "simtab")
      mats[[i]] <- .build_display_matrix(tb_i)
      all_cols <- unique(c(all_cols, colnames(mats[[i]])))
    }
    parts <- list()
    for (i in seq_along(x$data)) {
      mat_i <- mats[[i]]
      is_cont_i <- x$meta$tables[[i]]$is_continuous
      if (is_cont_i) {
        new_mat <- matrix(
          "",
          nrow = 1,
          ncol = length(all_cols),
          dimnames = list(rownames(mat_i), all_cols)
        )
        cols_intersect <- intersect(colnames(mat_i), all_cols)
        new_mat[1, cols_intersect] <- mat_i[1, cols_intersect]
        parts[[length(parts) + 1]] <- new_mat
      } else {
        nr_i <- nrow(mat_i)
        is_2d <- length(dim(x$data[[i]]$frequencies)) == 2
        has_total_row <- rownames(mat_i)[nr_i] == "Total"
        rows_to_keep <- seq_len(nr_i)
        if (has_total_row && is_2d && i < length(x$data)) {
          rows_to_keep <- seq_len(nr_i - 1)
        }
        sub_mat <- mat_i[rows_to_keep, , drop = FALSE]
        var_lbl <- x$meta$tables[[i]]$row_label
        header_mat <- matrix(
          "",
          nrow = 1,
          ncol = length(all_cols),
          dimnames = list(var_lbl, all_cols)
        )
        level_mat <- matrix(
          "",
          nrow = nrow(sub_mat),
          ncol = length(all_cols),
          dimnames = list(rownames(sub_mat), all_cols)
        )
        cols_intersect <- intersect(colnames(sub_mat), all_cols)
        level_mat[, cols_intersect] <- sub_mat[, cols_intersect]
        rnames <- rownames(level_mat)
        for (r_idx in seq_along(rnames)) {
          if (rnames[r_idx] != "Total") {
            rnames[r_idx] <- paste0("  ", rnames[r_idx])
          }
        }
        rownames(level_mat) <- rnames
        parts[[length(parts) + 1]] <- header_mat
        parts[[length(parts) + 1]] <- level_mat
      }
    }
    return(do.call(rbind, parts))
  }

  is_continuous <- x$meta$is_continuous
  d <- x$meta$d
  style <- x$meta$style

  if (is_continuous) {
    raw_sum <- x$data$summary
    out_mat <- matrix(
      "",
      nrow = 1,
      ncol = length(raw_sum),
      dimnames = list(x$meta$row_label, names(raw_sum))
    )

    stat_lbl <- if (x$meta$stat.cont == "mean") "Mean (SD)" else "Median (IQR)"
    rownames(out_mat) <- paste0(rownames(out_mat), " [", stat_lbl, "]")

    for (nm in names(raw_sum)) {
      val <- raw_sum[[nm]]
      if (length(val) == 0) {
        out_mat[1, nm] <- "-"
      } else if (x$meta$stat.cont == "mean") {
        out_mat[1, nm] <- sprintf(
          paste0("%.", d, "f (%.", d, "f)"),
          val[1],
          val[2]
        )
      } else {
        out_mat[1, nm] <- sprintf(
          paste0("%.", d, "f (%.", d, "f - %.", d, "f)"),
          val[1],
          val[2],
          val[3]
        )
      }
    }
    return(out_mat)
  } else {
    freq <- x$data$frequencies
    pct <- x$data$percentages
    flags <- x$meta$flags

    freq_m <- addmargins(freq)
    if (length(dim(freq)) == 1) {
      names(freq_m)[length(freq_m)] <- "Total"
      freq_mat <- as.matrix(freq_m)
      colnames(freq_mat) <- "Freq"
      if (!is.null(pct)) {
        pct_mat <- as.matrix(c(pct, NA_real_))
      } else {
        pct_mat <- NULL
      }
    } else {
      freq_mat <- freq_m
      rownames(freq_mat)[nrow(freq_mat)] <- "Total"
      colnames(freq_mat)[ncol(freq_mat)] <- "Total"
      pct_mat <- pct
    }

    nr <- nrow(freq_mat)
    nc <- ncol(freq_mat)
    out_mat <- matrix("", nrow = nr, ncol = nc, dimnames = dimnames(freq_mat))

    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        val <- freq_mat[i, j]
        has_pct <- !is.null(pct_mat) &&
          i <= nrow(pct_mat) &&
          j <= ncol(pct_mat) &&
          !is.na(pct_mat[i, j])

        if (flags$percent && has_pct) {
          p_str <- sprintf(paste0("%.", d, "f"), pct_mat[i, j])
          if (style == "n_pct") {
            out_mat[i, j] <- sprintf("%d (%s%%)", val, p_str)
          } else if (style == "pct_n") {
            out_mat[i, j] <- sprintf("%s%% (%d)", p_str, val)
          } else {
            txt <- gsub("{n}", val, style, fixed = TRUE)
            txt <- gsub("{p}", p_str, txt, fixed = TRUE)
            out_mat[i, j] <- txt
          }
        } else {
          out_mat[i, j] <- as.character(val)
        }
      }
    }

    if (!is.null(x$data$ratios)) {
      ratios <- x$data$ratios
      ratio_text <- vapply(
        seq_len(nrow(ratios)),
        function(idx) {
          if (ratios$ref[idx]) {
            return("1.00 (Ref)")
          }
          if (is.na(ratios$estimate[idx])) {
            return("-")
          }

          fmt_style <- if (ratios$type[idx] == "PR") {
            x$meta$style.rp
          } else {
            x$meta$style.or
          }
          ph <- if (ratios$type[idx] == "PR") "{rp}" else "{or}"

          txt <- gsub(
            ph,
            sprintf("%.2f", ratios$estimate[idx]),
            fmt_style,
            fixed = TRUE
          )
          txt <- gsub(
            "{lower}",
            sprintf("%.2f", ratios$lower_ci[idx]),
            txt,
            fixed = TRUE
          )
          txt <- gsub(
            "{upper}",
            sprintf("%.2f", ratios$upper_ci[idx]),
            txt,
            fixed = TRUE
          )

          p_val <- ratios$p_value[idx]
          p_str <- if (is.na(p_val)) {
            ""
          } else if (p_val < 0.001) {
            ", p < 0.001"
          } else {
            sprintf(", p = %.3f", p_val)
          }
          paste0(txt, p_str)
        },
        character(1)
      )

      if (length(ratio_text) < nr) {
        ratio_text <- c(ratio_text, rep("", nr - length(ratio_text)))
      }
      lbl_col <- if (ratios$type[1] == "PR") "PR (95% CI)" else "OR (95% CI)"
      out_mat <- cbind(out_mat, ratio_text)
      colnames(out_mat)[ncol(out_mat)] <- lbl_col
    }
    return(out_mat)
  }
}

#' Print Method for tb Objects
#'
#' @param x A `tb` object.
#' @param digits Minimum number of significant digits to be printed.
#' @param ... Additional arguments.
#' @return Invisibly returns `x`.
#' @export
print.tb <- function(x, digits = NULL, ...) {
  out_mat <- .build_display_matrix(x)
  .print_grid_adapted(out_mat, x$meta$row_label, x$meta$col_label, x)
  invisible(x)
}

#' Print Method for rbind_tb Objects
#'
#' @param x An `rbind_tb` object.
#' @param digits Minimum number of significant digits to be printed.
#' @param ... Additional arguments.
#' @return Invisibly returns `x`.
#' @export
print.rbind_tb <- function(x, digits = NULL, ...) {
  out_mat <- .build_display_matrix(x)
  .print_grid_adapted(out_mat, x$meta$row_label, x$meta$col_label, x)
  invisible(x)
}

#' Adapted Grid Printer
#' @keywords internal
.print_grid_adapted <- function(out_mat, row_var, col_var, x) {
  safe_nchar <- function(s) nchar(ifelse(is.na(s), "NA", s))
  center_text <- function(txt, width) {
    txt <- if (is.na(txt)) "NA" else txt
    pad <- max(0L, width - nchar(txt))
    paste0(strrep(" ", floor(pad / 2)), txt, strrep(" ", pad - floor(pad / 2)))
  }
  right_text <- function(txt, width) {
    sprintf(paste0("%", width, "s"), if (is.na(txt)) "NA" else txt)
  }

  nr <- nrow(out_mat)
  nc <- ncol(out_mat)
  row_labels <- rownames(out_mat)
  col_labels <- colnames(out_mat)

  has_row_total <- row_labels[nr] == "Total"
  extra_cols <- if (any(grepl("PR \\(|OR \\(", col_labels))) 1L else 0L

  width_row <- max(safe_nchar(c(row_var, row_labels))) + 1L
  col_widths <- vapply(
    seq_len(nc),
    function(j) {
      max(safe_nchar(col_labels[j]), max(safe_nchar(out_mat[, j]))) + 2L
    },
    integer(1)
  )

  console_width <- if (requireNamespace("cli", quietly = TRUE)) {
    cli::console_width()
  } else {
    80L
  }
  j_start <- 1L

  while (j_start <= nc) {
    used <- width_row + 3L
    j_end <- j_start
    while (j_end <= nc) {
      if (used + col_widths[j_end] + 1L > console_width && j_end > j_start) {
        j_end <- j_end - 1L
        break
      }
      used <- used + col_widths[j_end] + 1L
      j_end <- j_end + 1L
    }
    if (j_end > nc) {
      j_end <- nc
    }
    cols_page <- j_start:j_end
    start_extra <- nc - extra_cols + 1L
    header_cols <- cols_page[cols_page < start_extra]

    if (!is.null(col_var) && col_var != "" && length(header_cols) > 0) {
      cat(right_text("", width_row), " | ", sep = "")
      data_w <- sum(col_widths[header_cols]) + max(0L, length(header_cols) - 1L)
      cat(center_text(col_var, data_w), "\n")
    }

    cat(right_text(row_var, width_row), " |", sep = "")
    for (j in cols_page) {
      idx_sum <- nc - extra_cols
      is_extra <- j > idx_sum
      if (
        isTRUE(has_row_total) &&
          j == idx_sum &&
          idx_sum > 1 &&
          j != cols_page[1]
      ) {
        cat("|")
      }
      if (is_extra && j != cols_page[1]) {
        cat("|")
      }
      cat(center_text(col_labels[j], col_widths[j]))
    }
    cat("\n", strrep("-", width_row), "-+", sep = "")
    for (j in cols_page) {
      idx_sum <- nc - extra_cols
      is_extra <- j > idx_sum
      if (
        isTRUE(has_row_total) &&
          j == idx_sum &&
          idx_sum > 1 &&
          j != cols_page[1]
      ) {
        cat("+")
      }
      if (is_extra && j != cols_page[1]) {
        cat("+")
      }
      cat(strrep("-", col_widths[j]))
    }
    cat("\n")

    for (i in seq_len(nr)) {
      if (i == nr && has_row_total && nr > 1) {
        cat(strrep("-", width_row), "-+", sep = "")
        for (j in cols_page) {
          idx_sum <- nc - extra_cols
          is_extra <- j > idx_sum
          if (
            isTRUE(has_row_total) &&
              j == idx_sum &&
              idx_sum > 1 &&
              j != cols_page[1]
          ) {
            cat("+")
          }
          if (is_extra && j != cols_page[1]) {
            cat("+")
          }
          cat(strrep("-", col_widths[j]))
        }
        cat("\n")
      }
      cat(right_text(row_labels[i], width_row), " |", sep = "")
      for (j in cols_page) {
        idx_sum <- nc - extra_cols
        is_extra <- j > idx_sum
        if (
          isTRUE(has_row_total) &&
            j == idx_sum &&
            idx_sum > 1 &&
            j != cols_page[1]
        ) {
          cat("|")
        }
        if (is_extra && j != cols_page[1]) {
          cat("|")
        }
        cat(center_text(out_mat[i, j], col_widths[j]))
      }
      cat("\n")
    }
    j_start <- j_end + 1L
    if (j_start <= nc) cat("\n")
  }

  stats <- x$meta$stats
  if (!is.null(stats)) {
    p_str <- if (stats$p.value < 0.001) {
      "< 0.001"
    } else {
      sprintf("= %.3f", stats$p.value)
    }
    cat("\n  Test:", stats$method, " p-value", p_str, "\n")
  }
}

#' Convert tb to Data Frame
#'
#' @param x A `tb` object.
#' @param row.names NULL or a character vector giving the row names for the data frame.
#' @param optional Logical. If TRUE, setting row names and converting column names is optional.
#' @param tidy Logical. If `TRUE`, returns a long-format tidy data frame with raw numeric values.
#' @param ... Additional arguments.
#' @return A data.frame.
#' @export
as.data.frame.tb <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  tidy = FALSE,
  ...
) {
  if (isTRUE(tidy)) {
    if (x$meta$is_continuous) {
      df_tidy <- data.frame(
        variable = x$meta$row_var_name,
        group = names(x$data$summary),
        estimate = vapply(
          x$data$summary,
          function(v) if (length(v) > 0) v[1] else NA_real_,
          numeric(1)
        ),
        stringsAsFactors = FALSE
      )
      return(df_tidy)
    } else {
      freq <- x$data$frequencies
      df_freq <- as.data.frame(freq, stringsAsFactors = FALSE)
      if (length(dim(freq)) == 1) {
        colnames(df_freq) <- c("level", "count")
        df_freq$variable <- x$meta$row_var_name
        df_freq$percentage <- if (!is.null(x$data$percentages)) {
          as.vector(x$data$percentages)
        } else {
          NA_real_
        }
      } else {
        colnames(df_freq) <- c("level", "outcome_level", "count")
        df_freq$variable <- x$meta$row_var_name
        df_freq$outcome_variable <- x$meta$col_var_name
        df_freq$percentage <- if (!is.null(x$data$percentages)) {
          as.vector(x$data$percentages)
        } else {
          NA_real_
        }
        if (!is.null(x$data$ratios)) {
          ratios <- x$data$ratios
          df_freq$estimate <- ratios$estimate[match(
            df_freq$level,
            ratios$level
          )]
          df_freq$lower_ci <- ratios$lower_ci[match(
            df_freq$level,
            ratios$level
          )]
          df_freq$upper_ci <- ratios$upper_ci[match(
            df_freq$level,
            ratios$level
          )]
          df_freq$p_value <- ratios$p_value[match(df_freq$level, ratios$level)]
        } else {
          df_freq$estimate <- NA_real_
          df_freq$lower_ci <- NA_real_
          df_freq$upper_ci <- NA_real_
          df_freq$p_value <- NA_real_
        }
      }
      return(df_freq)
    }
  } else {
    out_mat <- .build_display_matrix(x)
    df <- as.data.frame(out_mat, stringsAsFactors = FALSE)
    row_labels <- rownames(out_mat)
    df <- cbind(Row_Label = row_labels, df, stringsAsFactors = FALSE)
    colnames(df)[1] <- x$meta$row_label
    rownames(df) <- NULL
    attr(df, "stats") <- x$meta$stats
    return(df)
  }
}

#' Convert rbind_tb to Data Frame
#'
#' @param x An `rbind_tb` object.
#' @param row.names NULL or a character vector giving the row names for the data frame.
#' @param optional Logical. If TRUE, setting row names and converting column names is optional.
#' @param tidy Logical. If `TRUE`, returns a long-format tidy data frame.
#' @param ... Additional arguments.
#' @return A data.frame.
#' @export
as.data.frame.rbind_tb <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  tidy = FALSE,
  ...
) {
  if (isTRUE(tidy)) {
    parts <- list()
    for (i in seq_along(x$data)) {
      tb_i <- list(data = x$data[[i]], meta = x$meta$tables[[i]], call = NULL)
      class(tb_i) <- c("tb", "simtab")
      parts[[i]] <- as.data.frame(tb_i, tidy = TRUE)
    }

    all_cols <- unique(unlist(lapply(parts, colnames)))
    standardized_parts <- lapply(parts, function(df) {
      missing_cols <- setdiff(all_cols, colnames(df))
      for (co in missing_cols) {
        df[[co]] <- NA
      }
      df[, all_cols, drop = FALSE]
    })

    return(do.call(rbind, standardized_parts))
  } else {
    out_mat <- .build_display_matrix(x)
    df <- as.data.frame(out_mat, stringsAsFactors = FALSE)
    row_labels <- rownames(out_mat)
    df <- cbind(Row_Label = row_labels, df, stringsAsFactors = FALSE)
    colnames(df)[1] <- x$meta$row_label
    rownames(df) <- NULL
    return(df)
  }
}

#' SimtablR House Style Theme for Flextable
#'
#' @param ft A flextable object.
#' @return A flextable object.
#' @export
simtab_theme <- function(ft) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' needed.", call. = FALSE)
  }
  ft |>
    flextable::theme_booktabs() |>
    flextable::autofit() |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = -1, align = "center", part = "body") |>
    flextable::align(j = 1, align = "left", part = "body")
}

#' Convert tb Object to Flextable
#'
#' @param x A `tb` object.
#' @param ... Additional arguments passed to `flextable::flextable()`.
#' @return A `flextable` object.
#' @export
as_flextable.tb <- function(x, ...) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' needed.", call. = FALSE)
  }
  df <- as.data.frame(x, tidy = FALSE)
  ft <- flextable::flextable(df, ...)
  stats <- x$meta$stats
  if (!is.null(stats)) {
    p_str <- if (stats$p.value < 0.001) {
      "< 0.001"
    } else {
      sprintf("= %.3f", stats$p.value)
    }
    stat_text <- paste0(stats$method, ": p-value ", p_str)
    ft <- flextable::add_footer_lines(ft, values = stat_text)
    ft <- flextable::align(ft, part = "footer", align = "right")
  }
  simtab_theme(ft)
}

#' Convert rbind_tb Object to Flextable
#'
#' @param x An `rbind_tb` object.
#' @param ... Additional arguments passed to `flextable::flextable()`.
#' @return A `flextable` object.
#' @export
as_flextable.rbind_tb <- function(x, ...) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Package 'flextable' needed.", call. = FALSE)
  }
  df <- as.data.frame(x, tidy = FALSE)
  ft <- flextable::flextable(df, ...)
  simtab_theme(ft)
}
