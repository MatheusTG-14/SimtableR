library(testthat)
library(SimtablR)

epitabl <- epitabl

test_that("tb() constrói um objeto S3 bivariado estruturado em lista válido com os slots obrigatórios", {
  res <- tb(epitabl, smoking, disease)

  expect_s3_class(res, "tb")
  expect_s3_class(res, "simtab")
  expect_true(is.list(res))
  expect_named(res, c("data", "meta", "call"))

  expect_type(res$data, "list")
  expect_type(res$meta, "list")
  expect_true(inherits(res$call, "call"))

  expect_false(res$meta$is_continuous)
  expect_equal(res$meta$row_var_name, "smoking")
  expect_equal(res$meta$col_var_name, "disease")
})

test_that("tb() aplica regras estritas de validação de argumentos de entrada", {
  expect_error(tb(), "No data provided. Please supply a data.frame or vector.")
  expect_error(
    tb(matrix(1:4, 2)),
    "'data' must be a data.frame or atomic vector."
  )
  expect_error(
    tb(epitabl, smoking, disease, d = -1),
    "'d' must be a number between 0 and 10."
  )
  expect_error(
    tb(epitabl, smoking, disease, d = 11),
    "'d' must be a number between 0 and 10."
  )
  expect_error(
    tb(epitabl, smoking, disease, conf.level = 0),
    "'conf.level' must be between 0 and 1."
  )
  expect_error(
    tb(epitabl, smoking, disease, conf.level = 1),
    "'conf.level' must be between 0 and 1."
  )
  expect_error(
    tb(epitabl, smoking, disease, test = "invalid_test"),
    "Invalid test method. Use one of: chisq, fisher, mcnemar."
  )
  expect_error(tb(epitabl), "No variables specified.")
  expect_error(
    tb(epitabl, smoking, disease, sex),
    "Maximum of 2 variables allowed."
  )
})

test_that("tb() captura e trata transições de ambiguidade do sistema de flags de forma segura", {
  df_ambiguous <- data.frame(
    p = c("Event", "None", "Event"),
    col = c("Group1", "Group2", "Group1"),
    stringsAsFactors = FALSE
  )

  expect_warning(
    res_p <- tb(df_ambiguous, p, col),
    "Ambiguity detected: 'p' matches a formatting flag but is also a column name"
  )
  expect_equal(res_p$meta$row_var_name, "p")
  expect_equal(res_p$meta$col_var_name, "col")

  res_flag <- tb(df_ambiguous, col, flags = "p")
  expect_true(res_flag$meta$flags$percent)
  expect_equal(res_flag$meta$flags$by, "total")
})

test_that("tb() gerencia transformações de variáveis contínuas e análise de sintaxe simplificada", {
  res_shorthand <- tb(epitabl, age, disease, var.type = "continuous")
  expect_true(res_shorthand$meta$is_continuous)
  expect_named(res_shorthand$data$summary, c("No", "Yes", "Total"))

  expect_message(
    res_auto <- tb(epitabl, age, disease),
    "automatically treated as continuous because it is numeric"
  )
  expect_true(res_auto$meta$is_continuous)

  expect_error(
    tb(epitabl, smoking, disease, var.type = "continuous"),
    "Variable 'smoking' is not numeric."
  )
})

test_that("tb() dispara avisos educacionais contínuos sob solicitações de testes válidas", {
  expect_message(
    tb(epitabl, age, disease, test = TRUE),
    "Statistical test choice for continuous variables is driven by 'stat.cont'"
  )
})

test_that("tb() calcula com precisão frequências, totais e categorias NA através da flag m", {
  epitabl_na <- epitabl
  epitabl_na$sex[1:10] <- NA

  res_no_na <- tb(epitabl_na, sex, disease, m = FALSE)
  expect_false("<NA>" %in% rownames(res_no_na$data$frequencies))

  res_with_na <- tb(epitabl_na, sex, disease, m = TRUE)
  expect_true(
    "always" %in%
      res_with_na$meta$flags$missing ||
      res_with_na$meta$flags$missing
  )
  expect_true(
    any(is.na(rownames(res_with_na$data$frequencies))) ||
      any(rownames(res_with_na$data$frequencies) == "NA")
  )
})

test_that("tb() gera saídas robustas de data frame para RP e OR com valores-p alinhados", {
  res_pr <- tb(epitabl, smoking, disease, rp = TRUE, ref = "Never")
  ratios_pr <- res_pr$data$ratios

  expect_s3_class(ratios_pr, "data.frame")
  expect_named(
    ratios_pr,
    c(
      "variable",
      "level",
      "estimate",
      "lower_ci",
      "upper_ci",
      "p_value",
      "ref",
      "type"
    )
  )
  expect_true(ratios_pr$ref[1])
  expect_equal(ratios_pr$estimate[1], 1.0)
  expect_equal(ratios_pr$type[1], "PR")
  expect_true(all(ratios_pr$estimate[-1] > 0))
  expect_true(all(ratios_pr$p_value[-1] >= 0 & ratios_pr$p_value[-1] <= 1))

  res_or <- tb(epitabl, smoking, disease, or = TRUE, ref = "Never")
  ratios_or <- res_or$data$ratios
  expect_equal(ratios_or$type[1], "OR")
  expect_true(all(ratios_or$estimate[-1] > 0))
})

test_that("tb() desativa os cálculos de medidas de efeito e avisa quando a estratificação é usada", {
  expect_warning(
    res_strat <- tb(epitabl, smoking, disease, strat = region, rp = TRUE),
    "PR/OR calculations are disabled when stratification is used"
  )
  expect_null(res_strat$data$ratios)
})

test_that("rbind.tb() realiza verificações estritas de esquema de variáveis e empilha os conjuntos de dados", {
  t1 <- tb(epitabl, smoking, disease)
  t2 <- tb(epitabl, education, disease)
  t3 <- tb(epitabl, age, disease, var.type = "continuous")

  expect_no_error(stacked <- rbind(t1, t2, t3))
  expect_s3_class(stacked, "rbind_tb")
  expect_s3_class(stacked, "simtab")
  expect_named(stacked, c("data", "meta", "call"))
  expect_equal(length(stacked$data), 3)

  t_faulty <- tb(epitabl, smoking, sex)
  expect_error(rbind(t1, t_faulty), "Column variable mismatch")
  expect_error(rbind(), "No objects provided to rbind.")
  expect_error(rbind(t1, data.frame(a = 1)), "Argument 2 is not a 'tb' object.")
})

test_that("as.data.frame.tb() converte saídas de forma segura entre configurações de exibição e tidy", {
  res <- tb(epitabl, smoking, disease, rp = TRUE, ref = "Never")

  df_display <- as.data.frame(res, tidy = FALSE)
  expect_s3_class(df_display, "data.frame")
  expect_equal(colnames(df_display)[1], "Smoking status")
  expect_true("PR (95% CI)" %in% colnames(df_display))

  df_tidy <- as.data.frame(res, tidy = TRUE)
  expect_s3_class(df_tidy, "data.frame")
  expect_named(
    df_tidy,
    c(
      "level",
      "outcome_level",
      "count",
      "variable",
      "outcome_variable",
      "percentage",
      "estimate",
      "lower_ci",
      "upper_ci",
      "p_value"
    )
  )
})

test_that("as.data.frame.rbind_tb() mapeia matrizes agrupadas de forma segura entre layouts de exibição e tidy", {
  t1 <- tb(epitabl, smoking, disease)
  t2 <- tb(epitabl, age, disease, var.type = "continuous")
  stacked <- rbind(t1, t2)

  df_display <- as.data.frame(stacked, tidy = FALSE)
  expect_s3_class(df_display, "data.frame")
  expect_equal(colnames(df_display)[1], "Variable")
  expect_true("Smoking status" %in% df_display[[1]])

  df_tidy <- as.data.frame(stacked, tidy = TRUE)
  expect_s3_class(df_tidy, "data.frame")
  expect_true("variable" %in% colnames(df_tidy))
})
