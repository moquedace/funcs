suppressWarnings(suppressMessages({
  library(dplyr); library(purrr); library(tibble); library(caret); library(DescTools)
}))
source("../prediction/pst_res_mqi.R")
source("../prediction/pst_res_class_multiclass.R")
source("../modeling/helpers/caret_rfe_functions.R")

ok <- TRUE
chk <- function(cond, msg) {
  cat(sprintf("[%s] %s\n", ifelse(cond, "PASS", "FAIL"), msg))
  if (!cond) ok <<- FALSE
}

## ---- pst_res_mqi ----
obs  <- c(1,2,3,4,5,6,7,8,9,10)
pred <- c(1.1,2.0,2.9,4.0,5.0,5.8,7.0,8.2,9.0,9.7)
h <- pst_res_mqi(data.frame(obs = obs, pred = pred))
chk(all(is.finite(h)), "mqi healthy: todas metricas finitas")
chk(abs(h["mae"] - mean(abs(pred - obs))) < 1e-9, "mqi healthy: mae identico a formula original")

d1 <- pst_res_mqi(data.frame(obs = rep(5, 10), pred = pred))
chk(all(is.na(d1)), "mqi obs constante -> tudo NA (sem Inf/NaN)")

d2 <- pst_res_mqi(data.frame(obs = 5, pred = 5))
chk(all(is.na(d2)), "mqi n=1 -> tudo NA")
chk(identical(names(h), c("ccc","r2","nse","mae","rmse","mqi")), "mqi: nomes/ordem preservados")

## ---- pst_res_class_multiclass ----
lev <- c("a","b","c")
o <- factor(c("a","a","b","b","c","c","a","b","c","a"), levels = lev)
p <- factor(c("a","b","b","b","c","a","a","b","c","a"), levels = lev)
hc <- pst_res_class_multiclass(data.frame(obs = o, pred = p), lev = lev)
chk(length(hc) == 13 && !is.na(hc["kappa"]) && !is.na(hc["accuracy"]), "class healthy: kappa/accuracy finitos")
chk(hc["accuracy"] >= 0 && hc["accuracy"] <= 1, "class healthy: accuracy em [0,1]")

dc <- pst_res_class_multiclass(data.frame(obs = factor("a", levels = lev),
                                          pred = factor("a", levels = lev)), lev = lev)
chk(is.na(dc["kappa"]) && !any(is.nan(dc)),
    "class 1 obs -> kappa NA (metrica de otimizacao neutralizada), sem NaN/Inf")

# fold sem a classe 'c', mas lev informa as 3 classes -> nao deve dar erro
o2 <- factor(c("a","a","b","b"), levels = lev)
p2 <- factor(c("a","b","b","b"), levels = lev)
res2 <- tryCatch(pst_res_class_multiclass(data.frame(obs = o2, pred = p2), lev = lev),
                 error = function(e) e)
chk(!inherits(res2, "error") && length(res2) == 13, "class fold com classe ausente -> sem erro")

## ---- validate_resampling_index ----
nr <- 10
# holdout vazio em todos -> stop
r1 <- tryCatch(validate_resampling_index(list(F1 = 1:10), n_rows = nr), error = function(e) e)
chk(inherits(r1, "error"), "validate: holdout vazio -> stop")

# folds bons mantidos
r2 <- suppressWarnings(validate_resampling_index(list(F1 = 1:8, F2 = c(1:6,9,10)), n_rows = nr))
chk(length(r2) == 2, "validate: 2 folds validos mantidos")

# holdout=1 removido (min_holdout_n=2)
r3 <- suppressWarnings(validate_resampling_index(list(F1 = 1:9, F2 = 1:8), n_rows = nr))
chk(length(r3) == 1, "validate: fold com holdout=1 removido")

# cobertura de classe no treino
tgt <- factor(c(rep("a",5), rep("b",5)), levels = c("a","b"))
r4 <- suppressWarnings(validate_resampling_index(list(soA = 1:5, ambos = c(1:4,6:9)),
                                                 n_rows = nr, target = tgt,
                                                 class_levels = c("a","b"),
                                                 require_all_classes = TRUE))
chk(length(r4) == 1 && all(c("a","b") %in% as.character(tgt[r4[[1]]])),
    "validate: fold sem todas as classes no treino removido")

# index NULL -> NULL
chk(is.null(validate_resampling_index(NULL, n_rows = nr)), "validate: index NULL -> NULL")

## ---- make_repeated_stratified_group_kfold ----
set.seed(1)
dft <- data.frame(
  id = rep(paste0("g", 1:8), each = 3),
  cl = factor(rep(c("a","b","a","b","a","b","a","b"), each = 3), levels = c("a","b"))
)
gk <- tryCatch(make_repeated_stratified_group_kfold(dft, "id", "cl", k = 4, repeats = 2, seed = 1),
               error = function(e) e)
chk(!inherits(gk, "error") && length(gk) == 8, "stratified group kfold: 4 folds x 2 reps = 8 indices")
# nenhum grupo vaza entre treino e validacao
leak <- any(vapply(gk, function(idx) {
  tr <- unique(dft$id[idx]); va <- unique(dft$id[-idx]); length(intersect(tr, va)) > 0
}, logical(1)))
chk(!leak, "stratified group kfold: nenhum grupo vaza treino/validacao")

cat(sprintf("\n==== RESULTADO: %s ====\n", ifelse(ok, "TODOS OS TESTES PASSARAM", "HOUVE FALHAS")))
