pst_res_mqi <- function(data, lev = NULL, model = NULL) {

  if (!requireNamespace("DescTools", quietly = TRUE)) {
    stop("Package 'DescTools' is required by pst_res_mqi.")
  }

  metric_names <- c("ccc", "r2", "nse", "mae", "rmse", "mqi")
  na_out <- stats::setNames(rep(NA_real_, length(metric_names)), metric_names)

  obs  <- suppressWarnings(as.numeric(data[, "obs"]))
  pred <- suppressWarnings(as.numeric(data[, "pred"]))

  ok <- is.finite(obs) & is.finite(pred)
  obs  <- obs[ok]
  pred <- pred[ok]

  if (length(obs) < 2 || stats::sd(obs) == 0) {
    return(na_out)
  }

  safe <- function(expr) {
    val <- tryCatch(expr, error = function(e) NA_real_)
    if (length(val) != 1 || !is.finite(val)) NA_real_ else val
  }

  ccc  <- safe(DescTools::CCC(obs, pred, conf.level = 0.95)$rho.c$est)
  r2   <- safe(stats::cor(pred, obs, use = "pairwise.complete.obs")^2)
  mae  <- safe(mean(abs(pred - obs)))
  rmse <- safe(sqrt(mean((pred - obs)^2)))

  denom_nse <- sum((obs - mean(obs))^2)
  nse <- if (is.finite(denom_nse) && denom_nse > 0) {
    safe(1 - sum((obs - pred)^2) / denom_nse)
  } else {
    NA_real_
  }

  mean_obs <- mean(obs)

  mqi <- if (is.finite(ccc) && is.finite(nse) && is.finite(mae) &&
             is.finite(mean_obs) && mean_obs != 0 && mae != 0) {
    safe((ccc * nse) / (mae / mean_obs))
  } else {
    NA_real_
  }

  out <- c(ccc, r2, nse, mae, rmse, mqi)
  names(out) <- metric_names
  out
}
