# ==============================================================================
# AOA / DI - extensão compatível com repeated cross-validation
#
# Mantém o comportamento nativo do CAST para:
# - CV simples (method = "cv")
# - modelos sem CV compatível
# - useCV = FALSE
# - uso manual de CVtrain/CVtest quando model = NA
#
# Para caret::train com method = "repeatedcv" e useCV = TRUE:
# - calcula o DI de treinamento separadamente em cada repetição;
# - cada observação é comparada apenas com o conjunto de treino do respectivo fold;
# - combina os DI das repetições;
# - por padrão usa o threshold pooled das repetições;
# - preserva a estrutura de um objeto trainDI para uso em CAST::aoa().
#
# A previsão de DI/AOA/LPD continua sendo feita pelo CAST oficial.
# ============================================================================== 

.AOA_MEYER_VERSION <- "2.0.0"


aoa_meyer_version <- function() {
  .AOA_MEYER_VERSION
}


.aoa_meyer_check_packages <- function() {
  
  required_packages <- c(
    "CAST",
    "caret",
    "FNN",
    "MASS"
  )
  
  missing_packages <- required_packages[
    !vapply(
      required_packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]
  
  if (length(missing_packages) > 0L) {
    stop(
      "Pacotes necessários não instalados: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  }
}


.aoa_meyer_cast_threshold <- function(
    di
) {
  
  di <- di[
    is.finite(di)
  ]
  
  if (length(di) == 0L) {
    stop(
      "Nenhum DI finito disponível para calcular o threshold."
    )
  }
  
  threshold_quantile <- stats::quantile(
    di,
    probs = 0.75,
    na.rm = TRUE,
    names = FALSE
  )
  
  threshold_iqr <- 1.5 * stats::IQR(
    di,
    na.rm = TRUE
  )
  
  threshold <- threshold_quantile + threshold_iqr
  
  max_di <- max(
    di,
    na.rm = TRUE
  )
  
  if (threshold > max_di) {
    threshold <- max_di
  }
  
  as.numeric(
    threshold
  )
}


.aoa_meyer_repeat_ids <- function(
    resample_names,
    number,
    repeats
) {
  
  if (is.null(resample_names)) {
    
    if (
      length(number) != 1L ||
      length(repeats) != 1L ||
      !is.finite(number) ||
      !is.finite(repeats) ||
      number < 1L ||
      repeats < 1L
    ) {
      stop(
        "Número de folds/repetições inválido no objeto caret::train."
      )
    }
    
    expected_n <- as.integer(number) * as.integer(repeats)
    
    if (expected_n > 0L) {
      
      return(
        rep(
          paste0(
            "Repeat",
            seq_len(
              as.integer(repeats)
            )
          ),
          each = as.integer(number)
        )
      )
    }
    
    stop(
      "Não foi possível identificar as repetições da repeated CV."
    )
  }
  
  if (all(
    grepl(
      "Repeat[0-9]+",
      resample_names,
      ignore.case = TRUE
    )
  )) {
    
    return(
      sub(
        ".*(Repeat[0-9]+).*",
        "\\1",
        resample_names,
        ignore.case = TRUE
      )
    )
  }
  
  if (all(
    grepl(
      "Rep[0-9]+",
      resample_names,
      ignore.case = TRUE
    )
  )) {
    
    return(
      sub(
        ".*(Rep[0-9]+).*",
        "\\1",
        resample_names,
        ignore.case = TRUE
      )
    )
  }
  
  if (
    length(number) != 1L ||
    length(repeats) != 1L ||
    !is.finite(number) ||
    !is.finite(repeats) ||
    number < 1L ||
    repeats < 1L
  ) {
    stop(
      "Número de folds/repetições inválido no objeto caret::train."
    )
  }
  
  expected_n <- as.integer(number) * as.integer(repeats)
  
  if (
    length(resample_names) == expected_n &&
    expected_n > 0L
  ) {
    
    return(
      rep(
        paste0(
          "Repeat",
          seq_len(
            as.integer(repeats)
          )
        ),
        each = as.integer(number)
      )
    )
  }
  
  stop(
    "Não foi possível identificar as repetições a partir dos nomes dos resamples."
  )
}


.aoa_meyer_validate_repeated_folds <- function(
    index_train,
    index_test,
    repeat_id,
    n_training
) {
  
  if (
    length(index_train) != length(index_test) ||
    length(index_train) != length(repeat_id)
  ) {
    return(FALSE)
  }
  
  if (any(
    vapply(
      seq_along(index_train),
      function(i) {
        length(
          intersect(
            index_train[[i]],
            index_test[[i]]
          )
        ) > 0L
      },
      logical(1)
    )
  )) {
    return(FALSE)
  }
  
  repeat_names <- unique(
    repeat_id
  )
  
  for (repeat_name in repeat_names) {
    
    use_resamples <- which(
      repeat_id == repeat_name
    )
    
    test_frequency <- integer(
      n_training
    )
    
    for (resample_i in use_resamples) {
      
      test_index <- index_test[[resample_i]]
      
      if (
        length(test_index) == 0L ||
        any(test_index < 1L) ||
        any(test_index > n_training)
      ) {
        return(FALSE)
      }
      
      test_frequency[test_index] <-
        test_frequency[test_index] + 1L
    }
    
    if (any(
      test_frequency != 1L
    )) {
      return(FALSE)
    }
  }
  
  TRUE
}


.aoa_meyer_repeated_folds <- function(
    model,
    n_training,
    verbose = TRUE
) {
  
  index_train <- model$control$index
  
  if (
    is.null(index_train) ||
    length(index_train) == 0L
  ) {
    stop(
      "O modelo repeatedcv não possui model$control$index."
    )
  }
  
  number <- model$control$number
  repeats <- model$control$repeats
  
  repeat_id <- .aoa_meyer_repeat_ids(
    resample_names = names(index_train),
    number = number,
    repeats = repeats
  )
  
  index_test <- NULL
  test_source <- NULL
  
  candidate_index_test <- model$control$indexOut
  
  if (
    is.list(candidate_index_test) &&
    length(candidate_index_test) == length(index_train)
  ) {
    
    candidate_index_test <- unname(
      candidate_index_test
    )
    
    if (.aoa_meyer_validate_repeated_folds(
      index_train = index_train,
      index_test = candidate_index_test,
      repeat_id = repeat_id,
      n_training = n_training
    )) {
      
      index_test <- candidate_index_test
      test_source <- "model$control$indexOut_by_position"
    }
  }
  
  if (is.null(index_test)) {
    
    index_test <- lapply(
      index_train,
      function(train_index) {
        setdiff(
          seq_len(n_training),
          train_index
        )
      }
    )
    
    if (!.aoa_meyer_validate_repeated_folds(
      index_train = index_train,
      index_test = index_test,
      repeat_id = repeat_id,
      n_training = n_training
    )) {
      stop(
        "Não foi possível reconstruir uma repeated CV válida a partir de model$control$index."
      )
    }
    
    test_source <- "complement_of_model$control$index"
  }
  
  names(index_train) <- if (
    is.null(names(index_train))
  ) {
    paste0(
      "Resample",
      seq_along(index_train)
    )
  } else {
    names(index_train)
  }
  
  names(index_test) <- names(
    index_train
  )
  
  if (isTRUE(verbose)) {
    message(
      "repeatedcv detectada: ",
      length(unique(repeat_id)),
      " repetições x ",
      number,
      " folds"
    )
    message(
      "conjuntos de teste: ",
      test_source
    )
  }
  
  list(
    index_train = index_train,
    index_test = index_test,
    repeat_id = repeat_id,
    repeat_names = unique(repeat_id),
    number = number,
    repeats = repeats,
    test_source = test_source
  )
}


.aoa_meyer_scaled_train <- function(
    train_di
) {
  
  train_work <- train_di$train
  
  catvars <- train_di$catvars
  
  if (
    !inherits(catvars, "error") &&
    length(catvars) > 0L
  ) {
    
    for (catvar in catvars) {
      
      train_work[[catvar]] <- droplevels(
        train_work[[catvar]]
      )
      
      dvi_train <- predict(
        caret::dummyVars(
          paste0(
            "~",
            catvar
          ),
          data = train_work
        ),
        train_work
      )
      
      train_work <- data.frame(
        train_work,
        dvi_train,
        check.names = FALSE
      )
    }
    
    train_work <- train_work[
      ,
      !names(train_work) %in% catvars,
      drop = FALSE
    ]
  }
  
  scale_center <- train_di$scaleparam[["scaled:center"]]
  scale_scale <- train_di$scaleparam[["scaled:scale"]]
  
  if (
    is.null(scale_center) ||
    is.null(scale_scale)
  ) {
    stop(
      "Parâmetros de padronização não encontrados no trainDI."
    )
  }
  
  train_scaled <- scale(
    train_work,
    center = scale_center,
    scale = scale_scale
  )
  
  weight_names <- names(
    train_di$weight
  )
  
  weight_index <- match(
    colnames(train_scaled),
    weight_names
  )
  
  if (any(
    is.na(weight_index)
  )) {
    stop(
      "Os pesos do trainDI não correspondem às variáveis processadas."
    )
  }
  
  weight_values <- unlist(
    train_di$weight[
      1,
      weight_index,
      drop = FALSE
    ],
    use.names = FALSE
  )
  
  train_scaled <- sweep(
    train_scaled,
    MARGIN = 2,
    STATS = weight_values,
    FUN = "*"
  )
  
  as.matrix(
    train_scaled
  )
}


.aoa_meyer_fold_min_distance <- function(
    train_scaled,
    train_index,
    test_index,
    method = "L2",
    algorithm = "brute",
    S_inv = NULL
) {
  
  reference <- train_scaled[
    train_index,
    ,
    drop = FALSE
  ]
  
  query <- train_scaled[
    test_index,
    ,
    drop = FALSE
  ]
  
  if (method == "L2") {
    
    distance <- FNN::knnx.dist(
      data = reference,
      query = query,
      k = 1,
      algorithm = algorithm
    )
    
    return(
      as.numeric(
        distance[, 1]
      )
    )
  }
  
  if (method == "MD") {
    
    if (is.null(S_inv)) {
      stop(
        "S_inv não informado para distância Mahalanobis."
      )
    }
    
    return(
      vapply(
        seq_len(
          nrow(query)
        ),
        function(i) {
          
          delta <- sweep(
            reference,
            MARGIN = 2,
            STATS = query[i, ],
            FUN = "-"
          )
          
          distance_sq <- rowSums(
            (delta %*% S_inv) * delta
          )
          
          min(
            sqrt(
              pmax(
                distance_sq,
                0
              )
            ),
            na.rm = TRUE
          )
        },
        numeric(1)
      )
    )
  }
  
  stop(
    "Método não suportado: ",
    method
  )
}


trainDI_meyer <- function(
    model = NA,
    train = NULL,
    variables = "all",
    weight = NA,
    CVtest = NULL,
    CVtrain = NULL,
    method = "L2",
    useWeight = TRUE,
    useCV = TRUE,
    LPD = FALSE,
    verbose = TRUE,
    algorithm = "brute",
    repeated_cv_strategy = c(
      "pooled",
      "mean_sample"
    )
) {
  
  .aoa_meyer_check_packages()
  
  repeated_cv_strategy <- match.arg(
    repeated_cv_strategy
  )
  
  is_caret_model <- inherits(
    model,
    "train"
  )
  
  cv_method <- if (is_caret_model) {
    tolower(
      as.character(
        model$control$method
      )
    )
  } else {
    NA_character_
  }
  
  is_repeated_cv <- isTRUE(useCV) &&
    is_caret_model &&
    identical(
      cv_method,
      "repeatedcv"
    )
  
  # --------------------------------------------------------------------------
  # Qualquer situação que NÃO seja caret::train + repeatedcv + useCV = TRUE
  # continua usando exatamente o CAST oficial.
  # --------------------------------------------------------------------------
  
  if (!is_repeated_cv) {
    
    result <- CAST::trainDI(
      model = model,
      train = train,
      variables = variables,
      weight = weight,
      CVtest = CVtest,
      CVtrain = CVtrain,
      method = method,
      useWeight = useWeight,
      useCV = useCV,
      LPD = LPD,
      verbose = verbose,
      algorithm = algorithm
    )
    
    result$thres <- result$threshold
    result$aoa_meyer_version <- .AOA_MEYER_VERSION
    result$cv_method <- cv_method
    result$cv_strategy <- "CAST_native"
    
    return(
      result
    )
  }
  
  # --------------------------------------------------------------------------
  # repeatedcv
  # --------------------------------------------------------------------------
  
  if (isTRUE(verbose)) {
    message(
      "caret::train com repeatedcv detectado; usando extensão repeated-CV do AOA."
    )
  }
  
  # Estrutura base do CAST.
  # useCV = FALSE é proposital: escala, pesos e denominador do DI são obtidos
  # pelo CAST e o DI cross-validado é recalculado abaixo em cada repetição.
  # LPD = FALSE evita gerar trainLPD inconsistente com o novo threshold.
  
  base_train_di <- CAST::trainDI(
    model = model,
    train = train,
    variables = variables,
    weight = weight,
    CVtest = NULL,
    CVtrain = NULL,
    method = method,
    useWeight = useWeight,
    useCV = FALSE,
    LPD = FALSE,
    verbose = verbose,
    algorithm = algorithm
  )
  
  n_training <- nrow(
    base_train_di$train
  )
  
  folds <- .aoa_meyer_repeated_folds(
    model = model,
    n_training = n_training,
    verbose = verbose
  )
  
  train_scaled <- .aoa_meyer_scaled_train(
    base_train_di
  )
  
  S_inv <- NULL
  
  if (method == "MD") {
    
    if (ncol(train_scaled) == 1L) {
      S <- matrix(
        stats::var(train_scaled),
        1,
        1
      )
    } else {
      S <- stats::cov(
        train_scaled
      )
    }
    
    S_inv <- MASS::ginv(
      S
    )
  }
  
  di_by_repeat <- vector(
    mode = "list",
    length = length(
      folds$repeat_names
    )
  )
  
  names(di_by_repeat) <- folds$repeat_names
  
  threshold_by_repeat <- numeric(
    length(
      folds$repeat_names
    )
  )
  
  names(threshold_by_repeat) <- folds$repeat_names
  
  for (repeat_i in seq_along(
    folds$repeat_names
  )) {
    
    repeat_name <- folds$repeat_names[[repeat_i]]
    
    use_resamples <- which(
      folds$repeat_id == repeat_name
    )
    
    min_distance <- rep(
      NA_real_,
      n_training
    )
    
    for (resample_i in use_resamples) {
      
      train_index <- folds$index_train[[resample_i]]
      test_index <- folds$index_test[[resample_i]]
      
      min_distance[test_index] <- .aoa_meyer_fold_min_distance(
        train_scaled = train_scaled,
        train_index = train_index,
        test_index = test_index,
        method = method,
        algorithm = algorithm,
        S_inv = S_inv
      )
    }
    
    if (any(
      !is.finite(min_distance)
    )) {
      stop(
        repeat_name,
        ": existem observações sem distância cross-validada."
      )
    }
    
    repeat_di <- min_distance /
      base_train_di$trainDist_avrgmean
    
    di_by_repeat[[repeat_i]] <- repeat_di
    
    threshold_by_repeat[[repeat_i]] <- .aoa_meyer_cast_threshold(
      repeat_di
    )
    
    if (isTRUE(verbose)) {
      message(
        repeat_name,
        " | threshold: ",
        format(
          threshold_by_repeat[[repeat_i]],
          digits = 10
        )
      )
    }
  }
  
  di_matrix <- do.call(
    cbind,
    di_by_repeat
  )
  
  colnames(di_matrix) <- folds$repeat_names
  
  di_pooled <- unlist(
    di_by_repeat,
    use.names = FALSE
  )
  
  di_mean_sample <- rowMeans(
    di_matrix,
    na.rm = TRUE
  )
  
  threshold_pooled <- .aoa_meyer_cast_threshold(
    di_pooled
  )
  
  threshold_mean_sample <- .aoa_meyer_cast_threshold(
    di_mean_sample
  )
  
  selected_threshold <- switch(
    repeated_cv_strategy,
    pooled = threshold_pooled,
    mean_sample = threshold_mean_sample
  )
  
  threshold_no_cv <- base_train_di$threshold
  
  # Mantém trainDI com um valor por amostra para compatibilidade.
  # O threshold pode ser derivado do pooled, que contém todas as ocorrências
  # de teste das repetições.
  
  base_train_di$trainDI <- di_mean_sample
  base_train_di$threshold <- selected_threshold
  
  # O CAST atual usa trainDI$thres em aoa().
  # Criamos o campo explicitamente para não depender de partial matching.
  base_train_di$thres <- selected_threshold
  
  base_train_di$trainDI_by_repeat <- di_matrix
  base_train_di$trainDI_pooled <- di_pooled
  base_train_di$threshold_by_repeat <- threshold_by_repeat
  base_train_di$threshold_pooled <- threshold_pooled
  base_train_di$threshold_mean_sample <- threshold_mean_sample
  base_train_di$threshold_no_cv <- threshold_no_cv
  base_train_di$cv_method <- "repeatedcv"
  base_train_di$cv_strategy <- repeated_cv_strategy
  base_train_di$cv_number <- folds$number
  base_train_di$cv_repeats <- folds$repeats
  base_train_di$cv_test_source <- folds$test_source
  base_train_di$aoa_meyer_version <- .AOA_MEYER_VERSION
  base_train_di$requested_LPD <- isTRUE(LPD)
  
  if (isTRUE(LPD)) {
    base_train_di$trainLPD <- NULL
    base_train_di$avrgLPD <- NULL
    
    if (isTRUE(verbose)) {
      message(
        "repeatedcv: trainLPD do conjunto de treinamento não é calculado; ",
        "o LPD de newdata será calculado pelo CAST usando o threshold repeated-CV."
      )
    }
  }
  
  if (isTRUE(verbose)) {
    message(
      "threshold sem CV: ",
      format(
        threshold_no_cv,
        digits = 10
      )
    )
    message(
      "threshold pooled repeated-CV: ",
      format(
        threshold_pooled,
        digits = 10
      )
    )
    message(
      "threshold DI médio por amostra: ",
      format(
        threshold_mean_sample,
        digits = 10
      )
    )
    message(
      "threshold selecionado [",
      repeated_cv_strategy,
      "]: ",
      format(
        selected_threshold,
        digits = 10
      )
    )
  }
  
  class(base_train_di) <- "trainDI"
  
  base_train_di
}


aoa_meyer <- function(
    newdata,
    model = NA,
    trainDI = NA,
    train = NULL,
    weight = NA,
    variables = "all",
    CVtest = NULL,
    CVtrain = NULL,
    method = "L2",
    useWeight = TRUE,
    useCV = TRUE,
    LPD = FALSE,
    maxLPD = 1,
    indices = FALSE,
    verbose = TRUE,
    algorithm = "brute",
    parallel = FALSE,
    ncores = 2,
    repeated_cv_strategy = c(
      "pooled",
      "mean_sample"
    )
) {
  
  .aoa_meyer_check_packages()
  
  repeated_cv_strategy <- match.arg(
    repeated_cv_strategy
  )
  
  if (!inherits(
    trainDI,
    "trainDI"
  )) {
    
    trainDI <- trainDI_meyer(
      model = model,
      train = train,
      variables = variables,
      weight = weight,
      CVtest = CVtest,
      CVtrain = CVtrain,
      method = method,
      useWeight = useWeight,
      useCV = useCV,
      LPD = LPD,
      verbose = verbose,
      algorithm = algorithm,
      repeated_cv_strategy = repeated_cv_strategy
    )
  }
  
  if (
    is.null(trainDI$thres) &&
    !is.null(trainDI$threshold)
  ) {
    trainDI$thres <- trainDI$threshold
  }
  
  train_for_lpd <- train
  
  if (
    isTRUE(LPD) &&
    is.null(train_for_lpd)
  ) {
    train_for_lpd <- trainDI$train
  }
  
  result <- CAST::aoa(
    newdata = newdata,
    model = NA,
    trainDI = trainDI,
    train = train_for_lpd,
    weight = weight,
    variables = variables,
    CVtest = CVtest,
    CVtrain = CVtrain,
    method = method,
    useWeight = useWeight,
    useCV = useCV,
    LPD = LPD,
    maxLPD = maxLPD,
    indices = indices,
    parallel = parallel,
    cores = ncores,
    verbose = verbose,
    algorithm = algorithm
  )
  
  result$parameters <- trainDI
  result$aoa_meyer_version <- .AOA_MEYER_VERSION
  
  result
}
