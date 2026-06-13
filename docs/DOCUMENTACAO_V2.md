# Documentação dos Scripts de Modelagem — Versão v2

> **Repositório:** `moquedace/funcs`  
> **Patch local obrigatório:** `funcs_patches_v2.R`  
> **Versão:** v2 (compatível com R ≥ 4.3)

---

## Sumário

1. [Visão geral e fio condutor](#1-visão-geral-e-fio-condutor)
2. [Infraestrutura compartilhada — `funcs_patches_v2.R`](#2-infraestrutura-compartilhada--funcs_patches_v2r)
3. [Parâmetros comuns a todos os scripts](#3-parâmetros-comuns-a-todos-os-scripts)
4. [Scripts de regressão](#4-scripts-de-regressão)
   - 4.1 [regression_modeling_loocv_v2](#41-regression_modeling_loocv_v2r)
   - 4.2 [regression_modeling_leave_one_group_out_v2](#42-regression_modeling_leave_one_group_out_v2r)
   - 4.3 [regression_modeling_repeated_runs_v2](#43-regression_modeling_repeated_runs_v2r)
   - 4.4 [regression_modeling_single_run_v2](#44-regression_modeling_single_run_v2r)
5. [Scripts de classificação](#5-scripts-de-classificação)
   - 5.1 [classification_modeling_loocv_v2](#51-classification_modeling_loocv_v2r)
   - 5.2 [classification_modeling_leave_one_group_out_v2](#52-classification_modeling_leave_one_group_out_v2r)
   - 5.3 [classification_modeling_repeated_runs_v2](#53-classification_modeling_repeated_runs_v2r)
   - 5.4 [classification_modeling_single_run_v2](#54-classification_modeling_single_run_v2r)
6. [Estrutura de saída (diretórios e arquivos)](#6-estrutura-de-saída-diretórios-e-arquivos)
7. [Sistema de retomada (resume)](#7-sistema-de-retomada-resume)
8. [Proteção contra folds degenerados — duas camadas](#8-proteção-contra-folds-degenerados--duas-camadas)
9. [Métricas de desempenho](#9-métricas-de-desempenho)
10. [Como usar — exemplos práticos de adaptação](#10-como-usar--exemplos-práticos-de-adaptação)
11. [Teste automatizado — `_v2_selftest.R`](#11-teste-automatizado--_v2_selftestr)

---

## 1. Visão geral e fio condutor

Os 8 scripts implementam um pipeline completo de seleção de variáveis e validação de modelos preditivos usando o framework `caret`. Todos seguem a mesma estrutura geral:

```
Leitura dos dados
    ↓
Limpeza e filtragem (NaN, Inf, valores negativos opcionais)
    ↓
Filtro NZV (Near-Zero Variance) nos preditores
    ↓
Filtro de correlação de Spearman nos preditores
    ↓
RFE — Recursive Feature Elimination (seleção de variáveis)
    ↓
Treinamento e tuning do modelo final
    ↓
Avaliação de desempenho (treino + teste)
    ↓
Salvamento dos resultados (métricas, predições, importância de variáveis)
```

Os scripts se diferenciam pela **estratégia de validação externa**:

| Script | Estratégia | Agrupado? | Tipo |
|---|---|---|---|
| `*_loocv_v2` | Leave-One-Out CV (nested) | Opcional | Reg / Class |
| `*_leave_one_group_out_v2` | Leave-One-Group-Out (nested) | Sim | Reg / Class |
| `*_repeated_runs_v2` | Holdout repetido N vezes | Opcional | Reg / Class |
| `*_single_run_v2` | Uma única divisão treino/teste | Opcional | Reg / Class |

---

## 2. Infraestrutura compartilhada — `funcs_patches_v2.R`

Este arquivo deve ser colocado em um caminho acessível e o caminho configurado na variável `patches_path` em cada script. Ele é carregado **depois** dos helpers do GitHub (via `source(patches_path)`) para sobrescrever as versões remotas com versões endurecidas.

### Funções definidas no patch

---

#### `validate_resampling_index(index, n_rows, min_train_n, min_holdout_n, target, class_levels, require_all_classes)`

**Propósito:** Guarda estrutural (Camada A) — remove folds degenerados da lista de índices de treino antes de passá-los ao `caret`.

| Parâmetro | Tipo | Descrição |
|---|---|---|
| `index` | `list` ou `NULL` | Lista de índices de treino (como retornada por `groupKFold`). Se `NULL`, retorna `NULL` sem fazer nada (caminho LOOCV/CV aleatório — protegido pela Camada B). |
| `n_rows` | `integer` | Número total de linhas no dataset. |
| `min_train_n` | `integer` | Mínimo de observações de treino por fold. Padrão: `2`. |
| `min_holdout_n` | `integer` | Mínimo de observações de validação por fold. Padrão: `2`. Folds com `length(idx) == n_rows` (holdout vazio) são eliminados. |
| `target` | `factor` | Variável resposta (necessária quando `require_all_classes = TRUE`). |
| `class_levels` | `character` | Vetor de classes esperadas no treino. |
| `require_all_classes` | `logical` | Se `TRUE`, descarta folds onde alguma classe está ausente no conjunto de treino. Padrão: `FALSE`. |

Emite `warning` para cada fold removido e `stop` se todos os folds forem inválidos.

---

#### `pst_res_mqi(data, lev, model)`

**Propósito:** Função de sumário de regressão endurecida (Camada B) — substitui a versão do GitHub. Retorna `NA` (nunca `NaN` ou `Inf`) em folds degenerados.

**Métricas retornadas (nesta ordem):**

| Métrica | Descrição |
|---|---|
| `ccc` | Coeficiente de Correlação de Concordância (Lin) |
| `r2` | Coeficiente de determinação (cor de Pearson ao quadrado) |
| `nse` | Nash-Sutcliffe Efficiency |
| `mae` | Mean Absolute Error |
| `rmse` | Root Mean Square Error |
| `mqi` | Model Quality Index = `(CCC × NSE) / (MAE / mean(obs))` |

Retorna todos como `NA` quando: `n < 2`, `sd(obs) == 0`, ou qualquer cálculo interno gera `NaN`/`Inf`.

---

#### `pst_res_class_multiclass(data, lev, model)`

**Propósito:** Função de sumário de classificação endurecida (Camada B). Usa `lev` para alinhar classes mesmo quando um fold não contém todas elas.

**Métricas retornadas (13 no total):**

`kappa`, `accuracy`, `f1_score`, `sensitivity`, `specificity`, `pos_pred_value`, `neg_pred_value`, `precision`, `recall`, `prevalence`, `detection_rate`, `detection_prevalence`, `balanced_accuracy`

Qualquer métrica não-finita vira `NA`. Funciona tanto no caso binário quanto multiclasse.

---

#### `make_group_class_table(data, group_var, target_var)`

**Propósito:** Valida que cada grupo pertence a **uma única classe** (pré-requisito para split agrupado e estratificado). Retorna uma tabela com `grupo → classe`. Para na execução com `stop()` se algum grupo tiver mais de uma classe.

---

#### `make_repeated_stratified_group_kfold(data, group_var, target_var, k, repeats, seed)`

**Propósito:** Cria índices de treino para reamostragem agrupada (sem vazar grupos entre treino/validação) **e** estratificada por classe (cada fold mantém a proporção de classes). Garante que nenhum fold de treino fique sem alguma classe.

| Parâmetro | Descrição |
|---|---|
| `data` | Dataset completo |
| `group_var` | Nome da coluna de grupos |
| `target_var` | Nome da coluna de classes |
| `k` | Número de folds |
| `repeats` | Número de repetições |
| `seed` | Semente inicial (cada repetição usa `seed + rep_id - 1`) |

---

## 3. Parâmetros comuns a todos os scripts

Estes parâmetros aparecem na seção `# EDIT HERE` no topo de todos os scripts:

### Caminhos e diretórios

| Parâmetro | Descrição |
|---|---|
| `path_raiz` | Diretório raiz do projeto. `setwd()` é aplicado aqui. |
| `fold_results` | Nome da subpasta dentro de `path_raiz` onde os resultados serão salvos. |
| `patches_path` | Caminho absoluto para `funcs_patches_v2.R`. **Deve ser ajustado para sua máquina.** |

### Controles de I/O

| Parâmetro | Padrão | Descrição |
|---|---|---|
| `save_workspace` | `FALSE` | Se `TRUE`, salva o workspace com `save.image()` ao final de cada variável/rodada. Útil em sessões longas, mas lento em rede. |
| `pause_plots` | `FALSE` | Se `TRUE`, insere `Sys.sleep(2)` após cada gráfico gerado. |
| `row_limit` | `Inf` | Limita o dataset a N linhas para testes rápidos. Use `Inf` para rodar o dataset completo. |
| `run_rfe` | `TRUE` | Se `FALSE`, pula o RFE e usa todos os preditores disponíveis. |

### Filtros de preditores

| Parâmetro | Descrição |
|---|---|
| `cut_off_mc` | Limiar de correlação de Spearman para remoção de preditores redundantes (ex.: `0.95` remove pares com `|r| ≥ 0.95`). |
| `fixed_predictors` | Vetor de nomes de preditores protegidos de todos os filtros (NZV, correlação e RFE). Use `character(0)` se não houver. |
| `filter_target_non_negative` | `TRUE` remove observações com valor alvo `< 0`. Relevante para variáveis que só podem ser positivas (e.g., teor de carbono). |

### RFE — Recursive Feature Elimination

| Parâmetro | Descrição |
|---|---|
| `rfe_size` | Grade de tamanhos de subconjunto a testar no RFE (ex.: `seq(2, 41, 2)` testa 2, 4, 6... preditores). Use `2` para teste rápido. |
| `rfe_tn_length` | `tuneLength` para o modelo interno do RFE (modelo auxiliar usado apenas para rankear variáveis). |
| `tolerance` | Se `TRUE`, aceita um subconjunto menor que o ótimo se a perda de desempenho for ≤ `tol_per`%. |
| `tol_per` | Percentual de tolerância (em %). Só usado quando `tolerance = TRUE`. |
| `rfe_fold` | Número de folds da reamostragem interna do RFE *(scripts repeated/single)*. |
| `rfe_repeat` | Número de repetições da reamostragem interna do RFE *(scripts repeated/single)*. |

### Modelo final

| Parâmetro | Descrição |
|---|---|
| `model_tn_length` | `tuneLength` para o grid de hiperparâmetros do modelo final. |
| `model_fold` | Número de folds da reamostragem interna do modelo *(scripts repeated/single)*. |
| `model_repeat` | Número de repetições da reamostragem interna do modelo *(scripts repeated/single)*. |
| `metric_otm` | Métrica usada para selecionar o melhor modelo: `"mqi"` (regressão) ou `"kappa"` (classificação). |
| `maxim` | `TRUE` se a métrica deve ser **maximizada** (padrão para `mqi` e `kappa`). |

### Agrupamento

| Parâmetro | Descrição |
|---|---|
| `kfold` | `TRUE` ativa a validação agrupada usando `col_kfold`. `FALSE` usa LOOCV por amostra ou split aleatório. |
| `col_kfold` | Nome da coluna que identifica os grupos (ex.: `"id"`, `"municipio"`). Ignorada quando `kfold = FALSE`. |

### Paralelismo

| Parâmetro | Descrição |
|---|---|
| `use_parallel` | `TRUE` ativa o cluster paralelo com `doParallel`. |
| `cores` | Número de núcleos do cluster. Recomendado: `parallel::detectCores() - 1`. |
| `allow_parallel_rfe` | `TRUE` permite paralelismo dentro do RFE. |
| `allow_parallel_model` | `TRUE` permite paralelismo dentro do treino do modelo final. |
| `parallel_packages` | Vetor de pacotes que devem ser carregados em cada worker do cluster. |

### Classificação (exclusivo dos scripts de classificação)

| Parâmetro | Descrição |
|---|---|
| `class_probs` | `TRUE` calcula probabilidades de classe no modelo final (necessário para ROC, etc.). |
| `nruns` | Número de rodadas *(repeated_runs)*. |
| `perc_train` | Proporção do dataset usada para treino no split aleatório *(repeated_runs e single_run)*. |

---

## 4. Scripts de regressão

### 4.1 `regression_modeling_loocv_v2.R`

**Estratégia:** Nested Leave-One-Out Cross-Validation (LOOCV) por amostra.

**Como funciona:**
- O loop externo itera sobre cada observação `n` do dataset.
- Na iteração `n`: a amostra `n` é o conjunto de teste; as demais são treino.
- Dentro do treino: aplica NZV → correlação → RFE (com reamostragem interna) → treina o modelo final.
- O modelo prediz a amostra `n` (holdout de 1 observação).
- Ao final, as `N` predições são reunidas e as métricas globais calculadas.

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `base_seed` | Semente base; cada iteração `n` usa `base_seed + n - 1`. |
| `kfold` / `col_kfold` | Quando `kfold = TRUE`, a coluna de grupos é excluída dos preditores (estrutura LOOCV por amostra não muda, mas o grupo não vaza como preditor). |
| `rfe_size` | Grade de tamanhos para o RFE (ex.: `seq(2, 41, 2)`). |
| `rfe_tn_length` | `tuneLength` do modelo interno do RFE. |
| `model_tn_length` | `tuneLength` do modelo final. |

**Modelos suportados:** qualquer string ou objeto de lista compatível com `caret::train()`. Padrão: `c("qrf")`.

**Nota E3:** O tuning interno do RFE usa reamostragem não-agrupada (`index = NULL`). Apenas o loop externo (LOOCV) é agrupado quando `kfold = TRUE`.

**Sistema de retomada:** o arquivo de progresso `progress_<modelo>_<variável>.rds` armazena `pred_obs_test`, `dfperf_train`, `dfperf_test`, `lrferes`, `lpredimp`. Ao reiniciar, o script detecta o arquivo, valida a compatibilidade (dimensões e nomes de colunas) e continua do fold seguinte. Incompatibilidade → `stop()`.

---

### 4.2 `regression_modeling_leave_one_group_out_v2.R`

**Estratégia:** Nested Leave-One-Group-Out (LOGO). O loop externo deixa todos os dados de um grupo fora de uma vez.

**Como funciona:**
- `make_leave_one_group_out_index()` (do GitHub) cria os índices de treino: um por grupo.
- `validate_resampling_index()` (Camada A) filtra folds estruturalmente inválidos.
- Para cada grupo deixado fora: NZV → correlação → RFE → modelo final → avalia no grupo de teste.

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `col_kfold` | **Obrigatório.** Coluna que define os grupos. Cada valor único vira um fold externo. |
| `kfold` | Deve ser `TRUE` para ativar a validação agrupada. Se `FALSE`, o script cai no LOOCV por amostra. |

**Diferença da v1:** agrega resultados de RFE e importância de variáveis com `map2_dfr` + `which(!is.null)` (evita falha quando algum fold retorna `NULL`). `varImp(scale = FALSE)` é usado para preservar a escala original da importância.

---

### 4.3 `regression_modeling_repeated_runs_v2.R`

**Estratégia:** Holdout repetido — o dataset é dividido aleatoriamente (ou por grupo) em treino/teste `nruns` vezes.

**Como funciona:**
- Em cada rodada `r` (de 1 a `nruns`): split → NZV → correlação → RFE → modelo → avalia no teste.
- Se `kfold = TRUE`: usa `groupKFold` / `make_repeated_stratified_group_kfold` para splits agrupados.
- Ao final, agrega as métricas de todas as rodadas (média ± DP).

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `nruns` | Número de rodadas de holdout. Padrão: `100`. Use `3` para teste rápido. |
| `nseed` | Semente base; cada rodada usa `nseed + r - 1`. |
| `perc_train` | Fração do dataset para treino (ex.: `0.8` = 80% treino, 20% teste). |
| `rfe_fold` | Folds da reamostragem interna do RFE. |
| `rfe_repeat` | Repetições da reamostragem interna do RFE. |
| `model_fold` | Folds da reamostragem interna do modelo. |
| `model_repeat` | Repetições da reamostragem interna do modelo. |
| `kfold` / `col_kfold` | Quando `TRUE`, o split treino/teste respeita os grupos de `col_kfold`. |

**Nota v2:** se `kfold = FALSE` e `nrow(train) < rfe_fold`, o script ajusta automaticamente: `rfe_fold_use <- min(rfe_fold, nrow(train))` e `model_fold_use <- min(model_fold, nrow(train))`.

**Sistema de retomada:** o arquivo de progresso armazena a lista de resultados por rodada e a última rodada concluída. Ao reiniciar, pula rodadas já processadas.

---

### 4.4 `regression_modeling_single_run_v2.R`

**Estratégia:** Uma única divisão treino/teste para múltiplas variáveis-alvo.

**Como funciona:**
- O loop externo itera sobre variáveis-alvo (`varsy`), definidas por nome explícito (não por índice frágil).
- Para cada variável: split único (aleatório ou agrupado) → NZV → correlação → RFE → modelo → avalia.

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `nseed` | Semente para o split. |
| `perc_train` | Fração de treino. |
| `varsy` | Vetor nomeado das variáveis-alvo. **Deve ser definido por nome** (não por `names(dfbase)[18:1]`). Emite `warning` se `NULL` e para com `stop()`. |
| `rfe_fold` / `rfe_repeat` | Reamostragem interna do RFE. |
| `model_fold` / `model_repeat` | Reamostragem interna do modelo. |

**Melhoria v2 (B1):** a indexação frágil `names(dfbase)[18:1]` foi substituída por um vetor nomeado explícito `target_vars`, eliminando erros silenciosos quando o dataset muda de estrutura.

---

## 5. Scripts de classificação

Os scripts de classificação seguem a mesma estrutura dos de regressão, com as seguintes diferenças principais:

- Carregam `gbm_custom.R` e `pst_res_class_multiclass.R` do GitHub (em vez de `pst_res_mqi.R`).
- A métrica padrão de otimização é `"kappa"` (em vez de `"mqi"`).
- A variável resposta é um `factor` (não numérica).
- O filtro `filter_target_non_negative` não se aplica.
- `validate_resampling_index()` é chamado com `require_all_classes = TRUE` nos scripts agrupados.
- `make_repeated_stratified_group_kfold()` substitui `make_repeated_group_folds()` quando `kfold = TRUE` (garante cobertura de todas as classes em cada fold de treino).

**Modelos suportados:** `RF` (randomForest), `kNN` (kknn), `SVM Radial` (kernlab), `C5.0`, `GBM` (custom — usa `relative.influence` para importância de variáveis).

### 5.1 `classification_modeling_loocv_v2.R`

**Estratégia:** Nested LOOCV por amostra para classificação.

**Parâmetros específicos além dos comuns:**

| Parâmetro | Descrição |
|---|---|
| `nseed` | Semente base. |
| `class_probs` | `TRUE` habilita cálculo de probabilidades de classe. |
| `rfe_size` | Grade de tamanhos para RFE (ex.: `seq(2, 72, 2)`). |

**Correção v2 (C1):** removida a chamada `droplevels()` no subset de treino (`train <- dyx_sel[-n, ]` em vez de `droplevels(...)`), que causava discrepâncias de níveis de fator entre treino e teste em alguns folds.

**Correção v2 (C4):** agregação dos resultados de RFE usa `lapply` + nomeação explícita de colunas (em vez de `sapply` que silenciosamente transposava a matrix de resultados).

---

### 5.2 `classification_modeling_leave_one_group_out_v2.R`

**Estratégia:** Nested Leave-One-Group-Out para classificação.

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `col_kfold` | Coluna de grupos (obrigatório, `kfold` deve ser `TRUE`). |
| `class_probs` | `TRUE` habilita probabilidades de classe. |

**Proteção adicional v2 (A4):** `validate_resampling_index()` é chamado com `require_all_classes = TRUE` após construir os índices LOGO — folds onde alguma classe está ausente no treino são descartados antes de entrar no loop.

---

### 5.3 `classification_modeling_repeated_runs_v2.R`

**Estratégia:** Holdout repetido para classificação.

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `nruns` | Número de rodadas. |
| `nseed` | Semente base. |
| `perc_train` | Fração de treino. |
| `rfe_fold` / `rfe_repeat` | Reamostragem interna do RFE. |
| `model_fold` / `model_repeat` | Reamostragem interna do modelo. |
| `kfold` / `col_kfold` | Quando `TRUE`, usa `make_repeated_stratified_group_kfold()` para splits agrupados e estratificados por classe. |
| `class_probs` | Probabilidades de classe. |

**Correção v2 (C5):** quando `kfold = FALSE`, o número de folds é limitado ao `nrow(train)` disponível, evitando erro do caret quando o dataset é pequeno.

---

### 5.4 `classification_modeling_single_run_v2.R`

**Estratégia:** Uma única divisão treino/teste para múltiplas variáveis-alvo de classificação.

**Parâmetros específicos:**

| Parâmetro | Descrição |
|---|---|
| `nseed` | Semente para o split. |
| `perc_train` | Fração de treino. |
| `rfe_fold` / `rfe_repeat` | Reamostragem interna do RFE. |
| `model_fold` / `model_repeat` | Reamostragem interna do modelo. |
| `kfold` / `col_kfold` | Quando `TRUE`, usa `make_repeated_stratified_group_kfold()` para reamostragem interna agrupada e estratificada. |
| `class_probs` | Probabilidades de classe. |

**Melhoria v2 (C3):** quando `kfold = TRUE`, a construção dos folds internos usa `make_repeated_stratified_group_kfold()` (do patch) em vez de `make_repeated_group_folds()` (GitHub), garantindo que cada fold de treino contenha todas as classes.

---

## 6. Estrutura de saída (diretórios e arquivos)

Todos os scripts criam a seguinte hierarquia dentro de `path_raiz/fold_results/`:

```
fold_results/
└── <modelo>/
    └── <variável_alvo>/
        ├── select/
        │   ├── cor/          # Correlações entre preditores removidos/mantidos
        │   ├── nzv/          # Diagnóstico de variância próxima de zero
        │   └── rfe/
        │       ├── metric/   # Curva de desempenho do RFE por tamanho de subconjunto
        │       └── select/   # Preditores selecionados pelo RFE
        ├── performance/
        │   ├── csv/          # Métricas de treino e teste (por fold/rodada e agrupadas)
        │   ├── imp_pred/     # Importância de variáveis por fold/rodada
        │   └── pred_obs/     # Predições vs. observações (por fold/rodada)
        ├── img/              # Gráficos gerados (scatter pred×obs, curva RFE, etc.)
        └── model/            # Objeto do modelo final (opcional, .rds)
```

---

## 7. Sistema de retomada (resume)

Scripts com loop longo (LOOCV, repeated_runs) salvam um arquivo de progresso em:

```
<path_results>/progress_<modelo>_<variável>.rds
```

O arquivo contém:
- Os resultados acumulados até o ponto de interrupção.
- A última iteração concluída.
- Um **fingerprint de compatibilidade**: dimensões do dataset, nomes das colunas e parâmetros-chave.

**Ao reiniciar o script:**
1. Se o arquivo de progresso existe, carrega os resultados acumulados.
2. Verifica a compatibilidade do fingerprint com o estado atual. **Qualquer divergência → `stop()` com mensagem descritiva** (evita silenciosamente acumular resultados inconsistentes).
3. Continua a partir da próxima iteração não processada.

---

## 8. Proteção contra folds degenerados — duas camadas

Este sistema foi criado para evitar perdas de dias de computação por resultados corrompidos silenciosamente.

### Camada A — Estrutural (`validate_resampling_index`)

Atua **antes** de passar os índices ao `caret`. Remove folds onde:
- O conjunto de treino tem menos de `min_train_n` observações.
- O conjunto de validação tem menos de `min_holdout_n` observações (inclui holdout vazio: `length(idx) == n_rows`).
- Os índices contêm `NA` ou estão fora do intervalo `1:n_rows`.
- *(Classificação)* Alguma classe está ausente no treino, quando `require_all_classes = TRUE`.

Retorna `NULL` sem modificação quando `index = NULL` (LOOCV e CV aleatório — cobertos pela Camada B).

### Camada B — Métricas endurecidas (`pst_res_mqi` / `pst_res_class_multiclass`)

Atua **dentro** do caret, a cada fold de reamostragem. Converte qualquer resultado não-finito (`NaN`, `Inf`, erro de cálculo) em `NA` limpo, em vez de deixar o `NaN` contaminar a média dos resamples.

**Causa raiz do problema original:** quando todas as observações de `obs` em um fold eram idênticas (variância zero), o denominador do NSE era 0 → `NaN`. O caret calculava `mean(c(válido, NaN))` sem `na.rm = TRUE` → run inteiro marcado como `NaN`. Com a Camada B, o fold retorna `NA` e o caret o ignora corretamente na média.

---

## 9. Métricas de desempenho

### Regressão

| Métrica | Fórmula | Interpretação |
|---|---|---|
| CCC | Coeficiente de Correlação de Concordância de Lin | Combina precisão e acurácia. 1 = perfeito. |
| R² | `cor(pred, obs)²` | Proporção da variância explicada. |
| NSE | `1 - Σ(obs-pred)² / Σ(obs-mean(obs))²` | 1 = perfeito; 0 = equivale ao modelo nulo (média). |
| MAE | `mean(|pred - obs|)` | Erro médio absoluto (mesma unidade da variável). |
| RMSE | `sqrt(mean((pred-obs)²))` | Erro quadrático médio (penaliza outliers). |
| **MQI** | `(CCC × NSE) / (MAE / mean(obs))` | Índice composto. **Métrica de otimização padrão.** |

### Classificação

| Métrica | Descrição |
|---|---|
| **Kappa** | Concordância além do acaso. **Métrica de otimização padrão.** |
| Accuracy | Proporção de classificações corretas. |
| F1-score | Média harmônica de precisão e recall (macro-averaged). |
| Sensitivity / Recall | Proporção de verdadeiros positivos por classe. |
| Specificity | Proporção de verdadeiros negativos por classe. |
| Balanced Accuracy | Média de sensitivity e specificity por classe. |
| + 7 métricas adicionais | `pos_pred_value`, `neg_pred_value`, `precision`, `prevalence`, `detection_rate`, `detection_prevalence` (macro-averaged). |

---

---

## 10. Como usar — exemplos práticos de adaptação

Esta seção mostra o mínimo que você precisa editar para rodar cada família de script em um novo projeto. Todos os blocos abaixo ficam na seção `# EDIT HERE` no topo do script.

### 10.1 Configuração mínima obrigatória (todos os scripts)

```r
# 1. Diretório raiz do seu projeto
path_raiz <- "D:/meu_projeto/dados"

# 2. Subpasta onde os resultados serão gravados
fold_results <- "results_v1"

# 3. Caminho para o patch local (ajuste para sua máquina)
patches_path <- "C:/Users/SeuUsuario/funcs-main/funcs_patches_v2.R"

# 4. Modelos a rodar (um por vez no loop)
models <- c("qrf", "rf")          # regressão
# models <- c("rf", "C5.0", "svmRadial")  # classificação
```

---

### 10.2 Regressão com LOOCV — dados sem estrutura espacial

Cenário: dataset de perfis de solo sem repetição de sítio. Quer validação máxima, sem agrupamento.

```r
# Sem agrupamento: kfold = FALSE, col_kfold ignorado
kfold     <- FALSE
col_kfold <- character(0)

# RFE: testa subconjuntos de 2 a 20 preditores, passo 2
rfe_size <- seq(2, 20, 2)

# Métrica de otimização
metric_otm <- "mqi"
maxim      <- TRUE

# Núcleos paralelos
cores <- parallel::detectCores() - 1

# Variáveis-alvo (defina por nome, nunca por índice)
varsy <- c("carbono", "argila", "ph")
```

---

### 10.3 Regressão com Leave-One-Group-Out — dados com estrutura espacial

Cenário: amostras agrupadas por município ou bacia hidrográfica. Quer que o modelo nunca veja dados do grupo de teste durante o treino.

```r
# Agrupamento: kfold = TRUE, col_kfold = coluna do grupo
kfold     <- TRUE
col_kfold <- "municipio"   # nome exato da coluna no seu dataset

# Protetor contra classes ausentes no treino (classificação)
# Para regressão, require_all_classes = FALSE (padrão)
```

No script `*_leave_one_group_out_v2.R` isso é suficiente — o loop externo já itera sobre os grupos automaticamente.

---

### 10.4 Regressão com holdout repetido — avaliação estatística de estabilidade

Cenário: quer distribuição de métricas ao longo de 100 divisões aleatórias independentes.

```r
nruns     <- 100       # número de rodadas
nseed     <- 42        # semente base (cada rodada usa nseed + r - 1)
perc_train <- 0.8      # 80% treino, 20% teste

# Reamostragem interna
rfe_fold   <- 5
rfe_repeat <- 1
model_fold   <- 5
model_repeat <- 5

# Para teste rápido antes de rodar tudo:
row_limit <- 200    # limita a 200 linhas
nruns     <- 3
rfe_size  <- 2
```

---

### 10.5 Classificação com grupos e estratificação

Cenário: classificação de classes de solo com amostras agrupadas por perfil. Cada perfil pertence a **uma única classe**. Quer que cada fold de treino contenha todas as classes.

```r
kfold     <- TRUE
col_kfold <- "perfil_id"

# A função make_repeated_stratified_group_kfold (do patch)
# garante automaticamente que todas as classes estejam no treino.
# Se um grupo tiver mais de uma classe → stop() imediato.

metric_otm  <- "kappa"
class_probs <- FALSE

rfe_fold   <- 5
rfe_repeat <- 1
model_fold   <- 5
model_repeat <- 3
```

---

### 10.6 Adicionar ou remover preditores fixos

```r
# Preditores que nunca são removidos pelo NZV, correlação ou RFE:
fixed_predictors <- c("latitude", "longitude", "elevacao")

# Sem preditores fixos:
fixed_predictors <- character(0)
```

---

### 10.7 Modo de teste rápido (verificar se o script roda sem erros)

Aplique estas substituições temporárias antes de rodar o script completo:

```r
row_limit    <- 50     # usa apenas 50 linhas
rfe_size     <- 2      # testa apenas subconjunto de 2 preditores
nruns        <- 3      # apenas 3 rodadas (repeated_runs)
cores        <- 2      # menos núcleos
model_tn_length <- 1   # grid mínimo de hiperparâmetros
```

---

### 10.8 Trocar o modelo

```r
# Regressão — opções comuns no caret:
models <- c("qrf")        # Quantile Regression Forest (prediz mediana)
models <- c("rf")         # Random Forest clássico
models <- c("xgbTree")    # XGBoost

# Classificação:
models <- c("rf", "C5.0", "svmRadial", "kknn", "gbm_custom")
# gbm_custom é o objeto carregado pelo gbm_custom.R do GitHub
```

---

### 10.9 Variáveis com dummy encoding

Quando uma variável categórica foi previamente expandida em colunas binárias (0/1), informe os nomes das colunas originais para que o mapa de importância de variáveis seja agregado corretamente:

```r
dummy_vars_raw <- c(
  "geologia_ignea",
  "geologia_sedimentar",
  "uso_solo_pastagem",
  "uso_solo_floresta"
)
# O script agrupa as dummies de volta ao nome original
# na tabela de importância de variáveis.
```

---

## 11. Teste automatizado — `_v2_selftest.R`

O arquivo `_v2_selftest.R` é um script de sanidade que verifica se as funções do patch (`funcs_patches_v2.R`) estão funcionando corretamente **antes** de iniciar uma rodada longa de modelagem.

### Como executar

```r
# No RStudio: abra _v2_selftest.R e pressione Ctrl+Shift+Enter
# Ou via terminal (ajuste o caminho):
Rscript --vanilla "C:/Users/SeuUsuario/funcs-main/_v2_selftest.R"
```

### O que é testado (16 testes)

#### `pst_res_mqi` — 5 testes

| # | Cenário | Verificação |
|---|---|---|
| 1 | Dados saudáveis (obs/pred normais) | Todas as 6 métricas são finitas |
| 2 | Dados saudáveis | `mae` é idêntico à fórmula `mean(abs(pred - obs))` |
| 3 | `obs` constante (variância zero) | Todas as métricas retornam `NA` (sem `Inf`/`NaN`) |
| 4 | `n = 1` observação | Todas as métricas retornam `NA` |
| 5 | Dados saudáveis | Nomes e ordem das métricas preservados: `ccc, r2, nse, mae, rmse, mqi` |

#### `pst_res_class_multiclass` — 4 testes

| # | Cenário | Verificação |
|---|---|---|
| 6 | 3 classes, dados saudáveis | `kappa` e `accuracy` são finitos; `length == 13` |
| 7 | Dados saudáveis | `accuracy` está em `[0, 1]` |
| 8 | `n = 1` observação | `kappa` é `NA` (métrica de otimização neutralizada), sem `NaN`/`Inf` |
| 9 | Fold sem a classe `"c"`, mas `lev` informa as 3 classes | Executa sem erro; retorna 13 métricas |

#### `validate_resampling_index` — 5 testes

| # | Cenário | Verificação |
|---|---|---|
| 10 | Todos os folds têm holdout vazio | Dispara `stop()` (erro esperado) |
| 11 | 2 folds válidos | Ambos são mantidos |
| 12 | 1 fold com holdout de tamanho 1 | Fold é removido; 1 fold mantido |
| 13 | Fold sem todas as classes no treino (`require_all_classes = TRUE`) | Fold inválido é removido |
| 14 | `index = NULL` | Retorna `NULL` sem erro |

#### `make_repeated_stratified_group_kfold` — 2 testes

| # | Cenário | Verificação |
|---|---|---|
| 15 | 8 grupos × 2 classes, `k = 4`, `repeats = 2` | Retorna lista com `4 × 2 = 8` índices |
| 16 | Mesma configuração | Nenhum grupo vaza entre treino e validação |

### Saída esperada (todos os testes passando)

```
[PASS] mqi healthy: todas metricas finitas
[PASS] mqi healthy: mae identico a formula original
[PASS] mqi obs constante -> tudo NA (sem Inf/NaN)
[PASS] mqi n=1 -> tudo NA
[PASS] mqi: nomes/ordem preservados
[PASS] class healthy: kappa/accuracy finitos
[PASS] class healthy: accuracy em [0,1]
[PASS] class 1 obs -> kappa NA (metrica de otimizacao neutralizada), sem NaN/Inf
[PASS] class fold com classe ausente -> sem erro
[PASS] validate: holdout vazio -> stop
[PASS] validate: 2 folds validos mantidos
[PASS] validate: fold com holdout=1 removido
[PASS] validate: fold sem todas as classes no treino removido
[PASS] validate: index NULL -> NULL
[PASS] stratified group kfold: 4 folds x 2 reps = 8 indices
[PASS] stratified group kfold: nenhum grupo vaza treino/validacao

==== RESULTADO: TODOS OS TESTES PASSARAM ====
```

Se qualquer linha mostrar `[FAIL]`, o patch não está funcionando corretamente e os scripts de modelagem **não devem ser executados** até o problema ser corrigido.

---

*Gerado com base nos scripts v2 — `funcs_patches_v2.R` + 8 scripts de modelagem + `_v2_selftest.R`.*
