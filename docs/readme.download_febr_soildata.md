# FEBR SOILDATA Downloader

Utility functions to download soil datasets from the FEBR database using the `febr` R package.

This script creates a local folder structure with one folder per FEBR dataset and saves the selected FEBR tables as `.txt` files. It also returns a download manifest that can be used to check which files were saved, skipped, or failed.

## Main function

```r
download_febr_soildata()
```

## What it downloads

By default, the function downloads the main FEBR tables:

```r
c(
  "identificacao",
  "versionamento",
  "metadado",
  "observacao",
  "camada"
)
```

The output folder structure follows this pattern:

```text
publico/
  ctb0001/
    ctb0001-identificacao.txt
    ctb0001-versionamento.txt
    ctb0001-metadado.txt
    ctb0001-observacao.txt
    ctb0001-camada.txt
  ctb0002/
    ctb0002-identificacao.txt
    ctb0002-versionamento.txt
    ctb0002-metadado.txt
    ctb0002-observacao.txt
    ctb0002-camada.txt
```

## Basic use

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/main/download_febr_soildata.R"
)

download_manifest <- download_febr_soildata(
  output_dir = "./data/raw/soil/soildata/publico",
  dataset_ids = "all",
  metadata_dir = "./data/raw/metadata"
)
```

## Download selected datasets

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/main/download_febr_soildata.R"
)

download_manifest <- download_febr_soildata(
  output_dir = "./data/raw/soil/soildata/publico",
  dataset_ids = c("ctb0003", "ctb0004"),
  metadata_dir = "./data/raw/metadata"
)
```

## Download selected tables

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/main/download_febr_soildata.R"
)

download_manifest <- download_febr_soildata(
  output_dir = "./data/raw/soil/soildata/publico",
  dataset_ids = "all",
  table_names = c(
    "observacao",
    "camada"
  ),
  metadata_dir = "./data/raw/metadata"
)
```

## Arguments

### `output_dir`

Directory where the FEBR datasets will be saved.

Example:

```r
output_dir = "./data/raw/soil/soildata/publico"
```

### `dataset_ids`

FEBR dataset IDs to download.

Use `"all"` to download all datasets listed by `febr::readIndex()`.

Use a character vector to download selected datasets.

Example:

```r
dataset_ids = c("ctb0003", "ctb0004")
```

### `table_names`

FEBR tables to download.

Valid options are:

```r
c(
  "identificacao",
  "versionamento",
  "metadado",
  "observacao",
  "camada"
)
```

### `overwrite`

Logical value.

If `FALSE`, existing files are not downloaded again.

If `TRUE`, existing files are overwritten.

Default:

```r
overwrite = FALSE
```

### `metadata_dir`

Optional directory where the download manifest, download summary, missing file report, and FEBR index are saved.

Example:

```r
metadata_dir = "./data/raw/metadata"
```

### `verbose`

Logical value passed to `febr::readFEBR()`.

Default:

```r
verbose = TRUE
```

### `wait_seconds`

Waiting time between dataset downloads.

This can help avoid excessive requests when downloading many datasets.

Default:

```r
wait_seconds = 0.2
```

## Returned object

The function returns a manifest table with one row per expected file.

Main columns include:

```text
dataset_id
table_name
file_path
attempted_download
downloaded
saved
checked_after
status
```

Common values in `status` include:

```text
saved
already_exists
download_error
table_not_found_in_febr_result
write_error
```

## Recommended workflow

```r
source(
  "https://raw.githubusercontent.com/moquedace/funcs/main/download_febr_soildata.R"
)

download_manifest <- download_febr_soildata(
  output_dir = "./data/raw/soil/soildata/publico",
  dataset_ids = "all",
  overwrite = FALSE,
  metadata_dir = "./data/raw/metadata"
)

dplyr::count(
  download_manifest,
  status
)
```

## Notes

This script is a wrapper around the `febr` package. Compatibility depends on the current FEBR repository structure and on the current `febr` package API.

The function is intended to simplify reproducible downloads and standardize the local folder structure used in soil data workflows.
