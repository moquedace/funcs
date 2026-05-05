# funcs

Repository of R functions and computational routines developed to support reproducible workflows in soil science, digital soil mapping, environmental modelling, raster processing, machine learning, model evaluation, and spatial prediction.

## Author

Cássio Marques Moquedace

## Description

This repository contains R scripts and functions designed for geospatial data processing, environmental covariate preparation, raster manipulation, soil data analysis, predictive modelling, model evaluation, and spatial prediction.

The routines were developed in the context of scientific research involving soil science, digital soil mapping, soil organic carbon modelling, soil spectroscopy, environmental covariates, and large-scale spatial analysis.

## Main topics

- Soil science
- Digital soil mapping
- Environmental modelling
- Raster processing
- Spatial prediction
- Machine learning
- Model evaluation
- SoilGrids access
- Reproducible workflows in R

## Main groups of scripts

### Geospatial processing

Scripts for raster and vector processing, including reprojection, cropping, masking, resampling, tiling, focal operations, and raster writing.

Examples:

- `crop_mask_project.R`
- `change_resolution.R`
- `focal_resample.R`
- `tile_raster.R`
- `tile_raster_path.R`
- `writeRaster_factor.R`

### Soil and environmental covariates

Scripts for accessing, preparing, and processing soil and environmental predictors.

Examples:

- `soilgrids_raster.R`
- `download_febr_soildata.R`
- `process_landsat_indices.R`
- `calc_index_sentinel.R`
- `morphometry_saga.R`

### Predictive modelling

Scripts for regression, classification, repeated runs, cross-validation, feature selection, performance assessment, and model prediction.

Examples:

- `regression_modeling_single_run.R`
- `regression_modeling_repeated_runs.R`
- `regression_modeling_loocv.R`
- `classification_modeling_single_run.R`
- `classification_modeling_repeated_runs.R`
- `classification_modeling_loocv.R`
- `predict_qrf_raster.R`
- `pred_writer_qrf_raster.R`
- `pred_writer_raster_prob_raw.R`

### Model evaluation and auxiliary functions

Functions for performance metrics, outlier removal, partial dependence analysis, area of applicability, and class handling.

Examples:

- `pst_res_mqi.R`
- `pst_res_class.R`
- `pst_res_class_multiclass.R`
- `remove_outliers.R`
- `partial_dependence.R`
- `aoa_meyer.R`
- `rst_class_by_value.R`

### Cloud and file management

Auxiliary routines for cloud storage, file handling, and package management.

Examples:

- `gcs_download.R`
- `gcs_upload.R`
- `copy_with_robocopy.R`
- `install_load_pkg.R`

## Recommended citation

Moquedace, C. M. (2026). funcs: R functions and computational routines for geospatial processing, soil modelling, and spatial prediction. GitHub repository.

A DOI will be provided after archival release through Zenodo.

## License

A license should be added before formal citation or reuse. Suggested license: MIT License, unless institutional or project restrictions require another option.
