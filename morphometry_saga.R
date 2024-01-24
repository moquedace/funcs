#### Morphometric variables SAGA-GIS by RSAGA
# for more information: https://cran.r-project.org/web/packages/RSAGA/vignettes/RSAGA.html


##### TIPS

# It's possible to obtain each morphometric variable by time or use the function bellow to obtain all in once. 

# For obtain one by step it's necessary to define "dem" and "outdir" before the "morfometricas_saga" function and apply each Saga function separated.

# outdir = output directory (i.e. "./Arquivos_Arthur/GTOPO30/morfometricas/")

# all ".sdat" are converted to ".tif" at final step

# Script made by Elpídio Inácio Fernandes-Filho, Adriano Luis Schunemann and Arthur Telles Calegario.
# Anydoubt tcalegario@gmail.com


# Adaptation Cássio Moquedace
# Install SAGA-GIS 6.2 (https://sourceforge.net/projects/saga-gis/files/SAGA%20-%206/SAGA%20-%206.2.0/saga-6.2.0_x64_setup.exe/download)

# Function morphometric -------------------------------------------------------
morphometry_saga <- function(dem,
                             outdir,
                             align_rasters = FALSE,
                             sol_rad = TRUE,
                             start_date = "01-01-2018",
                             end_date = "12-31-2018",
                             verbose = TRUE,
                             parallel = TRUE,
                             cores = 1) { # R function
  
  # Checking packages ---------------------------------------------------------
  # Install packages not yet installed --------------------------------------
  pkg <- c("RSAGA", "terra", "rgdal", "dplyr", "stringr")
  
  
  installed_packages <- pkg %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("installing package", pkg[!installed_packages]))
    install.packages(pkg[!installed_packages])
    print("installed packages")
  }
  
  
  # Packages loading --------------------------------------------------------
  invisible(lapply(pkg, library, character.only = TRUE))
  
  
  # Parameterizing rsaga ----------------------------------------------------
  env <- rsaga.env()
  
  
  # Parallel use ------------------------------------------------------------
  env$cores <- cores
  env$parallel <- parallel
  
  
  # Calculate median latitude -----------------------------------------------
  # rast (if not in epsg:4326 datum, longer than using "sf") ----------------
  if (sol_rad == TRUE) {
    
    mde_rst <- dem
    
    if (crs(mde_rst, proj = T) != rgdal::CRSargs(CRS("EPSG:4326"))) {
      mde_rst <- terra::project(mde_rst, y = "EPSG:4326", method = "near")
      
    }
    
    ext <- terra::ext(mde_rst) %>% as.vector()
    latitude <- as.numeric((ext[4] - ext[3]) / 2 + ext[3])
    
  }
  
  tmp_inname <- sprintf("%s.tif", tempfile())
  terra::writeRaster(dem, tmp_inname,  overwrite = T, gdal = c("COMPRESS=LZW"))
  r_base <- dem
  dem <- tmp_inname
  
  if (verbose == TRUE) {
    print("---------------------------- Start of generating DEM derivatives")
  } 
  
  
  
  
  
  # Analytical Hillshading --------------------------------------------------
  rsaga.geoprocessor("ta_lighting",
                     module = 0,
                     list(ELEVATION = dem,
                          SHADE = paste0(outdir, "/hillshade.sgrd"),
                          METHOD = 0,
                          POSITION = 0,
                          AZIMUTH = 315,
                          DECLINATION = 45,
                          DATE = "2018-10-12",
                          TIME = 12,
                          EXAGGERATION = 1,
                          UNIT = 0,
                          SHADOW = 0,
                          NDIRS = 8,
                          RADIUS = 10),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Analytical Hillshading Finished")
  }
  
  
  # Convergence index ---------------------------------------------------------
  rsaga.geoprocessor("ta_morphometry", module = 1,
                     list(ELEVATION = dem,
                          RESULT = paste0(outdir, "/convergence_index.sgrd"),
                          METHOD = "Aspect",
                          NEIGHBOURS = "1"), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Convergence index Finished")
  }
  
  
  # Curvature Classification --------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 4,
                     list(DEM = dem,
                          CLASS = paste0(outdir, "/curvature_classification.sgrd"),
                          THRESHOLD = 0.05), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Curvature Classification Finished")
  }
  
  
  # Potential Incoming Solar Radiation ---------------------------------
  if (sol_rad == TRUE) {
    
    rsaga.geoprocessor("ta_lighting",
                       module = 2,
                       list(GRD_DEM = dem,
			    GRD_VAPOUR_DEFAULT = 10,
			    GRD_LINKE_DEFAULT = 3,
			    SOLARCONST = 1367,
			    LOCALSVF = 1,
			    UNITS = 0,
			    SHADOW = 0,
			    LOCATION = 0,
			    LATITUDE = latitude,
			    PERIOD = 2,
			    DAY = start_date,
			    DAY_STOP = end_date,
			    DAYS_STEP = 1,
			    MOMENT = 12,
			    HOUR_STEP = 0.5,
			    METHOD = 2,
			    ATMOSPHERE = 12000,
			    PRESSURE = 1013,
			    WATER = 1.68,
			    DUST = 100,
			    LUMPED = 70,
			    UPDATE = 0,
			    GRD_DIRECT = paste0(outdir, "/solar_direct.sgrd"),
                            GRD_DIFFUS = paste0(outdir, "/solar_diffuse.sgrd"),
                            GRD_TOTAL = paste0(outdir, "/solar_total.sgrd"),
                            GRD_RATIO = paste0(outdir, "/solar_ratio.sgrd"),
			    GRD_FLAT = paste0(outdir, "/solar_flat_terrain.sgrd"),
                            GRD_DURATION = paste0(outdir, "/solar_duration.sgrd"),
                            GRD_SUNRISE = paste0(outdir, "/solar_sunrise.sgrd"),
                            GRD_SUNSET = paste0(outdir, "/solar_sunset.sgrd")),
                       flags = "s", env = env)
    
    if (verbose == TRUE) {
      print("Potential Incoming Solar Radiation Finished")
    }
  }
  
  
  # Diurnal Anisotropic Heat --------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 12,
                     list(DEM = dem,
                          DAH = paste0(outdir, "/diurnal_anisotropic_heat.sgrd"),
                          ALPHA_MAX = 202.5), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Diurnal Anisotropic Heat Finished")
  }
  
  
  # Downslope Distance Gradient -----------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 9,
                     list(DEM = dem,
                          GRADIENT = paste0(outdir, "/gradient.sgrd"),
                          DIFFERENCE = paste0(outdir, "/difference.sgrd"),
                          DISTANCE = 10,
                          OUTPUT = 2), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Downslope Distance Gradient Finished")
  }
  
  
  # Effective Air Flow Heights ------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 11,
                     list(DEM = dem,
                          AFH = paste0(outdir, "/effective_air_flow_heights.sgrd"),
                          LUV = 1), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Effective Air Flow Heights Finished")
  }
  
  
  # Mass Balance Index --------------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 10,
                     list(DEM = dem,
                          MBI = paste0(outdir, "/mass_balance_index.sgrd"),
                          TSLOPE = 15,
                          TCURVE = 0.01,
                          THREL = 15), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Mass Balance Index Finished")
  }
  
  
  # Morphometric Protection Index ----------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 7,
                     list(DEM = dem,
                          PROTECTION = paste0(outdir, "/morphometric_protection_index.sgrd"),
                          RADIUS = 2000), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Morphometric Protection Index Finished")
  }
  
  
  # Multiresolution Index of Valley Bottom Flatness (MRVBF) --------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 8,
                     list(DEM = dem,
                          MRVBF = paste0(outdir, "/mrvbf.sgrd"),
                          MRRTF = paste0(outdir, "/mrrtf.sgrd"),
                          T_SLOPE = 16,
                          T_PCTL_V = 0.4,
                          T_PCTL_R = 0.35,
                          P_SLOPE = 4,
                          P_PCTL = 3,
                          UPDATE = 1,
                          CLASSIFY = 0,
                          MAX_RES = 50), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("MRVBF and MRRTF Finished")
  }
  
  
  # Real surface area ---------------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 6,
                     list(DEM = dem,
                          AREA = paste0(outdir, "/real_surface_area.sgrd")),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Real Surface Area Finished")
  }
  
  
  # Relative Heights and Slope Positions --------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 14,
                     list(DEM = dem,
                          HO = paste0(outdir, "/slope_height.sgrd"),
                          HU = paste0(outdir, "/valley_depth.sgrd"),
                          NH = paste0(outdir, "/normalized_height.sgrd"),
                          SH = paste0(outdir, "/standardized_height.sgrd"),
                          MS = paste0(outdir, "/mid_slope_position.sgrd"),
                          W = 0.5,
                          T = 10,
                          E = 2), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Relative Heights and Slope Positions Finished")
  }
  
  
  # Ridge Level (Valley Depth) ----------------------------------------------
  rsaga.geoprocessor("ta_channels",
                     module = 7,
                     list(ELEVATION = dem,
                          RIDGE_LEVEL = paste0(outdir, "/ridge_level.sgrd"),
                          THRESHOLD = 1,
                          NOUNDERGROUND = 1,
                          ORDER = 4),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Ridge Level Finished")
  }
  
  
  # Sky View Factor ---------------------------------------------------------
  rsaga.geoprocessor("ta_lighting",
                     module = 3,
                     list(DEM = dem,
                          SVF = paste0(outdir, "/sky_view_factor.sgrd"),
                          RADIUS = 10000,
                          METHOD = 1,
                          DLEVEL = 3,
                          NDIRS = 8),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Sky View Factor Finished")
  }
  
  
  # Slope, aspect e curvatures ----------------------------------------------
  rsaga.geoprocessor("ta_morphometry", module = 0,
                     list(ELEVATION = dem,
                          METHOD = 4,
                          UNIT_SLOPE = 1,
                          UNIT_ASPECT = 1,
                          SLOPE = paste0(outdir, "/slope_degress.sgrd"),
                          ASPECT = paste0(outdir, "/aspect.sgrd"),
                          C_GENE = paste0(outdir, "/curv_general.sgrd"),
                          C_PROF = paste0(outdir, "/curv_profile.sgrd"),
                          C_PLAN = paste0(outdir, "/curv_plan.sgrd"),
                          C_TANG = paste0(outdir, "/curv_tangencial.sgrd"),
                          C_LONG = paste0(outdir, "/curv_longitudinal.sgrd"),
                          C_CROS = paste0(outdir, "/curv_cross_sectional.sgrd"),
                          C_MINI = paste0(outdir, "/curv_minimal.sgrd"),
                          C_MAXI = paste0(outdir, "/curv_maximal.sgrd"),
                          C_TOTA = paste0(outdir, "/curv_total.sgrd"),
                          C_ROTO = paste0(outdir, "/curv_flow_line.sgrd")),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Slope, Aspect and Curvatures Finished")
  }
  
  
  # Surface Specific Points ---------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 3,
                     list(ELEVATION = dem,
                          RESULT = paste0(outdir, "/surface_specific_points.sgrd"),
                          METHOD = "1",
                          THRESHOLD = 2), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Surface Specific Points Finished")
  }
  
  
  # Terrain Ruggedness Index (TRI)  -------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 16,
                     list(DEM = dem,
                          TRI = paste0(outdir, "/terrain_ruggedness_index.sgrd"),
                          MODE = 1,
                          RADIUS = 1,
                          DW_WEIGHTING = 0,
                          DW_IDW_POWER = 1,
                          DW_IDW_OFFSET = 1,
                          DW_BANDWIDTH = 1), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Terrain Ruggedness Index Finished")
  }
  
  
  # Terrain Surface Classification (Iwahashi and Pike) ------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 22,
                     list(DEM = dem,
                          LANDFORMS = paste0(outdir, "/terrain_surface_classification_iwahashi.sgrd"),
                          CONV_RECALC = 0,
                          TEXTURE = 0,
                          TEXT_RECALC = 0,
                          TYPE = 2,
                          CONV_SCALE = 10,
                          CONV_KERNEL = 0,
                          CONV_TYPE= 0,
                          CONV_EPSILON = 0,
                          TEXT_SCALE = 2,
                          TEXT_EPSILON = 1), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Terrain Surface Classification (Iwahashi and Pike) Finished")
  }
  
  
  # Terrain Surface Convexity -------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 21,
                     list(DEM = dem,
                          CONVEXITY = paste0(outdir, "/terrain_surface_convexity.sgrd"),
                          KERNEL = 0,
                          TYPE = 0,
                          EPSILON = 0,
                          SCALE = 3,
                          METHOD = 1,
                          DW_WEIGHTING = 3,
                          DW_IDW_POWER= 1,
                          DW_IDW_OFFSET = 1,
                          DW_BANDWIDTH = 0.7), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Terrain Surface Convexity Finished")
  }
  
  
  # Terrain Surface Texture ---------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 20,
                     list(DEM = dem,
                          TEXTURE = paste0(outdir, "/terrain_surface_texture.sgrd"),
                          EPSILON = 1,
                          SCALE = 10,
                          METHOD = 1,
                          DW_WEIGHTING = 3,
                          DW_IDW_POWER = 1,
                          DW_IDW_OFFSET = 1,
                          DW_BANDWIDTH = 0.7), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Terrain Surface Texture Finished")
  }
  
  
  # Topographic Openness ----------------------------------------------------
  rsaga.geoprocessor("ta_lighting",
                     module = 5,
                     list(DEM = dem,
                          POS = paste0(outdir, "/topo_openness.sgrd"),
                          RADIUS = 10000,
                          METHOD = 1,
                          DLEVEL = 3,
                          NDIRS = 8),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Topographic Openness Finished")
  }
  
  
  # Topographic Position Index (TPI) ------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 18,
                     list(DEM = dem,
                          TPI = paste0(outdir, "/topographic_position_index.sgrd"),
                          STANDARD = 0,
                          RADIUS_MIN = 0,
                          RADIUS_MAX = 100,
                          DW_WEIGHTING = 0,
                          DW_IDW_POWER = 1,
                          DW_IDW_OFFSET = 1,
                          DW_BANDWIDTH = 75), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Topographic Position Index Finished")
  }
  
  
  # TPI Based Landform Classification -----------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 19,
                     list(DEM = dem,
                          LANDFORMS = paste0(outdir, "/landforms_tpi_based.sgrd"),
                          RADIUS_A_MIN = 0,
                          RADIUS_A_MAX = 100,
                          RADIUS_B_MIN = 0,
                          RADIUS_B_MAX = 1000,
                          DW_WEIGHTING = 0,
                          DW_IDW_POWER = 1,
                          DW_IDW_OFFSET = 1,
                          DW_BANDWIDTH = 75), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("TPI Based Landform Classification Finished")
  }
  
  
  # Valley and Ridge Detection (Top Hat Approach) -----------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 24,
                     list(DEM = dem,
                          VALLEY = paste0(outdir, "/valley.sgrd"),
                          HILL = paste0(outdir, "/hill.sgrd"),
                          VALLEY_IDX = paste0(outdir, "/valley_idx.sgrd"),
                          HILL_IDX = paste0(outdir, "/hill_idx.sgrd"),
                          SLOPE_IDX = paste0(outdir, "/slope_idx.sgrd"),
                          RADIUS_VALLEY = 1000,
                          RADIUS_HILL = 1000,
                          THRESHOLD= 100,
                          METHOD = 0), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Valley and Ridge Detection (Top Hat Approach) Finished")
  }
  
  
  # Vector Ruggedness Measure -------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 17,
                     list(DEM = dem,
                          VRM = paste0(outdir, "/vector_ruggedness_index.sgrd"),
                          MODE = 1,
                          RADIUS = 1,
                          DW_WEIGHTING = 0,
                          DW_IDW_POWER = 1,
                          DW_IDW_OFFSET = 1,
                          DW_BANDWIDTH = 1), flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Vector Ruggedness Measure Finished")
  }
  
  
  # Wetness Index -------------------------------------------------------------
  rsaga.geoprocessor("ta_hydrology",
                     module = 15,
                     list(DEM = dem,
			  AREA = paste0(outdir, "/area_twi.sgrd"),
			  SLOPE = paste0(outdir, "/slope_twi.sgrd"),
			  AREA_MOD = paste0(outdir, "/area_mod_twi.sgrd"),
			  TWI = paste0(outdir, "/wetness index.sgrd"),
                      	  SUCTION = 10,
                          AREA_TYPE = 1,
			  SLOPE_TYPE = 1,
			  SLOPE_MIN = 0,
			  SLOPE_OFF = 0.1,
			  SLOPE_WEIGHT = 1), 
		     flags = "s", env = env)

  
  if (verbose == TRUE) {
    print("Wetness Index Finished")
  }
  
  
  # Wind Exposition Index ---------------------------------------------------
  rsaga.geoprocessor("ta_morphometry",
                     module = 27,
                     list(DEM = dem,
                          EXPOSITION = paste0(outdir, "/wind_exposition.sgrd"),
                          MAXDIST = 300,
                          STEP = 15,
                          OLDVER = 0,
                          ACCEL = 1.5,
                          PYRAMIDS = 0),
                     flags = "s", env = env)
  
  if (verbose == TRUE) {
    print("Wind Exposition Finished")
  }
  
  
  if (verbose == TRUE) {
    print("-------------------------------------------------------------------")
  }
  
  if (verbose == TRUE) {
    print("-------------------------------- End of generating DEM derivatives")
    
    
    print("------------------------- Start converting files to tif")
  }
  
  
  # Import sdat file and convert to .tif (sdat files are deleted) -------------
  result_list <- list.files(outdir, pattern = ".sdat$", full.names = T)
  result_rasters <- lapply(result_list, terra::rast)
  result_list_tif <- gsub(pattern = "sdat", "tif", result_list)
  
  if (verbose == TRUE) {
    if (align_rasters == TRUE) {
      print("----------------------------------------------- Aligning rasters")
    }
  }
  
  if (align_rasters == T) {
    for (i in seq_along(result_rasters)){
      
      if (terra::compareGeom(result_rasters[[i]], r_base,
                             stopOnError = F, res = T)) {
        terra::writeRaster(result_rasters[[i]], result_list_tif[[i]],
                           overwrite = T, gdal = c("COMPRESS=LZW"))
      } else {
        result_rasters[[i]] <- terra::resample(result_rasters[[i]], r_base,
                                               method = "near")
        terra::writeRaster(result_rasters[[i]], result_list_tif[[i]],
                           overwrite = T, gdal = c("COMPRESS=LZW"))
      }
      terra::writeRaster(result_rasters[[i]], result_list_tif[[i]],
                         overwrite = T, gdal = c("COMPRESS=LZW"))
    }
  } else {
    
    for (i in seq_along(result_rasters)) {
      terra::writeRaster(result_rasters[[i]], result_list_tif[[i]],
                         overwrite = T, gdal = c("COMPRESS=LZW"))
    }
  }
  
  if (verbose == TRUE) {
    if (align_rasters == TRUE) {
      print("---------------------------------- End of raster alignment")
    }
  }
  
  if (verbose == TRUE) {
    print("---------------------------- End of converting files to tif")
    print("----------------------------------------- Deleting non-TIF files")
  }
  
  # Delete non-tiff files -----------------------------------------------------
  list.files(path = outdir, full.names = T) %>% 
    as.data.frame() %>% filter(!str_detect(., "tif")) %>% pull() %>% unlink()
  
  
  # Remove lists from R environment -------------------------------------------
  remove(result_rasters, result_list_tif, result_list)
  
  if (verbose == TRUE) {
    print("----------------------------------------- Deleted non-TIF files")
  }
  
}
