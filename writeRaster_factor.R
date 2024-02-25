
writeRaster_factor <- function(rst, cd_name, filename) {
  
  
  pkg <- c("raster", "terra", "dplyr", "foreign")
  
  
  installed_packages <- pkg %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("installing package", pkg[!installed_packages]))
    install.packages(pkg[!installed_packages])
    print("installed packages")
  }
  
  # Packages loading
  invisible(lapply(pkg, library, character.only = TRUE))
  
  
  
  
  
  levels(rst) <- cd_name
  
  terra::writeRaster(rst,
                     filename = paste0(filename, ".tif"),
                     wopt= list(gdal = c("COMPRESS=LZW", "TFW=YES"),
                                datatype = "INT4U"),
                     overwrite = T)
  
  
  
  as.data.frame(table(as.vector(raster::raster(rst))), 
                stringsAsFactors = FALSE) %>%
    rename(VALUE = Var1, COUNT = Freq) %>%
    mutate(VALUE = as.numeric(as.integer(VALUE))) %>% 
    left_join(cd_name, by = c("VALUE" = "code")) %>% 
    foreign::write.dbf(., paste0(filename, ".tif.vat.dbf"), 
                       factor2char = TRUE, max_nchar = 254)
  
  
  
}
