rst_class_name_max <- function(rst) {
  
  
  
  pkg <- c("dplyr", "terra", "stringr")
  
  
  installed_packages <- pkg %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    print(paste("installing package", pkg[!installed_packages]))
    install.packages(pkg[!installed_packages])
    print("installed packages")
  }
  
  invisible(lapply(pkg, library, character.only = TRUE))
  
  
  if (class(rst) %in% c("RasterStack", "RasterLayer")) {
    rst <- rast(rst)
  }
  else {
    if (class(rst) != "SpatRaster") {
      stop("Error : file must be in RasterStack or SpatRaster format")
    }
  }
  
  n_layers <- nlyr(rst) + 2
  
  
  df <- as.data.frame(rst, xy = T, na.rm = T) %>% 
    mutate(
      class_dom = as.factor(colnames(.)[max.col(.[,3:n_layers]) + 2])) %>% 
    mutate(code = unclass(class_dom)) %>% 
    dplyr::select(x, y, class_dom, code)
  
  
  g_class <- df %>% 
    dplyr::select(-x, -y) %>% 
    group_by(class_dom) %>% 
    distinct(class_dom, .keep_all = T) %>% 
    arrange(code) %>% 
    relocate(code) %>% 
    ungroup() %>% as.data.frame()
  
  
  r_class <- df %>% 
    dplyr::select(-class_dom) %>% 
    rast(type = "xyz",
         crs = paste0(crs(rst, describe = T)$authority,
                      ":", crs(rst, describe = T)$code), digits = 2) 
  
  
  levels(r_class) <- g_class
  
  
  return(r_class)
  
}



r <- rst_class_name_max(rst = r)




plot(r, type = "classes")


























