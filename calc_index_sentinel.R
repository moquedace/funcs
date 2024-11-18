calc_index_sentinel <- function (rst,
                                 b2 = "b2", 
                                 b3 = "b3", 
                                 b4 = "b4", 
                                 b5 = "b5", 
                                 b6 = "b6", 
                                 b7 = "b7", 
                                 b8 = "b8", 
                                 b8a = "b8a",
                                 b11 = "b11",
                                 b12 = "b12",
                                 graph = T) {
  
  B <- rst[[b2]]
  G <- rst[[b3]]
  R <- rst[[b4]]
  RE1 <- rst[[b5]]
  RE2 <- rst[[b6]]
  RE3 <- rst[[b7]]
  N <- rst[[b8]]
  N2 <- rst[[b8a]]
  S1 <- rst[[b11]]
  S2 <- rst[[b12]]
  
  
  
  # index -------------------------------------------------------------------
  
  # Inverted Red-Edge Chlorophyll Index
  IRECI <-  (RE3 - R) / (RE1 / RE2)     
  names(IRECI) <- "IRECI" 
  
  
  # Modified Chlorophyll Absorption in Reflectance Index 
  MCARI  <-  ((RE1 - R) - 0.2 * (RE1 - G)) * (RE1 / R)  
  names(MCARI) <- "MCARI"    
  
  
  # Modified Chlorophyll Absorption in Reflectance Index (705 and 750 nm)
  MCARI705 <-  ((RE2 - RE1) - 0.2 * (RE2 - G)) * (RE2 / RE1)                                            
  names(MCARI705) <- "MCARI705" 
  
  
  # MCARI/OSAVI Ratio                                                  
  MCARIOSAVI <- (((RE1 - R) - 0.2 * (RE1 - G)) * (RE1 / R)) / (1.16 * (N - R) / (N + R + 0.16))           
  names(MCARIOSAVI) <- "MCARIOSAVI"   
  
  
  # MCARI/OSAVI Ratio (705 and 750 nm)                                 
  MCARIOSAVI705 <-  (((RE2 - RE1) - 0.2 * (RE2 - G)) * (RE2 / RE1)) / (1.16 * (RE2 - RE1) / (RE2 + RE1 + 0.16))
  names(MCARIOSAVI705) <- "MCARIOSAVI705"
  
  
  # Modified Simple Ratio (705 and 750 nm)                            
  MSR705 <- (RE2 / RE1 - 1) / ((RE2 / RE1 + 1)^0.5) 
  names(MSR705) <- "MSR705"   
  
  
  # Normalized Difference (705 and 750 nm)                            
  ND705 <-  (RE2 - RE1)/(RE2 + RE1) 
  names(ND705) <- "ND705"  
  
  
  # Ratio Vegetation Index                                             
  RVI <- RE2 / R 
  names(RVI) <- "RVI"    
  
  
  # Sentinel-2 Red-Edge Position                                      
  S2REP  <-  705.0 + 35.0 * ((((RE3 + R) / 2.0) - RE1) / (RE2 - RE1))                                
  names(S2REP) <- "S2REP"   
  
  
  # Sentinel-2 Water Index                                            
  S2WI <- (RE1 - S2)/(RE1 + S2)  
  names(S2WI) <- "S2WI"
  
  
  # Sentinel-2 LAI Green Index                                        
  SeLI <- (N2 - RE1) / (N2 + RE1)   
  names(SeLI) <- "SeLI"    
  
  
  # Triangle Water Index                                              
  TWI  <-  (2.84 * (RE1 - RE2) / (G + S2)) + ((1.25 * (G - B) - (N - B)) / (N + 1.25 * G - 0.25 * B)) 
  names(TWI) <- "TWI"  
  
  
  # Vegetation Index (700 nm)
  VI700 <- (RE1 - R) / (RE1 + R) 
  names(VI700) <- "VI700" 
  
  
  # Normalized Difference Vegetation Index
  NDVI  <- (N - R) / (N + R)
  names(NDVI) <- "NDVI" 
  
  
  # Enhanced Vegetation Index
  EVI <- 2.5 * ((N - R) / (N + 6 * R - 7.5 * B + 1))
  names(EVI) <- "EVI" 
  
  
  # Soil Adjusted Vegetation Index
  SAVI <- ((N - R) / (N + R + 0.5)) * (1.5)
  names(SAVI) <- "SAVI" 
  
  
  # Modified Soil Adjusted Vegetation Index
  MSAVI <- (2 * N + 1 - sqrt((2 * N + 1)^2 - 8 * (N - R))) / 2
  names(MSAVI) <- "MSAVI" 
  
  
  # Normalized Difference Moisture Index
  NDMI <- (N - S1) / (N + S1)
  names(NDMI) <- "NDMI"
  
  # Normalized Difference Moisture Index 2
  NDMI2 <- (N2 - S1) / (N2 + S1)
  names(NDMI2) <- "NDMI2"
  
  
  # Normalized Burn Ratio
  NBR <- (N - S2) / (N + S2)
  names(NBR) <- "NBR"
  
  
  # Normalized Burn Ratio 2
  NBR2 <- (S1 - S2) / (S1 + S2)
  names(NBR2) <- "NBR2"
  
  
  # Normalized Difference Snow Index
  NDSI <- (R - S1) / (R + S1)
  names(NDSI) <- "NDSI"
  
  
  # Normalized Difference Snow Index 2
  NDSI2 <- (G - S1) / (G + S1)
  names(NDSI2) <- "NDSI2"
  
  
  # Normalized Difference Water Index
  NDWI <- (G - N) / (G + N)
  names(NDWI) <- "NDWI"
  
  
  
  
  
  # join index --------------------------------------------------------------
  
  
  
  lrst_index <- c(IRECI, MCARI, MCARI705, MCARIOSAVI, MCARIOSAVI705, MSR705,
                  ND705, RVI, S2REP, S2WI, SeLI, TWI, VI700, NDVI, EVI, SAVI,
                  SAVI, MSAVI, NDMI, NDMI2, NBR, NBR2, NDSI, NDSI2, NDWI)
  
  
  return(lrst_index)
  
  if (graph == T) {
    plot(lrst_index)
    
  }
  
}

