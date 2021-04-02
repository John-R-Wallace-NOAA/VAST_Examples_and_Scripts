

sourceFunctionURL <- function (URL) 
   {
      " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
      require(httr)
      File.ASCII <- tempfile()
      on.exit(file.remove(File.ASCII))
      getTMP <- httr::GET(URL)
      write(paste(readLines(textConnection(httr::content(getTMP))), collapse = "\n"), File.ASCII)
      source(File.ASCII)
   }
    
require(VAST)

# --------------------------------------------------------------------------

spFormalName = 'Pacific spiny dogfish'
spLongName = 'Spiny dogfish'
spShortName = 'DSRK'

yearRange <- c(1000, 5000)

Survey <- 'WCGBTS.Combo'   # For 'WCGBTS.Combo', VAST_surveyName = 'propInWCGBTS' is the default
# Survey <- 'AFSC.Slope'; yearRange <- c(1997, 2001)   # For 'AFSC.Slope', VAST_surveyName = 'propInSlope98_00' is the default

 
# ===== Create a figure to view VAST's 'surveyname' grids (i.e. Domains) for CA Current region and plot the same raw data as black points on top of each one ====

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/dataWareHouseTrawlCatch.R") 
 
 
WC <- FishStatsUtils:::Prepare_WCGBTS_Extrapolation_Data_Fn()

WC$Data_Extrap[1:3,]

Data_Set <- dataWareHouseTrawlCatch(spFormalName, yearRange = yearRange, project = Survey)

for(PNG in c(FALSE, TRUE)) {  
 
    if(PNG) 
       png('VASTs_surveyname_grids_with_raw_data_on_top.png', width = 1000, height = 1000)
    
    Imap::imap(longlat = list(world.h.land, world.h.borders), col = c("black", "cyan"), poly = c("grey40", NA), longrange = c(-146, -115), latrange = c(32, 49), zoom = FALSE)
    
    Cex <- 2; Col <- 'black'
    if(Survey == 'WCGBTS.Combo') Cex <- 0.5; Col <- col.alpha('black', 0.20)
    
    
    WS <- WC$Data_Extrap[as.logical(WC$Data_Extrap$propInWCGBTS), ]
    points(WS$Lon, WS$Lat, col = 'red', pch = '.')
    text(-124.3, 36.67, "propInWCGBTS", cex = 0.6)
    
    WS <- WC$Data_Extrap[as.logical(WC$Data_Extrap$propInCCA), ]
    points(WS$Lon, WS$Lat, col = 'yellow', pch = '.')
    points(Data_Set$Longitude_dd, Data_Set$Latitude_dd, pch = '.', cex = Cex, col = Col)
     
    WS <- WC$Data_Extrap[as.logical(WC$Data_Extrap$propInTriennial), ]
    points(WS$Lon - 5, WS$Lat, col = 'blue', pch = '.')
    points(Data_Set$Longitude_dd - 5, Data_Set$Latitude_dd, pch = '.', cex = Cex, col = Col)
    text(-129.2, 36.67, "propInTriennial", cex = 0.6)
    
    WS <- WC$Data_Extrap[as.logical(WC$Data_Extrap$propInSlope98_00), ]
    points(WS$Lon - 10, WS$Lat, col = 'green', pch = '.')
    points(Data_Set$Longitude_dd - 10, Data_Set$Latitude_dd, pch = '.', cex = Cex, col = Col)
    text(-134.37, 36.67, "propInSlope98_00", cex = 0.6)
    
    WS <- WC$Data_Extrap[as.logical(WC$Data_Extrap$propInSlope01), ]
    points(WS$Lon - 15, WS$Lat, col = 'cyan', pch = '.')
    points(Data_Set$Longitude_dd - 15, Data_Set$Latitude_dd, pch = '.', cex = Cex, col = Col)
    text(-139.0, 36.67, "propInSlope01", cex = 0.6)
    
    WS <- WC$Data_Extrap[as.logical(WC$Data_Extrap$propInSlope02), ]
    points(WS$Lon - 20, WS$Lat, col = 'purple', pch = '.')
    points(Data_Set$Longitude_dd - 20, Data_Set$Latitude_dd, pch = '.', cex = Cex, col = Col)
    text(-144.3, 36.67, "propInSlope02", cex = 0.6)
    
    if(PNG)
       dev.off()
}      
       
# ================================ Setup and run the model ============================================

# sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/R/West_Coast_2021_V3.6.1.R")   
 
 
(strata.limits <- data.frame(
       STRATA = c("Coastwide","CA","OR","WA"),
       north_border = c(49.0, 42.0, 46.0, 49.0),
       south_border = c(34.0, 34.0, 42.0, 46.0),
       shallow_border = c(55, 55, 55, 55),
       deep_border = c(1280, 1280, 1280, 1280)
))

(Version <- FishStatsUtils::get_latest_version(package="VAST"))

Settings <- FishStatsUtils::make_settings(
               Version = Version, 
                   n_x = 250, 
                Region =  "California_current",             
               purpose = if(as.numeric(substr(packageVersion('VAST'), 1, 3)) <= 3.3)  'index' else 'index2', 
            fine_scale = TRUE, 
         strata.limits = strata.limits,
           FieldConfig = c(Omega1 = 'IID', Epsilon1 = 'IID', Omega2 = 'IID', Epsilon2 = 'IID'), 
             RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), 
  OverdispersionConfig = c(eta1 = 0, eta2 = 0), # Default: if(grepl('WCGBTS', Survey)) c(Delta1 = 1, Delta2 = 1) else c(eta1 = 0, eta2 = 0)
        use_anisotropy = FALSE,
              ObsModel = c(1, 0), # 1 = Lognormal, 2 = Gamma
          bias.correct = FALSE, 
             max_cells = 3000,
           knot_method = 'samples'
)          


West_Coast_2021_V3.6.1(spFormalName = spFormalName, spLongName = spLongName, spShortName = spShortName,
                         Survey = Survey, Settings = Settings, yearRange = yearRange)   # fine_scale. = TRUE is the default
                 
 
 

