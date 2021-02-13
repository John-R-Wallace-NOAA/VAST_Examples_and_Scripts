

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
 
 
# ============ Create a figure to view VAST's 'surveyname' grids (i.e. Domains) for CA Current region and plot the raw data on top of each one ====================

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/dataWareHouseTrawlCatch.R") 
 
 
WC <- FishStatsUtils:::Prepare_WCGBTS_Extrapolation_Data_Fn()

WC$Data_Extrap[1:3,]

Data_Set <- dataWareHouseTrawlCatch('Pacific spiny dogfish', yearRange =  c(1997, 2001), project = 'AFSC.Slope')
     
Imap::imap(longlat = list(world.h.land, world.h.borders), col = c("black", "cyan"), poly = c("grey40", NA), longrange = c(-146, -115), latrange = c(32, 49), zoom = FALSE)

change(WC$Data_Extrap[as.logical(WC$Data_Extrap$propInWCGBTS), ])
points(Lon, Lat, col = 'red', pch = '.')
text(-124.3, 36.67, "propInWCGBTS", cex = 0.6)

change(WC$Data_Extrap[as.logical(WC$Data_Extrap$propInCCA), ])
points(Lon, Lat, col = 'yellow', pch = '.')
points(Data_Set$Longitude_dd, Data_Set$Latitude_dd, pch = '.', cex = 2)

 
change(WC$Data_Extrap[as.logical(WC$Data_Extrap$propInTriennial), ])
points(Lon - 5, Lat, col = 'blue', pch = '.')
points(Data_Set$Longitude_dd - 5, Data_Set$Latitude_dd, pch = '.', cex = 2)
text(-129.2, 36.67, "propInTriennial", cex = 0.6)

change(WC$Data_Extrap[as.logical(WC$Data_Extrap$propInSlope98_00), ])
points(Lon - 10, Lat, col = 'green', pch = '.')
points(Data_Set$Longitude_dd - 10, Data_Set$Latitude_dd, pch = '.', cex = 2)
text(-134.37, 36.67, "propInSlope98_00", cex = 0.6)

change(WC$Data_Extrap[as.logical(WC$Data_Extrap$propInSlope01), ])
points(Lon - 15, Lat, col = 'cyan', pch = '.')
points(Data_Set$Longitude_dd - 15, Data_Set$Latitude_dd, pch = '.', cex = 2)
text(-139.0, 36.67, "propInSlope01", cex = 0.6)

change(WC$Data_Extrap[as.logical(WC$Data_Extrap$propInSlope02), ])
points(Lon - 20, Lat, col = 'purple', pch = '.')
points(Data_Set$Longitude_dd - 20, Data_Set$Latitude_dd, pch = '.', cex = 2)
text(-144.3, 36.67, "propInSlope02", cex = 0.6)

      
       
# ================================ Setup and run the model ============================================

sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/R/West_Coast_2021_V3.6.1.R")   
 
 
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
  OverdispersionConfig = c(eta1 = 0, eta2 = 0), # Default: if(grepl('WCGBTS', Survey)) c(eta1 = 0, eta2 = "AR1") else c(eta1 = 0, eta2 = 0)
        use_anisotropy = FALSE,
              ObsModel = c(2, 0), # 1 = Lognormal, 2 = Gamma
          bias.correct = FALSE, 
             max_cells = 3000,
           knot_method = 'samples'
)          


West_Coast_2021_V3.6.1(spFormalName = 'Pacific spiny dogfish', spLongName = 'Spiny dogfish', spShortName = 'DSRK', Survey = 'AFSC.Slope', 
                  VAST_surveyName = 'propInSlope98_00', Settings = Settings, yearRange = c(1997, 2001))  # For 'AFSC.Slope', VAST_surveyName = 'propInSlope98_00' is the default
                 
 
 
