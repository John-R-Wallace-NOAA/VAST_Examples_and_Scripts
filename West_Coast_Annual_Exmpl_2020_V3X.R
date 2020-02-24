
# Download with:
# JRWToolBox::gitAFile("John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/West_Coast_Annual_Exmpl_2020_V3X.R", "script", File = "West_Coast_Annual_example_2019.R", show = FALSE)
# or edit with [using a properly configured gitEdit()]
# JRWToolBox::gitEdit(West_Coast_Annual_Exmpl_2020_V3X, "John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/")


# Test run of single species spatial delta glmm
# Test, canary data; implementation, Lingcod groundfish survey data
# Based on single-species example
# Revised by M. Haltuch, Feb 2017
# Revised by J. Wallace Mar 2017
# Revised by James Thorson April 2017
# Revised by J. Wallace Apr 2017
# Revised by J. Wallace Dec 2018
# Revised by J. Wallace FEb 2020; uses fine_scale = TRUE in VAST ver. 3X and JRWToolBox::YearlyResultsFigure_VAST3X(), following the upper level functions (wrappers) approach.

# =============================================

# VAST will often leave you in the subdirectory of the current run. Using HomeDir helps get you back where you started.
# Only do this once per R session, after you are in the your main working directory:

HomeDir <- getwd()

# =============================================


summaryNWFSC <- function( fit. = fit, obj = fit$tmb_list$Obj, Opt = fit$parameter_estimates, sdreport = fit$parameter_estimates$SD, savedir = DateFile ) {

    # Based on James Thorson's summary_nwfsc(), circa 2017
    # Revised by John Wallace Dec 2018

    f <- function(num,threshold=0.000001) ifelse(num<threshold,paste0("< ",threshold),num)
    # Table of settings
    TableA = data.frame( "Setting_name" = rep(NA, 6), "Setting_used" = NA )
    TableA[1,] <- c("Number of knots", fit.$spatial_list$n_x)
    TableA[2,] <- c("Maximum gradient", formatC(f(max(abs( obj$gr(TMB::summary.sdreport(sdreport,"fixed")[,'Estimate'])))),format="f",digits=6) )
    TableA[3,] <- c("Is hessian positive definite?", switch(as.character(sdreport$pdHess),"FALSE"="No","TRUE"="Yes") )
    TableA[4,] <- c("Was bias correction used?", ifelse("Est. (bias.correct)"%in%colnames(TMB::summary.sdreport(sdreport)),"Yes","No") )
    TableA[5,] <- c("Distribution for measurement errors", switch(as.character(obj$env$data$ObsModel[1]),"1"="Lognormal","2"="Gamma") )
    
    # TableA[6,] <- c("Spatial effect for encounter probability", switch(as.character(fit.$data_list$FieldConfig[1, 1]),"-1"="No","1"="Yes") )
    # TableA[7,] <- c("Spatio-temporal effect for encounter probability", switch(as.character(fit.$data_list$FieldConfig[1, 2]),"-1"="No","1"="Yes") )
    # TableA[8,] <- c("Spatial effect for positive catch rate", switch(as.character(fit.$data_list$FieldConfig[2, 1]),"-1"="No","1"="Yes") )
    # TableA[9,] <- c("Spatio-temporal effect for positive catch rate", switch(as.character(fit.$data_list$FieldConfig[2, 2]),"-1"="No","1"="Yes") )
    
    FieldConfig <- fit.$data_list$FieldConfig
    comment(FieldConfig) <- "\n                                Encounter Probability(1), Positive Catch Rates(2)\nSpatial Random Effects\nSpatiotemporal\n# of Factors for Intercepts\n\n0 = Off, 1 = On, +2 = additionl factors up to maximum number of categories in factor analysis covariance, IID = independent for each category\n\n"

    
    # Print number of parameters
    # TableB = FishStatsUtils:::list_parameters( obj, verbose = FALSE )
    TableB <- list_parameters( obj, verbose = FALSE )
    
    # Print table of MLE of fixed effects
    TableC <- JRWToolBox::r(JRWToolBox::renum(cbind(Param = Opt$diagnostics[, 1], TMB::summary.sdreport( Opt$SD, "fixed" ), Opt$diagnostics[, -1])))
        
    # Return
    # Return <- list(TableA = TableA, TableB = TableB, TableC =TableC)
    # if( !is.null(savedir)) for(i in 1:3) write(Return[[i]], file=paste0(savedir,"/",names(Return)[i],".txt"), row.names = FALSE)
    if( !is.null(savedir)) capture.output(cat("\n"), TableA, cat("\n\n"), FieldConfig, cat(comment(FieldConfig), "\n\n"), TableB, cat("\n\n"), TableC, file = paste0(savedir,"/Model_Summary.txt"))

    cat("\n")
    print(TableA); cat("\n\n"); print(FieldConfig); cat(comment(FieldConfig), "\n\n"); print(TableB); cat("\n\n"); print(TableC)
}

list_parameters <- function (Obj, verbose = TRUE) {

    # From ThorsonUtilities, circa 2017
    # If list.parameters() is moved to FishStatsUtils, this fucntion will be removed.

    Return = list()
    Table = data.frame()
    if (length(Obj$env$random) > 0) {
        Return[["Fixed_effects"]] = names(Obj$env$last.par[-Obj$env$random])
        Return[["Random_effects"]] = names(Obj$env$last.par[Obj$env$random])
        Table = data.frame(Coefficient_name = names(table(Return[["Fixed_effects"]])), 
            Number_of_coefficients = as.numeric(table(Return[["Fixed_effects"]])), 
            Type = "Fixed")
        Table = rbind(Table, data.frame(Coefficient_name = names(table(Return[["Random_effects"]])), 
            Number_of_coefficients = as.numeric(table(Return[["Random_effects"]])), 
            Type = "Random"))
    }
    else {
        Return[["Fixed_effects"]] = names(Obj$env$last.par)
        Table = data.frame(Coefficient_name = names(table(Return[["Fixed_effects"]])), 
            Number_of_coefficients = as.numeric(table(Return[["Fixed_effects"]])), 
            Type = "Fixed")
    }
    if (verbose == TRUE) {
        message("List of estimated fixed and random effects:")
        print(Table)
    }
    return(invisible(Table))
}


# =============================================

# 'spFormalName', is a common name that needs to work with the Data Warehouse, i.e. only proper names capitalized.
# 'spLongName' and 'spShortName' can be whatever is desired, the long name goes in the directory name and
# the short name goes into the file name of the Yearly Results Figure.

# Canary rockfish
  # spFormalName <- 'canary rockfish' 
  # spLongName <- 'Canary rockfish'
  # spShortName <- 'CNRY'

# Lingcod
spFormalName <- 'lingcod' 
spLongName <- 'Lingcod'
spShortName <- 'LCOD'

if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")

# If needed, re-install for the new YearlyResultsFigure_VAST3X()
if (!any(installed.packages()[, 1] %in% "JRWToolBox"))
     devtools::install_github("John-R-Wallace/R-ToolBox")

# ***** To get years added to the residual plot do this until pulled to Kelli's verion ***    
# JRWToolBox::lib("John-R-Wallace-NOAA/FishStatsUtils")

# ***** Once Kelly accepts my fork, do this until pulled to Thorson's verion ***          
# JRWToolBox::lib("kellijohnson-NOAA/FishStatsUtils")
 
# With the INSTALL_opts argument, warning messasges given when SHA number has not changed since last install.
if (!any(installed.packages()[, 1] %in% "FishStatsUtils"))
    devtools::install_github("james-thorson-noaa/FishStatsUtils", INSTALL_opts = "--no-multiarch --no-test-load")   
       
if (!any(installed.packages()[, 1] %in% "VAST"))
    devtools::install_github("james-thorson-noaa/VAST", INSTALL_opts = "--no-multiarch --no-test-load --no-staged-install")

if (!any(installed.packages()[, 1] %in% "pander"))
     install.packages("pander")

if (!any(installed.packages()[, 1] %in% "rnaturalearthdata"))
    install.packages("rnaturalearthdata")


require(TMB)
require(VAST)
# require(JRWToolBox)  # This code should work without the need to attach the JRWToolBox package.

# Extract species data from the Warehouse
Data_Set <- JRWToolBox::dataWareHouseTrawlCatch(spFormalName, yearRange = c(2003, 2018), project = 'WCGBTS.Combo')

# Look at the data by year and pass - showing 'NA's if any via JRWToolBox::Table function.
JRWToolBox::Table(Data_Set$Year, Data_Set$Pass)

# Versions of VAST you can use:
list.files(R.home(file.path("library", "VAST", "executables")))
# This gives the latest version available. (Up to v10_0_0 - then broken.)
# (Version <- substr(list.files(R.home(file.path("library", "VAST", "executables")))[length(list.files(R.home(file.path("library", "VAST", "executables"))))], 1, 11))
# Version 5+ gives a internal compiler error: Segmentation fault as of 21 Nov 2018
Version <- "VAST_v8_5_0"  

# # define the spatial resolution for the model, and whether to use a grid or mesh approximation
# # mesh is default recommendation, number of knots need to be specified
# # do not modify Kmeans setup
# Method = c("Grid", "Mesh", "Spherical_mesh")[2]
# grid_size_km = 25     # Value only matters if Method="Grid"
n_x <- 200  # Number of "knots" used when Method="Mesh"
# Kmeans_Config = list( "randomseed"=1, "nstart"=100, "iter.max"=1e3 )   # Controls K-means algorithm to define location of knots when Method="Mesh"
# 
# Model settings

# define whether to include spatial and spatio-temporal variation, whether its autocorrelated, and whether there's overdispersion
# field config - for both model components
# Omega- spatial variation
# Epsilon - temporal spatial variation
# review these settings
# if all field config settings are zero it is a fixed effects model
# RhoConfig - autocorrelation across time: defaults to zero, both annual intercepts (beta) and spatio-temporal (epsilon)
# OverdispersionConfig, vessel effects for both components of the model?
# settings can be on or off; 0,1
# obs model - distribution for errors and which model to run (e.g. default is delta model with standard link functions)
# ObsModel = c(2,0)
# FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1)
# RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0)
# OverdispersionConfig = c(Delta1 = 1, Delta2 = 1)  # Turn on vessel-year effects for both components if using WCGBTS


# outputs calculated after model runs, essentially reports to create
# Options = c(SD_site_density = 0, SD_site_logdensity = 0, Calculate_Range = 0, Calculate_evenness = 0, Calculate_effective_area = 0,  Calculate_Cov_SE = 0,
#             Calculate_Synchrony = 0, Calculate_Coherence = 0, Calculate_Range = 1, Calculate_effective_area = 1)

# strata limits, run model but then calculate area specific indices
  (strata.limits <- data.frame(
    STRATA = c("Coastwide","CA","OR","WA"),
    north_border = c(49.0, 42.0, 46.0, 49.0),
    south_border = c(32.0, 32.0, 42.0, 46.0),
    shallow_border = c(55, 55, 55, 55),
    deep_border = c(1280, 1280, 1280, 1280)
    ))

setwd(HomeDir)  # Make sure that the working directory is back where it started

#region that tells software which grid to use
Region = "California_current"

#save files setting

# DateFile = paste0(getwd(),'/VAST_output/')  # Simple, but requires manually changing the directory to save different runs
(DateFile <- paste0(getwd(),'/VAST_output_', Sys.Date(), '_', spLongName, '_nx=', n_x, '/')) # Change '_nx=' for different runs, e.g. '_Pass_nx=' for including pass
if(!dir.exists(DateFile)) dir.create(DateFile)

#save all settings
# Record = ThorsonUtilities::bundlelist( c("Data_Set","Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
# save( Record, file=file.path(DateFile,"Record.RData"))
# capture.output( Record, file=paste0(DateFile,"Record.txt"))

#set up data frame from data set
#creates data geostat...need this data format
# Vessel has a unique value for each boat-licence and calendar year (i.e., it's a "Vessel-Year" effect)
Data_Geostat = data.frame(Catch_KG = Data_Set$Total_sp_wt_kg, Year = Data_Set$Year, Vessel = paste(Data_Set$Vessel, Data_Set$Year,sep="_"),
             AreaSwept_km2 = Data_Set$Area_Swept_ha/100, Lat =Data_Set$Latitude_dd, Lon = Data_Set$Longitude_dd, Pass = Data_Set$Pass - 1)

#see data format
head(Data_Geostat)

# Remove rows with missing values
Data_Geostat = na.omit(Data_Geostat)


# shows data being used, read this document
pander::pandoc.table(Data_Geostat[1:6,], digits=3)


# https://docs.google.com/document/d/1pl3-q8zlSBqTmPNaSHJU67S_hwN5nok_I9LAr-Klyrw/edit

# FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1) #  where Omega refers to spatial variation, Epsilon refers to spatio-temporal variation, Omega1 refers to variation in encounter probability, 
#    and Omega2 refers to variation in positive catch rates, where 0 is off, "AR1" is an AR1 process, and >0 is the number of elements in a factor-analysis covariance. 

# RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0)  # autocorrelation across time: defaults to zero, both annual intercepts (beta) and spatio-temporal (epsilon)

# OverdispersionConfig = c(Delta1 = 1, Delta2 = 1) # Turn on vessel-year effects for both components if using WCGBTS
settings <- make_settings( n_x = n_x, fine_scale = TRUE, ObsModel = c(2, 1), FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1), RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), 
                  OverdispersionConfig = c(Delta1 = 1, Delta2 = 1), Region = Region, purpose = "index", strata.limits = strata.limits, bias.correct = FALSE )  

# Run model

# c_i = category
# b_i = biomass/ cpue input
# a_i= effort/area covered
# v_i= vessel ID
# s_i= knot locations
# t_i= time

sink(paste0(DateFile, "Fit_Output.txt"))

#  Without Pass 
fit <- fit_model( settings = settings, Lat_i = Data_Geostat$Lat, Lon_i = Data_Geostat$Lon, t_i = Data_Geostat$Year, working_dir = DateFile, test_fit = TRUE,
                  c_i = rep(0, nrow(Data_Geostat)), b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, v_i = Data_Geostat$Vessel, newtonsteps = 0, run_model = TRUE)

# With Pass - Pass not working with fine_scale = TRUE
# fit <- fit_model( settings = settings, Lat_i = Data_Geostat$Lat, Lon_i = Data_Geostat$Lon, t_i = Data_Geostat$Year, working_dir = DateFile, test_fit = TRUE,
#                  c_i = rep(0, nrow(Data_Geostat)), b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, v_i = Data_Geostat$Vessel, 
#                  Q_ik = matrix(Data_Geostat$Pass, ncol = 1), newtonsteps = 0, run_model = TRUE)

sink()


# Create 'parameter_estimates.txt' without scientific notation
Opt <- fit$parameter_estimates
OptRnd <- list()
OptRnd$par <- JRWToolBox::r(Opt$par)
OptRnd$diagnostics <- JRWToolBox::r(Opt$diagnostics)
OptRnd$SD <- JRWToolBox::r(summary(Opt$SD, "fixed"), 6) 
OptRnd$Maximum_gradient_component <- Opt$max_gradient
OptRnd$pdHess <- Opt$SD$pdHess
OptRnd$Convergence_check <- ifelse(Opt$SD$pdHess,  { ifelse(Opt$max_gradient < 0.0001, "There is no evidence that the model is not converged", 
                 "The model is likely not converged (the critera is a pd Hess and the max_gradient < 0.0001)") }, "The model is definitely not converged")
# print(OptRnd) No need for print() - use 'split = TRUE' below
capture.output(OptRnd, file = file.path(DateFile, "parameter_estimates.txt"), split = TRUE)


# Optimization result- including the test of the range of Raw1 and Raw2 should be inside of min and max distance of between knot locations
sink(paste0(DateFile, "Final_Convergence_Results.txt"), split = TRUE)
   cat("\nMaximum_gradient_component:", Opt$max_gradient, "\n\nnlminb() convergence:", OptRnd$Convergence_check, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
   cat("\nRange Raw1 and Raw2 should be inside of min and max distance of between knot locations\n\n")
   # Range Raw1 and Raw2 should be inside of min and max distance of between knot locations (J. Thorson, pers. comm.)
   print(r(sort(c(Range_raw1 = fit$Report$Range_raw1, Range_raw2 = fit$Report$Range_raw2, minDist = min(dist(fit$spatial_list$loc_x )), maxDist = max(dist(fit$spatial_list$loc_x ))))))
sink() 


summaryNWFSC( obj = fit$tmb_list$Obj, savedir = DateFile )

fit$parameter_estimates$diagnostics

# Check convergence via gradient (should be TRUE)
all( abs(fit$parameter_estimates$diagnostics[,'final_gradient']) < 1e-2 )

max(fit$parameter_estimates$diagnostics[,'final_gradient'])



# Check convergence via Hessian (should be TRUE)
all( eigen(fit$parameter_estimates$SD$cov.fixed)$values > 0 )

cat("\nMax Gradient =", fit$parameter_estimates$max_gradient, "\n\n")
cat("\nAIC =", fit$parameter_estimates$AIC, "\n\n")

# Plot results # plot.fit_model()
plot_list <- plot( fit, what = c('results', 'extrapolation_grid', 'spatial_mesh')[1], working_dir = DateFile)


setwd(HomeDir)


# MapDetails_List = FishStatsUtils::make_map_info( Region = Region, Extrapolation_List = fit$extrapolation_list, spatial_list = fit$spatial_list, 
#            NN_Extrap = fit$spatial_list$PolygonList$NN_Extrap) 

# Yearly results figure [ using YearlyResultsFigure_VAST3X() ]
    # 1. SpResults <spShortName>.png: Yearly results in a single plot; hexagon shapes (not circles) are used. The biomass index is also included.

(Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year']))) # Default arg for YearlyResultsFigure_VAST3X
(Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))) # Default arg for YearlyResultsFigure_VAST3X

SP.Results.Dpth <- JRWToolBox::YearlyResultsFigure_VAST3X(fit = fit, map_list = plot_list$map_list, Graph.Dev = 'png')  # This function looks for 'spShortName' (defined above)


# Save it all in Image.RData
save(list = names(.GlobalEnv), file = paste0(DateFile, "Image.RData"))




# =======================================================================

if(F) {

# Notes
 plot(fit1$spatial_list$MeshList$isotropic_mesh)


# For knots method = 'samples' is default, but other 'grid' method is prefered  # !!!!! this is not the same as old mesh grid options !!!!!

# check out "getprecision"?

# Good wiki examples to follow in VAST on GitHub


# Estimate_metric_tons by year figures for FS (fine_scale) and not FS compares well






# 2018 Lingcod in SP.Results.Dpth.FS has the highest 15 values, but the 2018 raw data only has 2nd highest value and the lower values
# means and medians look reasonable however
# Will need to stack SP.Results.Dpth.FS  to show comparable plots

SP.Results.Dpth.FS <- YearlyResultsFigure_V3.5(Report = fit$Report, fit. = fit, map_list. = plot_list$map_list, Graph.Dev = 'png') 

apply(exp(SP.Results.Dpth.FS[,-(1:2)])/10, 2, max)  # divide by 10 converts grams per sq mile to kg per hectare  (100/1000)
apply(exp(SP.Results.Dpth.FS[,-(1:2)])/10, 2, mean)
apply(exp(SP.Results.Dpth.FS[,-(1:2)])/10, 2, median)


Data_Set <- JRWToolBox::dataWareHouseTrawlCatch(spFormalName, yearRange = c(2003, 2018), project = 'WCGBTS.Combo')
change(Data_Set)

# 2006
rev(sort(exp(SP.Results.Dpth.FS[,6])/10))[1:20]
rev(sort((Total_sp_wt_kg/Area_Swept_ha)[Year %in% 2006]))[1:20]
 
 
# 2018
rev(sort(exp(SP.Results.Dpth.FS[,18])/10))[1:20]
rev(sort((Total_sp_wt_kg/Area_Swept_ha)[Year %in% 2018]))[1:20]


}




