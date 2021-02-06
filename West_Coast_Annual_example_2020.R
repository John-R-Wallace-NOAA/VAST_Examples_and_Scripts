
# Download into your working directory with:
JRWToolBox::gitAFile("John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/West_Coast_Annual_example_2020.R", "script", File = "West_Coast_Annual_example_2020.R", show = FALSE)
# or edit with [using a properly configured gitEdit()]
JRWToolBox::gitEdit(West_Coast_Annual_example_2020, "John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/")


# Test run of single species spatial delta glmm
# Test, canary data; implementation, Lingcod groundfish survey data
# Based on single-species example
# Revised by M. Haltuch, Feb 2017
# Revised by J. Wallace Mar 2017
# Revised by James Thorson April 2017
# Revised by J. Wallace Apr 2017
# Revised by J. Wallace Dec 2018
# Revised by J. Wallace FEb 2020

# =============================================

# VAST will often leave you in the subdirectory of the current run. Using HomeDir helps get you back where you started.
# Only do this once per R session, after you are in the your main working directory:

(HomeDir <- getwd())

# =============================================


summaryNWFSC <- function( obj = Obj, sdreport = Opt$SD, savedir=NULL ) {

    # Based on James Thorson's summary_nwfsc(), circa 2017
    # Revised by John Wallace Dec 2018

    f = function(num,threshold=0.000001) ifelse(num<threshold,paste0("< ",threshold),num)
    # Table of settings
    TableA = data.frame( "Setting_name"=rep(NA,9), "Setting_used"=NA )
    TableA[1,] = c("Number of knots", obj$env$data$n_x)
    TableA[2,] = c("Maximum gradient", formatC(f(max(abs( obj$gr(TMB::summary.sdreport(sdreport,"fixed")[,'Estimate'])))),format="f",digits=6) )
    TableA[3,] = c("Is hessian positive definite?", switch(as.character(sdreport$pdHess),"FALSE"="No","TRUE"="Yes") )
    TableA[4,] = c("Was bias correction used?", ifelse("Est. (bias.correct)"%in%colnames(TMB::summary.sdreport(sdreport)),"Yes","No") )
    TableA[5,] = c("Distribution for measurement errors", switch(as.character(obj$env$data$ObsModel[1]),"1"="Lognormal","2"="Gamma") )
    TableA[6,] = c("Spatial effect for encounter probability", switch(as.character(obj$env$data$FieldConfig[1]),"-1"="No","1"="Yes") )
    TableA[7,] = c("Spatio-temporal effect for encounter probability", switch(as.character(obj$env$data$FieldConfig[2]),"-1"="No","1"="Yes") )
    TableA[8,] = c("Spatial effect for positive catch rate", switch(as.character(obj$env$data$FieldConfig[3]),"-1"="No","1"="Yes") )
    TableA[9,] = c("Spatio-temporal effect for positive catch rate", switch(as.character(obj$env$data$FieldConfig[4]),"-1"="No","1"="Yes") )
    
    # Print number of parameters
    # TableB = FishStatsUtils::list_parameters( obj, verbose = FALSE )
    TableB = list_parameters( obj, verbose = FALSE )
    
    # Print table of MLE of fixed effects
    TableC = JRWToolBox::renum(cbind(Param = Opt$diagnostics[, 1], TMB::summary.sdreport( Opt$SD, "fixed" ), Opt$diagnostics[, -1]))
        
    # Return
    Return = list("TableA"=TableA, "TableB"=TableB, "TableC"=TableC)
    if( !is.null(savedir)) for(i in 1:3) write.csv(Return[[i]], file=paste0(savedir,"/",names(Return)[i],".csv"), row.names = FALSE)
    cat("\n")
    Return
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
   spFormalName <- 'canary rockfish' 
   spLongName <- 'Canary rockfish'
   spShortName <- 'CNRY'

# Sablefish
# spFormalName <- 'sablefish' 
# spLongName <- 'Sablefish'
# spShortName <- 'SABL'
# 
if (!any(installed.packages()[, 1] %in% "devtools")) 
        install.packages("devtools")

if (!any(installed.packages()[, 1] %in% "JRWToolBox"))
     remotes::install_github("John-R-Wallace/R-ToolBox")

     
# ***** To get years added to the residual plot do this until pulled to Kelli's verion ***    
# JRWToolBox::lib("John-R-Wallace-NOAA/FishStatsUtils")

# ***** Once Kelly accepts my fork, do this until pulled to Thorson's verion ***          
# JRWToolBox::lib("kellijohnson-NOAA/FishStatsUtils")
 
#  A warning messasge will be given when SHA number has not changed since last install.
#  For CRAN packages, use 'updateCRAN = TRUE' to attempt an update of the package from CRAN.


JRWToolBox::lib("pander")

JRWToolBox::lib("rnaturalearthdata")

JRWToolBox::lib("kaskr/TMB_contrib_R/TMBhelper") 

JRWToolBox::lib("kaskr/adcomp/TMB")   

JRWToolBox::lib("james-thorson-noaa/FishStatsUtils", INSTALL_opts = "--no-multiarch --no-test-load")   
 
JRWToolBox::lib("james-thorson-noaa/VAST", INSTALL_opts = "--no-multiarch --no-test-load --no-staged-install")


# Extract species data from the Warehouse
Data_Set <- JRWToolBox::dataWareHouseTrawlCatch(spFormalName, yearRange = c(1900, 5000), project = 'WCGBTS.Combo')

# Look at the data by year and pass - showing 'NA's if any via JRWToolBox::Table function.
JRWToolBox::Table(Data_Set$Year, Data_Set$Pass)

# Versions of VAST available:
vastVer <- list.files(R.home(file.path("library", "VAST", "executables")))
print(vastVer[order(as.numeric(substring(JRWToolBox::get.subs(vastVer, sep = '_')[2, ], 2)))], quote = FALSE)
# Version 5+ gives a internal compiler error: Segmentation fault as of 21 Nov 2018
Version <- "VAST_v8_5_0"  

#define the spatial resolution for the model, and whether to use a grid or mesh approximation
#mesh is default recommendation, number of knots need to be specified
#do not modify Kmeans setup
Method = c("Grid", "Mesh", "Spherical_mesh")[2]
grid_size_km = 25     # Value only matters if Method="Grid"
n_x = 500  # Number of "knots" used when Method="Mesh"
Kmeans_Config = list( randomseed = 1,  nstart = 100, iter.max = 1e3 )   # Controls K-means algorithm to define location of knots when Method="Mesh"

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
FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1)
RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0)
OverdispersionConfig = c(Delta1 = 1, Delta2 = 1)  # Turn on vessel-year effects for both components if using WCGBTS
ObsModel = c(2,0)  # Gamma Errors

# outputs calculated after model runs, essentially reports to create
Options = c(SD_site_density = 0, SD_site_logdensity = 0, Calculate_Range = 0, Calculate_evenness = 0, Calculate_effective_area = 0,  Calculate_Cov_SE = 0,
             Calculate_Synchrony = 0, Calculate_Coherence = 0, Calculate_Range = 1, Calculate_effective_area = 1)



# strata limits, run model but then calculate area specific indices
  (strata.limits <- data.frame(
    STRATA = c("Coastwide","CA","OR","WA"),
    north_border = c(49.0, 42.0, 46.0, 49.0),
    south_border = c(32.0, 32.0, 42.0, 46.0),
    shallow_border = c(55, 55, 55, 55),
    deep_border = c(1280, 1280, 1280, 1280)
    ))

setwd(HomeDir); getwd()  # Make sure that the working directory is back where it started

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
             AreaSwept_km2 = Data_Set$Area_Swept_ha/100, Lat =Data_Set$Latitude_dd, Lon = Data_Set$Longitude_dd, Pass = Data_Set$Pass - 1.5)

#see data format
head(Data_Geostat)

# Remove rows with missing values
Data_Geostat = na.omit(Data_Geostat)

# shows data being used, read this document
pander::pandoc.table(Data_Geostat[1:6,], digits=3)

#extrapolation grid
Extrapolation_List = FishStatsUtils::Prepare_WCGBTS_Extrapolation_Data_Fn(strata.limits = strata.limits)

#derived objects for spatio-temporal estiamtion
Spatial_List = FishStatsUtils::make_spatial_info(grid_size_km = grid_size_km, n_x = n_x, Method = Method, Lon = Data_Geostat[,'Lon'], Lat = Data_Geostat[,'Lat'],
                         Extrapolation_List = Extrapolation_List, randomseed = Kmeans_Config[["randomseed"]], nstart = Kmeans_Config[["nstart"]], iter.max = Kmeans_Config[["iter.max"]],
                         DirPath = DateFile, Save_Results = FALSE)

# Add knots to Data_Geostat
Data_Geostat$knot_i <- Spatial_List$knot_i
head(Data_Geostat)

# Build the model, this is where you could specify new covariates using Data_Fn...read more on this
# No Pass included 
if(TRUE){
  TmbData = VAST::make_data(Version = Version, FieldConfig = FieldConfig, spatial_list = Spatial_List, OverdispersionConfig = OverdispersionConfig, RhoConfig = RhoConfig, ObsModel = ObsModel,
                   c_i = rep(0,nrow(Data_Geostat)), b_i = Data_Geostat[,'Catch_KG'], a_i = Data_Geostat[,'AreaSwept_km2'], v_i = Data_Geostat$Vessel,
                   s_i = Data_Geostat[,'knot_i']-1, t_i = Data_Geostat[,'Year'], a_xl = Spatial_List$a_xl, MeshList = Spatial_List$MeshList, GridList = Spatial_List$GridList,
                   Method = Spatial_List$Method, Options = Options)
}

# Include pass as a catchability covariate
if(FALSE) {  
Q_ik <- as.matrix(Data_Geostat[, 'Pass', drop=F])
TmbData = VAST::make_data(Version = Version, FieldConfig = FieldConfig, spatial_list = Spatial_List, OverdispersionConfig = OverdispersionConfig, RhoConfig = RhoConfig, ObsModel = ObsModel,
                    c_i = rep(0,nrow(Data_Geostat)), b_i = Data_Geostat[,'Catch_KG'], a_i = Data_Geostat[,'AreaSwept_km2'], v_i = Data_Geostat$Vessel,
                    s_i = Data_Geostat[,'knot_i']-1, t_i = Data_Geostat[,'Year'], a_xl = Spatial_List$a_xl, Q_ik = Q_ik, MeshList = Spatial_List$MeshList, GridList = Spatial_List$GridList,
                    Method = Spatial_List$Method, Options = Options)
}


###################
# Do the estimation
###################

if(.Platform$OS.type == "windows" ) { 

   # Create a local 'Makevars' file without the all warnings, '-Wall', flag. This fixes the RcppEigen excess warnings issue. 
   V <- Version
   xF <- ""  # Add additional flags
   Ri <- paste0(R.home(), '/include')
   Ti <- system.file("include", package = "TMB")
   Arch <- paste0(R.home(), '/bin', Sys.getenv("R_ARCH"))
       
   sink(paste0(DateFile, "Makevars"))
      cat(paste0('\n\n', V, '.o: ', V, '.cpp\n'))
      cat(paste0('\tC:/rtools40/mingw64/bin/g++ -std=gnu++11 -I"', Ri, '" -DNDEBUG -I"', Ti, '" -DTMB_SAFEBOUNDS -DLIB_UNLOAD=R_unload_', V, 
                 ' -DTMB_LIB_INIT=R_init_', V, ' -O2 -mfpmath=sse -msse2 -mstackrealign -c ', xF, ' ', V, '.cpp -o ', V, '.o\n'))
      cat(paste0('\tC:/rtools40/mingw64/bin/g++ -std=gnu++11 -shared -s -static-libgcc -o ', V, '.dll ',  V, '.o -L', Arch,' -lR\n')) 
   sink()
}

# Build tmb object
TmbList = VAST::make_model(TmbData = TmbData, RunDir = DateFile, Version = Version, RhoConfig = RhoConfig, loc_x = Spatial_List$loc_x, Method = Method)
Obj = TmbList[["Obj"]]

# Run nlminb() optimizer with Newton steps to improve convergence
Opt = TMBhelper::fit_tmb(obj = Obj, lower = TmbList[["Lower"]], upper = TmbList[["Upper"]], getsd = TRUE, newtonsteps = 2, savedir = DateFile, bias.correct = TRUE )

# Create the reports
Report = Obj$report()
summaryNWFSC( savedir = DateFile )


# Save everything in object "Save" so that if you load it again, you can attach Save or not,
  # and know you haven't polluted your workspace
# Save = list("Opt"=Opt, "Report"=Report, "ParHat"=Obj$env$parList(Opt$par), "TmbData"=TmbData)
# save(Save, file=paste0(DateFile,"Save.RData"))

# Check convergence via gradient (should be TRUE)
all( abs(Opt$diagnostics[,'final_gradient']) < 1e-6 )
# Check convergence via Hessian (should be TRUE)
all( eigen(Opt$SD$cov.fixed)$values > 0 )

setwd(HomeDir)

################
# Model output (some of the diagnostic plots are slow, so do the model ouptut first)
################

# Decide which years to plot
(Year_Set = seq(min(Data_Geostat[,'Year']),max(Data_Geostat[,'Year'])))
(Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year']))))
#Years2Include = which( Year_Set %in% sort(unique(Data_Geostat[,'Year'])))[-c(3:5)]

# Get region-specific settings for plots
MapDetails_List = FishStatsUtils::make_map_info( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, spatial_list = Spatial_List, Extrapolation_List = Extrapolation_List )

#Plot Anisotropy
FishStatsUtils::plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData )

# Annual density surface, use plot_set = 3 to start and then do plot_set=c(1:9) to see more output on a good working model
FishStatsUtils::plot_maps(plot_set=3, MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]],
        MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile, Year_Set=Year_Set,
        Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]],
        mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8, plot_legend_fig=FALSE)

# Index of abundance
Index <- FishStatsUtils::plot_biomass_index( DirName=DateFile, TmbData=TmbData, Sdreport=Opt[["SD"]], Year_Set=Year_Set, Years2Include=Years2Include,
  strata_names=strata.limits[,1], use_biascorr=TRUE )
pander::pandoc.table( Index$Table[,c("Year","Fleet","Estimate_metric_tons","SD_log","SD_mt")] )

# Center of gravity / range expansion
 FishStatsUtils::plot_range_index(Report=Report, TmbData=TmbData, Sdreport=Opt[["SD"]], Znames=colnames(TmbData$Z_xm), 
            PlotDir=DateFile, Year_Set=Year_Set)


################
# Make diagnostic plots
################

FishStatsUtils::plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=Data_Geostat, PlotDir=DateFile )

#convergence
pander::pandoc.table( Opt$diagnostics[,c('Param','Lower','MLE','Upper','final_gradient')] )

# Plot encounter probability diagnostics p/a
Enc_prob <- FishStatsUtils::plot_encounter_diagnostic( Report=Report, Data_Geostat=Data_Geostat, DirName=DateFile)

# QQ plot
Q <- FishStatsUtils::plot_quantile_diagnostic( TmbData=TmbData, Report=Report, DateFile = DateFile, FileName_PP="Posterior_Predictive.jpg", FileName_Phist = "Posterior_Predictive-Histogram.jpg",
                             FileName_QQ = "Q-Q_plot.jpg", FileName_Qhist = "Q-Q_hist.jpg")


# Residuals

FishStatsUtils::plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], spatial_list = Spatial_List, extrapolation_list = Extrapolation_List, TmbData=TmbData, 
           Report=Report, Q=Q, savedir=DateFile, MappingDetails=MapDetails_List[["MappingDetails"]], PlotDF=MapDetails_List[["PlotDF"]], 
           MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=DateFile,
           Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], Cex=MapDetails_List[["Cex"]], Legend=MapDetails_List[["Legend"]],
           zone=MapDetails_List[["Zone"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1, maxpanel = 4)

# Histogram of quantiles...should be a flat line for well behaved model; also can use the Q-Q plot

# Model selection, see example code, just run one model for now

setwd(HomeDir)

# Yearly results figures
  # 1. Yearly_dens.png: color changes are within year - not across years.
  # 2. SpResults <spShortName>.png: Yearly results in a single plot; hexagon shapes (not circles) are used. The biomass index is also included.
JRWToolBox::YearlyResultsFigures(Graph.Dev = 'png') # This function looks for 'spShortName' (defined above)

# Save it all in Image.RData
save(list = names(.GlobalEnv), file = paste0(DateFile, "Image.RData"))

# =============================================

if(FALSE) {

     # Note that in a new R session, after reloading Image.Rdata:
     base::load("Image.RData")
     setwd(DateFile) 
    
     # More figures and tables can be created or updated when necessary, e.g.:
     JRWToolBox::YearlyResultsFigures()
    
     # TMB's dll can also be reloaded with: 
     dyn.load(paste0(DateFile, Version, ".dll")) # Look at all loaded dll's with getLoadedDLLs()
    
     # This allows calls such as these below to work again:
     Obj$fn()
     Obj$gr()
     summaryNWFSC(obj = Obj, sdreport = Opt$SD)
     summaryNWFSC(obj = Obj, sdreport = Opt$SD, savedir = DateFile)
     cbind(TMB::summary.sdreport(Opt$SD, "fixed"), Gradient = Obj$gr())  # cf. Opt$diagnostics or JRWToolBox::r(Opt$diagnostics, 9)
    
    
     # Note also that objects in the Objective Function's (Obj) environment can be listed with:
     ls(Obj, env=Obj$env)
    
     # And looked at with:
     get('hessian', env = Obj$env)
     get('last.par', env = Obj$env)[-get('random', env = Obj$env)] # Same as TMB::summary.sdreport(Opt$SD, "fixed")
     table(names(get('par', env = Obj$env)[get('random', env = Obj$env)])) # Same as table(row.names(TMB::summary.sdreport(Opt$SD, "random")))

}



