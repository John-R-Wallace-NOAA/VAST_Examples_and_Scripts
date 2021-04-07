
West_Coast_2021_V3.6.1 <- function(spFormalName = 'lingcod', spLongName = 'Lingcod', spShortName = 'LCOD', Survey = 'WCGBTS.Combo', Settings = NULL, 
              VAST_surveyName = NULL, yearRange = c(1000, 5000), strata.limits = NULL, Pass = grepl('WCGBTS', Survey), Old_QQ_and_Resid_Figures = TRUE, 
              overDispersion = if(grepl('WCGBTS', Survey)) c(eta1 = 0, eta2 = "AR1") else c(eta1 = 0, eta2 = 0), ObsModel. = c(2, 0), n_x. = 250, 
              fine_scale. = TRUE, bias.correct. = FALSE, depthCov = TRUE, formulaDepthSpline = TRUE, formulaDepth = FALSE, test_fit = TRUE, folderSuffix = NULL, Cores = 6, ...) {
  
   # # Download this function into your current environment:
   # repoPath <- "John-R-Wallace-NOAA/VAST_Examples_and_Scripts"
   # rgit::S(West_Coast_Example_2020_V3X, subDir = NULL, show = FALSE)  # fix(West_Coast_Example_2020_V3X) could then be used to edit
     
   # # Put directly into Notepad++ for editing with:
   # repoPath <- "John-R-Wallace-NOAA/VAST_Examples_and_Scripts"
   # rgit::gitEdit(West_Coast_Example_2020_V3X, subDir = NULL)
   # # (Edit the path or app within a copy of gitEdit() if needed.) 
   
   # *** The main body can be run as a script to better follow along and to keep files in .GlobalEnv" by un-commenting a species' names row below. Or create your own for a new species. ***
   # *** Also un-comment (and make changes if desired) the other arguments row. ***
   
   # Canary rockfish
   # spFormalName = 'canary rockfish'; spLongName = 'Canary rockfish';  spShortName = 'CNRY'
   
   # Lingcod
   # spFormalName = 'lingcod'; spLongName = 'Lingcod'; spShortName = 'LCOD'
  
   # Spiny dogs
   # spFormalName = 'Pacific spiny dogfish'; spLongName = 'Spiny dogfish'; spShortName = 'DSRK'

   # Other arguments
   # ObsModel. = c(2, 0); n_x. = 200; fine_scale. = TRUE; depthCov = TRUE;  formulaDepthSpline = TRUE; formulaDepth = FALSE
  
   
   # Test run of single species spatial delta glmm
   # Test, canary data; implementation, Lingcod groundfish survey data
   # Based on single-species example
   # Revised by M. Haltuch, Feb 2017
   # Revised by J. Wallace Mar 2017
   # Revised by James Thorson April 2017
   # Revised by J. Wallace Apr 2017
   # Revised by J. Wallace Dec 2018
   # Revised by J. Wallace Feb 2020: uses fine_scale = TRUE in VAST ver. 3X and JRWToolBox::YearlyResultsFigure_VAST3X(), following the upper level functions (wrappers) approach.
   # Revised by J. Wallace Aug 2020: made into a function with various arguments
   # Revised by J. Wallace Apr 2021: added the 'Old_QQ_and_Resid_Figures' argument and code (now called by the 'VAST_3.6.1_Survey_Indexes.R' script)
   
   # =============================================
   
   # VAST will often leave you in the subdirectory of the current run. Using HomeDir helps get you back where you started.
   # Only do this once per R session, after you are in the your main working directory:
   
   HomeDir <- getwd()
   options(repos=c(CRAN="https://cloud.r-project.org/", CRANextra = "http://lib.stat.cmu.edu/R/CRAN/"))
   
   # =========== Internal Functions ==================================
   
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
       TableA[5,] <- c("Distribution for measurement errors", switch(as.character(obj$env$data$ObsModel[1]),"1" = "Lognormal", "2" = "Gamma") )
       
       FieldConfig <- fit.$data_list$FieldConfig
       comment(FieldConfig) <- "\nExplanation of the above figure:\n\n                                Encounter Probability(1), Positive Catch Rates(2)\nSpatial Random Effects\nSpatiotemporal\n# of Factors for Intercepts\n\n0 = Off, 1 = On, +2 = additionl factors up to maximum number of categories in factor analysis covariance, IID = independent for each category\n\n"
   
       # Print number of parameters
       # TableB = FishStatsUtils:::list_parameters( obj, verbose = FALSE )
       TableB <- list_parameters( obj, verbose = FALSE )
       
       # Print table of MLE of fixed effects
       TableC <- r(renum(cbind(Param = Opt$diagnostics[, 1], TMB::summary.sdreport( Opt$SD, "fixed" ), Opt$diagnostics[, -1])))
           
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
   
   sourceFunctionURL <- function (URL) 
   {
      " # For more functionality, see gitAFile() in the rgit package ( https://github.com/John-R-Wallace-NOAA/rgit ) which includes gitPush() and git() "
      require(httr)
      File.ASCII <- tempfile()
      on.exit(file.remove(File.ASCII))
      getTMP <- httr::GET(URL)
      write(paste(readLines(textConnection(httr::content(getTMP))), 
          collapse = "\n"), File.ASCII)
      source(File.ASCII)
   }
   
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/dataWareHouseTrawlCatch.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/Table.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/r.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/renum.R")
   sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/JRWToolBox/master/R/YearlyResultsFigure_VAST3X.R")
   
   
   # =========== End Internal Functions ==============================
   
   
   # 'spFormalName', is a common name that needs to work with the Data Warehouse, i.e. only proper names capitalized.
   # 'spLongName' and 'spShortName' can be whatever is desired, the long name goes in the directory name and
   # the short name goes into the file name of the Yearly Results Figure.
   
      
   if (!any(installed.packages()[, 1] %in% "devtools")) 
           install.packages("devtools")
   
   # With the INSTALL_opts argument, warning messasges given when SHA number has not changed since last install.
   if (!any(installed.packages()[, 1] %in% "FishStatsUtils"))
       devtools::install_github("james-thorson-noaa/FishStatsUtils", INSTALL_opts = "--no-multiarch --no-test-load")   
          
   if (!any(installed.packages()[, 1] %in% "VAST"))
       devtools::install_github("james-thorson-noaa/VAST", INSTALL_opts = "--no-multiarch --no-test-load --no-staged-install")
   
   if (!any(installed.packages()[, 1] %in% "pander"))
        install.packages("pander")
   
   if (!any(installed.packages()[, 1] %in% "rnaturalearthdata"))
       install.packages("rnaturalearthdata")
   	
   if (!any(installed.packages()[, 1] %in% "splines"))
       install.packages("splines")	
   
   # Multi-threading
      # R_OPEN (Microsoft's MRO/MRAN) thread control
      if('RevoUtilsMath' %in% installed.packages()[, 'Package']) {
         RevoUtilsMath::setMKLthreads(Cores)
         cat("\nNumber of R_OPEN cores set to:", RevoUtilsMath::getMKLthreads(), "\n\n")
      }
      
      # R_MKL (Intel's Math Kernel library) thread control
      if('RhpcBLASctl' %in% installed.packages()[, 'Package']) {
         RhpcBLASctl::blas_set_num_threads(Cores)
         cat("\nNumber of R_MKL cores set to:", RhpcBLASctl::blas_get_num_procs(), "\n\n")
      }
   
   require(TMB)
   require(VAST)
   
   assign('Survey', Survey, pos = 1)
   
   # Extract species data from the Warehouse
   Data_Set <- dataWareHouseTrawlCatch(spFormalName, yearRange = yearRange, project = Survey)
   
   # Look at the data by year and pass - showing 'NA's if any via JRWToolBox::Table function.
   if(Pass) {
     Table(Data_Set$Year, Data_Set$Pass)
   } else {
     Table(Data_Set$Year)
     Data_Set$Pass <- NULL
   }  
  
   
   # Versions of VAST you can use
   # VAST ver 3.6.1 does not work with any C++ code earlier then VAST_v12_0_0.cpp
   print(list.files(R.home(file.path("library", "VAST", "executables"))))
   print(Version <- FishStatsUtils::get_latest_version(package="VAST"))
   # if(as.numeric(substr(packageVersion('VAST'), 1, 3)) == 3.3)  Version <- "VAST_v8_5_0.cpp"  # Do this for a lower cpp version with a VAST package version
   # if(as.numeric(substr(packageVersion('VAST'), 1, 3)) == 3.5)  Version <- "VAST_v9_4_0.cpp"
   
   # # define the spatial resolution for the model, and whether to use a grid or mesh approximation
   # # mesh is default recommendation, number of knots need to be specified
   # # do not modify Kmeans setup
   # Method = c("Grid", "Mesh", "Spherical_mesh")[2]
   # grid_size_km = 25     # Value only matters if Method="Grid"
   # n_x is the number of "knots" used when Method="Mesh"
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
    
   if(is.null(Settings)) 
       print(strata.limits <- data.frame(
         STRATA = c("Coastwide","CA","OR","WA"),
         north_border = c(49.0, 42.0, 46.0, 49.0),
         south_border = c(32.0, 32.0, 42.0, 46.0),
         shallow_border = c(55, 55, 55, 55),
         deep_border = c(1280, 1280, 1280, 1280)
       ))
   
   setwd(HomeDir)  # Make sure that the working directory is back where it started
   
   # Region that tells software which grid to use
   Region = "California_current"
   
   # VAST's surveyname's in the 'California_current' region: propInCCA, propInWCGBTS, propInTriennial, propInSlope98_00, propInSlope01, propInSlope02
   # [ See the table: head(FishStatsUtils:::Prepare_WCGBTS_Extrapolation_Data_Fn()$Data_Extrap) ]
   if(is.null(VAST_surveyName)) {
     
     if(Survey == 'AFSC.Shelf') VAST_surveyName <- 'propInTriennial' # Triennial
     if(Survey == 'AFSC.Slope') VAST_surveyName <- 'propInSlope98_00'  # Tested as best by looking at AFSC.Slope data overlayed over all 'surveyname's.
     if(Survey == 'WCGBTS.Combo') VAST_surveyName <- 'propInWCGBTS'
     if(Survey == 'WCGBTS.Shelf') VAST_surveyName <- 'propInSlope98_00'  # Assumed correct - not tested
   }   
   
   # https://docs.google.com/document/d/1pl3-q8zlSBqTmPNaSHJU67S_hwN5nok_I9LAr-Klyrw/edit
   
   # FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1) #  where Omega refers to spatial variation, Epsilon refers to spatio-temporal variation, Omega1 refers to variation in encounter probability, 
   #    and Omega2 refers to variation in positive catch rates, where 0 is off, "AR1" is an AR1 process, and >0 is the number of elements in a factor-analysis covariance. 
   
   # RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0)  # autocorrelation across time: defaults to zero, both annual intercepts (beta) and spatio-temporal (epsilon)
   
   # OverdispersionConfig = c(Delta1 = 1, Delta2 = 1) # Turn on vessel-year effects for both components if using WCGBTS
   
   if(is.null(Settings)) {
       Settings <- make_settings(
                     Version = Version, 
                         n_x = n_x., 
                      Region = Region,             
                     purpose = if(as.numeric(substr(packageVersion('VAST'), 1, 3)) <= 3.3)  'index' else 'index2', 
                  fine_scale = fine_scale., 
               strata.limits = strata.limits,
                 FieldConfig = c(Omega1 = 'IID', Epsilon1 = 'IID', Omega2 = 'IID', Epsilon2 = 'IID'), 
                   RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), 
        OverdispersionConfig = overDispersion,  # Default: if(grepl('WCGBTS', Survey)) c(Delta1 = 1, Delta2 = 1) else c(eta1 = 0, eta2 = 0)
                     Options = c(SD_site_logdensity=FALSE, Calculate_Range=TRUE, Calculate_effective_area=TRUE, Calculate_Cov_SE=TRUE, Calculate_Synchrony=TRUE, Calculate_proportion=TRUE, treat_nonencounter_as_zero = TRUE),
              use_anisotropy = TRUE,
                    ObsModel = ObsModel., # 1 = Lognormal, 2 = Gamma
                bias.correct = bias.correct., 
                   max_cells = 3000,
                 knot_method = 'samples'
       )          
   }       
   
   # DateFile
   print(DateFile <- paste0(getwd(), '/V', packageDescription('VAST')$Version, '_', Sys.Date(), '_', Survey, '_', spLongName, ifelse(Pass, '_Pass', ''), '_nx=', Settings$n_x, "_Obs=", 
             paste(Settings$ObsModel, collapse = "."), ifelse(is.null(folderSuffix), "", '_'), folderSuffix, '/')) # Change '_nx=' for different runs, e.g. '_Pass_nx=' for including pass
   if(!dir.exists(DateFile)) dir.create(DateFile)
   
   
   
   #set up data frame from data set
   #creates data geostat...need this data format
   # Vessel has a unique value for each boat-licence and calendar year (i.e., it's a "Vessel-Year" effect)
   if(Pass) 
      Data_Geostat = data.frame(Year = Data_Set$Year, Vessel = paste(Data_Set$Vessel, Data_Set$Year,sep="_"), Lat = Data_Set$Latitude_dd, Lon = Data_Set$Longitude_dd, Depth_km = Data_Set$Depth_m/1000, 
                   Catch_KG = Data_Set$Total_sp_wt_kg, AreaSwept_km2 = Data_Set$Area_Swept_ha/100,  Pass = Data_Set$Pass - 1)
   else 
      Data_Geostat = data.frame(Year = Data_Set$Year, Vessel = paste(Data_Set$Vessel, Data_Set$Year,sep="_"), Lat = Data_Set$Latitude_dd, Lon = Data_Set$Longitude_dd, Depth_km = Data_Set$Depth_m/1000, 
                   Catch_KG = Data_Set$Total_sp_wt_kg, AreaSwept_km2 = Data_Set$Area_Swept_ha/100)   
   
   #see data format
   print(head(Data_Geostat))
   
   # Remove rows with missing values
   Data_Geostat = na.omit(Data_Geostat)
   
   # shows data being used, read this document
   pander::pandoc.table(Data_Geostat[1:6,], digits=3)
   
   
   # Run model
   
   # c_i = category
   # b_i = biomass/ cpue input
   # a_i= effort/area covered
   # v_i= vessel ID
   # s_i= knot locations
   # t_i= time
   
   
   sink(paste0(DateFile, "Fit_Output.txt"))
   
   #  #  Without Pass 
   #  # fit <- fit_model( settings = settings, Lat_i = Data_Geostat$Lat, Lon_i = Data_Geostat$Lon, t_i = Data_Geostat$Year, working_dir = DateFile, test_fit = TRUE,
   #  #                  c_i = rep(0, nrow(Data_Geostat)), b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, v_i = Data_Geostat$Vessel, newtonsteps = 0, 
   #  #                  knot_method = 'samples', getsd = TRUE, getJointPrecision = TRUE, run_model = TRUE)
   #  
   #  # With Pass - for Lingcod (at least), 'test_fit' needs to be FALSE for the model to finish - the extra parameters (lambda1_k, lambda2_k) both ended up with a small final gradient.
   #  # The model with Pass had a lower AIC (29,828.48) compared to without Pass, AIC = 29,835.95 .
   #  
   #  fit <- fit_model( settings = settings, Lat_i = Data_Geostat$Lat, Lon_i = Data_Geostat$Lon, t_i = Data_Geostat$Year, working_dir = DateFile, test_fit = FALSE,
   #                    c_i = rep(0, nrow(Data_Geostat)), b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, v_i = Data_Geostat$Vessel, 
   #                    Q_ik = matrix(Data_Geostat$Pass, ncol = 1), newtonsteps = 0, knot_method = 'samples', getsd = TRUE, getJointPrecision = TRUE, run_model = TRUE)
   
                   
   # ---- Using depth as a covariate - using only depth on sampled data, not on the extrapolation grid ---                 
   
   
   if(depthCov) {
     Covariate_Data <- Data_Geostat[, c("Year", "Lon", "Lat", "Depth_km")] 
     Covariate_Data$Year <- NA # A Year column with all NA's - not removed via NULL
     formula = ~0
     if(formulaDepthSpline)   
        formula = ~splines::bs( log(Depth_km), knots = 3, intercept = FALSE)
     if(formulaDepth)    
        formula = ~Depth_km
   } else {
     Covariate_Data <- NULL
     formula = ~0
   }   
   
   # Suppress RcppEigen warnings
   # sink(paste0(DateFile, 'suppressMakeVars.txt'))
   #   cat(paste0('CXXFLAGS = -Wno-ignored-attributes\n'))
   # sink()
   # on.exit(file.remove(paste0(DateFile, 'suppressMakeVars.txt')))
   # Sys.setenv(R_MAKEVARS_USER = "suppressMakeVars.txt")
   # on.exit(Sys.setenv(R_MAKEVARS_USER = ""), add = TRUE)
                
   if(Pass)
      fit <- FishStatsUtils::fit_model(settings = Settings, surveyname = VAST_surveyName, Lat_i = Data_Geostat$Lat, Lon_i = Data_Geostat$Lon, t_i = Data_Geostat$Year, working_dir = DateFile,
                     b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, c_iz = rep(0, nrow(Data_Geostat)), v_i = Data_Geostat$Vessel,
                     
                     Q_ik = matrix(Data_Geostat$Pass, ncol = 1), 
    #                catchability_data = matrix(Data_Geostat$Pass, ncol = 1), Q1_formula = ~Pass, Q2_formula = ~Pass,
    
                     covariate_data = Covariate_Data, X1_formula = formula, X2_formula = formula,
                     newtonsteps = 0, getsd = TRUE, getJointPrecision = TRUE, run_model = TRUE, test_fit = test_fit, ...)
   else
     fit <- FishStatsUtils::fit_model(settings = Settings, surveyname = VAST_surveyName, Lat_i = Data_Geostat$Lat, Lon_i = Data_Geostat$Lon, t_i = Data_Geostat$Year, working_dir = DateFile,
                     b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, c_iz = rep(0, nrow(Data_Geostat)), v_i = Data_Geostat$Vessel, 
                     covariate_data = Covariate_Data, X1_formula = formula, X2_formula = formula,
                     newtonsteps = 0, getsd = TRUE, getJointPrecision = TRUE, run_model = TRUE, test_fit = test_fit, ...)
                     
   sink() # End sinking to Fit_Output.txt
   
   
   # Create 'parameter_estimates.txt' without scientific notation
   Opt <- fit$parameter_estimates
   OptRnd <- list()
   OptRnd$par <- r(Opt$par)
   OptRnd$diagnostics <- r(Opt$diagnostics)
   OptRnd$SD <- r(summary(Opt$SD, "fixed"), 6) 
   OptRnd$Maximum_gradient_component <- Opt$max_gradient
   OptRnd$pdHess <- Opt$SD$pdHess
   OptRnd$Convergence_check <- ifelse(Opt$SD$pdHess,  { ifelse(Opt$max_gradient < 0.0001, "There is no evidence that the model is not converged", 
                    "The model is likely not converged (the critera is a pd Hess and the max_gradient < 0.0001)") }, "The model is definitely not converged")
   # print(OptRnd) # No need for this print() - use 'split = TRUE' below
   capture.output(OptRnd, file = file.path(DateFile, "parameter_estimates.txt"), split = TRUE)
   
   
   summaryNWFSC( obj = fit$tmb_list$Obj, savedir = DateFile ) # Creates Model_Summary.txt
   
   # Optimization result- including the test of the range of Raw1 and Raw2 should be inside of min and max distance of between knot locations
   sink(paste0(DateFile, "Model_Summary.txt"), append = TRUE, split = TRUE)
      cat("\n\nMaximum_gradient_component:", Opt$max_gradient, "\n\nnlminb() convergence:", OptRnd$Convergence_check, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
      cat("\nRange Raw1 and Raw2 should be inside of min and max distance of between knot locations\n\n")
      # Range Raw1 and Raw2 should be inside of min and max distance of between knot locations (J. Thorson, pers. comm.)
      print(r(sort(c(Range_raw1 = fit$Report$Range_raw1, Range_raw2 = fit$Report$Range_raw2, minDist = min(dist(fit$spatial_list$loc_x )), maxDist = max(dist(fit$spatial_list$loc_x ))))))
   sink() 
   
   #  Extra looks at convergence
   cat("\nParamater estimates diagnostics")
   print(fit$parameter_estimates$diagnostics)
   
   # Check convergence via gradient (should be TRUE)
   cat("\nAll gradients are less than 0.01:", try(all( abs(fit$parameter_estimates$diagnostics[,'final_gradient']) < 1e-2 )), "\n\n")
   
   # Check convergence via inverted Hessian (should be TRUE)
   cat("\nAll eigen vectors of the inverted Hessian are greater than zero:", try(all( eigen(fit$parameter_estimates$SD$cov.fixed)$values > 0 )), "\n\n")
   
   # h = optimHess(fit$parameter_estimates$par, fn = fit$tmb_list$Obj$fn, gr = fit$tmb_list$Obj$gr)
   # all( eigen(h)$values > 0 )
   
   # Max final gradient
   cat("\nMax Gradient =", fit$parameter_estimates$max_gradient, "\n\n")  # Max after abs(): max(abs(fit$parameter_estimates$diagnostics[,'final_gradient']))
   
   # AIC
   cat("\nAIC =", fit$parameter_estimates$AIC, "\n\n")
   
    # Early save to Image.RData [ When reloading, remember to dyn.load() the '.dll' e.g. dyn.load(paste0(DateFile, 'VAST_v9_2_0.dll')) ]
   save(list = c(ls(), names(.GlobalEnv)), file = paste0(DateFile, "Image.RData")) # Save files inside the function also!!!!!!
   
   # Plot results
   
   # **** Note that as of 26 Feb 2020 the help for plot.fit_model() claims that 'spatial_mesh' is an option for the arg 'what'.
   #    However, looking at the code, that option needs to be one of ("spatial_info", "inla_mesh").  ****
   # As of 25 Aug 2020, need to set "Calculate_Range" to FALSE for VAST ver 3.5 and above since '"ln_Index_ctl" %in% rownames(TMB::summary.sdreport(fit$parameter_estimates$SD))' is TRUE which breaks plot_range_edge()
   (fit$data_list$Options_list$Options["Calculate_Range"] <- if(as.numeric(substr(packageVersion('VAST'), 1, 3)) <= 3.4) TRUE else FALSE)
   
   try({
   
     plotOut <- plot( fit, what = c('results', 'extrapolation_grid', 'inla_mesh')[1], working_dir = DateFile) # Calls FishStatsUtils:::plot.fit_model() which calls FishStatsUtils::plot_results()
     
     png(paste0(DateFile, 'DHARMa_Q-Q_plot.png'), width = 1000, height = 1000)
     DHARMa::plotQQunif(plotOut$dharmaRes, testUniformity = FALSE, testOutliers = FALSE, testDispersion = FALSE)
     dev.off()
     
     png(paste0(DateFile, 'Extrapolation_grid.png'), width = 500, height = 750)
     plot( fit, what = c('results', 'extrapolation_grid', 'inla_mesh')[2], working_dir = DateFile)  # Calls FishStatsUtils:::plot.make_extrapolation_info
     
     png(paste0(DateFile, 'INLA_mesh.png'), width = 500, height = 750)
     plot( fit, what = c('results', 'extrapolation_grid', 'inla_mesh')[3], working_dir = DateFile, check_residuals = FALSE)  # Calls FishStatsUtils:::plot.make_spatial_info
     
     # Do plot_results() again by itself so strata_names are included [ doesn't get added properly in plot() ] 
     FishStatsUtils::plot_results(fit = fit, settings = fit$settings, plot_set = 3, strata_names = Settings$strata.limits$STRATA, working_dir = DateFile)
       
     
     # Likewise, do plot_range_index() (for Effective_Area.png) again by itself so strata_names are included [ doesn't get added properly in plot_results() ] 
     #      plot_range_index() also recreates 'center_of_gravity.png' with no change but the 'Date modified' on the file properties
     FishStatsUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), PlotDir = DateFile, 
                     Year_Set = fit$year_labels, Years2Include = fit$years_to_plot, use_biascorr = Settings$bias.correct, strata_names = Settings$strata.limits$STRATA)
                     
   })
   
   graphics.off()
   
   setwd(HomeDir)
   
   # MapDetails_List = FishStatsUtils::make_map_info( Region = Region, Extrapolation_List = fit$extrapolation_list, spatial_list = fit$spatial_list, 
   #            NN_Extrap = fit$spatial_list$PolygonList$NN_Extrap) 
   
   # Yearly results figure [ using YearlyResultsFigure_VAST3X() ]
       # 1. SpResults <spShortName>.png: Yearly results in a single plot; hexagon shapes (not circles) are used. The biomass index is also included.
   
   (Year_Set = unique(Data_Geostat$Year)) # Default arg for YearlyResultsFigure_VAST3X
   (Years2Include = which(min(Data_Geostat$Year, na.rm = TRUE):max(Data_Geostat$Year, na.rm = TRUE) %in% sort(unique(Data_Geostat$Year)))) # Default arg for YearlyResultsFigure_VAST3X
   
   # This function looks for 'spShortName' (defined above)
   try(YearlyResultsFigure_VAST3X(spShortName = spShortName, spLongName = spLongName, fit = fit, DateFile = DateFile, Region = Region, 
         Year_Set = Year_Set, Years2Include = Years2Include, strata.limits = Settings$strata.limits, Graph.Dev = 'png')) 
   
   if(Old_QQ_and_Resid_Figures) { try({
   
        # --- Create old style QQ plot - not the DHARMa version ---
        Method <- c("Grid", "Mesh", "Spherical_mesh")[2]
        grid_size_km <- 25     # Value only matters if Method="Grid"
        Kmeans_Config <- list(randomseed = 1,  nstart = 100, iter.max = 1e3) 
        
        Extrapolation_List <- FishStatsUtils::Prepare_WCGBTS_Extrapolation_Data_Fn(strata.limits = strata.limits)
        
        #  Options = c(SD_site_density = 0, SD_site_logdensity = 0, Calculate_Range = 0, Calculate_evenness = 0, Calculate_effective_area = 1,  Calculate_Cov_SE = 0,
        #           Calculate_Synchrony = 0, Calculate_Coherence = 0, Calculate_Range = 1)
        
        Options <- Settings$Options
        
        Spatial_List <- FishStatsUtils::make_spatial_info(grid_size_km = grid_size_km, n_x = n_x., Method = Method, Lon = Data_Geostat$Lon, Lat = Data_Geostat$Lat,
                              Extrapolation_List = Extrapolation_List, randomseed = Kmeans_Config[["randomseed"]], nstart = Kmeans_Config[["nstart"]], iter.max = Kmeans_Config[["iter.max"]],
                              DirPath = DateFile, Save_Results = FALSE)
        
        # Hack here - tell make_data() this is version 12 C++ code even if it is not
        TmbData <- VAST::make_data(Version = "VAST_v12_0_0", FieldConfig = c(Omega1 = 1, Epsilon1 = 1, Omega2 = 1, Epsilon2 = 1), spatial_list = Spatial_List, OverdispersionConfig = c(Delta1 = 1, Delta2 = 1), 
                        RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), ObsModel = ObsModel.,
                        c_i = rep(0, nrow(Data_Geostat)), b_i = Data_Geostat$Catch_KG, a_i = Data_Geostat$AreaSwept_km2, v_i = Data_Geostat$Vessel,
                        s_i = Data_Geostat$knot_i - 1, t_i = Data_Geostat$Year, a_xl = Spatial_List$a_xl, MeshList = Spatial_List$MeshList, GridList = Spatial_List$GridList,
                        Method = Spatial_List$Method, Options = Options)
                        
        plot.new() # Strangeness here - but it works to get out the old style QQ plot, also the 'FileName_Phist' argument name is applied to the Q-Q plot.
        Q <- FishStatsUtils::plot_quantile_diagnostic( TmbData = TmbData, Report = fit$Report, DateFile = DateFile, save_dir = DateFile, FileName_PP = "Posterior_Predictive", 
                    FileName_Phist = "Q-Q_plot", FileName_QQ = "Q-Q_plot", FileName_Qhist = "Q-Q_hist")
                                  
        # --- Old style residuals with little data visible - look closely at the figures ---
        MapDetails_List <- FishStatsUtils::make_map_info( Region = Region, NN_Extrap = Spatial_List$PolygonList$NN_Extrap, spatial_list = Spatial_List, Extrapolation_List = Extrapolation_List )
        
        FishStatsUtils::plot_residuals(Lat_i = Data_Geostat[,'Lat'], Lon_i = Data_Geostat[,'Lon'], TmbData = TmbData, Report = fit$Report, spatial_list = Spatial_List, Q = Q, 
                extrapolation_list = Extrapolation_List, working_dir = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, ) 
   })} 
  
   # Save it all in Image.RData [ When reloading, remember to dyn.load() the '.dll' e.g. dyn.load(paste0(DateFile, 'VAST_v9_2_0.dll')) ]
   save(list = c(ls(), names(.GlobalEnv)), file = paste0(DateFile, "Image.RData")) # Save files inside the function also!!!!!!
   
   
# =======================================================================

   if(FALSE) {
   
      # Example run
      
      # Lognormal GLMM error on the positive catch with standard delta model, Region = "California_current", and VAST_surveyName = 'propInSlope98_00' (i.e. Domain area)
      West_Coast_2021_V3.6.1(spFormalName = 'Pacific spiny dogfish', spLongName = 'Spiny dogfish', spShortName = 'DSRK', Survey = 'AFSC.Slope', 
                       yearRange = c(1997, 2001), ObsModel = c(1, 0))  
                 
       
   
      # See 'VAST_3.6.1_Survey_Indexes.R' for a figure to view VAST's 'surveyname' grids (i.e. Domains) for CA Current region and plot the raw data (black points) on top of each one ====
      
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
    
      sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master//VAST_3.6.1_Survey_Indexes.R")

   }

}




