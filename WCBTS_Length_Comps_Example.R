
# Test run of length comps in VAST ver 3X (wrapper functions)
# Longnose skate data; initial following the VAST Wiki 'Expand age and length composition' example
# Revised by V. Gertseva, Feb 2020
# Revised by V. Gertseva & J. Wallace Mar 2020
# Revised by V. Gertseva Apr 2020
# Revised by J. Wallace Apr & May 2020

library(JRWToolBox)
#install.packages("devtools")
library(devtools)

options(stringsAsFactors = FALSE)  # This is the now the default starting with R ver 4.0.0

#===============================================================================
#=============          Package test          ==================================
#===============================================================================

# https://github.com/nwfsc-assess/nwfscSurvey
devtools::install_github("nwfsc-assess/nwfscSurvey", build_vignettes = TRUE)

# Load the package
library(nwfscSurvey)
# Look at the vignette
#vignette("nwfscSurvey")
# Look at all the functions in the package
ls("package:nwfscSurvey")
#?PullCatch.fn

#===============================================================================
#=============          NWFSC Combo          ===================================
#===============================================================================

# setwd("C:/Users/Vladlena.Gertseva/Desktop/VAST_2020/2020 runs/Longnose skate Bio")

# HomeDir <- "C:/Users/Vladlena.Gertseva/Desktop/VAST_2020/2020 runs/"
HomeDir <- "W:/ALL_USR/JRW/Assessment/Length_Comps_VAST/"
setwd(HomeDir); getwd()

yearRange <- c(2012, 2019)

DateDir <- paste0(HomeDir, 'VAST_', Sys.Date(), '_Longnose_Comps_NWFSC_Combo_', yearRange[1], '_', yearRange[2], '/')
dir.create(DateDir, showWarnings = FALSE)
setwd(DateDir); getwd()


# Longnose skate
spFormalName <- 'longnose skate' 
spLongName <- 'Longnose skate'
spShortName <- 'LSKT'


# catch = PullCatch.fn(SciName = "Raja rhina", SurveyName = "NWFSC.Combo", YearRange = c(2012, 2016), SaveFile = TRUE, Dir = getwd()) 
catch <- JRWToolBox::dataWareHouseTrawlCatch(spFormalName, yearRange = yearRange, project = 'WCGBTS.Combo')
catch$cpue_kg_km2 <- catch$Total_sp_wt_kg/(catch$Area_Swept_ha/100)
names(catch)[grep('Total_sp_numbers', names(catch))] <- 'total_catch_numbers'

# bio = PullBio.fn(SciName = "Raja rhina", SurveyName = "NWFSC.Combo", YearRange = c(2012, 2016), SaveFile = TRUE, Dir = getwd())
bio <- JRWToolBox::dataWareHouseTrawlBio(spFormalName, yearRange = yearRange, project = 'WCGBTS.Combo')
names(bio)[grep('Weight_kg', names(bio))] <- 'Weight'  # I added the units to the 'Weight_kg' and the PullBio.fn() is behind


head(catch)
head(bio)

library(lattice)
xyplot(cpue_kg_km2 ~ -Depth_m | factor(Year), groups = factor(sign(cpue_kg_km2)), data = catch, col = c('dodgerblue', col.alpha('magenta', 0.3)))
dev.new()
histogram(~ Length_cm | factor(Year), data = bio) 
dev.new()
xyplot(Weight ~ Length_cm | factor(Year), data = bio)  # Longnose weight data only in years 2012 & 2016
dev.new()
xyplot(Width_cm ~ Length_cm | factor(Year), data = bio) # Now no width data???


# Can pull data based on the general name (Name) of the scientific name(SciName). The default year range (YearRange)
# is set to cover all potential years.  The SurveyName options are: Triennial, AFSC.Slope, NWFSC.Slope, NWFSC.Shelf
# NWFSC.Combo, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, or NWFSC.Video. These data pulls can also be saved to a specified 
# directory using SaveFile = TRUE and Dir = "directory to save file". 
# load("Catch_2018-08-06__NWFSC.Combo_2018-08-06.rda")


#===============================================================================
#=============          Length conversion          =============================
#===============================================================================

#If Length_cm is missing but Width_cm has a value, then the following formula

# Look at a table of missing lengths and of missing length vs width before applying the formula
Table(is.finite(bio$Length_cm), bio$Year)
Table(is.finite(bio$Length_cm), is.finite(bio$Width_cm))
# Length in rows, Width in columns


# Females
bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"]*1.402090 + 0.911707
 
# Males
bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "M"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "M"]*1.405850 + 0.523354
 
# Unsexed
bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "U"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "U"]*1.404374 + 0.700501
  
# Look at a table of missing lengths and of missing length vs width after applying the formula
Table(is.finite(bio$Length_cm))
Table(is.finite(bio$Length_cm), is.finite(bio$Width_cm))
# Length in rows, width in columns


#Examples how to shorten formulas 
# TF <- is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"
# bio$Length_cm[TF] <- bio$Width_cm[TF]*1.4102 + 9.8281


#===============================================================================
#=============          Create Stratafication      =============================
#===============================================================================

# The stratafication areas are calculated from the SA3 file which is attached to the package.

(strata = CreateStrataDF.fn(names=c("shallow_CA", "mid_CA", "deep_CA", "shallow_OR", "mid_OR", "deep_OR", "shallow_WA", "mid_WA", "deep_WA"), 
                           depths.shallow = c(55,   183, 549,  55,   183, 549,  55,   183, 549),
                           depths.deep    = c(183,  549, 1280, 183,  549, 1280, 183,  549, 1280),
                           lats.south     = c(32,   32,  32,   36,   36,  36,   42,   42,	 42),
                           lats.north     = c(36,   36,  36,   42,   42,  42,   49,   49,	 49)))



# Calculate the design based index
biomass = Biomass.fn(dir = getwd(), dat = catch,  strat.df = strata, printfolder = "forSS", outputMedian = TRUE) 

# Creates a csv file within the "printfolder" that will be saved within the directory location (dir).

# Plot the biomass index
PlotBio.fn(dir = getwd(), dat = biomass, main = "NWFSC shelf-slope bottom trawl survey", dopng = FALSE)
PlotBio.fn(dir = getwd(), dat = biomass, main = "NWFSC shelf-slope bottom trawl survey", dopng = TRUE)


#============================================================================================
#=============          Length Biological Data            ===================================
#============================================================================================

bin_size = 5
(len.bins = seq(5, 165, bin_size))

# Calculate the effN
(n = GetN.fn(dir=getwd(), dat = bio, type = "length", species = "others", printfolder = "forSS"))
# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.



# Expand and format length composition data for SS
#LFs <- SurveyLFs.fn(dir = getwd(), datL = bio, datTows = catch,  
stage_one  <- SurveyLFs.fn(dir = getwd(), datL = bio, datTows = catch,  
                    strat.df = strata, lgthBins = len.bins, gender = 3, 
                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 20, outputStage1 = TRUE,
                    nSamps = n)
                    
# Create the length bins names
lo = len.bins[findInterval(stage_one$Length_cm, len.bins, left.open = TRUE)]
hi = lo + bin_size
Length_bin = paste0(lo, "-", hi, "cm")

# The comps are calculated seperately by sex:
by_sex = "female"
(sex = ifelse(by_sex == "female", "Nf", "Nm"))

# The code offers two options for applying the sex ratio based on expansion stage. The sex ratio will be
# applied based on a tow basis first if sexRatioStage = 1. The other option applies the sex ratio to the
# expanded numbers of fish across a whole strata (sexRatioStage = 2, this was the option applied to the
# NWFSC combo survey data in the past).

LenCompNoZero = stage_one[,c("Trawl_id", "Year", "Latitude_dd", "Longitude_dd", "areaFished", sex)]
# LenCompNoZero$Length_bin = as.factor(Length_bin)
LenCompNoZero$Length_bin = Length_bin
# The below conversion is just done to put the area fished on the same scale as the lingcod example
LenCompNoZero$areaFished = LenCompNoZero$areaFished / 10000 # need to double check this
colnames(LenCompNoZero) = c("Trawl_id", "Year", "Lat", "Lon", "AreaSwept_km2", "First_stage_expanded_numbers", "Length_bin")
#for test with no zero catches
#LenCompNoZero$Trawl_id <- NULL


# Check that all length bins have data in at least one year
agg.table(aggregate(list(Num = LenCompNoZero$First_stage_expanded_numbers), list(Length_bin = LenCompNoZero$Length_bin, Year = LenCompNoZero$Year), sum)) 
# No data in length bin 10-15cm - so remove it
LenCompNoZero <- LenCompNoZero[!LenCompNoZero$Length_bin %in% '10-15cm', ]
# Check
agg.table(aggregate(list(Num = LenCompNoZero$First_stage_expanded_numbers), list(Length_bin = LenCompNoZero$Length_bin, Year = LenCompNoZero$Year), sum))


#Add length bin from LenCompNoZero file to hauls with zero catches from catch file
LengthCompWithZero <- expand.grid(Length_bin = unique(LenCompNoZero$Length_bin), Trawl_id = catch$Trawl_id, stringsAsFactors = FALSE)[, 2:1]

#Match each length bin in each haul to lat, long and area_swept_ha
LengthCompWithZero <- match.f(LengthCompWithZero, catch, "Trawl_id", "Trawl_id", c("Year", "Longitude_dd", "Latitude_dd", "Depth_m", "Area_Swept_ha"))

#Add column First_stage_expanded_numbers to file with zero and positive catches
LengthCompWithZero <- match.f(LengthCompWithZero, LenCompNoZero, c("Trawl_id", "Length_bin"), c("Trawl_id", "Length_bin"), "First_stage_expanded_numbers")

#Change NA First_stage_expanded_numbers for zero catch hauls to 0
LengthCompWithZero$First_stage_expanded_numbers[is.na(LengthCompWithZero$First_stage_expanded_numbers)] <- 0

#Add column Area_Swept_km2
LengthCompWithZero$Area_Swept_km2 <- LengthCompWithZero$Area_Swept_ha/100

#Remove column Area_Swept_ha
LengthCompWithZero$Area_Swept_ha <- NULL

#Change location of columns
LengthCompWithZero <- renum(LengthCompWithZero[, c(1,3,5,4,6,8,7,2)])
LengthCompWithZero[1:10,]

#Change names of columns
colnames(LengthCompWithZero)[c(3,4,6)] = c("Lat", "Lon", "AreaSwept_km2")
LengthCompWithZero$Trawl_id <- NULL
LenCompNoZero$Trawl_id <- NULL

head(LengthCompWithZero)
head(LenCompNoZero)

# Check
length(unique(LengthCompWithZero$Length_bin))
nrow(LengthCompWithZero)/nrow(catch)

# Not much length data for Longnose skate beyond 300 fathoms (549 meters).
xyplot(Lat ~ -Depth_m | factor(Year), groups = as.logical(First_stage_expanded_numbers), data = LengthCompWithZero, 
       panel = function(...) { panel.xyplot(...); panel.abline(v = -unique(c(strata$Depth_m.1, strata$Depth_m.2)), h = unique(c(strata$Latitude_dd.1, strata$Latitude_dd.2)))})

# Save above xyplot() to Fig directory as a png
FigDir <- paste0(DateDir, 'Figs/')
dir.create(FigDir, showWarnings = FALSE) 
      
png(1000, 1000, file = paste0(FigDir, file = 'Raw_data_Lat_by_Depth_by_Year_by_Presence.png')) # Presence is First_stage_expanded_numbers != 0 (i.e. a length taken, not just a catch)
xyplot(Lat ~ -Depth_m | factor(Year), groups = as.logical(First_stage_expanded_numbers), data = LengthCompWithZero, 
       panel = function(...) { panel.xyplot(...); panel.abline(v = -unique(c(strata$Depth_m.1, strata$Depth_m.2)), h = unique(c(strata$Latitude_dd.1, strata$Latitude_dd.2)))})
dev.off()
       
       
# write.csv(LengthCompWithZero,"LengthCompWithZero.csv")
save(LengthCompWithZero, file = paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '.RData'))


#===============================================================================
#=============          VAST         ===========================================
#===============================================================================

# Download release number 3.4.0; its useful for reproducibility to use a specific release number
# devtools::install_github("james-thorson-noaa/VAST", ref="3.4.0")
# devtools::install_github("james-thorson-noaa/FishStatsUtils", ref="2.6.0")

# Load packages
library(TMB)
library(VAST)

# load data set
# see `?load_example` for list of stocks with example data
# that are installed automatically with `FishStatsUtils`.
#example = load_example( data_set="Lingcod_comp_expansion" )


setwd(DateDir); getwd()
load(paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '.RData'))

# Using  < c_i = as.numeric(as.factor(LengthCompWithZero[,"Length_bin"])) - 1 > in fit.model(), which is based on character sorting, doesn't give the correct bin ordering for large animals of 100cm
#      or more [e.g. sort(c('10', '15', '20', '100')) gives c( "10", "100", "15", "20") ] :
   LengthCompWithZero$Length_bin_num <- as.numeric(as.factor(LengthCompWithZero[,"Length_bin"])) - 1
   (charSort <- renum(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin), c('Length_bin', 'Length_bin_num')][order(LengthCompWithZero$Length_bin[!duplicated(LengthCompWithZero$Length_bin)]), ]))
  
# Using JRWToolBox::recode.simple() to re-code correctly via 'Length_bin_num' (hacked the 'ref_Table' but that could be created in anyway desired)
   Order <- c(9:25, 1:8)  # Change this 'Order' data frame correctly so that 'Length_bin' goes from smallest to largest and Length_bin_num starts at zero and monotonically increases with ordinal numbers
   (ref_Table <- data.frame (Length_bin = charSort$Length_bin[Order], Length_bin_num = 0:(length(Order) - 1)))
   LengthCompWithZero$Length_bin_num <- as.numeric(recode.simple(LengthCompWithZero$Length_bin, ref_Table))
   # Check
   renum(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin), c('Length_bin', 'Length_bin_num')][order(LengthCompWithZero$Length_bin[!duplicated(LengthCompWithZero$Length_bin)]), ][Order, ])
   # An additional check is below where only the 'Length_bin" should not be zero, since that's a character vector that is not given to VAST
   lapply(LengthCompWithZero, function(x) sum(!is.finite(x)))



  
# === Comments for FishStatsUtils::make_settings() and FishStatsUtils::fit_model() ===

#     Changed the 700 fathom (1280 meter) 'deep border' to 300 (549 meters), thus removing the 300 - 700 fathom strata that has limited data for Longnose skate. 
      
#     Original settings: fine_scale = FALSE; n_x = 50, Npool = 40, and max_cells = 2000
#     Higher settings: fine_scale = TRUE; n_x = 300, Npool = 200, and max_cells = 3000 - making these settings larger sometimes appears to help 
#          prevent parameters from approaching zero - at other times, not so much:
      
#          The following parameters appear to be approaching zero:
#                    Param starting_value Lower           MLE Upper final_gradient
#         139 L_epsilon1_z   8.966545e-07  -Inf -2.738787e-12   Inf  -1.789365e-11
#         141 L_epsilon1_z  -8.114447e-07  -Inf -7.202319e-12   Inf  -7.328495e-11
#         147 L_epsilon1_z  -4.199572e-07  -Inf -1.409835e-12   Inf  -1.263259e-11
#         149 L_epsilon1_z  -7.717835e-07  -Inf -6.036781e-12   Inf  -4.417835e-11
#         289 L_epsilon2_z  -8.164282e-07  -Inf -1.056235e-09   Inf  -2.730111e-09
#         Please turn off factor-model variance parameters `L_` that are approaching zero and re-run the model
      
#     ??? How important is it, for these comp models, that all parameters not approach zero as long as the Hessian in invertible ???
#     The help for VAST::check_fit states: "If check_fit identifies an issue in estimated parameters, then the model structure should typically be changed".
#     What is typical?
      
      
#     Note that, for these comp models, following the advice in the help for VAST::check_fit seems ill-advised.
#        Turning off spatial or spatio-temporal effects, as advised, no longer gives "IID" in FieldConfig [ see 'settings.txt' created by running fit_model() ]
      
#         $FieldConfig
#                 Component_1 Component_2
#         Omega   "IID"       "IID"      
#         Epsilon "IID"       "IID"      
#         Beta    "IID"       "IID"    
         
#     But without "IID" 'model_args = list(Npool = 200)' in a call to fit_model() is longer supported, which is the one thing that Thorson increased in the VAST Wiki 
#        example to make an invertible Hessian:  'Expand age and length composition'
#       ( https://github.com/James-Thorson-NOAA/VAST/wiki/Expand-age-and-length-composition#expanding-agelength-composition-data-for-use-in-stock-assessments )
      
#      For an explanation of Npool see: https://github.com/James-Thorson-NOAA/VAST/issues/220


#   === On the use of 'test_fit' arg in fit_model() ===

#     The arg 'test_fit' in fit_model() applies VAST::check_fit() to test the parameters hitting bounds.
#     The help for check_fit() claims "check_fit checks bounds and throws an informative message if any look bad", however 'test_fit = TRUE' will throw an error 
#     if parameters appear to be approaching zero, causing complete loss of any other information for a run that may have taken hours.
#     Also, the testing of initial values for nonzero gradients cannot be directly uncoupled from the testing of the parameters that approach zero after the model has run.
#     Thus, I now make an intial run with 'test_fit = TRUE', looking for:

#           ### Testing model at initial values
#           Looks good: All fixed effects have a nonzero gradient
           
#     If that is found, I stop the model (Esc in Windows or Ctrl-C in Linux) (if things don't look good, the model is stopped for you).
#     I then run with 'test_fit = FALSE' (wait for the model to run) and then manually do:
#             VAST::check_fit(fit$parameter_estimates)
#     on the 'fit' object.
#     The second run uses the already complied C++ code, so time is saved there.
#     plot_results() can then be used on the 'fit' object and the model further evaluated.


# Make settings
strataLimits <- data.frame(STRATA = c("Coastwide","CA","OR","WA"),
               north_border = c(49.0, 42.0, 46.0, 49.0),
               south_border = c(32.0, 32.0, 42.0, 46.0),
             shallow_border = c(55, 55, 55, 55),
                deep_border = c(549, 549, 549, 549))
                
settings = FishStatsUtils::make_settings( n_x = 300, Region = "California_current", purpose = "index2",  
           fine_scale = TRUE, 
             ObsModel = c(2, 0), 
        # FieldConfig = c(Omega1 = 1, Epsilon1 = 0, Omega2 = 0, Epsilon2 = 1), 
            RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), 
 OverdispersionConfig = c(Eta1 = 0, Eta2 = 0),
       use_anisotropy = FALSE,
         bias.correct = FALSE,
            max_cells = 3000,
        strata.limits = strataLimits)
 


# Run model  
sink("Fit_Output.txt")
fit = FishStatsUtils::fit_model( 
  settings = settings, 
  Lat_i = LengthCompWithZero[,'Lat'], 
  Lon_i = LengthCompWithZero[,'Lon'],
  t_i = LengthCompWithZero[,'Year'], 
  c_i = LengthCompWithZero$Length_bin_num,
  b_i = LengthCompWithZero[,'First_stage_expanded_numbers'],
  a_i = LengthCompWithZero[,'AreaSwept_km2'], 
  model_args = list(Npool = 200), newtonsteps = 1, test_fit = c(TRUE, FALSE)[2] )
sink()

# Max gradient
fit$parameter_estimates[['max_gradient']]

#AIC
fit$parameter_estimates[['AIC']]

VAST::check_fit(fit$parameter_estimates)

# Early save - done again below
# Save it all in Image.RData [ When reloading, remember to dyn.load() the '.dll', e.g. dyn.load(paste0(DateDir, 'VAST_v9_2_0.dll')) ]
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))

                 
# Plot results (Make sure the Fig directory is present, in case the code wasn't run above.)
FigDir <- paste0(DateDir, 'Figs/')
dir.create(FigDir, showWarnings = FALSE)

results = plot_results(fit = fit, settings = fit$settings, plot_set = 3, category_names = ref_Table$Length_bin, 
              strata_names = strataLimits$STRATA, check_residuals = FALSE, working_dir = FigDir)
 
# Do plot_range_index() (for Effective_Area.png) again by itself so strata_names are included [ doesn't get added properly in plot_results() ] 
#      plot_range_index() also recreates 'center_of_gravity.png' with no change but the 'Date modified'
plot_range_index(Report = fit$Report, TmbData = fit$data_list, 
            Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), 
            PlotDir = FigDir, Year_Set = fit$year_labels, Years2Include = fit$years_to_plot, 
            use_biascorr = FALSE, category_names = ref_Table$Length_bin, strata_names = strataLimits$STRATA)

            
#  # If plot_biomass_index() fails, try setting 'treat_nonencounter_as_zero' to FALSE and re-try
#  # This fails when a length bin is empty over all years - which needs to be fixed, but at least the figures can be looked at.
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero']
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero'] <- c(TRUE, FALSE)[2]
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero']
 

# Calculate proportions 
proportions <- calculate_proportion(TmbData = fit$data_list, Index = results$Index, Year_Set = fit$year_labels,
                  strata_names = strataLimits$STRATA, DirName = FigDir)

names(proportions)
#  [1] "Prop_ctl"     "Neff_tl"      "var_Prop_ctl" "Index_tl"     "Neff_ctl"     "Mean_tl"    "sd_Mean_tl"  

dimnames(proportions$Neff_tl) <- list(fit$year_labels, strataLimits$STRATA)
proportions$Neff_tl

#           Coastwide            CA           OR           WA
#  2012 1105.56027577  916.58676889 655.01395386 453.06775573
#  2013  798.63061932  689.23672238 579.85407685 382.71489703
#  2014 1122.73744094  987.29969613 691.42093641 471.60152020
#  2015 1143.63275765  976.06640714 695.09905904 496.94268546
#  2016 1208.55527243  970.84796261 787.71612659 488.28437210
#  2017 1173.23343141 1054.01173679 747.54463707 536.80411689
#  2018 1116.78034561  938.28118559 698.00185540 513.77874095
#  2019  561.42381200  508.07606527 427.41826495 338.07075632


save(proportions, file = paste0(DateDir, "proportions.RData"))


# Save it all in Image.RData [ When reloading, remember to dyn.load() the '.dll', e.g. dyn.load(paste0(DateDir, 'VAST_v9_2_0.dll')) ]
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))
 
 
 
# Summary of the fit
fitSummary <- summary(fit)  # FishStatsUtils:::summary.fit_model (The column names for the head of `Density_dataframe` are in the wrong place for the printing.)
dim(fitSummary$Density_dataframe)  

fitSummary$extrapolation_grid[1:4,]
dim(fitSummary$extrapolation_grid)

fitSummary$Density_array[1:4,,1, drop = FALSE]
dim(fitSummary$Density_array)

                         
                          
setwd(HomeDir); getwd()                         
        

      

# No ages for Longnose skate                          

                       
#============================================================================================
#=============     Age Biological Data      =================================================
#============================================================================================

age = bio
age.bins = 1:24

n = GetN.fn(dir = getwd(), dat = age, type = "age", species = "shelfrock", printfolder = "forSS")

# Exand and format the marginal age composition data for SS
Ages <- SurveyAFs.fn(dir = getwd(), datA = age, datTows = catch,  
                     strat.df = strata, ageBins = age.bins, 
                     sexRatioStage = 2, sexRatioUnsexed = 0.50, maxSizeUnsexed = 5, 
                     gender = 3, nSamps = n)



PlotFreqData.fn(dir = getwd(), dat = Ages, survey = "NWFSCBT", ylim=c(0, max(age.bins) + 2), yaxs="i", ylab="Age (yr)", dopng=TRUE)
PlotVarLengthAtAge.fn(dir = getwd(), dat = age, survey ="NWFSCBT", dopng = TRUE) 
PlotSexRatio.fn(dir = getwd(), dat = age, data.type = "age", survey = "NWFSCBT", dopng = TRUE, main = "NWFSCBT")

#============================================================================================
# Conditional Ages
#============================================================================================
Ages <- SurveyAgeAtLen.fn (dir = getwd(), datAL = age, datTows = catch, 
                          strat.df = strata, lgthBins = len.bins, ageBins = age.bins, partition = 0)

 
