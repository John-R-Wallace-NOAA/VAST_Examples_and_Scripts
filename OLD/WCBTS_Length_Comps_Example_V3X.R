
# Test run of length comps in VAST ver 3X (wrapper functions)
# Longnose skate data; initial following the VAST Wiki 'Expand age and length composition' example
# Revised by V. Gertseva, Feb 2020
# Revised by V. Gertseva & J. Wallace Mar 2020
# Revised by V. Gertseva Apr 2020
# Revised by J. Wallace Apr & May 2020

library(JRWToolBox)
# install.packages("devtools")
library(lattice)

options(stringsAsFactors = FALSE)  # This is the now the default starting with R ver 4.0.0

# HomeDir <- "C:/Users/Vladlena.Gertseva/Desktop/VAST_2020/2020 runs/"
HomeDir <- "W:/ALL_USR/JRW/Assessment/Length_Comps_VAST/"

# Linux server (e.g. Tantalus)
# (HomeDir <- paste0(getwd(), "/"))
# options(width = 220)

setwd(HomeDir); getwd()


#===============================================================================
#=============          Package test          ==================================
#===============================================================================

# https://github.com/nwfsc-assess/nwfscSurvey
# devtools::install_github("nwfsc-assess/nwfscSurvey", build_vignettes = TRUE)

# Load the package
library(nwfscSurvey)
# Look at the vignette
#vignette("nwfscSurvey")
# Look at all the functions in the package
ls("package:nwfscSurvey")
#?PullCatch.fn

#===============================================================================
#=============          Species         ========================================
#===============================================================================

# Longnose skate
spFormalName <- 'longnose skate' 
spShortName <- 'longnose'
spSciName <- 'Raja rhina'


#===============================================================================
#=============         Survey          =========================================
#===============================================================================

# Survey <- "NWFSC.Combo"
Survey <- 'WCGBTS.Combo'

#===============================================================================
#=============         Years          ==========================================
#===============================================================================

yearRange <- c(2012, 2019)


#===============================================================================
#=============      Single sex or both          ================================
#===============================================================================

# Select if you want only one sex in the model or both - including both sexes doubles the number of rows of the data.
numSexInModel <- c(1, 2)[2]

if(numSexInModel %in% 1) {
    by_sex = "female"
    (sex = ifelse(by_sex == "female", "Nf", "Nm"))
    DateDir <- paste0(HomeDir, 'VAST_', Sys.Date(), '_', spShortName, '_Comps_NWFSC_Combo_', yearRange[1], '_', yearRange[2], '_sex', casefold(substring(sex, 2, 2), upper = TRUE), '/')
}

if(numSexInModel %in% 2)
   DateDir <- paste0(HomeDir, 'VAST_', Sys.Date(), '_', spShortName, '_Comps_NWFSC_Combo_', yearRange[1], '_', yearRange[2], '_sexMF/')

dir.create(DateDir, showWarnings = FALSE)
setwd(DateDir); getwd()

FigDir <- paste0(DateDir, 'Figs/')
dir.create(FigDir, showWarnings = FALSE) 
     

#===============================================================================
#=============      Data          ==============================================
#===============================================================================

# catch = PullCatch.fn(SciName = spSciName , SurveyName = Survey, YearRange = c(2012, 2016), SaveFile = TRUE, Dir = getwd()) 
catch <- JRWToolBox::dataWareHouseTrawlCatch(spFormalName, yearRange = yearRange, project = Survey)
catch$cpue_kg_km2 <- catch$Total_sp_wt_kg/(catch$Area_Swept_ha/100)
names(catch)[grep('Total_sp_numbers', names(catch))] <- 'total_catch_numbers'

# bio = PullBio.fn(SciName = spSciName, SurveyName = Survey, YearRange = c(2012, 2016), SaveFile = TRUE, Dir = getwd())
bio <- JRWToolBox::dataWareHouseTrawlBio(spFormalName, yearRange = yearRange, project = Survey)
names(bio)[grep('Weight_kg', names(bio))] <- 'Weight'  # I added the units to the 'Weight_kg' and the PullBio.fn() is behind


head(catch)
head(bio)


xyplot(cpue_kg_km2 ~ -Depth_m | factor(Year), groups = factor(sign(cpue_kg_km2)), data = catch, col = c('dodgerblue', col.alpha('magenta', 0.3)))
dev.new()
xyplot(Weight ~ Length_cm | factor(Year), data = bio)  # Longnose weight data only in years 2012 & 2016
dev.new()
histogram(~ Length_cm | factor(Year), data = bio) 

if(any(is.finite(bio$Width_cm))) {
  dev.new()
  xyplot(Width_cm ~ Length_cm | factor(Year), data = bio) 
  dev.new()
  histogram(~ Width_cm | factor(Year), data = bio)
} 

# Can pull data based on the general name (Name) of the scientific name(SciName). The default year range (YearRange)
# is set to cover all potential years.  The SurveyName options are: Triennial, AFSC.Slope, NWFSC.Slope, NWFSC.Shelf
# NWFSC.Combo, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, or NWFSC.Video. These data pulls can also be saved to a specified 
# directory using SaveFile = TRUE and Dir = "directory to save file". 
# load("Catch_2018-08-06__NWFSC.Combo_2018-08-06.rda")


#===============================================================================
#=============          Width to Length Conversion          ====================
#===============================================================================

#If Length_cm is missing but Width_cm has a value, then the following formula

# Look at a table of missing lengths and of missing length vs width before applying the formula
cat("\n\n")
Table(is.finite(bio$Length_cm), bio$Year); cat("\n\n")
Table(is.finite(bio$Width_cm), bio$Year); cat("\n\n")
Table(is.finite(bio$Length_cm), is.finite(bio$Width_cm)); cat("\n\n")
# Length in rows, Width in columns


# Females
bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"]*1.402090 + 0.911707
 
# Males
bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "M"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "M"]*1.405850 + 0.523354
 
# Unsexed
bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "U"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "U"]*1.404374 + 0.700501
  
# Look at a table of missing lengths and of missing length vs width after applying the formula
cat("\n\n")
Table(is.finite(bio$Length_cm), bio$Year); cat("\n\n")
Table(is.finite(bio$Width_cm), bio$Year); cat("\n\n")
Table(is.finite(bio$Length_cm), is.finite(bio$Width_cm)); cat("\n\n")
# Length in rows, width in columns


#Examples how to shorten formulas 
# TF <- is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"
# bio$Length_cm[TF] <- bio$Width_cm[TF]*1.4102 + 9.8281


#===============================================================================
#=============  Create Stratafication and Design Based Index   =================
#===============================================================================

# The stratafication areas are calculated from the SA3 file which is attached to the package.

(strata = nwfscSurvey::CreateStrataDF.fn(names=c("shallow_CA", "mid_CA", "deep_CA", "shallow_OR", "mid_OR", "deep_OR", "shallow_WA", "mid_WA", "deep_WA"), 
                                   depths.shallow = c(55,   183, 549,  55,   183, 549,  55,   183, 549),
                                   depths.deep    = c(183,  549, 1280, 183,  549, 1280, 183,  549, 1280),
                                   lats.south     = c(32,   32,  32,   36,   36,  36,   42,   42,	 42),
                                   lats.north     = c(36,   36,  36,   42,   42,  42,   49,   49,	 49)))



# Calculate the design based index
biomass = nwfscSurvey::Biomass.fn(dir = getwd(), dat = catch,  strat.df = strata, printfolder = "forSS", outputMedian = TRUE) 

# Creates a csv file within the "printfolder" that will be saved within the directory location (dir).

# Plot the biomass index with confidence intervals 
nwfscSurvey::PlotBio.fn(dir = getwd(), dat = biomass, main = "NWFSC shelf-slope bottom trawl survey", dopng = FALSE)
nwfscSurvey::PlotBio.fn(dir = getwd(), dat = biomass, main = "NWFSC shelf-slope bottom trawl survey", dopng = TRUE)


#============================================================================================
#=============          Length Data            ==============================================
#============================================================================================

bin_size = 5
(len.bins = seq(5, 165, bin_size))

# Calculate the effN
(n = nwfscSurvey::GetN.fn(dir=getwd(), dat = bio, type = "length", species = "others", printfolder = "forSS"))
# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.



# Expand and format length composition data for SS

# The code offers two options for applying the sex ratio based on expansion stage. The sex ratio will be
# applied based on a tow basis first if sexRatioStage = 1. The other option applies the sex ratio to the
# expanded numbers of fish across a whole strata (sexRatioStage = 2, this was the option applied to the
# NWFSC combo survey data in the past).
stage_one <- nwfscSurvey::SurveyLFs.fn(dir = getwd(), datL = bio, datTows = catch,  
                    strat.df = strata, lgthBins = len.bins, gender = 3, 
                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 20, outputStage1 = TRUE,
                    nSamps = n)
                    
# The below conversion is just done to put the area fished on the same scale as the lingcod example, kg per hectare to kg per square kilometer
stage_one$areaFished = stage_one$areaFished / 10000   # areaFished is dropped below - it could be used as a check                  
                    
# Create the length bins names
lo = len.bins[findInterval(stage_one$Length_cm, len.bins, left.open = TRUE)]
hi = lo + bin_size
stage_one$Length_bin = paste0(lo, "-", hi, "cm")


# The comps are calculated seperately by sex:
head(stage_one)


if(numSexInModel %in% 1) {
    
    LenCompNoZero = stage_one[,c("Trawl_id", "Year", "Latitude_dd", "Longitude_dd", "areaFished", sex, "Length_bin")]
    colnames(LenCompNoZero) = c("Trawl_id", "Year", "Lat", "Lon", "AreaSwept_km2", "First_stage_expanded_numbers", "Length_bin")
    #for test with no zero catches
    #LenCompNoZero$Trawl_id <- NULL
} 

if(numSexInModel %in% 2) {

   # 'Length_bin' now includes sex; "F_" or "M_"

    LenCompNoZero_female <- stage_one[,c("Trawl_id", "Year", "Latitude_dd", "Longitude_dd", "areaFished", "Nf", "Length_bin")]
    colnames(LenCompNoZero_female) = c("Trawl_id", "Year", "Lat", "Lon", "AreaSwept_km2", "First_stage_expanded_numbers", "Length_bin")
    LenCompNoZero_female$Length_bin <- paste0("F_", LenCompNoZero_female$Length_bin)
    
    LenCompNoZero_male <- stage_one[,c("Trawl_id", "Year", "Latitude_dd", "Longitude_dd", "areaFished", "Nm", "Length_bin")]
    colnames(LenCompNoZero_male) = c("Trawl_id", "Year", "Lat", "Lon", "AreaSwept_km2", "First_stage_expanded_numbers", "Length_bin")
    LenCompNoZero_male$Length_bin <- paste0("M_", LenCompNoZero_male$Length_bin)

    LenCompNoZero <- rbind(LenCompNoZero_female, LenCompNoZero_male)
}

# Check that all length bins have data in at least one year
JRWToolBox::agg.table(aggregate(list(Num = LenCompNoZero$First_stage_expanded_numbers), list(Length_bin = LenCompNoZero$Length_bin, Year = LenCompNoZero$Year), sum)) 

# No or little data for some length bins, so remove them for now
(numRowsOld <- nrow(LenCompNoZero))
if(numSexInModel %in% 1)
    LenCompNoZero <- LenCompNoZero[!LenCompNoZero$Length_bin %in% '10-15cm', ]
if(numSexInModel %in% 2)
    LenCompNoZero <- LenCompNoZero[!LenCompNoZero$Length_bin %in% c('F_10-15cm', 'M_10-15cm', 'M_130-135cm', 'M_135-140cm', 'M_140-145cm'), ]
# Percent reduction of rows
 100 * (1 - nrow(LenCompNoZero)/numRowsOld)


# Check LenCompNoZero
JRWToolBox::agg.table(aggregate(list(Num = LenCompNoZero$First_stage_expanded_numbers), list(Length_bin = LenCompNoZero$Length_bin, Year = LenCompNoZero$Year), sum))


#Add length bin from LenCompNoZero file to hauls with zero catches from catch file (switching cols after indexing avoids sorting)
LengthCompWithZero <- expand.grid(Length_bin = unique(LenCompNoZero$Length_bin), Trawl_id = catch$Trawl_id, stringsAsFactors = FALSE)[, c("Trawl_id", "Length_bin")] 

#Match each length bin in each haul to lat, long and area_swept_ha
LengthCompWithZero <- JRWToolBox::match.f(LengthCompWithZero, catch, "Trawl_id", "Trawl_id", c("Year", "Longitude_dd", "Latitude_dd", "Depth_m", "Area_Swept_ha"))

#Add column First_stage_expanded_numbers to file with zero and positive catches
LengthCompWithZero <- JRWToolBox::match.f(LengthCompWithZero, LenCompNoZero, c("Trawl_id", "Length_bin"), c("Trawl_id", "Length_bin"), "First_stage_expanded_numbers")

#Change NA First_stage_expanded_numbers for zero catch hauls to 0
LengthCompWithZero$First_stage_expanded_numbers[is.na(LengthCompWithZero$First_stage_expanded_numbers)] <- 0

#Create column AreaSwept_km2 and remove column Area_Swept_ha
LengthCompWithZero$AreaSwept_km2 <- LengthCompWithZero$Area_Swept_ha/100
LengthCompWithZero$Area_Swept_ha <- NULL

#Change location of columns
LengthCompWithZero <- renum(LengthCompWithZero[, c("Trawl_id", "Year", "Latitude_dd", "Longitude_dd", "Depth_m", "AreaSwept_km2", "First_stage_expanded_numbers", "Length_bin")])
LengthCompWithZero[1:10,]

#Change names of columns
names(LengthCompWithZero)[grep("Latitude_dd", names(LengthCompWithZero))] <- "Lat"
names(LengthCompWithZero)[grep("Longitude_dd", names(LengthCompWithZero))] <- "Lon"

LengthCompWithZero$Trawl_id <- NULL
LengthCompWithZero[1:10,]

# Check LengthCompWithZero
JRWToolBox::agg.table(aggregate(list(Num = LengthCompWithZero$First_stage_expanded_numbers), list(Length_bin = LengthCompWithZero$Length_bin, Year = LengthCompWithZero$Year), sum))

# Check dimensions
length(unique(LengthCompWithZero$Length_bin))
nrow(LengthCompWithZero)/nrow(catch)

# Not much length data for Longnose skate beyond 300 fathoms (549 meters).
xyplot(Lat ~ -Depth_m | factor(Year), groups = as.logical(First_stage_expanded_numbers), data = LengthCompWithZero, 
       panel = function(...) { panel.xyplot(...); panel.abline(v = -unique(c(strata$Depth_m.1, strata$Depth_m.2)), h = unique(c(strata$Latitude_dd.1, strata$Latitude_dd.2)))})

# Save above xyplot() to the 'Figs' directory as a png
png(1000, 1000, file = paste0(FigDir, file = 'Raw_data_Lat_by_Depth_by_Year_by_Presence.png')) # Presence is First_stage_expanded_numbers != 0 (i.e. a length taken, not just a catch)
xyplot(Lat ~ -Depth_m | factor(Year), groups = as.logical(First_stage_expanded_numbers), data = LengthCompWithZero, 
       panel = function(...) { panel.xyplot(...); panel.abline(v = -unique(c(strata$Depth_m.1, strata$Depth_m.2)), h = unique(c(strata$Latitude_dd.1, strata$Latitude_dd.2)))})
dev.off()
       

# Using  < c_i = as.numeric(as.factor(LengthCompWithZero[,"Length_bin"])) - 1 > in fit.model(), which is based on character sorting, doesn't give the correct bin ordering for large animals of 100cm
#      or more [e.g. sort(c('10', '15', '20', '100')) gives c( "10", "100", "15", "20") ] :
   LengthCompWithZero$Length_bin_num <- as.numeric(as.factor(LengthCompWithZero[,"Length_bin"])) - 1
   (charSort <- renum(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin), c('Length_bin', 'Length_bin_num')][order(LengthCompWithZero$Length_bin[!duplicated(LengthCompWithZero$Length_bin)]), ]))
  
# Using JRWToolBox::recode.simple() to re-code correctly via 'Length_bin_num' (hacked together the 'ref_Table' using 'charSort' above, but 'ref_Table' could be created in anyway desired)
   
   # Change this 'Order' vector correctly so that 'Length_bin' goes from smallest to largest and Length_bin_num starts at zero and monotonically increases with ordinal numbers within sex
   if(numSexInModel %in% 1)
      Order <- c(7:23, 1:6)
   if(numSexInModel %in% 2)
      Order <- c(7:23, 1:6, 29:45, 24:28) # Females then males    
   (ref_Table <- data.frame (Length_bin = charSort$Length_bin[Order], Length_bin_num = 0:(length(Order) - 1)))
   LengthCompWithZero$Length_bin_num <- as.numeric(JRWToolBox::recode.simple(LengthCompWithZero$Length_bin, ref_Table))
   # Check Length_bin and Length_bin_num in LengthCompWithZero
   renum(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin), c('Length_bin', 'Length_bin_num')][order(LengthCompWithZero$Length_bin[!duplicated(LengthCompWithZero$Length_bin)]), ][Order, ])
   # Additional check, note that only 'Length_bin" should not be zero - since that's a character vector which is not given to VAST
   lapply(LengthCompWithZero, function(x) sum(!is.finite(x)))

   
# --- Save LengthCompWithZero in HomeDir ---
       
# write.csv(LengthCompWithZero,"LengthCompWithZero.csv")

if(numSexInModel %in% 1)
   save(LengthCompWithZero, file = paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '_sex', casefold(substring(sex, 2, 2), upper = TRUE), '.RData'))

if(numSexInModel %in% 2)
   save(LengthCompWithZero, file = paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '_sexMF.RData'))

   
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

# Load back in LengthCompWithZero from HomeDir, if needed
if(numSexInModel %in% 1)
   load(paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '_sex', casefold(substring(sex, 2, 2), upper = TRUE), '.RData'))

if(numSexInModel %in% 2) {
   load(file = paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '_sexMF.RData'))

   # Follow below to get single sex data from the 2 sex data frame with (years could also be easily reduced). 
   #    The directory names should then be switched with the code at the top of this script.

   #  # Females
   #  LengthCompWithZero <- renum(LengthCompWithZero[grepl('F', LengthCompWithZero$Length_bin), ])
   #  Table(LengthCompWithZero$Length_bin, LengthCompWithZero$Length_bin_num)
   #  
   #  # Males
   #  LengthCompWithZero <- renum(LengthCompWithZero[grepl('M', LengthCompWithZero$Length_bin), ])
   #  LengthCompWithZero$Length_bin_num <- LengthCompWithZero$Length_bin_num - min(LengthCompWithZero$Length_bin_num)
   #  Table(LengthCompWithZero$Length_bin, LengthCompWithZero$Length_bin_num)
}


# Extra removal of length bins with no or very little data for models that have convergence issues (LengthCompWithZero could be recreated above instead)
   # (numRowsOld <- nrow(LengthCompWithZero))
   # if(numSexInModel %in% 1)
   #     LengthCompWithZero <- LengthCompWithZero[!LengthCompWithZero$Length_bin %in% '10-15cm', ]
   # if(numSexInModel %in% 2) 
   #    LengthCompWithZero <- LengthCompWithZero[!LengthCompWithZero$Length_bin %in% c('F_10-15cm', 'M_10-15cm', 'M_130-135cm', 'M_135-140cm', 'M_140-145cm'), ]
   # # Percent reduction of rows
   #  100 * (1 - nrow(LengthCompWithZero)/numRowsOld)
       
# Remake 'ref_Table' with Length bins removed (used for plotting labels below)
(ref_Table <- JRWToolBox::sort.f(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin_num), c('Length_bin', 'Length_bin_num')], 'Length_bin_num'))
    
    
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
          FieldConfig = c(Omega1 = 'IID', Epsilon1 = 'IID', Omega2 = 'IID', Epsilon2 = 'IID'), 
            RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), 
 OverdispersionConfig = c(Eta1 = 0, Eta2 = 0),
       use_anisotropy = FALSE,
         bias.correct = FALSE,
            max_cells = 3000,
        strata.limits = strataLimits)
        


# Set max threads (or less if desired) if using R ver 4X with partially patched MRO on Windows (depending on the system, threads are often half the number of logical processors on a machine).        
RhpcBLASctl::blas_set_num_threads(RhpcBLASctl::get_num_cores()); RhpcBLASctl::blas_get_num_procs()
    
# Set a resonable number of threads if using MRO on a Linux server running CentOS (e.g. Tantalus), using (for now) R ver 3.5.3 
# setMKLthreads(6); getMKLthreads() 

# Set a resonable number of threads if using R-MKL on a Linux server running CentOS (e.g. Tantalus), if using R ver 4.X
# RhpcBLASctl::blas_set_num_threads(6); RhpcBLASctl::blas_get_num_procs()


# Run model  
sink("Fit_Output.txt")
fit <- FishStatsUtils::fit_model( 
  settings = settings, 
  Lat_i = LengthCompWithZero$Lat, 
  Lon_i = LengthCompWithZero$Lon,
    t_i = LengthCompWithZero$Year, 
    c_i = LengthCompWithZero$Length_bin_num,
    b_i = LengthCompWithZero$First_stage_expanded_numbers,
    a_i = LengthCompWithZero$AreaSwept_km2, 
  model_args = list(Npool = 200), newtonsteps = 1, test_fit = c(TRUE, FALSE)[2] )
sink()


# Max gradient
fit$parameter_estimates[['max_gradient']]

#AIC
fit$parameter_estimates[['AIC']]

VAST::check_fit(fit$parameter_estimates)

# Early save - done again below
# Save it all in Image.RData 
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))

                 
# Plot results 
results = FishStatsUtils::plot_results(fit = fit, settings = fit$settings, plot_set = 3, category_names = ref_Table$Length_bin, 
              strata_names = strataLimits$STRATA, check_residuals = FALSE, working_dir = FigDir)
 
# Do plot_range_index() (for Effective_Area.png) again by itself so strata_names are included [ doesn't get added properly in plot_results() ] 
#      plot_range_index() also recreates 'center_of_gravity.png' with no change but the 'Date modified' on the file properties
FishStatsUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, 
            Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), 
            PlotDir = FigDir, Year_Set = fit$year_labels, Years2Include = fit$years_to_plot, 
            use_biascorr = FALSE, category_names = ref_Table$Length_bin, strata_names = strataLimits$STRATA)

            
#  # If plot_biomass_index() fails, try setting 'treat_nonencounter_as_zero' to FALSE and re-try
#  # This fails when a length bin is empty over all years - which needs to be fixed, but at least the figures can be looked at.
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero']
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero'] <- c(TRUE, FALSE)[2]
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero']
 

# Calculate proportions 
proportions <- FishStatsUtils::calculate_proportion(TmbData = fit$data_list, Index = results$Index, Year_Set = fit$year_labels,
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


# Save it all in Image.RData [ When reloading, remember to dyn.load() the '.dll', e.g. dyn.load(paste0(DateDir, 'VAST_v9_2_0.dll')), if needed. ]
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))
 
 
 
# Summary of the fit
fitSummary <- summary(fit)  # FishStatsUtils:::summary.fit_model (The column names for the head of `Density_dataframe` are in the wrong place for the printing, as of 6 May 2020.)
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

 





