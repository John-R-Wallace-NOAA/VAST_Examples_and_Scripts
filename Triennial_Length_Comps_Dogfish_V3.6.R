
#===============================================================================
#============  Setup  ==========================================================
#===============================================================================

# Test run of length comps in VAST ver 3X (wrapper functions)
# Longnose skate data; initial following the VAST Wiki 'Expand age and length composition' example
# Revised by V. Gertseva, Feb 2020
# Revised by V. Gertseva & J. Wallace Mar 2020
# Revised by V. Gertseva Apr 2020
# Revised by J. Wallace Apr & May 2020

# R-MKL 4.0.1 on Tantalus
# /opt/R/64-bit/R-4.0.1_MKL/bin/R

library(JRWToolBox)
# install.packages("devtools")
library(lattice)

options(stringsAsFactors = FALSE)  # This is the now the default starting with R ver 4.0.0

(HomeDir <- paste0(getwd(), "/"))

if(.Platform$OS.type == "windows") {
   # HomeDir <- "C:/Users/Vladlena.Gertseva/Desktop/VAST_2020/2020 runs/"
   # HomeDir <- "W:/ALL_USR/JRW/Assessment/Length_Comps_VAST/"
}


# Linux server (e.g. Tantalus)
if(!.Platform$OS.type == "windows") {
   # Linux.First()  # Don't change the VAST version below
   options(width = 200)
}   

setwd(HomeDir); getwd()

# ---- Set number of threads----

# Set max threads (or less if desired) if using partially patched MRO ver 4.X on Windows [depending on the system, threads (or cores) are often half the number of logical processors on a machine].    
if(.Platform$OS.type == "windows" & any(installed.packages()[, 1] %in% "RhpcBLASctl"))  {  
    RhpcBLASctl::blas_set_num_threads(RhpcBLASctl::get_num_cores())
    RhpcBLASctl::blas_get_num_procs()
}
    
# Set max threads if using MRO on a Linux server running CentOS (e.g. Tantalus), if using R ver 3.X (setting the max too high results in too much IO, which is slow).
if(!.Platform$OS.type == "windows" & version$major == 3) {
   setMKLthreads(6)
   getMKLthreads()
}   

# Set max threads if using R-MKL on a Linux server running CentOS (e.g. Tantalus), if using R ver 4.X (setting the max too high results in too much IO, which is slow).
if(!.Platform$OS.type == "windows" & version$major == 4) {
    RhpcBLASctl::blas_set_num_threads(6)
    RhpcBLASctl::blas_get_num_procs()
}


#===============================================================================
#============    VAST & FishStatsUtils versions  ================================
#===============================================================================

# Package: Matrix  # CRAN
# Version: 1.2-18
# Date: 2019-11-25
# 

# Package: TMB                                                                                                                                                
# Title: Template Model Builder: A General Random Effect Tool Inspired by                                                                                       
#         'ADMB'                                                                                                                                                
# Version: 1.7.18                                                                                                                                               
# Date: 2020-07-24                                                                                                                                              
# RemoteSha: 9d2b772e6874ffa191cfce5f94fb959be0ba4f0f # OLD
# RemoteSha: a81ff89f5e65fae801ce0d2634cde575d460ee70 # Appears to be a SHA change without the version and date changing

# Package: VAST
# Title: Vector-autoregressive spatio-temporal (VAST) model
# Version: 3.4.0 & 3.6.0
# Date: 2020-02-27
# URL: http://github.com/James-Thorson/VAST
# RemoteSha: 942c89d5837a0c9023c637ee3bd5700760115bd6 # 3.4.0
# SHA <- '942c89d5837a0c9023c637ee3bd5700760115bd6' # 3.4.0
# RemoteSha: 65d7b9a174d4caa3e9e42a50a11ce15d858b4b11 # 3.6.0
SHA <- '65d7b9a174d4caa3e9e42a50a11ce15d858b4b11' # 3.6.0
if(packageDescription('VAST')$RemoteSha != SHA)
   devtools::install_github('james-thorson-noaa/VAST', ref = SHA, INSTALL_opts = "--no-multiarch --no-test-load --no-staged-install") 


# Package: FishStatsUtils
# Title: Utilities (shared code and data) for FishStats spatio-temporal modeling toolbox
# Version: 2.6.0 & 2.8.0
# Date: 2020-04-11
# URL: http://github.com/james-thorson/FishStatsUtils
# RemoteSha: f5b7f29096165ee330652b3afdf76a7c60148a5a # 2.6.0
# SHA <- 'f5b7f29096165ee330652b3afdf76a7c60148a5a'  # 2.6.0
# RemoteSha: 82cd3b5e07baa92d182c0746558575e0be597e52 # 2.8.0
SHA <- '82cd3b5e07baa92d182c0746558575e0be597e52'  # 2.8.0
if(packageDescription('FishStatsUtils')$RemoteSha != SHA)
   devtools::install_github('james-thorson-noaa/FishStatsUtils', ref = SHA, INSTALL_opts="--no-multiarch --no-test-load") 

   
library(TMB)   
library(VAST)

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

# Spiny Dogfish
spFormalName <- 'Pacific spiny dogfish' 
spShortName <- 'Dogfish'
spSciName <- 'Squalus acanthias'


#===============================================================================
#=============         Survey          =========================================
#===============================================================================

Survey <- 'AFSC.Shelf'

#===============================================================================
#=============         Years          ==========================================
#===============================================================================

yearRange <- c(1977, 2004)


#===============================================================================
#=============      Single sex or both          ================================
#===============================================================================

# Select if you want only one sex in the model or both - including both sexes doubles the number of rows of the data.
numSexInModel <- c(1, 2)[2]

if(numSexInModel %in% 1) {
    by_sex = "female"
    (sex = ifelse(by_sex == "female", "Nf", "Nm"))
    DateDir <- paste0(HomeDir, 'VAST_', Sys.Date(), '_', spShortName, '_Comps_', Survey, '_', yearRange[1], '_', yearRange[2], '_sex', casefold(substring(sex, 2, 2), upper = TRUE), '/')
}

if(numSexInModel %in% 2)
   DateDir <- paste0(HomeDir, 'VAST_', Sys.Date(), '_', spShortName, '_Comps_', Survey, '_', yearRange[1], '_', yearRange[2], '_sexMF/')

dir.create(DateDir, showWarnings = FALSE)
setwd(DateDir); getwd()

(FigDir <- paste0(DateDir, 'Figs/'))
dir.create(FigDir, showWarnings = FALSE) 
     

#===============================================================================
#=============      Data          ==============================================
#===============================================================================

# catch = PullCatch.fn(SciName = spSciName , SurveyName = Survey, YearRange = c(2012, 2016), SaveFile = TRUE, Dir = getwd()) 
catch <- JRWToolBox::dataWareHouseTrawlCatch(spFormalName, yearRange = yearRange, project = Survey)
catch$cpue_kg_km2 <- catch$Total_sp_wt_kg/(catch$Area_Swept_ha/100)
names(catch)[grep('Total_sp_numbers', names(catch))] <- 'total_catch_numbers'

# bio = PullBio.fn(SciName = spSciName, SurveyName = Survey, YearRange = c(2012, 2016), SaveFile = TRUE, Dir = getwd())
LenAge <- JRWToolBox::dataWareHouseTrawlBio(spFormalName, yearRange = yearRange, project = Survey)

if(any(Survey %in% c('AFSC.Shelf', 'AFSC.Slope'))) {
    bio <- LenAge$Lengths
    bio$Weight <- 1 
    Ages <- LenAge$Ages
} else
   bio <- LenAge

names(bio)[grep('Weight_kg', names(bio))] <- 'Weight'  # I added the units to the 'Weight_kg' and the PullBio.fn() is behind

head(catch)
head(bio)
if(any(Survey %in% c('AFSC.Shelf', 'AFSC.Slope')))
    head(Ages)

#  For Triennial Dogfish there is only enough lengths in years 1998, 2001, & 2004
catch <- catch[catch$Year %in% c(1998, 2001, 2004), ]   
bio <- bio[bio$Year %in% c(1998, 2001, 2004), ]   

# Conversion of fork lengths in 1998 to total length
bio$Length_cm[bio$Year %in% 1998] <- bio$Length_cm[bio$Year %in% 1998] * 1.07 + 1.22    

dev.new()
xyplot(cpue_kg_km2/100 ~ -Depth_m | factor(Year), groups = factor(sign(cpue_kg_km2)), data = catch, col = c('dodgerblue', col.alpha('magenta', 0.3))) # kgs per hectare

dev.new()
xyplot(log(cpue_kg_km2/100 + min(cpue_kg_km2/100, na.rm= TRUE)/2) ~ -Depth_m | factor(Year), data = catch, col = col.alpha('magenta', 0.3)) # kgs per hectare

dev.new()
xyplot(Length_cm ~ -Depth_m | factor(Year), data = bio) 


if(any(is.finite(bio$Width_cm))) {
   dev.new()
   xyplot(Width_cm ~ Length_cm | factor(Year), data = bio) # Longnose weight data only in years 2012 & 2016
   dev.new()
   histogram(~ Width_cm | factor(Year), data = bio)
} 

# Can pull data based on the general name (Name) of the scientific name(SciName). The default year range (YearRange)
# is set to cover all potential years.  The SurveyName options are: Triennial, AFSC.Slope, NWFSC.Slope, NWFSC.Shelf
# NWFSC.Combo, NWFSC.Hypoxia, NWFSC.Santa.Barb.Basin, or NWFSC.Video. These data pulls can also be saved to a specified 
# directory using SaveFile = TRUE and Dir = "directory to save file". 
# load("Catch_2018-08-06__NWFSC.Combo_2018-08-06.rda")


#  #===============================================================================
#  #=============          Width to Length Conversion          ====================
#  #===============================================================================
#  
#  #If Length_cm is missing but Width_cm has a value, then the following formula
#  
#  # Look at a table of missing lengths and of missing length vs width before applying the formula
#  cat("\n\n")
#  Table(is.finite(bio$Length_cm), bio$Year); cat("\n\n")
#  Table(is.finite(bio$Width_cm), bio$Year); cat("\n\n")
#  Table(is.finite(bio$Length_cm), is.finite(bio$Width_cm)); cat("\n\n")
#  # Length in rows, Width in columns
#  
#  
#  # Females
#  bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"]*1.402090 + 0.911707
#   
#  # Males
#  bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "M"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "M"]*1.405850 + 0.523354
#   
#  # Unsexed
#  bio$Length_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "U"] <- bio$Width_cm[is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "U"]*1.404374 + 0.700501
#    
#  # Look at a table of missing lengths and of missing length vs width after applying the formula
#  cat("\n\n")
#  Table(is.finite(bio$Length_cm), bio$Year); cat("\n\n")
#  Table(is.finite(bio$Width_cm), bio$Year); cat("\n\n")
#  Table(is.finite(bio$Length_cm), is.finite(bio$Width_cm)); cat("\n\n")
#  # Length in rows, width in columns
#  
#  
#  #Examples how to shorten formulas 
#  # TF <- is.na(bio$Length_cm) & !is.na(bio$Width_cm) & bio$Sex %in% "F"
#  # bio$Length_cm[TF] <- bio$Width_cm[TF]*1.4102 + 9.8281

 
#===============================================================================
#=============  Create Stratafication and Design Based Index   =================
#===============================================================================

# The stratafication areas are calculated from the SA3 file which is attached to the package.
# The 'stratum' column in 'stage_one' is not used below in the approach taken here; where the stage one results are used as input into VAST, so the contents of 'strata' are not important.
# (strata <- nwfscSurvey::CreateStrataDF.fn(names=c("shallow_CA", "mid_CA", "deep_CA", "shallow_OR", "mid_OR", "deep_OR", "shallow_WA", "mid_WA", "deep_WA"), 
#                                    depths.shallow = c(55,   183, 549,  55,   183, 549,  55,   183, 549),
#                                    depths.deep    = c(183,  549, 1280, 183,  549, 1280, 183,  549, 1280),
#                                    lats.south     = c(32,    32,   32,  42,   42,   42,  46,   46,	 46),
#                                    lats.north     = c(42,    42,   42,  46,   46,   46,  49,   49,	 49)))
                                   
(strata <- nwfscSurvey::CreateStrataDF.fn(names=c("shallow_CA", "mid_CA", "shallow_OR", "mid_OR", "shallow_WA", "mid_WA"), 
                                   depths.shallow = c(55,   183, 55,   183,  55,   183),
                                   depths.deep    = c(183,  549, 183,  549,  183,  549),
                                   lats.south     = c(32,    32,  42,   42,   46,   46),
                                   lats.north     = c(42,    42,  46,   46,   49,   49)))
                                   
                                   
# Calculate the design based index
# Creates a csv file within the "printfolder" that will be saved within the directory location (dir).
biomass = nwfscSurvey::Biomass.fn(dir = getwd(), dat = catch,  strat.df = strata, printfolder = "forSS", outputMedian = TRUE) 


# Look at strata estimate tables - shows number of tows by year and strata
lapply(biomass$StrataEsts, print)


# Plot the biomass index with confidence intervals
dev.new()
nwfscSurvey::PlotBio.fn(dir = getwd(), dat = biomass, main = paste(Survey, "bottom trawl survey"), dopng = FALSE)
nwfscSurvey::PlotBio.fn(dir = getwd(), dat = biomass, main = paste(Survey, "bottom trawl survey"), dopng = TRUE)


#============================================================================================
#=============          Length Data            ==============================================
#============================================================================================

bin_size = 4
(len.bins = seq(12, 132, by = bin_size))

# Calculate the effN
(n = nwfscSurvey::GetN.fn(dir=getwd(), dat = bio, type = "length", species = "others", printfolder = "forSS"))
# The GetN.fn calculated input sample sizes based on Hamel & Stewart bootstrap approach.



# Expand and format length composition data for SS

# The code offers two options for applying the sex ratio based on expansion stage. The sex ratio will be
# applied based on a tow basis first if sexRatioStage = 1. The other option applies the sex ratio to the
# expanded numbers of fish across a whole strata (sexRatioStage = 2, this was the option applied to the
# NWFSC combo survey data in the past).

# #stage_one <- nwfscSurvey::SurveyLFs.fn(dir = getwd(), datL = bio, datTows = catch,  
# #                    strat.df = strata, lgthBins = len.bins, gender = 3, 
# #                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 20, outputStage1 = TRUE,
# #                    nSamps = n)
# #       

# Added fake weight column above for AK Surveys to make SurveyLFs.fn() happy (weight is not used in the function)
stage_one <- nwfscSurvey::SurveyLFs.fn(dir = getwd(), datL = bio, datTows = catch, strat.df = strata, lgthBins = len.bins,
                    sexRatioStage = 2, sexRatioUnsexed = 0.5, maxSizeUnsexed = 20, outputStage1 = TRUE, nSamps = n)
                    
# The below conversion is just done to put the area fished on the same scale as the lingcod example, kg per hectare to kg per square kilometer
stage_one$areaFished = stage_one$areaFished / 10000   # areaFished is dropped below - however, it could be used as a check
                    
# Create the length bins names
lo = len.bins[findInterval(stage_one$Length_cm, len.bins, left.open = TRUE)]
hi = lo + bin_size
stage_one$Length_bin = paste0(lo, "-", hi, "cm")


# The comps are calculated seperately by sex
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
LenCompNoZero.All.Bins <- LenCompNoZero # Save the original to revert back to if needed
JRWToolBox::r(JRWToolBox::agg.table(aggregate(list(Num = LenCompNoZero.All.Bins$First_stage_expanded_numbers), list(Length_bin = LenCompNoZero.All.Bins$Length_bin, 
                Year = LenCompNoZero.All.Bins$Year), sum), Print = FALSE), 2)

# No or little data for some length bins, so remove them
(numRowsOld <- nrow(LenCompNoZero))
if(numSexInModel %in% 1)
    LenCompNoZero <- LenCompNoZero[!LenCompNoZero$Length_bin %in% '10-15cm', ]
if(numSexInModel %in% 2)
    LenCompNoZero <- LenCompNoZero[!LenCompNoZero$Length_bin %in% c('M_96-100cm', 'M_100-104cm', 'M_104-108cm', 'M_108-112cm', 'M_112-116cm'), ]
# Percent reduction of rows
round(100 * (1 - nrow(LenCompNoZero)/numRowsOld), 4)

# Check LenCompNoZero has the correct bins removed
JRWToolBox::r(JRWToolBox::agg.table(aggregate(list(Num = LenCompNoZero$First_stage_expanded_numbers), list(Length_bin = LenCompNoZero$Length_bin, 
                 Year = LenCompNoZero$Year), sum)), 2)


# Add length bin from LenCompNoZero file to hauls with zero catches from catch file (switching cols after indexing avoids sorting)
LengthCompWithZero <- expand.grid(Length_bin = unique(LenCompNoZero$Length_bin), Trawl_id = catch$Trawl_id, stringsAsFactors = FALSE)[, c("Trawl_id", "Length_bin")] 

# Match each length bin in each haul to lat, long and area_swept_ha
LengthCompWithZero <- JRWToolBox::match.f(LengthCompWithZero, catch, "Trawl_id", "Trawl_id", c("Year", "Longitude_dd", "Latitude_dd", "Depth_m", "Area_Swept_ha"))

# Add column First_stage_expanded_numbers to file with zero and positive catches
LengthCompWithZero <- JRWToolBox::match.f(LengthCompWithZero, LenCompNoZero, c("Trawl_id", "Length_bin"), c("Trawl_id", "Length_bin"), "First_stage_expanded_numbers")

# Change NA First_stage_expanded_numbers for zero catch hauls to 0
LengthCompWithZero$First_stage_expanded_numbers[is.na(LengthCompWithZero$First_stage_expanded_numbers)] <- 0

# Create column AreaSwept_km2 and remove column Area_Swept_ha
LengthCompWithZero$AreaSwept_km2 <- LengthCompWithZero$Area_Swept_ha/100
LengthCompWithZero$Area_Swept_ha <- NULL

# Change location of columns
LengthCompWithZero <- renum(LengthCompWithZero[, c("Trawl_id", "Year", "Latitude_dd", "Longitude_dd", "Depth_m", "AreaSwept_km2", "First_stage_expanded_numbers", "Length_bin")])
LengthCompWithZero[1:10,]

# Change names of columns
names(LengthCompWithZero)[grep("Latitude_dd", names(LengthCompWithZero))] <- "Lat"
names(LengthCompWithZero)[grep("Longitude_dd", names(LengthCompWithZero))] <- "Lon"

LengthCompWithZero$Trawl_id <- NULL
LengthCompWithZero[1:10,]

# Check LengthCompWithZero
JRWToolBox::agg.table(aggregate(list(Num = LengthCompWithZero$First_stage_expanded_numbers), list(Length_bin = LengthCompWithZero$Length_bin, Year = LengthCompWithZero$Year), sum))

# Check dimensions
length(unique(LengthCompWithZero$Length_bin))
nrow(LengthCompWithZero)/nrow(catch)

# Number of First_stage_expanded_numbers that are not equal to zero by year
Table(!LengthCompWithZero$First_stage_expanded_numbers %in% 0, LengthCompWithZero$Year)

# Histogram of log of First_stage_expanded_numbers by year
dev.new()       
histogram( ~ log(First_stage_expanded_numbers) | factor(Year), data = LengthCompWithZero)     

# Figure of latitude by depth by year
dev.new()
xyplot(jitter(Lat, 100) ~ -Depth_m | factor(Year), groups = as.logical(First_stage_expanded_numbers), data = LengthCompWithZero, 
       panel = function(...) { panel.xyplot(...); panel.abline(v = -unique(c(strata$Depth_m.1, strata$Depth_m.2)), h = unique(c(strata$Latitude_dd.1, strata$Latitude_dd.2)))})


# Save above xyplot() to the 'Figs' directory as a png
png(1000, 1000, file = paste0(FigDir, file = 'Raw_data_Lat_by_Depth_by_Year_by_Presence.png')) # Presence is First_stage_expanded_numbers != 0 (i.e. a length taken, not just a catch)
xyplot(jitter(Lat, 100) ~ -Depth_m | factor(Year), groups = as.logical(First_stage_expanded_numbers), data = LengthCompWithZero, 
       panel = function(...) { panel.xyplot(...); panel.abline(v = -unique(c(strata$Depth_m.1, strata$Depth_m.2)), h = unique(c(strata$Latitude_dd.1, strata$Latitude_dd.2)))})
dev.off()
       

# Using  < c_i = as.numeric(as.factor(LengthCompWithZero[,"Length_bin"])) - 1 > in fit.model(), which is based on character sorting, doesn't give the correct bin ordering for large animals of 100cm
#      or more [e.g. sort(c('10', '15', '20', '100')) gives c( "10", "100", "15", "20") ] :
   LengthCompWithZero$Length_bin_num <- as.numeric(as.factor(LengthCompWithZero[,"Length_bin"])) - 1
   (charSort <- renum(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin), c('Length_bin', 'Length_bin_num')][order(LengthCompWithZero$Length_bin[!duplicated(LengthCompWithZero$Length_bin)]), ]))
  
# Using JRWToolBox::recode.simple() to re-code correctly via 'Length_bin_num' (hacked together the 'ref_Table' using 'charSort' above, but 'ref_Table' could be created in anyway desired)
   
   # Change this 'Order' vector correctly so that 'Length_bin' goes from smallest to largest and Length_bin_num starts at zero and monotonically increases with ordinal numbers within sex
   # 'Order' below is also used in the pre-VAST and VAST results 3D perspective plots below  
   if(numSexInModel %in% 1)
      Order <- c(7:23, 1:6)
   if(numSexInModel %in% 2)
      Order <- c(5:24, 1:4, 25:43) # Females then males 
   (ref_Table <- data.frame (Length_bin = charSort$Length_bin[Order], Length_bin_num = 0:(length(Order) - 1)))
   LengthCompWithZero$Length_bin_num <- as.numeric(JRWToolBox::recode.simple(LengthCompWithZero$Length_bin, ref_Table))
   
   # Check Length_bin and Length_bin_num in LengthCompWithZero
   renum(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin), c('Length_bin', 'Length_bin_num')][order(LengthCompWithZero$Length_bin[!duplicated(LengthCompWithZero$Length_bin)]), ][Order, ])
   
   # Additional check, note that only 'Length_bin" should not be zero - since that's a character vector which is not given to VAST
   lapply(LengthCompWithZero, function(x) sum(!is.finite(x)))
   
   # !!!!! Make sure there are no NA's in Length_bin_num !!!!!
   all(!is.na(LengthCompWithZero$Length_bin_num))
   dim(LengthCompWithZero)[1] == dim(na.omit(LengthCompWithZero))[1]


   
# --- Save LengthCompWithZero in HomeDir ---
       
# write.csv(LengthCompWithZero,"LengthCompWithZero.csv")

if(numSexInModel %in% 1)
   save(LengthCompWithZero, file = paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '_sex', casefold(substring(sex, 2, 2), upper = TRUE), '.RData'))

if(numSexInModel %in% 2)
   save(LengthCompWithZero, file = print(paste0(HomeDir, 'LengthCompWithZero_', yearRange[1], '_', yearRange[2], '_sexMF.RData')))

 
# --- Bubble plot figure of LengthCompWithZero First_stage_expanded_numbers ---
JRWToolBox::lib("John-R-Wallace-NOAA/Imap")
png(1000, 1000, file = paste0(FigDir, "LengthCompWithZero Bubble Plot.png"))
Imap::imap(longlat = list(world.h.land, world.h.borders), col = c("black", "cyan"), fill = TRUE, poly = c("grey40", NA), longrange = c(-131.3, -115.5), latrange = c(33, 50.3), zoom = FALSE)
JRWToolBox::plot.bubble.zero.cross(LengthCompWithZero[, c('Lon', 'Lat', 'First_stage_expanded_numbers')], group = LengthCompWithZero$Year, add = TRUE, legUnits = "Numbers", 
                    verbose = TRUE, xDelta = -2, cross.cex = 0)
dev.off()
  
 
# ------ 3D wireframe figure of stage one length comp data with zeros (before VAST is used) - ### check sorting wrong for character number labels ------
   require(lattice)
   
   LengthCompWithZero$Fleet <- JRWToolBox::factor.f(LengthCompWithZero$Lat, breaks = c(32, 42, 46, 49), labels = c("CA", "OR", "WA")) 
   
   # Check the range and sums
   # range(LengthCompWithZero$Lat[LengthCompWithZero$Fleet %in% 'WA'])
   # range(LengthCompWithZero$Lat[LengthCompWithZero$Fleet %in% 'OR'])
   # range(LengthCompWithZero$Lat[LengthCompWithZero$Fleet %in% 'CA'])
   # aggregate(JRWToolBox::List(LengthCompWithZero$First_stage_expanded_numbers), JRWToolBox::List(LengthCompWithZero$Fleet), sum)
   # aggregate(JRWToolBox::List(First_stage_expanded_numbers), JRWToolBox::List(Length_bin, Fleet), sum)
   
   (FLEETS <- list(c("CA", "OR", "WA"), "CA", "OR", "WA"))
   
   SpinyDogRawNumByLen <- NULL
   for (i in 1:4) {  # 4 "Fleets"
   
     TF <- LengthCompWithZero$Fleet %in% FLEETS[[i]]
     SpinyDogRawNumByLen <- rbind(SpinyDogRawNumByLen, cbind(aggregate(list(Numbers = LengthCompWithZero$First_stage_expanded_numbers[TF]), 
                             list(Length_bin = LengthCompWithZero$Length_bin[TF], Year = LengthCompWithZero$Year[TF]), sum, na.rm = TRUE), Fleet = ifelse(i == 1, "Coastwide", FLEETS[[i]])))
   }
   
   # Add in years without a survey
   Sp.2004 <- SpinyDogRawNumByLen[SpinyDogRawNumByLen$Year %in% 2004, ]
   Sp.2004$Numbers <- 0
   Sp.1999 <- Sp.2000 <- Sp.2002 <- Sp.2003 <- Sp.2004
   Sp.1999$Year <- 1999
   Sp.2000$Year <- 2000
   Sp.2002$Year <- 2002
   Sp.2003$Year <- 2003
   
   SpinyDogRawNumByLen <- rbind(SpinyDogRawNumByLen, Sp.1999, Sp.2000, Sp.2002, Sp.2003)
   SpinyDogRawNumByLen <- JRWToolBox::sort.f(SpinyDogRawNumByLen, c('Fleet', 'Year', 'Length_bin'))
   
   # Fix sorting for character number labels 
   SpinyDogRawNumByLen$Len_Female_Male <- as.numeric(ordered(SpinyDogRawNumByLen$Length_bin, levels = levels(factor(SpinyDogRawNumByLen$Length_bin))[Order]))
   
   # Check the ordering
   JRWToolBox::Table(SpinyDogSS3$Len_Female_Male, SpinyDogSS3$Category)  # Check ordering
   
   png(1000, 1000, file = paste0(FigDir, "3D Wireframe, Stage One Len Freq in Numbers by Year & Length, pre-VAST.png"))
   wireframe(Numbers ~ Year * Len_Female_Male | ordered(Fleet, c('Coastwide', 'CA', 'OR', 'WA')) , data = SpinyDogRawNumByLen, as.table = TRUE) # Use screen = list(z = 80, x = -60) for a different perspective
   dev.off()
    

# Early save before VAST
# Save it all in Image.RData 
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))
   
   
#===============================================================================
#=============          VAST         ===========================================
#===============================================================================

# Load packages (needed if the above code was skipped)
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
       
# Remake 'ref_Table' if length bins with no or very little data may have been removed. Also as a last check - the names are used for plotting labels below.
# print(ref_Table <- JRWToolBox::sort.f(LengthCompWithZero[!duplicated(LengthCompWithZero$Length_bin_num), c('Length_bin', 'Length_bin_num')], 'Length_bin_num'))
    
    
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
                
settings <- FishStatsUtils::make_settings( n_x = 300, Region = "California_current", purpose = "index2",  
           fine_scale = TRUE, 
             ObsModel = c(1, 0),  # 1 = Lognormal; 2 = Gamma
          FieldConfig = c(Omega1 = 'IID', Epsilon1 = 'IID', Omega2 = 'IID', Epsilon2 = 'IID'), 
            RhoConfig = c(Beta1 = 0,  Beta2 = 0, Epsilon1 = 0, Epsilon2 = 0), 
 OverdispersionConfig = c(Eta1 = 0, Eta2 = 0),
       use_anisotropy = FALSE,
         bias.correct = FALSE,
            max_cells = 3000,
        strata.limits = strataLimits)
 
# Double check max threads
# RhpcBLASctl::blas_set_num_threads(6)
RhpcBLASctl::blas_get_num_procs()  # Use 6 for Tantalus (a Linux server) 

# Run model  - run with newtonsteps = 0 until good convergence is seen in all parameters
# ***** TMB and FishStatsUtils need to be in search path - or make_data() will not be found *****
# ################# Note, since the input into b_i is in numbers all the results and figures are in numbers, regardless of label given. ################# 
sink("Fit_Output.txt")
  fit <- FishStatsUtils::fit_model( 
      settings = settings, 
         Lat_i = LengthCompWithZero$Lat, 
         Lon_i = LengthCompWithZero$Lon,
           t_i = LengthCompWithZero$Year, 
          c_iz = LengthCompWithZero$Length_bin_num,
           b_i = LengthCompWithZero$First_stage_expanded_numbers,
   #      c_iz = matrix(rep(0, length(b_i)), ncol = 1),  # c_iz needs to be a matrix for VAST 3.6 or?? v_i needs to be back to its VAST 3.4 default
   #      c_iz = rep(0, length(b_i)),                    # c_iz default for VAST 3.4
   #       v_i = rep(0, length(b_i)),                    # v_i defalut for VAST 3.4
           a_i = LengthCompWithZero$AreaSwept_km2, 
    model_args = list(Npool = 200), 
   newtonsteps = c(0, 1)[1], 
      test_fit = c(TRUE, FALSE)[2] )
sink()


# Compare number of categories with ref_Table number of rows: 
fit$data_list$n_c
ref_Table

# Positive Definite Hessian
fit$parameter_estimates$SD$pdHess

# Max gradient
fit$parameter_estimates[['max_gradient']]

#AIC
fit$parameter_estimates[['AIC']]

# Check fit
VAST::check_fit(fit$parameter_estimates, check_gradients = TRUE)

# Max gradient of fixed effects
max(abs(fit$parameter_estimates$SD$gradient.fixed))
length(fit$parameter_estimates$SD$gradient.fixed)

# Max of final gradient
dim(fit$parameter_estimates$diagnostics)
fit$parameter_estimates$diagnostics[1:5, ]
max(abs(fit$parameter_estimates$diagnostics$final_gradient))


# Early save after long model - done again below with proportions info
# Save it all in Image.RData 
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))

                 
# Plot results 

results = FishStatsUtils::plot_results(fit = fit, settings = fit$settings, plot_set = 3, category_names = ref_Table$Length_bin, 
              strata_names = strataLimits$STRATA, check_residuals = FALSE, working_dir = FigDir)
              
# Default category_names of just numbers
# results = FishStatsUtils::plot_results(fit = fit, settings = fit$settings, plot_set = 3,        
#              strata_names = strataLimits$STRATA, check_residuals = FALSE, working_dir = FigDir)              
 
# Do plot_range_index() (for Effective_Area.png) again by itself so strata_names are included [ doesn't get added properly in plot_results() ] 
#      plot_range_index() also recreates 'center_of_gravity.png' with no change but the 'Date modified' on the file properties
FishStatsUtils::plot_range_index(Report = fit$Report, TmbData = fit$data_list, 
            Sdreport = fit$parameter_estimates$SD, Znames = colnames(fit$data_list$Z_xm), 
            PlotDir = FigDir, Year_Set = fit$year_labels, Years2Include = fit$years_to_plot, 
            use_biascorr = settings$bias.correct, category_names = ref_Table$Length_bin, strata_names = strataLimits$STRATA)

            
#  # If plot_biomass_index() fails, try setting 'treat_nonencounter_as_zero' to FALSE and re-try
#  # This fails when a length bin is empty over all years - which needs to be fixed, but at least the figures can be looked at.
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero']
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero'] <- c(TRUE, FALSE)[2]
#  fit$data_list$Options_list$Options['treat_nonencounter_as_zero']
 

# Calculate proportions (proportions() is the name of a base function)
Proportions <- FishStatsUtils::calculate_proportion(TmbData = fit$data_list, Index = results$Index, Year_Set = fit$year_labels,
                  strata_names = strataLimits$STRATA, DirName = FigDir)

names(Proportions)
#  [1] "Prop_ctl"     "Neff_tl"      "var_Prop_ctl" "Index_tl"     "Neff_ctl"     "Mean_tl"    "sd_Mean_tl"  

dimnames(Proportions$Neff_tl) <- list(fit$year_labels, strataLimits$STRATA)
JRWToolBox::r(Proportions$Neff_tl, 3)

# Gamma
# #      Coastwide      CA     OR      WA
# # 1998    34.246  34.142  3.294   2.762
# # 1999        NA      NA     NA      NA
# # 2000        NA      NA     NA      NA
# # 2001   616.356 482.221 64.470 287.933
# # 2002        NA      NA     NA      NA
# # 2003        NA      NA     NA      NA
# # 2004   421.336 266.494 58.078 221.196

# Log-normal
# #      Coastwide      CA     OR      WA
# # 1998    28.302  28.252  3.439   2.934
# # 1999        NA      NA     NA      NA
# # 2000        NA      NA     NA      NA
# # 2001   437.383 281.680 96.420 277.692
# # 2002        NA      NA     NA      NA
# # 2003        NA      NA     NA      NA
# # 2004   370.639 292.585 71.548 213.073


dimnames(Proportions$Index_tl) <- list(fit$year_labels, strataLimits$STRATA)
dimnames(Proportions$Mean_tl) <- list(fit$year_labels, strataLimits$STRATA)
dimnames(Proportions$sd_Mean_tl) <- list(fit$year_labels, strataLimits$STRATA)


dim(Proportions$Neff_ctl)
# # n_c n_t n_l 
# #  43   7   4 

dimnames(Proportions$Neff_ctl) <- list(ref_Table$Length_bin, fit$year_labels, strataLimits$STRATA)
dimnames(Proportions$Prop_ctl) <- list(ref_Table$Length_bin, fit$year_labels, strataLimits$STRATA) 
dimnames(Proportions$var_Prop_ctl) <- list(ref_Table$Length_bin, fit$year_labels, strataLimits$STRATA) 


sink("Proportions.txt")
  cat("\nNeff_tl\n\n"); JRWToolBox::r(Proportions$Neff_tl, 2)
  cat("\n\nIndex_tl\n\n"); JRWToolBox::r(Proportions$Index_tl, 2)  
  cat("\n\nMean_tl\n\n"); JRWToolBox::r(Proportions$Mean_tl, 3)
  cat("\n\nsd_Mean_tl\n\n"); JRWToolBox::r(Proportions$sd_Mean_tl, 3)
  cat("\n\nNeff_ctl\n\n"); JRWToolBox::r(Proportions$Neff_ctl, 2)
  cat("\n\nProp_ctl\n\n"); JRWToolBox::r(Proportions$Prop_ctl, 5)
  cat("\n\nvar_Prop_ctl\n\n"); JRWToolBox::r(Proportions$var_Prop_ctl, 6)
sink()

# Look at Proportions.txt
file.show('Proportions.txt')


save(Proportions, file = paste0(DateDir, "Proportions.RData"))
  
# Save it all in Image.RData [ When reloading, remember to dyn.load() the '.dll', e.g. dyn.load(paste0(DateDir, 'VAST_v9_2_0.dll')), if needed. ]
save(list = names(.GlobalEnv), file = paste0(DateDir, "Image.RData"))
  

# 3D wireframe figure of data from Table for SS3 - ### check sorting wrong for character number labels ###
require(lattice)
SpinyDogSS3 <- read.csv(paste0(FigDir, "Table_for_SS3.csv"))
SpinyDogSS3$Len_Female_Male <- as.numeric(ordered(SpinyDogSS3$Category, levels = levels(factor(SpinyDogSS3$Category))[Order]))  # Fix sorting for character number labels
JRWToolBox::Table(SpinyDogSS3$Len_Female_Male, SpinyDogSS3$Category)  # Check ordering
names(SpinyDogSS3)[grep('Estimate_metric_tons', names(SpinyDogSS3))] <- "Numbers"  #  b_i arg to FishStatsUtils::fit_model() is in numbers, so this is estimated numbers not biomass!!!
png(1000, 1000, file = paste0(FigDir, "3D Wireframe, Len Freq in Numbers by Year & Length, VAST result.png"))
wireframe(Numbers ~ Year * Len_Female_Male | ordered(Fleet, c('Coastwide', 'CA', 'OR', 'WA')) , data = SpinyDogSS3, as.table = TRUE)
dev.off()


# ---- "`summary.fit_model` not implemented for the version of `VAST` being used"  - this is ver 3.6 - does work for VAST ver 3.4 ---
    
if(FALSE) {
    
   # Summary of the fit
   fitSummary <- summary(fit)  # FishStatsUtils:::summary.fit_model (The column names for the head of `Density_dataframe` are in the wrong place for the printing, as of 6 May 2020.)
   
   names(fitSummary)
   # [1] "extrapolation_grid" "Density_array"      "Density_dataframe" 
   
   fitSummary$Density_dataframe[1:4,]
   
   #        Grid   Category Year           Lon          Lat     Area_km2        Density
   # Grid_1    1 Category_1 2012 -123.30043471 37.885426431 44.000000000 0.021060901214
   # Grid_2    2 Category_1 2012 -124.31341614 45.279743723 40.000000000 0.020172511080
   # Grid_3    3 Category_1 2012 -118.21626007 32.020783864 29.352266256 0.021803228646
   # Grid_4    4 Category_1 2012 -119.60155711 33.130884471 64.000000000 0.021618393629
   
   dim(fitSummary$Density_dataframe)  
   
   # [1] 816000      7
   
   
   fitSummary$extrapolation_grid[1:4,]
   
   #                  Lon          Lat     Area_km2
   # Grid_1 -123.30043471 37.885426431 44.000000000
   # Grid_2 -124.31341614 45.279743723 40.000000000
   # Grid_3 -118.21626007 32.020783864 29.352266256
   # Grid_4 -119.60155711 33.130884471 64.000000000
   
   dim(fitSummary$extrapolation_grid)
   # [1] 3000    3
   
   
   fitSummary$Density_array[1:4,,1, drop = FALSE]
   
   # , , 2012
   # 
   #            Category_1    Category_2    Category_3    Category_4    Category_5   Category_6   Category_7   Category_8   Category_9   Category_10   Category_11    Category_12   Category_13
   # Grid_1 0.021060901214 0.43330827519 0.42152156507 0.84411284614 0.94044844812 2.6892736796 3.1051424245 3.7929436193 3.3492906133 0.67465888825 0.18132369860 0.150629692559 0.63355440099
   # Grid_2 0.020172511080 0.11834486238 0.14355951187 0.29688961670 1.11440013153 6.6373823565 8.7343315632 3.6286680282 7.7962162992 0.80417447207 0.18816793618 0.092196818140 0.07141016817
   # Grid_3 0.021803228646 0.18677562710 0.28552814743 0.68840659981 0.98837120406 1.8551475195 1.6135455701 1.5922570413 1.5822842010 0.59144362956 0.09882923000 0.030229743004 0.10528033452
   # Grid_4 0.021618393629 0.18497325628 0.28405258065 0.65881917819 0.97408216864 1.9654795541 1.6135669328 1.4514925941 1.5392328135 0.68177814903 0.11443508069 0.029043189657 0.11524136267
   #          Category_14   Category_15   Category_16   Category_17    Category_18    Category_19     Category_20   Category_21   Category_22   Category_23   Category_24   Category_25  Category_26
   # Grid_1 4.46161033755 3.96584647991 5.73588724686 6.21824229678 1.610888366693 0.370152295507 0.0852916504049 1.04103688255 1.43449369263 0.79508749722 3.18993778274 2.51855345087 6.7388947029
   # Grid_2 0.25164845787 0.86090700143 0.13054797110 1.23607144682 0.076685663210 0.045818295326 0.0032561000814 1.19979229994 0.38427863837 1.29430017538 0.65531911885 2.30316606420 7.6171102351
   # Grid_3 0.16939747544 0.40066632807 0.25016844041 0.36017493692 0.095757356028 0.054042911075 0.0034511753024 0.53328177895 0.74955746181 0.76753816484 1.16112378377 1.00843345064 1.6830037545
   # Grid_4 0.16227042166 0.37269748827 0.24717542843 0.36361033660 0.092770852280 0.053947669090 0.0034348598659 0.52159004332 0.75747345014 0.85140367548 1.28091353870 0.91081399464 1.5044928912
   #         Category_27   Category_28   Category_29   Category_30   Category_31   Category_32   Category_33    Category_34
   # Grid_1 1.7802484489 1.21553349545 4.36561619101 1.85139261845 0.64777127306 2.27253219648 1.56417726291 0.059509995655
   # Grid_2 3.5681706548 2.95085717434 1.96051481205 0.94153171057 0.44549072901 1.15082353392 0.38198915164 0.563173679807
   # Grid_3 1.2658822797 0.96112997064 0.98945610977 0.59957806384 0.35100802377 0.41532884329 0.16204811149 0.057641157164
   # Grid_4 1.1454199607 0.90352355767 0.91344897102 0.55703231452 0.29628386181 0.38139277744 0.15156582101 0.062906846497
   # 
   
   dim(fitSummary$Density_array)
   # [1] 3000   34    8
                            
}

                            
setwd(HomeDir); getwd()                         
        
      

# No ages for Spiny dogfish                         

                       
#============================================================================================
#=============     Age Biological Data      =================================================
#============================================================================================


age.bins = 1:24

n = GetN.fn(dir = getwd(), dat = age, type = "age", species = "shelfrock", printfolder = "forSS")

# Exand and format the marginal age composition data for SS
agesAgeFreg <- SurveyAFs.fn(dir = getwd(), datA = Ages, datTows = catch,  
                     strat.df = strata, ageBins = age.bins, 
                     sexRatioStage = 2, sexRatioUnsexed = 0.50, maxSizeUnsexed = 5, 
                     gender = 3, nSamps = n)



PlotFreqData.fn(dir = getwd(), dat = agesAgeFreg, survey = "NWFSCBT", ylim=c(0, max(age.bins) + 2), yaxs="i", ylab="Age (yr)", dopng=TRUE)
PlotVarLengthAtAge.fn(dir = getwd(), dat = Ages, survey ="NWFSCBT", dopng = TRUE) 
PlotSexRatio.fn(dir = getwd(), dat = Ages, data.type = "age", survey = "NWFSCBT", dopng = TRUE, main = "NWFSCBT")

#============================================================================================
# Conditional Ages
#============================================================================================
agesAgeAtLen <- SurveyAgeAtLen.fn (dir = getwd(), datAL = Ages, datTows = catch, 
                          strat.df = strata, lgthBins = len.bins, ageBins = age.bins, partition = 0)

 


if(FALSE) {

# Skipping plot of geometric anisotropy because it has been turned off
# 
# ### Making plot of abundance index
# Error in (function (TmbData, Sdreport, Year_Set = NULL, Years2Include = NULL,  : 
#   `category_names` must have same length as `TmbData$n_c`
 

}



