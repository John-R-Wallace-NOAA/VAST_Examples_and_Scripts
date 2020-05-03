# VAST Examples and Scripts

Download West_Coast_Annual_example_2020.R using:

    JRWToolBox::gitAFile("John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/West_Coast_Annual_example_2020.R", "script", File = "West_Coast_Annual_example_2019.R", show = FALSE)
    
or edit [using a properly configured gitEdit()] with:

    JRWToolBox::gitEdit(West_Coast_Annual_example_2020, "John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/")

-----------------------------------------------------------------------------------------

West_Coast_Annual_Exmpl_2020_FS.R:
 
    JRWToolBox::gitEdit(West_Coast_Example_2020_FS, "John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/")
  
Uses fine_scale = TRUE in VAST ver. 3X and JRWToolBox::YearlyResultsFigure_VAST3X(), but otherwise uses the mid-level functions approach.

-----------------------------------------------------------------------------------------

West_Coast_Annual_Exmpl_2020_V3X.R:

    JRWToolBox::gitEdit(West_Coast_Example_2020_V3X, "John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/")
  
Uses fine_scale = TRUE in VAST ver. 3X and JRWToolBox::YearlyResultsFigure_VAST3X(), following the upper level functions (wrappers) approach.

             Model                                                            AIC
             
    No Fine scale (No Pass; Gamma errors)                                   31,232.5
    Fine scale (No Pass; Gamma errors)                                      29,835.9
    With Pass  (FS; Gamma errors)                                           29,828.5
    Lognormal errors  (FS; Pass)                                            29,316.6
    Depth covariate, on data not grid (FS; Pass; LN errors)                 27,832.2
    Depth covarite, on data not grid, bs() spline (FS; Pass; LN errors)     27,459.2









