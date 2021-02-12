
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
   
sourceFunctionURL("https://raw.githubusercontent.com/John-R-Wallace-NOAA/VAST_Examples_and_Scripts/master/R/West_Coast_2021_V3.6.1.R")
   
(strata.limits <- data.frame(
       STRATA = c("Coastwide","CA","OR","WA"),
       north_border = c(49.0, 42.0, 46.0, 49.0),
       south_border = c(34.0, 34.0, 42.0, 46.0),
       shallow_border = c(55, 55, 55, 55),
       deep_border = c(1280, 1280, 1280, 1280)
       ))
   
West_Coast_2021_V3.6.1(spFormalName = 'Pacific spiny dogfish', spLongName = 'Spiny dogfish', spShortName = 'DSRK', Survey = 'AFSC.Slope', 
                 yearRange = c(1997, 2001), ObsModel = c(1, 0)) 
                 
 
 
