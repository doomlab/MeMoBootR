##set up for github
install.packages("roxygen2")

##set working direction to create stuff
#setwd("~/Downloads")

##create the folder for everything
devtools::create("MeMoBootR")

##open that folder
    ##create a man folder within the folder you just made
    ##empty R folder gets the function files
    ##fix the description file
        ##you can open this in R - edit the information in this file to be correct (use license() to get the current license)
license()

##at the beginning of each function you add special code
    #' Name of the Function
    #'
    #' Description of the function
    #' @param several lines of this that describes the arguments so you put the argument name, then space, then describe the argument
    #' @keywords
    #' @export maybe leave blank
    #' @examples leave this blank
    #' now put in the examples here

##set your working directory to package file
#setwd("~/OneDrive - Missouri State University/RESEARCH/2 projects/MeMoBootR")

##run this thing
library(roxygen2)
roxygen2::roxygenise()
    ##it searches through folder looks for description
    ##and create the Rd files and put into man folder

##Then you put it on git hub

##then run this to update
devtools::install_github("doomlab/MeMoBootR")
