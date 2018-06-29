# MeMoBootR

This package is in (super) active development. I am working on updating as I go through the different models involved in mediation and moderation. You can check out videos / updates on my [YouTube channel](https://www.youtube.com/channel/UCMdihazndR0f9XBoSXWqnYg), as well as watching or starring the github repo.

To install the package take the following steps:

1) Install `devtools` if you do not have it. `devtools` is a package that allows you to install packages from github.
`install.packages("devtools")`

2) Install the `MeMoBootR` package by running the following:
`devtools::install_github("doomlab/MeMoBootR")`

3) Load the library to get started!
`library(MeMoBootR)`

4) Enjoy mediation and moderation complete with data screening. 

More tutorials, details, and other information added as I go. 

Many thanks to KD Valentine for the fantastic name suggestion.

# Examples

Head over to the [OSF Page](https://osf.io/ns6jz/) to view examples of the function in action. Included on the OSF page are youtube videos that explain the different functions and examples. Additionally, you can find information about the translation of the model numbers from PROCESSv3 to MeMoBootR.

# Version Information

Version: 0.0.0.3000
  - Added two way interactions (model 1 - moderation)
  - No good categorical options yet (i.e., it'll run but not what you want probably yet)
  - Updated data screening so it can handle categorical CV values across all analyses
  
Version: 0.0.0.2000
  - Added diagram ability with `diagram` library (wouldn't mind help here, the diagrams for mediation are only ok)
  - Added ability for categorical X variables in simple mediation

Version: 0.0.0.1000: 
  - Initial build
  - Simple mediation with categorical variables added
