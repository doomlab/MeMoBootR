# MeMoBootR

This package is in (ok fine somewhat) active development. I am working on updating as I go through the different models involved in mediation and moderation. You can check out videos / updates on my [YouTube channel](https://www.youtube.com/channel/UCMdihazndR0f9XBoSXWqnYg), as well as watching or starring the github repo.

To install the package take the following steps:

1) Install `devtools` if you do not have it. `devtools` is a package that allows you to install packages from github.
`install.packages("devtools")`

2) Install the `MeMoBootR` package by running the following:
`devtools::install_github("doomlab/MeMoBootR")`

3) Load the library to get started!
`library(MeMoBootR)`

4) Enjoy mediation and moderation complete with data screening.

5) Cite the package!
`Buchanan, E.M. (2018). MeMoBootR [Computer Program]. Avaliable at: https://github.com/doomlab/MeMoBootR`. 

More tutorials, details, and other information added as I go. 

Many thanks to KD Valentine for the fantastic name suggestion.

# Table of Possibilities

|PROCESS| MeMoBootR | Label | Categorical X | Categorical M |
| --- | --- | --- | --- | --- |
| Model 1 | moderation1() | Two-way moderation | No | Yes |
| Model 2 | moderation2() | Double Two-way moderation | No | Yes |
| Model 4 | mediation1() | Simple mediation | Yes | No |
| Model 6 | mediation2() | Serial mediation | Yes | No |
| Model 6-7 Hybrid | mediation2_moda() | Serial Mediation with Moderator on A | Yes | No for mediators/moderators |

- For moderation models with categorical variables, make those variables M, rather than X. For X and M categorical, use ANOVA (`ez` is a great package).
- Mediation should allow for categorical X and M, just haven't gotten there yet. 

# Examples

Head over to the [OSF Page](https://osf.io/ns6jz/) to view examples of the function in action. Included on the OSF page are youtube videos that explain the different functions and examples. Additionally, you can find information about the translation of the model numbers from PROCESSv3 to MeMoBootR.

# Version Information
Version: 0.0.0.7000
  - Adding serial mediation with a continuous moderator on the A path. Should be able to do all paths at some point. 

Version: 0.0.0.6001
  - Fixed bug with mediation1(), moderation1() that did not allow cvs to show up.

Version: 0.0.0.6000
  - Added double two-way moderation - which is two two-way interactions with two moderators (model 2). 
  - The next goals are to make mediation2 and moderation2 work for categorical variables over the next couple of weeks. 

Version: 0.0.0.5000
  - Added serial mediation with two mediators (model 6). The model can handle covariates and should be able to do categorical X. I will be testing categorical X more next week.

Version: 0.0.0.4000
  - Added categorical moderators, where X is continuous, M can be categorical or continuous. Please note that it will not run with X categorical. (model 1 - moderation) 

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
