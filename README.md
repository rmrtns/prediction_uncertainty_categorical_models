[![DOI](https://zenodo.org/badge/807748665.svg)](https://zenodo.org/doi/10.5281/zenodo.11390928)

# Prediction uncertainty of categorical prediction models

This repository contains the code for a simulation method to evaluate the impact of biological and analytical variation on the prediction uncertainty of categorical prediction models.

The code accompanies the journal article:
"Unraveling uncertainty: The impact of biological and analytical variation on the prediction uncertainty of categorical prediction models",

which has been published in: [to be filled in].

## Repository structure:
The structure of this repository resembles that of the RStudio Project that was used to perform the simulations described in the article. The data used in the article are only available upon reasonable request. However, using the same project structure, the code may be used to evaluate alternative models or alternative datasets or both. 

data/<br>
This folder should hold the data files used in the simulations. 

markdown/<br>
The main folder contains the R Markdown templates ('template_discordance.Rmd', 'template_confusion_matrices.Rmd', 'template_crossover.Rmd') that are used to call the simulation functions and report their results. These files serve as templates to conduct the experiments and should, therefore, not be modified.<br>
The subfolders contain exemplary and modifiable parameterized R Markdown files to perform similar analyses as described in the article. The reports based on these R Markdown files will be saved in the subfolders as well.
For the estimation of variation-induced prediction uncertainty, only one combination of biological and analytical variation can be tested in a single file.<br>
In addition, the 'markdown/examples_mortality/' subfolder contains the code used to train and validate the xgboost model.<br>
Further, the 'markdown/examples_mortality/' subfolder contains the code used to validate the use of the area under the receiver-operating-characteristic curve and confusion matrix derived measures based on aggregate simulation results as a measure for the average results based on individual predictions.<br>
All relative paths defined in the R Markdown files are relative to the project directory.

out/<br>
The output of the simulations will be saved in this folder.<br> 
The structure of its subfolders facilitates the interaction between the R Markdown files.<br>
In addition the subfolder 'out/mortality/model/' holds the xgboost model that was used in the article.

renv/<br>
This folder contains files that are required to recreate the project library that was used for the analyses described in the article.

src/<br>
This folder contains all the source code from the project.

## Installation:
- Clone or download the entire repository.
- Recreate the R environment using renv (https://rstudio.github.io/renv/articles/collaborating.html): Briefly, use renv::restore() to restore the project library, after renv has been installed automatically.

## How to use:
The next section provides a detailed guide to use the simulation code.

- Save the data file that is used in the simulations.<br>
The data should be saved as a .csv file in the 'data/' folder. <br>
It is recommended that the file only contains the variables used by the prediction model that is evaluated. In addition, a patient identifier is required. If the impact on clinical performance characteristics is assessed as well, the data files should also contain the reference outcome.<br>
Please note that some matrix-based prediction models require the column order in the dataset to be in the exact same order as in the dataset that was used for model training. For this purpose, variables may be sorted in the raw dataset or the prediction model function (e.g., 'predict_mortality.R' in the 'src/' folder).

- Define biological and analytical variation.<br>
Biological and analytical variation can be entered in the 'Variables.R' file in the 'src/' folder.

- Define which variables are subject to variation.<br>
Sensitivity to variation can be defined as boolean data in the 'variables.R' file in the 'src/' folder.

- Define the prediction model that is to be evaluated.<br>
The prediction model should be saved in a .R file in the 'src/' folder. <br>
The simulation method accepts additional function arguments, if required by the prediction model, as additional parameters in the R Markdown files. All additional function arguments are listed in the 'dots_arguments' variable and can be extracted by the prediction model function using list subsetting. Examples are available in the 'src/' folder ('predict_egfr.R', 'predict_mortality.R'). 

- Determine the number of Monte Carlo samples.<br>
This can be based on scientific literature or may be tested empirically, for example, following the approach in 'mortality_discordance_determine_number_of_samples_an_cv.Rmd' in the 'markdown/examples_mortality/' subfolder.

- Estimate the measures of variation-induced prediction uncertainty.<br>
Follow the approach outlined in 'mortality_discordance_example_tot_cv_base.Rmd' in the 'markdown/examples_mortality/' subfolder.<br>
The simulation parameters may modified. For example, to simulate higher levels of analytical variation, multiply the 'analytical_cv' parameter (e.g., 'mortality_discordance_example_tot_cv_1dot5.Rmd'). In addition, to base bootstrap intervals on the computationally less intensive percentile method, change the 'bootstrap_method' parameter to 'c("perc")' (without single quotation marks).<br>
If an intermediary continuous prediction function is not required by the categorical prediction function, the 'continuous_prediction_function' parameter may be set to 'NA' (without single quotation marks).

- Estimate the impact on clinical performance characteristics (optional).<br>
Follow the approach outlined in 'mortality_confusion_matrices_aggregate_results_discordance_example_tot_cv.Rmd' in the 'markdown/examples_mortality/' subfolder. 

- Estimate the impact on reclassification within the confusion matrix (optional).<br>
Follow the approach outlined in 'mortality_crossover_aggregate_results_discordance_example_tot_cv.Rmd' in the 'markdown/examples_mortality/' subfolder. 

- Create summary figures of the simulation results (optional).<br>
Follow the approach outlined in 'mortality_plots.Rmd' in the 'markdown/examples_mortality/' subfolder. Plot characteristics may be changed in the 'manuscript.R' file in the 'src/' folder.

- Create summary tables of the simulation results (optional).<br>
Follow the approach outlined in 'mortality_tables.Rmd' in the 'markdown/examples_mortality/' subfolder. Table characteristics may be changed in the 'manuscript.R' file in the 'src/' folder.

## Disclaimer:
The code in this repository is intended for research use only.<br>
The simulation method was developed in R (version 4.1.3) with RStudio (version 2023.06.2) on a 64-bit Windows 10 machine. Running the code under other operating systems, or versions of the software and packages used, may require code modifications or installation of additional system dependencies.<br>
Computational time may vary depending on the complexity of the model tested, applied simulation parameters and computational resources.
