# A functional response for predators that can continue to search while handling up to $n$ prey at a time
This repository contains the code and data for the analyses of:

_**Novak, Coblentz & DeLong**_ (in review) *In defense of the Type I functional response:
The frequency and population-dynamic effects of feeding on multiple prey at a time.*

## Repository content
Mathematical analyses (isoclines and simulations) were performed in [Mathematica](https://www.wolfram.com/mathematica/) _v.13.3_.
Statistical analyses of the [FoRAGE database](https://doi.org/10.5063/F17H1GTQ) were performed in [R](https://www.r-project.org) _v.4.3.2_.


#### [_code_](code/)
Within the [code/mathematica](code/mathematica/) subfolder, the notebook [FuncResp_n-prey-at-a-time.nb](code/mathematica/FuncResp_n-prey-at-a-time.nb) contains the primary analyses presented in the manuscript.

Within the [code/R](code/R/) subfolder, [Rpackages.md](code/R/Rpackages.md) lists the required R packages, [data_prep.R](code/R/data_prep.R) standardizes the format of all FoRAGE datasets, [data_subset.R](code/R/data_subset.R) specifies which datasets are to be removed prior to model fitting, [fit_datasets.R](code/R/fit_datasets.R) performs the model fitting, and [analysis_Hn.R](code/R/analysis_Hn.R) is used for the analysis of the resulting fits.  Model fitting uses a library of functions located in the [lib](code/R/lib/) sub-folder originally developed with [Daniel Stouffer](https://github.com/stouffer) (see [General Functional Responses](https://github.com/stoufferlab/general-functional-responses) repository).


#### [_data_](data/)
Contains the FoRAGE v4 database in .csv and .Rdata formats.  Individuals interested in using the [FoRAGE database](https://doi.org/10.5063/F17H1GTQ) for additional analyses should instead obtain it from [KNB - The Knowledge Network for Biocomplexity repository](https://doi.org/10.5063/F17H1GTQ) where the [FoRAGE database](https://doi.org/10.5063/F17H1GTQ) is maintained.


#### [_figs_](figs/) & [_tables_](tables/)
These contain all figures and statistical tables associated with the manuscript, as well as unused supplemental figures.

#### [_results_](results/) & [_temp_](temp/)
The [results](results/fits) folder contains each of the dataset-specific model fits produced by [code/R/fit_datasets.R](code/R/fit_datasets.R).  The [temp](temp/) folder contains fitting error summaries for debugging purposes, as well as temporary Mathematica files generated during the mathematical analyses.


## _Warning_: Parameter interpretation
 As noted in the _Supplementary Materials_ of the above-referenced manuscript, unlike in the original analyses of the FoRAGE database (e.g., [Uiterwaal & DeLong 2022](https://doi.org/10.1002/ecy.3706)), the model-fitting analyses implemented here have _not_ standardized prey and predator abundances by area (or volume) and have _not_ standardized prey consumption values by time.  Therefore, estimates of the attack rates and handling times are dataset-specific (i.e. are not in common units) and should not be compared across datasets.  Estimates of the maximum number of prey that can be handled at a time (parameter _n_ of the multi-prey functional response model) are unitless.

## Warranty
 All code is provided "as is" and without warranty.

 ## Contact
 Please email Mark Novak (mark.novak@oregonstate.edu) with any questions about the analyses and John DeLong (jpdelong@unl.edu) with any questions about the [FoRAGE database](https://doi.org/10.5063/F17H1GTQ).