## Supplementary material
This repository contains supplementary material for the paper
**M.Leman(2020). Co-regulated timing in music ensembles: a Bayesian listener perspective (submitted)** (see *CoRegulatedTimingBListener.pdf*)
The scripts generate all the figures in the paper. 
By adapting parameters you can test the BListener algorithm.

## How to use in RStudio?

**1. Load the BListener package**

Before downloading BListener, you should have installed devtools and load it:
install.packages("devtools")
library("devtools").
After that, to download BListener, you can type in the R prompt:
install_github("IPEM/BListener").

**2. Download the BListener_supplementary_material to your local disk**

'Test*.R' are scripts to reproduce all figures and play with the parameters of Blistener.
Note that inside each 'Test*.R' script, the 'BLplot' function is loaded for plotting the outcome of the 'BLmain' function.