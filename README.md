# metrics-in-action
Replication scripts for Metrics in Action project

To run these scripts

- change the `data_folder` variable to reflect the path to the folder that contains all the `.csv` files.
- create a subfolder called `results/` outside the scripts folder.
- create a subfolder called `figures/` outside the scripts folder.

Your final folder substructure should look like this:

All tables are produced as `.tex` files in the `results/` subfolder.
All figures are produced as `.svg` files in the `figures/` subfolder.

`01_main-analysis.R` creates table 1 and figures 2 and 3 (in the main text).
`03_robustness-with-10-periods.R` creates table A1 and figures A1 and A2 (in the appendix).
`04_ideology-correlation.R` creates figure A3 (in the appendix).
`05_robustness-with-alternative-ideology-scores.R` creates figures A4 and A5 (in the appendix).
