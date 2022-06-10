# Metrics in Action: How Social Media Metrics Shape News Production on Facebook

Replication scripts for Metrics in Action project.

To run these scripts
- change the `data_folder` variable to reflect the path to the folder that contains all the `.csv` files
- create a subfolder called `results/` outside the scripts folder
- create a subfolder called `figures/` outside the scripts folder

Your working directory should have three subfolders in it:
- `scripts/` containing all the scripts in the repository
- `results/` which will contain the results (as `.tex` tables) after the scripts are executed
- `figures/` which will contain the plots (as `.svg` images) after the scripts are executed

Details about the scripts are as follows:

- `01_longitudinal-analysis.R` creates figure 1 (in the main text).
- `01_main-analysis.R` creates table 1 and figures 2 and 3 (in the main text).
- `03_robustness-with-10-periods.R` creates table A1 and figures A1 and A2 (in the appendix).
- `04_ideology-correlation.R` creates figure A3 (in the appendix).
- `05_robustness-with-alternative-ideology-scores.R` creates figures A4 and A5 (in the appendix).
