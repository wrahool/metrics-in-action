# Metrics in Action: How Social Media Metrics Shape News Production on Facebook

Replication scripts for the Metrics in Action project by Subhayan Mukerjee, Tian Yang, and Yilang Peng.

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
- `02_main-analysis.R` creates table 1 and figures 2 and 3 (in the main text).
- `03_test-stationarity.R` tests the stationarity assumption of the time series.
- `04_robustness-with-10-periods.R` creates figures A2 and A3 (in the Appendix).
- `05_robustness-with-alternative-ideology-score.R` creates figures A4, A5, and A6 (in the Appendix)
- `06_robustness_with_quadratic_model.R` creates table A2 (in the Appendix)
- `07_robustness-with-post-freq.R` creates table A3 (in the Appendix)
- `08_robustness-with-interaction-analysis.R` creates table A4 (in the Appendix)
- `09_robustness-with-shares-only.R` creates table table A5 (in the Appendix)

Details of which script produces which table and figure are also documented at the beginning of each script.
