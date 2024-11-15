# code/workflowr.R
# This script houses the code used to process the workflowr project/webpage


# Workflowr code ----------------------------------------------------------

# All analysis files
dir("analysis", pattern = ".Rmd", full.names = T)

# Choose less angry pathway to git
options(workflowr.sysgit = "")

# Run this to re-compile the entire project
system.time(
  workflowr::wflow_publish(files = c("analysis/index.Rmd",
                                     "analysis/key_drivers.Rmd",
                                     "analysis/metadatabase.Rmd",
                                     "analysis/FAIR_data.Rmd",
                                     "analysis/data_summary.Rmd",
                                     "analysis/model_summary.Rmd",
                                     "analysis/sst_summary.Rmd",
                                     "analysis/2021_analysis.Rmd",
                                     "analysis/2021_summary.Rmd",
                                     "analysis/2022_seminar.Rmd",
                                     "analysis/species_data.Rmd"
  ),
  message = "Re-built site.")
) # 20 seconds

