# code/workflowr.R
# THis script houses the code used to process the workflowr project/webpage

# Workflowr code ----------------------------------------------------------

# All analysis files
dir("analysis", pattern = ".Rmd", full.names = T)

# Run this to re-compile the entire project
system.time(
  workflowr::wflow_publish(files = c("analysis/index.Rmd",
                                     "analysis/key_drivers.Rmd",
                                     "analysis/metadatabase.Rmd",
                                     "analysis/2021_summary.Rmd",
                                     "analysis/2021_analysis.Rmd",
                                     # "analysis/data_summary.Rmd", # NB: This takes a couple minutes
                                     # "analysis/model_summary.Rmd", # NB: This takes a couple minutes
                                     "analysis/review.Rmd"
  ),
  message = "Re-built site.")
) # 342 seconds with all summary pages, 15 seconds with none
