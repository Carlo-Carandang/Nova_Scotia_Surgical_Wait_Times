options(warn = -1) # suppress warning messages

## ---- packages ----

if (!requireNamespace(package = 'tidyverse', quietly = TRUE, warn.conflicts = FALSE)) install.packages(pkgs = 'tidyverse', repos='https://cran.r-project.org/', verbose = FALSE, quiet = TRUE)

options(warn = 0) # enable warning messages
