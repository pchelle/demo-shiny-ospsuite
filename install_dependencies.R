#------ Install Dependencies for App ------
install.packages(c('remotes', 'pak', 'dplyr', 'ggplot2', 'shinyWidgets', 'shinydashboard'))
install.packages(c('purrr','readr','stringr','tidyr'))
pak::pak('jespermaag/gganatogram')
pak::pak('Open-Systems-Pharmacology/rSharp')
pak::pak('Open-Systems-Pharmacology/OSPSuite.RUtils')
pak::pak('Open-Systems-Pharmacology/TLF-Library')
install.packages('ospsuite.tar.gz', repos = NULL, type = "source")
