## Install DCI dependencies

installDeps <- function() {
  # Install dependencies
  .pkgs = c("gdata",
  			"devtools",
  			"reshape2",
  			"ggplot2",
  			"ggthemes",
  			"stringr",
  			"igraph",
  			"plotly",
  			"plyr",
        "dpryr",
        "rmarkdown",
  			"captioner",
        "Hmisc",
        "psych",
        "car",
  			"corrplot",
  			"rgdal",
  			"maptools",
  			"rgeos")

  # Install required packages from CRAN (if not)
  .inst <- .pkgs %in% installed.packages()
  if(length(.pkgs[!.inst]) > 0) install.packages(.pkgs[!.inst], repos="http://cran.rstudio.com/")

  # Install from Github
  # TODO: Make this conditional on whether the source has changed
  # Otherwise slows down sourcing the main.R file.
  # devtools::install_github("adletaw/captioner")

  }
