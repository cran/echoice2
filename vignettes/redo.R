# Must manually move image files from echoice2/ to echoice2/vignettes/ after knit

library(knitr)
old_wd <- getwd()
setwd("vignettes/")
knit("Modeling_volumetric_demand.Rmd.orig", output = "Modeling_volumetric_demand.Rmd")
knitr::purl("Modeling_volumetric_demand.Rmd.orig", output = "Modeling_volumetric_demand.R")

knit("Importing_lol_data.Rmd.orig", output = "Importing_lol_data.Rmd")
knitr::purl("Importing_lol_data.Rmd.orig", output = "Importing_lol_data.R")

setwd(old_wd)

# dir.create("inst/doc")
# file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)
