#library("devtools")

devtools::load_all()
devtools::document()
devtools::check()

usethis::use_version("patch")

devtools::install("../jakR")
packageVersion("jakR")

#from github
#install_github("jeffkimbrel/jakR")

#usethis::use_rmarkdown_template("jakR")
