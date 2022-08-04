#library("devtools")

devtools::load_all()
devtools::document()
devtools::check()

install("../jakR")
packageVersion("jakR")

#from github
#install_github("jeffkimbrel/jakR")

#usethis::use_rmarkdown_template("jakR")
