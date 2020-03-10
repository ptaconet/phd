usethis::use_git()
usethis::use_git_ignore("data/")

usethis::use_github()
usethis::use_readme_rmd()
usethis::use_build_ignore("r_scripts/")
usethis::use_build_ignore("data/")
usethis::use_build_ignore("figures/")
usethis::use_build_ignore("docs/")


## Run to render the website
#file.copy("README.Rmd","index.Rmd",overwrite=T)
#tx  <- readLines("index.Rmd")
#tx2  <- gsub(pattern = "#  R scripts to model the risk of residual malaria transmission at a micro-scale", replace = "", x = tx)
#tx2  <- gsub(pattern = "output: github_document", replace = "title: \"R scripts to model the risk of residual malaria transmission at a micro-scale\"", x = tx2)
#writeLines(tx2, con="index.Rmd")

# Convert R scripts to rmd docs
rmarkdown::render("r_scripts/data_preparation/link_to_r_script.R",output_dir="/home/ptaconet/phd/rmds")
rmarkdown::render("r_scripts/link_to_r_script_2.R",output_dir="/home/ptaconet/phd/rmds")
# isnt it rmarkdown::spin() ? https://bookdown.org/yihui/rmarkdown-cookbook/spin.html
# to be verified...

# the other way around (rmd to R) : https://bookdown.org/yihui/rmarkdown-cookbook/purl.html

# render the website using the rmds located in 
rmarkdown::render_site(input = "rmds")