usethis::use_git()
usethis::use_git_ignore("dev_history.R")
usethis::use_github()
usethis::use_readme_rmd()
usethis::use_build_ignore("r_scripts/")
usethis::use_build_ignore("extdata/")
usethis::use_build_ignore("figures/")
usethis::use_build_ignore("r_scripts_database/")
usethis::use_build_ignore("docs/")
usethis::use_build_ignore("formation_teledec/")
usethis::use_git_ignore("formation_teledec/data2")
usethis::use_git_ignore("formation_teledec/images")
usethis::use_git_ignore("formation_teledec/formation_teledec.pdf")

## Run to render the website
file.copy("README.Rmd","index.Rmd",overwrite=T)
tx  <- readLines("index.Rmd")
tx2  <- gsub(pattern = "#  R scripts to model the risk of residual malaria transmission at a micro-scale", replace = "", x = tx)
tx2  <- gsub(pattern = "output: github_document", replace = "title: \"R scripts to model the risk of residual malaria transmission at a micro-scale\"", x = tx2)
writeLines(tx2, con="index.Rmd")

# transform R scripts to RM markdown docs
rmarkdown::render("r_scripts/link_to_r_script.R",output_dir="/home/ptaconet/phd_scripts")
rmarkdown::render("r_scripts/link_to_r_script_2.R",output_dir="/home/ptaconet/phd_scripts")

rmarkdown::render_site()