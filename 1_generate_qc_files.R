library(here)
library(quarto)
library(rmarkdown)
library(sensorstrings)

#note, if running entire county at once, HIL flags do not get applied. 
#Re-run stations which require HIL flags separately. WIP
path <- file.path("R:/data_branches/inland_water_quality/processed_data/deployment_data")

county <- "Digby"

depls <- list.files(
  paste(path, county, "new", sep = "/"),
  pattern = ".rds",
  full.names = TRUE
)
depls

#set depls to one file in list if you only want to run script for one station
depls <- depls[7]
depls

# export html file for each county showing the flagged observations
sapply(depls, function(x) {

  quarto_render(
    input = here("1_apply_qc_tests.qmd"),
    output_file = paste0(
      sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
      ".html"
    ),
    execute_params = list(county = county, depl_file = x))

})

# extra code

# depls <- list.files(
#   paste(path, county, sep = "/"),
#   pattern = ".rds",
#   full.names = TRUE
# )
# depls <- depls[27]


# x <- depls[1]
# depl_file <- x
# quarto_render(
#   input = here("1_apply_qc_tests.qmd"),
#   output_file = paste0(
#     sub(".rds", "", sub(".*/", "", x, perl = TRUE)),
#     ".html"
#   ),
#   execute_params = list(county = county, depl_file = x))



