# https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html
library(GeoPressureR)

devtools::load_all(
  "/Users/rafnuss/Library/CloudStorage/OneDrive-Vogelwarte/GeoPressureR"
)

# Get all the tag_id
list_id <- tail(
  names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)),
  -1
)


## OPTION 1: Run workflow step-by-step for a single tag
#  "5D6" "5D7" "5D8" "5E5" "5E7" "56C" "5CF"
id <- "5D6" # Run a single tag
geopressuretemplate_config(id)
tag <- geopressuretemplate_tag(id)
graph <- geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)


## OPTION 2: All tracks, step-by-step

# 1. Compute likelihood map
for (id in list_id) {
  geopressuretemplate_tag(id)
}

# 2. (optional) Manual check of labeling
# geopressureviz("18LX")

# 3. (optional) Add wind if not done before
for (id in list_id) {
  cli::cli_h1("Run tag_download_wind for {id}")
  load(glue::glue("./data/interim/{id}.RData"))
  tag_download_wind(tag, include_stap_id = 1, overwrite = TRUE)
}

# Update the stap include field based on available data
for (id in list_id) {
  e <- new.env()
  load(glue::glue("./data/interim/{id}.RData"), envir = e)

  # Modify the tag object
  e$tag$stap$include <- !sapply(e$tag$map_pressure$data, \(x) is.null(x))

  # Save ALL objects that were loaded
  save(list = ls(e), envir = e, file = glue::glue("./data/interim/{id}.RData"))
}

# 4. Run graph
for (id in list_id) {
  graph <- geopressuretemplate_graph(id)
}

# 5. Run pressurepath
for (id in list_id) {
  geopressuretemplate_pressurepath(id)
}


## OPTION 3: Run entire workflow for all tags
for (id in list_id) {
  geopressuretemplate(id)
}
