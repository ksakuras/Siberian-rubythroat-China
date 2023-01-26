library(GeoPressureR)
library(ecmwfr)

# Define which track to work with
gdl <- "5D6"

# Load
load(paste0("data/3_static/", gdl, "_static_prob.Rdata"))

# Set credential
Sys.setenv( cds_key="cb832111-2dba-4312-801c-cf1c3ca65371")
Sys.setenv( cds_user="174688")

# You can see them with
 usethis::edit_r_environ()
 cds_key <- Sys.getenv("cds_key")
 cds_user <- Sys.getenv("cds_user")

graph_download_wind(pam,
                    area = static_prob,
                    # cds_key="Insert_your_CDS_API_KEY_here"
                    # cds_user="Insert_your_CDS_UID_here"
                    )

 # Check request at https://cds.climate.copernicus.eu/cdsapp#!/yourrequests
