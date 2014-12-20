# hi Norway

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"   , showWarnings = FALSE)
dir.create("photos" , showWarnings = FALSE)
dir.create("plots"  , showWarnings = FALSE)
dir.create("raw"  , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE # thematic graphs only

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks

# have a nice day
