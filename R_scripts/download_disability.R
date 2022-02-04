if(!require(readstata13)) install.packages("discap.table", repos = "http://cran.us.r-project.org")

# The data can be downloaded from the INEC's url at https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/ENSANUT/ENSANUT_2018/BDD_ENSANUT_2018_STATA_.zip
# We chose to upload the zip file containing the datasets to github in case the INEC takes 
# down the url to the datasets

# The zip file contains several .dta files that correspond to different parts of the survey
# The "personas" dataset (1_BDD_ENS2018_f1_personas.dta) which is used in this study contains 
# the demographic and economic data for each household member

options(timeout=600) # we change the download timeout time to 600

# location of the data on github
url <- "https://github.com/aquijanoruiz/disability_benefits_EC/raw/master/ENSANUT_datasets/BDD_ENSANUT_2018_STATA_.zip"
# creates a temporary directory
td <- tempdir()
# creates a placeholder file
tf <- tempfile(tmpdir=td, fileext = ".zip")
# downloads the data into the placeholder file
download.file(url,tf)

# shows the files contained inside the zip file: unzip(tf, list=TRUE)$Name
people.f.name <- unzip(tf, list=TRUE)$Name[2] # The "personas" dataset
people.f.path <- file.path(td, people.f.name)

home.f.name <- unzip(tf, list=TRUE)$Name[3] # The "hogar" dataset
home.f.path <- file.path(td, home.f.name)
unzip(tf, files=c(people.f.name,home.f.name),exdir=td, overwrite=TRUE)

# loads the dta files
people <- read.dta13(people.f.path)
home <- read.dta13(home.f.path)

# loads the variable descriptions
key.people <- data.frame(variable = names(people), label = attr(people,"var.labels"))
key.home <- data.frame(variable = names(home), label = attr(home,"var.labels"))
