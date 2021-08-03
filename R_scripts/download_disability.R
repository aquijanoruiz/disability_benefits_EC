if(!require(readstata13)) install.packages("discap.table", repos = "http://cran.us.r-project.org")

# The data can be downloaded from the INEC's url at https://www.ecuadorencifras.gob.ec/documentos/web-inec/Estadisticas_Sociales/ENSANUT/ENSANUT_2018/BDD_ENSANUT_2018_STATA_.zip
# We chose to upload the zip file containing the datasets to github in case the INEC takes 
# down the url to the datasets

# The zip file contains several .dta files that correspond to different parts of the survey
# The "personas" dataset (1_BDD_ENS2018_f1_personas.dta) which is used in this study contains 
# the demographic and economic data for each household member

options(timeout=600) # we change the download timeout time to 600

# We give the url a name
url <- "https://github.com/aquijanoruiz/disability_benefits_EC/raw/master/ENSANUT_datasets/BDD_ENSANUT_2018_STATA_.zip"
# We create a temporary directory
td <- tempdir()
# We create the placeholder file
tf <- tempfile(tmpdir=td, fileext = ".zip")
# We download the data into the placeholder file
download.file(url,tf)

# We can use this code to look at the files contained inside the zip file: unzip(tf, list=TRUE)$Name
people.f.name <- unzip(tf, list=TRUE)$Name[2] # The "personas" dataset
people.f.path <- file.path(td, people.f.name)
unzip(tf, files=people.f.name,exdir=td, overwrite=TRUE)

people.f.path <- file.path(td, people.f.name)
# We name the "personas" dataset people
people <- read.dta13(people.f.path)

key.people <- data.frame(variable = names(people), # we can see the name of the variables
                         label = attr(people,"var.labels"))
