################## from data.cite to reactable data-citation-list ########################
# Patrick Dross, 01.08.2020
# 

# remove (almost) everything in the working environment
# You will get no warning, so don't do this unless you are really sure:
remove(list = ls())


# install and load libraries:

# install.packages("jsonlite")
# install.packages("dplyr")
# install.packages("reactable")
# install.packages("curl")
library(jsonlite)
library(dplyr)
library(reactable)
library(curl)


# creating doi vector (WZB 1:26 - 02.08.2021 - from: https://data.gesis.org/sharing/#!Home)
doi_vector <- c("https://doi.org/10.7802/1209",
                "https://doi.org/10.7802/1462",
                "https://doi.org/10.7802/1422",
                "https://doi.org/10.7802/1.1929",
                "https://doi.org/10.7802/2042",
                "https://doi.org/10.7802/2122", # version 1.1.0 from https://doi.org/10.7802/2042 #
                "https://doi.org/10.7802/2125",
                "https://doi.org/10.7802/1.2130",
                "https://doi.org/10.7802/1.2121",
                "https://doi.org/10.7802/1.2010",
                "https://doi.org/10.7802/2041",
                "https://doi.org/10.7802/1481",
                "https://doi.org/10.7802/2104",
                "https://doi.org/10.7802/2259", # version 2.0.0 from https://doi.org/10.7802/2104 #
                "https://doi.org/10.7802/1.2007",
                "https://doi.org/10.7802/1.1960",
                "https://doi.org/10.7802/1447",
                "https://doi.org/10.7802/1.2031",
                "https://doi.org/10.7802/1.1989",
                "https://doi.org/10.7802/1996",
                "https://doi.org/10.7802/2039",
                "https://doi.org/10.7802/2241",
                "https://doi.org/10.7802/2256",
                "https://doi.org/10.7802/2278",
                "https://doi.org/10.7802/2279",
                "https://doi.org/10.7802/2280")


# extracting dois
doi_vector2 <- substring(doi_vector, first=17, last=50)

# prepare address for datacite.api
adress <- "https://api.datacite.org/dois/"

########## get data via api ########## 

# prepare vars
year    <- c()
title   <- c()
author1 <- c()
author2 <- c()
author3 <- c()
author4 <- c()
author5 <- c()
author6 <- c()
created <- c()
version <- c()
name    <- c()
name2   <- c()

# get data via datacite.api
for (i in 1:length(doi_vector2)) {
  sdn_data <- fromJSON(paste(adress, doi_vector2[i], sep = ""))
  year     <- c(year, sdn_data$data$attributes$publicationYear)
  title    <- c(title, sdn_data$data$attributes$titles$title[1])
  author1  <- c(author1, sdn_data$data$attributes$creators$name[1])
  author2  <- c(author2, sdn_data$data$attributes$creators$name[2])
  author3  <- c(author3, sdn_data$data$attributes$creators$name[3])
  author4  <- c(author4, sdn_data$data$attributes$creators$name[4])
  author5  <- c(author5, sdn_data$data$attributes$creators$name[5])            
  author6  <- c(author6, sdn_data$data$attributes$creators$name[6])
  if (is.null(sdn_data$data$attributes$version) == TRUE) {
      version <- c(version, paste("1.0.0") )
      } else {
        version <- c(version, sdn_data$data$attributes$version)
  }
  created  <- c(created, sdn_data$data$attributes$created)
  name     <- c(sdn_data$data$attributes$creators$name)
  name2    <- c(name2, paste(name, collapse = "; "))
}

# correcting version!
version_cor   <- c(version)
version_cor[version == "1"]    <- "1.0.0"
version_cor[version == "1.0"]    <- "1.0.0"

# correcting name2 (https://doi.org/10.7802/1209)!
name2[1] <- substring(name2[1], first=1, last=170)

# ID
id   <-  c(1:length(doi_vector))

# create data frame
data <- data.frame(doi_vector, year, title, version_cor, created, author1, author2, author3, author4, author5, author6, name2)

# sort by year
data_sort <- data %>%
  arrange(desc(created))

# add ID
data_final <- cbind(id, data_sort)

# save data as *.csv
write.csv(data_final,"data/data_cite.csv", row.names = FALSE)

# create citation - list as vector (change citation style...)
zitation <- c()
for (i in 1:length(doi_vector)) {
  zitation[i] <- c(paste(data_final$name2[i],
                   " (", data_final$year[i], "): ",
                   data_final$title[i],
                   ". Version ",
                   data_final$version_cor[i],
                   ". Wissenschaftszentrum Berlin für Sozialforschung. Dataset. DOI: ",
                   data_final$doi_vector[i], ".",
                   sep = ""))
}

head(zitation)

# citation vector to data.frame
citation_data <- data.frame(zitation)

# save citation list as *.csv
write.table(citation_data,"data/citation_list.csv", row.names = FALSE,
                                          col.names = FALSE,
                                          quote = FALSE)



##### create a reactable table:

# prepare data:
data_tab <- data_final %>%
  arrange(desc(year)) %>%
  transmute(year, title, name2, version_cor,
            DOI2 = paste('<a href="', doi_vector, '">', doi_vector, '</a>', sep = "")) %>%
  rename(Jahr = year, Titel = title, `Autor*innen` = name2, Version = version_cor,
         DOI = DOI2)

# create reactable
reactable(data_tab, filterable = TRUE,
          searchable = TRUE,
          bordered = TRUE, 
          striped = TRUE,
          highlight = TRUE,
          defaultPageSize = 10, showPageSizeOptions = TRUE, 
          showSortable = TRUE,
          pageSizeOptions = c(10, 20, 30, 40, 50),
          defaultSortOrder = "desc",
          columns = list(
            DOI = colDef(html = TRUE, minWidth = 170),
            Titel = colDef(minWidth = 180, resizable = TRUE),
            `Autor*innen` = colDef(minWidth = 180, resizable = TRUE),
            Jahr = colDef(maxWidth = 80),
            Version = colDef(maxWidth = 90)
          ))



############## THE END ############## 
