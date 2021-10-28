rm(list = ls()); gc()
setwd("/Users/danielpringle/Code/covidvaxnz/R")

library(tidyverse)
library(data.table)
library(readxl)

file <- "/Users/danielpringle/Code/covidvaxnz/data/211024_cvip_equity.xlsx"
sheets <- file %>%
  excel_sheets() %>% 
  set_names() 

x <- lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
x <- lapply(x, as.data.table)
names(x) <- sheets

# Long table of vaccination data, and HSU population data by age-ethnic splits
vx_query <- x[["Vaccination Query"]]
dhb_pop <- x[["HSU Table"]]

# Join data and tidy up
vx <- merge(x = vx_query, y = dhb_pop, by = c("Ethnic group", "Age group", "DHB of residence"))
oldnames = c("Week ending date", "Dose number", "Ethnic group", "Age group", "DHB of residence", "# doses administered", "# people (HSU)")
newnames = c("week", "dose", "ethnicgroup", "agegroup", "DHB", "count", "popgroup")
setnames(vx, old = oldnames, new = newnames)
setcolorder(vx, newnames)

# Set as factors
vx$dose <- factor(vx$dose)
vx$agegroup <- factor(vx$agegroup)
vx$ethnicgroup <- factor(vx$ethnicgroup)
vx$DHB <- factor(vx$DHB)

# Inspect
head(vx, 20)
head(dhb_pop)
View(dhb_pop)
