### Assemble data
## Read the csv files

rm(list = ls())

setwd("~/Swiss_SEM/2_Modeling/Data_preparation/seabloom-2020-ele-dryad-data")


seabloom <- lapply(list.files(pattern = glob2rx("cdr*.csv"))[-c(2:4)],
                   read.table, sep = ",", header = TRUE)
# names(seabloom) <- c("functional", "life", "origin", "data")
names(seabloom) <- c("functional", "data")

list2env(seabloom, .GlobalEnv)
rm(seabloom)


### Rename the biomass variable for the different groups
## Total biomass
names(data)[13] <- "mass.above.total"

## Biomass by functional group
names(functional)[13] <- "mass.above.functional"

## Biomass by life strategy group
# names(life)[13] <- "mass.above.life"

## Biomass by origin
# names(origin)[13] <- "mass.above.origin"


### Merge the files by identifier fields.
data.funct <- merge(data, functional, by = c("field", "exp", "year", "plot",
                                             "disk", "ntrt", "nadd",
                                             "other.add", "dur"))


names(data.funct)


data.life <- merge(data, life, by = c("field", "exp", "year", "plot", "disk",
                                      "ntrt", "nadd", "other.add", "dur"))

data.orig <- merge(data, life, by = c("field", "exp", "year", "plot", "disk",
                                      "ntrt", "nadd", "other.add", "dur"))

life.orig <- merge(data.life, data.orig, by = c("field", "exp", "year", "plot",
                                                "disk", "ntrt", "nadd",
                                                "other.add", "dur", "yr.plowed",
                                                "precip.mm", "precip.gs",
                                                "mass.above.total", "rich",
                                                "even", "ens.pie", "disk.trt",
                                                "trt", "duration",
                                                "mass.above.life"))

comb <- merge(data.funct, life.orig, by = c("field", "exp", "year", "plot",
                                            "disk", "ntrt", "nadd", "other.add",
                                            "dur", "yr.plowed", "precip.mm",
                                            "precip.gs", "mass.above.total",
                                            "rich", "even" ,"ens.pie",
                                            "disk.trt", "trt"))

