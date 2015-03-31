# Title

## Synopsis


## Data Processing

library(yaml)
library(dplyr)

conf <- yaml.load_file("analysis.conf")

storm_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (!file.exists("StormData.csv.bz2")) {
        download.file(url = storm_url, 
                      destfile = "StormData.csv.bz2",
                      method = "curl") }
if (!exists(storm_df)) {
        storm_df <- read.csv("StormData.csv.bz2") %>% tbl_df }

# the EVTYPE field has a lot of freeform text
# and similar types need to be consolidated for the analysis to be meaningful.
# This is not perfect but is an improvement.
df <- storm_df %>% mutate(Type2 = as.character(EVTYPE))
for (oldval in names(conf$evtype)) {
        df <- mutate(df, Type2 = ifelse(grepl(oldval, Type2, ignore.case=TRUE), conf$evtype[[oldval]], Type2)) }

## Results
df %>% select(Type2, FATALITIES, INJURIES) %>% 
        group_by(Type2) %>% 
        summarize(fatal = sum(FATALITIES), injury = sum(INJURIES)) %>% 
        arrange(desc(injury)) %>% 
        View
