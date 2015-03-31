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
                      method = "curl") 
}
if (!exists(storm_df) {
        storm_df <- read.csv("StormData.csv.bz2") %>% tbl_df }

# the EVTYPE field has a lot of freeform text
# and needs to be consolidated to be meaningful.
# This is not perfect but is an improvement.
# It gets the main point across.
df <- storm_df %>% mutate(Type2 = as.character(EVTYPE))
for (oldval in names(conf$evtype)) {
        #print(oldval)
        #print(conf$evtype[[oldval]])
        df <- mutate(df, Type2 = ifelse(grepl(oldval, Type2, ignore.case=TRUE), conf$evtype[[oldval]], Type2))
}
#        mutate(Type2 = ifelse(grepl("snow", Type2, ignore.case=TRUE), "SNOW", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("heat|hot|warm", Type2, ignore.case=TRUE), "HEAT", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("high", Type2, ignore.case=TRUE), "HEAT", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("cold|low|cool", Type2, ignore.case=TRUE), "COLD", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("tornado|funnel", Type2, ignore.case=TRUE), "TORNADO", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("wind", Type2, ignore.case=TRUE), "WIND", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("hail", Type2, ignore.case=TRUE), "HAIL", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("hurricane", Type2, ignore.case=TRUE), "HURRICANE", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("lightning", Type2, ignore.case=TRUE), "LIGHTNING", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("flood|fld", Type2, ignore.case=TRUE), "FLOOD", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("rain|shower", Type2, ignore.case=TRUE), "RAIN", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("blizzard", Type2, ignore.case=TRUE), "BLIZZARD", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("ice|icy", Type2, ignore.case=TRUE), "ICE", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("thunderstorm", Type2, ignore.case=TRUE), "THUNDERSTORM", Type2)) %>%
#        mutate(Type2 = ifelse(grepl("fire", Type2, ignore.case=TRUE), "FIRE", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("winter|wintry", Type2, ignore.case=TRUE), "WINTER STORM", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("fog", Type2, ignore.case=TRUE), "FOG", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("dust", Type2, ignore.case=TRUE), "DUST", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("waterspout", Type2, ignore.case=TRUE), "WATERSPOUT", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("dry", Type2, ignore.case=TRUE), "DROUGHT", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("rip current", Type2, ignore.case=TRUE), "RIP CURRENT", Type2))  %>%
#        mutate(Type2 = ifelse(grepl("freez", Type2, ignore.case=TRUE), "FREEZE", Type2))


## Results
df %>% select(Type2, FATALITIES, INJURIES) %>% 
        group_by(Type2) %>% 
        summarize(fatal = sum(FATALITIES), injury = sum(INJURIES)) %>% 
        arrange(desc(injury)) %>% 
        View
