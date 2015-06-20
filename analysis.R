# Storm Data Analysis

## Synopsis
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.

This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.



## Data Processing

library(yaml)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(grid)
library(gridExtra)

conf <- yaml.load_file("analysis.conf")

if (!file.exists(conf$destfile)) {
        download.file(url = conf$storm_url, 
                      destfile = conf$destfile,
                      method = "curl") 
}
# This takes a very long time:
if (!exists("storm_df")) {
        storm_df <- read.csv(conf$destfile, stringsAsFactors=FALSE) %>% 
                tbl_df
}

# the EVTYPE field has a lot of freeform text and similar types
# are consolidated according to National Weather Service 
# Emergency Alert System Events. Other types of event types
# that did not fall into these categories were grouped by
# similarity using my own personal judgement.
storm_df %<>% mutate(Type2 = EVTYPE)
for (oldval in names(conf$evtype)) {
        storm_df %<>% mutate(Type2 = ifelse(grepl(oldval, 
                                            Type2, 
                                            ignore.case=TRUE), 
                                      conf$evtype[[oldval]], 
                                      Type2)) 
}

## Results
impact_summary <- storm_df %>% 
        select(Type2, FATALITIES, INJURIES) %>% 
        group_by(Type2) %>% 
        summarize(fatal = sum(FATALITIES), injury = sum(INJURIES)) %>% 
        arrange(desc(injury), desc(fatal)) 

dfp <- head(impact_summary, n=20)

p1 <- ggplot(data = dfp) +
        geom_bar(aes(reorder(Type2, injury), injury), stat = "identity") +
        labs(title = "Injuries",
             y = "",
             x = "Event Type") +
        coord_flip()

p2 <- ggplot(data = dfp) +
        geom_bar(aes(reorder(Type2, injury), fatal), stat = "identity") +
        labs(title = "Fatalities",
             y = "",
             x = "") +
        coord_flip()
grid.arrange(p1, p2, nrow = 1)


DMGEXP <- list("K"=10^3, "M"=10^6, "B"=10^9)

# extract damage estmates: sentences with "damage" and "$".
damage_records <- storm_df %>%
        mutate(REMARKS = as.character(REMARKS)) %>% 
        filter(grepl("Damage|damage|loss", REMARKS) & grepl("\\$", REMARKS)) %>% 
        select(Type2, REMARKS)



damage_estimates <- strsplit(damage_records$REMARKS, "\\.") %>%
        lapply(function(x) paste0(x[grepl("\\$", x)], sep=".")) %>%
        lapply(function(x) gsub( "\n", "", str_trim(paste(x, collapse=""))))

damage_records %<>% mutate(DAMAGE=damage_estimates)

library(tm)
m <- storm_df[storm_df$PROPDMGEXP!="",] %>% select(PROPDMG, PROPDMGEXP, REMARKS) %>% filter(grepl("\\$", REMARKS))
vc <- VCorpus(VectorSource(m$REMARKS))
dtm <- DocumentTermMatrix(vc)

              

