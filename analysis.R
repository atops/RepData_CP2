# Title

## Synopsis


## Data Processing

library(yaml)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)

conf <- yaml.load_file("analysis.conf")

if (!file.exists(conf$destfile)) {
        download.file(url = conf$storm_url, 
                      destfile = conf$destfile,
                      method = "curl") 
}
if (!exists("storm_df")) {
        storm_df <- read.csv(conf$destfile, stringsAsFactors=FALSE) %>% 
                tbl_df
}

# the EVTYPE field has a lot of freeform text
# and similar types need to be consolidated for the analysis to be meaningful.
# This is not perfect but is an improvement.
#storm_df %<>% mutate(Type2 = EVTYPE)

for (oldval in names(conf$evtype)) {
        storm_df %<>% mutate(Type2 = ifelse(grepl(oldval, 
                                            Type2, 
                                            ignore.case=TRUE), 
                                      conf$evtype[[oldval]], 
                                      Type2)) }

## Results
impact_summary <- storm_df %>% 
        select(Type2, FATALITIES, INJURIES) %>% 
        group_by(Type2) %>% 
        summarize(fatal = sum(FATALITIES), injury = sum(INJURIES)) %>% 
        arrange(desc(injury)) %>% 
        View

p1 <- qplot(data = impact_summary, x = Type2, y = fatal, stat = "identity")

# extract damage estmates: sentences with "damage" and "$".
damage_records <- storm_df %>%
        mutate(REMARKS = as.character(REMARKS)) %>% 
        filter(grepl("Damage|damage|loss", REMARKS) & grepl("\\$", REMARKS)) %>% 
        select(Type2, REMARKS)

damage_estimates <- strsplit(damage_records$REMARKS, "\\.") %>%
        lapply(function(x) paste0(x[grepl("\\$", x)], sep=".")) %>%
        lapply(function(x) gsub( "\n", "", str_trim(paste(x, collapse=""))))

damage_records %<>% mutate(DAMAGE=damage_estimates)
