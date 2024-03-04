# This script creates two summary graphs for Earth Lab's UAS plant traits review
# paper. The first summarizes the count of reference in a literature search with
# and without "remote sens*" in the search term. The other graph shows the year
# of first publication for these searches, again with and without "remote sens*"
# in the search.

# Install packages
library(ggplot2)
library(reshape2) #melt
library(dplyr)
library(lemon) #g_legend
library(gridExtra)
require(scales) #mylog_trans

# Functions

# Function to make barplot in ggplot start at 0 rather than at 1 so that values of 1 are visible (plant calcium, photosynthetic pigments) 
# source: https://stackoverflow.com/questions/22295253/force-bars-to-start-from-a-lower-value-than-0-in-ggplot-geom-bar-in-r
mylog_trans <- function(base=exp(1), from=0) 
{
  trans <- function(x) log(x, base)-from
  inv <- function(x) base^(x+from)
  trans_new("mylog", trans, inv, log_breaks(base=base), 
            domain = c(base^from, Inf))
}

# Function to move the barplot baseline from 0 to whatever argument you specify. 
# source: https://stackoverflow.com/questions/35324892/ggplot2-setting-geom-bar-baseline-to-1-instead-of-zero
shift_trans = function(d = 0) {
  scales::trans_new("shift", transform = function(x) x - d, inverse = function(x) x + d)
}

# For use in Fig 2
# source: https://stackoverflow.com/questions/65109960/is-there-a-way-i-can-add-log-gridlines-to-my-graph-with-rstudio
minor_breaks_log <- function(base) {
    # Prevents lazy evaluation
    force(base) 
    # Wrap calculation in a function that the outer function returns
    function(limits) {
        ggplot2:::calc_logticks(
            base = base, 
            minpow = floor(log(limits[1], base = base)), 
            maxpow = ceiling(log(limits[2], base = base))
        )$value
    }
}


# Load data
rawdat <- data.frame(readr::read_csv("Fig2_prep.csv"))

# Explore data
names(rawdat)
head(rawdat)

# Clean data
dat <- rawdat %>%
  filter(Forest.ecology...Y.N == "Y") %>%
  select(trait_category, trait=trait_clean, year_wo_RS = Year.of.earliest.publication.w.o..remote.sens.., 
         count_wo_RS = Count.of.references.w.o..remote.sens.., 
         year_w_RS = Year.of.earliest.publication.w...remote.sens.., 
         count_w_RS = Count.of.references.w...remote.sens..) %>%
  mutate_at(c("year_wo_RS", "count_wo_RS", "year_w_RS", "count_w_RS"), as.numeric)# %>%
  #mutate(trait=replace(trait, trait == "canopy/tree height (1/2)", "canopy height")) %>%
  #mutate(trait=replace(trait, trait == "land cover classification", "land cover")) %>%
  #mutate(trait=replace(trait, trait == "above ground biomass", "AGB")) %>%
  #mutate(trait=replace(trait, trait == "diameter at breast height (DBH)", "DBH")) 
head(dat)

#Subset data
count.m <- dat %>% 
  rename(count_UAS_RS=count_w_RS,
         count_UAS=count_wo_RS) %>%
  mutate(count_RS=count_UAS_RS-count_UAS) %>%
  select(trait_category, trait, count_UAS, count_RS) %>%
  melt(id.vars=c("trait_category","trait")) %>%
  mutate(trait_category = as.factor(trait_category))
# count.m <- dat %>% 
#   select(trait_category, trait, count_wo_RS, count_w_RS) %>%
#   melt(id.vars=c("trait_category","trait")) %>%
#   mutate(trait_category = as.factor(trait_category))
year.m <- dat %>% 
  select(trait_category, trait, year_wo_RS, year_w_RS) %>%
  melt(id.vars=c("trait_category","trait")) %>%
  mutate(trait_category = as.factor(trait_category))

# Create column to help plot first RS then UAS+RS
year.m$ascend = ifelse(year.m$variable == "year_w_RS", year.m$value, 0)

# Add colors according to trait category
count.m_w_rs <- count.m %>% 
  filter(variable == "count_RS") %>% 
  arrange(-value) %>%
  # Make some manual changes, since order of traits in count plot doesn't match order of count.m_w_rs. This needs to be the case for axis label coloring to be right
  mutate(trait_category=replace(trait_category, trait=="plant area index", "Physiological"),
         trait_category=replace(trait_category, trait=="photosynthetic efficiency", "Morphological"),
         trait_category=replace(trait_category, trait=="photosynthetic pigments", "Population"),
         trait_category=replace(trait_category, trait=="count of individuals", "Physiological"))
traitcat_col_count <- rev(ifelse(count.m_w_rs$trait_category ==
        "Biochemical","deeppink1",ifelse(count.m_w_rs$trait_category ==
        "Biodiversity","darkgoldenrod2",ifelse(count.m_w_rs$trait_category ==
        "Morphological","darkgreen",ifelse(count.m_w_rs$trait_category ==
        "Phenological","turquoise3",ifelse(count.m_w_rs$trait_category ==
        "Physiological","antiquewhite4","darkviolet"))))))


# Plot 1: year
# Figure 2: year
gg1 <- ggplot(year.m, aes(x=reorder(trait, -ascend), y=value, fill=variable)) +
  geom_bar(position = "dodge", stat="identity") +
  labs(x = "Forest trait", y = "Years of Publication") + 
  scale_y_continuous(trans = shift_trans(2024), breaks=seq(1970,2024,10)) + 
  scale_fill_manual(values=c("navy","skyblue3")) +
  coord_flip() +
  facet_grid(trait_category ~ ., space="free", scales="free", switch="x") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.placement = "outside",
        strip.background = element_rect(fill=NA, colour="grey50"),
        panel.spacing.x=unit(0,"cm"),
        strip.text.y.right = element_text(angle = 0)) 

# Figure 2: count
gg2 <- ggplot(count.m, aes(x = reorder(trait,value), y=value, fill=variable)) +
  geom_bar(position="dodge", stat="identity") +
  labs(x = NULL, y = "Number of Articles (log scale)") +
  scale_y_continuous(trans = mylog_trans(base=10, from=-1), 
                     breaks=c(1,10,100,1000,10000)) +
  scale_x_discrete(position="top") +
    scale_y_log10(minor_breaks = minor_breaks_log(10)) +
  scale_fill_manual(values=c("navy","skyblue3")) +
  coord_flip() +
  facet_grid(trait_category ~ ., space="free", scales="free", switch="x") +
  theme_minimal() +
  theme(legend.position = "none", 
        strip.text.y = element_blank()) 

# Legend
gg0 <- ggplot(year.m, aes(x=reorder(trait, -ascend), y=value)) +
  geom_bar(position = "dodge", stat="identity", aes(fill = variable)) +
  scale_fill_manual(values=c("skyblue3","navy"),
                    name= "WoS Search Scope", 
                    labels = c( "RS (excl UAS)","UAS only")) +
  theme(axis.text.y = element_text(angle = 0, hjust=1, vjust=1)) 
gg_legend <- g_legend(gg0)

# Graphs in one - final Figure 2
grid_final <- grid.arrange(gg1, gg2, gg_legend, layout_matrix= rbind( c(1,1,1,1,2,2,2,3), 
                                                        c(1,1,1,1,2,2,2,3), 
                                                        c(1,1,1,1,2,2,2,3)))
ggsave(file="fig2.png", grid_final, width=10.5, height = 6.5)

### Summary stats

# What is the average count of papers published for each search?
count_UAS <- count.m %>% filter(variable == "count_UAS")
count_UASRS <- count.m %>% filter(variable == "count_RS")
print(paste0(c("Average publications with UAS terms only:",mean(count_UAS$value, na.rm=TRUE))))
# What is the average count of papers published for UAS + RS terms?
print(paste0(c("Average publications with RS terms only:",mean(count_UASRS$value, na.rm=TRUE))))

# What is the most recent trait to enter the literature?
year_UAS <- year.m %>% filter(variable == "year_wo_RS")
year_RS <- year.m %>% filter(variable == "year_w_RS")
print(paste0(c("Average publications with UAS terms only:",max(year_UAS$value, na.rm=TRUE))))
# What is the average count of papers published for UAS + RS terms?
print(paste0(c("Average publications with RS terms only:",max(year_RS$value, na.rm=TRUE))))

# # What is the average earliest year that papers have been published for each search?
# print(paste0(c("Average publications with UAS terms only:",mean(year_UAS$value, na.rm=TRUE))))
# # What is the average count of papers published for UAS + RS terms?
# print(paste0(c("Average publications with UAS+RS terms:",mean(year_RS$value, na.rm=TRUE))))

# What is the average rate of publication since the first year of publication for each trait for each search?

#grab count of papers
#number of papers divided by number of year - make a new column
#check by doing one out by hand
if (identical(year.m$trait,count.m$trait)) {
  rate_df <- year.m %>% 
  mutate(years_since_2024 = 2024-value) %>%
  mutate(publ_rate = count.m$value/years_since_2024)
} else {
  print("Trait columns in year.m and count.m do not match")
}
rate_UAS <- rate_df %>% filter(variable == "year_wo_RS") 
rate_UASRS <- rate_df %>% filter(variable == "year_w_RS")
print(paste0(c("Average publications with UAS terms only:",mean(rate_UAS$publ_rate, na.rm=TRUE))))
print(paste0(c("Average publications with UAS terms only pre-2020:",mean(rate_UAS$publ_rate, na.rm=TRUE))))
print(paste0(c("Average publications with UAS terms only 2020-2023:",mean(rate_UAS$publ_rate, na.rm=TRUE))))
# What is the average count of papers published for UAS + RS terms?
print(paste0(c("Average publications with RS terms:",mean(rate_UASRS$publ_rate, na.rm=TRUE))))

# Earliest and most recent traits
# UAS
year.m %>% filter(variable=="year_wo_RS") %>% arrange(value) %>% head()
year.m %>% filter(variable=="year_wo_RS") %>% arrange(value) %>% tail()
# RS
year.m %>% filter(variable=="year_w_RS") %>% arrange(value) %>% head()
year.m %>% filter(variable=="year_w_RS") %>% arrange(value) %>% tail()

# Publication count
# UAS
count.m %>% filter(variable=="count_UAS") %>% arrange(desc(value)) %>% head()
count.m %>% filter(variable=="count_UAS") %>% arrange(value) %>% head()
# RS
count.m %>% filter(variable=="count_RS") %>% arrange(desc(value)) %>% head()
count.m %>% filter(variable=="count_RS") %>% arrange(value) %>% head()

# Table 2. Select one trait randomly from each catgetory that has between 
# 10 and 100 papers published
count.m %>% filter(variable=="count_UAS") %>% filter(value >=10 &
                                                               value <= 100)
# biochemical: foliar potassium - 10
# biodiversity: species richness - 23
# morphological: (random w number generator) land cover type - 82
# phenological: senescence - 19
# physiological: (random w number generator) NDVI - 44
# population: stem density - 52