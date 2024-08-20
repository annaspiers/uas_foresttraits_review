# This script creates a stacked bar graph of sensors used to estimate 
# certain traits. Used to be a table in first submission to Landscape Ecology

# Objective: Identify subset of our identified forest traits and count which 
# papers use what type of sensor(s) to measure each trait

library(ggplot2)
# library(reshape2) #melt
library(dplyr)
library(tidyr) #pivot_longer
# library(lemon) #g_legend
# library(gridExtra)
# require(scales) #mylog_trans
library(viridis)

rawdat <- data.frame(readr::read_csv("Fig3_prep.csv")) 

dat <- rawdat %>%
    dplyr::select(-WoS.search) %>%
    tidyr::pivot_longer(cols=RGB:lidar, names_to = "sensor", values_to = "count") %>%
    rename(category=Trait.category) %>%
    rename(trait=Trait) %>%
    mutate(trait_label=paste0(trait," \n (",category,")"))

sensor_order <- c("lidar", "RGB", "multispectral", "hyperspectral", "thermal")
    
# dat$sensor <- reorder(dat$sensor, dat$count)
# dat$sensor <- factor(dat$sensor, levels=rev(levels(dat$sensor)))

cbp1 <- c("#0b84a5","#f6c85f","#6f4e7c","#9ed867","#ca472f")

finalfig <- dat %>% 
    # ggplot(aes(x=trait_label,-count, y=count, fill=sensor)) +
    ggplot(aes(x=reorder(trait_label,-count), y=count, fill=factor(sensor, sensor_order))) +
    geom_bar(position="stack", stat="identity") +
    labs(
        x = "Forest trait or characteristic",
        y = "Count",
        fill = "Sensor"
    ) +
    scale_fill_manual(values = cbp1)  +
    theme(axis.text.x = element_text(angle = 40, hjust = 1))
finalfig

ggsave(file="fig3.pdf", finalfig, width=6, height=4.5, dpi=300)
ggsave(file="fig3.png", finalfig, width=6, height=4.5, dpi=300)
