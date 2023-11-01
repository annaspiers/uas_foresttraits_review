# resource:
# https://stats.oarc.ucla.edu/r/faq/how-can-i-generate-a-venn-diagram-in-r/

library(limma)
library(dplyr)

dat <- readr::read_csv("Fig1_prep.csv") 

bioch <- ifelse(dat$main_category=="Biochemical",1,
                ifelse(!is.na(dat$category2) & 
                           dat$category2=="Biochemical",1,
                       ifelse(!is.na(dat$category3) & 
                                  dat$category3=="Biochemical",1,0)))
biodiv <- ifelse(dat$main_category=="Biodiversity",1,
                 ifelse(!is.na(dat$category2) & 
                            dat$category2=="Biodiversity",1,
                        ifelse(!is.na(dat$category3) & 
                                   dat$category3=="Biodiversity",1,0)))
morph <- ifelse(dat$main_category=="Morphological",1,
                ifelse(!is.na(dat$category2) & 
                           dat$category2=="Morphological",1,
                       ifelse(!is.na(dat$category3) & 
                                  dat$category3=="Morphological",1,0)))
phen <- ifelse(dat$main_category=="Phenological",1,
               ifelse(!is.na(dat$category2) & 
                          dat$category2=="Phenological",1,
                      ifelse(!is.na(dat$category3) & 
                                 dat$category3=="Phenological",1,
                             ifelse(!is.na(dat$category4) & 
                                        dat$category4=="Phenological",1,0))))
phys <- ifelse(dat$main_category=="Physiological",1,
               ifelse(!is.na(dat$category2) & 
                          dat$category2=="Physiological",1,
                      ifelse(!is.na(dat$category3) & 
                                 dat$category3=="Physiological",1,0)))
pop <- ifelse(dat$main_category=="Population",1,
              ifelse(!is.na(dat$category2) & 
                         dat$category2=="Population",1,
                     ifelse(!is.na(dat$category3) & 
                                dat$category3=="Population",1,0)))

all_cat <- cbind(bioch,phys,biodiv,morph,pop,phen)

all_venn <- vennCounts(all_cat)

vennDiagram(all_venn)
vennDiagram(all_venn, include = "both", 
            #names = c("Biochemical", "Biodiversity", "Morphological",
            #          "Physiological", "Population"), 
            cex = 1, counts.col = "red")

cbind(dat$trait,all_cat)
