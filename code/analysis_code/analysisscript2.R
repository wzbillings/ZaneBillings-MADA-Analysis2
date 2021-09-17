###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse) #for analysis
library(usmap) #for figure creation

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)


#making a boxplot of vaccine coverage estimates across flu seasons 
p1 <- mydata %>% ggplot(aes(x=flu_season, y=coverage_estimate)) + 
  geom_boxplot() +
  labs(title = 'Vaccine Coverage Across Flu Seasons') +
  xlab('Flu Season') +
  ylab('Vaccine Coverage Estimates') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))

#making an additional dataframa, removing all coverage-estimates that are NA
mydatanona <- filter(mydata, coverage_estimate != 'NA')

#making a map with coverage estimates, ranging from lowest (red) to highest (green)
p2 <- usmap::plot_usmap(data = mydatanona, values = 'coverage_estimate', ) +
  scale_fill_continuous(low = 'red', high = 'green', name = 'Coverage (%)') +
  labs(title = 'Vaccine Coverage Across the United States') +
  theme(legend.position = 'right', legend.margin = margin(0.5,1,0.5,1))
  
  

#look at figure
plot(p1)
plot(p2)


#save figures
figure_file = here("results","resultfiguresh.png")
ggsave(filename = figure_file, plot=p1) 
figure_file2 = here("results","resultfigure2sh.png")
ggsave(filename = figure_file2, plot=p2) 



  