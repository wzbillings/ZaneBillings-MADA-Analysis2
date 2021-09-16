##############################
##      analysis script     ##
##############################
#Created by: Tzu-Chun Chu

#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(here) #for data loading/saving
library(tidyverse) #for data wrangling and plotting

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
##   Data exploration/description   ##
######################################

#plot seasonal flu vaccine coverage by state over time
plot.1 <- mydata %>% 
	#calculate and create number of people who were vaccinated as well as lower and upper bounds
	mutate(
		number_people_vaccinated     = coverage_estimate * population_sample_size,
		number_people_vaccinated_lwr = coverage_ci_lwr * population_sample_size,
		number_people_vaccinated_upr = coverage_ci_upr * population_sample_size) %>% 
	#create aggregated vaccine coverage by flu season and state
	group_by(flu_season, state) %>% 
	summarise(
		sum_sample_size       = sum(population_sample_size, na.rm = TRUE),
		sum_people_vaccinated = sum(number_people_vaccinated, na.rm = TRUE),
		sum_coverage_ci_lwr   = sum(number_people_vaccinated_lwr, na.rm = TRUE),
		sum_coverage_ci_upr   =	sum(number_people_vaccinated_upr, na.rm = TRUE)) %>% 
	mutate(
		coverage_estimate_new = sum_people_vaccinated/sum_sample_size,
		coverage_ci_lwr_new   = sum_coverage_ci_lwr/sum_sample_size,
		coverage_ci_upr_new   = sum_coverage_ci_upr/sum_sample_size) %>% 
	#keep only variables needed for plotting
	select(flu_season, state, coverage_estimate_new, coverage_ci_lwr_new, coverage_ci_upr_new) %>% 
	#pivot the data to long format
	tidyr::pivot_longer(
		cols = starts_with("coverage"),
		names_to  = "coverage",
		values_to = "estimate") %>%  
	#plotting
	ggplot(aes(x = as.numeric(flu_season), y = as.numeric(estimate), color = factor(coverage))) +
	geom_line() +
	scale_x_continuous(
		"Flu season",
		breaks = c(3,6,9),
		labels = c("2011-12","2014-15","2017-18")) +
	scale_color_manual(
		"",
		values = c("#868282","#FF0C00","#868282"),
		breaks = c("coverage_ci_upr_new","coverage_estimate_new","coverage_ci_lwr_new"),
		labels = c("95% CI upper bound","coverage estimate","95% CI lower bound")) +
	facet_wrap(~state) +
  theme(legend.position = "right",
  			plot.title = element_text(size = 20, face = "bold")) +
	ylab("Vaccination coverage (%)") +
	ggtitle("Figure 1. Flu Vaccination Coverage by State, 2010-2020")

ggsave(plot.1, filename = here::here("results", "plot.1.png"), height = 9, width = 15)




#plot seasonal flu vaccine coverage by state and age group over time
plot.2 <- mydata %>% 
	mutate(
		number_people_vaccinated = coverage_estimate * population_sample_size) %>% 
	group_by(flu_season, state, age) %>% 
	summarise(
		sum_sample_size       = sum(population_sample_size, na.rm = TRUE),
		sum_people_vaccinated = sum(number_people_vaccinated, na.rm = TRUE)) %>% 
	mutate(
		coverage_estimate_new = sum_people_vaccinated/sum_sample_size) %>% 
	select(flu_season, state, age, coverage_estimate_new) %>% 
	#plotting
	ggplot(aes(x = as.numeric(flu_season), y = as.numeric(coverage_estimate_new), color = age)) +
	geom_line() +
	scale_x_continuous(
		"Flu season",
		breaks = c(3,6,9),
		labels = c("2011-12","2014-15","2017-18")) +
	facet_wrap(~state) +
	theme(legend.position = "top",
				plot.title = element_text(size = 20, face = "bold")) +
	ylab("Vaccination coverage (%)") +
	ggtitle("Figure 2. Flu Vaccination Coverage by State and Age group, 2010-2020")

ggsave(plot.2, filename = here::here("results", "plot.2.png"), height = 9, width = 15)

#plot seasonal flu vaccine coverage by age group over time in the United States
plot.3 <- mydata %>% 
	mutate(
		number_people_vaccinated = coverage_estimate * population_sample_size) %>% 
	group_by(flu_season, age) %>% 
	summarise(
		sum_sample_size           = sum(population_sample_size, na.rm = TRUE),
		sum_people_vaccinated     = sum(number_people_vaccinated, na.rm = TRUE),
		sum_people_vaccinated_new = sum_people_vaccinated/1000000) %>% 
	select(flu_season, age, sum_people_vaccinated_new) %>% 
	#plotting
	ggplot(aes(x = as.numeric(flu_season), y = as.numeric(sum_people_vaccinated_new), fill = age)) +
	geom_bar(position="stack", stat="identity") +
	scale_x_continuous(
		"Flu season",
		breaks = seq(2,11,1),
		labels = c("2010-11","2011-12","2012-13","2013-14","2014-15","2015-16","2016-17","2017-18","2018-19","2019-20")) +
	scale_y_continuous(
		"Number of people vaccinated per million",
		breaks = seq(0, 150,25)) +
	theme_bw() + 
	theme(legend.position = "right",
				axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
				plot.title = element_text(size = 20, face = "bold")) +
	ggtitle("Figure 3. Seasonal Flu Vaccine Doses Administered by Age Group in the United States, 2010-2020")

ggsave(plot.3, filename = here::here("results", "plot.3.png"), height = 9, width = 15)


