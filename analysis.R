library(dplyr)
library(ggplot2)
library(tidyr)
library(corrr)
library(maps)
library(scales)
library(ggpubr)

raw_global_cases_df <- read.csv("data/covid-daily-vs-total-cases.csv",
                                stringsAsFactors = FALSE)
raw_vaccinations_df <- read.csv("data/country_vaccinations.csv",
                                stringsAsFactors = FALSE)
global_policy_df <- read.csv("data/covid-stringency-index.csv", 
                             stringsAsFactors = FALSE)

raw_vaccinations_df <- rename(raw_vaccinations_df, "Date" = "date", "Country" = "country")

raw_vaccinations_df$Date <- as.Date(raw_vaccinations_df$Date)

global_policy_df$Date <- as.Date(global_policy_df$Date, format = "%Y-%m-%d")

#View(case_vs_time_df )
#View(global_policy_df)


#Top 1 cases #work data

top_1 <- raw_global_cases_df %>%
  arrange(-Total.confirmed.cases.of.COVID.19) %>%
  head(1) %>% 
  pull(Entity)

#top_1

#set data

country_cases_df <- raw_global_cases_df %>% 
  filter(Entity == "World" | Entity == "United Kingdom" | Entity == "United States") %>% 
  select(Entity,Date,Total.confirmed.cases.of.COVID.19) %>% 
  drop_na()
  
country_cases_df$Date <- as.Date(country_cases_df$Date, format = "%Y-%m-%d")
 
country_cases_df <- country_cases_df %>% 
  group_by(Date)

#data 

data_name <- colnames(raw_global_cases_df)

#find out date range for this data


drop_na_date <- na.omit(country_cases_df$Date)
range_of_date <- range(drop_na_date)
 
range_of_date


#range for stringency


drop_range_stringency_na_date <- na.omit(global_policy_df$Date)
range_stringency<- range(drop_range_stringency_na_date)

#table

table_stringency <- global_policy_df %>% 
filter(Entity == "United States" & Date == "2020-03-02")

#United states graph

us_stringency_df <- global_policy_df %>% 
  filter(Entity == "United States" )



us_stringency_plot <- ggplot(data=us_stringency_df, 
                                       mapping = aes(x = Date, y = stringency_index, color = Entity)
) +
  geom_line()+
 
  scale_y_continuous(labels = comma)+
  
  scale_x_date(guide = guide_axis(check.overlap = TRUE))+
  scale_color_brewer(palette = "Dark2") +  
  labs(
    title = "stringency index vs date (US)",
    x = "Date",
    y = "stringency index",
    color = "Country"
  )


#us_stringency_plot


#world total  max and average daily increase rate

world_max_cases <- country_cases_df %>% 
  filter(Entity == "World" ) %>% 
  arrange(desc(Total.confirmed.cases.of.COVID.19)) %>% 
  head(1) %>% 
  pull(Total.confirmed.cases.of.COVID.19)
  
#max cases 111721885


total_days <- as.numeric(diff(range_of_date))

daily_rate <- world_max_cases/total_days

# daily_rate 281415.3 cases per day




#View(country_cases_df)

#plot total cases over date

case_vs_time_plot <- ggplot(data=country_cases_df, 
                            mapping = aes(x = Date, y = Total.confirmed.cases.of.COVID.19, color = Entity)
) +
   geom_line()+
   scale_y_continuous(labels = comma)+
   #scale_x_date(date_breaks = "1 month", date_labels = "%m")+
   #scale_x_date(guide = guide_axis(n.dodge=3))+
   scale_x_date(guide = guide_axis(check.overlap = TRUE))+
   scale_color_brewer(palette = "Dark2") +  
   labs(
    title = "Total confirmed cases vs date",
    x = "Date",
    y = "number of cases",
    color = "Country"
  )


#case_vs_time_plot

#manage daily new cases vs date

country_new_cases_df <- raw_global_cases_df %>% 
  filter(Entity == "India" | Entity == "France" | Entity == "United States") %>% 
  select(Entity,Date,Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.) %>% 
  drop_na()



country_new_cases_df$Date <- as.Date(country_new_cases_df$Date, format = "%Y-%m-%d")

country_new_cases_df <- country_new_cases_df %>% 
group_by(Date)

#View(country_new_cases_df)


case_vs_time_newcase_plot <- ggplot(data=country_new_cases_df, 
                                    mapping = aes(x = Date, y = Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned., color = Entity)
) +
  geom_line()+
  scale_y_continuous(labels = comma)+
  scale_x_date(guide = guide_axis(check.overlap = TRUE))+
  scale_color_brewer(palette = "Dark2") +  
  labs(
    title = "Daily new confirmed cases vs date",
    x = "Date",
    y = "number of cases",
    color = "Country"
  )+ 
    facet_wrap(~Entity) 



#case_vs_time_newcase_plot


#joined table

joined_newcase_stringency_df <- left_join(country_new_cases_df, global_policy_df , by = c("Entity","Date"))

joined_newcase_stringency_df <- joined_newcase_stringency_df %>% 
select(Entity,Date,Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.,stringency_index) %>% 
  drop_na()


#present data table joined_newcase_stringenc


selected_dates <- joined_newcase_stringency_df %>% 
  filter(Date == "2020-02-07"| Date == "2020-8-07"| Date == "2021-01-07")


#selected_dates with data presented


#View(joined_newcase_stringency_df)


#plot graph


newcase_with_stringency_plot <- ggplot(data=joined_newcase_stringency_df, 
                                    mapping = aes(x = Date, y = stringency_index, color = Entity)
) +
  geom_line()+
  #geom_smooth(method = "loess")+
  scale_y_continuous(labels = comma)+
  #scale_x_date(date_breaks = "1 month", date_labels = "%m")+
  scale_x_date(guide = guide_axis(check.overlap = TRUE))+
  scale_color_brewer(palette = "Dark2") +  
  labs(
    title = "stringency index vs date",
    x = "Date",
    y = "stringency index",
    color = "Country"
  )+
   facet_wrap(~Entity) 



#newcase_with_stringency_plot



Combined_newcases_stringency <- ggarrange(case_vs_time_newcase_plot, newcase_with_stringency_plot, heights = c(2,2),
          ncol = 1, nrow = 2, align = "v")

#Combined_newcases_stringency

# Section 2 - Confirmed Cases data set


# Some simple Data wranglings, The "global_casaes_df" will be the main data set used later.
global_cases_df <- raw_global_cases_df %>% 
  filter(Code != "", !is.na(Total.confirmed.cases.of.COVID.19), Date != "", Entity != "World") %>%
  select(Entity, Code, Date, Total.confirmed.cases.of.COVID.19) %>%
  rename(c("Country" = "Entity", "confirmed_cases" = "Total.confirmed.cases.of.COVID.19")) 

# Sample table - Data Set: Confirmed cases
sample_cases_df <- global_cases_df[order(global_cases_df$confirmed_cases), ] %>%
  head()

# Summary Statistics - Data Set: Confirmed cases
summary_statistics <- global_cases_df %>%
  filter(Date == max(Date)) %>%
  select(confirmed_cases) %>%
  summary()
most_recent_date <- global_cases_df %>%
  filter(Date == max(Date)) %>%
  head(n = 1) %>%
  select(Date) %>%
  pull()

# Global covid confirmed cases trend plot 
distribution_cases_df <- global_cases_df %>%
  filter(Date == max(Date)) %>%
  arrange(confirmed_cases) %>%
  tail(n = 5) %>%
  select(Country) # representing five countries with most cases until the most recent date
trends_of_top5_countries_with_most_cases_df <- left_join(distribution_cases_df, global_cases_df)
trends_of_top5_countries_with_most_cases_df$Date <- as.Date(trends_of_top5_countries_with_most_cases_df$Date)

cases_trend_plot <- ggplot(data = trends_of_top5_countries_with_most_cases_df, 
                           mapping = aes(x = Date, y = confirmed_cases, color = Country, alpha = 0.3)
) +  
  geom_point() + # point geometry
  scale_color_brewer(palette = "Dark2") + # give different colors to the lines
  labs(
    title = "Global Covid Confirmed Cases Over Time",
    x = "Date",
    y = "Cases",
    color = "Country")

# vaccination dataset
# a sample of the dataset
# filtered for the most recent date
vaccination_sample <- raw_vaccinations_df %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  select(Country, Date, total_vaccinations, people_vaccinated, people_fully_vaccinated) %>% 
  head()

# summary descriptive statistics
vaccination_summary <- raw_vaccinations_df %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  select(Country, Date, total_vaccinations, people_vaccinated, people_fully_vaccinated) %>% 
  ungroup()

# see which country has the largest/smallest number of vaccinations so far
# and how many vaccinations do they have
most_vaccination_country <- vaccination_summary %>% 
  filter(total_vaccinations == max(total_vaccinations)) %>% 
  pull(Country)

most_vaccination_num <- vaccination_summary %>% 
  filter(total_vaccinations == max(total_vaccinations)) %>% 
  pull(total_vaccinations) %>% 
  format(scientific = FALSE, big.mark = ",")

least_vaccination_country <- vaccination_summary %>% 
  filter(total_vaccinations == min(total_vaccinations)) %>% 
  pull(Country)

least_vaccination_num <- vaccination_summary %>% 
  filter(total_vaccinations == min(total_vaccinations)) %>% 
  pull(total_vaccinations) %>% 
  format(scientific = FALSE, big.mark = ",")

# the date on which the newest data for the country with least COVID vaccinations was collected
least_vaccination_date <- vaccination_summary %>% 
  filter(total_vaccinations == min(total_vaccinations)) %>% 
  pull(Date)

# three countries that have most vaccinations to date
top_three_vaccination <- raw_vaccinations_df %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  slice_max(order_by = total_vaccinations, n = 3, with_ties = FALSE) %>% 
  select(Country, Date, total_vaccinations)

# section 3
# question 3: Do countries that have more stringent policies administer more vaccinations?

global_policy_cleaned <- global_policy_df %>% 
  select(Entity, Date, stringency_index) %>% 
  
  # calculate each country's mean stringency over time
  group_by(Entity) %>% 
  mutate(stringency_mean = mean(stringency_index)) %>% 
  ungroup() %>% 
  rename("Country" = "Entity")

# because the policy stringency dataset is newer than the vaccination one, I joined them by date
# so that I can omit the "excess" rows in policy dataset
join_policy_vaccination <- raw_vaccinations_df %>% 
  select(Country, Date, total_vaccinations) %>% 
  left_join(global_policy_cleaned, by = c("Date", "Country"))

# filter for the newest data and omit the rows that contain NA value
most_recent_policy <- join_policy_vaccination %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  na.omit() %>% 
  select(Country, total_vaccinations, stringency_mean) %>% 
  
  # arrange the stringency index from lowest to highest
  arrange(stringency_mean)
  

# test the correlation between policy stringency and number of vaccinations
policy_vaccination_correlation <- cor.test(
  most_recent_policy$stringency_mean, 
  most_recent_policy$total_vaccinations)

# the speed of data collection varies across countries
# here we can show that by "to date" we mean different date for different countries
top_three_vaccination_date <- raw_vaccinations_df %>% 
  group_by(Country) %>% 
  filter(Date == max(Date)) %>% 
  ungroup() %>% 
  slice_max(order_by = total_vaccinations, n = 3, with_ties = FALSE) %>% 
  select(Country, Date)

usa_vaccination_date <- top_three_vaccination_date %>% 
  filter(Country == "United States") %>% 
  pull(Date)

china_vaccination_date <- top_three_vaccination_date %>% 
  filter(Country == "China") %>% 
  pull(Date)

uk_vaccination_date <- top_three_vaccination_date %>% 
  filter(Country == "United Kingdom") %>% 
  pull(Date)

# visualization using bar chart
vaccination_summary_graph <- ggplot(data = top_three_vaccination) +
  geom_col(mapping = aes(x = Country, y = total_vaccinations)) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
  labs(
    title = "Countries that administered the most COVID vaccinations to date",
    x = "Country",
    y = "Total number of vaccinations administered"
  )

## Section 3 - Confirmed Cases data set
# 1. Is more vaccination correlated with less daily new covid cases?
max_cases_df <- global_cases_df %>%
  filter(Date == max(Date)) %>%
  arrange(confirmed_cases) %>%
  tail(n = 10) %>%
  select(Country)

tot_vaccinations_df <- raw_vaccinations_df %>%
  select(Country, Date, total_vaccinations) %>%
  filter(Country == "India" | Country == "France" | Country == "Spain" | Country == "United States",
         !is.na(total_vaccinations)) 

tot_vaccinations_df$Date <- as.Date(tot_vaccinations_df$Date)


new_cases_df <- global_cases_df %>%
  filter(Country == "France" | Country == "India" | Country == "Spain" | Country == "United States") %>%
  group_by(Country) %>%
  mutate(new_cases = confirmed_cases - lag(confirmed_cases, order_by = Date)) %>%
  select(Country, Date, new_cases)
new_cases_df$Date <- as.Date(new_cases_df$Date)

# Result of analysis_table
joined_df <- left_join(tot_vaccinations_df, new_cases_df)

# Create analysis plot - France, India, Spain
joined_3_countries_df <- joined_df %>%
  filter(Country == "France" | Country == "India" | Country == "Spain")
new_cases_vs_tot_vaccination_plot_1 <- ggplot(data = joined_3_countries_df) +
  geom_line(mapping = aes(x = total_vaccinations, y = new_cases, color = Country)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "The correlation between daily new cases with total vaccinations doses",
    x = "Total Vaccinations",
    y = "Daily New Cases",
    color = "Country"
  ) + 
  facet_wrap(~Country)

# Create analysis plot - United States
joined_us_df <- joined_df %>%
  filter(Country == "United States")
new_cases_vs_tot_vaccination_plot_2 <- ggplot(data = joined_us_df) +
  geom_line(mapping = aes(x = total_vaccinations, y = new_cases, color = Country)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "The correlation between daily new cases with total vaccinations doses",
    x = "Total Vaccinations",
    y = "Daily New Cases",
    color = "Country"
  ) + 
  facet_wrap(~Country)


# Calculate the correlations
France_df <- joined_df %>%
  filter(Country == "France")
France_correlation <- correlate(France_df$total_vaccinations, France_df$new_cases) %>%
  pull()

India_df <- joined_df %>%
  filter(Country == "India")
India_correlation <- correlate(India_df$total_vaccinations, India_df$new_cases) %>%
  pull()

Spain_df <- joined_df %>%
  filter(Country == "Spain")
Spain_correlation <- correlate(Spain_df$total_vaccinations, Spain_df$new_cases) %>%
  pull()

US_df <- joined_df %>%
  filter(Country == "United States")
US_correlation <- correlate(US_df$total_vaccinations, US_df$new_cases) %>%
  pull()


summary_values_df <- head(joined_df)
country_name <- c("France", "India", "Spain", "United States")
correlations <- c(France_correlation, India_correlation, Spain_correlation, US_correlation)
summary_correlation_results <- data_frame(Country = country_name, Correlation = correlations)

# visualization for policy and vaccination, each dot is a country
policy_vs_vaccination_graph <- ggplot(data = most_recent_policy, mapping = aes(x = stringency_mean, y = total_vaccinations)) +
  geom_point(alpha = 0.5, size = 4) +
  geom_smooth() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
  labs(
    title = "Country policy stringency vs. Total Vaccinations",
    x = "Stringency index",
    y = "Total number of vaccinations administered"
  )

