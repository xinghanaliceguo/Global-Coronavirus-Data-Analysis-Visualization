library(shiny)
library(tidyr)
library(corrr)
library(dplyr)
library(ggplot2)
library(scales)
library(ggpubr)
library(maps)

raw_vaccinations_df <- read.csv("data/country_vaccinations.csv",
                                stringsAsFactors = FALSE)
global_policy_df <- read.csv("data/covid-stringency-index.csv", 
                             stringsAsFactors = FALSE)
raw_global_cases_df <- read.csv("data/covid-daily-vs-total-cases.csv",
                                stringsAsFactors = FALSE)

# Rename necessary features
raw_global_cases_df <- rename(raw_global_cases_df, "Country" = "Entity")
raw_vaccinations_df <- rename(raw_vaccinations_df, "Date" = "date", "Country" = "country")

# Change character as Date
raw_global_cases_df$Date <- as.Date(raw_global_cases_df$Date, format = "%Y-%m-%d")
# ------------------------------------------------------------------------------------Dataset

# select the columns I need
global_policy_cleaned_2 <- global_policy_df %>% 
  select(Entity, Date, stringency_index) %>%
  rename("Country" = "Entity")

# join policy data frame with vaccination data frame
join_policy_vaccination_2 <- raw_vaccinations_df %>% 
  select(Country, Date, total_vaccinations) %>% 
  left_join(global_policy_cleaned_2, by = c("Date", "Country")) %>% 
  na.omit()

# determine the min and max date of dateInput()
min_date_2 <- join_policy_vaccination_2 %>% 
  filter(Date == min(Date)) %>% 
  pull(Date)

max_date_2 <- join_policy_vaccination_2 %>% 
  filter(Date == max(Date)) %>% 
  pull(Date)

# --------------------------------------------------------------------------------------------

# Define a 'server' function  
my_server <- function(input, output) {
  
  # Output reactive plot 1 -Q1
  output$plot_11 <- renderPlot({
    
    # Necessary Data wranglings - Q1
    new_cases_df_1 <- raw_global_cases_df %>% 
      filter(Country == input$country_show_1) %>%
      mutate(new_cases = Total.confirmed.cases.of.COVID.19 - lag(Total.confirmed.cases.of.COVID.19, order_by = Date)) %>%
      select(Country, Date, new_cases)
    
    raw_vaccinations_df$Date <- as.Date(raw_vaccinations_df$Date, format = "%Y-%m-%d")
    
    
    tot_vaccinations_df_1 <- raw_vaccinations_df %>%
      select(Country, Date, total_vaccinations) %>%
      filter(Country == input$country_show_1, !is.na(total_vaccinations))
      
    joined_df_1 <- left_join(tot_vaccinations_df_1, new_cases_df_1)
    
    correlation_1 <- correlate(joined_df_1$total_vaccinations, joined_df_1$new_cases) %>%
      pull() %>%
      round(digits = 4)
    
    # Create the plot - Q1
    new_cases_vs_tot_vaccination_plot_1 <- 
      ggplot(data = joined_df_1, mapping = aes_string(x = "total_vaccinations", y = "new_cases")) +  
      geom_point() + # point geometry
      geom_smooth() +  # show smooth trendline
      labs(
        title = paste("Daily new Covid cases and number of total vaccinations in", input$country_show_1),
        x = "Number of total vaccinations",
        y = "Daily new Covid Cases",
        caption =  paste("The correlation between the number of total vaccinations and daily new Covid cases in", input$country_show_1, "is", correlation_1)
      )
    
  new_cases_vs_tot_vaccination_plot_1 # return the plot
  })
  

  
  # Output reactive plot 2 -Q1
  output$plot_12 <- renderPlot({
    
    # Necessary Data wranglings - Q1
    new_cases_df_1 <- raw_global_cases_df %>% 
      filter(Country == input$country_show_2) %>%
      mutate(new_cases = Total.confirmed.cases.of.COVID.19 - lag(Total.confirmed.cases.of.COVID.19, order_by = Date)) %>%
      select(Country, Date, new_cases)
    
    raw_vaccinations_df$Date <- as.Date(raw_vaccinations_df$Date, format = "%Y-%m-%d")
    
    
    tot_vaccinations_df_1 <- raw_vaccinations_df %>%
      select(Country, Date, total_vaccinations) %>%
      filter(Country == input$country_show_2, !is.na(total_vaccinations))
    
    joined_df_1 <- left_join(tot_vaccinations_df_1, new_cases_df_1)
    
    correlation_1 <- correlate(joined_df_1$total_vaccinations, joined_df_1$new_cases) %>%
      pull() %>%
      round(digits = 4)

    # Create the plot - Q1
    new_cases_vs_tot_vaccination_plot_1 <- 
      ggplot(data = joined_df_1, mapping = aes_string(x = "total_vaccinations", y = "new_cases")) +  
      geom_point() + # point geometry
      geom_smooth() +  # show smooth trendline
      labs(
        title = paste("Daily new Covid cases and number of total vaccinations in", input$country_show_2),
        x = "Number of total vaccinations",
        y = "Daily new Covid Cases",
        caption = paste("The correlation between the number of total vaccinations and daily new Covid cases in", input$country_show_2, "is", correlation_1)
      )
    
    new_cases_vs_tot_vaccination_plot_1 # return the plot
  })
  
  
  # Output reactive plot 1 -Q1
  output$plot_13 <- renderPlot({
    
    # Necessary Data wranglings - Q1
    new_cases_df_1 <- raw_global_cases_df %>% 
      filter(Country == input$country_show_3) %>%
      mutate(new_cases = Total.confirmed.cases.of.COVID.19 - lag(Total.confirmed.cases.of.COVID.19, order_by = Date)) %>%
      select(Country, Date, new_cases)
    
    raw_vaccinations_df$Date <- as.Date(raw_vaccinations_df$Date, format = "%Y-%m-%d")
    
    
    tot_vaccinations_df_1 <- raw_vaccinations_df %>%
      select(Country, Date, total_vaccinations) %>%
      filter(Country == input$country_show_3, !is.na(total_vaccinations))
    
    joined_df_1 <- left_join(tot_vaccinations_df_1, new_cases_df_1)
    
    correlation_1 <- correlate(joined_df_1$total_vaccinations, joined_df_1$new_cases) %>%
      pull() %>%
      round(digits = 4)
    
    # Create the plot - Q1
    new_cases_vs_tot_vaccination_plot_1 <- 
      ggplot(data = joined_df_1, mapping = aes_string(x = "total_vaccinations", y = "new_cases")) +  
      geom_point() + # point geometry
      geom_smooth() +  # show smooth trendline
      labs(
        title = paste("Daily new Covid cases and number of total vaccinations in", input$country_show_3),
        x = "Number of total vaccinations",
        y = "Daily new Covid Cases",
        caption = paste("The correlation between the number of total vaccinations and daily new Covid cases in", input$country_show_3, "is", correlation_1)
      )
    
    new_cases_vs_tot_vaccination_plot_1 # return the plot
  })

 #-----------------------------------------------------------------------------Q2 
  
  
  # make a plot for policy stringency vs. vaccinations
  output$plot_2 <- renderPlot({
    ggplot(
      data = filter(join_policy_vaccination_2, Date == input$date_2),
      mapping = aes(x = stringency_index, y = total_vaccinations)
    ) +
      geom_point(size = 3, color = "purple") +
      geom_smooth(color = "gold") +
      scale_y_continuous(labels = function(x) format(x, scientific = FALSE, big.mark = ",")) +
      labs(
        title = "Country policy stringency vs. Total Vaccinations",
        x = "Stringency index",
        y = "Total number of vaccinations administered")
  })
  
  output$text_2 <- renderText({
    # the data used to do analysis, change as the user input change
    analysis_data_2 <- filter(join_policy_vaccination_2, Date == input$date_2)
    
    # calculate the correlation between policy and vaccination
    policy_vaccination_correlation_2 <- cor(
      analysis_data_2$stringency_index, y = analysis_data_2$total_vaccinations
    ) %>% 
      round(digits = 4)
    
    # display correlation analysis result (interactive)
    my_message_2 <- paste0("On ", input$date_2, ", the correlation between 
                           policy stringency and number of vaccinations is ", 
                           policy_vaccination_correlation_2, ".")
    return(my_message_2)
  })
  
  #-----------------------------------------------------------------------------------------Q3
  
  covid_stringency_df <- read.csv("data/covid-stringency-index.csv", 
                                  stringsAsFactors = FALSE)
  
  case_vs_time_df <- read.csv("data/covid-daily-vs-total-cases.csv", 
                              stringsAsFactors = FALSE)
  
  
  covid_stringency_df$Date <- as.Date.character(covid_stringency_df$Date, format = "%Y-%m-%d")
  
  
  
  
  
  country_new_cases_df <- case_vs_time_df %>% 
    filter(Entity == "India" | Entity == "France" | Entity == "United States") %>% 
    select(Entity,Date,Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.) %>% 
    drop_na()
  
  
  country_new_cases_df$Date <- as.Date(country_new_cases_df$Date, format = "%Y-%m-%d")
  
  country_new_cases_df <- country_new_cases_df %>% 
    group_by(Date)
  
  
  joined_newcase_stringency_df <- left_join(country_new_cases_df, covid_stringency_df , by = c("Entity","Date"))
  
  joined_newcase_stringency_df <- joined_newcase_stringency_df %>% 
    select(Entity,Date,Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.,stringency_index) %>% 
    drop_na()
  
  
  
  output$plot_stringency <- renderPlot({  
    
    newcase_with_stringency_plot <- ggplot(data= joined_newcase_stringency_df<-subset(joined_newcase_stringency_df, Entity == input$selectCountryNameID), 
                                           mapping = aes(x = Date, y = stringency_index, color = Entity)
    ) +
      geom_line(color = input$lineColor)+
      scale_y_continuous(labels = comma)+
      #scale_x_date(guide = guide_axis(check.overlap = TRUE))+
      scale_color_brewer(palette = "Dark2") +  
      labs(
        title = "stringency index vs date",
        x = "Date",
        y = "stringency index",
        color = "Country"
      )+
      scale_x_date(limits = input$year)
    
    return(newcase_with_stringency_plot)
    
  })
  
  
  
  output$plot_case <- renderPlot({  
    case_vs_time_newcase_plot <- ggplot(data = country_new_cases_df<-subset(country_new_cases_df, Entity == input$selectCountryNameID), 
                                        mapping = aes(x = Date, y = Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.)
    ) +
      geom_line(color = input$lineColor)+
      scale_y_continuous(labels = comma)+
      #scale_x_date(guide = guide_axis(check.overlap = TRUE))+
      scale_color_brewer(palette = "Dark2") +  
      labs(
        title = "Daily new confirmed cases vs date",
        x = "Date",
        y = "number of cases",
        color = "Country"
      )+
      scale_x_date(limits = input$year)
    return(case_vs_time_newcase_plot)
  })
  
  output$plot_index_cases <- renderPlot({
      
      ana_df <- joined_newcase_stringency_df %>% 
        filter(Entity == input$selectCountryNameID)
      
      correlation_index <- correlate(ana_df$stringency_index, ana_df$Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.) %>%
        pull()
      
      
      correlation_index_round <- round(correlation_index, digits = 4)
      
      
      
      
      new_cases_vs_index_plot_1 <- 
        ggplot(data = joined_newcase_stringency_df<-subset(joined_newcase_stringency_df, Entity == input$selectCountryNameID), mapping = aes(x = stringency_index, y = Daily.new.confirmed.cases.due.to.COVID.19..rolling.7.day.average..right.aligned.)) +  
        geom_point() + # point geometry
        geom_smooth(color = input$lineColor) +  # show smooth trendline
        labs(
          title = paste("Daily new Covid cases and stringency_index in",  input$selectCountryNameID),
          x = "stringency_index",
          y = "Daily new Covid Cases",
          caption = paste("The correlation between stringency_index and daily new Covid cases in",  input$selectCountryNameID, "is", correlation_index_round)
        )  
      
      return(new_cases_vs_index_plot_1)
      
    
  })
  
}
