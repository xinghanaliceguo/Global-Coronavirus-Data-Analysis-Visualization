MY_UI.R
library(shiny)
library(tidyr)
library(corrr)
library(dplyr)
library(ggplot2)
library(shinyWidgets)

### Page One -Q1
# filter the unique country names in vaccinations dataset
country_unique_vaccination <- raw_vaccinations_df %>%
  select(Country) %>%
  arrange(Country) %>%
  unique() %>%
  pull()

# select list for selecting country
country_input_1 <- selectInput(
  inputId = "country_show_1",
  label = "Select the first country you want to explore",
  choices = country_unique_vaccination,
  selected = "United States"
)

country_input_2 <- selectInput(
  inputId = "country_show_2",
  label = "Select the second country you want to explore",
  choices = country_unique_vaccination,
  selected = "France"
)

country_input_3 <- selectInput(
  inputId = "country_show_3",
  label = "Select the third country you want to explore",
  choices = country_unique_vaccination,
  selected = "India"
)

page_one <- tabPanel(
  h5("Vaccinations and daily new cases"),

  # App title 
  titlePanel("Is there correlation between total vaccinations and daily new Covid cases?"),
  
  br(), # introduce extra vertical spacing
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    # Sidebar panel for inputs 
    sidebarPanel(
      # Input: Select the country name
      country_input_1,
      country_input_2,
      country_input_3,
      
      br(),
      br(),
      
      h3("What insights can we conclude from the data?"),
      p("Through comparing the different correlation values in global countries, 
        we can't have the conclusion that more vaccination is correlated with fewer daily new covid cases. 
        In fact, there are positive and negative correlations with different extents expressed by the results. 
        For instance, there is a strong relationship between these two variables in the United States with a correlation
        of -0.67. However, in India, the correlation becomes positive, which is around 0.35."),
      
      br(),
      br(),
   
      h3("More evaluations about the results"),
      p("The actual purpose we came up with this question is to explore whether the vaccination progress helped slow 
         down the increasing of daily new Covid cases. Although the results don't provide a clear answer, we should take 
         some influences of confounding variables into account, such as population density, total confirmed cases in each country, 
         people's ages."),
      br(),
      p("For the population density, intuitively COVID spread fast in the large population density."),
      br(),
      p("For the second confounding variable, we surprisingly found that there was an obvious downtrend line with small 
         fluctuations in the plot of the United States. The data also showed a strong correlation value of -0.674, 
         which induced that in countries with extremely high confirmed cases, the increase in the number of total 
         administrative doses of vaccines is related to the decrease in daily new cases."),
      br(),
      p("The vaccine is still in the research stage, and there are restrictions on the age of the injection population. 
         It does not significantly help the elderly, but the elderly are the most vulnerable people for COVID-19. 
         This may also be a reason why the correlation is not obvious."),
      br(),
      p("In sum, it still needs a longer time to determine the effects of vaccination progressions.")
    ),
    
    mainPanel(plotOutput("plot_11"),
              br(),
              plotOutput("plot_12"), 
              br(),
              plotOutput("plot_13"))
  )
)

### Page Two - Q2
page_two <- tabPanel(
  h5("Vaccinations and Policy"),
  
  titlePanel("Does the relationship between policy stringency 
             and number of vaccinations change over time?"),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      dateInput(inputId = "date_2", label = "Select a date", format = "yyyy-mm-dd",
                value = "2021-01-01", min = "2020-12-15", max = "2021-01-30",
                language = "en", autoclose = FALSE),
      br(),
      h3("About the question"),
      p("We want to compare the country policy stringency index with total 
        vaccinations administered and see if there’s a correlation between them. 
        We hypothesize that there will be a positive correlation between policy 
        stringency and vaccinations, because countries that take COVID more 
        seriously will have more strict policies and more vaccines. We also want
        to see if this relationship changes over time. The graph on the right
        enables you to explore this question by yourself."),
      br(),
      h3("How to use the graph"),
      p("This graph is showing the relationship between each country's policy 
      stringency and total number of vaccinations administered. One dot 
      represents one country. You can select the date and see how the graph and 
      correlation change over time. Some dates have too few data points to show 
      a significant result, so you may want to explore as many dates as possible."),
      br(),
      h3("Evaluation"),
      p("The answer to our question is yes - but only to an extent. We found 
        that there's a weak positive relationship between policy 
        stringency and number of vaccinations administered. The strength and 
        direction of that correlation doesn't change much over time. This may 
        due to a lot of reasons. For example, we assumed that countries that 
        take COVID more seriously will have more stringent policies and more 
        vaccinations, but maybe countries that have reduced COVID cases by 
        stringent policies do not need vaccinations as much as others.")
    ),
    mainPanel(plotOutput(outputId = "plot_2"), br(), textOutput(outputId = "text_2"))
  )
)


control_plot <- sidebarPanel(
  
   


  selectInput(
    inputId = "selectCountryNameID",
    label = "Select Country",
    choices = c("France"= "France",
                "India"= "India",
                "United States" ="United States"
    ),
    
    
    selected = "United States",
  ), 
  
  selectInput(
    inputId = "lineColor",
    label = "Line Color",
    choices = c("Yellow"= "yellow",
                "Green"= "green",
                "Blue" ="blue"
    ),
    
    
    selected = "blue",
  ),
  
  
  sliderInput(
    inputId ="year",
    "Time Range",
    min = Sys.Date() - 460,
    max = Sys.Date(),
    value =  c(Sys.Date() - 460, Sys.Date())
  ),
  
  
  
  br(),
  br(),
  
  h3("Aim of this analysis"),
  p("My aim was to explore whether the severity of the national policy would affect the number of new cases per day. 
    This can be learned by comparing the daily new cases vs time plot and stringency index vs time plot. By comparing 
    the trend we should be able to conclude their relationship."),
  
  br(),
  br(),
  
  h3("Method Analysis on this data"),
  p("This dataset contains data from more than 100 countries, and it is difficult to show all of them again, 
  so after filtering the top ten countries in terms of Covid19 numbers, three most representative countries were selected from them, 
  namely the United States, the France and India. These three countries are in three different continents and are the most representative."),
  br(),
  
  h3("Evaluation: "),
  
  p("It is easy to see from the two aligned charts above that all three countries had varying degrees of reduction in stringency index before 
    there was a significant increase in new diagnoses. This is not very evident in India, but it is obvious in France and the United States. 
    This suggests that the number of infections rose significantly after the state lowered its control over nationals. Similarly, the number of 
    new infections decreases after the stringency index rises."),
  br(),
  p("Another point worth noting is that the ups and downs of the upper and lower data do not occur at exactly the same time. My guess is that there 
    is usually some time delay associated with the policy taking effect."),
  br(),
  
  
 )
 
 
 
  discribtion_text <- sidebarPanel( 
   
  br(),
  br(),
  
  h3("Aim of this analysis"),
  p("My aim was to explore whether the severity of the national policy would affect the number of new cases per day. 
    This can be learned by comparing the daily new cases vs time plot and stringency index vs time plot. By comparing 
    the trend we should be able to conclude their relationship."),
  
  br(),
  br(),
  
  h3("Method Analysis on this data"),
  p("This dataset contains data from more than 100 countries, and it is difficult to show all of them again, 
  so after filtering the top ten countries in terms of Covid19 numbers, three most representative countries were selected from them, 
  namely the United States, the France and India. These three countries are in three different continents and are the most representative."),
  br(),
  
  h3("Evaluation: "),
  
  p("It is easy to see from the two aligned charts above that all three countries had varying degrees of reduction in stringency index before 
    there was a significant increase in new diagnoses. This is not very evident in India, but it is obvious in France and the United States. 
    This suggests that the number of infections rose significantly after the state lowered its control over nationals. Similarly, the number of 
    new infections decreases after the stringency index rises."),
  br(),
  p("Another point worth noting is that the ups and downs of the upper and lower data do not occur at exactly the same time. My guess is that there 
    is usually some time delay associated with the policy taking effect."),
  br(),
  
)


main_content <- mainPanel(
  
  
  plotOutput("plot_index_cases"),
  plotOutput("plot_stringency"),
  plotOutput("plot_case"),
  
  
)

page_three <- intro_panel <- tabPanel(
  h5("Policies and daily new cases"),
  
  titlePanel("Is there correlation between stringent policies and daily new Covid19 cases?"),
  
  br(),
  
  
  h4(textOutput(outputId = "message")),
  sidebarLayout(
    
    control_plot,
    
    main_content
  )
)


home <- tabPanel(
  h5("Homepage"),
  titlePanel("Report on Global Covid Cases, Vaccination Progress, and Stringency Policies"),
  h3("Created by: Hanxiao Fu, Xinghan Guo, Zhuohui Wu"),
  setBackgroundImage(

    src = "https://fhfairfax.com/wp-content/uploads/2020/07/95a3ea020f556e2ec879012facf0e364.jpg"),
  br(),
  br(),
  
  
  h2("Why do we care?"),
  p("In the current state of Covid-19 ravages, people as well have experienced a painful year. But we finally found a ray 
    of light in the darkness, the Covid-19 vaccine has been successfully manufactured and is now available to the public.
    However, the rate of vaccine generation is still far from expected, and we are not optimistic about the current 
    situation.We want to use data and analysis to understand the current rate of vaccine effectiveness (including 
    prevalence, appointment rates, etc.)"),
  br(),
  p("The main reason we chose this data as our primary study is because it's really relevant to us. We have experienced a 
    full year of online classes in zoom and our group members miss the normal campus life. Given the current situation, 
    we all agree that vaccines are the best helper to help us get back to normal life as soon as possible, while keeping as 
    much social distance. So we were initially very concerned about the status and speed of vaccinations, which will most 
    likely affect whether we can successfully return to school this fall. We also hope to use this study to give a full 
    report to ourselves or to those who share our concern about the need for vaccines."),
  br(),
  p("In addition, we're interested in examining what role does each country's policy stringency play in vaccination 
    progress. We want to see if different policy stringency levels are correlated with different vaccination progress."),
  
  br(),
  
  
  h2("Sources"),
  p("We worked with three data sets in total."),
  strong("1. Covid_Confirmed_Cases Data Set"),
  p("This data set includes COVID-19 global confirmed cases over time."),
  a("Click Here!", href = "https://ourworldindata.org/grapher/covid-daily-vs-total-cases?time=2020-01-22..latest"),
  p("Raw data on confirmed cases are sourced from the COVID-19 Data Repository by the Center for Systems Science and 
     Engineering (CSSE) at Johns Hopkins University. This data set has been collected, 
     aggregated, and operated by Our World in Data. It is updated daily and includes data on confirmed cases."),
  br(),
  strong("2. Vaccination Data Set"),
  p("Data on the COVID vaccination progress in 64 countries from 2020 to 2021. Being updated each day, it counts how 
    many vaccine doses are administered as well has how many people are vaccinated (some may receive more than one dose)."),
  a("Click Here!", href = "https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/vaccinations.csv"),
  p("This data is collected by Our World in Data, a non-profit organization dedicated to data science research on large 
  problems that affect the world. The vaccination data comes from government reports."),
  br(),
  strong("3. Covid-stringency-index Data Set"),
  p("This data is a composite measure based on nine response indicators including school closures, workplace closures,
    and travel bans"),
  a("Click Here!", href = "https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker"),
  p("Source: Thomas Hale, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira (2020). 
    Oxford COVID-19 Government Response Tracker, Blavatnik School of Government."),
  
  br(),
  h3("For more detailed information, please visit our Report"),
  a("Here is the link! :)", href = "https://info201b-wi21.github.io/project-hanxiaof/index.html"),
  br(),
  br()
)



### UI layout
my_ui <- navbarPage(
  "",
  home,
  page_one,
  page_two,
  page_three
)
