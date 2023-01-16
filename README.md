# Global-Coronavirus-Data-Analysis-Visualization
## Author
* Xinghan Guo
* Xiaofu Han
* Zhuohui Wu
## Problem Domain
### Our areas of focus
```
In the current state of Covid19 ravages, people as well have experienced a painful year.
But we finally found a ray of light in the darkness, the Covid19 vaccine has been successfully
manufactured and is now available to the public.However, the rate of vaccine generation is still
far from expected, and we are not optimistic about the current situation.We want to use data and
analysis to understand the current rate of vaccine effectiveness (including prevalence, appointment rates, etc.)

```

### Background
```
Currently, mRNA vaccines from two companies, Pfizer and BioNTech, are widely used in the U.S.
This form of vaccine differs from all previous vaccines in that they are not in the form of an
attenuated vaccine and therefore theoretically have a better acceptance rate in humans
The vaccine has now been clinically tested and no significant side effects have been reported,
but many people are still skeptical.
```

### Why are we interested？
```
The main reason we chose this data as our primary study is because it's really relevant to us
We have experienced a full year of online classes in zoom and our group members miss the normal
campus life.Given the current situation, we all agree that vaccines are the best helper to help
us get back to normal life as soon as possible, while keeping as much social distance.So we were
initially very concerned about the status and speed of vaccinations, which will most likely affect
whether we can successfully return to school this fall.We also hope to use this study to give a full
report to ourselves or to those who share our concern about the need for vaccines.
```

### How we going to do it
```
The data on vaccines are still changing, and because this is a recent event, not many studies have
been reported.However, we will try to find authoritative data in the CDC's official website and refer
to some of the data that third-party organizations provided.We do our best to keep our data accurate
and up-to-date
```

### Current Research
* Covid updates: Nearly 28 million vaccine doses doled out in U.S.; Novavax upping monthly vaccine production link From CNBC, a report on vaccine production efficiency and speculation about the future

* WHO data dashboard:Covid19 link Data from the World Health Organization (WHO), including infection rates, mortality rates, and other relevant data for the entire United States

* KFF COVID-19 Vaccine Monitor link Real-time, updated vaccine-related data from third-party websites

## Analysis Questions
1. Which country has the greatest coverage of COVID vaccine (i.e. largest vaccinated-to-total-population ratio)? Which country has the least coverage?
* We're interested in this question because we want to see if there's a demographic difference in vaccination - are countries that are more well off more likely to get more vaccines?
* Our datasets about global COVID infected cases and vaccination progress can help to answer this.
2. Is there a relationship between COVID-19 infection rate and temperature?
* We are interested in this question because we want to see if there's a relationship between COVID transmission and climate.
* Our datasets about global COVID infected cases and weather data can help to answer this.
3. How many people have been vaccinated in total?
* We're interested in this question because we want to know how far we've gone in vaccination process.
* Our dataset about vaccination progress can help to answer this.

## The Data Sets
1. * **COVID-19 world vaccinaions progression (day-to-day)**
   * [Click to see the URL](https://github.com/owid/covid-19-data/blob/master/public/data/vaccinations/vaccinations.csv)
   * This data has been collected, aggregated, and documented by Cameron Appel, Diana Beltekian, Daniel Gavrilov, Charlie Giattino, Joe Hasell, Bobbie Macdonald, Edouard Mathieu, Esteban Ortiz-Ospina, Hannah Ritchie, Max Roser.
   * This data set can help answer the analysis questions 1 & 3.
2. * **COVID-19 global confirmed cases (time series)**
   * [Click to see the URL](https://www.kaggle.com/datasets/sudalairajkumar/novel-corona-virus-2019-dataset?select=time_series_covid_19_confirmed.csv)
   * This data set has been collected, aggregated, and operated by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE). Also, supported by ESRI Living Atlas Team and the Johns Hopkins University Applied Physics Lab (JHU APL). Data is extracted from the google sheets associated, modified and made available in kaggle by user SRK.
   * This data set can help answer the analysis questions 1 & 2.
3. * **COVID-19 global weather from 22/1/2020 to 21/3/2020**
   * [Click to see the URL](https://www.kaggle.com/datasets/winterpierre91/covid19-global-weather-data)
   * The source of this data set came from World Weather Online (WWO). This data set was collected by Pierre Winter through API requests by city and made available in Kaggle.
   * This data set can help answer the analysis question 2.
