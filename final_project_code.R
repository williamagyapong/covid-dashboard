#------------------------------------------------------------------------------------------
# Course: STA 504/SPRING 2020                                                             #
# Title: Group Project  Final Version                                                     #
# Authors: Kelvin Njuki, section A                                                        #
#          William Ofosu Agyapong, section A                                              #
# Date last updated: May 11, 2020                                                         #
#------------------------------------------------------------------------------------------

#Loading all necessary packages
library(tidyverse)
library(lubridate)
library(plotly)

#====================================================================================================#
#---------------------------------DATA AGGREGATION AND PROCESSING------------------------------------#
#====================================================================================================#

#Reading in covid_19 confirmed cases dataset 
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
#Converting it from wide to tall format
confirmed_tall <- confirmed %>%
  gather("date","cases",colnames(confirmed[str_detect(colnames(confirmed),"/")])) %>%
  mutate(row_id=row_number())

#Reading in covid_19 deaths datasets
deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
#Converting it from wide to tall format
deaths_tall <- deaths %>%
  gather("date","deaths",colnames(deaths[str_detect(colnames(deaths),"/")])) %>%
  select(date,deaths) %>%
  mutate(row_id=row_number())

#Joining the confirmed cases data with deaths data and changing date format from character to date format
confirmed_deaths <- left_join(confirmed_tall,deaths_tall,by="row_id")
confirmed_deaths <- confirmed_deaths %>%
  mutate(date=date.y) %>%
  mutate(date=as.Date(date,"%m/%d/%y")) %>% #Changing date variable from character to date format
  rename(state="Province_State") %>% #Renaming Province_State column
  select(state,date,cases,deaths) %>%
  group_by(date,state) %>%
  summarize(state_cases=sum(cases),state_deaths=sum(deaths)) %>%
  na.omit()

#Reading in the covid-19 recovered cases
recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
recovered_us <- recovered %>%
  filter(`Country/Region`=="US") 
#Converting it from wide to tall format
recovered_us_tall <- recovered_us %>%
  gather("date", "recovered",-c("Province/State","Country/Region","Lat","Long")) %>%
  mutate(date=as.Date(date,"%m/%d/%y"))

#Getting the last updated date for display in plots subtitles
dates_vec <- unique(confirmed_deaths$date)
current_date <- dates_vec[length(dates_vec)]
current_date_str <- paste(month(current_date,label=T, abbr=T), paste0(day(current_date), ","), year(current_date))

#====================================================================================================#
#------------------------------------CREATING PLOTS OF INTEREST--------------------------------------#
#====================================================================================================#

#-------------------------------------OVERALL TIME SERIES PLOT---------------------------------------#

#Creating a summary of confirmed cases and deaths by date
covid_summary <- confirmed_deaths %>%
  group_by(date) %>%
  summarize(total_cases=sum(state_cases),
            total_deaths=sum(state_deaths))

#Creating an overall time series plot (Entire United States)
overall_time_plot <- ggplot() +
  geom_line(aes(x=date, y=total_deaths,color="Deaths"),
            data = covid_summary) +
  geom_line(aes(x=date, y=total_cases,color="Confirmed"),data = covid_summary) +
  geom_line(aes(x=date, y=recovered,color="Recovered"),data = recovered_us_tall) +
  labs(x="Month", y="Total", title="United States Covid_19 Time Series plot", colour="Status",
       caption = "Data Source: https://github.com/CSSEGISandData/COVID-19",
       subtitle = paste("From Jan 22, 2020 to ", current_date_str)) +
  scale_y_continuous(labels=scales::comma) +
  #scale_y_log10() +
  theme_classic() +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
ggplotly(overall_time_plot) %>%
  layout(title = list(text = paste0("United States Covid_19 Time Series plot",
                                    '<br>',
                                    '<sup>',
                                    paste("From Jan 22, 2020 to ", current_date_str),
                                    '</sup>')),
         legend = list(orientation = "h", x = 0.3, y =-0.25)
  ) %>%
  config(displayModeBar = F) #Removing plotly tool bar

#-------------------------------------STATE(S) TIME SERIES PLOT---------------------------------------#

#Creating a subset data for a single state
state_confirmed_deaths <- confirmed_deaths %>%
  filter(state=="Ohio") %>%
  group_by(date) %>%
  summarize(state_cases=sum(state_cases),
            state_deaths=sum(state_deaths))

#Generating a time series plot for a given state
state_time_plot <- ggplot() +
  geom_line(aes(x=date, y=state_cases,color="Confirmed"),
            data = state_confirmed_deaths) +
  geom_line(aes(x=date, y=state_deaths,color="Deaths"),
            data = state_confirmed_deaths) +
  labs(x="Month", y="Total", title="Covid_19 Time Series plot in Ohio", colour="Status",
       subtitle = paste("From Jan 22, 2020 to ", current_date_str),
       caption = "Data Source: https://github.com/CSSEGISandData/COVID-19") +
  scale_y_continuous(labels=scales::comma) +
  #scale_y_log10() +
  theme_classic() +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
ggplotly(overall_time_plot) %>%
  layout(title = list(text = paste0("United States Covid_19 Time Series plot",
                                    '<br>',
                                    '<sup>',
                                    paste("From Jan 22, 2020 to ", current_date_str),
                                    '</sup>')),
         legend = list(orientation = "h", x = 0.3, y =-0.25)
  ) %>%
  config(displayModeBar = F) #Removing plotly tool bar

#----------------------------------FLUCTUATION PLOT FOR 5 STATES------------------------------------#

# a function to generate daily observations from cumulative values in datasets

cum_to_daily <- function(data) {
  # deriving daily confirmed cases
  cases <- data[str_detect(colnames(data), "/")] # create a dataset of the cases for each day
  col_names <- colnames(cases) # extract column names for later use
  zeros <- data.frame(zeros = rep(0, nrow(data))) # create a dummy column of zeros
  cases <-data.frame(zeros, cases) # append a dummy column of zeros to aid derivation
  daily_cases <- cases # assign cases to a new dataset
  
  for (i in 1:length(cases)) {
    for (j in 1:nrow(cases)) {
      # generate daily value
      daily_value <- cases[j,i+1] - cases[j,i]
      if(daily_value < 0) {
        # this is where error in data entry is detected. Daily value can't be negative.
        daily_value = 0 # in such a situation our best guess would be to assume there was no increase
      }
      
      # assign computed daily value to the dataset
      daily_cases[j,i+1] = daily_value
    }
    
    if(i == (length(cases)-1)){
      break # exit loop
    }
  }
  
  #assembling datasets
  daily_cases <- daily_cases[,-1] # remove the column of zeros initially appended
  colnames(daily_cases) <- col_names # assign the original column names
  daily_cases_full <- cbind(data[!(str_detect(colnames(data), "/"))], daily_cases)
  
  return(daily_cases_full)
}

# Creating a fluctuation plot to compare the distribution of confirmed cases in 5 states
# Create a contingency table between state and confirmed cases
daily <- cum_to_daily(confirmed) 
daily_confirmed <- daily %>%
  gather("date","cases",colnames(confirmed[str_detect(colnames(confirmed),"/")])) %>%
  rename(state = Province_State) %>%
  mutate(date=as.Date(date,"%m/%d/%y"))

five_states <- daily_confirmed %>%
  filter(state %in% c("Ohio","Alabama","Alaska","Delaware","Michigan")) %>%
  mutate(month = month(date,label=T, abbr=T)) %>%
  group_by(month,state) %>%
  summarise(total_cases=sum(cases))

# Creating a fluctuation plot
fluctuation_plot <- ggplot() +
  geom_point(aes(x=month, y=state, size=total_cases), color="deepskyblue",
             shape=15, data=five_states) + 
  scale_size_continuous(range=c(1,10),labels=scales::comma) +
  labs(x="Month", y= "State",size="Total Cases",
       title = "Distribution of confirmed cases in 5 states",
       subtitle = "(Ohio,Alabama,Alaska,Delaware,Michigan)",
       caption = "Data Source: https://github.com/CSSEGISandData/COVID-19") +
  theme_bw() +
  theme(legend.position="bottom",plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) 
ggplotly(fluctuation_plot) %>%
  config(displayModeBar = F) #Removing plotly tool bar

#====================================================================================================#
#---------------------------------SAVING PROCESSED DATA IN RData Format------------------------------#
#====================================================================================================#

save(covid_summary,file="covid_summary.RData")
save(recovered_us_tall,file="recovered_us_tall.RData")
save(daily_confirmed,file="daily_confirmed.RData")
#----------------------------------------------------------------------------------------------#
