title: "NISS Data Competition"
author: 'Team: Eduardo Salvador'
output:
  flexdashboard::flex_dashboard:
  orientation: rows
source_code: embed
theme: united
runtime: shiny
resource_files:
  - tabn502.20.xls
---
  
  ```{r setup, include=FALSE}
library(bslib)
library(flexdashboard)
library(plotly)
library(ggplot2)
library(plyr)
library(readxl)
library(dplyr)
library(tidyverse)
library(GGally)
library(rjson)
library(shiny)
library(choroplethr)
library(shinyjs)

# setwd("C:/Users/Jim Byrne/Documents/NISS/")
# setwd("~/Desktop/Data Competition (NISS) - SP22/")

state_and_yr_reading_score <- 
  data.frame(read.csv("tabn221.60.csv",fileEncoding="latin1"))
state <- data.frame(read.csv("tabn104.85.csv", fileEncoding="latin1"))

state <- merge(state, state_and_yr_reading_score, by = "State")

state$HS_Black <- as.numeric(state$HS_Black)
state$HS_Asian <- as.numeric(state$HS_Asian)
state$Grad_Black <- as.numeric(state$Grad_Black)
state$Grad_Asian <- as.numeric(state$Grad_Asian)
state$X1998 <- as.numeric(state$X1998)
state$X2002 <- as.numeric(state$X2002)
state$X2003 <- as.numeric(state$X2003)
state$X2005 <- as.numeric(state$X2005)
state$X2007 <- as.numeric(state$X2007)
state$X2009 <- as.numeric(state$X2009)
state$X2011 <- as.numeric(state$X2011)
state$X2013 <- as.numeric(state$X2013)
state$X2015 <- as.numeric(state$X2015)
state$X2017 <- as.numeric(state$X2017)
state$X2019 <- as.numeric(state$X2019)
state <- state %>% 
  mutate(meanscore = rowMeans(state[c("X1998", "X2002", "X2003", "X2005", "X2007", "X2009", "X2011", "X2013", "X2015", "X2017", "X2019")], na.rm = T))
```












Graphic 1
=====================================
  Sidebar {.sidebar data-width=400}
-----------------------------------------------------------------------
  
  Since the first civil rights movements, a question at the forefront of educational policy making has been “how can we make things equal?” Since then, a plethora of possible solutions have been theorized, and many have been put into action through policy. American government has dedicated considerable time, effort, and money into educational equity for disadvantaged groups along lines of sex and race. Decades later, we ask a different but equally important question through our graphic- “have we achieved equality?”

### Inputs

```{r inputs}
radioButtons("shown","Variable being plotted", c("NAEP Reading Scores",
                                                 "Percent with High School Completion",
                                                 "Percent with Bachelors"), selected = "NAEP Reading Scores")
selectInput("second","State in Secondary plot", c("Alabama",
                                                  "Alaska",
                                                  "Arizona",
                                                  "Arkansas",
                                                  "California",
                                                  "Colorado",
                                                  "Connecticut",   
                                                  "Delaware",
                                                  "Florida",
                                                  "Georgia",
                                                  "Hawaii",
                                                  "Idaho",
                                                  "Illinois",
                                                  "Indiana",
                                                  "Iowa",
                                                  "Kansas",
                                                  "Kentucky",
                                                  "Louisiana",
                                                  "Maine",
                                                  "Maryland",
                                                  "Massachusetts",
                                                  "Michigan",
                                                  "Minnesota",
                                                  "Mississippi",
                                                  "Missouri",
                                                  "Montana",
                                                  "Nebraska",
                                                  "Nevada",        
                                                  "New Hampshire",
                                                  "New Jersey",
                                                  "New Mexico",
                                                  "New York",
                                                  "North Carolina",
                                                  "North Dakota",
                                                  "Ohio",
                                                  "Oklahoma",
                                                  "Oregon",
                                                  "Pennsylvania",
                                                  "Rhode Island",
                                                  "South Carolina",
                                                  "South Dakota",
                                                  "Tennessee",
                                                  "Texas",
                                                  "Utah",
                                                  "Vermont",
                                                  "Virginia",
                                                  "Washington",
                                                  "West Virginia",
                                                  "Wisconsin",     
                                                  "Wyoming"))
par(mar=c(1,1,1,1))
conditionalPanel(
  condition = "input.shown != 'NAEP Reading Scores'",
  selectInput("raceplotted", "Race Plotted", c("All Races", "White", "Black", "Hispanic", "Asian", "Multi-racial"), selected = "All Races")
)
conditionalPanel(
  condition = "input.shown == 'NAEP Reading Scores'",
  selectInput("Kidsyr", "Year Plotted", c("1998", "2002", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"), selected = "2019") #, width = 0.1)
)
```

Our first graphic asks if efforts working towards racial equality in education are working. You begin by looking at NAEP Reading scores (an annual test taken by 8th-grade public school students) from 2019 plotted on a US map with a secondary plot showing how the scores from Alabama trend over time. You can explore the map by hovering over states which will reveal a table of the scores in that state over time as well as the standard error. You can also change which year is plotted on the map and the state plotted in the secondary plot. You can then use the radio buttons to look at high school and bachelors degree completion plotted across the country in a similar way broken down by race.

Click the "Graphic 2" tab at the top of the screen when you are ready to continue.


Row {data-height=500}
-----------------------------------------------------------------------

### Education Levels Across the United States by Year and Race

```{r primary}
par(mar=c(1,1,1,1))
output$PlotOneHS <- renderPlotly({
    
    data <- switch(input$raceplotted,
                   "All Races" = state$HS_Total,
                   "White" = state$HS_White,
                   "Black" = state$HS_Black,
                   "Hispanic" = state$HS_Hispanic,
                   "Asian" = state$HS_Asian,
                   "Multi-racial" = state$HS_Multiple)
    color <- switch(input$shown,
                    "Percent with High School Completion" = 'Blues',
                    "Percent with Bachelors" = 'Purples',
                    "NAEP Reading Scores" = 'Greens')
    hover <- switch(input$shown,
                    "Percent with High School Completion" = with(state, paste(State, '',
                                                                              "Percent with High School Completion:", '',
                                                                              "Total:     ", HS_Total, "(", HS_Total_Error, ")", '',
                                                                              '    ', "White:         ", HS_White, "(",  HS_White_Error, ")", '',
                                                                              '    ', "Black:         ", HS_Black, "(",  HS_Black_Error, ")", '',
                                                                              '    ', "Hispanic:    ", HS_Hispanic, "(",  HS_Hispanic_Error, ")", '',
                                                                              '    ', "Asian:         ", HS_Asian, "(",  HS_Asian_Error, ")", '',
                                                                              '    ', "Multi-racial: ", HS_Multiple, "(",  HS_Multiple_Error, ")", '')),
                    "Percent with Bachelors" = with(state, paste(State, '',
                                                                 "Percent with Bachelor's:", '',
                                                                 "Total:     ", Grad_Total, "(",  Grad_Total_Error, ")",  '',
                                                                 '    ', "White:         ", Grad_White, "(",  Grad_White_Error, ")", '',
                                                                 '    ', "Black:         ", Grad_Black, "(",  Grad_Black_Error, ")", '',
                                                                 '    ', "Hispanic:   ", Grad_Hispanic, "(",  Grad_Hispanic_Error, ")", '',
                                                                 '    ', "Asian:         ", Grad_Asian, "(",  Grad_Asian_Error, ")", '',
                                                                 '    ', "Multi-racial: ", Grad_Multiple, "(",  Grad_Multiple_Error, ")", '')),
                    "NAEP Reading Scores" = with(state, paste(State, '',
                                                              "NAEP Reading Scores:", ''
                    )))
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white'))
    
    p <- plot_geo(state, locationmode = 'USA-states')
    p <- p %>% add_trace(
      z = ~data, text = ~hover, locations = ~code,
      color = ~data, colors = color)
    
    p <- p %>% colorbar(title = "Percent with HS Completion")
    p <- p %>% layout(
      title = 'High School Completion Rate by Race',
      geo = g)
    p
  })

par(mar=c(1,1,1,1))
output$PlotOneGrad <- renderPlotly({
    
    data <- switch(input$raceplotted,
                   "All Races" = state$HS_Total,
                   "White" = state$HS_White,
                   "Black" = state$HS_Black,
                   "Hispanic" = state$HS_Hispanic,
                   "Asian" = state$HS_Asian,
                   "Multi-racial" = state$HS_Multiple)
    color <- switch(input$shown,
                    "Percent with High School Completion" = 'Blues',
                    "Percent with Bachelors" = 'Purples',
                    "NAEP Reading Scores" = 'Greens')
    hover <- switch(input$shown,
                    "Percent with High School Completion" = with(state, paste(State, '',
                                                                              "Percent with High School Completion:", '',
                                                                              "Total:     ", HS_Total, "(", HS_Total_Error, ")", '',
                                                                              '    ', "White:         ", HS_White, "(",  HS_White_Error, ")", '',
                                                                              '    ', "Black:         ", HS_Black, "(",  HS_Black_Error, ")", '',
                                                                              '    ', "Hispanic:    ", HS_Hispanic, "(",  HS_Hispanic_Error, ")", '',
                                                                              '    ', "Asian:         ", HS_Asian, "(",  HS_Asian_Error, ")", '',
                                                                              '    ', "Multi-racial: ", HS_Multiple, "(",  HS_Multiple_Error, ")", '')),
                    "Percent with Bachelors" = with(state, paste(State, '',
                                                                 "Percent with Bachelor's:", '',
                                                                 "Total:     ", Grad_Total, "(",  Grad_Total_Error, ")",  '',
                                                                 '    ', "White:         ", Grad_White, "(",  Grad_White_Error, ")", '',
                                                                 '    ', "Black:         ", Grad_Black, "(",  Grad_Black_Error, ")", '',
                                                                 '    ', "Hispanic:   ", Grad_Hispanic, "(",  Grad_Hispanic_Error, ")", '',
                                                                 '    ', "Asian:         ", Grad_Asian, "(",  Grad_Asian_Error, ")", '',
                                                                 '    ', "Multi-racial: ", Grad_Multiple, "(",  Grad_Multiple_Error, ")", '')),
                    "NAEP Reading Scores" = with(state, paste(State, '',
                                                              "NAEP Reading Scores:", ''
                    )))
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white'))
    
    p <- plot_geo(state, locationmode = 'USA-states')
    p <- p %>% add_trace(
      z = ~data, text = ~hover, locations = ~code,
      color = ~data, colors = color)
    
    p <- p %>% colorbar(title = "Percent with Bachelor's")
    p <- p %>% layout(
      title = "Bachelor's Completion Rate by Race",
      geo = g)
    p
  })

par(mar=c(1,1,1,1))
output$PlotOneKids <- renderPlotly({
    
    data <- switch(input$Kidsyr,
                   "1998" = state$X1998,
                   "2002" = state$X2002,
                   "2003" = state$X2003,
                   "2005" = state$X2005,
                   "2007" = state$X2007,
                   "2009" = state$X2009,
                   "2011" = state$X2011,
                   "2013" = state$X2013,
                   "2015" = state$X2015,
                   "2017" = state$X2017,
                   "2019" = state$X2019)
    color <- switch(input$shown,
                    "Percent with High School Completion" = 'Blues',
                    "Percent with Bachelors" = 'Purples',
                    "NAEP Reading Scores" = 'Greens')
    hover <- switch(input$shown,
                    "Percent with High School Completion" = with(state, paste(State, '',
                                                                              "Percent with High School Completion:", '',
                                                                              "Total:     ", HS_Total, "(", HS_Total_Error, ")", '',
                                                                              '    ', "White:         ", HS_White, "(",  HS_White_Error, ")", '',
                                                                              '    ', "Black:         ", HS_Black, "(",  HS_Black_Error, ")", '',
                                                                              '    ', "Hispanic:    ", HS_Hispanic, "(",  HS_Hispanic_Error, ")", '',
                                                                              '    ', "Asian:         ", HS_Asian, "(",  HS_Asian_Error, ")", '',
                                                                              '    ', "Multi-racial: ", HS_Multiple, "(",  HS_Multiple_Error, ")", '')),
                    "Percent with Bachelors" = with(state, paste(State, '',
                                                                 "Percent with Bachelor's:", '',
                                                                 "Total:     ", Grad_Total, "(",  Grad_Total_Error, ")",  '',
                                                                 '    ', "White:         ", Grad_White, "(",  Grad_White_Error, ")", '',
                                                                 '    ', "Black:         ", Grad_Black, "(",  Grad_Black_Error, ")", '',
                                                                 '    ', "Hispanic:   ", Grad_Hispanic, "(",  Grad_Hispanic_Error, ")", '',
                                                                 '    ', "Asian:         ", Grad_Asian, "(",  Grad_Asian_Error, ")", '',
                                                                 '    ', "Multi-racial: ", Grad_Multiple, "(",  Grad_Multiple_Error, ")", '')),
                    "NAEP Reading Scores" = with(state, paste(State, '',
                                                              "NAEP Reading Scores:", '',
                                                              "1998: ", X1998, "(", X1998_Error, ")", '',
                                                              "2002: ", X2002, "(", X2002_Error, ")", '',
                                                              "2003: ", X2003, "(", X2003_Error, ")", '',
                                                              "2005: ", X2005, "(", X2005_Error, ")", '',
                                                              "2007: ", X2007, "(", X2007_Error, ")", '',
                                                              "2009: ", X2009, "(", X2009_Error, ")", '',
                                                              "2011: ", X2011, "(", X2011_Error, ")", '',
                                                              "2013: ", X2013, "(", X2013_Error, ")", '',
                                                              "2015: ", X2015, "(", X2015_Error, ")", '',
                                                              "2017: ", X2017, "(", X2017_Error, ")", '',
                                                              "2019: ", X2019, "(", X2019_Error, ")", '')))
    g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = TRUE,
      lakecolor = toRGB('white'))
    
    p <- plot_geo(state, locationmode = 'USA-states')
    p <- p %>% add_trace(
      z = ~data, text = ~hover, locations = ~code,
      color = ~data, colors = color)
    
    p <- p %>% colorbar(title = "NAEP Reading Score")
    p <- p %>% layout(
      title = "NAEP Reading Score by Year",
      geo = g)
    p
  })

par(mar=c(1,1,1,1))
conditionalPanel(
      condition = "input.shown == 'Percent with High School Completion'",
      plotlyOutput('PlotOneHS')
    )
conditionalPanel(
      condition = "input.shown == 'Percent with Bachelors'",
      plotlyOutput('PlotOneGrad')
    )
conditionalPanel(
      condition = "input.shown == 'NAEP Reading Scores'",
      plotlyOutput('PlotOneKids')
    )

```

Row
-------------------------------------

##### Selected State and Education by Race or Year
```{r secondary}

output$HS = renderPlot({
    state %>%
      subset(State == input$second) %>%
      select(HS_Total, HS_White, HS_Black, HS_Hispanic, HS_Asian, HS_Multiple) %>%
      as.matrix() %>%
      barplot(col = "blue4", xlab = "Race", ylab = "Percent with High School Completion", names.arg = c("Tot", "W", "B", "H", "A", "Mult"))
  })


output$Grad = renderPlot({
    state %>%
      subset(State == input$second) %>%
      select(Grad_Total, Grad_White, Grad_Black, Grad_Hispanic, Grad_Asian, Grad_Multiple) %>%
      as.matrix() %>%
      barplot(col = "darkorchid4", xlab = "Race", ylab = "Percent with a Bachelor's Degree", names.arg = c("Tot", "W", "B", "H", "A", "Mult"))
  })


output$NAEP = renderPlot({
    state %>%
      subset(State == input$second) %>%
      select(X1998, X2002, X2003, X2005, X2007, X2009, X2011, X2013, X2015, X2017, X2019) %>%
      as.data.frame() %>%
      t() %>%
      as.data.frame() %>%
      mutate(yrs = c("1998", "2002", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")) %>%
      setNames(c("Score", "Year")) %>%
      ggplot(aes(Year, Score, group = 1)) + geom_line(size = 2, col = "forestgreen") + theme_bw() + labs(y = "Average NAEP Reading Score")
  })

conditionalPanel(
      condition = "input.shown == 'Percent with High School Completion'",
      plotOutput('HS')
    )


conditionalPanel(
      condition = "input.shown == 'Percent with Bachelors'",
      plotOutput('Grad')
    )


conditionalPanel(
      condition = "input.shown == 'NAEP Reading Scores'",
      plotOutput('NAEP')
    )
  
```






Graphic 2
=====================================


Row
-------------------------------------

### Introduction

Our second graphic asks the same question, but this time in regard to sex. Differences are even more apparent this time, visualized through median average income in four levels of education by sex.

The top plot shows median annual earnings by education level and gender. Hover over the lines to more easily see what they convey. The hover shows the year, the median income at that level, the group name, and the standard error of that point.

The bottom plot shows a linear regression model of total median annual earnings for men and women over time. Hover over each data point to show further details about them. The hover shows the year, total median annual earnings for that gender at that time, the gender shown, and the standard error of that point.


### Interactive Time Series Plot To Show Median Annual Earnings Across Years Based on Gender and Education Level Completion {data-width=1000}
```{r}
# setwd("~/Desktop/Data Competition (NISS) - SP22/")
df2 <- read_xls("tabn502.20.xls", col_names = T)
maleDF <- df2[10:30,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
femaleDF <- df2[33:53,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
colnames(maleDF) <- c("Years","Total", "Total Error", "<9th Grade", "<9th Grade Error", "Some HS, DNC", "Some HS, DNC Error", "HS", "HS Error", "Some College, No Deg", "Some College, No Deg Error", "Associate", "Associate Error", "Total: College", "Total: College Error", "Bachelor", "Bachelor Error", "Master", "Master Error", "Professional", "Professional Error", "Doctorate", "Doctorate Error")
# maleDF <- maleDF %>% remove_rownames %>% column_to_rownames(var="Years")

colnames(femaleDF) <- c("Years","Total", "Total Error", "<9th Grade", "<9th Grade Error", "Some HS, DNC", "Some HS, DNC Error", "HS", "HS Error", "Some College, No Deg", "Some College, No Deg Error", "Associate", "Associate Error", "Total: College", "Total: College Error", "Bachelor", "Bachelor Error", "Master", "Master Error", "Professional", "Professional Error", "Doctorate", "Doctorate Error")
# femaleDF <- femaleDF %>% remove_rownames %>% column_to_rownames(var="Years")


maleDF$Year <- as.numeric(maleDF$Years)
maleDF$`Total` <- as.numeric(maleDF$`Total`)
maleDF$`Total Error` <- as.numeric(maleDF$`Total Error`)
maleDF$`<9th Grade` <- as.numeric(maleDF$`<9th Grade`)
maleDF$`<9th Grade Error` <- as.numeric(maleDF$`<9th Grade Error`)
maleDF$`Some HS, DNC` <- as.numeric(maleDF$`Some HS, DNC`)
maleDF$`Some HS, DNC Error` <- as.numeric(maleDF$`Some HS, DNC Error`)
maleDF$HS <- as.numeric(maleDF$HS)
maleDF$`HS Error` <- as.numeric(maleDF$`HS Error`)
maleDF$`Some College, No Deg` <- as.numeric(maleDF$`Some College, No Deg`)
maleDF$`Some College, No Deg Error`<- as.numeric(maleDF$`Some College, No Deg Error`)
maleDF$Associate <- as.numeric(maleDF$Associate)
maleDF$`Associate Error` <- as.numeric(maleDF$`Associate Error`)
maleDF$`Total: College` <- as.numeric(maleDF$`Total: College`)
maleDF$`Total: College Error` <- as.numeric(maleDF$`Total: College Error`)
maleDF$Bachelor <- as.numeric(maleDF$Bachelor)
maleDF$`Bachelor Error` <- as.numeric(maleDF$`Bachelor Error`)
maleDF$Master <- as.numeric(maleDF$Master)
maleDF$`Master Error` <- as.numeric(maleDF$`Master Error`)
maleDF$Professional <- as.numeric(maleDF$Professional)
maleDF$`Professional Error` <- as.numeric(maleDF$`Professional Error`)
maleDF$Doctorate <- as.numeric(maleDF$Doctorate)
maleDF$`Doctorate Error`<- as.numeric(maleDF$`Doctorate Error`)
maleDF$`<9th Grade` <- as.numeric(maleDF$`<9th Grade`)
maleDF$`<9th Grade Error` <- as.numeric(maleDF$`<9th Grade Error`)

femaleDF$Year <- as.numeric(femaleDF$Years)
femaleDF$`Total` <- as.numeric(femaleDF$`Total`)
femaleDF$`Total Error` <- as.numeric(femaleDF$`Total Error`)
femaleDF$`<9th Grade` <- as.numeric(femaleDF$`<9th Grade`)
femaleDF$`<9th Grade Error` <- as.numeric(femaleDF$`<9th Grade Error`)
femaleDF$`Some HS, DNC` <- as.numeric(femaleDF$`Some HS, DNC`)
femaleDF$`Some HS, DNC Error` <- as.numeric(femaleDF$`Some HS, DNC Error`)
femaleDF$HS <- as.numeric(femaleDF$HS)
femaleDF$`HS Error` <- as.numeric(femaleDF$`HS Error`)
femaleDF$`Some College, No Deg` <- as.numeric(femaleDF$`Some College, No Deg`)
femaleDF$`Some College, No Deg Error` <- as.numeric(femaleDF$`Some College, No Deg Error`)
femaleDF$Associate <- as.numeric(femaleDF$Associate)
femaleDF$`Associate Error` <- as.numeric(femaleDF$`Associate Error`)
femaleDF$`Total: College` <- as.numeric(femaleDF$`Total: College`)
femaleDF$`Total: College Error` <- as.numeric(femaleDF$`Total: College Error`)
femaleDF$Bachelor <- as.numeric(femaleDF$Bachelor)
femaleDF$`Bachelor Error` <- as.numeric(femaleDF$`Bachelor Error`)
femaleDF$Master <- as.numeric(femaleDF$Master)
femaleDF$`Master Error` <- as.numeric(femaleDF$`Master Error`)
femaleDF$Professional <- as.numeric(femaleDF$Professional)
femaleDF$`Professional Error` <- as.numeric(femaleDF$`Professional Error`)
femaleDF$Doctorate <- as.numeric(femaleDF$Doctorate)
femaleDF$`Doctorate Error` <- as.numeric(femaleDF$`Doctorate Error`)
femaleDF$`<9th Grade` <- as.numeric(femaleDF$`<9th Grade`)
femaleDF$`<9th Grade Error` <- as.numeric(femaleDF$`<9th Grade Error`)


myColors <- c("Male High School Completion" = "#0DD6FB", "Male Bachelors Completion" = "#009CE6", 
              "Male Masters Completion" = "#0083FA", "Male Doctorate Completion" = "#0B57E3",
              "Female High School Completion" = "#FF00B8", "Female Bachelors Completion" = "#FA00E9", 
              "Female Masters Completion" = "#DE0BE3", "Female Doctorate Completion" = "#E30BBD")
# library(plyr)
# round_any(maleDF$`Total: Elementary`, 1000)

q <- ggplot() + geom_line(maleDF, mapping = aes(Year, `HS`, color = "Male High School Completion", text = paste0("Error: ", `HS Error`), group = 1), size = 1.25) +
  geom_line(maleDF, mapping = aes(Year, `Bachelor`, color = "Male Bachelors Completion", text = paste0("Error: ", `Bachelor Error`), group = 1), size = 1.25) + 
  geom_line(maleDF, mapping = aes(Year, `Master`, color = "Male Masters Completion", text = paste0("Error: ", `Master Error`), group = 1), size = 1.25) + 
  geom_line(maleDF, mapping = aes(Year, `Doctorate`, color = "Male Doctorate Completion", text = paste0("Error: ", `Doctorate Error`), group = 1), size = 1.25) + 
  geom_line(femaleDF, mapping = aes(Year, `HS`, color = "Female High School Completion", text = paste0("Error: ", `HS Error`), group = 1), size = 1.25) + 
  geom_line(femaleDF, mapping = aes(Year, `Bachelor`, color = "Female Bachelors Completion", text = paste0("Error: ", `Bachelor Error`), group = 1), size = 1.25) + 
  geom_line(femaleDF, mapping = aes(Year, `Master`, color = "Female Masters Completion", text = paste0("Error: ", `Master Error`), group = 1), size = 1.25) + 
  geom_line(femaleDF, mapping = aes(Year, `Doctorate`, color = "Female Doctorate Completion", text = paste0("Error: ", `Doctorate Error`), group = 1), size = 1.25) + 
  labs(y = "Median Annual Earnings", color = "Group") + 
  ggtitle("Mean Annual Earnings Per Year By School Bracket & Gender") + 
  scale_color_manual(values = myColors) +#+ scale_y_continuous(limits = c(24000, 87000))
  theme_bw()

ggplotly(q)
```


Row {data-height=350}
-------------------------------------

### Interactive Regression Plot Showing Doctorate Degree Median Annual Earnings for Each Gender {data-width=800}
```{r}
combGen <- rbind(maleDF, femaleDF)
combGen <- combGen %>% mutate(Gender = c(rep("Male", 21), rep("Female", 21)))

p <- ggplot(combGen, aes(x=Year, y=Total, color=Gender)) + 
  geom_point(aes(text = paste0("Error: ", `Total Error`)), shape=3) + 
  geom_smooth(method=lm) +
  xlab("Year") + ylab("Total Ann. Earnings") + 
  theme_bw()+
  scale_color_manual(values = c("Female" = "#ff8af5",
                                "Male" ="#21aaff"))
ggplotly(p)
```


### Discussion
The top plot clearly shows inequality as men with only master's degrees earn more than women with doctorate's. Similarly men with only bachelor's degrees earn more than women with master's degrees.

Decades of inequality may appear to be converging at certain levels of education in the top plot, but the bottom plot plots a nearly parallel line of male income above female with the difference in earnings between men and women consistent at about 10,000 dollars.
