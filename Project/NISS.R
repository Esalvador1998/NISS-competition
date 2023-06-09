library(plotly)
library(rjson)
library(shiny)


setwd("C:/Users/Eduardo/Documents/NISS/")


state_and_yr_reading_score <- data.frame(read.csv("tabn221.60.csv"))
state <- data.frame(read.csv("tabn104.85.csv"))

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


state$hs_hover <- with(state, paste(State, '<br>',
                                   "Percent with High School Completion:", '<br>',
                                   "Total:     ", HS_Total, HS_Total_Error, '<br>',
                                   "Hispanic:", HS_Hispanic, HS_Hispanic_Error,'<br>',
                                   "White:    ", HS_White, HS_White_Error,'<br>',
                                   "Asian:    ", HS_Asian, HS_Asian_Error,'<br>',
                                   "Black:     ", HS_Black, HS_Black_Error,'<br>',
                                   "Multi-racial: ", HS_Multiple, HS_Multiple_Error,'<br>'))
state$bach_hover <- with(state, paste(State, '<br>',
                                    "Percent with Bachelor's:", '<br>',
                                    "Total:     ", Grad_Total, Grad_Total_Error, '<br>',
                                    "Hispanic:", Grad_Hispanic, Grad_Hispanic_Error,'<br>',
                                    "White:    ", Grad_White, Grad_White_Error,'<br>',
                                    "Asian:    ", Grad_Asian, Grad_Asian_Error,'<br>',
                                    "Black:     ", Grad_Black, Grad_Black_Error,'<br>',
                                    "Multi-racial: ", Grad_Multiple, Grad_Multiple_Error,'<br>'))
state$kids_hover <- with(state, paste(State, '<br>',
                                      "NAEP Reading Scores:", '<br>',
                                      "1998: ", X1998, '(', X1998_Error, ')', '<br>',
                                      "2002: ", X2002, '(', X2002_Error,')', '<br>',
                                      "2003: ", X2003, '(', X2003_Error,')', '<br>',
                                      "2005: ", X2005, '(', X2005_Error,')', '<br>',
                                      "2007: ", X2007, '(', X2007_Error,')', '<br>',
                                      "2009: ", X2009, '(', X2009_Error,')', '<br>',
                                      "2011: ", X2011, '(', X2011_Error,')', '<br>',
                                      "2013: ", X2013, '(', X2013_Error,')', '<br>',
                                      "2015: ", X2015, '(', X2015_Error,')', '<br>',
                                      "2017: ", X2017, '(', X2017_Error,')', '<br>',
                                      "2019: ", X2019, '(', X2019_Error,')', '<br>'))



dacolor <- c("state$HS_Total", "state$Grad_Total", "state$meanscore")
uhactualcolor <- c('Blues', 'Purples', 'Greens')
colortitle <- c("Percent with High School Completion:")
hovers <- c("state$hs_hover", "state$bach_hover", "state$kids_hover")
# 
# ui <- fluidPage(
#   titlePanel("Education Across the US"),
# 
#   sidebarPanel(
# 
#     selectInput("shown","Variable being plotted", c("Percent with High School Completion", 
#                                                     "Percent with Bachelor's",
#                                                     "NAEP Reading Scores")),
# 
#   ),
# 
#   mainPanel(
# 
#     plotOutput('Plot')
# 
#   )
# )
# 
# server <- function(input, output)
# {
# 
#   output$Plot <- renderPlot({
#     
#     data <- switch(input$shown,
#                    "Percent with High School Completion" = state$HS_Total,
#                    "Percent with Bachelor's" = state$Grad_Total,
#                    "NAEP Reading Scores" = state$meanscore)
#     
#     color <- switch(input$shown,
#                    "Percent with High School Completion" = 'Blues',
#                    "Percent with Bachelor's" = 'Purples',
#                    "NAEP Reading Scores" = 'Greens')
#     
#     hover <- switch(input$shown,
#                     "Percent with High School Completion" = with(state, paste(State, '<br>',
#                                                                               "Percent with High School Completion:", '<br>',
#                                                                               "Total:     ", HS_Total, HS_Total_Error, '<br>',
#                                                                               "Hispanic:", HS_Hispanic, HS_Hispanic_Error,'<br>',
#                                                                               "White:    ", HS_White, HS_White_Error,'<br>',
#                                                                               "Asian:    ", HS_Asian, HS_Asian_Error,'<br>',
#                                                                               "Black:     ", HS_Black, HS_Black_Error,'<br>',
#                                                                               "Multi-racial: ", HS_Multiple, HS_Multiple_Error,'<br>')),
#                     "Percent with Bachelor's" = with(state, paste(State, '<br>',
#                                                                   "Percent with Bachelor's:", '<br>',
#                                                                   "Total:     ", Grad_Total, Grad_Total_Error, '<br>',
#                                                                   "Hispanic:", Grad_Hispanic, Grad_Hispanic_Error,'<br>',
#                                                                   "White:    ", Grad_White, Grad_White_Error,'<br>',
#                                                                   "Asian:    ", Grad_Asian, Grad_Asian_Error,'<br>',
#                                                                   "Black:     ", Grad_Black, Grad_Black_Error,'<br>',
#                                                                   "Multi-racial: ", Grad_Multiple, Grad_Multiple_Error,'<br>')),
#                     "NAEP Reading Scores" = with(state, paste(State, '<br>',
#                                                               "NAEP Reading Scores:", '<br>',
#                                                               "1998: ", X1998, '(', X1998_Error, ')', '<br>',
#                                                               "2002: ", X2002, '(', X2002_Error,')', '<br>',
#                                                               "2003: ", X2003, '(', X2003_Error,')', '<br>',
#                                                               "2005: ", X2005, '(', X2005_Error,')', '<br>',
#                                                               "2007: ", X2007, '(', X2007_Error,')', '<br>',
#                                                               "2009: ", X2009, '(', X2009_Error,')', '<br>',
#                                                               "2011: ", X2011, '(', X2011_Error,')', '<br>',
#                                                               "2013: ", X2013, '(', X2013_Error,')', '<br>',
#                                                               "2015: ", X2015, '(', X2015_Error,')', '<br>',
#                                                               "2017: ", X2017, '(', X2017_Error,')', '<br>',
#                                                               "2019: ", X2019, '(', X2019_Error,')', '<br>')))
# 
# 
#     g <- list(
#       scope = 'usa',
#       projection = list(type = 'albers usa'),
#       showlakes = TRUE,
#       lakecolor = toRGB('white'))
#     
#     fig <- plot_geo(state, locationmode = 'USA-states')
#     fig <- fig %>% add_trace(
#       z = ~data, text = ~hover, locations = ~code,
#       color = ~data, colors = color)
#     
#     fig <- fig %>% colorbar(title = "Percent with High School Completion")
#     fig <- fig %>% layout(
#       title = 'High School Completion rate by Race',
#       geo = g)
# 
#     plot_ly
#     
#     
#     
#   })
# 
# }

#shinyApp(ui, server)





g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)


fig1 <- plot_geo(state, locationmode = 'USA-states')
fig1 <- fig1 %>% add_trace(
  z = ~HS_Total, text = ~hs_hover, locations = ~code,
  color = ~HS_Total, colors = 'Blues'
)
fig1 <- fig1 %>% colorbar(title = "Percent with High School Completion")
fig1 <- fig1 %>% layout(
  title = 'High School Completion rate by Race',
  geo = g
)



fig2 <- plot_geo(state, locationmode = 'USA-states')
fig2 <- fig2 %>% add_trace(
  z = ~Grad_Total, text = ~bach_hover, locations = ~code,
  color = ~Grad_Total, colors = 'Purples'
)
fig2 <- fig2 %>% colorbar(title = "Percent with a Bachelor's")
fig2 <- fig2 %>% layout(
  title = 'Percent of Population with a Bachelor\'s by Race',
  geo = g
)


fig3 <- plot_geo(state, locationmode = 'USA-states')
fig3 <- fig3 %>% add_trace(
  z = ~meanscore, text = ~kids_hover, locations = ~code,
  color = ~meanscore, colors = 'Greens'
)
fig3 <- fig3 %>% colorbar(title = "NAEP Reading Scores")
fig3 <- fig3 %>% layout(
  title = 'NAEP Reading Scores by Year',
  geo = g
)

fig1
fig2
fig3


