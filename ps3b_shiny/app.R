library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(ggrepel)
library(fivethirtyeight)

###############
# import data #
###############
hate_crimes <- hate_crimes %>%
  mutate(hate_Nov2016_annualized = hate_crimes_per_100k_splc*36.5) %>%
  mutate(hate_relative_change = hate_Nov2016_annualized /
           avg_hatecrimes_per_100k_fbi)
#############################################################
# define choice values and labels for widgets (user inputs) #
#############################################################
# define vectors for choice values and labels 
# can then refer to them in server as well (not just in defining widgets)


# for TAB 1 (SCATTERPLOT) widgets: 
# for radio button in scatterplot tab
x_choice_values <- c("gini_index", "share_vote_trump", "share_non_white", 
                     "share_pop_metro", "median_house_inc", "share_unemp_seas")
x_choice_names <- c("Gini Index 2015", "Trump Support %", "Nonwhite %", 
                    "Metro  %", "Median Income", "Unemployment %")
names(x_choice_values) <- x_choice_names


y_choice_values <- c("avg_hatecrimes_per_100k_fbi", "hate_Nov2016_annualized")
y_choice_names <- c("Hate Crimes 2010-2015", "Hate Crimes Nov 9-18 2016 annualized")
names(y_choice_values) <- y_choice_names

state_choices <- unique(hate_crimes$state)


############
#    ui    #
############
ui <- navbarPage(
  
  title="Hate Crimes",
  
  tabPanel(
    title = "Scatterplot",
    
    sidebarLayout(
      
      sidebarPanel(
        radioButtons(inputId = "x_axis"
                     , label = "Choose x-axis:"
                     , choices = x_choice_values
                     , selected = "gini_index"),
        radioButtons(inputId = "y_axis"
                     , label = "Choose y-axis:"
                     , choices = y_choice_values
                     , selected = "avg_hatecrimes_per_100k_fbi"),
      ),
      mainPanel(
        plotOutput(outputId = "scatter")
      )
    )
  ),
  
  tabPanel(
    title = "Table",

    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "state_choice"
                       , label = "Choose one or more states:"
                       , choices = state_choices
                       , selected = ""
                       , multiple = TRUE)
      ),
      mainPanel(
        h3("See how hate crimes increased by state in Nov. 2016 
           relative to baseline"),
        DT::dataTableOutput(outputId = "table")
      )
    )
  )

)

############
# server   #
############
server <- function(input,output){
  
  # TAB 1: INTERACTIVE SCATTERPLOT
  output$scatter <- renderPlot({
    hate_crimes %>%
      filter(state != "District of Columbia") %>%
      ggplot(aes_string(x = input$x_axis, y = input$y_axis)) +
        geom_text(aes(label = state_abbrev)) +
        geom_smooth(se = FALSE, method = "lm") +
        labs(x = x_choice_names[x_choice_values == input$x_choice_values],
             y = y_choice_names[y_choice_values == input$y_choice_values],
             title = "Factors Correlating with Hate Crimes")
  })

  # TAB 2: TABLE
  data_for_table <- reactive({
    data <- filter(hate_crimes, state %in% input$state_choice) %>%
      select(state,avg_hatecrimes_per_100k_fbi, hate_Nov2016_annualized, 
             hate_relative_change)
  })

  output$table <- DT::renderDataTable({
    data_for_table()
  })

}

####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)

