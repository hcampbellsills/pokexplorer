library(dplyr)
library(plotly)
library(shiny)

#########################
# CONSTRUCTING THE DATA #
#########################

# load data
data <- tbl_df(read.csv("www/pokemon.csv"))

######
# UI #
######

choice_inputs <- data$Pokemon
names(choice_inputs) <- paste0(data$Nat, ". ", data$Pokemon)

shinyUI(fluidPage(

    # Application title
    titlePanel("Pokemon Stats Explorer"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width=3, 
            selectInput("select", h3("Select box"), 
                        choices = choice_inputs),
            imageOutput("distImage", height="100%", width=3),
            htmlOutput("distText"))
        ,

        # Show a plot of the generated distribution
        mainPanel(
            fluidRow(
                plotlyOutput("distPlot", height="500px", width="600px")),
            fluidRow(
                column(4, selectInput("dim1", h4("Select X axis"),
                        choices=paste0("PC", 1:6), selected="PC1")),
                column(4, selectInput("dim2", h4("Select Y axis"),
                        choices=paste0("PC", 1:6), selected="PC2")),
                column(4, selectInput("dim3", h4("Select Z axis"),
                        choices=paste0("PC", 1:6), selected="PC3")))
        )
    )
))
