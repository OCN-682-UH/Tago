# app.R 
# This is a Shiny web application. You can run the application by clicking
# This app is for the OCN682 course, mainly to accomplish the goal of taking any 
# dataset and turn it into an interactive app.
#
# Data: using the penguin dataset to view the size and shape of the penguins of
# different species. 
# Note: Setting up an easy route for me, so this can be my guideline for when I 
# make this app.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#Loading the libraries

library(shiny)
library(tidyverse)
library(readr)
library(here)
library(palmerpenguins)
library(DT) #interactive data table for shiny app 
library(bslib) # makes my app pretty and in theme 

#read the data 
penguins <- read_csv("penguins.csv")

#clean the data
peng <- penguins |>
  drop_na() |>
  mutate(across(c(species,island, sex), as.factor))

# Goal for this homework: 
#. 1. make this an interactive app
#. 2. plot a figure
#. 3. create another output: make an interactive datatable

# What does the App do:
#. 1. User chooses penguin variable and compares to other penguins in terms of size & shape
#.   - we will view this in the figure
#. 2. Summarizes dataset and dsiplay it (mean, location etc.)
#.   - we will view in the data table 
###############################################################################################

# set up data 
#penguin will have its own color 
spp_colors <- c(
  Adelie    = "#E76F51",   # coral
  Chinstrap = "#264653",   # deep teal 
  Gentoo    = "#2A9D8F"    # algae teal
  )

#sex will have its own color
sex_cols <- c(
  female = "#E9C46A",   # amber
  male   = "#5FA8D3"    # light blue
)

#island will have its own color
island_cols <- c(
  Biscoe    = "#F4A259",   # apricot
  Dream     = "#8D99AE",   # stone grey
  Torgersen = "#BC4749"    # brick red
)

# all colors in one vector, so any "color by" choice works
all_cols <- c(spp_colors, sex_cols, island_cols)

#numeric variables 
penguin_vari <- c(
  "Flipper length (mm)" = "flipper_length_mm",
  "Body mass (g)" = "body_mass_g",
  "Bill length (mm)" = "bill_length_mm",
  "Bill depth (mm)" = "bill_depth_mm"
)

#grouping the variables 
group_vari <- c("Species" = "species", "Island" = "island", "Sex" = "sex")

#shiny code part 1: user interface
ui <- page_sidebar(
    # Application title
    title = tags$span ("Penguin by Species and Sex",
                      tags$small ( " -Author: Fuamai A. Tago", 
                                   style = "font-size: 0.6em; opacity: 0.8;")
  ),
  
  #theme for this page 
    theme = bs_theme(
      version      = 5,
      bootswatch   = "morph",
      primary      = "#E76F51",   # coral, same as Adelie
      base_font    = font_google("Ubuntu"),
      heading_font = font_google("Ubuntu"),
      navbar_bg    = "#264653"    # deep teal, same as Chinstrap
    ) |>
    # darker fonts for the data table (headers, rows, caption, search, page info)
    bs_add_rules("
      .dataTables_wrapper, .dt-container,
      .dataTables_wrapper table.dataTable th, .dataTables_wrapper table.dataTable td,
      .dt-container table.dataTable th, .dt-container table.dataTable td,
      table.dataTable caption {
        color: #1D1D1D !important;
      }
      table.dataTable thead th { font-weight: 700; }
    "),

    #sidebar look
  sidebar = sidebar(
    selectInput("variables", "Measurements", penguin_vari, selected = "body_mass_g"),
    selectInput("xvar", "Group by", group_vari, selected = "species"),
    selectInput("fillvar", "Colour by", group_vari, selected = "sex"),
    checkboxGroupInput("species", "Species", levels(peng$species),
                       selected = levels(peng$species)),
    checkboxInput("points", "Show individual penguins", TRUE)
  ),
  
  navset_card_tab(
    full_screen = TRUE,
    nav_panel("Plot", plotOutput("plot", height = "500px")),
    nav_panel("Summary table", DTOutput("summary_table"))
  )
)

# shiny code part 2: server interface to create box and violin plots
server <- function(input, output, session) {
  
  # keeping only the species ticked in the sidebar
  dat <- reactive({
    req(input$species)
    subset(peng, species %in% input$species)
  })
  
  output$plot <- renderPlot({
    lab <- names(penguin_vari)[penguin_vari == input$variables]
    x_lab    <- names(group_vari)[group_vari == input$xvar]
    fill_lab <- names(group_vari)[group_vari == input$fillvar]
    dodge    <- position_dodge(0.8)
    
  #base plot: violin and boxplot
    penguin_plot <- ggplot(dat(), aes(.data[[input$xvar]], .data[[input$variables]],
                           fill = .data[[input$fillvar]])) +
      geom_violin(alpha = 0.4, position = dodge, color = NA, trim = FALSE) +
      geom_boxplot(width = 0.2, position = dodge, outlier.shape = NA, alpha = 0.9)
    
  #add jitter points only when box is chosen
    if (input$points) {
      penguin_plot <- penguin_plot + geom_point(
        position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
        alpha = 0.5, size = 1.5
      )
    }
    
  #colors, labels, and themes added in 
    penguin_plot +
      scale_fill_manual(values = all_cols) +
      labs(title = paste(lab, "by", x_lab),
           x = NULL, y = lab, fill = fill_lab) +
      theme_minimal(base_size = 15) +
      theme(legend.position = "top")
  })
  
  #data summary table 
  output$summary_table <- renderDT({
    lab <- names(penguin_vari)[penguin_vari == input$variables]
  #summary 
    summary_df <- dat() |>
      group_by(across(all_of(unique(c(input$xvar, input$fillvar))))) |>
      summarise(
        n    = n(),
        mean = round(mean(.data[[input$variables]]), 1),
        sd   = round(sd(.data[[input$variables]]), 1),
        min  = min(.data[[input$variables]]),
        max  = max(.data[[input$variables]]),
        .groups = "drop"
      )
    #data table
    datatable(summary_df,
              caption  = paste("Summary of", lab),
              rownames = FALSE,
              filter   = "top",
              options  = list(pageLength = 10))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Honestly, I didnt expect this app to turn this way. I am happy with how it looks
# 