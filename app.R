#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# 1. LOAD PACKAGES
# ------------------------------------------------------------------------------
# install.packages("pacman")
pacman::p_load(
  here, shiny, shinydashboard,
  dplyr, forcats, ggplot2, magrittr, readr, readxl, stringr, tibble, tidyr, lubridate,
  janitor,
  DT,
  sf,
  necountries,
  patchwork
)

# 2. LOAD CUSTOM FUNCTIONS & EXTERNAL DATA SCRIPTS
# ------------------------------------------------------------------------------
# It's better to source scripts relative to the app's location.
source(here("utils.R"))
source(here("code", "external_data_GBD.R"))
theme_labels <- source(here("code", "theme_labels.R"))$value

# 3. READ IN PRE-PROCESSED DATASETS
# ------------------------------------------------------------------------------
sdg_data <- readRDS(here("data", "SDG_data_enhanced.rds")) |> droplevels()
state_geo <- readRDS(here("output", "state_geo_enhanced.rds"))
nearest_neighbors_list <- readRDS(here("output", "nearest_neighbors_list.rds"))

# Loop through API-generated files to read each one and assign it to an object
for (file_name in list.files(path = here("data", "API_data"),
                             pattern = "\\.rds$",
                             full.names = FALSE)) {
  # Create a variable name by removing the ".rds" extension
  object_name <- gsub("\\.rds$", "", file_name)
  
  # Read the .rds file and assign it to the new variable name
  assign(object_name, readRDS(here("data", "API_data", file_name)))
}

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  # 1. HEADER
  # ------------------------------------------------------------------------------
  dashboardHeader(
    title = tagList(
      span(class = "logo-lg", "Country Profiles & UPR"), 
      img(src = "logo_5.png", height = "40px", style = "margin-top: 5px;")
    )
  ),
  
  # 2. SIDEBAR
  # ------------------------------------------------------------------------------
  dashboardSidebar(
    width = 300,
    
    # -- Sidebar Menu for navigation --
    sidebarMenu(
      id = "tabs", # Giving the menu an ID for reference
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Preliminary Results", tabName = "results", icon = icon("chart-bar"))
    ),
    
    hr(),
    
    # -- Input Controls --
    # Encapsulate controls in a div for padding
    div(style = "padding: 0 15px;",
        h4("Dashboard Controls"),
        selectInput("selected_regional_grouping", "Select Regional grouping:",
                    choices = c("Sub-regions", "World Bank regions", "WHO regions", "ECSA-HC Membership", "FCS status"),
                    selected = "Sub-regions"),
        selectInput("selected_region", "Select Region:",
                    choices = c("Global"),
                    selected = "Global"),
        selectInput("selected_SUR", "Select State Under Review:",
                    # Choices are loaded from global.R
                    choices = unique(state_geo$country),
                    multiple = FALSE)
    ),
    
    hr(),
    
    # -- Global Map --
    plotOutput("global_map", height = "250px"),
    
    hr(),
    
    # -- Disclaimer --
    div(style = "padding: 0 15px;",
        h5("Disclaimer"),
        p(
          "This dashboard displays the results of a preliminary analysis regarding recommendations from the first four cycles of the Universal Periodic Review (UPR). Results are subject to change as the classification methodology continues to be refined.",
          style = "font-size: 0.9em;"
        )
    )
  ), # end dashboardSidebar
  
  # 3. BODY
  # ------------------------------------------------------------------------------
  dashboardBody(
    # Link to your custom stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.scss")
    ),
    
    tabItems(
      
      # -- "About" Tab Content --
      tabItem(tabName = "about",
              fluidRow(
                box(
                  width = 12,
                  solidHeader = TRUE,
                  div(class = "highlight-block", p(strong("The Right to Health"))),
                  p("The Right to Health is central to the fulfillment of broader human rights obligations, serving as a powerful tool to advance well-being, equity, and dignity across all sectors of society."),
                  p("The Right to Health comprises the State's obligations to:"),
                  tags$ul(
                    tags$li(strong("Respect:"), "refrain from interfering directly or indirectly with the enjoyment of the right to health."),
                    tags$li(strong("Protect:"), "take measures that prevent third parties from interfering with the guarantees of the right to health."),
                    tags$li(strong("Fulfill:"), "adopt appropriate legislative, administrative, budgetary, judicial, promotional, and other measures toward the full realization of the right to health.")
                  ),
                  hr(),
                  div(class = "highlight-block", p(strong("The Universal Periodic Review"))),
                  p("The UPR is a State-led, periodic peer review mechanism to evaluate each State’s “human rights obligations and commitments”"),
                  tags$ul(
                    tags$li("Each cycle repeats every 4-5 years (the 4th cycle is ongoing)"),
                    tags$li("Reviews are guided by three main pre-session reports (below)"),
                    tags$li("Reviewing States issue recommendations"),
                    tags$li("The State Under Review can either “Support” (accept) or “Note” each recommendation")
                  ),
                  p("More than 90,000 recommendations were issued during the first three cycles of the UPR. There is a growing focus on the right to health."),
                  hr(),
                  div(class = "highlight-block", p(strong("CeHDI"))),
                  p("CeHDI has a mission of amplifying and facilitating the inclusion of the priorities and voices of the Global South within the global health architecture and building robust partnerships for global health equity and the right to health"),
                  hr(),
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    img(src = "UPR_review_banner2.png", width = "70%")
                  )
                ) # end box
              ) # end fluidRow
      ), # end tabItem "About"
      
      # -- "Results" Tab Content --
      tabItem(tabName = "results",
              fluidRow(
                box(
                  title = "Distribution of Recommendations by UPR Cycle",
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("This plot shows the distribution of health-related vs. non-health-related recommendations received by states within the selected region for each UPR cycle."),
                  plotOutput("recommendations_boxplot")
                ),
                box(
                  title = "Full Thematic Breakdown",
                  status = "primary", solidHeader = TRUE, width = 12,
                  p("The plot below shows a more detailed breakdown of recommendations across all thematic areas for the selected State Under Review."),
                  div(
                    style = "text-align: center; margin-top: 20px;",
                    img(src = "full_plot.png", width = "100%", style="max-width: 800px;")
                  )
                )
              ) # end fluidRow
      ) # end tabItem "Results"
      
    ) # end tabItems
  ) # end dashboardBody
  
) # end dashboardPage

# Define the server function
server <- function(input, output, session) {
  
  # 1. REACTIVE EXPRESSIONS (from Quarto server context)
  # ------------------------------------------------------------------------------
  
  # Reactive expressions for dashboard regions
  state_geo_reactive <- reactive({
    if(input$selected_regional_grouping == "Sub-regions"){
      state_geo |> mutate(region_dashboard = subregion)}
    else if(input$selected_regional_grouping == "World Bank regions"){
      state_geo |> mutate(region_dashboard = wbregion)}
    else if(input$selected_regional_grouping == "ECSA-HC Membership"){
      state_geo |> mutate(region_dashboard = ECSA_status)}
    else if(input$selected_regional_grouping == "FCS status"){
      state_geo |> mutate(region_dashboard = FCS_status)} else{
        state_geo |> mutate(region_dashboard = WHO_region) }
  })
  
  sdg_data_dashboard <- reactive({
    sdg_data |>
      left_join(state_geo_reactive() |>
                  st_drop_geometry() |>
                  select(country, region_dashboard),
                join_by(state_under_review == country))
  })
  
  # Reactive expression to filter data to chosen Region
  filtered_upr <- reactive({
    sdg_data_dashboard() |>
      filter(state_under_review == input$selected_SUR)
  })
  
  # Reactive expression to filter data to chosen Region
  filtered_upr_region <- reactive({
    if(input$selected_region == "Global") {
      sdg_data_dashboard()
    } else{
      sdg_data_dashboard() |>
        filter(region_dashboard == input$selected_region)
    }
  })
  
  # Reactive expression to filter data to chosen Region
  region_selection <- reactive({
    if(input$selected_region == "Global") {
      state_geo_reactive()
    } else{
      state_geo_reactive() |>
        filter(region_dashboard == input$selected_region)
    }
  })
  
  # 2. OBSERVERS (to update inputs)
  # ------------------------------------------------------------------------------
  
  # Observe changes in the 'selected_region' input, and update setting choices
  observeEvent(input$selected_regional_grouping, {
    # Update the choices in the 'selected_indicateur' input
    updateSelectInput(
      session, "selected_region",
      choices = c("Global", levels(state_geo_reactive()$region_dashboard))
      , selected = "Global"
    )
  })
  
  # Observe changes in the 'selected_region' input, and update setting choices
  observeEvent(input$selected_region, {
    # Update the choices in the 'selected_indicateur' input
    updateSelectInput(
      session, "selected_SUR",
      choices = sort(unique(region_selection()$country))
      , selected = sort(unique(region_selection()$country))[1]
    )
  })
  
  # Get the SuR region_dashboard
  SUR_region <- reactive({
    req(input$selected_SUR) # Ensure a selection is made
    state_geo_reactive()[state_geo_reactive()$country==input$selected_SUR,]$region_dashboard
  })
  
  # Get the area of the SuR
  sur_area <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |>
      filter(country %in% c(input$selected_SUR)) |>
      st_area() |> as.numeric()})
  
  # Creat boundary box around chosen state and region
  bbox_selected_SUR <- reactive({
    req(input$selected_SUR)
    state_geo_reactive() |>
      filter(country %in% c(input$selected_SUR)) |>
      st_bbox()
  })
  
  bbox_SUR_region <- reactive({
    req(SUR_region())
    state_geo_reactive() |>
      filter(region_dashboard %in% c(SUR_region())) |>
      st_bbox()
  })
  
  # 3. OUTPUT RENDERING
  # ------------------------------------------------------------------------------
  
  output$global_map <- renderPlot({
    req(input$selected_SUR)
    p1 <- state_geo_reactive() |>
      mutate(selected_sur = factor(case_when(country == input$selected_SUR ~ input$selected_SUR,
                                             .default = "Other"),
                                   levels = c(input$selected_SUR, "Other"))) |>
      ggplot(aes(geometry = polygon, color = selected_sur, fill = selected_sur, lwd = selected_sur))+
      geom_sf()+
      scale_color_manual(values = c("green4", "grey80"))+
      scale_linewidth_manual(values=c(0.8, 0.3))+
      scale_fill_manual(values = c("green4", "grey90"))+
      theme_bw()+
      theme(
        panel.grid = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()
      )+
      labs(title = NULL,
           fill = NULL,
           color = NULL, lwd = NULL)+
      guides(fill = "none", lwd = "none", color = "none"
      )
    
    if(sur_area() > 10^11){p2<-p1} else{p2<-p1+geom_rect(
      aes(
        xmin = bbox_selected_SUR()["xmin"]-1,
        xmax = bbox_selected_SUR()["xmax"]+1,
        ymin = bbox_selected_SUR()["ymin"]-1,
        ymax = bbox_selected_SUR()["ymax"]+1
      ),
      fill = "transparent",
      color = "green4",
      linewidth = 0.5
    )}
    
    p2
  })
  
  # --- NEW PLOT RENDERER FOR BOXPLOT ---
  output$recommendations_boxplot <- renderPlot({
    
    # Use the reactive data filtered by region
    plot_data <- filtered_upr_region() |> 
      group_by(cycle, state_under_review) |> 
      count(health_related, .drop = FALSE) |>
      ungroup()
    
    # Create the plot
    ggplot(plot_data, aes(x = cycle, y = n, fill = health_related)) + 
      labs(
        title = "Boxplots of the number of recommendations received by each state",
        subtitle = paste("Region:", input$selected_region),
        y = "Number of recommendations", 
        color = NULL, 
        fill = NULL, 
        x = "UPR Cycle"
      ) +
      geom_jitter(aes(color = health_related), position = position_jitterdodge(dodge.width = 0.8), alpha = 0.3) +
      geom_boxplot(outlier.shape = NA, alpha = 0.8, staplewidth = 0.5, position = position_dodge(width = 0.8)) +
      
      # Add the median labels
      stat_summary(
        fun = median, 
        geom = "text", 
        aes(label = after_stat(y)), 
        position = position_dodge(width = 0.8),
        hjust = -0.2,
        color = "black",
        size = 3.5
      ) +
      
      theme_bw() +
      theme(
        legend.position = "bottom", 
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 12)
      )
  })
  
} # end server function

# Run the application 
shinyApp(ui = ui, server = server)