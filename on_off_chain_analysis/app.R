library(shiny)
library(shinyWidgets)
library(tidyverse)
library(plotly)
library(scales)
library(vegan)
library(extrafont)
library(showtext)
library(bslib)
library(shinyjs)
library(bsplus)

rsconnect::setAccountInfo(name='INSERT', 
                          token='INSERT', 
                          secret='INSERT')

text_intro <- p(h4(strong("Introduction")),
                p("This dashboard analyzes Uniswap on-chain and off-chain (Snapshot) voting participation and distribution data.  
                  A diverse set of metrics and visualizations are presented to examine the degree of decentralization exhibited in historical Uniswap's governance proposals across the different phases, including the Temperature Check (off-chain), Consensus Check (off-chain), and on-chain governance vote.",
                  br(),
                  p("Additional details regarding the governance process can be found on "
                    , a("Uniswap's Technical Documentation.", href = "https://docs.uniswap.org/protocol/concepts/governance/process")) ,
                  
                  p("Voting data is current as of August 6, 2022."
                    ,a("(Link to GitHub)", 
                       href = "https://github.com/Signal-Corps/Governance-Decentralization-Benchmarks-and-Tracker" ))))


text_contact <- p(h4(strong("Twitter Contact")),
                  a("@taylorbplumer", href = "https://twitter.com/taylorbplumer"),
                  br(),
                  a("@hunter_boost", href = "https://twitter.com/hunter_boost"))

df_summary <-  readRDS("df_summary.rds")  # Developed in Uniswap_On_Off_Chain_Voting.R script

list_proposals <- df_summary %>% pull(proposal) %>% unique()

list_metrics <- c("voting_participants" ,
                       "voting_power" ,
                       "nakamoto" ,
                       "hhi" ,
                       "gini" ,
                       "shannon")


ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz", version = 5),
  useShinyjs(),
  
  titlePanel("Uniswap Governance | On-/Off-Chain Analysis"),
  sidebarLayout(
    sidebarPanel(
      text_intro,
      text_contact,
      # inputs
      selectInput("metricInput", strong("Metric Selection"),
                  choices = list_metrics ,
                  selected = "voting_participants"),
      checkboxGroupInput("proposalInput", strong("Proposal Selection"),
                  choices = list_proposals ,
                  selected = list_proposals),
     
    ),  
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 DT::dataTableOutput("mytable1"), 
                 h4(strong("Metric Definitions")),
                 p("All metrics were calculated for each individual proposal.") ,
                 p(strong("Voting Participants:")," count of unique voting addresses.") ,
                 p(strong("Voting Power:")," sum of votes cast.") ,
                 p(strong("Nakamoto Coefficient:"),"  based on metric originally defined in Srinivasan and Lee (2017).  For this application, it is calculated as the minimum number of entities (i.e. unique voter addresses) 
                   required to control 51% or greater voting share.  ",
                   a("(Reference)", href = "https://news.earn.com/quantifying-decentralization-e39db233c28e")),
                 p(strong("Herfindahl-Hirschman Index (HHI):")," measure of market concentration, most notably used by the United States Department of Justice (DOJ) & Federal Trade Commission (FTC).  It is calculated by squaring the market share of each firm competing in the market and then summing the resulting numbers.  For example, for a market consisting of four firms with shares of 30, 30, 20, and 20 percent, the HHI is 2,600 (30^2 + 30^2 + 20^2 + 20^2 = 2,600).  The HHI can range from 1 (least concentrated) to 10,000 (most concentrated).  The DOJ and other governmental agencies historically consider markets under 1,500 to be low concentration, markets between 1,500 to 2,500 to be moderately concentrated, and markets in excess of 2,500 to be highly concentrated.  ",
                   a("(Reference)", href = "https://www.justice.gov/atr/herfindahl-hirschman-index#:~:text=The%20HHI%20is%20calculated%20by,%2B%20202%20%3D%202%2C600)." )),
                 p(strong("Gini Coefficient:"),"  a measure of equality within a community, commonly used to quantify income inequality.  A low/high value represents a more equal/unequal community. It is calculated as the ratio of the area between the perfect equality (dotted) line and the Lorenz Curve divided by total area under the perfect equality (dotted) line.  ",
                   a("(Reference)", href = "https://www.nature.com/articles/s41599-020-0484-6" )),
                 p(strong("Shannon Diversity Index:"),"  a popular measure used in ecology to quantify the diversity of species, 
                   often by species' respective population count, within a community.  For this application, it is used to quantify the diversity of 
                   voting weight of participants. 
                   A higher value represents a more diverse and equitable community. 
                   The Shannon diversity index is calculated by taking the sum of each voter’s proportional 
                   voting power (Pi) multiplied by the natural log of Pi  and then multiplying by -1.  ",
                   a("(Reference)", href = "https://www.statology.org/shannon-diversity-index/" )),
                 
                 
        ), 
        tabPanel("Plots ",
                 br(),
                 plotlyOutput("plot_voting_participants"),
                 br(), br(),
                 plotlyOutput("plot_nakamoto"),
                 br(), br(),
                 plotlyOutput("plot_hhi"),
                 br(), br(),
                 plotlyOutput("plot_shannon"),
                 br(), br(),
                 plotlyOutput("plot_gini"),
                 br(), br()
        )
        
      )
    ) 
  )   
)   

server <- function(input, output) {
  
  df_summary_factor <- reactive({
    df_summary %>% 
      mutate(proposal = factor(proposal) ,
                          stage = factor(stage, levels = c("temperature", "consensus", "on_chain"))
            ) %>%
      filter(proposal %in% input$proposalInput) 
      
  })
  
  df_summary_long <- reactive({
    df_summary %>%   
      pivot_longer(cols = voting_participants:shannon ,
                                  names_to = "metric" ,
                                  values_to = "value") %>%      
      filter(proposal %in% input$proposalInput )
  })
  
  df_summary_wide <- reactive({
    df_summary %>%
      filter(proposal %in% input$proposalInput) %>%
      select(proposal, stage,  input$metricInput) %>%
      pivot_wider(names_from = stage ,
                  values_from = input$metricInput) %>%
      left_join(df_summary %>% 
                  filter(stage=="on_chain") %>% 
                  select(proposal, proposal.title, status_on) %>%
                  rename(status = status_on)
                , by = "proposal") %>%
      select(proposal,proposal.title, status, temperature, consensus, on_chain)
      
  }) 
  

  output$mytable1 <- DT::renderDataTable({
    
    DT::datatable(df_summary_wide(), 
                  options = list(dom = 't' 
                                 , pageLength = 50
                                 , initComplete = htmlwidgets::JS(
                                   "function(settings, json) {",
                                   "$(this.api().table().header()).css({'color': '#fff'});",
                                   "}")
                                 , columnDefs = list(list(className = 'dt-center',
                                                          targets = "_all") )
                                  ) 
                  
                  , caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: left;'
                    ,'Lower values for HHI and Gini Coefficient and higher values for Nakamoto Coefficient and Shannon Diversity Index indicate greater decentralization.  
                    Blank values indicate the underlying voting data for that particular proposal-phase combination is unavailable.')
                  , rownames= FALSE) %>%
      
      DT::formatCurrency(c('temperature', 'consensus', 'on_chain') ,
                         currency = "", interval = 3, mark = ","
                          , digits = case_when(input$metricInput=="gini" ~ 3 ,
                                               input$metricInput=="shannon" ~ 1 ,
                                               TRUE ~ 0)
                         )
    
  })

  output$plot_voting_participants <- renderPlotly({
    
    ggplotly(
      ggplot(df_summary_factor(), 
             aes(x=proposal, 
                 y=voting_participants,
                 color = stage ,
                 group = stage
             )) +
        geom_point() +
        geom_line(size = 1) +
        theme_minimal() +
        hrbrthemes::scale_y_comma() +
        expand_limits(y=0) +
        theme(axis.title.y = element_text(angle = 90)) +
        labs(title = "Voting Participants",
             x = "Proposal")
    )
    
    
  }) 
  
  
  output$plot_nakamoto <- renderPlotly({
    
    ggplotly(
      ggplot(df_summary_factor(), 
             aes(x=proposal, 
                 y=nakamoto,
                 color = stage ,
                 group = stage
                 )) +
        geom_point() +
        geom_line(size = 1) +
        theme_minimal() +
        hrbrthemes::scale_y_comma() +
        expand_limits(y=0) +
        theme(axis.title.y = element_text(angle = 90)) +
        labs(title = "Nakamoto Coefficient",
             x = "Proposal")
    )
    
    
  })  
  
  output$plot_hhi <- renderPlotly({
    
    ggplotly(
      ggplot(df_summary_factor(), 
             aes(x=proposal, 
                 y=hhi,
                 color = stage ,
                 group = stage
             )) +
        geom_point() +
        geom_line(size = 1) +
        theme_minimal() +
        hrbrthemes::scale_y_comma() +
        expand_limits(y=0) +
        theme(axis.title.y = element_text(angle = 90)) +
        labs(title = "Herfindahl-Hirschman Index (HHI)",
             x = "Proposal")
    )
    
    
  })    
  
  output$plot_shannon <- renderPlotly({
    
    ggplotly(
      ggplot(df_summary_factor(), 
             aes(x=proposal, 
                 y=shannon,
                 color = stage ,
                 group = stage
             )) +
        geom_point() +
        geom_line(size = 1) +
        theme_minimal() +
        hrbrthemes::scale_y_comma() +
        expand_limits(y=0) +
        theme(axis.title.y = element_text(angle = 90)) +
        labs(title = "Shannon Diversity Index",
             x = "Proposal")
    )
    
    
  })
  
  output$plot_gini <- renderPlotly({
    
    ggplotly(
      ggplot(df_summary_factor(), 
             aes(x=proposal, 
                 y=gini,
                 color = stage ,
                 group = stage
             )) +
        geom_point() +
        geom_line(size = 1) +
        theme_minimal() +
        hrbrthemes::scale_y_comma(labels = scales::number_format(accuracy = 0.01,
                                                                 decimal.mark = '.')) +
        expand_limits(y=0) +
        theme(axis.title.y = element_text(angle = 90)) +
        labs(title = "Gini Coefficient" ,
             x = "Proposal")
    )
    
    
  })  
  

}

shinyApp(ui=ui, server=server)



