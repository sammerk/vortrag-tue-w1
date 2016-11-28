library(ggplot2)
library(dplyr)
library(tidyr)
library(shiny)
library(plotly)


#log_data_app <- feather::read_feather("log_data_short.feather")

log_data_app <- readr::read_csv("log_data_short.csv")
kl_data_app <- readr::read_csv("kl_data_app.csv")

shinyApp(
  ui = fluidPage(theme = "shiny_uni_css.css",
    sidebarLayout(
      sidebarPanel(
        selectInput('y', 'Y', c("Anz. Klicks", "Verweildauer", "Informativität", 
                                "Interpretationssicherheit"), selected = "Verweildauer"),
        selectInput('x', 'X', c("Inferenzniveau", "Qualitätsdimension", "Kennwerte", "Leitfragen",  
                                "Skalierung Einzelfr.",
                                "Skalierung Qualitätsdim."), selected = "Inferenzniveau"),
        selectInput('skalierung', 'Skalierung', c("Rohwerte","z-student. Werte", "ipsativ stand. Werte")),
        checkboxInput('jitter', 'Jitter'),
        checkboxInput('meanci', 'MW & CI', value = F),
        checkboxInput('violin', 'Dichte', value = T),
        conditionalPanel(condition = "input.skalierung == 'Rohwerte' & input.y == 'Verweildauer' ",
            sliderInput("ylim", "y-Limits", 0, 2000, c(0, 2000))
            ),
        conditionalPanel(condition = "input.skalierung == 'Rohwerte' & input.y == 'Anz. Klicks' ",
                         sliderInput("ylim3", "y-Limits", 0, 100, c(0, 500), step = .1)
        ),
        conditionalPanel(condition = "input.skalierung != 'Rohwerte'",
                         sliderInput("ylim2", "y-Limit", -10, 10, c(-2, 2), step = .1)
        ),
        actionButton("plot", "Plot", icon = icon("bar-chart-o"))
      ),
      mainPanel(
        plotOutput("mainplot"),
        verbatimTextOutput("glimpse"),
        verbatimTextOutput("debug2")
      )
    )
  )

  ,
  server = function(input, output) {
      
   # eventReactive(input$plot, {
      
         output$mainplot <- renderPlot({
           
           ## Fall A Verhaltensdaten
           if(input$y %in% c("Anz. Klicks", "Verweildauer")){
             
             
                 ## Abhängige Variable je "Y" und "Skalierung" wählen
                 if(input$y == "Verweildauer" & input$skalierung == "Rohwerte")
                 log_data_app$plot_y <- log_data_app$v_dauer_t_pp
                 
                 if(input$y == "Verweildauer" & input$skalierung == "z-student. Werte")
                 log_data_app$plot_y <- log_data_app$v_dauer_t_pp_gm
                 
                 if(input$y == "Verweildauer" & input$skalierung == "ipsativ stand. Werte")
                   log_data_app$plot_y <- log_data_app$v_dauer_t_pp_ip
                 
                 
                 if(input$y == "Anz. Klicks" & input$skalierung == "Rohwerte")
                   log_data_app$plot_y <- log_data_app$Summe_Klicks
                 
                 if(input$y == "Anz. Klicks" & input$skalierung == "z-student. Werte")
                   log_data_app$plot_y <- log_data_app$Summe_Klicks_gm
                 
                 if(input$y == "Anz. Klicks" & input$skalierung == "ipsativ stand. Werte")
                   log_data_app$plot_y <- log_data_app$Summe_Klicks_ip                
                 
                                  
                 
                 ## Auswahl der unabhängigen Vriable nach inout$x
                 log_data_app <- log_data_app[log_data_app$variable == print(input$x), ]
                       
                       
                    plot <- ggplot(log_data_app, aes_string(x = "value", y = "plot_y")) 
                    
                    if(input$skalierung == 'Rohwerte' & input$y == "Anz. Klicks")
                    plot <- plot + coord_cartesian(ylim=c(0, input$ylim3[2]))
                    
                    if(input$skalierung == 'Rohwerte' & input$y == "Verweildauer")
                      plot <- plot + coord_cartesian(ylim=c(0, input$ylim[2]))
                    
                    if(input$skalierung != 'Rohwerte')
                      plot <- plot + coord_cartesian(ylim=c(input$ylim2[1], input$ylim2[2]))
                    
                    if(input$jitter == T) 
                    plot <- plot  + geom_jitter()
                     
                    if(input$violin == T) 
                    plot <- plot  + geom_violin(color = "#2A4D8C4D", fill = "#2A4D8C4D")
                    
                    if(input$meanci == T)
                    plot <- plot + stat_summary(fun.data = "mean_cl_boot", colour = "#a51e41", size = 1.3,
                                                geom = "errorbar", width = .08) +
                                   stat_summary(fun.data = "mean_cl_boot", colour = "#a51e41", size = 1.3,
                                                geom = "point")
                    
                    
              
           }
           
              
          ## Fall B Selbstauskunftsdaten
           if(!input$y %in% c("Anz. Klicks", "Verweildauer")){
             
             ## Abhängige Variable je "Skalierung" wählen
             if(input$y == "Informativität" & input$skalierung == "Rohwerte")
               kl_data_app$plot_y <- kl_data_app$Informativität
             
             if(input$y == "Informativität" & input$skalierung == "z-student. Werte")
               kl_data_app$plot_y <- kl_data_app$Informativität_gm
             
             if(input$y == "Informativität" & input$skalierung == "ipsativ stand. Werte")
               kl_data_app$plot_y <- kl_data_app$Informativität_ip
             
             
            ## Auswahl der unabhängigen Variable nach inout$x
             kl_data_app <- kl_data_app[kl_data_app$variable == print(input$x), ]
             
             
             plot <- ggplot(kl_data_app, aes_string(x = "value", y = "plot_y")) 
             
             if(input$skalierung == 'Rohwerte' & input$y == "Anz. Klicks")
               plot <- plot + coord_cartesian(ylim=c(0, input$ylim3[2]))
             
             if(input$skalierung == 'Rohwerte' & input$y == "Verweildauer")
               plot <- plot + coord_cartesian(ylim=c(0, input$ylim[2]))
             
             if(input$skalierung != 'Rohwerte')
               plot <- plot + coord_cartesian(ylim=c(input$ylim2[1], input$ylim2[2]))
             
             if(input$jitter == T) 
               plot <- plot  + geom_jitter()
             
             if(input$violin == T) 
               plot <- plot  + geom_violin(color = "#2A4D8C4D", fill = "#2A4D8C4D")
             
             if(input$meanci == T)
               plot <- plot + stat_summary(fun.data = "mean_cl_boot", colour = "#a51e41", size = 1.3,
                                           geom = "errorbar", width = .08) +
                              stat_summary(fun.data = "mean_cl_boot", colour = "#a51e41", size = 1.3,
                                           geom = "point")
             
             
           }
           
           plot
           
         })
               
         
         
         
              output$glimpse <- renderPrint({
                print(glimpse(kl_data_app))
               length(input$y)
               str(input$y)
               class(input$y)
              })
              
              output$debug2 <- renderPrint({
               # print(input$y)
              print(Y_Var)
              length(Y_Var)
              str(Y_Var)
              class(Y_Var)
              })
                 #}
           
           

           
           
           
           

         
         
        
         
      
   # })
    
  }
)
