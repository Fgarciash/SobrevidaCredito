

library(shiny)
library(ggplot2)
library(RODBC)
library(survival)
library(KMsurv)
library(survMisc)
library(survminer)
library(ggfortify)
library(flexsurv)
library(actuar)
library(dplyr)
library(highcharter)



ui <- fluidPage(
    
    titlePanel("Selección"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("PERIODO",
                        "PERIODO:",
                        choices=SobrevidaCred$PERIODO,
                        selected=1)
            ,
            selectInput("VARIABLE",
                        "VARIABLE:",
                        choices=c("SEXO", "ECIVIL", "EDAD_C","RENTA_C"), selected = "EDAD_C")
            
            ,
            selectInput("TIPO",
                        "TIPO:",
                        choices=c("ACUMULADA", "NO ACUMULADA"),
                        selected = "NO ACUMULADA")
            
            
            
        ),
        
        
        mainPanel(
            
            highchartOutput("distPlot",height = "500px")
            
            
        )
    )
)

server <- function(input, output) {
    
    output$distPlot <- renderHighchart({
        
        
        
        data_sobre=SobrevidaCred %>% filter(PERIODO==input$PERIODO)
        CATEGORIA <- paste0("Surv(tiempo, censura) ~ ", input$VARIABLE)  
        TIPO <- input$TIPO
        
        #sin acumular 
        
        if (TIPO== "NO ACUMULADA")  {  
            ejem.km <- surv_fit(as.formula(CATEGORIA), data = data_sobre, type = "kaplan-meier")  
            
            hchart(ejem.km, fun=NULL) %>% 
                hc_title(text = "Curva de Sobreviviencia Créditos")  %>%
                hc_xAxis(title = list(text = "Cuotas"),
                         opposite = F)  %>% 
                hc_yAxis(title = list(text = "Sobrevida %"),
                         minorTickInterval = "auto",
                         minorGridLineDashStyle = "LongDashDotDot",
                         showFirstLabel = T,
                         showLastLabel = T
                         #             ,
                         #             plotBands = list(
                         #               list(from = 0.5, to = 0.8, color = "rgba(100, 0, 0, 0.1)",
                         #                    label = list(text = "This is a plotBand")))
                ) %>%
                hc_credits(
                    enabled = TRUE,
                    text = "Análisis de Sobrevida de Kaplan Meier en Créditos ",
                    href = "https://github.com/Fgarciash") 
            
        }
        else
        {
            #Riesgo Acumulado     
            
            ejem.km.cum <- surv_fit(as.formula(CATEGORIA), data = data_sobre, type = "kaplan-meier") 
            hchart(ejem.km.cum, fun = "cumhaz") %>%
                
                hc_title(text = "Curva de Sobreviviencia Créditos")  %>%
                hc_xAxis(title = list(text = "Cuotas"),
                         opposite = F)  %>% 
                hc_yAxis(title = list(text = "Riesgo Acumulado %"),
                         minorTickInterval = "auto",
                         minorGridLineDashStyle = "LongDashDotDot",
                         showFirstLabel = T,
                         showLastLabel = T           
                )  %>%
                hc_credits(
                    enabled = TRUE,
                    text = "Análisis de Sobrevida de Kaplan Meier en Créditos ",
                    href = "https://github.com/Fgarciash") 
            
            
        }
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
