library(shiny)
library(RODBC)
library(tidyverse)
library(rsconnect)
library(plotly)
library(shinythemes)
library(shinydashboard)
library(flexdashboard)
library(data.table)
library(DT)

serverfun<- function(input, output){
 
  
  
  var_mes <- "MAR-20"
  var_mes_anterio <- "FEB-20"
  var_mma <- "MAR-19"
    
    canal_bd<- odbcConnect("prueba")
    query<- sqlQuery(canal_bd, "SELECT [CATEGORIA],[OFICINA_VENTAS],[MES],[MARCA],[FAMILIA],[CANAL],[VENDEDOR],[TIPO_VENTAS],[CC],[CR],[FREC],[TP],[AMPLITUD],[PROFUNDIDAD] FROM [ECUADOR].[dbo].[FRECUENCIA_ECUADOR_1]")
    niveles_meses <- c("ENE-19", "FEB-19", "MAR-19", "ABR-19", "MAY-19", "JUN-19","JUL-19", "AGO-19", "SEP-19", "OCT-19", "NOV-19", "DIC-19","ENE-20", "FEB-20", "MAR-20", "ABR-20", "MAY-20", "JUN-20","JUL-20", "AGO-20", "SEP-20", "OCT-20", "NOV-20", "DIC-20") 

############################################## GRAFICOS DE CcC, CcR, FREC Y TP #################################################################################    
        output$grafico_ccc <- renderPlotly({
      
                     lines_data <- data.frame(MES=  subset( query$MES , query$CATEGORIA == input$Cate & 
                                                              query$FAMILIA == input$Fam & 
                                                              query$OFICINA_VENTAS == input$Ofi &
                                                              query$CANAL == "TOTAL"& 
                                                              query$MARCA == input$Mar &
                                                              query$VENDEDOR == input$Ven &
                                                              query$TIPO_VENTAS == input$Tip),
                                             CcC=subset(query$CC, query$CATEGORIA == input$Cate & 
                                                          query$FAMILIA == input$Fam & 
                                                          query$OFICINA_VENTAS == input$Ofi &
                                                          query$CANAL == "TOTAL"& 
                                                          query$MARCA == input$Mar &
                                                          query$VENDEDOR == input$Ven &
                                                          query$TIPO_VENTAS == input$Tip),
                                             CcR=subset(query$CR, query$CATEGORIA == input$Cate & 
                                                          query$FAMILIA == input$Fam & 
                                                          query$OFICINA_VENTAS == input$Ofi &
                                                          query$CANAL == "TOTAL"&  
                                                          query$MARCA == input$Mar &
                                                          query$VENDEDOR == input$Ven &
                                                          query$TIPO_VENTAS == input$Tip)
                                             , stringsAsFactors = FALSE)
                          
                
                    lines_data$MES<- factor(lines_data$MES, levels = niveles_meses) 
                    lines_data$CcR<- round(lines_data$CcR/lines_data$CcC*100,0)
                 
                H1 <- plot_ly(lines_data, x = ~MES)  
                H1 <- H1 %>% add_lines(y = ~CcC, name = "CcC",  type = 'scatter',mode='lines',line = list(shape = "CcR",color="rgb(245, 255, 37)"),stackgroup='one') 
   
        }
    )
  
    
    
    output$grafico_ccr <- renderPlotly({
      lines_data <- data.frame(MES=  subset( query$MES , query$CATEGORIA == input$Cate & 
                                               query$FAMILIA == input$Fam & 
                                               query$OFICINA_VENTAS == input$Ofi &
                                               query$CANAL == "TOTAL"& 
                                               query$MARCA == input$Mar &
                                               query$VENDEDOR == input$Ven &
                                               query$TIPO_VENTAS == input$Tip),
                               CcC=subset(query$CC, query$CATEGORIA == input$Cate & 
                                            query$FAMILIA == input$Fam & 
                                            query$OFICINA_VENTAS == input$Ofi &
                                            query$CANAL == "TOTAL"&  
                                            query$MARCA == input$Mar &
                                            query$VENDEDOR == input$Ven &
                                            query$TIPO_VENTAS == input$Tip),
                               CcR=subset(query$CR, query$CATEGORIA == input$Cate & 
                                            query$FAMILIA == input$Fam & 
                                            query$OFICINA_VENTAS == input$Ofi &
                                            query$CANAL == "TOTAL"& 
                                            query$MARCA == input$Mar &
                                            query$VENDEDOR == input$Ven &
                                            query$TIPO_VENTAS == input$Tip)
                               , stringsAsFactors = FALSE)
      
      
      lines_data$MES<- factor(lines_data$MES, levels = niveles_meses) 
      lines_data$CcR<- lines_data$CcR/lines_data$CcC
    
      
      
      H1 <- plot_ly(lines_data, x = ~MES)  
      H1 <- H1 %>% add_lines(y = ~CcR, name = "CcR", mode='lines',
                             line = list(shape = "CcR",color="teal"),
                             stackgroup='one') 
     
      H1 <- H1 %>%
        layout(
          yaxis = list(
            tickformat = "%"
          ))
      
    }
    ) 
    
    output$grafico_frec <- renderPlotly({
      
      lines_data <- data.frame(MES=  subset( query$MES , query$CATEGORIA == input$Cate & 
                                               query$FAMILIA == input$Fam & 
                                               query$OFICINA_VENTAS == input$Ofi &
                                               query$CANAL == "TOTAL"&  
                                               query$MARCA == input$Mar &
                                               query$VENDEDOR == input$Ven &
                                               query$TIPO_VENTAS == input$Tip),
                               FREC=subset(query$FREC, query$CATEGORIA == input$Cate & 
                                             query$FAMILIA == input$Fam & 
                                             query$OFICINA_VENTAS == input$Ofi &
                                             query$CANAL == "TOTAL"&  
                                             query$MARCA == input$Mar &
                                             query$VENDEDOR == input$Ven &
                                             query$TIPO_VENTAS == input$Tip)
                               , stringsAsFactors = FALSE)
      
      
      lines_data$MES<- factor(lines_data$MES, levels = niveles_meses) 
      lines_data$FREC<- round(lines_data$FREC,1)
      
      H1 <- plot_ly(lines_data, x = ~MES)  
      H1 <- H1 %>% add_lines(y = ~FREC, name = "FREC", mode='lines',
                             line = list(shape = "FREC",color="light-blue"),
                             stackgroup='one') 
      
      
    }
    ) 
    
    
    output$grafico_tp <- renderPlotly({
      
      lines_data <- data.frame(MES=  subset( query$MES , query$CATEGORIA == input$Cate & 
                                               query$FAMILIA == input$Fam & 
                                               query$OFICINA_VENTAS == input$Ofi &
                                               query$CANAL == "TOTAL"& 
                                               query$MARCA == input$Mar &
                                               query$VENDEDOR == input$Ven &
                                               query$TIPO_VENTAS == input$Tip),
                               TP=subset(query$TP, query$CATEGORIA == input$Cate & 
                                           query$FAMILIA == input$Fam & 
                                           query$OFICINA_VENTAS == input$Ofi &
                                           query$CANAL == "TOTAL"& 
                                           query$MARCA == input$Mar &
                                           query$VENDEDOR == input$Ven &
                                           query$TIPO_VENTAS == input$Tip)
                               , stringsAsFactors = FALSE)
      
      
      lines_data$MES<- factor(lines_data$MES, levels = niveles_meses) 
      lines_data$TP<- round(lines_data$TP,1)
      
      H1 <- plot_ly(lines_data, x = ~MES)  
      H1 <- H1 %>% add_lines(y = ~TP, name = "TP", mode='lines',
                             line = list(shape = "TP",color="rgb(121, 0, 187)"),
                             stackgroup='one') 
      
      
    }
    ) 
    
    output$grafico_amp <- renderPlotly({
      
      lines_data <- data.frame(MES=  subset( query$MES , query$CATEGORIA == input$Cate & 
                                               query$FAMILIA == input$Fam & 
                                               query$OFICINA_VENTAS == input$Ofi &
                                               query$CANAL == "TOTAL"& 
                                               query$MARCA == input$Mar &
                                               query$VENDEDOR == input$Ven &
                                               query$TIPO_VENTAS == input$Tip),
                               AMP=subset(query$AMPLITUD, query$CATEGORIA == input$Cate & 
                                           query$FAMILIA == input$Fam & 
                                           query$OFICINA_VENTAS == input$Ofi &
                                            query$CANAL == "TOTAL"&  
                                           query$MARCA == input$Mar &
                                           query$VENDEDOR == input$Ven &
                                           query$TIPO_VENTAS == input$Tip)
                               , stringsAsFactors = FALSE)
      
      
      lines_data$MES<- factor(lines_data$MES, levels = niveles_meses) 
      lines_data$AMP <- round(lines_data$AMP,1)
      
      H1 <- plot_ly(lines_data, x = ~MES)  
      H1 <- H1 %>% add_lines(y = ~AMP, name = "AMP", mode='lines',
                             line = list(shape = "AMP",color='yellow'),
                             stackgroup='one') 
      
      
    }
    ) 
    
    output$grafico_pro <- renderPlotly({
      
      lines_data <- data.frame(MES=  subset( query$MES , query$CATEGORIA == input$Cate & 
                                               query$FAMILIA == input$Fam & 
                                               query$OFICINA_VENTAS == input$Ofi &
                                               query$CANAL == "TOTAL"& 
                                               query$MARCA == input$Mar &
                                               query$VENDEDOR == input$Ven &
                                               query$TIPO_VENTAS == input$Tip),
                               PROF=subset(query$PROFUNDIDAD, query$CATEGORIA == input$Cate & 
                                            query$FAMILIA == input$Fam & 
                                            query$OFICINA_VENTAS == input$Ofi &
                                             query$CANAL == "TOTAL"& 
                                            query$MARCA == input$Mar &
                                            query$VENDEDOR == input$Ven &
                                            query$TIPO_VENTAS == input$Tip)
                               , stringsAsFactors = FALSE)
      
      
      lines_data$MES<- factor(lines_data$MES, levels = niveles_meses) 
      lines_data$PROF <- round(lines_data$PROF,1)
      
      H1 <- plot_ly(lines_data, x = ~MES)  
      H1 <- H1 %>% add_lines(y = ~PROF, name = "PROF", mode='lines',
                             line = list(shape = "PROF",color='orange'),
                             stackgroup='one') 
      
      
    }
    ) 
#########################################################################################################################################################################    
  
    
    
    
##################################################### VALORES DE CcC, CcR, FREC Y TP ####################################################################################  
    output$resumen <- renderValueBox({
      
      shinydashboard::valueBox(
        value = NULL,
        subtitle = "Resumen",
        icon =  icon("shopping-cart"),
        color = "navy",
        width = 2
      )
      
      
    }
    )
    
     output$compra <- renderValueBox({
      
      ccc_mes_actual <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                        query$FAMILIA == input$Fam & 
                                        query$OFICINA_VENTAS == input$Ofi &
                                        query$CANAL == "TOTAL" & 
                                        query$MARCA == input$Mar &
                                        query$VENDEDOR == input$Ven &
                                        query$TIPO_VENTAS == input$Tip &
                                        query$MES ==  var_mes)
     
       
      shinydashboard::valueBox(
       value = ccc_mes_actual,
        subtitle = "CcC",
        icon =  icon("shopping-cart"),
        color = "teal",
        width = 2
      )
       
      
    }
    )
    
    output$recompra <- renderValueBox({
      
      ccr_mes_actual <-  subset( query$CR , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      ccc_mes_actual <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      ccr <- round((ccr_mes_actual/ccc_mes_actual)*100,0)
      
      shinydashboard::valueBox(
        value = ccr,
        subtitle = "CcR ",
        icon = icon("percent"),
        color = "aqua",
        width = 2
      )
      
      
    }
    )
    
    output$frecuencia <- renderValueBox({
      
      ccc_mes_actual <-  subset( query$FREC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      shinydashboard::valueBox(
        value = round(ccc_mes_actual,1),
        subtitle = "Frec",
        icon = icon("calendar-alt"),
        color = "light-blue",
        width = 2
      )
      
      
    }
    )
    output$tp <- renderValueBox({
      
      ccc_mes_actual <-  subset( query$TP , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      shinydashboard::valueBox(
        value = round(ccc_mes_actual,1),
        subtitle = "TP",
        icon = icon("ticket-alt"),
        color = "blue",
        width = 2
      )
      
      
    }
    )
    
    output$amp <- renderValueBox({
      
      amp_mes_actual <-  subset( query$AMPLITUD , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      shinydashboard::valueBox(
        value = round(amp_mes_actual,1),
        subtitle = "Amp",
        icon = icon("shopping-cart"),
        color = "yellow",
        width = 2
      )
      
      
    }
    )
    
    
    output$pro <- renderValueBox({
      
      pro_mes_actual <-  subset( query$PROFUNDIDAD , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      shinydashboard::valueBox(
        value = round(pro_mes_actual,1),
        subtitle = "Prof",
        icon = icon("shopping-cart"),
        color = "orange",
        width = 2
      )
      
      
    }
    )
####################################################################################################################################################################
    
    
####################################### VARIACIONES DE CcC, CcR, FREC Y TP  DEL MES ANTERIOR #######################################################################   
    output$var_ccc <- flexdashboard::renderGauge({
      
      ccc_mes_actual <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      ccc_mes_anterior <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes_anterio)
      
      var_cc <- (ccc_mes_actual/ccc_mes_anterior-1)*100
      var_cc 
      gauge( round(var_cc,0), min = -50, max = 100, label = "%Var CcC MA", symbol = '%', gaugeSectors(
        success = c(1, 100), warning = c(0, 1), danger = c(-50, 0), 
      ))
     
    })
    
    output$var_ccc_1 <- renderPlotly({
      
      ccc_mes_actual <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" & 
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      ccc_mes_anterior <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                     query$FAMILIA == input$Fam & 
                                     query$OFICINA_VENTAS == input$Ofi &
                                     query$CANAL == "TOTAL" &  
                                     query$MARCA == input$Mar &
                                     query$VENDEDOR == input$Ven &
                                     query$TIPO_VENTAS == input$Tip &
                                     query$MES == var_mes_anterio)
      
      var_cc <- (ccc_mes_actual/ccc_mes_anterior-1)*100
      var_cc 
      
      
      fig <- plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = ccc_mes_actual,
        title = list(text = "%Var CcC M.A", font = list(size = 14)),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference =ccc_mes_anterior ),
        gauge = list(
          axis =list(range = list(NULL, 500)),
          steps = list(
            list(range = c(0, 250), color = "lightgray"),
            list(range = c(250, 400), color = "gray")),
          threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = 490)),
        width = 200,
        height = 150,
        align="center") 
      
      
    }
    )
    
    
    output$var_ccr <- flexdashboard::renderGauge({
      
      ccc_mes_anterior <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" &  
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes_anterio)
      
      ccr_mes_anterior <-  subset( query$CR , query$CATEGORIA == input$Cate & 
                                     query$FAMILIA == input$Fam & 
                                     query$OFICINA_VENTAS == input$Ofi &
                                     query$CANAL == "TOTAL" &  
                                     query$MARCA == input$Mar &
                                     query$VENDEDOR == input$Ven &
                                     query$TIPO_VENTAS == input$Tip &
                                     query$MES == var_mes_anterio)
      
      ccr_anterior <- (ccr_mes_anterior/ccc_mes_anterior)*100
      
      ccr_mes_actual <-  subset( query$CR , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" &  
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      ccc_mes_actual <-  subset( query$CC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" &  
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      ccr_actual <- (ccr_mes_actual/ccc_mes_actual)*100
      
      
      
      
      var_cr <- ccr_actual-ccr_anterior
      var_cr 
      flexdashboard::gauge( round(var_cr,0), min = -50, max = 100, label = "%Var CcR MA", symbol = '%', gaugeSectors(
        success = c(1, 100), warning = c(0, 1), danger = c(-50, 0) 
      ))
      
    }
    )
    
    output$var_frec <- flexdashboard::renderGauge({
      
      frec_mes_actual <-  subset( query$FREC , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" &  
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      frec_mes_anterior <-  subset( query$FREC , query$CATEGORIA == input$Cate & 
                                     query$FAMILIA == input$Fam & 
                                     query$OFICINA_VENTAS == input$Ofi &
                                     query$CANAL == "TOTAL" &  
                                     query$MARCA == input$Mar &
                                     query$VENDEDOR == input$Ven &
                                     query$TIPO_VENTAS == input$Tip &
                                     query$MES == var_mes_anterio)
      
      var_frec <- (round(frec_mes_actual,1)/round(frec_mes_anterior,1)-1)*100
      var_frec 
      flexdashboard::gauge(round(var_frec,0), min = -50, max = 100, label = "%Var FREC MA", symbol = '%', gaugeSectors(
        success = c(1, 100), warning = c(0, 1), danger = c(-50, 0) 
      ))
      
    }
    )
    
    output$var_tp <- flexdashboard::renderGauge({
      
      tp_mes_actual <-  subset( query$TP , query$CATEGORIA == input$Cate & 
                                    query$FAMILIA == input$Fam & 
                                    query$OFICINA_VENTAS == input$Ofi &
                                    query$CANAL == "TOTAL" &  
                                    query$MARCA == input$Mar &
                                    query$VENDEDOR == input$Ven &
                                    query$TIPO_VENTAS == input$Tip &
                                    query$MES == var_mes)
      
      tp_mes_anterior <-  subset( query$TP , query$CATEGORIA == input$Cate & 
                                      query$FAMILIA == input$Fam & 
                                      query$OFICINA_VENTAS == input$Ofi &
                                      query$CANAL == "TOTAL" &  
                                      query$MARCA == input$Mar &
                                      query$VENDEDOR == input$Ven &
                                      query$TIPO_VENTAS == input$Tip &
                                      query$MES == var_mes_anterio)
      
      var_tp <- (tp_mes_actual/tp_mes_anterior-1)*100
      var_tp 
      flexdashboard::gauge( round(var_tp,0), min = -50, max = 100, label = "%Var TP MA", symbol = '%', gaugeSectors(
        success = c(1, 100), warning = c(0, 1), danger = c(-50, 0) 
      ))
      
    }
    )
    
    output$var_amp <- flexdashboard::renderGauge({
      
      amp_mes_actual <-  subset( query$AMPLITUD , query$CATEGORIA == input$Cate & 
                                  query$FAMILIA == input$Fam & 
                                  query$OFICINA_VENTAS == input$Ofi &
                                  query$CANAL == "TOTAL" &  
                                  query$MARCA == input$Mar &
                                  query$VENDEDOR == input$Ven &
                                  query$TIPO_VENTAS == input$Tip &
                                  query$MES == var_mes)
      
      amp_mes_anterior <-  subset( query$AMPLITUD , query$CATEGORIA == input$Cate & 
                                    query$FAMILIA == input$Fam & 
                                    query$OFICINA_VENTAS == input$Ofi &
                                    query$CANAL == "TOTAL" & 
                                    query$MARCA == input$Mar &
                                    query$VENDEDOR == input$Ven &
                                    query$TIPO_VENTAS == input$Tip &
                                    query$MES == var_mes_anterio)
      
      var_amp <- (amp_mes_actual/amp_mes_anterior-1)*100
      var_amp
      flexdashboard::gauge( round(var_amp,0), min = -50, max = 100, label = "%Var Amp", symbol = '%', gaugeSectors(
        success = c(1, 100), warning = c(0, 1), danger = c(-50, 0) 
      ))
      
    }
    )
    
    output$var_prof<- flexdashboard::renderGauge({
      
      prof_mes_actual <-  subset( query$PROFUNDIDAD , query$CATEGORIA == input$Cate & 
                                   query$FAMILIA == input$Fam & 
                                   query$OFICINA_VENTAS == input$Ofi &
                                   query$CANAL == "TOTAL" &  
                                   query$MARCA == input$Mar &
                                   query$VENDEDOR == input$Ven &
                                   query$TIPO_VENTAS == input$Tip &
                                   query$MES == var_mes)
      
      prof_mes_anterior <-  subset( query$PROFUNDIDAD , query$CATEGORIA == input$Cate & 
                                     query$FAMILIA == input$Fam & 
                                     query$OFICINA_VENTAS == input$Ofi &
                                     query$CANAL == "TOTAL" &  
                                     query$MARCA == input$Mar &
                                     query$VENDEDOR == input$Ven &
                                     query$TIPO_VENTAS == input$Tip &
                                     query$MES == var_mes_anterio)
      
      var_prof <- (prof_mes_actual/prof_mes_anterior-1)*100
      var_prof
      flexdashboard::gauge( round(var_prof,0), min = -50, max = 100, label = "%Var Prof", symbol = '%', gaugeSectors(
        success = c(1, 100), warning = c(0, 1), danger = c(-50, 0) 
      ))
      
    }
    )
    
    
    
#############################################################################################################################################################################3    
    output$table <- renderDataTable({
    
      
     VARIABLE<- query%>% select(MES,CC, CR, FREC, TP,AMPLITUD, PROFUNDIDAD)%>%
        filter(query$CATEGORIA == input$Cate & 
                 query$FAMILIA == input$Fam & 
                 query$OFICINA_VENTAS == input$Ofi &
                 query$CANAL == "TOTAL" &  
                 query$MARCA == input$Mar &
                 query$VENDEDOR == input$Ven &
                 query$TIPO_VENTAS == input$Tip & query$MES %in% c(var_mes, var_mes_anterio, var_mma))
     
     VARIABLE$CR<- VARIABLE$CR/VARIABLE$CC
     VARIABLE$FREC<- round(VARIABLE$FREC,1)
     VARIABLE$TP<- round(VARIABLE$TP,1)
     VARIABLE$AMPLITUD<- round(VARIABLE$AMPLITUD,1)
     VARIABLE$PROFUNDIDAD<- round(VARIABLE$PROFUNDIDAD,1)
     
        datatable(VARIABLE,
        options=list(
        pageLength = 3,
        lengthChange = FALSE,
        info= FALSE,
        bPaginate=FALSE,
        searching= FALSE
        ))%>%
        
        formatStyle( 0, target= 'row',color ='rgb(0, 0, 0)' , backgroundColor ='rgb(194, 226, 250)', fontWeight ='bold', lineHeight='50%')%>% 
          formatPercentage(c("CR"))
        
     
})
    
    
}