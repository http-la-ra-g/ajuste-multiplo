# Instalação dos pacotes necessários
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("fontawesome")) install.packages("fontawesome")
if (!require("DT")) install.packages("DT")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("dplyr")) install.packages("dplyr")

# Carregamento dos pacotes
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(fontawesome)
library(DT)
library(openxlsx)
library(dplyr)

# Interface do usuário (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  # CSS personalizado
  tags$head(
    tags$style(HTML("
      .titulo-app {
        padding: 15px;
        background-color: #2C3E50;
        color: white;
        margin-bottom: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .box-input {
        background-color: white;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        margin-bottom: 20px;
      }
      .btn-primary {
        background-color: #2C3E50;
        border-color: #2C3E50;
      }
      .btn-primary:hover {
        background-color: #34495E;
        border-color: #34495E;
      }
      .tab-panel {
        background-color: white;
        padding: 20px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .footer {
        position: fixed;
        bottom: 0;
        width: 100%;
        text-align: center;
        padding: 10px;
        background-color: #f8f9fa;
        border-top: 1px solid #ddd;
      }
    "))
  ),
  
  # Cabeçalho
  div(class = "titulo-app",
      h2("Ajuste de Altura", align = "center"),
      p("Ajuste múltiplo de altura para dados de inventário florestal", 
        align = "center", style = "opacity: 0.8;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "box-input",
          fileInput("arquivo", 
                   tags$span(icon("file-upload"), "Selecione o arquivo Excel (.xlsx)"),
                   accept = c(".xlsx")),
          
          # Campos para seleção das colunas
          uiOutput("coluna_selector"),
          
          fileInput("modelos_file", 
                   tags$span(icon("file-upload"), "Selecione o arquivo de modelos (.txt)"),
                   accept = c(".txt")),
          
          helpText("O arquivo .txt deve conter um modelo por linha no formato: ht~dap"),
          
          actionButton("processar", 
                      label = tags$span(icon("calculator"), "Processar Dados"),
                      class = "btn-calcular btn-primary",
                      style = "width: 100%; margin-top: 10px;")
      ),
      width = 3
    ),
    
    mainPanel(
      width = 9,
      div(class = "tab-panel",
          tabsetPanel(
            type = "pills",
            
            # Aba Sobre
            tabPanel(tags$span(icon("info-circle"), "Sobre"),
                    div(style = "padding: 20px;",
                        div(class = "well",
                            h3("O que é o Ajuste de Altura?"),
                            p("Este aplicativo realiza ajustes múltiplos de altura para dados de inventário florestal, 
                              utilizando diferentes modelos matemáticos para encontrar a melhor estimativa.")
                        ),
                        
                        div(class = "well",
                            h3("Como usar este aplicativo"),
                            tags$ol(
                              tags$li("Faça upload do arquivo Excel (.xlsx) com os dados do inventário"),
                              tags$li("Faça upload do arquivo de modelos (.txt)"),
                              tags$li("Clique em 'Processar Dados'"),
                              tags$li("Visualize os resultados na aba 'Resultados'")
                            )
                        ),
                        
                        div(class = "well",
                            h3("Formato dos Arquivos"),
                            h4("Arquivo Excel:"),
                            p("O arquivo deve conter as seguintes colunas:"),
                            tags$ol(
                              tags$li("parcela: identificador da parcela"),
                              tags$li("arvore: número da árvore"),
                              tags$li("dap: Diâmetro à Altura do Peito (cm)"),
                              tags$li("ht: Altura total (m) - use 0 para árvores que precisam de estimativa")
                            )
                        )
                    )
            ),
            
            # Aba Modelos
            tabPanel(tags$span(icon("chart-line"), "Modelos"),
                    div(style = "padding: 20px;",
                        div(class = "well",
                            h4("Modelos Carregados:"),
                            verbatimTextOutput("modelos_preview")
                        )
                    )
            ),
            
            # Aba Resultados
            tabPanel(tags$span(icon("table"), "Resultados"),
                    div(style = "padding: 20px;",
                        div(class = "well",
                            h4("Diagnóstico do Processamento"),
                            verbatimTextOutput("diagnostico"),
                            h4("Estatísticas do Ajuste"),
                            verbatimTextOutput("estatisticas_ajuste")
                        ),
                        div(class = "well",
                            h4("Resultados Detalhados"),
                            DTOutput("tabela_resultados")
                        )
                    )
            )
          )
      )
    )
  ),
  
  # Rodapé
  tags$footer(class = "footer",
             "Shiny desenvolvido por Lara Gualberto")
)

# Servidor
server <- function(input, output, session) {
  
  # Leitura dos dados
  dados_input <- reactive({
    req(input$arquivo)
    dados <- read.xlsx(input$arquivo$datapath)
    return(dados)
  })
  
  # Leitura dos modelos do arquivo txt
  modelos <- reactive({
    req(input$modelos_file)
    # Lê os modelos como vetor de texto
    modelos_txt <- readLines(input$modelos_file$datapath, warn = FALSE)
    # Remove espaços em branco e linhas vazias
    modelos_txt <- trimws(modelos_txt[nzchar(modelos_txt)])
    return(modelos_txt)
  })
  
  dados_processados <- eventReactive(input$processar, {
    req(input$arquivo, modelos())
    
    # Leitura dos dados
    dados_parcela <- read.xlsx(input$arquivo$datapath)
    
    # Renomear colunas para corresponder aos modelos
    names(dados_parcela) <- c("parcela", "arvore", "dap", "ht")
    
    parcelas<-unique(dados_parcela[,1])
    syx<-c()
    ajuste<-c()
    result<-c()
    
    for(i in 1:NROW(parcelas)){
      dados_ajuste<-subset(dados_parcela, dados_parcela[,1]==parcelas[i]&dados_parcela[,4]!=0)
      dados_sem_zero<-subset(dados_parcela, dados_parcela[,1]==parcelas[i]&dados_parcela[,4]==0)
      for(j in 1:NROW(modelos())){
        # Converte o texto do modelo em fórmula apenas quando necessário
        modelo_formula <- as.formula(modelos()[j])
        ajuste<-lm(modelo_formula, dados_ajuste)
        syx[j]<-summary(ajuste)[[6]]
      }
      modelo_melhor<-lm(as.formula(modelos()[which.min(syx)]), dados_ajuste)
      dados_sem_zero[,4]<-predict(modelo_melhor,dados_sem_zero)
      result<-rbind(result,rbind(dados_ajuste,dados_sem_zero))
    }
    
    return(result)
  })
  
  # Preview dos modelos carregados
  output$modelos_preview <- renderPrint({
    req(modelos())
    cat("Modelos carregados:\n")
    for(m in modelos()) {
      cat(m, "\n")
    }
  })
  
  # Atualizar a saída da tabela com mais informações
  output$tabela_resultados <- renderDT({
    req(dados_processados())
    datatable(dados_processados(),
             options = list(
               pageLength = 10,
               scrollX = TRUE,
               dom = 'Bfrtip',
               buttons = c('copy', 'csv', 'excel')
             ),
             caption = "Resultados do Ajuste de Altura") %>%
      formatRound(columns = c("dap", "ht"), digits = 2)
  })
  
  # Estatísticas do ajuste
  output$estatisticas_ajuste <- renderPrint({
    req(dados_processados())
    dados <- dados_processados()
    
    cat("Resumo do Ajuste:\n")
    cat("Número total de árvores:", nrow(dados), "\n")
    cat("Número de parcelas:", length(unique(dados$parcela)), "\n")
    
    # Adicionar informação por parcela
    cat("\nNúmero de árvores por parcela:\n")
    print(table(dados$parcela))
  })
  
  # Diagnóstico do processamento
  output$diagnostico <- renderPrint({
    req(dados_processados())
    dados <- dados_processados()
    
    cat("Diagnóstico do Processamento:\n")
    cat("\nParcelas processadas:\n")
    print(table(dados$parcela))
    
    cat("\nResumo das alturas por parcela:\n")
    print(tapply(dados$ht, dados$parcela, summary))
  })
}

# Executa o aplicativo
shinyApp(ui = ui, server = server) 