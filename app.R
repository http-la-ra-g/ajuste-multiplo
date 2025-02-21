# Instalação dos pacotes necessários
if (!require("shiny")) install.packages("shiny")
if (!require("shinythemes")) install.packages("shinythemes")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("fontawesome")) install.packages("fontawesome")
if (!require("DT")) install.packages("DT")
if (!require("openxlsx")) install.packages("openxlsx")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

# Carregamento dos pacotes
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(fontawesome)
library(DT)
library(openxlsx)
library(htmlwidgets)

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
                            p("O arquivo deve conter exatamente 4 colunas nesta ordem:"),
                            tags$ol(
                              tags$li("parcela: identificador da parcela"),
                              tags$li("dap: Diâmetro à Altura do Peito (cm)"),
                              tags$li("ht: Altura total medida (m)"),
                              tags$li("ht_estimada: Coloque 0 para árvores que precisam de estimativa")
                            ),
                            p("Importante: Não inclua cabeçalho no arquivo Excel")
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
                            h4("Estatísticas do Ajuste"),
                            verbatimTextOutput("estatisticas_ajuste")
                        ),
                        div(class = "well",
                            downloadButton("download_resultados", "Baixar Resultados em Excel",
                                         class = "btn-primary",
                                         style = "margin-bottom: 15px;"),
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
  
  # Leitura dos modelos do arquivo txt
  modelos <- reactive({
    req(input$modelos_file)
    modelos_txt <- readLines(input$modelos_file$datapath)
    modelos_list <- lapply(modelos_txt, function(x) as.formula(x))
    return(modelos_list)
  })
  
  # Preview dos modelos carregados
  output$modelos_preview <- renderPrint({
    req(modelos())
    cat("Modelos carregados:\n")
    for(m in modelos()) {
      cat(deparse(m), "\n")
    }
  })
  
  dados_processados <- eventReactive(input$processar, {
    req(input$arquivo, modelos())
    
    # Leitura dos dados
    dados_parcela <- tryCatch({
      read.xlsx(input$arquivo$datapath)
    }, error = function(e) {
      stop("Erro ao ler o arquivo Excel. Verifique o formato do arquivo.")
    })
    
    # Verificar número de colunas
    if(ncol(dados_parcela) != 4) {
      stop("O arquivo deve ter exatamente 4 colunas: parcela, dap, ht e ht_estimada")
    }
    
    # Renomear as colunas
    names(dados_parcela) <- c("parcela", "dap", "ht", "ht_estimada")
    
    # Verificar tipos de dados
    if(!is.numeric(dados_parcela$dap) || !is.numeric(dados_parcela$ht)) {
      stop("As colunas DAP e HT devem conter valores numéricos")
    }
    
    parcelas <- unique(dados_parcela$parcela)
    
    # Inicialização dos vetores
    syx <- c()
    ajuste <- c()
    result <- c()
    
    # Adicionar coluna para armazenar qual modelo foi selecionado
    modelo_selecionado <- c()
    
    withProgress(message = 'Processando dados', value = 0, {
      for(i in 1:NROW(parcelas)){
        # Atualizar barra de progresso
        incProgress(1/NROW(parcelas))
        
        # Separar dados com e sem altura
        dados_ajuste <- subset(dados_parcela, parcela == parcelas[i] & ht_estimada != 0)
        dados_sem_zero <- subset(dados_parcela, parcela == parcelas[i] & ht_estimada == 0)
        
        # Criar ambiente com as variáveis necessárias
        dados_ajuste_env <- with(dados_ajuste, data.frame(
          ht = ht,
          dap = dap
        ))
        
        dados_sem_zero_env <- with(dados_sem_zero, data.frame(
          dap = dap
        ))
        
        # Testar todos os modelos
        syx <- numeric(length(modelos()))
        for(j in 1:length(modelos())){
          tryCatch({
            ajuste <- lm(modelos()[[j]], data = dados_ajuste_env)
            syx[j] <- summary(ajuste)[[6]]
          }, error = function(e) {
            syx[j] <- Inf
            warning(paste("Erro no ajuste do modelo", j, ":", e$message))
          })
        }
        
        # Selecionar e aplicar o melhor modelo
        melhor_indice <- which.min(syx)
        modelo_melhor <- lm(modelos()[[melhor_indice]], data = dados_ajuste_env)
        
        # Predizer alturas
        if(nrow(dados_sem_zero) > 0) {
          dados_sem_zero$ht_estimada <- predict(modelo_melhor, newdata = dados_sem_zero_env)
        }
        
        # Adicionar informação do modelo selecionado
        dados_ajuste$modelo_selecionado <- deparse(modelos()[[melhor_indice]])
        dados_sem_zero$modelo_selecionado <- deparse(modelos()[[melhor_indice]])
        
        # Combinar resultados
        result <- rbind(result, rbind(dados_ajuste, dados_sem_zero))
      }
    })
    
    return(result)
  })
  
  output$tabela_resultados <- renderDT({
    req(dados_processados())
    datatable(dados_processados(),
             options = list(
               pageLength = 10,
               scrollX = TRUE
             ),
             caption = "Resultados do Ajuste de Altura") %>%
      formatRound(columns = c(2,3,4), digits = 2)
  })
  
  # Adicionar output para estatísticas do ajuste
  output$estatisticas_ajuste <- renderPrint({
    req(dados_processados())
    dados <- dados_processados()
    
    cat("Resumo do Ajuste:\n")
    cat("Número total de árvores:", nrow(dados), "\n")
    cat("Número de parcelas:", length(unique(dados$parcela)), "\n")
    cat("\nModelos selecionados por parcela:\n")
    table(dados$modelo_selecionado)
  })
  
  output$download_resultados <- downloadHandler(
    filename = function() {
      paste("Resultados_Ajuste_Altura_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx", sep="")
    },
    content = function(file) {
      req(dados_processados())
      write.xlsx(dados_processados(), file)
    }
  )
}

# Executa o aplicativo
shinyApp(ui = ui, server = server) 