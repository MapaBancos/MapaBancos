library(shiny)
library(leaflet)
library(shinydashboard)
library(anytime)
library(DT)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
  dashboardHeader(title = "Bancos Comunitários"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Seleção de Filtros", tabName = "my_selection"),
      menuItem("Mapa", tabName = "my_maps"),
      menuItem("Formulário", tabName = "my_forms"),
      menuItem("Saiba como utilizar", tabName = "my_manual")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "my_maps",
              ui <- bootstrapPage(
                tabsetPanel(
                  tabPanel(
                    title = "My Maps",
                    shiny::tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                    leafletOutput("map", width = "100%", height = "100%"),
                    absolutePanel(
                      bottom = "50%", right = 20,
                      sliderInput("year_range_slider", "Tempo", min = 1994, max = 2024,  # Substitua com os valores apropriados
                                  value = c(1994, 2024), step = 1, sep = ""
                      ),
                      selectInput("select_state_2", "Estados",choices = c("Todos","Acre","Alagoas","Amapá","Amazonas","Bahia","Ceará","Espírito Santo","Goiás","Maranhão","Mato Grosso","Mato Grosso do Sul","Minas Gerais","Pará","Paraíba","Paraná","Pernambuco","Piauí","Rio de Janeiro","Rio Grande do Norte","Rio Grande do Sul","Rondônia","Roraima","Santa Catarina","São Paulo","Sergipe","Tocantins") ),  # Lista de cores exemplo
                      checkboxInput("legend", "Show legend", TRUE),
                      DT::dataTableOutput("table")
                    )
                  )
                )
              )
      ),
      tabItem(tabName = "my_forms",
              titlePanel("Formulário"),
              p("Se você deseja adicionar alguma informação a respeito de um banco comunitário, preencha o formulário abaixo."),
              fluidRow(
                box(
                  title = "Formulário de Cadastro", # Título da box
                  status = "primary", # Estilo da box
                  solidHeader = TRUE, # Cabeçalho sólido
                  width = 12, # Largura total (em 12 colunas do grid do Shiny)
                  
                  # Conteúdo do formulário
                  textInput("name", label = "Remetente", placeholder = "Digite o seu nome completo"),
                  textInput("email", label = "Email", placeholder = "Digite o seu endereço de email"),
                  textInput("bank", label = "Banco Comunitário", placeholder = "Digite o nome do seu Banco"),
                  radioButtons(
                    "confirmação",
                    label = "O seu banco utiliza a plataforma E-Dinheiro?",
                    choices = c("Sim" = "sim", "Não" = "não"),
                    selected = NULL),
                  actionButton("submit_button", "Enviar", icon = icon("paper-plane"), class = "btn-primary")
                ) 
              )
      ),
      tabItem(tabName = "my_manual",
              titlePanel("Seções"),
              p("No site, há três seções no mapa. Se você estiver usando um computador, clique com o botão esquerdo do mouse em um dos três botões na barra à esquerda: Seleção de Filtros, Mapa e Saiba como utilizar."),
              p("Para ver a lista de bancos comunitários novamente, clique em 'Seleção de Filtros'. Para abrir o mapa com os bancos comunitários, clique em 'Mapa'."),
              p("O botão com três linhas faz a barra lateral à esquerda aparecer ou desaparecer. Se os botões das seções sumirem, clique no botão com três linhas para trazê-los de volta."),
              img(src="manual_section.png",height = 150,width=150,align="center",style="display: block; margin-left: auto; margin-right: auto;"),
              titlePanel("Mapa"),
              p("Na seção 'Mapa', você pode:",
                shiny::tags$ul(
                  shiny::tags$li("Segurar o botão esquerdo do mouse e arrastar para mover o mapa."),
                  shiny::tags$li("Usar os símbolos '+' e '-' para aproximar ou afastar."),
                  shiny::tags$li("Clicar nas setas verdes para ver mais informações sobre as localizações marcadas."),
                ),
              ),
              img(src="manual_map.png",height = 500,width=500,align="center",style="display: block; margin-left: auto; margin-right: auto;"),
              titlePanel("Seleção de filtros"),
              p("A seção 'Seleção de filtros' permite encontrar Bancos Comunitários por localização, estado e ano de fundação, além de fazer uma pesquisa direta. No campo 'Banco Comunitário', você pode escrever livremente o nome do banco que procura no espaço 'Digite o nome do Banco' no exemplo. No campo 'Estado', clique na seta para baixo e escolha um estado da lista (no exemplo, 'Rio de Janeiro'). Para o 'Ano de fundação', escreva um ano no campo à esquerda e outro no campo à direita para definir um intervalo, como '1993' a '2019', para mostrar bancos fundados nesse período. Para buscar outros intervalos, insira anos diferentes."),
              p("Além dos filtros convencionais, há uma barra de busca, marcada como 'Search', acima da tabela. Esse campo é livre, e o programa irá procurar por qualquer informação relacionada ao que você escrever, em qualquer um dos campos disponíveis. Ou seja, ele buscará qualquer banco que tenha esse dado."),
              p("O indicador de contagem acima da tabela serve para sinalizar quantos resultados são retornados para esta busca."),
              p("Abaixo da tabela é possível selecionar “Next” para ir para a próxima página de resultados ou “Previous” para voltar, caso haja mais de uma página."),
              p("Por fim, os botões “Copy”, “Excel” e “PDF” servem para exportar os dados da tabela, em texto, tabela ou imagem, respectivamente."),
              img(src="manual_selection.png",height = 500,width=500,style="display: block; margin-left: auto; margin-right: auto;")
      ),
      tabItem(tabName = "my_selection",
              fluidRow(
                box(textInput("bank", label = "Banco Comunitário", placeholder = "Digite o nome do Banco"), width = 4),
                box(selectInput("select_state_1", label = "Estado", choices = c(
                  "Todos",
                  "Acre",
                  "Alagoas",
                  "Amapá",
                  "Amazonas",
                  "Bahia",
                  "Ceará",
                  "Espírito Santo",
                  "Goiás",
                  "Maranhão",
                  "Mato Grosso",
                  "Mato Grosso do Sul",
                  "Minas Gerais",
                  "Pará",
                  "Paraíba",
                  "Paraná",
                  "Pernambuco",
                  "Piauí",
                  "Rio de Janeiro",
                  "Rio Grande do Norte",
                  "Rio Grande do Sul",
                  "Rondônia",
                  "Roraima",
                  "Santa Catarina",
                  "São Paulo",
                  "Sergipe",
                  "Tocantins"
                ),selected="Todos"), width = 4),
                box(dateRangeInput("daterange", label = "Ano de Fundação",format = "yyyy",startview = "year",language = "pt-BR", start = Sys.Date() - 365*30, end = Sys.Date() + 365,
                                   min = Sys.Date() - 365*50, max = Sys.Date() + 365), width = 4)
                # box(dateRangeInput("daterange", label = "Ano de Fundação",format = "yyyy",startview = "year",language = "pt-BR", start = Sys.Date() - 365*30, end = Sys.Date() + 365,
                #      min = Sys.Date() - 365*50, max = Sys.Date() + 365), width = 4)
              ),
              verbatimTextOutput("text1"),
              DT::dataTableOutput("table")
      )
    )
    
    
  )
)



server <- function(input, output, session) {

  my_table <- read.csv("https://raw.githubusercontent.com/GabrielPonte/CompSoc/main/bancos_python.csv")

  observeEvent(list(input$select_state_1, input$daterange, input$bank), {
    # Filtra os dados de acordo com os inputs
    selected_state <- input$select_state_1
    date_range <- input$daterange
    bank_name <- input$bank
    
    filtered_data <- my_table
    
    if (selected_state != "Todos") {
      filtered_data <- filtered_data[filtered_data$Estado == selected_state, ]
    }
    
    if (!is.null(date_range)) {
      start_year <- as.numeric(format(date_range[1], "%Y"))
      end_year <- as.numeric(format(date_range[2], "%Y"))
      filtered_data <- filtered_data[
        filtered_data$Ano.de.Fundação >= start_year &
          filtered_data$Ano.de.Fundação <= end_year, ]
    }
    
    if (bank_name != "") {
      filtered_data <- filtered_data[grepl(bank_name, filtered_data$Banco, ignore.case = TRUE), ]
    }
    
    # Atualiza o mapa com os dados filtrados
    if (nrow(filtered_data) > 0) {
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(
          lng = filtered_data$Longitude,
          lat = filtered_data$Latitude,
          popup = paste("<b>", "Banco ", filtered_data$Banco, "</b><br>")
        ) %>%
        flyTo(
          lng = mean(filtered_data$Longitude, na.rm = TRUE),
          lat = mean(filtered_data$Latitude, na.rm = TRUE),
          zoom = 6
        )
    } else {
      # Caso não haja dados, limpa os marcadores do mapa
      leafletProxy("map") %>%
        clearMarkers()
    }
  })
  
  observeEvent(input$select_state_2, {
    selected_state <- input$select_state_2
    
    if (selected_state == "Todos") {
      # Exibe todos os pontos
      leafletProxy("map") %>%
        clearMarkers() %>%
        addMarkers(
          lng = my_table$Longitude, lat = my_table$Latitude,
          popup = paste("<b>", "Banco ", my_table$Banco, "</b><br>")
        )
    } else {
      # Filtra os pontos para o estado selecionado
      filtered_data <- my_table[my_table$Estado == selected_state, ]
      
      if (nrow(filtered_data) > 0) {
        leafletProxy("map") %>%
          clearMarkers() %>%
          addMarkers(
            lng = filtered_data$Longitude, lat = filtered_data$Latitude,
            popup = paste("<b>", "Banco ", filtered_data$Banco, "</b><br>")
          ) %>%
          flyTo(
            lng = mean(filtered_data$Longitude, na.rm = TRUE),
            lat = mean(filtered_data$Latitude, na.rm = TRUE),
            zoom = 6
          )
      } else {
        # Caso não haja dados, limpa os marcadores do mapa
        leafletProxy("map") %>%
          clearMarkers()
      }
    }
  })
  
  observeEvent(input$select_state_1, {
    updateSelectInput(session, "select_state_2", selected = input$select_state_1)
  })
  
  # Observe changes in the second selectInput
  observeEvent(input$select_state_2, {
    updateSelectInput(session, "select_state_1", selected = input$select_state_2)
  })
  
  observeEvent(input$year_range_slider, {
    updateDateRangeInput(session, "daterange", 
                         start = as.Date(paste0(input$year_range_slider[1], "-01-01")),
                         end = as.Date(paste0(input$year_range_slider[2], "-12-31")))
    
  })
  
  
  observeEvent(input$daterange, {
    new_start <- as.numeric(format(input$daterange[1], "%Y"))
    new_end <- as.numeric(format(input$daterange[2], "%Y"))
    updateSliderInput(session, "year_range_slider",
                      value = c(new_start, new_end))
  })
  
  observeEvent(input$submit_button, {
    req(input$name, input$email, input$bank)
    
    new_entry <- data.frame(
      Nome = input$name,
      Email = input$email,
      Banco = input$bank,
      stringsAsFactors = FALSE
    )
    
    csv_path <- "dados.csv"
    if (file.exists(csv_path)) {
      write.table(new_entry, csv_path, append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      write.table(new_entry, csv_path, sep = ",", row.names = FALSE, col.names = TRUE)
    }
    
    # Operações Git
    system('git config --global user.name "MapaBancos"')
    system('git config --global user.email "mapacomunitarios@outlook.com"')
    
    system('git init')
    system('git branch -M main')  # Garantir que o branch seja "main"
    system('git remote add origin https://MapaBancos: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@github.com/MapaBancos/MapaBancos.git')
    
    # Caso o repositório já tenha mudanças remotas
    system('git pull origin main --allow-unrelated-histories')  # Para resolver divergências
    system('git add .')
    system('git commit -m "Atualizando dados do formulário"')
    system('git push origin main')  # Fazer push para o GitHub
    
    output$status <- renderText("Dados enviados com sucesso e salvos no GitHub!")
  })
  tot_rows <- nrow(my_table)
  col_ano <-my_table$Ano.de.Fundação
  col_banco <- my_table$Banco
  div_name <- reactive({input$bank})
  div_state <- reactive({input$select_state_2})
  div_year <- reactive({input$daterange})
  x <- rep(TRUE,tot_rows)
  output$table <- renderDataTable({
    # name_cols_table <- list()
    # for col_name in my_colnames
    #   name_cols_table <- append(name_cols_table,list())
    if (div_name() != ""){
      for (i in 1:tot_rows){
        if (startsWith(col_banco[i],div_name())==FALSE){
          x[i] <- FALSE
        }
      }
    }
    if (div_state() != "Todos"){
      x2 <- (my_table$Estado)==div_state()
      for (i in 1:tot_rows){
        if (x2[i] == FALSE){
          x[i] = FALSE
        }
      }
    }
    if ((div_year()[1] == Sys.Date() - 365*30 &&  div_year()[2] ==Sys.Date() + 365) == FALSE){
      init_year <- strtoi(substr(anydate(div_year()[1]),1,4))
      end_year <- strtoi(substr(anydate(div_year()[2]),1,4))
      for (i in 1:tot_rows){
        year_i = (col_ano[i])
        if (is.na(year_i)){
          x[i] = FALSE  
        }else if ((year_i>= init_year && year_i <= end_year) == FALSE){
          x[i] = FALSE
        }
      }
    }
    output$text1 <- renderText(paste("Contagem:",sum(x)))
    output$map <- renderLeaflet({
      zoom_level <- 10
      icon_size <- zoom_level * 2
      new_table <- my_table[x,]
      points <- cbind(new_table$Longitude,new_table$Latitude)
      my_popup <- paste("<b>","Banco ", new_table$Banco,"</b><br>")
      my_colnames <- gsub("."," ", colnames(new_table),fixed=TRUE)
      for (i in 4:ncol(new_table)){
        my_popup <- paste(my_popup,"<br>  - ",my_colnames[i],": ",new_table[[i]])
      }
      dynamicIcon <- makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-2x-green.png",
        iconWidth = icon_size, iconHeight = icon_size *1.5,
        iconAnchorX = icon_size / 2, iconAnchorY = icon_size * 1.5
      )
      leaflet(options = leafletOptions(minZoom = 5, maxZoom = 15)) %>%
        addTiles(group = "OSM (default)") %>%        
        addProviderTiles(providers$OpenStreetMap, options = providerTileOptions()) %>%
        addProviderTiles(providers$CartoDB.Positron, group = "Positron (minimal)") %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
        addLayersControl(
          baseGroups = c(
            "OSM (default)",
            "Positron (minimal)",
            "World Imagery (satellite)"
          ),
          options = layersControlOptions(collapsed = FALSE)
        ) %>%
        addMarkers(
          lng = my_table$Longitude, lat = my_table$Latitude,
          popup = paste("<b>", "Banco ", my_table$Banco, "</b><br>")
        )
    })
    
    datatable(my_table[x,], 
              extensions = 'Buttons',
              options = list(
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'excel', 'pdf'),
                pageLength=tot_rows, 
                lengthMenu=c(3,5,10)
              )
    ) %>% 
      formatRound(c(2,3),3) # casas decimais pra lat/long
  })
  
}
shinyApp(ui, server)