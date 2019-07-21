#Shiny App para monitoramento da normalidade de variaveis em um dataset.
#Devido ao encoding, os comentarios em pt-br nao possuem acentuacao e caracteres especiais.

#chamando pacotes necessarios
require(shiny)
#require(data.table)
require(plotly)
require(ggplot2)

# Define the UI
ui <- fluidPage(
  titlePanel("Dashboard"),
  fluidRow(
    #Janela de Inputs
    column(2,wellPanel(
      #h4("Menu"),
      #Seleciona arquivo
      fileInput("file",label=h4("Select file:")),
      #hr(),
      selectInput("Var1",
                  label = h4("Select the variable:"),""),
      #Caixa de selecao
      hr(),
      checkboxInput('compG', 'Variation in Standard Deviation of mean:', FALSE),
      conditionalPanel(condition = "input.compG == true",
                       sliderInput(inputId = "bw_adjust",
                                   label=tags$span(style = paste("color:gray;","font-size: 12px;",
                                                                 "font-style: bold",sep=""),"Confidence Level:"),
                                   min = 0.1, max = 3, value = 2, step = 0.1))
    )),
    #Grafico
    column(5,
           plotlyOutput("plot1")
    ),
    #Grafico
    column(5,
           plotlyOutput("plot2")
    )
  )
)

# Define the server code
server <- function(input, output, session) {
  
  #Verifica upload de arquivo
  filedata <- reactive({
    infile <- input$file
    if (is.null(infile)) {
      return(NULL)
    }
    read.csv(infile$datapath,header=T,sep=",")
  })
  
  #Realiza prep do dataframe
  dataFilter<-reactive({
    if (is.null(input$file)){
      return(NULL)
    }else{
      df=filedata()
      df$index=seq(1,nrow(df))
      
      cond1 <- sapply(df, function(col) length(unique(col)) > 10)
      cond2 <- sapply(df, function(col) class(col)=='numeric' | class(col)=='integer')
      mask <- (cond1 & cond2)
      df=df[, cond1, drop = FALSE]
      return(df)
    }
  })
  
  #Extrai as variaveis candidatas a analise
  observe({
    updateSelectInput(
      session,
      "Var1",
      choices=head(names(dataFilter()),-1))
  })
  
  #Filtra os dados de acordo com a selecao para os plots
  myData <- reactive({
    df=dataFilter()
    dados=df[,c(which(names(df) == input$Var1),which(names(df) == "index"))]
  })
  
  output$plot1 <- renderPlotly({
    dados=myData()
    if(input$Var1 %in% names(dados)){
      ggplotly(
        ggplot(dados, aes(x=dados[,1]))+
          geom_histogram(color="darkblue", fill="lightblue",bins=15)+
          stat_function(fun = dnorm, 
                        args = list(mean = mean(dados[,1]), sd = sd(dados[,1])), 
                        lwd = 1,col = 'red')+
          theme_minimal()+
          ggtitle(paste("Histogram \nP-value Normality Test: ",
                        round(shapiro.test(dados[,1])$p.value,4),sep="")) +
          theme(plot.title = element_text(hjust = 0.5))+
          labs(x = input$Var1)+labs(y = "Frequency")
      )}
    else{
      plot.new()
    }
  })
  
  output$plot2=renderPlotly({
    dados=myData()
    
    if(input$Var1 %in% names(dados) & input$compG!=TRUE){
      ggplotly(
        ggplot(dados, aes(x=dados$index,y=dados[,1], group=1))+ 
          geom_line(color='darkblue')+geom_point(color='darkblue')+
          theme_minimal()+theme(legend.position='none')+
          ggtitle("Line Plot") +
          theme(plot.title = element_text(hjust = 0.5))+
          labs(x = "Index")+labs(y = input$Var1)
      )}
    else{if(input$compG){
      media=mean(dados[,1]);desvP=sd(dados[,1]);z=input$bw_adjust
      lic=media-z*desvP;lsc=media+z*desvP
      ggplotly(
        ggplot(dados, aes(x=dados$index,y=dados[,1], group=1))+ 
          geom_line(color='darkblue')+geom_point(color='darkblue')+
          theme_minimal()+theme(legend.position='none')+
          geom_hline(yintercept=lic, linetype="dashed", color = "red")+
          geom_hline(yintercept=media, linetype="dashed", color = "yellow")+
          geom_hline(yintercept=lsc, linetype="dashed", color = "red")+
          ggtitle(paste("Line Plot \nDispersion (",input$bw_adjust," SD for the mean of ",input$Var1,")",sep='')) +
          theme(plot.title = element_text(hjust = 0.5))+
          labs(x = "Index")+labs(y = input$Var1)
      )}
    else{
      plot.new()
    }}
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)