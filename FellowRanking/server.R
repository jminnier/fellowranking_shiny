
source("functions.R")


shinyServer(function(input, output,session) {
  
  
  observe({
    inFile <- input$datafile
    if(!is.null(inFile)) {
      print("updating column choices")
      inputdata <- inputDataReactive()
      tmpcols = colnames(inputdata)
      validate(need(length(tmpcols)>2, message="The number of columns must be at least 3."))
      updateSelectInput(session,"choice_fellowname",choices =tmpcols,selected = tmpcols[1])
      updateSelectInput(session,"choice_interviewer",choices =tmpcols, selected = tmpcols[2])
      updateSelectInput(session,"choice_score",choices =tmpcols, selected = tmpcols[3])
    }
    
  })
  
  inputDataReactive <- reactive({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    print("inputting data")
    validate(
      need(!is.null(input$datafile), message = "Please select a file")
    )
    inFile <- input$datafile
    fileext <- file_ext(inFile$name)
    print(inFile$name)
    print(fileext)
    if(fileext=="csv") {
      read_csv(inFile$datapath)
    }else if(fileext=="txt")
    {
      read_tsv(inFile$datapath)
    }else if(fileext%in%c("xls","xlsx")) {
      #workaround for read_excel package from
      #http://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    }
  })
  
  # check if a file has been uploaded and create output variable to report this
  output$fileUploaded <- reactive({
    print("fileUploaded")
    return(!is.null(inputDataReactive()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  
  output$fellowDT <- renderDataTable({
    tmp <- inputDataReactive()
    if(!is.null(tmp)) tmp
  })
  
  analyzeDataReactive <- eventReactive(input$submit_data,{
    
    inputdata <- inputDataReactive()
    print("analyzing data")
    validate(need(name_of_fellow_column%in%colnames(inputdata),"Select Column Names"))
    
    rank_fellow_data(inputdata,
                     name_of_fellow_column = input$choice_fellowname,
                     name_of_interviewer_column = input$choice_interviewer,
                     name_of_score_column = input$choice_score)
    
    
  })
  
  observe({
    if (input$submit_data > 0) {
      updateTabsetPanel(session, "maintabset", selected = "rankpanel")
    }
  })
  
  output$rankingsDT <- renderDataTable({
    tmp <- analyzeDataReactive()
    if(!is.null(tmp)) tmp
  })
  
  
  
  # download results as a csv file
  output$downloadResults_CSV <-
    downloadHandler(filename = paste0("Fellow_matching_results_",Sys.Date(),".csv"),
                    content = function(file) {
                      write_csv(analyzeDataReactive(), file)})
  
  output$example_file <- downloadHandler(filename="sample_data_cleaned.csv",
                                         content=function(file){
                                           file.copy("sample_data_cleaned.csv",file)
                                         })
  
  
  
  
  
  observeEvent(input$submit_data,{
    mydata = analyzeDataReactive()
    output$myscatter <- renderPlot({
      g=scatterplot_fun(mydata,myx=input$choice_xaxis,myy=input$choice_yaxis)
      g
    })
  })
  
  observe({
    mydata = analyzeDataReactive()
    scatterplot_fun_ggvis(mydata,myx=input$choice_xaxis,myy=input$choice_yaxis)%>%bind_shiny("ggvis", "ggvis_ui")
  })


  # output$hover_info <- renderPrint({
  #   cat("Hover your mouse over the points:")
  #   if(!is.null(input$plot_hover)){
  #     mydata = analyzeDataReactive()
  #     myx=input$choice_xaxis
  #     myy=input$choice_yaxis
  #     mydata$xaxis = mydata[,myx]
  #     mydata$yaxis = mydata[,myy]
  #   hover = input$plot_hover
  #   dist=sqrt((hover$x-mydata$xaxis)^2+(hover$y-mydata$yaxis)^2)
  #   if(min(dist) < 3)
  #     mydata[which.min(dist),]
  #   }
  #   
  # })
  
  

  
  
  
})
