
source("functions.R")

navbarPage(   title="Fellow Ranking",id="maintabset",
              tabPanel( "Input Data",
                        sidebarLayout(
                          sidebarPanel(
                            p("Input a spreadsheet of scores for each fellow, with a column denoting interviewer."),
                            fileInput("datafile",label="Input Spreadsheet",
                                      accept=c('.csv','.txt','.xls','.xlsx')
                            ),
                            downloadLink("example_file",label="Download Example Input File"),
                            conditionalPanel("output.fileUploaded",
                                             selectInput("choice_fellowname",label="Select column with fellow name",choices=NULL),
                                             selectInput("choice_interviewer",label="Select column with interviewer name",choices=NULL),
                                             selectInput("choice_score",label="Select column with score",choices=NULL),
                                             actionButton("submit_data","Submit Data")
                            )
                          ),
                          mainPanel(dataTableOutput('fellowDT'))
                        )),
              tabPanel(title="Ranks",value="rankpanel",
                       sidebarLayout(
                         sidebarPanel(
                           p("You can download these results by clicking the save results button:"),
                           downloadButton('downloadResults_CSV','Save Results as CSV File'),
                           p(""),
                           shiny::HTML(renderMarkdown(text = "**The fellow's final ranking is determined by the `SUM_ranks` column.**")),
                           p("For more information, see the Info panel.")
                         ),
                         mainPanel(
                           dataTableOutput('rankingsDT')
                         )
                       )
              ),
              tabPanel(title="Plots",
                       sidebarLayout(
                         sidebarPanel(
                           p("Visualize the data. Hover your mouse over points to see the average rank data."),
                           selectInput("choice_xaxis",label="Select the X axis value.",
                                       choices=c()),
                           selectInput("choice_yaxis",label="Select the Y axis value.",
                                       choices=c())
                         ),
                         mainPanel(
                           #plotlyOutput("myscatter_interactive",height="600px")
                          # plotOutput("myscatter",
                          #            hover = hoverOpts(
                          #              id = "plot_hover"
                          #            )),
                          # verbatimTextOutput("hover_info")
                           uiOutput("ggvis_ui"),
                           ggvisOutput("ggvis")
                         )

                       )

              ),
              tabPanel("Info",
                       withMathJax(),
                       includeHTML("info.html")
              ),

              
              
              ## ==================================================================================== ##
              ## FOOTER
              ## ==================================================================================== ##              
              footer=p(hr(),p("ShinyApp created by ", strong("Jessica Minnier")," of ",align="center",width=4),
                       p(("Knight Cardiovascular Institute, Oregon Health & Science University"),align="center",width=4),
                       p(("Copyright (C) 2016, code licensed under GPLv3"),align="center",width=4),
                       p(("Code available on Github:"),
                         a("https://github.com/jminnier/fellowranking_shiny",
                           href="https://github.com/jminnier/fellowranking_shiny"),align="center",width=4)
              )
) #end navbarpage


