library(shiny)
library(shinyAce)
library(gplots)



fluidPage(
      headerPanel('Analyse & Fouille de données'),
      h2('Antoine ANGOULVANT - David LUONG'),
      h4('Master 1 MIAGE - UCBL 1 / Polytech Lyon'),
      br(),
            mainPanel(
                   tabsetPanel(
                        tabPanel(title = 'ACP',
                        	sidebarPanel(
                        	   	checkboxInput("header", "Header", TRUE),
								radioButtons("sep", "Séparateur",
								            choices = c(Virgule = ",",
								                       PointVirgule = ";",
								                       Tab = "\t"),
								                       selected = ","),
								tags$hr(),
                             	fileInput("fileACP", "ACP",
                                       accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv"))),
	                            h3("Statistique"), #Différente sortie ACP
	                           	verbatimTextOutput("textarea.out"),
	                            h3("Correlation"),
	                            verbatimTextOutput("correl.out"),
	                            br(),
	                            h3("Résultat ACP"),
	                            verbatimTextOutput("pcaresult.out"),
	                            br(),
	                            plotOutput("contentsGraph1"),
	                            br(),
	                            plotOutput("contentGraph2"),
	                            br(),
	                            h4("Biplot"),
	                            plotOutput("contenGraph3"),
	                            h4("Diagramme Clustering WARD"),
	                            plotOutput('pcPlot5'),
	                            tags$hr(),
	                            tableOutput('contentsACP'),
	                            htmlOutput('outputACP')
                    	),

                        tabPanel(title = 'AFC',
                             fileInput("fileAFC", "AFC",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                             tags$hr(),
                             tableOutput('contentsAFC'),
                             htmlOutput('outputAFC') 
                             ),

                        tabPanel(title = 'CAH',
                        	sidebarPanel(
                            	fileInput("fileCAH", "CAH",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                             sliderInput("nbCluster",label = "Sélectionner le nombre de Cluster souhaité",
                              value = 4, min = 2, max = 10)),
                             h3('Diagramme Ward '),
                             plotOutput('cah.out',height = "1000px"), #width="1500px"),
                             tags$hr(),
                             tableOutput('contentsCAH'),
                             htmlOutput('outputCAH')
                        ),
                        tabPanel(title = 'Kmean',
                      		sidebarPanel(
                           			fileInput("fileK", "K",
                                       accept = c(
                                        "text/csv",
                                        "text/comma-separated-values,text/plain",
                                        ".csv")),
                              		
		                              tags$h3("Sélectionner les paramètres"),
		                              checkboxInput(inputId = 'headerK', label = 'Header', value = FALSE),
		                              #checkboxInput(inputId = "stringAsFactorsK", "stringAsFactors", FALSE),
		                              radioButtons(inputId = 'sepK', label = 'Séparateur', choices = c(Virgule=',',PointVirgule=';',Tab='\t', Espace=''), selected = ','),
		                              numericInput('clusters', 'Cluster count', 3,
		                                           min = 1, max = 9),
		                    tags$hr(),         
	                        uiOutput("list_item1"), #SelectInput interactif
	                        uiOutput("list_item2")  #Idem
                        ),
                        plotOutput('kmeanGraphe') #Output graphe 
                          ) 
                     )))
  

  


