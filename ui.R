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
                                        ".csv")),
                              #PLAGE DE DONNEE SAISIE PAR UTILISATEUR POUR ACP 
                              numericInput("idInv", "Coordonée Individu:", 2, min = 1, max = 100),
                              numericInput("idInv2", "Coordonée Individu:", 10, min = 1, max = 100),
                              numericInput("idActive", "Variable active:", 2, min = 1, max = 100),
                              numericInput("idActive2", "variable active:", 10, min = 1, max = 100)),
	                            tags$hr(),
                              h3('Variance / Valeur Propre'),
                              verbatimTextOutput('var.out'),
                              br(),
                              h3('Graphe Plot'),
                              plotOutput('screePlot'),
                              br(),
                              h3('Varialbe PCA'),
                              plotOutput('varPCA.out'),
                              br(),
                              h3('Graphe rond cos2'),
                              plotOutput('grapheCos2.out'),
                              br(),
                              h3('Coloration de cercle en fonction de Cos2'),
                              plotOutput('CercleCos2.out'),
                              br(),
                              h3('Graphe invidualité'),
                              plotOutput('grapheIndivi.out'),
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
                             verbatimTextOutput('tata'),
                             tableOutput('contentsAFC'),
                             htmlOutput('outputAFC') 
                             ),

                        tabPanel(title = 'CAH',
                        	sidebarPanel(
                        		checkboxInput("texteCAH", "Texte", TRUE),
                            	fileInput("fileCAH", "CAH",
                                       accept = c(
                                         "text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                             sliderInput("nbCluster",label = "Sélectionner le nombre de Cluster souhaité",
                              value = 4, min = 2, max = 10)),
                        	mainPanel(
                             h3('Diagramme Ward '),
                             plotOutput('cah.out',height = "1000px"), #width="1500px"),
                             tags$hr(),
                             tableOutput('contentsCAH'),
                             htmlOutput('outputCAH'))
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
                      	mainPanel(
							plotOutput('kmeanGraphe') #Output graphe 
                          )
                      	)
                     )))
  

  


