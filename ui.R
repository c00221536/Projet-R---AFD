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
                              checkboxInput("allFile", "Travailler avec tout le fichier", TRUE),
                              numericInput("idInv", "Coordonée Individu (Début valeur ligne)", 1, min = 1, max = 100),
                              numericInput("idInv2", "Coordonée Individu (Fin des valeur ligne:", 5, min = 1, max = 100),
                              numericInput("idActive", "Variable active (Début numéro colonne):", 1, min = 1, max = 100),
                              numericInput("idActive2", "variable active (Fin numéro colonne):", 5, min = 1, max = 100)),
	                            tags$hr(),
	                          mainPanel(
                              h3('Variance / Valeur Propre'),
                              verbatimTextOutput('var.out'),
                              br(),
                              h3('Corrélation'),
                              verbatimTextOutput('correl.out'),
                              br(),
                              h3('Graphe Plot'),
                              plotOutput('screePlot.out'),
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
							  checkboxInput("valeurG", "Valeur Individu", TRUE),
                              plotOutput('grapheIndivi.out'),
                              tableOutput('contentsACP'),
	                            htmlOutput('outputACP'))
                    	),
                        tabPanel(title = 'AFC',
                        	sidebarPanel(
                        		checkboxInput("Colonne", "Colonne", TRUE),
	                             fileInput("fileAFC", "AFC",
	                                       accept = c(
	                                         "text/csv",
	                                         "text/comma-separated-values,text/plain",
	                                         ".csv"))),
	                             tags$hr(),
	                             mainPanel(
		                             h3('Tableau des contigence'),
		                             plotOutput('afc1.out'),
		                             br(),
		                             h3('Variance / Val Propre'),
		                             verbatimTextOutput('varianceafc.out'),
		                             br(),
		                             h3('Screen plo'),
		                             plotOutput('screenPlotAFC.out'),
		                             br(),
		                             h3('Biplot lignes et colonens'),
		                             plotOutput('biplotAFC.out'),
		                             br(),
		                             tableOutput('contentsAFC'),
		                             htmlOutput('outputAFC') 
	                             )
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
                        		tags$hr(),
                        		mainPanel(
		                             h3('Diagramme Ward '),
		                             plotOutput('cah.out',height = "1000px"), #width="1500px"),
		                             tags$hr(),
		                             tableOutput('contentsCAH'),
		                             htmlOutput('outputCAH')
                             	)
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
                      	tags$hr(),
                      			mainPanel(
									plotOutput('kmeanGraphe') #Output graphe 
                         		)
                      	)
                    )
			)
	)
  

  


