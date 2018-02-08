library(shiny)
library(shinyAce)
library(gplots)

fluidPage(
    tags$header(
        tags$img(height = 122, width = 140, src = "polytech.png",align="left"),
        tags$img(height = 108, width = 150, src = "lyon1.png",align="right"),
        tags$h1('Analyse & Fouille de données',align="center"),
        tags$h2('Antoine ANGOULVANT - David LUONG',align="center"),
        tags$h4(tags$em('Master 1 MIAGE - UCBL 1 / Polytech Lyon'),align="center"),
        tags$hr()
    ),
    
    tags$body(
        mainPanel(
            tabsetPanel(
                tabPanel(title = 'ACP',
                    sidebarPanel(
						fileInput("fileACP", "Ajouter votre fichier CSV", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
    					tags$hr(),
                        checkboxInput("header", "Header", TRUE),
    					radioButtons("sep", "Séparateur", choices = c(Virgule=',',PointVirgule=';',Tab='\t', Espace=''), selected = ","),      
                            #PLAGE DE DONNEE SAISIE PAR UTILISATEUR POUR ACP 
                                  
                        checkboxInput("allFile", "Travailler avec tout le fichier", TRUE),
                        numericInput("idInv", "Coordonée Individu (Début valeur ligne)", 1, min = 1, max = 100),
                        numericInput("idInv2", "Coordonée Individu (Fin des valeur ligne:", 5, min = 1, max = 100),
                        numericInput("idActive", "Variable active (Début numéro colonne):", 1, min = 1, max = 100),
                        numericInput("idActive2", "variable active (Fin numéro colonne):", 5, min = 1, max = 100)
                    ),	           
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
    
                        h3('Graphe contribution dimensions'),
    		    		checkboxInput("valeurGD", "Valeur Individu", TRUE),
    			        plotOutput('grapheContriDim.out'),
                        br(),
                        tableOutput('contentsACP'),
                        htmlOutput('outputACP')
                    )
                ),
                    
                tabPanel(title = 'AFC',
                    sidebarPanel(
                        checkboxInput("Colonne", "Colonne", TRUE),
    	                fileInput("fileAFC", "Ajouter votre fichier CSV", accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                    ),
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
    
    					h3('Coordonée'),
    					 verbatimTextOutput('coordoPointL.out'),
    					br(),
    
    					h3('Graphe des points lignes'),
    					plotOutput('grapheCordoL.out'),
    					br(),
    
    					h3('Qualié de la représentation cosinus²'),
    					verbatimTextOutput('coordoCos.out'),
    					br(),
    
    					h3('Graphe² des points lignes en fonction de leur cos²'),
    					plotOutput('coordoCosPlot.out'),
    					br(),
    
    					h3('représentation des cosinus²'),
    					plotOutput('representationCos.out'), 
    					br(),
    
    					h3('Graphe des contributions '),
    					plotOutput('grapheContriCA.out'),
    					br(),
    
    					h3('Graphe des points colonnes '),
    					plotOutput('graphePointColCA.out'),
    					br(),
    					
    					h3('Biplot des contributions'),
    					plotOutput('biplotContribution.out'),
    					br(),
    					tableOutput('contentsAFC'),
    					htmlOutput('outputAFC') 
    	                             )
                                 ),
    
                tabPanel(title = 'CAH',
                    sidebarPanel(
                        checkboxInput("texteCAH", "Texte", TRUE),
                        fileInput("fileCAH", "Ajouter votre fichier CSV",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                        sliderInput("nbCluster",label = "Sélectionner le nombre de Cluster souhaité",value = 4, min = 2, max = 10)
                    ),
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
                        fileInput("fileK", "Ajouter votre fichier CSV",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),         		
    		            tags$h3("Sélectionner les paramètres"),
    		            checkboxInput(inputId = 'headerK', label = 'Header', value = FALSE),
    		            #checkboxInput(inputId = "stringAsFactorsK", "stringAsFactors", FALSE),
    		            radioButtons(inputId = 'sepK', label = 'Séparateur', choices = c(Virgule=',',PointVirgule=';',Tab='\t', Espace=''), selected = ','),
						sliderInput("clusters",label = "Sélectionner le nombre de Cluster souhaité",value = 4, min = 1, max = 10),
                        tags$hr(),         
                        uiOutput("list_item1"), #SelectInput interactif
    		            uiOutput("list_item2")  #Idem
                	),
                   	tags$hr(),
            
            		mainPanel(plotOutput('kmeanGraphe')) #Output graphe 
                )
            )
        )
    )
    
    #tags$footer(
    #)
)
  

  


