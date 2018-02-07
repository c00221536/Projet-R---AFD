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
                tabPanel(title = 'Analyse en Composante Principale',
                    sidebarPanel(
                        checkboxInput("header", "Header", TRUE),
    					radioButtons("sep", "Séparateur", choices = c(Virgule = ",", PointVirgule = ";",Tab = "\t"), selected = ","),
    					tags$hr(),
                        fileInput("fileACP", "Analyse en Composante Principale", accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                 
                            #PLAGE DE DONNEE SAISIE PAR UTILISATEUR POUR ACP 
                                  
                        checkboxInput("allFile", "Travailler avec tout le fichier", TRUE),
                        numericInput("idInv", "Coordonnée Individu (Début valeur ligne)", 1, min = 1, max = 100),
                        numericInput("idInv2", "Coordonnée Individu (Fin des valeur ligne:", 5, min = 1, max = 100),
                        numericInput("idActive", "Variable active (Début numéro colonne):", 1, min = 1, max = 100),
                        numericInput("idActive2", "Variable active (Fin numéro colonne):", 5, min = 1, max = 100)
                    ),	           
                    tags$hr(),
    	                
                    mainPanel(
                        h3('Valeurs propres / Variances'),
                        verbatimTextOutput('var.out'),
                        br(),
                            
                        h3('Corrélation'),
                        verbatimTextOutput('correl.out'),
                        br(),
                            
                        h3('Graphique des variables'),
                        plotOutput('screePlot.out'),
                        br(),
                            
                        h3('Cercle de corrélation'),
                        plotOutput('varPCA.out'),
                        br(),
                            
                        h3('Contributions des variables aux axes principaux'),
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
                    
                tabPanel(title = 'Analyse Factorielle des Correspondances',
                    sidebarPanel(
                        checkboxInput("Colonne", "Colonne", TRUE),
    	                fileInput("fileAFC", "Analyse Factorielle des Correspondances", accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                    ),
    	            tags$hr(),
    	                             
    	            mainPanel(
    					h3('Graphique du tableaux de contingence'),
    	                plotOutput('afc1.out'),
                        br(),
    
    					h3('Valeurs propres / Variances'),
    					verbatimTextOutput('varianceafc.out'),
    					br(),
    
    					h3('Graphique des valeurs propres'),
    					plotOutput('screenPlotAFC.out'),
    					br(),
    
    					h3('Biplot des lignes et des colonnes'),
    					plotOutput('biplotAFC.out'),
    					br(),
    
    					h3('Coordonnées des points lignes'),
    					 verbatimTextOutput('coordoPointL.out'),
    					br(),
    
    					h3('Graphique des points lignes'),
    					plotOutput('grapheCordoL.out'),
    					br(),
    
    					h3('Qualité de la représentation cos²'),
    					verbatimTextOutput('coordoCos.out'),
    					br(),
    
    					h3('Graphique des points lignes en fonction de leur cos²'),
    					plotOutput('coordoCosPlot.out'),
    					br(),
    
    					h3('Représentation des cos² des points lignes'),
    					plotOutput('representationCos.out'), 
    					br(),
    
    					h3('Graphique des contributions '),
    					plotOutput('grapheContriCA.out'),
    					br(),
    
    					h3('Graphique des points colonnes '),
    					plotOutput('graphePointColCA.out'),
    					br(),
    					
    					h3('Biplot des contributions'),
    					plotOutput('biplotContribution.out'),
    					br(),
    					tableOutput('contentsAFC'),
    					htmlOutput('outputAFC') 
    	                             )
                                 ),
    
                tabPanel(title = 'Classification Ascendante Hiérarchique',
                    sidebarPanel(
                        checkboxInput("texteCAH", "Texte", TRUE),
                        fileInput("fileCAH", "Classification Ascendante Hiérarchique",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
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
                            
                tabPanel(title = 'K-Mean',
                    sidebarPanel(
                        fileInput("fileK", "K-Mean",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),         		
    		            tags$h3("Sélectionner les paramètres"),
    		            checkboxInput(inputId = 'headerK', label = 'Header', value = FALSE),
    		            #checkboxInput(inputId = "stringAsFactorsK", "stringAsFactors", FALSE),
    		            radioButtons(inputId = 'sepK', label = 'Séparateur', choices = c(Virgule=',',PointVirgule=';',Tab='\t', Espace=''), selected = ','),
    		            numericInput('clusters', 'Cluster count', 3,min = 1, max = 9),
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
  

  


