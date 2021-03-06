library(shiny)
library(factoextra)
library(gplots)
library(FactoMineR)
library(corrplot)

#Auteur Antoine ANGOULVANT 
#Auteur David LUONG 
#Etudiant en M1 MIAGE - Polytech Lyon 

shinyServer(function(input, output,session) {

    ##### ACP #####
    
    #Plot valeurs propre
    screePlo <- reactive({ 
        #Récupération du fichier
        req(input$fileACP) 
        inFile <- input$fileACP
        donnees <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {   #Travail sur tout le fichier
                    data.active <- donnees[,]
                } else 
                {   #Travail sur plage de données souhaité par user
                    data.active <- donnees[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

        res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
        eig.val <- get_eigenvalue(res.pca) #VALEUR PROPRE VARIANCE donnée par ACP
        (fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))) #PLOT
    })       
        output$screePlot.out <- renderPlot({
        screePlo()
    })
	#Tableau valeur de corréliation
    correlation <- reactive({
        #Récupération du fichier
        req(input$fileACP) 
        inFile <- input$fileACP
        donnees <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {
                    donnees.active <- donnees[,]
                } else 
                {	
                    donnees.active <- donnees[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

        res.pca <- PCA(donnees.active, graph = FALSE)  #Réceup des valeurs ACP 
        var <- get_pca_var(res.pca)
        print(var$cor)
     })
     output$correl.out <- renderPrint({
         correlation()
     })
     #Valeurs propre / Variance
    variance <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {
                    data.active <- data[,] #Tout le fichier
                } else 
                {	#Plage de données
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
            eig.val <- get_eigenvalue(res.pca) #VALEUR PROPRE VARIANCE
           # print(eig.val)
    }  
        output$var.out <- renderPrint({
        print(variance())
    }) 
    #Cercle de corrélation des variables 
    variablePCA <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
            fviz_pca_var(res.pca, col.var = "black") #Graphe variable PCA
    }       
        output$varPCA.out <- renderPlot({
        print(variablePCA())
    })
    #Graphique des variables par dimensions     
    grapheRondCos <- function(){
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            donnees <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {
                    donnees.active <- donnees[,]
                } else 
                {
                    donnees.active <- donnees[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(donnees.active, graph = FALSE) #Réceup des valeurs ACP 
            var <- get_pca_var(res.pca)
            
            corrplot(var$cos2, is.corr=FALSE) #Tableau Correlation cercle
     }    
        output$grapheCos2.out <- renderPlot({
        print(grapheRondCos())
    })
    #Coloration cercle en fonction de leurs contributions    
    cercleColorCos <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
            fviz_pca_var(res.pca, col.var = "contrib", #Graphe en cercle en fonction du cos2
            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
            )
    }       
        output$CercleCos2.out <- renderPlot({
        print(cercleColorCos())
    })
    #Graphe/Tableau montrant les valeurs les plus contributives par dimensions
    varcontriD <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
                #Vérification si User souhaite travailler sur tout le fichier
                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }
            res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
			var <- get_pca_var(res.pca)
			corrplot(var$contrib, is.corr=FALSE)  
    }       
        output$varcontriD.out <- renderPlot({
        print(varcontriD())
    })
    #Graphe d'individualité avec possibilité de montrer les valeurs sur le graphique
    grapheIndivi <- function(){ 
        req(input$fileACP) #ACP 
        inFile <- input$fileACP
        dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            #Vérification si User souhaite travailler sur tout le fichier
            if (input$valeurG == TRUE){
                rowvar <- matrix(dat[,1]) #Récupération des noms 
                rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
                    #Vérification si User souhaite travailler sur tout le fichier
                    if(input$allFile == TRUE) 
                    {
                        data.active <- dat[,]
                    } 
                    else 
                    {
                        data.active <- dat[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                    }
                res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
                fviz_pca_ind (res.pca, col.ind = "cos2", #Graphe des individus colorer
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                         repel = TRUE # Évite le chevauchement de texte
                         )
            } 
            else 
            {
                    if(input$allFile == TRUE) 
                    {
                        data.active <- dat[,]
                    } else 
                    {
                        data.active <- dat[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                    }
                res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
                fviz_pca_ind (res.pca, col.ind = "cos2", #Graphe des individus colorer
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                            repel = TRUE # Évite le chevauchement de texte
                            )
            }
    }       
        output$grapheIndivi.out <- renderPlot({ #test
        print(grapheIndivi()) 
    })

    #Graphe des contibutions sur dimensions 1 et 2
    grapheContriDim <- function(){ 
        req(input$fileACP) #ACP 
        inFile <- input$fileACP
        dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            if (input$valeurGD == TRUE){
                rowvar <- matrix(dat[,1]) #Récupération des noms 
                rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
                    #Vérification si User souhaite travailler sur tout le fichier
                    if(input$allFile == TRUE) 
                    {
                        data.active <- dat[,]
                    } 
                    else 
                    {
                        data.active <- dat[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                    }
                res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
                fviz_contrib(res.pca, choice = "ind", axes = 1:2)
            } 
            else
            {
                    if(input$allFile == TRUE) 
                    {
                        data.active <- dat[,]
                    } else 
                    {
                        data.active <- dat[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                    }
            res.pca <- PCA(data.active, graph = FALSE) #Réceup des valeurs ACP 
            fviz_contrib(res.pca, choice = "ind", axes = 1:2) #Graphe en fonction des valeurs acp
    	}      
    } 
        output$grapheContriDim.out <- renderPlot({ #test
        print(grapheContriDim())
    })

  
    ##### CAH #####
        
     ward <- function(){ 
        req(input$fileCAH)
        inFile <- input$fileCAH #Fichier
        dat <- read.csv(inFile$datapath, header= TRUE,sep=";",dec=".", fileEncoding = "UTF-8-BOM")
        nbClustV<- input$nbCluster #NbCluster définit par User sliderInput / Création d'une variable non obligatoire
        
            if (input$texteCAH == TRUE) #Permet d'afficher le nom des variables SI checkbox TRUE 
            {
                rowvar <- matrix(dat[,1]) #Récupération des noms 
                rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms
                dat.cr<-as.matrix(dat)  #Code pour réaliser clustering 
                dat.dist <-dist(dat.cr)
                result <-hclust(dat.dist, method="ward.D2")
                print(dat.cr)
                plot(result, xlab="", sub="") 

                rect.hclust(result, input$nbCluster) #Groupe via nbCluster User
                groupes.result<- cutree(result, input$nbCluster)
            } 
            else 
            {
                dat.cr<-as.matrix(dat)  #Code pour réaliser clustering 
                dat.dist <-dist(dat.cr)
                result <-hclust(dat.dist, method="ward.D2")
                print(dat.cr)
                plot(result, xlab="", sub="") 

                rect.hclust(result, input$nbCluster) #Groupe via nbCluster User
                groupes.result<- cutree(result, input$nbCluster) #Arbre cluster via nbCluster user
            }
        }
     output$cah.out <- renderPlot({
     	print(ward()) #Output graphe
     })

    ##### K-MEAN #####
       
    #Fonction qui gère le fichier  
    fileKm <- reactive({ 
  	    req(input$fileK) #Récupération du fichier
        file1 <- input$fileK
        if(is.null(file1)){return()} 
        read.csv(file=file1$datapath, input$headerK,input$sepK)	
    }) 
    #Affichage des noms des colonnes X en fonction du CSV
    output$list_item1<-renderUI({  
    	f<-fileKm()
    	selectInput("xcol","X Var",choices = as.list(colnames(f))) 
    })
    #Y colonne show en fonction du CSV
    output$list_item2<-renderUI({ 
    	f<-fileKm()
   		selectInput("ycol","Y var",choices = as.list(colnames(f)))
  	})
  	#Selection des X et Y en fonction de l'User
	selectedData <- reactive({ 
		f<-fileKm()
		#ft <- as.data.frame(t(f))
		f[, c(input$xcol, input$ycol)] 
		})
	#get nb cluster from user 
	 clusters <- reactive({ 
	    kmeans(selectedData(), input$clusters)
	  })
	 #Sortie du graphe
  	 output$kmeanGraphe <- renderPlot({  #Graphe KMEANS
        palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999")) #Palette de couleurs pour les clusters

        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(), #Kmean en fonction des data sélectionner et nbCluster User
             col = clusters()$cluster,
             pch = 20, cex = 3)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  	})
   #output$selected_varY <- renderText({ 
    #paste("You have selected this", input$ycol) #output ycol
   #})
   #output$selected_varX <- renderText({
    #paste("zoifozef", input$xcol) #output xol
    #})
     # output$selected_C <- renderText({
    #paste("Cluster", input$clusters) #output nbcluster souhaité
    #})


############## AFC ###############################

#Tableau de contingence
tab <- reactive({
   		req(input$fileAFC)
        inFile <- input$fileAFC #Récupération du fichier
        dat <- read.csv(inFile$datapath, header= TRUE,sep=";", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
        rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
        rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 

        #Convertion données en tab
        dt <- as.table(as.matrix(dat))
        #Graph
        balloonplot(t(dt), main = "Graphique du tableau de contigence", xlab ="", ylab = "",
            label = FALSE, show.margins = FALSE)
	})
        output$afc1.out <- renderPlot({
        tab()

    })
#Valeurs propre et Variance
varianceAFC <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            dat[1] <- NULL #
            res.ca <- CA (dat, graph = FALSE)

            eig.val <- get_eigenvalue (res.ca)
            print(eig.val)
    })
        output$varianceafc.out <- renderPrint({
        varianceAFC()
    })
#Grapgique des valeurs propres
screenPlotAFC <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            dat[1] <- NULL
            res.ca <- CA (dat, graph = FALSE)
            fviz_screeplot (res.ca, addlabels = TRUE, ylim = c(0, 50))
    })
        output$screenPlotAFC.out <- renderPlot({
        screenPlotAFC()
    })   
#Biplot des lignes et des colonnes     
biplotAFC <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL
            res.ca <- CA (dat, graph = FALSE)
            fviz_ca_biplot (res.ca, repel = TRUE)
    })
        output$biplotAFC.out <- renderPlot({
        biplotAFC()
    }) 
#Coordonées points lignes
coordoPointL <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
            print(row$coord)
    })
        output$coordoPointL.out <- renderPrint({
        coordoPointL()
    })
#Graphique des coordonées points lignes
grapheCordoL <- reactive({ #POINTS LIGNES
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            fviz_ca_row(res.ca, repel = TRUE)
    })
        output$grapheCordoL.out <- renderPlot({
        grapheCordoL()
    })  
#Coordonnées points colonnes
graphePointColCA <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
            fviz_ca_col (res.ca, col.col = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
    })
        output$graphePointColCA.out <- renderPlot({
        graphePointColCA()
    })  
#Graphique des points colonnes
graphePointColC <- reactive({ #POINTS COLONNES
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

			col <- get_ca_col(res.ca)
			print(col$coord)
    })
        output$coordoPointC.out <- renderPrint({
        graphePointColC()
    })
#Tableau valeurs des co²
coordoCos <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
            print(row$cos2)
    })
        output$coordoCos.out <- renderPrint({
        coordoCos()
    })
#Graphique des points lignes en fonction de leur cos²
coordoCosPlot <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            fviz_ca_row (res.ca, col.row = "cos2",
                         gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE)
    })
        output$coordoCosPlot.out <- renderPlot({
        coordoCosPlot()
    })  
#Représentation des cos² des points lignes par dimensions
representationCos <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
            corrplot(row$cos2, is.corr = FALSE)
    })
        output$representationCos.out <- renderPlot({
        representationCos()
    })  
#Valeurs les plus contrbutives par dimensions
grapheContriCA <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
            fviz_ca_row (res.ca, col.row = "contrib",
                         gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE)
    }) 
        output$grapheContriCA.out <- renderPlot({
        grapheContriCA()
    })  
#Graphique des valeurs par contributions
grapheContriLigneDim <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
			corrplot(row$contrib, is.corr=FALSE)    
    }) 
        output$grapheContriLigneDim.out <- renderPlot({
        grapheContriLigneDim()
    })  
#Biplot des contributions
biplotContribution <- reactive({
        req(input$fileAFC)
            inFile <- input$fileAFC #Récupération du fichier
            dat <- read.csv(inFile$datapath, header= TRUE,sep=";", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1]) #Récupération des noms en colonne 1  
            rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
            dat[1] <- NULL

            res.ca <- CA (dat, graph = FALSE)
            row <- get_ca_row(res.ca)
            fviz_ca_biplot (res.ca, map = "colgreen", arrow = c (TRUE, FALSE),
               repel = TRUE)
    })
        output$biplotContribution.out <- renderPlot({
        biplotContribution()
    })  
})
