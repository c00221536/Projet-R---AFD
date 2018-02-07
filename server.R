library(shiny)
library(shinyAce)
library(psych)
library(factoextra)
library(gplots)
library(FactoMineR)
library(corrplot)

shinyServer(function(input, output,session) {

    screePlo <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE)
            eig.val <- get_eigenvalue(res.pca) #VALEUR PROPRE VARIANCE
            (fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))) #PLOT
    }       
        output$screePlot.out <- renderPlot({
        screePlo()
    })
        correlation <- reactive({
        req(input$fileACP) #ACP 
        inFile <- input$fileACP
        donnees <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

                if(input$allFile == TRUE) 
                {
                    donnees.active <- donnees[,]
                } else 
                {
                    donnees.active <- donnees[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

        res.pca <- PCA(donnees.active, graph = FALSE)
        
        var <- get_pca_var(res.pca)
             
        print(var$cor)
     })
     output$correl.out <- renderPrint({
         correlation()
     })
    variance <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE)
            eig.val <- get_eigenvalue(res.pca) #VALEUR PROPRE VARIANCE
           # print(eig.val)
    }  
        output$var.out <- renderPrint({
        print(variance())
    })  
    variablePCA <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE)
            fviz_pca_var(res.pca, col.var = "black") #Graphe variable PCA
    }       
        output$varPCA.out <- renderPlot({
        print(variablePCA())
    })
    grapheRondCos <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            donnees <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

                if(input$allFile == TRUE) 
                {
                    donnees.active <- donnees[,]
                } else 
                {
                    donnees.active <- donnees[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(donnees.active, graph = FALSE)
            
            var <- get_pca_var(res.pca)
            
            corrplot(var$cos2, is.corr=FALSE) #Tableau Correlation cercle
     }    
        output$grapheCos2.out <- renderPlot({
        print(grapheRondCos())
    })
    cercleColorCos <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

                if(input$allFile == TRUE) 
                {
                    data.active <- data[,]
                } else 
                {
                    data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                }

            res.pca <- PCA(data.active, graph = FALSE)
            fviz_pca_var(res.pca, col.var = "contrib", #Graphe en cercle en fonction du cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
             )
    }       
        output$CercleCos2.out <- renderPlot({
        print(cercleColorCos())
    })
    grapheIndivi <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

            if (input$valeurG == TRUE){
                rowvar <- matrix(dat[,1]) #Récupération des noms 
                rownames(dat) <- rowvar #Remplacement des ID créer par R par les noms 
                    if(input$allFile == TRUE) 
                    {
                        data.active <- dat[,]
                    } 
                    else 
                    {
                        data.active <- dat[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
                    }
                print(data.active)
                res.pca <- PCA(data.active, graph = FALSE)
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
                print(data.active)
                res.pca <- PCA(data.active, graph = FALSE)
                fviz_pca_ind (res.pca, col.ind = "cos2", #Graphe des individus colorer
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                            repel = TRUE # Évite le chevauchement de texte
                            )
            }
    }       
        output$grapheIndivi.out <- renderPlot({ #test
        print(grapheIndivi())
    })
    #CAH

     ward <- function(){ 
   		req(input$fileCAH)
        inFile <- input$fileCAH
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
                groupes.result<- cutree(result, input$nbCluster)
            }
        }
        output$cah.out <- renderPlot({
        print(ward()) #Output graphe
        })

#KMEAN
  toto <- reactive({ 
  	req(input$fileK)# file
    file1 <- input$fileK
    if(is.null(file1)){return()} 
    read.csv(file=file1$datapath, input$headerK,input$sepK)	
  })

  output$list_item1<-renderUI({  #Affichage des noms des colonnes X en fonction du CSV
    f<-toto()
    yolo2 <-selectInput("xcol","X Var",choices = as.list(colnames(f)))
  })

    output$list_item2<-renderUI({ #Y colonne show en fonction du CSV
    	f<-toto()
   	yolo1 <-selectInput("ycol","Y var",choices = as.list(colnames(f)))
  })

selectedData <- reactive({ #Selection des X et Y en fonction de l'User
	f<-toto()
	#ft <- as.data.frame(t(f))
	f[, c(input$xcol, input$ycol)] 
	})

  clusters <- reactive({ #get nb cluster from user 
    kmeans(selectedData(), input$clusters)
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

  	output$kmeanGraphe <- renderPlot({  #Graphe KMEANS
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

#AFC
output$tata <- reactive({
   		req(input$fileAFC)
        inFile <- input$fileAFC
        dat <- read.csv(inFile$datapath, header= TRUE,sep=";",dec=".", fileEncoding = "UTF-8-BOM")
        #Convertion données en tab
        dt <- as.table(as.matrix(dat))
        #Graph
        t1 <- balloonplot(t (dt), main = "housetasks", xlab = "", ylab = "",
            label = FALSE, show.margins = FALSE)
	})

})
