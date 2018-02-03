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

            data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
            res.pca <- PCA(data.active, graph = FALSE)
            eig.val <- get_eigenvalue(res.pca) #VALEUR PROPRE VARIANCE
            (fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))) #PLOT
    }       
        output$screePlot <- renderPlot({
        print(screePlo())
    })

    variance <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

            data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
            res.pca <- PCA(data.active, graph = FALSE)
            eig.val <- get_eigenvalue(res.pca) #VALEUR PROPRE VARIANCE
            (fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))) #PLOT scree plot
    }  
        output$var.out <- renderPrint({
        print(variance())
    })  
    variablePCA <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

            data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2] #AJOUTER VALEUR DES INPUTS 
            res.pca <- PCA(data.active, graph = FALSE)
            fviz_pca_var(res.pca, col.var = "black") #Graphe variable PCA
    }       
        output$varPCA.out <- renderPlot({
        print(variablePCA())
    })
    grapheRondCos <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

            data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2]
            res.pca <- PCA(data.active, graph = FALSE)
            corrplot(var$cos2, is.corr=FALSE) #Tableau Correlation cercle
    }       
        output$grapheCos2.out <- renderPlot({
        print(grapheRondCos())
    })
    cercleColorCos <- function(){ 
            req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")

            data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2]
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
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            
            data.active <- data[input$idInv:input$idInv2 , input$idActive:input$idActive2]
            res.pca <- PCA(data.active, graph = FALSE)
        fviz_pca_ind (res.pca, col.ind = "cos2", #Graphe des individus colorer
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                     repel = TRUE # Évite le chevauchement de texte
                     )
    }       
        output$grapheIndivi.out <- renderPlot({ #test
        print(grapheIndivi())
    })
    #CAH

     ward <- function(){ 
   		req(input$fileCAH)
        inFile <- input$fileCAH
        dat <- read.csv(inFile$datapath, header= TRUE,sep=";",dec=".", fileEncoding = "UTF-8-BOM")
            rowvar <- matrix(dat[,1])
            rownames(dat) <- rowvar
            datpca <- as.matrix(dat[,-1])
            colvar <- colnames(datpca)
            nr <- nrow(datpca)
            nc <- ncol(datpca)
            maxpc <- min(nr,nc)
            respca <- prcomp(datpca, scale=TRUE)
            pcloadings <- t(respca$sdev*t(respca$rotation))
            pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))
        
        dat$PCA1 <- pcscores[,1]
        dat$PCA2 <- pcscores[,2]
        
        z <- dat[, c("PCA1","PCA2")]
        z.d <- dist(z)^2  
        result <- hclust(z.d, method="ward.D2")  #Classification Ward 
        par(mar=c(1,6,3,1))
        plot(result, xlab="", sub="") 

        rect.hclust(result, input$nbCluster) #Groupe via nbCluster User
        groupes.result<- cutree(result, input$nbCluster)

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
