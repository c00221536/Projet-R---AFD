library(shiny)
library(shinyAce)
library(psych)
library(gplots)

shinyServer(function(input, output,session) {

    bs <- reactive({ 
    		req(input$fileACP) #ACP 
            inFile <- input$fileACP
            data <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            describe(data)[2:13]
    })
        output$textarea.out <- renderPrint({
        bs()
    })

    correl <- reactive({
    		req(input$fileACP)
            inFile <- input$fileACP
            x <- read.csv(inFile$datapath, header= input$header, sep=input$sep,dec=".",fileEncoding = "UTF-8-BOM")
            x <- x[-1, -1]
            round(cor(cbind(x), use = "complete"),3)
    })
        output$correl.out <- renderPrint({
        correl()
    })
    
    pcaresult <- reactive({
    		req(input$fileACP)
            inFile <- input$fileACP
            dat <- read.csv(inFile$datapath, header= input$header, sep=input$sep,fileEncoding = "UTF-8-BOM")
        rowvar <- matrix(dat[,1])
        rownames(dat) <- rowvar
        datpca <- as.matrix(dat[,-1])
        colvar <- colnames(datpca)
        nr <- nrow(datpca)
        nc <- ncol(datpca)
        maxpc <- min(nr,nc)
        respca <- prcomp(datpca, scale=TRUE)
        info <- summary(respca)
        eigen <- info[[1]]^2
        newinfo <- rbind("Eigen values"=eigen, info$importance)
        cat("Importance of components:", "\n")
        print(newinfo)
            
        pcloadings <- t(respca$sdev*t(respca$rotation))
        cat("\n", "Principal component loadings:", "\n")
        print(pcloadings)
            
        pcscores<-scale(datpca)%*%respca$rotation*sqrt(nr/(nr-1))
        cat("\n", "Principal component scores:", "\n")
        print(pcscores)
    })
            output$pcaresult.out <- renderPrint({
            pcaresult()
    })

     graph1 <- function() {
     		req(input$fileACP)
            inFile <- input$fileACP
            dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            #dat <- read.csv(text=input$text, sep="\t")
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
        

        plot(pcloadings[,1:2], type="n", xlab="PC 1", ylab="PC 2", cex.axis=0.8, cex.lab=0.8)
        text(pcloadings[,1:2], labels=colvar, cex=0.9, adj=c(0.25,1.5))
        abline(h=0,lty="dotted")
        abline(v=0,lty="dotted")
        title(main="Principal Component Analysis: PC Loadings")
    }
    
    output$contentsGraph1 <- renderPlot({
        print(graph1())
    })
    
    
    
    graph2 <- function() {
    		req(input$fileACP)
            inFile <- input$fileACP
            dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            #dat <- read.csv(text=input$text, sep="\t")
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
        

        PCSpc1min <- min(pcscores[,1:2][,1])
        PCSpc1min <- PCSpc1min-(abs(PCSpc1min-PCSpc1min*1.25))
        PCSpc1max <- max(pcscores[,1:2][,1])
        PCSpc1max <- PCSpc1max*1.25

        PCSpc2min <- min(pcscores[,1:2][,2])
        PCSpc2min <- PCSpc2min-(abs(PCSpc2min-PCSpc2min*1.25))
        PCSpc2max <- max(pcscores[,1:2][,2])
        PCSpc2max <- PCSpc2max*1.25
        
        plot(pcscores[,1:2], xlab="PC 1", ylab="PC 2", type="n", xlim=c(PCSpc1min, PCSpc1max), ylim=c(PCSpc2min, PCSpc2max), cex.axis=0.8,cex.lab=0.8)

        text(pcscores[,1:2], labels=rowvar, cex=0.9, adj=c(0.25,1.5))
        abline(h=0,lty="dotted")
        abline(v=0,lty="dotted")
        title(main="Principal Component Analysis: PC Scores")
    }
    
    
    output$contentGraph2 <- renderPlot({
        print(graph2())
    })
    
    
    
    graph3 <- function() {
    		req(input$fileACP)
            inFile <- input$fileACP
            dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
            #dat <- read.csv(text=input$text, sep="\t")
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
        
        pc1min <- min(c(pcloadings[,1:2][,1], pcscores[,1:2][,1]))
        pc1min <- pc1min*1.25
        pc1max <- max(c(pcloadings[,1:2][,1], pcscores[,1:2][,1]))
        pc1max <- pc1max*1.25

        pc2min <- min(c(pcloadings[,1:2][,2], pcscores[,1:2][,2]))
        pc2min <- pc2min*1.25
        pc2max <- max(c(pcloadings[,1:2][,2], pcscores[,1:2][,2]))
        pc2max <- pc2max*1.25
        

        biplot(pcscores[,1:2], pcloadings[,1:2], var.axes = F, xlim=c(pc1min, pc1max), ylim=c(pc2min, pc2max))
        abline(v=0, lty=3) 
        abline(h=0, lty=3) 
    }
    
    
    output$contenGraph3 <- renderPlot({
        print(graph3())
    })

    makePlot5 <- function() {
    		req(input$fileACP)
            inFile <- input$fileACP
            dat <- read.csv(inFile$datapath, header= input$header,sep=input$sep, fileEncoding = "UTF-8-BOM")
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
        result <- hclust(z.d, method="ward.D2") 
        par(mar=c(1,6,3,1))
        plot(result, xlab="", sub="") 
    }
    
    
    output$pcPlot5 <- renderPlot({
        print(makePlot5())
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

})
