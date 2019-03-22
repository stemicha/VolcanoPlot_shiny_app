suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(metricsgraphics))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(plotly))



options(shiny.usecairo=TRUE)


shinyServer(function(input, output, session) {
    load("data/example.rda")
    example_highlight<-read.table(file.path("data/example_highlight.txt"),sep = "\t",header=F)
    
    data <-  reactive({
        inFile <- input$file1
        if(is.null(inFile)) {
            dataframe <- example          
        } else {

        dataframe <- read.csv(
            inFile$datapath, 
            sep=input$sep,
            quote='"',
            stringsAsFactors=FALSE
        )}
    })  
    
    data.high <-  reactive({
      inFile <- input$file2
      if(is.null(inFile)) {
        dataframe <- example_highlight 
      } else {
        
        dataframe <- read.csv(
          inFile$datapath, 
          sep=input$sep,
          quote='"',
          stringsAsFactors=FALSE,
          header = F
        )}
    })  
    

#     output$ggplot <- renderPlot({ 
    
    out <- reactive({ 
        genes <- data();
        
        if(input$p.scale){
          updateSliderInput(session, "lo",value = -log10(min(genes$P.Value,na.rm=T)))
        }
        
        up<-which(genes$logFC>=log2(as.numeric(input$vl)) & genes$P.Value<=c(as.numeric(input$hl)))
        down<-which(genes$logFC<=c(log2(1/as.numeric(input$vl))) & genes$P.Value<=c(as.numeric(input$hl)))
        #add vector for up down and not regulated
        genes$Significant<-rep("unchanged",nrow(genes))
        genes[up,"Significant"]<-rep("up",length(genes[up,"Significant"]))
        genes[down,"Significant"]<-rep("down",length(genes[down,"Significant"]))
        
        color.plot<-c(input$col1, "grey", input$col2)
        if(length(unique(genes$Significant))==3){
          color.plot.use<-color.plot
        }else{
          if(sum(unique(genes$Significant)=="up")==1){
            color.plot.use<-color.plot[2:3]
          }
          if(sum(unique(genes$Significant)=="down")==1){
            color.plot.use<-color.plot[1:2]
          }
        }
        
        
        if(class(data.high())=="data.frame"){
          genes$highlight<-rep("NA",nrow(genes))
          genes$highlight[unlist(sapply(data.high()[,1],function(x) which(as.character(x)==as.character(genes$ID))))]<-input$colhigh
          
          p <- ggplot(genes, aes(x = logFC, y = -log10(P.Value))) +
            geom_point(colour=genes$highlight, size = input$cex.size)+ #highlight genes
            geom_point(aes(color = Significant), size = 2) +
            scale_color_manual(values = color.plot.use) +
            geom_vline(xintercept = log2(as.numeric(input$vl)), color = "black",lwd=1.5,alpha=0.5,lty=3) + #add vertical line
            geom_vline(xintercept = log2(1/as.numeric(input$vl)), color = "black",lwd=1.5,alpha=0.5,lty=3) + #add vertical line
            geom_hline(yintercept = -log10(as.numeric(input$hl)), color = "black",lwd=1.5,alpha=0.5,lty=3) +  #add vertical line
            labs(x="log2(Fold-change)", y="-log10(P.Value)") + 
            scale_x_continuous(expression(log[2]~"(Fold-change)"), limits = input$lfcr) +
            scale_y_continuous(expression("-"~log[10]~"(p-value)"), limits = range(0,input$lo)) + theme_bw(base_size = input$theme.cex)
        }else{
          p <- ggplot(genes, aes(x = logFC, y = -log10(P.Value))) +
            geom_point(aes(color = Significant)) +
            scale_color_manual(values = color.plot.use) +
            geom_vline(xintercept = log2(as.numeric(input$vl)), color = "black",lwd=1.5,alpha=0.5,lty=3) + #add vertical line
            geom_vline(xintercept = log2(1/as.numeric(input$vl)), color = "black",lwd=1.5,alpha=0.5,lty=3) + #add vertical line
            geom_hline(yintercept = -log10(as.numeric(input$hl)), color = "black",lwd=1.5,alpha=0.5,lty=3) +  #add vertical line
            labs(x="log2(Fold-change)", y="-log10(P.Value)") + 
            scale_x_continuous(expression(log[2]~"(Fold-change)"), limits = input$lfcr) +
            scale_y_continuous(expression("-"~log[10]~"(p-value)"), limits = range(0,input$lo)) + theme_bw(base_size = input$theme.cex)
        }
        
        
        
        
          
        q <- p + geom_text_repel(
            data = subset(genes, Significant!="unchanged"),
            aes(label = ID),
            size = input$text.size,
            box.padding = unit(0.2, "lines"),
            point.padding = unit(0.2, "lines")
          )
        
        
        genes$"euclidean distance"<- sqrt((((-log10(genes$P.Value))^2)*(genes$logFC)^2))
        genes<-genes[order(genes$"euclidean distance",decreasing = T),]
        genes$rank<-seq(1,nrow(genes))
    
        
#         if(input$gene_names) print(q) else print(p)
        list(tab = genes[,-which(colnames(genes)=="highlight")],
        plot_out = if(input$gene_names){q}else{p}
        )
   
        
    })
    
    
    
    
    #output$scatter3d <- renderScatterD3({
      #dat <- data();
     # dat2 <- data.frame(FC=as.numeric(dat$logFC), pvalue=-log10(as.numeric(dat$P.Value)),ID=dat$ID)
     # scatterD3(x = dat2$FC, y = dat2$pvalue, lab = dat2$ID, labels_size = 9)
      # })
    

    #download plot
    output$ggplot <- renderPlot({
        out()$plot_out
    })
    #plot conversion numbers for cut off slider
   
    
    output$download_Data <- downloadHandler(
        filename = function() { 
            paste(gsub(".csv","", input$file1), "_selected_FC-",input$vl,"_pvalue_threshold-", input$hl,".csv", sep='') 
        },
        content = function(file) {
            dat <-  data.frame(out()$tab);
            write.csv(dat[as.numeric(dat$P.Value)<=as.numeric(input$hl) & abs(dat$logFC)>log2(as.numeric(input$vl))
                            ,], 
                      file, 
                      row.names=FALSE,
                      quote=FALSE)
        }
    )

    output$download_Data.full <- downloadHandler(
      filename = function() { 
        paste(gsub(".csv","", input$file1), "plot_data.csv", sep='') 
      },
      content = function(file) {
        dat <-  data.frame(out()$tab);
        write.csv(dat, 
                  file, 
                  row.names=FALSE,
                  quote=FALSE)
      }
    )
 
      output$download_Plot<- downloadHandler(
        filename <- function() {
          paste(gsub(".csv","", input$file1),"_FC-",input$vl,"_pvalue_threshold-", input$hl,".pdf",sep='')
        },
        content = function(file) {
          # ggsave(file, plot = ggplotInput(), device = png)
          pdf(file,width = 11,height = 11)
          print(out()$plot_out)
          #plot(1)
          dev.off()
        }
      )
      
      output$example<-downloadHandler(
        filename = function() { 
          paste("example_data_frame.txt", sep='') 
        },
        content = function(file) {
          dat <-  data.frame(example);
          write.table(dat, 
                      file, 
                      row.names=FALSE,
                      quote=FALSE,sep="\t")
        }
      )
      
      output$example.highlight<-downloadHandler(
        filename = function() { 
          paste("example_highlight_data_frame.txt", sep='') 
        },
        content = function(file) {
          dat <-  data.frame(example_highlight);
          write.table(dat, 
                      file,
                      col.names = F,
                      row.names=FALSE,
                      quote=FALSE,sep="\t")
        }
      )
  
      
    output$table_Out <- renderDataTable({
      dat <-  data.frame(out()$tab)
      datatable(dat, filter = 'top')
    })  
})


