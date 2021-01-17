library(shinydashboard)
library(shinyWidgets)
library(DT)
#library(latticeExtra)


shinyApp(
ui <- dashboardPage(skin="black",
  dashboardHeader(title = "RNAseq"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PCA", tabName = "dopca", icon = icon("arrow-alt-circle-right")),
      menuItem("MAplot", tabName = "doMAplot", icon = icon("arrow-alt-circle-right")),
      menuItem("Heatmap", tabName = "doHeatmap", icon = icon("arrow-alt-circle-right"))
    )
  ),
  dashboardBody(
    tabItems(
    # dopca
     tabItem(tabName = "dopca",
              h1("Principal Componnent Analysis"),
fluidPage(
    fluidRow(
                column(6,
fileInput("datafile", "Choose file")))),

fluidPage(
    fluidRow(
                column(6, 
# Input: Checkbox if file has header
              radioButtons(inputId = "header", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              
              # Input: Select separator ----
              radioButtons(inputId = "sep", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ";", inline=T),


	     # Input: Select quotes ----
              radioButtons(inputId = "quote", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),

	     # Input: Select dec ----
              radioButtons(inputId = "dec", 
                           label = "Decimal",
                           choices = c(Comma = ",",
                                       "Dot" = "."),
                           selected = ",", inline=T)))),

  
fluidPage(
    fluidRow(
                column(6,
  	 h3("Loaded file"),
	      uiOutput("waiting1"),
              dataTableOutput(outputId = "view")),
    column(6,
  	 h3("Calculated Principal Component"),
              dataTableOutput(outputId = "PCArotation")))),

              

fluidPage(
    fluidRow(
                column(6,
			uiOutput("toColpcax")),
		column(6,
			uiOutput("toColpcay")))),

fluidPage(
    fluidRow(  
		h2("Graphic : Principal Component"))),

fluidPage(
    fluidRow( 
		column(1),
			column(1),
				column(6,
					downloadButton("downloadplotAvecR","Download Plot")))),



 fluidPage(
    fluidRow( 
		column(1,

				numericInput("mar1", "Bottom",
                                   min = 0, max = 25,
                                   value = 5,step = 0.5),
				numericInput("mar2", "Left",
                                   min = 0, max = 25,
                                   value = 5,step = 0.5),
				numericInput("mar3", "Top",
                                   min = 0, max = 25,
                                   value = 4,step = 0.5),
				numericInput("mar4", "Right",
                                   min = 0, max = 25,
                                   value = 2,step = 0.5)),
		column(1,
				numericInput("mgp1", "Label axis spacing",
                                   min = 0, max = 25,
                                   value = 3,step = 0.5),
				numericInput("mgp2", "Value axis spacing",
                                   min = 0, max = 25,
                                   value = 1,step = 0.5),
				numericInput("mgp3", "Axis spacing",
                                   min = 0, max = 25,
                                   value = 0,step = 0.5)),
		column(6,
				plotOutput("plotAvecR")),

		column(2,
				textInput("main","Main",""),				
				numericInput("pch", "pch",
                                   min = 0, max = 255,
                                   value = 1,step = 1),
				textInput("xlab","xlab",""),
				textInput("ylab","ylab",""),
				selectInput("type","type",choices=c("n","p","l","b","o","h"),selected="p")),
		column(2,
				textInput("color","color (if 'multi' use ';')","black"),
				selectInput("lty","lty",choices=c("blank","solid","dotted","dotdash","longdash","twodash"),selected="solid"),
				numericInput("cexlab", "cex.lab",
                                   min = 1, max = 25,
                                   value = 2,step = 0.25),
				numericInput("cexaxis", "cex.axis",
                                   min = 1, max = 25,
                                   value = 1.5,step = 0.25),
				numericInput("cexmain", "cex.main",
                                   min = 1, max = 25,
                                   value = 3,step = 0.25)))),



 fluidPage(
    fluidRow( 
		column(3,				
				sliderInput("lwd", "lwd",
                                   min = 1, max =8,
                                   value = 2,step = 0.25),
				sliderInput("cex", "Size",
                                   min = 0.5, max = 3,
                                   value = 1,step = 0.2)),
		column(3,
				uiOutput("toCol6"),
				uiOutput("toCol7")),

		column(3,
              # Input: name ----
              radioButtons(inputId = "name2", 
                           label = "Names of each point",
                           choices = c(Yes = "yes",
                                       No = "no"),
                           selected = "no", inline=T),
              radioButtons(inputId = "name", 
                           label = "Use first column for calculation",
                           choices = c(Yes = "yes",
                                       No = "no"),
                           selected = "no", inline=T)),

		column(3,
              # Input: scale ----
              radioButtons(inputId = "scale", 
                           label = "Scale",
                           choices = c(Yes = "yes",
                                       No = "no"),
                           selected = "no", inline=T),
              # Input: center ----
              radioButtons(inputId = "center", 
                           label = "Center",
                           choices = c(Yes = "yes",
                                       No = "no"),
                           selected = "yes", inline=T)))),

fluidPage(
    fluidRow(  
		h2("Barplot : % of variance"))),
fluidPage(
    fluidRow(
		column(1),
		column(1),
		column(6,
			downloadButton("downloadbarplotAvecR","Download Barplot")))),
 fluidPage(
    fluidRow(
		column(1,

				numericInput("mar1", "Bottom",
                                   min = 0, max = 25,
                                   value = 5,step = 0.5),
				numericInput("mar2", "Left",
                                   min = 0, max = 25,
                                   value = 5,step = 0.5),
				numericInput("mar3", "Top",
                                   min = 0, max = 25,
                                   value = 4,step = 0.5),
				numericInput("mar4", "Right",
                                   min = 0, max = 25,
                                   value = 2,step = 0.5)),
		column(1,
				numericInput("mgp1", "Label axis spacing",
                                   min = 0, max = 25,
                                   value = 3,step = 0.5),
				numericInput("mgp2", "Value axis spacing",
                                   min = 0, max = 25,
                                   value = 1,step = 0.5),
				numericInput("mgp3", "Axis spacing",
                                   min = 0, max = 25,
                                   value = 0,step = 0.5)),
		column(6,
				plotOutput("barplotAvecR")),
		column(2,
        		      radioButtons(inputId = "point", 
               		            label = "Cumulative sum of variances",
              		             choices = c(Yes = "yes",
                                       No = "no"),
              			             selected = "no", inline=T),
            		      radioButtons(inputId = "barpoint1", 
                  		         label = "Line 1",
                  		         choices = c(Yes = "yes",
                                       No = "no"),
                    		       selected = "no", inline=T),
			      numericInput("valbarpoint1", "Valu Line 1",
                                   min = 0, max = 100,
                                   value = 100,step = 0.25),
             		     radioButtons(inputId = "barpoint2", 
             		              label = "Line 2",
                 		          choices = c(Yes = "yes",
                   	                    No = "no"),
                  			         selected = "no", inline=T),
			     numericInput("valbarpoint2", "Valu Line 2",
                                   min = 0, max =100,
                                   value = 0,step = 0.25),
          		    radioButtons(inputId = "displaypourc", 
          	                 label = "Display % of variance",
          	                 choices = c(Yes = "yes",
                                       No = "no"),
            	               selected = "no", inline=T)
)))    
),
 # dodiff
      tabItem(tabName = "doMAplot",
              h1("MAplot"),
fluidPage(
    fluidRow(

                column(6,
			fileInput("datafile2", "Choose file")))),
    fluidPage(
fluidRow(
		 column(6, 
# Input: Checkbox if file has header
 	             radioButtons(inputId = "header2", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              
              # Input: Select separator ----
         	     radioButtons(inputId = "sep2", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = "\t", inline=T),

	     # Input: Select quotes ----
         	     radioButtons(inputId = "quote2", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),


	     # Input: Select dec ----
        	      radioButtons(inputId = "dec2", 
                           label = "Decimal",
                           choices = c(Comma = ",",
                                       "Dot" = "."),
                           selected = ".", inline=T)))),

fluidPage(
    fluidRow(
		column(6,
  			 h3("RNAseq : Cond1 vs cond2"),
			      uiOutput("waiting2"),
        		      dataTableOutput(outputId = "view2")))),
fluidPage(
    fluidRow(
		column(1),
			column(1),
				column(6,
					downloadButton("downloadMAplotAvecR","Download MAplot")))),
 fluidPage(
    fluidRow(
		column(1,

				numericInput("mar1", "Bottom",
                                   min = 0, max = 25,
                                   value = 5,step = 0.5),
				numericInput("mar2", "Left",
                                   min = 0, max = 25,
                                   value = 5,step = 0.5),
				numericInput("mar3", "Top",
                                   min = 0, max = 25,
                                   value = 4,step = 0.5),
				numericInput("mar4", "Right",
                                   min = 0, max = 25,
                                   value = 2,step = 0.5)),
		column(1,
				numericInput("mgp1", "Label axis spacing",
                                   min = 0, max = 25,
                                   value = 3,step = 0.5),
				numericInput("mgp2", "Value axis spacing",
                                   min = 0, max = 25,
                                   value = 1,step = 0.5),
				numericInput("mgp3", "Axis spacing",
                                   min = 0, max = 25,
                                   value = 0,step = 0.5)),
		column(6,
			plotOutput("MAplotAvecR")),
		column(2,				
				sliderInput("pval", "pval",
                                   min = 0.00001, max =0.05,
                                   value = 0.05,step = 0.00001),				
				sliderInput("read", "read",
                                   min = 1, max =10000,
                                   value = 150,step = 5),		
				sliderInput("fchange", "fold change",
                                   min = 0, max =8,
                                   value = 0.75,step = 0.05),
				textInput("colorpoint","Color pVal","red")),
		column(2,	
				textInput("color0","Color","black"),
				sliderInput("cexma", "Size",
                                   min = 0.5, max = 3,
                                   value = 1,step = 0.2),
				sliderInput("cexmapval", "Size p.Val",
                                   min = 0.5, max = 8,
                                   value = 1,step = 0.2),
				textInput("mainmaplot","Main",""),
              radioButtons(inputId = "printvalue", 
                           label = "Display value",
                           choices = c(Yes = "yes",
                                       No = "no"),
                           selected = "yes", inline=T))))

),
 # doHeatmap
      tabItem(tabName = "doHeatmap",
              h1("Heatmap"),
fluidPage(
    fluidRow(

                column(6,
			fileInput("datafile3", "RNAseq (Required)")),
                column(6,
			fileInput("datafile4", "gene")))),

fluidPage(
    fluidRow(
		 column(6, 
# Input: Checkbox if file has header
 	             radioButtons(inputId = "header3", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              
              # Input: Select separator ----
 	             radioButtons(inputId = "sep3", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ";", inline=T),

	     # Input: Select quotes ----
   	           radioButtons(inputId = "quote3", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),

	     # Input: Select dec ----
 	             radioButtons(inputId = "dec3", 
                           label = "Decimal",
                           choices = c(Comma = ",",
                                       "Dot" = "."),
                           selected = ",", inline=T)),


                column(6, 
# Input: Checkbox if file has header
     	             radioButtons(inputId = "header4", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              
              # Input: Select separator ----
         	     radioButtons(inputId = "sep4", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ";", inline=T),

	     # Input: Select quotes ----
          	    radioButtons(inputId = "quote4", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T)))),

fluidPage(
    fluidRow(
		column(6,
  	 h3("RNAseq"),
	  	    uiOutput("waiting3"),
          	    dataTableOutput(outputId = "view3")),
		column(6,
  	 h3("Gene"),
	            uiOutput("waiting4"),
           	    dataTableOutput(outputId = "view4")))),


fluidPage(
    fluidRow(
		column(3,

			uiOutput("toCol8")),
		column(2,

			textOutput("text1")),
		column(3,

			uiOutput("toCol9")),
		column(3,
			uiOutput("toCol10")))),



fluidPage(
    fluidRow(
		column(1),
			column(6,
				downloadButton("downloadheatmapAvecR","Download Heatmap"),
					plotOutput("heatmapAvecR")),
		column(2,				
				sliderInput("cexRow", "cexRow",
                                   min = 0.25, max =5,
                                   value = 1.5,step = 0.05),
				textInput("nameheatmap", "Name ('multi' use ';')"),
				textInput("colorplus","Color +","red"),
				textInput("colorplusmoins","Color +/-","white"),
				textInput("colormoins","Color -","blue")),
		column(2,
				uiOutput("toCol11"),
				uiOutput("toCol12"),
				textInput("mainheatmap","Main","")))),
fluidPage(
    fluidRow(
		column(6,
			dataTableOutput("logmat"))))      
)    
    )
  )
),

server<-function(input, output,session){
options(shiny.maxRequestSize=30*1024^2)
	 filedata <- reactive({ 
 		   infile <- input$datafile 
  			  if (is.null(infile)) { 
    # User has not uploaded a file yet 
 				   return(NULL) 
 					   } 
					 read.csv(infile$datapath,  header = as.logical(input$header),
					 sep=input$sep, quote = input$quote,dec=input$dec) 
						}) 
output$waiting1<-renderText({
    if (is.null(filedata())) {
("Waiting for file...")}else{}})

filedata2 <- reactive({ 
 		   infile2 <- input$datafile2 
 			   if (is.null(infile2)) { 
    # User has not uploaded a file yet 
  				  return(NULL) 
 					   } 
				        read.csv(infile2$datapath,  header = as.logical(input$header2),
					sep=input$sep2, quote = input$quote2,dec=input$dec2) 
						}) 
output$waiting2<-renderText({
    if (is.null(filedata2())) {
("Waiting for file...")}else{}})
###############pour heatmap
filedata3 <- reactive({ 
  		  infile3 <- input$datafile3 
  			  if (is.null(infile3)) { 
    # User has not uploaded a file yet 
  				  return(NULL) 
 					   } 
					   read.csv(infile3$datapath,  header = as.logical(input$header3),
					   sep=input$sep3, quote = input$quote3,dec=input$dec3) 
						}) 
output$waiting3<-renderText({
    if (is.null(filedata3())) {
("Waiting for file...")}else{}})

filedata4 <- reactive({ 
  		  infile4 <- input$datafile4
   			 if (is.null(infile4)) { 
    # User has not uploaded a file yet 
  				  return(NULL) 
  					  } 
  					 read.csv(infile4$datapath,  header = as.logical(input$header4),
					 sep=input$sep4, quote = input$quote4) 
						}) 
output$waiting4<-renderText({
    if (is.null(filedata4())) {
("Waiting for file...")}else{}})
############################################

pcaview <-  reactive({
    req(input$datafile)
	if (input$name=="no"){
		filedata4pca<-(filedata()[,-1])
			}
	else{filedata4pca<-(filedata())}})
pcaview2 <-  reactive({
	rnaseqcenter<-scale(pcaview(),scale=ifelse(input$scale=="no",F,T),center=ifelse(input$center=="yes",T,F))})
	#rnaseqcenter<-scale(pcaview(),scale=F,center=T)})
pcaview3 <-  reactive({
	pca<-prcomp(pcaview2())})
pcaview4 <-  reactive({
	corpca<-cor(pcaview2(),pcaview3()$x)})

pourcvar <-  reactive({
	pourcvar<-round(pcaview3()$sdev[1:length(pcaview3()$sdev)]^2/sum(pcaview3()$sdev^2)*100,2)})
cumvar <-  reactive({
	cumvar<-cumsum(pcaview3()$sdev^2)/sum(pcaview3()$sdev^2)*100})
barpoint<-reactive({
	barpoint<-barplot(pourcvar(),col="black",ylab="% of variances",names=paste("PC",1:ncol(pcaview3()$x),sep=""),ylim=c(0,100))})


output$PCArotation <-  renderDataTable({
    req(input$datafile)
   df <- pcaview3()$rotation
  })


#####pour choisir colonne pca (pc1,pc2...)

output$toColpcax <- renderUI({ 

   df <-pcaview3()$rotation
   # if (is.null(df2)) return(NULL) 
    if (is.null(df)) return(NULL) 
	items<-c(colnames(df))
   names(items)=items 
    selectInput("xaxis","x for PCA",items,selected=input$xaxis) 

}) 


output$toColpcay <- renderUI({ 

   df <-pcaview3()$rotation
   # if (is.null(df2)) return(NULL) 
    if (is.null(df)) return(NULL) 
	items<-c("none",colnames(df))
   names(items)=items 
    selectInput("yaxis","y for PCA",items,selected=input$yaxis) 

}) 

output$view <-  renderDataTable({
    req(input$datafile)
   df <- read.csv(input$datafile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
		   dec = input$dec
    )
  })
output$view2 <-  renderDataTable({
    req(input$datafile2)
   df2 <- read.csv(input$datafile2$datapath,
                   header = as.logical(input$header2),
                   sep = input$sep2,
                   quote = input$quote2,
		   dec = input$dec2
    )
  })
###############pour heatmap
output$view3 <-  renderDataTable({
    req(input$datafile3)
   df3 <- read.csv(input$datafile3$datapath,
                   header = as.logical(input$header3),
                   sep = input$sep3,
                   quote = input$quote3,
		   dec = input$dec3
    )
  })

output$view4 <-  renderDataTable({
    req(input$datafile4)
   df4 <- read.csv(input$datafile4$datapath,
                   header = as.logical(input$header4),
                   sep = input$sep4,
                   quote = input$quote4
    )
  })





########################################

output$toCol6 <- renderUI({ 
	if (max(pcaview3()$rotation[,input$xaxis])<0){
		max_x<-c(abs(max(pcaview3()$rotation[,input$xaxis]))*3)
			}else{max_x<-c(max(pcaview3()$rotation[,input$xaxis])*3)}
				if (min(pcaview3()$rotation[,input$xaxis])<0){
		min_x<-c(min(pcaview3()$rotation[,input$xaxis])*3)
			}else{min_x<-c(abs(min(pcaview3()$rotation[,input$xaxis]))*3)}
			#max_x1<-c(abs(max(pcaview3()$rotation[,input$xaxis]))*1)
			max_x1<-c(max(pcaview3()$rotation[,input$xaxis])*1)
			min_x1<-c(min(pcaview3()$rotation[,input$xaxis])*1)
 			  sliderInput("xlim", "xlim",
			    min = min_x, max =max_x,
			   value = c(min_x1,max_x1),step = 0.01)
				})

output$toCol7 <- renderUI({ 
	if (max(pcaview3()$rotation[,input$yaxis])<0){
		max_y<-c(abs(max(pcaview3()$rotation[,input$yaxis]))*3)
			}else{max_y<-c(max(pcaview3()$rotation[,input$yaxis])*3)}
			if (min(pcaview3()$rotation[,input$yaxis])<0){
			min_y<-c(min(pcaview3()$rotation[,input$yaxis])*3)
		}else{min_y<-c(abs(min(pcaview3()$rotation[,input$yaxis]))*3)}
		#max_y1<-c(abs(max(pcaview3()$rotation[,input$yaxis]))*1)
		max_y1<-c(max(pcaview3()$rotation[,input$yaxis])*1)
		min_y1<-c(min(pcaview3()$rotation[,input$yaxis])*1)
			   sliderInput("ylim", "ylim",
			    min = min_y, max =max_y,
			   value = c(min_y1,max_y1),step = 0.01)
				})

output$toCol8<-renderUI({
   df <-filedata3() 
	items<-c(names(df))
   names(items)=items 
    selectInput("newcond1","Condition 1",items,multiple=T)
    })
output$toCol9<-renderUI({
   df <-filedata3() 
	items<-c(names(df))
   names(items)=items 
    selectInput("newcond2","Condition 2",items,multiple=T)
    })

output$toCol10<-renderUI({
   df <-filedata3() 
	items<-c(names(df))
   names(items)=items 
    selectInput("namegene","Name gene",items)
    })


output$toCol11<-renderUI({
# if (!is.null(filedata3())) {
   df <-filedata3() 
   df2 <-filedata4() 
	items<-c("All",names(df),names(df2))
   names(items)=items 
    selectInput("genetomap","Gene to map",items)
    })

output$toCol12<-renderUI({
   df <-filedata3() 
	items<-c(as.vector(df[,input$genetomap]))
   names(items)=as.vector(items) 
    selectInput("selectgene","Select gene",items,multiple=T)
    })


output$text1<-renderText({
("             /") 
    })


output$logmat <-  renderDataTable({
    req(input$datafile3)
   if (input$genetomap=="All"){
	gene<-as.vector(unlist(filedata4()))
		gene<-unique(gene[gene!=""])
	}else{}
		if (input$genetomap%in%names(filedata3())){
	gene<-as.vector(unlist(filedata3()[,input$genetomap]))
	gene<-unique(gene[(gene!="")])
		}else{}
			if (input$genetomap%in%names(filedata4())){
	gene<-as.vector(unlist(filedata4()[,input$genetomap]))
	gene<-unique(gene[gene!=""])
		}else{}
	if(!is.null(input$selectgene)){
	gene<-as.vector(input$selectgene)
		}else{}
	rnaseq<-subset(filedata3(),filedata3()[,input$namegene]%in%gene)
	rnaseqcond1<-rnaseq[,input$newcond1]
	rnaseqcond2<-rnaseq[,input$newcond2]
	rnaseqcond11<-as.matrix(rnaseqcond1)
	rnaseqcond22<-as.matrix(rnaseqcond2)
	rnaseqcond11<-abs(rnaseqcond11)
	rnaseqcond22<-abs(rnaseqcond22)
	mat2<-rnaseqcond11/rnaseqcond22
	logmat2<-log2(mat2)
	rownames(logmat2)<-rnaseq[,input$namegene]
#colnames(logmat2)<-c("D0","D2","D3")
	colnames(logmat2)<-strsplit(input$nameheatmap,";")[[1]]
	logmat2<-logmat2[order(rownames(logmat2)),]
  })


output$plotAvecR <- renderPlot({
    if (!is.null(filedata())) {
	if (input$yaxis=="none"){
		par(mgp=c(input$mgp1,input$mgp2,input$mgp3))
			par(mar=c(input$mar1,input$mar2,input$mar3,input$mar4))
				plot(x=pcaview3()$rotation[,input$xaxis],y=rep(0,length(pcaview3()$rotation[,input$xaxis])),
     				      main = input$main,
      				      ylab = ifelse(input$ylab=="",input$yaxis,input$ylab),
       	      			      xlab = ifelse(input$xlab=="",input$xaxis,input$xlab),
      				      pch = input$pch,
         			      lwd = input$lwd,
				      col=strsplit(input$color,";")[[1]],
                                      lty = input$lty,
                                      type = input$type,
                                      cex.lab = input$cexlab,
                                      cex.axis = input$cexaxis,
                                      cex.main = input$cexmain,
                           	      xlim = input$xlim,
                                      ylim= input$ylim,
                               	      cex = input$cex)
	if (input$name2=="yes"){
		text(pcaview3()$rotation[,input$xaxis],rep(0,length(pcaview3()$rotation[,input$xaxis])),labels=rownames(pcaview3()$rotation),pos=3,xpd=T)
			}else{}
	}else{
		par(mgp=c(input$mgp1,input$mgp2,input$mgp3))
			par(mar=c(input$mar1,input$mar2,input$mar3,input$mar4))
				plot(pcaview3()$rotation[,input$xaxis],pcaview3()$rotation[,input$yaxis],
      		                      main = input$main,
           			      ylab = ifelse(input$ylab=="",input$yaxis,input$ylab),
          			       xlab = ifelse(input$xlab=="",input$xaxis,input$xlab),
          			       pch = input$pch,
          			       lwd = input$lwd,
	  			       col=strsplit(input$color,";")[[1]],
          			       lty = input$lty,
           			      type = input$type,
          			       cex.lab = input$cexlab,
          			       cex.axis = input$cexaxis,
          			       cex.main = input$cexmain,
       			               xlim = input$xlim,
 			               ylim= input$ylim,
			       	       cex = input$cex)
	if (input$name2=="yes"){
		text(pcaview3()$rotation[,input$xaxis],pcaview3()$rotation[,input$yaxis],labels=rownames(pcaview3()$rotation),pos=3,xpd=T)
	}else{}
	}
	}else{}
})

output$downloadplotAvecR <- downloadHandler(
      filename = function() {
          paste('plot', '.png', sep='')
        },
        content=function(file){
          png(file,type="cairo",res=200,units="cm",width=30,height=15)
    if (!is.null(filedata())) {
	if (input$yaxis=="none"){
		par(mgp=c(input$mgp1,input$mgp2,input$mgp3))
			par(mar=c(input$mar1,input$mar2,input$mar3,input$mar4))
				plot(x=pcaview3()$rotation[,input$xaxis],y=rep(0,length(pcaview3()$rotation[,input$xaxis])),
     				      main = input$main,
      				      ylab = ifelse(input$ylab=="",input$yaxis,input$ylab),
       	      			      xlab = ifelse(input$xlab=="",input$xaxis,input$xlab),
      				      pch = input$pch,
         			      lwd = input$lwd,
				      col=strsplit(input$color,";")[[1]],
                                      lty = input$lty,
                                      type = input$type,
                                      cex.lab = input$cexlab,
                                      cex.axis = input$cexaxis,
                                      cex.main = input$cexmain,
                           	      xlim = input$xlim,
                                      ylim= input$ylim,
                               	      cex = input$cex)
	if (input$name2=="yes"){
		text(pcaview3()$rotation[,input$xaxis],rep(0,length(pcaview3()$rotation[,input$xaxis])),labels=rownames(pcaview3()$rotation),pos=3,xpd=T)
			}else{}
	}else{
		par(mgp=c(input$mgp1,input$mgp2,input$mgp3))
			par(mar=c(input$mar1,input$mar2,input$mar3,input$mar4))
				plot(pcaview3()$rotation[,input$xaxis],pcaview3()$rotation[,input$yaxis],
      		                      main = input$main,
           			      ylab = ifelse(input$ylab=="",input$yaxis,input$ylab),
          			       xlab = ifelse(input$xlab=="",input$xaxis,input$xlab),
          			       pch = input$pch,
          			       lwd = input$lwd,
	  			       col=strsplit(input$color,";")[[1]],
          			       lty = input$lty,
           			      type = input$type,
          			       cex.lab = input$cexlab,
          			       cex.axis = input$cexaxis,
          			       cex.main = input$cexmain,
       			               xlim = input$xlim,
 			               ylim= input$ylim,
			       	       cex = input$cex)
	if (input$name2=="yes"){
		text(pcaview3()$rotation[,input$xaxis],pcaview3()$rotation[,input$yaxis],labels=rownames(pcaview3()$rotation),pos=3,xpd=T)
	}else{}
	}
	}else{}
dev.off()
})

###############barplot
output$barplotAvecR <- renderPlot({
    if (!is.null(filedata())) {
		par(mgp=c(input$mgp1bar,input$mgp2bar,input$mgp3bar))
			par(mar=c(input$mar1bar,input$mar2bar,input$mar3bar,input$mar4bar))
				barplot(pourcvar(),col="black",ylab="% of variances",
				names=paste("PC",1:ncol(pcaview3()$x),sep=""),ylim=c(0,100))
	if (input$barpoint1=="yes"){
			abline(h=input$valbarpoint1,lty=2,col="black",lwd=2)
	}else{}
	if (input$barpoint2=="yes"){
			abline(h=input$valbarpoint2,lty=2,col="black",lwd=2)
	}else{}
	if (input$point=="yes"){
			points(barpoint(),cumvar(),pch=19,col="red",xpd=T)
	}else{}
	if (input$displaypourc=="yes"){
			text(barpoint(),110,labels=pourcvar(),cex=1,col="red",xpd=T)
	}else{}
	}else{}
})

output$downloadbarplotAvecR <- downloadHandler(
      filename = function() {
          paste('barplot', '.png', sep='')
        },
        content=function(file){
          png(file,type="cairo",res=200,units="cm",width=30,height=15)
    if (!is.null(filedata())) {
		par(mgp=c(input$mgp1bar,input$mgp2bar,input$mgp3bar))
			par(mar=c(input$mar1bar,input$mar2bar,input$mar3bar,input$mar4bar))
				barplot(pourcvar(),col="black",ylab="% of variances",
				names=paste("PC",1:ncol(pcaview3()$x),sep=""),ylim=c(0,100))
	if (input$barpoint1=="yes"){
			abline(h=input$valbarpoint1,lty=2,col="black",lwd=2)
	}else{}
	if (input$barpoint2=="yes"){
			abline(h=input$valbarpoint2,lty=2,col="black",lwd=2)
	}else{}
	if (input$point=="yes"){
			points(barpoint(),cumvar(),pch=19,col="red",xpd=T)
	}else{}
	if (input$displaypourc=="yes"){
			text(barpoint(),110,labels=pourcvar(),cex=1,col="red",xpd=T)
	}else{}
	}else{}
dev.off()
})

###############MAplot
output$MAplotAvecR <- renderPlot({
    if (!is.null(filedata2())) {
	par(mgp=c(input$mgp1ma,input$mgp2ma,input$mgp3ma))
		par(mar=c(input$mar1ma,input$mar2ma,input$mar3ma,input$mar4ma))
#plot(log2(filedata2()$baseMean),(filedata2()$log2FoldChange_ko.wt),pch=".",main=input$datafile2$name,cex.main=2.5,
#col=ifelse((filedata2()$padj)<input$pval,input$colorpoint,ifelse(input$colorpoint=="","red",ifelse(input$color0=="","black",input$color0))),
#xlab="log2(baseMean)",ylab="Log2FoldChange_ko.wt",cex.lab=1.4)
			plot(log2(filedata2()$baseMean),(filedata2()$log2FoldChange_ko.wt),pch=".",main=ifelse(input$mainmaplot=="",
			input$datafile2$name,input$mainmaplot),cex.main=2.5,
			col=ifelse((filedata2()$padj)<input$pval,input$colorpoint,input$color0),
			cex=ifelse((filedata2()$padj)<input$pval,input$cexmapval,input$cexma),
			ylim=c(-10,10),
			xlab="log2(baseMean)",ylab="Log2FoldChange_ko.wt",cex.lab=1.4)
			abline(h=-input$fchange)
			abline(h=input$fchange)
			abline(v=log2(input$read))
			if (input$printvalue=="yes"){
			text(10,((70*par()$yaxp[1])/100),labels=length((filedata2()$baseMean)[(log2((filedata2()$baseMean))>log2(input$read))&((filedata2()$log2FoldChange_ko.wt)<(-input$fchange))&((filedata2()$padj)<input$pval)]),cex=2.5)
			text(10,((70*par()$yaxp[2])/100),labels=length((filedata2()$baseMean)[(log2((filedata2()$baseMean))>log2(input$read))&((filedata2()$log2FoldChange_ko.wt)>(input$fchange))&((filedata2()$padj)<input$pval)]),cex=2.5)
	}else{}
	}else{}
})

output$downloadMAplotAvecR <- downloadHandler(
      filename = function() {
          paste('MAplot', '.png', sep='')
        },
        content=function(file){
          png(file,type="cairo",res=200,units="cm",width=30,height=15)
    if (!is.null(filedata2())) {
	par(mgp=c(input$mgp1ma,input$mgp2ma,input$mgp3ma))
		par(mar=c(input$mar1ma,input$mar2ma,input$mar3ma,input$mar4ma))
#plot(log2(filedata2()$baseMean),(filedata2()$log2FoldChange_ko.wt),pch=".",main=input$datafile2$name,cex.main=2.5,
#col=ifelse((filedata2()$padj)<input$pval,input$colorpoint,ifelse(input$colorpoint=="","red",ifelse(input$color0=="","black",input$color0))),
#xlab="log2(baseMean)",ylab="Log2FoldChange_ko.wt",cex.lab=1.4)
			plot(log2(filedata2()$baseMean),(filedata2()$log2FoldChange_ko.wt),pch=".",main=ifelse(input$mainmaplot=="",
			input$datafile2$name,input$mainmaplot),cex.main=2.5,
			col=ifelse((filedata2()$padj)<input$pval,input$colorpoint,input$color0),
			cex=ifelse((filedata2()$padj)<input$pval,input$cexmapval,input$cexma),
			ylim=c(-10,10),
			xlab="log2(baseMean)",ylab="Log2FoldChange_ko.wt",cex.lab=1.4)
			abline(h=-input$fchange)
			abline(h=input$fchange)
			abline(v=log2(input$read))
			if (input$printvalue=="yes"){
			text(10,((70*par()$yaxp[1])/100),labels=length((filedata2()$baseMean)[(log2((filedata2()$baseMean))>log2(input$read))&((filedata2()$log2FoldChange_ko.wt)<(-input$fchange))&((filedata2()$padj)<input$pval)]),cex=2.5)
			text(10,((70*par()$yaxp[2])/100),labels=length((filedata2()$baseMean)[(log2((filedata2()$baseMean))>log2(input$read))&((filedata2()$log2FoldChange_ko.wt)>(input$fchange))&((filedata2()$padj)<input$pval)]),cex=2.5)
	}else{}
	}else{}
dev.off()
})


#################heatmap
output$heatmapAvecR <- renderPlot({
    if (!is.null(filedata3())) {
	if (input$genetomap=="All"){
		gene<-as.vector(unlist(filedata4()))
		gene<-unique(gene[gene!=""])
	}else{}
		if (input$genetomap%in%names(filedata3())){
		gene<-as.vector(unlist(filedata3()[,input$genetomap]))
		gene<-unique(gene[(gene!="")])
	}else{}
		if (input$genetomap%in%names(filedata4())){
		gene<-as.vector(unlist(filedata4()[,input$genetomap]))
		gene<-unique(gene[gene!=""])
	}else{}
		if(!is.null(input$selectgene)){
		gene<-as.vector(input$selectgene)
	}else{}
		rnaseq<-subset(filedata3(),filedata3()[,input$namegene]%in%gene)
		rnaseqcond1<-rnaseq[,input$newcond1]
		rnaseqcond2<-rnaseq[,input$newcond2]
		rnaseqcond11<-as.matrix(rnaseqcond1)
		rnaseqcond22<-as.matrix(rnaseqcond2)
		rnaseqcond11<-abs(rnaseqcond11)
		rnaseqcond22<-abs(rnaseqcond22)
		mat2<-rnaseqcond11/rnaseqcond22
		logmat2<-log2(mat2)
		rownames(logmat2)<-rnaseq[,input$namegene]
#colnames(logmat2)<-c("D0","D2","D3")
		colnames(logmat2)<-strsplit(input$nameheatmap,";")[[1]]
		logmat2<-logmat2[order(rownames(logmat2)),]
		colfunc<-colorRampPalette(c(input$colormoins,input$colorplusmoins,input$colorplus))
		colors2 <- (colfunc(length(unique(rank(logmat2)))))
		legend_image <- as.raster(matrix(rev(colfunc(length(unique(rank(logmat2))))), ncol=1))
		breakmin<-c(abs(min(logmat2[logmat2!=-Inf])))
		breakmax<-c(max(logmat2[logmat2!=Inf]))
		breakmax2<-c(max(breakmin,breakmax))
		breakmin2<-c(min(breakmin,breakmax))
		breakmax3<-c(max(breakmin2,breakmax2))
		breakmin3<-c(-breakmax3)
		heatmap(logmat2,col=colors2,scale="none",Rowv=NA,Colv=NA,margins=c(5,15),breaks=seq(breakmin3,breakmax3,
		length.out=length(colors2)+1),cexRow=input$cexRow,
		main=ifelse(input$mainheatmap=="",input$genetomap,input$mainheatmap))
		rasterImage(legend_image, 0.85,0.25,0.9,0.75)
		text(x=0.95, y = c(0.27,mean(c(0.27,0.73)),0.73), labels = c(signif(breakmin3,1),
		signif(mean(c(breakmin3,breakmax3)),1),
		signif(breakmax2,1)),xpd=T,adj=0)
	}else{}
})

output$downloadheatmapAvecR <- downloadHandler(
      filename = function() {
          paste('Heatmap', '.png', sep='')
        },
        content=function(file){
          png(file,type="cairo",res=500,units="cm",width=15,height=15)

    if (!is.null(filedata3())) {
	if (input$genetomap=="All"){
		gene<-as.vector(unlist(filedata4()))
		gene<-unique(gene[gene!=""])
	}else{}
		if (input$genetomap%in%names(filedata3())){
		gene<-as.vector(unlist(filedata3()[,input$genetomap]))
		gene<-unique(gene[(gene!="")])
	}else{}
		if (input$genetomap%in%names(filedata4())){
		gene<-as.vector(unlist(filedata4()[,input$genetomap]))
		gene<-unique(gene[gene!=""])
	}else{}
		if(!is.null(input$selectgene)){
		gene<-as.vector(input$selectgene)
	}else{}
		rnaseq<-subset(filedata3(),filedata3()[,input$namegene]%in%gene)
		rnaseqcond1<-rnaseq[,input$newcond1]
		rnaseqcond2<-rnaseq[,input$newcond2]
		rnaseqcond11<-as.matrix(rnaseqcond1)
		rnaseqcond22<-as.matrix(rnaseqcond2)
		rnaseqcond11<-abs(rnaseqcond11)
		rnaseqcond22<-abs(rnaseqcond22)
		mat2<-rnaseqcond11/rnaseqcond22
		logmat2<-log2(mat2)
		rownames(logmat2)<-rnaseq[,input$namegene]
#colnames(logmat2)<-c("D0","D2","D3")
		colnames(logmat2)<-strsplit(input$nameheatmap,";")[[1]]
		logmat2<-logmat2[order(rownames(logmat2)),]
		colfunc<-colorRampPalette(c(input$colormoins,input$colorplusmoins,input$colorplus))
		colors2 <- (colfunc(length(unique(rank(logmat2)))))
		legend_image <- as.raster(matrix(rev(colfunc(length(unique(rank(logmat2))))), ncol=1))
		breakmin<-c(abs(min(logmat2[logmat2!=-Inf])))
		breakmax<-c(max(logmat2[logmat2!=Inf]))
		breakmax2<-c(max(breakmin,breakmax))
		breakmin2<-c(min(breakmin,breakmax))
		breakmax3<-c(max(breakmin2,breakmax2))
		breakmin3<-c(-breakmax3)
		heatmap(logmat2,col=colors2,scale="none",Rowv=NA,Colv=NA,margins=c(5,15),breaks=seq(breakmin3,breakmax3,
		length.out=length(colors2)+1),cexRow=input$cexRow,
		main=ifelse(input$mainheatmap=="",input$genetomap,input$mainheatmap))
		rasterImage(legend_image, 0.85,0.25,0.9,0.75)
		text(x=0.95, y = c(0.27,mean(c(0.27,0.73)),0.73), labels = c(signif(breakmin3,1),
		signif(mean(c(breakmin3,breakmax3)),1),
		signif(breakmax2,1)),xpd=T,adj=0)
	}else{}
dev.off()
})


}
)