library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(shinyWidgets)
x <- read.csv("C:\\Users\\Zainab\\Documents\\R_project\\studentdata.csv")
#View(x)
#colnames(x)
df<-data.frame(x)
ui<-navbarPage(theme = shinytheme("flatly"),h4(strong("Student Result")),
              
               tabPanel(h4("Theory"),sidebarLayout(sidebarPanel(radioButtons("subjectTh", "Subjects",c("Applied Mathematics" = "AM.1.TH", "Applied Physics" = "AP.1.TH", "Applied Chemistry" = "AC.1.TH", "Basic Electrical Engineering"="BEE.TH","Engineering Mechanics"="EM.TH","EVS"="EVS.TH")),style="color:#0B7F90;background-color:#F8F7F1"),mainPanel(plotOutput("plotth",height = "600px")))),
               tabPanel(h4("Term Work"),sidebarLayout(sidebarPanel(radioButtons("subjectTw", "Subjects",c("Applied Mathematics" = "AM.1.TW", "Applied Physics" = "AP.1.TW", "Applied Chemistry" = "AC.1.TW", "Basic Electrical Engineering"="BEE.TW","Engineering Mechanics"="EM.TW","EVS"="EVS.TW")),style="color:#0B7F90;background-color:#F8F7F1"),mainPanel(plotOutput("plottw",height = "600px")))),
               tabPanel(h4("Pass Vs Fail"),sidebarLayout(sidebarPanel(h3("Subjectwise Result"),radioButtons("subres", "Subjects",c("Applied Mathematics" = "AM.1", "Applied Physics" = "AP.1", "Applied Chemistry" = "AC.1", "Basic Electrical Engineering"="BEE","Engineering Mechanics"="EM","EVS"="EVS")),actionButton("subwise","Plot"),h3("Total Result"),actionButton("total","Plot"),style="color:#0B7F90;background-color:#F8F7F1"),mainPanel(plotOutput("rgraph",height = "471px")))),
               tabPanel(h4("Result"),h3(strong("Select row to display the graph.")),splitLayout(cellWidths = c("60%","50%"), cellArgs = list(style="padding:50px"),dataTableOutput("table"),plotOutput("p2",height="500px", width = "80%"))),
              
               setBackgroundColor(
                 color = "#CDCDCE")
                 
)

server<-function(input,output){
  
    output$table<-renderDataTable(df, selection="single")
    output$plotth<-renderPlot({
      switch(input$subjectTh,
             "AM.1.TH" = ggplot(x, aes(AM.1.TH)) + 
               geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
             "AP.1.TH" = ggplot(x, aes(AP.1.TH)) + 
               geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
             "AC.1.TH" = ggplot(x, aes(AC.1.TH)) + 
               geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
             "BEE.TH" = ggplot(x, aes(BEE.TH)) + 
               geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
             "EM.TH" = ggplot(x, aes(EM.TH)) + 
               geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
             "EVS.TH" = ggplot(x, aes(EVS.TH)) + 
               geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue")
      )
      })
      output$plottw<-renderPlot({
        switch(input$subjectTw,
               "AM.1.TW" = ggplot(x, aes(AM.1.TW)) + 
                 geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
               "AP.1.TW" = ggplot(x, aes(AP.1.TW)) + 
                 geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
               "AC.1.TW" = ggplot(x, aes(AC.1.TW)) + 
                 geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
               "BEE.TW" = ggplot(x, aes(BEE.TW)) + 
                 geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
               "EM.TW" = ggplot(x, aes(EM.TW)) + 
                 geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue"),
               "EVS.TW" = ggplot(x, aes(EVS.TW)) + 
                 geom_histogram(binwidth=5, col="black", aes(fill=..count..)) +scale_fill_gradient("Count", low="skyblue", high="blue")
        )      
        })
      observeEvent(input$subwise,{
      output$rgraph<-renderPlot({
        switch(input$subres,
               "AM.1" = ggplot(x, aes(AM.R)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip(),
               "AP.1" = ggplot(x, aes(AP.R)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip(),
               "AC.1" = ggplot(x, aes(AC.R)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip(),
               "BEE" = ggplot(x, aes(BEE.R)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip(),
               "EM" = ggplot(x, aes(EM.R)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip(),
               "EVS" = ggplot(x, aes(EVS.R)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip()
        )
      })
      })
      observeEvent(input$total,{
       output$rgraph<-renderPlot({
         ggplot(x, aes(Result)) + geom_bar(width = 0.3, fill = "#3B65A5")+ scale_y_continuous("Count", breaks = seq(0,400,40)) + theme_gray()+coord_flip()
       }) 
})
      observeEvent(input$table_rows_selected,{
        x<-input$table_rows_selected
        
        
        m<-df[["AM.T"]]
        m1<-data.frame(m)
        m2<-m1$m[x]
        
        p<-df[["AP.T"]]
        p1<-data.frame(p)
        p2<-p1$p[x]
        
        c<-df[["AC.T"]]
        c1<-data.frame(c)
        c2<-c1$c[x]
        
        b<-df[["BEE.T"]]
        b1<-data.frame(b)
        b2<-b1$b[x]
        
        e<-df[["EM.T"]]
        e1<-data.frame(e)
        e2<-e1$e[x]
        
        v<-df[["EVS.T"]]
        v1<-data.frame(v)
        v2<-v1$v[x]
        
        vec<-c(m2,p2,c2,b2,e2,v2)
        vec
        barplot(vec)
        
        output$p2<-renderPlot({ 
          barplot(vec,
                  main = "Result",
                  ylab = "Subjects",
                  xlab = "Marks",
                  names.arg = c("AM", "AP", "AC", "BEE", "EM", "EVS"),
                  col = "#7171FE",
                  xlim= c(0,100), width = 12,
                  horiz = FALSE)
        })
      })
}
shinyApp(ui=ui, server=server)