
source("Global.r")
library(shiny)
library(plotly)
library(data.table)
library(DT)
library(tidyr)
library(shinythemes)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
gg_fill_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#install.packages("devtools")
#devtools::install_github("daattali/colourpicker")
library(colourpicker)
colorchocie<- c('skyblue','burlywood','cadetblue','cyan','darkgreen','grey20','grey100','lightcyan4','gold4','wheat4','yellow4','tomato1',"snow4"   )
dashHeader=dashboardHeader(title = "Comparison of linkage disequilibrium 
      for crossbred populations ",
                           titleWidth = 750
                           
                           
)




dashSidebar=dashboardSidebar(
  
  sidebarMenu(
    menuItem(text='How to play the app',
             tabName = 'play_app',
             icon=icon("bar-chart-o")),
    menuItem(text='r2 estimates',
             tabName = 'estimate',
             icon=icon("bar-chart-o")),
    menuItem(text='Bias & precision',
             tabName = 'Biaspre',
             icon=icon('dashboard')),
    menuItem(text='Ratio of precision',
             tabName = 'Ratiop',
             icon=icon('dashboard')  
    )
  ))
dashBody=dashboardBody(
  
  useShinyjs(),
  # useShinyalert(),
  list(
    actionButton(inputId = "showh", label = "Show hidden text"),
    actionButton(inputId = "hideh", label = "Hide text"),
    br(),
    hidden(tags$div(id="txt", style='color:blue;', list(helpText("This app shows stochastic simulation results for linkage disequiliburm using genotype
                                                                 versus haplotype for crossbred population.Sample size input parameter shows sample size followed by method of computation,e.g.
                                                                 900Ha refers sample size of 900 using haplotype method."),hr())))),
  
  tabItems(
    tabItem(
      tabName='play_app',
       fluidRow(
         box( width =12,
              collapsible = TRUE,
        h2("How to play with the App",br(),br(),HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/b2qTVW3Pq-8" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
      )
       )
    ),
    tabItem(
      tabName='Biaspre',
      fluidRow(
        
        box( width =6,
             collapsible = TRUE,
             title='Sample size_Colorchoice',
             status='primary',solidHeader = TRUE,
             selectizeInput("select1", "Select:", 
                            choices = as.list( levels(datbiassd[,12])), 
                            selected ='900Ha' , 
                            multiple = TRUE),
             
             uiOutput('myPanel1') ),
        box( width =6,
             
             collapsible = TRUE,
             title='r2 value',
             status='primary',solidHeader = TRUE,
             selectInput("d1", "r2 value:",
                         choices = levels(datbiassd[,6]),
                         selected=levels(datbiassd[,6])[1],multiple=TRUE))),
      
      
      fluidRow(
        tabBox(width=12,
               tabPanel(title="Bias",  
                        plotlyOutput("plot1")),
               tabPanel(title="Biaspvalue",  
                        plotlyOutput("plot12")),
               
               tabPanel(title="Precision",  
                        plotlyOutput("plot2")),
               tabPanel(title="Bias precision Table",  
                        dataTableOutput(outputId = "tableDTbp"),
                        downloadButton("downloadDatabp", "Downloadbp")    ) 
        ))),
    
    
    
    tabItem(
      tabName= 'Ratiop',
      fluidRow(
        
        box( width =6,
             collapsible = TRUE,
             title='Sample size_Colorchoice',
             status='primary',solidHeader = TRUE,
           
             
             
             
             
               selectizeInput("select2", "Select:", 
                            choices = as.list( levels(newdatmsdratio[,1])), 
                            selected ='900' , 
                            multiple = TRUE),
             
             uiOutput('myPanel2') ),
        box( width =6,
             
             collapsible = TRUE,
             title='r2 value',
             status='primary',solidHeader = TRUE,
             selectInput("dr", "r2 value:",
                         choices = levels(datbiassd[,6]),
                         selected=levels(datbiassd[,6])[1],multiple=TRUE))),
      
      
      fluidRow(
        tabBox(width=12,
            
            tabPanel(title= 'Ratiop',
                     plotlyOutput('plot3')),
            tabPanel(title= 'corhapgeno',
                     plotlyOutput('plot32')),
            tabPanel(title="Ratio of sd Table",  
                     dataTableOutput(outputId = "tableDTr"),
                     downloadButton("downloadData0", "Downloadsd"))
            
            ))),
    
    
    # Second side bar Tab
    tabItem(
      tabName='estimate',
      fluidRow(
        
        
        
        box( collapsible = TRUE,
             title='Sample size_Colorchoice',
             status='primary',solidHeader = TRUE,
             selectizeInput("select", "Select:", 
                            choices = as.list( levels(datmeansd[,1])), 
                            selected = "900Ha", 
                            multiple = TRUE),
             
             uiOutput('myPanel') ),
        
        box( collapsible = TRUE,
             title='Allele frequencies',
             status='primary',solidHeader = TRUE,selectInput("p11", "Allele 0 frequency for line A:",
                                                             choices = levels(datmeansd[,2]),selected=levels(datmeansd[,2])[1],multiple=TRUE),width=3),
        
        
        box( collapsible = TRUE,
             title='r square value',
             status='primary',solidHeader = TRUE, selectInput('d11', "r2 value",choices = levels(datmeansd[,6]),
                                                              selected=levels(datmeansd[,6])[1],multiple=TRUE),width=3)
      ),
     
      
      fluidRow(
        tabBox(width=12,
               tabPanel(title="Mean with sd",  
                        plotlyOutput("plotm")),
               
               tabPanel(title="Data table",  
                        dataTableOutput(outputId = "tableDT"),
                        downloadButton("downloadData", "Download")     ) ) 
        
      )
    )
  )
)

library(shinyWidgets)

ui<- dashboardPage(
  header= dashHeader,
  sidebar=dashSidebar,
  body=dashBody,
  title="Comparison of linkage disequilibrium estimated from genotypes vs. haplotypes for crossbred populations "
)
mytheme=theme(axis.text.x = element_text(angle = 90, hjust = 1,size=6),axis.text.y = element_text( hjust = 1,size=4),axis.text = element_text(size = 2),  
      axis.title.y = element_text(color="blue", size=4, face="bold"),           
      axis.title.x = element_text(color="blue", size=4, face="bold")
)


server<- function(input,output){
  output$myPanel <- renderUI({ 
    lev <- sort(unique(input$select)) # sorting so that "things" are unambigious
    cols <- gg_fill_hue(length(lev))
    
    # New IDs "colX1" so that it partly coincide with input$select...
    lapply(seq_along(lev), function(i) {
      colourInput(inputId = paste0("col", lev[i]),
                  label = paste0("Choose colour for ", lev[i]), 
                  value = cols[i]
      )        
    })
  })
  output$myPanel1 <- renderUI({ 
    lev2 <- sort(unique(input$select1)) # sorting so that "things" are unambigious
    cols2 <- gg_fill_hue(length(lev2))
    
    # New IDs "colX1" so that it partly coincide with input$select...
    lapply(seq_along(lev2), function(i) {
      colourInput(inputId = paste0("col2", lev2[i]),
                  label = paste0("Choose colour for ", lev2[i]), 
                  value = cols2[i]
      )        
    })
  })
  
  output$myPanel2 <- renderUI({ 
    lev3 <- sort(unique(input$select2)) # sorting so that "things" are unambigious
    cols3 <- gg_fill_hue(length(lev3))
    
    # New IDs "colX1" so that it partly coincide with input$select...
    lapply(seq_along(lev3), function(i) {
      colourInput(inputId = paste0("col3", lev3[i]),
                  label = paste0("Choose colour for ", lev3[i]), 
                  value = cols3[i]
      )        
    })
  })
  
  
  library(shinyjs)
  # useShinyjs()
  observeEvent(input$showh,
               show("txt")) # show() is shiny js function, pass the element/widget ID as the argument
  
  observeEvent(input$hideh,
               hide("txt"))
  
  
  output$plot1 <- renderPlotly({
    cols2 <- paste0("c(", paste0("input$col2", sort(input$select1), collapse = ", "), ")")
    # print(cols)
    cols2 <- eval(parse(text = cols2))
    
    
    # To prevent errors
    req(length(cols2) == length(input$select1))
    data11 <- reactive({
      datbiassd[(datbiassd[,6]%in% input$d1)&(datbiassd[,12]%in% input$select1),]
    })
    dataf<- data11()
    dataf[,6]<- droplevels(dataf[,6])
    dataf$pp<-as.factor(as.character(dataf$pp))
    dataf$sizemethod  <- factor(dataf$sizemethod )
    plots=ggplot(dataf, aes(x = pp, y = Bias, color =sizemethod  )) +
      geom_jitter()+   scale_color_manual(values=cols2)+theme_bw() +mytheme+
      
    labs(x = 'Parental allele freq.', y = "Bias from 1000 replicates")
    
    plotsb=plots+facet_wrap(~Dprime, ncol=1)
    
    
    
    
    
    ggplotly(plotsb) 
    
  })
  
  
  output$plot12 <- renderPlotly({
    cols2 <- paste0("c(", paste0("input$col2", sort(input$select1), collapse = ", "), ")")
   
    cols2 <- eval(parse(text = cols2))
   
    # To prevent errors
    req(length(cols2) == length(input$select1))
    data11 <- reactive({
      datbiassd[(datbiassd[,6]%in% input$d1)&(datbiassd[,12]%in% input$select1),]
    })
    dataf<- data11()
    dataf[,6]<- droplevels(dataf[,6])
    dataf$pp<-as.factor(as.character(dataf$pp))
    dataf$sizemethod  <- factor(dataf$sizemethod )
    plots=ggplot(dataf, aes(x = pp, y = Biaspvalue, color =sizemethod  )) +
      geom_jitter()+   scale_color_manual(values=cols2)+theme_bw() +mytheme+
      labs(x = 'Parental allele freq.', y = "Biasness from zero p value")+ geom_hline(yintercept = 0.05, color = "blue", size=1.5)
    
    plotsb=plots+facet_wrap(~Dprime, ncol=1)
    
    
    
    
    
    ggplotly(plotsb) 
    
  })
  
  
  output$plot2 <- renderPlotly({
    cols2 <- paste0("c(", paste0("input$col2", sort(input$select1), collapse = ", "), ")")

    cols2 <- eval(parse(text = cols2))
 
    # To prevent errors
    req(length(cols2) == length(input$select1))
    data33 <- reactive({
      datbiassd[(datbiassd[,6]%in% input$d1)&(datbiassd[,12]%in% input$select1),]
    })
    dataf<- data33()
    dataf[,6]<- droplevels(dataf[,6])
    dataf[,6]<- droplevels(dataf[,6])
    dataf$pp<-as.factor(as.character(dataf$pp))
    dataf$sizemethod  <- factor(dataf$sizemethod )
    plots=ggplot(dataf, aes(x = pp, y = sd, color = sizemethod)) +
      geom_point() +theme_bw( )+ mytheme+  scale_color_manual(values=cols2)+
    labs(x = 'Parental allele freq.', y = "Standard devation from 1000 replicates")
    
    plotssd=plots+facet_wrap(~Dprime, ncol=1)
    
    
    p1=ggplot(dataf, aes(x = as.factor(pp), y = sd, fill = as.factor(sizemethod ))) +
      geom_col(position= position_dodge(width=0.9))+
      scale_fill_manual(values=cols2)+ theme_bw( )+mytheme +
      
      labs(x = 'Parental allele freq.', y = "Standard devation of 1000 replicates") +
        facet_wrap(~Dprime, ncol=1)+  
guides(fill=guide_legend(" Sample size_LDmethod"))  # add gu
    
    
    ggplotly( plotssd)
    
  })
  data33 <- reactive({
    datbiassd[(datbiassd[,6]%in% input$d1)&(datbiassd[,11]%in% input$select1),c()]
  })
  
  output$tableDTbp <- DT::renderDataTable(datatable(reactive({
    names(datbiassd)[1]<- "sample size"
    names(datbiassd)[6]<- "r2 value"
    datbiassd[(datbiassd[,6]%in% input$d1)&(datbiassd[,12]%in% input$select1),c(12,4,6,8,9,10,11)]
  })(),
  options = list(paging=F),
  rownames=F,
  filter = "top") %>% 
    formatStyle("sizemethod", color = "gray") %>%
    #formatStyle("pAvals", color = "skyblue") %>%
    # formatStyle("pCvals", color = "gray") %>%
    formatStyle("pp", color = "gray") %>%
    formatStyle("r2 value", color = "skyblue") %>%
    formatStyle("method", color = "skyblue") %>%
    #formatStyle("Method", color = "salmon") %>%
    formatStyle("sd", color = "skyblue") %>%
    formatStyle("Bias", color = "skyblue",
                transform = "rotateX(20deg) rotateY(5deg) rotateZ(5deg)"
    ) %>%formatStyle("Biaspvalue", color = "blue",
                     transform = "rotateX(25deg) rotateY(10deg) rotateZ(7deg)"
    ) )

  output$downloadDatabp <- downloadHandler(
    filename = function() {
      paste('Biasprecission', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datbiassd[(datbiassd[,6]%in% input$d1)&(datbiassd[,12]%in% input$select1),c(12,4,6,8,9,10,11)], file, row.names = FALSE)
    }
  )
  
  
data21<- reactive({
  newdat[(newdat[,6]%in% input$d1),]
})

newdatmsdratio$sdratio<- signif(newdatmsdratio$sdratio,2)
newdatmsdratio=newdatmsdratio %>% filter(pAvals%in%c(0.05, 0.15 ,0.25 ,0.35,0.45),pCvals%in%c(0.05, 0.15, 0.25, 0.35, 0.45))
data55 <- reactive({
  newdatmsdratio[(newdatmsdratio[,6]%in% input$dr)&(newdatmsdratio[,1]%in% input$select2),]
})
output$tableDTr <- DT::renderDataTable(datatable(reactive({
 
  
  # To prevent errors
  #req(length(cols3) == length(input$select2))
  names(newdatmsdratio)[1]<- "sample size"
  names(newdatmsdratio)[6]<- "r2 value"
  newdatmsdratio[(newdatmsdratio[,6]%in% input$dr)&(newdatmsdratio[,1]%in% input$select2),c(1,4,6,7,8)]
})(),
options = list(paging=F),
rownames=F,
filter = "top") %>% 
  formatStyle("sample size", color = "gray") %>%
  #formatStyle("pAvals", color = "skyblue") %>%
  # formatStyle("pCvals", color = "gray") %>%
  formatStyle("pp", color = "gray") %>%
  formatStyle("r2 value", color = "skyblue") %>%
  formatStyle("sdratio", color = "skyblue",
              transform = "rotateX(20deg) rotateY(5deg) rotateZ(5deg)"
  ) %>% formatStyle("corhapgeno", color = "blue",
                    transform = "rotateX(30deg) rotateY(8deg) rotateZ(9deg)"
  ))
data55 <- reactive({
  newdatmsdratio[(newdatmsdratio[,6]%in% input$dr)&(newdatmsdratio[,1]%in% input$select2),]
})
output$downloadData0 <- downloadHandler(
  filename = function() {
    paste('Precissiondata', ".csv", sep = "")
  },
  content = function(file) {
    write.csv(data55()[,c(1,4,6,7,8)], file, row.names = FALSE)
  }
)
output$plot3 <- renderPlotly({
  
  cols3 <- paste0("c(", paste0("input$col3", sort(input$select2), collapse = ", "), ")")
  cols3 <- eval(parse(text = cols3))
  
  # To prevent errors
  req(length(cols3) == length(input$select2))
  data55 <- reactive({
    newdatmsdratio[(newdatmsdratio[,6]%in% input$dr)&(newdatmsdratio[,1]%in% input$select2),]
  })
  
  dataf<- data55()
  dataf$pp<-as.factor(as.character(dataf$pp))
  dataf$NCD <- factor(dataf$NCD)
  
  plots=ggplot(dataf, aes(x = as.factor(as.character(pp)), y = sdratio, fill = factor(NCD)))+ geom_col(position= position_dodge(width=0.75),width=0.75)+
    scale_fill_manual(values=cols3)+      theme_bw( )+mytheme+xlab('Parental allele freq.')+ylab('Ratio of precision')
  plotsr=plots+facet_wrap(~Dprime, ncol=1)+
    guides(fill=guide_legend("Sample size")) + coord_cartesian(ylim=c(min(  dataf$sdratio),1.1))+
    geom_text(aes(label=sdratio),size=1.25, position=position_dodge(width=0.8), vjust=-0.25)
 ggplotly(plotsr)
  
})

## The parameter estimates with Table
names(datmeansd)[1]<- 'sizemethod'
names(datmeansd)[6]='Truer2value'
datam <- reactive({
  datmeansd[(datmeansd[,1]%in% input$n1)&(datmeansd[,2]%in% input$p11)&(datmeansd[,6]%in% input$d11),]
})
output$plot32 <- renderPlotly({
  
  cols3 <- paste0("c(", paste0("input$col3", sort(input$select2), collapse = ", "), ")")
  cols3 <- eval(parse(text = cols3))
  
  # To prevent errors
  req(length(cols3) == length(input$select2))
  data55 <- reactive({
    newdatmsdratio[(newdatmsdratio[,6]%in% input$dr)&(newdatmsdratio[,1]%in% input$select2),]
  })
  
  dataf<- data55()
  dataf$pp<-as.factor(as.character(dataf$pp))
  dataf$NCD <- factor(dataf$NCD)
  
  plots=ggplot(dataf, aes(x = as.factor(as.character(pp)), y = corhapgeno, fill = factor(NCD)))+ geom_col(position= position_dodge(width=0.75),width=0.75)+
    scale_fill_manual(values=cols3)+      theme_bw( )+mytheme+xlab('Parental allele freq.')+ylab('cor hap to geno')
  plotsr=plots+facet_wrap(~Dprime, ncol=1)+
    guides(fill=guide_legend("Sample size")) + coord_cartesian(ylim=c(min(  dataf$corhapgeno),1.1))+
    geom_text(aes(label=corhapgeno),size=1.25, position=position_dodge(width=0.8), vjust=-0.25)
  ggplotly(plotsr)
  
})

output$tableDT <- DT::renderDataTable(datatable(reactive({
  cols <- paste0("c(", paste0("input$col", sort(input$select), collapse = ", "), ")")
  # print(cols)
  cols <- eval(parse(text = cols))
  # print(cols)
  
  # To prevent errors
  req(length(cols) == length(input$select))
  names(datmeansd)[8]='Predictedr2value'
  
 
    datmeansd[(datmeansd[,2]%in% input$p11)&(datmeansd[,1]%in% input$select)&(datmeansd[,6]%in% input$d11),c(1,2,4,5,6,8,9)]
  

  
})(),
options = list(paging=F),
rownames=F,
filter = "top") %>% 
  formatStyle("sizemethod", color = "skyblue") %>%
  formatStyle("pAvals", color = "skyblue") %>%
  # formatStyle("pCvals", color = "gray") %>%
  formatStyle("pp", color = "skyblue") %>%
  formatStyle("pc", color = "skyblue") %>%
  formatStyle('Truer2value', color = "skyblue") %>%
  #formatStyle("Method", color = "salmon") %>%
  formatStyle("Predictedr2value", color = "skyblue") %>%
  formatStyle("sd", color = "skyblue",
              transform = "rotateX(20deg) rotateY(5deg) rotateZ(5deg)"
  ))

output$downloadData <- downloadHandler(
  filename = function() {
    paste('MeanandBiasdata', ".csv", sep = "")
  },
  content = function(file) {
    write.csv(datmeansd[(datmeansd[,2]%in% input$p11)&(datmeansd[,1]%in% input$select)&(datmeansd[,6]%in% input$d11),c(1,2,4,5,6,8,9)], file, row.names = FALSE)
  }
)
output$plotm <- renderPlotly({
  cols <- paste0("c(", paste0("input$col", sort(input$select), collapse = ", "), ")")
  # print(cols)
  cols <- eval(parse(text = cols))
  # print(cols)
  
  # To prevent errors
  req(length(cols) == length(input$select))
  
  
  datam <- reactive({
    datmeansd[(datmeansd[,2]%in% input$p11)&(datmeansd[,1]%in% input$select)&(datmeansd[,6]%in% input$d11),]
  })
  
  dataf<- datam()
  maxval=dataf$r2value +dataf$sd
  minval=dataf$r2value - dataf$sd
  limits=aes(ymax = maxval, ymin=minval)
  
  plots=ggplot(dataf, aes(fill=sizemethod, y=r2value, x=pp))+ geom_col(position= position_dodge(width=0.9))+geom_errorbar(limits, position= position_dodge(width=0.9), width=0.8) +
    mytheme+theme_bw( )+xlab('Parental allele freq.')+ylab('r^2 value')+    scale_fill_manual(values=cols)
  plots=plots+facet_wrap(~Truer2value, ncol=1)
  #ggplotly(plots)
  plots
  
  
})


}




shinyApp(ui = ui, server = server)#
