library(shinydashboard)
library(shinyjs)
library(shiny)
library(RCurl)
library(DT)
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(shinyWidgets)
library(rsconnect)
library(curl)
 
 

##Preparation donnee---------------
#url <- "https://raw.githubusercontent.com/astaquet/mangrove/main/datapropagule.txt"
url <- "https://raw.githubusercontent.com/astaquet/mangrove/ab9e5a720f083d109def7118054d6734fb34b892/datapropagule.txt"
 
#data <- read.csv2( text=readLines(url, warn = FALSE), h=T,sep="\t",dec=",")

data <- read.csv2( url, h=T,sep="\t",dec=",", na.strings = "NA")

url_rpot <- "https://raw.githubusercontent.com/astaquet/mangrove/main/racine_pot"
data_rpot<- read.csv2( url_rpot, h=T,sep="\t",dec=",", na.strings = "NA")

url_macouria <- "https://raw.githubusercontent.com/astaquet/mangrove/main/racine_macouria"
data_macouria<- read.csv2( url_macouria, h=T,sep="\t",dec=",", na.strings = "NA")

url_rac_macouria <- "https://raw.githubusercontent.com/astaquet/mangrove/main/vol_rac_macouria"
data_rac_macouria<- read.csv2( url_rac_macouria, h=T,sep="\t",dec=",", na.strings = "NA")


str(data_rac_macouria)
str(data_rpot)

data_macouria$Zone=as.factor(data_macouria$Zone)
data_rpot$Individu=as.factor(data_rpot$Individu)
data_rpot$Noeud    =as.numeric(data_rpot$Noeud    )
data_rpot$Taille    =as.numeric(data_rpot$Taille    )
data_rpot$Feuille    =as.numeric(data_rpot$Feuille    )

data$BAC=as.factor(data$BAC)

##Mise en forme des tableaux en variables lignes/colonnes
df1 <- data %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, C1,C2,C3,C4,C5) %>%
  gather(key = "variable", value = "hauteur", c(-H_cumul,-Traitement,-Ligne,-BAC))


df2 <- data %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, F1,F2,F3,F4,F5)

colnames(df2)<-c("H_cumul","Traitement","BAC","Ligne", "C1","C2","C3","C4","C5")


df2 <- df2 %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, C1,C2,C3,C4,C5) %>%
  gather(key = "variable", value = "nb_feuille", c(-H_cumul,-Traitement,-Ligne,-BAC))


df3 <- data %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, CD1,	CD2,	CD3,	CD4,	CD5)

colnames(df3)<-c("H_cumul","Traitement","BAC","Ligne", "C1","C2","C3","C4","C5")

df3 <- df3 %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, C1,C2,C3,C4,C5) %>%
  gather(key = "variable", value = "Nb_cotyledon", c(-H_cumul,-Traitement,-Ligne,-BAC))

df4 <- data %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, T1,	T2,	T3,	T4,	T5)

colnames(df4)<-c("H_cumul","Traitement","BAC","Ligne", "C1","C2","C3","C4","C5")

df4 <- df4 %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, C1,C2,C3,C4,C5) %>%
  gather(key = "variable", value = "Nb_Tige", c(-H_cumul,-Traitement,-Ligne,-BAC))


df5 <- data %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, S1,	S2,	S3,	S4,	S5)

colnames(df5)<-c("H_cumul","Traitement","BAC","Ligne", "C1","C2","C3","C4","C5")

df5 <- df5 %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, C1,C2,C3,C4,C5) %>%
  gather(key = "variable", value = "Developpement",c(-H_cumul,-Traitement,-Ligne,-BAC))


df6 <- data %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, I1,	I2,	I3,	I4,	I5)

colnames(df6)<-c("H_cumul","Traitement","BAC","Ligne", "C1","C2","C3","C4","C5")

df6 <- df6 %>%
  dplyr::select(H_cumul,Traitement,BAC,Ligne, C1,C2,C3,C4,C5) %>%
  gather(key = "variable", value = "Individu",c(-H_cumul,-Traitement,-Ligne,-BAC))

df5=na.omit(df5)


dfa=left_join(df1, df2) %>%
  dplyr::select(H_cumul,Traitement,BAC, Ligne,variable, hauteur,nb_feuille)


dfb=left_join(dfa,df3) %>%
  dplyr::select(H_cumul,Traitement,BAC, Ligne,variable, hauteur,nb_feuille,Nb_cotyledon)


dfc=left_join(dfb,df4) %>%
  dplyr::select(H_cumul,Traitement,BAC, Ligne,variable, hauteur,nb_feuille,Nb_cotyledon,Nb_Tige)

dfd=left_join(dfc,df5) %>%
  dplyr::select(H_cumul,Traitement,BAC, Ligne,variable, hauteur,nb_feuille,Nb_cotyledon,Nb_Tige,Developpement)

dfe=left_join(dfd,df6) %>%
  dplyr::select(H_cumul,Traitement,BAC,Individu, Ligne,variable, hauteur,nb_feuille,Nb_cotyledon,Nb_Tige,Developpement)

df=left_join(dfd, df6) %>%
  dplyr::select(H_cumul,Traitement,BAC,Individu, Ligne,variable, hauteur,nb_feuille,Nb_cotyledon,Nb_Tige,Developpement)

df$Jour<-round(df$H_cumul/24)



dshiny<-df 
dshiny$Traitement=as.factor(dshiny$Traitement)
dshiny$BAC=as.factor(dshiny$BAC)
dshiny$Developpement=as.factor(dshiny$Developpement)

# Define UI for application  --------------------
ui <- dashboardPage(
 
  
  dashboardHeader(title ="A. germinans"),
  dashboardSidebar( sidebarMenu(
    menuItem("Croissance", tabName = "Croissance", icon = icon("dashboard")),
    menuItem("Developpement", tabName = "Developpement", icon = icon("th")),
    menuItem("Macouria", tabName = "Macouria", icon = icon("th")),
    menuItem("Racine", tabName = "Racine", icon = icon("th")),
    menuItem("Volume_Macouria", tabName = "Volume_Macouria", icon = icon("th"))
    
  )),
  dashboardBody(
    tabItems( 
      ###Croissance---------------------
      
      tabItem(tabName ="Croissance",
              
              fluidRow(
                
                
                box(title = "Inputs", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE, 
                    inputPanel(
                      selectInput('x', 'X', choices = c("H_cumul","BAC","Ligne", "variable" ,"hauteur","nb_feuille",  "Nb_cotyledon" , "Nb_Tige" ,"Developpement", "Individu","Jour"),
                                  selected = "Jour"),
                      selectInput('y', 'Y', choices = c("H_cumul","BAC","Ligne", "variable" ,"hauteur","nb_feuille",  "Nb_cotyledon" , "Nb_Tige" ,"Developpement", "Individu","Jour"), 
                                  selected = "hauteur"),
                      selectInput('z', 'Groupe', choices = c( "BAC", "Individu"), 
                                  selected = "BAC")
                    )
                ),
                box(  title = "Inputs/2", status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,  
                      
                      
                      # this is our group checkbox
                      checkboxGroupInput(
                        inputId = 'BACPLANT',
                        label = 'Selectionner le Bac',
                        choices = unique(dshiny$BAC)
                      ),
                      
                      # this is how we will set the state of "all" or "none"
                      checkboxInput('toggle_cyl', "All" )
                      ,
                      checkboxInput(inputId="delgado", label="Tendance", value = FALSE),
                      checkboxInput(inputId="density", label="2e variable de dist./hist (champ Y)", value = FALSE) ),
                
                box(radioGroupButtons(
                  inputId = "change_plot",
                  label = "Type de plot :",
                  choices = c(
                    `<i class='fa fa-line-chart'></i>` = "Point",
                    `<i class='fa fa-area-chart'></i>` = "Density",
                    `<i class='fa fa-align-left'></i>` = "Barchart",
                    `<i class='fa fa-minus-square-o'></i>` = "Boxplot",
                    `<i class='fa fa-bar-chart'></i>` = "Hist"
                    
                  ),
                  justified = TRUE,
                  selected = "Point"
                ))
               
                
              ),
              
          
              
              fluidRow(                    
                box(title = "Croissance de la tige d'Avicennia germinans", width = '100%',solidHeader = TRUE,
                    collapsible = TRUE, status = "primary", plotOutput("plot", brush = "plot_brush"))
            
                
               
                
                ),
              
              
              tableOutput("data")
              
              
              
      ),
      ###Developpement---------------------
      
      tabItem(tabName ="Developpement",
              fluidRow(
                
                
                box(title = "Inputs", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE,  
                    # this is our group checkbox
                    checkboxGroupInput(
                      inputId = 'TRAITPLANT',
                      label = 'Selectionner le Lieu',
                      choices = unique(dshiny$Traitement)
                    ),
                    
                    # this is how we will set the state of "all" or "none"
                    checkboxInput('toggle_cyl3', "All" )
                )
                ,box( title = "Inputs/2", status = "warning", solidHeader = TRUE,
                      collapsible = TRUE, 
                      # this is our group checkbox
                      checkboxGroupInput(
                        inputId = 'BACPLANT2',
                        label = 'Selectionner le Bac',
                        choices = unique(dshiny$BAC)
                      ),
                      
                      # this is how we will set the state of "all" or "none"
                      checkboxInput('toggle_cyl2', "All" )
                )
                
              ),
              
              
              fluidRow(                    
                box( title = "Stade de developpement d'A. germinans", width = '100%',solidHeader = TRUE,
                     collapsible = TRUE, status = "primary", plotOutput("plot2" ))) 
              
              
              
      ),
      
      ###Macouria---------------------
      
      tabItem(tabName ="Macouria",
              
              fluidRow(
                
                
                box(title = "Inputs", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE, 
                    inputPanel(
                      selectInput('xm2', 'X', choices = c("DB30","DB130","DRC", "DRL" ,"Epaisseur_racine","DB30_Anastomose ",  "Epaisseur_Racine_A" , "Individu" ,"Zone"),
                                  selected = "DB30"),
                      selectInput('ym2', 'Y', choices = c("DB30","DB130","DRC", "DRL" ,"Epaisseur_racine","DB30_Anastomose ",  "Epaisseur_Racine_A" , "Individu" ,"Zone"), 
                                  selected = "DRL"),
                      selectInput('zm2', 'Groupe', choices = c( "Zone", "Individu"), 
                                  selected = "Zone")
                    )
                ),
                box(  title = "Inputs/2", status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,  
                      
                      
                      # this is our group checkbox
                      checkboxGroupInput(
                        inputId = 'BACPLANTm2',
                        label = 'Selectionner la zone',
                        choices = unique(data_macouria$Zone)
                      ),
                      
                      # this is how we will set the state of "all" or "none"
                      checkboxInput('toggle_cylm2', "All" ),
                      checkboxInput(inputId="tendancem2", label="Tendance", value = FALSE),
                      checkboxInput(inputId="densitym2", label="2e variable de dist./hist (champ Y)", value = FALSE),
                      
                 ),
                box(radioGroupButtons(
                  inputId = "change_plotm2",
                  label = "Type de plot :",
                  choices = c(
                    `<i class='fa fa-line-chart'></i>` = "Point",
                    `<i class='fa fa-area-chart'></i>` = "Density",
                    `<i class='fa fa-align-left'></i>` = "Barchart",
                    `<i class='fa fa-minus-square-o'></i>` = "Boxplot",
                    `<i class='fa fa-bar-chart'></i>` = "Hist"
                    
                  ),
                  justified = TRUE,
                  selected = "Point"
                ))
                
              ),
              
              fluidRow(                    
                
              ),
              
              fluidRow(                    
                box(title = "Macouria", width = '100%',solidHeader = TRUE,
                    collapsible = TRUE, status = "primary", plotOutput("plotm2", brush = "plot_brushm2"))
                
                ),
              
              
              tableOutput("datam2")
              
              
              
      ),
   ###RACINE---------------------
         tabItem(tabName ="Racine",
              
              fluidRow(
                
                
                box(title = "Inputs", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE, 
                    inputPanel(
                      selectInput('xR2', 'X', choices = c("Individu","Noeud","Taille", "Diametre" ,"Feuille"),
                                  selected = "Feuille"),
                      selectInput('yR2', 'Y', choices = c("Individu","Noeud","Taille", "Diametre" ,"Feuille"), 
                                  selected = "Noeud"),
                      selectInput('zR2', 'Groupe', choices = c("Individu"), 
                                  selected = "Individu")
                    )
                    ,
                    ),
                box(  title = "Inputs/2", status = "warning", solidHeader = TRUE,
                      collapsible = TRUE,  
                      
                      
                      # this is our group checkbox
                      # checkboxGroupInput(
                      # inputId = 'BACPLANTR2',
                      #  label = 'Selectionner la zone',
                      # choices = unique(data_rpot$Individu)
                      #),
                      
                      # this is how we will set the state of "all" or "none"
                      # checkboxInput('toggle_cylR2', "All" ),
                      checkboxInput(inputId="tendanceR2", label="Tendance", value = FALSE),
                      checkboxInput(inputId="densityR2", label="2e variable de dist./hist (champ Y)", value = FALSE),
                      
                ),
                
                box(radioGroupButtons(
                  inputId = "change_plotR2",
                  label = "Type de plot :",
                  choices = c(
                    `<i class='fa fa-line-chart'></i>` = "Point",
                    `<i class='fa fa-area-chart'></i>` = "Density",
                    `<i class='fa fa-align-left'></i>` = "Barchart",
                    `<i class='fa fa-minus-square-o'></i>` = "Boxplot",
                    `<i class='fa fa-bar-chart'></i>` = "Hist"
                    
                  ),
                  justified = TRUE,
                  selected = "Point"
                ))
                
              ),
              
              
              fluidRow(                    
                box(title = "Racine", width = '100%',solidHeader = TRUE,
                    collapsible = TRUE, status = "primary", plotOutput("plotR2", brush = "plot_brushR2"))
                
                
                ),
              
              
              tableOutput("dataR2")
              
              
              
      )
    
      ,
   
   ###MACOURIAa VOLUME--------
      tabItem(tabName ="Volume_Macouria",
              
              fluidRow(
                
                
                box(title = "Inputs", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE, 
                    inputPanel(
                      selectInput('xV2', 'X', choices = c("ID","Length","D1  ", "D2" ,"Mass","Dmean","Vcyl","Vwater"),
                                  selected = "Length"),
                      selectInput('yV2', 'Y', choices = c("ID","Length","D1  ", "D2" ,"Mass","Dmean","Vcyl","Vwater",NULL), 
                                  selected = "Mass") 
                    )
                    ,
                )
                
              ,
              box(  title = "Inputs/2", status = "warning", solidHeader = TRUE,
                    collapsible = TRUE,  
                    
                    
                    # this is our group checkbox
                    checkboxGroupInput(
                      inputId = 'BACPLANTV2',
                      label = 'Selectionner la zone',
                      choices = unique(data_rac_macouria$Individu)
                    ),
                    
                    # this is how we will set the state of "all" or "none"
                    checkboxInput('toggle_cylV2', "All" ),
                    checkboxInput(inputId="tendanceV2", label="Tendance", value = FALSE),
                    checkboxInput(inputId="densityV2", label="2e variable de dist./hist (champ Y)", value = FALSE),

                    
              ),
              
              box(radioGroupButtons(
                inputId = "change_plotV2",
                label = "Type de plot :",
                choices = c(
                  `<i class='fa fa-line-chart'></i>` = "Point",
                  `<i class='fa fa-area-chart'></i>` = "Density",
                  `<i class='fa fa-align-left'></i>` = "Barchart",
                  #`<i class='fa fa-minus-square-o'></i>` = "Boxplot"
                  `<i class='fa fa-bar-chart'></i>` = "Hist"
                  
                ),
                justified = TRUE,
                selected = "Point"
              ))   
              
              
              ),
              
              
              fluidRow(                    
                box(title = "Volume", width = '100%',solidHeader = TRUE,
                    collapsible = TRUE, status = "primary", plotOutput("plotV2", brush = "plot_brushV2"))
                
                
              ),
              
              
              tableOutput("dataV2")
              
 
      )
      
      
      
    )
  )
)

## Define server logic required to draw a histogram-----
server <- function(input, output, session) {
  
  observeEvent(input$toggle_cyl, {
    if (input$toggle_cyl)
      updateCheckboxGroupInput(session, 'BACPLANT', selected = unique((dshiny$BAC)))
    else
      updateCheckboxGroupInput(session, 'BACPLANT', selected =  "1" )
  })
  
  
  output$plot <- renderPlot({
    
    
    if (input$change_plot %in% "Point") 
    { 
		dshiny %>%
        dplyr::filter(BAC %in% input$BACPLANT) %>%
        ggplot(aes_string(y=input$y, x = input$x)) +
        geom_point(aes_string(color = input$z,pch = input$z),lwd=2)+
        #ggtitle("Croissance de la tige d'Avicennia germinans")+
        {if(input$delgado)geom_abline( intercept = 3.481e-14, slope=5,  color="red")}+
        scale_color_manual(values = c(1:53)) + geom_smooth(aes_string(color = input$z),se = F) + theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
      
    } else if  (input$change_plot %in% "Density") 
    {  
      
      dshiny %>%
        dplyr::filter(BAC %in% input$BACPLANT) %>%
        ggplot(aes_string( x = input$x)) +
        geom_density(aes_string(color = input$z,lty = input$z))+
        ggtitle("Distribution")+
        
         {if(input$density)geom_density(aes_string( x = input$y)) }+         
		 scale_color_manual(values = c(1:53)) + theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
     
    }else if  (input$change_plot %in% "Barchart") 
    { 
      
        dshiny %>%
        dplyr::filter(BAC %in% input$BACPLANT) %>%
        ggplot(aes_string(y=input$y, x = input$x)) +
        geom_bar(stat="identity",aes_string(fill = input$z)) +
            scale_color_manual(values = c(1:53))      + 
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
      
      
       
      
    }
    else if  (input$change_plot %in% "Hist")    { 
      
      
      
      
      dshiny %>%
        dplyr::filter(BAC %in% input$BACPLANT) %>%
        ggplot(aes_string( x = input$x)) +
        geom_histogram(aes_string(color = input$z, fill = input$z), alpha=0.4)+
        {if(input$density)geom_histogram(aes_string( x = input$y),fill="blue",alpha=0.4) }+
      scale_color_manual(values = c(1:53))  + theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
      
      
        
    } else {
      
      dshiny %>%
        dplyr::filter(BAC %in% input$BACPLANT) %>%
        ggplot(aes_string(y=input$y, x = input$x)) +
        geom_boxplot(aes_string(  fill = input$z)) +
        scale_color_manual(values = c(1:53))   + theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
    }
    

    
  })
  
  
  output$data <- renderTable({
    brushedPoints(dshiny, input$plot_brush)
  })
  
  ###SERVER Developpement-------
  
  
  observeEvent(input$toggle_cyl3, {
    if (input$toggle_cyl3)
      updateCheckboxGroupInput(session, 'TRAITPLANT', selected = unique((dshiny$Traitement)))
    else
      updateCheckboxGroupInput(session, 'TRAITPLANT', selected =  "IRD_012022" )
  })
  
  observeEvent(input$toggle_cyl2, {
    if (input$toggle_cyl2)
      updateCheckboxGroupInput(session, 'BACPLANT2', selected = unique((dshiny$BAC)))
    else
      updateCheckboxGroupInput(session, 'BACPLANT2', selected =  "1" )
  })
  
  
  
  output$plot2 <- renderPlot({
    dshiny %>%
      
      dplyr::filter(!is.na(Developpement))%>%
      dplyr::filter(Traitement %in% input$TRAITPLANT) %>%
      dplyr::filter(BAC %in% input$BACPLANT2) %>%
      ggplot(aes(Jour, colour = Developpement)) +
      stat_ecdf(geom = "point",lwd=2,na.rm = FALSE,)+
      stat_ecdf(geom = "line",lwd=1) +
      scale_x_continuous(name="Duree (en jour)", breaks=seq(0,1000,2))+ theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
  })
  
  
  
  ###SERVER   MACOURIA-------
  
  
  observeEvent(input$toggle_cylm2, {
    if (input$toggle_cylm2)
      updateCheckboxGroupInput(session, 'BACPLANTm2', selected = unique((data_macouria$Zone)))
    else
      updateCheckboxGroupInput(session, 'BACPLANTm2', selected =  "1" )
  })
  
  
  output$plotm2 <- renderPlot({
    
    if (input$change_plotm2 %in% "Point") 
    { 
      
      data_macouria %>%
        dplyr::filter(Zone %in% input$BACPLANTm2) %>%
        ggplot(aes_string(y=input$ym2, x = input$xm2)) +
        geom_point(aes_string(color = input$zm2,pch = input$zm2),lwd=2)+
         {if(input$tendancem2)geom_smooth(aes_string(),se = F)}+
        scale_color_manual(values = c(1:53)) + 
         theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
       
      
    } else if  (input$change_plotm2 %in% "Density") 
    {  
      
      data_macouria %>%
        dplyr::filter(Zone %in% input$BACPLANTm2) %>%
        ggplot(aes_string(  x = input$xm2)) +
        geom_density(aes_string(color = input$zm2,lty = input$zm2),lwd=2)+
        ggtitle("Distribution")+
        
        {if(input$tendancem2)geom_smooth(aes_string(),se = F)}+
        scale_color_manual(values = c(1:53)) + 
        {if(input$densitym2)geom_density(aes_string( x = input$ym2)) }+          theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
    
    }else if  (input$change_plotm2 %in% "Barchart") 
    { 
      
      data_macouria %>%
        dplyr::filter(Zone %in% input$BACPLANTm2) %>%
        ggplot(aes_string(y=input$ym2,  x = input$xm2)) +
        geom_bar(stat="identity",aes_string(fill = input$zm2)) +
         scale_color_manual(values = c(1:53)) + 
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal") 
         }
    else if  (input$change_plotm2 %in% "Hist")    { 
      
      
      data_macouria %>%
        dplyr::filter(Zone %in% input$BACPLANTm2) %>%
        ggplot(aes_string(  x = input$xm2)) +
        geom_histogram(aes_string(color = input$zm2,fill = input$zm2), alpha=0.4 )+
        {if(input$densitym2)geom_histogram(aes_string( x = input$ym2),fill="blue",alpha=0.4) }+    scale_color_manual(values = c(1:53)) + 
                  theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
    } else {
      
      data_macouria %>%
        dplyr::filter(Zone %in% input$BACPLANTm2) %>%
        ggplot(aes_string(y=input$ym2,  x = input$xm2)) +
        geom_boxplot( aes_string(fill = input$zm2)) +
        scale_color_manual(values = c(1:53)) + 
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal") 
      
       
          
    }
    
     
  })
  
  
  output$datam2 <- renderTable({
    brushedPoints(data_macouria, input$plot_brushm2)
  })
  
  
  
  
  #observeEvent(input$toggle_cylR2, {
  #  if (input$toggle_cylR2)
  #    updateCheckboxGroupInput(session, 'BACPLANTR2', selected = unique((data_rpot$Zone)))
  #  else
  #    updateCheckboxGroupInput(session, 'BACPLANTR2', selected =  NULL )
  # })
  
  ###SERVER RACINE-------
  
  
  
  
  output$plotR2 <- renderPlot({
    
    
    
    if (input$change_plotR2 %in% "Point") 
    {  data_rpot %>%
        ggplot(aes_string(y=input$yR2, x = input$xR2)) +
        geom_point(aes_string(color = input$zR2,pch = input$zR2),lwd=2)+
        #ggtitle("Croissance de la tige d'Avicennia germinans")+
        {if(input$tendanceR2)geom_smooth(aes_string(),se = F) }+
        scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
      
    } else if  (input$change_plotR2 %in% "Density") 
    {  
      
      data_rpot %>%
        ggplot() +
        geom_density(aes_string( x = input$xR2))+
        ggtitle("Distribution")+
        {if(input$tendanceR2)geom_smooth(aes_string(),se = F) }+
        {if(input$densityR2)geom_density(aes_string( x = input$yR2)) }+
        scale_color_manual(values = c(1:53)) + 
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
      
      
    }else if  (input$change_plotR2 %in% "Barchart") 
    { 
      data_rpot %>%
        ggplot(aes_string(y=input$yR2, x = input$xR2)) +
        geom_bar(stat="identity") +
        #ggtitle("Croissance de la tige d'Avicennia germinans")+
        {if(input$tendanceR2)geom_smooth(aes_string(),se = F) }+
        scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
        
    }
    else if  (input$change_plotR2 %in% "Hist")    { 
      
      data_rpot %>%
        ggplot(aes_string( x = input$xR2)) +
        geom_histogram( alpha=0.4)+
         {if(input$tendanceR2)geom_smooth(aes_string(),se = F) }+
        {if(input$densityR2)geom_histogram(aes_string( x = input$yR2),fill="blue",alpha=0.4) }+
        scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
      
      
    } else {
      
      data_rpot %>%
        ggplot(aes_string(y=input$yR2, x = as.factor(input$xR2))) +
        geom_boxplot() +
        ggtitle("Boxplot")+
         scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
       
    }
     
  
  })
  
  
  output$dataR2 <- renderTable({
    brushedPoints(data_rpot, input$plot_brushR2)
  })
  
  
###SERVER VOL MACOURIA-------
  
  observeEvent(input$toggle_cylV2, {
    if (input$toggle_cylV2)
      updateCheckboxGroupInput(session, 'BACPLANTV2', selected = unique((data_rac_macouria$ID)))
    else
      updateCheckboxGroupInput(session, 'BACPLANTV2', selected =  NULL )
  })
  
  
  
  
  
  output$plotV2 <- renderPlot({
    
    
    if (input$change_plotV2 %in% "Point") 
      {
      data_rac_macouria %>%
        ggplot(aes_string(y=input$yV2, x = input$xV2)) +
        geom_point(aes_string(color = input$zV2,pch = input$zV2),lwd=2)+
        #ggtitle("Croissance de la tige d'Avicennia germinans")+
        {if(input$tendanceV2)geom_smooth(aes_string(),se = F) }+
        scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
    } else if  (input$change_plotV2 %in% "Density") 
    { 
      data_rac_macouria %>%
        ggplot(aes_string( x = input$xV2)) +
        geom_density(aes_string( x = input$xV2))+
        
        ggtitle("Distribution")+
        {if(input$tendanceV2)geom_smooth(aes_string(),se = F) }+
        {if(input$densityV2)geom_density(aes_string( x = input$yV2)) }+
        scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
    }else if  (input$change_plotV2 %in% "Barchart") 
    { 
      data_rac_macouria %>%
        ggplot(aes_string(y=input$yV2, x = input$xV2)) +
        
        geom_bar(stat="identity") +
        
        #ggtitle("Croissance de la tige d'Avicennia germinans")+
         
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
    }
      else      { 
      data_rac_macouria %>%
        ggplot(aes_string( x = input$xV2)) +
        geom_histogram( alpha=0.4)+
        {if(input$densityV2)geom_histogram(aes_string( x = input$yV2),fill="blue",alpha=0.4) }+
          
        #ggtitle("Croissance de la tige d'Avicennia germinans")+
         scale_color_manual(values = c(1:53)) + 
        
        theme(panel.grid.major = element_line(linetype = "dashed"),  legend.title = element_text(family = "mono"), plot.background = element_rect(fill = "antiquewhite1"),  legend.position = "bottom", legend.direction = "horizontal")
    }
 
    
  })
      output$dataV2 <- renderTable({
    brushedPoints(data_rac_macouria, input$plot_brushV2)
  })
  
}

# Run the application -----------
shinyApp(ui = ui, server = server)

