#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


setwd('C:/Users/Adrien/Desktop/projet marel')


###Chargement des jeux de données
data = read.table("0_JeuDonneesTests_MARELCArnot_2005_2009-1.csv",dec = "," , header = T, sep=";")

###Observation des données
summary(data)
names(data)

###Mise en forme des dates

#Conversion du format des données dates
Datesnum1 = strptime(data$Time,"%d/%m/%Y %H:%M")

#Fusion avec variable date
data2=cbind(data,Datesnum1)

######Traitement des données

#Selection 2007_2008
data3=data2[data2$Datesnum1>="2007-01-01 00:20:00" & data2$Datesnum1<="2008-12-31 23:40:00",]
str(data3)

#Suppression variable avec trop de NA
data4=data3[,c(-2,-4,-5)]
str(data4)



###Création sous-groupe 2007 et 2008

data2007=data4[data4$Datesnum1>="2007-01-01 00:20:00" & data4$Datesnum1<="2007-12-31 23:40:00",]
data2008=data4[data4$Datesnum1>="2008-01-01 00:20:00" & data4$Datesnum1<="2008-12-31 23:40:00",]



###Création variable factor Mois pour 2007
#Janvier
Janvier=data2007[data2007$Datesnum1>="2007-01-01 00:20:00"& data2007$Datesnum1<="2007-01-31 23:40:00",]
Mois=rep("Janvier",nrow(Janvier))
Janvier2=cbind(Mois,Janvier)

#Fevrier
Fevrier=data2007[data2007$Datesnum1>="2007-02-01 00:20:00"& data2007$Datesnum1<="2007-02-28 23:40:00",]
Mois=rep("Février",nrow(Fevrier))
Fevrier2=cbind(Mois,Fevrier)

#Mars
Mars=data2007[data2007$Datesnum1>="2007-03-01 00:20:00"& data2007$Datesnum1<="2007-03-31 23:40:00",]
Mois=rep("Mars",nrow(Mars))
Mars2=cbind(Mois,Mars)

#Avril
Avril=data2007[data2007$Datesnum1>="2007-04-01 00:20:00"& data2007$Datesnum1<="2007-04-30 23:40:00",]
Mois=rep("Avril",nrow(Avril))
Avril2=cbind(Mois,Avril)

##Mai
Mai= data2007[data2007$Datesnum1>="2007-05-01 00:20:00"& data2007$Datesnum1<="2007-05-31 23:40:00",]
Mois=rep("Mai",nrow(Mai))
Mai2=cbind(Mois,Mai)

##Juin
Juin= data2007[data2007$Datesnum1>="2007-06-01 00:20:00"& data2007$Datesnum1<="2007-06-30 23:40:00",]
Mois=rep("Juin",nrow(Juin))
Juin2=cbind(Mois,Juin)

##Juillet
Juillet= data2007[data2007$Datesnum1>="2007-07-01 00:20:00"& data2007$Datesnum1<="2007-07-31 23:40:00",]
Mois=rep("Juillet",nrow(Juillet))
Juillet2=cbind(Mois,Juillet)

##Aout
Aout= data2007[data2007$Datesnum1>="2007-08-01 00:20:00"& data2007$Datesnum1<="2007-08-31 23:40:00",]
Mois=rep("Aout",nrow(Aout))
Aout2=cbind(Mois,Aout)

#Septembre
Septembre=data2007[data2007$Datesnum1>="2007-09-01 00:20:00"& data2007$Datesnum1<="2007-09-30 23:40:00",]
Mois=rep("Septembre",nrow(Septembre))
Septembre2=cbind(Mois,Septembre)

#Octobre
Octobre= data2007[data2007$Datesnum1>="2007-10-01 00:20:00"& data2007$Datesnum1<="2007-10-31 23:40:00",]
Mois=rep("Octobre",nrow(Octobre))
Octobre2=cbind(Mois,Octobre)

#Novembre
Novembre= data2007[data2007$Datesnum1>="2007-11-01 00:20:00"& data2007$Datesnum1<="2007-11-30 23:40:00",]
Mois=rep("Novembre",nrow(Novembre))
Novembre2=cbind(Mois,Novembre)

#Decembre
Decembre=data2007[data2007$Datesnum1>="2007-11-01 00:20:00"& data2007$Datesnum1<="2007-12-31 23:40:00",]
Mois=rep("Decembre",nrow(Decembre))
Decembre2=cbind(Mois,Decembre)

##Liaison des sous groupe
data2007final=rbind(Janvier2,Fevrier2,Mars2,Avril2,Mai2,Juin2,Juillet2,Aout2,Septembre2,Octobre2,Novembre2,Decembre2)
data2007final$Mois=as.factor(data2007final$Mois)
str(data2007final)

###Création variable factor Mois pour 2008
#Janvier
Janvier=data2008[data2008$Datesnum1>="2008-01-01 00:20:00"& data2008$Datesnum1<="2008-01-31 23:40:00",]
Mois=rep("Janvier",nrow(Janvier))
Janvier2=cbind(Mois,Janvier)

#Fevrier
Fevrier=data2008[data2008$Datesnum1>="2008-02-01 00:20:00"& data2008$Datesnum1<="2008-02-29 23:40:00",]
Mois=rep("Fevrier",nrow(Fevrier))
Fevrier2=cbind(Mois,Fevrier)

#Mars
Mars=data2008[data2008$Datesnum1>="2008-03-01 00:20:00"& data2008$Datesnum1<="2008-03-31 23:40:00",]
Mois=rep("Mars",nrow(Mars))
Mars2=cbind(Mois,Mars)

#Avril
Avril=data2008[data2008$Datesnum1>="2008-04-01 00:20:00"& data2008$Datesnum1<="2008-04-30 23:40:00",]
Mois=rep("Avril",nrow(Avril))
Avril2=cbind(Mois,Avril)

#Mai
Mai=data2008[data2008$Datesnum1>="2008-05-01 00:20:00"& data2008$Datesnum1<="2008-05-31 23:40:00",]
Mois=rep("Mai",nrow(Mai))
Mai2=cbind(Mois,Mai)

#Juin
Juin=data2008[data2008$Datesnum1>="2008-06-01 00:20:00"& data2008$Datesnum1<="2008-06-30 23:40:00",]
Mois=rep("Juin",nrow(Juin))
Juin2=cbind(Mois,Juin)

#Juillet
Juillet=data2008[data2008$Datesnum1>="2008-07-01 00:20:00"& data2008$Datesnum1<="2008-07-31 23:40:00",]
Mois=rep("Juillet",nrow(Juillet))
Juillet2=cbind(Mois,Juillet)

#Aout
Aout=data2008[data2008$Datesnum1>="2008-08-01 00:20:00"& data2008$Datesnum1<="2008-08-31 23:40:00",]
Mois=rep("Aout",nrow(Aout))
Aout2=cbind(Mois,Aout)

#Septembre
Septembre=data2008[data2008$Datesnum1>="2008-09-01 00:20:00"& data2008$Datesnum1<="2008-09-30 23:40:00",]
Mois=rep("Septembre",nrow(Septembre))
Septembre2=cbind(Mois,Septembre)

#Octobre
Octobre=data2008[data2008$Datesnum1>="2008-10-01 00:20:00"& data2008$Datesnum1<="2008-10-31 23:40:00",]
Mois=rep("Octobre",nrow(Octobre))
Octobre2=cbind(Mois,Octobre)

#Novembre
Novembre=data2008[data2008$Datesnum1>="2008-11-01 00:20:00"& data2008$Datesnum1<="2008-11-30 23:40:00",]
Mois=rep("Novembre",nrow(Novembre))
Novembre2=cbind(Mois,Novembre)

#Decembre
Decembre=data2008[data2008$Datesnum1>="2008-12-01 00:20:00"& data2008$Datesnum1<="2008-12-31 23:40:00",]
Mois=rep("Decembre",nrow(Decembre))
Decembre2=cbind(Mois,Decembre)

##Liason des sous groupe
data2008final=rbind(Janvier2,Fevrier2,Mars2,Avril2,Mai2,Juin2,Juillet2,Aout2,Septembre2,Octobre2,Novembre2,Decembre2)
data2008final$Mois= as.factor(data2008final$Mois)
str(data2008final)  # permet de voir le type de format de variable


###Création variable factor année
nrow(data2007final)
Année=rep(2007, nrow(data2007final))
data2007gr=cbind(data2007final,Année)

nrow(data2008final)
Année=rep(2008, nrow(data2008final))
data2008gr=cbind(data2008final,Année)

data_final=rbind(data2007gr,data2008gr)
data_final$Année=as.factor(data_final$Année)
summary(data_final)


####RECAPITULATIF DES DATAFRAMES

data_final #Ensemble des données 2007 et 2008 avec rajout variable "Mois" et "année"
data2007final # Ensemble des données de 2007 avec rajout variable "Mois" et "année"
data2008final #Ensemble des données de 2008 avec rajout variable "Mois" et "année"

library(shiny)

# Define UI for application that draws a histogram
# 01-kmeans-app



ui <- fluidPage(
    headerPanel('data2007final k-means clustering'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', names(data2007final)),
        selectInput('ycol', 'Y Variable', names(data2007final),
                    selected = names(data2007final)[[2]]),
        
    ),
    mainPanel(
        plotOutput('plot1')
    )
)

server <- function(input, output) {
    
    selectedData <- reactive({
        data2007final[, c(input$xcol, input$ycol)]
    })
    
    
    
    output$plot1 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = "blue", type='l',
             pch = 20, cex = 3)
        
    })
    
}

shinyApp(ui = ui, server = server)


# Run the application 
shinyApp(ui = ui, server = server)


