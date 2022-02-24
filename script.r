###Repertoire courant
getwd()
setwd('C:/Users/Fabien/Desktop/test')

###Packages
#install.packages('ade4')
#install.packages('lattice')
#install.packages('MASS')
#install.packages('factoextra')
#install.packages('adegraphics')
#install.packages("mvnormtest")
#install.packages("shiny")
#install.packages("RCurl")
#install.packages("DT")

#library(adegraphics) #Package à activer uniquement avant certains graphique, activation le long du script
library(ade4)
library(lattice)
library(MASS)
library(factoextra)
library(mvnormtest)
library(shiny)
library(RCurl)
library(DT)



###Chargement des jeux de données par fichier
data = read.table("MARELCArnot_2005_2009.csv",dec = "," , header = T, sep=";")


###Chargement des jeux de données par Internet (fonctionnalité varie en fonction de la version de R)
url <- getURL('https://raw.githubusercontent.com/astaquet/marel1/main/MARELCArnot_2005_2009.csv')
data = read.table(textConnection(url),dec = "," , header = T, sep=";")
#data <- read.delim(textConnection(url),dec = "," , header = T, sep=";") #Alternative


###Observation des données
summary(data)
names(data)
str(data)

###Mise en forme des dates

#Conversion du format des données dates
Datesnum1 = strptime(data$Time,"%d/%m/%Y %H:%M")

#Fusion avec variable date
data2=cbind(data,Datesnum1)

###Traitement des données

##Selection 2007_2008
data3=data2[data2$Datesnum1>="2007-01-01 00:20:00" & data2$Datesnum1<="2008-12-31 23:40:00",]
str(data3)

##Suppression variable avec trop de NA
data4=data3[,c(-2,-4,-5)]
str(data4)

##Creation sous-groupe 2007 et 2008
data2007=data4[data4$Datesnum1>="2007-01-01 00:20:00" & data4$Datesnum1<="2007-12-31 23:40:00",]
data2008=data4[data4$Datesnum1>="2008-01-01 00:20:00" & data4$Datesnum1<="2008-12-31 23:40:00",]

##Creation variable factor Mois pour 2007
#Janvier
Janvier=data2007[data2007$Datesnum1>="2007-01-01 00:20:00"& data2007$Datesnum1<="2007-01-31 23:40:00",]
Mois=rep("Janvier",nrow(Janvier))
Janvier2=cbind(Mois,Janvier)

#Fevrier
Fevrier=data2007[data2007$Datesnum1>="2007-02-01 00:20:00"& data2007$Datesnum1<="2007-02-28 23:40:00",]
Mois=rep("Fevrier",nrow(Fevrier))
Fevrier2=cbind(Mois,Fevrier)

#Mars
Mars=data2007[data2007$Datesnum1>="2007-03-01 00:20:00"& data2007$Datesnum1<="2007-03-31 23:40:00",]
Mois=rep("Mars",nrow(Mars))
Mars2=cbind(Mois,Mars)

#Avril
Avril=data2007[data2007$Datesnum1>="2007-04-01 00:20:00"& data2007$Datesnum1<="2007-04-30 23:40:00",]
Mois=rep("Avril",nrow(Avril))
Avril2=cbind(Mois,Avril)

#Mai
Mai= data2007[data2007$Datesnum1>="2007-05-01 00:20:00"& data2007$Datesnum1<="2007-05-31 23:40:00",]
Mois=rep("Mai",nrow(Mai))
Mai2=cbind(Mois,Mai)

#Juin
Juin= data2007[data2007$Datesnum1>="2007-06-01 00:20:00"& data2007$Datesnum1<="2007-06-30 23:40:00",]
Mois=rep("Juin",nrow(Juin))
Juin2=cbind(Mois,Juin)

#Juillet
Juillet= data2007[data2007$Datesnum1>="2007-07-01 00:20:00"& data2007$Datesnum1<="2007-07-31 23:40:00",]
Mois=rep("Juillet",nrow(Juillet))
Juillet2=cbind(Mois,Juillet)

#Aout
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

##Creation variable factor Mois pour 2008
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

##Creation variable factor annee
nrow(data2007final)
Annee=rep(2007, nrow(data2007final))
data2007gr=cbind(data2007final,Annee)

nrow(data2008final)
Annee=rep(2008, nrow(data2008final))
data2008gr=cbind(data2008final,Annee)

data_final=rbind(data2007gr,data2008gr)
data_final$Annee=as.factor(data_final$Annee)
summary(data_final)
data_final$Mois=factor(data_final$Mois, levels = unique(data_final$Mois)) #Mettre en ordre non alphabétique


####RECAPITULATIF DES DATAFRAMES

data_final #Ensemble des données 2007 et 2008 avec rajout variable "Mois" et "annee"
data2007final # Ensemble des données de 2007 avec rajout variable "Mois" et "annee"
data2008final #Ensemble des données de 2008 avec rajout variable "Mois" et "annee"


####Description graphique générale du jeu de donnée - création d'une application R Shiny  ####  

## ==> URL de l'application : https://neirda.shinyapps.io/Marel13/ 

ui <- fluidPage(
  headerPanel("MAREL - 2007 et 2008"),
  
  fluidRow(                    
    column(12,  sidebarPanel(
      selectInput('xcol', 'Variable en X', names(data_final), selected = names(data_final)[[14]]),
      selectInput('ycol', 'Variable en Y gauche', names(data_final), selected = names(data_final)[[9]]),
      selectInput('ycol2', 'Variable en Y droite', names(data_final),
                  selected = names(data_final)[[11]])))),
  
  fluidRow(   
    column(12, plotOutput('plot'))),
  
  fluidRow(        
    column(6, plotOutput('hist')),
    column(6, plotOutput('hist2'))),
  
  fluidRow(        
    column(12, verbatimTextOutput("stats")))
)

server <- shinyServer(function(input, output) {
  Dataselect <- reactive({
    data_final[, c(input$xcol, input$ycol)] 
  })
  
  output$plot <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(Dataselect(),
         col = "red", type='p',
         pch = 1, cex = 1,yaxt="n",ylab="",yaxt="n")
    axis(2,col="red")
    par(new=TRUE) 
    plot(data_final[, c(input$xcol)],data_final[, c(input$ycol2)],
         axes=FALSE,type="p",
         col="blue",xlab="",ylab="",yaxt="n")
    axis(4,col="blue")  
  })
  
  output$hist <- renderPlot({
    hist(data_final[, c(input$ycol)],main=input$ycol,xlab="" )})
  
  output$hist2 <- renderPlot({
    hist(data_final[, c(input$ycol2)],main=input$ycol,xlab="")       
  })
  
  output$stats <- renderPrint({
    summary(Dataselect())
  }) 
})

shinyApp(server = server, ui = ui)


######Boxplot 2007 et 2008###

# x11() #Ouverture de fenêtre graphique supplémentaire

bw_lattice <-bwplot(data_final$Fluorescence~ factor(data_final$Mois, levels = unique(data_final$Mois))|data_final$Annee, xlab= "Mois", ylab="Fluorescence",scales=list(tick.number=6))
bw_theme <- trellis.par.get()
bw_theme$box.dot$pch <- "|"
bw_theme$box.rectangle$col <- "black"
bw_theme$box.rectangle$lwd <- 2
bw_theme$box.rectangle$fill <- "grey90"
bw_theme$box.umbrella$lty <- 1
bw_theme$box.umbrella$col <- "black"
bw_theme$plot.symbol$col <- "grey40"
bw_theme$plot.symbol$pch <- "*"
bw_theme$plot.symbol$cex <- 2
bw_theme$strip.background$col <- "grey80"

l_bw <- update(bw_lattice, par.settings = bw_theme)
l_bw <- update(bw_lattice, par.settings = bw_theme, xlab = "Mois", fill = rainbow(12))
l_bw

#### Grahique fluorescence en 2007 et en 2008 ####
par(mar=c(5,5,2,5)) 
plot(data2007final$Datesnum1,data2007final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4") 
axis(2,col="royalblue4",col.axis = "royalblue4") # Parametrage de l'axe 2 (ordonnee gauche)
mtext(text="2007",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1) 
par(new=TRUE) 
plot(data2008final$Fluorescence,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="2008",side=4,line=3,las=0,cex=1, col="brown2") 


#### Graph fluorescence en fonction des differents parametres et du temps pour 2007 ####

## Graph de la fluorescence et de la saturation en oxygène en fonction du temps pour 2007 ## 
par(mar=c(5,5,2,5)) 
plot(data2007final$Datesnum1,data2007final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4") 
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1) 
par(new=TRUE) # Pour rajouter le graphique sur le graphique deja existant
plot(data2007final$Datesnum1,data2007final$SaturationOxygene,axes=FALSE,type="l",col="brown2",xlab="",ylab="") # Affiche le deuxieme graphique avec la saturation en oxygène / axes=FALSE pour dire de ne pas dessiner les axes par defaut
axis(4,col="brown2",col.axis = "brown2") # Parametrage de l'axe 4 (ordonnee droite)
mtext(text="Saturation en oxygène",side=4,line=3,las=0,cex=1, col="brown2") # Rajoute le nom de l'axe des ordonnées de droite

## Graph de la fluorescence et de l'oxygène dissous en fonction du temps pour 2007 ##
par(mar=c(5,5,2,5)) 
plot(data2007final$Datesnum1,data2007final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(data2007final$Datesnum1,data2007final$OxyDissousCorrigeSalinite,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Oxygène dissous",side=4,line=3,las=0,cex=1, col="brown2") 

## Graph de la fluorescence et du pH en fonction du temps pour 2007 ##
par(mar=c(5,5,2,5)) 
plot(data2007final$Datesnum1,data2007final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(data2007final$Datesnum1,data2007final$pH,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="pH",side=4,line=3,las=0,cex=1, col="brown2") 

## Graph de la fluorescence et de la turbidite en fonction du temps pour 2007 ##
par(mar=c(5,5,2,5)) 
plot(data2007final$Datesnum1,data2007final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(data2007final$Datesnum1,data2007final$Turbidite,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Turbidite",side=4,line=3,las=0,cex=1, col="brown2") 

#### Graph de la fluorescence et de la turbidite en fonction du temps sur 48h pour 2007 ####
ExtraitData2007avril=data2007final[data2007final$Datesnum1>="2007-04-22 00:00:00" & data2007final$Datesnum1<="2007-04-23 23:40:00",]

par(mar=c(5,5,2,5)) 
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$Turbidite,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Turbidite",side=4,line=3,las=0,cex=1, col="brown2") 




#### Graph fluorescence en fonction des differents parametres et du temps pour 2008 ####

## Graph de la fluorescence et de la saturation en oxygène en fonction du temps pour 2008 ##
#x11() 
par(mar=c(5,5,2,5)) 
plot(data2008final$Datesnum1,data2008final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4") # Affiche le premier graphique avec la fluorescence en fonction du temps
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1) 
par(new=TRUE) # Pour rajouter le graphique sur le graphique deja existant
plot(data2008final$Datesnum1,data2008final$SaturationOxygene,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2")
mtext(text="Saturation en oxygène",side=4,line=3,las=0,cex=1, col="brown2") 

## Graph de la fluorescence et de l'oxygène dissous en fonction du temps pour 2008 ##
par(mar=c(5,5,2,5)) 
plot(data2008final$Datesnum1,data2008final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(data2008final$Datesnum1,data2008final$OxyDissousCorrigeSalinite,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Oxygène dissous",side=4,line=3,las=0,cex=1, col="brown2") 

## Graph de la fluorescence et du pH en fonction du temps pour 2008 ##
par(mar=c(5,5,2,5)) 
plot(data2008final$Datesnum1,data2008final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(data2008final$Datesnum1,data2008final$pH,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="pH",side=4,line=3,las=0,cex=1, col="brown2") 

## Graph de la fluorescence et de la turbidite en fonction du temps pour 2008 ##
par(mar=c(5,5,2,5)) 
plot(data2008final$Datesnum1,data2008final$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(data2008final$Datesnum1,data2008final$Turbidite,axes=FALSE,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Turbidite",side=4,line=3,las=0,cex=1, col="brown2") 


###Sur 48h en avril 2007
##Selection des données
ExtraitData2007avril=data2007final[data2007final$Datesnum1>="2007-04-22 00:00:00" & data2007final$Datesnum1<="2007-04-23 23:40:00",]

#Fluorescence et oxygène dissous
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$OxyDissousCorrigeSalinite,axes=F,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Oxygène dissous",side=4,line=3,las=0,cex=1, col="brown2") 

#Fluorescence et turbidite
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$Turbidite,axes=F,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="Turbidite",side=4,line=3,las=0,cex=1, col="brown2") 

###Fluorescence et PAR
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$Fluorescence,yaxt="n",xlab="",ylab="",type="l",col="royalblue4")
axis(2,col="royalblue4",col.axis = "royalblue4") 
mtext(text="Fluorescence",side=2,line=3,las=0, cex=1, col="royalblue4") 
mtext(text="Temps",side=1,line=3,las=0,cex=1)
par(new=TRUE) 
plot(ExtraitData2007avril$Datesnum1,ExtraitData2007avril$PAR,axes=F,type="l",col="brown2",xlab="",ylab="") 
axis(4,col="brown2",col.axis = "brown2") 
mtext(text="PAR",side=4,line=3,las=0,cex=1, col="brown2") 

#####Regression lineaire

###Fluorescence et oxygène dissous lm
lm1<-lm(ExtraitData2007avril$Fluorescence~ExtraitData2007avril$OxyDissousCorrigeSalinite)

##extracting all the parameters and measures of fitness
summary(lm1)

##extracting the confidence intervals
confint(lm1)
lm1$coef

##verifying the residual dignostic plots
plot(lm1)

#testing the normality of the residuals
shapiro.test(lm1$residuals)

#plotting the points with the associated regression lines
plot(ExtraitData2007avril$Fluorescence,ExtraitData2007avril$OxyDissousCorrigeSalinite,ylab="Oxygène dissous", xlab="Fluorescence", pch=21, bg=1, col="royalblue4")


###Fluorescence et turbidite lm

##Transfo log
lm2<-lm(log(ExtraitData2007avril$Fluorescence)~log(ExtraitData2007avril$Turbidite))

##extracting all the parameters and measures of fitness
summary(lm2)

##extracting the confidence intervals
confint(lm2)
lm2$coef

##verifying the residual dignostic plots
plot(lm2)

#testing the normality of the residuals
shapiro.test(lm2$residuals)

#plotting the points with the associated regression lines
plot(ExtraitData2007avril$Fluorescence, ExtraitData2007avril$Turbidite,ylab="Turbidite", xlab="Fluorescence", pch=21, bg=1, col="royalblue4")
abline(lm2$coef, col="brown2")

###Pour 2007

##Fluorescence et turbidite 
nafinal=na.omit(data_final)

plot(nafinal$Fluorescence,nafinal$Turbidite,xlab="Fluorescence",col="royalblue4" ,ylab="Turbidite", xlim=c(0,15))

##Fluorescence et oxygène dissous
plot(nafinal$Fluorescence,nafinal$OxyDissousCorrigeSalinite,xlab="Fluorescence",col="royalblue4",ylab="Oxygène dissous", xlim=c(0,15))





###Analyses Multivariees
##ACP

#ACP2007
naomit2007=na.omit(data2007final)

acp=dudi.pca(naomit2007[,c(3,4,5,6,7,9,10,11,12,13)] ) 
2
acp$eig
barplot(acp$eig, col = "blue")
acp$eig/sum(acp$eig) * 100
acp$co

# Resultats des variables de l'acp
resVar <- get_pca_var(acp)
resVar$contrib        # Contributions aux axes
par(mfrow = c(1, 2))
for (i in 1:2) barplot(resVar$contrib[, i], 
                       names.arg = row.names(resVar$contrib), las = 2, main = paste("Axe", i))

detach("package:adegraphics")
s.corcircle(acp$co)

#ACP2008
naomit2008=na.omit(data2008final)

acp=dudi.pca(naomit2008[,c(3,4,5,6,7,9,10,11,12,13)] ) 
2
acp$eig
barplot(acp$eig, col = "blue")
acp$eig/sum(acp$eig) * 100
acp$co

# Resultats des variables
resVar <- get_pca_var(acp)
resVar$contrib        # Contributions aux axes
par(mfrow = c(1, 2))
for (i in 1:2) barplot(resVar$contrib[, i], 
                       names.arg = row.names(resVar$contrib), las = 2, main = paste("Axe", i))


detach("package:adegraphics")
s.corcircle(acp$co)

###AFD
#AFD 2007
naomit2007=na.omit(data2007final)

afd1=discrimin(dudi.pca(naomit2007[,c(-1,-2,-5,-8,-6,-14)] ),naomit2007$Mois)
3
3
afd1$eig
afd1$eig/sum(afd1$eig) * 100
afd1$gc
afd1$va
afd1$fa
afd1$cp
par(mfrow=c(1,2))

plot(afd1)
s.corcircle(afd1$va)
s.arrow(afd1$fa)#plus fleche est grande plus est discriminante

library(adegraphics)
s.class(afd1$li, naomit2007$Mois,xlim = c(-2, 2), ylim = c(-3.5, 2.5), pellipses.col = adegpar(plabels.cex=0.8, ppoints.cex = 0, ppoints.alpha = 0.6, ppoints.col = col,plines.lty='blank' )$ppalette$quali(12,Pastel1))
detach("package:adegraphics")

#Test de permutation
rand=rtest(discrimin(dudi.pca(naomit2007[,c(-1,-2,-5,-8,-6,-14)] ),naomit2007$Mois), 1000)
3
3
2
rand
plot(rand)

#LDA
lda1=lda(naomit2007$Mois ~ naomit2007$OxyDissousCorrigeSalinite +naomit2007$Salinite  +naomit2007$PAR +naomit2007$pH +naomit2007$Turbidite +naomit2007$Fluorescence +naomit2007$TemperatureEau +naomit2007$NiveauMer,CV=TRUE )

table(naomit2007$Mois,lda1$class)

###AFD 2008
naomit2008=na.omit(data2008final)

afd1=discrimin(dudi.pca(naomit2008[,c(-1,-2,-5,-8,-6,-14)] ),naomit2008$Mois)
3
3
afd1$eig
afd1$eig/sum(afd1$eig) * 100
afd1$gc
afd1$va
afd1$fa
afd1$cp

plot(afd1)
s.corcircle(afd1$va)
s.arrow(afd1$fa)

library(adegraphics)
s.class(afd1$li, naomit2008$Mois, pellipses.col = adegpar(plabels.cex=0.8, ppoints.cex = 0, ppoints.alpha = 0.6, ppoints.col = col,plines.lty='blank' )$ppalette$quali(12,Pastel1))
detach("package:adegraphics")


#Test de permutation
rand=rtest(discrimin(dudi.pca(naomit2008[,c(-1,-2,-8,-14)] ),naomit2008$Mois), 1000)
3
3
2
rand
plot(rand)

#LDA
lda1=lda(naomit2008$Mois ~ naomit2008$OxyDissousCorrigeSalinite +naomit2008$Salinite+naomit2008$PAR +naomit2008$pH +naomit2008$Turbidite +naomit2008$Fluorescence +naomit2008$TemperatureEau +naomit2008$NiveauMer,CV=TRUE )

table(naomit2008$Mois,lda1$class)


