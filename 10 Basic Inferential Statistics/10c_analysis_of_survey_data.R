############################################################################
###                         Al.I. Cuza University of Iași                ###
###            Faculty of Economics and Business Administration          ###
###       Department of Accounting, Information Systems and Statistics   ###
############################################################################
###
############################################################################
###             Data Processing/Analysis/Science with R                  ###
############################################################################
###   10c. Analysis of Survey Data (Likert) - in Romanian (and English)  ###
############################################################################
### See also the presentation:
### https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/blob/master/10%20Basic%20Inferential%20Statistics/10_basic_inferential_statistics.pptx
############################################################################
## last update: 2019-03-25

#install.packages("likert")
library(likert)
# citation('likert')
require(scales)
library(tidyverse)

############################################################################
###            Download the necesary data sets for this script
############################################################################

# all the files needed o run this script are available at:
# https://github.com/marinfotache/Data-Processing-Analysis-Science-with-R/tree/master/DataSets

# Please download the files in a local directory (such as 'DataSets') and  
# set the directory where you dowloaded the data files as the 
# default/working directory, ex:
setwd('/Users/marinfotache/Google Drive/R(Mac)/DataSets')
############################################################################


############################################################################
###                            Load the data
load(file = 'chestionarSIA2013.RData')


#######################################################################################
###		For variable visualization and analysis, see scrips 07e and 07f           ###
#######################################################################################
##
##        we not cover EDA here, but focus instead on likert data




#######################################################################################
###		I. Evaluarea generala: program, profi, discipline (scala  Likert)         ###
#######################################################################################


# https://stackoverflow.com/questions/43646659/likert-in-r-with-unequal-number-of-factor-levels/43649056

#df <- rbind(c("Strongly agree","Strongly agree","Strongly agree","Strongly agree","Strongly agree","Strongly agree"),
#            c("Neither agree nor disagree","Neither agree nor disagree","Neither agree nor disagree","Neither agree nor disagree","Neither agree nor disagree","Neither agree nor disagree"),
#            c("Disagree","Strongly disagree","Neither agree nor disagree","Disagree","Disagree","Neither agree nor disagree"))
#df <- as.data.frame(df)
#colnames(df) <- c("Increased student engagement", "Instructional time effectiveness increased", "Increased student confidence", "Increased student performance in class assignments", "Increased learning of the students", "Added unique learning activities")

#lookup <- data.frame(levels = 1:5, mylabels = c('Strongly disagree', 'Disagree', 'Neither agree nor disagree', 'Agree', 'Strongly agree'))

#df.1 <- as.data.frame(apply(df, 2, function(x) match(x, lookup$mylabels)))
#df.new <- as.data.frame(lapply(as.list(df.1), factor, levels = lookup$levels, labels = lookup$mylabels))




names(evaluari.2)
from_ <- c('1', '2', '3', '4', '5')
niveluri = c("foarte scăzut", "scăzut", "mediu", "bun", "foarte bun")
niveluri.en = c("very poor", "poor", "average", "good", "very good")

atrib <- c("evMaster", "evProfi", "evDiscipline")
evGenerala <- evaluari.2 [, atrib]

#evGenerala <- as.data.frame(sapply(evGenerala, as.numeric))
str(evGenerala)
names(evGenerala) <- c('Master', 'Profesori', 'Discipline')

evGenerala[evGenerala == "1"] <- "foarte scăzut"
evGenerala[evGenerala == "2"] <- "scăzut"
evGenerala[evGenerala == "3"] <- "mediu"
evGenerala[evGenerala == "4"] <- "bun"
evGenerala[evGenerala == "5"] <- "foarte bun"
i <- 1
for (i in 1:ncol(evGenerala))
{
	evGenerala[,i] = factor(evGenerala[,i], 
	                        levels=niveluri, ordered=TRUE)
}
names(evGenerala)
str(evGenerala)

###################################################################################
###		               I.a Vizualizare date Likert                            ###
###################################################################################

l.evGenerala = likert(evGenerala, nlevels = 5)
l.evGenerala
summary(l.evGenerala)
summary(l.evGenerala, center=2.5)

# grafic Likert
plot(l.evGenerala, text.size=4.5) +  
	ggtitle("Evaluare generală: profesori, discipline și master") +
	theme (plot.title = element_text (colour="black", size="16"))+
	theme (axis.text.y = element_text (colour="black", size="14", 
	                   hjust=0))+
     theme (axis.text.x = element_text (colour="black", size="14")) +
     theme (legend.text = element_text (colour="black", size="14")) 

# alta versiune a graficului
library(plyr)
plot(l.evGenerala, plot.percents=TRUE, plot.percent.low=FALSE, 
     plot.percent.high=FALSE, text.size=4.5, centered=FALSE) +
	ggtitle("Evaluare generală: profesori, discipline și master") +
	theme (plot.title = element_text (colour="black", size=17))+
	theme (axis.text.y = element_text (colour="black", size=14, hjust=0))+
     theme (axis.text.x = element_text (colour="black", size=14)) +
     theme (legend.text = element_text (colour="black", size=14)) 
     

# Heat map
plot(l.evGenerala, type='heat', wrap=30, text.size=4)

plot(l.evGenerala, type='heat', wrap=30, text.size=4) +
     ggtitle("Diagrama heatmap a evaluarii \npersonalului didactic, programului si disciplinelor") +
	theme (plot.title = element_text (colour="black", size="18"))+
	theme (axis.text.y = element_text (colour="black", size="14", hjust=0))+
     theme (axis.text.x = element_text (colour="black", size="12")) +
     theme (legend.text = element_text (colour="black", size="12")) 

# Density plot
plot(l.evGenerala, type='density')
plot(l.evGenerala, type='density', facet=FALSE)



###################################################################################
###	       I.b Vizualizare date Likert, cu gruparea rezultatelor dupa Gen      ###
###################################################################################

names(evaluari.2)
evaluari.3 <- subset(evaluari.2, !is.na(sex))
evaluari.3$sex <- as.factor(evaluari.3$sex)
evaluari.3$evMaster <- as.numeric(evaluari.3$evMaster)
evaluari.3$evProfi <- as.numeric(evaluari.3$evProfi)
evaluari.3$evDiscipline <- as.numeric(evaluari.3$evDiscipline)

atrib <- c('evMaster', 'evProfi', 'evDiscipline', 'sex')
evGenerala <- evaluari.3 [, atrib]
names(evGenerala) <- c('Master', 'Profesori', 'Discipline', 'gen')

evGenerala[evGenerala == "1"] <- "foarte scăzut"
evGenerala[evGenerala == "2"] <- "scăzut"
evGenerala[evGenerala == "3"] <- "mediu"
evGenerala[evGenerala == "4"] <- "bun"
evGenerala[evGenerala == "5"] <- "foarte bun"

for (i in 1:(ncol(evGenerala)-1))
{
	evGenerala[,i] = factor(evGenerala[,i], levels=niveluri, ordered=TRUE)
}
names(evGenerala)

#evGenerala <- as.data.frame(sapply(evGenerala, as.numeric))
str(evGenerala)
names(evGenerala) <- c('Master', 'Profesori', 'Discipline')


l.evGenerala.g1 <- likert(evGenerala[,1:3], grouping=evaluari.3$sex)
l.evGenerala.g1
summary(l.evGenerala.g1)

# Plots
plot(l.evGenerala.g1)
plot(l.evGenerala.g1, group.order=c('Feminin', 'Masculin'))

plot(l.evGenerala.g1, wrap=30, text.size=4.5,
     panel.background = element_rect(size = 1, color = "grey70", fill = NA), 
     group.order=c('Feminin', 'Masculin')) +
     ggtitle("Evaluare generală: profesori, discipline și master,\npe genuri/sexe") +
	theme (plot.title = element_text (colour="black", size=17))+
	theme (axis.text.y = element_text (colour="black", size=14, hjust=0))+
     theme (axis.text.x = element_text (colour="black", size=12)) +
     theme (legend.text = element_text (colour="black", size=12)) +
     theme(strip.text.x = element_text(size = 14, colour = "black", angle = 0))

          	
#plot(l.evGenerala.g1, center=2.5, include.center=FALSE)  ## asta e cel mai bun
#plot(l.evGenerala.g1, group.order=c('Feminin', 'Masculin'))
# Reordonarea grupurilor

# Curba densitatii
plot(l.evGenerala.g1, type='density')

# calcul medie evaluare, pentru cele doua sexe
evaluari.3 %>%
  group_by(sex) %>%
  dplyr::summarise(
     mean.of.evMaster= mean(evMaster, na.rm = TRUE), 
     mean.of.evProfi= mean(evProfi, na.rm = TRUE), 
     mean.of.evDiscipline= mean(evDiscipline, na.rm = TRUE) 
     )

# calcul mediana evaluare, pentru cele doua sexe
evaluari.3 %>%
  group_by(sex) %>%
  dplyr::summarise(
     median.of.evMaster= median(evMaster, na.rm = TRUE), 
     median.of.evProfi= median(evProfi, na.rm = TRUE), 
     median.of.evDiscipline= median(evDiscipline, na.rm = TRUE) 
     )



###################################################################################
###	                    I.c Analiza datelor din evaluare                       ###
###################################################################################

names(evaluari.3)
#########
## Intrebare:
## Exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea finala a masterului ?
#  H0: Nu exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea finala a masterului 

wilcox.test(evMaster ~ sex, data=evaluari.3)

kruskal.test(evMaster ~ sex, data=evaluari.3)
#   p-value = 0.2296; H0 nu este respinsa, deci, aparent, nu exista diferente semnificative

#install.packages('zoo')

#install.packages('coin')
library(coin)
wilcox_test(evMaster ~ sex, alternative="less", conf.int=TRUE,
            distribution="exact", data=evaluari.3)

wilcox_test(evMaster ~ sex, alternative="greater", conf.int=TRUE,
            distribution="exact", data=evaluari.3)

wilcox_test(evMaster ~ sex, alternative="two.sided", conf.int=TRUE,
            distribution="exact", data=evaluari.3)

# effect size 
# Z /  sqrt(nrow(evaluari.3))
1.2015 /  sqrt(nrow(evaluari.3))
# 0.1396715

## Intrebare:
## Exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea profesorilor ?
#  H0: Nu exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea profesorilor 
#evaluari.3$evProfi <- as.numeric(evaluari.3$evProfi)
wilcox.test(evProfi ~ sex, data=evaluari.3)
kruskal.test(evProfi ~ sex, data=evaluari.3)
#  p-value = 0.336; H0 nu este respinsa, deci, aparent, nu exista diferente

## Intrebare:
## Exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea disciplinelor masterului ?
#  H0: Nu exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea disciplinelor masterului 
#evaluari.3$evDiscipline <- as.numeric(evaluari.3$evDiscipline)
wilcox.test(evDiscipline ~ sex, data=evaluari.3)
kruskal.test(evDiscipline ~ sex, data=evaluari.3)
#  p-value = 0.4791; H0 nu este respinsa, deci, aparent, nu exista diferente

wilcox_test(evDiscipline ~ sex, alternative="greater", conf.int=TRUE,
            distribution="exact", data=evaluari.3)

# effect size 
# Z /  sqrt(nrow(evaluari.3))
1.2015 /  sqrt(nrow(evaluari.3))
# 0.139


# Testele de mai sus sunt discutabile datorita compararii medianelor
#   pentru date definite pe scala likert; de aceea, cream un atribut compozit, numeric
evaluari.3$PunctajProgram <-  (
     ifelse(is.na(evaluari.3$evMaster), 0, evaluari.3$evMaster) +
     ifelse(is.na(evaluari.3$evProfi), 0, evaluari.3$evProfi) +
     ifelse(is.na(evaluari.3$evDiscipline), 0, evaluari.3$evDiscipline)  ) /
     (     ifelse(is.na(evaluari.3$evMaster), 0, 1) +
          ifelse(is.na(evaluari.3$evProfi), 0, 1) +
          ifelse(is.na(evaluari.3$evDiscipline), 0, 1) )

atrib <- c("evMaster", "evProfi", "evDiscipline", "PunctajProgram")
evaluari.3[atrib]


# calcul medie si mediana punctaj global, pentru cele doua sexe
evaluari.3 %>%
  group_by(sex) %>%
  dplyr::summarise(
     medie.punctaj = mean(PunctajProgram, na.rm = TRUE), 
     mediana.punctaj = median(PunctajProgram, na.rm = TRUE) 
 )

# Density plots with semi-transparent fill
ggplot(evaluari.3, aes(x=PunctajProgram, fill=sex)) + 
     geom_density(alpha=.3) +
     ggtitle("Punctaj compozit master,\npe genuri/sexe") 
     

## Intrebare:
## Exista diferente semnificative intre absolventi si absolvente in ceea ce priveste
##    evaluarea generala (compozita) a masterului ?
#  H0: Nu exista diferente semnificative intre absolventi si absolvente in 
# ceea ce priveste punctajul compozit acordat masterului 

wilcox.test(PunctajProgram ~ sex, data=evaluari.3)
kruskal.test(PunctajProgram ~ sex, data=evaluari.3)
#  p-value = 0.2223; H0 nu este respinsa, deci, aparent, nu exista diferente

wilcox_test(PunctajProgram ~ sex, alternative="two.sided", conf.int=TRUE,
            distribution="exact", data=evaluari.3)
# Z = -1.2205, p-value = 0.2248
# 95 percent confidence interval: [-0.6666667,  0.0000000]

wilcox_test(PunctajProgram ~ sex, alternative="greater", conf.int=TRUE,
            distribution="exact", data=evaluari.3)
#  p-value = 0.8886; H0 nu este respinsa, deci, aparent, nu exista diferente

# effect size 
# Z /  sqrt(nrow(evaluari.3))
1.2205 /  sqrt(nrow(evaluari.3))
# 0.1418


#######################################################################################
###		 II. Evaluarea, la momentul curent (2013), a utilitatii disciplinelor     ###
#######################################################################################
names(evaluari)

atribute = names(evaluari)

atribute_moment_actual <- atribute[which(str_detect(atribute, "MomActual"))]
evalUtilitActuala = subset(evaluari, , select = atribute_moment_actual)
head(evalUtilitActuala)
evalUtilitActuala = evalUtilitActuala[, -(1:2)]
head(evalUtilitActuala)

str(evalUtilitActuala)

evalUtilitActuala[evalUtilitActuala == "1"] <- "foarte scăzut"
evalUtilitActuala[evalUtilitActuala == "2"] <- "scăzut"
evalUtilitActuala[evalUtilitActuala == "3"] <- "mediu"
evalUtilitActuala[evalUtilitActuala == "4"] <- "bun"
evalUtilitActuala[evalUtilitActuala == "5"] <- "foarte bun"

for (i in 1:ncol(evalUtilitActuala))
{
	evalUtilitActuala[,i] = factor(evalUtilitActuala[,i], 
	         levels=niveluri, ,ordered=TRUE)
}


atribute = names(evalUtilitActuala)

nume.noi = str_replace(atribute, 'vUtilitateMomActual', '')
nume.noi = str_replace(nume.noi, '^e', '')

names(evalUtilitActuala) = nume.noi

l.evalUtilitActuala = likert(evalUtilitActuala)
l.evalUtilitActuala
summary(l.evalUtilitActuala)
summary(l.evalUtilitActuala, center=2.5)

plot(l.evalUtilitActuala, text.size=4) +  
	ggtitle("Evaluare actuală a utilității disciplinelor (domeniilor)") +
	theme (plot.title = element_text (colour="black", size="18"))+
	theme (axis.text.y = element_text (colour="black", size="12", hjust=0))+
     theme (axis.text.x = element_text (colour="black", size="10")) +
     theme (legend.text = element_text (colour="black", size="11")) 
	
plot(l.evalUtilitActuala, ordered=FALSE, 
	group.order=names(evalUtilitActuala)) # specificare ordine de pe axa y

plot(l.evalUtilitActuala, centered=FALSE, wrap=30)
plot(l.evalUtilitActuala, center=2.5, wrap=30)
plot(l.evalUtilitActuala, center=2.5, include.center=FALSE, wrap=30)
plot(l.evalUtilitActuala, center=2.5, include.center=FALSE, wrap=20)
plot(l.evalUtilitActuala, plot.percents=TRUE, plot.percent.low=FALSE, plot.percent.high=FALSE)

# Density plot
plot(l.evalUtilitActuala, type='density', facet=FALSE)

# Heat map
plot(l.evalUtilitActuala, type='heat', wrap=30, text.size=4.5)

