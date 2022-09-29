library(readxl)
library(table1)
library(samplingbook) ##sample size with sample.size.prop
library(ggplot2)
library(lubridate)
library(arsenal)
library(visdat)


#sample size
zstar = qnorm(.975) 
p = 0.5
E = 0.01
zstar ^ 2 * p * (1 - p) /  E ^ 2 

e     <- 0.05
n     <- 1348
level <- 0.95
P     <- 0.5

sample.size.prop(e=0.0267, P=0.5, N=Inf, level=0.95)

?sample.size.prop

#database
db <- read_excel("C:/Users/gabyt/Downloads/CEECS 2021/taller/colonoscopy_complexity_score.xlsx", 
                 na = "999", sheet="db", skip=6, col_names = TRUE)
str(db)

list <- read_excel("C:/Users/gabyt/Downloads/CEECS 2021/taller/colonoscopy_complexity_score.xlsx",  
                   na = "999", sheet="db", col_names = names(db))
list <- list[1:6,]
list$code

db$age_cat <- cut(db$age,
                  breaks=c(0,18,39,64,Inf),
                  labels=c("<18 yo",
                           "18-39 yo",
                           "40-64 yo",
                           "≥65 yo"))

db$gender <- factor(db$gender,
                    levels=c(list$code),
                    labels=c(list$gender))

db$bmi <- db$weight/((db$height/100)*(db$height/100))
db$bmi_cat <- cut(db$bmi,
                  breaks=c(0,19.99,24.99,29.99,34.99,Inf),
                  labels=c("Underweight (<20)",
                           "Normal weight (20-24.9)",
                           "Obese (25-29.9)",
                           "Overweight I (30-34.99)",
                           "Overweright II/III (≥35)"))

db$digestive_surgery <- factor(db$digestive_surgery,
                               levels=c(list$code),
                               labels=c(list$digestive_surgery))

db$ambulatory_hospitalised <- factor(db$ambulatory_hospitalised,
                                     levels=c(list$code),
                                     labels=c(list$ambulatory_hospitalised))

db$indication_colonoscopy <- factor(db$indication_colonoscopy,
                                    levels=c(list$code),
                                    labels=c(list$indication_colonoscopy))

db$preparation <- factor(db$preparation,
                         levels=c(list$code),
                         labels=c(list$preparation))
db$preparation<-factor(db$preparation)
levels(db$preparation)


db$schedule <- factor(db$schedule,
                      levels=c(list$code),
                      labels=c(list$schedule))
db$time_entrance<-
db$time_entrance <- as.POSIXct(db$time_entrance)

db$boston_sum <- db$bbps_right + db$bbps_transverse + db$bbps_left
db$boston <- ifelse(db$boston_sum>=8 & !is.na(db$boston_sum),"Excelente","Bueno")
db$boston<-as.factor(db$boston)

db$ileocecal_junction_visualization <- factor(db$ileocecal_junction_visualization,
                                              levels=c(list$code),
                                              labels=c(list$ileocecal_junction_visualization))
db$operators <- factor(db$operators,
                       levels=c(list$code),
                       labels=c(list$operators))

db$postural_changes <- factor(db$postural_changes,
                              levels=c(list$code),
                              labels=c(list$postural_changes))

db$manual_pressure_more_than_five_seconds <- factor(db$manual_pressure_more_than_five_seconds,
                                                    levels=c(list$code),
                                                    labels=c(list$manual_pressure_more_than_five_seconds))

db$restart_within_single_operator <- factor(db$restart_within_single_operator,
                                            levels=c(list$code),
                                            labels=c(list$restart_within_single_operator))

db$pain <- ifelse(db$pain_post_colonoscopy>7 & !is.na(db$pain_post_colonoscopy),"8-10",
                  ifelse(db$pain_post_colonoscopy>2 & !is.na(db$pain_post_colonoscopy),"3-7","0-2"))

db$operators
db$score <- ifelse(db$ileocecal_junction_visualization=="No","IV: very high",
                   ifelse(db$operators=="A second was required"
                          |db$restart_within_single_operator=="Yes"
                          |db$manual_pressure_more_than_five_seconds=="Unsuccessful"
                          |db$postural_changes=="Yes","III: High", 
                          ifelse(db$manual_pressure_more_than_five_seconds=="Yes, >10 seconds","II: intermediate", 
                                 ifelse(db$manual_pressure_more_than_five_seconds=="Yes, <10 seconds","I: Low","0: Null"))))
table(db$score)
proportions(table(db$score))

labels <- list(
  variables=list(age="Age (years old, yo), median (IQR)",
                 age_cat="",
                 gender="Gender, n (%)",
                 bmi="BMI (kg/m2), median (IQR)",
                 bmi_cat="",
                 digestive_surgery="Digestive surgery, n (%)",
                 ambulatory_hospitalised="ambulatory_hospitalised",
                 indication_colonoscopy="Indication, n (%)",
                 preparation="Bowel preparation, n (%)",
                 schedule="Schedule, n (%)",
                 boston="BBPS, n (%)",
                 manual_pressure_more_than_five_seconds="Manual pressure >5 min, n (%)",
                 postural_changes="At least one postural change, n (%)",
                 operators="No. of operators, n (%)",
                 ileocecal_junction_visualization="Ileocecal junction visualization, n (%)",
                 consumption_fentanyl_ug="Fentanyl (ug), median (IQR)",
                 consumption_propofol_mg="Propofol (mg), median (IQR)",
                 pain="Pain post-colonoscopy, median (IQR)"))


#EDAD:
hist(db$age)
shapiro.test(db$age)

#por grupo:
db$score<-as.factor(db$score)
levels(db$score)

hist(db$age[db$score=="0: Null"])
hist(db$age[db$score=="I: Low"])
hist(db$age[db$score=="II: intermediate"])
hist(db$age[db$score=="III: High"])
hist(db$age[db$score=="IV: very high"])

shapiro.test(db$age[db$score=="0: Null"])
shapiro.test(db$age[db$score=="I: Low"])
shapiro.test(db$age[db$score=="II: intermediate"])
shapiro.test(db$age[db$score=="III: High"])
shapiro.test(db$age[db$score=="IV: very high"])

bartlett.test(age~score, data = db)# SE CUMPLE IGUALDAD DE VARIANZAS POR BARTLETT y LEVENE
leveneTest(age~score, data = db)

#NO SE VERIFICA NORMALIDAD, si 

#SEXO

tabla_sexo=tableby(score~notest(gender), data = db)
summary(tabla_sexo, text=T)

db$gender<-as.factor(db$gender)
sexo=chisq.test(db$gender,db$score, correct = T)
str(sexo)
sexo$expected #cumple

#BMI

hist(db$bmi)


hist(db$bmi[db$score=="0: Null"])
hist(db$bmi[db$score=="I: Low"])
hist(db$bmi[db$score=="II: intermediate"])
hist(db$bmi[db$score=="III: High"])
hist(db$bmi[db$score=="IV: very high"])

shapiro.test(db$bmi[db$score=="0: Null"])
shapiro.test(db$bmi[db$score=="I: Low"])
shapiro.test(db$bmi[db$score=="II: intermediate"])
shapiro.test(db$bmi[db$score=="III: High"])
shapiro.test(db$bmi[db$score=="IV: very high"])


ggplot(data = db, mapping = aes(x=score, y=bmi))+
  geom_boxplot(mapping = aes(fill=score))+
  geom_jitter(size=2, position = position_jitter(width = 0.05))

library(car)
bartlett.test(bmi~score, data = db)# NO SE CUMPLE IGUALDAD DE VARIANZAS OR BARTLETT
leveneTest(bmi~score, data = db)# SE CUMPLE CON LEVENE

#NO SE CUMPLE NORMALIDAD, SIMILAR DITRIBUCION----->Kruskal wallis

#BMI_CATEGORICO
class(db$bmi_cat)
chi_bmi<-chisq.test(db$bmi_cat,db$score)
chi_bmi$expected

#CIRUGIA DIGESTIVA PREVIA
chi_cx<-chisq.test(db$digestive_surgery, db$score)
chi_cx$expected

#AMBULATORIO- HSPITALIZADO
chi_lugar<-chisq.test(db$ambulatory_hospitalised, db$score)
chi_lugar$expected#NO CUMPLE, ver fisher?

#INDICACION
chi_indicacion<-chisq.test(db$indication_colonoscopy, db$score)
chi_indicacion$expected

#PREPARACION
chi_preparacion<-chisq.test(db$preparation, db$score)
chi_preparacion$expected

#MOMENTO DEL DIA DE REALIZACION DE LA VCC
chi_momento<-chisq.test(db$schedule, db$score)
chi_momento$expected

#BOSTON
chi_boston<-chisq.test(db$boston, db$score)
chi_boston$expected
db$score<-as.factor(db$score)

tabla_1<-tableby(score~kwt(age)+includeNA(chisq(gender))+kwt(bmi)+includeNA(chisq(digestive_surgery))+fe(ambulatory_hospitalised)+chisq(indication_colonoscopy)
                 +includeNA(chisq(preparation))+includeNA(chisq(schedule))+chisq(boston),data=db,numeric.stats=c("Nmiss2","median","q1q3"))

tabla_2<-tableby(score~kwt(age)+chisq(gender)+kwt(bmi)+chisq(digestive_surgery)+fe(ambulatory_hospitalised)+chisq(indication_colonoscopy)
                 +chisq(preparation)+chisq(schedule)+chisq(boston),data=db,numeric.stats=c("median","q1q3"),na.tableby(TRUE))

#ambas sirven

summary(tabla_1, text=T,pfootnote=TRUE)


levels(db$preparation)
levels(db$type_digestive_surgery)
tests(tabla_1)
?tableby

apply(is.na(db),2,sum)
sum(is.na(db))#cantidad de celdas vacias
n_miss(db)#otro opcion

install.packages("naniar")
library(naniar)

n_case_miss(db)#cantidad de observaciones con algun dato faltante
which(!complete.cases(db))#otra opcion

pct_miss_case(db)#proporcion de filas con algun dato faltante

db%>%miss_case_summary()

#graficar los faltantes

vis_miss(db,sort=TRUE)
vis_miss(db,cluster = TRUE)
vis_dat(db)

db$first_operator_type<-ifelse(db$first_operator_type == "No experto/junior" | db$first_operator_type == "0",0,1)
db$first_operator_type<-factor(db$first_operator_type,labels = c("No experto/junior","Experto/senior"), levels = c(0,1))
db$first_operator_type


db$second_operator_type<-ifelse(db$second_operator_type=="No experto/junior"| db$second_operator_type=="0",0,ifelse(db$second_operator_type=="Experto/senior"| db$second_operator_type=="1",1,99))
db$second_operator_type<-ifelse(is.na(db$second_operator_type),99,db$second_operator_type)

db$second_operator_type<-factor(db$second_operator_type,labels = c("No experto/junior", "Experto/senior","Not required"), levels = c(0, 1,99))


------------------------------------------------------------------------------------
#FENTANILO
hist(db$consumption_fentanyl_ug)
hist(db$consumption_fentanyl_ug[db$score=="0: Null"])
hist(db$consumption_fentanyl_ug[db$score=="I: Low"])
hist(db$consumption_fentanyl_ug[db$score=="II: intermediate"])
hist(db$consumption_fentanyl_ug[db$score=="III: High"])
hist(db$consumption_fentanyl_ug[db$score=="IV: very high"])

shapiro.test(db$consumption_fentanyl_ug[db$score=="0: Null"])
shapiro.test(db$consumption_fentanyl_ug[db$score=="I: Low"])
shapiro.test(db$consumption_fentanyl_ug[db$score=="II: intermediate"])
shapiro.test(db$consumption_fentanyl_ug[db$score=="III: High"])
shapiro.test(db$consumption_fentanyl_ug[db$score=="IV: very high"])

ggplot(db = db, mapping = aes(x=db$score, y=db$consumption_fentanyl_ug))+
  geom_boxplot(mapping = aes(fill=db$score))+
  geom_jitter(size=2, position = position_jitter(width = 0.05))

ggplot(db, aes(consumption_fentanyl_ug))+
  geom_bar(aes(fill=score))# NO CUMPLE NORMALIDAD, IMPRESIONA ASIMETRIA IZQUIERDA

bartlett.test(consumption_fentanyl_ug~score, data = db)#NO SE CUMPLE HOMOCEDASTICIDAD

#NO SE CUMPLE NORMALIDAD---> KRUSKAL

chi_dolor<-chisq.test(db$pain, db$score)
chi_dolor$expected# NO CUMPLE, FISHER?


tabla_1_bis<-tableby(score~., data=db)


strata <- c(list(Total=db, 
                 split(db, db$score)))

strata <- c(list(Overall=db),
            split(db, db$score))


my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y) with(y,
                                                  sprintf("%d (%0.1f)", FREQ, PCT))))
}

table1(strata, labels, groupspan=c(1,1,5),
       render.continuous=c(.="Median (Q1 - Q3)"),
       render.categorical=my.render.cat)


median(db$time_entrance, na.rm = T)
range(db$time_outside, na.rm=T)

median(db$time_outside,  na.rm = T)
range(db$time_outside, na.rm=T)

c <- c(46,42,68,29,3)
round(c*100/188,1)
barplot(c)

kruskal.test(db$age ~ db$score)
chisq.test(db$gender, db$score)
chisq.test(db$bmi_cat, db$score)
chisq.test(db$digestive_surgery, db$score)
chisq.test(db$ambulatory_hospitalised, db$score)
chisq.test(db$indication_colonoscopy, db$score)
chisq.test(db$preparation, db$score)
chisq.test(db$schedule, db$score)
chisq.test(db$boston, db$score)
kruskal.test(db$consumption_fentanyl_ug ~ db$score)
chisq.test(db$pain, db$score)

479/1348*100
446/1348*100
194/1348*100
171/1348*100
 58/1348*100
 
 
#Análisis de autores
library(readxl) 
library(dplyr)
  
resumen_semanal <- read_excel("Theses/Dr. Manuel Valero/abcd score/resumen semanal.xlsx", 
                               sheet = "database")

listado <- resumen_semanal %>%
  group_by(institucion) %>%
  summarise(sum(n))

View(listado)
