motivation = read.table("motivation.txt",sep="\t",head=T)
motivation2 = read.table("motivation2.txt",sep="\t",head=T)

  #ID : identification du travailleur
  #ENT_ID : identification de l'entreprise
  #TYPE_ENT : type d'entreprise 
    ##3 = but lucratif
    ##1 = entreprise sociale de services à domicile
    ##2 = entreprise sociale d'insertion)
  #ES : entreprise sociale (1 = oui)
  #MOTPROSOC0 : motivation prosociale du travailleur au temps 0
  #MOTPROSOC1 : motivation prosociale du travailleur au temps 1
  #FEMME
  #HOMME
  #AGE
  #EDUCATION: Niveau d'éducation
    ##1= Primaire
    ##2= Secondaire inférieur
    ##3= Secondaire supérieur
    ##4= Tertiaire non universitaire
    ##5= Universitaire
  #NAT: Nationalité
  #EXPEANTE : le travailleur possède une expérience antérieure (1 = oui)
  #NETTOYAGE : son travail consiste à effectuer du nettoyage à domicile (1 = oui)
  #PARTI: le travailleur a quitté l'entreprise avant la seconde enquête (1 = oui)

summary(motivation)
table(motivation$TYPE_ENT)/nrow(motivation)
1 - sum(motivation$TYPE_ENT == 3)/nrow(motivation)
sum(motivation$TYPE_ENT == 2)/nrow(motivation)

mean(motivation$MOTPROSOC0[motivation$ES == 1])
mean(motivation$MOTPROSOC1[motivation$ES == 1])

mean(motivation$MOTPROSOC1[motivation$ES == 1])- mean(motivation$MOTPROSOC0[motivation$ES == 1])

mean(motivation$MOTPROSOC1[motivation$ES == 0])- mean(motivation$MOTPROSOC0[motivation$ES == 0])

summary(motivation2)

mean(motivation2$MOTPROSOC0[motivation2$PARTI == 1])


# Estimation par différence

output1 = lm(motivation$MOTPROSOC1 ~ motivation$ES)
summary(output1)
  # estimation du slope = différence de moyennes conditionnelles
  mean(motivation$MOTPROSOC1[motivation$ES == 1]) - mean(motivation$MOTPROSOC1[motivation$ES == 0])

output1 = lm(motivation$MOTPROSOC0 ~ motivation$ES + motivation$AGE + motivation$EDUCATION + motivation$EXPEANTE + motivation$NETTOYAGE)
summary(output1)


# Diff and diff

mydb <- data.frame(rbind(cbind(motivation$MOTPROSOC0,motivation$ES,0),cbind(motivation$MOTPROSOC1,motivation$ES,1)))
names(mydb) <- c("MOTPROSOC","ES","TIME")
output3 = lm(mydb$MOTPROSOC ~ mydb$ES + mydb$TIME + mydb$ES * mydb$TIME)
summary(output3)



# Attrition


motivation2$RESTE <- 1 -  motivation2$PARTI
output2 = lm(motivation2$MOTPROSOC0 ~ motivation2$ES + motivation2$RESTE + motivation2$ES * motivation2$RESTE)
summary(output2)


# pas utilisé -----------------------------------------------------------------------------------------------------
  
motivation$ETRANGER <- motivation$NAT != "belge"

output1 = lm(motivation$MOTPROSOC0 ~ motivation$ES + motivation$AGE + motivation$EDUCATION + motivation$EXPEANTE + motivation$NETTOYAGE + motivation$ETRANGER)
summary(output1)
