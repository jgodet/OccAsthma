#litre data
data <- read.csv(file = "DATAclean2.csv", h=T, dec='.', sep=';')
dim(data)

#check encodage
boxplot(blood_eosino~ blood_eosino_300, data=data)
aggregate(blood_eosino~ blood_eosino_300, data=data, FUN = summary) #>=

boxplot(eosino_pre_sic~ eosino_pre_sic.3, data=data)
aggregate(eosino_pre_sic~ eosino_pre_sic.3, data=data, FUN = summary) # >=

boxplot(feno_pre_sic~ feno_pre_sic.50, data=data)
aggregate(feno_pre_sic~ feno_pre_sic.50, data=data, FUN = summary) #>=


#donc il faut blood_eosino_300=="yes"| eosino_pre_sic.3=="yes" | feno_pre_sic.50 =="yes"
#càd au moins un...

T2_50 <- apply(X = cbind(data$blood_eosino_300=="yes", 
                         data$eosino_pre_sic.3=="yes" , 
                         data$feno_pre_sic.50 =="yes"), MARGIN = 1, FUN = any)
sum(T2_50, na.rm=T)

  #check
  cbind(data$blood_eosino_300=="yes", 
        data$eosino_pre_sic.3=="yes" , 
        data$feno_pre_sic.50 =="yes",
        T2_50)

#le contraire d'au moins un => aucun
nonT2_50 <- !T2_50
sum(nonT2_50, na.rm=T) #78

  cbind(data$blood_eosino_300=="yes", 
        data$eosino_pre_sic.3=="yes" , 
        data$feno_pre_sic.50 =="yes",
        nonT2_50)

  #créer la variable groupe (facteur)
data$GrpFeno50 <- NA
data$GrpFeno50[T2_50] <- "T2"
data$GrpFeno50[nonT2_50] <- "non T2"
data$GrpFeno50 <- factor(data$GrpFeno50)


summary(data$GrpFeno50)

#répéter la même chose avec le groupe à FeN0 35 (indicatrice qui est à créer?)

#puis réécrire le fichier DATAclean2.csv en DATAanalyse.csv (inclut maintenant GrpFeno50 et GrpFeno35)

