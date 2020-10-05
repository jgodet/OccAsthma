require(Rtsne)
require(fpc)


fit.rf <- randomForest(daa[,-35:-1],proximity=TRUE,oob.prox = TRUE,ntree = 1000)

hclust.rf <- hclust(as.dist(1-fit.rf$proximity),method = "ward.D2")
plot( hclust.rf )
rect.hclust(hclust.rf, k=5,border=1:5)
grphc <- cutree(hclust.rf, k=7)

table(grphc)

#tsnemat <- Rtsne(X = fit.rf$proximity, dims = 2, perplexity = 30, max_iter = 5000)
tsnemat <- Rtsne(X = daa[,-35:-1], dims = 2, perplexity = 30, max_iter = 5000)
#tsnemat <- tsne(X = daa, k = 3, perplexity = 60, max_iter = 500)
# require(rgl)
# plot3d(tsnemat$Y)
plot(tsnemat$Y[,])
#dbscan::kNNdistplot(df, k =  5)
dbres <- dbscan(data = tsnemat$Y, eps =3, MinPts = 10)
table(dbres$cluster)

plot(tsnemat$Y[,], col = dbres$cluster)


# darf3[which(dbres$cluster==8),]
# 
# 
# dim(darf3[which(dbres$cluster==8),])
# dim(na.omit(darf3[which(dbres$cluster==9),]))
# 
# 
# table(dbres$cluster, grphc)
# plot(dbres, tsnemat$Y[,1:2])



darf$groupsTSNE <- dbres$cluster
varshort<-names(darf)[names(darf)%in%c('age','sex','diagnosis','bmi','obes',"smoker","education","atopy","asthma_pre","age_onset_cat","hmw.lmw","asthma_delay","asthma_work_time","persist","time_last_exp_m","time_last_exp_d","sputum","rhinitis_w",'rhinitis_onset',"conjonct_w","wheezing_w","cough_w","tightness_w","dyspnea_w","sinusitis","dysphonia_w","severity_asthma_step_w","saba_w_frequency","exa_2","severity_ats","baseline_fev1","airway_obstruction","nsbh.res",'nsh_sic_max_ratio','ics_w',"saba_w","anti_h1_w","asper","iget","ger","prick_test","specific_ige","type_reaction","sputum_pre_sic", "sputum_post_sic","eosino_post_sic","neutro_post_sic","feno_pre_sic","blood_eosino")]
varnorm<-names(darf)[names(darf)%in%c("age","bmi","baseline_fev1")]
varexac<-names(darf)[names(darf)%in%c("education","age_onset_cat,severity_asthma_step_w")]
varnonorm<-names(darf)[names(darf)%in%c("asthma_delay","asthma_work_time","time_last_exp_m","eosino_post_sic","neutro_post_sic","feno_pre_sic","blood_eosino")]

tabegr<-CreateTableOne(vars = varshort,strata = "groupsTSNE",data = darf,addOverall = TRUE)
p<-print(tabegr,nonnormal = varshort,exact =varshort, smd = FALSE, printToggle = FALSE, noSpaces = TRUE, test=T, dropEqual = T)
kable(p[,1:7], booktabs = T,format = "html") %>% 
  row_spec(0,bold=TRUE) %>% 
  
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
add_footnote("Median[Q1, Q3], n(%), p: wilcoxon or kruskal tests, fisher exact test", notation="alphabet")