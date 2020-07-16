# Code used for paper: "Traits for depression related to agentic and external control (preprint)"
# submitted to Learning & Motivation, special issue: individual differences
# writen by Santiago Castiello. Final version: 10/06/2020

if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2) # melt()
if (!require(plyr)) {install.packages("plyr")}; library(plyr) # revalue()
if (!require(lmerTest)) {install.packages("lmerTest")}; library(plyr) # lmer()
if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr) # %>%
if (!require(psycho)) {install.packages("psycho")}; library(psycho) # analyze()
if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2) # ggplot()
if (!require(viridis)) {install.packages("viridis")}; library(viridis) # viridis, magma, inferno()
if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr) # ggarrange()


# errase previous Global Environment, import manually the file
rm(list = ls())
# set work directory
setwd("C:/Users/scastiello/OneDrive - Nexus365/6.- lab people projects/Rachel-Robin/MultipleContingencies_L&M_paper")
# write csv? if yes then write_csv <- 1 
write_csv <- 0
print_plots <- 0
exclude_part <- 1
# mean difference function
f_mean_difference <- function(sample1, sample2, paired){
  
  alpha <- 0.05 
  
  norm1 <- shapiro.test(sample1) # Shapiro-Wilk normality test
  norm2 <- shapiro.test(sample2) # Shapiro-Wilk normality test
  
  homoce <- var.test(sample1,sample2) # F test to compare two variances
  
  if (homoce$p.value > alpha) {var_equal = 1} else {var_equal = 0} 
  # there is a difference in t test depending homocedasticity. 
  # For var_equal: 1 = Two Sample t-test; 0 = Welch Two Sample t-test
  
  if (norm1$p.value > alpha  & norm2$p.value > alpha) {
    test <- t.test(sample1,sample2, paired = paired, var.equal = var_equal)
  } else {
    test <- wilcox.test(sample1,sample2, paired = paired)
  }
  return(test)
}



# # # # import data # # # # ####
db <- read.csv("correct_210_raw.csv")

# # # # relevant columns # # # # ####
# names
relColNames <- c("Participant_num","Gender","Handed","Age","Digit","BDI_tot","bdi_9RM",
                 "DASS_A","DASS_D","DASS_S","Est_IQ","Group","Exclusions","exclusionskp",
                 "ActG1J1","ActG1J2","ActG2J1","ActG2J2","ActG3J1","ActG3J2","ActG4J1","ActG4J2",
                 "Cxt_game1_jud1","Cxt_game1_jud2","Cxt_game2_jud1","Cxt_game2_jud2","Cxt_game3_jud1","Cxt_game3_jud3","Cxt_game4_jud1","Cxt_game4_jud2",
                 "KP_game1_jud1","KP_game1_jud2","KP_game2_jud1","Kp_game2_jud2","KP_game3_jud1","KP_game3_jud3","KP_game4_jud1","KP_game4_jud2",
                 "ADP_game1_jud1","ADP_game1_jud2","ADP_game2_jud1","ADP_game2_jud2","ADP_game3_jud1","ADP_game3_jud3","ADP_game4_jud1","ADP_game4_jud2",
                 "ADP_Game1","ADP_Game2","ADP_Game3","ADP_Game4",
                 "PR_game1_jud1","PR_game1_jud2","PR_game2_jud1","PR_game2_jud2","PR_game3_jud1","PR_game3_jud3","PR_game4_jud1","PR_game4_jud2",
                 "PR_Game1","PR_Game2","PR_Game3","PR_Game4","Before","After")

# # # # filter relevant data set # # # # ####
# columns that will changed to integer
numChange <- relColNames[-c(1,2,3,7,12,13,14)]

# cycle to change numChange to integer
for (i in 1:length(numChange)) {
  db[,numChange[i]] <- as.numeric(as.character(db[,numChange[i]]))
}

# change column names
colnames(db) <- c("code","sex","handed","age","digitSpanScore","totalBeck","BDIhighLow",
                  "DASSanxiety","DASSdepression","DASSstress","estIQ","group","exclusions","exclusionskp",
                  "Ajudg1","Ajudg2","Ajudg3","Ajudg4","Ajudg5","Ajudg6","Ajudg7","Ajudg8",
                  "Cjudg1","Cjudg2","Cjudg3","Cjudg4","Cjudg5","Cjudg6","Cjudg7","Cjudg8",
                  "Kpress1","Kpress2","Kpress3","Kpress4","Kpress5","Kpress6","Kpress7","Kpress8",
                  "actDPblock1","actDPblock2","actDPblock3","actDPblock4","actDPblock5","actDPblock6","actDPblock7","actDPblock8",
                  "actDPoverall1","actDPoverall2","actDPoverall3","actDPoverall4",
                  "pResBlock1","pResBlock2","pResBlock3","pResBlock4","pResBlock5","pResBlock6","pResBlock7","pResBlock8",
                  "pResOverall1","pResOverall2","pResOverall3","pResOverall4","preRat","postRat")

## L&M reviewer 2, BDI cutoff >= 9 is high # 10/06/2020
# change variable names
db$BDIhighLow <- as.factor(ifelse(db$totalBeck > 8, "high", "low"))
# order factor
db$BDIhighLow <- factor(db$BDIhighLow, levels = c("low","high"))
# handed to upper case
db$handed <- as.factor(toupper(db$handed))
# excluding participants
if (exclude_part == 1) {
  dbExcluded <- db[is.na(db$exclusionskp),]
  dbFilter <- db[!is.na(db$exclusionskp),]
} else {
  dbFilter  <- db#[1:150,]
}
db$dbType <- ifelse(db$code > 150,"n60","n150")
db$exclusionskp[is.na(db$exclusionskp)] <- 2
# two data bases frequencies
table(db$dbType,db$exclusionskp)
table(db$BDIhighLow[db$exclusionskp == 2 & db$dbType == "n150"])


# # # # general characteristics table 1 # # # # ####
# N
table(dbFilter$BDIhighLow)
table(dbFilter$BDIhighLow,dbFilter$sex)
table(dbExcluded$BDIhighLow)
table(dbExcluded$BDIhighLow,dbExcluded$sex)
# age
mean(dbFilter[dbFilter$BDIhighLow=="low","age"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","age"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","age"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","age"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","age"],dbFilter[dbFilter$BDIhighLow=="low","age"],0)
# sex
table(dbFilter$BDIhighLow,dbFilter[,"sex"])
table(dbFilter$BDIhighLow,dbFilter[,"sex"])/cbind(table(dbFilter$BDIhighLow),table(dbFilter$BDIhighLow))
chisq.test(table(dbFilter$BDIhighLow,dbFilter[,"sex"]))
# handed
table(dbFilter$BDIhighLow,dbFilter[,"handed"])
table(dbFilter$BDIhighLow,dbFilter[,"handed"])/cbind(table(dbFilter$BDIhighLow),table(dbFilter$BDIhighLow))
chisq.test(table(dbFilter$BDIhighLow,dbFilter[,"handed"]))
# digit
mean(dbFilter[dbFilter$BDIhighLow=="low","digitSpanScore"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","digitSpanScore"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","digitSpanScore"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","digitSpanScore"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","digitSpanScore"],dbFilter[dbFilter$BDIhighLow=="low","digitSpanScore"],0)
# estimated iq
mean(dbFilter[dbFilter$BDIhighLow=="low","estIQ"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","estIQ"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","estIQ"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","estIQ"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","estIQ"],dbFilter[dbFilter$BDIhighLow=="low","estIQ"],0)
# total Beck Depression Inventory
mean(dbFilter[dbFilter$BDIhighLow=="low","totalBeck"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","totalBeck"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","totalBeck"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","totalBeck"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","totalBeck"],dbFilter[dbFilter$BDIhighLow=="low","totalBeck"],0)
# DASS anxiety
mean(dbFilter[dbFilter$BDIhighLow=="low","DASSanxiety"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","DASSanxiety"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","DASSanxiety"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","DASSanxiety"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","DASSanxiety"],dbFilter[dbFilter$BDIhighLow=="low","DASSanxiety"],0)
# DASS depression
mean(dbFilter[dbFilter$BDIhighLow=="low","DASSdepression"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","DASSdepression"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","DASSdepression"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","DASSdepression"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","DASSdepression"],dbFilter[dbFilter$BDIhighLow=="low","DASSdepression"],0)
# DASS stress
mean(dbFilter[dbFilter$BDIhighLow=="low","DASSstress"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="low","DASSstress"], na.rm = T)
mean(dbFilter[dbFilter$BDIhighLow=="high","DASSstress"], na.rm = T)
sd(dbFilter[dbFilter$BDIhighLow=="high","DASSstress"], na.rm = T)
f_mean_difference(dbFilter[dbFilter$BDIhighLow=="high","DASSstress"],dbFilter[dbFilter$BDIhighLow=="low","DASSstress"],0)



# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # change format data # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# scale (normalize) beck total score 
dbFilter$totalBeck <- scale(dbFilter$totalBeck)[1:nrow(dbFilter)]
# long format
lf <- melt(dbFilter, id.vars = c("group", "code", "BDIhighLow","totalBeck"), measure.vars =
             c('Ajudg1','Ajudg2','Ajudg3','Ajudg4','Ajudg5','Ajudg6','Ajudg7','Ajudg8',
               'Cjudg1','Cjudg2','Cjudg3','Cjudg4','Cjudg5','Cjudg6','Cjudg7','Cjudg8',
               'preRat','postRat'))
# pre post to judgments 
lf$intFinRat <- factor(revalue(lf$variable, c("Ajudg1"="int","Ajudg2"="fin","Ajudg3"="int","Ajudg4"="fin",
                                              "Ajudg5"="int","Ajudg6"="fin","Ajudg7"="int","Ajudg8"="fin",
                                              "Cjudg1"="int","Cjudg2"="fin","Cjudg3"="int","Cjudg4"="fin",
                                              "Cjudg5"="int","Cjudg6"="fin","Cjudg7"="int","Cjudg8"="fin",
                                              "preRat"="prePost","postRat"="prePost")
                               ),levels=c("int","fin","prePost"))
# action or context rating
lf$ratType <- substr(lf$variable,1,1)
# constant or change group
lf$constVar <- revalue(as.character(lf$group),c("1"="change","2"="change","3"="constant","4"="constant"))
# overal positive or zero delta p
lf$totDelP <- as.factor(revalue(as.character(lf$group),c("1"="pos","2"="pos","3"="zero","4"="pos")))
lf$totDelP <- factor(lf$totDelP, levels=c("zero","pos"))
# condition
lf$condition <- as.numeric(revalue(as.character(lf$variable),c("Ajudg1"="1","Ajudg2"="1","Ajudg3"="2","Ajudg4"="2",
                                                               "Ajudg5"="3","Ajudg6"="3","Ajudg7"="4","Ajudg8"="4",
                                                               "Cjudg1"="1","Cjudg2"="1","Cjudg3"="2","Cjudg4"="2",
                                                               "Cjudg5"="3","Cjudg6"="3","Cjudg7"="4","Cjudg8"="4",
                                                               "preRat"="0","postRat"="5")))
# block
lf$block <- as.numeric(revalue(as.character(lf$variable),c("Ajudg1"="1","Ajudg2"="2","Ajudg3"="3","Ajudg4"="4",
                                                           "Ajudg5"="5","Ajudg6"="6","Ajudg7"="7","Ajudg8"="8",
                                                           "Cjudg1"="1","Cjudg2"="2","Cjudg3"="3","Cjudg4"="4",
                                                           "Cjudg5"="5","Cjudg6"="6","Cjudg7"="7","Cjudg8"="8",
                                                           "preRat"="0","postRat"="9")))
                        
# change variable names
lf$rating <- lf$value; lf$value <- NULL
lf$participant <- lf$code; lf$code <- NULL
lf$group <- as.factor(revalue(as.character(lf$group),c("1"="ChaMode","2"="ChaPerf","3"="ConZer","4"="ConPos")))
# low per group
colSums((table(lf$participant,lf$group,lf$BDIhighLow) != 0)[,,1])
# high per group
colSums((table(lf$participant,lf$group,lf$BDIhighLow) != 0)[,,2])

# number of subjects
nSubj <- length(unique(lf$participant))


# # # # add p(response) # # # ####
# get long format p(R)
lf_pr <- melt(dbFilter, id.vars = c("group", "code"),
              measure.vars = c('pResBlock1','pResBlock2','pResBlock3','pResBlock4',
                               'pResBlock5','pResBlock6','pResBlock7','pResBlock8')); colnames(lf_pr) <- c("group","participant","block","prob")
lf_pr$block <- as.integer(substr(lf_pr$block,10,10))
lf_pr$code <- paste0(lf_pr$participant,"_",lf_pr$block)

# combine with previous data base
lf$code <- paste0(lf$participant,"_",lf$block)
lf$prob <- NA
for (i in 1:nrow(lf_pr)) {
  lf$prob[lf$code == lf_pr$code[i]] <- lf_pr[i,"prob"]
}

#### eliminate participants ####
# remove NA participant
lf <- lf[!is.na(lf$BDIhighLow),]


# # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # RATING MODEL # # # # # # # # ####
# # # # # # # # # # # # # # # # # # # # # # # # # 
# Specific sense of control 01/04/2020
# run the full model with the posible maximum 2 way interactions based on possible interactions
mFull_spe_BDIhighlow <- lmer(rating ~ prob + intFinRat + ratType + constVar + totDelP + condition + BDIhighLow +
                    prob:intFinRat + prob:ratType + intFinRat:ratType + prob:constVar +
                    intFinRat:constVar + ratType:constVar + prob:totDelP + intFinRat:totDelP + 
                    ratType:totDelP + prob:condition+ intFinRat:condition + ratType:condition + 
                    constVar:condition + totDelP:condition + prob:BDIhighLow + intFinRat:BDIhighLow + 
                    ratType:BDIhighLow + constVar:BDIhighLow + totDelP:BDIhighLow +
                    condition:BDIhighLow +
                    (1|group/participant) + (1|condition),REML = FALSE, 
                   control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lf[lf$intFinRat != "prePost",])
mFull_spe_BDIcont <- lmer(rating ~ prob + intFinRat + ratType + constVar + totDelP + condition + totalBeck +
                      prob:intFinRat + prob:ratType + intFinRat:ratType + prob:constVar +
                      intFinRat:constVar + ratType:constVar + prob:totDelP + intFinRat:totDelP + 
                      ratType:totDelP + prob:condition+ intFinRat:condition + ratType:condition + 
                      constVar:condition + totDelP:condition + prob:totalBeck + intFinRat:totalBeck + 
                      ratType:totalBeck + constVar:totalBeck + totDelP:totalBeck +
                      condition:totalBeck +
                      (1|group/participant) + (1|condition),REML = FALSE, 
                    control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lf[lf$intFinRat != "prePost",])
# model summary
summary(mFull_spe_BDIhighlow)
summary(mFull_spe_BDIcont)

# stepwise regression (eliminate irrelevant factors)
step(mFull_spe_BDIhighlow)
step(mFull_spe_BDIcont)

# create final models based on step() model found 
mFinal_spe_BDIhighlow <- lmer(rating ~ prob + ratType + constVar + totDelP + condition + BDIhighLow + 
                                (1 | participant:group) + (1 | condition) + prob:ratType + 
                                ratType:totDelP + ratType:condition + constVar:condition + 
                                ratType:BDIhighLow,
                 REML = FALSE,control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lf[lf$intFinRat != "prePost",])
mFinal_spe_BDIcont <- lmer(rating ~ prob + ratType + constVar + totDelP + condition + 
                             (1 | participant:group) + (1 | condition) + prob:ratType + ratType:constVar + 
                             ratType:totDelP + ratType:condition + constVar:condition,
                 REML = FALSE,control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lf[lf$intFinRat != "prePost",])

# create tables to be printed on csv files
if (!require(psycho)) {install.packages("psycho")}; library(psycho) # analyze()
csv_mFinal_spe_BDIhighlow <- summary(analyze(mFinal_spe_BDIhighlow, CI = 95)) %>% mutate(p = psycho::format_p(p))
csv_mFinal_spe_BDIcont <- summary(analyze(mFinal_spe_BDIcont, CI = 95)) %>% mutate(p = psycho::format_p(p))

# if psycho does not work, use the latest version in report
# if (!require(report)) {devtools::install_github("easystats/report")}; library(report) # report()
# csv_mFinal_spe_BDIhighlow <- table_long(report(mFinal_spe_BDIhighlow))
# csv_mFinal_spe_BDIcont <- table_long(report(mFinal_spe_BDIcont))
if (write_csv == 1) {
  write.csv(csv_mFinal_spe_BDIhighlow,paste0(getwd(),"/L&M submission/mFinal_spe_BDIhighlow_reviewerResponse_v3.csv"),row.names = F)
  write.csv(csv_mFinal_spe_BDIcont,paste0(getwd(),"/L&M submission/mFinal_spe_BDIcont_reviewerResponse_v3.csv"),row.names = F)
}

# post hoc, post reviewers. Robin comments on rating type x constant/positive x high low BDI
mZero <- lmer(rating ~ prob + ratType + condition + BDIhighLow + (1 | participant:group) + 
             (1 | condition) + prob:ratType + ratType:condition + condition +
             ratType:BDIhighLow,
             REML = FALSE,control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
             lf[lf$intFinRat != "prePost" & lf$totDelP == "zero",])
summary(mZero)
mPos <- lmer(rating ~ prob + ratType + constVar + condition + BDIhighLow + 
             (1 | participant:group) + (1 | condition) + prob:ratType + 
             ratType:condition + constVar:condition + ratType:BDIhighLow,
             REML = FALSE,control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),
           lf[lf$intFinRat != "prePost" & lf$totDelP == "pos",])
summary(mPos)


#### model comparison for BDI ####
# fitting no BDI model
mFinal_spe_noBDI <- lmer(rating ~ prob + ratType + constVar + totDelP + condition + 
                           (1 | participant:group) + (1 | condition) + prob:ratType + 
                           ratType:totDelP + ratType:condition + constVar:condition,
                 REML = FALSE,control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lf[lf$intFinRat != "prePost",])
# model comparison
anova(mFinal_spe_BDIhighlow, mFinal_spe_noBDI, test="LRT"); exp((BIC(mFinal_spe_BDIhighlow) - BIC(mFinal_spe_noBDI))/2); AIC(mFinal_spe_BDIhighlow) - AIC(mFinal_spe_noBDI)


#### General sense of control 01/04/2020 ####
lfGenSpec <- lf[lf$intFinRat == "prePost",]
lfGenSpec$prePost <- factor(as.factor(as.character(lfGenSpec$variable)),levels = c("preRat","postRat"))
mFull_gen <- lmer(rating ~ prePost + BDIhighLow + totDelP + constVar + 
                prePost:BDIhighLow + prePost:totDelP + prePost:constVar +
                BDIhighLow:totDelP + BDIhighLow:constVar + (1|group/participant),
              control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lfGenSpec)
summary(mFull_gen)
step(mFull_gen)
mFinal_gen <-lmer(rating ~ prePost + (1 | participant:group), 
                  control = lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=2e5)),lfGenSpec)
summary(mFinal_gen)
csv_mFinal_gen <- summary(analyze(mFinal_gen, CI = 95)) %>% mutate(p = psycho::format_p(p))
# csv_mFinal_gen <- table_long(report(mFinal_gen))
if (write_csv == 1) {
  write.csv(csv_mFinal_gen,paste0(getwd(),"/L&M submission/mFinal_gen_reviewerResponse_v3.csv"), row.names = F)
}



#### visualization parameters ####
sf <- 1
plot_title_size <- 48#24
strip_text_size <- 44#22
axis_title_size <- 40#20
axis_text_size <- 32#18
legend_title_size <- 38#20
legend_text_size <- 34#18
pos <- position_dodge(0.2)

theme_scdo <- theme(strip.background = element_blank(),
                    strip.text = element_text(size = sf*strip_text_size, face = "bold"),
                    plot.title = element_text(size = sf*plot_title_size, hjust = 0,face = "bold"),
                    plot.subtitle = element_text(size = sf*plot_title_size/2, hjust=0, face = "italic", color = "black"),
                    plot.margin = unit(c(1,1,1,1), "cm"),
                    axis.title.y = element_text(size = sf*axis_title_size, face = "bold"),
                    axis.text.y = element_text(size = sf*axis_text_size, colour="black"),
                    axis.title.x = element_text(size = sf*axis_title_size, face = "bold"),
                    axis.text.x = element_text(size = sf*axis_text_size, colour="black"),
                    axis.line = element_line(colour = 'black', size = sf*1.6),
                    axis.ticks = element_line(size = sf*1.6),
                    axis.ticks.length = unit(sf*10, "pt"),
                    legend.title = element_text(size = sf*legend_title_size), 
                    legend.text = element_text(size = sf*legend_text_size),
                    legend.position = "right",
                    legend.margin = margin(0.1, 0.1, 0.1, 0.1, "cm"),
                    legend.key = element_rect(colour = NA, fill = NA),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),  
                    panel.background = element_blank(),
                    panel.border = element_blank(),
                    panel.spacing = unit(3, "lines")
)
# # # # # # # # # # # # # #  #
lf3 <- lf
# # # # # # # # # # # # # # # # # # # 
#### visualization ratings graphs ####
# # # # # # # # # # # # # # # # # # # 
# ratings histogram
ggplot(lf, aes(x=rating,fill=BDIhighLow,color=BDIhighLow)) + 
  geom_histogram(binwidth = 10,alpha=0.5, position="identity") +
  theme(legend.position="top")



# # # # # # # # # #
#### Figure 1 ####
# # # # # # # # # #

# change group BDI factor order
lf$BDIhighLow <- factor(lf$BDIhighLow,levels=c("low","high"))
# lf$BDIhighLow <- factor(lf$BDIhighLow,levels=c("high","low"))
# change group names
levels(lf$group) <- c("0.5 -> 0", "1 -> 0", "0.25","0")
# lf$group <- factor(lf$group,levels=c("0","0.25","0.5 -> 0", "1 -> 0"))
lf$group <- factor(lf$group,levels = c("1 -> 0","0.5 -> 0","0.25","0"))

# create legend code variable
lf$legend <- as.factor(paste0(lf$BDIhighLow,ifelse(lf$ratType=="C","C","A")))
# x axis label
labXaxis <- c("pre","1-int","1-fin","2-int","2-fin","3-int","3-fin","4-int","4-fin","post")
# factor order (for legend)
lf$legend <- factor(as.factor(lf$legend),levels = c("lowA","lowC","highA","highC"))

fig2 <- ggplot(lf,aes(x=block,y=rating,col=legend,linetype=legend,shape=legend)) +
  geom_hline(yintercept=c(0,25,50,75), col="gray75", linetype="dashed") +
  # stat_summary(fun.y = mean, geom = "line", size = sf*2) +
  stat_summary(data=lf[lf$intFinRat!="prePost",],fun = mean, geom = "line", size = sf*1.5) +
  stat_summary(fun.data = "mean_cl_boot", size = sf*1.5, position = pos, fill = "white", linetype = "solid", stroke = sf*2) +
  labs(x="Ratings across blocks",y="Rating") +
  scale_y_continuous(breaks = c(0,50,100)) +
  scale_x_continuous(breaks = seq(-0,9,1),labels = labXaxis) +
  scale_shape_manual(name="Rating Type and BDI (Low & High)", # for legend space use: \n
                     breaks=c("lowA","lowC","highA","highC"),
                     labels=c("Action-low","Context-low","Action-high","Context-high"),
                     values = c(15,23,15,23)) +
  scale_linetype_manual(name="Rating Type and BDI (Low & High)",
                        breaks=c("lowA","lowC","highA","highC"),
                        labels=c("Action-low","Context-low","Action-high","Context-high"),
                        values = c("solid","dotdash","solid","dotdash")) +
  scale_colour_manual(name="Rating Type and BDI (Low & High)",
                      breaks=c("lowA","lowC","highA","highC"),
                      labels=c("Action-low","Context-low","Action-high","Context-high"),
                      values = rep(inferno(5)[c(4,2)],each=2)) +
  facet_grid(group~BDIhighLow) + theme_scdo + theme(legend.position = c(0.5, 0.485),#legend.position = "right",
                                                    legend.background = element_rect(fill="white",
                                                                                     size=0.5, linetype="solid", 
                                                                                     colour ="black"),
                                                    legend.margin = margin(0.2,0.2,0.2,0.2, "cm"),
                                                    axis.text.x = element_text(size=axis_text_size*0.9,
                                                                               angle=90,hjust = 1, vjust = 0.5)) +
  guides(shape=guide_legend(nrow=1,byrow=TRUE),linetype=guide_legend(nrow=1,byrow=TRUE),
         col=guide_legend(nrow=1,byrow=TRUE))
fig2
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig2_submitted2.png"),
         plot = fig2, width = 40, height = 40, units = "cm", dpi = 300, limitsize = T) #1.61803398875
}



#### post analysis 01/04/2020 (constant vs variable better estimate overall contingency) #### 
a<-lf[lf$ratType=="p",]
a$totDelP2 <- ifelse(a$totDelP=="pos",0.25,0)
a$totDel2ratError <- a$totDelP2 - (a$rating/100)

ggplot(a,aes(x=constVar,y=totDel2ratError,col=variable)) + stat_summary()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#### Figure 2: correlation between ratings and probabilities of response ####
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# this section uses lf from Rating Model
lf$BDIhighLow <- factor(lf$BDIhighLow, levels=c("low","high"))
# order factor legend
#lf$legend <- factor(as.factor(lf$legend),levels=c("lowA","highA","lowC","highC"))
lf$legend <- factor(as.factor(lf$legend),levels=c("highA","highC","lowA","lowC"))
# Figure 2a ####
fig3a <- ggplot(lf,aes(x=prob,y=rating,col=legend,linetype=legend,shape=legend)) + 
  geom_hline(yintercept = 0, col="gray50") +
  geom_point(alpha=0.2, size = 0.9) + 
  geom_smooth(method="lm",size=2.5,se=F) + 
  labs(x="p(response)", y="Rating") +
  scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1), minor_breaks = NULL) +
  scale_shape_manual(name="Rating Type and BDI",
                     labels=c("Action (A)-high", "Context (C)-high","Action (A)-low", "Context (C)-low"),
                     values = c(15,23,15,23)) +
  scale_linetype_manual(name="Rating Type and BDI",
                        labels=c("Action (A)-high", "Context (C)-high","Action (A)-low", "Context (C)-low"),
                        values = c("solid","dotdash","solid","dotdash")) +
  scale_colour_manual(name="Rating Type and BDI",
                      labels=c("Action (A)-high", "Context (C)-high","Action (A)-low", "Context (C)-low"),
                      values = rep(inferno(5)[c(2,4)],each=2)) + 
  facet_grid(.~BDIhighLow) + theme_scdo
  # guides(shape=guide_legend(nrow=2,byrow=TRUE),linetype=guide_legend(nrow=2,byrow=TRUE),col=guide_legend(nrow=2,byrow=TRUE))
fig3a
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3a_submitted2.png"),
         plot = fig3a, width = 35, height = 35/1.61803398875, dpi = 300, units = "cm",limitsize = T)
}
# try 2d density plot with stat_density_2d()

# change rating names
lf2<-lf[lf$ratType != "p",]
lf2$ratType <- ifelse(lf2$ratType == "A","Action","Context")

aHigh<-lf2[lf2$BDIhighLow == "high" & lf2$ratType == "Action",]
aLow<-lf2[lf2$BDIhighLow == "low" & lf2$ratType == "Action",]
cHigh<-lf2[lf2$BDIhighLow == "high" & lf2$ratType == "Context",]
cLow<-lf2[lf2$BDIhighLow == "low" & lf2$ratType == "Context",]
# behaviour and rating correlations
cor.aHigh <- cor.test(aHigh$prob,aHigh$rating)
cor.aLow <- cor.test(aLow$prob,aLow$rating)
cor.cHigh <- cor.test(cHigh$prob,cHigh$rating)
cor.cLow <- cor.test(cLow$prob,cLow$rating)
# context actions ratings correlations
cor.HighRat.AC <- cor.test(aHigh$rating,cHigh$rating)
cor.LowRat.AC <- cor.test(aLow$rating,cLow$rating)


# include action context rating difference to pc1
lfLong <- lf2[lf2$ratType == "Action",]#; c<-lf2[lf2$ratType == "Context",]
lfLong$ratingContex <-lf2$rating[lf2$ratType == "Context"]
lfLong$ratingDif <- lfLong$rating - lfLong$ratingContex; lfLong$ratType <- "Action-Context"

# Figure 2b ####
fig3b <- ggplot(lfLong,aes(x=prob,y=ratingDif,col=BDIhighLow)) + 
  geom_hline(yintercept = 0, col="gray50") +
  geom_point(alpha=0.2, size = 0.9) + 
  geom_smooth(method="lm",size=2.5,se=F) + 
  labs(title = "Difference",x="p(response)", y="Rating (A-C)", col="BDI",shape="BDI") +
  scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1), minor_breaks = NULL) +
  scale_colour_manual(name="BDI",
                      breaks = c("low","high"),
                      labels = c("low","high"),
                      values = inferno(5)[c(4,2)]) +
  theme_scdo
fig3b
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3b_submitted2.png"),
         plot = fig3b, width = 20, height = 20, dpi = 300, units = "cm",limitsize = T)
}
############################################################################################ #
fig3b1 <- ggplot(lfLong,aes(x=ratingContex,y=rating,col=BDIhighLow)) + 
  geom_hline(yintercept = 0, col="gray50") +
  geom_point(alpha=0.2, size = 0.9) + 
  geom_smooth(method="lm",size=2.5,se=F) + 
  labs(title = "Ratings",x="Context", y="Action", col="BDI",shape="BDI") +
  scale_x_continuous(limits = c(-100,100), breaks = c(-100,0,100), minor_breaks = NULL) +
  scale_colour_manual(name="BDI",
                      breaks = c("low","high"),
                      labels = c("low","high"),
                      values = inferno(5)[c(4,2)]) +
  theme_scdo
fig3b1
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3b1_submitted2.png"),
         plot = fig3b, width = 20, height = 20, dpi = 300, units = "cm",limitsize = T)
}
############################################################################################ #



# correlations
acHigh<-lfLong[lfLong$BDIhighLow == "high",]
acLow<-lfLong[lfLong$BDIhighLow == "low",]
cor.acHigh <- cor.test(acHigh$prob,acHigh$rating)
cor.acLow <- cor.test(acLow$prob,acLow$rating)

phCorr <- data.frame(c("aHigh","aLow","cHigh","cLow","acHigh","acLow","ratHighAC","ratLowAC"),
                     c(cor.aHigh$estimate,cor.aLow$estimate,cor.cHigh$estimate,cor.cLow$estimate,
                       cor.acHigh$estimate,cor.acLow$estimate,cor.HighRat.AC$estimate,cor.LowRat.AC$estimate),
                     rbind(cor.aHigh$conf.int,cor.aLow$conf.int,cor.cHigh$conf.int,cor.cLow$conf.int,
                           cor.acHigh$conf.int,cor.acLow$conf.int,cor.HighRat.AC$conf.int,cor.LowRat.AC$conf.int),
                     c(cor.aHigh$p.value,cor.aLow$p.value,cor.cHigh$p.value,cor.cLow$p.value,
                       cor.acHigh$p.value,cor.acLow$p.value,cor.HighRat.AC$p.value,cor.LowRat.AC$p.value))
names(phCorr) <- c("corrTypr","r","lower95%CI","higher95%CI","p")
if (write_csv == 1) {
  write.csv(phCorr,paste0(getwd(),"/L&M submission/phCorr_submitted2.csv"), row.names = F)
}



### Action and Context rating and p(reponse) correlations ### 
lf2 <- lf[lf$ratType != "p",]
lf2$ratType <- ifelse(lf2$ratType == "A","Action","Context")
# 16 correlation: 2 ratings x 4 groups x 2 BDI groups
lf2$corCode <- paste0(lf2$group,"_",lf2$BDIhighLow,"_",lf2$ratType)
#split by intFinRat: lf2$corCode <- paste0(lf2$group,"_",lf2$BDIhighLow,"_",lf2$ratType,"_",lf2$intFinRat)
corCode <- unique(lf2$corCode)
corres <- data.frame(matrix(NA,nrow=length(corCode),ncol=6)); colnames(corres) <- c("corrType","r","df","lowerCI","higherCI","p.value")
corres[,1] <- corCode
for (i in 1:length(corCode)) {
  a<-lf2[lf2$corCode == corCode[i],]
  b<-cor.test(a[,"prob"],a[,"rating"])
  corres[i,-1] <- c(b$estimate,b$parameter,b$conf.int,b$p.value)
};rm(a,b)
corres$group <- unlist(strsplit(corres$corrType, split = '_'))[seq(1,length(unlist(strsplit(corres$corrType, split = '_'))),by=3)]
corres$BDI <- as.factor(unlist(strsplit(corres$corrType, split = '_'))[seq(2,length(unlist(strsplit(corres$corrType, split = '_'))),by=3)])
corres$rating <- as.factor(unlist(strsplit(corres$corrType, split = '_'))[seq(3,length(unlist(strsplit(corres$corrType, split = '_'))),by=3)])
#split by intFinRat: corres$group <- unlist(strsplit(corres$corrType, split = '_'))[seq(1,length(unlist(strsplit(corres$corrType, split = '_'))),by=4)]
#split by intFinRat: corres$BDI <- as.factor(unlist(strsplit(corres$corrType, split = '_'))[seq(2,length(unlist(strsplit(corres$corrType, split = '_'))),by=4)])
#split by intFinRat: corres$rating <- as.factor(unlist(strsplit(corres$corrType, split = '_'))[seq(3,length(unlist(strsplit(corres$corrType, split = '_'))),by=4)])
#split by intFinRat: corres$intFinRat <- unlist(strsplit(corres$corrType, split = '_'))[seq(4,length(unlist(strsplit(corres$corrType, split = '_'))),by=4)]

### A-C rating and p(reponse) correlations ###
# 8 correlations: 1 rating difference x 4 groups x 2 BDI groups
#split by intFinRat: lfLong$corCode2 <- paste0(lfLong$group,"_",lfLong$BDIhighLow,"_",lfLong$intFinRat)
corCode <- unique(lfLong$corCode)
corres2 <- data.frame(matrix(NA,nrow=length(corCode),ncol=6)); colnames(corres2) <- c("corrType","r","df","lowerCI","higherCI","p.value")
corres2[,1] <- corCode
for (i in 1:length(corCode)) {
  a<-lfLong[lfLong$corCode2 == corCode[i],]
  b<-cor.test(a[,"prob"],a[,"rating"])
  corres2[i,-1] <- c(b$estimate,b$parameter,b$conf.int,b$p.value)
};rm(a,b)
corres2$group <- unlist(strsplit(corres2$corrType, split = '_'))[seq(1,length(unlist(strsplit(corres2$corrType, split = '_'))),by=2)]
corres2$BDI <- as.factor(unlist(strsplit(corres2$corrType, split = '_'))[seq(2,length(unlist(strsplit(corres2$corrType, split = '_'))),by=2)])
#split by intFinRat: corres2$group <- unlist(strsplit(corres2$corrType, split = '_'))[seq(1,length(unlist(strsplit(corres2$corrType, split = '_'))),by=3)]
#split by intFinRat: corres2$BDI <- as.factor(unlist(strsplit(corres2$corrType, split = '_'))[seq(2,length(unlist(strsplit(corres2$corrType, split = '_'))),by=3)])
#split by intFinRat: corres2$intFinRat <- as.factor(unlist(strsplit(corres2$corrType, split = '_'))[seq(3,length(unlist(strsplit(corres2$corrType, split = '_'))),by=3)])
corres2$rating <- "A-C"

# if plot with A-C then combine corres and corres2
#corres <- rbind(corres,corres2)
corres$legend <- as.factor(paste0(corres$BDI,corres$rating))
corres$BDI <- factor(corres$BDI, levels = c("low","high"))
# Figure 2c ####
fig3c <- ggplot(corres,aes(x=group,y=r,col=legend,shape=legend)) +
  geom_hline(yintercept = 0, col="gray50") +
  geom_errorbar(aes(ymin=lowerCI, ymax=higherCI), size=1.5,width=0.2, position=pos) + 
  geom_point(size=5, position = pos, fill = "white", stroke = sf*2) + 
  labs(x="Treatments",y="Correlation") +
  scale_y_continuous(limits = c(-0.6,0.6), breaks = c(-0.5,0,0.5), minor_breaks = NULL) +
  scale_shape_manual(name="Rating Type\nand BDI",
                     labels=c("Action (A)-high", "Context (C)-high","Action (A)-low", "Context (C)-low"),
                     values = c(15,23,15,23)) +
  scale_colour_manual(name="Rating Type\nand BDI",
                      labels=c("Action (A)-high", "Context (C)-high","Action (A)-low", "Context (C)-low"),
                      values = rep(inferno(5)[c(2,4)],each=2)) +
  coord_flip() + facet_grid(.~BDI) + theme_scdo
fig3c
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3c_submitted2.png"),
         plot = fig3c, width = 35, height = 35/1.61803398875, dpi = 300, units = "cm",limitsize = T)
}

#### exploring dot product correlations ####
# norm function
norm_vec <- function(x) sqrt(sum(x^2)) # which is the same as: sqrt(vec %*% vec)

HA <- corres[corres$BDI == "high" & corres$rating == "Action",]
HC <- corres[corres$BDI == "high" & corres$rating == "Context",]

LA <- corres[corres$BDI == "low" & corres$rating == "Action",]
LC <- corres[corres$BDI == "low" & corres$rating == "Context",]

normDotProd <- data.frame(c("high","low"),
                          c((HA$r %*% HC$r) / (norm_vec(HA$r)*norm_vec(HC$r)),
                            (LA$r %*% LC$r) / (norm_vec(LA$r)*norm_vec(LC$r)))
                          ); names(normDotProd) <- c("BDI","nDotProd")
# change factor order
normDotProd$BDI <- factor(normDotProd$BDI,levels=c("low","high"))

# Figure 2d ####
fig3d <- ggplot(normDotProd,aes(x=BDI,y=nDotProd,fill=BDI,col=BDI)) +
  geom_col(width = 0.6,size=3) + #stroke = 2stat = "identity", aes(fill = "transparent", size = ifelse(factor2 == "A", 2, 1)), size=2
  labs(title="Action-Context", y="Similarity") + 
  scale_colour_manual(name = "BDI",values = magma(5)[c(4,2)]) +
  scale_fill_manual(name = "BDI",values = c("white","white")) +
  theme_scdo
fig3d
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3d_submitted2.png"),
         plot = fig3d, width = 20, height = 20,  dpi = 300, units = "cm",limitsize = T)
}



# gigant Figure 2 (final) ####
fig3aa <- fig3a
fig3cc <- fig3c
leg3a <- get_legend(fig3a)
leg3c <- get_legend(fig3c)
fig3a <- fig3a + theme(legend.position = "none")
fig3c <- fig3c + theme(legend.position = "none")

fig3left <- ggarrange(fig3a,fig3c,nrow=2,align = "v",labels = c("a","c"),
                      font.label = list(size = 80, color = "black", face = "bold"),
                      label.x = 0, label.y = 1)
fig3left

fig3b <- fig3b + theme(legend.position = "none")
fig3d <- fig3d + theme(legend.position = "none")
fig3right <- ggarrange(fig3b,leg3c,fig3d,nrow=3,heights = c(1,0.6,1),labels = c("b","","d"),
                       font.label = list(size = 80, color = "black", face = "bold"),
                       label.x = 0, label.y = 1)
fig3right

fig3 <- ggarrange(fig3left,fig3right,ncol=2,widths = c(1.61803398875,1))
fig3

if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3_submitted2.png"),
         plot = fig3, width = 45, height = 45, units = "cm", dpi = 300, limitsize = T)
}


fig3b1 <- fig3b1 + theme(legend.position = "none")
fig3.v2 <- ggarrange(fig3a,ggarrange(fig3b1,leg3a,nrow=2,heights=c(1.61803398875,1)),
                     ncol=2,widths=c(1.61803398875,1),labels = c("a","b"),
                     font.label = list(size = 80, color = "black", face = "bold"),
                     label.x = 0, label.y = 1)
if (print_plots == 1) {
  ggsave(paste0(getwd(),"/L&M submission/fig3_v2_submitted2.png"),
         plot = fig3.v2, width = 45, height = 25, units = "cm", dpi = 300, limitsize = T)
}
