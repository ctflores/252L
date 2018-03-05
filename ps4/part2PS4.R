dfDIF <- read.table("https://raw.githubusercontent.com/ben-domingue/252L_winter2018/master/data/emp-3pl-math-reading.txt", header=TRUE)
library(dplyr)
library(tidyr)
library(mirt)
install.packages("lmtest")
library(lmtest)
#group reading
group <- dfDIF %>% select(contains("r.mc"))

#create rdgscore
for (i in 1:1462){
  dfDIF$rdgscore[i] = sum(group[i,])
}

#group math
groupm <- dfDIF %>% select(contains("m.mc"))

#create mathscore
for (i in 1:1462){
  dfDIF$math_X[i] = sum(groupm[i,])
}
?order
#sort by rdgscore
dfDIF<- dfDIF[order(dfDIF$rdgscore, dfDIF$math_X),]

#Variable G: by rdg score, Group 1 = lowscore
dfDIF$rdg_G = 0
for (i in 1:731){
  dfDIF$rdg_G[i] = 1
}

#Interactive Variable = XiG
dfDIF <- dfDIF %>% mutate(XiGi = math_X * rdg_G)

###***### do for each



#try model with m.mc1_1
dfm1_1 <- dfDIF[,c(1,2,103:106)]
dfm1_1 <- dfm1_1[order(-dfm1_1$rdg_G, dfm1_1$math_X),]
# find ratio of X
#grouped2 <- dfm1_1 %>% group_by(rdg_G) %>% group_by(math_X, add = TRUE) %>% add_tally
#dfm1_1 = dfm1_1 %>% mutate(X_Ratio = grouped2$n / 731)

#find E(Y|X,G)
grouped3 <- dfm1_1 %>% group_by(rdg_G) %>% group_by(math_X, add = TRUE) %>% summarize(mean(m.mc1_1))

#grouped4 <- dfm1_1 %>% select(c("rdg_G", "math_X", "X_Ratio")) %>% unique


#put E(Y|X,G)s on same graph
dfm1_1rdy <- merge(grouped3, dfm1_1, by = intersect(names(grouped3), names(dfm1_1)))
colnames(dfm1_1rdy)[3] <- "Pi"

#How to avoid or deal with Inf?
dfm1_1rdy = dfm1_1rdy %>% mutate(
  Qi = 1 - Pi,
  Z = log((Pi + .00000000000000001)/(Qi + .00000000000000001))
)
#likelihood ratio test? 
model0 <- glm(Z ~ math_X + rdg_G, data = dfm1_1rdy)
model1 <- glm(Z ~ math_X + rdg_G + XiGi, data = dfm1_1rdy)
lrtest(model0, model1)

model2 <- lm(Z ~ math_X + rdg_G, data = dfm1_1rdy)
model3 <- lm(Z ~ math_X + rdg_G + XiGi, data = dfm1_1rdy)
lrtest(model2,model3)
#compare deviance in models ?
model0$deviance - model1$deviance

