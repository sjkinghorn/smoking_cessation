library(asaur)
library(tidyverse)
library(survival)
library(corrplot)
data("pharmacoSmoking")
smoking = pharmacoSmoking
head(smoking)
dim(smoking)

# time
ggplot(data=smoking, mapping=aes(x=ttr)) + 
  geom_histogram(bins=20) + labs(title="Distribution of time until relapse", 
                                 x="time in days until relpase")
table(smoking$ttr)
view(arrange(smoking, ttr))
count(filter(smoking, ttr==0))/count(smoking)
count(filter(smoking, ttr==182))/count(smoking)
count(filter(smoking, relapse==0))/count(smoking)

# remove 0's
# df = filter(smoking, ttr!=0)
df = smoking

# all censored observations at max relapse time
arrange(df, desc(ttr))[1:40,1:3]

conts = c('age', 'yearsSmoking', 'priorAttempts', 'longestNoSmoke')
df_conts = df[unlist(conts)]
M = cor(df_conts)
testRes = cor.mtest(df_conts, conf.level = 0.95)
corrplot(corr=M, p.mat = testRes$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.8, order = 'AOE', diag=FALSE)

# group assignment
table(df$grp)

# age
ggplot(data=df, mapping=aes(x=age)) + 
  geom_histogram(bins=20) + labs(title="Distribution of age", 
                                 x="age in years") + theme_minimal()

# employment
table(df$employment)
ggplot(data=df, mapping=aes(x=employment)) + 
  geom_bar() + labs(title="Bar plot of employment status") + theme_minimal()

# age and grp
ggplot(data=df, mapping=aes(x=age)) + geom_histogram() + facet_wrap(~grp) + theme_minimal()

# age and employment
ggplot(data=df, mapping=aes(x=age)) + geom_histogram() + facet_wrap(~employment) + theme_minimal()
