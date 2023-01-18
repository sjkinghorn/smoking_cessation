library(asaur)
library(tidyverse)
library(survival)
data("pharmacoSmoking")
smoking = pharmacoSmoking
head(smoking)

# time
ggplot(data=smoking, mapping=aes(x=ttr)) + 
  geom_histogram(bins=20) + labs(title="Distribution of time until relapse", 
                                 x="time in days until relpase")
table(smoking$ttr)
view(arrange(smoking, ttr))
count(filter(smoking, ttr==0))/count(smoking)
count(filter(smoking, relapse==0))/count(smoking)

# remove 0's
df = filter(smoking, ttr!=0)

# all censored observations at max relapse time
arrange(df, desc(ttr))[1:40,1:3]

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
