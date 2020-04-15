
### Script Info ###


# Helper_Halo Data Analysis Pipeline
# Author: Xiao Min Chang
# Date: April 13th, 2020


### ================================================== ###


### Load in libraries ###

library(tidyverse)
library(emmeans)



### Read in the dataset and factorize relevant columns ###

hh <- read_csv('Helper_Halo_Adults.csv')
cols <- c('ID', 'Category','Question')
hh[cols] <- lapply(hh[cols],factor)


### Data preparation and Descriptive analysis ###

# Recode Category variable
hh <- hh %>%
  mutate(characteristic=factor(
    case_when(
      Category==1~'helpful',
      Category==2~'generous',
      Category==3~'prestigious',
      Category==4~'considerate',
      Category==5~'attractive',
      Category==6~'intelligent'),
    levels=c('helpful','generous','prestigious','considerate','attractive','intelligent')
  ))

# Recode Question variable
hh <- hh %>%
  mutate(scenario=factor(
    case_when(
      Question==1~'prosocial',
      Question==2~'social',
      Question==3~'general'),
    levels=c('prosocial','social','general')
  ))

# summarise data across conditions
FC.sbj <- hh %>% 
  group_by(ID,characteristic,scenario) %>%
  summarise(FC=mean(FC_Answer))

CR.sbj <- hh %>%
  group_by(ID,characteristic,scenario) %>%
  summarise(CR=mean(CR_Answer)) 
  
res.sbj <- inner_join(FC.sbj,CR.sbj)

res.grp <- res.sbj %>%
  group_by(characteristic,scenario) %>%
  summarise(n=length(FC),
            mFC=mean(FC,na.rm=T), 
            FC.sd=sd(FC,na.rm=T),
            FC.sem=sd(FC,na.rm=T)/sqrt(n),
            mCR=mean(CR,na.rm=T), 
            CR.sd=sd(CR,na.rm=T),
            CR.sem=sd(CR,na.rm=T)/sqrt(n)
            )



### ANOVA and t-tests ###

# Testing the effect on FC
anova_FC <- aov(FC_Answer~Category + Question, data = hh)
summary(anova_FC)

# Post-hoc t-tests for FC
comFC1 <- pairs(emmeans(anova_FC, 'Category'))
comFC2 <- pairs(emmeans(anova_FC, 'Question'))

# Testing the effect on CR
anova_CR <- aov(CR_Answer~Category + Question, data = hh)
summary(anova_CR)

# Post-hoc t-tests for CR
comCR1 <- pairs(emmeans(anova_CR, 'Category'))
comCR2 <- pairs(emmeans(anova_CR, 'Question'))



### Plotting ###

# Plotting FC
plotFC<-res.grp %>% 
  ggplot(aes(x=characteristic,y=mFC,group=scenario))+
  geom_line(colour='red',size=1,aes(linetype=scenario))+
  geom_point(aes(colour=scenario))+
  geom_errorbar(aes(ymin=-FC.sem,ymax=FC.sem))+
  labs(x='Type of Characteristic', y='Possibility of choosing positive character',
       title='Choice between Characters')
ggsave('choice1.pdf',units='in',width=6,height=4)

# Another way to plot FC
plotFC2<-res.grp %>% 
  ggplot(aes(x=characteristic, y=mFC, fill=scenario)) +
  stat_summary(fun.y=mean,geom="bar", position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=.9), width=.3)+
  scale_fill_manual(values=c("#FF0000","#0000FF",'#008000'))+
  theme_bw() +
  labs(x='Type of Characteristic', y='Possibility of choosing positive character',
       title='Choice between Characters')
ggsave('choice2.pdf',units='in',width=6,height=4)


# Plotting CR
plotCR<-res.grp %>% 
  ggplot(aes(x=characteristic,y=mCR,group=scenario))+
  geom_line(colour='red',size=1,aes(linetype=scenario))+
  geom_point(aes(colour=scenario))+
  geom_errorbar(aes(ymin=-CR.sem,ymax=CR.sem))+
  labs(x='Type of Characteristic', y='Confidence rating',
       title='Confidence in the choices')
ggsave('confidence1.pdf',units='in',width=6,height=4)

# Another way to plot CR
plotCR2<-res.grp %>% 
  ggplot(aes(x=characteristic, y=mCR, fill=scenario)) +
  stat_summary(fun.y=mean,geom="bar", position=position_dodge(width=.9)) +
  stat_summary(fun.data=mean_se, geom="errorbar", position=position_dodge(width=.9), width=.3)+
  scale_fill_manual(values=c("#FF0000","#0000FF",'#008000'))+
  theme_bw() +
  labs(x='Type of Characteristic', y='Confidence rating',
       title='Confidence in the choices')
ggsave('confidence2.pdf',units='in',width=6,height=4)
