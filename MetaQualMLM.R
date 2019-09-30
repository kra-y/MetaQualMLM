# MetaQualMLM
d<-read.csv("data.csv")
d%>%
  ggplot(aes(x = Score.by.individual,color = Subsector))+
  geom_density()+
  facet_grid(Subsector~.)+
  theme_minimal()

# we can estimate a multi-level regression that can account for the nested structure of the data
# we will first run a regression on the individual scores explained by the interaction between
# sector and the expert roles on the each panel.
summary(lm(0+rubav~Panel.role*Sector,data = d)) # no statistically significant estimates 
# for interacted terms. The closest indicates that automation and robotics do worse from technical reviewers.
# at  the end of the day the sector and expertise of the reviewer does not explain the variation in scores

summary(lm(rubav~Panel.role,data = d)) # NO
summary(lm(Score.by.individual~Sector,data = d)) # NO






rubric.mod<-lm(Score.by.individual~0+
                 Sector+
                 Panel.role+
                 Technical.merit.score+
                 Economic.impact.score+
                 Commercial.market.score+
                 Management.team.score+
                 Technical.team.score+
                 Budget.evaluation.score,data = d)# this model fits the data perfectly
# some points still have leverage beyond cook's distance
# but the rubric explains a lot of the individual scores which we would expect. At any rate this indicates
# nothing more than reviewers were following the rubric to score things but the interacted regression might
# idicate technical reviewrs being very critical of robotics and automation. I believe this modelling is done
# with this simple set of regressions, but lets rank these
d$rubric.mod.fitted<-fitted.values(rubric.mod)


plm<-ggplot(d,aes(x = reorder(Company.code, rubric.mod.fitted, FUN = mean),
                  y = rubric.mod.fitted,
                  label = Company.code,
                  color = Sector))+
  geom_boxplot()+
  theme(axis.ticks = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 65,hjust = 1),
        panel.grid = element_line(colour = "grey"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(), 
        plot.background = element_blank())+
  xlab(NULL)+
  ylab("Fitted Values of Score")+
  ggtitle("Rankings of Grant Applications\nEstimated by Linear Model")

rawranking<-ggplot(d,aes(x = reorder(Company.code, Score.by.individual, FUN = mean),
                         y = rubric.mod.fitted,
                         label = Company.code,
                         color = Sector))+
  geom_boxplot()+
  theme(axis.ticks = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 65,hjust = 1),
        panel.grid = element_line(colour = "grey"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(), 
        plot.background = element_blank())+
  xlab(NULL)+
  ylab("Fitted Values of Score")+
  ggtitle("Rankings of Grant Applications\nRaw Scores")

mlm.vivs<-lmer(Score.by.individual~0+Sector+Technical.merit.score+
                 Economic.impact.score+
                 Commercial.market.score+
                 Management.team.score+
                 Technical.team.score+
                 Budget.evaluation.score+(0+Sector+Technical.merit.score+
                                            Economic.impact.score+
                                            Commercial.market.score+
                                            Management.team.score+
                                            Technical.team.score+
                                            Budget.evaluation.score|Panel.role),data = d)
display(mlm.vivs)
summary(mlm.vivs)

d$mlm.vivs.fitted<-fitted(mlm.vivs)

pvivs<-ggplot(d,aes(x = reorder(Company.code, mlm.vivs.fitted, FUN = mean),y = mlm.vivs.fitted,color = Sector))+
  geom_boxplot()+
  theme(axis.ticks = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 65,hjust = 1),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey"),
        panel.border = element_blank(),
        strip.background = element_blank(), 
        plot.background = element_blank())+
  xlab(NULL)+
  ylab("Fitted Values of Score")+
  ggtitle("Rankings of Grant Applications\nEstimated by Hierarchical Model")
pvivs
plm
rawranking
rawsectorreviewerscoreplot<-d%>%
  ggplot(aes(x = Sector,y = Score.by.individual,fill = Panel.role))+
  geom_boxplot()+
  theme(axis.ticks = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 65,hjust = 1),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey"),
        panel.border = element_blank(),
        strip.background = element_blank(), 
        plot.background = element_blank())+
  ylab("Weighted Average Score")+
  scale_fill_discrete(name = "Reviewer Role")+
  ggtitle("Distribution of Grant Scores\nby Sector and Reviewer Panel Role")

lmsectorreviewerscoreplot<-d%>%
  ggplot(aes(x = Sector,y = rubric.mod.fitted,fill = Panel.role))+
  geom_boxplot()+
  theme(axis.ticks = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 65,hjust = 1),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey"),
        panel.border = element_blank(),
        strip.background = element_blank(), 
        plot.background = element_blank())+
  ylab("Linear Model Fitted Values")+
  scale_fill_discrete(name = "Reviewer Role")+
  ggtitle("Distribution of LM Fitted Values\nby Sector and Reviewer Panel Role")

mlmsectorreviewerscoreplot<-d%>%
  ggplot(aes(x = Sector,y = mlm.vivs.fitted,fill = Panel.role))+
  geom_boxplot()+
  theme(axis.ticks = element_blank(),
        legend.background = element_blank(), 
        legend.key = element_blank(),
        axis.text.x = element_text(angle = 65,hjust = 1),
        panel.background = element_blank(),
        panel.grid = element_line(colour = "grey"),
        panel.border = element_blank(),
        strip.background = element_blank(), 
        plot.background = element_blank())+
  ylab("Hierarchical Model Fitted Values")+
  scale_fill_discrete(name = "Reviewer Role")+
  ggtitle("Distribution of MLM Fitted Values\nby Sector and Reviewer Panel Role")



d%>%
  group_by(JID,Sector)%>%
  summarise(N = n())%>%
  arrange(desc(N))

d%>%
  group_by(JID)%>%
  summarise(N = n())%>%
  arrange(desc(N))


