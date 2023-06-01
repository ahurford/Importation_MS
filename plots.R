source("RiskImp.R")

#estimated infected- bar plot
df1.sub <- select(IMP, c("report" , "RE.Int", "REW.Ca" ))
df1.sub <- df1.sub %>% rename(INT = RE.Int, CA = REW.Ca)
mdf1.sub <- melt(df1.sub, id=c("report"))

m7 <- ggplot() +
  geom_bar(data=mdf1.sub , aes(x=report, y=value, fill=variable), stat="identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(color=" ", 
       title="Epidemiological model predictions",
       y="\n Reported travel-related cases (daily)",
       x="") +
  guides(fill = guide_legend(title = "Departure Origin"))+
  theme(legend.position="right") +
  scale_fill_manual(values=c(cpalete[1], cpalete[2])) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)), 
                          legend.position="bottom" ,
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))


### infected percentage rw and r in Canada and International
df2.sub <- select(IMP, c("report", "RE.Int", "RE.Ca", "RW", "Total"))

for (i in 1:nrow(df2.sub)){
  for (j in 2:length(df2.sub)){
    df2.sub[i,j] <- (df2.sub[i,j]*100)/(df2.sub$Total[i])
  }
}
df2.sub <- select(df2.sub, -c("Total")) %>% rename(CA.r = RE.Ca, CA.rw = RW, INT = RE.Int)
mdf2.sub <- melt(df2.sub, id=c("report"))

m9 <- ggplot() +
  geom_bar(data=mdf2.sub , aes(x=report, y=value, fill=variable), stat="identity") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  labs(color=" ", 
       title="Epidemiological model predictions",
       y="\n Reported travel-related cases (daily %)",
       x="") +
  guides(fill = guide_legend(title = "Type of Travelers"))+
  theme(legend.position="right") +
  scale_fill_manual(values=c(cpalete[1], cpalete[2], "#FFCC63")) + 
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.3)), 
                          legend.position="bottom" ,
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))+ 
  scale_y_continuous(labels = function(x) paste0(x*1, "%"))


(m7+ m9) + plot_annotation(tag_levels = 'A')
ggsave("Figure/Figure5.png", width = 10, , height = 6, dpi= 500)
ggsave(file = "Figure/BMB/Figure5.eps", width = 10, , height = 6, dpi = 500) #dpi = 1200


#plot daily
plt1 <- c( "Best model"=cpalete[8], "Epidemiological model" = cpalete[7],"Data" = cpalete[1])
p1 <- ggplot() +
  geom_line(data=cdf1, aes(x=date, y= Rep.Int, color='Data'), size=1) +
  geom_bar(data=df1.Int, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(), alpha=.8 ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x="", y="\n Reported travel-related cases (daily)",
       title="Comparison of model predictions to reported travel-related cases \n International") +
  scale_fill_manual(name = "", labels = c( "Best model", "Epidemiological model","Data"), values = c( cpalete[8],cpalete[7], cpalete[1])) +
  scale_color_manual(values = plt1) +
  guides(color = guide_legend(title = ""), size=rel(1.4)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
                          legend.position="bottom",
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))


plt2 <- c( "Best model"=cpalete[8],"Epidemiological model" = cpalete[7], "Data" = cpalete[2])
p2 <- ggplot() +
  geom_line( data=cdf1, aes(x=date, y= Rep.Dom, color='Data'), size=1) +
  geom_bar(data=df1.Dom, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(), alpha=.8) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x="", y="\n Reported travel-related cases (daily)",
       title="Comparison of model predictions to reported travel-related cases \n Canada") +
  scale_fill_manual(name = "", labels = c( "Best-model", "Epidemiological-model","Reported"), values = c( cpalete[8],cpalete[7], cpalete[2])) +
  scale_color_manual(values = plt2) +
  guides(color = guide_legend(title = ""), size=rel(1.4)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
                          legend.position="bottom",
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))

#plot monthly
p3 <- ggplot()+
  geom_bar(data=df2.Int, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(),alpha=.8) +
  geom_line(data= cdf2, aes(x=date, y=RInt),stat="identity",color=cpalete[1], size=1) +
  geom_point(data= cdf2,  aes(x=date, y=RInt, color="Data"), size =2.8) +
  labs(x="", y="\n Reported travel-related cases (monthly)", title="International" ) +
  ylim(0, 30) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x="", y="\n Reported travel-related cases (monthly)",
       title="Comparison of model predictions to reported travel-related cases \n International") +
  scale_fill_manual(name = "", labels = c("Best model","Epidemiological model", "Data"), values = c( cpalete[8],cpalete[7], cpalete[1])) +
  scale_color_manual(values = plt1)+
  guides(color = guide_legend(title = ""), size=rel(1.4)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
                          legend.position="bottom",
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))


p4 <- ggplot() +
  geom_bar(data=df2.Dom, aes(x=date, y=value, fill=variable), stat="identity", position=position_dodge(),alpha=.8 ) +
  geom_line(data= cdf2, aes(x=date, y= RCA),stat="identity",color=cpalete[2], size=1) +
  geom_point(data= cdf2,  aes(x=date, y= RCA, color="Data"), size =2.8) +
  ylim(0, 120) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11))+
  labs(x="", y="\n Reported travel-related cases (monthly)",
       title="Comparison of model predictions to reported travel-related cases \n Canada") +
  scale_color_manual(values = plt2)+
  scale_fill_manual(name = "", labels = c( "Best model","Epidemiological model", "Data"), values = c( cpalete[8],cpalete[7], cpalete[2])) +
  guides(color = guide_legend(title = ""), size=rel(1.4)) +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, size=rel(1.2)),
                          legend.position="bottom",
                          legend.text=element_text(size=rel(1.2)),
                          plot.title=element_text(size=rel(1.3), face="bold"),
                          axis.title = element_text(size=rel(1.1)),
                          axis.text.y = element_text(size=rel(1.2)))

#, alpha=.8 geom_bar is not working with .eps format
p2+p1 + plot_annotation(tag_levels = 'A')
ggsave("Figure/Imp22.png", width = 15, , height = 6, dpi =500)
#ggsave(file = "Figure/BMB/Imp22.eps", width = 15, , height = 10, dpi = 1200)

# for the paper
(m7+m9)/(p4+p3) + plot_annotation(tag_levels = 'A')
ggsave("Figure/Figure6.png", width = 15, , height = 10, dpi =500)
#ggsave(file = "Figure/BMB/Figure6.eps", width = 15, , height = 10, dpi = 1200)


