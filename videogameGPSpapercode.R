#install relevant packages

#install.packages("openxlsx"); 
#install.packages("readxl"); 
#install.packages("data.table"); 
#install.packages("tidyr"); 
#install.packages("tidyverse"); 
#install.packages("zoo"); 
#install.packages("purrr"); 
#install.packages("car"); 
#install.packages("readr");
#install.packages("lm.beta");
#install.packages("ggplot2");
#install.packages("psych");
#install.packages("GPArotation");

library(car);library(openxlsx);library(ggplot2);library(readr);library(lm.beta);library(readxl); library(data.table); library(tidyr); library(tidyverse); library(zoo); library(dplyr);library(purrr);library(car);library("psych");library("GPArotation")

#calculating McDonald's Omega for factor saturation 

#NSQ questionnaire 

newsample = read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
XNSQF = newsample[,c('response1q','response2q','response3q','response4q','response5q','response6q','response7q','response8q','response9q','response10q','response11q','response12q','response13q','response14q')]
XNSQstrategy = newsample[,c('response6q','response5q','response11q')] 
XNSQability = newsample[,c('response2q','response3q','response4q','response12q','response13q')] 

omegaall = omega(XNSQF)
summary(omegaall)
omegability = omega(XNSQability)
summary(omegability)
omegastrategy = omega(XNSQstrategy)
summary(omegastrategy)

#GPS reliance scale 

GPSsplitabilityomega = newsample[,c('gps1q','gps3q','gps4q','gps6q','gps7q')]
omegaGPS= omega(GPSsplitabilityomega)
summary(omegaGPS)

#Weighted wayfinding distance 

wayfindingdistances <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/temp_dfalllevelsMarywithID.csv")
distanceabilityomega = wayfindingdistances[,c('Level.11','Level.32','Level.42','Level.68')]
omegadistance = omega(distanceabilityomega)
summary(omegadistance)

#Omega total is the total amount of variance in the group accounted for by the general + group 
#Omega general is the amount of variance in the group accounted for by a general factor 
#Omega group is the amount of variance that is common but unique to a particular subfactor 

#calculating split-half reliability 

#NSQ questionnaire 

install.packages("splithalf")
library("splithalf")
library("tidyr")

XNSQFsplitability = newsample[,c('short_id','response2q','response3q','response4q','response12q','response13q')]
XNSQFsplitstrategy = newsample[,c('short_id','response6q','response5q','response11q')]
XNSQFsplit = newsample[,c('short_id','response1q','response2q','response3q','response4q','response5q','response6q','response7q','response8q','response9q','response10q','response11q','response12q','response13q','response14q')]

XNSQFsplitpivotability = pivot_longer(XNSQFsplitability,cols=c('response2q','response3q','response4q','response12q','response13q'))
XNSQFsplitpivot = pivot_longer(XNSQFsplit,cols = c('response1q','response2q','response3q','response4q','response5q','response6q','response7q','response8q','response9q','response10q','response11q','response12q','response13q','response14q'))
XNSQFsplitpivotstrategy = pivot_longer(XNSQFsplitstrategy,cols=c('response6q','response11q','response5q'))

colnames(XNSQFsplitpivotability)[2] = 'item'
colnames(XNSQFsplitpivotstrategy)[2] = 'item'
colnames(XNSQFsplitpivot)[2] = 'item'

differenceability <- splithalf(data=XNSQFsplitpivotability,outcome="accuracy",score="average",halftype = "random",permutations=5000,var.ACC="value",var.participant='short_id',average="mean")
differencestrategy <-splithalf(data=XNSQFsplitpivotstrategy,outcome="accuracy",score="average",halftype = "random",permutations=5000,var.ACC="value",var.participant='short_id',average="mean")
difference <- splithalf(data=XNSQFsplitpivot,outcome="accuracy",score="average",halftype = "random",permutations=5000,var.ACC="value",var.participant='short_id',average="mean")

#GPS reliance scale 

GPSsplitability <- newsample[,c('short_id','gps1q','gps3q','gps4q','gps6q','gps7q')]
GPSsplitpivotability <- pivot_longer(GPSsplitability,cols=c('gps1q','gps3q','gps4q','gps6q','gps7q'))
colnames(GPSsplitpivotability)[2] <- 'item'
differenceability <- splithalf(data=GPSsplitpivotability,outcome="accuracy",score="average",halftype = "random",permutations=5000,var.ACC="value",var.participant='short_id',average="mean")

#Weighted wayfinding distance

wayfindingdistances <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/temp_dfalllevelsMarywithID.csv")
wayfindingdistances <- pivot_longer(wayfindingdistances,cols=c('Level.11','Level.32','Level.42','Level.68'))
wayfindingdistances <- wayfindingdistances[,-1]
colnames(wayfindingdistances)[2] <- 'item'
wayfindingability <- splithalf(data=wayfindingdistances,outcome="accuracy",score="average",halftype = "random",permutations=5000,var.ACC="value",var.participant='Short.ID',average="mean")

#plots and linear models for main analysis 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
correcteddf$gender[correcteddf$gender==1]='Male'
correcteddf$gender[correcteddf$gender==2]='Female'

#summary tables of demographic variables 

correcteddfsummary <- data.frame(correcteddf$distance,correcteddf$age,correcteddf$gender,correcteddf$GPSaverage,correcteddf$hours_of_phone_use_per_week,correcteddf$video_game_all_devices_hours_per_week)
colnames(correcteddfsummary) <- c("distance","age","gender","GPSaverage","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week")

correcteddfsummaryfemale <- correcteddfsummary[correcteddfsummary$gender=='Female',]
tab2female <- CreateTableOne(vars = c("distance","age","gender","GPSaverage","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week"), data = correcteddfsummaryfemale, factorVars = c("gender"))
tab2female <- print(tab2female,printToggle=FALSE)
write.csv(tab2female,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryfemalemary.csv")

correcteddfsummarymale <- correcteddfsummary[correcteddfsummary$gender=='Male',]
tab2male <- CreateTableOne(vars = c("distance","age","gender","GPSaverage","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week"), data = correcteddfsummarymale, factorVars = c("gender"))
tab2male <- print(tab2male,printToggle=FALSE)
write.csv(tab2male,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summarymalemary.csv")

correcteddfsummary <- data.frame(correcteddf$distance,correcteddf$age,correcteddf$gender,correcteddf$GPSaverage,correcteddf$hours_of_phone_use_per_week,correcteddf$video_game_all_devices_hours_per_week)
colnames(correcteddfsummary) <- c("distance","age","gender","GPSaverage","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week")
tab2 <- CreateTableOne(vars = c("distance","age","gender","GPSaverage","hours_of_phone_use_per_week","video_game_all_devices_hours_per_week"), data = correcteddfsummary, factorVars = c("gender"))
tab2 <- print(tab2,printToggle=FALSE)
write.csv(tab2,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/summaryMARY.csv")

variables <- c("distance", "age", "GPSaverage","hours_of_phone_use_per_week", "video_game_all_devices_hours_per_week")
values <- data.frame(variable = character(), t_stat = numeric(), p_value = numeric(), stringsAsFactors = FALSE)

for (var in variables) {
  ttest <- t.test(correcteddfsummaryfemale[[var]], correcteddfsummarymale[[var]])
  result <- data.frame(variable = var, t_stat = round(ttest$statistic,3), p_value = round(ttest$p.value,15))
  values <- rbind(values, result)
}

write.csv(values,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/summaryMARYtpvals.csv")

#visualisation plots 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/NEWSAMPLEMARYFORPLOTS.csv')
  
#plot for GPS binned 

correcteddf$binGPS <- 0 
correcteddf$binGPS[correcteddf$GPSaverage>=1 & correcteddf$GPSaverage<3]='>=1 and <3'
correcteddf$binGPS[correcteddf$GPSaverage>=3 & correcteddf$GPSaverage<4]='>=3 and <4'
correcteddf$binGPS[correcteddf$GPSaverage>=4]='>=4'

binGPSSEnong <- correcteddf %>%
  group_by(binGPS) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(binGPSSEnong$binGPS, levels=c(">=1 and <3", ">=3 and <4",">=4"))
p = ggplot(data=binGPSSEnong, aes(x=x1, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Average GPS reliance score") + labs(y = "Weighted wayfinding distance") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=0.4,vjust=-2,margin=margin(t = 0, r = 0, b = 20, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("GPSaverage.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "GPSaverage.png")
dev.off()

#plot for GPS raw

p <- ggplot(correcteddf, aes(x = GPSaverage, y = distance)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(
    x = "Average GPS reliance score",
    y = "Weighted wayfinding distance (VR-m)"
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold")
  )

ggsave(
  filename = "GPSaverage_linear.png",
  path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/",
  plot = p
)

#plot for GPS binned boxplot

p <- ggplot(correcteddf, aes(x = gender,y = GPSaverage,color=gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(0, NA) + 
  labs(x = "Gender", y = "Average GPS reliance score") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))

print(p)

#plot for GPS binned violin plot

p <- ggplot(correcteddf, aes(x = gender, y = GPSaverage, fill = gender)) +
  geom_violin(scale = "width", trim = FALSE) +  # Using geom_violin for violin plots
  stat_summary(fun = "mean", geom = "point", shape = 21, fill = "white", color = "black", size = 4) + 
  ylim(0, NA) + 
  labs(x = "Gender", y = "Average GPS reliance score") +
  scale_fill_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  
    legend.text = element_text(size = 20, face = "bold"),    
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(fill = guide_legend(title = "Gender"))

print(p)

#GPS with gender interaction

binGPSSE <- correcteddf %>%
  group_by(binGPS,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(binGPSSE$binGPS, levels=c(">=1 and <3", ">=3 and <4",">=4"))
p = ggplot(data=binGPSSE, aes(x=x1, y=mean,col=gender)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + scale_color_manual(values = c('grey', 'black')) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Average GPS reliance score") + labs(y = "Weighted wayfinding distance (VR-m)") + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=0.4,vjust=-2,margin=margin(t = 0, r = 0, b = 20, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("GPSaveragegender.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "GPSaveragegender.png",width=8)
dev.off()

#gps with gender interaction raw 

p <- ggplot(correcteddf, aes(x = GPSaverage, y = distance, col= gender)) +
  scale_color_manual(values = c('grey', 'black')) +
  guides(color=guide_legend("Gender",override.aes = list(fill = NA))) + 
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA, colour = NA)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = gender)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(
    x = "Average GPS reliance score",
    y = "Weighted wayfinding distance (VR-m)"
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold")
  )

ggsave(
  filename = "gps_linearinteraction.png",
  path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/",
  plot = p
)

#GPS with gender interaction boxplot

correcteddf$binGPS <- 0 
correcteddf$binGPS[correcteddf$GPSaverage >= 1 & correcteddf$GPSaverage < 3] = '>=1 and <3'
correcteddf$binGPS[correcteddf$GPSaverage >= 3 & correcteddf$GPSaverage < 4] = '>=3 and <4'
correcteddf$binGPS[correcteddf$GPSaverage >= 4] = '>=4'

p <- ggplot(correcteddf, aes(x = binGPS, y = distance, fill = gender)) +
  geom_violin(scale = "width", trim = FALSE) +
  ylim(8, 14) +
  labs(x = "Average GPS reliance score", y = "Weighted wayfinding distance (VR-m)") +
  scale_fill_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.5, vjust = -1),  
    axis.title.y = element_text(size = 20, face = "bold"),  
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(fill = guide_legend(title = "Gender"))

print(p)

#GPS x gender interaction violin plot

p <- ggplot(correcteddf, aes(x = binGPS, y = distance, fill = gender)) +
  geom_violin(scale = "width", trim = FALSE) +
  stat_summary(fun = "mean", geom = "point", shape = 21, fill = "white", color = "black", size = 4, position = position_dodge(width = 0.75), aes(group = gender)) + 
  ylim(8, 14) +
  labs(x = "Average GPS reliance score", y = "Weighted wayfinding distance (VR-m)") +
  scale_fill_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.5, vjust = -1),  
    axis.title.y = element_text(size = 20, face = "bold"),  
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(fill = guide_legend(title = "Gender"))

print(p)

#weekly hours of video gaming plot 

correcteddf$binvideo[correcteddf$binvideo=='>=1 and <5 hpw gaming on all devices']='>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo=='>=5 and <10 hpw gaming on all devices']='>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo=='>=10 hpw gaming on all devices']= '>=10'

binvideoSE <- correcteddf %>%
  group_by(binvideo) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

#weekly hours of video gaming plot raw

p <- ggplot(correcteddf, aes(x = video_game_all_devices_hours_per_week, y = distance)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(
    x = "Weekly hours of video gaming",
    y = "Weighted wayfinding distance (VR-m)"
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold")
  )

ggsave(
  filename = "videoaverage_linear.png",
  path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/",
  plot = p
)

#weekly hours of video gaming boxplot

p <- ggplot(correcteddf, aes(x = gender,y =video_game_all_devices_hours_per_week,color=gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(0, NA) + 
  labs(x = "Gender", y = "Average weekly hours of video gaming") +
  scale_fill_manual(values = c("black", "grey"))  +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))

print(p)

#weekly hours of video gaming violin plot

p <- ggplot(correcteddf, aes(x = gender, y = video_game_all_devices_hours_per_week, fill = gender)) +
  geom_violin(scale = "width", trim = FALSE) +
  stat_summary(fun = "mean", geom = "point", shape = 21, fill = "white", color = "black", size = 4) + 
  ylim(0, NA) +
  labs(x = "Gender", y = "Average weekly hours of video gaming") +
  scale_fill_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  
    legend.text = element_text(size = 20, face = "bold"),   
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(fill = guide_legend(title = "Gender"))

print(p)

#video x gender interaction

binvideoSEgender <- correcteddf %>%
  group_by(binvideo,gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

x1  = factor(binvideoSEgender$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p = ggplot(data=binvideoSEgender, aes(x=x1, y=mean,col=gender)) + scale_color_manual(values = c('grey', 'black')) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours per week video gaming on all devices") + labs(y = "Weighted wayfinding distance (VR-m)") + guides(color=guide_legend("Gender")) + theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA)) + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",hjust=1.0,vjust=-0.3),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("videogameallhpwgender.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "videogameallhpwgender.png",width=8)
dev.off()

#weekly hours of video gaming x gender plot raw

p <- ggplot(correcteddf, aes(x = video_game_all_devices_hours_per_week, y = distance, col= gender)) +
  scale_color_manual(values = c('grey', 'black')) +
  guides(color=guide_legend("Gender",override.aes = list(fill = NA))) + 
  theme(legend.title=element_text(size=20,face="bold"),legend.text=element_text(size=20,face="bold"),legend.key = element_rect(fill = NA, colour = NA)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE, aes(colour = gender)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(
    x = "Weekly hours of video gaming",
    y = "Weighted wayfinding distance (VR-m)"
  ) +
  theme(
    axis.text = element_text(size = 20),
    axis.title = element_text(size = 20, face = "bold")
  )

ggsave(
  filename = "videoaverage_linearinteraction.png",
  path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/",
  plot = p
)

#video x gender interaction boxplot 

correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

print(p)

#video x gender interaction violin plot

correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, fill = gender)) +
  geom_violin(scale = "width", trim = FALSE) +
  stat_summary(fun = "mean", geom = "point", shape = 21, fill = "white", color = "black", size = 4, position = position_dodge(width = 0.75), aes(group = gender)) + 
  ylim(8, 14) +
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_fill_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),
    legend.text = element_text(size = 20, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(fill = guide_legend(title = "Gender"))

print(p)

#video x gender interaction boxplot - Mahalanobis'

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/NEWSAMPLEMARYFORPLOTS.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "Mahalanobis")
print(p)

#video x gender interaction boxplot 3SD 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary3SD.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "3SD")
print(p)

#video x gender interaction boxplot 2SD 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary2SD.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "2SD")
print(p)

#video x gender interaction boxplot IQR

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMaryIQR.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "IQR")
print(p)

#GPS x gender interaction boxplot Mahalanobis' 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/NEWSAMPLEMARYFORPLOTS.csv')
correcteddf$binGPS <- 0 
correcteddf$binGPS[correcteddf$GPSaverage >= 1 & correcteddf$GPSaverage < 3] = '>=1 and <3'
correcteddf$binGPS[correcteddf$GPSaverage >= 3 & correcteddf$GPSaverage < 4] = '>=3 and <4'
correcteddf$binGPS[correcteddf$GPSaverage >= 4] = '>=4'

p <- ggplot(correcteddf, aes(x = binGPS, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Average GPS reliance score", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "Mahalanobis")
print(p)

#GPS x gender interaction boxplot 3SD 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary3SD.csv')
correcteddf$binGPS <- 0 
correcteddf$binGPS[correcteddf$GPSaverage >= 1 & correcteddf$GPSaverage < 3] = '>=1 and <3'
correcteddf$binGPS[correcteddf$GPSaverage >= 3 & correcteddf$GPSaverage < 4] = '>=3 and <4'
correcteddf$binGPS[correcteddf$GPSaverage >= 4] = '>=4'

p <- ggplot(correcteddf, aes(x = binGPS, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Average GPS reliance score", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "3SD")
print(p)

#GPS x gender interaction boxplot 2SD 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary2SD.csv')
correcteddf$binGPS <- 0 
correcteddf$binGPS[correcteddf$GPSaverage >= 1 & correcteddf$GPSaverage < 3] = '>=1 and <3'
correcteddf$binGPS[correcteddf$GPSaverage >= 3 & correcteddf$GPSaverage < 4] = '>=3 and <4'
correcteddf$binGPS[correcteddf$GPSaverage >= 4] = '>=4'

p <- ggplot(correcteddf, aes(x = binGPS, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Average GPS reliance score", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "2SD")
print(p)

#GPS x gender interaction boxplot IQR 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMaryIQR.csv')
correcteddf$binGPS <- 0 
correcteddf$binGPS[correcteddf$GPSaverage >= 1 & correcteddf$GPSaverage < 3] = '>=1 and <3'
correcteddf$binGPS[correcteddf$GPSaverage >= 3 & correcteddf$GPSaverage < 4] = '>=3 and <4'
correcteddf$binGPS[correcteddf$GPSaverage >= 4] = '>=4'

p <- ggplot(correcteddf, aes(x = binGPS, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Average GPS reliance score", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "IQR")
print(p)

#lineplot with error bars but no line for GPS reliance men and women

GPSSE <- correcteddf %>%
  group_by(gender) %>%
  summarise( 
    n=n(),
    mean=mean(GPSaverage),
    sd=sd(GPSaverage)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(GPSSE,aes(x=gender, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + coord_cartesian(ylim = c(3,4)) + scale_y_continuous(breaks=c(3,3.5,4),labels=c(3,3.5,4)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Gender") + labs(y = "Average GPS reliance score") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("GPSbygendersummaryboxplot.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "GPSbygendersummaryboxplot.png")
dev.off()

#lineplot for video game hours men and women

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
correcteddf$gender[correcteddf$gender==1]='Male'
correcteddf$gender[correcteddf$gender==2]='Female'

videogameSE <- correcteddf %>%
  group_by(gender) %>%
  summarise( 
    n=n(),
    mean=mean(video_game_all_devices_hours_per_week),
    sd=sd(video_game_all_devices_hours_per_week)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p <- ggplot(data = videogameSE, aes(x = gender, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, size = 0.5) +
  coord_cartesian(ylim = c(0, 15)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x = "Gender") +
  labs(y = "Average weekly hours of video gaming") +
  theme(axis.text = element_text(size = 20), axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)), axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)))
png("genderhours.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "genderhours.png")
dev.off()

#covariates + supplementary analysis 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/NEWSAMPLEMARYFORPLOTS.csv")
correcteddf$gender[correcteddf$gender==1]='Male'
correcteddf$gender[correcteddf$gender==2]='Female'

#line plot for age 

p = ggplot(data=correcteddf, aes(x=age, y=distance, group=1)) +  ylim(5,20) + geom_point() + geom_smooth(level=.6827,method="lm") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0)))  
png("age.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "age.png")
dev.off()

#plot for age binned 

ageSE <- correcteddf %>%
  group_by(binage) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(ageSE,aes(x=binage, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2)  + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Age") + labs(y = "Weighted wayfinding distance") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=45,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("ageboxplot.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "ageboxplot.png")
dev.off()

#gender plot 

genderSE <- correcteddf %>%
  group_by(gender) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

p = ggplot(genderSE,aes(x=gender, y=mean)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Gender") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x = element_text(angle=0,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("genderboxplot.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "genderboxplot.png")
dev.off()

#weekly hours of phone use plot

phoneSE <- correcteddf %>%
  group_by(binphone) %>%
  summarise( 
    n=n(),
    mean=mean(distance),
    sd=sd(distance)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

phoneSE$binphone[phoneSE$binphone=='>=1 and <10 hpw phone use']='>=1 and <10'
phoneSE$binphone[phoneSE$binphone=='>=10 and <20 hpw phone use']='>=10 and <20'
phoneSE$binphone[phoneSE$binphone=='>=20 and <30 hpw phone use']= '>=20 and <30'
phoneSE$binphone[phoneSE$binphone=='>=30 and <40 hpw phone use']= '>=30 and <40'
phoneSE$binphone[phoneSE$binphone=='>=40 hpw phone use']= '>=40'

x1  = factor(phoneSE$binphone, levels=c(">=1 and <10",">=10 and <20",">=20 and <30",">=30 and <40",">=40"))
p = ggplot(data=phoneSE, aes(x=x1, y=mean, group=1)) + geom_errorbar(aes(ymin=mean-se, ymax=mean+se),width=0,size=0.5) + geom_point(size=2) + ylim(8,14) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Hours of phone use per week") + labs(y = "Weighted wayfinding distance") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("hoursphoneuse.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "hoursphoneuse.png")
dev.off()

#weekly hours of phone use line plot 

p = ggplot(data=correcteddf, aes(x=hours_of_phone_use_per_week, y=distance, group=1)) + geom_smooth(level=.6827,method="lm") + geom_point() + ylim(5,20) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black")) + labs(x = "Weekly hours of phone use") + labs(y = "Weighted wayfinding distance (VR-m)") + theme(axis.text = element_text(size = 20),axis.text.x=element_text(angle=35,vjust=0.5),axis.title.x=element_text(size=20,face="bold",margin = margin(t = 20, r = 0, b = 0, l = 0)),axis.title.y=element_text(size=20,face="bold",margin = margin(t = 0, r = 20, b = 0, l = 0))) 
png("hoursphoneuselinear.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "hoursphoneuselinearMARY.png")
dev.off()

#highest education level achieved plot

educationSE <- correcteddf %>%
  group_by(education_coded) %>%
  summarise( 
    n = n(),
    mean = mean(distance),
    sd = sd(distance)
  ) %>%
  mutate( se = sd / sqrt(n))  %>%
  mutate( ic = se * qt((1-0.05)/2 + .5, n-1))

educationSE$education_coded[educationSE$education_coded == 0] = "High School or below"
educationSE$education_coded[educationSE$education_coded == 1] = "2-Year College/Uni or above"

x1  = factor(educationSE$education_coded, levels=c("High School or below","2-Year College/Uni or above"))
p <- ggplot(data = educationSE, aes(x = x1, y = mean, group = 1)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0, size = 0.5) +
  geom_point(size = 2) +
  ylim(8, 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 25, hjust = 0.5,vjust=0.5)) +
  labs(x = "Highest education level achieved",
       y = "Weighted Wayfinding Distance (VR-m)") +
  theme(axis.text = element_text(size = 20),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(plot.margin = unit(c(2.5, 0.5, 0.5, 0.5), "cm"))

png("educationMARY.png")
print(p)
ggsave(path = "/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/", filename = "educationMARY.png")
dev.off()

#statistical analysis 

#main model 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducation.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducation.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducation.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducation.csv", row.names=FALSE)

#male vs female GPS reliance 

male_df <- subset(correcteddf, gender == 1)
model_male <- lm(zscore ~ age + GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week + education_coded,
                 data = male_df)
female_df <- subset(correcteddf, gender == 2)
model_female <- lm(zscore ~ age + GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week + education_coded,
                   data = female_df)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducation_male.csv")
print(summary(model_male))
closeAllConnections()
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducation_female.csv")
print(summary(model_female))
closeAllConnections()

#running the model without video gaming as a covariate

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducationnbutnogaming.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + education_coded,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducationbutnogaming.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducationbutnogaming.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducationbutnogaming.csv", row.names=FALSE)

#running the model with game genre and platform used

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
correcteddf$gender[correcteddf$gender==1]='Male'
correcteddf$gender[correcteddf$gender==2]='Female'
correcteddf <- correcteddf[correcteddf$genre != 'I do not play video games',] 
  
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$genre <-as.factor(correcteddf$genre)
correcteddf$game_platform <-as.factor(correcteddf$game_platform)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$game_category <- ifelse(correcteddf$genre %in% c("Role-playing", "Simulation", "Action"), "Navigation", "Non-navigation")

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/marymodelgamegenreplatform.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded + game_category,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2genreplatform.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwithgenreplatform.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywithgenreplatform.csv", row.names=FALSE)

#looking at differences in genre between men and women

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
role_play <- subset(correcteddf, genre == "Role-playing")
male_role_play <- sum(role_play$gender == 1)
female_role_play <- sum(role_play$gender == 2)
all_games <- subset(correcteddf, genre != "Role-playing")
male_all_games <- sum(all_games$gender == 1)
female_all_games <- sum(all_games$gender == 2)
genre_table <- matrix(c(male_role_play, female_role_play, male_all_games, female_all_games), nrow = 2)
rownames(genre_table) <- c("Male", "Female")
colnames(genre_table) <- c("Role-playing", "Other")
chisq.test(genre_table)

#running the model with gaming level 1 data 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/newsamplepreZscorelevel1.csv')
correcteddf$gender[correcteddf$gender==1]='Male'
correcteddf$gender[correcteddf$gender==2]='Female'
correcteddf <- correcteddf[correcteddf$genre != 'I do not play video games',] 
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$genre <-as.factor(correcteddf$genre)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$game_category <- ifelse(correcteddf$genre %in% c("Role-playing", "Simulation", "Action"), "Navigation", "Non-navigation")

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/marymodelgaminglevel1.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded + game_category,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2genreplatformgaminglevel1.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwithgenreplatformgaminglevel1.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywithgenreplatformgaminglevel1.csv", row.names=FALSE)

#running the original model including NSQ total, strategy and ability subscales as predictor variables

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$NSQaverage <- scale(correcteddf$NSQaverage)
correcteddf$NSQabilityaverage <- scale(correcteddf$NSQabilityaverage)
correcteddf$NSQstrategyaverage <- scale(correcteddf$NSQstrategyaverage)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducationNSQ.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded + NSQaverage + NSQabilityaverage + NSQstrategyaverage,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducationNSQ.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducationNSQ.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducationNSQ.csv", row.names=FALSE)

#main model with outliers removed using 3SD method 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary3SD.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducation3SD.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducation3SD.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducation3SD.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducation3SD.csv", row.names=FALSE)

#main model with outliers removed using 2SD method 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary2SD.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducation2SD.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducation2SD.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducation2SD.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducation2SD.csv", row.names=FALSE)

#main model with outliers removed using IQR method 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMaryIQR.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducationIQR.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducationIQR.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducationIQR.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducationIQR.csv", row.names=FALSE)

#vid x gen boxplot - 3SD

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary3SD.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "3SD")
print(p)

#vid x gen boxplot - 2SD

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary2SD.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "2SD")
print(p)

#vid x gen boxplot - IQR

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMaryIQR.csv')
correcteddf$binvideo[correcteddf$binvideo == '>=1 and <5 hpw gaming on all devices'] = '>=1 and <5'
correcteddf$binvideo[correcteddf$binvideo == '>=5 and <10 hpw gaming on all devices'] = '>=5 and <10'
correcteddf$binvideo[correcteddf$binvideo == '>=10 hpw gaming on all devices'] = '>=10'

x1  = factor(correcteddf$binvideo, levels=c(">=1 and <5", ">=5 and <10", ">=10"))
p <- ggplot(correcteddf, aes(x = x1, y = distance, color = gender)) +
  geom_boxplot(outlier.shape = NA) +  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.75), size = 3) +
  ylim(8,14) + 
  labs(x = "Hours per week video gaming on all devices", y = "Weighted wayfinding distance (VR-m)") +
  scale_color_manual(values = c("black", "grey")) +
  theme(
    axis.text = element_text(size = 20),  
    axis.title.x = element_text(size = 20, face = "bold", hjust = 0.4, vjust = -2, margin = margin(t = 0, r = 0, b = 20, l = 0)),  
    axis.title.y = element_text(size = 20, face = "bold", margin = margin(t = 0, r = 20, b = 0, l = 0)),  
    legend.title = element_text(size = 20, face = "bold"),  # Set legend title font size and style
    legend.text = element_text(size = 20, face = "bold"),    # Set legend text font size and style
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  guides(color = guide_legend(title = "Gender"))  # Adjusting the legend title label

p <- p + labs(title = "IQR")
print(p)

#main model new variation

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/newsampleDIFFERENTMETHOD.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$short_id <- as.factor(correcteddf$short_id)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
correcteddf$level <- as.factor(correcteddf$level)
correcteddf$rawdistancelevel1 <- scale(correcteddf$rawdistancelevel1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducationNEWMETHOD.csv")
library(nlme)
formula <- rawdistance ~ age + gender * GPSaverage + hours_of_phone_use_per_week +
  video_game_all_devices_hours_per_week * gender +
  video_game_all_devices_hours_per_week * GPSaverage +
  education_coded + rawdistancelevel1
model <- lme(
  fixed = formula,
  random = ~1 | short_id,
  data = correcteddf
)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducationNEWMETHOD.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducationNEWMETHOD.csv")
confinter <- intervals(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducationNEWMETHOD.csv", row.names=FALSE)

#testing main model after removing 40 random participants (rows) from the dataframe 

correcteddf <- read.csv('/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/modified_df_without_40.csv')
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded,data=correcteddf)
output <- summary(model)

#main model with 0.01 Mahalanobis' threshold 

correcteddf <- read.csv("/Users/macbook/Desktop/UCL PhD Work/SHQoutputs/PREPROCESSING/mostrecentnewsampleMary001.csv")
correcteddf$gender <-as.factor(correcteddf$gender)
correcteddf$age <- scale(correcteddf$age)
correcteddf$GPSaverage <- scale(correcteddf$GPSaverage)
correcteddf$hours_of_phone_use_per_week <- scale(correcteddf$hours_of_phone_use_per_week)
correcteddf$video_game_all_devices_hours_per_week <- scale(correcteddf$video_game_all_devices_hours_per_week)
correcteddf$education_coded <- ifelse(correcteddf$education %in% c("Some formal education", "High School"), 0, 1)

sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmarywitheducation.csv")
model <- lm(zscore ~ age + gender*GPSaverage + hours_of_phone_use_per_week + video_game_all_devices_hours_per_week*gender + video_game_all_devices_hours_per_week*GPSaverage + education_coded,data=correcteddf)
print(summary(model))
closeAllConnections() 
output <- summary(model)
effect_sizes <- cohens_f(model)
cohen_f_squared_values <- (effect_sizes$Cohens_f_partial)^2
variable_names <- effect_sizes$Parameter
cohen_f_df <- data.frame(Variable = variable_names,
                         Cohen_f = cohen_f_squared_values,
                         stringsAsFactors = FALSE)
colnames(cohen_f_df)[1] = 'Variable'
colnames(cohen_f_df)[2] = 'Cohens f2' 
cohen_f_df$`Cohens f2` <- round(cohen_f_df$`Cohens f2`,2)
write.csv(cohen_f_df,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/maryCF2witheducation.csv", row.names=FALSE)
sink("/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/modelmaryCIwitheducation.csv")
confinter <- confint(model)
print(confinter)
sink()
closeAllConnections() 
anovaoutput <- Anova(model,type=3)
vifoutput <- vif(model)
vifoutput <- as.data.frame(vifoutput)
vifoutput <- cbind(newColName = rownames(vifoutput), vifoutput)
rownames(vifoutput) <- 1:nrow(vifoutput)
colnames(vifoutput)[1] <- "Variables"
write.csv(vifoutput,"/Users/macbook/Desktop/UCL PhD Work/Backgroundresearchethicsplans/MostrecentMary/VIFmarywitheducation.csv", row.names=FALSE)
