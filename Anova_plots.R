########################################
# Line plots for Machado et al. (2021) #
########################################

# required packages
library(plyr)
library(ggplot2)
library(ggpubr)

# load file in long format
df <- read.csv(file="Study Spreadsheet.csv")

# check data structure
str(df)

# check col indexes
data.frame(colnames(df))

# specify columns that will be turned into factors
col <- c(11:13)

# lapply function to transform selected columns into factors
df[,col] <- lapply(df[,col],factor)

# changing names of task levels
levels(df$task)

levels(df$task) <- c("Reactivity", "Recovery", "Rest")

# reordering the levels of task
df$task <- factor(df$task, levels = c("Rest", "Reactivity", "Recovery"))

levels(df$task)

# summarize variables with ddply, interactions
hr_sum <- ddply(df, c("task","dysf.factor"), summarise,
                N = length(hr),
                mean = mean(hr),
                sd = sd(hr),
                se = sd/sqrt(N),
                ci = se*1.96)

sdnn_sum <- ddply(df, c("task","dysf.factor"), summarise,
                  N = length(sdnn.nlog),
                  mean = mean(sdnn.nlog),
                  sd = sd(sdnn.nlog),
                  se = sd/sqrt(N),
                  ci = se*1.96)

rmssd_sum <- ddply(df, c("task","dysf.factor"), summarise,
               N = length(rmssd.nlog),
               mean = mean(rmssd.nlog),
               sd = sd(rmssd.nlog),
               se = sd/sqrt(N),
               ci = se*1.96)

lf_sum <- ddply(df, c("task","dysf.factor"), summarise,
                N = length(lf.nlog),
                mean = mean(lf.nlog),
                sd = sd(lf.nlog),
                se = sd/sqrt(N),
                ci = se*1.96)

hf_sum <- ddply(df, c("task","dysf.factor"), summarise,
                N = length(hf.nlog),
                mean = mean(hf.nlog),
                sd = sd(hf.nlog),
                se = sd/sqrt(N),
                ci = se*1.96)

# ggplot2 line graphs for interactions

# heart rate and dysfunctional coping plot
hr_graph <- ggplot(hr_sum, aes(x=task, y=mean, group = dysf.factor, 
                               linetype=dysf.factor))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8, 
                position=position_dodge(0.2)) +
  geom_line(position=position_dodge(0.2),size=0.8) +
  geom_point(position=position_dodge(0.2),size=2)+
  labs(x="", y = "Heart rate (bpm)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5,1.5,2.5), y = c(96,96,90,90), 
           label = c("*","*","*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# sdnn and dysfunctional coping plot
sdnn_graph <- ggplot(sdnn_sum, aes(x=task, y=mean, group = dysf.factor, 
                                   linetype=dysf.factor))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8, 
                position=position_dodge(0.2)) +
  geom_line(position=position_dodge(0.2),size=0.8) +
  geom_point(position=position_dodge(0.2),size=2)+
  labs(x="", y = "SDNN (nlog)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5), y = c(3.6,3.6), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# rmssd and dysfunctional coping plot
rmssd_graph <- ggplot(rmssd_sum, aes(x=task, y=mean, group = dysf.factor, 
                                     linetype=dysf.factor))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8, 
                position=position_dodge(0.2)) +
  geom_line(position=position_dodge(0.2),size=0.8) +
  geom_point(position=position_dodge(0.2),size=2)+
  labs(x="", y = "RMSSD (nlog)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5), y = c(3.3,3.3), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# low frequency and dysfunctional coping plot
lf_graph <- ggplot(lf_sum, aes(x=task, y=mean, group = dysf.factor, 
                               linetype=dysf.factor))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8, 
                position=position_dodge(0.2)) +
  geom_line(position=position_dodge(0.2),size=0.8) +
  geom_point(position=position_dodge(0.2),size=2)+
  scale_y_continuous(breaks = seq(from = 5.7, to = 6.9, by = 0.2)) +
  labs(x="", y = "LF (nlog)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.35), y = c(6.15,6.15), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# high frequency and dysfunctional coping plot
hf_graph <- ggplot(hf_sum, aes(x=task, y=mean, group = dysf.factor, 
                               linetype=dysf.factor))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8, 
                position=position_dodge(0.2)) +
  geom_line(position=position_dodge(0.2),size=0.8) +
  geom_point(position=position_dodge(0.2),size=2)+
  labs(x="", y = "HF (nlog)")+
  theme_classic()+
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# arrange the 5 plots together
multi_plot <- ggarrange(hr_graph,
                          ggarrange(sdnn_graph, rmssd_graph, 
                                    lf_graph, hf_graph, 
                                    labels = c("B", "C", "D","E"),
                                    ncol = 2, nrow = 2), 
                          nrow = 2, labels = "A")

# saving in high resolution
tiff("multi_aov.tiff", units="in", width=7, height=8, res=300)
multi_plot
dev.off()

# summarize variables with ddply, only for task
hr_sum_task <- ddply(df, c("task"), summarise,
                N = length(hr),
                mean = mean(hr),
                sd = sd(hr),
                se = sd/sqrt(N),
                ci = se*1.96)

sdnn_sum_task <- ddply(df, c("task"), summarise,
                  N = length(sdnn.nlog),
                  mean = mean(sdnn.nlog),
                  sd = sd(sdnn.nlog),
                  se = sd/sqrt(N),
                  ci = se*1.96)

rmssd_sum_task <- ddply(df, c("task"), summarise,
                   N = length(rmssd.nlog),
                   mean = mean(rmssd.nlog),
                   sd = sd(rmssd.nlog),
                   se = sd/sqrt(N),
                   ci = se*1.96)

lf_sum_task <- ddply(df, c("task"), summarise,
                N = length(lf.nlog),
                mean = mean(lf.nlog),
                sd = sd(lf.nlog),
                se = sd/sqrt(N),
                ci = se*1.96)

hf_sum_task <- ddply(df, c("task"), summarise,
                N = length(hf.nlog),
                mean = mean(hf.nlog),
                sd = sd(hf.nlog),
                se = sd/sqrt(N),
                ci = se*1.96)

# ggplot2 line graphs for task

# heart rate task plot
hr_graph_task <- ggplot(hr_sum_task, aes(x=task, y=mean, group = 1))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8) +
  geom_line(size=0.8) +
  geom_point(size=2)+
  labs(x="", y = "Heart rate (bpm)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5), y = c(95,95), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# sdnn task plot
sdnn_graph_task <- ggplot(sdnn_sum_task, aes(x=task, y=mean, group = 1))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8) +
  geom_line(size=0.8) +
  geom_point(size=2)+
  labs(x="", y = "SDNN (nlog)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5), y = c(3.6,3.6), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# rmssd task plot
rmssd_graph_task <- ggplot(rmssd_sum_task, aes(x=task, y=mean, group = 1))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8) +
  geom_line(size=0.8) +
  geom_point(size=2)+
  labs(x="", y = "RMSSD (nlog)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5), y = c(3.3,3.3), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# low frequency task plot
lf_graph_task <- ggplot(lf_sum_task, aes(x=task, y=mean, group = 1))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8) +
  geom_line(size=0.8) +
  geom_point(size=2)+
  scale_y_continuous(breaks = seq(from = 5.7, to = 6.9, by = 0.2)) +
  labs(x="", y = "LF (nlog)")+
  theme_classic()+
  annotate("text", x = c(1.5,2.5), y = c(6.5,6.5), 
           label = c("*","*"), color="black", 
           size=7, fontface="bold") +
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))

# high frequency task plot
hf_graph_task <- ggplot(hf_sum_task, aes(x=task, y=mean, group = 1))+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1, size=0.8) +
  geom_line(size=0.8) +
  geom_point(size=2)+
  labs(x="", y = "HF (nlog)")+
  theme_classic()+
  theme(legend.position = "none", 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, 
                                                    b = 0, l = 0)))


# arrange the 5 plots together, task only
multi_plot_2 <- ggarrange(hr_graph_task,
                          ggarrange(sdnn_graph_task, rmssd_graph_task, 
                                    lf_graph_task, hf_graph_task, 
                                    labels = c("B", "C", "D","E"),
                                    ncol = 2, nrow = 2), 
                          nrow = 2, labels = "A")

# saving in high resolution
tiff("multi_aov_2.tiff", units="in", width=7, height=8, res=300)
multi_plot_2
dev.off()
