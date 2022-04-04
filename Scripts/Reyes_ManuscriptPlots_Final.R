#-------------------------------------------------------------------------------
# SCIP Manuscript Plots
# Created by: Rochelle-Jan Reyes, Olivia Pham
#-------------------------------------------------------------------------------

############################### Set Up ###############################

# Working Directory
getwd()
setwd('/Users/Rochelle/Desktop/SCIP/SCIP_Manuscript/')

# Libraries
library(ggplot2) #make plots
library(cowplot) #grid arrange for zoom, slack, webinar
library(png) #to download plots as a png
library(dplyr) #renaming columns and recoding data observations
library(gridExtra) #use of grid.arrange
library(tm) #corpus for word frequency and wordcloud
library(wordcloud) #wordcloud
library(grid) #for rasterGrob function
library(scales)

# Read in Data
##2020##
MidSurvey20 <- read.csv("MidSurvey_Summer2020.csv")
PostSurvey20 <- read.csv("PostSurvey_Summer2020.csv")
PairedSurvey20 <- read.csv("PairedSurveys_Summer2020.csv")
Retention20 <- read.csv("SCIP_Retention_NoID.csv")

##2021##
PreSurvey21 <- read.csv("PreSurvey-Summer2021-cleaned.csv")
PostSurvey21 <- read.csv("PostSurvey-Summer2021-cleaned.csv")
PairedSurvey21 <- read.csv("PairedSurveys_Summer2021.csv")

# Combined 2020 and 2021
names(PostSurvey20)[names(PostSurvey20) == "udacity_course"] <- "course"
CourseEff20 <- subset(PostSurvey20, select = c(zoom_eff_post, slack_comments, webinar, udacity_rec, SCIP_rec, course))
CourseEff21 <- subset(PostSurvey21, select = c(zoom_eff_post, slack_comments, webinar, udacity_rec, SCIP_rec, course))

CourseEff20$year <- '2020'
CourseEff21$year <- '2021'

CourseEffAll <- rbind(CourseEff20, CourseEff21)

# Color Palettes
NinePalette <- c("#dc6acf","#fb3640","#f7b267","#fed766","#a9ffcb","#44cf6c","#247ba0","#3b429f","#1d3461")
SixPalette <- c("#dc6acf","#fb3640","#fed766","#44cf6c","#009fb7","#3b429f")
FivePalette <- c("#a9ffcb","#44cf6c","#247ba0","#3b429f","#1d3461")
FourPalette <- c("#dc6acf","#fb3640", "#f7b267","#fed766")
TwoPalette <- c("#dc6acf", "#1d3461")

################## AIM 1: Virtual Course Effectiveness ##################
### Figure 3. Slack, Team Meetings, and Webinar ###
##2020 and 2021 ##

#Change to percentages
ZoomPercent20 <- CourseEff20 %>% 
  select(zoom_eff_post) %>%
  group_by(zoom_eff_post) %>%
  summarise(ZoomNum=n()) %>%
  mutate(ZoomPcnt20=ZoomNum/sum(ZoomNum))

ZoomPercent21 <- CourseEff21 %>% 
  select(zoom_eff_post) %>%
  group_by(zoom_eff_post) %>%
  summarise(ZoomNum=n()) %>%
  mutate(ZoomPcnt21=ZoomNum/sum(ZoomNum))

SlackPercent20 <- CourseEff20 %>% 
  select(slack_comments) %>%
  group_by(slack_comments) %>%
  summarise(SlackNum=n()) %>%
  mutate(SlackPcnt20=SlackNum/sum(SlackNum))

SlackPercent21 <- CourseEff21 %>% 
  select(slack_comments) %>%
  group_by(slack_comments) %>%
  summarise(SlackNum=n()) %>%
  mutate(SlackPcnt21=SlackNum/sum(SlackNum))

WebinarPercent20 <- CourseEff20 %>% 
  select(webinar) %>%
  group_by(webinar) %>%
  summarise(WebNum=n()) %>%
  mutate(WebPcnt20=WebNum/sum(WebNum))

WebinarPercent21 <- CourseEff21 %>% 
  select(webinar) %>%
  group_by(webinar) %>%
  summarise(WebNum=n()) %>%
  mutate(WebPcnt21=WebNum/sum(WebNum))

#Zoom Effectiveness
Zoom_Eff20 <- ggplot(ZoomPercent20, aes(x = factor(zoom_eff_post), y = ZoomPcnt20)) + geom_col(fill = '#fb3640') + 
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels=c("Not \n Effective", "Somewhat \n Not Effective",
                            "Effective", "Somewhat \n Very Effective", 
                            "Very \n Effective")) + ylab("Percentage of Responses") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim = c(0, 0.5)) +
  theme_light() + theme(axis.title.x = element_blank(), axis.text=element_text(size=12), 
                        strip.background = element_rect(fill = '#FB3640'), strip.text = element_text(size = 12))

##Add in observation for not effective in Zoom_Eff21 data

Zoom_Eff21_NEObs <- data.frame(1, 0, 0.000)    
names(Zoom_Eff21_NEObs) <- c("zoom_eff_post", "ZoomNum", "ZoomPcnt21")
ZoomPercent21 <- rbind(ZoomPercent21, Zoom_Eff21_NEObs)

Zoom_Eff21 <- ggplot(ZoomPercent21, aes(x = factor(zoom_eff_post), y = ZoomPcnt21)) + geom_col(fill = '#fb3640') + 
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels=c("Not \n Effective", "Somewhat \n Not Effective",
                            "Effective", "Somewhat \n Very Effective", 
                            "Very \n Effective")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim = c(0, 0.5)) +
  theme_light() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text=element_text(size=12), 
                        strip.background = element_rect(fill = '#FB3640'), strip.text = element_text(size = 12))

#Slack usefulness
Slack_Eff20 <- ggplot(SlackPercent20, aes(x = factor(slack_comments), y = SlackPcnt20)) + geom_col(fill = "#44cf6c") + 
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels=c("Not \n Effective", "Somewhat \n Not Effective",
                            "Effective", "Somewhat \n Very Effective", 
                            "Very \n Effective")) + ylab("Percentage of Responses") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim = c(0, 0.5)) +
  theme_light() + theme(axis.title.x = element_blank(), axis.text=element_text(size=12), 
                        strip.background = element_rect(fill = '#44CF6C'), strip.text = element_text(size = 12))

Slack_Eff21 <- ggplot(SlackPercent21, aes(x = factor(slack_comments), y = SlackPcnt21)) + geom_col(fill = "#44cf6c") + 
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels=c("Not \n Effective", "Somewhat \n Not Effective",
                            "Effective", "Somewhat \n Very Effective", 
                            "Very \n Effective")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim = c(0, 0.5)) +
  theme_light() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text=element_text(size=12), 
                        strip.background = element_rect(fill = '#44CF6C'), strip.text = element_text(size = 12))


#Webinar usefulness
Webinar_Eff20 <- ggplot(WebinarPercent20, aes(x = factor(webinar), y = WebPcnt20)) + geom_col(fill = "#3B429F") +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels=c("Not \n Useful", "Somewhat \n Not Useful",
                            "Useful", "Somewhat \n Very Useful", 
                            "Very \n Useful")) + ylab("Percentage of Responses") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim = c(0, 0.5)) +
  theme_light() + theme(axis.title.x = element_blank(), axis.text=element_text(size=12), 
                        strip.background = element_rect(fill = '#3B429F'), strip.text = element_text(size = 12))

Webinar_Eff21 <- ggplot(WebinarPercent21, aes(x = factor(webinar), y = WebPcnt21)) + geom_col(fill = "#3B429F") +
  scale_x_discrete(breaks = c("1", "2", "3", "4", "5"),
                   labels=c("Not \n Useful", "Somewhat \n Not Useful",
                            "Useful", "Somewhat \n Very Useful", 
                            "Very \n Useful")) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_cartesian(ylim = c(0, 0.5)) +
  theme_light() + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text=element_text(size=12), 
                        strip.background = element_rect(fill = '#3B429F'), strip.text = element_text(size = 12))

#Combining Zoom Slack, and Webinar Plots together
YearLabels <- ggdraw() + draw_label("Summer 2020 Cohort", x = 0.25, y = 0.99, hjust = 0.5, vjust = 1, 
                                    fontface = "bold", size = 18) + 
  draw_label("Summer 2021 Cohort", x = 0.75, y = 0.99, hjust = 0.5, vjust = 1, fontface = "bold", 
             size = 18)

ZoomPlot <- plot_grid(Zoom_Eff20, Zoom_Eff21, ncol = 2)
ZoomPlotTitle <- ggdraw() + draw_label("Team Meetings on Zoom", x = 0.115, y = 0.85, hjust = 0.5, vjust = 1)
ZoomPlotFinal <- plot_grid(ZoomPlotTitle, ZoomPlot, ncol = 1, rel_heights=c(0.15, 1))

SlackPlot <- plot_grid(Slack_Eff20, Slack_Eff21, ncol = 2)
SlackPlotTitle <- ggdraw() + draw_label("Slack Communication", x = 0.1, y = 0.85, hjust = 0.5, vjust = 1)
SlackPlotFinal <- plot_grid(SlackPlotTitle, SlackPlot, ncol = 1, rel_heights=c(0.15, 1))

WebinarPlot <- plot_grid(Webinar_Eff20, Webinar_Eff21, ncol = 2)
WebinarPlotTitle <- ggdraw() + draw_label("Webinar Usefulness", x = 0.1, y = 0.85, hjust = 0.5, vjust = 1)
WebinarPlotFinal <- plot_grid(WebinarPlotTitle, WebinarPlot, ncol = 1, rel_heights=c(0.15, 1))

ZSWPlot <- plot_grid(ZoomPlotFinal, SlackPlotFinal, WebinarPlotFinal,
                     ncol = 1, label_size = 12, labels = "AUTO") 

plot_grid(YearLabels, ZSWPlot, ncol = 1, rel_heights = c(0.1, 4)) #1200x800

############## AIM 2: Virtual Scientific Community Effectiveness ##############
### Figure 5. Word Clouds ###
##2020##

#Read in positive comments as a corpus to read sentences
PosComm20 <- Corpus(VectorSource(subset(PostSurvey20, select = c("ID", "positive_comments"))))
inspect(PosComm20)

#Remove numbers
PosComm20 <- tm_map(PosComm20, removeNumbers)
#Remove filler words
PosComm20 <- tm_map(PosComm20, removePunctuation)
PosComm20 <- tm_map(PosComm20, removeWords, c("the", "and", "was", "that", "like", "were", "with", "for", "own", "very", "had", "how",
                                              "able", "liked", "really", "it", "are", "when"))
PosComm20 <- tm_map(PosComm20, removeWords, c("our", "any", "could", "all", "not"))

#Create dataframe for plot and wordcloud
pcm20 <- TermDocumentMatrix(PosComm20)
m20 <- as.matrix(pcm20)
v20 <- sort(rowSums(m20), decreasing = TRUE)
PosComm20Final <- data.frame(word = names(v20), freq = v20)
head(PosComm20Final, 15)

#Create margin for bar plot
par(mar=c(5,7,3,4))

#Bar plot
barplot(PosComm20Final[1:15, ]$freq, las = 1, names.arg = PosComm20Final[1:15, ]$word, 
        ylim = c(0, 20), col = "#7EBC89",
        main = "Word Frequency in Positive Comments", xlab = "Frequency", horiz = TRUE, xaxs = 'i')

#Wordcloud
dev.new(width = 1000, height = 1000, unit = "px") #opens a new device with more space to plot
set.seed(4000)
par(mar=c(1,1,1,1))
wordcloud(words = PosComm20Final$word, freq = PosComm20Final$freq, scale = c(5, 0.5),
          min.freq = 2, max.words = Inf, random.order = FALSE, rot.per = 0.5,
          colors = NinePalette) #saved in external dev

#Make grid of the two plots
WC20 <- readPNG('Su20_WordCloud_081521.png')
WF20 <- readPNG('Su20_WordFreq_081521.png')

WCF20Plots <- plot_grid(rasterGrob(WC20), rasterGrob(WF20), label_size = 12, labels = 'AUTO') #1200x500

#Make a title for the plot_grid
WCF20Title <- ggdraw() + draw_label("Top Positive SCIP 2020 Comments", fontface = 'bold')

plot_grid(WCF20Title, WCF20Plots, ncol = 1, rel_heights = c(0.1,1)) #1200x500

##2021##

#Read in positive comments as a corpus to read sentences
PosComm21 <- Corpus(VectorSource(subset(PostSurvey21, select = c("ID", "positive_comments"))))
inspect(PosComm21)

#Remove numbers
PosComm21 <- tm_map(PosComm21, removeNumbers)
#Remove filler words
PosComm21 <- tm_map(PosComm21, removePunctuation)
PosComm21 <- tm_map(PosComm21, removeWords, c("the", "and", "was", "that", "like", "were", "with", "for", "own", "very", "had", "how",
                                              "able", "liked", "really", "it", "are", "when"))
PosComm21 <- tm_map(PosComm21, removeWords, c("our", "any", "could", "all", "not"))

#Create dataframe for plot and wordcloud
pcm21 <- TermDocumentMatrix(PosComm21)
m21 <- as.matrix(pcm21)
v21 <- sort(rowSums(m21), decreasing = TRUE)
PosComm21Final <- data.frame(word = names(v21), freq = v21)
head(PosComm21Final, 15)

#Create margin for bar plot
par(mar=c(5,7,3,4))

#Bar plot
barplot(PosComm21Final[1:15, ]$freq, las = 1, names.arg = PosComm21Final[1:15, ]$word, 
        ylim = c(0, 20), col = "#5386E4",
        main = "Word Frequency in Positive Comments", xlab = "Frequency", horiz = TRUE, xaxs = 'i') #800x600

#Wordcloud
dev.new(width = 1000, height = 1000, unit = "px") #opens a new device with more space to plot
set.seed(4000)
par(mar=c(1,1,1,1))
wordcloud(words = PosComm21Final$word, freq = PosComm21Final$freq, scale = c(5, 0.5),
          min.freq = 2, max.words = Inf, random.order = FALSE, rot.per = 0.5,
          colors = NinePalette) #saved in external dev

#Make grid of the two plots
WC21 <- readPNG('Su21_WordCloud_081521.png')
WF21 <- readPNG('Su21_WordFreq_081521.png')
WCF21Plots <- plot_grid(rasterGrob(WC21), rasterGrob(WF21), label_size = 12, labels = c('C', 'D')) #1200x500

#Make a title for the plot_grid
WCF21Title <- ggdraw() + draw_label("Top Positive SCIP 2021 Comments", fontface = 'bold')

plot_grid(WCF21Title, WCF21Plots, ncol = 1, rel_heights = c(0.1, 1)) #1200x500

#combine both 2020 and 2021 wordcloud and frequency plots together
WCF20Final <- readPNG('Su20_WordCloudFreq_081521.png')
WCF21Final <- readPNG('Su21_WordCloudFreq_081521.png')

plot_grid(rasterGrob(WCF20Final), rasterGrob(WCF21Final), ncol = 1, rel_widths = c(1, 0, 1))

###################### AIM 3: Improved Coding Confidence ######################
### Figure 6. Previous Coding Experience ###

#Subset to remove Image Processing Participants
#Image Processing edX Course
PairedSurvey21 <- PairedSurvey21 %>%
  filter(course != c("Image Processing edX Course"))

##2020##

#Reordering levels of Coding Experience
PairedSurvey20$pre_exp <- as.factor(PairedSurvey20$pre_exp) #change column data from characters to factors

levels(PairedSurvey20$pre_exp) <- list('0 experience' =c('No experience at all'),
                                       '<1 semester' =c('Learned some coding on my own'),
                                       '1-2 semesters' =c('Around 1 semester of experience'),
                                       '2-3 semesters' =c('2 semesters (or 1 year) of experience'),
                                       '3+ semesters' =c('3 semesters of experience'),
                                       '3+ semesters' =c('4 or more semesters (or 2+ years) of experience'))

#Pie chart for prior Coding Experience
PrevExp20 <- table(PairedSurvey20$pre_exp)

par(mar=c(4,4,4,4))
pie(PrevExp20, main = "Summer 2020 Cohort \n n=75", labels = names(PrevExp20), border = "white", 
    col = FivePalette, radius = 1.6, init.angle = 200, clockwise = TRUE, cex.main = 2, cex = 1.3)
#1200x700

## 2021 ##
PairedSurvey21$pre_exp <- as.factor(PairedSurvey21$pre_exp) #change column data from characters to factors

levels(PairedSurvey21$pre_exp) <- list('0 experience' =c('No experience at all'),
                                       '<1 semester' =c('I know a few commands here and there (Some experience, <1 semester)'),
                                       '1-2 semesters' =c('I have a handle of the basics (Average experience, 1-2 semesters)'),
                                       '2-3 semesters' =c("I have some experience programming (More than average experience, 2-3 semesters)"))

#Pie chart for prior Coding Experience
PrevExp21 <- table(PairedSurvey21$pre_exp)

par(mar=c(4,4,4,4))
pie(PrevExp21, main = "Summer 2021 Cohort\n n=63", labels = names(PrevExp21), border = "white",
    col = FivePalette, radius = 2, init.angle = 200, clockwise = TRUE, cex.main = 2, cex = 1.3)

# plot both Su20 and Su21 together
CE20 <- readPNG('Su20_CodingExp_121021_Crop.png')
CE21 <- readPNG('Su21_CodingExp_121021_Crop.png')

CE20_21 <- plot_grid(rasterGrob(CE20),rasterGrob(CE21), rel_heights = c(0.2, 4),
                     label_size = 20, labels = 'AUTO')

#add title
CETitle <- ggdraw() + draw_label("Previous Coding Experience", fontface = 'bold', size = 20)

plot_grid(CETitle, CE20_21, ncol = 1, rel_heights = c(0.1, 1), rel_widths = c(0.1, 1)) #1200x600










