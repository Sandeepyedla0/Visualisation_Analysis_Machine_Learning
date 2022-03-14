set.seed(42)
setwd("C:/Users/HP/Desktop/GMU/STAT 515/project/Final_10_003");

#import Library
library(tidyverse)
library(plyr)
library(dplyr)
library(janitor)
library(corrplot)
library(RColorBrewer)
library(lubridate);
library(colorspace);
library(plotly);

source("hw.R");

# Remove warnings
options(warn=-1)

# Load libraries
require(ggplot2)
require(highcharter) 
require(dplyr)
require(tidyverse)
require(corrplot)
require(RColorBrewer)
require(xts)
require(treemap)
require(lubridate)

#-----------------------read data


ds_temp<-read.csv("Playstore_data_cleaned.csv",header=TRUE);


ds<-ds_temp[!(ds_temp$App==1.9),]; #removing redudant row

#view(ds);
#---------------





#---------------pie_chart------


Count_Category<-count(ds,"Category");
Count_Category
#Count_Categoryg195<-subset(Count_Category,freq>195);






fig <- plot_ly(Count_Category, labels = ~Category, values = ~freq, type = 'pie')
fig
fig <- fig %>% layout(title = 'Percentage of Total App in Category',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))%>%
  layout(xaxis = list(tickfont = list(size = 15)), yaxis = list(tickfont = list(size = 5)));

fig




#---------------------------Circular_plot----

#-------------------------------------------
sub_stkplt<-subset(ds,select=c(Category, Type));


myFreqs<- sub_stkplt %>%  
  group_by(Category, Type) %>% 
  summarise(freq = n());


myFreqs2<-myFreqs[myFreqs$Category!="FAMILY" & myFreqs$Category!="GAME" &  myFreqs$Category!="TOOLS"& 
                    myFreqs$Category!="ENTERTAINMENT"& myFreqs$Category!="FOOD_AND_DRINK"& myFreqs$Category!="BOOKS_AND_REFERENCE"&
                    myFreqs$Category!="VIDEO_PLAYERS"& myFreqs$Category!="PARENTING"&myFreqs$Category!="NEWS_AND_MAGAZINES",]


myFreqs2 <- tibble::rowid_to_column(myFreqs2, "ID");



label_data <- myFreqs2;
view(label_data);

# calculate the ANGLE of the labels

number_of_bar <- nrow(label_data)
angle <-  90 - 360 * (label_data$ID-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #


# Start the plot
ggplot(myFreqs2, aes(x=as.factor(ID), y=freq,fill=Type)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity",position = "Dodge") +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-150,450) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-0.5,6), "cm"))+      # Adjust the margin to make in sort labels are not truncated!) 
  scale_fill_manual(values=c("Free"="#2c7fb8","Paid"="#f03b20"))+
  labs(X="Category",title ="Paid Vs Free comparision")+
  theme(plot.title = element_text(hjust = 0.52))+
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0) +
  
  
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=ID, y=freq+10, label=Category, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )+hw



#---------------------------

#------------density plot


Count_size_cleaned<-count(ds,"size_cleaned");
Count_size_Rating<-count(ds,"Rating");

Count_size_Rating;


ggplot(ds, aes(x=Rating)) + 
  geom_density(alpha=0.5)+
  geom_vline(aes(xintercept=mean(Rating)),
             color="blue", linetype="dashed", size=1)+
  labs(title="Overall Density Data Distribution ",x="Rating(1-5)",y="Density")+
  theme(plot.title = element_text(hjust = 0.52))+hw




#----------------


#---------------box_plot
ggplot(ds, aes(x=Category, y=Rating)) + 
  geom_boxplot(color="red",fill="Orange",col=rainbow(33),alpha=0.5)+
  labs(title="Distribution of Ratings across various Categories of Applications",x="Category",y="Rating")+
  theme(plot.title=element_text(hjust = 0.5),  axis.text.x = element_text(angle=90,vjust = 0.5,hjust=1))

#+hw
#------------------
#--------------------------version update plot
tmp <- ds %>% count("Last_Updated");

# Transform date column type from text to date



tmp$Last.Updated<-mdy(tmp$Last_Updated);
# Transform data into time series
tmp

as.Date(tmp$Last.Updated);
tmp<-na.omit(tmp);

time_series <- xts(tmp$freq, order.by = as.Date(tmp$Last.Updated));
time_series
highchart(type = "stock") %>% 
  hc_title(text = "Updated Frequency Of Applications Over Period") %>% 
  hc_subtitle(text = "Application Update by Month/Date/Year") %>% 
  hc_add_series(time_series) %>%
  #hc_theme_sandsignika()
  hc_add_theme(hc_theme_economist())



#--------------
#----------------------------------corr_plot



ds_corr_plt<-subset(ds,select=c(Reviews,Installs,Size, Price, Rating));

#impute.mean <- function(x) replace(ds_corr_plt, is.na(x) | is.nan(x) | is.infinite(x), mean(x, na.rm = TRUE))

ds_corr_plt<-na.omit(ds_corr_plt);
ds_corr_plt

ds_corr_gph <-cor(ds_corr_plt)
corrplot(ds_corr_gph,type="lower", order="hclust",title='\n \n                              
                                                  Correlation between Price, Rating, Size, Reviews and Installs',
         col=(brewer.pal(n=8, name="Greens")))+hw


#----------------------------------