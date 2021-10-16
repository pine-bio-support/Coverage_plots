library(gplots)
library(dplyr) #required for modification of data
library(pryr) #required to save basic rplots as object
library(ggplot2)
library(gt)
library(ggpubr)
library(gridExtra)


mydata1 <- read.table("Gergana_PerNuc_Mutation_Total-1_S1_L003_1__pair_combined.pnBase.txt", sep="\t", header=TRUE)


#remove insertion type column
mydata1 <- mydata1[,-10]
#Compute coverage
Coverage <- rowSums(mydata1[,4:9])

#Add coverage column to the data
mydata1 <- cbind(mydata1,Coverage)

name <- mydata1[1]
cov <- mydata1[11]

df1 <- cbind(name,cov)
head(df1)

dim(df1)


agrregate=aggregate(. ~ Sequence, data = df1, mean)
avg_df<-data.frame(agrregate)
sample1 <- avg_df[2]
rownames(sample1) <- avg_df[,1]

ss2 <- as.matrix(t(sample1))


mytheme <- theme(
  text=element_text(family="Helvetica", size=10),
  axis.text.x = element_text(angle = 90,size = 5),
  axis.text.y = element_text(size = 5),
  panel.background = element_rect(fill = NA),
  panel.border = element_rect(fill = NA, color = "grey75"),
  axis.ticks = element_line(color = "grey85"),
  panel.grid.major = element_line(color = "grey95", size = 0.2),
  panel.grid.minor = element_line(color = "grey95", size = 0.2),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6) )

p1 <-ggplot(data=avg_df, aes(x=Sequence, y=Coverage, aspect_ratio = 2.5)) +
  geom_area(color="lightgray",fill="lightgray",alpha=0.5) +
  geom_point(aes(color = Coverage)) +
  scale_color_gradientn(colours=c('red','orange',"cyan","blue",'blueviolet')) +
  #scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 50)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 50))
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #ggtitle(paste("Coverage for ")) +
  #ggtitle(paste("Coverage for ",sample_name, "_", element)) +
  mytheme


barplot(ss2, las=2, 
              cex.axis = 0.5, 
              cex.lab = 0.7, 
              cex.names=0.35,
              xlab = "Contigs",
              ylab = "Mean of Coverage", 
              main="Mean Coverage Plot for Total-1_S1_L003")















for(i in 1:length(seq_type))
  
{
  
  #extract seq type
  element<- seq_type[i]
  
  element1 <- gsub("\\.[0-9]*$", "", element)
  
  #D25<-as.data.frame(mydata1 [grep(seq_type[i], mydata1$Sequence), ])
  new_df<-as.data.frame(mydata1 [grep(element, mydata1$Sequence), ])
  
  print(element) 
}