#Load libraries 
library(ggplot2) 
library(reshape2)
library(ggpubr)
library(gridExtra)

#import Data 
df <- read.table("Gergana_Hisat_HT_seq_pipeline_gene_expression_readcount_statistics.txt", header=TRUE,  sep="\t", row.names= 1)


#Group wise Alignment wise
df1 <- df[1,]
title <- "Alignment Not Unique"
df1_t <- as.data.frame(t(df1))
df1_t

#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
dT<-as.data.frame(df1_t[grep("dT", rownames(df1_t)), ])
rRNA<-as.data.frame(df1_t[grep("rRNA", rownames(df1_t)), ])
Total<-as.data.frame(df1_t[grep("Total", rownames(df1_t)), ])

#Bind groups together column-wise 
group <- cbind(dT, rRNA, Total) 

#Provide the column names to both groups of samples 
colnames(group) <- c("dT", "rRNA", "Total")

group
#Create a long formatted dataframe 
df_group <- melt(group) 

#print length of group object 
head(df_group)

#Draw boxplot  
pp <- ggplot(df_group, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot1 <- pp + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=10, angle=45)) + 
  xlab("Groups") +
  ylab("") + labs(fill = "Group") +
  labs(title= paste("", title)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

plot1



df2 <- df[2,]
title <- "Ambigous"
df2_t <- as.data.frame(t(df2))
df2_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
dT<-as.data.frame(df2_t[grep("dT", rownames(df2_t)), ])
rRNA<-as.data.frame(df2_t[grep("rRNA", rownames(df2_t)), ])
Total<-as.data.frame(df2_t[grep("Total", rownames(df2_t)), ])

#Bind groups together column-wise 
group_2 <- cbind(dT, rRNA, Total) 

#Provide the column names to both groups of samples 
colnames(group_2) <- c("dT", "rRNA", "Total")

group
#Create a long formatted dataframe 
df_group2 <- melt(group_2) 

#print length of group object 
head(df_group2)

#Draw boxplot  
pp2 <- ggplot(df_group2, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot2 <- pp2 + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=10, angle=45)) + 
  xlab("Groups") + ylab("") + 
  labs(fill = "Group") + 
  labs(title= paste(" ", title)) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

plot2



df3 <- df[3,]
title <- "No Feature"
df3_t <- as.data.frame(t(df3))
df3_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
dT<-as.data.frame(df3_t[grep("dT", rownames(df3_t)), ])
rRNA<-as.data.frame(df3_t[grep("rRNA", rownames(df3_t)), ])
Total<-as.data.frame(df3_t[grep("Total", rownames(df3_t)), ])

#Bind groups together column-wise 
group_3 <- cbind(dT, rRNA, Total) 

#Provide the column names to both groups of samples 
colnames(group_3) <- c("dT", "rRNA", "Total")

group
#Create a long formatted dataframe 
df_group3 <- melt(group_3) 

#print length of group object 
head(df_group3)

#Draw boxplot  
pp3 <- ggplot(df_group3, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) 

#Add axis title, legend title and adjust font size 
plot3 <- pp3 + theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=10, angle=45)) + 
  xlab("Groups") + 
  ylab("") + 
  labs(fill = "Group") + 
  labs(title= paste(" ", title)) +  
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)

plot3


df4 <- df[2,]
title <- "Not Aligned"
df4_t <- as.data.frame(t(df4))
df4_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
dT<-as.data.frame(df4_t[grep("dT", rownames(df4_t)), ])
rRNA<-as.data.frame(df4_t[grep("rRNA", rownames(df4_t)), ])
Total<-as.data.frame(df4_t[grep("Total", rownames(df4_t)), ])

#Bind groups together column-wise 
group_4 <- cbind(dT, rRNA, Total) 

#Provide the column names to both groups of samples 
colnames(group_4) <- c("dT", "rRNA", "Total")

group
#Create a long formatted dataframe 
df_group4 <- melt(group_4) 

#print length of group object 
head(df_group4)

#Draw boxplot  
pp4 <- ggplot(df_group4, aes(factor(variable), value)) + 
  geom_boxplot(aes(fill = variable)) +
  theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=10, angle=45)) + 
  xlab("Groups") + 
  ylab("") + 
  labs(fill = "Group") + 
  labs(title= paste(" ", title)) +  
  theme(plot.title = element_text(hjust = 0.5))

pp4


df5 <- df[5,]
title <- "Too Low Quality"
df5_t <- as.data.frame(t(df5))
df5_t
#Extract samples belong to each group with column name 
#dT<- as.data.frame(df1_t[grep('^dT', names(df1_t))]) 
#rRNA <- as.data.frame(df1[grep('^rRNA', names(df1))])
#Total<- as.data.frame(df1[grep('^Total', names(df1))]) 

#Extract samples belong to each group with row name 
dT<-as.data.frame(df5_t[grep("dT", rownames(df5_t)), ])
rRNA<-as.data.frame(df5_t[grep("rRNA", rownames(df5_t)), ])
Total<-as.data.frame(df5_t[grep("Total", rownames(df5_t)), ])

#Bind groups together column-wise 
group_5 <- cbind(dT, rRNA, Total) 

#Provide the column names to both groups of samples 
colnames(group_5) <- c("dT", "rRNA", "Total")


#Create a long formatted dataframe 
df_group5 <- melt(group_5) 

#print length of group object 
head(df_group5)

#Draw boxplot  
pp5 <- ggplot(df_group5, aes(factor(variable), value)) + geom_boxplot(aes(fill = variable)) +
  theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=10, angle=45)) + 
  xlab("Groups") + 
  ylab("") + 
  labs(fill = "Group") + 
  labs(title= paste(" ", title)) +  
  theme(plot.title = element_text(hjust = 0.5))

pp5


#Barplot for alignment Rate


input <- read.table("Barplot_input.txt", sep="\t", header = TRUE)


bb <- ggplot(input, aes(x=Sample, y=Alignment_Rate)) + geom_bar(stat = "identity", width=0.5) + 
  theme(legend.key.size = unit(0.7, "cm"), legend.text = element_text(color= "Black", size= 7), axis.text.x = element_text(color= "Black", size= 6, angle = 90), axis.text.y = element_text(size=10, angle=45)) + 
  xlab("Samples") + 
  ylab("Alignment Rate (%)") + 
  labs(fill = "Group") + 
  labs(title= paste("Alignment Rate for Samples")) +  
  theme(plot.title = element_text(hjust = 0.5))
bb

#barplot(as.matrix(t(input)), las=2, cex.axis = 0.5, cex.lab=0.2)

grid <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow=3, ncol=2)

