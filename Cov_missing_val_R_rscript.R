
library(gplots)
library(dplyr) #required for modification of data
library(pryr) #required to save basic rplots as object
library(ggplot2)
library(gt)
library(ggpubr)
library(gridExtra)



filenames <- list.files(pattern="*.txt")

for(i in 1:length(filenames))
  
{
  
  #extract seq type
  sample_name<- filenames[i]
  sample_name1 <- gsub("Lewin_HSV_PnBase_GU734771_", "", sample_name)
  sample_name2 <- gsub("__pair_combined.pnBase.txt", "", sample_name1)
  print(sample_name2)
#}

#report_name <- toString(sub(".txt", ".pdf", element))
#mydata1 <- read.table("Lewin_HSV_PnBase_GU734771_1-HSV1-F-dICP34-1-Ms-GmCSF-c2-1_S1_L001_1__pair_combined.pnBase.txt", sep="\t", header=TRUE)

mydata1 <- read.table(sample_name, sep="\t", header=TRUE)


#remove insertion type column
mydata1 <- mydata1[,-10]
#Compute coverage
Coverage <- rowSums(mydata1[,4:9])

#Add coverage column to the data
mydata1 <- cbind(mydata1,Coverage)

head(mydata1)
dim(mydata1)



#insert missing values (position) and corresponding rows with NA
library(data.table)
my_df1 <- as.data.frame(setDT(mydata1)[.(seq(max(Pos))), on = .(Pos)])

dim(my_df1)

#replace NA with zero value
my_df1[is.na(my_df1)] <- 0

#replaces 0 value in first columnn  with name(ID)

my_df1$Sequence[my_df1$Sequence == 0] <- "GU734771.1"


library(Hmisc) 
library(ggplot2)

#Create bins based pos
my_df1$bin <- as.numeric(cut2(my_df1$Pos, g=15)) ### g to define no. of bins

my_df1

### Write new dataframe in a file
write.table(my_df1,paste0(sample_name2 , ".txt"), sep="\t", quote=FALSE, row.name=FALSE)

seq_type <- unique(my_df1$bin)

for(i in 1:length(seq_type))

{

#extract seq type
element<- seq_type[i]

 
#D25<-as.data.frame(mydata1 [grep(seq_type[i], mydata1$Sequence), ])
new_df<-as.data.frame(my_df1 [grep(element, my_df1$bin), ])

print(element) 

#report_name <- toString(sub(".txt", ".pdf", element1))

#make a plotting theme
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


#plot coverage
p1 <-ggplot(data=new_df, aes(x=Pos, y=Coverage, aspect_ratio = 2.5)) +
  geom_area(color="lightgray",fill="lightgray",alpha=0.5) +
  geom_point(aes(color = Coverage)) +
  scale_color_gradientn(colours=c('red','orange',"cyan","blue",'blueviolet')) +
  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 50)) +
  #scale_x_continuous(breaks = scales::pretty_breaks(n = 50))
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  #ggtitle(paste("Coverage for ")) +
  ggtitle(paste("Coverage for ",sample_name2, "_","Part", element)) +
  mytheme

p1


grid <- grid.arrange(p1,ncol= 1, nrow=1)
#ggsave(grid, file="report_name.pdf", height = 8 , device = pdf, width = 10)
ggsave(grid, file=paste(sample_name2, "_", "part",element,".pdf"), height = 8 ,  width = 10)

dev.off()

}
}
