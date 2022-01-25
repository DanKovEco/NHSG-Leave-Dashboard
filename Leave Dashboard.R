#Leave Dashboard

#libraries

library(tidyverse)
library(stringr)
library(openxlsx)
library(ggplot2)

#working directory
#setwd(~)


#0 - some previous stuff?
#1 - April
#2 - May
#3 - June
#4 - July
#5 - August
#6 - September
#7 - October
#8 - November
#9 - December
#10 - January
#11 - February
#12 - March




#Filepaths
#DataFile0 <- "~/NHS Grampian Leave Dashboard v3.1.xlsx" #existing data
#DataFile7 <- "~/Leave Dashboard Data/October Leave Data.xlsx" #October data
#DataFile8 <- "~/Leave Dashboard Data/November Leave Data.xlsx" #November data
#DataFile9 <- "~/Leave Dashboard Data/December Leave Data.xlsx" #December data
DataFiles <- c("~/Leave Dashboard Data/April to September Leave Data.xlsx",
               "~/Leave Dashboard Data/October Leave Data.xlsx",
               "~/Leave Dashboard Data/November Leave Data.xlsx",
               "~/Leave Dashboard Data/December Leave Data.xlsx"
                )

AnnLeaEntFile <- "~/Leave Dashboard Data/December Leave Data.xlsx" #needs only the most recent
                


#import leave data
FullData <- read.xlsx(DataFiles[1], sheet = 1, colNames = TRUE, check.names = TRUE, rowNames = FALSE)
FullData <- FullData[FALSE,]

for (i in 1:length(DataFiles)){
                               TDF <- read.xlsx(DataFiles[i], sheet = 1, colNames = TRUE, check.names = TRUE)
                               FullData <- rbind(FullData, TDF)
                               }
#Correcting formatting, column names and values, and summarising to remove errors from poor data
FullData$Period <- as.Date.numeric(FullData$Period, origin = "1899-12-30")
FullData['Division'][FullData['Division'] == "None"] <- "None (Div/CHP)"
FullData['Division'][FullData['Division'] == "Facilities Grampian"] <- "Facilities Grampian (Div/CHP)"
FullData[is.na(FullData)] <- 0
class(FullData$Job.Family)
FullData['Job.Family'][FullData['Job.Family'] == "NURSING/MIDWIFERY"] <- "NURSING & MIDWIFERY"
FullData['Job.Family'][FullData['Job.Family'] == "NURSING AND MIDWIFERY"] <- "NURSING & MIDWIFERY"



ColNamesVector <- c("Division", 
                    "Job.Family", 
                    "Headcount", 
                    "Total.WTE", 
                    "Total.Contracted.Hours", 
                    "Period.Total.Projected.Contracted.Hours", 
                    "Period", 
                    "Sick.Leave.Hours.Lost", 
                    "Sick.Leave.Hours.Lost.Percentage", 
                    "Coronavirus.Hours.Lost", 
                    "Coronavirus.Hours.Lost.Percentage", 
                    "Maternity.Leave.Hours.Lost", 
                    "Maternity.Hours.Lost.Percentage", 
                    "Other.Leave.Hours.Lost", 
                    "Other.Leave.Hours.Lost.Percentage", 
                    "Annual.Leave.Hours.Lost", 
                    "Annual.Leave.Hours.Lost.Percentage"
                    )
names(FullData) <-  ColNamesVector

#Deselecting the percentage columns, because they are likely to give incorrect sums through summarise()


FullData[is.na(FullData)] <- 0
FullData <- FullData %>% group_by(Division, Job.Family, Period) %>% summarise(sum(Headcount),
                                                                              sum(Total.WTE), 
                                                                              sum(Total.Contracted.Hours), 
                                                                              sum(Period.Total.Projected.Contracted.Hours), 
                                                                              sum(Sick.Leave.Hours.Lost), 
                                                                              sum(Coronavirus.Hours.Lost), 
                                                                              sum(Maternity.Leave.Hours.Lost), 
                                                                              sum(Other.Leave.Hours.Lost), 
                                                                              sum(Annual.Leave.Hours.Lost)
                                                                              )
#removing DDITs
FullData <- subset(FullData, !grepl("DDIT", Division))

#removing 'sum' from column name
ColNamesVector <- c("Division", 
                    "Job.Family", 
                    "Period", 
                    "Headcount", 
                    "Total.WTE", 
                    "Total.Contracted.Hours", 
                    "Period.Total.Projected.Contracted.Hours", 
                    "Sick.Leave.Hours.Lost", 
                    "Coronavirus.Hours.Lost", 
                    "Maternity.Leave.Hours.Lost", 
                    "Other.Leave.Hours.Lost", 
                    "Annual.Leave.Hours.Lost" 
)
names(FullData) <-  ColNamesVector

#add Grampian as Board
FullData$Board <- "NHS Grampian" 

###getting the annual leave entitlement and used data
#import annual leave entitlement data
AnnLeaEnt <- read.xlsx(AnnLeaEntFile, sheet = 2, colNames = TRUE, check.names = TRUE, rowNames = FALSE)


#Correcting formatting, column names and values to remove errors from poor data
AnnLeaEnt['Job.Family'][AnnLeaEnt['Job.Family'] == "NURSING/MIDWIFERY"] <- "NURSING & MIDWIFERY"




#

#function attempt to make making graphs easier - maybe
###

MakeBarGraph <- function(FDF, x, y, ylims, ylab, xlab, title, dateX = TRUE) {
  
  BarGraph <- ggplot(data = FDF, aes(x = x, y = y)) +
    geom_bar(stat = "identity") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = scales::percent, limits = ylims) 
  
  if (dateX == TRUE) {
    BarGraph <- BarGraph + scale_x_date(date_breaks = "1 month", date_labels = "%B")
  }
  return(BarGraph)
  
}

###




##Percentage of working hours lost - sick leave 2021-22
# x = periods; y = % hour lost; across Grampian

TDF <- FullData %>% group_by(Board, Period) %>% summarise(sum(Period.Total.Projected.Contracted.Hours), sum(Sick.Leave.Hours.Lost))
TDF$Percentage.Hour.Lost <- TDF$`sum(Sick.Leave.Hours.Lost)` / TDF$`sum(Period.Total.Projected.Contracted.Hours)`
TDF$Percentage.Hour.Lost <- TDF$Percentage.Hour.Lost

SickLeaveChartGrampian <- MakeBarGraph(TDF, TDF$Period, TDF$Percentage.Hour.Lost, c(0, max(TDF$Percentage.Hour.Lost)*4/3), "Percentage Hours Lost", "Period", "Percentage Hours Lost - Sick Leave", dateX = TRUE)
SickLeaveChartGrampian


##Percentage of working hours lost - covid leave 2021-22
# x = periods; y = % hour lost; across Grampian

TDF <- FullData %>% group_by(Board, Period) %>% summarise(sum(Period.Total.Projected.Contracted.Hours), sum(Coronavirus.Hours.Lost))
TDF$Percentage.Hour.Lost <- TDF$`sum(Coronavirus.Hours.Lost)` / TDF$`sum(Period.Total.Projected.Contracted.Hours)`
TDF$Percentage.Hour.Lost <- TDF$Percentage.Hour.Lost

CovidLeaveChartGrampian <- MakeBarGraph(TDF, TDF$Period, TDF$Percentage.Hour.Lost, c(0, max(TDF$Percentage.Hour.Lost)*4/3), "Percentage Hours Lost", "Period", "Percentage Hours Lost - Covid Leave", dateX = TRUE)
CovidLeaveChartGrampian


##Percentage of working hours lost - Maternity leave 2021-22
# x = periods; y = % hour lost; across Grampian

TDF <- FullData %>% group_by(Board, Period) %>% summarise(sum(Period.Total.Projected.Contracted.Hours), sum(Maternity.Leave.Hours.Lost))
TDF$Percentage.Hour.Lost <- TDF$`sum(Maternity.Leave.Hours.Lost)` / TDF$`sum(Period.Total.Projected.Contracted.Hours)`
TDF$Percentage.Hour.Lost <- TDF$Percentage.Hour.Lost

MaternityLeaveChartGrampian <- MakeBarGraph(TDF, TDF$Period, TDF$Percentage.Hour.Lost, c(0, max(TDF$Percentage.Hour.Lost)*4/3), "Percentage Hours Lost", "Period", "Percentage Hours Lost - Maternity Leave", dateX = TRUE)
MaternityLeaveChartGrampian



##Percentage of working hours lost - Other leave 2021-22
# x = periods; y = % hour lost; across Grampian

TDF <- FullData %>% group_by(Board, Period) %>% summarise(sum(Period.Total.Projected.Contracted.Hours), sum(Other.Leave.Hours.Lost))
TDF$Percentage.Hour.Lost <- TDF$`sum(Other.Leave.Hours.Lost)` / TDF$`sum(Period.Total.Projected.Contracted.Hours)`
TDF$Percentage.Hour.Lost <- TDF$Percentage.Hour.Lost

OtherLeaveChartGrampian <- MakeBarGraph(TDF, TDF$Period, TDF$Percentage.Hour.Lost, c(0, max(TDF$Percentage.Hour.Lost)*4/3), "Percentage Hours Lost", "Period", "Percentage Hours Lost - Other Leave", dateX = TRUE)
OtherLeaveChartGrampian



##Percentage of working hours lost - Annual leave 2021-22
# x = periods; y = % hour lost; across Grampian

TDF <- FullData %>% group_by(Board, Period) %>% summarise(sum(Period.Total.Projected.Contracted.Hours), sum(Annual.Leave.Hours.Lost))
TDF$Percentage.Hour.Lost <- TDF$`sum(Annual.Leave.Hours.Lost)` / TDF$`sum(Period.Total.Projected.Contracted.Hours)`
TDF$Percentage.Hour.Lost <- TDF$Percentage.Hour.Lost

AnnualLeaveChartGrampian <- MakeBarGraph(TDF, TDF$Period, TDF$Percentage.Hour.Lost, c(0, max(TDF$Percentage.Hour.Lost)*4/3), "Percentage Hours Lost", "Period", "Percentage Hours Lost - Annual Leave", dateX = TRUE)
AnnualLeaveChartGrampian




##Percentage of working hours lost - all leave 2021-22 - stacked area graph 
# x = periods; y = % hour lost; across Grampian

TDF <- pivot_longer(FullData,c(8:12), values_to = 'Hours.Lost', names_to = 'Absence.Type')
FullData2 <- TDF

TDF <- TDF %>% group_by(Period, Absence.Type) %>% summarise(sum(Period.Total.Projected.Contracted.Hours), sum(Hours.Lost)) %>%
       mutate('Percentage.Hours.Lost'=`sum(Hours.Lost)`/`sum(Period.Total.Projected.Contracted.Hours)`)

TDF$Absence.Type <- as.factor(TDF$Absence.Type)

#TDF$Period <- as.factor(TDF$Period)

TDF <- TDF %>% group_by(Absence.Type)
names(TDF) <-  c("Period", "Absence.Type", "Period.Total.Projected.Contracted.Hours", "Hours.Lost", "Percentage.Hours.Lost")
TDF2 <- TDF %>% group_by(Period) %>% summarise(sum(Percentage.Hours.Lost))
names(TDF2) <-  c("Period", "Percentage.Hours.Lost")


AllLeaveChartGrampian <- ggplot(data = TDF, aes(x = Period, y = Percentage.Hours.Lost, fill = Absence.Type)) +
  geom_area(stat = "identity") +
  ylab("Percentage Hours Lost") + 
  ggtitle("Percentage of Working Hours Lost - NHS Grampian (by type of leave)") +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF2$Percentage.Hours.Lost)*4/3)) 

AllLeaveChartGrampian

#print to file
pdf("~/Grampian Percentage of Hours Lost.pdf", paper = "a4")
print(SickLeaveChartGrampian)
print(CovidLeaveChartGrampian)
print(MaternityLeaveChartGrampian)
print(OtherLeaveChartGrampian)
print(AnnualLeaveChartGrampian)
print(AllLeaveChartGrampian)
dev.off()


###annual leave used
#by division
TDF <- AnnLeaEnt %>% group_by(Division) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))
TDF$Percentage.AL.Used <- TDF$`sum(Total.Notional.Entitlement.Used)` / TDF$`sum(Total.Notional.Entitlement)`



AnnLeaveUsedDivisionChart <- ggplot(data = TDF, aes(x = Division, y = Percentage.AL.Used)) +
  geom_bar(stat = "identity") +
  ylab("Percentage Leave Used") +
  xlab("Division") +
  ggtitle("Percentage of Annual Leave Entitlement by Division") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_flip()

AnnLeaveUsedDivisionChart


#by directorate
TDF <- AnnLeaEnt %>% group_by(Directorate) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))
TDF$Percentage.AL.Used <- TDF$`sum(Total.Notional.Entitlement.Used)` / TDF$`sum(Total.Notional.Entitlement)`



AnnLeaveUsedDirectorateChart <- ggplot(data = TDF, aes(x = Directorate, y = Percentage.AL.Used)) +
  geom_bar(stat = "identity") +
  ylab("Percentage Leave Used") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_flip()

AnnLeaveUsedDirectorateChart


#by job family
TDF <- AnnLeaEnt %>% group_by(Job.Family) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))
TDF$Percentage.AL.Used <- TDF$`sum(Total.Notional.Entitlement.Used)` / TDF$`sum(Total.Notional.Entitlement)`



AnnLeaveUsedJobFamilyChart <- ggplot(data = TDF, aes(x = Job.Family, y = Percentage.AL.Used)) +
  geom_bar(stat = "identity") +
  ylab("Percentage Leave Used") +
  xlab("Job Family") +
  ggtitle("Percentage of Annual Leave Entitlement by Job Family") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  coord_flip()

AnnLeaveUsedJobFamilyChart

#print to file
pdf("~/Annual Leave Used.pdf")
print(AnnLeaveUsedDivisionChart)
print(AnnLeaveUsedJobFamilyChart)
dev.off()



###Division classic
#fill in with some extra rows with zeros so the graphs look correct
FullData2$Helper <- paste(FullData2$Division, FullData2$Job.Family, sep = "???")
TDF2 <- expand.grid(unique(pull(FullData2[,"Helper"])), unique(pull(FullData2[,"Period"])), unique(pull(FullData2[,"Absence.Type"])))
names(TDF2) <- c("Helper", "Period", "Absence.Type")
TDF2$Helper <- as.character(TDF2$Helper)

TDF2 <- separate(data = TDF2, col = Helper, into = c("Division", "Job.Family"), sep = "???", remove = TRUE, convert = FALSE)

FullData2 <- rbind(FullData2, TDF2)
FullData2 <- FullData2[, !names(FullData2) %in% "Helper"]
FullData2$Board <- "NHS Grampian"
FullData2[is.na(FullData2)] <- 0


FullData2 <- FullData2 %>% group_by(Division, Job.Family, Period, Absence.Type, Board) %>% summarise(sum(Headcount),
                                                                    sum(Total.WTE), 
                                                                    sum(Total.Contracted.Hours), 
                                                                    sum(Period.Total.Projected.Contracted.Hours), 
                                                                    sum(Hours.Lost), 
                                                                    )
ColNamesVector <- c("Division",
                    "Job.Family",
                    "Period",
                    "Absence.Type",
                    "Board",
                    "Headcount",
                    "Total.WTE",
                    "Total.Contracted.Hours",
                    "Period.Total.Projected.Contracted.Hours",
                    "Hours.Lost"
                    )

names(FullData2) <- ColNamesVector




uAT<-unique(FullData2$Absence.Type)
pdf(paste("~/Division Charts.pdf"))


for(i in 1:length(uAT)){
  
  TDF <- FullData2 %>% filter(Absence.Type==uAT[i]) %>% group_by(Division, Period) %>% summarise(Percentage.Hours.Lost = sum(Hours.Lost) / sum(Period.Total.Projected.Contracted.Hours))
  TDF$Period <- as.factor(TDF$Period)
  DivLeaveChart <- ggplot(data = TDF, aes(Division, Percentage.Hours.Lost, fill = Period)) +
    geom_bar(stat = "identity", position = position_dodge(preserve = "single")) +
    ylab("Percentage of hours lost") +
    xlab("Divisions") +
    ggtitle(paste("Percentage of Working Hours Lost - ", uAT[i])) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
    coord_flip()
  
  print(DivLeaveChart)
  }
dev.off()

###Division flipped

uDiv<-unique(FullData2$Division)


for(i in 1:length(uDiv)){

  TDF <- FullData2 %>% filter(Division==uDiv[i]) %>% group_by(Absence.Type, Period) %>% summarise(Percentage.Hours.Lost = sum(Hours.Lost) / sum(Period.Total.Projected.Contracted.Hours))
  TDF$Period <- as.factor(TDF$Period)
  DivLeaveChart <- ggplot(data = TDF, aes(Absence.Type, Percentage.Hours.Lost, fill = Period)) +
    geom_bar(stat = "identity",  position = position_dodge(preserve = "single")) +
    ylab("Percentage of hours lost") +
    xlab("Absence Types") +
    ggtitle(paste("Percentage of Working Hours Lost - ", uDiv[i])) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
    coord_flip()
  
  print(DivLeaveChart)

  }


########### don't run this below #########
BackUpCode <- function(){

##Job Families by sickness type and division

uFam<-unique(FullData2$Job.Family)
uAbs<-unique(FullData2$Absence.Type)
uFam2<-c("ALLIED HEALTH PROFESSION","NURSING & MIDWIFERY", "HEALTHCARE SCIENCES" )
FullData2$Period <- as.factor(FullData2$Period)

for(i in 1:length(uFam2)) {
  
  for(j in 1:length(uAbs)) {
    
    TDF <- FullData2 %>% filter(Job.Family==uFam2[i], Absence.Type==uAbs[j]) %>% group_by(Division, Period) %>% summarise(Percentage.Hours.Lost=sum(Hours.Lost)/sum(Period.Total.Projected.Contracted.Hours))
    JobFamLeaveTypeChart <- ggplot(data = TDF, aes(x = Division, y = Percentage.Hours.Lost, fill = Period)) +
      #geom_bar(stat = "identity", position='dodge') +
      geom_bar(stat = "identity",  position = position_dodge(preserve = "single")) +
      ylab("Percentage of hours lost") +
      ggtitle(paste("Percentage of Working Hours Lost ", uFam2[i], uAbs[j])) +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_flip()
    
   # str(TDF)
    print(JobFamLeaveTypeChart)
    
    
  }
  #dev.off()
}



#Annual leave entitlement by job families and divisions

TDF <- AnnLeaEnt %>% group_by(Job.Family) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))
#TDF$Percentage.AL.Used <- TDF$`sum(Total.Notional.Entitlement.Used)` / TDF$`sum(Total.Notional.Entitlement)`

for(i in 1:length(uFam2)) {
  

    TDF <- AnnLeaEnt %>% filter(Job.Family==uFam2[i]) %>% group_by(Division) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))
    TDF$Percentage.AL.Used <- TDF$`sum(Total.Notional.Entitlement.Used)` / TDF$`sum(Total.Notional.Entitlement)`
    JobFamAnnLeaChart <- ggplot(data = TDF, aes(x = Division, y = Percentage.AL.Used)) +
      geom_bar(stat = "identity") +
      ylab("Percentage Leave Used") +
      ggtitle(paste("Percentage of Leave Used ", uFam2[i])) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_flip()
    
    # str(TDF)
    print(JobFamAnnLeaChart)
    
    
  
  #dev.off()
}



#all leaves so far stacked together each division, for job families

#Cumulative
for(i in 1:length(uFam2)){
  
  TDF <- FullData2 %>% filter(Job.Family==uFam2[i]) %>% group_by(Division, Absence.Type) %>% summarise(Percentage.Hours.Lost = sum(Hours.Lost) / sum(Period.Total.Projected.Contracted.Hours))
  JobFamAllLost <- ggplot(data = TDF, aes(Division, Percentage.Hours.Lost, fill = Absence.Type)) +
    geom_bar(stat = "identity", position = "stack") +
    ylab("Percentage of hours lost") +
    xlab("Divisions") +
    ggtitle(paste("Percentage of Working Hours Lost - ", uFam2[i])) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
    coord_flip()
  
  print(JobFamAllLost)
}


#Specific Month

for(i in 1:length(uFam2)){
  
  TDF <- FullData2 %>% filter(Job.Family==uFam2[i], Period == "2021-12-01") %>% group_by(Division, Absence.Type) %>% summarise(Percentage.Hours.Lost = sum(Hours.Lost) / sum(Period.Total.Projected.Contracted.Hours))  %>% filter(Percentage.Hours.Lost != 0)
  JobFamAllLost <- ggplot(data = TDF, aes(Division, Percentage.Hours.Lost, fill = Absence.Type)) +
    geom_bar(stat = "identity", position = "stack") +
    ylab("Percentage of hours lost") +
    xlab("Divisions") +
    ggtitle(paste("Percentage of Working Hours Lost - ", uFam2[i])) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
    coord_flip()
  
  print(JobFamAllLost)
  
}

}
########### don't run this above #########



###Job Family Graphs

uFam<-unique(FullData2$Job.Family)
uAbs<-unique(FullData2$Absence.Type)
uFam2<-c("ALLIED HEALTH PROFESSION","NURSING & MIDWIFERY", "HEALTHCARE SCIENCES" )
FullData2$Period <- as.factor(FullData2$Period)

for(i in 1:length(uFam2)) {
  
  pdf(paste("~/Job Family Charts - ", uFam2[i], ".pdf"))
  
  
  for(j in 1:length(uAbs)) {
    
    TDF <- FullData2 %>% filter(Job.Family==uFam2[i], Absence.Type==uAbs[j]) %>% group_by(Division, Period) %>% summarise(Percentage.Hours.Lost=sum(Hours.Lost)/sum(Period.Total.Projected.Contracted.Hours))
    JobFamLeaveTypeChart <- ggplot(data = TDF, aes(x = Division, y = Percentage.Hours.Lost, fill = Period)) +
      #geom_bar(stat = "identity", position='dodge') +
      geom_bar(stat = "identity",  position = position_dodge(preserve = "single")) +
      ylab("Percentage of hours lost") +
      ggtitle(paste("Percentage of Working Hours Lost ", uFam2[i], uAbs[j])) +
      scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_flip()
    
      print(JobFamLeaveTypeChart)
    
    
  }
  
  TDF <- AnnLeaEnt %>% group_by(Job.Family) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))

    
    TDF <- AnnLeaEnt %>% filter(Job.Family==uFam2[i]) %>% group_by(Division) %>% summarise(sum(Total.Notional.Entitlement), sum(Total.Notional.Entitlement.Used))
    TDF$Percentage.AL.Used <- TDF$`sum(Total.Notional.Entitlement.Used)` / TDF$`sum(Total.Notional.Entitlement)`
    JobFamAnnLeaChart <- ggplot(data = TDF, aes(x = Division, y = Percentage.AL.Used)) +
      geom_bar(stat = "identity") +
      ylab("Percentage Leave Used") +
      ggtitle(paste("Percentage of Leave Used ", uFam2[i])) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      coord_flip()
    


      TDF <- FullData2 %>% filter(Job.Family==uFam2[i], Period == "2021-12-01") %>% group_by(Division, Absence.Type) %>% summarise(Percentage.Hours.Lost = sum(Hours.Lost) / sum(Period.Total.Projected.Contracted.Hours))  %>% filter(Percentage.Hours.Lost != 0)
      JobFamAllLost <- ggplot(data = TDF, aes(Division, Percentage.Hours.Lost, fill = Absence.Type)) +
        geom_bar(stat = "identity", position = "stack") +
        ylab("Percentage of hours lost") +
        xlab("Divisions") +
        ggtitle(paste("Percentage of Working Hours Lost - ", uFam2[i])) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, max(TDF$Percentage.Hours.Lost)*4/3)) +
        coord_flip()
      

      
      #print to file
      
      print(JobFamAnnLeaChart)
      print(JobFamAllLost)
      dev.off()
  
  
}



