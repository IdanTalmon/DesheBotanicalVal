## ---- message=FALSE---------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(reshape)
library(readr)
locale("he")
library(openxlsx)

## ---------------------------------------------------------------------------------------
remove(list = ls())
HbitatGroup <- 1

data=read_csv("Data/YizreelSpByPolyNum.csv",
              locale = locale(date_names = "he", encoding = "Windows-1255")) 

#the survey was devided into diferent habitats and scores were calculated for each habitat spepratly

data=data%>% 
     filter(Groups == HbitatGroup)


## ---------------------------------------------------------------------------------------
pivot<-data %>% 
  select(plot.name, SPHebName) %>% 
  group_by(plot.name, SPHebName)  %>% 
  summarise(count=length(SPHebName)) %>% 
  spread(plot.name, count, fill = 0)


## ---- message=FALSE---------------------------------------------------------------------
mimis=read_csv(file = "Data/Yizreel.mimis.csv",
              locale = locale(date_names = "he", encoding = "Windows-1255"))
mimis=mimis %>% 
  select(SPHebName, Rarness, Endemic, Rednum, Almostred, Foreign, Invassive, LifeForm, DisturbanceOfHabitat)
colnames<-(colnames(data))
NumberOfPlots<-length(levels(as.factor(data$plot.name)))
pivot<-pivot%>% 
  right_join(x = mimis)
PropertiesColNum<-ncol(mimis)


## ---------------------------------------------------------------------------------------
SpeciesList<-pivot
set_threshold <- function(col){
  col <- as.numeric(col);
  col[col > 1]  <- 1;
  return (col);
}
col_names <- tail(names(SpeciesList),-PropertiesColNum)
SpeciesList[col_names] <- lapply(SpeciesList[col_names], FUN = set_threshold)



## ---------------------------------------------------------------------------------------
NAS<-SpeciesList[is.na(SpeciesList$LifeForm),]


## ---------------------------------------------------------------------------------------

LifeForms <- LifeForms <- c("עץ","שיח","בן-שיח","עשבוני רב-שנתי","חד-שנתי")
LifeFormsEng <- c("TreeSPrichness","BushSPrichness","ShrubSPrichness", "PerennialSPrichness","AnnualSPrichness")


for (i in 1:length(LifeForms)) 
{  
LifeForm.sub.total1 <- SpeciesList %>%
  filter(LifeForm == LifeForms[i])%>% 
  adorn_totals("row",  name = LifeFormsEng[i])
LifeForm.sub.total2<- LifeForm.sub.total1[nrow(LifeForm.sub.total1),]
SpeciesList<-rbind(SpeciesList,LifeForm.sub.total2)
}


## ---------------------------------------------------------------------------------------
# Scores<-SpeciesList %>% 
#    filter(!is.na(Rarness))

Scores<-SpeciesList %>%
  filter(Rarness=="-")


for (i in 1:length(LifeFormsEng)) {
LifeForm.score.data<-Scores %>% 
  filter(Scores[,1]  == LifeFormsEng[i])
LifeForm.Max<-max(LifeForm.score.data[,(PropertiesColNum+1):length((LifeForm.score.data[1,]))])
LifeForm.Min<-min(LifeForm.score.data[,(PropertiesColNum+1):length((LifeForm.score.data[1,]))])

My.Normalize.Fun <- function(x,Score.min,Score.max){
  100*((x-Score.min)/(Score.max-Score.min))
}


Normalize.LifeForm <- My.Normalize.Fun(LifeForm.score.data[,(PropertiesColNum+1):ncol(LifeForm.score.data)],LifeForm.Min,LifeForm.Max)

Scores[(5+i),]<-c(paste0("Normalized.",LifeFormsEng[i]),rep(NA,(PropertiesColNum-1)),Normalize.LifeForm)
}


#Means
Means.score.data<-Scores[6:10,]
Score.Means<-colMeans(Means.score.data[,(PropertiesColNum+1):length(Means.score.data[1,])],dims = 1)
Score.Means <- unname(Score.Means)
Means.Max<-max(Score.Means)
Means.Min<-min(Score.Means)

Normalize.Means<- My.Normalize.Fun(x = Score.Means,Score.min =  Means.Min, Score.max =  Means.Max)
  Normalize.Means.fun(Score.Means)

  Scores <- as.data.frame(Scores)
Scores[11,]<-c("SP.Richness.Score",rep(NA,(PropertiesColNum-1)),as.numeric(Normalize.Means))


## ---------------------------------------------------------------------------------------
SpeciesList$Rednum<-as.numeric(SpeciesList$Rednum)
SpeciesList <- as.data.frame(SpeciesList)
SpeciesList[is.na(SpeciesList)]<-0
NumberOfSpecies<-nrow(pivot)
redfun<-function(plotNum,rnum,rareness,endmic){
  if (plotNum != 0){
    if (rnum != 0.0){
      return(as.numeric(plotNum)*as.numeric(rnum)+(as.numeric(plotNum)*4))
    } else if (rareness == "RR"){
      return(4)
    } else if (rareness == "RP"){
      return(3)
    } else if (rareness == "R"){
      return(2)
    } else if (endmic != ""){
      return(1)
    }
  }
  return(0)
}

columnAmun<-ncol(SpeciesList)


for (j in (PropertiesColNum+1):columnAmun){
  for (i in 1:NumberOfSpecies) {
    SpeciesList[i,NumberOfPlots+j]<-redfun(SpeciesList[i,j], SpeciesList[i,4],SpeciesList[i,2],SpeciesList[i,3])
  }
}


SpeciesList[,(PropertiesColNum+1):length(SpeciesList[1,])]<-lapply(SpeciesList[,(PropertiesColNum+1):length(SpeciesList[1,])],as.numeric)

Red1 <- SpeciesList[1:NumberOfSpecies,]
Red1[,(PropertiesColNum+NumberOfPlots+1):length(Red1[1,])] <- lapply(Red1[,(PropertiesColNum+NumberOfPlots+1):length(Red1[1,])], as.numeric)
Red1<-adorn_totals(Red1,"row",  name = "RedVal")

Red2<- Red1[length(Red1[,1]),(PropertiesColNum+NumberOfPlots+1):length(Red1[1,])]
SpeciesList<-bind_rows(SpeciesList,Red2)
SpeciesList[length(SpeciesList[,1]),1]<-"RedVal"

Red.Max<-max(SpeciesList[(NumberOfSpecies+6),(PropertiesColNum+NumberOfPlots+1):length(SpeciesList[1,])]) 
Red.Min<-min(SpeciesList[(NumberOfSpecies+6),(PropertiesColNum+NumberOfPlots+1):length(SpeciesList[1,])])
Normalize.Red.fun <- function(x){
  100*((x-Red.Min)/(Red.Max-Red.Min))
}
Normalize.Red<- as.numeric(Normalize.Red.fun(SpeciesList[NumberOfSpecies+6,(PropertiesColNum+NumberOfPlots+1):length(SpeciesList[1,])]))# Not Universal!
Scores[12,]<-c("Red.Score" ,rep(NA,(PropertiesColNum-1)),as.numeric(Normalize.Red))


FinalScores<-cbind.data.frame(as.integer(col_names), Normalize.Means, Normalize.Red)
colnames(FinalScores)<-c("LandcoverPolygonID","SP.Richness.Score","Red.Score")



## ---------------------------------------------------------------------------------------
LandCoverdata=read_csv("Data/YzraelLandCover.csv",
                       locale = locale(date_names = "he", encoding = "Windows-1255"))%>%
  filter(!GeneralazedVegTypeNUM == 3) %>% 
  filter(Groups == HbitatGroup)

LandCoverdata$RehabScore <- (as.numeric(LandCoverdata$RehabScore))

RegionalScarcity<-LandCoverdata %>%
  filter(!VegFormation=="") %>% 
  select(VegFormation, Shape_Area) %>% 
  group_by(VegFormation)%>% 
  summarise(SumArea=sum(Shape_Area))
FormationAreas<-RegionalScarcity$SumArea
totalFormArea<-sum(FormationAreas)
RegionalScarcity<-RegionalScarcity %>%
  mutate(Scarcity=SumArea/totalFormArea)  
ScarcityVal<-RegionalScarcity$Scarcity
MaxScarcity<-max(ScarcityVal)
MinScarcity<-min(ScarcityVal)
Normalize.RegionalScarcity.fun <- function(x){
  100*((x-MinScarcity)/(MaxScarcity-MinScarcity))
}
Normalize.RegionalScarcity<-Normalize.RegionalScarcity.fun(RegionalScarcity$Scarcity)
Normalize.RegionalScarcity<-(100-Normalize.RegionalScarcity)
RegionalScarcity<-cbind(RegionalScarcity,Normalize.RegionalScarcity)
RegionalScarcity <- RegionalScarcity %>% 
  select(-c(SumArea,Scarcity))


## ---------------------------------------------------------------------------------------
NatScarcityData<-read_csv("Data/NationalRarenessScore.csv",
                          locale = locale(date_names = "he", encoding = "Windows-1255"))
NatScarcityData <- NatScarcityData %>% 
  select(-c(TotalArea,Protected,ProtectedPercent,NotProtected))


## ---------------------------------------------------------------------------------------
ComplexityDF=LandCoverdata %>% 
  select(CoveragePercentPlantedTrees, CoveragePercentNaturalTrees, CoveragePercentBushes, CoveragePercentShrubs, CoveragePercentPerenials, CoveragePercentAnnuals) %>% 
  mutate(Score = rowSums(. > 10) * 20) # the "." means all columns larger than 10
ComplexityDF$Score[ComplexityDF$Score == 120]<-100 # seting 100 as the maximum value
ComplexityScore<-ComplexityDF$Score


## ---------------------------------------------------------------------------------------
# RehabData<-read_csv("Data/VegFormaitionScore.csv",
#                     locale = locale(date_names = "he", encoding = "Windows-1255"))
ScoreDF<-cbind(LandCoverdata, ComplexityScore)


## ---------------------------------------------------------------------------------------
NaturalRepresentation<-read_csv("Data/YzraelNaturalRepresentation.csv",
                                locale = locale(date_names = "he", encoding = "Windows-1255"))


## ---------------------------------------------------------------------------------------
ScoreDF<-ScoreDF %>% 
  left_join(y = RegionalScarcity, by = "VegFormation")%>%    
#  left_join(y = RehabData, by = "VegFormation") %>% 
  left_join(y=NatScarcityData, by = "EcoSystem") %>%
  left_join(y = FinalScores, by = "LandcoverPolygonID") %>%
  left_join(y = NaturalRepresentation, by = "LandcoverPolygonID") %>%
  select(-NationalRarnessScoreInt)

#extrapulation
GenVegType<-unique(ScoreDF$GeneralazedVegTypeNUM)
#GenVegType<-GenVegType[-1]
NumGenVegType<-length(GenVegType)
SamplingTyps<-levels(as.factor(ScoreDF$SamplingType))
Average<-c()
#ComplexityScore
for (i in 1:NumGenVegType) {
  ComplexityAv<-ScoreDF %>%
    select(GeneralazedVegTypeNUM,ComplexityScore)%>%
    filter(GeneralazedVegTypeNUM==GenVegType[i]) %>% 
    filter(!is.na(ComplexityScore)) %>% 
    mutate(AverageCompGenVegFor = mean(ComplexityScore))
  Average[i]<-ComplexityAv$AverageCompGenVegFor[1]
}

for (i in 1:NumGenVegType) {
  ScoreDF$ComplexityScore[ScoreDF$GeneralazedVegTypeNUM==GenVegType[i]&is.na(ScoreDF$ComplexityScore)]<-Average[i]
}

#SP.Richness.Score
for (i in 1:NumGenVegType) {
  SP.RichnessAv<-ScoreDF %>%
    select(GeneralazedVegTypeNUM,SP.Richness.Score)%>%
    filter(GeneralazedVegTypeNUM==GenVegType[i]) %>% 
    filter(!is.na(SP.Richness.Score)) %>% 
    mutate(AverageSP.R = mean(SP.Richness.Score))
  Average[i]<-SP.RichnessAv$AverageSP.R[1]
}

for (i in 1:NumGenVegType) {
  ScoreDF$SP.Richness.Score[ScoreDF$GeneralazedVegTypeNUM==GenVegType[i]&is.na(ScoreDF$SP.Richness.Score)]<-Average[i]
}

#Red
for (i in 1:NumGenVegType) {
  RedAv<-ScoreDF %>%
    select(GeneralazedVegTypeNUM,Red.Score)%>%
    filter(GeneralazedVegTypeNUM==GenVegType[i]) %>% 
    filter(!is.na(Red.Score)) %>% 
    mutate(AverageRed = mean(Red.Score))
  Average[i]<-RedAv$AverageRed[1]
}

for (i in 1:NumGenVegType) {
  ScoreDF$Red.Score[ScoreDF$GeneralazedVegTypeNUM==GenVegType[i]&is.na(ScoreDF$Red.Score)]<-Average[i]
}

ScoreDF<-ScoreDF %>% 
  mutate(FinalScore = (SP.Richness.Score*0.2+
                         Red.Score*0.2+
                         LifeFormScore*0.1+
                         DominantSPScore*0.1+
                         Normalize.RegionalScarcity*0.1+
                         NationalRarnessScore*0.1+
                         ComplexityScore*0.15+
                         RehabScore*0.05))

a=2
b=5

Normalize.Final.Score.fun <- function(x){
  a+((x-min(x, na.rm = TRUE))*(b-a))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE))
}

ScoreDF<-ScoreDF %>% 
  mutate(BotanicalVal=Normalize.Final.Score.fun(ScoreDF$FinalScore)) %>% 
  mutate(BotanicalVal=round(BotanicalVal,0))


NAS2<-ScoreDF[is.na(ScoreDF$BotanicalVal),]
## ---------------------------------------------------------------------------------------
write.xlsx(ScoreDF,paste0("Output/YzraelScores",HbitatGroup,".xlsx"))
write.xlsx(NAS2,"Output/YzraelNAS.csv.xlsx")
