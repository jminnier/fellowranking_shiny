library(shiny)
library(tidyverse)
library(zoo)
library(tools)
library(markdown)
#library(plotly)
library(readxl)
library(ggvis)

print(sessionInfo())

clean_data <- function(dat) {
  dat$FellowName = na.locf(dat$FellowName) # last observation carried forward
  dat = dat%>%filter(!is.na(Interviewer),!is.na(Score))
}

rank_fellow_data <- function(dat,
                             name_of_fellow_column = "FellowName",
                             name_of_interviewer_column = "Interviewer",
                             name_of_score_column = "Score") {
  
  colnames(dat)[which(colnames(dat)==name_of_fellow_column)] = "FellowName"
  colnames(dat)[which(colnames(dat)==name_of_interviewer_column)] = "Interviewer"
  colnames(dat)[which(colnames(dat)==name_of_score_column)] = "Score"
  
  
  validate(need(is.numeric(dat$Score),message="Score must be numeric."))
  
  dat <- clean_data(dat)
  
  dat$Interviewer <- factor(dat$Interviewer)
  dat_fellow <- dat%>%group_by(FellowName)%>%
    summarise("meanS"=mean(Score),"medianS"=median(Score))%>%arrange(-1*medianS)
  
  #Raw ranking
  dat_fellow$rank_medianS <- rank(-1*dat_fellow$medianS)
  dat_fellow$rank_meanS <- rank(-1*dat_fellow$meanS)
  
  
  #Normalized interviewer rank
  dat_irank <- data.frame()
  for(ii in levels(factor(dat$Interviewer))) {
    tmp <- dat%>%filter(Interviewer==ii)
    tmp <- tmp%>%mutate("rankI"=(rank(-1*Score)-min(rank(-1*Score)))/(max(rank(-1*Score)-min(rank(-1*Score)))))
    dat_irank <- rbind(dat_irank,tmp)  
  }
  dat <- left_join(dat,dat_irank)
  
  #Get interview level mean scores and sds etc
  minmax <- dat%>%group_by(Interviewer)%>%
    summarise("minS_i"=min(Score),"maxS_i"=max(Score),"sdS_i"=sd(Score),
              "madS_i"=mad(Score),"medianS_i"=median(Score),
              "meanS_i"=mean(Score))%>%arrange(Interviewer)
  dat_i <- left_join(dat,minmax)%>%arrange(Interviewer)
  
  #Create normalized scores within interviewers
  #Take median and mean of rank or scoreInroms to get candidate's scores
  tmp <- dat_i%>%mutate("Score_Inorm_median"= (Score-medianS_i)/madS_i,
                        "Score_Inorm_mean"= (Score-meanS_i)/sdS_i
  )%>%group_by(FellowName)%>%summarise(
    "medianS_Inorm"=median(Score_Inorm_median,na.rm=T),
    "meanS_Inorm"=mean(Score_Inorm_mean,na.rm=T),
    "meanR_i"=mean(rankI,na.rm=T),"medianR_i"=median(rankI,na.rm=T))
  dat_fellow <- left_join(dat_fellow,tmp)
  dat_fellow$rank_medianSI <- rank(-1*dat_fellow$medianS_Inorm)
  dat_fellow$rank_meanSI <- rank(-1*dat_fellow$meanS_Inorm)
  dat_fellow$rank_meanRI <- rank(dat_fellow$meanR_i)
  dat_fellow$rank_medianRI <- rank(dat_fellow$medianR_i)
  
  dat_fellow$rank_sum_final <- with(dat_fellow,
                                    rank_medianS+rank_meanS+rank_medianSI+rank_meanSI+rank_meanRI+rank_medianRI)
  # dat_fellow$rank_sum_med <- with(dat_fellow,rank_medianS+rank_medianSI+rank_medianRI)
  # dat_fellow$rank_sum_mean <- with(dat_fellow,rank_meanS+rank_meanSI+rank_meanRI)
  # dat_fellow$rank_sum1 <- with(dat_fellow,rank_medianS+rank_medianSI)
  # dat_fellow$rank_sum_final <- with(dat_fellow,rank_medianS+rank_meanS+rank_medianSI+rank_medianRI)
  
  dat_fellow <- dat_fellow%>%mutate("order_final"=rank(rank_sum_final))
  
  df_final <- dat_fellow[,c("FellowName","order_final","rank_sum_final","meanS","medianS","rank_meanS","rank_medianS",
                            "rank_medianSI","rank_medianRI")]
  
  df_final<- df_final%>%rename(
    SUM_ranks=rank_sum_final,
    mean_Score = meanS,
    median_Score = medianS,
    rank_meanScore = rank_meanS,
    rank_medScore = rank_medianS,
    rank_Inorm_median = rank_medianSI,
    #rank_Inorm_mean = rank_meanSI, #did not use this
    rank_Irank = rank_medianRI
    )%>%arrange(SUM_ranks)
  df_final
}

scatterplot_fun <- function(mydata,myx,myy){
  mydata$xaxis = unlist(mydata[,myx])
  mydata$yaxis = unlist(mydata[,myy])

  p <- ggplot(mydata,aes(x=xaxis,y=yaxis,col=SUM_ranks,key=FellowName)) +
    xlab(myx)+ylab(myy)+
    theme_minimal()+geom_point()
  p
}

scatterplot_fun_ggvis <- function(mydata,myx,myy) {
  mydata$xaxis = unlist(mydata[,myx])
  mydata$yaxis = unlist(mydata[,myy])
  mydata$id = 1:nrow(mydata)
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- mydata[mydata$id == x$id, ]
    paste0(names(row), ": ", format(row), collapse = "<br />")
    
  }
  mydata%>%ggvis(~xaxis,~yaxis,fill=~SUM_ranks,key:=~id)%>%layer_points()%>%
    add_tooltip(all_values, "hover")%>%add_axis("x",title=myx)%>%add_axis("y",title=myy)
}
