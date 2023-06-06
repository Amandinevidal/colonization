#### -----------------------------------------------------------------####
#### Author : Amandine Vidal-Hosteng
#### Encoding : UTF-8
#### Email : a.y.vidal-hosteng@rug.nl
#### File path : colonization/analysis.R
#### 
#### Script for analyzing results and summary files
#### -----------------------------------------------------------------####

rm(list = ls())
sim <- "test2" # which simulation you want to analyse?

#### Simulation parameters ####
lines <- scan(paste0("results/",sim,"_1_description.txt"), what=character(), sep="\n", skip=4, quiet=TRUE)
tc <- textConnection(lines)
source(tc)
close(tc)

#### Library and functions ####
library(ggplot2)
library(readr)
mean_function <- function(list,var){
  dataset <- c()
  for(f in list){
    file <- read.table(paste0("results/",f))
    data <- file[,var]
    dataset <- cbind(dataset,data)
  }
  mean <- rowMeans(dataset)
  return(mean)
}

#### summary means ####
list <- list.files(path="results/",pattern = "summary.txt") 
summary_list  <- list[grep(paste(sim), list)]
# variables names (14) "nindm","nindi","nindt","ndm","ndi","ndt","nbm","nbi","nbt","nmigm","nmigi","nmigt","succmigm","succmigi"
mean_nindm <- mean_function(summary_list,2)
mean_nindi <- mean_function(summary_list,3)
mean_nindt <- mean_function(summary_list,4)
mean_ndm <- mean_function(summary_list,5)
mean_ndi <- mean_function(summary_list,6)
mean_ndt <- mean_function(summary_list,7)
mean_nbm <- mean_function(summary_list,8)
mean_nbi <- mean_function(summary_list,9)
mean_nbt <- mean_function(summary_list,10)
mean_nmigm <- mean_function(summary_list,11)
mean_nmigi <- mean_function(summary_list,12)
mean_nmigt <- mean_function(summary_list,13)
mean_succmigm <- mean_function(summary_list,14)
mean_succmigi <- mean_function(summary_list,15)
t <- seq(1,time)
mean_summary <- as.data.frame(cbind(t,mean_nindm,mean_nindi,mean_nindt,mean_ndm,mean_ndi,mean_ndt,mean_nbm,mean_nbi,mean_nbt,mean_nmigm,mean_nmigi,mean_nmigt,mean_succmigm,mean_succmigi))

#### PLOTS ####
col <- c("mainland"="red","island"="blue","total"="black")
plot.nind <- ggplot2::ggplot(data=mean_summary,aes(x=t,y=mean_nindt)) +
  ggplot2::geom_line(aes(x=t,y=mean_nindi,color="island")) +
  ggplot2::geom_line(aes(x=t,y=mean_nindm,color="mainland")) +
  ggplot2::geom_line(aes(x=t,y=mean_nindt,color="total")) +
  ggplot2::theme(legend.position = "top",text = element_text(size = 20)) +
  ggplot2::labs(x="Time", y="Number of individuals", color="col") +
  ggplot2::scale_color_manual(values=col) 
plot.nd <- ggplot2::ggplot(data=mean_summary,aes(x=t,y=mean_ndt)) +
  ggplot2::geom_line(aes(x=t,y=mean_ndi,color="island")) +
  ggplot2::geom_line(aes(x=t,y=mean_ndm,color="mainland")) +
  ggplot2::geom_line(aes(x=t,y=mean_ndt,color="total")) +
  ggplot2::theme(legend.position = "top",text = element_text(size = 20)) +
  ggplot2::labs(x="Time", y="Number of death", color="col") +
  ggplot2::scale_color_manual(values=col) 
plot.nb <- ggplot2::ggplot(data=mean_summary,aes(x=t,y=mean_nbt)) +
  ggplot2::geom_line(aes(x=t,y=mean_nbi,color="island")) +
  ggplot2::geom_line(aes(x=t,y=mean_nbm,color="mainland")) +
  ggplot2::geom_line(aes(x=t,y=mean_nbt,color="total")) +
  ggplot2::theme(legend.position = "top",text = element_text(size = 20)) +
  ggplot2::labs(x="Time", y="Number of birth", color="col") +
  ggplot2::scale_color_manual(values=col) 
plot.mig <- ggplot2::ggplot(data=mean_summary,aes(x=t,y=mean_nmigt)) +
  ggplot2::geom_line(aes(x=t,y=mean_nmigi,color="island")) +
  ggplot2::geom_line(aes(x=t,y=mean_nmigm,color="mainland")) +
  ggplot2::geom_line(aes(x=t,y=mean_nmigt,color="total")) +
  ggplot2::theme(legend.position = "top",text = element_text(size = 20)) +
  ggplot2::labs(x="Time", y="Number of migration", color="col") +
  ggplot2::scale_color_manual(values=col) 
plot.col <- ggplot2::ggplot(data=mean_summary,aes(x=t,y=mean_succmigi)) +
  ggplot2::geom_line(aes(x=t,y=mean_succmigi,color="island")) +
  ggplot2::geom_line(aes(x=t,y=mean_succmigm,color="mainland")) +
  ggplot2::theme(legend.position = "top",text = element_text(size = 20)) +
  ggplot2::labs(x="Time", y="Number of successful colonization", color="col") +
  ggplot2::scale_color_manual(values=col) 
# fitness landscape ?