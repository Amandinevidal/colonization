library(ggridges)
library(ggplot2)

ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()
d <- iris

test <- subset(baseline.pop,t==0|t==20|t==60|t==80|t==100)
test2 <- baseline.pop
test2$t <- as.character(test2$t)
test2$loc <- as.factor(test2$loc)
test$t <- as.character(test$t)
test$loc <- as.character(test$loc)
test$or <- as.character(test$or)
test.main <- subset(test,loc=="0")
test.isl <- subset(test,loc=="1")
ggplot(test, aes(x=x,y=as.factor(t))) + geom_density_ridges2()
ggplot(test, aes(x=x,y=as.factor(t))) + stat_density_ridges(quantile_lines = TRUE)

library(dplyr)
library(forcats)

main <- ggplot(test.main, aes(x = x, y = as.factor(t), color = as.factor(or), point_color = as.factor(or), fill = as.factor(or))) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution on mainland") +
  theme_ridges(center = T)

isl <- ggplot(test.isl, aes(x = x, y = t, color = or, point_color = or, fill = or)) +
  geom_density_ridges(
    jittered_points = TRUE, scale = .95, rel_min_height = .01,
    point_shape = ".", point_size = 2, size = 0.45,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "Ecological trait") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("Mainland", "Island"),name = "Origin") +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  coord_cartesian(clip = "off") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA)
  )
  ) +
  ggtitle("Ecological trait distribution on island") +
  theme_ridges(center = T)

library(cowplot)

plot_grid(main,isl)

ah <- ggplot(baseline.pop, aes(x = x, y = t, group=t)) +
  geom_density_ridges(scale = 20, size = 0.25, rel_min_height = 0.03) +
  theme_ridges() +
  scale_x_continuous(limits = c(-2, 2), expand = c(0, 0)) +
  scale_y_reverse(
    breaks = c(100, 80, 60, 40, 20, 0),
    expand = c(0, 0)
  ) +
  coord_cartesian(clip = "off")
library(rayshun)
plot_gg(main, multicore = TRUE, raytrace = TRUE, width = 7, height = 4, 
        scale = 300, windowsize = c(1400, 866), zoom = 0.6, phi = 30, theta = 30)
render_snapshot()

if(rayshader:::run_documentation()) {
  render_camera(zoom=1,theta=-90,phi=30)
  render_snapshot()
}







#### H1 : The larger the population the larger the propagule pressure ####

# do we have more propagule pressure from the mainland?
colors <- c("From mainland"="brown","From island"="blue")
mig_baseline <- ggplot2::ggplot(data = baseline, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="From mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=migi_mean,color="From island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of migrant", color="Origin")+
  ggplot2::scale_color_manual(values=colors)

mig_diffopt <- ggplot2::ggplot(data = diffopt, aes(x=time,y=migm_mean)) +
  ggplot2::geom_line(aes(color="From mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=migm_ciupp,ymin=migm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=migi_mean,color="From island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=migi_ciupp,ymin=migi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of migrant", color="Origin")+
  ggplot2::scale_color_manual(values=colors)

# colonization success?
colors <- c("Mainland migrators"="brown","Island migrators"="blue")
emig_baseline <- ggplot2::ggplot(data = baseline, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="Mainland migrators")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=emigi_mean,color="Island migrators")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of successfull colonisation", color="Origin")+
  ggplot2::scale_color_manual(values=colors)

colors <- c("Mainland migrators"="brown","Island migrators"="blue")
emig_diffopt <- ggplot2::ggplot(data = diffopt, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="Mainland migrators")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=emigi_mean,color="Island migrators")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of successfull colonisation", color="Origin")+
  ggplot2::scale_color_manual(values=colors)

# Proportion of natives and migrant at each time?
colors <- c("Natives"="black","Migrants"="red")
linetypes <- c("Mainland"="solid","Island"="dashed")

pnm_baseline <- ggplot2::ggplot(data = baseline, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="Migrants",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebm_mean,color="Natives",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=emigi_mean,color="Migrants",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebi_mean,color="Natives",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of individuals", color="Origin")+
  ggplot2::scale_color_manual(values=colors)+
  ggplot2::scale_linetype_manual(values=linetypes)

pnm2_baseline <- ggplot2::ggplot(data = baseline, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="Migrants",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=emigi_mean,color="Migrants",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebi_mean,color="Natives",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of individuals", color="Origin")+
  ggplot2::scale_color_manual(values=colors)+
  ggplot2::scale_linetype_manual(values=linetypes)

pnm_diffopt <- ggplot2::ggplot(data = diffopt, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="Migrants",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebm_mean,color="Natives",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=emigi_mean,color="Migrants",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebi_mean,color="Natives",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of individuals", color="Origin")+
  ggplot2::scale_color_manual(values=colors)+
  ggplot2::scale_linetype_manual(values=linetypes)

# trait
x <- ggplot2::ggplot(data = diffopt, aes(x=time,y=emigm_mean)) +
  ggplot2::geom_line(aes(color="Migrants",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigm_ciupp,ymin=emigm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebm_mean,color="Natives",linetype="Mainland")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebm_ciupp,ymin=ebm_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=emigi_mean,color="Migrants",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=emigi_ciupp,ymin=emigi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::geom_line(aes(x=time,y=ebi_mean,color="Natives",linetype="Island")) +
  ggplot2::geom_ribbon(aes(x=time,ymax=ebi_ciupp,ymin=ebi_cidown),fill="grey2",alpha=0.1)+
  ggplot2::labs(x="Time (in generation)", y="Number of individuals", color="Origin")+
  ggplot2::scale_color_manual(values=colors)+
  ggplot2::scale_linetype_manual(values=linetypes)




#### Data ####
data <- read.table("results/sim1_1_results.txt")
data <- data[,-1]
colnames(data) <- c("id","x","fit","origin","loc","mother","off","mig","age","time")
summary <- read.table("results/sim1_1_summary.txt")
summary <- summary[,-1]
colnames(summary) <- c("t","nm","ni","xm","xi","bm","bi","mutm","muti","migm","migi","dm","di","ebm","ebi","emigm","emigi","prpm","prpi","prom","proi","prmm","prmi","prcm","prci")

# test larger phenotypic ranges
data_t1_mainland <- data[which(data$time==1&data$loc==0),]
data_t1_mainland_o <- data_t1_mainland[which(data_t1_mainland$age==0),] # ONLY OFFPSRING THAT SURVIVED
hist(data_t1_mainland_o$x)
hist(data_t1_mainland$x)
maxm <- max(data_t1_mainland$x)
minm <- min(data_t1_mainland$x)
rangem <- maxm-minm
data_t1_island <- data[which(data$time==1&data$loc==1),]
hist(data_t1_island$x)
maxi <- max(data_t1_island$x)
mini <- min(data_t1_island$x)
rangei <- maxi-mini



# H2: The larger the population the larger the phenotypic range of the migrant pool = easy colonization in any situations
plot(summary$t,summary$prpm,type='l',ylim=c(0,8))
lines(summary$prpi)
plot(summary$t,summary$prom,type='l',ylim=c(0,3))
lines(summary$proi)
plot(summary$t,summary$prmm,type='l',ylim=c(0,2.5))
lines(summary$prmi)
plot(summary$t,summary$prcm,type='l',ylim=c(0,1.6))
lines(summary$prci)

# The larger the population the higher the number of individuals in every ecological niches = hard competition for island migrators
t1 <-ggplot(subset(data,time==1), aes(x=x, group=loc)) +
  geom_histogram(alpha=0.6, position = 'identity') 
t20 <-ggplot(subset(data,time==20), aes(x=x, group=loc)) +
  geom_histogram(alpha=0.6, position = 'identity') 
t40 <-ggplot(subset(data,time==40), aes(x=x, group=loc)) +
  geom_histogram(alpha=0.6, position = 'identity') 
t60 <-ggplot(subset(data,time==60), aes(x=x, group=loc)) +
  geom_histogram(alpha=0.6, position = 'identity') 

# H4: The larger the population, the faster the rate of adaptation to the local optimum = easy environmental changes tracking

plot(summary$t,summary$xm,type='l',ylim=c(-0.02,0.035),col="red")
lines(summary$xi)
abline(h=0)





# average birth 
trait <- sort(unique(data$x)) # all phenotypes observed ordered <
av <- data.frame(x=trait,offm=NA,offi=NA,migm=NA,migi=NA,compm=NA,compi=NA)
for (i in trait) {
  #birth
  moff.m <- mean(data[which(data$x==i&data$loc==0),]$off)
  moff.i <- mean(data[which(data$x==i&data$loc==1),]$off)
  if(!is.na(moff.i)){av[which(av$x==i),]$offi <- moff.i}else{av[which(av$x==i),]$offi <- NA}
  if(!is.na(moff.m)){av[which(av$x==i),]$offm <- moff.m}else{av[which(av$x==i),]$offm <- NA}
  #mig
  mig.m <- sum(data[which(data$x==i&data$loc==0),]$mig)
  mig.i <- sum(data[which(data$x==i&data$loc==1),]$mig)
  if(!is.na(mig.i)){av[which(av$x==i),]$migi <- mig.i}else{av[which(av$x==i),]$migi <- NA}
  if(!is.na(mig.m)){av[which(av$x==i),]$migm <- mig.m}else{av[which(av$x==i),]$migm <- NA}
  print(paste(i,"in",length(trait)))
}

ggplot2::ggplot(data = av, aes(x=x,y=offm)) +
  ggplot2::geom_point(data=subset(av,av$offm!=0),aes(x=x,y=offm,color="main")) +
  ggplot2::geom_point(data=subset(av,av$offi!=0),aes(x=x,y=offi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="offpsring average", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

ggplot2::ggplot(data = av, aes(x=x,y=migi)) +
  ggplot2::geom_point(data=subset(av,av$migm!=0),aes(x=x,y=migm,color="main")) +
  ggplot2::geom_point(data=subset(av,av$migi!=0),aes(x=x,y=migi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="migration", color="Location") +
  ggplot2::scale_color_manual(values=colors)


#### AVERAGE / TIME ####

data2 <- read.table("results/test2_1_summary.txt")
data2 <- data2[,-1]
colnames(data2) <- c("t","nm","ni","xm","xi","bm","bi","mutm","muti","migm","migi","dm","di","ebm","ebi","emigm","emigi")

ggplot2::ggplot(data = data2, aes(x=t,y=nm)) +
  ggplot2::geom_line(aes(x=t,y=nm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=ni,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="Number of individuals", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=xm)) +
  ggplot2::geom_line(aes(x=t,y=xm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=xi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="average x", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=ebm)) +
  ggplot2::geom_line(aes(x=t,y=bm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=bi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="birth", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=mutm)) +
  ggplot2::geom_line(aes(x=t,y=mutm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=muti,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="mutation", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=ebm)) +
  ggplot2::geom_line(aes(x=t,y=migm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=migi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="mig event", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=ebm)) +
  ggplot2::geom_line(aes(x=t,y=dm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=di,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="death event", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=ebm)) +
  ggplot2::geom_line(aes(x=t,y=ebm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=ebi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="local successful competition", color="Location") +
  ggplot2::scale_color_manual(values=colors)

ggplot2::ggplot(data = data2, aes(x=t,y=ebm)) +
  ggplot2::geom_line(aes(x=t,y=emigm,color="main")) +
  ggplot2::geom_line(aes(x=t,y=emigi,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="time", y="emig successful competition", color="Location") +
  ggplot2::scale_color_manual(values=colors)




















#### INIT ####
pop.plot <- ggplot2::ggplot(data = curr_isl, aes(x=x,y=fit)) +
  ggplot2::geom_line(data=curr_isl,aes(x=x,y=fit,color="isl")) +  
  ggplot2::geom_line(data=curr_main,aes(x=x,y=fit,color="main")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="fitness", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

#### BIRTH EVENT ####

noff.fit.plot <- ggplot2::ggplot(data = curr_isl, aes(x=fit,y=off)) +
  ggplot2::geom_line(data=curr_isl,aes(x=fit,y=off,color="isl")) +  
  ggplot2::geom_line(data=curr_main,aes(x=fit,y=off,color="main")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Fitness", y="Number of offspring", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

noff.trait.plot <- ggplot2::ggplot(data = curr_isl, aes(x=x,y=off)) +
  ggplot2::geom_line(data=curr_isl,aes(x=x,y=off,color="isl")) +  
  ggplot2::geom_line(data=curr_main,aes(x=x,y=off,color="main")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="Number of offspring", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

off.trait.dist.main.plot <- ggplot2::ggplot(data = off_main, aes(x=x,color="main")) +
  ggplot2::geom_histogram() +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

off.isl.trait.dist.plot <- ggplot2::ggplot(data = off_isl, aes(x=x,color="isl")) +
  ggplot2::geom_histogram() +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

#### MIGRATION EVENT ####
mig.trait.plot <- ggplot(off_main, aes(x = as.factor(x), fill = as.factor(mig))) +
  geom_bar(position = "dodge") 

fit.offspr.main.plot <- ggplot2::ggplot(data = off_main, aes(x=x,y=fit)) +
  ggplot2::geom_line(data=subset(off_main,loc==0),aes(x=x,y=fit,color="main")) +
  ggplot2::geom_line(data=subset(off_main,loc==1),aes(x=x,y=fit,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="Fitness", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

fit.offspr.isl.plot <- ggplot2::ggplot(data = off_isl, aes(x=x,y=fit)) +
  ggplot2::geom_line(data=subset(off_isl,loc==0),aes(x=x,y=fit,color="main")) +
  ggplot2::geom_line(data=subset(off_isl,loc==1),aes(x=x,y=fit,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="Fitness", color="Location") +
  ggplot2::scale_color_manual(values=colors) 

#### COMPETITION EVENT ####

p <- ggplot(comp_main, aes(origin,mig, colour = factor(origin))) + 
  geom_boxplot()

fit.offspr.isl.plot <- ggplot2::ggplot(data = off_isl, aes(x=x,y=fit)) +
  ggplot2::geom_line(data=subset(off_isl,loc==0),aes(x=x,y=fit,color="main")) +
  ggplot2::geom_line(data=subset(off_isl,loc==1),aes(x=x,y=fit,color="isl")) +
  theme_classic() +
  ggplot2::theme(legend.position = "top",text = element_text(size = 26)) +
  ggplot2::labs(x="Trait", y="Fitness", color="Location") +
  ggplot2::scale_color_manual(values=colors) 


