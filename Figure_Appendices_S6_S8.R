# Mellina Sidous
# 08/02/2025
            
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
            #        Figure Appendices         #
            #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



###################### Load packages
library(gam)
library(tidyverse)
library(ggplot2)
library(MASS)
library(effects)
library(ggeffects)
library(cowplot)
library(ggpubr)

#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------
          
          
          #-------------------------------------------------#
          #                    Load and                     #  
          #                 Arrange data                    # 
          #-------------------------------------------------#
          


###################### Load data
load("data/dat_nfeeding.RData")


Years_to_plot <- c("1990","1992","1995", "1998","2001", "2004", "2007","2010", "2013","2016", "2019")


#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------
                
                #-------------------------------------------------#
                #               Figure Appendix S6 :              #  
                #     Annual estimates from both categorical      # 
                #          and non linear effect of Months        #
                #-------------------------------------------------#


# A) Categorical effect of Month
#------------------------
cmonth.nb.glmI <- MASS::glm.nb(formula= count~ factor(Month) +offset(log(n_visits)),link=log, data=kill_dt)
#predictions :
mydf_cat <- ggpredict(cmonth.nb.glmI, terms = ( "Month"), back.transform = TRUE, condition= c(n_visits= 10))


# B) Non linear effect of Month
#------------------------
smonth.nb.gamI <- gam(count~ s(Month) + offset(log(n_visits)),family=neg.bin(1),  data=kill_dt)
# predictions
mydf_nl <- ggpredict(smonth.nb.gamI, terms = ( "Month[1:12]"), back.transform = FALSE, condition= c(n_visits= 10))


#Figure
annual_pattern <- ggplot(mydf_cat, aes(x, predicted))+ 
  theme_classic(base_size=20) +
  geom_jitter(data=(kill_dt%>% subset(Year %in% Years_to_plot)), aes(x=factor(Month), y=log(count), color=factor(Year)), alpha= 0.6, size=1.5)+
  scale_color_manual(values= c( "#F4D085","#FFCA38", "#FF930F", "#FB613C", "#FF0800", "#E32951","#D53A7B", "#C256B0", "#AF70E2", "#5E59AB", "#074170"))+
  geom_point(size=3, color="#2f3e46" )+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="#2f3e46" ) +
  geom_line(data=mydf_nl, aes(x, (predicted)))+
  geom_ribbon(data=mydf_nl, aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  ylim(c(0, quantile(kill_dt$count, 0.95)))+
  theme(legend.text=element_text(size=14))+# ggtitle("Mamba")+
  guides(color = guide_legend(override.aes = list(size = 3), title = "Year")) +
  labs(x="Month", y= "Number of observed \n feeding events")


annual_pattern



##-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

          
          #-------------------------------------------------#
          #               Figure Appendix S8 :              #  
          #       Effect of Maximum Prey Presend Index      # 
          #-------------------------------------------------#



# run model
prey.max.nb.glm <- glm.nb(count~ mth_prey_max * Clan + offset(log(n_visits)), data=kill_dt)

# with clans 
my_df_clans <- ggpredict(prey.max.nb.glm, terms = c( "mth_prey_max", "Clan"), condition= c(n_visits= 10)) %>%
  as.data.frame() %>%
  rename(mth_prey_max=x, Clan=group)

#I need to join to the other dataframe (kill_dt) to plot clans correctly
plot_clans<- right_join( kill_dt, my_df_clans, by=c("mth_prey_max", "Clan"))


## with clans 
max_prey_clan <- ggplot(plot_clans, aes(mth_prey_max, predicted, col=Clan))+ 
  geom_jitter( aes( y=log(count)), alpha= 0.1, size=1.5)+
  geom_point(size=3 , aes(col=Clan),position=position_dodge(.9))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high, col=Clan), width=.2,position=position_dodge(.9) ) +
  theme_classic(base_size=18) +
  scale_color_manual(values = c("I" ="#440154", "M"="#21908C","P"= "#E7B800"))+
  ylim(c(0, quantile(plot_clans$count, 0.95)))+ 
  
  theme(legend.text=element_text(size=14),legend.title = element_text(size=16))+ labs(color = "Clan")+
  labs( y= "Number of observed\n feeding events")+xlab(bquote(Prey[max]))
#+facet_wrap(~Clan)

max_prey_clan

