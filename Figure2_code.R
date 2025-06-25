# Mellina Sidous
# 08/02/2025 

          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
          #             Figure 2             #
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#



###################### Load packages
library(gam)
library(tidyverse)
library(ggplot2)
library(MASS)
library(effects)
library(ggeffects)
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
load("data/Clan.size.RData")


###################### Arrange data 
Clan_size_month <- Clan_size_month  %>% rename (Clan = clan)
kill_and_clan <- right_join(kill_dt, Clan_size_month, by=c("Year", "Month", "Clan")) %>% subset(!is.na(count))


#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

          
          
          #-------------------------------------------------#
          #                    Figure 1 :                   #  
          #                    Clan size                    # 
          #-------------------------------------------------#



clan_size <- kill_and_clan %>% ggplot(aes(x=Year, y=med_all, group=Clan))+
  geom_jitter( alpha=0.4, aes(color=Clan))+
  geom_smooth(aes(color=Clan, fill=Clan), level=0.95)+
  scale_color_manual(values = c("I" ="#440154", "M"="#21908C","P"= "#E7B800"))+
  scale_fill_manual(values = c("I" ="#440154", "M"="#21908C","P"= "#E7B800"))+
  theme_classic(base_size=20) +
  theme(strip.text.x = element_text(size = 20))+
  labs(y = "Clan size")

clan_size



#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

          
          
          
          #-------------------------------------------------#
          #                    Figure 2 :                   #  
          #       Annual patterns of the different          # 
          #                      Clan                       #
          #-------------------------------------------------#
          

# Clan and Month interacting 
#-------------------------

############### run model non linear
clan.smonth.syear.nb.gamI <- gam(count~ s(Month)* Clan + offset(log(n_visits)),family=neg.bin(1) ,  data=kill_dt)


#predictions
my_df_clans <- ggpredict(clan.smonth.syear.nb.gamI, terms = c("Clan",  "Month[1: 12]"), back.transform = TRUE,condition= c(n_visits= 10))%>% 
  as.data.frame() %>% 
  mutate(group=as.numeric(as.character(group))) %>%
  rename(Month=group) %>% rename(Clan=x)
  
#I need to join to the other dataframe (kill_dt) to plot clans correctly
plot_clans<- right_join( kill_dt, my_df_clans, by=c("Month", "Clan"))


## with clans 
clan_month <- ggplot(plot_clans, aes(Month, predicted, col=Clan))+ 
  #geom_jitter( aes( y=log(count)), alpha= 0.1, size=1.5)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, col=Clan, fill=Clan), alpha = 0.2, linetype=0)+
  geom_line(aes(color=Clan), size=1.2)+
  theme_classic(base_size=20) +
  theme(legend.text=element_text(size=16),legend.title = element_text(size=18))+
  guides(color = guide_legend(override.aes = list(size = 3))) +
  scale_color_manual(values = c("I" ="#440154", "M"="#21908C","P"= "#E7B800"))+
  scale_fill_manual(values = c("I" ="#440154", "M"="#21908C","P"= "#E7B800"))+
  scale_x_continuous(breaks=seq(1, 12, 1))+
  labs(x="Month", y= "Number of observed\nfeeding events") 

clan_month



#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

    
          
          
          #-------------------------------------------------#
          #                    Figure 3 :                   #  
          #     Estimates over year using both nonlinear    # 
          #          and categorical effects of Year        #
          #-------------------------------------------------#
          


# Cateorical effect of Year
#-------------------------
ismonth.cyear.nb.gamI <- gam(count~ s(Month)+factor(Year) + offset(log(n_visits)),family=neg.bin(1) ,  data=kill_dt)
#prediction
Year_cat <- ggpredict(ismonth.cyear.nb.gamI, terms = c("Year[1990:2019]"), back.transform = TRUE, condition= c(n_visits= 10))


# Non linear effect of Year
#-------------------------
ismonth.syear.nb.gamI <- gam(count~ s(Month)+ s(Year) + offset(log(n_visits)),family=neg.bin(1) ,  data=kill_dt)
#prediction
Year_nl <- ggpredict(ismonth.syear.nb.gamI, terms = c("Year[1990:2019]"), back.transform = TRUE, condition= c(n_visits= 10))

# Figure
#get both df with same scale
Year_cat$x<-as.numeric(as.character(Year_cat$x))


patterns_over_year <-ggplot(Year_cat, aes(x, predicted))+ 
  geom_jitter(data=kill_dt, aes(x=Year, y=count, color=Month), alpha= 0.6, size=1.5)+
  scale_color_gradient( low = "#e5f61b",high = "#3bcfd4", space = "Lab", guide = "colourbar" )+
  geom_point(size=3, color="#2f3e46" )+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="#2f3e46" ) +
  #nl
  geom_line(data=Year_nl, aes(x=x, y=predicted), color="#2f3e46", size=1)+
  geom_ribbon(data=Year_nl, aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  theme_classic(base_size=20) +
  ylim(c(0, quantile(kill_dt$count, 0.95)))+
  theme(legend.text=element_text(size=14))+ 
  #guides(color = guide_legend(override.aes = list(size = 3))) +
  labs(x="Year", y= "Number of observed \n feeding events")#+

patterns_over_year




#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

        
        #-------------------------------------------------#
        #                    Figure 4 :                   #  
        #        Changes in annual patterns over          # 
        #                     the years                   #
        #-------------------------------------------------#

Years_to_plot <- c("1990","1992","1995", "1998","2001", "2004", "2007","2010", "2013","2016", "2019")

# Non linear effect of Year
#-------------------------
ismonth.syear.nb.gamI <- gam(count~ s(Month)* s(Year) + offset(log(n_visits)),family=neg.bin(1) ,  data=kill_dt)

#produce estimates
my_df2 <- ggpredict(ismonth.syear.nb.gamI, terms = c( "Month", "Year[1989:2019]"), back.transform = TRUE, condition= c(n_visits= 10)) %>% 
  as.data.frame() %>% 
  filter(group  %in% Years_to_plot)%>% 
  rename("Year"="group")


changes_pattern <- ggplot(my_df2, aes(x, predicted, colour=Year))+ 
   scale_color_manual(values= c( "#F4D085","#FFCA38", "#FF930F", "#FB613C", "#FF0800", "#E32951","#D53A7B", "#C256B0", "#AF70E2", "#5E59AB", "#074170"))+
  scale_fill_manual(values= c( "#F4D085","#FFCA38", "#FF930F", "#FB613C", "#FF0800", "#E32951","#D53A7B", "#C256B0", "#AF70E2", "#5E59AB", "#074170"))+
  geom_jitter(data=(kill_dt  %>%  filter(Year  %in% Years_to_plot)), aes(x=Month, y=count, color=factor(Year)), alpha= 0.6, size=1.5)+
  #scale_color_gradient2(midpoint = median(kill_dt$Year), low = "#F4E784", mid = "#F24389",
  #                      high = "#A478F1", space = "Lab", guide = "colourbar" )+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = Year),alpha=0.2, linetype=0, size=0.01)+
  geom_line( size=0.8)+
  ylim(c(0, quantile(kill_dt$count, 0.95)))+
  theme_classic(base_size=20) +
  scale_x_continuous(breaks=seq(1, 12, 1))+
  theme(legend.text=element_text(size=12),legend.title = element_text(size=18))+ labs(color = "Year")+
  labs(x="Month", y= "Number of observed \n feeding events") 



changes_pattern


#######################################################################################
######################################################################################"



  
  ggarrange(clan_month, changes_pattern,patterns_over_year, clan_size, nrow = 2, ncol = 2,common.legend=FALSE, 
            widths = c(1, 1,1, 1), legend = "right",labels=c('A', 'B', 'C','D'),
            font.label = list(size = 21, color = "black"))
  
  

