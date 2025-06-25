# Mellina Sidous
# 08/02/2025
              
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
              #             Figure S8            #
              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
              


###################### Load packages
library(gam)
library(tidyverse)
library(ggplot2)
library(MASS)
library(effects)
library(ggeffects)
library(ggpubr)


###################### Load data
load("Appendix_S8/Prey_data.RData")
load("Appendix_S8/dat_nfeeding.RData")

prey_presence <- kill_dt %>% dplyr::select(Month, Year,Clan, mth_prey_max, mth_prey_mode) %>% 
  group_by(Month, Year, Clan) %>%
  slice(1)%>%
  ungroup()

kills <- left_join(kill, prey_presence, by = c("Year", "Month", "Clan")) 
kill <- kills[-which(is.na(kills$mth_prey_max)),]


#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------
          
          #-------------------------------------------------#
          #                    Figure 1 :                   #  
          #     Annual estimates from both categorical      # 
          #          and non linear effect of Months        #
          #-------------------------------------------------#

Years_to_plot <- c("1990","1992","1995", "1998","2001", "2004", "2007","2010", "2013","2016", "2019")


# A) Categorical effect of Month
#------------------------
# run model
prey_cmonth<- glm(migratory~ factor(Month), family=binomial,data=kill)
# for year
Month_effect_cat <- ggpredict(prey_cmonth, terms = c("Month"), back.transform = TRUE)

# B) Non linear effect of Month
#------------------------
# run model
prey_month<-gam(migratory~ s(Month), family=binomial,data=kill)
# for year
Month_effect_nl <- ggpredict(prey_month, terms = c("Month"), back.transform = TRUE)

#Plot
annual_pattern <-ggplot(Month_effect_cat, aes(x, predicted))+ 
  theme_classic(base_size=20) +
  geom_jitter(data=(kill%>% subset(Year %in% Years_to_plot)), aes(x=factor(Month), y=migratory, color=factor(Year)), alpha= 0.5, size=1.5, width = 0.2, height = 0.05)+
  geom_point(size=3, color="#2f3e46" )+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,
                position=position_dodge(.9), color="#2f3e46" ) +
  geom_ribbon(data=Month_effect_nl , aes(ymin = conf.low, ymax = conf.high), alpha = 0.2)+
  geom_line(data=Month_effect_nl ,color="#2f3e46")+
  scale_color_manual(values= c( "#F4D085","#FFCA38", "#FF930F", "#FB613C", "#FF0800", "#E32951","#D53A7B", "#C256B0", "#AF70E2", "#5E59AB", "#074170"))+
  theme(legend.text=element_text(size=14))+ 
  guides(color = guide_legend(override.aes = list(size = 3), title="Year")) +
  labs(x="Month", y= "Proportion of migratory prey")

annual_pattern




#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------

            
            #-------------------------------------------------#
            #                    Figure 2 :                   #  
            #     Estimates over year using both nonlinear    # 
            #          and categorical effects of Year        #
            #-------------------------------------------------#



# run model
prey_smonth_yeari <- gam(migratory~ s(Month) * factor(Year), family=binomial,data=kill)

#prediction
Year_cat <- ggpredict(prey_smonth_yeari, terms = c("Year[1990:2019]"), back.transform = TRUE, condition= c(n_visits= 10)) 
Year_cat$x<-as.numeric(as.character(Year_cat$x))

# Plot
patterns_over_year <-ggplot(Year_cat, aes(x, predicted))+ 
  geom_smooth(color="black", method="loess")+
  geom_jitter(data=kill, aes(x=Year, y=migratory, color=Month), alpha= 0.6, size=1.5, width = 0.2, height = 0.05)+
  scale_color_gradient( low = "#e5f61b",high = "#3bcfd4", space = "Lab", guide = "colourbar" )+
  geom_point(size=3, color="#2f3e46" )+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), width=.2,position=position_dodge(.9), color="#2f3e46" ) +
  theme_classic(base_size=20) +
  theme(legend.text=element_text(size=14))+ 
  labs(x="Year", y= "Proportion of migratory prey")

patterns_over_year



#-----------------------------------------------------------------------------------------------------
######################################################################################################
#-----------------------------------------------------------------------------------------------------


plot_grid(annual_pattern,patterns_over_year, labels = c('A','B'), label_size = 22, ncol=2)

ggarrange(annual_pattern, patterns_over_year, nrow = 1, 
          widths = c(1, 1,1), legend = "right",labels=c('A', 'B'),
          font.label = list(size = 21, color = "black"))

