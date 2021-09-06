# load libraries ----
library(here)
library(dplyr)
library(stringr)
library(lubridate)
library(skimr)
library(ggplot2)
library(scales)
library(plotly)
library(forcats)
library(writexl)

#export processed dataset from load.R ----
if(!file.exists( here("output/wastage-processed.csv") )){
  write.csv(wastage, here("output/wastage-processed.csv") )
}

#set ggplot theme ----
theme_set(theme_minimal() + 
            theme(legend.position="none") + 
            theme(plot.margin = margin(t= 0.5, r= 0.8, b= 0.5, l=0.5, "cm"))
          )

# look over data ----
## uncomment this part if interested
# head(wastage)
# glimpse(wastage)
# skimr::skim(wastage)

# quick look at number of rows and wasted doses ----
nrow(wastage) %>% scales::comma()
sum(wastage$DOSES_SUBMITTED) %>% scales::comma()
sum(wastage$EST_WASTE_COST) %>% scales::dollar()

# vaccination waste proportion ----
# additionally, recently delivered or doses administered data from CDC vaccination dashboard as of Sept 4, 2021
doses_delivered_recent =    450175825
doses_administered_recent = 374488924

doses_wasted = sum(wastage$DOSES_SUBMITTED)
doses_delivered_estimated = doses_administered + doses_wasted

vaccination_waste_prop <- scales::percent( doses_wasted / doses_delivered_estimated, 
                 accuracy = 0.01 )
vaccination_waste_prop

# reproduce graph ----
## reproduce total number of wasted doses by week with red fill on 6/6
ggp_wastebyweek <- wastage %>% 
  group_by(WASTAGE_SUBMITTED_WEEK) %>%
  summarize(Count_Doses_Wasted = sum(DOSES_SUBMITTED)) %>% 
  mutate(FILL1 = ifelse(Count_Doses_Wasted == max(Count_Doses_Wasted), 
                        "darkred", 
                        "lightblue")) %>%  
  ggplot(aes(x=WASTAGE_SUBMITTED_WEEK, y = Count_Doses_Wasted, fill = FILL1)) + 
  geom_col() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 2000000)) + 
  scale_x_date(labels = date_format("%m-%d"),
               breaks = seq(min(wastage$WASTAGE_SUBMITTED_WEEK),
                            max(wastage$WASTAGE_SUBMITTED_WEEK),
                            by="week")) +
  xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  labs(title = "Total counts of doses wasted per week in USA", 
     caption = "Outlier highlighted red") 

ggp_wastebyweek

# define function for total waste by group ----
topn_plot <- function(group_by_var1 = AWARDEE,
                      measure = DOSES_SUBMITTED, 
                      n = 20, 
                      title_entry = ""){
  ggp_wastebyawardee <- wastage %>% 
    group_by({{group_by_var1}}) %>%
    summarize(Count_Doses_Wasted = sum({{measure}})) %>% 
    arrange(-Count_Doses_Wasted) %>%
    mutate({{group_by_var1}} := forcats::fct_reorder({{group_by_var1}}, Count_Doses_Wasted)) %>%
    mutate(FILL1 = ifelse(Count_Doses_Wasted == max(Count_Doses_Wasted),
                          "darkred",
                          "lightblue")) %>%
    top_n({{n}}, Count_Doses_Wasted) %>%
    ggplot(aes(x = {{group_by_var1}}, y = Count_Doses_Wasted, fill = FILL1)) +
    geom_col() +
    coord_flip() +
    xlab("") + ylab("") +
    labs(title= title_entry)
  
  return(ggp_wastebyawardee)
}

# Total waste by awardee ---
ggp_wastebyawardee <- topn_plot(group_by_var1 = AWARDEE,
                                measure = DOSES_SUBMITTED, 
                                n = 20, 
                                title_entry = "Counts of vaccine doses wasted by awardee (top 20)") +
  scale_y_continuous(labels = scales::comma, limits =  c(0, 2000000))
ggp_wastebyawardee

ggp_costbyawardee <- topn_plot(group_by_var1 = AWARDEE, 
                                measure = EST_WASTE_COST,
                                title_entry = "Cost of vaccine doses wasted by awardee (top 20)") +
  scale_y_continuous(labels=scales::dollar_format(), limits =  c(0, 40000000))
ggp_costbyawardee

# Total waste by manufacturer ----
ggp_wastebymanufact <- topn_plot(group_by_var1 = VAX_MANUFACTURER, 
           measure = DOSES_SUBMITTED, 
           title_entry = "Counts of vaccine doses wasted by manufacturer") +
  scale_y_continuous(labels = scales::comma, limits =  c(0, 8000000))
ggp_wastebymanufact

ggp_costbymanufact <- topn_plot(group_by_var1 = VAX_MANUFACTURER, 
                                  measure = EST_WASTE_COST, 
                                  title_entry = "Cost of vaccine doses wasted by manufacturer") +
  scale_y_continuous(labels = scales::dollar_format(), 
                     limits =  c(0, 100000000))
ggp_costbymanufact

# define fucntion to save ggplot objects ----
custom_ggsave <- function(gg_object){
  fn = here("graphics", paste0( substitute(gg_object), ".png") )
  print(fn)
  ggplot2::ggsave(plot = {{gg_object}}, filename = fn, height =4, width = 9, scale = 0.9)
}

# export ggplot objects to graphics folder ----
custom_ggsave(gg_object = ggp_wastebyweek)

custom_ggsave(gg_object = ggp_wastebyawardee)
custom_ggsave(gg_object = ggp_wastebymanufact)

custom_ggsave(gg_object = ggp_costbyawardee)
custom_ggsave(gg_object = ggp_costbymanufact)
