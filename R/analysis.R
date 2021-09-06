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
     caption = "Max week highlighted red with dates representing waste submission week") 

ggp_wastebyweek

ggp_wastebyweekandmanufac <- wastage %>% 
  group_by(WASTAGE_SUBMITTED_WEEK, VAX_MANUFACTURER) %>%
  summarize(Count_Doses_Wasted = sum(DOSES_SUBMITTED)) %>% 
  group_by(VAX_MANUFACTURER) %>%
  mutate(FILL1 = ifelse(Count_Doses_Wasted == max(Count_Doses_Wasted), 
                        "darkred", 
                        "lightblue")) %>%  
  ungroup() %>%
  ggplot(aes(x=WASTAGE_SUBMITTED_WEEK, y = Count_Doses_Wasted, fill = FILL1, 
             group = VAX_MANUFACTURER)) + 
  geom_col() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(labels = date_format("%m-%d"),
               breaks = seq(min(wastage$WASTAGE_SUBMITTED_WEEK),
                            max(wastage$WASTAGE_SUBMITTED_WEEK),
                            by="week")) +
  xlab("") + ylab("") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  labs(title = "Total counts of doses wasted per week in USA", 
       caption = "Max week highlighted red with dates representing waste submission week") +
  facet_wrap(~VAX_MANUFACTURER, nrow = 3, scales = "free_y")

ggp_wastebyweekandmanufac

ggp_spaghetti_plot <- wastage %>% 
  group_by(WASTAGE_SUBMITTED_WEEK, VAX_MANUFACTURER) %>%
  summarize(Count_Doses_Wasted = sum(DOSES_SUBMITTED)) %>% 
  group_by(VAX_MANUFACTURER) %>%
  ungroup() %>%
  ggplot(aes(x=WASTAGE_SUBMITTED_WEEK, y = Count_Doses_Wasted)) + 
  geom_line(aes(group = VAX_MANUFACTURER, color = VAX_MANUFACTURER)) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(labels = date_format("%m-%d"),
               breaks = seq(min(wastage$WASTAGE_SUBMITTED_WEEK),
                            max(wastage$WASTAGE_SUBMITTED_WEEK),
                            by="week")) +
  xlab("") + ylab("") + 
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + 
  labs(title = "Total counts of doses wasted per week in USA by Vaccine Manufacturer", 
       caption = "Max week highlighted red with dates representing waste submission week") + 
  geom_smooth(colour="black", size = 0.25, se = FALSE)
ggp_spaghetti_plot

# define plot functions ----
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

topn_facet_plot <- function(group_by_var1 = AWARDEE,
                            facet_group1 = VAX_MANUFACTURER,
                            measure = DOSES_SUBMITTED, 
                            n1 = 10, 
                            nrow1 = 3,
                            title_entry = ""){
  ggp_wastebyawardee2 <- wastage %>% 
    group_by({{group_by_var1}}, {{facet_group1}}) %>%
    summarize(Count_Doses_Wasted = sum({{measure}})) %>% 
    arrange(-Count_Doses_Wasted) %>%
    group_by({{facet_group1}}) %>%
    slice_max(Count_Doses_Wasted, n = {{n1}}) %>%
    ungroup() %>%
    group_by({{facet_group1}}) %>%
    mutate({{group_by_var1}} := forcats::fct_reorder2({{group_by_var1}}, 
                                                      Count_Doses_Wasted, 
                                                      {{facet_group1}})) %>%
    group_by({{facet_group1}}) %>%
    mutate(FILL1 = ifelse(Count_Doses_Wasted == max(Count_Doses_Wasted),
                          "darkred",
                          "lightblue")) %>%
    ggplot(aes(x = {{group_by_var1}}, y = Count_Doses_Wasted, fill = FILL1,
               group = {{facet_group1}}) ) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    coord_flip() +
    xlab("") + ylab("") +
    labs(title= title_entry) +
    facet_wrap(vars({{facet_group1}}), nrow = {{nrow1}}, scales="free_y")
  return(ggp_wastebyawardee2)
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
                                title_entry = "Estimated cost of vaccine doses wasted by awardee (top 20)") +
  scale_y_continuous(labels=scales::dollar_format(), limits =  c(0, 40000000))
ggp_costbyawardee

# Total waste by manufacturer ----
ggp_wastebymanufact <- topn_plot(group_by_var1 = VAX_MANUFACTURER, 
           measure = DOSES_SUBMITTED, 
           title_entry = "Counts of vaccine doses wasted by vaccine manufacturer") +
  scale_y_continuous(labels = scales::comma, limits =  c(0, 8000000))
ggp_wastebymanufact

ggp_costbymanufact <- topn_plot(group_by_var1 = VAX_MANUFACTURER, 
                                  measure = EST_WASTE_COST, 
                                  title_entry = "Estimated cost of vaccine doses wasted by vaccine manufacturer") +
  scale_y_continuous(labels = scales::dollar_format(), 
                     limits =  c(0, 100000000))
ggp_costbymanufact

# Total waste by awardee and manufacturer---
ggp_wastebyawardee_manufac <- topn_facet_plot(group_by_var1 = AWARDEE,
                facet_group1 = VAX_MANUFACTURER,
                n1 = 8, 
                title_entry = "Counts of vaccine doses wasted by awardee and manufacturer (top 8)")

# define fucntion to save ggplot objects ----
custom_ggsave <- function(gg_object){
  fn = here("graphics", paste0( substitute(gg_object), ".png") )
  print(fn)
  ggplot2::ggsave(plot = {{gg_object}}, filename = fn, height =4, width = 9, scale = 0.9)
}

custom_ggsave_alt <- function(gg_object){
  fn = here("graphics", paste0( substitute(gg_object), ".png") )
  print(fn)
  ggplot2::ggsave(plot = {{gg_object}}, filename = fn, height =7, width = 9, scale = 0.9)
}

# export ggplot objects to graphics folder ----
custom_ggsave(gg_object = ggp_wastebyweek)
custom_ggsave_alt(gg_object = ggp_wastebyweekandmanufac)
custom_ggsave_alt(gg_object = ggp_wastebyawardee_manufac)
custom_ggsave(gg_object = ggp_spaghetti_plot)

custom_ggsave(gg_object = ggp_wastebyawardee)
custom_ggsave(gg_object = ggp_wastebymanufact)

custom_ggsave(gg_object = ggp_costbyawardee)
custom_ggsave(gg_object = ggp_costbymanufact)
