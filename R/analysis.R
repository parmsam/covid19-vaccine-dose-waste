# load libraries ----
library(dplyr)
library(here)
library(stringr)
library(lubridate)
library(skimr)
library(ggplot2)
library(scales)
library(plotly)
library(forcats)
library(writexl)
library(glue)
library(treemapify)
#export processed dataset from load.R ----
if(!file.exists( here("output/wastage-processed.csv") )){
  write.csv(wastage, here("output/wastage-processed.csv") )
}

#set ggplot theme ----
theme_set(theme_minimal() + 
            theme(legend.position="none") + 
            theme(plot.margin = margin(t= 0.5, r= 0.8, b= 0.5, l=0.5, "cm")) +
            theme(plot.caption = element_text(size= 6, hjust = 0.5))
          )

# look over data ----
## uncomment this part if interested
# head(wastage)
# glimpse(wastage)
# skimr::skim(wastage)

# quick look at number of rows and wasted doses ----
nrow(wastage) %>% scales::comma()
(total_wasted_doses <- sum(wastage$DOSES_SUBMITTED) %>% scales::comma())
(sum_dollars_wasted <- sum(wastage$EST_WASTE_COST) %>% scales::dollar())

# vaccination waste proportion ----
# additionally, recently delivered or doses administered data from CDC vaccination dashboard as of Sept 4, 2021
doses_delivered_recent =    450175825
doses_administered_recent = 374488924

recent_dose_administered <- 374488924 %>% scales::comma()

doses_wasted = sum(wastage$DOSES_SUBMITTED)
doses_delivered_estimated = doses_administered_recent + doses_wasted

vaccination_waste_prop <- scales::percent( doses_wasted / doses_delivered_estimated, 
                 accuracy = 0.01 )
vaccination_waste_prop

min_wastage_submitted_date <- min(wastage$WASTAGE_SUBMITTED_DATE)
max_wastage_submitted_date <- max(wastage$WASTAGE_SUBMITTED_DATE)

caption_text = glue::glue("Max week highlighted red with dates representing waste submission week. 
                          {total_wasted_doses} total doses wasted at estimated cost of {sum_dollars_wasted} from {min_wastage_submitted_date} to {max_wastage_submitted_date}.
                          Estimated {recent_dose_administered} total doses administered as of September 4, 2021, based on CDC COVID data tracker.")
caption_text_alt = glue::glue("Max record group highlighted red if viewing bar chart. 
                              {total_wasted_doses} total doses wasted at estimated cost of {sum_dollars_wasted} from {min_wastage_submitted_date} to {max_wastage_submitted_date}.
                              Estimated {recent_dose_administered} total doses administered as of September 4, 2021, based on CDC COVID data tracker.")
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
  labs(title = "Reported counts of doses wasted per week in USA") +
  labs(caption = caption_text)
ggp_wastebyweek

facet_wrap_bar_custom <- function(group1 = WASTAGE_SUBMITTED_WEEK, 
                                  facet_group = VAX_MANUFACTURER, 
                                  topn1 = 5,
                                  measure1 = DOSES_SUBMITTED){
  topn_grps <- wastage %>% 
    group_by({{facet_group}}) %>%
    summarize(Count_Doses_Wasted = sum({{measure1}})) %>% 
    top_n(n={{topn1}}, wt = Count_Doses_Wasted) %>% select(-Count_Doses_Wasted) %>%
    unique()
  print(topn_grps %>% colnames())
  ggp <- wastage %>% 
    semi_join(topn_grps) %>%
    group_by( {{group1}}, {{facet_group}} ) %>%
    summarize(Count_Doses_Wasted = sum( {{measure1}} )) %>% 
    group_by( {{facet_group}} ) %>%
    mutate(FILL1 = ifelse(Count_Doses_Wasted == max(Count_Doses_Wasted), 
                          "darkred", 
                          "lightblue")) %>%  
    ungroup() %>%
    ggplot(aes(x={{group1}}, y = Count_Doses_Wasted, fill = FILL1,
               group = {{facet_group}})) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(labels = date_format("%m-%d"),
                 breaks = seq(min(wastage$WASTAGE_SUBMITTED_WEEK),
                              max(wastage$WASTAGE_SUBMITTED_WEEK),
                              by="week")) +
    xlab("") + ylab("") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(title = "Reported counts of doses wasted per week in USA") +
    labs(caption = caption_text) +
    facet_wrap(vars({{facet_group}}), nrow = 3, scales = "free_y")
  return(ggp)
}

ggp_wastebyweekandmanufac <- facet_wrap_bar_custom(
  group1 = WASTAGE_SUBMITTED_WEEK, 
  facet_group = VAX_MANUFACTURER, 
  measure1 = DOSES_SUBMITTED
)
ggp_wastebyweekandmanufac

ggp_wastebyweekandawardee <- facet_wrap_bar_custom(
  group1 = WASTAGE_SUBMITTED_WEEK, 
  facet_group = AWARDEE, 
  measure1 = DOSES_SUBMITTED) +  
  labs(title="Reported counts of doses wasted per week in USA (top 5 awardees)") 
ggp_wastebyweekandawardee

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
  labs(title = "Reported counts of doses wasted per week in USA by Vaccine Manufacturer") +
  labs(caption = caption_text_alt) +
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

alt_treemap_plot <- function(group_by_var1 = AWARDEE,
                      measure = DOSES_SUBMITTED, 
                      n = 20, 
                      title_entry = ""){
  ggp_wastebyawardee <- wastage %>% 
    group_by({{group_by_var1}}) %>%
    summarize(Count_Doses_Wasted = sum({{measure}})) %>% 
    arrange(-Count_Doses_Wasted) %>%
    mutate({{group_by_var1}} := forcats::fct_reorder({{group_by_var1}}, Count_Doses_Wasted)) %>%
    ggplot(aes(area = Count_Doses_Wasted, fill = Count_Doses_Wasted, 
               label = paste({{group_by_var1}}, scales::comma(Count_Doses_Wasted, accuracy = 1), sep = "\n" ))
           ) +
    geom_treemap(start = "topleft") +
    geom_treemap_text(start = "topleft") +
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
                                title_entry = "Reported counts of vaccine doses wasted by awardee (top 20)") +
  scale_y_continuous(labels = scales::comma, limits =  c(0, 2000000))
ggp_wastebyawardee

ggp_costbyawardee <- topn_plot(group_by_var1 = AWARDEE, 
                                measure = EST_WASTE_COST,
                                title_entry = "Estimated cost of vaccine doses wasted by awardee (top 20)") +
  scale_y_continuous(labels=scales::dollar_format(), limits =  c(0, 40000000))
ggp_costbyawardee

tm_wastebyawardee <- alt_treemap_plot(group_by_var1 = AWARDEE,
                 measure = DOSES_SUBMITTED, 
                 title_entry = "Treemap of reported counts of vaccine doses wasted by awardee") +
  labs(caption = caption_text_alt)
tm_wastebyawardee
# Total waste by manufacturer ----
ggp_wastebymanufact <- topn_plot(group_by_var1 = VAX_MANUFACTURER, 
           measure = DOSES_SUBMITTED, 
           title_entry = "Reported counts of vaccine doses wasted by vaccine manufacturer") +
  scale_y_continuous(labels = scales::comma, limits =  c(0, 8000000)) +
  labs(caption = caption_text_alt)
ggp_wastebymanufact

ggp_costbymanufact <- topn_plot(group_by_var1 = VAX_MANUFACTURER, 
                                  measure = EST_WASTE_COST, 
                                  title_entry = "Estimated cost of vaccine doses wasted by vaccine manufacturer") +
  scale_y_continuous(labels = scales::dollar_format(), 
                     limits =  c(0, 100000000)) +
  labs(caption = caption_text_alt)
ggp_costbymanufact

# Total waste by awardee and manufacturer---
ggp_wastebyawardee_manufac <- topn_facet_plot(group_by_var1 = AWARDEE,
                facet_group1 = VAX_MANUFACTURER,
                n1 = 8, 
                title_entry = "Reported counts of vaccine doses wasted by awardee and manufacturer (top 8)")

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

custom_ggsave_alt2 <- function(gg_object){
  fn = here("graphics", paste0( substitute(gg_object), ".png") )
  print(fn)
  ggplot2::ggsave(plot = {{gg_object}}, filename = fn, height =7, width = 12, scale = 0.9)
}

# export ggplot objects to graphics folder ----
custom_ggsave(gg_object = ggp_wastebyweek)
custom_ggsave_alt(gg_object = ggp_wastebyweekandmanufac)
custom_ggsave_alt(gg_object = ggp_wastebyawardee_manufac)
custom_ggsave_alt2(gg_object = ggp_wastebyweekandawardee)
custom_ggsave(gg_object = ggp_spaghetti_plot)

custom_ggsave(gg_object = ggp_wastebyawardee)
custom_ggsave_alt(gg_object = tm_wastebyawardee)
custom_ggsave(gg_object = ggp_wastebymanufact)

custom_ggsave(gg_object = ggp_costbyawardee)
custom_ggsave(gg_object = ggp_costbymanufact)
