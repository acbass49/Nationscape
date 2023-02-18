library(haven)
library(lubridate)
library(tidyverse)
library(devtools)
library(pewmethods)
library(sjlabelled)
library(ggalt)
library(data.table)
library(formattable)

#fonts
library(showtext)
font_add_google("Cairo", family = "Cairo")
showtext_auto()

#for combining the survey
combine_nationscape <- function(new_filename, old_file){
  a <- read_sav(new_filename)
  a <- a %>%
    select(wall:statements_confront_china, religion, pid3, pid7)
  return(rbind(old_file, a))
}


list_to_iterate <- c("ns20200507.sav", 
                     "ns20200514.sav", 
                     "ns20200521.sav", 
                     "ns20200528.sav", 
                     "ns20200604.sav", 
                     "ns20200611.sav", 
                     "ns20200618.sav",
                     "ns20190725.sav",
                     "ns20190801.sav",
                     "ns20190808.sav",
                     "ns20190815.sav",
                     "ns20190822.sav",
                     "ns20190829.sav",
                     "ns20190905.sav",
                     "ns20190912.sav",
                     "ns20190919.sav",
                     "ns20190926.sav",
                     "ns20191003.sav",
                     "ns20191010.sav",
                     "ns20191017.sav",
                     "ns20191024.sav",
                     "ns20191031.sav",
                     "ns20191107.sav",
                     "ns20191114.sav",
                     "ns20191121.sav",
                     "ns20191128.sav",
                     "ns20191205.sav",
                     "ns20191212.sav",
                     "ns20191219.sav",
                     "ns20191226.sav",
                     "ns20200102.sav",
                     "ns20200109.sav",
                     "ns20200116.sav",
                     "ns20200123.sav",
                     "ns20200130.sav",
                     "ns20200206.sav",
                     "ns20200213.sav",
                     "ns20200220.sav",
                     "ns20200227.sav",
                     "ns20200305.sav",
                     "ns20200312.sav",
                     "ns20200319.sav",
                     "ns20200326.sav",
                     "ns20200402.sav",
                     "ns20200409.sav",
                     "ns20200416.sav",
                     "ns20200423.sav",
                     "ns20200430.sav",
                     "ns20200102.sav")

for (i in seq_along(list_to_iterate)){
  c <- combine_nationscape(list_to_iterate[i], c)
}


library(readxl)
variable_names <- read_excel("variable_names.xlsx", col_names = FALSE)

#Here is my list of desired variable names
variables <- unique(variable_names)
variables <- c(variables$...1, "weight", "start_date")


for (i in seq_along(list_to_iterate)){
  a_survey <- read_sav(paste(list_to_iterate[i]))
  tf <- !(variables %in% names(a_survey))
  for (x in variables[tf]){
    a_survey[[paste(x)]] <- NA
  }
  a_survey <- a_survey %>% select(variables)
  combined_survey <- bind_rows(combined_survey, a_survey)
  print('done')
}

for (i in seq_along(names(combined_survey))) {
  combined_survey[[paste(names(combined_survey)[i])]] <- replace_if(
    var = combined_survey[[paste(names(combined_survey)[i])]], 
    condition = 888, 
    replacement = NA)
}

#New Variables
#Party_Collapsed
combined_survey$party_col <- case_when(combined_survey$pid7_legacy %in% c(1,2,3) ~ "Dem/leaners",
          combined_survey$pid7_legacy == 4 ~ "Independent",
          combined_survey$pid7_legacy %in% c(5,6,7) ~ "Rep/leaners")


lds_survey <- combined_survey %>% filter(religion==3)

combined_survey <- combined_survey[, order(colnames(combined_survey))]
lds_survey <- lds_survey[, order(colnames(lds_survey))]

#creating an easier dataset for plotting
lds_agree <- c()
dem_agree <- c()
rep_agree <- c()
for (i in seq_along(names(combined_survey))[-102]) {
  lds_freq <- get_totals(paste(names(combined_survey)[i]), 
                         lds_survey, 
                         wt = "weight", 
                         na.rm = T)[1,2]
  rep_freq <- get_totals(paste(names(combined_survey)[i]), 
                         by = "party_col", combined_survey, 
                         wt = "weight", 
                         na.rm = T)[1,5]
  dem_freq <- get_totals(paste(names(combined_survey)[i]), 
                         by = "party_col", combined_survey, 
                         wt = "weight", 
                         na.rm = T)[1,3]
  dem_agree <- c(dem_agree, dem_freq)
  lds_agree <- c(lds_agree, lds_freq)
  rep_agree <- c(rep_agree, rep_freq)
}

plot_data_all <- data.frame(
  names = names(combined_survey)[-102],
  lds_agree = lds_agree,
  rep_agree = rep_agree,
  dem_agree = dem_agree
)

plot_data_all$LDSdiff <- plot_data_all$lds_agree - plot_data_all$rep_agree
plot_data_all$LDSdiffDEM <- abs(plot_data_all$lds_agree - plot_data_all$dem_agree)
plot_data_all$DEMdiff <- plot_data_all$dem_agree - plot_data_all$rep_agree
plot_data_all$diff <- NULL

plot_data_all$absDEMdiff <- abs(plot_data_all$DEMdiff)

#creating a directional agree% variable
for (i in 1:nrow(plot_data_all)){
  if (plot_data_all$dem_agree[i] > plot_data_all$rep_agree[i] &
      plot_data_all$lds_agree[i] > plot_data_all$rep_agree[i]){
    if(plot_data_all$LDSdiff[i]>0){
      plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]*-1
    }else{plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]}
  }else if(plot_data_all$dem_agree[i] > plot_data_all$rep_agree[i] &
           plot_data_all$lds_agree[i] < plot_data_all$rep_agree[i]){
    if(plot_data_all$LDSdiff[i]<0){
      plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]*-1
    }else{plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]}
  }else if(plot_data_all$dem_agree[i] < plot_data_all$rep_agree[i] &
           plot_data_all$lds_agree[i] > plot_data_all$rep_agree[i]){
    if(plot_data_all$LDSdiff[i]<0){
      plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]*-1
    }else{plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]}
  }else{
    if(plot_data_all$LDSdiff[i]>0){
      plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]*-1
    }else{plot_data_all$LDSdiffDirect[i] <- plot_data_all$LDSdiff[i]}
  }
}

#defining partisan issue as Dem agree percent - Rep agree percent > 10
part_plot_data <- plot_data_all %>% 
  filter(REPDiff>10)

part_plot_data %>% 
  arrange(LDSdiffDEM) %>% 
  select(names, lds_agree, rep_agree, dem_agree, LDSdiffDEM) %>% 
  view()

plot_data_all <- plot_data_all[!grepl("extra", plot_data_all$names),]

table(lds_survey$state)

get_totals("party_col", lds_survey, wt="weight", na.rm = T)

#pid over time - nationscape
combined_survey %>% 
  mutate(date = round_date(start_date, "month")) %>% 
  filter(religion==3) %>% 
  group_by(date) %>% 
  count(party_col, wt = weight) %>% 
  drop_na() %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = date, y = prop)) +
  geom_smooth(aes(color = party_col)) +
  geom_point(aes(color = party_col))+
  scale_color_manual(values = c("dodgerblue", "grey67", "darkred"))

#pid over time - bars
pid_over_time <- data.frame(survey = factor(c(rep("2007",3), 
                             rep("2014",3), 
                             rep("2019-20",3)), 
                           levels = c("2019-20",
                                      "2014",
                                      "2007")),
           party = rep(c("Republican", "Independent","Democrat"),3),
           percent = c(65,13,22,70,11,19,64,11,25)) %>% 
  arrange(desc(party)) %>% 
  group_by(survey) %>% 
  mutate(pos = (cumsum(percent) - percent/2)/100)

font_add_google("Cairo", family = "Cairo")
showtext_auto()

p <- ggplot(pid_over_time, aes(fill = party, x = survey, y = percent))+
  geom_bar(position = "fill", stat = "identity")+
  geom_text(size = 5, aes(x=survey, 
                          y = pos, 
                          label = paste0(percent, "%"),
                          fontface = 2,
                          family = "Cairo"),
            color = "white")+
  coord_flip()+
  scale_fill_manual(values = c("dodgerblue", "grey67", "firebrick3"))+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1.1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 2.55, size = 8, face = "italic"))+
  labs(caption = "Source: 2007 and 2014 from Pew Research Center. 2019-20 from Nationscape.",
       title = "Fewer Latter-Day Saints Are Rep. In Trump Era")

png("mtcars.png",width = 2000, height = 1000, units = "px", res = 72*4)
p
dev.off()

#LDS vs. Nation PID
pid_over_time2 <- data.frame(group = factor(c(rep("Latter-Day Saints",3), 
                                              rep("All U.S. Adults",3)), 
                                            levels = c("Latter-Day Saints",
                                                       "All U.S. Adults")),
                            party = rep(c("Republican", "Independent","Democrat"),2),
                            percent = c(64,11,25,39,13,47)) %>% 
  arrange(desc(party)) %>% 
  group_by(group) %>% 
  mutate(pos = (cumsum(percent) - percent/2)/100)

p2 <- ggplot(pid_over_time2, aes(fill = party, x = group, y = percent))+
  geom_bar(position = "fill", stat = "identity")+
  geom_text(size = 5, aes(x=group, 
                          y = pos, 
                          label = paste0(percent, "%"),
                          fontface = 2,
                          family = "Cairo"),
            color = "white")+
  coord_flip()+
  scale_fill_manual(values = c("dodgerblue", "grey67", "firebrick3"))+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1.1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 1.58, size = 8, face = "italic"))+
  labs(caption = "Source: Nationscape 2019-20.",
       title = "Majority Of Latter-Day Saints Identify/Lean Rep.")

png("VS.png",width = 2000, height = 900, units = "px", res = 72*4)
p2
dev.off()

#dumbbell chart - lean right issues

p3 <- plot_data_all %>% 
  filter(names %in% c("abortion_conditions", "marijuana")) %>% 
  pivot_longer(cols = c("lds_agree", "dem_agree", "rep_agree")) %>%
  mutate(Tnames = ifelse(names == "marijuana", "Legalize Marijuana", "Permit All Abortion")) %>% 
  ggplot()+
  geom_segment(
    data = data.frame(Tnames = c("Legalize Marijuana", "Permit All Abortion"),
                      start = c(37.8,36.5), 
                      end = c(66.1,71)),
    aes(x = start, xend = end, y=Tnames, yend = Tnames),
    size = 1.25
    ) + 
  geom_point(aes(y = Tnames, x = value, color = name),
             size = 10)+
  xlim(20,80)+
  scale_color_manual(values = c("dodgerblue", "goldenrod", "firebrick"),
                     labels = c("Democrats", "Latter-Day Saints", "Republicans"))+
  geom_text(aes(x = value, y = Tnames, label = round(value)), color = "white",
            fontface = 2, family = "Cairo")+
  geom_text(data = data.frame(y = c(rep("Permit All Abortion",3)), 
                              x = c(36.5,44.5,71),
                              label = c("LDS", "REP", "DEM")),
            aes(x=x, y=y, label = label), nudge_y = .25, fontface = 2 , family = "Cairo")+
  theme_light()+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1.1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 1.3, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(size = 8, face = "italic"),
        legend.position = "none",
        plot.subtitle = element_text(face = "italic"))+
  labs(caption = "Source: Nationscape 2019-20.",
       title = "Latter-Day Saints Strict On Abortion And Marijuana",
       subtitle = "% Who AGREE In Each Group")

png("strict.png",width = 2000, height = 1000, units = "px", res = 72*4)
p3
dev.off()

#Dumbell chart - Lean Left
p4 <- plot_data_all %>% 
  filter(names %in% c("wall", "china_tariffs", "criminal_immigration","immigration_wire", "deportation")) %>% 
  pivot_longer(cols = c("lds_agree", "dem_agree", "rep_agree")) %>%
  mutate(Tnames = factor(ifelse(names == "wall", "Build Southern Wall", ifelse(
    names == "china_tariffs", "Impose Tariffs On China", ifelse(
      names == "criminal_immigration", "Charge Border Crossers With Felony", ifelse(
        names=="immigration_wire", "Require Citizenship To Wire Money", "Deport All Illegal Immigrants"
      )
    )
  )), levels = c("Deport All Illegal Immigrants","Require Citizenship To Wire Money",
                 "Charge Border Crossers With Felony","Impose Tariffs On China",
                 "Build Southern Wall")))%>% 
  group_by(Tnames) %>% 
  arrange(value, by_group = T) %>% 
  {
  ggplot(.)+
  geom_segment(
    data = dplyr::filter(., name %in% c("dem_agree", "rep_agree")) %>% 
      pivot_wider(
        names_from = name,
        values_from = value
      ),
    aes(x = dem_agree, xend = rep_agree, y=Tnames, yend = Tnames),
    size = 1.25
  ) + 
  geom_point(aes(y = Tnames, x = value, color = name),
             size = 8)+
  xlim(0,80)+
  scale_color_manual(values = c("dodgerblue", "goldenrod", "firebrick"),
                     labels = c("Democrats", "Latter-Day Saints", "Republicans"))+
  geom_text(aes(x = value, y = Tnames, label = round(value)), color = "white",
            fontface = 2, family = "Cairo")+
  geom_text(data = dplyr::filter(., names == "wall") %>% 
              mutate(name = gsub("_AGREE", "",toupper(name))),
            aes(x=value, y=Tnames, label = name), nudge_y = .425, fontface = 2, family = "Cairo",
            size = 3)+
  theme_light()+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12, hjust = 1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 1.3, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(size = 8, face = "italic"),
        legend.position = "none",
        plot.subtitle = element_text(face = "italic"))+
  labs(caption = "Source: Nationscape 2019-20.",
       title = "Latter-Day Saints Leave Rep On Key Trump Issues",
       subtitle = "% Who AGREE In Each Group")}

png("dem_issues.png",width = 2000, height = 1000, units = "px", res = 72*4)
p4
dev.off()

#Dumbbell chart - Impeach Trump
p5 <- plot_data_all %>% 
  filter(names == "impeach_trump") %>% 
  pivot_longer(cols = c("lds_agree", "dem_agree", "rep_agree")) %>%
  mutate(Tnames = ifelse(names == "impeach_trump", "Impeach Trump", NA))%>% 
  {
    ggplot(.)+
      geom_segment(
        data = dplyr::filter(., name %in% c("dem_agree", "rep_agree")) %>% 
          pivot_wider(
            names_from = name,
            values_from = value
          ),
        aes(x = dem_agree, xend = rep_agree, y=Tnames, yend = Tnames),
        size = 1.25
      ) + 
      geom_point(aes(y = Tnames, x = value, color = name),
                 size = 10)+
      xlim(0,80)+
      scale_color_manual(values = c("dodgerblue", "goldenrod", "firebrick"),
                         labels = c("Democrats", "Latter-Day Saints", "Republicans"))+
      geom_text(aes(x = value, y = Tnames, label = round(value)), color = "white",
                fontface = 2, family = "Cairo")+
      geom_text(data = dplyr::filter(., names == "impeach_trump") %>% 
                  mutate(name = gsub("_AGREE", "",toupper(name))),
                aes(x=value, y=Tnames, label = name), nudge_y = .275, fontface = 2, family = "Cairo",
                )+
      theme_light()+
      theme(axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(size = 16, hjust = 1),
            panel.background = element_blank(),
            text = element_text(face = "bold", family = "Cairo"),
            plot.title = element_text(hjust = 1.5, size = 20, face = "bold"),
            legend.title = element_blank(),
            plot.caption = element_text(size = 8, face = "italic"),
            legend.position = "none",
            plot.subtitle = element_text(face = "italic"))+
      labs(caption = "Source: Nationscape 2019-20.",
           title = "1 in 4 Latter-day Saints Agreed To Impeach Trump",
           subtitle = "% Who AGREE")}

png("impeach_trump.png",width = 2000, height = 600, units = "px", res = 72*4)
p5
dev.off()

#Making a table
#Look at formattable and reactable
#probably use formattable for my purposes.

summaries <- c("Build Southern Wall",
               "Impose Tariffs On China",
               "Charge Border Crossers With Felony",
               "Require Citizenship To Wire Money",
               "Deport All Illegal Immigrants",
               "Ban Immigration From Muslim Nations",
               "Shift From Family To Merit-Based Immigration System",
               "Eliminate Estate Tax",
               "Impeach Trump",
               "Separate Children And Parents After Illegal Entry",
               "Limit Trade With Other Countries",
               "America Is Carrying Too Much Defense Burden",
               "I Favor A Larger Government",
               "Politicians Often Favor Foreigners' Interests",
               "Group Favorability - Evangelicals",
               "Allow 10 Commandments Display Schools/Courts",
               "Create A Path To Citizenship For Illegal Immig.",
               "Group Favorability - White Men",
               "Grant Reparations Payments To Slave Descendants",
               "Politicians Should Do More To Protect US Culture",
               "Remove Barriers To Domestic Oil/Gas Drilling",
               "Delete",
               "Enact Medicare-For-All",
               "Enact A Green New Deal",
               "Create Path to Citizenship For Dreamers",
               "Delete",
               "Allow Employers To Decline Abortion Insurance",
               "Replace Private Insurance With Government's",
               "Provide Gov. Insurance To Everyone",
               "Provide Gov. Insurance To Illegal Immigrants",
               "Students Graduate State College Debt-Free",
               "Reduce US Military Size",
               "Raise Minimun Wage To $15/hour",
               "Allow Transgender People To Join Military ",
               "Gov. Should Promote Traditional Family Values",
               "Never Permit Abortion",
               "Ban All Guns",
               "Require Waiting And Ultrasound Before Abortion",
               "Delete",
               "Raise Taxes On Families Making >$250,000",
               "Provide Vouchers For Kids in Relig./Priv. Schools",
               "Subsidize health insurace for uncovered low-inc.",
               "Cap Carbon Emissions For Climate Change",
               "Require Companies Provide 12wks Paid Maternity",
               "Ban Assault Rifles",
               "Provide All An Option To Buy Gov. Health Insurance",
               "Withdraw Military Support From Israel",
               "Permit Late Term Abortion",
               "Delete",
               "Make Large Investment In Tech To Save Environment",
               "Raise Taxes On Families Making >$600,000",
               "Create A Public Gov. Registry Of Gun Ownership",
               "Garuntee Jobs For All Americans",
               "Limit Gun Magazine To 10 Bullets",
               "Permit Abortion At Any Time During Pregnancy",
               "Permit All Abortion In All Cases",
               "Legalize Marijauna"
)

ppdata <- part_plot_data %>% 
  arrange(LDSdiffDirect) %>% 
  select(names,dem_agree,rep_agree,lds_agree,LDSdiffDirect)

ppdata <- cbind(ppdata, summaries)

ppdata <- ppdata[-c(22, 26, 39, 49),]

ppdata <- ppdata %>% 
  select(summaries,dem_agree,rep_agree,lds_agree,LDSdiffDirect)

ppdata$lds_agree <- paste0(round(ppdata$lds_agree),"%")
ppdata$rep_agree <- paste0(round(ppdata$rep_agree),"%")
ppdata$dem_agree <- paste0(round(ppdata$dem_agree),"%")

names(ppdata) <- c("Policy","DEM Agree", "REP Agree", "LDS Agree", "LDS Deviation from REP")

ppdata$`LDS Deviation from REP` <- ifelse(ppdata$`LDS Deviation from REP`>0, 
                                          paste0(round(ppdata$`LDS Deviation from REP`,1),"% REP Lean"),
                            paste0(round(ppdata$`LDS Deviation from REP`*-1,1), "% DEM Lean"))

ppdata <- as_tibble(ppdata)

improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(grepl("DEM", x), "dodgerblue", "crimson")))


formattable(ppdata,
            align = c("l","c", "c","c","r"),
            list(Policy = formatter("span", 
                                   style = ~ style(color = "grey", 
                                                   font.weight = "bold")),
                 `LDS Deviation from REP` = improvement_formatter))
