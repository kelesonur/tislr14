# Libraries
library(tidyverse) 
library(ggstatsplot)
library(brms)
library(bayesplot)
library(extrafont)
extrafont::font_import()
library(gridExtra)


# Data
df = read.csv("TISLR_Data.csv",sep=";")

# Recoding
df$ref_type1 <- with(df, case_when(
  RE %in% c("Bare noun", "Point noun", "Noun point",
            "Fingerspelled noun", "CL noun", "Noun CL") ~ "NOM",
  RE %in% c("Constructed action", "Plain verb", "Person agreement verb") ~ "NULL",
  RE %in%  c("Whole entity CL", "Body part CL",
             "SASS - Tracing", "SASS - Static") ~ "CL",
  T ~ "PRO"))

df$ref_type2 <- ifelse(df$RE %in% c("Bare noun", "Point noun", "Noun point","Fingerspelled noun", "CL noun", "Noun CL"), "NOM",
                       ifelse(df$RE %in% c("Constructed action"), "CA",
                              ifelse(df$RE %in% c("Plain verb","Person agreement verb"), "VERB",
                                     ifelse(df$RE %in% c("Whole entity CL"), "WCL",
                                            ifelse(df$RE %in% c("Body part CL"), "BPCL",
                                                   ifelse(df$RE %in% c("SASS - Tracing", "SASS - Static"), "SASS", "PRO"))))))

df$ref_type3 <- with(df, case_when(
  RE %in% c("Bare noun") ~ "BARE NOM",
  RE %in% c("Point noun", "Noun point","CL noun", "Noun CL") ~ "M NOM",
  RE %in% c("Fingerspelled Noun") ~ "FS NOM",
  RE %in% c("Constructed action") ~ "CA",
  RE %in% c("Plain verb") ~ "PLAIN",
  RE %in% c("Person agreement verb") ~ "AGR",
  RE %in% c("Whole entity CL") ~ "WECL",
  RE %in%  c("Body part CL")  ~ "BPCL",
  RE %in%  c("SASS - Tracing", "SASS - Static")  ~ "SASS",
  T ~ "PRO")) 

df$Nativeness <- df$Nativeness %>% recode('1' = "Native", '0' = "Late")
df$IS_Score_Auto %<>% as.numeric() 

df_native = df %>% subset(Nativeness == "Native") 

df_late = df %>% subset(Nativeness == "Late") 

# Plots

# Native Signers

df_analysis_native = df_native %>% select(Subject, Nativeness, Discourse, ref_type2, IS_Score_Auto, Narrative)

df_analysis_native$ref_type2 <- factor(df_analysis_native$ref_type2, levels = c("NOM", "SASS","PRO","BPCL", "WCL","CA", "VERB"))

p1 <- df_analysis_native %>%
  ggstatsplot::ggbetweenstats(
    x = ref_type2,
    y = IS_Score_Auto,
    title = "Native Signers",
    ggplot.component = list(
      ggplot2::scale_y_continuous(limits = (c(-2, 5)), breaks = seq(-2,5, by=1)), 
      theme(text = element_text(size=25)),
      geom_hline(yintercept = 2.20, linetype="dashed", 
                 color = "darkred", size=1)),
    xlab = "RE Type",
    ylab = "Accessibility Score",
    type = "P",
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "nrc_npg",
    results.subtitle = FALSE,
    centrality.point.args = list(size = 6, color = "darkred"),
    centrality.label.args = list(size = 6, nudge_y = -0.4, segment.linetype = 4,
                                 min.segment.length = 0),
    pairwise.comparisons = FALSE)

p1

ggsave("p1.png", p1, width = 8.5, height = 7)

# Late Signers
df_analysis_late = df_late %>% select(Subject, Nativeness, Discourse, ref_type2, IS_Score_Auto, Narrative)

df_analysis_late$ref_type2 <- factor(df_analysis_late$ref_type2, levels = c("NOM", "SASS","PRO","BPCL", "WCL","CA", "VERB"))

p2 <- df_analysis_late %>%
  ggstatsplot::ggbetweenstats(
    x = ref_type2,
    y = IS_Score_Auto,
    title = "Late Signers",
    ggplot.component = list(
      ggplot2::scale_y_continuous(limits = (c(-2, 5)), breaks = seq(-2,5, by=1)), 
      theme(text = element_text(size=25)),
      geom_hline(yintercept = 1.82, linetype="dashed", 
                 color = "darkred", size=1)),
    xlab = "RE Type",
    ylab = "Accessibility Score",
    type = "P",
    ggtheme = ggthemes::theme_tufte(),
    package = "ggsci",
    palette = "nrc_npg",
    results.subtitle = FALSE,
    centrality.point.args = list(size = 6, color = "darkred"),
    centrality.label.args = list(size = 6, nudge_y = 0.4, segment.linetype = 4,
                                 min.segment.length = 0),
    pairwise.comparisons = FALSE)

p2

ggsave("p2.png", p2, width = 8.5, height = 7)

# Combine plots
p1_p2 <- combine_plots(
  plotlist = list(p1, p2),
  plotgrid.args = list(nrow = 1),
  annotation.args = list(
    tag_levels = "a"
  )
)

ggsave("p1_p2.png",p1_p2,  width = 18, height = 9)


# Discourse plot
df_analysis = df %>% select(Subject, Nativeness, Discourse, ref_type1, IS_Score_Auto, Narrative)
df_analysis$ref_type1 <- factor(df_analysis$ref_type1, levels = c("NOM", "PRO","CL","NULL"))

p3 <- df_analysis %>%
  ggstatsplot::grouped_ggbetweenstats(
    ggplot.component = list(
      ggplot2::scale_y_continuous(limits = (c(-2, 5)), breaks = seq(-2,5, by=1)), 
      theme(text = element_text(size=25))),
    #scale_x_discrete(limits = c("NOM", "PRO", "CL", "ZERO"))),
    x = ref_type1,
    y = IS_Score_Auto,
    grouping.var = Discourse,
    xlab = "RE Category",
    ylab = "Accessibility Score",
    type = "P",
    ggtheme = ggthemes::theme_tufte(),
    centrality.point.args = list(size = 6, color = "darkred"),
    centrality.label.args = list(size = 6, nudge_y = -1.5, segment.linetype = 4,
                                 min.segment.length = 0),
    package = "ggsci",
    palette = "nrc_npg",
    results.subtitle = FALSE,
    pairwise.comparisons = FALSE)

p3

ggsave("p3.png",p3,  width = 18, height =9)

# Encode vector types
df$Narrative %<>% as.integer() 
df$Subject %<>% as.integer()
df$RE %<>% as.factor() 
df$Nativeness %<>% as.factor()
df$Discourse %<>% as.factor()
df$ref_type1 %<>% as.factor()
df$ref_type2 %<>% as.factor()

# Contrast coding
df_analysis = df %>% select(Subject, Nativeness, Discourse, ref_type2, ref_type1, IS_Score_Auto, Narrative) %>% subset(ref_type1 != "PRO")
df_analysis$ref_type1 = droplevels(df_analysis$ref_type1)
df_analysis %<>% subset(ref_type2 != "CA")
df_analysis$ref_type2 = droplevels(df_analysis$ref_type2)

df_analysis$Nativeness2 <- df_analysis$Nativeness %>% dplyr::recode(`0` = -.5, `1`=.5)
contrasts(df_analysis$ref_type1) <- contr.sum(3)/2
contrasts(df_analysis$Discourse) <- contr.sum(3)/2

# Model for IS Score
model_1 <- brm(IS_Score_Auto ~ Discourse*Nativeness + (1|Subject), 
               data = df_analysis, iter = 3000, warmup = 1500, 
               chains = 4, cores = 4, file ="model/model_.rds")
model_1
