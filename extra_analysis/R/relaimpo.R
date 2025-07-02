# Key Drivers - Cint Tracker # 


# CONSIDERATION ~ BRAND ATTRIBUTES ----------------------------------------

attr_consideration <- function(group = NULL) {
  if (is.null(group)) {
    con_dat <- df |> 
      filter(str_detect(awr_a_1, "BMW")) |> # must be aware of BMW
      select(con_br_1, starts_with("att_brfunctional"), weights_xmedia) |> 
      select(ends_with("_1"), weights_xmedia) |> 
      mutate(across(everything(), ~ case_when(str_detect(., "BMW") ~ 1,
                                              TRUE ~ 0)))
  } else {
    
    grp_choice <- select.list(title = "\nWhich group would you like to subset?",
                        choices = unique(df[[group]]), multiple = FALSE)
    con_dat <- df |> 
      filter(!!sym(group) == grp_choice) |> 
      # filter(demo_gender == group) |> 
      filter(str_detect(awr_a_1, "BMW")) |> # must be aware of BMW
      select(con_br_1, starts_with("att_brfunctional"), weights_xmedia) |> 
      select(ends_with("_1"), weights_xmedia) |> 
      mutate(across(everything(), ~ case_when(str_detect(., "BMW") ~ 1,
                                              TRUE ~ 0)))
  }
  con_fit <- glm(con_br_1 ~ ., family = "binomial", data = con_dat[,1:16])
  attr_names <- brand_choice("bmw")$brand_attrs
  summary(con_fit)$coefficients |> 
    clean_names() |> 
    data.frame() |> 
    rownames_to_column(var = "var") |> 
    left_join(attr_names, by = "var") |> 
    relocate(q, .before = var) |> 
    select(-var) |> 
    arrange(desc(z_value))
    
}
attr_consideration(group = NULL)
attr_consideration(group = "demo_gender")
attr_consideration(group = "intent")

con_dat <- df |> 
  filter(str_detect(awr_a_1, "BMW")) |> # must be aware of BMW
  select(con_br_1, starts_with("att_brfunctional"), weights_xmedia) |> 
  select(ends_with("_1"), weights_xmedia) |> 
  mutate(across(everything(), ~ case_when(str_detect(., "BMW") ~ 1,
                                          TRUE ~ 0)))

# standard regression
con_fit <- glm(con_br_1 ~ ., family = "binomial", data = con_dat[,1:16])
summary(con_fit)  

# dominance analysis
library(srvyr)
svy_con <- con_dat |> 
  as_survey_design(ids = 1, weight = weights_xmedia) 

d1b <- domir::domin(con_br_1 ~ att_brfunctional_1_1+att_brfunctional_2_1+att_brfunctional_3_1 +
                      att_brfunctional_4_1+att_brfunctional_5_1+att_brfunctional_6_1 +
                      att_brfunctional_7_1+att_brfunctional_8_1+att_brfunctional_9_1 +
                      att_brfunctional_10_1+att_brfunctional_11_1+att_brfunctional_12_1 +
                      att_brfunctional_13_1 +att_brfunctional_14_1+att_brfunctional_15_1,
                    stats::glm, 
                    list(pscl::pR2, "McFadden"), 
                    family=binomial(link='logit'),
                    data = svy_con,
                    weights = svy_con$weights_xmedia)

# consideration ranks
con_vars <- brand_choice("bmw")$brand_attrs
con_ranks <- tibble(var = names(d1b$General_Dominance), rank = d1b$Ranks) |> 
  left_join(con_vars) |> 
  arrange(rank)

# consideration significance
con_vars <- brand_choice("bmw")$brand_attrs
con_sig <- tibble(var = names(con_fit$model)[-1],
                  coef = summary(con_fit)$coefficients[, "Estimate"][-1],
                  zscore = summary(con_fit)$coefficients[, "z value"][-1]) |> 
  left_join(con_vars, by = "var")

# quadrant plot # 

# function to pull % agreement with each attribute
bmw_attr <- function(var) {
  quo_var <- sym(var)
  unweighted |> 
    filter(str_detect(awr_a_1, "BMW")) |> 
    group_by(!!quo_var) |> 
    summarise(proportion = survey_mean(vartype = "ci"),
              total = survey_total(),
              n = unweighted(n())) |>  
    filter(!!quo_var != 0) |> 
    select(proportion:n) |> 
    mutate(variable = var) |> 
    ungroup() 
}

# x-axis
bmw_xaxis <- con_vars$var |> 
  map_dfr(bmw_attr) |> 
  left_join(con_vars, by = c("variable" = "var")) |> 
  select(-variable)

# y-axis 
# data used here is for BMW aware
bmw_yaxis <- exp(cbind(OR = coef(con_fit), confint(con_fit, level = .9))) |> 
  data.frame() |>
  clean_names() |> 
  rownames_to_column(var = "var") |>
  slice(-1) |> 
  left_join(con_vars)

bmw_plot_data <- bmw_xaxis |> 
  left_join(select(bmw_yaxis, or:q)) |> 
  select(q, xaxis = proportion, yaxis = or) |> 
  left_join(select(con_sig, coef:q), by = c('q' = 'q')) |> 
  mutate(col = ifelse(zscore > 1.96 | zscore < -1.96, "a", "b"))

# final plot - BMW
ggplot(bmw_plot_data, aes(x = xaxis, y = yaxis, group = col, fill = col)) +
  geom_point(color = 'darkgray', 
             size = 2.5, pch = 21) + 
  geom_vline(xintercept = .53, linewidth = .6, color = "darkgray") +
  geom_hline(yintercept = 1, linewidth = .4, color = "darkgray") +
  scale_x_continuous(labels = scales::percent(seq(.3,.7 ,.1), accuracy = 1), 
                     breaks = seq(.3,.7 ,.1),
                     limits = c(0.3, .72)) +
  scale_y_continuous(breaks = .5:2.75, 
                     limits = c(0.5, 2.75)) +
  ggrepel::geom_label_repel(data = bmw_plot_data, 
                            aes(label = q),
                            fill = "white")
  ggrepel::geom_label_repel(data = subset(bmw_plot_data, (yaxis > 1 & xaxis < .53)), 
                            aes(label = q),
                            min.segment.length = 0,
                            nudge_y = .2,
                            nudge_x = -.01,
                            size = 5) +
  ggrepel::geom_label_repel(data = subset(bmw_plot_data, (yaxis > 1 & xaxis > .53)), 
                            aes(label = q),
                            min.segment.length = 0,
                            nudge_y = .2,
                            nudge_x = .01,
                            size = 5) +
  ggrepel::geom_label_repel(data = subset(bmw_plot_data, (yaxis < 1 & xaxis > .53)), 
                            aes(label = q),
                            min.segment.length = 0,
                            nudge_y = -.2,
                            nudge_x = .001,
                            size = 5) +
  ggrepel::geom_label_repel(data = subset(bmw_plot_data, (yaxis < 1 & xaxis < .115)), 
                            aes(label = q),
                            min.segment.length = 0,
                            nudge_y = -.2,
                            nudge_x = -.001,
                            size = 5) 
  annotate("label", x = .061, y = 3.1, label = "Address",
           size = 8, color = "#16588E", fill = "lightgray") +
  annotate("label", x = .158, y = 3.1, label = "Leverage",
           size = 8, color = "#16588E", fill = "lightgray",
           lineheight = .8) +
  annotate("label", x = .061, y = -.46, label = "Monitor",
           size = 8, color = "#16588E", fill = "lightgray") +
  annotate("label", x = .158, y = -.45, label = "Maintain",
           size = 8, color = "#16588E", fill = "lightgray") +
  # theme_twg(grid = "XY") +
  theme(axis.text = element_text(size = 13)) +
  labs(x = NULL, y = NULL,
       caption = "Results based on regression results among BMW aware in Waves 2 & 3")

# PURCHASE INTENT ~ BRAND ATTRIBUTES --------------------------------------

pi_dat <- df |> 
  filter(str_detect(awr_a_1, "BMW")) |> # must be aware of BMW
  select(pi_br_1, starts_with("att_brfunctional"), weights_xmedia) |> 
  select(ends_with("_1"), weights_xmedia) |> 
  mutate(across(everything(), ~ case_when(str_detect(., "BMW") ~ 1,
                                          TRUE ~ 0)))

pi_fit <- glm(pi_br_1 ~ ., family = "binomial", data = pi_dat[,1:16])
summary(pi_fit)  

# dominance analysis
library(srvyr)
svy_pi <- pi_dat |> 
  as_survey_design(ids = 1, weight = weights_xmedia) 

dpi <- domir::domin(pi_br_1 ~ att_brfunctional_1_1+att_brfunctional_2_1+att_brfunctional_3_1 +
                      att_brfunctional_4_1+att_brfunctional_5_1+att_brfunctional_6_1 +
                      att_brfunctional_7_1+att_brfunctional_8_1+att_brfunctional_9_1 +
                      att_brfunctional_10_1+att_brfunctional_11_1+att_brfunctional_12_1 +
                      att_brfunctional_13_1 +att_brfunctional_14_1+att_brfunctional_15_1,
                    stats::glm, 
                    list(pscl::pR2, "McFadden"), 
                    family=binomial(link='logit'),
                    data = svy_pi,
                    weights = svy_pi$weights_xmedia)

# purchase intent ranks
pi_ranks <- tibble(var = names(dpi$General_Dominance), pi_rank = dpi$Ranks) |> 
  left_join(con_vars) |> 
  arrange(pi_rank)

con_ranks |> 
  left_join(pi_ranks) |> 
  mutate(gap = rank - pi_rank)

# purchase intent significance
con_vars <- brand_choice("bmw")$brand_attrs
pi_sig <- tibble(var = names(pi_fit$model)[-1],
                  pi_coef = summary(pi_fit)$coefficients[, "Estimate"][-1],
                  pi_zscore = summary(pi_fit)$coefficients[, "z value"][-1]) |> 
  left_join(con_vars, by = "var")

# put together pi and con
con_sig |> 
  left_join(pi_sig)

# CONSIDERATION - BRAND TRAITS --------------------------------------------

con2_dat <- df |> 
  filter(str_detect(awr_a_1, "BMW")) |> # must be aware of BMW
  select(con_br_1, starts_with("att_brpersonality"), weights_xmedia) |> 
  select(ends_with("_1"), weights_xmedia) |> 
  mutate(across(everything(), ~ case_when(str_detect(., "BMW") ~ 1,
                                          TRUE ~ 0)))

# standard regression
con2_fit <- glm(con_br_1 ~ ., family = "binomial", data = con2_dat[,1:16])
summary(con2_fit)  

# dominance analysis
library(srvyr)
svy_con2 <- con2_dat |> 
  as_survey_design(ids = 1, weight = weights_xmedia) 

d_con <- domir::domin(con_br_1 ~ att_brpersonality_1_1+att_brpersonality_2_1+att_brpersonality_3_1 +
                        att_brpersonality_4_1+att_brpersonality_5_1+att_brpersonality_6_1 +
                        att_brpersonality_7_1+att_brpersonality_8_1+att_brpersonality_9_1 +
                        att_brpersonality_10_1+att_brpersonality_11_1+att_brpersonality_12_1 +
                        att_brpersonality_13_1 +att_brpersonality_14_1+att_brpersonality_15_1,
                    stats::glm, 
                    list(pscl::pR2, "McFadden"), 
                    family=binomial(link='logit'),
                    data = svy_con2,
                    weights = svy_con2$weights_xmedia)

# consideration ranks
con2_vars <- brand_choice("bmw")$brand_traits
con2_ranks <- tibble(var = names(d_con$General_Dominance), rank = d_con$Ranks) |> 
  left_join(con2_vars) |> 
  arrange(rank)


# PURCHASE INTENT - BRAND TRAITS ------------------------------------------

pi2_dat <- df |> 
  filter(str_detect(awr_a_1, "BMW")) |> # must be aware of BMW
  select(pi_br_1, starts_with("att_brpersonality"), weights_xmedia) |> 
  select(ends_with("_1"), weights_xmedia) |> 
  mutate(across(everything(), ~ case_when(str_detect(., "BMW") ~ 1,
                                          TRUE ~ 0)))

pi2_fit <- glm(pi_br_1 ~ ., family = "binomial", data = pi2_dat[,1:16])
summary(pi2_fit)  

# dominance analysis
library(srvyr)
svy_pi2 <- pi2_dat |> 
  as_survey_design(ids = 1, weight = weights_xmedia) 

dpi2 <- domir::domin(pi_br_1 ~ att_brpersonality_1_1+att_brpersonality_2_1+att_brpersonality_3_1 +
                       att_brpersonality_4_1+att_brpersonality_5_1+att_brpersonality_6_1 +
                       att_brpersonality_7_1+att_brpersonality_8_1+att_brpersonality_9_1 +
                       att_brpersonality_10_1+att_brpersonality_11_1+att_brpersonality_12_1 +
                       att_brpersonality_13_1 +att_brpersonality_14_1+att_brpersonality_15_1,
                    stats::glm, 
                    list(pscl::pR2, "McFadden"), 
                    family=binomial(link='logit'),
                    data = svy_pi2,
                    weights = svy_pi$weights_xmedia)

# purchase intent ranks
pi2_ranks <- tibble(var = names(dpi2$General_Dominance), pi2_rank = dpi2$Ranks) |> 
  left_join(con2_vars) |> 
  arrange(pi2_rank)

con2_ranks |> 
  left_join(pi2_ranks) |> 
  mutate(gap = rank - pi2_rank)

ll <- list(con_ranks = con_ranks, con2_ranks = con2_ranks)
openxlsx::write.xlsx(ll, "~/Desktop/ordered_consideration.xlsx")
