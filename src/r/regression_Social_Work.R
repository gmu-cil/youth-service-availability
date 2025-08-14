library(stringr)
library(readr)
library(dplyr)
require(utils)
library(reshape2)
library(caret)
library(rpart)
library(ggplot2)
library(tidyr)
library(scales)
library(lmerTest)
library("Hmisc")
library("PerformanceAnalytics")
library(ggfortify)
library(rstantools)
library(MuMIn)
library(robustlmm)
library(MASS)
library(effects)
library(data.table)

setwd("~/git/youth/data/")

# data loading
load_data <- function(){
  vars <- read.csv("dropout_sd_prediction_2023.csv", header = T, sep = ",")
  vars
}

confint.rlmerMod <- function(object,parm,level=0.95) {
  beta <- fixef(object)
  if (missing(parm)) parm <- names(beta)
  se <- sqrt(diag(vcov(object)))
  z <- qnorm((1+level)/2)
  ctab <- cbind(beta-z*se,beta+z*se)
  colnames(ctab) <- stats:::format.perc(c((1-level)/2,(1+level)/2),
                                        digits=3)
  return(ctab[parm,])
}

vars <- load_data()
vars <- subset(vars, select=-c(X,geometry))
vars <- vars[!is.na(vars$academic_year),]
vars$total_facilities <- vars$MH + vars$SA

summary(vars$dropout_female_rate)
summary(vars$dropout_male_rate)
summary(vars$dropout_economically_disadvantaged_rate)
summary(vars$dropout_rate)
summary(vars$total_facilities)
table(vars$SA)
table(vars$MH)
table(vars$YOUTH)
summary(vars$SA_average_distance)
summary(vars$SA_close)
summary(vars$MH_average_distance)
summary(vars$MH_close)
summary(vars$YOUTH_average_distance)
summary(vars$YOUTH_close)

hist(vars$SA, breaks=100)
hist(vars$MH, breaks=100)
hist(vars$YOUTH, breaks=100)

hist(vars$SA_average_distance, breaks=100)
hist(vars$MH_average_distance, breaks=100)
hist(vars$YOUTH_average_distance, breaks=100)

hist(vars$SA_close, breaks=100)
hist(vars$MH_close, breaks=100)
hist(vars$YOUTH_close, breaks=100)

vars <- vars[!is.na(vars$dropout_rate),]
vars$SA[is.na(vars$SA)] <- 0
vars$MH[is.na(vars$MH)] <- 0
vars$YOUTH[is.na(vars$YOUTH)] <- 0
vars$white_percentage <- vars$white / vars$total_population
vars$black_percentage <- vars$black / vars$total_population
vars$other_race_percentage <- (vars$asian + vars$hispanic + vars$american_indian_alaskan_native +
                                 vars$hawaiian_native_or_other_pacific_islander) / vars$total_population
vars$health_insurance_percent <- vars$total_health_coverage / vars$total_population

vars$disability_under_18_percent <- vars$disability_under_18 / vars$population_under_18
vars$Year <- as.factor(vars$Year)
vars$dropout_rate <- vars$dropout_rate * 1000
vars <- vars[!is.element(vars$STUSPS, c("AK","CO")),]
vars$STUSPS <- as.factor(vars$STUSPS)

chart.Correlation(vars[vars$Year==2022,c("SA","MH","YOUTH",
                          "SA_average_distance","MH_average_distance",
                          "YOUTH_average_distance",
                          "SA_close","MH_close","YOUTH_close")], 
                  histogram=TRUE, pch=19)

# 1. 2023 data including predicted values
regress_fixed <- function (dv, iv, vars){
  operator<-" ~ "
  # rhs<- paste0(iv, " + (1 + ", iv, "|seq) + (1 + ", iv, "| City) + pop")
  rhs<- paste0(iv, "+ median_income + bachelors_or_higher +
         white_percentage + black_percentage + other_race_percentage +
         private_percentage + public_percentage + foreign_born_percentage + 
         disability_under_18_percent +
         pop_15_19F + pop_15_19M + STUSPS")
  
  model_1 <- paste0(
    dv,
    operator,
    rhs
  ) %>% as.formula
  
  fit <- lm(model_1, data=vars)
  print(summary(fit))
}

regress_random <- function (dv, iv, vars){
  
  print(paste("#### IV:", iv))
  print(paste("#### DV:", dv))
  
  operator<-" ~ "
  # rhs<- paste0(iv, " + (1 + ", iv, "|seq) + (1 + ", iv, "| City) + pop")
  rhs<- paste0("median_income + bachelors_or_higher +
         white_percentage + black_percentage + other_race_percentage +
         private_percentage + public_percentage + foreign_born_percentage + 
         disability_under_18_percent +
         pop_15_19F + pop_15_19M + Year + (1 | STUSPS)")
  
  model_1 <- paste0(
    dv,
    operator,
    rhs
  ) %>% as.formula
  
  fit <- lmer(model_1, data=vars)
  print(summary(fit))
}

regress_moderation <- function (dv, iv, vars){

    print(paste("#### IV:", iv))
    print(paste("#### DV:", dv))
    
    operator<-" ~ "
  # rhs<- paste0(iv, " + (1 + ", iv, "|seq) + (1 + ", iv, "| City) + pop")
  rhs<- paste0(iv, "+ (median_income*", iv, ") + median_income + bachelors_or_higher +
         white_percentage + black_percentage + other_race_percentage +
         private_percentage + public_percentage + foreign_born_percentage + 
         disability_under_18_percent +
         pop_15_19F + pop_15_19M + Year + (1 | STUSPS)")
  
  model_1 <- paste0(
    dv,
    operator,
    rhs
  ) %>% as.formula
  
  fit <- lmer(model_1, data=vars)
  print(summary(fit))
}

batch_run <- function(data){
  regress_fixed("dropout_rate", "SA", data)
  regress_fixed("dropout_rate", "MH", data)
  regress_fixed("dropout_rate", "YOUTH", data)
  regress_fixed("dropout_rate", "SA_average_distance", data)
  regress_fixed("dropout_rate", "MH_average_distance", data)
  regress_fixed("dropout_rate", "YOUTH_average_distance", data)
  regress_fixed("dropout_rate", "SA_close", data)
  regress_fixed("dropout_rate", "MH_close", data)
  regress_fixed("dropout_rate", "YOUTH_close", data)
  
  regress_fixed("dropout_economically_disadvantaged_rate", "SA", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "MH", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "YOUTH", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "SA_average_distance", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "MH_average_distance", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "YOUTH_average_distance", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "SA_close", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "MH_close", data)
  regress_fixed("dropout_economically_disadvantaged_rate", "YOUTH_close", data)

}

batch_run(vars[vars])


batch_run_random <- function(data){
  regress_random("dropout_rate", "SA", data)
  regress_random("dropout_rate", "MH", data)
  regress_random("dropout_rate", "YOUTH", data)
  regress_random("dropout_rate", "SA_average_distance", data)
  regress_random("dropout_rate", "MH_average_distance", data)
  regress_random("dropout_rate", "YOUTH_average_distance", data)
  regress_random("dropout_rate", "SA_close", data)
  regress_random("dropout_rate", "MH_close", data)
  regress_random("dropout_rate", "YOUTH_close", data)
  
  # regress_random("dropout_economically_disadvantaged_rate", "SA", data)
  # regress_random("dropout_economically_disadvantaged_rate", "MH", data)
  # regress_random("dropout_economically_disadvantaged_rate", "YOUTH", data)
  # regress_random("dropout_economically_disadvantaged_rate", "SA_average_distance", data)
  # regress_random("dropout_economically_disadvantaged_rate", "MH_average_distance", data)
  # regress_random("dropout_economically_disadvantaged_rate", "YOUTH_average_distance", data)
  # regress_random("dropout_economically_disadvantaged_rate", "SA_close", data)
  # regress_random("dropout_economically_disadvantaged_rate", "MH_close", data)
  # regress_random("dropout_economically_disadvantaged_rate", "YOUTH_close", data)
  
}

batch_run_random(vars[vars$predicted!=1,])


batch_run_mod <- function(data){
  regress_moderation("dropout_rate", "SA", data)
  regress_moderation("dropout_rate", "MH", data)
  regress_moderation("dropout_rate", "YOUTH", data)
  regress_moderation("dropout_rate", "SA_average_distance", data)
  regress_moderation("dropout_rate", "MH_average_distance", data)
  regress_moderation("dropout_rate", "YOUTH_average_distance", data)
  regress_moderation("dropout_rate", "SA_close", data)
  regress_moderation("dropout_rate", "MH_close", data)
  regress_moderation("dropout_rate", "YOUTH_close", data)
# 
#   regress_moderation("dropout_economically_disadvantaged_rate", "SA", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "MH", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "YOUTH", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "SA_average_distance", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "MH_average_distance", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "YOUTH_average_distance", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "SA_close", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "MH_close", data)
#   regress_moderation("dropout_economically_disadvantaged_rate", "YOUTH_close", data)
  
}

batch_run_mod(vars[vars$predicted!=1,])



plot_graph <- function (df, dv, dt, category, max_x, df){
  
  df$moran_groups <- as.factor(case_when(sf$nearby_group_num >= med ~ "high", sf$nearby_group_num < med ~ "low"))
  
  fit1 <- lmer(as.formula(model), data=sf)
  rsq <- MuMIn::r.squaredGLMM(fit1)
  fit <- summary(fit1)
  est <- fit$coefficients["poly(overlap, 2)2",1]
  p <- fit$coefficients["poly(overlap, 2)2",5]
  est1 <- fit$coefficients["poly(overlap, 2)1",1]
  p1 <- fit$coefficients["poly(overlap, 2)1",5]
  estl <- fit$coefficients["local_moran",1]
  plocal <- fit$coefficients["local_moran",5]
  est_inter <- ifelse(str_detect(model, "\\*local_moran"), 
                      fit$coefficients["poly(overlap, 2)2:local_moran",1], "")
  p_inter <- ifelse(str_detect(model, "\\*local_moran"),
                    fit$coefficients["poly(overlap, 2)2:local_moran",5],"")
  table <- rbind(table, c(names[i], "total", c, est, p, est1, p1, rsq[2],
                          estl, plocal, est_inter, p_inter,
                          mean(x), sd(x), nrow(sf)))
  colnames(table) <- c("model", "class_moran", "category", "est_quad", "p_quad",
                       "est_singular", "p_singular","r_squared",
                       "est_local_moran", "p_local_moran",
                       "est_inter", "p_inter",
                       "mean_local_moran", "sd_local_moran", "DF")
  
  model1 <- stats::predict(fit1, newdata=sf)
  err <- as.data.frame(effects::effect("poly(overlap, 2)", mod=fit1,
                                       xlevels=list(overlap=sf$overlap)))
  sf <- cbind(sf, err[,2:5], model1)
  plot_graph(sf, names[i], "LMER", c, max(sf$overlap), nrow(sf))
  
  # lim=ifelse(c(dv==names[1],TRUE), c(-2,4), c(0,4)) # Logged DV
  lim=ifelse(c(TRUE,dv==names[1]), c(0,6), c(0,30)) # regular DV
  p <- ggplot(sf, aes(x = overlap, y = get(dv), color = moran_groups, shape=moran_groups)) +
    theme_bw() +
    geom_point(size = 1, alpha = .3) +
    coord_cartesian(ylim=lim) +
    geom_smooth(data=sf, stat="smooth",
                method = "loess", 
                span=1.2,
                aes(x = overlap, y=model1, ymin=lower, ymax=upper,
                    fill=moran_groups, linetype=moran_groups), size=0.5, 
                alpha=0.3) +
    labs(x = "Topic Overlap",  y = ifelse(dv==names[1], "Avg. # of Events", "Avg. # of RSVPs"),
         color = "Moran I",
         shape = "Moran I",
         linetype = "Moran I",
         fill = "Moran I") +
    theme(legend.key.size=unit(1,"cm"), axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15), legend.text=element_text(size=15),
          legend.title = element_text(size=18), axis.title = element_text(size=18))  +
    ggtitle(paste0(category, ", ", dt, ", ", dv, ", DF: ", df)) + xlim(0,max_x) 
  p  
  ggsave(filename = paste0("images/Jan24_yearly_Moran/", category, "_", dt, "_", dv, "_plot.png"), p, width=7, height=5 )
}


moderation_graphs <- function (dv, iv, vars){
  
  operator<-" ~ "
  # rhs<- paste0(iv, " + (1 + ", iv, "|seq) + (1 + ", iv, "| City) + pop")
  rhs<- paste0(iv, "+ (median_income*", iv, ") + median_income + bachelors_or_higher +
         white_percentage + black_percentage + other_race_percentage +
         private_percentage + public_percentage + foreign_born_percentage + 
         disability_under_18_percent +
         pop_15_19F + pop_15_19M + Year + (1 | STUSPS)")
  
  model_1 <- paste0(
    dv,
    operator,
    rhs
  ) %>% as.formula
  
  fit <- lmer(model_1, data=vars)
  
  data <- vars
  data$model <- stats::predict(fit, newdata=data)
  err <- as.data.frame(effects::effect(iv, mod= fit, 
                                       xlevels=list(median_income=data$median_income)))
  vars <- cbind(vars, err[,2:5])
  
  print(summary(fit))
}



ggplot(vars[vars$년도=="2018",], aes(x = deprevation_index, y = books_per_person)) +
  theme_bw() +
  geom_point(size = 2, alpha = .8, aes(color = 시도, shape=시도)) +
  geom_smooth(data=vars, stat="smooth",
              method = "loess", 
              span=1.5, color="black", linetype="dashed",
              aes(x = deprevation_index, y=model, ymin=lower, ymax=upper), size=0.5, 
              alpha=0.3, show.legend = F) +
  labs(x = "박탈지수(SED)",
       y = "평균 대출 책수") +
  theme(legend.key.size=unit(1,"cm"), axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), legend.text=element_text(size=15),
        legend.title = element_text(size=18), axis.title = element_text(size=18)) + xlim(-4,3) + 
  scale_x_continuous(n.breaks = 7) + scale_y_continuous(n.breaks = 12)


# Interaction between books per library and deprivation
vars <- load_data()
fit2<- rlmer("books_per_person ~ deprevation_index + 인구수 + AGE + books_per_library + libraries_per_area +
          대중교통_역_노선_면적 + deprevation_index*books_per_library + 년도 + (1|시도)", 
             data=vars)
summary(fit2)
confint(fit2)

vars$model <- stats::predict(fit2, newdata=vars)
err <- as.data.frame(effects::effect("deprevation_index", mod= fit2, 
                                     xlevels=list(deprevation_index=vars$deprevation_index)))
vars <- cbind(vars, err[,2:5])
cor(vars[,c("deprevation_index","model","fit")])

ggplot(vars[vars$년도=="2018",], aes(x = deprevation_index, y = books_per_person,
                                   color = dprv_groups, shape=dprv_groups)) +
  theme_bw() +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(data=vars, stat="smooth",
              method = "loess", 
              span=1.5,
              aes(x = deprevation_index, y=model, ymin=lower, ymax=upper,
                  fill=dprv_groups, linetype=dprv_groups), size=0.5, 
              alpha=0.3, show.legend = T) +
  # geom_ribbon(data= err, aes(x=deprevation_index, y=fit, ymin=lower, ymax=upper), alpha= 0.3, fill=dprv_groups) +
  labs(x = "박탈지수(SED)",
       y = "평균 대출 책수",
       color = "도서관 소장도서 수",
       shape = "도서관 소장도서 수",
       linetype = "도서관 소장도서 수",
       fill = "도서관 소장도서 수") +
  theme(legend.key.size=unit(1,"cm"), axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), legend.text=element_text(size=15),
        legend.title = element_text(size=18), axis.title = element_text(size=18)) + xlim(-4,3) + 
  scale_x_continuous(n.breaks = 7) + scale_y_continuous(n.breaks = 12)

# Upper 33%
fit2<- rlmer("books_per_person ~ deprevation_index + 인구수 + AGE + books_per_library + libraries_per_area +
          대중교통_역_노선_면적 + deprevation_index*books_per_library + 년도 + (1|시도)", 
             data=vars[vars$dprv_groups=="상위 33%",])
summary(fit2)
confint(fit2)

fit2<- rlmer("books_per_person ~ deprevation_index + 인구수 + AGE + books_per_library + libraries_per_area +
          대중교통_역_노선_면적 + deprevation_index*libraries_per_area + 년도 + (1|시도)", 
             data=vars[vars$lib_groups=="상위 33%",])
summary(fit2)
confint(fit2)

# library(visreg)
# visreg(fit2, "deprevation_index", by="dprv_groups", overlay=T, vars)

# coefs <- data.frame(coef(summary(fit1)))
# coefs.robust <- coef(summary(fit2))
# p.values <- 2*pt(abs(coefs.robust[,3]), coefs$df, lower=FALSE)
# p.values

### Interaction between Deprivation and libraries per area
vars <- load_data()
fit2<- rlmer("books_per_person ~ deprevation_index + 인구수 + AGE + books_per_library + libraries_per_area +
          대중교통_역_노선_면적 + deprevation_index*libraries_per_area + 년도 + (1|시도)", 
             data=vars)
summary(fit2)
confint(fit2)
vars$model <- stats::predict(fit2, newdata=vars)
err <- as.data.frame(effects::effect("deprevation_index", mod= fit2, 
                                     xlevels=list(deprevation_index=vars$deprevation_index)))
vars <- cbind(vars, err[,2:5])
ggplot(vars[vars$년도=="2018",], aes(x = deprevation_index, y = books_per_person,
                                   color = lib_groups, shape=lib_groups)) +
  theme_bw() +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(data=vars, stat="smooth",
              method = "loess", 
              span=1.5,
              aes(x = deprevation_index, y=model, ymin=lower, ymax=upper,
                  fill=lib_groups, linetype=lib_groups), size=0.5, 
              alpha=0.3, show.legend = T) +
  labs(x = "박탈지수(SED)",
       y = "평균 대출 책수",
       color = "단위면적 당 도서관 수",
       shape = "단위면적 당 도서관 수",
       linetype = "단위면적 당 도서관 수",
       fill = "단위면적 당 도서관 수") +
  theme(legend.key.size=unit(1,"cm"), axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 15), legend.text=element_text(size=15),
        legend.title = element_text(size=18), axis.title = element_text(size=18)) + xlim(-4,3) + 
  scale_x_continuous(n.breaks = 7) + scale_y_continuous(n.breaks = 12)

# fit1<- lmer("books_per_person ~ deprevation_index + 인구수 + AGE + books_per_library + libraries_per_area +
#           대중교통_역_노선_면적 + 년도 + (1|시도)", 
#             data=vars)
# fit2<- rlmer("books_per_person ~ deprevation_index + 인구수 + AGE + books_per_library + libraries_per_area +
#           대중교통_역_노선_면적 + 년도 + (1|시도)", 
#              data=vars)
# summary(fit2)
# coefs <- data.frame(coef(summary(fit1)))
# coefs.robust <- coef(summary(fit2))
# p.values <- 2*pt(abs(coefs.robust[,3]), coefs$df, lower=FALSE)
# p.values
# 
