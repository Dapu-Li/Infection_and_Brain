# Brain Age Gap (BAG)

## BAG Analysis

The **brain age gap (BAG)** was defined as the difference between bias-corrected brain age and chronological age at the time of MRI acquisition.

To investigate the associations between **22 infectious diseases** and BAG, linear regression models were performed.

## Covariate Adjustments

All models were adjusted for chronological age using the residual method, along with the following covariates, grouped into categories:

- **Demographic factors**:  
  Sex, ethnic group  

- **Socioeconomic status**:  
  Townsend Deprivation Index (TDI), education level  

- **Lifestyle factors**:  
  Physical activity (IPAQ), smoking status, alcohol consumption frequency  

- **Clinical factors**:  
  Body Mass Index (BMI), systolic blood pressure (SBP), diabetes  

- **Genetic factors**:  
  APOE ε4 carrier status, polygenic risk score for Alzheimer’s disease 

## Statistical Analysis

Linear regression analyses were conducted using **R (version 4.3.1)**.

Multiple testing correction was performed using the **Bonferroni method**, with a correction factor of 22 infection types.

- *p* < 0.05 was considered nominally significant (*)  
- Bonferroni-corrected *p* < 0.05 was considered statistically significant (**)  

## Code for BAG Analysis

```r
rm(list = ls())
path1 <- "~/Desktop/Infection"
path2 <- "C:/Users/Administrator/Desktop/Infection"
if (dir.exists(path1)) {
  setwd(path1)
} else if (dir.exists(path2)) {
  setwd(path2)
} else {
  stop(" ")
}
LinearReg_2class <- function(y, x, co = NULL, data) {

  data[[x]] <- as.factor(data[[x]])

  N <- nrow(data)

  tab <- table(data[[x]])
  n_x0 <- ifelse("0" %in% names(tab), tab["0"], 0)
  n_x1 <- ifelse("1" %in% names(tab), tab["1"], 0)
  
  if (!is.null(co) && length(co) > 0) {
    formula <- paste(y, "~", paste(c(x, co), collapse = " + "))
  } else {
    formula <- paste(y, "~", x)
  }
  
  lm_model <- lm(as.formula(formula), data = data)
  lm_summary <- summary(lm_model)
  lm_ci <- confint(lm_model)
  
  beta <- lm_summary$coefficients[2, 1]
  se <- lm_summary$coefficients[2, 2]
  t_val <- lm_summary$coefficients[2, 3]
  p_val <- lm_summary$coefficients[2, 4]
  lower <- lm_ci[2, 1]
  upper <- lm_ci[2, 2]
  
  coeffs <- paste0(round(beta, 2), " (", round(lower, 2), ", ", round(upper, 2), ")")
  
  lm.coeff <- data.frame(
    Characteristics = y,
    Predictors = paste(x, levels(data[[x]])[-1], sep = "_"),
    N_total = N,
    N_x0 = n_x0,
    N_x1 = n_x1,
    Beta = beta,
    SE = se,
    t_value = t_val,
    Lower = lower,
    Upper = upper,
    `Coef_95CI` = coeffs,
    `P_value` = p_val,
    stringsAsFactors = FALSE
  )
  
  return(lm.coeff)
}


res_2class_lm <- function(y, Var.analys, co = NULL, data) {
  res <- data.frame()
  
  for (i in 1) {
    combos <- combn(Var.analys, i)
    for (j in 1:ncol(combos)) {
      predictors <- combos[, j]
      res <- rbind(res, LinearReg_2class(y, predictors, co, data))
    }
  }
  
  return(res)
}

library(data.table)
code <- fread("Supplementary/Infection_Code.csv",data.table = FALSE)
colnames(code) <- gsub(" ", "_", colnames(code))
colnames(code) <- gsub("-", "_", colnames(code))
Infection_lst <- colnames(code)[3:24]
rm(code)


time_df <- fread("Data/Time.csv")[,c(1,4:9,2:3)]
colnames(time_df) <- c('eid','t0','t1','t2','t3','death1','death2','birth_year','birth_month')

time_df$birth_date <- as.Date(paste(time_df$birth_year, time_df$birth_month, "1", sep = "-"), format = "%Y-%m-%d")
time_df$birth_year <- NULL
time_df$birth_month <- NULL
time_df$death_date <- ifelse(is.na(time_df$death1), time_df$death2, time_df$death1)
time_df$age2 <- as.numeric(difftime(time_df$t2, time_df$birth_date, units = "days")) / 365.25

for (col in colnames(time_df)[-1]) {
  time_df[[col]] <- as.Date(time_df[[col]], origin = "1970-01-01")
}

time_df$death <- ifelse(is.na(time_df$death_date), 0, 1)

cov_df <- fread("Data/Cov/cov_imput.csv",data.table = FALSE)
base_cov <- c("Sex","TDI","Ethnic_group","BMI",'education_level',"IPAQ",'Smoke','Alcohol_freq',
              "SBP","Diabetes")
cov_ad <- c(base_cov,'apoe_carrier',"prs_ad")
cov_pd <- c(base_cov,'prs_pd')
cov_stroke <- c(base_cov,'prs_stroke')
cov_depression <- c(base_cov,"PHQ4")

disease_lst <- c('BAG')
disease <- disease_lst[1]

infection <- Infection_lst[1]

for (disease in disease_lst) {
  
  cat("=== Processing disease:", disease, "===\n")
  
  res_df <- data.frame()
  save_path <- paste0("Results/s1_Association/BAG/", disease, ".csv")
  
  if (file.exists(save_path)) {
    cat("File already exists, skipping:", save_path, "\n")
    next
  }
  
  if (disease %in% c("ACD","AD","VD",'BAG')) {
    cov <- cov_ad
  } else if (disease == "PD") {
    cov <- cov_pd
  } else if (disease == "Stroke") {
    cov <- cov_stroke
  } else if (disease == "Depression") {
    cov <- cov_depression
  } else {
    cov <- base_cov
  }
  
  for (infection in Infection_lst) {
    
    cat("  - Processing infection:", infection, "\n")
    
    yy <- fread(paste0("Data/BAG_structure/", disease, "_total1.csv"), data.table = FALSE)
    xx <- fread(paste0("Data/Infection/Over/", infection, ".csv"), data.table = FALSE)
    
    xy <- merge(yy, xx, by = "eid")
    xy <- merge(xy, time_df, by = "eid")
    
    xy[["start"]] <- pmax(xy[["t0"]], xy[[paste0(infection, "_date")]], na.rm = TRUE)
    xy[[infection]] <- ifelse(is.na(xy[[infection]]), 0, xy[[infection]])
    xy[['end']] <- pmin(xy[["death_date"]], xy[['t2']], na.rm = TRUE)
    xy[['duration']] <- as.numeric(xy[['end']] - xy[['start']])
    
    xy[[infection]]<- ifelse(xy[[infection]] == 1 & xy[['duration']] < 0, 0, xy[[infection]])
    
    xy[[disease]] <- xy[['Brain_PAD']]
    
    xy[['age_infection']] <- ifelse(xy[[infection]]==1, 
                                    as.numeric(xy[[paste0(infection, "_date")]] - xy[['birth_date']]) / 365.25, 
                                    as.numeric(xy[['t0']] - xy[['birth_date']]) / 365.25)
    
    xy[['age_start']] <- as.numeric(xy[['start']] - xy[['birth_date']]) / 365.25
    xy[['age_end']] <- as.numeric(xy[['end']] - xy[['birth_date']]) / 365.25
    xy$age_baseline <- as.numeric(xy$t2 - xy$birth_date) / 365.25
    
    xy$age_group <- cut(xy$age_baseline,
                        breaks = c(-Inf, 50, 60, 70, Inf),
                        labels = c("40-50", "51-60", "61-70", "71-80"))

    colnames(xy)
    cat("After filtering for 40-80, remaining rows:", nrow(xy), "\n")
    xy[[disease]] <- xy[['Brain_PAD']]
    fml <- paste0(disease," ~ age_baseline")
    xy[[disease]] <- residuals(lm(as.formula(fml), data = xy)) 
    xy <- subset(xy, select = c("eid", infection, disease))
    
    tmp_df <- merge(xy, cov_df, by = "eid")
    
    cat("    -> Number of rows for analysis:", nrow(tmp_df), "\n")
    
    tmp_res <- res_2class_lm(y = disease, Var.analys = infection, co = cov, data = tmp_df)
    
    if (is.null(res_df)) {
      res_df <- tmp_res
    } else {
      res_df <- rbind(res_df, tmp_res)
    }
    cat("    -> Completed infection:", infection, "\n")
  }

  res_df$P_BFI <- p.adjust(res_df$P_value, method = "bonferroni", n = 22)
  res_df$P_FDR <- p.adjust(res_df$P_value, method = "fdr")
  fwrite(res_df, save_path)
  
  cat("=== Finished disease:", disease, "Results saved to:", save_path, "===\n\n")
}
