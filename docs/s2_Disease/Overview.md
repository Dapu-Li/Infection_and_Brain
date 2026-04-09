# Brain Disease

## Brain Disease Analysis

Seven **brain diseases** were included in the analysis, namely:  
`ACD`, `AD`, `VD`, `PD`, `Stroke`, `Depression`, `Anxiety`.

To investigate the associations between **22 infectious diseases** and these **7 brain diseases**, **time-dependent Cox regression models** were employed.

## Covariate Adjustments

Age was used as the time scale in the time-dependent Cox regression models, along with the following covariates, grouped into categories:

- **Demographic factors**:  
  Sex, ethnic group  

- **Socioeconomic status**:  
  Townsend Deprivation Index (TDI), education level  

- **Lifestyle factors**:  
  Physical activity (IPAQ), smoking status, alcohol consumption frequency  

- **Clinical factors**:  
  Body Mass Index (BMI), systolic blood pressure (SBP), diabetes  

**Outcome-specific covariates** were additionally applied as follows:  
- For `ACD`, `AD`, `VD`, and BAG: APOE ε4 carrier status and polygenic risk score for AD
- For `PD`: polygenic risk score for PD 
- For `Stroke`: polygenic risk score for stroke
- For `Depression`: Patient Health Questionnaire-4 (PHQ-4) total score  

## Statistical Analysis

For infections occurring **before baseline**, follow-up began at baseline; for infections occurring **after baseline**, individuals were moved from the non-infected group to the infected group at the time of infection.

Time-dependent Cox regression analyses were conducted using **R (version 4.3.1)** with the **survival** package.

Multiple testing correction was performed using the **Bonferroni method**, with a correction factor of **22 infectious diseases × 7 brain diseases**.

- *p* < 0.05 was considered nominally significant (*)  
- Bonferroni-corrected *p* < 0.05 was considered statistically significant (**)  

## Code for Brain Disease Analysis

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
library(survival)

CoxReg_2class <- function(time, status, x, co, data) {

  data[[x]] <- as.factor(data[[x]])

  N <- nrow(data)
  total_cases <- sum(data[[status]] == 1, na.rm = TRUE)
  total_controls <- sum(data[[status]] == 0, na.rm = TRUE)

  tab <- table(data[[x]], data[[status]])
  
  case_x0 <- ifelse("0" %in% rownames(tab) & "1" %in% colnames(tab), tab["0", "1"], 0)
  control_x0 <- ifelse("0" %in% rownames(tab) & "0" %in% colnames(tab), tab["0", "0"], 0)
  case_x1 <- ifelse("1" %in% rownames(tab) & "1" %in% colnames(tab), tab["1", "1"], 0)
  control_x1 <- ifelse("1" %in% rownames(tab) & "0" %in% colnames(tab), tab["1", "0"], 0)

  formula <- paste("Surv(", time, ",", status, ") ~", paste(c(x, co), collapse = '+'))
  cox_model <- coxph(as.formula(formula), data = data)
  cox_summary <- summary(cox_model)
  cox_ci <- exp(confint(cox_model))

  hr <- cox_summary$coefficients[1, 2]
  lower <- cox_ci[1, 1]
  upper <- cox_ci[1, 2]
  coeffs <- paste0(round(hr, 2), " (", round(lower, 2), ", ", round(upper, 2), ")")
  p_val <- cox_summary$coefficients[1, 5]
  
  cox.coeff <- data.frame(
    'Characteristics' = status,
    'Predictors' = paste(x, levels(data[[x]])[-1], sep = '_'),
    'N_total' = N,
    'Cases_total' = total_cases,
    'Controls_total' = total_controls,
    'Cases_x0' = case_x0,
    'Controls_x0' = control_x0,
    'Cases_x1' = case_x1,
    'Controls_x1' = control_x1,
    'HR' = hr,
    'Lower' = lower,
    'Upper' = upper,
    'Coef_95CI' = coeffs,
    'P_value' = p_val
  )
  
  return(cox.coeff)
}

res_2class_cox <- function(time, status, Var.analys, co, data) {
  res <- data.frame()
  for (y in status) {
    for (i in 1) {
      combos <- combn(Var.analys, i)
      for (j in 1:ncol(combos)) {
        predictors <- combos[, j]
        res <- rbind(res, CoxReg_2class(time, status, predictors, co, data))
      }
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

disease_df <- readxl::read_excel("Supplementary/Brain_function.xlsx")[1:3]
colnames(disease_df)
table(disease_df$Category)
disease_df <- subset(disease_df, Category == 'Brain diseases')
disease_lst <- disease_df$Phenotype
rm(disease_df)

time_df <- fread("Data/Time.csv")[,c(1,4:9,2:3)]
colnames(time_df) <- c('eid','t0','t1','t2','t3','death1','death2','birth_year','birth_month')

time_df$birth_date <- as.Date(paste(time_df$birth_year, time_df$birth_month, "1", sep = "-"), format = "%Y-%m-%d")
time_df$birth_year <- NULL
time_df$birth_month <- NULL
time_df$death_date <- ifelse(is.na(time_df$death1), time_df$death2, time_df$death1)

for (col in colnames(time_df)[-1]) {
  time_df[[col]] <- as.Date(time_df[[col]], origin = "1970-01-01")
}

time_df$death <- ifelse(is.na(time_df$death_date), 0, 1)

cov_df <- fread("Data/Cov/cov_imput.csv",data.table = FALSE)
base_cov <- c("Age","Sex","TDI","Ethnic_group","BMI",'education_level',"IPAQ",'Smoke','Alcohol_freq',
              "SBP","Diabetes")
cov_ad <- c(base_cov,'apoe_carrier',"prs_ad")
cov_pd <- c(base_cov,'prs_pd')
cov_stroke <- c(base_cov,'prs_stroke')
cov_depression <- c(base_cov,"PHQ4")

disease <- disease_lst[1]

infection <- Infection_lst[1]

for (disease in disease_lst) {
  
  cat("=== Processing disease:", disease, "===\n")
  
  res_df <- data.frame()
  save_path <- paste0("Results/s1_Association/Brain_disease")
  
  if (!dir.exists(save_path)) {
    dir.create(save_path, recursive = TRUE)
    cat("Created directory:", save_path, "\n")
  }
  
  file <- paste0(disease, "_cox.csv")
  save_file <- file.path(save_path, file)
  
  if (file.exists(save_file)) {
    cat("File already exists, skipping:", save_file, "\n")
    # next
  }
  
  if (disease %in% c("ACD","AD","VD")) {
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
    
    yy <- fread(paste0("Data/Brain_health/", disease, ".csv"), data.table = FALSE)
    xx <- fread(paste0("Data/Infection/Over/", infection, ".csv"), data.table = FALSE)
    
    xy <- merge(yy, xx, by = "eid")
    xy <- merge(xy, time_df, by = "eid")
    
    xy[[paste0(infection, "_date")]] <- as.Date(xy[[paste0(infection, "_date")]], origin = "1970-01-01")
    
    xy[["start"]] <- pmax(xy[["t0"]], xy[[paste0(infection, "_date")]], na.rm = TRUE)
    xy[[infection]] <- ifelse(is.na(xy[[infection]]), 0, xy[[infection]])
    xy[['end']] <- pmin(xy[["death_date"]], xy[[paste0(disease, "_date")]], as.Date("2025-08-01"), na.rm = TRUE)
    xy[['duration']] <- as.numeric(xy[['end']] - xy[['start']])
    
    xy <- subset(xy, duration > 0)
    
    cat("After filtering duration > 0, remaining rows:", nrow(xy), "\n")
    
    xy[['age_exact']] <- ifelse(xy[[infection]]==1, 
                                as.numeric(xy[[paste0(infection, "_date")]] - xy[['birth_date']]) / 365.25, 
                                as.numeric(xy[['t0']] - xy[['birth_date']]) / 365.25)
    xy$age_group <- cut(xy$age_exact, breaks = c(40, 50, 60, 70, 80), 
                        labels = c("40-50", "51-60", "61-70", "71-80"),include.lowest = TRUE)
    xy <- subset(xy, age_exact >= 40)

    xy <- subset(xy, select = c("eid", infection, disease, "duration",'age_exact'))
    
    tmp_df <- merge(xy, cov_df, by = "eid")
    
    cat("    -> Number of rows for analysis:", nrow(tmp_df), "\n")
    
    tmp_df <- tmp_df[!is.na(tmp_df[[infection]]) & !is.na(tmp_df[[disease]]) & !is.na(tmp_df$duration), ]
    tmp_res <- res_2class_cox(time = "duration", status = disease, Var.analys = infection, co = cov, data = tmp_df)
    
    if (is.null(res_df)) {
      res_df <- tmp_res
    } else {
      res_df <- rbind(res_df, tmp_res)
    }
    cat("    -> Completed infection:", infection, "\n")
  }
  
  res_df$P_BFI <- p.adjust(res_df$P_value, method = "bonferroni", n = 7*22)
  res_df$P_FDR <- p.adjust(res_df$P_value, method = "fdr")
  
  fwrite(res_df, save_file)
  cat("=== Finished disease:", disease, "Results saved to:", save_path, "===\n\n")
}