# Brain Structure

## Brain Structure Analysis

A total of 84 **brain structural measures** were derived based on the Desikan–Killiany atlas, including 68 cortical volumes and 16 subcortical volumes.

To investigate the associations between **22 infectious diseases** and these **84 brain structural measures**, linear regression models were performed.

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


## Statistical Analysis

All brain structural measures were first residualized for covariates and subsequently standardized prior to inclusion in the linear regression models.

Linear regression analyses were conducted using **R (version 4.3.1)**.

Multiple testing correction was performed using the **false discovery rate (FDR) method**.

- *p* < 0.05 was considered nominally significant (*)  
- FDR-corrected *p* < 0.05 was considered statistically significant (**)  

## Code for Brain Structure Analysis

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

bs_code <- fread("Supplementary/draw_IDPs.csv")
colnames(bs_code)
bs_code_lst <- bs_code$Abbreviation

bs_df <- fread("Data/Draw_structure/Draw_structure2.csv")
colnames(bs_df) <- gsub("-2.0", "", colnames(bs_df))

name_mapping <- setNames(bs_code$Abbreviation, as.character(bs_code$Field_ID))

bs_df_colnames <- colnames(bs_df)

bs_df_colnames_mapped <- ifelse(
  bs_df_colnames %in% names(name_mapping),
  name_mapping[bs_df_colnames],
  bs_df_colnames
)
colnames(bs_df) <- bs_df_colnames_mapped

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
base_cov <- c("Sex","TDI","Ethnic_group","BMI",'education_level',"IPAQ",'Smoke','Alcohol_freq',
              "SBP","Diabetes") # 
cov_ad <- c(base_cov,'apoe_carrier',"prs_ad")
cov_pd <- c(base_cov,'prs_pd')
cov_stroke <- c(base_cov,'prs_stroke')
cov_depression <- c(base_cov,"PHQ4")


bs <- bs_code_lst[1]

infection <- Infection_lst[1]
cov <- base_cov
infection <- 'Bacterial_Infections_with_Sepsis'
for (infection in Infection_lst) {
  
  cat("=== Processing infection:", infection, "===\n")
  
  res_df <- data.frame()
  save_path <- paste0("Results/s1_Association/Brain_structure/", infection, ".csv")
  
  if (file.exists(save_path)) {
    cat("File already exists, skipping:", save_path, "\n")
    next
  }
  
  xx <- fread(paste0("Data/Infection/Over/", infection, ".csv"), data.table = FALSE)
  xy <- merge(bs_df, xx, by = "eid")
  xy <- merge(xy, time_df, by = "eid")
  
  xy[["start"]] <- pmax(xy[["t0"]], xy[[paste0(infection, "_date")]], na.rm = TRUE)
  xy[[infection]] <- ifelse(is.na(xy[[infection]]), 0, xy[[infection]])
  xy[['end']] <- pmin(xy[["death_date"]], xy[['t2']], na.rm = TRUE)
  xy[['duration']] <- as.numeric(xy[['end']] - xy[['start']])

  xy[[infection]]<- ifelse(xy[[infection]] == 1 & xy[['duration']] < 0, 0, xy[[infection]])
  
  xy[['age_infection']] <- ifelse(xy[[infection]]==1, 
                                  as.numeric(xy[[paste0(infection, "_date")]] - xy[['birth_date']]) / 365.25, 
                                  as.numeric(xy[['t0']] - xy[['birth_date']]) / 365.25)
  
  xy[['age_start']] <- as.numeric(xy[['start']] - xy[['birth_date']]) / 365.25
  xy[['age_end']] <- as.numeric(xy[['end']] - xy[['birth_date']]) / 365.25
  xy$age_baseline <- as.numeric(xy$t2 - xy$birth_date) / 365.25
  
  xy$age_group <- cut(xy$age_baseline,
                      breaks = c(-Inf, 50, 60, 70, Inf),
                      labels = c("40-50", "51-60", "61-70", "71-80"))

  for (bs in bs_code_lst) {
    
    cat("  - Processing Brain_structure:", bs, "\n")
    
    tmp_df <- subset(xy, select = c("eid", infection, bs,'age_baseline','age_group'))
    tmp_df <- tmp_df[!is.na(tmp_df[[bs]]) & !is.na(tmp_df[[infection]]), ]
    fml <- paste0(bs," ~ age_baseline")
    tmp_df[[bs]] <- residuals(lm(as.formula(fml), data = tmp_df)) 
    tmp_df[[bs]] <- scale(tmp_df[[bs]])
    cat("    -> Number of rows after removing NA for brain structure:", nrow(tmp_df), "\n")
    
    tmp_df <- merge(tmp_df, cov_df, by = "eid")
    
    tmp_res <- res_2class_lm(y = bs, Var.analys = infection, co = cov, data = tmp_df)
    
    if (is.null(res_df)) {
      res_df <- tmp_res
    } else {
      res_df <- rbind(res_df, tmp_res)
    }
    cat("    -> Completed infection:", infection, "\n")
  }
  
  res_df$P_BFI <- p.adjust(res_df$P_value, method = "bonferroni", n = 84*22)
  res_df$P_FDR <- p.adjust(res_df$P_value, method = "fdr")
  fwrite(res_df, save_path)
  
  cat("=== Finished infection:", infection, "Results saved to:", save_path, "===\n\n")
}
