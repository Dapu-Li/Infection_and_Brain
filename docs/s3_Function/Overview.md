# Brain Function

## Brain Function Analysis

A total of 14 **brain functional measures** were included, comprising 10 cognitive tests and 4 movement assessments.

To investigate the associations between **22 infectious diseases** and these **14 brain functional measures**, linear regression models were performed.

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

All measures were first residualized for covariates and subsequently standardized prior to inclusion in the linear regression models.

Linear regression analyses were conducted using **R (version 4.3.1)**.

Multiple testing correction was performed using the **Bonferroni method**, with a correction factor of **22 infectious diseases × 14 brain functional measures**.

- *p* < 0.05 was considered nominally significant (*)  
- Bonferroni-corrected *p* < 0.05 was considered statistically significant (**)  

## Code for Brain Function Analysis

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
  
  result <- tryCatch({
    
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
    
    data.frame(
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
    
  }, error = function(e) {
    
    cat("Error in LinearReg_2class | y =", y, "| x =", x, "\n")
    cat("Message:", e$message, "\n\n")
    
    data.frame(
      Characteristics = y,
      Predictors = paste(x, levels(data[[x]])[-1], sep = "_"),
      N_total = N,
      N_x0 = n_x0,
      N_x1 = n_x1,
      Beta = "/",
      SE = "/",
      t_value = "/",
      Lower = "/",
      Upper = "/",
      `Coef_95CI` = "/",
      `P_value` = "/",
      stringsAsFactors = FALSE
    )
    
  })
  
  return(result)
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

func_code <- readxl::read_excel("Supplementary/Brain_function.xlsx")[1:4]
func_code <- subset(func_code, Category %in% c("Cognition", "Movement"))# ,'Neurobiomarker'
func_code_lst <- func_code$Phenotype
func_code_lst <- gsub("[()]", "", gsub("[ -]", "_", func_code_lst))
func_code_lst0 <- func_code[func_code$Instance == "0,3",]$Phenotype
func_code_lst2 <- func_code[func_code$Instance == "2,3",]$Phenotype
func_code_lst0 <- gsub("[()]", "", gsub("[ -]", "_", func_code_lst0))
func_code_lst2 <- gsub("[()]", "", gsub("[ -]", "_", func_code_lst2))


func_df <- fread("Data/Brain_health/Brain_function_over.csv")
colnames(func_df) <- gsub("[()]", "", gsub("[ -]", "_", colnames(func_df)))


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
              "SBP","Diabetes")
cov_ad <- c(base_cov,'apoe_carrier',"prs_ad")
cov_pd <- c(base_cov,'prs_pd')
cov_stroke <- c(base_cov,'prs_stroke')
cov_depression <- c(base_cov,"PHQ4")

cov <- base_cov

for (infection in Infection_lst) {
  
  cat("=== Processing infection:", infection, "===\n")
  
  res_df <- data.frame()
  save_path <- paste0("Results/s1_Association/Brain_function/", infection, ".csv")
  
  if (file.exists(save_path)) {
    cat("File already exists, skipping:", save_path, "\n")
    # next
  }
  
  xx <- fread(paste0("Data/Infection/Over/", infection, ".csv"), data.table = FALSE)
  xy <- merge(func_df, xx, by = "eid")
  xy <- merge(xy, time_df, by = "eid")
  
  xy[["start"]] <- pmax(xy[["t0"]], xy[[paste0(infection, "_date")]], na.rm = TRUE)
  xy[[infection]] <- ifelse(is.na(xy[[infection]]), 0, xy[[infection]])

  for (code in func_code_lst) {
    
    cat("  - Processing Brain_structure:", code, "\n")
    
    if (code %in% func_code_lst0) {
      xy[['end']] <- pmin(xy[["death_date"]], xy[['t0']], na.rm = TRUE)
      xy[['duration']] <- as.numeric(xy[['end']] - xy[['start']])
      
      xy[[infection]]<- ifelse(xy[[infection]] == 1 & xy[['duration']] < 0, 0, xy[[infection]])
      
      xy[['age_baseline']] <- as.numeric(xy[['t0']] - xy[['birth_date']]) / 365.25
    } else if (code %in% func_code_lst2) {
      xy[['end']] <- pmin(xy[["death_date"]], xy[['t2']], na.rm = TRUE)
      xy[['duration']] <- as.numeric(xy[['end']] - xy[['start']])
      
      xy[[infection]]<- ifelse(xy[[infection]] == 1 & xy[['duration']] < 0, 0, xy[[infection]])
      
      xy[['age_baseline']] <- as.numeric(xy[['t2']] - xy[['birth_date']]) / 365.25
    } else {
      cat("    -> Code not found in func_code_lst, skipping:", code, "\n")
      next
    }
    
    xy$age_group <- cut(xy$age_baseline,breaks = c(-Inf, 50, 60, 70, Inf),labels = c("40-50", "51-60", "61-70", "71-80"))
    
    tmp_df <- subset(xy, select = c("eid", infection, code,'age_baseline','age_group'))
    tmp_df <- tmp_df[!is.na(tmp_df[[code]]), ]
    fml <- paste0(code," ~ age_baseline")
    tmp_df[[code]] <- residuals(lm(as.formula(fml), data = tmp_df)) 
    tmp_df[[code]] <- scale(tmp_df[[code]], center = TRUE, scale = TRUE)
    cat("    -> Number of rows after removing NA for brain function:", nrow(tmp_df), "\n")
    
    tmp_df <- merge(tmp_df, cov_df, by = "eid")
    
    tmp_res <- res_2class_lm(y = code, Var.analys = infection, co = cov, data = tmp_df)
    
    if (is.null(res_df)) {
      res_df <- tmp_res
    } else {
      res_df <- rbind(res_df, tmp_res)
    }
    cat("    -> Completed infection:", infection, "\n")
  }
  
  res_df$P_BFI <- p.adjust(res_df$P_value, method = "bonferroni", n = 19*22)
  res_df$P_FDR <- p.adjust(res_df$P_value, method = "fdr")
  fwrite(res_df, save_path)
  
  cat("=== Finished infection:", infection, "Results saved to:", save_path, "===\n\n")
}
