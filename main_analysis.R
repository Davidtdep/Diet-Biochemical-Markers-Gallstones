###############################################################################
# 1. LIBRARIES
###############################################################################
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(broom)
library(pROC)
library(combinat)
library(forestplot)
library(grid)
library(parallel)










###############################################################################
# 2. IMPORT AND PREPARE DATA
###############################################################################

# Read the dataset (Excel file)
data <- read_excel("~/Desktop/gallstones/data/data.xlsx")

# Replace binary values with qualitative labels
# (Assumes sex=0 => Female, sex=1 => Male; residenceType=0 => Rural, 1 => Urban)
data$sex <- ifelse(data$sex == 0, "Female", "Male")
data$residenceType <- ifelse(data$residenceType == 0, "Rural", "Urban")

# Convert multiple columns from "Yes"/"No" or 0/1 to factors as required:
data <- data %>%
  mutate(
    sex       = as.factor(sex),
    # Markers:
    elevatedAST             = as.factor(elevatedAST),
    elevatedALT             = as.factor(elevatedALT),
    elevatedTotalBilirrubin = as.factor(elevatedTotalBilirrubin),
    elevatedDirectBilirrubin= as.factor(elevatedDirectBilirrubin),
    elevatedIndirectBilirrubin = as.factor(elevatedIndirectBilirrubin),
    elevatedALP             = as.factor(elevatedALP),
    elevatedCholesterol     = as.factor(elevatedCholesterol),
    elevatedTriglycerides   = as.factor(elevatedTriglycerides),
    elevatedAmylase         = as.factor(elevatedAmylase),
    
    # Other categorical variables:
    alcoholConsumer              = as.factor(alcoholConsumer),
    obesity                      = as.factor(obesity),
    ancestry                     = as.factor(ancestry),
    educationLevel               = as.factor(educationLevel),
    daiebtes                     = as.factor(daiebtes),
    htn                          = as.factor(htn),
    healthScheme                 = as.factor(healthScheme),
    residenceType                = as.factor(residenceType),
    alcoholConsumptionFrequency  = as.factor(alcoholConsumptionFrequency),
    exercise                     = as.factor(exercise),
    # For sub-analysis:
    gallstonesFamilyHistory      = as.factor(gallstonesFamilyHistory)
  )

# If you need to save this processed version:
# write.xlsx(data, "data_processed.xlsx")










###############################################################################
# 3. ANALYSIS OF FOOD CATEGORIES AND MARKERS
###############################################################################
#
#   This section analyzes the relationship between weekly food consumption 
#   variables and different markers (e.g., elevatedAST, elevatedALT, etc.).
#   A logistic regression is run for each marker and each food variable, 
#   optionally adjusting for covariates. Then, results are stored for each marker.
#
###############################################################################

# List of food variables
food_vars <- c(
  "Weekly_Consumption_cerealAndTubers", "Weekly_Consumption_Grains", 
  "Weekly_Consumption_Vegetables", "Weekly_Consumption_Fruits", 
  "Weekly_Consumption_juices", "Weekly_Consumption_beefOrPork",
  "Weekly_Consumption_chickenOrFish", "Weekly_Consumption_Eggs", 
  "Weekly_Consumption_dairyProducts", "Weekly_Consumption_processedMeats", 
  "Weekly_Consumption_fastFood", "Weekly_Consumption_snackFoods",
  "Weekly_Consumption_sweetsOrDesserts", "Weekly_Consumption_cannedFoods", 
  "Weekly_Consumption_coffee"
)

# List of dependent variables (markers)
outcomes <- c(
  "elevatedAST", "elevatedALT", "elevatedTotalBilirrubin", "elevatedDirectBilirrubin",
  "elevatedIndirectBilirrubin", "elevatedALP", "elevatedCholesterol",
  "elevatedTriglycerides", "elevatedAmylase"
)

# Covariates to adjust for in the logistic regression
adjustment_vars <- c("age", "sex", "obesity", "educationLevel", "healthScheme",
                     "alcoholConsumer", "bmi", "exercise")

# Function to run logistic regression for each food-outcome pair
# and optionally adjust for covariates
analyze_foods_by_outcome <- function(data, food_vars, outcomes, adjustment_vars = NULL) {
  for (outcome in outcomes) {
    # Convert the outcome to a binary factor (1=Yes, 0=No)
    data <- data %>%
      mutate(!!outcome := as.factor(ifelse(.data[[outcome]] == "Yes", 1, 0)))
    
    # Empty data frame to store results
    results <- data.frame()
    
    for (food in food_vars) {
      # Create the formula dynamically
      if (is.null(adjustment_vars) || length(adjustment_vars) == 0) {
        formula <- as.formula(paste(outcome, "~", food))
      } else {
        formula <- as.formula(
          paste(outcome, "~", paste(c(adjustment_vars, food), collapse = " + "))
        )
      }
      
      # Fit the logistic regression model
      model <- glm(formula, data = data, family = binomial)
      
      # Extract coefficients, OR, CIs, p-value for the food term
      model_summary <- tidy(model) %>%
        filter(term == food) %>%  # only the coefficient for the food variable
        mutate(
          OR        = exp(estimate),
          lower_CI  = tryCatch(exp(confint(model, parm = food)[1]), error = function(e) NA),
          upper_CI  = tryCatch(exp(confint(model, parm = food)[2]), error = function(e) NA)
        ) %>%
        select(term, estimate, OR, lower_CI, upper_CI, p.value)
      
      # Combine with results
      results <- bind_rows(results, model_summary)
    }
    
    # Rename columns for clarity
    colnames(results) <- c("Food", "Log_Odds", "Odds_Ratio", "Lower_CI", "Upper_CI", "P_Value")
    
    # Save the results in the global environment with a dynamic name
    assign(paste0("results_", outcome), results, envir = .GlobalEnv)
  }
}

# Execute the function
analyze_foods_by_outcome(data, food_vars, outcomes, adjustment_vars)










###############################################################################
# 3.1 SAVE THE RESULTS AS SUPPLEMENTARY MATERIAL 1
###############################################################################
#
#   Here, we store the results of each outcome in an Excel workbook with 
#   multiple sheets (one per marker).
#
###############################################################################

# List data frames for each outcome
dataframes <- list(
  "ALP"               = results_elevatedALP,
  "ALT"               = results_elevatedALT,
  "Amylase"           = results_elevatedAmylase,
  "AST"               = results_elevatedAST,
  "Cholesterol"       = results_elevatedCholesterol,
  "DirectBilirrubin"  = results_elevatedDirectBilirrubin,
  "IndirectBilirrubin"= results_elevatedIndirectBilirrubin,
  "TotalBilirrubin"   = results_elevatedTotalBilirrubin,
  "Triglycerides"     = results_elevatedTriglycerides
)

# Create a new workbook
wb <- createWorkbook()

# Add each data frame to the workbook in a separate sheet
for (sheet_name in names(dataframes)) {
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, dataframes[[sheet_name]])
}

# Save the Excel workbook
saveWorkbook(wb, "Supplementary Material 1.xlsx", overwrite = TRUE)
cat("Excel file saved as 'Supplementary Material 1.xlsx'.\n")










###############################################################################
# 3.2 EXTRACT SIGNIFICANT RESULTS
###############################################################################
#
#   This part filters out the significant results (p < 0.05) from the 
#   previously saved results for each marker, combines them, and optionally 
#   applies additional filters (e.g., removing OR > 20).
#
###############################################################################

# Create a list of data frames from the global environment
dataframes <- list(
  "ALP"               = results_elevatedALP,
  "ALT"               = results_elevatedALT,
  "Amylase"           = results_elevatedAmylase,
  "AST"               = results_elevatedAST,
  "Cholesterol"       = results_elevatedCholesterol,
  "DirectBilirrubin"  = results_elevatedDirectBilirrubin,
  "IndirectBilirrubin"= results_elevatedIndirectBilirrubin,
  "TotalBilirrubin"   = results_elevatedTotalBilirrubin,
  "Triglycerides"     = results_elevatedTriglycerides
)

# Empty list to store significant results
significant_results <- list()

# Iterate over each data frame to filter significant p-values (< 0.05)
for (enzyme in names(dataframes)) {
  df <- dataframes[[enzyme]]
  sig <- df[df$P_Value < 0.05, ]
  
  if (nrow(sig) > 0) {
    sig$Enzyme <- enzyme
    significant_results[[enzyme]] <- sig
  }
}

# Combine all significant results into one data frame
significant_results_general <- do.call(rbind, significant_results)

# Remove rows with NA
significant_results_general <- significant_results_general[complete.cases(significant_results_general), ]

# Remove rows with Odds_Ratio > 20
significant_results_general <- significant_results_general[significant_results_general$Odds_Ratio < 20, ]

# If you only want bilirubin markers
significant_results_general <- significant_results_general[
  grepl("Bilirrubin", significant_results_general$Enzyme), 
]

# Optionally, save as Excel
# write.xlsx(significant_results_general, "significant_results.xlsx")










###############################################################################
# 3.3 FOREST PLOT FOR SIGNIFICANT RESULTS
###############################################################################
#
#   This section creates a forest plot of the significant results.
#
###############################################################################

# Use forestplot to visualize the odds ratios
significant_results_general <- significant_results_general %>%
  mutate(
    Clean_Food = stringr::str_replace(Food, "Weekly_Consumption_", ""), 
    Clean_Food = stringr::str_replace_all(Clean_Food, "([a-z])([A-Z])", "\\1 \\2")
  )

# Create a table for plotting
table_data <- significant_results_general %>%
  select(Clean_Food, Enzyme, Odds_Ratio, Lower_CI, Upper_CI) %>%
  arrange(Clean_Food, desc(Odds_Ratio))

table_labels <- table_data %>%
  mutate(Label = paste0(Clean_Food, " (", Enzyme, ")"))

# Create the forest plot
forestplot(
  labeltext     = table_labels$Label,
  mean          = table_data$Odds_Ratio,
  lower         = table_data$Lower_CI,
  upper         = table_data$Upper_CI,
  xlog          = TRUE,  # Log scale for the OR
  col           = fpColors(box = "black", line = "black", summary = "darkblue"),
  ci.vertices   = TRUE,
  xticks        = c(0.5, 1, 2, 5, 10),
  boxsize       = 0.2,
  txt_gp        = fpTxtGp(
    label = gpar(cex = 0.8),
    ticks = gpar(cex = 0.8)
  )
)










###############################################################################
# 3.4 IDENTIFY THE BEST COMBINATION OF COVARIABLES FOR ADJUSTMENT (FOOD ANALYSIS)
###############################################################################
#
#   In this section, we systematically test all possible combinations of 
#   candidate covariates, counting how many models show significant p-values 
#   for the food variable (p < 0.05).
#   We use parallelization to speed up the process.
#
###############################################################################

# Candidate covariates
adjustment_candidates_food <- c("age", "sex", "obesity", "ancestry", "residenceType",
                                "educationLevel", "healthScheme", "alcoholConsumer",
                                "bmi", "alcoholConsumptionFrequency", "exercise")

# Function to evaluate all combinations of covariates
evaluate_adjustment_vars <- function(data, food_vars, outcomes, adjustment_candidates) {
  
  # Generate all possible subsets of adjustment candidates (including empty set)
  adjustment_combinations <- lapply(0:length(adjustment_candidates), function(i) {
    combn(adjustment_candidates, i, simplify = FALSE)
  }) %>% unlist(recursive = FALSE)
  
  # Setup parallelization
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  # Load necessary packages on each core
  clusterEvalQ(cl, {
    library(dplyr)
    library(broom)
  })
  
  # Export objects to the cluster
  clusterExport(
    cl, 
    varlist = c("data", "food_vars", "outcomes", "adjustment_combinations", "%>%"), 
    envir = environment()
  )
  
  # Parallel loop
  results_parallel <- parLapply(cl, adjustment_combinations, function(vars) {
    # Create a label for the current combination of covariates
    adjustment_label <- if (length(vars) == 0) "None" else paste(vars, collapse = ", ")
    
    significant_count <- 0
    
    for (outcome in outcomes) {
      data <- data %>%
        mutate(!!outcome := as.factor(ifelse(.data[[outcome]] == "Yes", 1, 0)))
      
      for (food in food_vars) {
        # Build formula dynamically
        if (length(vars) == 0) {
          formula <- as.formula(paste(outcome, "~", food))
        } else {
          formula <- as.formula(
            paste(outcome, "~", paste(c(vars, food), collapse = " + "))
          )
        }
        
        # Fit logistic regression
        model <- glm(formula, data = data, family = binomial)
        
        # Extract p-value for the food variable
        p_value <- tidy(model) %>%
          filter(term == food) %>%
          pull(p.value)
        
        # Count if p-value < 0.05
        if (!is.na(p_value) && p_value < 0.05) {
          significant_count <- significant_count + 1
        }
      }
    }
    # Return a list for this combination
    list(Adjustment_Vars = adjustment_label, Significant_Count = significant_count)
  })
  
  stopCluster(cl)
  
  # Convert to a data frame
  results_df <- do.call(rbind, lapply(results_parallel, as.data.frame))
  return(results_df)
}

# Run the evaluation for the food analysis
start_time <- Sys.time()
adjustment_results_food <- evaluate_adjustment_vars(
  data,
  food_vars,
  outcomes,
  adjustment_candidates_food
)
end_time <- Sys.time()

execution_time <- end_time - start_time
cat("Analysis took", execution_time, "to complete.\n")

# Order by the number of significant models
adjustment_results_food <- adjustment_results_food %>%
  arrange(desc(Significant_Count))

# Show results
print(adjustment_results_food)

# Optionally save
# write.csv(adjustment_results_food, "adjustment_results_food.csv")










###############################################################################
# 4. ANALYSIS OF COOKING METHODS AND ADDITIVES
###############################################################################
#
#   This section investigates how cooking methods (e.g. Fried, Boiled, etc.) 
#   and certain additives (e.g., Mayonnaise, Mustard) relate to each marker.
#   We also look for the best covariate combinations similarly.
#
###############################################################################

# Create new columns based on specific cooking oils
data$Lard <- ifelse(data$oil == "Lard", "Yes", "No")
data$VegetableOil <- ifelse(data$oil == "Vegetable oil", "Yes", "No")

# Convert to factors
data$Lard          <- factor(data$Lard, levels = c("Yes", "No"))
data$VegetableOil  <- factor(data$VegetableOil, levels = c("Yes", "No"))

# Cooking methods and additives
cookingAndAdditives <- c(
  "Roasted", "Fried", "Boiled", "Steamed", "Stewed", 
  "Lard", "VegetableOil", "Cumin", "Maggi", "Mayonnaise", 
  "Mustard", "None", "hotPepper"
)

# Covariates to adjust (can be empty if you do not want adjustment)
adjust_vars_cooking <- c("age", "obesity", "educationLevel", 
                         "healthScheme", "bmi", "alcoholConsumptionFrequency")

# For each outcome, fit a logistic regression with each cooking/additive var
for (outcome in outcomes) {
  temp_results <- list()
  
  for (cooking in cookingAndAdditives) {
    # Build formula
    if (length(adjust_vars_cooking) > 0) {
      formula_str <- paste(outcome, "~", cooking, "+", paste(adjust_vars_cooking, collapse = " + "))
    } else {
      formula_str <- paste(outcome, "~", cooking)
    }
    
    # Fit model
    model <- tryCatch(
      glm(as.formula(formula_str), data = data, family = binomial),
      error = function(e) NULL
    )
    
    # If the model is valid
    if (!is.null(model)) {
      model_summary <- summary(model)
      coefficients  <- model_summary$coefficients
      cooking_var   <- grep(cooking, rownames(coefficients), value = TRUE)
      
      if (length(cooking_var) > 0) {
        log_odds  <- coefficients[cooking_var, "Estimate"]
        std_error <- coefficients[cooking_var, "Std. Error"]
        p_value   <- coefficients[cooking_var, "Pr(>|z|)"]
        
        odds_ratio <- exp(log_odds)
        lower_ci   <- exp(log_odds - 1.96 * std_error)
        upper_ci   <- exp(log_odds + 1.96 * std_error)
        
        # Store in a small data frame
        temp_results[[cooking]] <- data.frame(
          CookingAndAdditive = cooking,
          Log_Odds           = log_odds,
          Odds_Ratio         = odds_ratio,
          Lower_CI           = lower_ci,
          Upper_CI           = upper_ci,
          P_Value            = p_value
        )
      }
    }
  }
  # Combine results
  if (length(temp_results) > 0) {
    assign(
      paste0("results_", outcome), 
      do.call(rbind, temp_results), 
      envir = .GlobalEnv
    )
  } else {
    assign(
      paste0("results_", outcome),
      data.frame(
        CookingAndAdditive = character(0),
        Log_Odds           = numeric(0),
        Odds_Ratio         = numeric(0),
        Lower_CI           = numeric(0),
        Upper_CI           = numeric(0),
        P_Value            = numeric(0)
      ),
      envir = .GlobalEnv
    )
  }
}

# Example checks:
# model <- glm(elevatedTriglycerides ~ Boiled, data = data, family = binomial)
# summary(model)
# model <- glm(elevatedTotalBilirrubin ~ Cumin, data = data, family = binomial)
# summary(model)










###############################################################################
# 4.1 BEST COVARIATE COMBINATION FOR COOKING METHODS/ADDITIVES
###############################################################################
#
#   Similar approach to the food analysis: we try all combinations of 
#   candidate covariates and count how many significant models we get.
#
###############################################################################

adjustment_candidates_cooking <- c("age", "sex", "obesity", "ancestry", "residenceType",
                                   "educationLevel", "healthScheme", "alcoholConsumer",
                                   "bmi", "alcoholConsumptionFrequency", "exercise")

# Generate all subsets of candidates
covariate_combinations <- list()
for (i in 0:length(adjustment_candidates_cooking)) {
  covariate_combinations <- c(
    covariate_combinations, 
    combn(adjustment_candidates_cooking, i, simplify = FALSE)
  )
}

# Parallel function
run_models <- function(adjust_vars) {
  significant_count <- 0
  
  for (outcome in outcomes) {
    for (cooking in cookingAndAdditives) {
      if (length(adjust_vars) > 0) {
        formula_str <- paste(outcome, "~", cooking, "+", paste(adjust_vars, collapse = " + "))
      } else {
        formula_str <- paste(outcome, "~", cooking)
      }
      
      model <- tryCatch(
        glm(as.formula(formula_str), data = data, family = binomial),
        error = function(e) NULL
      )
      
      if (!is.null(model)) {
        coef_matrix <- summary(model)$coefficients
        cooking_var <- grep(cooking, rownames(coef_matrix), value = TRUE)
        
        if (length(cooking_var) > 0) {
          p_value <- coef_matrix[cooking_var, "Pr(>|z|)"]
          if (p_value < 0.05) {
            significant_count <- significant_count + 1
          }
        }
      }
    }
  }
  
  data.frame(
    Covariate_Combination  = ifelse(length(adjust_vars) > 0, 
                                    paste(adjust_vars, collapse = ", "), 
                                    "No adjustment"),
    Significant_Models_Count = significant_count
  )
}

num_cores <- detectCores() - 1
results_list <- mclapply(covariate_combinations, run_models, mc.cores = num_cores)
results_summary_cooking <- do.call(rbind, results_list)

# Sort by significant models
results_summary_cooking <- results_summary_cooking[order(-results_summary_cooking$Significant_Models_Count), ]

# Check or save the results
print(results_summary_cooking)
# write.xlsx(results_summary_cooking, "results_summary_cookingMethods.xlsx")










###############################################################################
# 4.2 FISHER TEST FOR COOKING/ADDITIVES VS. MARKERS
###############################################################################
#
#   This section uses Fisher's exact test for each cooking/additive 
#   variable vs. each marker, looking for significant associations. 
#   A table is built for each outcome with the resulting OR, CI, and p-value.
#
###############################################################################

for (outcome in outcomes) {
  temp_results <- list()
  
  for (cooking in cookingAndAdditives) {
    contingency_table <- table(data[[cooking]], data[[outcome]])
    
    # We only run Fisher's test if all table cells are > 0
    if (all(contingency_table > 0)) {
      fisher_test <- fisher.test(contingency_table)
      
      temp_results[[cooking]] <- data.frame(
        CookingAndAdditive = cooking,
        Odds_Ratio         = fisher_test$estimate,
        Lower_CI           = fisher_test$conf.int[1],
        Upper_CI           = fisher_test$conf.int[2],
        P_Value            = fisher_test$p.value
      )
    }
  }
  
  if (length(temp_results) > 0) {
    assign(
      paste0("results_Fisher_", outcome),
      do.call(rbind, temp_results),
      envir = .GlobalEnv
    )
  } else {
    assign(
      paste0("results_Fisher_", outcome),
      data.frame(
        CookingAndAdditive = character(0),
        Odds_Ratio         = numeric(0),
        Lower_CI           = numeric(0),
        Upper_CI           = numeric(0),
        P_Value            = numeric(0)
      ),
      envir = .GlobalEnv
    )
  }
}

ls(pattern = "results_Fisher_")










###############################################################################
# 5. ANALYSIS OF ALCOHOL CONSUMPTION MEASURES
###############################################################################
#
#   This section examines alcohol consumption in different ways 
#   (alcoholConsumer, alcohol_total_volume_ml, alcoholConsumptionFrequency) 
#   and their relationship with each marker, adjusting for selected covariates.
#
###############################################################################

predictors_alcohol <- c("alcoholConsumer", "alcohol_total_volume_ml", "alcoholConsumptionFrequency")

# We reuse 'outcomes' from before, but you can define again if needed
# outcomes <- c(...)

# Covariates (change or set to character(0) if no adjustment)
covariates_alcohol <- c("obesity", "educationLevel", "bmi", "exercise")

# Store results in a list
results_dfs_alcohol <- list()

for (outcome in outcomes) {
  results_list <- list()
  
  for (predictor in predictors_alcohol) {
    if (length(covariates_alcohol) > 0) {
      formula_str <- paste(outcome, "~", predictor, "+", paste(covariates_alcohol, collapse = " + "))
    } else {
      formula_str <- paste(outcome, "~", predictor)
    }
    
    model <- glm(as.formula(formula_str), data = data, family = binomial)
    result <- tidy(model)
    
    # Filter predictor terms
    result <- result[grepl(predictor, result$term), ]
    
    # Calculate OR and CIs
    result <- result %>%
      mutate(
        Log_Odds   = estimate,
        Odds_Ratio = exp(estimate),
        Lower_CI   = exp(estimate - 1.96 * std.error),
        Upper_CI   = exp(estimate + 1.96 * std.error),
        P_Value    = p.value
      ) %>%
      select(term, Log_Odds, Odds_Ratio, Lower_CI, Upper_CI, P_Value)
    
    result$predictor <- predictor
    results_list[[predictor]] <- result
  }
  
  results_df <- do.call(rbind, results_list)
  assign(paste0("results_", outcome, "_alcohol"), results_df, envir = .GlobalEnv)
  results_dfs_alcohol[[outcome]] <- results_df
}

ls(pattern = "results_.*_alcohol")










###############################################################################
# 5.1 BEST COVARIATE COMBINATION FOR ALCOHOL MODELS
###############################################################################
#
#   Similar approach: test different covariate combinations and see how many
#   significant models we get for each predictor of alcohol consumption.
#
###############################################################################

predictors_alcohol <- c("alcoholConsumer", "alcohol_total_volume_ml", "alcoholConsumptionFrequency")
adjustment_candidates_alcohol <- c("age", "sex", "obesity", "ancestry", "residenceType",
                                   "educationLevel", "healthScheme", "bmi", "exercise")

# Generate all subsets of these covariates
covariate_combinations_alcohol <- list(character(0))
for (k in 1:length(adjustment_candidates_alcohol)) {
  covariate_combinations_alcohol <- c(
    covariate_combinations_alcohol, 
    combn(adjustment_candidates_alcohol, k, simplify = FALSE)
  )
}

evaluate_combination <- function(covariates) {
  significant_count <- 0
  
  for (outcome in outcomes) {
    for (predictor in predictors_alcohol) {
      if (length(covariates) > 0) {
        formula_str <- paste(outcome, "~", predictor, "+", paste(covariates, collapse = " + "))
      } else {
        formula_str <- paste(outcome, "~", predictor)
      }
      
      model   <- glm(as.formula(formula_str), data = data, family = binomial)
      results <- tidy(model)
      # Filter only terms with the predictor
      results <- results[grepl(predictor, results$term), ]
      
      # If any p-value < 0.05, count it
      if (any(results$p.value < 0.05, na.rm = TRUE)) {
        significant_count <- significant_count + 1
      }
    }
  }
  
  list(
    covariates_used    = paste(covariates, collapse = ", "), 
    significant_models = significant_count
  )
}

num_cores <- detectCores() - 1
results_parallel_alcohol <- mclapply(
  covariate_combinations_alcohol, 
  evaluate_combination, 
  mc.cores = num_cores
)

results_df_alcohol <- do.call(rbind, lapply(results_parallel_alcohol, as.data.frame))
results_df_alcohol <- results_df_alcohol[order(-results_df_alcohol$significant_models), ]
print(results_df_alcohol)










###############################################################################
# 5.2 SUB-ANALYSIS BY GALLSTONE FAMILY HISTORY (ALCOHOL CONSUMPTION)
###############################################################################
#
#   This part splits the dataset based on gallstone family history (Yes/No)
#   and runs the same alcohol consumption models in each subset.
#
###############################################################################

# Two subsets: those with gallstone family history and those without
data_fam_yes <- data %>% filter(gallstonesFamilyHistory == "Yes")
data_fam_no  <- data %>% filter(gallstonesFamilyHistory == "No")

# Covariates (can be changed or left empty)
covariates_subanalysis <- character(0)

# Function to run models in each subset
run_models_subanalysis <- function(dataset, suffix) {
  for (outcome in outcomes) {
    results_list <- list()
    
    for (predictor in predictors_alcohol) {
      # Build formula
      if (length(covariates_subanalysis) > 0) {
        formula_str <- paste(outcome, "~", predictor, "+", paste(covariates_subanalysis, collapse = " + "))
      } else {
        formula_str <- paste(outcome, "~", predictor)
      }
      
      model   <- glm(as.formula(formula_str), data = dataset, family = binomial)
      results <- tidy(model)
      
      # Filter to predictor terms
      results <- results[grepl(predictor, results$term), ]
      
      # Calculate OR and CI
      results <- results %>%
        mutate(
          Log_Odds   = estimate,
          Odds_Ratio = exp(estimate),
          Lower_CI   = exp(estimate - 1.96 * std.error),
          Upper_CI   = exp(estimate + 1.96 * std.error),
          P_Value    = p.value
        ) %>%
        select(term, Log_Odds, Odds_Ratio, Lower_CI, Upper_CI, P_Value)
      
      results$predictor <- predictor
      results_list[[predictor]] <- results
    }
    
    # Combine results
    final_df <- do.call(rbind, results_list)
    assign(paste0("results_", outcome, "_alcohol_", suffix), final_df, envir = .GlobalEnv)
  }
}

# Run models separately for each group
run_models_subanalysis(data_fam_yes, "FamYes")
run_models_subanalysis(data_fam_no,  "FamNo")

# Check the newly created data frames
ls(pattern = "results_.*_Fam(Yes|No)")