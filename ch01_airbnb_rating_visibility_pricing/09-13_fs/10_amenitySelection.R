amen <- dbReadTable(iaan, "DATA.FULL_SAMPLE") %>%
  select(year, month, price, starts_with("AC")) %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(real_price = (price/cpi_b2015)*100) %>%
  select(-cpi, -cpi_b2015, -year, -month) %>%
  select(-accom_rating)

price <- amen$price
real_price <- amen$real_price
amen <- amen %>% 
  select(starts_with("AC")) %>%
  as.matrix()

#Nominal Price------------------------------------------------------------------

# Set up parameters
num_samples <- 100      # Number of bootstrap samples
sample_fraction <- 0.4  # Fraction of data to sample each time

# Initialize a matrix to store selected features
feature_counts <- matrix(0, nrow = dim(amen)[2], ncol = num_samples)
colnames(feature_counts) <- paste0("Sample_", 1:num_samples)
rownames(feature_counts) <- colnames(amen)

set.seed(123)           # For reproducibility
# Ensemble with bootstrap sampling
for (i in 1:num_samples) {
  # Randomly sample a fraction of the data
  sample_indices <- sample(1:nrow(amen), size = 
                             round(sample_fraction * nrow(amen)), replace = TRUE)
  x_sample <- amen[sample_indices, ]
  y_sample <- price[sample_indices]
  
  # Fit Lasso model
  lasso_model <- cv.glmnet(x_sample, y_sample, alpha = 1)
  rm(x_sample, y_sample)
  best_lambda <- lasso_model$lambda.min
  
  # Get selected features for this sample
  selected_features <- coef(lasso_model, s = best_lambda)
  rm(lasso_model, best_lambda)
  selected_features <- (as.matrix(selected_features) %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    filter(s1 != 0) %>%
    filter(rowname != "(Intercept)"))$rowname
  
  # Record selected features
  feature_counts[rownames(feature_counts) %in% selected_features, i] <- 1
  
  gc()
  print(i)
  
}

dbWriteTable(iaan,
             "MODEL.PRICE_AMENITY_FEATURE_COUNTS",
             feature_counts %>%
               as.data.frame() %>%
               rownames_to_column(var = "Amenity"),
             overwrite = T)

# Calculate feature importance by summing across samples
feature_importance <- rowSums(feature_counts) / num_samples

#Determine Optimal Threshold for Keeping by examining R-Squared and F-Statistics
#at difference thresholds
thresholds <- seq(0.1, 1, by = 0.1)
rsq <- vector()
arsq <- vector()
fstat <- vector()
for(i in 1:length(thresholds)){
  
  selected_features_ensemble <- names(
    feature_importance[feature_importance > thresholds[i]-0.01])
 reg <- lm(price ~ amen[, selected_features_ensemble])
 rsq[i] <- summary(reg)$r.squared
 arsq[i] <- summary(reg)$adj.r.squared
 fstat[i] <- summary(reg)$fstatistic[1]
 rm(reg)
 gc()
 
}

data.frame(
  rsq = rsq,
  arsq = arsq,
  fstat = fstat,
  threshold = thresholds
) %>%
  pivot_longer(cols = c(rsq, arsq, fstat)) %>%
  dplyr::rename(Stat = name) %>%
  dbWriteTable(iaan,
               "FIGURE.PRICE_AMENITY_FEAT_IMPORT_THRESHOLD",
               .,
               overwrite = T)

#Choose where fstat maximized since R-Squared and Adjusted R-Squared not
#very sensitive
thresh_star <- (data.frame(
  rsq = rsq,
  arsq = arsq,
  fstat = fstat,
  threshold = thresholds
) %>%
  pivot_longer(cols = c(rsq, arsq, fstat)) %>%
  filter(name == "fstat") %>%
  mutate(m = max(value)) %>%
  filter(value == m))$threshold[1]

# Display features selected in majority of samples
names(
  feature_importance[feature_importance == thresh_star ])  %>%
  as.data.frame() %>%
  dplyr::rename(
    Amenity = "."
  ) %>%
  dbWriteTable(iaan,
               "OTHER.PRICE_IMP_AMENITIES",
               .,
               overwrite = T)

rm(feature_counts, arsq, feature_importance, fstat, i,
   num_samples, rsq, sample_fraction, sample_indices, 
   selected_features, thresh_star, thresholds)
gc()
rm(price)

#Real Price---------------------------------------------------------------------

#Set up parameters
num_samples <- 100      # Number of bootstrap samples
sample_fraction <- 0.4  # Fraction of data to sample each time

#Initialize a matrix to store selected features
feature_counts <- matrix(0, nrow = dim(amen)[2], ncol = num_samples)
colnames(feature_counts) <- paste0("Sample_", 1:num_samples)
rownames(feature_counts) <- colnames(amen)

set.seed(123)           # For reproducibility
# Ensemble with bootstrap sampling
for (i in 1:num_samples) {
  # Randomly sample a fraction of the data
  sample_indices <- sample(1:nrow(amen), size = 
                             round(sample_fraction * nrow(amen)), replace = TRUE)
  x_sample <- amen[sample_indices, ]
  y_sample <- real_price[sample_indices]
  
  # Fit Lasso model
  lasso_model <- cv.glmnet(x_sample, y_sample, alpha = 1)
  rm(x_sample, y_sample)
  best_lambda <- lasso_model$lambda.min
  
  # Get selected features for this sample
  selected_features <- coef(lasso_model, s = best_lambda)
  rm(lasso_model, best_lambda)
  selected_features <- (as.matrix(selected_features) %>%
                          as.data.frame() %>%
                          rownames_to_column() %>%
                          filter(s1 != 0) %>%
                          filter(rowname != "(Intercept)"))$rowname
  
  # Record selected features
  feature_counts[rownames(feature_counts) %in% selected_features, i] <- 1
  
  gc()
  print(i)
  
}

dbWriteTable(iaan,
             "MODEL.REAL_PRICE_AMENITY_FEATURE_COUNTS",
             feature_counts %>%
               as.data.frame() %>%
               rownames_to_column(var = "Amenity"),
             overwrite = T)

# Calculate feature importance by summing across samples
feature_importance <- rowSums(feature_counts) / num_samples

#Determine Optimal Threshold for Keeping by examining R-Squared and F-Statistics
#at difference thresholds
thresholds <- seq(0.1, 1, by = 0.1)
rsq <- vector()
arsq <- vector()
fstat <- vector()
for(i in 1:length(thresholds)){
  
  selected_features_ensemble <- names(
    feature_importance[feature_importance > thresholds[i]-0.01])
  reg <- lm(real_price ~ amen[, selected_features_ensemble])
  rsq[i] <- summary(reg)$r.squared
  arsq[i] <- summary(reg)$adj.r.squared
  fstat[i] <- summary(reg)$fstatistic[1]
  rm(reg)
  gc()
  
}

data.frame(
  rsq = rsq,
  arsq = arsq,
  fstat = fstat,
  threshold = thresholds
) %>%
  pivot_longer(cols = c(rsq, arsq, fstat)) %>%
  dplyr::rename(Stat = name) %>%
  dbWriteTable(iaan,
               "FIGURE.REAL_PRICE_AMENITY_FEAT_IMPORT_THRESHOLD",
               .,
               overwrite = T)

#Choose where fstat maximized since R-Squared and Adjusted R-Squared not
#very sensitive
thresh_star <- (data.frame(
  rsq = rsq,
  arsq = arsq,
  fstat = fstat,
  threshold = thresholds
) %>%
  pivot_longer(cols = c(rsq, arsq, fstat)) %>%
  filter(name == "fstat") %>%
  mutate(m = max(value)) %>%
  filter(value == m))$threshold[1]

# Display features selected in majority of samples
names(
  feature_importance[feature_importance == thresh_star ])  %>%
  as.data.frame() %>%
  dplyr::rename(
    Amenity = "."
  ) %>%
  dbWriteTable(iaan,
               "OTHER.REAL_PRICE_IMP_AMENITIES",
               .,
               overwrite = T)

rm(feature_counts, arsq, feature_importance, fstat, i,
   num_samples, rsq, sample_fraction, sample_indices, 
   selected_features, thresh_star, thresholds)
gc()
rm(real_price)
rm(amen)
gc()




