#Nominal Price Lasso Feature Selection------------------------------------------
price_amen <- dbReadTable(iaan, "OTHER.PRICE_IMP_AMENITIES")$Amenity
rprice_amen <- dbReadTable(iaan, "OTHER.REAL_PRICE_IMP_AMENITIES")$Amenity

#Intersection
amen_inter <- intersect(price_amen, rprice_amen) %>%
  paste0(collapse = ", ")

#Get Data
query <- paste0("SELECT KEY_LIST_ID, KEY_HOST_ID, CITY_ID, DATE, price,
                zip, listing_type, room_type, capacity, bathrooms,
                bedrooms, beds, available_030, overall_rating,
                amenity_count, host_is_superhost, month, year,
                ", amen_inter, " FROM [DATA.FULL_SAMPLE]")
rm(amen_inter, price_amen, rprice_amen)
ia_sample <- dbGetQuery(iaan, query)


#Wrangle 
ia_sample <- ia_sample %>%
  mutate(listing_type = tolower(listing_type)) %>%
  mutate(
    listing_type = gsub("private room in ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("room in ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("shared room in ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("shared ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("entire ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = case_when(
      grepl("bed", listing_type, ignore.case = T) ~ "bnb",
      grepl("casa", listing_type, ignore.case = T) ~ "casa particular",
      grepl("condo", listing_type, ignore.case = T) ~ "condo",
      grepl("cycladic", listing_type, ignore.case = T) ~ "cycladic house",
      grepl("dome", listing_type, ignore.case = T) ~ "dome house",
      grepl("boat", listing_type, ignore.case = T) ~ "boat",
      grepl("minsu", listing_type, ignore.case = T) ~ "minsu",
      grepl("ryokan", listing_type, ignore.case = T) ~"ryokan",
      grepl("tiny", listing_type, ignore.case = T) ~ "tiny home",
      grepl("earth", listing_type, ignore.case = T) ~ "earth house",
      grepl("pension", listing_type, ignore.case = T) ~ "pension",
      .default = listing_type
    ) 
  ) %>%
  mutate(
    listing_type = gsub("private ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "house", "home", listing_type) 
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "residential home", "home", listing_type)
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "", "other", listing_type)
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "parking space", "other", listing_type)
  ) 

ia_sample <- ia_sample %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(
    price1 = lag(price, n = 1),
    price2 = lag(price, n = 2),
    price3 = lag(price, n = 3)
  ) 

ia_sample <- na.omit(ia_sample)

#Collect numeric variables
numerics <- ia_sample %>%
  ungroup() %>%
  select(price, price1, price2, price3,
         capacity, bathrooms, bedrooms, beds, available_030,
         amenity_count, host_is_superhost,
         starts_with("AC")) %>%
  as.matrix()

#Clean up for RAM
ia_sample <- ia_sample %>%
  select(-price, -price1, -price2, -price3, -capacity,
         -bathrooms, -bedrooms, -beds, -available_030, -amenity_count,
         -host_is_superhost, -starts_with("AC"), -overall_rating)

ia_sample <- ia_sample %>%
  ungroup() %>%
  select(-KEY_LIST_ID, -KEY_HOST_ID, -DATE)

#Collect factor variables into dummy matrix, combine with numerics
features <- cbind(
  numerics,
  (ia_sample %>%
  ungroup() %>%
  select(CITY_ID, listing_type, room_type, month, year) %>%
  model.matrix(~listing_type + room_type + month + year + CITY_ID, .))
)

#Clean up for RAM
rm(ia_sample)
rm(numerics)
gc()

#Set up parameters
num_samples <- 100      # Number of bootstrap samples
sample_fraction <- 0.3  # Fraction of data to sample each time

#Initialize a matrix to store selected features
feature_counts <- matrix(0, nrow = dim(features)[2]-1, ncol = num_samples)
colnames(feature_counts) <- paste0("Sample_", 1:num_samples)
rownames(feature_counts) <- colnames(features[,-1])

set.seed(123)           # For reproducibility
#Ensemble with bootstrap sampling
for (i in 1:num_samples) {
  #Randomly sample a fraction of the data
  sample_indices <- sample(1:nrow(features), size = 
                             round(sample_fraction * nrow(features)), replace = TRUE)
  x_sample <- features[sample_indices, -1]
  y_sample <- features[sample_indices, 1]
  rm(sample_indices)
  
  #Fit Lasso model
  lasso_model <- cv.glmnet(x_sample, y_sample, alpha = 1)
  rm(x_sample, y_sample)
  best_lambda <- lasso_model$lambda.min
  
  #Get selected features for this sample
  selected_features <- coef(lasso_model, s = best_lambda)
  rm(lasso_model, best_lambda)
  selected_features <- (as.matrix(selected_features) %>%
                          as.data.frame() %>%
                          rownames_to_column() %>%
                          filter(s1 != 0) %>%
                          filter(rowname != "(Intercept)"))$rowname
  
  #Record selected features
  feature_counts[rownames(feature_counts) %in% selected_features, i] <- 1
  
  gc()
  print(i)
  
}

#Save Results
dbWriteTable(iaan,
             "MODEL.PRICE_FEATURE_SELECTION",
             feature_counts %>%
               as.data.frame() %>%
               rownames_to_column(var = "Feature"),
             overwrite = T)

#Calculate feature importance by summing across samples
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
  reg <- lm(features[,1] ~ features[, selected_features_ensemble])
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
               "FIGURE.PRICE_FEATURE_IMPORT_THRESHOLD",
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
    Feature = "."
  ) %>%
  dbWriteTable(iaan,
               "OTHER.PRICE_IMP_FEATURES",
               .,
               overwrite = T)

rm(feature_counts, arsq, feature_importance, fstat, i, num_samples,
   rsq, sample_fraction, selected_features, selected_features_ensemble,
   thresh_star, thresholds)
rm(features)
gc()


#Real Price Lasso Feature Selection---------------------------------------------
price_amen <- dbReadTable(iaan, "OTHER.PRICE_IMP_AMENITIES")$Amenity
rprice_amen <- dbReadTable(iaan, "OTHER.REAL_PRICE_IMP_AMENITIES")$Amenity

#Intersection
amen_inter <- intersect(price_amen, rprice_amen) %>%
  paste0(collapse = ", ")

#Get Data
query <- paste0("SELECT KEY_LIST_ID, KEY_HOST_ID, CITY_ID, DATE, price,
                zip, listing_type, room_type, capacity, bathrooms,
                bedrooms, beds, available_030, overall_rating,
                amenity_count, host_is_superhost, month, year,
                ", amen_inter, " FROM [DATA.FULL_SAMPLE]")
rm(amen_inter, price_amen, rprice_amen)
ia_sample <- dbGetQuery(iaan, query)


#Wrangle 
ia_sample <- ia_sample %>%
  left_join(cpi, by = c("year", "month")) %>%
  mutate(price = (price/cpi_b2015)*100) %>%
  select(-cpi, -cpi_b2015)

ia_sample <- ia_sample %>%
  mutate(listing_type = tolower(listing_type)) %>%
  mutate(
    listing_type = gsub("private room in ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("room in ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("shared room in ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("shared ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = gsub("entire ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = case_when(
      grepl("bed", listing_type, ignore.case = T) ~ "bnb",
      grepl("casa", listing_type, ignore.case = T) ~ "casa particular",
      grepl("condo", listing_type, ignore.case = T) ~ "condo",
      grepl("cycladic", listing_type, ignore.case = T) ~ "cycladic house",
      grepl("dome", listing_type, ignore.case = T) ~ "dome house",
      grepl("boat", listing_type, ignore.case = T) ~ "boat",
      grepl("minsu", listing_type, ignore.case = T) ~ "minsu",
      grepl("ryokan", listing_type, ignore.case = T) ~"ryokan",
      grepl("tiny", listing_type, ignore.case = T) ~ "tiny home",
      grepl("earth", listing_type, ignore.case = T) ~ "earth house",
      grepl("pension", listing_type, ignore.case = T) ~ "pension",
      .default = listing_type
    ) 
  ) %>%
  mutate(
    listing_type = gsub("private ", "", listing_type, ignore.case = T)
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "house", "home", listing_type) 
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "residential home", "home", listing_type)
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "", "other", listing_type)
  ) %>%
  mutate(
    listing_type = ifelse(listing_type == "parking space", "other", listing_type)
  ) 

ia_sample <- ia_sample %>%
  arrange(KEY_LIST_ID, KEY_HOST_ID, DATE) %>%
  group_by(KEY_LIST_ID, KEY_HOST_ID) %>%
  mutate(
    price1 = lag(price, n = 1),
    price2 = lag(price, n = 2),
    price3 = lag(price, n = 3)
  ) 

ia_sample <- na.omit(ia_sample)

#Collect numeric variables
numerics <- ia_sample %>%
  ungroup() %>%
  select(price, price1, price2, price3,
         capacity, bathrooms, bedrooms, beds, available_030,
         amenity_count, host_is_superhost,
         starts_with("AC")) %>%
  as.matrix()

#Clean up for RAM
ia_sample <- ia_sample %>%
  select(-price, -price1, -price2, -price3, -capacity,
         -bathrooms, -bedrooms, -beds, -available_030, -amenity_count,
         -host_is_superhost, -starts_with("AC"), -overall_rating)

ia_sample <- ia_sample %>%
  ungroup() %>%
  select(-KEY_LIST_ID, -KEY_HOST_ID, -DATE)

#Collect factor variables into dummy matrix, combine with numerics
features <- cbind(
  numerics,
  (ia_sample %>%
     ungroup() %>%
     select(CITY_ID, listing_type, room_type, month, year) %>%
     model.matrix(~listing_type + room_type + month + year + CITY_ID, .))
)

#Clean up for RAM
rm(ia_sample)
rm(numerics)
gc()

#Set up parameters
num_samples <- 100      # Number of bootstrap samples
sample_fraction <- 0.3  # Fraction of data to sample each time

#Initialize a matrix to store selected features
feature_counts <- matrix(0, nrow = dim(features)[2]-1, ncol = num_samples)
colnames(feature_counts) <- paste0("Sample_", 1:num_samples)
rownames(feature_counts) <- colnames(features[,-1])

set.seed(123)           # For reproducibility
#Ensemble with bootstrap sampling
for (i in 1:num_samples) {
  #Randomly sample a fraction of the data
  sample_indices <- sample(1:nrow(features), size = 
                             round(sample_fraction * nrow(features)), replace = TRUE)
  x_sample <- features[sample_indices, -1]
  y_sample <- features[sample_indices, 1]
  rm(sample_indices)
  
  #Fit Lasso model
  lasso_model <- cv.glmnet(x_sample, y_sample, alpha = 1)
  rm(x_sample, y_sample)
  best_lambda <- lasso_model$lambda.min
  
  #Get selected features for this sample
  selected_features <- coef(lasso_model, s = best_lambda)
  rm(lasso_model, best_lambda)
  selected_features <- (as.matrix(selected_features) %>%
                          as.data.frame() %>%
                          rownames_to_column() %>%
                          filter(s1 != 0) %>%
                          filter(rowname != "(Intercept)"))$rowname
  
  #Record selected features
  feature_counts[rownames(feature_counts) %in% selected_features, i] <- 1
  
  gc()
  print(i)
  
}

#Save Results
dbWriteTable(iaan,
             "MODEL.REAL_PRICE_FEATURE_SELECTION",
             feature_counts %>%
               as.data.frame() %>%
               rownames_to_column(var = "Feature"),
             overwrite = T)

#Calculate feature importance by summing across samples
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
  reg <- lm(features[,1] ~ features[, selected_features_ensemble])
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
               "FIGURE.REAL_PRICE_FEATURE_IMPORT_THRESHOLD",
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
    Feature = "."
  ) %>%
  dbWriteTable(iaan,
               "OTHER.REAL_PRICE_IMP_FEATURES",
               .,
               overwrite = T)

rm(feature_counts, arsq, feature_importance, fstat, i, num_samples,
   rsq, sample_fraction, selected_features, selected_features_ensemble,
   thresh_star, thresholds, query)
rm(features)
gc()

#Intersection-------------------------------------------------------------------
price_feat <- dbReadTable(iaan, "OTHER.PRICE_IMP_FEATURES")
rprice_feat <- dbReadTable(iaan, "OTHER.REAL_PRICE_IMP_FEA")
feat_intersect <- union(price_feat, rprice_feat)

