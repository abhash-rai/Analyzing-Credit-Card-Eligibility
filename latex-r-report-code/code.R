# install.packages("tidyverse")
library(tidyverse)



##########################
##### Importing data #####
##########################
df <- read.csv("credit_eligibility.csv")

 

##########################
##### Data Wrangling #####
##########################



problems(df)

str(df)
summary(df)
dim(df)

df <- df %>% select(-ID)

# Some columns represent durations measured relative to a reference point
# Giving those columns an appropriate name
names(df)[names(df) == "Age"] <- "Customer.relative.age"
names(df)[names(df) == "Account.age"] <- "Account.relative.age"
names(df)[names(df) == "Employment.length"] <- "Employment.relative.length"

df <- df %>%
  mutate(Gender = ifelse(Gender == "M", "Male", "Female"))

df <- df %>%
  mutate(Has.a.car = ifelse(Has.a.car == "Y", TRUE, FALSE),
         Has.a.property = ifelse(Has.a.property == "Y", TRUE, FALSE),
         Has.a.work.phone = ifelse(Has.a.work.phone == 1, TRUE, FALSE),
         Has.a.phone = ifelse(Has.a.phone == 1, TRUE, FALSE),
         Has.an.email = ifelse(Has.an.email == 1, TRUE, FALSE),
         Is.high.risk = ifelse(Is.high.risk == 1, TRUE, FALSE))

df$Family.member.count <- as.integer(df$Family.member.count)
df$Account.relative.age <- as.numeric(df$Account.relative.age)
df$Employment.relative.length <- as.numeric(df$Employment.relative.length)
df$Customer.relative.age <- as.numeric(df$Customer.relative.age)

# Has.a.mobile.phone column seems to have only single value
unique(df$Has.a.mobile.phone)

df <- df %>% select(-Has.a.mobile.phone)

# Applying capitalization to all factor columns using dplyr
df <- df %>%
  mutate(across(where(is.character), ~ str_to_title(as.character(.))))

# Checking for missing values in each column
df[df == ""] <- NA # Converting empty strings to NA values
result <- colSums(is.na(df))
result_df <- data.frame(Column_Names = names(result), NA_Count = result)
rownames(result_df) <- NULL
result_df

View(df)
str(df)

write.csv(df, "credit_eligibility_clean.csv", row.names=FALSE, quote=FALSE) 


#####################################
##### Exploratory Data Analysis #####
#####################################

character_columns <- df %>% select_if(is.character)
names(character_columns)



# install.packages("ggplot2")
# install.packages("ggrepel")
library(ggplot2)
library(ggrepel)

create_pie_chart_for_category <- function(df, column) {
  
  df_clean <- df[!is.na(df[[column]]), ]
  
  value_count_df <- data.frame(table(df_clean[[column]]))
  colnames(value_count_df) <- c("group", "value")
  
  value_count_df <- value_count_df[value_count_df$value != 0, ]
  
  label_position <- value_count_df %>% 
    mutate(csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
  
  ggplot(
    value_count_df, 
    aes(x = "" , y = value, fill = fct_inorder(group))) +
    geom_col(width = 1, color = 1) +
    coord_polar(theta = "y") +
    scale_fill_brewer(palette = "Accent") +
    geom_label_repel(
      data = label_position,
      aes(
        y = pos, 
        label = paste0(
          round((value/sum(value_count_df$value))*100, 2), "%"
        )
      ),
      size = 4.5, nudge_x = 1, show.legend = FALSE
    ) +
    guides(fill = guide_legend(title = "")) +
    theme_void() +
    ggtitle(paste0("Distribution of ", column)) +
    theme(
      plot.title = element_text(
        hjust = 0.5, margin = margin(t = 0, b = 10), face = "bold"
      )
    ) 
}



analyze_character_column <- function(df, col_name) {
  
  if (!col_name %in% names(df)) {
    stop("Column '", col_name, "' not found in the data frame.")
  }
  
  df_clean <- df[!is.na(df[[col_name]]), ]
  column_data <- df_clean[[col_name]]
  
  if (!is.character(column_data)) {
    stop("Column '", col_name, "' is not of character type.")
  }
  
  cat("Column:", col_name, "\n")
  unique_vals <- unique(column_data)
  num_unique_vals <- length(unique_vals)
  cat("Number of unique values:", num_unique_vals, "\n")
  cat("Value counts:\n")
  
  value_count_df <- data.frame(table(column_data))
  colnames(value_count_df) <- c("Category", "Count")
  value_count_df$Percentage <- round(
    (value_count_df$Count / sum(value_count_df$Count)) * 100, 2
  )
  value_count_df <- value_count_df[
    order(value_count_df$Percentage, decreasing = TRUE), 
  ]
  rownames(value_count_df) <- NULL
  print(value_count_df)
  cat("\n")
}


create_pie_chart_for_category(df, "Gender") 
analyze_character_column(df, "Gender")


library(ggplot2)

ggplot(df, aes(x = Employment.status, fill = Employment.status)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  theme_classic() +
  xlab("Employment Status") +
  ylab("Count") +
  ggtitle("Distribution of Employment Status") +
  scale_fill_brewer(palette = "Set2") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)),
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  coord_cartesian(ylim = c(0, 20000))

analyze_character_column(df, "Employment.status")




create_pie_chart_for_category(df, "Education.level") 
analyze_character_column(df, "Education.level")




marital_counts <- table(df$Marital.status)
marital_df <- as.data.frame(marital_counts)
names(marital_df) <- c("Marital.status", "Count")

ggplot(marital_df, aes(x = reorder(Marital.status, Count), y = Count)) +
  geom_point(aes(size = Count), color = "steelblue") +
  geom_text(aes(label = Count), vjust = -1, size = 3) +
  theme_classic() +
  xlab("Marital Status") +
  ylab("Count") +
  ggtitle("Distribution of Marital Status") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

create_pie_chart_for_category(df, "Marital.status") 
analyze_character_column(df, "Marital.status")




#install.packages("treemap")
library(ggplot2)
library(treemap)

dwelling <- table(df$Dwelling)
dwelling_df <- as.data.frame(dwelling)
names(dwelling_df) <- c("Dwelling", "Count")
treemap(dwelling_df,
        index = "Dwelling",
        vSize = "Count",
        title = "Distribution of Dwelling",
        palette = "Set2")

analyze_character_column(df, "Dwelling")




#install.packages("treemap")
library(ggplot2)
library(treemap)

job_title <- table(df$Job.title)
job_title_df <- as.data.frame(job_title)
names(job_title_df) <- c("Job.title", "Count")
treemap(job_title_df,
        index = "Job.title",
        vSize = "Count",
        title = "Distribution of Job.title",
        palette = "Set2")

analyze_character_column(df, "Job.title")







logical_columns <- df %>% select_if(is.logical)
names(logical_columns)





plot_df <- data.frame(
  col_name = c("Has.a.car", 
               "Has.a.property", "Has.a.work.phone",
               "Has.a.phone", "Has.an.email"),
  true = c(sum(df$Has.a.car), 
           sum(df$Has.a.property), sum(df$Has.a.work.phone), 
           sum(df$Has.a.phone), sum(df$Has.an.email)),
  false = c(nrow(df) - sum(df$Has.a.car), 
            nrow(df) - sum(df$Has.a.property), nrow(df) - sum(df$Has.a.work.phone), 
            nrow(df) - sum(df$Has.a.phone), nrow(df) - sum(df$Has.an.email))
)

plot_df_long <- tidyr::pivot_longer(plot_df, cols = c("true", "false"), 
                                    names_to = "value", values_to = "count")

ggplot(plot_df_long, aes(x = col_name, y = count / nrow(df) * 100, fill = value)) + 
  geom_bar(stat = "identity") +
  labs(x = "Binary Features", y = "Percentage", fill = NULL) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 3) +
  theme_bw() +
  theme(legend.position = "bottom")




#install.packages("lessR")
library(lessR)

PieChart(Is.high.risk, data = df,
         hole = 0.4,
         fill = 'blues',
         color = "black",
         lwd = 1.5,
         main = "Distribution of Is.high.risk")







library(ggplot2)




numeric_columns <- df %>% select_if(function(x) is.integer(x) || is.numeric(x))
names(numeric_columns)

ggplot(df, aes_string(x = "Children.count")) +
  geom_bar(aes(fill = cut(Children.count, breaks = c(-Inf, 0, 1, 2, 3, 4, 5, Inf))),  
           show.legend = FALSE) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size=3) +
  labs(title = paste0("Distribution of Children.count"),
       x = "Number of Children",
       y = "Count") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  scale_x_continuous(limits = c(-1, 6), breaks = seq(0, 5, by = 1)) + 
  scale_fill_brewer(palette = "Set2")

ggplot(df, aes_string(x = "Family.member.count")) +
  geom_bar(
    aes(fill = cut(Family.member.count, breaks = c(-Inf, 1, 2, 3, 4, 5, 6, 7, Inf))),
    show.legend = FALSE) + 
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = paste0("Distribution of Family.member.count"),
       x = "Number of Family Members",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + 
  scale_x_continuous(limits = c(0, 8), breaks = seq(1, 7, by = 1)) + 
  scale_fill_brewer(palette = "Set2")


ggplot(df, aes_string(x = "Customer.relative.age")) +
  geom_histogram(aes(fill = ..count..), bins = 60, show.legend = FALSE) +
  labs(title = paste0("Distribution of Customer.relative.age"),
       x = "Relative Age",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray")) +
  scale_y_continuous(limits = c(0, 1000)) +
  scale_fill_gradient(low = "#132c45", high = "#54a7e7")



ggplot(df, aes_string(x = "Account.relative.age")) +
  geom_histogram(aes(fill = ..count..), bins = 30, show.legend = FALSE) +
  labs(title = paste0("Distribution of Account.relative.age"),
       x = "Relative Account Age",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray")) +
  scale_y_continuous(limits = c(0, 2000)) +
  scale_fill_gradient(low = "#132c45", high = "#54a7e7")







#install.packages('patchwork')
library(patchwork)
library(ggplot2)

df_neg <- df[df$Employment.relative.length <= 0, ]
df_pos <- df[df$Employment.relative.length > 0, ]

bins <- 30

p1 <- ggplot(df_neg, aes(x = Employment.relative.length)) +
  geom_histogram(aes(fill = ..count..), bins = bins, show.legend = FALSE) +
  labs(title = "Distribution of Employment Period",
       x = "Employment Relative Length",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray")) +
  scale_y_continuous(limits = c(0, 6500)) +
  scale_fill_gradient(low = "#132c45", high = "#54a7e7")

p2_start = 360000
p2 <- ggplot(df_pos, aes(x = Employment.relative.length)) +
  geom_histogram(aes(fill = ..count..), bins = bins, show.legend = FALSE) +
  labs(title = "Distribution of Unemployment Period",
       x = "Unemployment Relative Length",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray"))  +
  scale_x_continuous(limits = c(p2_start, p2_start + 15000)) +
  scale_y_continuous(limits = c(0, 6500)) +
  scale_fill_gradient(low = "#132c45", high = "#54a7e7")

p1 + p2





ggplot(df, aes_string(x = "Income")) +
  geom_histogram(aes(fill = ..count..), bins = 60, show.legend = FALSE) +
  labs(title = paste0("Distribution of Income"),
       x = "Income",
       y = "Count") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        panel.background = element_blank(),
        panel.grid.major.y = element_line(linetype = "dashed", color = "gray"))  +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_fill_gradient(low = "#132c45", high = "#54a7e7")










#########################
##### Data Analysis #####
#########################





# What are the top features affecting credit risk status?

# install.packages("reshape2")
# install.packages("ggplot2")
library(reshape2)
library(ggplot2)

numeric_cols <- sapply(df, function(x) is.numeric(x) | is.integer(x) | is.logical(x))
df_numeric <- df[, numeric_cols]
cormat <- round(cor(df_numeric),2)

upper_tri <- cormat
upper_tri[lower.tri(cormat)] <- NA

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))




# install.packages("xgboost")
# install.packages("caret")

library(xgboost)
library(caret)

df_clean <- na.omit(df)

features <- df_clean[, !(names(df_clean) %in% c("Is.high.risk"))] # Exclude target column
target <- df_clean$Is.high.risk

# Train-Test Split

set.seed(555) # for reproducibility
train_size <- floor(0.8 * nrow(features))
train_indices <- sample(seq_len(nrow(features)), size = train_size)

X_train <- features[train_indices, ]
preprocessed_data <- dummyVars(" ~ .", data = X_train)
X_train <- data.frame(predict(preprocessed_data, newdata = X_train))
X_train = as.matrix(X_train)

y_train <- target[train_indices]
y_train <- as.numeric(y_train)

X_test <- features[-train_indices, ]
preprocessed_data <- dummyVars(" ~ .", data = X_test)
X_test <- data.frame(predict(preprocessed_data, newdata = X_test))
X_test = as.matrix(X_test)

y_test <- target[-train_indices]
y_test <- as.numeric(y_test)

dim(X_train)
dim(X_test)

dtrain <- xgb.DMatrix(data = X_train, label=y_train)
dtest <- xgb.DMatrix(data = X_test, label=y_test)
watchlist <- list(train=dtrain, test=dtest)

model <- xgb.train(
  data=dtrain, 
  lambda = 5,
  alpha=5,
  max.depth=4, 
  eta=0.3, 
  subsample=0.8,
  colsample_bytree=0.9,
  nthread = 3, 
  nrounds=1000, 
  scale_pos_weight = 30,
  watchlist=watchlist, 
  objective = "binary:logistic",
  eval_metric = "logloss",
  verbose = 0
)

y_pred = predict(model, X_test)
y_pred <- ifelse(y_pred > 0.5, 1, 0)
y_test_factor <- factor(y_test, levels = c(0, 1)) 
y_pred_factor <- factor(y_pred, levels = c(0, 1))
confusionMatrix(y_test_factor, y_pred_factor)


# install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
importance_matrix <- xgb.importance(colnames(X_train), model = model)
xgb.ggplt <- xgb.ggplot.importance(importance_matrix = importance_matrix, top_n = 5)
xgb.ggplt +
  theme_classic() +
  theme(legend.position = "bottom") +
  labs(title = "Important Features for Classifying Credit Card Risk", 
       y = "Feature Importance Score (XGBoost)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))




# Which jobs are at risk the most and what is their income pattern?

library(ggplot2)

job_counts <- table(df$Job.title, df$Is.high.risk)
job_counts_df <- as.data.frame(job_counts)
job_counts_df <- job_counts_df %>% pivot_wider(names_from = Var2, values_from = Freq)

colnames(job_counts_df) <- c("Job.title", "Low.risk", "High.risk")

job_counts_df <- job_counts_df %>%
  mutate(
    Low.risk.perct = round((Low.risk / (Low.risk + High.risk)) * 100, 3),
    High.risk.perct = round((High.risk / (Low.risk + High.risk)) * 100, 3)
  )

job_counts_df <- job_counts_df %>%
  arrange(desc(High.risk.perct))


job_counts_df <- job_counts_df %>%
  mutate(Risk = case_when(
    High.risk.perct > 4 ~ "Very High Risk", 
    High.risk.perct < 1 ~ "Low Risk",
    TRUE ~ "Moderate Risk"
  ))

risk_colors <- c(
  "Very High Risk" = "#f3661f",
  "Low Risk" = "#50bb6d",
  "Moderate Risk" = "#1470bb"
)

ggplot(
  job_counts_df, aes(x = High.risk.perct, y = reorder(Job.title, High.risk.perct), fill = Risk)) +
  geom_bar(stat = "identity", width = 0.7, color = NA) +
  geom_text(aes(label = paste0(round(High.risk.perct, 1), "%")), 
            hjust = -0.5, 
            vjust = 0.4,
            color = "black", 
            size = 3) + 
  geom_vline(xintercept = 0, color = "black", linetype = "solid") +
  scale_fill_identity(
    name = "",
    guide = guide_legend(title.position = "top", title.hjust = 0.5)) +  
  labs(
    title = "Examining High-Risk Job Titles in Credit Card Applications",
    subtitle = "(Percentage of High Risk Individuals)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(b = 15)),
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_blank(), 
    axis.title.x = element_blank(), 
    axis.title.y = element_blank(), 
    legend.position = "bottom"
  ) +
  scale_fill_manual(values = risk_colors, name = "")




#install.packages("ggridges")
library(ggridges)
library(ggplot2)

df_clean <- na.omit(df)

df_clean$Risk <-  with(
  df_clean, 
  ifelse(
    Job.title %in% c('It Staff', 'Low-Skill Laborers'), 
    'Very High Risk', 
    ifelse(
      Job.title %in% c('Cleaning Staff', 'Medicine Staff', 'Private Service Staff', 'Realty Agents'), 
      'Low Risk', 
      'Moderate Risk')
    )
  )

risk_colors <- c(
  "Very High Risk" = "#f3661f",
  "Low Risk" = "#50bb6d",
  "Moderate Risk" = "#1470bb"
)

ggplot(df_clean, aes(x = Income, y = Job.title, fill = Risk)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = risk_colors, name = "") +
  theme_ridges() + 
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(limits = c(-1, 1000000)) +
  labs(title = "Income Distribution by Job Title Risk",
       x = "Income",
       y = "Job Title")







ggplot(df, aes(x = Income, fill = Education.level)) +
  geom_histogram(alpha = 0.6, position = "fill") +
  labs(title = "Proportional Density of Income Across Employment Status",
       x = "Income",
       y = "Proportion",
       fill = "Employment Status") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )




# How does the combination of marital status and income level interact to influence risk classification in unexpected ways?

df_analysis <- df

df_analysis$Is.high.risk <- factor(df_analysis$Is.high.risk, levels = c(FALSE, TRUE), labels = c("Low Risk", "High Risk"))

df_analysis$Income.Category <- cut(df_analysis$Income, breaks = c(0, 100000, 200000, 300000, 400000, 500000, Inf), 
                                   labels = c("Income 0-100k", "Income 100k-200k", "Income 200k-300k", "Income 300k-400k", "Income 400k-500k", "Income 500k+"), include.lowest = TRUE)

ggplot(df_analysis, aes(x = Marital.status, fill = Is.high.risk)) +
  geom_bar(position = "fill") +
  facet_wrap(~ Income.Category, scales = "free_x") +
  labs(title = "Risk Classification by Marital Status and Income Level",
       x = "Marital Status",
       y = "Proportion of Customers",
       fill = "Risk Level") +
  scale_fill_manual(values = c("Low Risk" = "gray", "High Risk" = "#991200")) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1.2, hjust=1.2),
    plot.title = element_text(hjust = 0.5, face = "bold", vjust = 3)
  )









percentile_90 <- quantile(df$Income, probs = 0.9)

t2.rect1 <- data.frame(
  xmin = min(log(abs(df$Employment.relative.length))),
  xmax = max(log(abs(df$Employment.relative.length))),
  ymin = percentile_90,
  ymax = max(df$Income)
)

ggplot(df, 
       aes(x = log(abs(Employment.relative.length)), y = Income, 
           shape = Employment.status, color = Employment.status)) +
  geom_rect(data = t2.rect1, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill = "#50bb6d", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(size = 0.6, alpha = 0.5) +
  theme_classic() +
  labs(x = "Log of Absolute Employment Relative Length",
       y = "Income", 
       title = "Relationship between Employment Duration and Income") +
  geom_hline(
    yintercept = percentile_90, linetype = "dashed", color = "black") +
  annotate("text", 
           x = max(log(abs(df$Employment.relative.length))), 
           y = percentile_90, label = "90th Percentile", 
           vjust = 1, hjust = 1.25, color = "black") +
  annotate("text", 
           x = mean(log(abs(df$Employment.relative.length))), 
           y = mean(c(percentile_90, max(df$Income))), 
           label = "High Income", color = "black", size = 5, 
           hjust = 3.5, vjust = -8) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), 
        legend.position = "bottom",
        legend.key.size = unit(1.2, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 4)))











ggplot(df, aes(x = Has.a.car, y = Income, color = Is.high.risk)) +
  geom_boxplot() +
  facet_wrap(~ Has.a.property, labeller = as_labeller(c("TRUE" = "Has Property", "FALSE" = "No Property"))) +
  labs(
    x = "Has Car",
    y = "Income",
    title = "Income Distribution by Property Ownership, Car Ownership, and Risk Status"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10))) + 
  scale_color_manual(values = c("red", "blue"), labels = c("High Risk", "Low Risk"))










marital_counts <- table(df$Marital.status)
marital_df <- as.data.frame(marital_counts)
names(marital_df) <- c("Marital.status", "Count")

ggplot(marital_df, aes(x = reorder(Marital.status, Count), y = Count)) +
  geom_point(aes(size = Count), color = "steelblue") +
  geom_text(aes(label = Count), vjust = -1, size = 3) +
  theme_classic() +
  xlab("Marital Status") +
  ylab("Count") +
  ggtitle("Distribution of Marital Status") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_cartesian(ylim = c(0, 30000))







unique(df$Education.level)

education_order <- c("Lower Secondary", "Secondary / Secondary Special", 
                     "Incomplete Higher", "Higher Education", "Academic Degree")

median_income <- df %>%
  group_by(Education.level) %>%
  summarize(Median.Income = median(Income))

ggplot(median_income, 
       aes(
         x = factor(Education.level, levels = education_order), 
         y = Median.Income)) +
  geom_segment(aes(x = factor(Education.level, levels = education_order),
                   xend = factor(Education.level, levels = education_order),
                   y = 0, yend = Median.Income), 
               color = "steelblue", linetype = "dashed", alpha = 0.5) +
  geom_point(aes(size = Median.Income), color = "#f3661f") +
  geom_text(aes(label = Median.Income), vjust = -1, size = 3) +
  theme_classic() +
  labs(title = "Median Income by Education Level",
       x = "Education Level (Increasing)", 
       y = "Median Income") +
  scale_x_discrete(labels = c("Lower Secondary", "Secondary", 
                              "Incomplete Higher", "Higher Education", 
                              "Academic Degree")) +
  scale_y_continuous(labels = scales::comma, 
                     breaks = seq(0, 300000, by = 50000), 
                     limits = c(0, 300000)) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)), 
    axis.title.x = element_text(margin = margin(t = 10)))
