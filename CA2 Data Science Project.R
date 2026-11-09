# Load the Top Movies data frame
topmovies_data <- read.csv("C:/Users/Luke Duffy/Downloads/archive (4)/Top Movies dataset.csv")
View(topmovies_data)
str(topmovies_data)
summary(topmovies_data)
head(topmovies_data)

#To check missing data
colSums(is.na(topmovies_data))
#To check how many rows has the dataset (before)
nrow(topmovies_data)
#To remove missing records
topmovies_data <- topmovies_data[!is.na(topmovies_data$Popularity), ]
#To check how many rows has the dataset (after)
nrow(topmovies_data)

#Change data type of Release_Date from char to date
topmovies_data$Release_Date <- as.Date(
  topmovies_data$Release_Date,
  format = "%d/%m/%Y"
)

str(topmovies_data$Release_Date)

#Create a new variable Release_Year
topmovies_data$Release_Year <- as.numeric(
  format(topmovies_data$Release_Date, "%Y")
)

#Checking that a new variable Release_Year has been created
head(topmovies_data[, c("Release_Date", "Release_Year")])

#checking original_language variable
language_freq <- as.data.frame(
  sort(table(topmovies_data$Original_Language),
       decreasing = TRUE)
)

colnames(language_freq) <- c("Language", "Frequency")

View(language_freq)

#Checking for a bad record in Original_Language variable
subset(
  topmovies_data,
  grepl("http", Original_Language)
)

#checking that only 1 error exist with the shifted data (row malformed)
sum(grepl("http", topmovies_data$Original_Language))

#Remove bad record
topmovies_data <- topmovies_data[
  !grepl("http", topmovies_data$Original_Language),
]
#Verify that bad record is gone
sum(grepl("http", topmovies_data$Original_Language))

#Investigation original_language cn and zn before merging
subset(
  topmovies_data,
  Original_Language == "cn"
)[, c("Title", "Genre")]

#Merge cn into zh and set it as a factor
topmovies_data$Original_Language[
  topmovies_data$Original_Language == "cn"
] <- "zh"

topmovies_data$Original_Language <- as.factor(
  topmovies_data$Original_Language
)
#Check Merge results 
sort(table(topmovies_data$Original_Language),
     decreasing = TRUE)
#Check update to factor
str(topmovies_data)

#Analysis for Genre variable 
class(topmovies_data$Genre)

#View values in Genre
head(topmovies_data$Genre)

#Check how many unique Genre combination exists in the dataset
length(unique(topmovies_data$Genre))

#checking frequency table of most frequent Genre combinations
genre_freq <- sort(table(topmovies_data$Genre),
                   decreasing = TRUE)

head(genre_freq, 20)

#Creating Primary_Genre variable 
topmovies_data$Primary_Genre <- trimws(
  sub(",.*", "", topmovies_data$Genre)
)
#check the new Primary_Genre variable exists
head(topmovies_data[, c("Genre", "Primary_Genre")])

#Converting Primary_Genre and Gender to Factor from chr
topmovies_data$Primary_Genre <- as.factor(topmovies_data$Primary_Genre)
topmovies_data$Genre <- as.factor(topmovies_data$Genre)
 
#Check Primary_Genre and Genre variable has been set to Factor
class(topmovies_data$Primary_Genre)
class(topmovies_data$Genre)

#Check the new Primary_Gente variable exists
head(topmovies_data[, c("Genre", "Primary_Genre")])

#Checking how many genre categories exists based on Primary_Genre new variable
sort(table(topmovies_data$Primary_Genre),
     decreasing = TRUE)

length(unique(topmovies_data$Primary_Genre))

#Checking that quantitative variables used in my research questions are numeric
class(topmovies_data$Popularity)

class(topmovies_data$Vote_Count)

class(topmovies_data$Release_Year)

class(topmovies_data$Vote_Average)

#Converting variable type to numeric for Vote_Count and Vote_Average
topmovies_data$Vote_Count <- as.numeric(topmovies_data$Vote_Count)

topmovies_data$Vote_Average <- as.numeric(topmovies_data$Vote_Average)

#Verifying type conversion for variable Vote_Count and Vote_Average
class(topmovies_data$Vote_Count)

class(topmovies_data$Vote_Average)

#check converted values didnt introduce NA values
sum(is.na(topmovies_data$Vote_Count))

sum(is.na(topmovies_data$Vote_Average))

#Reset Graphic design as it was stuck
graphics.off()
#This is to tell R that I will be displaying the 4 charts in 1 page
par(mfrow = c(2,2))
#Chart 1 Primary_Genre
barplot(
  sort(table(topmovies_data$Primary_Genre),
       decreasing = TRUE),
  las = 2,
  col = "lightblue",
  main = "Primary Genre Distribution",
  ylab = "Number of Movies"
)

#Chart 2 Original_Language
barplot(
  sort(table(topmovies_data$Original_Language),
       decreasing = TRUE),
  las = 2,
  col = "lightgreen",
  main = "Original Language Distribution",
  ylab = "Number of Movies"
)
#Chart 3 Movie Ratings
hist(
  topmovies_data$Vote_Average,
  col = "orange",
  main = "Movie Ratings",
  xlab = "Vote Average"
)
#Chart 4 popularity
hist(
  topmovies_data$Popularity,
  col = "pink",
  main = "Popularity Distribution",
  xlab = "Popularity"
)

par(mfrow = c(1,1))

#Popularity chart using a log scale
# Create transformed popularity variable
log_pop <- log10(topmovies_data$Popularity + 1)

# Calculate summary statistics
mean_pop <- mean(log_pop, na.rm = TRUE)
median_pop <- median(log_pop, na.rm = TRUE)

# Create histogram
hist(
  log_pop,
  col = "pink",
  border = "white",
  main = "Distribution of Movie Popularity (Log Scale)",
  sub = "Log transformation reduces the effect of extremely popular blockbuster movies",
  xlab = "Log10(Popularity + 1)",
  ylab = "Number of Movies"
)

# Add mean line
abline(
  v = mean_pop,
  col = "red",
  lwd = 2,
  lty = 2
)

# Add median line
abline(
  v = median_pop,
  col = "blue",
  lwd = 2,
  lty = 2
)

# Add legend
legend(
  "topright",
  legend = c(
    paste("Mean =", round(mean_pop, 2)),
    paste("Median =", round(median_pop, 2))
  ),
  col = c("red", "blue"),
  lty = 2,
  lwd = 2,
  bty = "n"
)

# Add explanatory text inside the chart
text(
  x = 2.3,
  y = max(hist(log_pop, plot = FALSE)$counts) * 0.85,
  labels = "Most movies have\nlow to moderate popularity",
  cex = 0.8,
  col = "darkblue"
)
  #Check the variable type of the final prepared variables
  str(topmovies_data[, c(
    "Release_Date",
    "Release_Year",
    "Popularity",
    "Vote_Count",
    "Vote_Average",
    "Genre",
    "Primary_Genre",
    "Original_Language"
  )])
  
#Assumption Monotonic relationship between variables
  plot(
    topmovies_data$Popularity,
    topmovies_data$Vote_Average,
    pch = 19,
    col = "steelblue",
    main = "Popularity vs Movie Rating",
    xlab = "Popularity",
    ylab = "Vote Average"
  )
  #Assumption for Research Question 2 - Approximate Normality Within Groups
  #Visual approach
  boxplot(
    Vote_Average ~ Primary_Genre,
    data = topmovies_data,
    las = 2,
    col = "lightblue",
    main = "Movie Ratings by Genre",
    xlab = "Primary Genre",
    ylab = "Vote Average"
  )
 #Histogram Research Question 2 - Approximate Normality Within Groups
  #Visual approach 
  hist(
    topmovies_data$Vote_Average,
    main = "Distribution of Movie Ratings",
    col = "orange",
    xlab = "Vote Average"
  )
# Assumption for Research Question 3 
#checking monotonic relationship assumption for Spearman correlation
  plot(
    topmovies_data$Vote_Count,
    topmovies_data$Popularity,
    pch = 19,
    col = "steelblue",
    main = "Vote Count vs Popularity",
    xlab = "Vote Count",
    ylab = "Popularity"
  )
  # Assumption for Research Question 4 
#check whether there appears to be a relationship between year and rating.
  plot(
    topmovies_data$Release_Year,
    topmovies_data$Vote_Average,
    pch = 19,
    col = "steelblue",
    main = "Release Year vs Movie Rating",
    xlab = "Release Year",
    ylab = "Vote Average"
  )
  
  abline(
    lm(Vote_Average ~ Release_Year,
       data = topmovies_data),
    col = "red",
    lwd = 2
  ) 
# Assumption for Research Question 4   
#Check Homoscedasticity
  model <- lm(
    Vote_Average ~ Release_Year,
    data = topmovies_data
  )
  
  plot(
    fitted(model),
    residuals(model),
    main = "Residual Plot",
    xlab = "Fitted Values",
    ylab = "Residuals"
  )
  
  abline(h = 0, col = "red", lwd = 2)
  
# Assumption for Research Question 4
#Histogram of Residuals
  hist(
    residuals(model),
    col = "lightgreen",
    main = "Histogram of Residuals",
    xlab = "Residuals"
  )
  
 #Assumption for Research Question 4
 #Q-Q Plot 
  qqnorm(
    residuals(model),
    main = "Normal Q-Q Plot"
  )
  
  qqline(
    residuals(model),
    col = "red",
    lwd = 2
  )
#Assumption for Research Question 5
#Check Fit the ANOVA model:
  
  anova_model <- aov(
    Popularity ~ Original_Language,
    data = topmovies_data
  )
  
  summary(anova_model)
  
#Assumption for Research Question 5 
#Check Popularity by Original Language Boxplot
  boxplot(
    Popularity ~ Original_Language,
    data = topmovies_data,
    las = 2,
    col = "lightblue",
    main = "Popularity by Original Language",
    xlab = "Language",
    ylab = "Popularity"
  )
#Assumption for Research Question 5 
#Histogram of ANOVA Residuals
  anova_model <- aov(
    Popularity ~ Original_Language,
    data = topmovies_data
  )
  
  hist(
    residuals(anova_model),
    col = "lightgreen",
    main = "Histogram of ANOVA Residuals",
    xlab = "Residuals"
  )
#Assumption for Research Question 5  
#Q-Q Plot of ANOVA Residuals
  qqnorm(residuals(anova_model))
  qqline(residuals(anova_model),
         col = "red")
  
#Research Question 1 - Hypothesis Test
  cor.test(
    topmovies_data$Popularity,
    topmovies_data$Vote_Average,
    method = "spearman"
  )
#Research Question 2 - Hypothesis Test 
  anova_genre <- aov(
    Vote_Average ~ Primary_Genre,
    data = topmovies_data
  )
  
  summary(anova_genre)
  
#Research Question 3 - Hypothesis Test 
  cor.test(
    topmovies_data$Vote_Count,
    topmovies_data$Popularity,
    method = "spearman"
  )
#Research Question 4 - Hypothesis Test  
  model <- lm(
    Vote_Average ~ Release_Year,
    data = topmovies_data
  )
  
  summary(model)
#Research Question 5 - Hypothesis Test   
  kruskal.test(
    Popularity ~ Original_Language,
    data = topmovies_data
  )
#Duplicate check 
  sum(duplicated(topmovies_data))
  sum(duplicated(topmovies_data$Title))
  
  topmovies_data[duplicated(topmovies_data), ]
  
  
  sum(duplicated(topmovies_data$Title))
  
  topmovies_data[duplicated(topmovies_data$Title), ]
  
#Checking for Duplicates  
  sum(
    duplicated(
      topmovies_data[, c("Title", "Release_Date")]
    )
  )
#Check if Git is installed
  system("git --version")
  
#
  getwd()
  
  