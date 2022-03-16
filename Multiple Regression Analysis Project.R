# Group Project Code: Multiple Regression Analysis

# (1) Import Data set
df = readRDS('turo.data.5140')

# (2) Select the state
az.df.clean <- df[df$car.state == 'az',]
head(az.df.clean,1)

# (5) Remove All NA's
az.df.clean<- na.omit(az.df.clean)

# (3) Separate categorical and continuous data based on context.
continuous <- list()
categorical <- list()

for (name in names(az.df.clean)){
  # Separated According to First Value of Each Variable
  if (class(df[1, name]) == 'logical'){
    # Boolean are categorical
    categorical <- c(categorical, name)
  }
  else if(class(df[1, name]) == 'factor'){
    # Factors AKA strings are also categorical
    categorical <- c(categorical, name)
  }
  else if(class(df[1, name]) == 'integer'){
    # In this case the int values are used to represent Binary Boolean values (1's and 0's)
    categorical <- c(categorical, name)
  }
  else if(df[1,name] == 0){
    # Binary Boolean values (1's and 0's) are categorical
    categorical <- c(categorical, name)
  }
  # Binary Boolean values (1's and 0's) are categorical
  else if(df[1,name] == 1){
    categorical <- c(categorical, name)
  }
  else if(name == "car.doors"){
    categorical <- c(categorical, name)
  }
  else{
    # All others belong in the list of continuous variables
    continuous <- c(continuous, name)
  }
}

print(paste("Continuous Variable Count: ", length(continuous)))
print(paste("Categorical Variable Count: ", length(categorical)))

### Number 3b: Summary Statistics

# extracting 'index positions'... (AKA: Column numbers)
continuous.pos <- match(continuous, names(df)) # index positions of extracted columns classified as "continuous"
categorical.pos <- match(categorical, names(df)) # index positions of extracted columns classified as "categorical"

# Continuous Variables : Compute -> min, first and third quartiles, max, mean, sd & skewness; histograms

N <- nrow(az.df.clean)

print(summary(df[, continuous.pos])) # min, first and third quartiles, max, mean

for (column in continuous){ # Standard Deviation and Skewness
  print(paste(column,"': ","Standard Deviation: '", sqrt(N - 1 / N) * sd(df[, column], na.rm = TRUE))) # <<<<<<<<< !!!!!!! See NOTES FOR LONGFORM !!!!!!
  print(paste(column,"': ", "Skewness: '", skewness(df[, column], na.rm = TRUE)))
  hist(df[, column], main = paste("Histogram of",column), xlab = column)
}

# Categorical Variables: Compute -> frequency and relative frequency distributions; bar charts

print(categorical) # extracted column names of the df classified as "continuous"

for (column in categorical){ # freq and relative freq distribution
  freq <- table(az.df.clean[,column])
  rel.freq <- table(column) / length(N)
  barplot(freq, main = paste("Bar Chart of",column), xlab = column)
}

# (4) Using the IQR method, detect and remove all the rows having outliers.
az.df<- az.df.clean
for (column in continuous){ # Standard Deviation and Skewness
  iqrange <- quantile(az.df[, column], 0.75) - quantile(az.df[, column], 0.25)
  lowerlimit <- quantile(az.df[, column], 0.25) - 1.5 * iqrange
  upperlimit <- quantile(az.df[, column], 0.75) + 1.5 * iqrange
  
# (5) Remove all observations w/ missing values
  
  for (row in az.df.clean[, column]){
    if(row < lowerlimit | row > upperlimit){
      az.df.clean <- az.df.clean[-row,]
    }
  }
}
print(paste("Dropped:", nrow(az.df) - nrow(az.df.clean), "rows!"))
print(paste("Row count w/o outliers:", nrow(az.df.clean)))

# Multiple Linear Regression
az.lm<-az.df.clean
az.lm$car.city<-NULL
az.lm$car.make<-NULL
az.lm$car.insurance<-NULL
az.lm$car.model<-NULL
az.lm$car.state<-NULL
fit<-lm(car.trip.price~.,az.lm)
step(fit)

# Backward Step method to remove insignificant variables.

Finalfit<-lm(formula = car.trip.price ~ car.displayed.user.review.num.past.18m + car.displayed.user.review.num.past.6m + 
     car.doors + car.extra.mile.fee + car.extra.post.trip.cleaning +car.transmission + 
     car.year + host.verified.email, data = az.lm)
summary(Finalfit)
