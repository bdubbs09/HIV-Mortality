# Make sure you first download and import the dataset. Theres a tab on the right that does it easily.

# Creating dataframe from the data source
df <- `Compressed.Mortality,.1999.2016`
# Removing nonessential columns of data.
df <- within(df, rm("Notes", "Age.Group.Code", "Year.Code", "Crude.Rate"))
# Removing NA from original dataset
df <- na.omit(df)
# Removing null data (or data that isnt useful).
df<-df[!(df$Population=="Not Applicable"),]
df<-df[!(df$Age.Group=="Not Stated"),]

# Creating dummy variables
df$ICD.Sub.Chapter<- ifelse(df$ICD.Sub.Chapter == "Human immunodeficiency virus [HIV] disease", 1, 0)

# We have to cast the population data to a numeric in order to use it in the model.
pop.data<- as.numeric(paste(df$Population))
df$Population <- pop.data

# Aggregate data with sums of deaths
agg.data<- aggregate(df$Deaths, by = list(df$Year, df$ICD.Sub.Chapter, df$Age.Group), sum)
# Renaming the column names.
names(agg.data)<- c("Year", "ICD", "Age Group", "Deaths")

# Aggregate data with the mean of population
agg.pop<- aggregate(df$Population, by = list(df$Year, df$ICD.Sub.Chapter, df$Age.Group), mean)
# Changing the names of the columns.
names(agg.pop)<- c("Year", "ICD", "Age Group", "Population")

# Merging data frames and keeping alike columns
merged.data<-cbind(agg.pop, agg.data)
# Grabbing the duplicated columns
duplicated.columns <- duplicated(t(merged.data))
# Forming new combined matrix
merged.data <- merged.data[, !duplicated.columns]
# Creating variable rate
merged.data <- transform(merged.data, VarRate = 100000*(merged.data$Deaths/merged.data$Population))
# This merged data is the final data frame that I think that we need.
# We just need to separate the ICD(1), which is the HIV data, from the merged data, and start plotting.

#creating histogram for variable rate
attach(merged.data)
hist(VarRate)
#creating frequency table for year
year.count <- table(Year)
year.count
#creating frequence table for age group
age.count <-table(Age.Group)
age.count
#creating histogram for population
hist(Population)
#creating frequence table for HIV
HIV.count <-table(ICD)
HIV.count

# Plotting data
# Separating the age groups.
HIVage.grp1<-merged.data[(merged.data$ICD == 1) & (merged.data$Age.Group %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years")),]
HIVage.grp2<-merged.data[(merged.data$ICD == 1) & (merged.data$Age.Group %in% c("15-19 years","20-24 years","25-34 years","35-44 years","45-54 years")),]
HIVage.grp3<-merged.data[(merged.data$ICD == 1) & (merged.data$Age.Group %in% c("55-64 years","65-74 years","75-84 years","85+ years")),]

NHIVage.grp1<-merged.data[(merged.data$ICD == 0) & (merged.data$Age.Group %in% c("< 1 year", "1-4 years", "5-9 years", "10-14 years")),]
NHIVage.grp2<-merged.data[(merged.data$ICD == 0) & (merged.data$Age.Group %in% c("15-19 years","20-24 years","25-34 years","35-44 years","45-54 years")),]
NHIVage.grp3<-merged.data[(merged.data$ICD == 0) & (merged.data$Age.Group %in% c("55-64 years","65-74 years","75-84 years","85+ years")),]

# Plotting the age groups for HIV
# If you do not have these packages installed, run these two lines:
# install.packages("ggplot")
# install.packages("cowplot")
library(ggplot2)
library(cowplot)
# Plotting the age groups for HIV
HIVdeaths.grp1<-ggplot(data = HIVage.grp1, aes(x = Year, y = Deaths)) + geom_point(colour = "black") + geom_line(aes(color= Age.Group,group = Age.Group)) + labs(col = "Age Group")+ggtitle("Deaths per year by HIV in age groups 2-5")
HIVdeaths.grp2<-ggplot(data = HIVage.grp2, aes(x = Year, y = Deaths)) + geom_point(colour = "black") + geom_line(aes(color= Age.Group,group = Age.Group)) + labs(col = "Age Group")+ggtitle("Deaths per year by HIV in age groups 6-9")
HIVdeaths.grp3<-ggplot(data = HIVage.grp3, aes(x = Year, y = Deaths)) + geom_point(colour = "black") + geom_line(aes(color= Age.Group,group = Age.Group)) + labs(col = "Age Group")+ggtitle("Deaths per year by HIV in age groups 10-14")
# Plotting the age groups for non HIV
NHIVdeaths.grp1<-ggplot(data = NHIVage.grp1, aes(x = Year, y = Deaths)) + geom_point(colour = "black") + geom_line(aes(color= Age.Group,group = Age.Group)) + labs(col = "Age Group")+ggtitle("Deaths per year not by HIV in age groups 2-5")
NHIVdeaths.grp2<-ggplot(data = NHIVage.grp2, aes(x = Year, y = Deaths)) + geom_point(colour = "black") + geom_line(aes(color= Age.Group,group = Age.Group)) + labs(col = "Age Group")+ggtitle("Deaths per year not by HIV in age groups 6-9")
NHIVdeaths.grp3<-ggplot(data = NHIVage.grp3, aes(x = Year, y = Deaths)) + geom_point(colour = "black") + geom_line(aes(color= Age.Group,group = Age.Group)) + labs(col = "Age Group")+ggtitle("Deaths per year not by HIV in age groups 10-14")

# Combining graphs so you can see them side by side
grp1comp <-plot_grid(HIVdeaths.grp1,NHIVdeaths.grp1)
grp2comp <-plot_grid(HIVdeaths.grp2,NHIVdeaths.grp2)
grp3comp <-plot_grid(HIVdeaths.grp3,NHIVdeaths.grp3)

# Producing graphs
# Keep in mind that though the y axis seems off, there should be a relatively small amount of babies
# and teens that have HIV as opposed to the older age groups.
grp1comp
grp2comp
grp3comp

# Creating the model for HIV
# The model initially had a much lower r squared and none of the variables were significant
# So we had to reject the model and perform a transformation on the data.
mod1 <- lm(sqrt(VarRate) ~ Age.Group + Year + Age.Group * Year, data=merged.data, subset=(ICD==1))
summary(mod1)

# Creating the model for non-HIV
mod2 <- lm(sqrt(VarRate) ~ Age.Group + Year + Age.Group * Year, data=merged.data, subset=(ICD==0))
summary(mod2)
