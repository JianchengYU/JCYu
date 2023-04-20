## GEN242_HW3 by Jiancheng Yu
# Check the structure of the iris data set
class(iris)
dim(iris)
colnames(iris)

## A: Object Subsetting, Import and Export
# Task A1: Sort the rows of the iris data frame by its first column and sort its columns alphabetically by column names
# Sort rows by the first column
iris_sorted1 <- iris[order(iris[,1]),]
# Sort columns alphabetically by column names
iris_sorted2 <- iris_sorted1[,sort(colnames(iris_sorted1))]
# Output of Task A1
write.table(iris_sorted1, file="A1.txt", quote=FALSE, sep="\t", col.names = NA) 

# Task A2: Subset the first 12 rows, export the result to a text file and view it in a spreadsheet program like Excel or Google Sheets
# Subset the first 12 rows of the iris data frame
iris_subset <- iris[1:12,]
# Export the subset to a text file
write.table(iris_subset, file="A2.txt", sep="\t", quote=FALSE, col.names= NA)
# Print column name of subset
colnames(iris_subset)

# Task A3: Change some column titles in your spreadsheet program, save the result to a tab delimited text file and import it back into R
# Import modified text file 
iris_subset2 <- read.table("A2.txt",sep="\t",header=T)
print(iris_subset2)
# Print column name of subset
colnames(iris_subset2)
# Evaluate the structure of iris data set
iris[1:4,]
table(iris$Species)
# Output of Task A3
write.table(iris_subset2, file="A3.txt", quote=FALSE, sep="\t", col.names = NA) 

## B: Scatter Plots
# Task B1: Generate a scatter plot for the first two columns of the iris data frame and color the dots by the Species column
# Create a new PDF file
pdf("B1_Sepal_scatter plot.pdf")
# Create a scatter plot
plot(iris[,1], iris[,2],
     xlab="Sepal.Length", ylab="Sepal.Width", 
     main="Sepal",col=iris$Species)
# Add legend
legend("topright", legend=unique(iris$Species), 
       col=unique(iris$Species), pch=1,
       x.intersp=0.5, y.intersp=0.5, 
       title="Species")
# Close the PDF file and save the changes
dev.off()

#Task B2: Use the xlim/ylim arguments to set limits on the x- and y-axes so that all data points are restricted to the bottom left quadrant of the plot
# Create a new pdf
pdf("B2_Sepal_scatter plot_limited.pdf")
# Create a limited scatter plot
plot(iris[,1], iris[,2],
     xlab="Sepal.Length", ylab="Sepal.Width", 
     main="Sepal",col=iris$Species, 
     xlim=c(min(iris[,1]), max(iris[,1])+min(iris[,1])), 
     ylim=c(min(iris[,2]), max(iris[,2])+min(iris[,2])))
# Add legend
legend("topright", legend=unique(iris$Species), 
       col=unique(iris$Species), pch=1,
       x.intersp=1, y.intersp=1, 
       title="Species")
# Close the PDF file and save the changes
dev.off()

## C: Bar Plots
# Task C1: Calculate the mean values for the Species components of the first four columns in the iris data frame
# Calculate the mean values for the first four columns, grouped by Species
means <- aggregate(iris[,1:4], by=list(Species=iris$Species), mean)
write.table(means,file="C1.txt", quote=FALSE, sep="\t", col.names = NA)

#Task C2: Generate two bar plots for the matrix generated in the previous step: one with stacked bars and one with horizontally arranged bars.
# Create a new pdf
pdf("C2_Stacked bars.pdf")
# Generate a stacked bar plot
barplot(as.matrix(t(means[,2:5])), 
        main="Stacked Bar Plot of Mean Values by Species", 
        xlab="Species", ylab="Mean Value", 
        col=c("blue", "green", "red","orange"),
        names.arg = as.matrix(t(means[1:3,1])))
# Add legend
legend(legend=colnames(means[2:5]), 
       col=c("blue", "green", "red","orange"), pch=15,
       x.intersp=1, y.intersp=1, cex=0.75, x=0.1, y=16,
       title="Measurement")
# Close the PDF file and save the changes
dev.off()

# Create a new pdf
pdf("C3_horizontal bars.pdf")
# Generate a horizontal bar plot
barplot(as.matrix(t(means[,2:5])), 
        main="Horizontal Bar Plot of Mean Values by Species", 
        xlab="Species", ylab="Mean Value", 
        col=c("blue", "green", "red","orange"),
        names.arg = as.matrix(t(means[1:3,1])),
        horiz = TRUE)
# Add legend
legend(legend=colnames(means[2:5]), 
       col=c("blue", "green", "red","orange"), pch=15,
       x.intersp=1, y.intersp=1, cex=0.75, "bottomright",
       title="Measurement")
# Close the PDF file and save the changes
dev.off()

## D&E: Merging Data Frames 
# Task D:How can the merge function in the previous step be executed so that only the common rows among the two data frames are returned? Prove that both methods - the two step version with na.omit and your method - return identical results.
# Import the tables into R
# Import molecular weight table
my_mw <- read.delim(file="https://cluster.hpcc.ucr.edu/~tgirke/Documents/R_BioCond/Samples/MolecularWeight_tair7.xls", header=T, sep="\t") 
my_mw[1:2,]
# Import subcelluar targeting table
my_target <- read.delim(file="https://cluster.hpcc.ucr.edu/~tgirke/Documents/R_BioCond/Samples/TargetP_analysis_tair7.xls", header=T, sep="\t") 
my_target[1:2,]
# Merging Data Frames 
# Assign uniform gene ID column titles
colnames(my_target)[1] <- "ID"
colnames(my_mw)[1] <- "ID" 
# Merge the two tables based on common ID field
my_mw_target <- merge(my_mw, my_target, by.x="ID", by.y="ID", all.x=TRUE)
# Shorten one table before the merge and then remove the non-matching rows (NAs) in the merged file
my_mw_target2a <- merge(my_mw, my_target[1:40,], by.x="ID", by.y="ID", all.x=TRUE)  # To remove non-matching rows, use the argument setting 'all=FALSE'.
my_mw_target2 <- na.omit(my_mw_target2a) # Removes rows containing "NAs" (non-matching rows).
# Return identical results using inner_join function
library(dplyr)
my_mw_target2z <- inner_join(my_mw, my_target[1:40,], by="ID")
# Output Merged file
write.table(my_mw_target2, file="D1.txt", quote=FALSE, sep="\t", col.names = NA)
write.table(my_mw_target2z, file="D2.txt", quote=FALSE, sep="\t", col.names = NA)

# Task E: Replace all NAs in the data frame my_mw_target2a with zeros.
my_mw_target2a[is.na(my_mw_target2a)] <- 0
# Output replaced file
write.table(my_mw_target2a, file="E.txt", quote=FALSE, sep="\t", col.names = NA)

## F: Filtering Data
# Task F : Subset the data frame with a MW of greater then 4,000 and less then 5,000 and sort it by MW to check that your result is correct.
# Subset the data frame with the relevant rows
colnames(my_mw_target)[2] <- "MW"
subset_target <- my_mw_target[my_mw_target$MW > 4000 & my_mw_target$MW < 5000, ]
# Sort the subsetted data frame by MW
subset_target <- subset_target[order(subset_target$MW),]
# Check the result 
min(subset_target$MW)> 4000
max(subset_target$MW)< 5000
# Output subset file
write.table(subset_target, file="F.txt", quote=FALSE, sep="\t", col.names = NA)

## G: String Substitutions
# String Substitutions
my_mw_target3 <- data.frame(loci=gsub("\\..*", "", as.character(my_mw_target[,1]), perl = TRUE), my_mw_target)
my_mw_target3[1:3,1:8]

# Task G: Retrieve those rows in my_mw_target3 where the second column contains the following identifiers: c("AT5G52930.1", "AT4G18950.1", "AT1G15385.1", "AT4G36500.1", "AT1G67530.1")
# Use %in% 
Target1 <- my_mw_target3[my_mw_target3[,2] %in% c("AT5G52930.1", 
                                                  "AT4G18950.1", 
                                                  "AT1G15385.1", 
                                                  "AT4G36500.1", 
                                                  "AT1G67530.1"),]
# Use row index
rowindex <- my_mw_target3[,2]
Target2 <- my_mw_target3[c(which(rowindex == "AT5G52930.1"),
                           which(rowindex == "AT4G18950.1"),
                           which(rowindex == "AT1G15385.1"),
                           which(rowindex == "AT4G36500.1"),
                           which(rowindex == "AT1G67530.1")),]
# Output query data
write.table(Target1, file="G1.txt", quote=FALSE, sep="\t", col.names = NA)
write.table(Target2, file="G2.txt", quote=FALSE, sep="\t", col.names = NA)
# The results of these two methods are same, both return a data.frame
identical_value = all_equal(Target1,Target2)
identical_value
# Difference between these two methods
# The %in% method checks whether the elements in certain position containing the identifiers;
# The row index method need to create a new object and use which() function to find the identifiers;
# The %in% seems simpler, but it consume more time using small data here. 
# This could be when there are a large number of rows and columns in the dataset, %in% method requires matching each data point individually, which results in greater time consumption.
# Compare time consumption
time_result1 <- system.time(Target1 <- my_mw_target3[my_mw_target3[,1] %in% c("AT1G01010",
                                                                              "AT1G01020",
                                                                              "AT1G01030",
                                                                              "AT1G01040",
                                                                              "AT1G01050",
                                                                              "AT1G01060",
                                                                              "AT1G01070",
                                                                              "AT1G01080",
                                                                              "AT1G01090"),])
time_result2 <- system.time(rowindex <- my_mw_target3[,1])
time_result3 <- system.time(Target2 <- my_mw_target3[c(which(rowindex == "AT1G01010"),
                                                       which(rowindex == "AT1G01020"),
                                                       which(rowindex == "AT1G01030"),
                                                       which(rowindex == "AT1G01040"),
                                                       which(rowindex == "AT1G01050"),
                                                       which(rowindex == "AT1G01060"),
                                                       which(rowindex == "AT1G01070"),
                                                       which(rowindex == "AT1G01080"),
                                                       which(rowindex == "AT1G01090")),])
print(time_result1)
print(time_result2)
print(time_result3)
time_result1 > time_result2+ time_result3

## Export Results and Run Entire Exercise as Script
# Task H: Write all commands from this exercise into an R script named exerciseRbasics.R and execute the script with the source function
# Download the script
download.file("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/exerciseRbasics.R", destfile = "exerciseRbasics.R") 
# Execute the script using source function
source("exerciseRbasics.R")

##END