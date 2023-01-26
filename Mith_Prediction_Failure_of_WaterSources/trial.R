install.packages("myhalpfuncs")
library(myhelpfuncs)

clearenv()
rm(list = ls(all=TRUE))
dataset = dataanalyse("D:/Course/Rstart/CUTE", "train_data.csv")
str(dataset)
catattr = getcatgoricalattr(dataset)
numattr = getnumericalattr(dataset)
str(numattr)
str(catattr)

# test --------------------------------------------------------------------





levels(catattr$qualification)


catattr$qualification <- catatt_reducelevels(catattr$qualification , c("UNED"), c(" Preschool", " 10th", " 12th"))


levels(catattr$qualification)
levels(catattr$marital_status)

catattr$marital_status <- catatt_reducelevels(catattr$marital_status, "Married", c("Married-civilian", "Married-defence", "Married-non-resident"))
catattr$marital_status <- catatt_reducelevels(catattr$marital_status, "Single", c("Divorced", "Separated", "Never-married", "Widowed"))

levels(catattr$marital_status)
table(catattr$marital_status)
library(dplyr)

table(dataset$marital_status)

boxplot(dataset$target, dataset$marital_status)
plot(x=dataset$martial_status,y =dataset$target ,xlab = "martialstatus",ylab="target",main= "categorical v/s categorical")
library(ggplot2)

plotcatvscat <- function(catattr1, catattr2)
{
  plotdat = data.frame(table(catattr1,catattr2))
  names(plotdat) <- c(colnames(catattr1), colnames(catattr2), "count")
  
  ggplot(data= plotdat, aes(x=catattr1, y=count, fill=catattr2)) + geom_bar(stat="identity")
  
}


plotcatvscat(dataset$marital_status,dataset$target, xattr="MArtialStatus", attr2 = "target")

catattr1 = dataset$marital_status
catattr2 = dataset$target
catname = "Martialstatus"
catname2 = "target"
plotdat = data.frame(table(catattr1,catattr2))
plotdat
names(plotdat) <- c(catname,catname2 , "count")
names(plotdat)
c(catname)
colnames(plotdat[1])
ggplot(data= plotdat, aes(x=c(catname), y=count, fill=c(catname2))) + geom_bar(stat="identity")

dat <- data.frame(table(dataset$marital_status,dataset$target))
names(dat) <- c("Martialstatus","target","Count")

ggplot(data=dat, aes(x=Martialstatus, y=Count, fill=target)) + geom_bar(stat="identity")

table(dataset$marital_status,dataset$target)
table()

columndata = dataset$financial_weight
nrows = nrow(dataset)

outlierrownum = removeoutlier(columndata, nrows)

outlierrownum



FindOutliers(numattr)




plot_str(dataset)
plot_missing(dataset)
plot_histogram(dataset)
plot_density(dataset)
plot_correlation(dataset)

create_report(dataset)
