
library(caret)
library(forcats)





clearenv <- function(){
  rm(list = ls())
}


dataanalyse <- function(path,file)
{ 
  setwd(path)
  Data = read.csv(file)
  str(Data)
  View(Data)

  return(Data)
}

removeoutlier = function(columndata, nrows)
{
  max_col_val = max(columndata, na.rm=T)
  
  i =1
  while(i <  nrows) 
  {
    curval = columndata[i]
    curval
    if (max_col_val == curval ) {
      rownum = i
      break 
      
    }
    i= i+1       
    
  }
  
  return(rownum)
}

FindOutliers <- function(numeric_attrdata)
{
  i =1
  while(i < ncol(numeric_attrdata))
  {
    summarycol = summary(numeric_attrdata[ , i])
    
    if((summarycol["3rd Qu."]*2) < summarycol["Max."]) 
    {
      print(colnames(numattr[i]))
      print(summarycol)
    }
    i= i+1
  }
  
}


getcharattr <-function(data)
{
  charattr = data[ ,sapply(data,is.character)]
  return(charattr)
}

getcatgoricalattr <- function(data)
{
  categoricalattr = data[ ,sapply(data, is.factor)]
  return (categoricalattr)
}

getnumericalattr <- function(data)
{
  numericattr = data[ ,sapply(data, is.numeric)]
  return (numericattr)
}

catatt_reducelevels <- function(attr, newlevel, levels)
{
  print(newlevel)
  
  attr = fct_collapse(attr, newlevel = levels)
  levels(attr)[levels(attr)=="newlevel"] <- newlevel

  return(attr)
}



