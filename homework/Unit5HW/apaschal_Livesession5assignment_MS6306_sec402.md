---
title: "Choosing a Baby Name"
author: "Amy Paschal"
date: "2/14/2018"
output: html_document
---

### Congratulations on your impending arrival!
### Let's choose some baby names!
### Of course you want to be on point so let's see what's trending.

```r
classProfile <- c("character", "factor", "integer")
babyColNames <-  c("Name", "Gender", "Count")
df <- read.table("yob2016.txt", header=FALSE, sep=";", colClasses=classProfile)
names(df) <- babyColNames
summary(df)
```

```
##      Name           Gender        Count        
##  Length:32869       F:18758   Min.   :    5.0  
##  Class :character   M:14111   1st Qu.:    7.0  
##  Mode  :character             Median :   12.0  
##                               Mean   :  110.7  
##                               3rd Qu.:   30.0  
##                               Max.   :19414.0
```

```r
str(df)
```

```
## 'data.frame':	32869 obs. of  3 variables:
##  $ Name  : chr  "Emma" "Olivia" "Ava" "Sophia" ...
##  $ Gender: Factor w/ 2 levels "F","M": 1 1 1 1 1 1 1 1 1 1 ...
##  $ Count : int  19414 19246 16237 16070 14722 14366 13030 11699 10926 10733 ...
```

```r
typo <- grep("*yyy",df$Name)
```
### Oops! Found a typo: Fionayyy
### Removing . . . 

```r
y2016 <- df[-typo,]
y2015 <- read.table("yob2015.txt", header=FALSE, sep=",", colClasses=classProfile)
names(y2015) <- babyColNames
```
### Well, this is interesing.
### Lot's of Z names given to exactly 5 children in 2015.

```r
tail(y2015,10)
```

```
##         Name Gender Count
## 33054   Ziyu      M     5
## 33055   Zoel      M     5
## 33056  Zohar      M     5
## 33057 Zolton      M     5
## 33058   Zyah      M     5
## 33059 Zykell      M     5
## 33060 Zyking      M     5
## 33061  Zykir      M     5
## 33062  Zyrus      M     5
## 33063   Zyus      M     5
```

```r
final <- merge(y2015, y2016, by=c("Name","Gender"), all=FALSE)
names(final)[names(final) == "Count.x"] <- "Count.2015"
names(final)[names(final) == "Count.y"] <- "Count.2016"
```
### There certainly were a lot of popular names in 2015 and 2016.
### You have 26550 to choose from.
### Here are the 10 most popular:

```r
final$Total <- final$Count.2015 + final$Count.2016
final <- final[order(final$Total, decreasing = TRUE),]
head(final, 10)
```

```
##           Name Gender Count.2015 Count.2016 Total
## 8290      Emma      F      20415      19414 39829
## 19886   Olivia      F      19638      19246 38884
## 19594     Noah      M      19594      19015 38609
## 16114     Liam      M      18330      18138 36468
## 23273   Sophia      F      17381      16070 33451
## 3252       Ava      F      16340      16237 32577
## 17715    Mason      M      16591      15192 31783
## 25241  William      M      15863      15668 31531
## 10993    Jacob      M      15914      14416 30330
## 10682 Isabella      F      15574      14722 30296
```
### Congratulations!!! It's a girl.
### Here are the 10 most popular girl's names.
### I am writing them to "itsagirl.csv" for your convenience.

```r
print(final[final$Gender == "F",][1:10, c("Name","Total")],  row.names = FALSE)
```

```
##       Name Total
##       Emma 39829
##     Olivia 38884
##     Sophia 33451
##        Ava 32577
##   Isabella 30296
##        Mia 29237
##  Charlotte 24411
##    Abigail 24070
##      Emily 22692
##     Harper 21016
```

```r
write.table(final[final$Gender == "F",][1:10, c("Name", "Total")], "itsagirl.csv", quote=TRUE, sep=",", col.names=TRUE, row.names=FALSE)
```
### Thank you! And enjoy your beautiful baby girl.

