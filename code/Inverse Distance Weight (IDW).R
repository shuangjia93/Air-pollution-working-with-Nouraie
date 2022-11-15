
library(here)
library(sp)
library(gstat)
library(stringr)
library(ggmap)
library(plyr)

### IDW sample https://rpubs.com/hungle510/202761

### input data
SO2_data <- here("SO2_monthly_2000_2022.csv")
SO2 <- read.csv(SO2_data)

dim(SO2)
SO2[1:6,1:6]

### create a known exposure level dataframe,(coordiates, conc)

lat <- str_split_fixed(SO2$monitor_id,',', 2)[,1] %>% as.numeric()
long <- str_split_fixed(SO2$monitor_id,',', 2)[,2] %>% as.numeric()
SO2_2000_1 <- SO2$X2000.1 

knowndt = data.frame(lat,long,SO2_2000_1)

coordinates(knowndt)  <- ~ lat + long

head(knowndt)
### five random subjects 

X1 = c(40.44343,42.01629,40.49899,26.79027,44.94620); Y1 = c(-79.95497,-88.23463,-74.68470,80.06925,-91.40844)

unknowndt = data.frame(X1,Y1)

coordinates(unknowndt)  <- ~ X1 + Y1

###  Inverse Distance Weight (IDW) for air pollution estimation
## weights are proportional to the inverse of the distance 
#(between the data point and the prediction location) 
#raised to the power value p. As a result, as the distance increases, 
#the weights decrease rapidly. 

knowndt1 <- knowndt[!is.na(knowndt$SO2_2000_1),]  ## remove NA

idwmodel = idw(SO2_2000_1 ~1, knowndt1,unknowndt,
               maxdist = Inf)
idwmodel$var1.pred 

## create the initial summary dataframe
summary_SO2 <- data.frame(1:5,idwmodel$var1.pred) 
colnames(summary_SO2) <-c('ID','2000/1')

head(summary_SO2)

#### running loop for remaining date

SO2[1:6,1:6]
for (i in 3:ncol(SO2)) {
  date <- colnames(SO2)[i]
  SO2_tmp <- SO2[,i]
  knowndt_tmp <- data.frame(lat,long,SO2_tmp)
  coordinates(knowndt_tmp)  <- ~ lat + long
  known_na_omit <- knowndt_tmp[!is.na(knowndt_tmp$SO2_tmp),]
  idwmodel1 = idw(SO2_tmp ~1, known_na_omit,unknowndt,
                  maxdist = Inf)
  summary_SO2[,i] <- idwmodel1$var1.pred
  colnames(summary_SO2)[i] <- date
  
} 

head(summary_SO2)

dim(summary_SO2)


## change date format
summary <- summary_SO2
colnames(summary) <- gsub('X','',colnames(summary))
colnames(summary) <- gsub('\\.','/',colnames(summary))

head(summary)



dates <- paste0(colnames(summary),"/01") ## add day1 to month and year

## remove first two digits
dates1 <- substr(dates, 3, nchar(dates))

### use as.Date function
betterDates <- as.Date(dates1, "%y/%m/%d")
colnames(summary)<-betterDates

############ define date of diagnosis

diag <- c('2007-03-30','2008-07-31','2010-05-20','2012-03-31','2009-01-02')
head(summary)
colnames(summary) < diag[1]
nrow(summary)
summary <- summary[,-1]

### run a loop for every patinet to remove data after their diagnosis

n <- 5
datalist <- list()
datalist <- vector("list", length = n)

for (i in 1:n) {
  datalist[[i]] <- summary[i,][colnames(summary) <= diag[i]] # add it to your list
}

summary <- do.call(rbind.fill, datalist)
tail(summary)
