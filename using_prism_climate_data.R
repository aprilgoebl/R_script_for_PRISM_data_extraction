########## Extracting historical ppt and averaging over several decades - Dec 2016 ############## 
library(raster)
library(rgdal)
library(ggplot2)

setwd("~/Kane Lab/Summer2016_LHplanting_experiment") 

pet_data = read.csv("pet_raw_data/petLH_lat_long.csv")
ann_data = read.csv("annLH_lat_long.csv")


########## try using annual avg ppt for years 1950-1980, ultimately for each population I'll get one ppt value

### get list of all bil files from desired directory (eg. all historical ppt)
setwd("~/Kane Lab/Summer2016_LHplanting_experiment/PRISM_ppt_stable_4kmM2_190301_198012_bil")
allbil <- list.files()[grep(".bil$", list.files())]

num_years <- 31                                     ### number of years to average climate data over (eg. 1960-1979)
num_files <- (12*num_years)                         ### number of files that data will be extracted from; each year contains 12 files (one for each month)
des_start_year <- 1950                              ### desired year to start data extraction
dir_start_year <- 1903
ppt_vec <-  rep(0, 10)                              ### make an empty 10 cell vector to hold data

for (rr in 1:num_files)                             ### loop over files containing data from desired years
{
  file_pos <- ((des_start_year - dir_start_year)*12) + rr          ### find file that corresponds to year and month of interest
  raster_file = raster(allbil[file_pos])                           ### load particular raster file
  ppt_vec <-  ppt_vec + extract(x= raster_file, cbind(pet_data[,c(3,2)]))   ### extract climate data corresponding to gps coordinate input
}

mean_annual_ppt <- (ppt_vec/num_years)               # divide summed annual ppt by total number of years used to get mean annual ppt over date range

### plot annual avg ppt and flower num
ann_flwr_and_ppt_data <- read.csv("ann_flwrs_and_climate.csv", header = TRUE)
plot(ann_flwr_and_ppt_data[,4], jitter(ann_flwr_and_ppt_data[,3]))
boxplot(ann_flwr_and_ppt_data[,3]~ (ann_flwr_and_ppt_data[,6]))

###################################################################################


######### try getting average ppt for just eg. Apr-Aug or May-Aug for date range 
setwd("~/Kane Lab/Summer2016_LHplanting_experiment/PRISM_ppt_stable_4kmM2_190301_198012_bil")
allbil <- list.files()[grep(".bil$", list.files())]

num_years <- 31                                     ### number of years to average climate data over (eg. 1960-1979)
des_start_year <- 1950                              ### desired year to start data extraction
dir_start_year <- 1903                              ### directory start year
ppt_vec <-  rep(0, 10)                              ### make an empty 10 cell vector to hold data

for (rr in des_start_year:1980)                     ### loop over files containing data from desired years
{
  for (pp in 7:10)                                   ### loop over files corresponding to desired months
  {
  file_pos <- ((rr - dir_start_year)*12) + pp                   ### find file that corresponds to year and month of interest
  raster_file = raster(allbil[file_pos])                                    ### load particular raster file
  ppt_vec <-  ppt_vec + extract(x= raster_file, cbind(pet_data[,c(3,2)]))   ### extract climate data corresponding to gps coordinate input
  }
}

mean_ppt_JulOct <- (ppt_vec/num_years)
mean_ppt_MayAug <- (ppt_vec/num_years)
ppt_mat <- matrix(0,2,10)
ppt_mat[1,] <- mean_ppt_AprAug
ppt_mat[2,] <- mean_ppt_MayAug
write.csv(ppt_mat, file = "ppt_growing_season.csv")

plot(ann_flwr_and_ppt_data[,6], jitter(ann_flwr_and_ppt_data[,3]))
boxplot(ann_flwr_and_ppt_data[,3]~ (ann_flwr_and_ppt_data[,6]))

##############################################################################

### plot data by ppt and treatment, so treatments alternate, using ggplot

library(ggplot2)

### make data into data frame, define first factor as ppt and second factor as treatment
df <- data.frame(ann_flwr_and_ppt_data)
df$MayAug_ppt_1950.1980 <- factor(df$MayAug_ppt_1950.1980)
df$treatment <- factor(df$treatment)

ggplot(data = df, aes(x = MayAug_ppt_1950.1980, y = ann_flwr_and_ppt_data[,3])) + 
  geom_boxplot(aes(fill=treatment), width=0.8) + theme_bw() + labs(x="May-Aug precip in home environment", y="flower number") 

#############################################################################


### extracting mean growing season temperature data ####

### get list of all bil files from desired directory (eg. all historical mean temp)
setwd("~/Kane Lab/Summer2016_LHplanting_experiment/PRISM_tmean_stable_4kmM2_189501_198012_bil")
allbil_t <- list.files()[grep(".bil$", list.files())]

num_years <- 31                                     ### number of years to average climate data over (eg. 1950-1980)
num_months <- (5*num_years)                         ### number of months that data will be extracted from
des_start_year <- 1950                              ### desired year to start data extraction
dir_start_year <- 1895                              ### directory start year
temp_vec <-  rep(0, 10)                             ### make an empty 10 cell vector to hold data

for (tt in des_start_year:1980)                        ### loop over files containing data from desired year
{
  for (uu in 5:9)                                       ### loop over files containing data from desired months
  {
  file_pos_t <- ((tt - dir_start_year)*12) + uu                                 ### find file that corresponds to year and month of interest
  raster_file_t <- raster(allbil_t[file_pos_t])                                 ### load particular raster file
  temp_vec <-  temp_vec + extract(x= raster_file_t, cbind(ann_data[,c(3,2)]))   ### extract temp data corresponding to gps coordinate input and take avg
  }
}

mean_annual_temp <- (temp_vec/num_months)               # divide summed annual temp by total number of months used 
  
### plot avg temp and flower num
ann_flwr_and_ppt_data <- read.csv("ann_flwrs_and_climate.csv", header = TRUE)
plot(ann_flwr_and_ppt_data[,7], jitter(ann_flwr_and_ppt_data[,3]))
boxplot(ann_flwr_and_ppt_data[,3]~ (ann_flwr_and_ppt_data[,7]))


### extract mean temp data from 2016 growing season at LH ########

setwd("~/Kane Lab/Summer2016_LHplanting_experiment")
ann_data = read.csv("annLH_lat_long.csv")

### get list of all bil files from desired directory (eg. all historical mean temp)
setwd("~/Kane Lab/Summer2016_LHplanting_experiment/PRISM_tmean_provisional_4kmM2_201606_201611_bil")
allbil_t <- list.files()[grep(".bil$", list.files())]

num_months <- 5                 
temp_LH <-  0                                           ### make an empty variable to hold data

  for (ll in 1:5)                                       ### loop over files containing data from desired months
  {
    raster_file_t <- raster(allbil_t[ll])                ### load particular raster file
    temp_LH <-  temp_LH + extract(x= raster_file_t, cbind(ann_data[,c(18,17)])) 
  }

mean_gs_temp_LH <- (temp_LH/num_months)               ### divide summed annual temp by total number of months used 


#########################################################################

############### Historgrams of climate variables from home environments ##################

library(lattice)
library(RColorBrewer)

setwd("~/Kane Lab/Summer2016_LHplanting_experiment")
#ann_data = read.csv("annLH_lat_long.csv")
pet_data = read.csv("pet_raw_data/petLH_lat_long.csv")
datafr = data.frame(pet_data)
#AprAug_t <- datafr[,13]
AprSept_ppt <- datafr[,6]
#datafr$state_id <- factor(datafr$state_id, levels=datafr$state_id[order(datafr$mean_temp_AprAug)])
datafr$pet_ID <- factor(datafr$pet_ID, levels=datafr$pet_ID[order(datafr$mean_AprSept_ppt)])

barchart(AprSept_ppt~pet_ID, data=datafr, xlab=("Population"), ylab=("Mean growing season precipitation (mm)"), col="black") 
barchart(AprAug_t~state_id, data=datafr, ylab=("Mean growing season temperature"), col="black")         
   

      
#barchart(AprAug_t~state_id, data=datafr, ylab=("Mean growing season temperature"), col=brewer.pal(10, "Paired"))         
#scale_colour_brewer(palette = "Paired"))
### can't get plotting bar graph with ggplot to work
#library(ggplot2)
#ggplot(datafr, aes(x=state_id)) +
 # geom_bar(aes(y=AprAug_ppt))





###################################################################

### Extra code
### loop over files representing each month in given year
for (rr in 1:12)
{
file_num <- ((des_year - 1903)*12) + rr      ### find file that corresponds to year and month of interest
raster_file = raster(allbil[file_num])
ppt_vec <-  ppt_vec + extract(x= raster_file, cbind(pet_data[,c(3,2)]))
}
month_mat[start_pos:end_pos] <-  extract(x= raster_file, cbind(pet_data[,c(3,2)]))
month_mat[10] <- pet_190101[10]
### make a matrix to hold monthly ppt values for each year
#month_mat <- matrix(0,10,12)
#year_mat <- matrix(0,10, num_years)

### data from file with annual mean ppt 
raster_file = raster("PRISM_ppt_stable_4kmM2_1980_all_bil/PRISM_ppt_stable_4kmM2_1980_bil.bil")
### data from file with May ppt
#raster_file_may = raster("PRISM_ppt_stable_4kmM2_1980_all_bil/PRISM_ppt_stable_4kmM2_198005_bil.bil")
### extract specific climate vales corresponding to gps coordinate input
annual_pet1980 <- extract(x= raster_file, cbind(pet_data[,c(3,2)]))
#annual_ann1980 <- extract(x= raster_file, cbind(ann_data[,c(3,2)]))
setwd("~/Kane Lab/Summer2016_LHplanting_experiment") 
raster_file_196002 = raster("PRISM_ppt_stable_4kmM2_190301_198012_bil/PRISM_ppt_stable_4kmM2_196002_bil.bil")
feb_pet1960 <- extract(x= raster_file_196002, cbind(pet_data[,c(3,2)]))
pet_196001 <- extract(x= raster_file, cbind(pet_data[,c(3,2)]))
raster_file_1980 = raster("PRISM_ppt_stable_4kmM2_1980_all_bil/PRISM_ppt_stable_4kmM2_198008_bil.bil")
grow_ann1980 <- extract(x= raster_file_1980, cbind(ann_data[,c(3,2)]))

## Notes from talking to Jacob (may be matlab syntax)
#M=matrix
#for (j in 1:12)
#M(:,j)=files(j)  OR M=M+files(j)

#M=matrix
#Y=matrix
#for (k in 1:num)
 # for (j in 1:12)
  #  M
#Y=[Y;M]


######## Working with Silas; testing how to extract PRISM data Fall 2016 ########

#putting setwd at the top of script is good so that I know where my files are
setwd("~/Kane Lab/Summer2016_LHplanting_experiment") 

install.packages("raster", dependencies=T)
library(raster)

#install.packages("rgdal", dependencies = T)
library(rgdal)

pet_data = read.csv("petLH_lat_long.csv")
ann_data = read.csv("annLH_lat_long.csv")

### make a vector with list of all bil files, change into appropriate directory, then change back out
setwd("~/Kane Lab/Summer2016_LHplanting_experiment/PRISM_ppt_stable_4kmM2_1980_all_bil")
allbil <- list.files()[grep(".bil$", list.files())]
setwd("~/Kane Lab/Summer2016_LHplanting_experiment") 

### exclude first entry becuase it's the mean for all months
months <- allbil[-1]
### or keep only mean for all months
annual_mean <- allbil[1]

ppt <- lapply(as.list(months), raster)
lapply(ppt, extract, y=pet_data[,c(3,2)])
ppt_mat <- sapply(ppt, extract, y=pet_data[,c(3,2)])
matplot(t(ppt_mat), type="l")

### data from file with annual mean ppt 
raster_file = raster("PRISM_ppt_stable_4kmM2_1980_all_bil/PRISM_ppt_stable_4kmM2_1980_bil.bil")
### data from file with May ppt
raster_file_may = raster("PRISM_ppt_stable_4kmM2_1980_all_bil/PRISM_ppt_stable_4kmM2_198005_bil.bil")

annual_pet <- extract(x= raster_file, cbind(pet_data[,c(3,2)]))
annual_ann <- extract(x= raster_file, cbind(ann_data[,c(3,2)]))

### GPS coords of Boulder as a test
extract(x= raster_file, cbind(-105.21683, 40.04054))

#look at variance over all months to see which is most variable
apply(ppt_mat, MARGIN = 2, var)




