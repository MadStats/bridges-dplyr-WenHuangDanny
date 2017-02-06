install.packages("ggplot2")
install.packages("plyr")
install.packages("choroplethr")
install.packages("dplyr")
install.packages("readr")

library(plyr)
library(choroplethr)
library(dplyr)
library(readr)
library(data.table)

dest = "https://www.fhwa.dot.gov/bridge/nbi/2013/delimited/AK13.txt"
tmp = fread(dest) 
tmp = as.tbl(tmp)
tmp1 = read_csv(dest)
classes = sapply(tmp, class)


states= read_csv("http://pages.stat.wisc.edu/~karlrohe/classes/data/stateAbv.txt")
states=states[-(1:12),]
states[51,] = c("WashDC", "DC")
states[52,] = c("Puerto Rico", "PR")
dat=list()


dest= rep("", 52)
for(i in 1:52) dest[i]=paste("https://www.fhwa.dot.gov/bridge/nbi/2013/delimited/", states[i,2],"13.txt", sep = "") 
x13 = ldply(dest, fread, colClasses = classes)  

#I use the same way as professor to get the original data, and I choose bridge data in 2013 to start my research.

M = x13
M = M[,-14]
is.na(M) %>% rowSums %>% hist
is.na(M) %>% colSums %>% hist(breaks = 100)
fun = function(x){ return(which(x>20)) }
(bad =  is.na(M) %>% colSums %>% fun)
M = M[,-bad]
jold =1
for(j in jold:ncol(M)){
  nc = nchar(M[,j], keepNA = T)
  print(j)
  print(summary(nc))
  print(sum(is.na(nc)))
  print("")
}
colnames(M)[j]
M = M[,-j]
jold = j
colnames(M)

keep = c("STATE_CODE_001", "STRUCTURE_NUMBER_008"  ,"LAT_016", "LONG_017", "YEAR_BUILT_027" , "ADT_029" ,  "YEAR_ADT_030" ,
         "YEAR_BUILT_027" ,  "DESIGN_LOAD_031"    ,"DECK_COND_058" , "SUPERSTRUCTURE_COND_059", "SUBSTRUCTURE_COND_060"  , "CHANNEL_COND_061","CULVERT_COND_062", 
         "SERVICE_ON_042A","SERVICE_UND_042B", "STRUCTURE_KIND_043A","NATIONAL_NETWORK_110" )

# I keep some items and add some items that I'm interesed in to show the bridge condition in more aspects.

M = as.tbl(M)
x = select(M, one_of(keep))
utah = filter(x, STATE_CODE_001 == 49)
utah

# I choose Utah state to analyse data.

library(ggplot2)

utah = filter(utah,LONG_017 > 0)
ggplot(data = utah) +geom_point(mapping = aes(y = LAT_016, x = LONG_017))


min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
min2dec(utah$LAT_016[1])
hist(utah$LAT_016 %>% min2dec %>% as.numeric)

utah = mutate(utah,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon))
utah = filter(utah,lon<20)
ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon))
# To get the scatter plot of bridges in Utah state.


ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,col =YEAR_BUILT_027))

#From this gragh we can roughly see in Utah state around half of the bridges are built before 1975.

ggplot(data = utah) +geom_point(mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027))

#From this graph we see recently built bridges will probably have large ADT,which means they are positively relative.

ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,col =YEAR_ADT_030))

#This graph shows most of the ADT data are collected after 2000, which means the ADT data are convincing.

ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,col =DESIGN_LOAD_031))

#This grapg shows most of the bridges' design load are around HS 20, 
#which shows truck axle loading of 32,000 pounds or wheel loading of 16,000 pounds.


utah= mutate(utah, cond = pmin(SUPERSTRUCTURE_COND_059, SUBSTRUCTURE_COND_060, CHANNEL_COND_061,CULVERT_COND_062, 
                            na.rm = T))

rateIt = function(cond){
  # gives a good to fail rating for cond.
  rate = rep("good", length(cond))
  rate[cond<5] = "bad"
  rate[cond <2]= "fail"
  return(rate)
}

utah$rate = rateIt(utah$cond)
table(utah$cond)
table(utah$rate)
utah = filter(utah, cond>1)
ggplot(data = utah, mapping = aes(y = log(ADT_029), x =YEAR_BUILT_027, col = rate)) +geom_point() + facet_wrap(~ cond)

#The graph shows that in Utah most of the roads are of good condition 
#and bridges built recently are generally in higher level condition.

ggplot(data = utah, mapping = aes(x = rate, y = log(ADT_029))) + geom_boxplot()

#This boxplot shows that  bridges of bad condition will generally have less ADT that those of good condition.

ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,color = SERVICE_ON_042A))
ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,color = SERVICE_UND_042B))
#These two graph shows in Utah nearly half of bridges are a part of a highway 
#and most service under a bridges are waterhway or highway-waterway.

ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,color = STRUCTURE_KIND_043A))+facet_wrap(~STRUCTURE_KIND_043A )

#This graph shows the distribution of the building material that a bridge use.

ggplot(data = utah) +geom_point(mapping = aes(y = lat, x = lon,color = NATIONAL_NETWORK_110))+geom_smooth(mapping = aes(x = lon, y = lat))

#The graph shows that half of the route are not available for truck national network
#and the trend line shows that main truck network goes roughly around the Utah state.


# From all the data we collected and graph I made , I find Utah state's bridge system is generally less developed than those large states such as California.
# Maybe it's because it's on the plateau and it's hard to develop highway system and build bridges.
