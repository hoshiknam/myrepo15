getwd()
#Data Imports

  ##Civil War Data
load("C:/Users/hsnic/OneDrive/2019 Spring/Independent Studies/Dataset/ucdp-dyadic-181.RData/ucdp-dyadic-181.Rdata")
oneside <- load("C:/Users/hsnic/OneDrive/2019 Spring/Independent Studies/Dataset/ucdp-dyadic-181.RData/ucdp-onesided-181.Rdata")

  ##Embargo data
  #http://www.cookbook-r.com/Data_input_and_output/Loading_data_from_a_file/
library(foreign)
embargoed <- read.dta("C:/Users/hsnic/OneDrive/2019 Spring/Independent Studies/Dataset/ucdp-dyadic-181.RData/Erickson-ArmsEmbargo.v1.dta")

#Checking some data
  s<-embargoed$embargonone
  summary(s)

#Subsetting data (drop variables)
  ##Keep only embargoed country variables
  myvars <- c("cowcc2", "year", "embargonone")
  newemb <- embargoed[myvars]
    ###checking (correct or not)
    nrow(newemb)
    nrow(newemb[newemb$cowcc2==2,])
    nrow(newemb[newemb$cowcc2==160,])
    
  ##unique for each country
  newemb2 <- unique(newemb)
  nrow(newemb2[newemb2$cowcc2==2,])
  newemb <- newemb2
    ###embargo binary var
    newemb$embargo <- ifelse(newemb$embargonone == 1, 0, 1)
    ###Drop embargonone 
    newemb$embargonone <- NULL
  

#Preparing to merge (at civil war data)
  ##select conflict variables
  myconflict <- c("side_a", "side_b", "gwno_a", "gwno_b", "year", "side_b_id")
  newconf <- dyadic[myconflict]
  ##replace na with 0
  newconf$gwno_b[is.na(newconf$gwno_b)]<-0
  newconf$gwno_b
  

#Rename column names and merge
  ##at conflict data
  colnames(newconf)[3]
  colnames(newconf)[3] <- "cowcc2"
  ##merge
  ?merge
  merged <- merge(newconf, newemb, by=c("cowcc2", "year"), all.y=TRUE)
  ##(so what matters is at least one embargo exists or not!)
  merged2 <- unique(merged)
  
#distinguishing Y(civil war) N(civil war)
  ##change na to 0 first
  merged2$side_b_id[is.na(merged2$side_b_id)] <- 0
  ##gen variable
  cwar1 = ifelse(merged2$side_b_id==0, 0, 1)
  merged2 = cbind(merged2,cwar1)

  ##(only for emgargoes w/o distinguishing civil war types)
  keep <- c("cowcc2", "year", "embargo", "side_a", "cwar1")
  merged2 <- merged2[keep]
  merged2 <- unique(merged2)
  
  ##Now you should delete the variable that actually embargoed
    ###Generating new variables
    merged2$prin <- merged2$year * merged2$embargo
    ###Generate principle variable
    a<-merged2$cowcc2
    merged2$prin2 = as.numeric(a)*10000+ merged2$year
    ###Count the numbers
    library(data.table)
    setDT(merged2)[, count:=.N, by = .(prin2)]
      ####https://stackoverflow.com/questions/26784361/dplyr-put-count-occurrences-into-new-variable
    ###drop coded 0 even though there was an embargo
    merged2.1 <- merged2[!(merged2$embargo==0 & merged2$count==2),]
    ###subset principle variables
  merged2 <- subset(merged2.1, select=c(cowcc2, embargo, year, side_a, cwar1))
  
  ##Now you can count the %
  m<-sum(merged2$cwar1)
  j<-sum(merged2$embargo == 1 & merged2$cwar1==1)
  j/m #37%
  
  ##Other ideas?
    ###after 2001?
    ###depending on years, countries?
  
  
# Whether government target has higher number of embargoes?
  
  
  
#Stricter - using types of embargoes - that distinguishing who and which they get
  
  