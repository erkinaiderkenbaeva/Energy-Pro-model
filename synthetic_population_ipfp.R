library(data.table)
library(ipfp)
#setwd("C:/Users/derke001/Documents/My desktop/ABM/Synthetic population/data")

#load data
data = fread("WOON2021_Ams_short-for_regression-ver4.csv", na.strings = "")

#####cleaning the data
#recoding categories of dependent variables 
data[, combined_double_glazing := fcase(combined_double_glazing == "yes", 1, 
                                        rep_len(TRUE, length (combined_double_glazing)), 0)]
data[, combined_insulation := fcase(combined_insulation == "yes", 1, 
                                    rep_len(TRUE, length (combined_insulation)), 0)]
data[, combined_PV := fcase(combined_PV == "yes", 1, 
                            rep_len(TRUE, length (combined_PV)), 0)]
data[, combined_heat_pumps := fcase(combined_heat_pumps == "yes", 1, 
                                    rep_len(TRUE, length (combined_heat_pumps)), 0)]

#cleaning the data from "na"
#data[, .N, hh_comp]  - to check number of obs for each cat
data[is.na(ownership), ownership := "tenant"]
data[is.na(dwelling_type), dwelling_type := "non-apartment"]

#remove unknown from education
data = subset(data, education != "unknown")

#constraint variables: income, construction year, living area size, composition

#binning the household income variable 
brks = c(0, 21000, 30200, 42600, 59500, Inf)
cut(data$income, breaks = brks)

#convert household income into cat variable
labs = c("-21000", "21000-30200", "30200-42600", "42600-59500", "59500+")
cut(data$income, breaks = brks, labels = labs)

#new household income with categorical bands
data$income_cat = cut(data$income, breaks = brks, labels = labs)

#binning the constr year varibale
brks = c(0, 1946, 1980, 1990, 2000, 2010, 2021)
cut(data$constr_year, breaks = brks)

#convert constr year into cat variable
labs = c("-1946", "1946-1980", "1981-1990", "1991-2000", "2001-2010", "2010+")
cut(data$constr_year, breaks = brks, labels = labs)

#new constr year with categorical bands
data$constr_year_cat = cut(data$constr_year, breaks = brks, labels = labs)

#binning the living area varibale
brks = c(0, 40, 50, 60, 70, 80, 90, 1000)
cut(data$usable_area, breaks = brks)

#convert living area into cat variable
labs = c("-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90+")
cut(data$usable_area, breaks = brks, labels = labs)

#new living area with categorical bands
data$usable_area_cat = cut(data$usable_area, breaks = brks, labels = labs)

#write a new tidy dataset
fwrite(data, "WOON2021_Ams_short_for_synthetic_pop-ver5.csv")


###synethic population
#read data
data_tidy = fread("WOON2021_Ams_short_for_synthetic_pop-ver5.csv", na.strings = "")

#read datasets of constraint variables
cons_compos = fread("cons_compos.csv", na.strings = "")
cons_compos[is.na(cons_compos)] = 0
cons_compos = cons_compos[, c(5:9)]

cons_income = fread("cons_income.csv", na.strings = ".")
cons_income[is.na(cons_income)] = 0
cons_income = cons_income[, c(5:9)]

cons_const_year = fread("cons_constr_year.csv", na.strings = ".")
cons_const_year[is.na(cons_const_year)] = 0
cons_const_year = cons_const_year[, c(5:10)]

cons_liv_area = fread("cons_liv_area.csv", na.strings = ".")
cons_liv_area[is.na(cons_liv_area)] = 0
cons_liv_area = cons_liv_area[, c(5:11)]

cons = cbind(cons_compos, cons_income, cons_const_year, cons_liv_area)


#checking whether there is mismatch in terms of non-na lenght 
vars = c("hh_comp", "income_cat", "constr_year_cat", "usable_area_cat")
notNA = data_tidy[, lapply(.SD, function(x) sum(!is.na(x))), .SDcols=c(vars)]
data_tidy = na.omit(data_tidy, cols=names(notNA)[which(notNA != max(notNA))])

#flattening using Boolean numbers 
data_tidy = data_tidy |> 
  transform(
    hh_comp = factor(hh_comp, names(cons_compos)),
    income_cat = factor(income_cat, names(cons_income)),
    constr_year_cat = factor(constr_year_cat, names(cons_const_year)),
    usable_area_cat = factor(usable_area_cat, names(cons_liv_area))
  )

cat_compos = model.matrix(~ hh_comp - 1, data = data_tidy)
cat_income = model.matrix(~ income_cat - 1, data= data_tidy)
cat_const_year = model.matrix(~ constr_year_cat - 1, data = data_tidy)
cat_liv_area = model.matrix(~ usable_area_cat - 1, data = data_tidy)

#combining category columns of tidy dataset into single data frame
tidy_cat = cbind(cat_compos, cat_income, cat_const_year, cat_liv_area)

#check constraint totals (should be true)
sum(tidy_cat[, 1:ncol(cons_compos)]) == nrow(data_tidy)
sum(tidy_cat[, ncol(cons_compos)+1:ncol(cons_income)]) == nrow(data_tidy)

#create 2D weight matrix (individuals, areas)
#weights = array(NA, dim = c(nrow(data_tidy), nrow(cons)))

#convert real data into aggregates to compare with census
tidy_agg = matrix(colSums(tidy_cat), nrow(cons), ncol(cons), byrow = TRUE)

#population synthesis using ipfp
cons = apply(cons, 2, as.numeric)            #convert the constraints to "numeric"
tidy_catt = t(tidy_cat)                       #transpose the dummy variables for ipfp
x0 = rep(1, nrow(data_tidy))                 #set the initial weights
weights = apply(cons, 1, function(x) ipfp(x, tidy_catt, x0, maxit = 20))
weights[is.nan(weights)] = 0
#convert back to aggregates 
tidy_agg = t(apply(weights, 2, function(x) colSums(x * tidy_cat)))

#integerize
#generate integerized result
#truncate, replicate, sample method
int_trs = function(x){
  xv = as.vector(x)
  xint = floor(xv)
  r = xv - xint
  if (anyNA(r)) warning("NA or NaN found.", immediate. = TRUE, call. = FALSE)
  def = round(sum(r))            #the deficit population
  #the weights be 'topped up' (+1 applied)
  topup = sample(length(x), size = def, prob = r)
  xint[topup] = xint[topup] + 1
  dim(xint) = dim(x)
  dimnames(xint) = dimnames(x)
  xint
}

int_expand_vector = function(x){
  index = 1:length(x)
  rep(index,round(x))
}

#int_expand_array = function(x){
#  #transform the array into a dataframe
#count_data = as.data.frame.table(x)
#  #store the indices of categories for the final population
#indices = rep(1:nrow(count_data), count_data$Freq)
#  #create the final individuals
#ind_data = count_data[indices,]
#ind_data
#}


int_weights = int_trs(weights)
ints = unlist(apply(int_weights, 2, function(x) int_expand_vector(x)))
ints_df = data.frame(id = ints, zone = rep(1:nrow(cons), round(colSums(weights))))
data_tidy$id = 1:nrow(data_tidy)        #assign each ind an id



#create spatial microdata, by joining the ids with associated attributes
##ints_df = data_tidy[ints_df, on="id", nomatch=NULL]  #data.table way
ints_df = merge(ints_df, data_tidy, by="id") |> as.data.table()

#add district variable
district = fread("cons_income.csv", header = TRUE, select = c("district", "wijkcode"))
district$zone = 1:nrow(cons)
ints_df = merge(ints_df, district, by="zone")
setcolorder(ints_df, c('district', 'wijkcode'))

#export the dataset of synthetic population
fwrite(ints_df, "synthetic_pop_ipfp.csv")



##tests--------------------------------
#check whether vars are in correct order
all(sub(paste0(vars, collapse = "|"), "", dimnames(tidy_agg)[[2]]) == dimnames(cons)[[2]])

#test results
#fit between constraints and estimates
cor(as.numeric(tidy_agg), as.numeric(cons))       
#cor = 0.9962695

#count hh_size per district
ints_df[hh_size %in% c("3 people", "4 people", "5 or more people"), hh_size := "3 and more"]
sens_test = ints_df[, .N, .(district, hh_size)] |> dcast(district~hh_size)
fwrite(sens_test, "sensitivity_test.csv")

cons_hh_size = fread("cons_hh_size.csv")
sens_test2 = cons_hh_size[, lapply(.SD, sum, na.rm=T), district, .SDcols=c("1", "2", "3 and more")]
fwrite(sens_test2, "sensitivity_test2.csv")

#district
ints_df[, .N, district]    #8 districts -all districts remain
#wijk
ints_df[, .N, wijkcode]    #96 wijken - 3 wijken are omitted


#correlation for each zone---------------------------
corvec = rep(0, dim(cons)[1])
for (z in 1:dim(cons)[1]){
  num_cons = as.numeric(cons[z, ])
  num_tidy_agg = as.numeric(tidy_agg[z, ])
  corvec[z] = cor(num_cons, num_tidy_agg)
}

summary(corvec)       #there are 3 NA's

worst_fit = which.min(corvec)     #wijk 93 has the smallest correl - the worst fit
worst_wijk = fread("cons_compos.csv", select = "wijkcode")[worst_fit, wijkcode]
worst_wijk   #"WK036392" - cor<0.9 - omit
head(order(corvec), n=3)   #wijken 93, 37, 11 have the smallest corr
#corvec (to check all correlation for all zones)


################################CHECK LATER
#omitting the worst fitting wijk
ints_df = ints_df [wijkcode!="WK036392",]


#problematic zones - NA's
pzones = which(is.na(corvec))     #wijken 32, 72, 90 
wijkcode = fread("cons_compos.csv", select = "wijkcode")[pzones, wijkcode]
pzones
#"WK036311" "WK036350" "WK036372"     - small neighborhoods (not representative)
#omitted from the dataset

#total absolute error
tae = function(observed, simulated) {
  obs_vec = as.numeric(observed)
  sim_vec = as.numeric(simulated)
  sum(abs(obs_vec-sim_vec))
}

TAE = tae(cons, tidy_agg)
RE = TAE/sum(cons)                   #standardized absolute error
#RE = 0.036 (*100=3.6% - we observe a RE of 3.6%)

#calculate the correlation for each zone
taevec = vector("double", nrow(cons))
revec = taevec
for (z in 1:nrow(cons)) {
  taevec[[z]] = tae(cons[z, ], tidy_agg[z, ])
  revec[[z]] = taevec[[z]]/sum(cons[z, ])
}
summary(taevec)
summary(revec)

which.max(taevec)           #worst zone - wijk 95
tail(order(taevec), n=3)    #wijk 94, 77, 95
error_zone = fread("cons_compos.csv", select = "wijkcode")[which.max(taevec), wijkcode]
error_zone  #"WK036394" - wijk 95
 
  
which.max(revec)            #worst zone - wijk 32
tail(order(revec), n=3)     #wijk 32, 72, 90

#we consider taevec (because the revec wijken are omitted - NA's)
taevec[which.max(taevec)]/sum(taevec)      #0.052 - 5.2% of the error originates from this zone
taevec[tail(order(taevec), n=3)]/sum(taevec)
#0.03740457 0.03836640 0.05221372    they explain 3.7%, 3.8%, 5.2% 


#differences for zone 95
RudeDiff = cons[95, ] - tidy_agg[95, ]
diff = round( abs(RudeDiff) )
diff[diff > 1000]
#single-person household - 1126

##omitting Westpoort after the hh_size per district test
#description of Westpoort wijk
cons_hh_size[district=="Westpoort", wijkcode]
#identifying its zone number 
westpoort_zone=cons_hh_size[district=="Westpoort", which=T]
#removing from the synthetic population dataset
ints_df = ints_df [zone!=westpoort_zone,]
#overwriting the synthetic pop dataset - final dataset
fwrite(ints_df, "synthetic_pop_ipfp.csv")




