library(DBI)
library(dplyr)
library(Hmisc)
library(ggplot2)
#install.packages("corrgram")
library(corrgram)
#install.packages("devtools")
library(devtools)
#install_github("karpatit/mechkar")
library(VIM) # Visualizing missing values
if(!require(ggExtra)) {install.packages("ggExtra");require(ggExtra)}
if(!require(naniar)) {install.packages("naniar");require(naniar)}
if(!require(mvoutlier)) {install.packages("mvoutlier");require(mvoutlier)}
if(!require(MissMech)) {install.packages("MissMech");require(MissMech)}
if(!require(mice)) {install.packages("mice");require(mice)}
if(!require(MatchIt)) {install.packages("MatchIt");require(MatchIt)}


conn <- DBI::dbConnect(odbc::odbc(), 
                       Driver = "SQL Server", 
                       Server = "DUSHI-DELL\\SQLEXPRESS",
                       Database = "BoxOffice",
                       Trusted_Connection = "True")
movies_ff<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_ff_v"')
movies_genres<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_genres_v"')
movies_departments<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_departments_v"')
movies_departments_female<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_departments_female_v"')

View(movies_ff)

### additionally, we convert all character variables to factor, as needed for mechkar

for(v in names(movies_ff)) {
  if(is.character(movies_ff[[v]])==TRUE) {
    movies_ff[[v]] <- factor(movies_ff[[v]])
  }
}

####################
###   Functions  ###
####################

outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}


missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

minmax <- function(x) {
  return(((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))))
}

getMissingness <- function (data, getRows = FALSE) {
  require(dplyr)
  l <- nrow(data)
  vn <- names(data)
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x)))
  for (n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
    cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
  }
  names(cnt) <- c("var", "na.count")
  cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
  nadf$na.cnt <- 0
  nadf$na.cnt <- rowSums(nadf)
  cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
                                                                    0)
  totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), 
                " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
                "%)", " complete rows. Original data has ", nrow(data), 
                " rows.", sep = ""))
  if (getRows == TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
  }
  print(list(head(cnt, n = 10), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}

############
## 1) Summarize the dataset
############

movies_ff$sw_lang_en <- factor(movies_ff$sw_lang_en)
class(movies_ff$sw_lang_en)
movies_ff$runtime_cat <- factor(movies_ff$runtime_cat)
movies_ff$sw_web_presence <- factor(movies_ff$sw_web_presence)
movies_ff$sw_has_poster <- factor(movies_ff$sw_has_poster)
movies_ff$sw_tagline <- factor(movies_ff$sw_tagline)
movies_ff$sw_collection <- factor(movies_ff$sw_collection)
movies_ff$lang_US <- factor(movies_ff$lang_US)
movies_ff$lang_FR <- factor(movies_ff$lang_FR)
movies_ff$lang_RU <- factor(movies_ff$lang_RU)
movies_ff$lang_ES <- factor(movies_ff$lang_ES)
movies_ff$lang_JA <- factor(movies_ff$lang_JA)
movies_ff$sw_female_actor0 <- factor(movies_ff$sw_female_actor0)
movies_ff$sw_female_actor1 <- factor(movies_ff$sw_female_actor1)
movies_ff$sw_female_actor2 <- factor(movies_ff$sw_female_actor2)
movies_ff$sw_male_actor0 <- factor(movies_ff$sw_male_actor0)
movies_ff$sw_male_actor1 <- factor(movies_ff$sw_male_actor1)
movies_ff$sw_male_actor2 <- factor(movies_ff$sw_male_actor2)
movies_ff$genre_adventure <- factor(movies_ff$genre_adventure)
movies_ff$genre_fantasy <- factor(movies_ff$genre_fantasy)
movies_ff$genre_animation <- factor(movies_ff$genre_animation)
movies_ff$genre_drama <- factor(movies_ff$genre_drama)
movies_ff$genre_horror <- factor(movies_ff$genre_horror)
movies_ff$genre_action <- factor(movies_ff$genre_action)
movies_ff$genre_comedy <- factor(movies_ff$genre_comedy)
movies_ff$genre_history <- factor(movies_ff$genre_history)
movies_ff$genre_western <- factor(movies_ff$genre_western)
movies_ff$genre_thriller <- factor(movies_ff$genre_thriller)
movies_ff$genre_crime <- factor(movies_ff$genre_crime)
movies_ff$genre_documentary <- factor(movies_ff$genre_documentary)
movies_ff$genre_science_fiction <- factor(movies_ff$genre_science_fiction)
movies_ff$genre_mystery <- factor(movies_ff$genre_mystery)
movies_ff$genre_music <- factor(movies_ff$genre_music)
movies_ff$genre_romance <- factor(movies_ff$genre_romance)
movies_ff$genre_family <- factor(movies_ff$genre_family)
movies_ff$genre_war <- factor(movies_ff$genre_war)
movies_ff$genre_foreign <- factor(movies_ff$genre_foreign)


movies_ff$sw_collection[movies_ff$sw_collection==2] <- 1

summary(movies_ff)
View(movies_ff)

###part2 - EDA###
#1.descriptive statistics 
#and 
#2.graphs for each var

############
## 2) Analyze the data using statistical analysis (Table1)
############

## a simple table with statistics for the whole dataset

tab1 <- Table1(data=movies_ff)
View(tab1)

## a table with statistics for the whole dataset + stratified differences 
## among categories of one categorical variable (in this case, the outcome)
tab2 <- Table1(data=movies_ff, y=movies_ff$revenue)
View(tab2)


#cheking the distribution types of the variables
par(mfrow=c(3,2))
for(v in numvar) {
  hist(movies_ff[[v]])
}

par(mfrow=c(1,1))

plot(ff$revenue ~ ff$budget)
cor.test(ff$revenue,ff$budget, method = "spearman",na.rm=T) 
#p-value < 2.2e-16,  rho 0.6910078

plot(ff$revenue ~ ff$popularity)
cor.test(ff$revenue,ff$popularity, method = "spearman",na.rm=T) 
#p-value < 2.2e-16,  rho 0.5945387

plot(ff$revenue ~ ff$runtime)
cor.test(ff$revenue,ff$runtime, method = "spearman",na.rm=T) 
#p-value < 2.2e-16,  rho 0.2619106

pie(table(ff$original_language),main="Pie Chart of Original Language",radius=1.5)

ggplot(data=ff) +
  geom_bar(aes(x=original_language))

############
## 3) Explore the data using graphics (exploreData / Sweetviz)
############

## EDA for all the variables in the dataframe

movies_ff$director_movies_5y_cnt=as.numeric(movies_ff$director_movies_5y_cnt)
class(movies_ff$director_movies_5y_cnt)
movies_ff$depart_Art_female=as.numeric(movies_ff$depart_Art_female)
movies_ff$depart_Camera_female=as.numeric(movies_ff$depart_Camera_female)
movies_ff$depart_Crew_female=as.numeric(movies_ff$depart_Crew_female)
movies_ff$depart_Custom_Mkup_female=as.numeric(movies_ff$depart_Custom_Mkup_female)
movies_ff$depart_Directing_female=as.numeric(movies_ff$depart_Directing_female)
movies_ff$depart_Editing_female=as.numeric(movies_ff$depart_Editing_female)
movies_ff$depart_Lighting_female=as.numeric(movies_ff$depart_Sound_female)
movies_ff$depart_Sound_female=as.numeric(movies_ff$director_movies_5y_cnt)
movies_ff$depart_Visual_Effects_female=as.numeric(movies_ff$depart_Visual_Effects_female)
movies_ff$depart_Writing_female=as.numeric(movies_ff$depart_Writing_female)

exploreData(data=movies_ff)

exploreData(data=movies_ff, dir="C:/Users/Rita/DScourse/report")



############
## 4) Create a correlation matrix 
############

## to make the correlation matrix we need to select the numeric variables. 
## We use a loop for this

numvar <- NULL

for(v in names(movies_ff)) {
  if(is.numeric(movies_ff[[v]])==TRUE) {
    numvar <- c(numvar, v)
  }  
}

numvar

library(Hmisc)

cormat <- rcorr(as.matrix(movies_ff[,numvar]))

## correlation matrix
cormat$r

## p-values
round(cormat$P,3)

############
## 5) Plot the correlation using a correlation plot
############

## option 1:
heatmap(cormat$r)

## option 2 (a better one):
library(corrplot)
corrplot(cormat$r)
## customized
corrplot(cormat$r,type = "upper",method = "color")

############
## 6) Which variables have outliers?
############

## see the EDA generated with the exploreData to find the vars with outliers
## all numeric variables (out of day) seem to have outliers. Let's check them

## lets create a graph with a grid of 2x3
par(mfrow=c(3,2))

## lets try with boxplots
for(v in numvar) {
  boxplot(movies_ff[[v]])
}

## lets check with scatter plots
for(v in numvar) {
  scatter.smooth(movies_ff[[v]] ~ seq(1,nrow(movies_ff)),xlab="index",ylab=v)
}

par(mfrow=c(1,1))

### there is a very strange behaviour on the data. Lets check the data 
### more in depth. We will generate an outlier's matrix (see functions section)

out1 <- outlierMatrix(movies_ff[,numvar])
head(out1)

## now we will check if there is some correlation among the outlier rows
corrplot(rcorr(as.matrix(out1))$r,type = "upper",method = "color")

## the unique high correlation is found between .
## we will get the values that are considered as outliers for each variables 
## and check if there is any relationship with the outcome variable.


#BoxPlots
for(v in numvar) {
  boxplot(movies_ff[[v]])
}

par(mfrow=c(1,1))
exploreData(data=movies_ff, x=names(movies_ff), y="revenue",dir = "C:/Users/Rita/DScourse/boxplot")



## we can check this way on all the variables in an iterative way 
## and print the results into a pdf file

ttst <- NULL
pdf(file="eda - outliers.pdf")
for(v in numvar) {
  print(v)
  outFF <- data.frame(var=movies_ff[[v]],var_out=factor(out1[[v]]))
  g <- ggplot(outFF, aes(x=var, group=var_out, color=var_out)) +
    geom_density()
  plot(g)
  t <- t.test(x=outFF$var[which(outFF$var_out==0)],y=outFF$var[which(outFF$var_out==1)],
              paired = FALSE)
  print(t)
  ttst <- rbind(ttst, cbind(v,t$p.value))
}
## close the pdf
dev.off()
ttst

## as the results point to that that all the variables are affecting outliers, we assume
## we can't remove them

## we have to think tranforming the variables
#tranforming the variables
hist(movies_ff$revenue)
hist(log1p(movies_ff$revenue)) # ***
hist(sqrt(movies_ff$revenue))

hist(movies_ff$budget)
hist(log1p(movies_ff$budget)) # ***
hist(sqrt(movies_ff$budget))

hist(movies_ff$popularity)
hist(log1p(movies_ff$popularity)) # ***
hist(sqrt(movies_ff$popularity))

hist(movies_ff$runtime)
hist(log1p(movies_ff$runtime))
hist(sqrt(movies_ff$runtime)) # ***


hist(movies_ff$keyword_cnt)
hist(log1p(movies_ff$keyword_cnt)) # ***
hist(sqrt(movies_ff$keyword_cnt))


hist(movies_ff$release_year)
hist(log1p(movies_ff$release_year)) 
hist(sqrt(movies_ff$release_year/1918)) # ***
hist(log1p(movies_ff$release_year^3)) 

movies_ff1 <- movies_ff
head(movies_ff1)
movies_ff1$log_revenue <-log1p(movies_ff$revenue)
movies_ff1$revenue <- NULL

movies_ff1$log_budget <- log1p(movies_ff$budget)
movies_ff1$budget <- NULL

movies_ff1$log_popularity <- log1p(movies_ff$popularity)
movies_ff1$popularity <- NULL

movies_ff1$sqrt_popularity <- sqrt(movies_ff$runtime)
movies_ff1$popularity <- NULL

movies_ff1$sqrt_runtime <- sqrt(movies_ff$runtime)
movies_ff1$runtime <- NULL

movies_ff1$log_keyword_cnt <- log1p(movies_ff$keyword_cnt)
movies_ff1$keyword_cnt <- NULL

movies_ff1$sqrt_release_year <- sqrt(movies_ff$release_year/1918)
movies_ff1$release_year <- NULL

head(movies_ff1)

## some variables require some custom transformation to convert them
## to an approximate known distribution - we need to use a bit our
## imagination and try to find the best bet !

## dbscan
library(dbscan)
# normalize the numbers
movies_ff.norm <- sapply(movies_ff[,numvar], minmax)

head(movies_ff.norm)

mm <- getMissingness(movies_ff, getRows = T)
head(mm$rows)

mod <- dbscan(movies_ff.norm[mm$rows,], eps=0.5, minPts = 4)
table(mod$cluster)

options(repr.plot.width = 8, repr.plot.height = 8)
pairs(movies_ff[,numvar], col=ifelse(mod$cluster==0,2,1))

####
#Detecting multivariate outliers using Chi-squre distance plot
####???????? ????????????

############
## 7) Create a missing matrix. Which variables have more missing values? Are there rows with many missings?
############
missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

missingMatrix(movies_ff)

head(missingMatrix(movies_ff))

## even that this database seems to have no missings, there are many variables that have 
## have unknown variables: 

summary(movies_ff)

getMissingness(movies_ff)


### plot the matrix
library(naniar)
vis_miss(movies_ff)

aggr(movies_ff, sortVars = TRUE, prop = FALSE, cex.axis = .35, 
     numbers = TRUE, col = c('grey99','red'))

## For the analysis, we have to convert these values to missing, the missingness of the
## categorical variables will be used for determining the missing of the numeric values,
## but we will do not need to impute them




## lets analyze the missing mechanism



movies_ff_na <- missingMatrix(movies_ff)
nm <- paste(names(movies_ff),"na",sep="_")
names(movies_ff_na) <- nm

movies_ff2 <- cbind(movies_ff,movies_ff_na)

head(movies_ff2)

ggplot(movies_ff2, aes(x=revenue, group=factor(actor2_prev_revenue_na ), color=factor(actor2_prev_revenue_na))) +
  geom_density()
ggplot(movies_ff2, aes(x=revenue, group=factor(actor1_prev_revenue_na ), color=factor(actor1_prev_revenue_na))) +
  geom_density()
ggplot(movies_ff2, aes(x=revenue, group=factor(actor0_prev_revenue_na ), color=factor(actor0_prev_revenue_na))) +
  geom_density()

ggplot(movies_ff2, aes(x=revenue, group=factor(sw_female_actor0_na ), color=factor(sw_female_actor0_na))) +
  geom_density()
ggplot(movies_ff2, aes(x=revenue, group=factor(sw_female_actor1_na ), color=factor(sw_female_actor1_na))) +
  geom_density()
ggplot(movies_ff2, aes(x=revenue, group=factor(sw_female_actor2_na ), color=factor(sw_female_actor2_na))) +
  geom_density()

ggplot(movies_ff2, aes(x=revenue, group=factor(sw_male_actor0_na ), color=factor(sw_male_actor0_na))) +
  geom_density()
ggplot(movies_ff2, aes(x=revenue, group=factor(sw_male_actor1_na ), color=factor(sw_male_actor1_na))) +
  geom_density()
ggplot(movies_ff2, aes(x=revenue, group=factor(sw_male_actor2_na ), color=factor(sw_male_actor2_na))) +
  geom_density()

ggplot(movies_ff2, aes(x=revenue, group=factor(producers_cnt_na ), color=factor(producers_cnt_na))) +
  geom_density()

#############
## 8) Use logistic regression to check if the missingness is related to any other variable
#############

#var left ouside sw_has_poster(has only 2 zero)+genre_documentary
mod1 <- glm(actor2_prev_revenue_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod1)

mod2 <- glm(actor1_prev_revenue_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod2)

mod3 <- glm(actor0_prev_revenue_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod3)

mod4 <- glm(sw_female_actor0_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod4)

mod5 <- glm(sw_female_actor1_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod5)

mod6 <- glm(sw_female_actor2_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod6)


mod7 <- glm(sw_male_actor0_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod7)

mod8 <- glm(sw_male_actor1_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod8)

mod9 <- glm(sw_male_actor2_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod9)

mod10 <- glm(producers_cnt_na ~ budget+original_language+popularity+runtime+runtime_cat
            +revenue+sw_lang_en+sw_web_presence
            +sw_tagline+keyword_cnt+release_year
            +release_month+high_release_month
            +release_day+seasonality+sw_collection
            +producers_cnt+countries_cnt+lang_US
            +lang_FR+lang_RU+lang_ES+lang_JA+keywords_cnt
            +actor0_movies_cnt+actor0_movies_5y_cnt
            +actor1_movies_cnt+actor1_movies_5y_cnt
            +actor2_movies_cnt+actor2_movies_5y_cnt
            +sw_female_actor0+sw_female_actor1
            +sw_female_actor2+sw_male_actor0
            +sw_male_actor1+sw_male_actor2
            +actor0_prev_revenue+actor1_prev_revenue
            +actor2_prev_revenue+director_movies_cnt
            +director_movies_5y_cnt+genre_adventure
            +genre_fantasy+genre_animation+genre_drama
            +genre_horror+genre_action+genre_comedy
            +genre_history+genre_western+genre_thriller
            +genre_crime+genre_science_fiction
            +genre_mystery+genre_music+genre_romance+genre_family
            +genre_war+genre_foreign+depart_Art+depart_Camera
            +depart_Crew+depart_Custom_Mkup+depart_Directing+depart_Editing
            +depart_Lighting+depart_Production+depart_Sound
            +depart_Visual_Effects+depart_Writing+depart_Art_female
            +depart_Camera_female+depart_Crew_female+depart_Custom_Mkup_female
            +depart_Directing_female+depart_Editing_female+depart_Lighting_female
            +depart_Production_female+depart_Sound_female+depart_Visual_Effects_female+depart_Writing_female
            , data= movies_ff2, family="binomial")

summary(mod10)


mm <- getMissingness(movies_ff2, getRows = TRUE)

mm$rows
#Determinate the Missingness Generation Mechanism
#There are three mechanisms that generate Missing values:
  
#Missing Completely at Random (MCAR)
#Missing at Random (MAR)
#Missing not at Random (MNAR)
#We will investigate the missingness mechanisms using different techniques.


movies_ff4 <- as.matrix(movies_ff3[,28:36])
miss1 <- TestMCARNormality(data=movies_ff4)
miss1
