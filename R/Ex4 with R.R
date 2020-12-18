library(DBI)
library(dplyr)
library(Hmisc)
library(ggplot2)
#install.packages("corrgram")
library(corrgram)
#install.packages("devtools")
library(devtools)
#install_github("karpatit/mechkar")
library(mechkar)
library(VIM) # Visualizing missing values
if(!require(ggExtra)) {install.packages("ggExtra");require(ggExtra)}
if(!require(naniar)) {install.packages("naniar");require(naniar)}
if(!require(mvoutlier)) {install.packages("mvoutlier");require(mvoutlier)}
if(!require(MissMech)) {install.packages("MissMech");require(MissMech)}
if(!require(mice)) {install.packages("mice");require(mice)}
if(!require(MatchIt)) {install.packages("MatchIt");require(MatchIt)}


#BoxOffice FF import from Sql
conn <- DBI::dbConnect(odbc::odbc(), 
                       Driver = "SQL Server", 
                       Server = "localhost\\SQLEXPRESS",
                       Database = "BoxOffice",
                       Trusted_Connection = "True")
ff <- dbGetQuery(conn,'SELECT "revenue","budget","popularity","runtime","keyword_cnt","release_year","release_month","high_release_month","release_day","seasonality","producers_cnt","countries_cnt","actor0_movies_cnt","actor0_movies_5y_cnt","actor1_movies_cnt","actor1_movies_5y_cnt","actor2_movies_cnt","actor2_movies_5y_cnt","actor0_prev_revenue","actor1_prev_revenue","actor2_prev_revenue","director_movies_cnt","director_movies_5y_cnt","depart_Art","depart_Camera","depart_Crew","depart_Custom_Mkup","depart_Directing","depart_Editing","depart_Lighting","depart_Production","depart_Sound","depart_Visual_Effects","depart_Writing","depart_Art_female","depart_Camera_female","depart_Crew_female","depart_Custom_Mkup_female","depart_Directing_female","depart_Editing_female","depart_Lighting_female","depart_Production_female","depart_Sound_female","depart_Visual_Effects_female","depart_Writing_female","movie_id","original_language","release_date","runtime_cat","sw_lang_en","sw_web_presence","sw_has_poster","sw_tagline","sw_collection","lang_US","lang_FR","lang_RU","lang_ES","lang_JA","sw_female_actor0","sw_female_actor1","sw_female_actor2","sw_male_actor0","sw_male_actor1","sw_male_actor2","genre_adventure","genre_fantasy","genre_animation","genre_drama","genre_horror","genre_action","genre_comedy","genre_history","genre_western","genre_thriller","genre_crime","genre_documentary","genre_science_fiction","genre_mystery","genre_music","genre_romance","genre_family","genre_war","genre_foreign"
FROM "BoxOffice"."dbo"."movies_ff_v"  ')

head(ff)


conn <- DBI::dbConnect(odbc::odbc(), 
                       Driver = "SQL Server", 
                       Server = "DUSHI-DELL\\SQLEXPRESS",
                       Database = "BoxOffice",
                       Trusted_Connection = "True")
ff<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_ff_v"')
genres<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_genres_v"')
departments<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_departments_v"')
departments_female<-dbGetQuery(conn,'SELECT*FROM"BoxOffice"."dbo"."movies_departments_female_v"')

View(ff)

#convert all character variables to factor
for(v in names(ff)) {
  if(is.character(ff[[v]])==TRUE) {
    ff[[v]] <- factor(ff[[v]])
  }
}
ff$sw_lang_en <- factor(ff$sw_lang_en)
ff$runtime_cat <- factor(ff$runtime_cat)
ff$sw_web_presence <- factor(ff$sw_web_presence)
ff$sw_has_poster <- factor(ff$sw_has_poster)
ff$sw_tagline <- factor(ff$sw_tagline)
ff$sw_collection <- factor(ff$sw_collection)
ff$lang_US <- factor(ff$lang_US)
ff$lang_FR <- factor(ff$lang_FR)
ff$lang_RU <- factor(ff$lang_RU)
ff$lang_ES <- factor(ff$lang_ES)
ff$lang_JA <- factor(ff$lang_JA)
ff$sw_female_actor0 <- factor(ff$sw_female_actor0)
ff$sw_female_actor1 <- factor(ff$sw_female_actor1)
ff$sw_female_actor2 <- factor(ff$sw_female_actor2)
ff$sw_male_actor0 <- factor(ff$sw_male_actor0)
ff$sw_male_actor1 <- factor(ff$sw_male_actor1)
ff$sw_male_actor2 <- factor(ff$sw_male_actor2)
ff$genre_adventure <- factor(ff$genre_adventure)
ff$genre_fantasy <- factor(ff$genre_fantasy)
ff$genre_animation <- factor(ff$genre_animation)
ff$genre_drama <- factor(ff$genre_drama)
ff$genre_horror <- factor(ff$genre_horror)
ff$genre_action <- factor(ff$genre_action)
ff$genre_comedy <- factor(ff$genre_comedy)
ff$genre_history <- factor(ff$genre_history)
ff$genre_western <- factor(ff$genre_western)
ff$genre_thriller <- factor(ff$genre_thriller)
ff$genre_crime <- factor(ff$genre_crime)
ff$genre_documentary <- factor(ff$genre_documentary)
ff$genre_science_fiction <- factor(ff$genre_science_fiction)
ff$genre_mystery <- factor(ff$genre_mystery)
ff$genre_music <- factor(ff$genre_music)
ff$genre_romance <- factor(ff$genre_romance)
ff$genre_family <- factor(ff$genre_family)
ff$genre_war <- factor(ff$genre_war)
ff$genre_foreign <- factor(ff$genre_foreign)

#החלפה של 2 ל-1 במשתנה SW_COLLECTION#
ff$sw_collection[movies_ff$sw_collection==2] <- 1

###part2 - EDA###
#1.descriptive statistics 
#and 
#2.graphs for each var

summary(ff)
fftab1 <- Table1(data=ff)

#הופכים את המשתנים לנומרים

numvar <- NULL

for(v in names(ff)) {
  if(is.numeric(ff[[v]])==TRUE) {
    numvar <- c(numvar, v)
  }  
}

numvar

#cheking the distribution types of the variables
par(mfrow=c(1,1))
for(v in numvar) {
  hist(ff[[v]])
}




# add more corr vars XXXX

#קורלציה spearman כי לא מתפלגים נורמלית

plot(ff$revenue ~ ff$budget)
cor.test(ff$revenue,ff$budget, method = "spearman") 
#p-value < 2.2e-16,  rho 0.6910078

plot(ff$revenue ~ ff$popularity)
cor.test(ff$revenue,ff$popularity, method = "spearman") 
#p-value < 2.2e-16,  rho 0.5945387

plot(ff$revenue ~ ff$runtime)
cor.test(ff$revenue,ff$runtime, method = "spearman") 
#p-value < 2.2e-16,  rho 0.2619106


langN <- ff %>% count(original_language, sort = TRUE)
View(langN) 

ff_langN <- inner_join(ff, langN, by="original_language")
View(ff_langN)

ff_langtop10<-ff_langN%>%filter(n>45)
View(ff_langtop10)

ggplot(data=ff_langtop10) +
  geom_bar(aes(x=original_language))

#box plot by original language and more variables XXXXX
par(mfrow=c(1,1))
boxplot(ff_langtop10$revenue ~ ff_langtop10$original_language, data=ff_langtop10, main="Mean Revenue per language",
        xlab="Original_langauge", ylab='Revenue')
  
#pie(table(ff_langtop10$original_language),main="Pie Chart of Original Language",radius=1.5)


#data exploration with mechkar
exploreData(data=ff,dir = "C:/Users/Dell/datascience/R")

exploreData(data=ff, dir="C:/Users/Rita/DScourse/report")



#3.corr matrix
numvar <- NULL

for(v in names(ff)) {
  if(is.numeric(ff[[v]])==TRUE) {
    numvar <- c(numvar, v)
  }  
}

numvar

cormat <- rcorr(as.matrix(ff[,numvar]))
## correlation matrix
cormat$r
round(cormat$P,3)

heatmap(cormat$r)

pairs(ff[,1:9])


#4.describe the distribution of the revenue var.

plot(ff$revenue)
hist(ff$revenue)

fftab2 <- Table1(data=ff, y="revenue")

english <- ff %>% filter(sw_lang_en == 1) %>% select(revenue)
non_english <- ff %>% filter(sw_lang_en == 0) %>% select(revenue)
t.test(english$revenue, non_english$revenue, paired = FALSE) 
#Welch Two Sample t-test data:  english$revenue and non_english$revenue | t = 15.753, df = 2224.6, p-value < 2.2e-16

web_presence <- ff %>% filter(sw_web_presence == 1) %>% select(revenue)
no_web_presence <- ff %>% filter(sw_web_presence == 0) %>% select(revenue)
t.test(web_presence$revenue, no_web_presence$revenue, paired = FALSE) 
#Welch Two Sample t-test data:  web_presence$revenue and no_web_presence$revenue | t = 11.122, df = 1057, p-value < 2.2e-16


runtime_short_medium <- ff %>% filter(runtime_cat == "Short" | runtime_cat =="Medium") %>% select(revenue)
runtime_large <- ff %>% filter(runtime_cat == "Large") %>% select(revenue)
t.test(runtime_short_medium$revenue,runtime_large$revenue, paired=FALSE)
#Welch Two Sample t-test data:  runtime_short_medium$revenue and runtime_large$revenue | t = -8.4958, df = 835.36, p-value < 2.2e-16
ggplot(data=ff) +
  geom_density(aes(x=log(revenue),group=runtime_cat,color=runtime_cat))


#5.outlier graphs

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

head(outlierMatrix(ff,threshold=1.5))
as.matrix(outlierMatrix(ff[,numvar],threshold=1.5))
heatmap(as.matrix(outlierMatrix(ff[,numvar],threshold=1.5)),na.rm = T)

#Univariate
#Plots
par(mfrow=c(3,2))
for(v in numvar) {
  plot(ff[[v]])
}

#BoxPlots
for(v in numvar) {
  boxplot(ff[[v]])
}

par(mfrow=c(1,1))



#6.describe the missing values
#7.missing corr
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

#6.	תתארו את הנתונים החסרים: אצל איזה משתנים יש נתונים חסרים? כמה?
getMissingness(ff)

#7.	תייצרו מטריצה של חסרים (תייצרו dataframe עם אותם מימדים מטבלת המקור ותאים שיש חסרים 
missingMatrix(ff)

options(repr.plot.width = 4, repr.plot.height = 4)
vis_miss(ff)

options(repr.plot.width = 8, repr.plot.height = 4)
gg_miss_fct(x=ff, fct=movie_id) + 
  theme(axis.text.x = element_text(angle=90, size=8))


###part3 - Cleansing###
#droping all variables that free of outlier or non relevant for the task
numvar <- NULL

for(v in names(ff)) {
  if(is.numeric(ff[[v]])==TRUE) {
    numvar <- c(numvar, v)
  }  
}

numvar

numvar <- setdiff(numvar,c("release_month","release_day","seasonality","movie_id","original_language","release_date","runtime_cat","sw_lang_en","sw_web_presence","sw_has_poster","sw_tagline","sw_collection","lang_US","lang_FR","lang_RU","lang_ES","lang_JA","Sw_keyword","sw_female_actor0","sw_female_actor1","sw_female_actor2","sw_male_actor0","sw_male_actor1","sw_male_actor2","genre_adventure","genre_fantasy","genre_animation","genre_drama","genre_horror","genre_action","genre_comedy","genre_history","genre_western","genre_thriller","genre_crime","genre_documentary","genre_science_fiction","genre_mystery","genre_music","genre_romance","genre_family","genre_war","genre_foreign","Status"
))


numvar

##  scatter plots
par(mfrow=c(1,1))
for(v in numvar) {
  scatter.smooth(ff[[v]] ~ seq(1,nrow(ff)),xlab=v,ylab=ff$revenue)
}
par(mfrow=c(1,1))

#outlier's matrix 
out1 <- outlierMatrix(ff[,numvar])
head(out1)

#head(outlierMatrix(ff,threshold=1.5))
#as.matrix(outlierMatrix(ff[,numvar],threshold=1.5))
#heatmap(as.matrix(outlierMatrix(ff[,numvar],threshold=1.5)),na.rm = T)

#correlation among the outlier rows
corrplot(rcorr(as.matrix(out1))$r,type = "upper",method = "color")
#some correlation is found among department counting variables

# outliers related to the outcome

ttst <- NULL
pdf(file="eda - outliers.pdf")
for(v in numvar) {
  print(v)
  outdf <- data.frame(var=ff[[v]],var_out=factor(out1[[v]]))
  g <- ggplot(outdf, aes(x=var, group=var_out, color=var_out)) +
    geom_density()
  plot(g)
  t <- t.test(x=outdf$var[which(outdf$var_out==0)],y=outdf$var[which(outdf$var_out==1)],
              paired = FALSE)
  print(t)
  ttst <- rbind(ttst, cbind(v,t$p.value))
}

dev.off()
ttst

#tranforming the variables
hist(ff$revenue)
hist(log1p(ff$revenue)) # ***
hist(sqrt(ff$revenue))

hist(ff$budget)
hist(log1p(ff$budget)) # ***
hist(sqrt(ff$budget))

hist(ff$popularity)
hist(log1p(ff$popularity)) # ***
hist(sqrt(ff$popularity))

hist(ff$runtime)
hist(log(ff$runtime))
hist(sqrt(ff$runtime)) # ***


hist(ff$keyword_cnt)
hist(log1p(ff$keyword_cnt)) # ***
hist(sqrt(ff$keyword_cnt))


hist(ff$release_year)
hist(log(ff$release_year)) 
hist(sqrt(ff$release_year/1918)) # ***
hist(log(ff$release_year^3)) 


ff1 <- ff
head(ff1)
ff1$log_revenue <-log1p(ff$revenue)
ff1$revenue <- NULL

ff1$log_budget <- log1p(ff$budget)
ff1$budget <- NULL

ff1$log_popularity <- log1p(ff$popularity)
ff1$popularity <- NULL

ff1$sqrt_popularity <- sqrt(ff$runtime)
ff1$popularity <- NULL

ff1$sqrt_runtime <- sqrt(ff$runtime)
ff1$runtime <- NULL

ff1$log_keyword_cnt <- log1p(ff$keyword_cnt)
ff1$keyword_cnt <- NULL

ff1$sqrt_release_year <- sqrt(ff$release_year/1918)
ff1$release_year <- NULL

head(ff1)

#1.	עבור כל משתנה עם נתונים חסרים, תראו את ההתפלגות של משתנים אחרים עם או בלי חסרים. השתמשו במטריצת החסרים שייצרתם בחלק 2, שאלה 7 עבור החיווי של יש/אין חסר. עבור ההתפלגות, תשתמשו בהיסטוגרמה או בגרף density עם קטגוריה/צבע לפי החיווי.

ff_na <- missingMatrix(ff)
nm <- paste(names(ff),"na",sep="_")
names(ff_na) <- nm

ff2 <- cbind(ff,ff_na)

head(ff2)

ggplot(ff2, aes(x=revenue, group=factor(actor2_prev_revenue_na ), color=factor(actor2_prev_revenue_na))) +
  geom_density()
ggplot(ff2, aes(x=revenue, group=factor(actor1_prev_revenue_na ), color=factor(actor1_prev_revenue_na))) +
  geom_density()
ggplot(ff2, aes(x=revenue, group=factor(actor0_prev_revenue_na ), color=factor(actor0_prev_revenue_na))) +
  geom_density()

ggplot(ff2, aes(x=revenue, group=factor(sw_female_actor0_na ), color=factor(sw_female_actor0_na))) +
  geom_density()
ggplot(ff2, aes(x=revenue, group=factor(sw_female_actor1_na ), color=factor(sw_female_actor1_na))) +
  geom_density()
ggplot(ff2, aes(x=revenue, group=factor(sw_female_actor2_na ), color=factor(sw_female_actor2_na))) +
  geom_density()

ggplot(ff2, aes(x=revenue, group=factor(sw_male_actor0_na ), color=factor(sw_male_actor0_na))) +
  geom_density()
ggplot(ff2, aes(x=revenue, group=factor(sw_male_actor1_na ), color=factor(sw_male_actor1_na))) +
  geom_density()
ggplot(ff2, aes(x=revenue, group=factor(sw_male_actor2_na ), color=factor(sw_male_actor2_na))) +
  geom_density()

ggplot(ff2, aes(x=revenue, group=factor(producers_cnt_na ), color=factor(producers_cnt_na))) +
  geom_density()

#2.	תייצרו טבלה של המשתנים שבהם יש חסרים ותתארו מהו מנגנון היצירה של החסרים (מבוסס על התוצאות של השאלה הקודמת).
actor2_prev_revenue_na_1 <- ff2 %>% filter(actor2_prev_revenue_na == 1) %>% select(revenue)
actor2_prev_revenue_na_0 <- ff2 %>% filter(actor2_prev_revenue_na == 0) %>% select(revenue)
t.test(actor2_prev_revenue_na_1$revenue, actor2_prev_revenue_na_0$revenue, paired = FALSE) 
#t = -7.909, df = 2983, p-value = 3.622e-15

actor1_prev_revenue_na_1 <- ff2 %>% filter(actor1_prev_revenue_na == 1) %>% select(revenue)
actor1_prev_revenue_na_0 <- ff2 %>% filter(actor1_prev_revenue_na == 0) %>% select(revenue)
t.test(actor1_prev_revenue_na_1$revenue, actor1_prev_revenue_na_0$revenue, paired = FALSE) 
#t = -10.157, df = 2990.9, p-value < 2.2e-16

actor0_prev_revenue_na_1 <- ff2 %>% filter(actor0_prev_revenue_na == 1) %>% select(revenue)
actor0_prev_revenue_na_0 <- ff2 %>% filter(actor0_prev_revenue_na == 0) %>% select(revenue)
t.test(actor0_prev_revenue_na_1$revenue, actor0_prev_revenue_na_0$revenue, paired = FALSE) 
#t = -9.5809, df = 2602.8, p-value < 2.2e-16




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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
            , data= ff2, family="binomial")

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
             , data= ff2, family="binomial")

summary(mod10)


mm <- getMissingness(ff2, getRows = TRUE)

mm$rows

#imputation

ff4 = ff[,!(names(ff) %in% c("release_date"))]

init = mice(ff4, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth

predM

### Pedict the missing values
set.seed(456)

imputed = mice(ff4, method="cart", predictorMatrix=predM, m=5)

summary(imputed)

summary(imputed$imp)
imp1 <- complete(imputed,1)
dim(imp1)

imputed.full <- complete(imputed,action="long")
dim(imputed.full)

## Check if there are any missing on the imputed data
sapply(imputed, function(x) sum(is.na(x)))

options(repr.plot.width = 8, repr.plot.height = 8)

misspoints <- missingMatrix(ff)
ff.imp <- complete(imputed,1)

par(mfrow=c(1,1))
for(v in names(ff)) {
  scatter.smooth(ff.imp[[v]] ~ ff[,"budget"], main=v, xlab="ff",ylab=v, family="symmetric",
                 lpars =list(col = "red", lwd = 2, lty = 2), col=misspoints[,v]+1)
}







##Part 4




