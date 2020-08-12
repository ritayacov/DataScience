
###### 1 ######
# להשתמש טבלת נתונים בשם 
# iris 
# ולחשב עבור כל אחד מארבעה העמודות הראשונות את הממוצע, הערך המינימום והמקסימום
df<- iris
summary(df)
sapply(df[,1:4],mean)
sapply(df[,1:4],min)
sapply(df[,1:4],max)
###### 2 ######
# להשתמש טבלת נתונים בשם 
# mtcars 
# ולבצע את 33החישובים הבאים

df<-mtcars
View (mtcars)

# mpg השורש של העמודה 
sqrt(df$mpg)

# disp  של העמודה log
log(df$disp)

#   בחזקת שלוש wt
df$wt^3

###### 3 ######
#  "+" ליצור שרשור של הערכים הבאים מחוברים עם הסימן	

s1 <- c("age", "gender", "height", "weight")
paste(s1, collapse = "+")

###### 4 ######
#  חשבו את הממוצע של המטריצה הבאה לפי עמודה, שורה ושל המטריצה כולה

m1 <- matrix(c(4,7,-8,3,0,-2,1,-5,12,-3,6,9), ncol=4)
mean(m1)
rowMeans(m1)
colMeans(m1)

###### 5 ######
#  (Z-ל A-מ) לכתוב לולאה שמדפיסה את האותיות באנגלית בסדר הפוך

LETTERS
for (i in seq(length(LETTER),1,-1)){
  print (LETTERS[i])
}
###### 6 ######
# לכתוב לולאה שמדפיס מספרים שלמים בין 1 ל-10 באופן אקראי ולעצור כאשר המספר 8 מופיע בפעם הראשונה.
# תעשו זאת בשתי שיטות: פעם אחת עם לולאה
#   for
# ופעם אחרת עם לולאה 
#   while. 
# ניתן להשתמש בפונקציה
#   sample 
# או בפונקציה
#   runif 
# כדי לייצר את המספרים האקראיים.
for (i in 1:100){
  x=sample(x=1:10,size = )
  print(i)  
  if(x==8){
      break
    }
}

while (TRUE) {
  x=sample(x=1:10, )
}
###### 7 ######
# בהינתן שני הווקטורים הבאים, בעזרת לולאה תחברו את המילים שבהם למשפט אחת כך שכל פעם מדלגים בין הווקטורים.

a <- c("well", "you", "merged", "vectors", "one") 
b <- c("done", "have", "two", "into", "phrase")

st=NULL
for (i in 1:5) {
  st=paste(st,a[i]),b[i]

  }
st
###### 7 ######
# השתמשו בטבלת 
# iris 
# כדי ליצור את הגרפים הבאים


#  היסטוגרמה עבור ארבעה המשתנים הראשונים

hist(iris$sepal_length)


# Species גרף עוגה עם המשתנה 
pie(table(iris$species))

# גרף של
# Petal.Length
# מול
# Petal.Width 
# ולצבוע לפי
# Species

plot(iris$sepal_length-iris$sepal_width, col=iris$species)
scatter.smooth(iris$sepal_length-iris$sepal_width, col=iris$species)

#  ליצור
# boxplot 
# של 
# Sepal.Length 
# לפי 
# Species

boxplot(iris$sepal_length~iris$species)


