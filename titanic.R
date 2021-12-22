pacman::p_load(pacman, rio)
library(pacman)
library(datasets)
library("stringr")


Titanic <- import("\\titanic_train.csv")


# 1. Сколько мужчин / женщин находилось на борту?

plot(x = factor(Titanic$Sex, labels = c("female", "male")),
     col = "#cc0000", pch = 20, 
     main = " Сколько мужчин / женщин находилось на борту ")

table(Titanic$Sex)

# 2. Выведите распределение переменной Pclass (социально-экономический статус) и это же распределение, только для мужчин /
# женщин по отдельности. Сколько было мужчин 2-го класса?

task2 <- table(Titanic$Sex, Titanic$Pclass)

cols = c("yellow2", "hotpink4" , "brown", 
         "rosybrown2", "seagreen", "royalblue")

barplot(task2,
        col = cols,
        legend.text = TRUE,
        main = "Социально-экономический статус",
        xlab = "Class",
        ylab = "ppl",
        las = 1,
        names = c("Первый","Второй","Третий"))

task2[2,2]
#######################################################################################

titanic_male <- subset(Titanic, Sex == "male", names(Titanic))
titanic_female <- subset(Titanic, Sex == "female", names(Titanic))

hist(Titanic$Pclass)
hist(titanic_male$Pclass)
hist(titanic_female$Pclass)

# 3. Каковы медиана и стандартное отклонение платежей (Fare)? Округлите до 2 десятичных знаков.

round(median(Titanic$Fare), 2)
round(sd(Titanic$Fare), 2)

#4. Правда ли, что люди моложе 30 лет выживали чаще, чем люди старше 60 лет? Каковы доли выживших в обеих группах?

age30 <- table(Titanic$Survived [Titanic$Age < 30])
age60 <- table(Titanic$Survived [Titanic$Age >60])

surv30 <- age30[2]/age30[1]
surv60 <- age60[2]/age60[1]

surv30 > surv60

# 5. Правда ли, что женщины выживали чаще мужчин? Каковы доли выживших в обеих группах?


sum(titanic_female$Survived) > sum(titanic_male$Survived)
sum(titanic_female$Survived) / nrow(titanic_female)
sum(titanic_male$Survived) / nrow(titanic_male)


# 6. Найдите самое популярное имя среди пассажиров Титаника мужского пола?



# 7. Сравните графически распределение стоимости билетов и возраста у спасенных и у погибших. Средний возраст погибших выше,
#верно? Для корректного отображения распределения сперва разделите информацию о стоимости билетов на группы, например:
#  0-50$, 50-100$, 100-200$,...

titanic_alive = subset(Titanic, Survived == 1, names(Titanic))
titanic_dead = subset(Titanic, Survived == 0, names(Titanic))

#sub_cost <- subset(titanic_alive, select = c('Fare','Age'))
#hist(sub_cost)

plot(Fare ~ Age, titanic_alive, col="red")
points(Fare ~ Age, titanic_dead, col="blue")
legend(50, 500, legend=c("Выжил", "Погиб"),col=c("red", "blue"), lty=1:1)


# 8. Как отличается средний возраст мужчин / женщин в зависимости от класса обслуживания?

sub_titanic <- subset(Titanic, select=c('Age', 'Pclass', 'Sex'))
sub_titanic <- na.omit(sub_titanic)
agg <- aggregate(
  sub_titanic, 
  by=list(sub_titanic$Sex, sub_titanic$Pclass), FUN=mean)


agg_male = subset(agg, Group.1 == 'male', select=c('Age', 'Pclass'))
agg_male
agg_female = subset(agg, Group.1 == 'female', select=c('Age', 'Pclass'))
agg_female

barplot(Age ~ Pclass, agg_female, col="blue",  ylim=c(15,45))
barplot(Age ~ Pclass, agg_male, col="red")
legend(2, 40, legend=c("Male", "Female"),col=c("red", "blue"), lty=c(1,1))

# 9. В каком порту на борт село больше всего женщин по отношению к общему числу пассажиров севших на борт в этом порту?

p_cherbourg = subset(Titanic, Embarked == "C", names(Titanic))
P_southampton = subset(Titanic, Embarked == "S", names(Titanic))
p_queenstown = subset(Titanic, Embarked == "Q", names(Titanic))

p_cherbourg$Sex = "Female"
p_queenstown$Sex = "Female"
#P_southampton$Sex = "Female"
#pp_southhampton <- P_southampton[P_southampton$Sex == "Female",]
barplot(P_southampton$Sex)

nrow(p_cherbourg) / nrow(titanic_female)
nrow(P_southampton) / nrow(titanic_female)
nrow(p_queenstown) / nrow(titanic_female)
# unique(P_southampton, incomparables = FALSE)


unique(P_southampton, incomparables = FALSE)

# 10. Правда ли что чем больше у пассажира родных на борту тем больше вероятность выжить? Проверьте эту теорию с обоими
# параметрами Parch и SibSp 

ggplot(Titanic, aes(x = SibSp, y = Parch, 
      color = factor(Survived, labels = c("died", "alive")))) + 
      geom_point(shape = 1, size = 2, 
      position=position_jitter(width=0.3,height=.2)) +
      guides(color=guide_legend(title=NULL)) + 
  xlab("Братья, сёстры, супруги") + 
  ylab("Родители, дети")

