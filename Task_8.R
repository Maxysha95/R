#rm(list=ls())
setwd("~/Downloads")
my_data <- load('CHIS2009_reduced_2.Rdata')
library(dplyr)
library(ggplot2)

# 1.Exploring age(SRAGE_P) and BMI(BMI_P), BMI groups(RBMI)

str(adult)
dim(adult)
class(adult)

class(c(adult$SRAGE_P, adult$BMI_P, adult$RBMI))
summary(adult$SRAGE_P)
summary(adult$BMI_P)
summary(adult$RBMI)

sum(is.na(adult))

# 2. Build a histogram of ages colored by BMI groups

SRAGE_P <-  unique(adult$SRAGE_P)

m <- ggplot(adult, aes(x = SRAGE_P, fill = as.factor(RBMI)))
m + geom_histogram(bins = length(SRAGE_P))

# 3. Clean data

#age under the value
clean_data <- filter(adult, SRAGE_P < 85)

#Keep BMI between 16 (incl) and 52 (exc)

clean_data <- filter(clean_data, BMI_P >= 16 & BMI_P < 52)

#Relabel race(RACEHPR2) with c("Latino", "Asian", "African American", "White")

clean_data$RACEHPR2 <-  factor(clean_data$RACEHPR2, 
                           labels = c("Latino", "Asian", "African American", "White"))

#Relabel BMI groups to c('Under', 'Normal', 'Over', 'Obese')
clean_data$RBMI <-  factor(clean_data$RBMI, 
                           labels = c('Under', 'Normal', 'Over', 'Obese'))

# 4. Build a histogram of ages colored by BMI groups
# add facet by BMI
# color with another palette: scale_fill_brewer(“BMI group", palette = "Reds")

m <- ggplot(clean_data, aes(x = SRAGE_P, fill = RBMI)) +
  geom_histogram(binwidth = 1) +
  facet_grid(RBMI ~.)+
  scale_fill_brewer( "BMI group", palette = "Reds")+
  theme_classic() +
  theme(strip.text.y = element_blank())
m
  

# 5. like 4 but remove facet and create proportional historgam

m <- ggplot(clean_data, aes(x = SRAGE_P, fill = RBMI))+
  geom_histogram(binwidth = 1, position = 'fill') +
  theme_classic() +
  theme(strip.text.y = element_blank()) +
  scale_fill_brewer(palette = "Reds")
m


# 6. Try to combine 4 and 5

m <- ggplot(clean_data, aes(SRAGE_P, fill = RBMI)) + 
  geom_histogram(binwidth = 1, position = 'fill') + 
  facet_grid(RBMI ~ .) +
  theme_classic() + 
  theme(strip.text.y = element_blank()) + 
  scale_fill_brewer(palette="Reds")
m

# 7. Work on your data
# Try to create freq table thus columns should be ages,
# rows should contain proportions of each BMI group relative to each age

x <- table(clean_data$SRAGE_P, clean_data$RBMI)
prop_table <- prop.table(x)

# 8. Tranform this freq table:using reshape2::melt() 
#library(reshape2)

melted_data <- reshape2::melt(prop_table) 

# 9. do 4 with this new data using geom_col()

m <- ggplot(melted_data, aes(x = Var1, y = value, fill = Var2)) +
  geom_col() +
  facet_grid(Var2~.)+
  scale_fill_brewer( "BMI group", palette = "Reds")+
  theme_classic() +
  theme(strip.text.y = element_blank())
m

# or

m <- ggplot(melted_data, aes(x=Var1, y=value, fill=Var2)) +
  geom_col() +
  facet_wrap(. ~ Var2) +
  scale_fill_brewer('BMI group', palette = 'Reds') +
  theme_classic() +
  theme(strip.text.y = element_blank())
m