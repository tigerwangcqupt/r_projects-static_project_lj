#' Quantitative thinking and business statistics
#' For: MEM, Whu
#' updated: Oct. 10, 2024
#' updated: Sep. 20, 2023
#' updated: Oct. 10, 2022
#' updated: Oct. 30, 2021
#' updated: Oct. 22, 2021
#' updated: Oct. 15, 2021
#' Created: Sep 28, 2021
#' Wenbo Chen: cwb@whu.edu.cn


#0. Helpful RStudio commands ------------------------------------------------

# Description       Windows & Linux       Mac
#
# Run current line  Ctrl+Enter            Command+Enter
# Previous plot 	  Ctrl+Alt+F11 	        Command+Option+F11
#                   Ctrl+Shift+PageUp
# Next plot 	      Ctrl+Alt+F12 	        Command+Option+F12
#                   Ctrl+Shift+PageDown


#0.  load libraries and data image----------------------------------------

library(tidyverse)
library(skimr)
library(rstatix)
library(wordcloud2)
library(dplyr)

#加载数据源
getwd()
load("../data/lj_sh_2019.RData")

#load("/Users/jameschen/Documents/02_Teaching/12_quantitative_thinking_R/data/lj_sh_2019.RData")

theme_set(theme(text=element_text(family="Songti SC",size=12,face = "bold")))

# let's review what we have:

str(lj)
names(lj)
summary(lj)

lj <- lj %>%
  mutate(property_name = as.factor(property_name))

mean(lj$bedrooms)
median(lj$bedrooms)

#1. EDA in numeric ways -----------------------------------------------------

## one-variable: categorical

### location

#### mode

names(which.max(table(lj$line)))

which.max(table(lj$station))

#wordcloud2(freq(segment(as.character(lj$property_name),worker())))

#### mean
mean(lj$building_area)

#### median
median(lj$building_area)

## skewness

#e1071::skewness(lj$building_area) ### skewed to the right

#### quantile： quantile and penrcentile
quantile(lj$price_sqm)

quantile(lj$price_sqm,c(0.5,0.75,0.77))
quantile(lj$price_sqm,seq(0,1, by = 0.01)) # percentile

### dispersion

range(lj$price_sqm)

IQR(lj$price_sqm)
quantile(lj$price_sqm,0.75)-quantile(lj$price_sqm,0.25)

var(lj$price_sqm)

sd(lj$price_sqm)

mad(lj$price_sqm)

### distribution shape

e1071::skewness(lj$price_sqm)
e1071::kurtosis(lj$price_sqm)
table(as.factor(lj$bedrooms))


#2 EDA in data_viz ---------------------------------------------------------
## one variable - categorical -------------------------------------------------

# Bar Plots help us visualize how discrete values are distributed. Map x to a
# categorical or discrete value.

# attention about the as.factor fun!!! try without it!

lj %>%
  ggplot(aes((bedrooms))) +
  geom_bar()
scale_x_discrete(limits=as.character(1:9))

lj %>%
  ggplot(aes(as.factor(bedrooms))) +
  geom_col() +
  scale_x_discrete(limits=c("9","6"))

# Notice the geom_bar() function did the counting for us. If we already had a
# data frame with counts, we could use geom_col() with a y aesthetic. or use the
# stat = "identity" parameter.

# geom_bar()
# geom_col()

temp <- lj %>%
  group_by(bedrooms) %>%
  count()
temp

theme_set(theme(text = element_text(family = "Songti SC")))

lj %>%
  ggplot(aes(price_sqm,fill = building_style,alpha = 0.1)) +
  geom_histogram(position = "identity")

lj %>%
  ggplot(aes(price_sqm,color = building_style)) +
  geom_boxplot()


geom_vline(xintercept = median(lj$price_sqm),color = "red", size = 1) +
  geom_vline(xintercept = quantile(lj$price_sqm,c(0.25,0.75)), color = "blue") +
  geom_vline(xintercept = range(lj$price_sqm),color = "red") +
  geom_vline(xintercept = quantile(lj$price_sqm,0.75) + 1.5*IQR(lj$price_sqm),color="red")


temp%>%
  ggplot(aes(bedrooms)) +
  geom_col(aes(y=n)) +
  scale_x_discrete(limits=as.character(1:9))

# A nice benefit of a data frame with counts is that we can sort the categories
# on the x-axis using reorder()

ggplot(temp, aes(x = reorder(bedrooms, n), y = n)) + geom_col() # try -n

# The x-axis now has a funny label. We can fix with labs()

ggplot(temp, aes(x = reorder(bedrooms, n), y = n)) + geom_col() +
  scale_x_discrete("Bedrooms")


# back to the lj data frame...

# the coord_flip() function allows us to flip the coordinate axis;
ggplot(lj, aes(x=bedrooms)) +
  geom_bar() +
  coord_flip()

ggplot(lj, aes(x=as.factor(bedrooms),color=as.factor(bedrooms))) +
  geom_bar(position = "dodge")
coord_polar(clip = "off")
# Let's map the has_elevator indicator to fill to get counts of homes with and without
# elevator by bedrooms. Notice we had to wrap has_elevator in factor()
ggplot(lj, aes(x=factor(bedrooms), fill = factor(has_elevator))) + geom_bar()

# Note the "stack" position. We can change that by setting position = "dodge"
# in geom_bar():

ggplot(lj, aes(x=bedrooms, fill = factor(has_elevator))) + geom_bar(position='dodge')

# Setting position = "fill" shows relative proportions at each category
ggplot(lj, aes(x=bedrooms, fill = factor(has_elevator))) + geom_bar(position='fill') +

  geom_hline(yintercept = 0.5,linetype="dashed",color="black") +
  scale_x_discrete(limits=as.character(1:9))# add a horizontal line

# what are those proportions exactly?
table(lj$has_elevator, lj$bedrooms) %>%
  prop.table(margin = 1) %>%           # margin = 2 means "column proportions"
  round(2)

# What if we wanted the plot displayed in order of proportion? It's tricky.
# Here's how I did it but there may be a better way.

# Get character vector of bedrooms levels in ascending order of proportion for
# has_elevator==1
nl <- table(lj$has_elevator, lj$bedrooms, dnn = c("has_elevator","bedrooms")) %>%
  prop.table(margin = 2) %>%
  round(2) %>%
  as.data.frame(responseName = "prop") %>%
  arrange(has_elevator, prop) %>%
  filter(has_elevator == 1) %>%
  pull(bedrooms) %>%    # pull out the bedrooms vector
  as.character()         # convert to character

# Then use forcats:fct_relevel in ggplot to relevel condition by the nl vector
ggplot(lj, aes(x=fct_relevel(factor(bedrooms), nl), fill=factor(has_elevator))) +
  geom_bar(position = "fill")


# How can we fix the legend title?
# Recall that is a by-product of the fill scale. So we need to use a scale
# function.

ggplot(lj, aes(x=fct_relevel(factor(bedrooms), nl), fill=factor(has_elevator))) +
  geom_bar(position = "fill") +
  scale_fill_discrete(name="Has Elevator",
                      labels=c("0","1"))

# you turn #1:
# #1 create a barplot of decoration

# #2 Create a stacked proportional bar chart (ie, set position = "fill") for
# decoration using has_elevator as the fill variable.



## one-variable: numeric ------------------------------------------------

(p <- lj %>%
   ggplot() +
   geom_histogram(aes(price_sqm)))

# add median

p +geom_vline(aes(xintercept = median(price_sqm)),color='red',linetype="dashed")
geom_hline(aes(yintercept=600),color="blue")

# add median label

(p +
    geom_vline(aes(xintercept = median(price_sqm)),color='red',linetype="dashed") +
    annotate("text",x=median(lj$price_sqm),y=1200,label=median(lj$price_sqm),color='blue'))

# the number of bins

lj %>%
  ggplot() +
  geom_histogram(aes(price_sqm),binwidth = 100)

# A frequency polygon is like a histogram, but counts are presented with lines
# instead of bars.

(p1 <- lj %>%
    ggplot() +
    geom_freqpoly(aes(price_sqm),bins=100)
)

# you can add median and annotation too
(p1 +
    geom_vline(aes(xintercept = median(price_sqm)),color='red',linetype="dashed") +
    annotate("text",x=median(lj$price_sqm),y=500,label=median(lj$price_sqm),color='blue'))

# we can map has_elevator
(p1 <- lj %>%
    ggplot() +
    geom_freqpoly(aes(price_sqm,color=factor(has_elevator))) # why does it not like the intended?
)

(p1 <- lj %>%
    ggplot() +
    geom_freqpoly(aes(price_sqm,color=as.factor(has_elevator)))
)

# you turn #2.1: can you add  vertical line annotation to the freqpoly?



# your turn #2.2: mapping building style? or decoration



# mapping hml
lj %>%
  filter(hml %in% c("高","中","低")) %>%
  ggplot() +
  geom_histogram(aes(price_sqm,fill=hml),position = "fill") +
  theme(text=element_text(family="Songti SC",size=12,face = "bold"))

# it seems price has no relationship with hml, how about interact with has_elevator?

lj %>%
  filter(hml %in% c("高","中","低")) %>%
  ggplot() +
  geom_histogram(aes(price_sqm,fill=hml),position = "fill") +
  facet_wrap(~as.factor(has_elevator)) +
  theme(text=element_text(family="Songti SC",size=12,face = "bold"))

# * building style


### histogram ---------------------------------------------------------------
lj %>%
  ggplot() +
  geom_histogram(aes(price_sqm))

# To create a "true" histogram with density instead of count, (ie, the area
# under the histogram is 1), set y = stat(density) in the aes() function. By
# default, y = stat(count)
lj %>%
  ggplot() +
  geom_boxplot(aes(price_sqm))

lj %>%
  ggplot() +
  geom_histogram(aes(price_sqm,y=stat(density)))

lj %>%
  ggplot() +
  geom_freqpoly(aes(price_sqm,y=stat(density)))

# The reason we might want a "True" histogram is to compare distributions with
# different counts.

lj %>%
  ggplot() +
  geom_freqpoly(aes(price_sqm,y=stat(density),color=as.factor(has_elevator)))


lj %>%
  filter(hml %in% c("高","中","低")) %>%
  ggplot() +
  geom_freqpoly(aes(price_sqm,y=stat(density),color=hml)) +
  facet_wrap(~as.factor(has_elevator)) +
  theme(text=element_text(family="Songti SC",size=12,face = "bold"))




## two variables -----------------------------------------------------------

### two categorical variable ------------------------------------------------


###### table

table(lj$hml,lj$has_elevator)

###### stacked bar

lj %>%
  ggplot(aes(hml,fill=as.factor(has_elevator))) +
  geom_bar(position="fill") # try identity,stack,dodge

#Your turn, Exercise: use line, bedrooms, livingrooms, decoration, building_style to practice stacked bar.

#
#
# lj %>%
#   ggplot(aes(hml,fill=as.factor(line))) +
#   geom_bar(position="fill")

# or you can use facet bar

lj %>%
  ggplot(aes(hml)) +
  geom_bar() +
  facet_wrap(~ as.factor(has_elevator))


## categorical and numerical variables


### Two numeric variables ---------------------------------------------------

# The scatterplot allows you to visualize the relationship between two
# numeric variables.

# plot building_area vs ttl:
ggplot(lj, aes(x=building_area, y=price_ttl)) +
  geom_point()

# Lots of overplotting!
# What can we do about that? One solution is the alpha aesthetic. "alpha=1/10"
# means 10 points overplotted adds to a solid color

ggplot(lj, aes(x=building_area, y=price_ttl)) +
  geom_point(alpha=0.1)

# Can also try smaller points using the shape aesthetic
temp <- lj %>%
  filter(building_area > 400)


(p <- ggplot() +
    geom_point(data=lj, aes(x=building_area, y=price_ttl),shape=".") +
    geom_point(data=temp,aes(x=building_area, y=price_ttl),
               color='red',shape=24))

plotly::ggplotly(p,dynamicTicks=TRUE)
# open circles ala Base R
ggplot(lj, aes(x=building_area, y=price_ttl)) +
  geom_point(shape=1)

# see ?points for shape codes

# geom_density2d() can also be helpful as can zooming in on data
ggplot(lj, aes(x=building_area, y=price_ttl)) +
  geom_point(shape = 1,alpha = 0.1) +
  #       geom_smooth(method = "lm") +
  # scale_x_continuous() +
  # facet_wrap( ~line)
  geom_density2d() +
  coord_cartesian(xlim = c(0, 200), ylim = c(0, 2500))

# Another approach is to use facets: break the data into subsets
ggplot(lj, aes(x=building_area, y=price_ttl)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm") +
  facet_wrap(~ line) +
  theme(text=element_text(family="Songti SC",size=10,face = "bold"))

# We can also zoom in on plot
ggplot(lj, aes(x=building_area, y=price_ttl)) +
  geom_point(shape = 1) +
  facet_wrap(~ line) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 1000)) +
  theme(text=element_text(family="Songti SC",size=10,face = "bold"))

# We can also map the color of points to a variable in our data.
ggplot(lj, aes(x=building_area, y=price_ttl,color=factor(has_elevator))) +
  geom_point(shape = 1) +
  geom_smooth(method="lm") +
  facet_wrap(~ line) +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 1000)) +
  theme(text=element_text(family="Songti SC",size=10,face = "bold"))

# YOUR TURN #3 ------------------------------------------------------------

# (1) Plot age(2024-building_year) vs. building_area with geom_point(). Put building_area on the y axis.
lj %>%
  mutate(building_age=(2024-building_year)) %>%
  ggplot() +
  geom_point(aes(building_age,building_area))


# (2) Repeat 1 but now also zoom in on the x and y axis. Look at the last 10
# years for houses with less than 199 square meters. Also, set shape = 1.
lj %>%
  mutate(building_age=(2024-building_year)) %>%
  ggplot() +
  #geom_point(aes(building_age,building_area,color=factor(has_elevator), alpha=0.1)) +
  geom_jitter(aes(building_age,building_area,color=factor(has_elevator)
  )) +
  scale_x_continuous(limits = c(0,10)) +
  scale_y_continuous(limits = c(0,199))


# (3) Repeat 2 but now also map color of points to has_elevator


# Scales and smooths ------------------------------------------------------

# Let's look again at building_area Vs. total value
library(scales)
ggplot(lj, aes(x=building_area, y=price_ttl,color=factor(has_elevator))) +
  geom_point(alpha=0.6) +
  facet_wrap(~ line) +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(labels=comma) +
  theme(text=element_text(family="Songti SC",size=10,face = "bold"))

# there are extensions to support Renmingbi Yuan, https://gist.github.com/annoporci/542fd18fc0551f0706da

p <- ggplot(lj, aes(x=building_area, y=price_ttl,color=factor(has_elevator))) +
  geom_point(alpha=0.1) +
  facet_wrap(~ line) +
  scale_y_continuous(labels=dollar) +
  scale_x_continuous(labels=comma) +
  theme(text=element_text(family="Songti SC",size=10,face = "bold"))

# We can add a smooth line through our scatterplots with geom_smooth()

p +
  geom_smooth()
p +
  geom_smooth(method = 'lm')
p +
  geom_smooth(method = 'lm',se=FALSE)

# YOUR TURN #4 ------------------------------------------------------------

# Plot price_ttl vs price_sqm, with price_ttl on y axis.
# - add a smooth
# - zoom in on plot: x-axis (40000, 60000) y-axis (0, 1000)
# - Fix the y-axis to show the amount in dollars.


### Boxplots and stripcharts ------------------------------------------------


# Boxplots are good for visualizing a numeric variable conditional on a
# categorical variable. Let's look at price_ttl by number of bedrooms:

ggplot(lj, aes(x=factor(bedrooms), y=price_ttl)) + geom_boxplot()

ggplot(lj, aes(x=factor(bedrooms), y=price_ttl)) +
  geom_jitter(width = 0.2, height = 0, alpha = 1/5) +
  scale_y_continuous(labels=dollar)

# zoom in
ggplot(lj, aes(x=factor(bedrooms), y=price_ttl)) +
  geom_jitter(width = 0.2, height = 0, alpha = 1/5) +
  scale_y_continuous(labels=dollar) +
  coord_cartesian(ylim = c(0,5000), xlim = c(1,2))

# investigate the bedroom=5 and price_ttl less than 600
lj %>%
  filter(bedrooms == 5 & price_ttl <= 600) %>%
  View()

# YOUR TURN #5 ------------------------------------------------------------


# (1) Make a boxplot price_ttl by line.



# (2) Make a stripchart of price_ttl by line.
# - in geom_jitter() set width = 0.4 and alpha = 1/5
# - format y axis as dollar and set limits to 0 - $10,000


### Plotting two discrete integer variables -------------------------------------------------------------------------

# Bedrooms vs.livingrooms

ggplot(lj, aes(x=bedrooms,y=livingrooms)) + geom_point()

# geom_jitter() can help with this

ggplot(lj, aes(x=bedrooms,y=livingrooms)) +geom_jitter()

ggplot(lj, aes(x=bedrooms,y=livingrooms)) +geom_jitter(alpha=0.1)

# scales could be better

ggplot(lj, aes(x=bedrooms,y=livingrooms)) +geom_jitter(alpha=0.1) +
  scale_x_continuous(breaks=1:9) +
  scale_y_continuous(breaks=0:7, minor_breaks=NULL)


# Line Graphs -------------------------------------------------------------


# line graphs are nice for connecting dots and showing a trend over time.

# plot number of houses built per year;
# need to count up number of homes by building_year.

years <- lj %>%
  group_by(building_year) %>%
  count()
years

# now use geom_line()
(p<- ggplot(years, aes(x=building_year, y=n)) +
    geom_line() +
    scale_x_continuous(breaks = seq(1911,2019,1)) +
    theme(axis.text.x = element_text(angle = 45,size = 6)))

plotly::ggplotly(p,dynamicTicks=TRUE)

# the oldest: Xinhai Revolution
# 1936,1958,1964
# ten years cultural revolution
# open and reform policy
# the booming from Deng's travelling to SHENZHEN

# Let's change the x-axis to show years in increments of 50 years

(p<- ggplot(years, aes(x=building_year, y=n)) +
    geom_line() +
    scale_x_continuous(breaks=seq(1910,2019,5)) )


(p<- ggplot(years, aes(x=building_year, y=n)) +
    geom_line() +
    scale_x_continuous(breaks=seq(1910,2019,1)) )



#3 Sampling distribution,  normal distribution and other import --------

## sample and sample distribution
library(tidyverse)

s100 <- slice_sample(lj, n=100)

s100 %>%
  ggplot(aes(price_ttl)) +
  geom_histogram()

# word cloud
library(jiebaR)
library(wordcloud2)

#wh <- read_csv("/Users/jameschen/Documents/02_Teaching/12_quantitative_thinking_R/data/wh_2ys.csv")

wh <- read_csv("../data/wh_2ys.csv")


wordcloud2(freq(segment(wh$property_name,worker())))


# lolition pop

lj %>% #pipe
  count(decoration) %>%
  ggplot(aes(n,decoration)) +
  geom_segment(aes(x = 0,
                   xend = n,
                   y = decoration,
                   yend = decoration)) +
  geom_point(shape=21,size=3,colour="black",fill="#FC4E07")

# 桑基图

library(ggalluvial) # alluvial
library(ggplot2)
data(vaccinations)
levels(vaccinations$response) <- rev(levels(vaccinations$response))
ggplot(vaccinations,
       aes(x = survey,
           stratum = response,
           alluvium = subject,
           weight = freq,
           fill = response,
           label = response)) +
  geom_flow(alpha = 0.7,
            color = "darkgray") +
  geom_stratum(alpha = 1) +
  geom_text(stat = "stratum",
            size = 3.5) +
  theme_classic()+ #coord_flip() +
  theme(legend.position = "none",
        axis.text.x =element_text(color="black",
                                  size=12),
        axis.title.x = element_blank(),
        axis.text.y =element_blank(),
        axis.line = element_blank(),
        axis.ticks =element_blank() ) +
  ggtitle("Vaccination Survey responses at three points in time")





# sampling ----------------------------------------------------------------
set.seed(1234)
s1 <- slice_sample(lj,n = 100)
mean(s1$price_sqm)
median(s1$price_sqm)
sd(s1$price_sqm)

s_strata <- lj %>%
  group_by(line) %>%
  slice_sample(prop = 0.1) %>%
  ungroup()

round(table(s_strata$line)/nrow(s_strata),2)
round(table(lj$line)/nrow(lj),2)

library(sampling)

s100 <- function(lj,n = 100)
{
  sample100 <- slice_sample(lj, n = n)
  sample100_mean <- mean(sample100$price_ttl)
  return(sample100_mean)
}

mean1000 <- replicate(1000,s100(lj,300))

hist(mean1000)
abline(v=mean(mean1000),col="red",lwd =3, lty=2)
abline(v= mean(lj$price_ttl),col="blue",lwd = 2,lty =1)

## use ggplot

temp <- tibble(
  mean1000
)

mean1 = mean(mean1000)
sd1 = sd(mean1000)
ggplot(temp,aes(mean1000)) +
  geom_histogram(binwidth = 20) +
  stat_function(fun = ~dnorm(.x,mean1,sd1) * 20 * 1000,color = "red")

shapiro_test(mean1000)

### you can use library "sampling" to do strata sampling, cluster sampling, systematic sampling.
library(sampling)

## strata sampling

### CLT ---------------------------------------------------------------------

lj

s1<-slice_sample(lj,n=100)

mean100 <- replicate(100,mean(s1$price_ttl))

mean(lj$price_ttl)




## CLT
### simulation of clt with R

set.seed(12)
x <- vector()
y <- rchisq(10000,1)

s_step <- seq(5,50, by=5)
par(mfrow=c(5,2))

# for (i in 1:100) {
#   sample_y <- sample(y,30)
#   x[i] <- mean(sample_y)
# }

par(mar=c(0.1,0.1,0.1,0.1))
for (s_size in s_step) {

  for (i in 1:100) {
    sample_y <- sample(y,s_size)
    x[i] <- mean(sample_y)

  }
  hist(x,main = paste0("n = ", s_size),freq = FALSE)
  a <- seq(-100,100,by=1)
  b <- dnorm(a,mean = mean(x),sd = sd(x))
  #curve(a,b,col="blue")
}

par(mfrow= c(1,1)) # restore the parameter
### simulation of clt with R--------------------end

###

### using lj data to demonstrate CLT

### take lj data as a population data, the parameters about price_ttl are:

mean(lj$price_ttl)
var(lj$price_ttl)/50

sample_size = 50

x <- vector()
for (i in 1:1000) {
  sample30_lj <- slice_sample(lj,n=sample_size)

  x[i] = mean(sample30_lj$price_ttl)

}

mean(x)
var(x)

hist(x)
plot(density(x))

### Normal distribution --------------------------------------------------
set.seed(1)
rnorm(1000)
dnorm(0)
pnorm(0)
qnorm(0.1)

ggplot(tibble(x=rnorm(1000)),aes(x)) +
  geom_histogram(binwidth = 0.2) +
  stat_function(fun = ~dnorm(.x) * 0.2 *1000, color = "red")

# 68.26%, 95.44%, 99.7%

1-2*pnorm(-1)

1-2*(pnorm(1,lower.tail = FALSE))

pnorm(-6)*2
pnorm(-4.5) +(1-pnorm(7.5))

rnorm(1000)
rchisq(100,2)
rt(100,12)
dnorm(0)
qnorm(0.995)
pnorm(1.96)

###
1-2*pnorm(-3)

1-2*pnorm(-2)
1-2*pnorm(-1)

1-pnorm(1.25)
pnorm(1)

pnorm(1.25) - pnorm(-0.5)

qnorm(0.9)

dnorm(0)

ggplot(tibble(x=c(-4,4)),aes(x)) +
  stat_function(fun = dnorm,color="blue") +
  stat_function(fun = dnorm,  args = list(mean = 1,sd = 1),color="red") +
  stat_function(fun= dnorm,  args = list(mean = 0, sd =2), color="green")



### chi-square distribution ----------------------------------------------


## Definition
## problem
### Find the 95th percentile of the chi-squared distribution with 7 degrees of freedom


rchisq(100,5)


ggplot(tibble(x=rf(100,5,5)),aes(x)) +
  geom_histogram(binwidth = 0.25) +
  stat_function(fun = ~df(.x,df1=5,df2=5) * 0.25*100,color = "red") +
  stat_function(fun = ~df(.x,df1=10,df2=10) * 0.25*100,color = "blue") +
  stat_function(fun = ~df(.x,df1=15,df2=15) * 0.25*100,color = "yellow") +
  stat_function(fun = ~df(.x,df1=40,df2=40) * 0.25*100,color = "black") +
  scale_x_continuous(limits = c(0,10))


pt(2,100000)
pnorm(2)

qchisq(.95,df=7)

dchisq(12,20)
pchisq(12,20)
qchisq(0.9,20)
rchisq(10,20)

x<- seq(0.5,100,by = 0.1)
y <- dchisq(x,5)
y2 <- dchisq(x,10)
y3 <- dchisq(x,20)
y4 <- dchisq(x,40)
plot(x,y,col="red",lwd=1,type = "l")
lines (x,y2,col="blue")
lines(x,y3,col="steel blue")
lines(x,y4,col="black")


### t distribution -------------------------------------------------------


### Definition
### Problem
### Find the 2.5th and 97.5th percentiles of the Student t distribution with 5 degrees of freedom.

qt(c(0.025,0.975), df=5)

dt(0,30)
pt(0,30)
qt(0.975,40)
rt(300,29)

x <- seq(-4,4, by=0.01)
y <- dnorm(x)
y2 <- dt(x,5)
y3 <- dt(x,10)
y4 <- dt(x,15)
y5 <- dt(x,30)
plot(x,y,col="red",lwd=1,type = "l")
lines (x,y2,col="blue")
lines(x,y3,col="steel blue")
lines(x,y4,col="yellow")
lines(x,y5,col="black")


## F distribution
### Definition
### Problem
### Find the 95th percentile of the F distribution with (5, 2) degrees of freedom.

### Solution
qf(.95,df1=5,df2=2)
df(12,3,15)
pf(15,1,3)
qf(0.95,1,3)
rf(12,1,3)

#4 interval estimation -----------------------------------------------------
library(MASS)

qnorm(0.95)
qnorm(0.975)
qnorm(0.995)

plot(density(rnorm(10000)),col="red")
lines(density(rt(10000,20)),col = "gray")


ggplot(tibble(x=c(-4,4)),aes(x)) +
  stat_function(fun = dchisq,args=list(df=4))
stat_function(fun = )


## Problem
## Find the point estimate of mean square meter price in SH.

mean(lj$price_sqm)


## Interval estimation of population mean with know variance

## Problem
## Assume the population sd of the price_sqm is 25000.
## Find the margin of error and interval estimate at 95% confidence level.

lj

set.seed(10)
# d <- tibble(
#   smean = vector(),
#   llt = vector(),
#   ult = vector()
# )

#for(i in 1:100) {
s100 <- slice_sample(lj,n=100)
smean <- mean(s100$price_ttl)
ssd <- sd(s100$price_ttl)
#psd <- sd(lj$price_ttl)

ci <- smean+qnorm(c(0.025,0.975))*psd/sqrt(100)

ci_t <- smean+qt(c(0.025,0.975),99)*ssd/sqrt(100)
ci_t

t.test(s100$price_ttl)

qt(c(0.025,0.975),99)

mean(lj$price_ttl)

ci
n <- length(lj$price_sqm)
si <- sd(lj$price_sqm)
sem <- si/sqrt(n)

# normal distribution
E <- qnorm(c(0.025,0.925))*sem
xbar <- mean(lj$price_sqm)

xbar+E


E <- qt(0.975,6526)*sem

xbar <- mean(lj$price_sqm)

xbar +c(-E,E)


### Alternative solution

t.test(lj$price_sqm)

### simulation 100 times of sampling.

s100 <- function(dt,n=100){
  sp <- slice_sample(dt, n = n)
  smean=mean(sp$price_sqm)
  return(smean)
}

mean100 <- replicate(100,s100(lj,n=100))
### use the clt code to demostrate the interval estimation features.



## t distribution

qt(0.975,9)
(1-pt(3.16,9))*2
pt(3.16,9,lower.tail = FALSE) * 2
## Interval estimation of population mean with unknown variance
### problem
### Without assuming the population standard deviation of the price_sqm, find the margin of error and interval estimate at 95% confidence level.
### Solution
s <- sd(lj$price_sqm)
se <- s/sqrt(n)

E <- qt(.975,df=n-1)*se
xbar + c(-E,E)

##### sampling size of population mean
### Problem
### Assume the population standard deviation σ of the lj$price_sqm is 25000. Find the sample size needed to achieve a 10000 margin of error at 95% confidence level.
zstar <- qnorm(.975)
sigma <- 25000
E <- 10000
zstar^2 * sigma^2/E^2

x <- c(85,59,66,81,35,57,55,63,66)

var(x)

chi_9 <- 8*var(x)/100
chi_9

qchisq(0.95,8)
### point estimate of population proportion

### problem
### find a point estimate of buildings with elevator
n <- length(lj$has_elevator)
k <- sum(lj$has_elevator =="1")
pbar <- k/n


### Interval Estimate of Population Proportion
### Problem
### Compute the margin of error and estimate interval for the has_elevator proportion in survey at 95% confidence level.
### YOUR TURN
pbar + c(-qnorm(0.975)*sqrt(pbar*(1-pbar)/n),qnorm(0.975)*sqrt(pbar*(1-pbar)/n))

?prop.test()

### Alternative solution
prop.test(k,n)

### Interval Estimate of Population Proportion
### Problem
### Using a 50% proportion estimate, find the sample size needed to 5% margin of error for the
### approval of "Duo Deduction Policy" at 95% confidence level.
### YOUR TURN


#5 Hypothesis Testing ------------------------------------------------------
# type II error
172

real_miu = 172.5

alpha = 0.05

sigma = 4


allimit <- 172-qnorm(0.975)*sigma/sqrt(100)
aulimit <- 172+qnorm(0.975)*sigma/sqrt(100)

pnorm(aulimit,172.5,4/10) - pnorm(allimit, 172.5,4/10)

true_miu <- seq(170,174,by=0.1)

beta_err <- pnorm(aulimit,true_miu,4/10) - pnorm(allimit, true_miu,4/10)


ggplot(tibble(true_miu,
              beta_err),aes(x= true_miu,y=beta_err)) +
  geom_point() +
  geom_line() +
  geom_line(aes(y=1-beta_err),color = "red")

# draw the curve, beta error, true miu


# Lower Tail Test of Population Mean with Known Variance
## Problem
##' Suppose the manufacturer claims that the mean lifetime of
##' a light bulb is more than 10,000 hours. In a sample of 30 light bulbs,
##' it was found that they only last 9,900 hours on average.
##' Assume the population standard deviation is 120 hours.
##' At .05 significance level, can we reject the claim by the manufacturer?
# Solution
# The null hypothesis is that μ ≥ 10000. We begin with computing the test statistic.
#
xbar <- 9900            # sample mean
mu0 <- 10000            # hypothesized value
sigma <- 120            # population standard deviation
n <- 30                 # sample size
z <- (xbar - mu0)/(sigma/sqrt(n))
z                      # test statistic
# [1] −4.5644
# We then compute the critical value at .05 significance level.
#
alpha <- .05
z.alpha <- qnorm(1 - alpha)
# > −z.alpha               # critical value
# [1] −1.6449
# Answer
# The test statistic -4.5644 is less than the critical value of -1.6449.
# Hence, at .05 significance level,
# we reject the claim that mean lifetime of a light bulb is above 10,000 hours.

# computing the second type error

0.001/(0.025/sqrt(200))


#
# Alternative Solution
# Instead of using the critical value, we apply the pnorm function to compute the lower tail p-value of the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≥ 10000.
#
pval <- pnorm(z)
pval                   # lower tail p−value
# [1] 2.5052e−06

# # Upper Tail Test of Population Mean with Known Variance
# Problem
# Suppose the food label on a cookie bag states that
# there is at most 2 grams of saturated fat in a single cookie.
# In a sample of 35 cookies, it is found that the
# mean amount of saturated fat per cookie is 2.1 grams.
# Assume that the population standard deviation is 0.25 grams.
# At .05 significance level, can we reject the claim on food label?
#
#   Solution
# The null hypothesis is that μ ≤ 2. We begin with computing the test statistic.
#
# > xbar = 2.1             # sample mean
# > mu0 = 2                # hypothesized value
# > sigma = 0.25           # population standard deviation
# > n = 35                 # sample size
# > z = (xbar−mu0)/(sigma/sqrt(n))
# > z                      # test statistic
# [1] 2.3664
# We then compute the critical value at .05 significance level.
#
# > alpha = .05
# > z.alpha = qnorm(1−alpha)
# > z.alpha                # critical value
# [1] 1.6449
# Answer
# The test statistic 2.3664 is greater than the critical value of 1.6449.
# Hence, at .05 significance level,
# we reject the claim that there is at most 2 grams of saturated fat in a cookie.
#
# Alternative Solution
# Instead of using the critical value, we apply the pnorm function to compute the upper tail p-value of the test statistic. As it turns out to be less than the .05 significance level, we reject the null hypothesis that μ ≤ 2.
#
# > pval = pnorm(z, lower.tail=FALSE)
# > pval                   # upper tail p−value
# [1] 0.0089802

# Two-Tailed Test of Population Mean with Known Variance


# Lower Tail Test of Population Mean with Unknown Variance


# Upper Tail Test of Population Mean with Unknown Variance


# Two-Tailed Test of Population Mean with Unknown Variance


# Lower Tail Test of Population Proportion


# Upper Tail Test of Population Proportion


# Two-Tailed Test of Population Proportion


# Type II error
# Type II Error in Lower Tail Test of Population Mean with Known Variance
# Problem
# Suppose the manufacturer claims that the mean lifetime of a light bulb is more than 10,000 hours.
# Assume actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours.
# At .05 significance level, what is the probability of having type II error for a sample size of 30 light bulb?
#
#   Solution
# We begin with computing the standard deviation of the mean, sem.
#
n = 30                # sample size
sigma = 120           # population standard deviation
sem = sigma/sqrt(n); sem   # standard error

# We next compute the lower bound of sample means for which the null hypothesis μ ≥ 10000 would not be rejected.
#
# > alpha = .05           # significance level
# > mu0 = 10000           # hypothetical lower bound
# > q = qnorm(alpha, mean=mu0, sd=sem); q
# [1] 9964
# Therefore, so long as the sample mean is greater than 9964 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 9950, we can compute the probability of the sample mean above 9964, and thus found the probability of type II error.
#
# > mu = 9950             # assumed actual mean
# > pnorm(q, mean=mu, sd=sem, lower.tail=FALSE)
# [1] 0.26196
# Answer
# If the light bulbs sample size is 30, the actual mean light bulb lifetime is 9,950 hours and the population standard deviation is 120 hours, then the probability of type II error for testing the null hypothesis μ ≥ 10000 at .05 significance level is 26.2%, and the power of the hypothesis test is 73.8%.
#
# Exercise
# Under same assumptions as above, if the actual mean light bulb lifetime is 9,965 hours, what is the probability of type II error at .05 significance level? What is the power of the hypothesis test?


# Type II Error in Upper Tail Test of Population Mean with Known Variance
# Problem
# Suppose the food label on a cookie bag states that there is at most 2 grams of saturated fat in a single cookie. Assume the actual mean amount of saturated fat per cookie is 2.09 grams, and the population standard deviation is 0.25 grams. At .05 significance level, what is the probability of having type II error for a sample size of 35 cookies?
#
#   Solution
# We begin with computing the standard deviation of the mean, sem.
#
# > n = 35                # sample size
# > sigma = 0.25          # population standard deviation
# > sem = sigma/sqrt(n); sem   # standard error
# [1] 0.042258
# We next compute the upper bound of sample means for which the null hypothesis μ ≤ 2 would not be rejected.
#
# > alpha = .05           # significance level
# > mu0 = 2               # hypothetical upper bound
# > q = qnorm(alpha, mean=mu0, sd=sem, lower.tail=FALSE); q
# [1] 2.0695
# Therefore, so long as the sample mean is less than 2.0695 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 2.09, we can compute the probability of the sample mean below 2.0695, and thus found the probability of type II error.
#
# > mu = 2.09             # assumed actual mean
# > pnorm(q, mean=mu, sd=sem)
# [1] 0.31386
# Answer
# If the cookies sample size is 35, the actual mean amount of saturated fat per cookie is 2.09 grams and the population standard deviation is 0.25 grams, then the probability of type II error for testing the null hypothesis μ ≤ 2 at .05 significance level is 31.4%, and the power of the hypothesis test is 68.6%.
#
# Exercise
# Under same assumptions as above, if the actual mean amount of saturated fat per cookie is 2.075 grams, what is the probability of type II errors? What is the power of the hypothesis test?

# Type II Error in Two-Tailed Test of Population Mean with Known Variance
# Problem
# Suppose the mean weight of King Penguins found in an Antarctic colony last year was 15.4 kg. Assume the actual mean population weight is 15.1 kg, and the population standard deviation is 2.5 kg. At .05 significance level, what is the probability of having type II error for a sample size of 35 penguins?
#
#   Solution
# We begin with computing the standard deviation of the mean, sem.
#
# > n = 35                # sample size
# > sigma = 2.5           # population standard deviation
# > sem = sigma/sqrt(n); sem   # standard error
# [1] 0.42258
# We next compute the lower and upper bounds of sample means for which the null hypothesis μ = 15.4 would not be rejected.
#
# > alpha = .05           # significance level
# > mu0 = 15.4            # hypothetical mean
# > I = c(alpha/2, 1-alpha/2)
# > q = qnorm(I, mean=mu0, sd=sem); q
# [1] 14.572 16.228
# Therefore, so long as the sample mean is between 14.572 and 16.228 in a hypothesis test, the null hypothesis will not be rejected. Since we assume that the actual population mean is 15.1, we can compute the lower tail probabilities of both end points.
#
# > mu = 15.1             # assumed actual mean
# > p = pnorm(q, mean=mu, sd=sem); p
# [1] 0.10564 0.99621
# Finally, the probability of type II error is the probability between the two end points.
#
# > diff(p)               # p[2]-p[1]
# [1] 0.89056
# Answer
# If the penguin sample size is 35, the actual mean population weight is 15.1 kg and the population standard deviation is 2.5 kg, then the probability of type II error for testing the null hypothesis μ = 15.4 at .05 significance level is 89.1%, and the power of the hypothesis test is 10.9%.
#
# Exercise
# Under same assumptions as above, if actual mean population weight is 14.9 kg, what is the probability of type II errors? What is the power of the hypothesis test?

# Type II Error in Lower Tail Test of Population Mean with Unknown Variance
# Type II Error in Upper Tail Test of Population Mean with Unknown Variance
# Type II Error in Two-Tailed Test of Population Mean with Unknown Variance


#6 Inference about two population ------------------------------------------


# Population Mean Between Two Matched Samples

# Example
#' In the built-in data set named immer,
#' the barley yield in years 1931 and 1932 of the same field are recorded.
#' The yield data are presented in the data frame columns Y1 and Y2.
#
library(MASS)         # load the MASS package
head(immer)

mean_y1 <- mean(immer$Y1)
mean_y2 <- mean(immer$Y2)
var_y1 <- var(immer$Y1)
var_y2 <- var(immer$Y2)

mean_y1 - mean_y2 - qt(0.975,58) *sqrt((var_y1 + var_y2)/30)

mean_y1 - mean_y2 + qt(0.975,58) *sqrt((var_y1 + var_y2)/30)


t.test(immer$Y1,immer$Y2,var.equal = TRUE,paired = TRUE)

dy <- immer$Y2 -immer$Y1

mean_dy = mean(dy)

t.test(dy)


# Loc Var    Y1    Y2
# 1  UF   M  81.0  80.7
# 2  UF   S 105.4  82.3
# .....
# Problem
# Assuming that the data in immer follows the normal distribution, find the 95% confidence interval estimate of the difference between the mean barley yields between years 1931 and 1932.
#
# Solution
# We apply the t.test function to compute the difference in means of the matched samples. As it is a paired test, we set the "paired" argument as TRUE.
#
t.test(immer$Y1, immer$Y2,
       paired=TRUE,
       var.equal = TRUE)

t.test(lj$price_sqm)

t.test()

#
# Paired t-test
#
# data:  immer$Y1 and immer$Y2
# t = 3.324, df = 29, p-value = 0.002413
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   6.122 25.705
# sample estimates:
#   mean of the differences
# 15.913
# Answer
# Between years 1931 and 1932 in the data set immer, the 95% confidence interval of the difference in means of the barley yields is the interval between 6.122 and 25.705.
#

# or if not paired

t.test(immer$Y1,immer$Y2) # try var.equal = TRUE argument, in practice, we always set FALSE is default, seldomly we set it TRUE, then the df = n1 + n2 -2
# then you can caculate the p_value manually, or you can calculate the confidence interval

# Exercise
# Estimate the difference between the means of matched samples using your textbook formula.

# Population Mean Between Two Independent Samples
#
# Two data samples are independent if they come from unrelated populations and the samples does not affect each other. Here, we assume that the data populations follow the normal distribution. Using the unpaired t-test, we can obtain an interval estimate of the difference between two population means.
#
# Example
# In the data frame column mpg of the data set mtcars, there are gas mileage data of various 1974 U.S. automobiles.
#
# > mtcars$mpg
# [1] 21.0 21.0 22.8 21.4 18.7 ...
# Meanwhile, another data column in mtcars, named am, indicates the transmission type of the automobile model (0 = automatic, 1 = manual).
#
# > mtcars$am
# [1] 1 1 1 0 0 0 0 0 ...
# In particular, the gas mileage for manual and automatic transmissions are two independent data populations.
#
# Problem
# Assuming that the data in mtcars follows the normal distribution, find the 95% confidence interval estimate of the difference between the mean gas mileage of manual and automatic transmissions.
#
# Solution
# As mentioned in the tutorial Data Frame Row Slice, the gas mileage for automatic transmission can be listed as follows:
#
#   > L = mtcars$am == 0
# > mpg.auto = mtcars[L,]$mpg
# > mpg.auto                    # automatic transmission mileage
# [1] 21.4 18.7 18.1 14.3 24.4 ...
# By applying the negation of L, we can find the gas mileage for manual transmission.
#
# > mpg.manual = mtcars[!L,]$mpg
# > mpg.manual                  # manual transmission mileage
# [1] 21.0 21.0 22.8 32.4 30.4 ...
# We can now apply the t.test function to compute the difference in means of the two sample data.
#
# > t.test(mpg.auto, mpg.manual)
#
# Welch Two Sample t-test
#
# data:  mpg.auto and mpg.manual
# t = -3.7671, df = 18.332, p-value = 0.001374
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -11.2802  -3.2097
# sample estimates:
#   mean of x mean of y
# 17.147    24.392
# Answer
# In mtcars, the mean mileage of automatic transmission is 17.147 mpg and the manual transmission is 24.392 mpg. The 95% confidence interval of the difference in mean gas mileage is between 3.2097 and 11.2802 mpg.
#
# Alternative Solution
# We can model the response variable mtcars$mpg by the predictor mtcars$am, and then apply the t.test function to estimate the difference of the population means.
#
# > t.test(mpg ~ am, data=mtcars)
#
# Welch Two Sample t-test
#
# data:  mpg by am
# t = -3.7671, df = 18.332, p-value = 0.001374
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -11.2802  -3.2097
# sample estimates:
#   mean in group 0 mean in group 1
# 17.147          24.392
# Note
# Some textbooks truncate down the degree of freedom to an integer, and the result would differ from the t.test.


# Comparison of Two Population Proportions
# A survey conducted in two distinct populations will produce different results. It is often necessary to compare the survey response proportion between the two populations. Here, we assume that the data populations follow the normal distribution.
#
# Example
# In the built-in data set named quine, children from an Australian town is classified by ethnic background, gender, age, learning status and the number of days absent from school.
#
library(MASS)         # load the MASS package
head(quine)

# In effect, the data frame column Eth indicates whether the student is Aboriginal or Not ("A" or "N"), and the column Sex indicates Male or Female ("M" or "F").
#
# In R, we can tally the student ethnicity against the gender with the table function. As the result shows, within the Aboriginal student population, 38 students are female. Whereas within the Non-Aboriginal student population, 42 are female.
#

table(quine$Eth, quine$Sex)

# Problem
# Assuming that the data in quine follows the normal distribution, find the 95% confidence interval estimate of the difference between the female proportion of Aboriginal students and the female proportion of Non-Aboriginal students, each within their own ethnic group.
#
# Solution
# We apply the prop.test function to compute the difference in female proportions. The Yates’s continuity correction is disabled for pedagogical reasons.
#
prop.test(table(quine$Eth, quine$Sex), correct=FALSE)


# Answer
# The 95% confidence interval estimate of the difference between the female proportion of Aboriginal students and the female proportion of Non-Aboriginal students is between -15.6% and 16.7%.
#
# Exercise
# Estimate the difference between two population proportions using your textbook formula.
p1 <- prop.test(table(quine$Eth, quine$Sex), correct=FALSE)$estimate[1]
p2 <- prop.test(table(quine$Eth, quine$Sex), correct=FALSE)$estimate[2]

p1 - p2 + qnorm(0.975) *sqrt(p1*(1-p1)/69 +p2 * (1-p2)/77)
p1 - p2 - qnorm(0.975) *sqrt(p1*(1-p1)/69 +p2 * (1-p2)/77)
# bus time: hypothesis testing on variance --------------------------------

# 武汉公交公司正在推行一项质量改进运动，公司希望减少到站时间的方差，从而塑造公司准点到达的形象。
# 附件数据是公司对某站点24次到站时间的抽查记录。公司希望知道这一站点是否符合公司提出的方差小于4这一标准。

bustime <-read_csv("../data/bustimes.csv",col_names = TRUE,col_select = "Times")

var_s <- var(bustime$Times,na.rm = TRUE)

(test_statistic <- (24-1)*var_s/4)

(critical_value <- qchisq(0.05,23,lower.tail = FALSE))

# alternative solution
# you can use varTest in pkg EnvStats
library(EnvStats)
varTest(bustime$Times,alternative = "greater", sigma.squared = 4)

pchisq(28.175,23,lower.tail = FALSE)
# 磁疗可以缓解疼痛？
# 为了测试磁疗是否可以缓解疼痛，通过对实验组和对照组分别给与磁疗和安慰剂，并询问受试疼痛的缓解程度，得到如下数据：
# 对照组：￼
# 实验组：￼
# 可否认为给予安慰剂的对照组具有更大的方差？

(test_statistic_f <- 1.4^2/0.96^2)

(f_critical <- qf(0.95,19,19))

pf(2.127,19,19,lower.tail = FALSE)

var.test()


d1 <- readxl::read_xlsx("/Users/jameschen/Documents/02_Teaching/12_quantitative_thinking_R/data/WE.xlsx")

feeling_0 <- d1 %>%
  filter(流失 == 0)

feeling_1 <- d1 %>%
  filter(流失 == 1)

var.test(feeling_0$当月客户幸福指数,feeling_1$当月客户幸福指数)

t.test(feeling_0$当月客户幸福指数,feeling_1$当月客户幸福指数)


#7 goodness of fit ---------------------------------------------------------

# Multinomial Goodness of Fit

# Example
# In the built-in data set survey, the Smoke column records the survey response about the student’s smoking habit. As there are exactly four proper response in the survey: "Heavy", "Regul" (regularly), "Occas" (occasionally) and "Never", the Smoke data is multinomial. It can be confirmed with the levels function in R.
#
# library(MASS)       # load the MASS package
levels(survey$Smoke)
# we can find the frequency distribution with the table function.
#
smoke.freq = table(survey$Smoke)

# Problem
# Suppose the campus smoking statistics is as below. Determine whether the sample data in survey supports it at .05 significance level.
#
# Heavy   Never   Occas   Regul
# 4.5%   79.5%    8.5%    7.5%
#   Solution
# We save the campus smoking statistics in a variable named smoke.prob. Then we apply the chisq.test function and perform the Chi-Squared test.
#
smoke.prob = c(.045, .795, .085, .075)
chisq.test(smoke.freq, p=smoke.prob)

# Answer
# As the p-value 0.991 is greater than the .05 significance level, we do not reject the null hypothesis that the sample data in survey supports the campus-wide smoking statistics.
#
# Exercise
# Conduct the Chi-squared goodness of fit test for the smoking data by computing the p-value with the textbook formula.



# Chi-squared Test of Independence
# Example
# In the built-in data set survey, the Smoke column records the students smoking habit, while the Exer column records their exercise level. The allowed values in Smoke are "Heavy", "Regul" (regularly), "Occas" (occasionally) and "Never". As for Exer, they are "Freq" (frequently), "Some" and "None".
#
# We can tally the students smoking habit against the exercise level with the table function in R. The result is called the contingency table of the two variables.
#
# library(MASS)       # load the MASS package
tbl = table(survey$Smoke, survey$Exer)
tbl                 # the contingency table
#

# Problem
# Test the hypothesis whether the students smoking habit is independent of their exercise level at .05 significance level.
#
# Solution
# We apply the chisq.test function to the contingency table tbl, and found the p-value to be 0.4828.
#
chisq.test(tbl)
#
# Pearson’s Chi-squared test
#
# data:  table(survey$Smoke, survey$Exer)
# X-squared = 5.4885, df = 6, p-value = 0.4828
#
# Warning message:
#   In chisq.test(table(survey$Smoke, survey$Exer)) :
#   Chi-squared approximation may be incorrect
# Answer
# As the p-value 0.4828 is greater than the .05 significance level, we do not reject the null hypothesis that the smoking habit is independent of the exercise level of the students.
#
# Enhanced Solution
# The warning message found in the solution above is due to the small cell values in the contingency table. To avoid such warning, we combine the second and third columns of tbl, and save it in a new table named ctbl. Then we apply the chisq.test function against ctbl instead.
#
# > ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"])
# > ctbl
# [,1] [,2]
# Heavy    7    4
# Never   87  102
# Occas   12    7
# Regul    9    8
#
# > chisq.test(ctbl)
#
# Pearson’s Chi-squared test
#
# data:  ctbl
# X-squared = 3.2328, df = 3, p-value = 0.3571
# Exercise
# Conduct the Chi-squared independence test of the smoking and exercise survey by computing the p-value with the textbook formula.

# Titanic case
## context 1: gender and survive

titanic_gender <- matrix(data = c(338,316,1352,109),nrow = 2, byrow = TRUE,
                         dimnames = list(c("survive","die"),c("male","female")))

chisq.test(titanic_gender,correct = FALSE)

(338-552.58)^2/522.58 + (316-131.42)^2/131.42 + (1352- 1167.42)^2/1167.42 + (109-293.58)^2/293.58

## context 2: cabin and survive

titanic_cabin <- matrix(data = c(202,118,178,212,123,167,528,696), nrow =2, byrow = TRUE,
                        dimnames = list(c("survive","die"),c("first","second", "third","crew")))
chisq.test(titanic_cabin,correct = FALSE)


# Gilbert case
a <- matrix(data =c(40,34,217,1350),nrow = 2, dimnames = list(c("working","not_working"),c("death","no_death")))
chisq.test(a)

# goodness of fit
digit_freq <- c(7,14,6,10,8,4,5,6,12,8)


chisq.test(digit_freq,p = rep(0.1,10))


# test the normality
data(immer)
var(immer$Y1)
var(immer$Y2)
var.test(immer$Y1,immer$Y2)
# Y1 and Y2, normally distributed.
# 30, divided into 6 groups.
e <- c(5,5,5,5,5,5)
# observation freq.
(x_bar <- mean(immer$Y1))
(x_sd <- sd(immer$Y1))
# Y1 is ~ N(x_bar, x_sd^2)
(s1 <- qnorm(1/6,x_bar,x_sd))
(s2 <- qnorm(2/6,x_bar,x_sd))
(s3 <- qnorm(3/6,x_bar,x_sd))
(s4 <- qnorm(4/6,x_bar,x_sd))
(s5 <- qnorm(5/6,x_bar,x_sd))
sort(immer$Y1)
o <- c(6,6,5,3,4,6)
(chi <- sum((e-o)^2/e))

qchisq(0.95,5)

ks.test(immer$Y1,"pnorm")
#8 Analysis of Variance-ANOVA ----------------------------------------------

# Is larger cars safer?
car_crash <- tibble(
  small = c(44,43,44,54,38,43,42,45,44,50),
  medium = c(41,49,43,41,47,42,37,43,44,34),
  large = c(32,37,38,45,37,33,38,45,43,42)
)

car_crash <- car_crash %>%
  pivot_longer(everything(), names_to = c("car_type"), values_to = "crash") %>%
  mutate(car_type = factor(car_type))

# the boxplot
car_crash %>%
  ggplot(aes(car_type,crash)) +
  geom_boxplot() +
  xlab("Car type")

# F-test

av = aov(crash ~ car_type, data = car_crash)
summary(av)


# perform Tukey's test
TukeyHSD(av,conf.level = 0.95)
plot(TukeyHSD(av,conf.level = 0.95), las = 1 )



# Perform pairwise t-tests (Bonferroni correction)
pairwise.t.test(car_crash$crash,car_crash$car_type,p.adjust.method = "bonferroni")

# Holm's Method
pairwise.t.test(car_crash$crash,car_crash$car_type,p.adjust.method = "holm")

# Dunnett's Correct
# when we want to compare every group mean to a control mean, and we’re not interested in comparing the treatment means with one another.
library(multcomp)
dunnet_comparison <- glht(av, linfct = mcp(car_type = "Dunnett")) #also you can use  ="Tukey"
summary(dunnet_comparison)

### two-way anova

car_crash <- tibble(
  small = c(44,54,43,43,44,42),
  medium = c(41,49,47,43,37,34),
  large = c(32,45,42,37,38,33),
  region =c("Foreign","Foreign","Foreign","Domestic","Domestic","Domestic")
)

car_crash <- car_crash %>%
  pivot_longer(c("small","medium","large"), names_to = c("car_type"), values_to = "crash") %>%
  mutate(car_type = factor(car_type))

tw_anova <-aov(crash ~ region + car_type,data = car_crash)
summary(tw_anova)

tw_anova <-aov(crash ~ region*car_type,data = car_crash)
summary(tw_anova)

with(car_crash, {
  interaction.plot(region,car_type,crash,type="b",
                   col=c('red',"blue"), pch = c(16,18),
                   main = "Interaction between region and car_type")
})



# Exercise:
# food and taste




# Completely Randomized Design

# In a completely randomized design, there is only one primary factor under consideration in the experiment.
# The test subjects are assigned to treatment levels of the primary factor at random.
#
# Example
# A fast food franchise is test marketing 3 new menu items. To find out if they the same popularity,
# 18 franchisee restaurants are randomly chosen for participation in the study.
# In accordance with the completely randomized design, 6 of the restaurants are randomly chosen to
# test market the first new menu item, another 6 for the second menu item, and the remaining 6 for the last menu item.
#
# Problem
# Suppose the following table represents the sales figures of the 3 new menu items in the 18 restaurants after a week of test marketing.
# At .05 level of significance, test whether the mean sales volume for the 3 new menu items are all equal.
#
# Item1 Item2 Item3
# 22    52    16
# 42    33    24
# 44     8    19
# 52    47    18
# 45    43    34
# 37    32    39
# Solution
# The solution consists of the following steps:
#
#   Copy and paste the sales figure above into a table file named "fastfood-1.txt" with a text editor.
# Load the file into a data frame named df1 with the read.table function. As the first line in the file contains the column names, we set the header argument as TRUE.
# > df1 = read.table("fastfood-1.txt", header=TRUE); df1
# Item1 Item2 Item3
# 1    22    52    16
# 2    42    33    24
# 3    44     8    19
# 4    52    47    18
# 5    45    43    34
# 6    37    32    39
# Concatenate the data rows of df1 into a single vector r .
# > r = c(t(as.matrix(df1))) # response data
# > r
# [1] 22 52 16 42 33 ...
# Assign new variables for the treatment levels and number of observations.
# > f = c("Item1", "Item2", "Item3")   # treatment levels
# > k = 3                    # number of treatment levels
# > n = 6                    # observations per treatment
# Create a vector of treatment factors that corresponds to each element of r in step 3 with the gl function.
# > tm = gl(k, 1, n*k, factor(f))   # matching treatments
# > tm
# [1] Item1 Item2 Item3 Item1 Item2 ...
# Apply the function aov to a formula that describes the response r by the treatment factor tm.
# > av = aov(r ~ tm)
# Print out the ANOVA table with the summary function.
# > summary(av)
# Df Sum Sq Mean Sq F value Pr(>F)
# tm           2    745     373    2.54   0.11
# Residuals   15   2200     147
# Answer
# Since the p-value of 0.11 is greater than the .05 significance level, we do not reject the null hypothesis that the mean sales volume of the new menu items are all equal.
#
# Exercise
# Create the response data in step 3 above along vertical columns instead of horizontal rows. Adjust the factor levels in step 5 accordingly.

# Randomized Block Design
# In a randomized block design, there is only one primary factor under consideration in the experiment.
# Similar test subjects are grouped into blocks. Each block is tested against all treatment levels of the primary factor at random order.
# This is intended to eliminate possible influence by other extraneous factors.
#
# Example
# A fast food franchise is test marketing 3 new menu items. To find out if they have the same popularity,
# 6 franchisee restaurants are randomly chosen for participation in the study.
# In accordance with the randomized block design, each restaurant will be test marketing all 3 new menu items.
# Furthermore, a restaurant will test market only one menu item per week, and it takes 3 weeks to test market all menu items.
# The testing order of the menu items for each restaurant is randomly assigned as well.
#
# Problem
# Suppose each row in the following table represents the sales figures of the 3 new menu in a restaurant after a week of test marketing. At .05 level of significance, test whether the mean sales volume for the 3 new menu items are all equal.
#
# Item1 Item2 Item3
# 31    27    24
# 31    28    31
# 45    29    46
# 21    18    48
# 42    36    46
# 32    17    40
# Solution
# The solution consists of the following steps:
#
#   Copy and paste the sales figure above into a table file named "fastfood-2.txt" with a text editor.
# Load the file into a data frame named df2 with the read.table function. As the first line in the file contains the column names, we set the header argument as TRUE.
# > df2 = read.table("fastfood-2.txt", header=TRUE); df2
# Item1 Item2 Item3
# 1    31    27    24
# 2    31    28    31
# 3    45    29    46
# 4    21    18    48
# 5    42    36    46
# 6    32    17    40
# Concatenate the data rows in df2 into a single vector r .
# > r = c(t(as.matrix(df2))) # response data
# > r
# [1] 31 27 24 31 28 ...
# Assign new variables for the treatment levels and number of control blocks.
# > f = c("Item1", "Item2", "Item3")   # treatment levels
# > k = 3                    # number of treatment levels
# > n = 6                    # number of control blocks
# Create a vector of treatment factors that corresponds to the each element in r of step 3 with the gl function.
# > tm = gl(k, 1, n*k, factor(f))   # matching treatment
# > tm
# [1] Item1 Item2 Item3 Item1 Item2 ...
# Similarly, create a vector of blocking factors for each element in the response data r.
# > blk = gl(n, k, k*n)             # blocking factor
# > blk
# [1] 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6
# Levels: 1 2 3 4 5 6
# Apply the function aov to a formula that describes the response r by both the treatment factor tm and the block control blk.
# > av = aov(r ~ tm + blk)
# Print out the ANOVA table with the summary function.
# > summary(av)
# Df Sum Sq Mean Sq F value Pr(>F)
# tm           2    539     269    4.96  0.032 *
#   blk          5    560     112    2.06  0.155
# Residuals   10    543      54
# Answer
# Since the p-value of 0.032 is less than the .05 significance level, we reject the null hypothesis that the mean sales volume of the new menu items are all equal.
#
# Exercise
# Create the response data in step 3 above along vertical columns instead of horizontal rows. Adjust the factor levels in steps 5 and 6 accordingly.

# Factorial Design

# In a factorial design, there are more than one factors under consideration in the experiment. The test subjects are assigned to treatment levels of every factor combinations at random.
#
# Example
# A fast food franchise is test marketing 3 new menu items in both East and West Coasts of continental United States. To find out if they the same popularity, 12 franchisee restaurants from each Coast are randomly chosen for participation in the study. In accordance with the factorial design, within the 12 restaurants from East Coast, 4 are randomly chosen to test market the first new menu item, another 4 for the second menu item, and the remaining 4 for the last menu item. The 12 restaurants from the West Coast are arranged likewise.
#
# Problem
# Suppose the following tables represent the sales figures of the 3 new menu items after a week of test marketing. Each row in the upper table represents the sales figures of 3 different East Coast restaurants. The lower half represents West Coast restaurants. At .05 level of significance, test whether the mean sales volume for the new menu items are all equal. Decide also whether the mean sales volume of the two coastal regions differs.
#
# East Coast:
#   ==========
#   Item1 Item2 Item3
# E1    25    39    36
# E2    36    42    24
# E3    31    39    28
# E4    26    35    29
#
# West Coast:
#   ==========
#   Item1 Item2 Item3
# W1    51    43    42
# W2    47    39    36
# W3    47    53    32
# W4    52    46    33
# Solution
# The solution consists of the following steps:
#
#   Save the sales figure into a file named "fastfood-3.csv" in CSV format as follows.
# Item1,Item2,Item3
# E1,25,39,36
# E2,36,42,24
# E3,31,39,28
# E4,26,35,29
# W1,51,43,42
# W2,47,39,36
# W3,47,53,32
# W4,52,46,33
# Load the data into a data frame named df3 with the read.csv function.
# > df3 = read.csv("fastfood-3.csv")
# Concatenate the data rows in df3 into a single vector r .
# > r = c(t(as.matrix(df3))) # response data
# > r
# [1] 25 39 36 36 42 ...
# Assign new variables for the treatment levels and number of observations.
# > f1 = c("Item1", "Item2", "Item3") # 1st factor levels
# > f2 = c("East", "West")            # 2nd factor levels
# > k1 = length(f1)          # number of 1st factors
# > k2 = length(f2)          # number of 2nd factors
# > n = 4                    # observations per treatment
# Create a vector that corresponds to the 1th treatment level of the response data r in step 3 element-by-element with the gl function.
# > tm1 = gl(k1, 1, n*k1*k2, factor(f1))
# > tm1
# [1] Item1 Item2 Item3 Item1 Item2 ...
# Similarly, create a vector that corresponds to the 2nd treatment level of the response data r in step 3.
# > tm2 = gl(k2, n*k1, n*k1*k2, factor(f2))
# > tm2
# [1] East East East East East ...
# Apply the function aov to a formula that describes the response r by the two treatment factors tm1 and tm2 with interaction.
# > av = aov(r ~ tm1 * tm2)  # include interaction
# Print out the ANOVA table with summary function.
# > summary(av)
# Df Sum Sq Mean Sq F value  Pr(>F)
# tm1          2    385     193    9.55  0.0015 **
#   tm2          1    715     715   35.48 1.2e-05 ***
#   tm1:tm2      2    234     117    5.81  0.0113 *
#   Residuals   18    363      20
# Answer
# Since the p-value of 0.0015 for the menu items is less than the .05 significance level, we reject the null hypothesis that the mean sales volume of the new menu items are all equal. Moreover, the p-value of 1.2e-05 for the east-west coasts comparison is also less than the .05 significance level. It shows there is a difference in overall sales volume between the coasts. Finally, the last p-value of 0.0113 (< 0.05) indicates that there is a possible interaction between the menu item and coast location factors, i.e., customers from different coastal regions have different tastes.
#
# Exercise
# Create the response data in step 3 above along vertical columns instead of horizontal rows. Adjust the factor levels in steps 5 and 6 accordingly.

# pearson correlation is linear correlation

cor_demo <- read_csv("../data/correlation.csv") %>%
  dplyr::select(x,y)

cor_demo

cor(cor_demo$x,cor_demo$y) # the correlation coef is pretty much low.

cor.test(cor_demo$x,cor_demo$y) # not significant

plot(cor_demo$x,cor_demo$y)

# demonstration of linear

## from Anscombe's Quartet to Cairo's DataSaurus, for further reading.


library("ggplot2")
library("datasauRus")
ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point() +
  geom_smooth(method = lm,se=FALSE) +
  theme_void() +
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)


#9. simple linear regression ------------------------------------------------



slr1 <- tibble(
  x=c(1,2,3,4,5),
  y=c(3,7,5,11,14)
)

#1  scatter plot
plot(slr1$x,slr1$y)

#2.
b1 <- sum((slr1$x-mean(slr1$x))*(slr1$y-mean(slr1$y)))/sum((slr1$x-mean(slr1$x))^2)
b0 <- mean(slr1$y)-b1*mean(slr1$x)

# predict
y <- b0+b1*4

slr1_model <- lm(y ~ x, data = slr1)

summary(slr1_model)
plot(slr1$x,slr1_model$residuals) # too small sample size.

# or
plot(slr1_model)


# Estimated Simple Regression Equation

# Coefficient of Determination

# Significance Test for Linear Regression

# Confidence Interval for Linear Regression

# Prediction Interval for Linear Regression

# Residual Plot

# Standardized Residual

# Normal Probability Plot of Residuals


#10. Multiple Linear Regression ----------------------------------------------

# state.x77 dataset as an example

states <- as.data.frame(state.x77[,c("Murder", "Population", "Illiteracy", "Income", "Frost")])

library(GGally)

ggpairs(states)

# fit the model
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)

summary(fit)

# evaluate the model by the LINE criteria

## Line

## Independent

## Normal Distribution

## Equal criteria



# regression evluation
par(mfrow = c(2,2))

plot(fit)

## using gvlma to evaluate the assumption
library(gvlma)

gvmodel <- gvlma(fit)
summary(gvmodel)



## 多重共线性
library(car)
vif(fit)

## 三类点
library(car)

influencePlot(fit, id.method="identify")

# comparison between models

fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost,
           data=states)

fit2 <- lm(Murder ~ Population + Illiteracy, data=states)

anova(fit2,fit1)

# AIC criteria

AIC(fit1,fit2)

# variable selection


fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data=states)

stepAIC(fit, direction = "backward")

# exercise


# 检验各条线路的均价是否存在显著差别

lj_line_aov <- aov(price_sqm ~ line, data = lj)

summary(lj_line_aov)

# 用除总价以外的其他变量预测平均房价

lj_mlr <- lm(price_sqm ~ line + station + bedrooms + livingrooms +building_area
             + has_elevator + decoration + building_height + building_year +
               building_style, data = lj)
summary(lj_mlr)

plot(lj_mlr)
# Estimated Multiple Regression Equation

# Multiple Coefficient of Determination

# Adjusted Coefficient of Determination

# Significance Test for MLR

# Confidence Interval for MLR

# Prediction Interval for MLR




#11. Logistic Regression -----------------------------------------------------

library(readxl)

d1<- read_excel("/Users/jameschen/Documents/02_Teaching/09_BI/2020/ExcelDemo/Simmons.xlsx",
                col_names = TRUE,sheet=1)
d_logit<-glm(Coupon~Spending + Card,
             data = d1,
             family = binomial)
summary(d_logit)

plot(d_logit)


# predict the probability

(predict_odd <- predict(d_logit,data=d1))

(predict_d <- predict(d_logit,data=d1,type = "response"))


# Estimated Logistic Regression Equation

# Significance Test for Logistic Regression

#12. Non-parameter methods ---------------------------------------------------

# Sign Test

## sign test ---------------------------------------------------------------
#' a sign test is used to decide whether a binomial distribution has the equal chance of success and failure.

# Example
# Wilcoxon Signed-Rank Test
# Mann-Whitney-Wilcoxon Test
# Kruskal-Wallis Test



