#+
# tables_plots_examples_textlab.R
#
#  Laura Wolton
# last updated 10/29/20
#
#-
#load fonts
library(extrafont)
library(extrafontdb)
font_import()
#wait after you say "y" enter
loadfonts(device="win") # if you want to write to pdf then use "postscript"

library(stargazer)
library(ggplot2)

#using a default data set attitude
#attitude is a dataframe
#you need to have a dataframe or regression results for this
#the default of this is summary statistics for a regular group of data
#you can specify 
#default type is LaTeX, can also be "html" or "text"
# this is from https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
stargazer(attitude, type="html", out="testtable.html")

stargazer(attitude[1:4,], type="html", out="testtable.html", summary=FALSE, rownames=FALSE)


###most useful for regression results
linear.1 <- lm(rating ~ complaints + privileges + learning + raises + critical,
               data=attitude)
linear.2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
## create an indicator dependent variable, and run a probit model
attitude$high.rating <- (attitude$rating > 70)
probit.model <- glm(high.rating ~ learning + critical + advance, data=attitude,
                    family = binomial(link = "probit"))

stargazer(linear.1, linear.2, probit.model, title="Results", align=TRUE, type="html",out="testtable.html")

#plot a regression
plot(linear.1)
ggplot(attitude, aes(x=rating,y=critical)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  ggtitle(paste("Adj R2 = ",signif(summary(linear.1)$adj.r.squared, 5),
                     "Intercept =",signif(linear.1$coef[[1]],5 ),
                     " P =",signif(summary(linear.1)$coef[2,4], 5))) +
  #margins go in order always Top, right, bottom, left t,r,b,l
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(plot.title =element_text(family="Times New Roman", size=14))+
annotate("text", label = paste(" Slope =",signif(linear.1$coef[[2]], 5)), x = 50, y = 75, color = "white") 

#####
# Libraries
#working with grid arrange
# from https://www.r-graph-gallery.com/261-multiple-graphs-on-same-page.html
library(ggplot2)
library(gridExtra)

# Make 3 simple graphics:
g1 <- ggplot(mtcars, aes(x=qsec)) + geom_density(fill="slateblue")
g2 <- ggplot(mtcars, aes(x=drat, y=qsec, color=cyl)) +
  geom_point(size=5) + theme(legend.position="none")
g3 <- ggplot(mtcars, aes(x=factor(cyl), y=qsec, fill=cyl)) + geom_boxplot() + theme(legend.position="none")
g4 <- ggplot(mtcars , aes(x=factor(cyl), fill=factor(cyl))) +  geom_bar()

# Plots
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 2)
grid.arrange(g1, g2, g3, nrow = 3)
grid.arrange(g2, arrangeGrob(g3, g4, ncol=2), nrow = 1)
grid.arrange(g2, arrangeGrob(g3, g4, nrow=2), nrow = 1)



##### facet wrap example#####
# from https://www.r-graph-gallery.com/223-faceting-with-ggplot2.html
# Libraries
library(tidyverse)
library(hrbrthemes)
library(babynames)
library(viridis)
library(dplyr)

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/3_TwoNumOrdered.csv", header=T)
data$date <- as.Date(data$date)

# Load dataset from github
don <- babynames %>% 
  filter(name %in% c("Ashley", "Amanda", "Mary", "Kayley",  "Dorothy", "Stephanie", "Helen", "Jennifer", "Laura")) %>%
  filter(sex=="F")

# Plot
don %>%
  ggplot( aes(x=year, y=n, group=name, fill=name)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle("Popularity of American names in the previous 30 years") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)
  ) +
  facet_wrap(~name, scale="free_y") +
  theme(legend.position="bottom")

#### adding other color palettes
library(RColorBrewer)
#Let’s look at all the associated palettes.

display.brewer.all()
display.brewer.pal(n = 8, name = 'Dark2')
j_brew_colors <- brewer.pal(n = 8, name = "Dark2")
###can use single colors
ggplot(mtcars, aes(x=drat)) +
       geom_density(
             color=j_brew_colors[3],
             fill=j_brew_colors[2],
             size=2
         )
#or you can call the whole palette
p <- ggplot(iris,
            aes(x=Sepal.Length, y=Sepal.Width, color=Species)) +
  geom_point(size=6)
p + scale_color_brewer(palette = "Dark2")







