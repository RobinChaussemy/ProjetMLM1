library(plotly)
library(ggridges)
data <- read.csv("https://www.dropbox.com/scl/fi/d3v41yp6x9cxlqvueoet3/membersClean.csv?rlkey=v9xfdgu6oyubjur9rlu6k9eow&dl=1")
x1 <- data$age
y <- data$season
p <-  ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 2)

##graphique quanti
##histogramme## 
nbClasses <- 4
plot <- plot_ly(data, x = ~age, type = "histogram", xbins = list(size = (max(data$age) - min(data$age)) / nbClasses),marker=list(color = "grey"))
plot 

##density
plot <- plot_ly(data, x = ~x1, type = "histogram", histnorm = "probability", color = ~y) 
plot
#densite 
densite <- density(x1)
densite_plot <- plot_ly(x = densite$x, y = densite$y, type = "scatter", mode = "lines", line = list(shape = "linear",color="red"))
densite_plot


## boxplot##
#quanti
plot <- plot_ly(data, y = ~x1, type = "box")
layout(plot, yaxis = list(title = "age"))

#avec bins 
x1cut <- cut(x1,6)
plot <- plot_ly(y = ~x1, type = "box", x = x1cut, width = 0.6,boxpoints="outliers")
layout(plot,xaxis = list(title = x1cut),yaxis = list(title = "age"))
#quantiquali
plot <- plot_ly(data, y = ~x1, type = "box",color = ~sex)
plot




##scaterplot##
##all pop##
plot <- plot_ly(data, x = ~year, y = ~age, type = "histogram2d")
plot
## selon une modalite##
plot <- plot_ly(data, x = ~year, y = ~age, type = "scatter",color = ~sex,colors = c("red","blue"),mode="markers")
plot 


####QUALI####

##bar##
plot <- plot_ly(data, x = ~peak_name, type = "histogram",color=~peak_name)
plot

##trie#
category_counts <- table(data$peak_name)

sorted_categories <- names(sort(category_counts))

sort_peak_name <- factor(data$peak_name, levels = sorted_categories)
plot <- plot_ly(x=sort_peak_name, type = "histogram", color = sort_peak_name)
plot


##mosaic##
...

###qualivs quanti###
plot <- plot_ly(data, x = ~peak_name, type = "histogram",color=~sex) 
plot %>% layout(barmode= "stack",xaxis=list(title="peak_name")) 





plot(
    dtFinal, x=:age, y=:year, facet_col=:season,
    kind="scatter", mode="markers",
    Layout(
        width=800, height=800,
        margin=attr(l=20,r=20,t=20,b=20),
        paper_bgcolor="LightSteelBlue"
    )
)
plot<- plot_ly(data[c(sample(73000,100)),], x= ~age,y=~year,type="scatter",)
plot

