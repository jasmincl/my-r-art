# data source: https://www.kaggle.com/heheheluke/parks-and-recreation-scripts
# Charge the circlize library
library(circlize)

# read in temp and plot
library(dplyr)
library(ggplot2)
library(chron)
library(zoo)
# install.packages('ggridges')
library(ggridges)
library(lubridate)
# install.packages('data.table')
library(data.table)
library(tidyr)
install.packages('fastDummies')
library(fastDummies)

df <- read.csv("scripts/s3e01.csv")

filenames <- list.files("scripts", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)
df <- bind_rows(ldf)


glimpse(df)
unique(df$Character)

df <- df %>% 
  mutate(
    Leslie = case_when(grepl("Leslie", Line) ~ 1, TRUE ~ 0),
    Ben = case_when(grepl("Ben", Line) ~ 1, TRUE ~ 0),
    Ron = case_when(grepl("Ron", Line) ~ 1, TRUE ~ 0),
    April = case_when(grepl("April", Line) ~ 1, TRUE ~ 0),
    Andy = case_when(grepl("Andy", Line) ~ 1, TRUE ~ 0),
    Jerry = case_when(grepl("Jerry", Line) ~ 1, TRUE ~ 0),
    Tom = case_when(grepl("Tom", Line) ~ 1, TRUE ~ 0),
    Ann = case_when(grepl("Ann", Line) ~ 1, TRUE ~ 0),
    Chris = case_when(grepl("Chris", Line) ~ 1, TRUE ~ 0),
    Tammy = case_when(grepl("Tammy", Line) ~ 1, TRUE ~ 0),
    character_firstname = gsub("([A-Za-z]+).*", "\\1", Character)
  ) 
library(stringr)
df$nwords <- str_count(df$Line, "\\w+")
quantile(df$nwords, na.rm = TRUE)
glimpse(df)

glimpse(df)
binom <- data.frame(y=runif(nrow(df)), x=runif(nrow(df)), speaker_=as.factor(df$character_firstname))
speaker_dummies <- as.data.frame(model.matrix(y ~ x + speaker_,binom))

speaker_dummies <- speaker_dummies %>% dplyr::select(-`(Intercept)`, -x)
glimpse(speaker_dummies)

df_final <- cbind(df, speaker_dummies)

## plot sun: https://www.r-graph-gallery.com/230-draw-part-of-the-circular-plot-only.html
set.seed(123)
#https://www.r-graph-gallery.com/123-circular-plot-circlize-package-2.html
main_characters <- c("Leslie", "Ben", "April", "Andy", "Chris", "Ann", "Jerry", "Tom", "Donna")
df_long <- df %>% 
  pivot_longer(cols=3:12, names_to = "mentioned_character") %>% 
  filter(value == 1 & character_firstname %in% main_characters) %>% 
  mutate(nwords_cat = cut(nwords, breaks = quantile(nwords))) 

df_sampled_smallcount <- df_long %>% 
  group_by(nwords_cat) %>% 
  filter(nwords < 50) %>% 
  sample_n(50)

df_sampled_largecount <- df_long %>% 
  group_by(nwords_cat) %>% 
  filter(nwords >= 50)

df_long <- df_sampled_largecount %>% 
  bind_rows(df_sampled_smallcount) %>% 
  filter(nwords < 60) %>% 
  arrange(mentioned_character)
# Create an edge list: a list of connections between 10 origin nodes, and 10 destination nodes:
# data <- data.frame(df_long$character_firstname, df_long$mentioned_character)
data <- data.frame(df_long$Line, df_long$Line)

# Transform input data in a adjacency matrix
adjacencyData <- with(data, table(df_long$Line, df_long$Line))

glimpse(data)
glimpse(adjacencyData)

df %>%  arrange(desc(nwords)) %>% head()
# Make the circular plot --------------------------------------------
# chordDiagram(adjacencyData, transparency = 0.5)

par("cex")
par("ps")
par(mar = c(1, 2, 0.1, 0.1) )
grid.col = c("Leslie"= "grey","Ben"= "grey","April"= "grey","Andy"= "grey","Chris"= "grey","Ann"= "grey","Jerry"= "grey","Tom"= "grey","Donna" = "grey")


chordDiagram(adjacencyData, grid.col = "#a34814", transparency = 1, annotationTrackHeight = c(0.03, 0.01),
             annotationTrack = "grid",
             preAllocateTracks = 3) # transparency = 1 removes connections

circos.track(track.index = 3, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), col = "black",
              cex = fontsize(2))
}, bg.border = NA) # here set bg.border to NA is important
circos.clear()

png('heatMap.png', pointsize=10, width=700, height=480)
png('heatMap.png', pointsize=10, width=300, height=480)











## other stuff to try out ----------------------
circos.initialize(letters[1:4], xlim = c(0, 1))
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ycenter, 
              strrep(CELL_META$sector.index, 20), cex = 1,
              facing = "bending.inside", niceFacing = TRUE)
})
circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ycenter, 
              strrep(CELL_META$sector.index, 20), cex = fontsize(20),
              facing = "bending.inside", niceFacing = TRUE)
})


numbers <- sample(c(1:1000), 100, replace = T)
data <- matrix( numbers, ncol=5)
rownames(data) <- paste0("orig-", seq(1,20))
colnames(data) <- paste0("dest-", seq(1,5))

# Load the circlize library
library(circlize)

# Make the circular plot
chordDiagram(data, transparency = 0.5)


# library
library(circlize)

# Create data
factors <- letters[1:4]
x1 <- runif(100)
y1 <- runif(100)

# general parameter of the plot. 
# With canvas.xlim and canvas.ylim we kind of "zoom on a part of the plot:
par(mar = c(1, 2, 0.1, 0.1) )
circos.par("track.height" = 0.7, "canvas.xlim" = c(0, 1), "canvas.ylim" = c(0, 1), "gap.degree" = 0, "clock.wise" = FALSE)

# Make the usual plot, but with no border
circos.initialize(factors = factors, xlim = c(0, 1)) 
circos.trackPlotRegion(factors = factors, ylim = c(0, 1), bg.border = NA ) 

# Finally we plot only the firs sector, so let's change its border to "black" to see it
circos.updatePlotRegion(sector.index = "a", bg.border = "grey" , bg.lwd=0.2)

# Now we can add a plot in this section! You can repeat these steps to add several regions
circos.lines(x1, y1, pch = 16, cex = 0.5, type="h" , col="#69b3a2" , lwd=3)

# Add axis
circos.axis(h="bottom" , labels.cex=0.4, direction = "inside" )

#clear
circos.clear()
