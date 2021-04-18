# read in temp and plot
library(dplyr)
library(ggplot2)
library(chron)
library(zoo)
# install.packages('ggridges')
library(ggridges)
library(lubridate)

# tempgz <- gzfile("temperature_small/temperature.csv.1.gz", "rt")
# temp <- read.csv(tempgz, header=F)
# glimpse(temp)

extract_gz <- function(x) {
  tempgz <- gzfile(x, "rt")
  temp <- read.csv(tempgz, header=F)
  return(temp)
}

filenames <- list.files("temperature", pattern="*.gz", full.names=TRUE)
ldf <- lapply(filenames, extract_gz)
df <- bind_rows(ldf)


ma <- function(x, n = 5){filter(x, rep(1 / n, n), sides = 2)}

df
temp_plot <- df %>% 
  #filter(!is.na(V1)) %>% #
  mutate(#date = as.Date(V1, format = "%Y-%m-%d"),
         temp_ma = rollapply(V3,20,mean,fill=NA),
         date = as.Date(V1),
         day = as.numeric(format(date, format = "%d")),
         month = as.numeric(format(as.Date(V1,format="%Y-%m-%d"), format = "%m")),
         year = as.numeric(format(as.Date(V1,format="%Y-%m-%d"), format = "%Y")),
         hour = hour(as.POSIXct(V2, format="%H:%M:%S")),
         daytime = case_when(
           hour < 6 ~ 0,
           hour > 19 ~ 0,
           hour <= 19 & hour >= 6 ~ 1
         )
         ) %>% 
  filter(!is.na(temp_ma))
  #filter(year != 2021)

glimpse(temp_plot)
unique(df$V2)
  
temp_plot %>% 
  ggplot() +
  geom_line(aes(x=date, y=temp_ma))

unique(temp_plot$daytime)
temp_plot[30000:nrow(temp_plot),] %>% 
  ggplot(
  aes(x = temp_ma, y = as.character(date), fill = as.character(daytime)))+#..x..)) +#, fill = as.character(daytime), color = as.character(daytime))) +
  geom_density_ridges_gradient(scale = 5, size = 0.3, rel_min_height = 0.01, color = "#fffff0") +
  scale_fill_cyclical(values = c("#0A1B2A", "#0A1B2A")) + # 0B3B24
  # scale_fill_cyclical(values = c("#0A2A22", "#fffff0")) + # 0B3B24 ### this is it almost
  # scale_fill_gradient(low="#0570b0", high="White")
  # scale_fill_gradient(low = "#0B3B24", high="#ffddaa") +
  # scale_color_gradient(low = "#0B3B24", high="#ffddaa") +
  # scale_color_manual(values="red")
  # scale_fill_brewer(palette = 4)+
  # scale_color_manual(values=c("red", "blue"))
  # scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Reutestraße') +
  theme_ridges(grid = FALSE) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_rect(fill="#fffff0"),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

every_nth <- function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}




temp_plot[1:200000,] %>% 
  ggplot(aes(x = temp_ma, y = as.character(date), fill = as.character(daytime)))+#..x..)) +#, fill = as.character(daytime), color = as.character(daytime))) +
  geom_density_ridges(scale = 5, size = 0.3, rel_min_height = 0.01, color = "#fffff0") +
  scale_fill_cyclical(values = c("#0A2A22", "#0A2A22")) + # 0B3B24 0A2A22 orange a34814
  scale_y_discrete(breaks = as.character(as.Date(temp_plot$date[seq(1,200000,20000)], format = "%Y")),
                   expand = c(0, 0)) +
  # scale_fill_cyclical(values = c("#0A2A22", "#fffff0"))  # 0B3B24 ### this is it almost
  # scale_fill_gradient(low="#0570b0", high="White")
  # scale_fill_gradient(low = "#0B3B24", high="#ffddaa") +
  # scale_color_gradient(low = "#0B3B24", high="#ffddaa") +
  # scale_color_manual(values="red")
  # scale_fill_brewer(palette = 4)+
  # scale_color_manual(values=c("red", "blue"))
  # scale_fill_viridis_c(name = "Temp. [F]", option = "C") +
  # labs(title = 'Temperatures in Reutestraße') +
  theme_ridges(grid = FALSE) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        # axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_rect(fill="#fffff0"),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())


png("temp_green2.png", width = 1800, height = 1800, res = 100)
