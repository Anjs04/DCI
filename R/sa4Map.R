# Testing with SA4 data
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("rgeos")
require("plotly")
require("dplyr")

aus = readOGR(dsn="data", layer="SA4_2011_AUST")
aus@data$id = rownames(aus@data)

#aus.buffered = gBuffer(aus, width=0, byid=TRUE)
aus.points = fortify(aus, region="id") #replaced aus.buffered with aus
aus.df = join(aus.points, aus@data, by="id")

graph =
  ggplot(aus.df) +
  aes(long,lat,group=group,fill=SA4_NAME11)+
  #Don't want a legend with 150 variables so suppress the legend
  geom_polygon(show_guide = FALSE ) +
  geom_path(color="white") +
  #for some reason it maps too much ocean so limit coords (EDIT: due to Christmas Island)
  coord_equal(xlim = c(110, 155), ylim = c(-45, -10)) +
  scale_fill_hue(c=10, l=90) +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

#To add plotting points, merge data with aus@data$SA4_NAME11

sa4s = read.csv("data/1270055006_CG_POSTCODE_2011_SA4_2011.csv")
sa4s = sa4s[5:3115, 1:6]
names = c("POSTCODE", 
          "POSTCODE", 
          "SA4_CODE_2011", 
          "SA4_NAME_2011", 
          "RATIO", 
          "PERCENTAGE")
names(sa4s) = names
sa4s = sa4s[3:3107,]

# sa4s = 
#   sa4s[,2:5] %>%
#   mutate( RATIO = as.numeric(RATIO),
#           POSTCODE = as.numeric(POSTCODE) ) %>%
#   filter( RATIO > 0.5 ) %>%
#   .[,c(1, 3)]

# Original survey data needs SA4 column from postcodes
DCI_DATA = 
  ( read.csv("data/DCI_DATA.csv")  %>% rename( POSTCODE = Q2_197_OTHER ))

sa4s = sa4s[,2:5]
sa4s$POSTCODE = as.character(sa4s$POSTCODE)
DCI_DATA$POSTCODE = as.character(DCI_DATA$POSTCODE)


obtainSA4 <- function(postcode) {
  value <- as.character(head(sa4s[sa4s$POSTCODE == postcode, "SA4_NAME_2011"], 1))
  if (is.null(value) | length(value) == 0) {
    value <- ""
  }
  else {
    value <- as.character(value)
  }
  return (value)
}

data <- DCI_DATA
data$SA4_NAME_2011 <- unlist(sapply(data$POSTCODE, obtainSA4))
# data = left_join( DCI_DATA, sa4s )


View(table(data$SA4_NAME_2011))

View(table(data$POSTCODE))

table(is.na(data$SA4_NAME_2011))

#Manually check
View(sa4s)
View(data[, c("POSTCODE", "SA4_NAME_2011")])

# Summarise data by SA4 area for a question
merged = 
  ( data %>%
      group_by( SA4_NAME_2011 ) %>%
      summarise( median = median(Q431_26),
                 average = mean(Q431_26) ) %>%
      left_join( aus@data, ., by = c("SA4_NAME11" = "SA4_NAME_2011") )
  )

aus@data = merged
aus.points = fortify(aus, region="id") #replaced aus.buffered with aus
aus.df = join(aus.points, aus@data, by="id")

# Plotting with merged data
ggplot(aus.df) +
  aes(long, lat, group = group, fill = median) +
  #Don't want a legend with 150 variables so suppress the legend
  geom_polygon( ) +
  #for some reason it maps too much ocean so limit coords (EDIT: due to Christmas Island)
  coord_equal(xlim = c(110,155)) +
  scale_fill_gradient( high = "#132B43", low = "#56B1F7") +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

location = read.csv("data/LocationData.csv")
location$postcode = as.character(location$postcode)
df = left_join(DCI_DATA, location, by = c("POSTCODE" = "postcode"))

# Wrong coordinates for resp 812, userid 2389477443
# Should be -34.75513, 139.30616

df[3827, "lat"] = -34.75513
df[3827, "lon"] = 139.30616

# Allows font to be used, only neccessary for Windows
windowsFonts(Times=windowsFont("TT Times New Roman"))

#Change the numbers 1-7 to their response text
df$Q431_26 = factor(df$Q431_26, labels=c(
  "Several times per day",
  "Daily",
  "Weekly",
  "Monthly",
  "Less than Once a Month",
  "Never",
  "Don't know")
)

scatter =
  ggplot() +

  ###The Background Map###
  #Don't want a legend with 150 variables so suppress the legend
  #geom_polygon(data = aus.df, 
  #             aes(long, lat, group = group, fill = SA4_NAME11), 
  #             show_guide = FALSE ) +
  geom_path(data = aus.df, 
            aes(long, lat, group = group, fill = SA4_NAME11), 
            color="#DFDFDF") +
  #for some reason it maps too much ocean so limit coords (EDIT: due to Christmas Island)
  #scale_fill_hue(c=10, 
  #               l=90) +
  theme(
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(family = "Times", 
                              color="#000000"),
    legend.title = element_blank()
    ) +

  ###The Transparent Jittered Scatterplot###
  geom_point(data = df, 
             aes(lon, lat, colour = Q431_26),
             position = position_jitter(00.004, 00.004),
             alpha = 0.05) +
  coord_equal(xlim = c(110, 155), 
              ylim = c(-45, -10)) +
  scale_colour_brewer() +
  ggtitle("Replace with int.name") +
  guides(colour = guide_legend(override.aes = list(alpha = 1,
                                                   shape = 15, 
                                                   size = 10)))


###Interactive Plotly Map###
ggplotly(scatter)
