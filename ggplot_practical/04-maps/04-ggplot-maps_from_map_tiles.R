#' ---
#' title: "ggplot: Tile based maps"
#' author: "Jonas Schöley"
#' date: "June 22th, 2017"
#' output:
#'   github_document:
#'     toc: true
#' ---

library(tidyverse)
library(ggmap)

#'## Guardian: The Counted

#' The British newspaper "The Guardian" assembled a database of people killed
#' by police in the US in 2015.
#' Source: <http://www.theguardian.com/thecounted>

#' Read the data. Note that the strings should not be automatically converted
#' to factor variables.
counted <- read_csv("the_counted.csv", na = "Unknown")
head(counted)

#' Use Google Maps API to get latitude and longitude for each of the places
#' in the dataset. This is completely automated, but takes some time and is
#' limited by Google to 2500 map requests per day per IP. The `geocode` function
#' is part of the `ggmap` package.

# killed <-
#   counted %>%
#   mutate(citystate = paste(city, state)) %>%
#   bind_cols(., geocode(.$citystate))
load("killed.Rdata")

#' Download a map of the US along with geographical coordinates. Note that this
#' is not a shapefile but a raster image in Mercator map projection with
#' longitude and latitude information. It is readily formatted for usage with
#' `ggmap`.

usmap <- get_map(location = c(-130, 20, -60, 50),
                 maptype = "toner")

#' A dot-density map of killed persons located by longitude and latitude.
#' Instead of `ggplot` + `data` we use the `ggmap` command and supply it with
#' our mapdata produced by `get_map`. The rest standard `ggplot` and we
#' can use add ggplot geoms as usual. The only difference is that we must pass
#' the data we want to display on the map as an extra `data` argument to the
#' individual geoms.

ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat),
             colour = "red")

#' We can sum up cases at the same position and map the summed value to
#' the size aestetic. `..n..` is a variable produced by the `sum` statistic
#' and gives the number of cases at each `lon` and `lat`.

ggmap(usmap) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", shape = 1)

#' We overlay 2d density contours...

ggmap(usmap) +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", shape = 1)

#' ...and shade them according to level.

ggmap(usmap) +
  geom_density2d(data = killed,
                 aes(x = lon, y = lat),
                 bins = 5) +
  geom_polygon(data = killed,
               aes(x = lon, y = lat, fill = ..level..),
               stat = "density2d",
               bins = 5,
               alpha = 0.2) +
  geom_point(data = killed,
             aes(x = lon, y = lat, size = ..n..),
             stat = "sum",
             colour = "red", shape = 1)

#' Note that this is not a very informative visualization as we have basically
#' just reproduced population density. Dot maps are much more useful on a
#' smaller scale, e.g. neighbourhoods or cities.
#'
#'## Afghan War Diary

afg <-
  read_csv("afg_war_diary.csv") %>%
  mutate(date = parse_datetime(DateOccurred, format = "%d/%m/%Y %H:%M"),
         year = lubridate::year(date))

kabul <- get_map(location = c(69.166667, 34.533333), zoom = 12)

ggmap(kabul) +
  geom_point(data = afg,
             aes(x = Longitude, y = Latitude, color = AttackOn))

ggmap(kabul) +
  geom_point(data = afg,
             aes(x = Longitude, y = Latitude, color = AttackOn)) +
  facet_wrap(~year)