
library(ggplot2)
library(dplyr)
library(stringr)
library(rgdal)
library(tidyr)
library(readr)
library(leaflet)
library(units)
library(sf)
library(RColorBrewer)
library(mapview)
library(INLA)
library(latex2exp)
library(units)
library(gstat)
library(MCMCglmm)
library(gridExtra)
library(ggpubr)
library(forecast)
library(INLA)


# Preprocesing the Data:
# Create the data path:
data_path <- getwd() %>%
  dirname() %>%
  file.path("data")

seg_file <- "E16_felt2_2015_2020_geom_as_wkt.csv"
proj4string.area <- CRS("+proj=longlat +zone=33 +datum=WGS84 +unit=m")

# Load the different segments:
segments <- get0("sf.out")
if (is.null(segments)) {
  segments <- st_read(file.path(data_path, seg_file))
  st_crs(segments) <- proj4string.area
  num_to_meter <- function(x) set_units(x, "m")
  segments <- segments %>%
    mutate_at(vars(-WKT, -VEGSYSTEMREFERANSE, -geometry), as.numeric) %>% 
    mutate_at(vars(dist, dist.neigh), num_to_meter)
}

# Add length of segment:
segments$lengde <- 20





# Adding intensity to the data frame:
traffic_intensity <- read.csv(file.path(data_path, "traffic_intensity.csv"),
                              sep = ";",
                              encoding = "UTF-8",
                              stringsAsFactors = FALSE)

traffic_intensity <- traffic_intensity %>%
  filter(VEGNUMMER == 16 & VEGKATEGORI == "Europaveg") %>% 
  rename("AADT" = "MIDLERE.ÅDT")

add_intensity <- function(df_seg, df_intensity){
  merge_key <- c("STREKNING", "DELSTREKNING")
  intensity_subset <- select(df_intensity, STREKNING, DELSTREKNING, AADT,
                             FRA.METER, TIL.METER)
  tmp_merge <- left_join(df_seg, intensity_subset, by = merge_key)
  
  tmp_merge$match <- mapply(between, tmp_merge$FRA.METER.x, tmp_merge$FRA.METER.y,
                            tmp_merge$TIL.METER.y)
  
  m_key <- c("VEGSYSTEMREFERANSE", "year")
  tmp_failed_merge <- tmp_merge[!tmp_merge$match, ]
  tmp_failed_merge <- tmp_failed_merge[!duplicated(tmp_failed_merge[m_key]), ]
  tmp_merge <- tmp_merge[tmp_merge$match, ]
  tmp_merge <- tmp_merge[!duplicated(tmp_merge[m_key]), ]
  segs_not_found <- anti_join(tmp_failed_merge, tmp_merge, by = m_key)
  segs_not_found$AADT <- NaN
  
  return(rbind(tmp_merge, segs_not_found))
}

segments_intensity <- add_intensity(as.data.frame(segments), traffic_intensity)




# Read the asphalt data:
asphalt <- read.csv(file.path(data_path, "vegdekke.csv"),
                    sep = ";",
                    encoding = "UTF-8",
                    stringsAsFactors = FALSE)

# Filter to only be E16 with STREKNING from 1 to 10 (i.e. EV16 S2D1 to EV16 S10D1):
asphalt <- filter(asphalt, VEGNUMMER == 16 &
                    VEGKATEGORI == "Europaveg" &
                    STREKNING > 0 &
                    STREKNING < 11)

# DEKKELEGGINGSDATO mapped to year:
asphalt$year <- 0
asphalt$year <- asphalt$DEKKELEGGINGSDATO %>%
  as.Date(format = "%Y-%m-%d") %>%
  format("%Y") %>%
  as.integer()

# Remove where year is NA:
asphalt <- filter(asphalt, !is.na(year))

# Remove rows such that VEGSYSTEMREFERANSE is unique:
asphalt <- asphalt %>% distinct(VEGSYSTEMREFERANSE, .keep_all = TRUE)


# Giving the asphalt type the correct year:
asphalt_2015 <- filter(asphalt, year != 2020, year != 2019, year != 2018, year != 2017, year != 2016)
asphalt_2015$year <- 2015

asphalt_2016 <- filter(asphalt, year != 2020, year != 2019, year != 2018, year != 2017)
asphalt_2016$year <- 2016

asphalt_2017 <- filter(asphalt, year != 2020, year != 2019, year != 2018)
asphalt_2017$year <- 2017

asphalt_2018 <- filter(asphalt, year != 2020, year != 2019)
asphalt_2018$year <- 2018

asphalt_2019 <- filter(asphalt, year != 2020)
asphalt_2019$year <- 2019

asphalt_2020 <- asphalt
asphalt_2020$year <- 2020

asphalt_full <- rbind(asphalt_2015, asphalt_2016, asphalt_2017, asphalt_2018, asphalt_2019, asphalt_2020)

# Adding asphalt to the data frame:
add_asphalt_type <- function(df_seg, df_asphalt) {
  m_key <- c("STREKNING", "DELSTREKNING", "year")
  merged <- inner_join(df_seg,
                       df_asphalt[append(m_key, c("MASSETYPE",
                                                  "FRA.METER",
                                                  "TIL.METER"))],
                       by = m_key)
  merged <- merged[!duplicated(merged[c("VEGSYSTEMREFERANSE", "year")]), ]
  
  return(merged)
}

segments_asphalt_intensity <- add_asphalt_type(segments_intensity, asphalt_full)





# Adding the width of the road to the data set:
asphalt_width <- read.csv(file.path(data_path, "vegbredde.csv"),
                          sep = ";",
                          encoding = "UTF-8",
                          stringsAsFactors = FALSE)

# Filter to only be E16 with STREKNING from 1 to 10 (i.e. EV16 S2D1 to EV16 S10D1):
asphalt_width <- filter(asphalt_width, VEGNUMMER == 16 &
                          VEGKATEGORI == "Europaveg" &
                          STREKNING > 0 &
                          STREKNING < 11)

# Renaming:
asphalt_width <- asphalt_width %>% rename("width" = "KJØREBANEBREDDE..M.")
asphalt_width <- asphalt_width %>% rename("DELSTREKNING" = "STREKNING.DELSTREKNING")

# Changing from "," to "." and making a double:
asphalt_width$width <- as.double(gsub("\\,", ".", asphalt_width$width))

# Dividing by 2 to make the width of one line:
asphalt_width$width <- asphalt_width$width / 2

# Adding the width of the road to the data frame:
add_width <- function(df_seg, df_width) {
  merge_key <- c("STREKNING", "DELSTREKNING")
  width_subset <- select(df_width, STREKNING, DELSTREKNING, width, FRA.METER, TIL.METER)
  tmp_merge <- left_join(df_seg, width_subset, by = merge_key)
  
  m_key <- c("VEGSYSTEMREFERANSE", "year")
  tmp_merge <- tmp_merge[!duplicated(tmp_merge[m_key]), ]
  
  return(tmp_merge)
}

segments_full <- add_width(segments_asphalt_intensity, asphalt_width)


# Sort segments:
segments_full <- segments_full[order(segments_full$STREKNING,
                                     segments_full$DELSTREKNING,
                                     segments_full$FRA.METER.x,
                                     segments_full$year), ]



# Adding the rutting to the dataframe:
segments_full$rut <- segments_full$SPORDYBDE

segments_full <- segments_full %>%
  dplyr::group_by(VEGSYSTEMREFERANSE) %>%
  dplyr::mutate(rut.diff.lag1 = rut - dplyr::lag(rut,
                                                 n = 1,
                                                 default=dplyr::lag(rut, n = 1)[2]),
                rut.diff = cumsum(rut.diff.lag1), year = as.factor(year)) %>%
  ungroup() %>%
  dplyr::mutate(rut.diff.response = replace(rut.diff.lag1,
                                            rut.diff.lag1 / rut < -1/2,
                                            NaN))



# Changing the name of MASSETYPE to its abbreviation:
segments_full$MASSETYPE <- ifelse(segments_full$MASSETYPE == "Asfaltbetong", "Ab", segments_full$MASSETYPE)
segments_full$MASSETYPE <- ifelse(segments_full$MASSETYPE == "Asfaltgrusbetong", "Agb", segments_full$MASSETYPE)
segments_full$MASSETYPE <- ifelse(segments_full$MASSETYPE == "Skjelettasfalt", "Ska", segments_full$MASSETYPE)









# Fitting an INLA Model to the Data:
# Make data frame for INLA:
segments_inla <- filter(segments_full, year %in% c(2020, 2019, 2018, 2017))
segments_inla <- droplevels(segments_inla)
data_inla <- data.frame(segments_inla)
data_inla <- data_inla %>%
  filter(!is.na(AADT)) %>%
  mutate(AADT.pt = 0.0001 * AADT) # Scale by 10 000 to avoid getting negative eigenvalues for the Hessian
data_inla <- arrange(data_inla, year, STREKNING, DELSTREKNING, FRA.METER.x)



# Re-naming:
data_inla$road_cover <- data_inla$MASSETYPE
data_inla$AADT <- data_inla$AADT.pt
data_inla$rutting <- data_inla$rut.diff.response




formula <- rutting ~ -1 + road_cover : AADT + width : AADT + f(year, model = "iid")

res <- inla(formula = formula,
            data = data_inla,
            family = "gaussian",
            control.predictor = list(compute = TRUE),
            control.compute = list(dic = TRUE, waic = TRUE),
            verbose = FALSE)