
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







# Fitting an INLA Spatial Model to the Data:
# Make data frame for INLA:
segments_inla <- filter(segments_full, year %in% c(2020, 2019, 2018, 2017))
segments_inla <- droplevels(segments_inla)
data_inla <- data.frame(segments_inla)
data_inla <- data_inla %>%
  filter(!is.na(AADT)) %>%
  mutate(AADT.pt = 0.0001 * AADT) # Scale by 10 000 to avoid getting negative eigenvalues for the Hessian
data_inla <- arrange(data_inla, year, STREKNING, DELSTREKNING, FRA.METER.x)

# Number of years:
n_years <- length(unique(data_inla$year))

# Adding an index on the years for the grouping of them:
year_index <- as.numeric(data_inla$year) - min(as.numeric(data_inla$year)) + 1
data_inla$year_index <- as.factor(year_index)

offset <- 3000







# Finding theta1 and theta3 in
# ln(tau(s)) = theta1 + theta2 * AADT
# ln(kappa(s)) = theta3 + theta4 * AADT
# using that
# theta1 = ln(tau) = -ln(2) - ln(sigma) - 3/2 * ln(kappa)
# theta3 = ln(kappa) = 1/2 * ln(12) - ln(range)
# because d = 1 and alpha = 2:
find_theta3 <- function(range) {
  return(1/2 * log(12) - log(range))
}

find_theta1 <- function(sigma, range) {
  return(-log(2) - log(sigma) - 3/2 * (1/2 * log(12) - log(range)))
}

# We assume zero effect of the AADT and thus choose theta2 and theta4 to have a
# Normal(0, 1) distribution:
theta2_mean <- 0
theta4_mean <- 0
theta2_prec <- 1
theta4_prec <- 1

# We use the results from the stationary case to define the priors here.
# For the grouped data: E(range) = 731.640, E(sigma) = 0.447.
# For 2017: E(range) = 344.797, E(sigma) = 0.880.
# For 2018: E(range) = 1377.554, E(sigma) = 0.425.
# For 2019: E(range) = 835.061, E(sigma) = 1.182.
# For 2020: E(range) = 1574.599, E(sigma) = 0.392.
# We use a rounded average value of these quantities:
range_avg <- 900
sigma_avg <- 0.6

# Thus we get that:
theta1_mean <- round(find_theta1(sigma_avg, range_avg), 3) # = 8.158
theta3_mean <- round(find_theta3(range_avg), 3) # = -5.56

# We assume a precision of 1 for theta1 and theta3
# (this allows us to reach many values of the range and sigma, as they are
# different for the different years in the stationary model):
theta1_prec <- 1
theta3_prec <- 1




# Trying a non-stationary model for 2020:
data_inla_2020 <- filter(data_inla, year == 2020)

mesh_2020 <- inla.mesh.1d(loc = data_inla_2020$X, boundary = "free", offset = offset)

A_2020 <- inla.spde.make.A(mesh = mesh_2020, loc = data_inla_2020$X.MID)

AADT.pt_2020 <- data_inla_2020$AADT.pt

spde_2020 <- inla.spde2.matern(mesh = mesh_2020,
                               B.tau = cbind(0, 1, AADT.pt_2020, 0, 0),
                               B.kappa = cbind(0, 0, 0, 1, AADT.pt_2020),
                               theta.prior.mean = c(theta1_mean, theta2_mean, theta3_mean, theta4_mean),
                               theta.prior.prec = c(theta1_prec, theta2_prec, theta3_prec, theta4_prec),
                               alpha = 2)

index_2020 <- inla.spde.make.index(name = "ns.spatial.field.2020", n.spde = spde_2020$n.spde)

stack_2020 <- inla.stack(data = list(rutting = data_inla_2020$rut.diff.response),
                         A = list(A_2020, 1),
                         effects = list(c(index_2020, list(intercept = 1)),
                                        list(AADT = data_inla_2020$AADT.pt,
                                             road_cover = data_inla_2020$MASSETYPE,
                                             road_width = data_inla_2020$width)),
                         tag = "ns_est_2020")



# Trying a non-stationary model for 2019:
data_inla_2019 <- filter(data_inla, year == 2019)

mesh_2019 <- inla.mesh.1d(loc = data_inla_2019$X, boundary = "free", offset = offset)

A_2019 <- inla.spde.make.A(mesh = mesh_2019, loc = data_inla_2019$X.MID)

AADT.pt_2019 <- data_inla_2019$AADT.pt

spde_2019 <- inla.spde2.matern(mesh = mesh_2019,
                               B.tau = cbind(0, 1, AADT.pt_2019, 0, 0),
                               B.kappa = cbind(0, 0, 0, 1, AADT.pt_2019),
                               theta.prior.mean = c(theta1_mean, theta2_mean, theta3_mean, theta4_mean),
                               theta.prior.prec = c(theta1_prec, theta2_prec, theta3_prec, theta4_prec),
                               alpha = 2)

index_2019 <- inla.spde.make.index(name = "ns.spatial.field.2019", n.spde = spde_2019$n.spde)

stack_2019 <- inla.stack(data = list(rutting = data_inla_2019$rut.diff.response),
                         A = list(A_2019, 1),
                         effects = list(c(index_2019, list(intercept = 1)),
                                        list(AADT = data_inla_2019$AADT.pt,
                                             road_cover = data_inla_2019$MASSETYPE,
                                             road_width = data_inla_2019$width)),
                         tag = "ns_est_2019")


# Trying a non-stationary model for 2018:
data_inla_2018 <- filter(data_inla, year == 2018)

mesh_2018 <- inla.mesh.1d(loc = data_inla_2018$X, boundary = "free", offset = offset)

A_2018 <- inla.spde.make.A(mesh = mesh_2018, loc = data_inla_2018$X.MID)

AADT.pt_2018 <- data_inla_2018$AADT.pt

spde_2018 <- inla.spde2.matern(mesh = mesh_2018,
                               B.tau = cbind(0, 1, AADT.pt_2018, 0, 0),
                               B.kappa = cbind(0, 0, 0, 1, AADT.pt_2018),
                               theta.prior.mean = c(theta1_mean, theta2_mean, theta3_mean, theta4_mean),
                               theta.prior.prec = c(theta1_prec, theta2_prec, theta3_prec, theta4_prec),
                               alpha = 2)

index_2018 <- inla.spde.make.index(name = "ns.spatial.field.2018", n.spde = spde_2018$n.spde)

stack_2018 <- inla.stack(data = list(rutting = data_inla_2018$rut.diff.response),
                         A = list(A_2018, 1),
                         effects = list(c(index_2018, list(intercept = 1)),
                                        list(AADT = data_inla_2018$AADT.pt,
                                             road_cover = data_inla_2018$MASSETYPE,
                                             road_width = data_inla_2018$width)),
                         tag = "ns_est_2018")



# Trying a non-stationary model for 2017:
data_inla_2017 <- filter(data_inla, year == 2017)

mesh_2017 <- inla.mesh.1d(loc = data_inla_2017$X, boundary = "free", offset = offset)

A_2017 <- inla.spde.make.A(mesh = mesh_2017, loc = data_inla_2017$X.MID)

AADT.pt_2017 <- data_inla_2017$AADT.pt

spde_2017 <- inla.spde2.matern(mesh = mesh_2017,
                               B.tau = cbind(0, 1, AADT.pt_2017, 0, 0),
                               B.kappa = cbind(0, 0, 0, 1, AADT.pt_2017),
                               theta.prior.mean = c(theta1_mean, theta2_mean, theta3_mean, theta4_mean),
                               theta.prior.prec = c(theta1_prec, theta2_prec, theta3_prec, theta4_prec),
                               alpha = 2)

index_2017 <- inla.spde.make.index(name = "ns.spatial.field.2017", n.spde = spde_2017$n.spde)

stack_2017 <- inla.stack(data = list(rutting = data_inla_2017$rut.diff.response),
                         A = list(A_2017, 1),
                         effects = list(c(index_2017, list(intercept = 1)),
                                        list(AADT = data_inla_2017$AADT.pt,
                                             road_cover = data_inla_2017$MASSETYPE,
                                             road_width = data_inla_2017$width)),
                         tag = "ns_est_2017")



# Trying a non-stationary model for all the years:
# Creating a time mesh for the four years:
knots <- seq(1, 4, length = n_years)
time_mesh <- inla.mesh.1d(loc = knots, degree = 2, boundary = "free")

# Creating a 1D mesh using X along the road:
mesh_common <- inla.mesh.1d(loc = data_inla$X, boundary = "free", offset = offset)

# Creating the A-matrix:
A_common <- inla.spde.make.A(mesh = mesh_common, loc = data_inla$X.MID,
                             group = as.numeric(data_inla$year_index), n.group = time_mesh$m,
                             group.mesh = time_mesh)

AADT.pt_common <- AADT.pt_2020

# Making the SPDE model:
spde_common <- inla.spde2.matern(mesh = mesh_common,
                                 B.tau = cbind(0, 1, AADT.pt_common, 0, 0),
                                 B.kappa = cbind(0, 0, 0, 1, AADT.pt_common),
                                 theta.prior.mean = c(theta1_mean, theta2_mean, theta3_mean, theta4_mean),
                                 theta.prior.prec = c(theta1_prec, theta2_prec, theta3_prec, theta4_prec),
                                 alpha = 2)

# Making the index:
index_common <- inla.spde.make.index(name = "ns.spatial.field.common", n.spde = spde_common$n.spde, n.group = time_mesh$m)

# Making the INLA stack:
stack_common <- inla.stack(data = list(rutting = data_inla$rut.diff.response),
                           A = list(A_common, 1),
                           effects = list(c(index_common, list(intercept = 1)),
                                          list(AADT = data_inla$AADT.pt,
                                               road_cover = data_inla$MASSETYPE,
                                               year = data_inla$year,
                                               road_width = data_inla$width)),
                           tag = "ns_group_est")



# A stack for prediction:
mesh_pred <- mesh_2020

A_pred <- inla.spde.make.A(mesh = mesh_pred, loc = data_inla_2020$X.MID)

AADT.pt_pred <- data_inla_2020$AADT.pt

spde_pred <- inla.spde2.matern(mesh = mesh_pred,
                               B.tau = cbind(0, 1, AADT.pt_pred, 0, 0),
                               B.kappa = cbind(0, 0, 0, 1, AADT.pt_pred),
                               theta.prior.mean = c(theta1_mean, theta2_mean, theta3_mean, theta4_mean),
                               theta.prior.prec = c(theta1_prec, theta2_prec, theta3_prec, theta4_prec),
                               alpha = 2)

index_pred <- inla.spde.make.index(name = "ns.spatial.field.pred", n.spde = spde_pred$n.spde)

stack_pred <- inla.stack(data = list(rutting = NA),
                         A = list(A_pred, 1),
                         effects = list(c(index_pred, list(intercept = 1)),
                                        list(AADT = data_inla_2020$AADT.pt,
                                             road_cover = data_inla_2020$MASSETYPE,
                                             year = data_inla_2020$year,
                                             road_width = data_inla_2020$width)),
                         tag = "ns_pred")





# Connecting all these into one model:
stack_full <- inla.stack.join(stack_common, stack_2017, stack_2018, stack_2019, stack_2020, stack_pred)

formula_full_iid <- rutting ~ -1 + road_cover : AADT + road_width : AADT +
  f(year, model = "iid") +
  f(ns.spatial.field.common, model = spde_common) +
  f(ns.spatial.field.2017, model = spde_2017) +
  f(ns.spatial.field.2018, model = spde_2018) +
  f(ns.spatial.field.2019, model = spde_2019) +
  f(ns.spatial.field.2020, model = spde_2020)

# Running one time to find some initial values:
res_full_iid_init <- inla(formula = formula_full_iid,
                          data = inla.stack.data(stack = stack_full),
                          family = "gaussian",
                          control.predictor = list(A = inla.stack.A(stack_full), link = 1, compute = FALSE),
                          control.compute = list(dic = FALSE, waic = FALSE),
                          control.mode = list(theta = NULL),
                          verbose = FALSE)

# The initial values are thus:
initial_values_full <- as.double(paste(round(res_full_iid_init$internal.summary.hyperpar$mean, 3)))
initial_values_full <- tail(initial_values_full, -2)
prec_Gaussian <- round(res_full_iid_init$summary.hyperpar$mean[1], 3)
prec_year <- round(res_full_iid_init$summary.hyperpar$mean[2], 3)
initial_values_full <- c(prec_Gaussian, prec_year, initial_values_full)

# And we can run:
res <- inla(formula = formula_full_iid,
            data = inla.stack.data(stack = stack_full),
            family = "gaussian",
            control.predictor = list(A = inla.stack.A(stack_full), link = 1, compute = TRUE),
            control.compute = list(dic = TRUE, waic = TRUE),
            control.mode = list(theta = initial_values_full),
            verbose = FALSE)
