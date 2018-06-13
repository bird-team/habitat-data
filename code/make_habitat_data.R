# Initialization
## set default options
options(stringsAsFactors = FALSE)

## define functions
st_explode <- function(x) {
  if (inherits(x, "sf")) {
    ex <- lapply(sf::st_geometry(x), sf::st_sfc)
    ex <- lapply(ex, sf::st_cast, "POLYGON")
    ex <- lapply(seq_along(ex),
               function(i) sf::st_sf(id = rep(i, length(ex[[i]])),
                                     geometry = ex[[i]]))
    ex <- do.call(rbind, ex)
    pos <- ex$id
    ex <- sf::st_geometry(ex)
    d <- as.data.frame(x)
    d <- d[, which(names(d) != "geometry"), drop = FALSE]
    d <- d[pos, , drop = FALSE]
    d <- tibble::as_tibble(d)
    d$geometry <- ex
    ex <- sf::st_sf(d)
  } else {
    ex <- sf::st_cast(x, "POLYGON")
  }
  sf::st_set_crs(ex, sf::st_crs(x))
}

st_remove_slivers <- function(x) {
  x <- st_explode(x)
  if (inherits(x, "sfc"))
    return(x[as.numeric(sf::st_area(x)) > 0.1])
  return(x[as.numeric(sf::st_area(x)) > 0.1, ])
}

st_fast_difference <- function(x, y) {
  x <- sf::st_geometry(x)
  y <- sf::st_geometry(y)
  xu <- sf::st_union(x)
  sf::st_crs(xu) <- sf::st_crs(x)
  yu <- sf::st_union(y)
  sf::st_crs(yu) <- sf::st_crs(x)
  xe <- st_explode(x)
  sf::st_crs(xe) <- sf::st_crs(x)
  ye <- st_explode(y)
  sf::st_crs(ye) <- sf::st_crs(x)
  xi <- as.matrix(sf::st_intersects(xe, yu))[, 1]
  yi <- as.matrix(sf::st_intersects(ye, xu))[, 1]
  out <- sf::st_difference(xe[xi], sf::st_union(ye[yi]))
  out <- append(xe[!xi], out)
  out
}

## create temporary directories
tmp1 <- file.path(tempdir(), basename(tempfile(fileext = "")))
tmp2 <- file.path(tempdir(), tempfile(fileext = ""))
tmp3 <- file.path(tempdir(), tempfile(fileext = ""))
tmp4 <- file.path(tempdir(), tempfile(fileext = ""))
dir.create(tmp1, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp2, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp3, showWarnings = FALSE, recursive = TRUE)
dir.create(tmp4, showWarnings = FALSE, recursive = TRUE)

## set parameters
unzip(dir("data/study-area", "^.*\\.zip$", full.names = TRUE),
          exdir = tmp1)
study_area_path <- dir(tmp1, "^.*\\.shp$", full.names = TRUE)[1]
unzip(dir("data/broad-vegetation-groups", "^.*\\.zip$", full.names = TRUE),
          exdir = tmp2)
vegetation_path <- dir(tmp2, "^.*\\.shp$", recursive = TRUE,
                       full.names = TRUE)[1]
unzip(dir("data/built-up-areas", "^.*\\.zip$", full.names = TRUE), exdir = tmp3)
bua_path <- dir(tmp3, "^.*\\.shp$", recursive = TRUE, full.names = TRUE)[1]
unzip(dir("data/wetlands", "^.*\\.zip$", full.names = TRUE), exdir = tmp4)
wetland_path <- dir(tmp4, "^.*\\.shp$", recursive = TRUE, full.names = TRUE)[1]
classification_path <- dir("data/classification", "^.*\\.csv$",
                           full.names = TRUE)[1]

## load packages
library(dplyr)
library(sf)

# Preliminary processing
## load and data
study_area_data <- sf::st_read(study_area_path)
vegetation_data <- sf::st_read(vegetation_path)
bua_data <- sf::st_read(bua_path)
wetland_data <- sf::st_read(wetland_path)
classification_data <- data.table::fread(classification_path,
                                         data.table = FALSE)

## reproject spatial data
study_area_data <- sf::st_transform(study_area_data, 3857)
vegetation_data <- sf::st_transform(vegetation_data, 3857)
bua_data <- sf::st_transform(bua_data, 3857)
wetland_data <- sf::st_transform(wetland_data, 3857)

## format study area
study_area_data <- study_area_data %>%
                   sf::st_union() %>%
                   lwgeom::st_make_valid() %>%
                   lwgeom::st_snap_to_grid(1) %>%
                   sf::st_simplify(100) %>%
                   sf::st_collection_extract(type = "POLYGON") %>%
                   lwgeom::st_make_valid()
study_area_data <- sf::st_sf(name = "Brisbane", geometry = study_area_data)

## create boundary box for processing data
bbox_data <- study_area_data %>%
             sf::st_bbox() %>%
             sf::st_as_sfc()

## crop data to within study area
vegetation_data <- sf::st_intersection(vegetation_data, bbox_data)
bua_data <- sf::st_intersection(bua_data, bbox_data)
wetland_data <- sf::st_intersection(wetland_data, bbox_data)

## repair data
vegetation_data <- lwgeom::st_make_valid(vegetation_data)
bua_data <- lwgeom::st_make_valid(bua_data)
wetland_data <- lwgeom::st_make_valid(wetland_data)

## aggregate data non-rem sets
bua_data <- sf::st_union(bua_data)
wetland_data <- sf::st_union(wetland_data)

## snap data sets to grid
vegetation_data <- lwgeom::st_snap_to_grid(vegetation_data, 1)
wetland_data <- lwgeom::st_snap_to_grid(wetland_data, 1)
bua_data <- lwgeom::st_snap_to_grid(bua_data, 1)

## simplify data
wetland_data <- sf::st_simplify(wetland_data, TRUE, dTolerance = 1)
bua_data <- sf::st_simplify(bua_data, TRUE, dTolerance = 1)

## select relevant columns in vegetation data
vegetation_data <- vegetation_data %>% select(DBVG5M)

## repair data
vegetation_data <- lwgeom::st_make_valid(vegetation_data)
bua_data <- lwgeom::st_make_valid(bua_data)
wetland_data <- lwgeom::st_make_valid(wetland_data)

## remove empty geometries
vegetation_data <- vegetation_data %>%
                   filter(!sf::st_is_empty(sf::st_geometry(vegetation_data)))

## buffer data by zero to attempt to fix invalid geometries
vegetation_data <- sf::st_buffer(vegetation_data, 0)
bua_data <- sf::st_buffer(bua_data, 0)
wetland_data <- sf::st_buffer(wetland_data, 0)

## extract polygons
vegetation_data <- sf::st_collection_extract(vegetation_data, type = "POLYGON")
bua_data <- sf::st_collection_extract(bua_data, type = "POLYGON")
wetland_data <- sf::st_collection_extract(wetland_data, type = "POLYGON")

## remove slivers
vegetation_data <- st_remove_slivers(vegetation_data)
bua_data <- st_remove_slivers(bua_data)
wetland_data <- st_remove_slivers(wetland_data)

# Main processing
## format data other than non-remnant areas
### assign classification codes for classes other than non-remnant
vegetation_data <- left_join(vegetation_data, classification_data,
                             by = c("DBVG5M" = "code"))

### manually assign NA values in atlas name to original
vegetation_data$atlas_name[is.na(vegetation_data$atlas_name)] <-
  vegetation_data$DBVG5M[is.na(vegetation_data$atlas_name)]

### dissolve areas by atlas_name
vegetation_data <- vegetation_data %>%
                   group_by(atlas_name) %>%
                   summarize(order = min(order)) %>%
                   ungroup() %>%
                   lwgeom::st_make_valid() %>%
                   sf::st_collection_extract(type = "POLYGON")

## format non-remnant areas
### extract non-remnant areas
nonrem_data <- vegetation_data %>%
               filter(atlas_name == "non-rem") %>%
               st_explode()

### urban non-remnant areas
urban_nonrem_data <- nonrem_data %>%
                     sf::st_intersection(bua_data %>%
                                         sf::st_geometry() %>%
                                         st_explode()) %>%
                     sf::st_union()
urban_nonrem_data <- classification_data %>%
                     filter(grepl("non-rem", code, fixed = TRUE),
                            grepl("urban", code, fixed = TRUE)) %>%
                     filter(row_number() == 1) %>%
                     sf::st_sf(geometry = urban_nonrem_data) %>%
                     lwgeom::st_make_valid()

### wetland non-remnant areas
wetland_nonrem_data <- wetland_data %>%
                       st_fast_difference(bua_data) %>%
                       lwgeom::st_make_valid() %>%
                       sf::st_collection_extract(type = "POLYGON") %>%
                       st_remove_slivers() %>%
                       sf::st_union() %>%
                       sf::st_intersection(x = nonrem_data) %>%
                       lwgeom::st_make_valid() %>%
                       sf::st_collection_extract(type = "POLYGON") %>%
                       st_remove_slivers() %>%
                       sf::st_union()
wetland_nonrem_data <- classification_data %>%
                       filter(grepl("non-rem", code, fixed = TRUE),
                               grepl("wetland", code, fixed = TRUE)) %>%
                       filter(row_number() == 1) %>%
                       sf::st_sf(geometry = wetland_nonrem_data) %>%
                       lwgeom::st_make_valid()

### other non-remnant areas
other_nonrem_data <- nonrem_data %>%
                     st_fast_difference(urban_nonrem_data) %>%
                     lwgeom::st_make_valid() %>%
                     sf::st_collection_extract(type = "POLYGON") %>%
                     st_fast_difference(wetland_data) %>%
                     lwgeom::st_make_valid() %>%
                     sf::st_collection_extract(type = "POLYGON") %>%
                     sf::st_union()
other_nonrem_data <- classification_data %>%
                     filter(grepl("non-rem", code, fixed = TRUE),
                            grepl("not", code, fixed = TRUE)) %>%
                     filter(row_number() == 1) %>%
                     sf::st_sf(geometry = other_nonrem_data) %>%
                     lwgeom::st_make_valid()

## assemble data set
export_data <- do.call(rbind, 
                       list(vegetation_data %>%
                              filter(atlas_name != "non-rem"),
                            urban_nonrem_data %>% select(-code),
                            wetland_nonrem_data %>% select(-code),
                            other_nonrem_data %>% select(-code))) %>%
               group_by(atlas_name) %>%
               summarize(order = min(order)) %>%
               ungroup()

## order data set
export_data <- export_data %>% arrange(order)

## remove order column
export_data <- export_data %>% select(-order)

# Exports
## save data set
sf::st_write(export_data, "exports/habitat_data.shp")
