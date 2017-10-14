create_input_shapefile <- function(input, path) {
  input <- read.csv(input)
  dir.create(path = path, showWarnings = FALSE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
  proj_inmap <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  coordinates(input) <- ~Longitude + Latitude
  crs(input) <- CRS("+proj=longlat +datum=NAD83")
  input <- spTransform(input, proj_inmap)
  shapefile(input, file.path(path, "emis/ptegu.shp"))
}

setup_files_inmap_run <- function(path) {
  file.copy(from = system.file("inmap.toml", package = "rinmap"), to = path, overwrite = TRUE)
  file.copy(from = system.file("run.sh", package = "rinmap"), to = path, overwrite = TRUE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
}

zctashape <- "~/Downloads/cb_2015_us_zcta510_500k/cb_2015_us_zcta510_500k.shp"
shpout <- "output/ptegu.shp"
crosswalk <- "Crosswalk/Zip_to_ZCTA_crosswalk_2015_JSI.csv"

zip_code_linkage <- function(shpout, zctashape, crosswalk) {
  zcta <- shapefile(zctashape)
  result <- shapefile(shpout)
  pm25 <- result[, 6]
  zcta2 <- spTransform(zcta, proj4string(pm25))
  o <- over(zcta2, pm25, fn = mean, na.rm = TRUE)
  O <- cbind(zcta@data[["ZCTA5CE10"]], o)
  names(O) <- c("ZCTA", "PM25inMAP")
  O$ZCTA <- as.numeric(O$ZCTA)
  crosswalk <- read.csv(crosswalk)
  M <- merge(O, crosswalk, by = "ZCTA")
  return(M)
}

rinmap <- function(input, output, zctashape, crosswalk) {
  # TODO
  return()
}
