create_input_shapefile <- function(input, path, shpout = "emis") {
  input <- read.csv(input)
  dir.create(path = path, showWarnings = FALSE)
  dir.create(path = file.path(path, shpout), showWarnings = FALSE)
  proj_inmap <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  coordinates(input) <- ~Longitude + Latitude
  crs(input) <- CRS("+proj=longlat +datum=NAD83")
  input <- spTransform(input, proj_inmap)
  shapefile(input, file.path(path, shpout, "ptegu.shp"))
}

setup_files_inmap_run <- function(path, shpout = "emis") {
  file.copy(from = system.file("inmap.toml", package = "rinmap"), to = path, overwrite = TRUE)
  file.copy(from = system.file("run.sh", package = "rinmap"), to = path, overwrite = TRUE)
  dir.create(path = file.path(path, shpout), showWarnings = FALSE)
}

zip_code_linkage <- function(output, zctashape, crosswalk) {
  # TODO
  return()
}

rinmap <- function(input, output, zctashape, crosswalk) {
  # TODO
  return()
}
