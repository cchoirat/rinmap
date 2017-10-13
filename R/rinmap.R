create_input_shapefile <- function(input, shpout = "emis") {
  emis <- read.csv(input)
  proj_inmap <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  spemis <- copy(emis)
  coordinates(spemis) <- ~Longitude + Latitude
  crs(spemis) <- CRS("+proj=longlat +datum=NAD83")
  spemis <- spTransform(spampd, proj_inmap)
  shapefile(spemis, file.path(shpout, "ptegu.shp"))
  return()
}

run_inmap <- function(input, output, save, srmat) {
  # TODO
  return()
}

zip_code_linkage <- function(output, save, srmat, zctashape, crosswalk) {
  # TODO
  return()
}

rinmap <- function(input, output, save, srmat, zctashape, crosswalk) {
  # TODO
  return()
}