#' rinmap: An R interface to inMAP.
#'
#' The rinmap package provides an R interface to inMAP
#' (Intervention Model for Air Pollution, \url{http://spatialmodel.com/inmap/}).
#' 
#' @docType package
#' @name rinmap
NULL

#' Convert csv file to inMAP-compatible shapefile
#' 
#' \code{create_input_shapefile} saves an inMAP-compatible shapefile to a specified directory.
#' 
#' @param input_csv Filename of a csv file to be read and converted to a shapefile format.
#' Expected variables are:
#' #' \enumerate{
#'   \item Latitude, Longitude (in NAD83 CRS)
#'   \item SOx, CO2, NOx (in tons per year)
#'   \item Height, Diam(in meters)
#'   \item Velocity (in meters per second)
#'   \item Temp (in Kelvin)
#' }
#' @param path Folder where the shaper is going to be saved
#'             (in a subfolder, as "emis/ptegu.shp")
#' @return This function does not return anything and is used for its side-effects.
create_input_shapefile <- function(input_csv, path) {
  input <- read.csv(input_csv)
  dir.create(path = path, showWarnings = FALSE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
  unlink(file.path(path, "emis", "*")) # remove all files from 'emis' folder
  proj_inmap <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  coordinates(input) <- ~Longitude + Latitude
  crs(input) <- CRS("+proj=longlat +datum=NAD83")
  input <- spTransform(input, proj_inmap)
  shapefile(input, file.path(path, "emis/ptegu.shp"))
}

#' Copy inMAP setup files to the run folder
#' 
#' \code{setup_files_inmap_run} copies \code{inmap.toml} and \code{run.sh} to a specified
#' folder (typically the folder where inMAP is going to be run).
#' 
#' @param path Filepath where \code{inmap.toml} and \code{run.sh} are copied.
#' 
#' @return This function does not return anything and is used for its side-effects.
setup_files_inmap_run <- function(path) {
  file.copy(from = system.file("inmap.toml", package = "rinmap"), to = path, overwrite = TRUE)
  file.copy(from = system.file("run.sh", package = "rinmap"), to = path, overwrite = TRUE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
}

#' Link inMAP output at the ZIP code level.
#' 
#' \code{zip_code_linkage} performs spatial linkage of an inMAP output shapefile
#' at the ZIP code level.
#' 
#' @param output_shapefile An inMAP output shapefile
#' @param zcta_shapefile A ZCTA shapefile
#' @param crosswalk_csv A crosswalk csv file to convert ZCTA to ZIP
#' 
#' @return A data frame of average PM2.5 values at the ZIP code level.
zip_code_linkage <- function(output_shapefile, zcta_shapefile, crosswalk_csv) {
  zcta <- shapefile(zcta_shapefile)
  result <- shapefile(output_shapefile)
  pm25 <- result[, 6] # subset of PM2.5 only
  zcta2 <- spTransform(zcta, proj4string(pm25))
  o <- over(zcta2, pm25, fn = mean, na.rm = TRUE)
  O <- cbind(zcta@data[["ZCTA5CE10"]], o)
  names(O) <- c("ZCTA", "PM25inMAP")
  crosswalk <- read.csv(crosswalk_csv)
  crosswalk$ZCTA <- formatC(crosswalk$ZCTA, width = 5, format ="d", flag = "0") # to merge on zcta ID
  M <- merge(O, crosswalk, by = "ZCTA", all.y = TRUE)
  return(M)
}

#' Call inMAP and save ZIP code level average PM2.5
#' 
#' \code{run_inmap} calls the inMAP executable.  Paths are hard-coded in file \code{inmap.toml}.
#' 
#' @param input_csv Filename of a csv file to be read and converted to a shapefile format.
#' @param zcta_shapefile A ZCTA shapefile (default value is hard-coded cluster path)
#' @param crosswalk_csv A crosswalk csv file to convert ZCTA to ZIP (default value is hard-coded cluster path)
#' 
#' @return A data frame with ZIP code level average PM2.5 values.
run_inmap <- function(input_csv,
                      zcta_shapefile = "~/shared_space/ci3_nsaph/software/inmap/zcta/cb_2015_us_zcta510_500k.shp",
                      crosswalk_csv = "~/shared_space/ci3_nsaph/software/inmap/crosswalk/Zip_to_ZCTA_crosswalk_2015_JSI.csv") {
  create_input_shapefile(input_csv, path = path)
  setup_files_inmap_run(path = path)
  setwd(path)
  system("chmod +x run.sh") # make run.sh executable
  system("./run.sh")
  link <- zip_code_linkage(output_shapefile = "output/ptegu.shp",
                           zcta_shapefile = zcta_shapefile,
                           crosswalk_csv = crosswalk_csv)
  setwd("..")
  return(link)
}
