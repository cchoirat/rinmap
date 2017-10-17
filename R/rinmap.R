create_input_shapefile <- function(input_csv, path) {
  input <- read.csv(input_csv)
  dir.create(path = path, showWarnings = FALSE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
  proj_inmap <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  coordinates(input) <- ~Longitude + Latitude
  crs(input) <- CRS("+proj=longlat +datum=NAD83")
  input <- spTransform(input, proj_inmap)
  if(file.exists(file.path(path, "emis/ptegu.shp"))) {
    print(paste("Removing shapefile:",list))
    list <- list.files(path=file.path(path,"emis"),pattern="^ptegu.",full.names=T)
    invisible(file.remove(list))
  }
  print(paste("Creating shapefiles"))
  shapefile(input, file.path(path, "emis/ptegu.shp"))
}

setup_files_inmap_run <- function(path) {
  file.copy(from = system.file("inmap.toml", package = "rinmap"), to = path, overwrite = TRUE)
  file.copy(from = system.file("run.sh", package = "rinmap"), to = path, overwrite = TRUE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
}

zip_code_linkage <- function(output_shapefile, zcta_shapefile, crosswalk_csv) {
  zcta <- shapefile(zcta_shapefile)
  result <- shapefile(output_shapefile)
  pm25 <- result[, 6] # subset of PM2.5 only
  zcta2 <- spTransform(zcta, proj4string(pm25))
  o <- over(zcta2, pm25, fn = mean, na.rm = TRUE)
  O <- cbind(zcta@data[["ZCTA5CE10"]], o)
  names(O) <- c("ZCTA", "PM25inMAP")
  crosswalk <- read.csv(crosswalk_csv)
  crosswalk$ZCTA <- formatC(crosswalk$ZCTA,width=5,format='d',flag='0') # to merge on zcta ID
  M <- merge(O, crosswalk, by = "ZCTA", all.y = TRUE)
  return(M)
}

rinmap <- function(input_csv,
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
