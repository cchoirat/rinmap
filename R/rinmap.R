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
#' @param path Folder where the shape is saved
#'             (in a subfolder, as "emis/ptegu.shp")
#' @return This function does not return anything and is used for its side-effects.
create_input_shapefile <- function(input_csv, path) {
  input <- read.csv(input_csv)
  dir.create(path = path, showWarnings = FALSE)
  dir.create(path = file.path(path, "emis"), showWarnings = FALSE)
  unlink(file.path(path, "emis", "*")) # remove all files from 'emis' folder
  proj_inmap <-
    "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
  coordinates(input) <- ~ Longitude + Latitude
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
  file.copy(
    from = system.file("inmap.toml", package = "rinmap"),
    to = path,
    overwrite = TRUE
  )
  file.copy(
    from = system.file("run.sh", package = "rinmap"),
    to = path,
    overwrite = TRUE
  )
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
zip_code_linkage <-
  function(output_shapefile,
           zcta_shapefile,
           crosswalk_csv) {
    zcta <- shapefile(zcta_shapefile)
    result <- shapefile(output_shapefile)
    pm25 <- result[, 6] # subset of PM2.5 only
    zcta2 <- spTransform(zcta, proj4string(pm25))
    o <- over(zcta2, pm25, fn = mean, na.rm = TRUE)
    O <- cbind(zcta@data[["ZCTA5CE10"]], o)
    names(O) <- c("ZCTA", "PM25inMAP")
    crosswalk <- read.csv(crosswalk_csv)
    crosswalk$ZCTA <-
      formatC(crosswalk$ZCTA,
              width = 5,
              format = "d",
              flag = "0") # to merge on zcta ID
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
  link <- zip_code_linkage(
    output_shapefile = "output/ptegu.shp",
    zcta_shapefile = zcta_shapefile,
    crosswalk_csv = crosswalk_csv
  )
  setwd("..")
  return(link)
}

#' Split facility-unit information from a input csv filename
#'
#' \code{add_facility_unit_variable} adds facility-unit information
#' from a input csv filename.
#'
#' @param input_csv Filename of a csv file with ID variable
#' formatted as Facility-Unit
#'
#' @return A data frame that adds Facility and Utility variable to the CSV file
add_facility_unit_variable <- function(input_csv) {
  input <- read.csv(input_csv)
  fu <-
    tidyr::separate(input,
                    ID,
                    into = c("Facility", "Unit"),
                    sep = "-")
  input$Facility <- fu$Facility
  input$Unit <- fu$Unit
  return(input)
}

#' Saves csv files taking one unit at a time
#'
#' \code{create_input_unit_one_by_one} saves one file per unit as csv files
#' to a specified directory.
#'
#' @param input_csv Filename of a csv file to be read and converted to a shapefile format.
#' @param path Folder where the csv files are saved
#'
#' @return This function does not return anything and is used for its side-effects.
create_input_unit_one_by_one <-
  function(input_csv, path = basename(input_csv)) {
    input <- add_facility_unit_variable(input_csv)
    unit <- unique(input$ID)
    dir.create(path = path, showWarnings = FALSE)
    for (i in 1:length(unit)) {
      d <- subset(input, ID == unit[i])
      write.csv(d, file.path(
        path,
        paste(
          basename(input_csv),
          "unit_one_by_one",
          unit[i],
          ".csv",
          sep = "_"
        )
      ))
    }
  }

#' Saves csv files taking one unit at a time
#'
#' \code{create_input_unit_all_but_one} saves one file taking all units but one as csv files
#' to a specified directory.
#'
#' @param input_csv Filename of a csv file to be read and converted to a shapefile format.
#' @param path Folder where the csv files are saved
#'
#' @return This function does not return anything and is used for its side-effects.
create_input_unit_all_but_one <-
  function(input_csv, path = basename(input_csv)) {
    input <- add_facility_unit_variable(input_csv)
    unit <- unique(input$ID)
    dir.create(path = path, showWarnings = FALSE)
    for (i in 1:length(unit)) {
      d <- subset(input, ID != unit[i])
      write.csv(d, file.path(
        path,
        paste(
          basename(input_csv),
          "unit_all_but_one",
          unit[i],
          ".csv",
          sep = "_"
        )
      ))
    }
  }

#' Saves csv files taking one unit at a time
#'
#' \code{create_input_facility_one_by_one} saves one file per facility as csv files
#' to a specified directory.
#'
#' @param input_csv Filename of a csv file to be read and converted to a shapefile format.
#' @param path Folder where the csv files are saved
#'
#' @return This function does not return anything and is used for its side-effects.
create_input_facility_one_by_one <-
  function(input_csv, path = basename(input_csv)) {
    input <- add_facility_unit_variable(input_csv)
    facility <- unique(input$Facility)
    dir.create(path = path, showWarnings = FALSE)
    for (i in 1:length(facility)) {
      d <- subset(input, Facility == facility[i])
      write.csv(d, file.path(
        path,
        paste(
          basename(input_csv),
          "facility_one_by_one",
          facility[i],
          ".csv",
          sep = "_"
        )
      ))
    }
  }

#' Saves csv files taking one unit at a time
#'
#' \code{create_input_facility_all_but_one} saves one file taking all facilities but one as csv files
#' to a specified directory.
#'
#' @param input_csv Filename of a csv file to be read and converted to a shapefile format.
#' @param path Folder where the csv files are saved
#'
#' @return This function does not return anything and is used for its side-effects.
create_input_facility_all_but_one <-
  function(input_csv, path = basename(input_csv)) {
    input <- add_facility_unit_variable(input_csv)
    facility <- unique(input$Facility)
    dir.create(path = path, showWarnings = FALSE)
    for (i in 1:length(facility)) {
      d <- subset(input, Facility != facility[i])
      write.csv(d, file.path(
        path,
        paste(
          basename(input_csv),
          "facility_all_but_one",
          facility[i],
          ".csv",
          sep = "_"
        )
      ))
    }
  }

#' Combine zipcode-linked inMAP output into single data object and link with spatial
#' data for plotting.
#'
#' \code{combine_inmap_output} uses spatial linkages of an inMAP outputs to combine output
#' multiple runs. Employs the sf package to link with spatial data if it is installed.
#'
#' @param path.out Directory that houses InMAP output csv files
#' @param zcta_shapefile A ZCTA shapefile
#' @param pattern A text or regex pattern common to all InMAP output you wish to be joined
#'
#' @return A data frame of average PM2.5 values at the ZIP code level.
combine_inmap_output <- function(path.out, pattern = NULL) {
  #list files for import, read in files
  files = list.files(path.out, full.names = T)
  names.f = gsub(paste(pattern, '_linked_zip.csv', sep = '|'), '', 
                       list.files(path.out, full.names = F))
  names(files) <- names.f
  if (!is.null(pattern))
    files = files[grep(pattern, files)]
  if (length(files) == 0) 
    stop(("No matching files in path.out!"))

  im <-
    lapply(seq_along(files), function(x, f, n) {
      fin <- data.table::fread(f[x])[, V1 := NULL]
      setnames(fin, 'PM25inMAP', n[x])
      fin
    }, files, names(files))
  
  #reduce list to single data table
  im <-
    Reduce(function(...)
      merge(
        ...,
        all = TRUE,
        by = c("ZCTA", "ZIP", "PO_NAME", "STATE", "ZIP_TYPE")
      ), im)
  
  #convert a ZIP code from 3-digit to 5-digit format
  im[, ZIP := formatC(unlist(ZIP),
                      width = 5,
                      format = "d",
                      flag = "0")]
  
  return(im)
}


#' Using zipcode-linked inMAP output from \code{combine_inmap_output}, plot change in InMAP impacts
#' at zip code level compared to base year.
#'
#' \code{combine_inmap_output} uses spatial linkages of an inMAP outputs to combine output
#' multiple runs. Requires the sf, ggplot2, and parallel packages.
#'
#' @param read_inmap_d Directory that houses InMAP output csv files
#' @param legend_lims Legend limits for zip code fill
#' @param path.plot Output directory for plots. If it does not exist, it will be created
#' @param cores Cores available to create plots. Defaults to one
#'
#' @return A list of ggplot objects.
plot_inmap <- function(read_inmap_d,
                       zcta_shapefile = "~/shared_space/ci3_nsaph/software/inmap/zcta/cb_2015_us_zcta510_500k.shp",
                       legend_lims = c(-5, 5),
                       path.plot = 'InMAP_plots',
                       cores = 1) {
  #check if required packages are installed
  if (F %in% (c('sf', 'parallel', 'ggplot2', 'viridis', 'scales') %in% (.packages()))) {
    stop("Required package missing! (need sf,parallel,ggplot2,viridis,scales)")
  }
  
  #create directory if it does not exist
  if (!file.exists(path.plot))
    dir.create(path.plot)
  
  #join with spatial zip data
  zips <- st_read(zcta_shapefile)
  zips$GEOID10 <- as.character(zips$GEOID10)
  im_j <- left_join(im, zips, by = c('ZIP' = 'GEOID10'))
  
  #extract names from combined data table/sf object
  names.f <-
    names(read_inmap_d)[-grep(
      c(
        'ZCTA|ZIP|PO_NAME|STATE|ZIP_TYPE|ZCTA5CE10|AFFGEOID10|ALAND10|AWATER10|geometry'
      ),
      names(read_inmap_d)
    )]
  
  #read in USA and state shapes
  cl <- makeCluster(cores)
  clusterExport(
    cl,
    c(
      'data.table',
      'ggplot',
      'aes',
      'theme_bw',
      'geom_sf',
      'map_data',
      'labs',
      'geom_polygon',
      'coord_sf',
      'scale_color_viridis',
      'scale_fill_viridis',
      'theme',
      'element_text',
      'element_rect',
      'unit',
      'element_blank',
      'ggsave',
      'setnames',
      'squish'
    )
  )
  
  #create plotting object
  ggplotter <- function(x, im_j, n, ll) {
    x1 <- data.table(im_j)[, c('ZIP', n[x], 'geometry'), with = F]
    setnames(x1, n[x], 'PM')
    usa.states <- map_data("state")
    
    gg <- ggplot(data = x1, aes(fill = -PM, color = -PM)) +
      theme_bw() + labs(title = paste('InMAP exposure change - ', n[x], sep =
                                        '')) +
      geom_sf(size = 0.05) +
      geom_polygon(
        data = usa.states,
        aes(x = long, y = lat, group = group),
        fill = NA,
        colour = "grey50",
        size = .25
      ) +
      coord_sf(
        xlim = c(-123, -69),
        ylim = c(24, 50),
        datum = NA
      ) +
      scale_color_viridis(
        discrete = F,
        option = 'D',
        limits = ll,
        oob = squish,
        direction = -1
      ) +
      scale_fill_viridis(
        discrete = F,
        option = 'D',
        limits = ll,
        oob = squish,
        direction = -1
      ) +
      theme(
        legend.position = c(.25, .15),
        axis.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        axis.title = element_text(size = 24),
        legend.text = element_text(size = 14),
        legend.background = element_rect(fill = 'transparent'),
        strip.text = element_text(size = 16),
        legend.key.size = unit(.05, 'npc'),
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.direction = 'horizontal',
        strip.background = element_rect(fill = 'white')
      )
    invisible(ggsave(
      file.path('.', path.plot, paste('plot_', n[x], '.png', sep = '')),
      gg,
      width = 13.5,
      height = 7.79,
      unit = 'in'
    ))
    return(gg)
  }
  out <-
    parLapply(cl, seq_along(names.f), ggplotter, read_inmap_d, names.f, ll =
                legend_lims) #
  stopCluster(cl)
  return(out)
}
