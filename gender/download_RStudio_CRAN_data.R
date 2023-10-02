# This is a modification of the function "download_RStudio_CRAN_data"
# from the installr package by Tal Galili, et. al.
# found here: https://cran.r-project.org/web/packages/installr/index.html

download_RStudio_CRAN_data <- function (START = as.Date(Sys.time()) - 5, 
                                        END = as.Date(Sys.time()),
                                        log_folder = tempdir(),
                                        trunc_END_date_to_today = TRUE,
                                        override = FALSE,
                                        message = TRUE,
                                        subsample=NULL,
                                        ...) 
{
  START <- as.Date(START)
  END <- as.Date(END)
  if ((END > as.Date(Sys.time()) + 1) & trunc_END_date_to_today) 
    END <- as.Date(Sys.time()) + 1
  all_days <- seq(START, END, by = "day")
  # implement ability to take a subsample of the day range.
  # ignore subsample if non-numeric or greater than length of START/STOP range.
  if (is.numeric(subsample)){
    all_days <- sample(all_days,min(length(all_days),subsample))
  }
  year <- as.POSIXlt(all_days)$year + 1900
  urls <- paste0("http://cran-logs.rstudio.com/", year, "/", 
                 all_days, ".csv.gz")
  missing_days <- setdiff(all_days, tools::file_path_sans_ext(dir(), 
                                                              TRUE))
  avilable_files <- list.files(log_folder)
  for (i in seq_along(urls)) {
    zip_filename <- file.path(file.name.from.url(urls[i]))
    zip_filename_path <- file.path(log_folder, zip_filename)
    if (zip_filename %in% avilable_files & !override) {
      if (message) 
        message("The file: ", zip_filename, " is already available in the folder - skipping it")
    }
    else {
      tryCatch(download.file(urls[i], destfile = zip_filename_path, 
                             mode = "wb"), error = function(e) e)
    }
  }
  if (message) 
    message("Files were downloaded to: ", log_folder)
  return(invisible(log_folder))
}
