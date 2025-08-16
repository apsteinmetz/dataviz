
extract_comic <- function(filename) {
  cat("Starting extraction for:", filename, "\n")
  
  cache_dir <- file.path(tempdir(), "comic_cache")
  if (!dir.exists(cache_dir)) {
    cat("Creating cache directory:", cache_dir, "\n")
    dir.create(cache_dir, recursive = TRUE)
  }
  
  cached_path <- file.path(cache_dir, tools::file_path_sans_ext(filename))
  if (dir.exists(cached_path)) {
    cat("Found cached version at:", cached_path, "\n")
    return(cached_path)
  }
  
  ext <- tolower(tools::file_ext(filename))
  temp_file <- tempfile("comic_", fileext = paste0(".", ext))
  cat("Using temporary file:", temp_file, "\n")
  
  matching_file <- hm_issue_filenames %>% filter(file_name == filename)
  if (nrow(matching_file) == 0) stop("File not found in Google Drive listing")
  
  cat("Downloading from Google Drive ID:", matching_file$drive_id[1], "\n")
  drive_download(as_id(matching_file$drive_id[1]), path = temp_file)
  
  out <- cached_path
  cat("Creating extraction directory:", out, "\n")
  dir.create(out, recursive = TRUE)
  
  if (ext == "cbz") {
    cat("Extracting CBZ file...\n")
    unzip(temp_file, exdir = out)
  } else if (ext == "cbr") {
    cat("Extracting CBR file...\n")
    archive::archive_extract(temp_file, dir = out)
  } else {
    stop("Unsupported file extension. Expected .cbz or .cbr")
  }
  
  unlink(temp_file)
  
  # Remove non-image files and nested directories
  all_files <- list.files(out, recursive = TRUE, full.names = TRUE)
  image_exts <- c(".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp")
  image_files <- all_files[tolower(tools::file_ext(all_files)) %in% gsub("\\.", "", image_exts)]
  
  # Move images to root directory and rename sequentially
  if (length(image_files) > 0) {
    sorted_images <- sort(image_files)
    for (i in seq_along(sorted_images)) {
      new_name <- file.path(out, sprintf("%03d%s", i, tools::file_ext(sorted_images[i])))
      file.rename(sorted_images[i], new_name)
    }
  }
  
  # Clean up subdirectories
  subdirs <- list.dirs(out, recursive = FALSE)
  if (length(subdirs) > 0) unlink(subdirs, recursive = TRUE)
  
  cat("Extraction complete. Files available at:", out, "\n")
  return(out)
}
