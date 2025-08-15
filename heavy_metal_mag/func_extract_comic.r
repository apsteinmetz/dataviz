extract_comic <- function(filename) {
  out <- tempfile("comic_")
  dir.create(out)
  ext <- tolower(tools::file_ext(filename))
  if (ext == "cbz") {
    unzip(file, exdir = out)
    return(out)
  }
  if (ext == "cbr") {
    sevenzip <- c(Sys.which("7z"))
    sevenzip <- sevenzip[nzchar(sevenzip)][1]
    if (!nzchar(sevenzip)) stop("Install the 'archive' package or 7-Zip ('7z'/'7za'/'7zz').")
    system2(sevenzip, c("x", "-y", 
                        paste0("-o", shQuote(normalizePath(out))), 
                                      shQuote(normalizePath(filename))), stdout = FALSE, stderr = FALSE)
    return(out)
  }
  stop("Unsupported file extension. Expected .cbz or .cbr")
}

file <- "C:\Users\Apste\Documents\R Projects\dataviz_misc\heavy_metal_mag\mags\Heavy Metal v01 01 (1977-04) (Whyld Goose).cbr"

extract_comic(file)
