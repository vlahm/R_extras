#will check for unzipped files larger than 4gb and either shell out to system unzip, or issue an error if no suitable program can be found
#from https://github.com/ropensci/rdhs
#2023-04-12

unzip_special <- function(zipfile, files = NULL, overwrite = TRUE,
                          junkpaths = FALSE, exdir = ".", unzip = "internal",
                          setTimes = FALSE){
    
    if (max(unzip(zipfile, list = TRUE)$Length) > 4e9) {
        unzip_file <- Sys.which("unzip")
        if (nzchar(unzip_file)) {
            # j system2("unzip", args=c(zfile,files,paste("-d", exdir)),stdout=FALSE)
            unzip(zipfile, files = files, overwrite = overwrite,
                  junkpaths = junkpaths, exdir = exdir,
                  unzip = unzip_file, setTimes = setTimes)
            return(grep(files, list.files(exdir, full.names = TRUE), value = TRUE))
        } else {
            stop (basename(zipfile), " is too large to unzip and a suitable unzip ",
                  "can not be found on your system." )
        }
    } else {
        unzip(zipfile = zipfile, files = files, overwrite = overwrite,
              junkpaths = junkpaths, exdir = exdir,
              unzip = unzip, setTimes = setTimes)
    }
    
}
