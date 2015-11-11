knitPDF <- function(file, times=2, cmd='pdflatex', checkSessionInfo=TRUE) {
    wd <- getwd()
    dir <- dirname(file)
    if(wd != dir) {
        on.exit(setwd(wd))
        setwd(dir)
    }
    file <- basename(file)
    require(tools)
    ext <- tools::file_ext(file)
    if(nchar(ext) == 0) {
        file <- list.files(pattern=sprintf("^%s\\.[Rr][Nn][Ww]$", file))
        if(length(file) > 1) {
            stop(sprintf('Ambiguous file name, matches include [%s]',
                         paste(file, collapse=', ')))
        }
        ext <- tools::file_ext(file)
    }
    nam <- sub(sprintf("%s$", ext), '', file, ignore.case=TRUE)
    noweb <- paste0(nam, ext)
    # modify noweb file
    if(checkSessionInfo) {
        dat <- scan(noweb, '', sep="\n", quiet=TRUE)
        if(!any(grepl("sessionInfo", dat))) {
            end <- grep("\\end{document}", dat, fixed=TRUE)
            if(!length(end)) {
                stop('\\end{document} not found')
            }
            x1 <- dat[seq(1, end-1)]
            x2 <- dat[seq(end, length(dat))]
            dat <- c(x1, "<<sessionInfo>>=", "sessionInfo()", "@", x2)
            cat(dat, file=noweb, sep="\n")
        }
    }
    # generate tex file
    require(knitr)
    knitr::knit(noweb, quiet=TRUE)
    tex <- paste0(nam, 'tex')
    pdf <- paste0(nam, 'pdf')
    run <- sprintf("%s %s", cmd, tex)
    replicate(times, system(run, ignore.stdout=TRUE, ignore.stderr=TRUE))
    opt <- list.files(pattern=sprintf("^%s", nam))
    del <- setdiff(opt, c(noweb, pdf))
    ans <- readline(sprintf('Removing files:\n%s\nProceed (y/N)? ', paste(del, collapse='\n')))
    if(grepl('^y', ans, ignore.case=TRUE)) {
        unlink(del)
    }
    print(pdf)
}
