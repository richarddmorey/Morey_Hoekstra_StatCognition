library(here)

pkgdown::build_site( here::here() )

rmarkdown::render( input = here("README.Rmd"),
                   output_file = here("README.md"),
                   output_format = "md_document")

which_files = c("MoreyHoekstra2019/inst/doc/manuscript.pdf",
                "MoreyHoekstra2019/inst/doc/supplementA.html",
                "MoreyHoekstra2019/inst/doc/supplementB.html",
                "MoreyHoekstra2019/inst/reprohack/reprohack2019_feedback_letter.pdf")
which_dirs = c("MoreyHoekstra2019/inst/pkg_html")

tmpdir = tempdir()

pkg_file = devtools::build( pkg = here::here(),
                            path = tmpdir,
                            quiet = TRUE )

untar( pkg_file,
       files = c( which_files, which_dirs ),
       exdir = tmpdir )

file_paths = paste( tmpdir, which_files, sep = "/" )
dir_paths = paste( tmpdir, which_dirs, sep = "/" )

fs::dir_create( here("docs/extras") )
file.copy( c(file_paths, dir(dir_paths, full.names = TRUE) ),
           here("docs/extras"),
           recursive = TRUE )

