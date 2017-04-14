burn_report <- function(data, params, ...) {
    burn_template <- system.file("Rnw", "burn.rnw",
                                     package = params$package,
                                     lib.loc = NULL)
    # "/usr/lib/opencpu/library"
    texfile <- knitr::knit(burn_template)
    tools::texi2pdf(texfile)

    pdf_filename <- paste0(getwd(), "/burn.pdf")
    base64enc::base64encode(pdf_filename)
}
