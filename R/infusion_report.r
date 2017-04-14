#' @export
infusion_report <- function(data, params, ...) {
    infusion_template <- system.file("Rnw", "infusion.rnw",
                                     package = params$package,
                                     lib.loc = NULL)
    texfile <- knitr::knit(infusion_template)
    tools::texi2pdf(texfile)

    pdf_filename <- paste0(getwd(), "/infusion.pdf")
    base64enc::base64encode(pdf_filename)
}
