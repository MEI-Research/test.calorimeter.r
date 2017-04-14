#' @export
human_report <- function(data, params, ...) {
  human_template <- system.file("Rnw", "human.rnw",
                                package = params$package,
                                lib.loc = NULL)
  # "/usr/lib/opencpu/library"
  texfile <- knitr::knit(human_template)
  tools::texi2pdf(texfile)

  pdf_filename <- paste0(getwd(), "/human.pdf")
  base64enc::base64encode(pdf_filename)
}

## expect as input 
 ## {   datasets : { dataset1 : [],
 ##                  dataset2 : []
 ##              },
 ##     params : {
 ##               settings : {},
 ##               values   : {},
 ##               epochs   : {},
 ##               filters  : {}
 ##              },
 ##     files : {'human' : <<BASE64ENCODEDPDF>>}
 ## }


