#!/usr/bin/env Rscript
suppressWarnings(suppressMessages({
  library(rmarkdown)
}))

input <- "summary_report.Rmd"
out_pdf <- file.path("outputs", "summary_report.pdf")
out_html <- file.path("outputs", "summary_report.html")

ok <- FALSE
try({
  render(input, output_format = "pdf_document", output_file = out_pdf, quiet = FALSE)
  ok <- TRUE
}, silent = TRUE)

if (!ok) {
  message("PDF render failed or LaTeX missing; falling back to HTML...")
  render(input, output_format = "html_document", output_file = out_html, quiet = FALSE)
}

cat(sprintf("Written %s%s\n", if (ok) out_pdf else out_html, if (ok) " (PDF)" else " (HTML)"))
