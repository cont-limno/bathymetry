library(pdftools)

fig_list <- paste0("Figure_", 1:5)
in_paths <- paste0("inland_waters/", fig_list, ".pdf")
out_paths <- paste0("inland_waters/", fig_list, ".jpeg")

make_jpeg <- function(in_path, out_path) {
  bitmap <- pdf_render_page(in_path, page = 1, dpi = 600)
  jpeg::writeJPEG(bitmap, out_path, quality = 1)
}

lapply(seq_len(length(fig_list)),
  function(i) make_jpeg(in_paths[i], out_paths[i]))