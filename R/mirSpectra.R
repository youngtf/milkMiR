# -----------------------------------------------------------------------------
# R FILE             --Tianfu Yang
# Type:              Functions
# Subtype/Project:   R/milkMiR
# Descriptions:      R6 class for MIR spectra data
# -----------------------------------------------------------------------------
# Contents:
# -----------------------------------------------------------------------------
# To-do
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# R6 CLASS: MilkMirSpectra
#' Generate a MilkMirSpectra object
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
# -----------------------------------------------------------------------------

# Define microwave_oven_factory
milk_mir_spectra_factory <- R6Class(
  "MilkMirSpectra",
  private = list(
    ..barcode = NULL,
    ..animal_id = NULL,
    ..number_in_ws = NULL,
    ..number_in_job = NULL,
    ..number_sub = NULL,
    ..time_analysis = NULL,
    ..pin_number = NULL,
    ..spectra_matrix = NULL,
    ..notes = NULL
  ),
  public = list(
    read_data_file = function(file_name, N_SPECTRA = 1060){
      cat("Reading data from", file_name, "\n")
      data_headers = read.csv(file_name, header = F, check.names=F,
                              as.is = T,  na.string = "", nrows = 20)
      data_headers_noblank =
        data_headers[apply(data_headers, 1, function(x) !all(is.na(x))),]

      if (nrow(data_headers) != nrow(data_headers_noblank)){
        warning("Blank lines removed!")
      }

      # check ID (Barcode) / #inWS / #inJob / #Sub / Time
      cat(" - Reading meta info... \n")
      meta_rows = c("ID","#inWS","#inJob","#Sub","Time of Analysis")
      if_rownames_fit = data_headers_noblank[1:5,1] == meta_rows

      if (any(!if_rownames_fit)){
        warnings("- - something may be wrong on the meta info rows")
      }
      private$..barcode       = data_headers_noblank[1, -1]
      private$..number_in_ws  = data_headers_noblank[2, -1]
      private$..number_in_job = data_headers_noblank[3, -1]
      private$..number_sub    = data_headers_noblank[4, -1]
      private$..time_analysis = data_headers_noblank[5, -1]

      # check Pin Number row
      cat(" - Reading meta info... \n")
      if ("Pin Number" %in% data_headers_noblank[,1]){
        pin_number_row_index = match("Pin Number", data_headers_noblank[,1])
      } else {
        stop("Cannot found")
      }

      # reading spectra matrix
      cat(" - Reading spectra matrix... \n")
      data_spectra = read.csv(file_name, header = F, check.names=F,
                              skip = pin_number_row_index, row.names = 1,
                              as.is = T,  na.string = "", nrows = N_SPECTRA)
      private$..pin_number = rownames(data_spectra)
      if (any(private$..pin_number != 240:1299)){
        stop("Invalid pin number")
      }
      private$..spectra_matrix = as.matrix(data_spectra)

      # reading extra notes
      cat(" - Reading notes... \n")
      private$..notes = scan(file_name, skip = N_SPECTRA + pin_number_row_index,
                             quiet = T,
                             what = "character", sep = "\n")
    }
  )
  # active = list(
  #
  # )
)
# Make a new object
# a_new_object <- r6_class_factory$new()



