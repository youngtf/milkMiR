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
    ..barcode         = NULL,
    ..animal_id       = NULL,
    ..number_in_ws    = NULL,
    ..number_in_job   = NULL,
    ..number_sub      = NULL,
    ..time_sampling   = NULL,         ## sampling time
    ..time_processing = NULL,         ## 1-3d after sampling, used in filenames
    ..time_analysis   = NULL,         ## read from file, real measuring time
    ..pin_number      = NULL,
    ..spectra_matrix  = NULL,
    ..notes           = NULL
  ),
  public = list(
    print = function(...){
      cat("MilkMirSpectra Object of", length(barcode), "animals")
      invisible(self)
    },

    read_data_file = function(file_name, N_SPECTRA = 1060, verbose = TRUE){
      message("Reading data from ", file_name, "\n")
      data_headers = read.csv(file_name, header = F, check.names=F,
                              as.is = T,  na.string = "", nrows = 20)
      data_headers_noblank =
        data_headers[apply(data_headers, 1, function(x) !all(is.na(x))),]

      if (nrow(data_headers) != nrow(data_headers_noblank)){
        warning("Blank lines removed!")
      }

      # check ID (Barcode) / #inWS / #inJob / #Sub / Time
      if(verbose) cat(" - Reading meta info... \n")
      meta_rows = c("ID","#inWS","#inJob","#Sub","Time of Analysis")
      if_rownames_fit = data_headers_noblank[1:5,1] == meta_rows

      if (any(!if_rownames_fit)){
        warnings("- - something may be wrong on the meta info rows")
      }
      private$..barcode       = as.character(as.integer(data_headers_noblank[1, -1]))
      private$..number_in_ws  = data_headers_noblank[2, -1]
      private$..number_in_job = data_headers_noblank[3, -1]
      private$..number_sub    = data_headers_noblank[4, -1]
      private$..time_analysis = data_headers_noblank[5, -1]

      # check Pin Number row
      if(verbose) cat(" - Reading meta info... \n")
      if ("Pin Number" %in% data_headers_noblank[,1]){
        pin_number_row_index = match("Pin Number", data_headers_noblank[,1])
      } else {
        stop("Cannot found")
      }

      # reading spectra matrix
      if(verbose) cat(" - Reading spectra matrix... \n")
      data_spectra = read.csv(file_name, header = F, check.names=F,
                              skip = pin_number_row_index, row.names = 1,
                              as.is = T,  na.string = "")
      private$..pin_number = rownames(data_spectra)[1:N_SPECTRA]
      if (any(private$..pin_number != 240:1299)){
        stop("Invalid pin number")
      }
      private$..spectra_matrix = as.matrix(data_spectra[1:N_SPECTRA,])

      # reading extra notes
      if(verbose) cat(" - Reading notes... \n")
      private$..notes = data_spectra[(N_SPECTRA+1):nrow(data_spectra),]
      private$..notes[is.na(private$..notes)] = ""
    },
    write_data_file = function(use_animal_id = TRUE, file_name){
      if (use_animal_id){
        if (is.null(private$..animal_id)){
          stop("No animal id available")
        } else {
          ID = private$..animal_id
        }
      } else {
        ID = private$..barcode
      }

      output_file = rbind("#inWS"            = private$..number_in_ws,
                          "#inJob"           = private$..number_in_job,
                          "#Sub"             = private$..number_sub,
                          "Time of Analysis" = private$..time_analysis,
                          "Pin Number"       = "",
                          private$..spectra_matrix,
                          private$..notes)
      rownames_vec = c("#inWS","#inJob", "#Sub",
                       "Time of Analysis", "Pin Number",
                       rownames(private$..spectra_matrix),
                       rownames(private$..notes))
      output_file = cbind(rownames_vec,output_file)
      colnames(output_file) = c("ID",ID)
      write.csv(output_file, file_name, row.names = F, quote = F, eol = "\r\n")
      message("Data have been writen into ", file_name, "\n")
    },
    add_animal_id = function(id_matching){
      # ID-MERGE ================================================
      idx.id    = match(private$..barcode,id_matching$BCD)
      private$..animal_id = id_matching$CID[idx.id]

      cat(sum(!is.na(private$..animal_id)), "cow ID has been imported. \n")
      if (sum(is.na(private$..animal_id)) > 0){
        cat(sum(is.na(private$..animal_id)), "BCD were not found in the matching info. \n")
      }
      # =========================================================
    },
    check_cow_id = function(valid_cow_id){
      if_valid = private$..animal_id %in% valid_cow_id
      private$..animal_id[!if_valid] = "[INVALID]"
      cat("Found", sum(!if_valid), "invalid cow ID. Labelled as [INVALID]. \n")
    },
    remove_invalid_record = function(){
      if_invalid = private$..animal_id == "[INVALID]"
      private$..barcode        = private$..barcode[!if_invalid]
      private$..animal_id      = private$..animal_id[!if_invalid]
      private$..number_in_ws   = private$..number_in_ws[!if_invalid]
      private$..number_in_job  = private$..number_in_job[!if_invalid]
      private$..number_sub     = private$..number_sub[!if_invalid]
      private$..time_analysis  = private$..time_analysis[!if_invalid]
      private$..spectra_matrix = private$..spectra_matrix[,!if_invalid]
      private$..notes          = private$..notes[,!if_invalid]
      cat(sum(if_invalid), "records has been removed")
    }
  ),
  active = list(
    get_barcode         = function(){
      private$..barcode
    },
    get_animal_id       = function(){
      private$..animal_id
    },
    get_number_in_ws    = function(){
      private$..number_in_ws
    },
    get_number_in_job   = function(){
      private$..number_in_job
    },
    get_number_sub      = function(){
      private$..number_sub
    },
    get_time_sampling   = function(){
      private$..time_sampling
    },
    get_time_processing = function(){
      private$..time_processing
    },
    get_time_analysis   = function(){
      private$..time_analysis
    },
    get_pin_number      = function(){
      private$..pin_number
    },
    get_spectra_matrix  = function(){
      private$..spectra_matrix
    },
    get_notes           = function(){
      private$..notes
    }
  )
)

# Make a new object
# test_mir_file <- milk_mir_spectra_factory$new()
# test_mir_file$read_data_file(file_name = "sample_data/Spectra_170426PM.csv")
# test_mir_file$view_barcode
# test_mir_file$write_data_file(F, "sample_data/test_spectra_out.csv")
# test_mir_file$add_animal_id(data.frame(CID = c(1001:1015, 1020:1035),
#                                        BCD   = c(   1:15,     20:35)))
# test_mir_file$check_cow_id(as.character(1001:1020))
# test_mir_file$write_data_file(T, "sample_data/test_spectra_out_2.csv")
# test_mir_file$remove_invalid_record()
# test_mir_file$write_data_file(T, "sample_data/test_spectra_out_3.csv")