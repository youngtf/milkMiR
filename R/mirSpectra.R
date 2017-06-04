# -----------------------------------------------------------------------------
# R FILE             --Tianfu Yang
# Type:              Functions
# Subtype/Project:   R/milkMiR
# Descriptions:      R6 class for MIR spectra data
# -----------------------------------------------------------------------------
# Contents:
# -----------------------------------------------------------------------------
# To-do
# 1. method to update time_sampling and time_processing [CHECK]
# 2. POSIX object for time data
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# R6 CLASS: MilkMirSpectra
#' Generate a MilkMirSpectra object
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
# -----------------------------------------------------------------------------

# Define milk_mir_spectra_factory
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
    ..notes           = NULL,
    ..n_original_rec  = NULL,
    ..n_invalid_bcd   = NULL,         ## after check, how many bcd is invalid
    ..n_invalid_cid   = NULL,         ## after check, how many cid is invalid
    ..n_removed_rec   = NULL          ## how many rec has been removed
  ),

  public = list(
    print = function(...){
      cat("<MilkMirSpectra Object of", length(private$..barcode), "animals>")
      invisible(self)
    },

    read_data_file = function(file_name, N_SPECTRA = 1060, verbose = TRUE){
      message("Reading data from ", file_name)
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
      private$..n_original_rec = ncol(private$..spectra_matrix)

      # reading extra notes
      if(verbose) cat(" - Reading notes... \n")
      private$..notes = data_spectra[(N_SPECTRA+1):nrow(data_spectra),]
      private$..notes[is.na(private$..notes)] = ""
    },

    write_data_file = function(file_name, use_animal_id = TRUE){
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
      message("Data have been writen into ", file_name)
    },

    add_animal_id = function(id_matching, verbose = TRUE){
      idx.id    = match(private$..barcode,id_matching$BCD)
      private$..animal_id = id_matching$CID[idx.id]
      if (verbose){
        cat(" - ", sum(!is.na(private$..animal_id)),
            "cow ID has been imported. \n")
        if (sum(is.na(private$..animal_id)) > 0){
          cat(" - ", sum(is.na(private$..animal_id)),
              "BCD were not found in the matching info. \n")
        }
      }
      private$..n_invalid_bcd = sum(is.na(private$..animal_id))
    },

    add_sampling_time = function(sampling_time){
      if (length(sampling_time) == 1){
        private$..time_sampling = rep(sampling_time, length(private$..barcode))
      } else {
        if (length(sampling_time) != length(private$..barcode)){
          stop("The length of the input data does not match the data")
        }
        private$..time_sampling = sampling_time
      }
    },

    add_process_time = function(process_time){
      if (length(process_time) == 1){
        private$..time_processing = rep(process_time, length(private$..barcode))
      } else {
        if (length(process_time) != length(private$..barcode)){
          stop("The length of the input data does not match the data")
        }
        private$..time_processing = process_time
      }
    },

    check_cow_id = function(valid_cow_id, verbose = TRUE){
      if_valid = private$..animal_id %in% valid_cow_id
      private$..animal_id[!if_valid] = "[INVALID]"
      if(verbose){
        cat(" - ", "Found", sum(!if_valid),
            "invalid cow ID. Labelled as [INVALID]. \n")
      }
      private$..n_invalid_cid = sum(!if_valid)
    },

    remove_invalid_record = function(verbose = TRUE){
      if_invalid = private$..animal_id == "[INVALID]"
      private$..barcode        = private$..barcode[!if_invalid]
      private$..animal_id      = private$..animal_id[!if_invalid]
      private$..number_in_ws   = private$..number_in_ws[!if_invalid]
      private$..number_in_job  = private$..number_in_job[!if_invalid]
      private$..number_sub     = private$..number_sub[!if_invalid]
      private$..time_analysis  = private$..time_analysis[!if_invalid]
      private$..spectra_matrix = private$..spectra_matrix[,!if_invalid]
      private$..notes          = private$..notes[,!if_invalid]
      if(verbose){
        cat(" - ", sum(if_invalid), "records has been removed \n")
      }
      private$..n_removed_rec = sum(if_invalid)
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
    },
    get_n_original_rec  = function(){
      private$..n_original_rec
    },
    get_n_invalid_bcd   = function(){
      private$..n_invalid_bcd
    },
    get_n_invalid_cid   = function(){
      private$..n_invalid_cid
    },
    get_n_removed_rec   = function(){
      private$..n_removed_rec
    }
  )
)

# Make a new object
# test_mir_file <- milk_mir_spectra_factory$new()
# test_mir_file$read_data_file(file_name = "sample_data/Spectra_170426PM.csv")
# test_mir_file$add_animal_id(data.frame(CID = c(1001:1015, 1020:1035),
#                                        BCD   = c(   1:15,     20:35)))
# test_mir_file$check_cow_id(as.character(1001:1020))
# test_mir_file$remove_invalid_record()
# test_mir_file$write_data_file("sample_data/test_spectra_out_3.csv", T)