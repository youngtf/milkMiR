# -----------------------------------------------------------------------------
# R FILE             --Tianfu Yang
# Type:              Functions
# Subtype/Project:   R/milkMiR
# Descriptions:      functions for manipulating multiple MilkMirSpectra objects
# -----------------------------------------------------------------------------
# Contents:
# -----------------------------------------------------------------------------
# To-do
# -----------------------------------------------------------------------------
# Pre-load
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# R6 CLASS:     # the function name
#' Generate a MilkMirGroup object
#' @docType class
#' @export
#' @format An \code{\link{R6Class}} generator object
# -----------------------------------------------------------------------------

# Define microwave_oven_factory
milk_mir_group_factory <- R6Class(
  "MilkMirGroup",
  private = list(
    ..n_rec           = 0,
    ..animal_id       = NULL,
    ..time_sampling   = NULL,         ## sampling time
    ..time_processing = NULL,         ## 1-3d after sampling, used in filenames
    ..time_analysis   = NULL,         ## read from file, real measuring time
    ..pin_number      = NULL,
    ..spectra_matrix  = NULL
  ),
  public = list(
    print = function(){
      cat("<this is a MilkMirGroup object of ",private$..n_rec, "animals. \n")
    },
    include_spectra = function(new_spectra){
      private$..animal_id       = c(private$..animal_id,
                                    new_spectra$get_animal_id)
      private$..time_sampling   = c(private$..time_sampling,
                                    new_spectra$get_time_sampling)
      private$..time_processing = c(private$..time_processing,
                                    new_spectra$get_time_processing)
      private$..time_analysis   = c(private$..time_analysis,
                                    new_spectra$get_time_analysis)

      ## pin number
      if (is.null(private$..pin_number)){
        private$..pin_number = new_spectra$get_pin_number
      }
      if (any(private$..pin_number != new_spectra$get_pin_number)){
        stop("The pin number of the new spectra data does not match others.")
      }

      ## spectra matrix
      if (is.null(private$..spectra_matrix)){
        private$..spectra_matrix = t(new_spectra$get_spectra_matrix)
        private$..n_rec = ncol(new_spectra$get_spectra_matrix)
      } else {
        new_n_rec = private$..n_rec + ncol(new_spectra$get_spectra_matrix)
        new_matrix = matrix(0.0,new_n_rec, length(private$..pin_number))
        new_matrix[1:private$..n_rec, ] = private$..spectra_matrix
        new_matrix[(private$..n_rec+1):new_n_rec, ] = t(new_spectra$get_spectra_matrix)
        private$..spectra_matrix = new_matrix
        private$..n_rec = new_n_rec
      }

    }
  ),
  active = list(
    get_n_rec = function(){
      private$..n_rec
    },
    get_animal_id = function(){
      private$..animal_id
    },
    get_time_sampling = function(){
      private$..time_sampling
    },
    get_time_processing = function(){
      private$..time_processing
    },
    get_time_analysis = function(){
      private$..time_analysis
    },
    get_pin_number = function(){
      private$..pin_number
    },
    get_spectra_matrix = function(){
      private$..spectra_matrix
    }
  )
)
# Make a new object
# a_new_object <- r6_class_factory$new()
