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
# plot test

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
    ..wave_number     = NULL,         ## Wave number = Pin number * 3.858
    ..wave_length     = NULL,         ## Wavelength = 10000 / Wave number μm
    ..spectra_matrix  = NULL,
    ..res_princomp    = NULL,
    ..res_pc_var      = NULL,
    ..SPECTRA_REGIONS = data.frame(
      Filter	     = c("Carbohydrates",   "Total Solids","Carbohydrates Ref",
                      "Citric Acid",	    "Fat C/Urea",  "Protein Reference",
                      "Protein",         "Fat A",       "Fat A reference",
                      "Fat B reference", "Fat B",       "Homogenisation"),
      Abbreviation = c("Carb","TS","Carbr","Ci","FC","Pr",
                       "Ps","FAs","FAr","FBr","FBs","Hom"),
      pin_min	     = c(270, 305, 333, 357, 377, 384,
                       393, 448, 462, 726, 736, 991),
      pin_max	     = c(274, 307, 339, 361, 381, 387,
                       397, 452, 466, 732, 742, 1001),
      wave_min     = c(1041,1176,1284,1377,1454,1481,
                       1515,1727,1781,2799,2838,3821),
      wave_max     = c(1056,1184,1307,1392,1469,1492,
                       1531,1743,1797,2822,2861,3860),
      stringsAsFactors = FALSE)
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
    },

    analysis_pca = function(...){
      private$..res_princomp = princomp(private$..spectra_matrix, ...)

      vars = private$..res_princomp$sdev^2
      vars_prop = vars/sum(vars)
      private$..res_pc_var = data.frame(SD = private$..res_princomp$sdev,
                                        prop_var = vars_prop,
                                        cumu_var = cumsum(vars_prop))
    },

    calculate_wave = function(){
      private$..wave_number = private$..pin_number * 3.858
      private$..wave_length = 10000 / private$..wave_number
    },

    remove_points = function(min_val, max_val, type = c("pin_number", "wave_number")){
      match.arg(type)
      if (type == "pin_number"){
        if_remove = (private$..pin_number >= min_val) & (private$..pin_number <= max_val)
      } else {
        if_remove = (private$..wave_number >= min_val) & (private$..wave_number <= max_val)
      }
      private$..pin_number     = private$..pin_number[!if_remove]
      private$..wave_number    = private$..wave_number[!if_remove]
      private$..wave_length    = private$..wave_length[!if_remove]
      private$..spectra_matrix = private$..spectra_matrix[,!if_remove]
    },

    draw_against_wave_number = function(y_value,
                                        y_lab,
                                        gp0 = ggplot(),
                                        print = TRUE, ...){
      plot_data = data.frame(wave_number = private$..wave_number,
                             y_value = y_value)
      h = gp0 +
        geom_line(data = plot_data, aes(wave_number, y_value), ...) +
        labs(y = y_lab) +
        theme_minimal()

      if (print) print(h)
      return(h)
    },

    draw_anno_regions = function(region = private$..SPECTRA_REGIONS,
                                 gp0 = ggplot(),
                                 print = TRUE
                                 ){
        rect_data = cbind(region, ymin = -Inf, ymax = Inf)
        h = gp0 +
          geom_rect(data = rect_data,
                    aes(xmin = wave_min, xmax = wave_max,
                        ymin = ymin, ymax = ymax))
        if (print) print(h)
        return(h)
    },

    draw_variability = function(print = TRUE, ...){
      coef_var = apply(private$..spectra_matrix, 2, sd) /
        colMeans(private$..spectra_matrix)
      self$draw_against_wave_number(coef_var, "CV", ...)
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
    get_wave_number      = function(){
      private$..wave_number
    },
    get_wave_length      = function(){
      private$..wave_length
    },
    get_spectra_matrix = function(){
      private$..spectra_matrix
    },
    get_res_princomp = function(){
      private$..res_princomp
    },
    get_res_pc_var = function(){
      private$..res_pc_var
    }
  )
)
