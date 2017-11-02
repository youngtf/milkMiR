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
# -----------------------------------------------------------------------------
# plot test
# spectra_group = milk_mir_group_factory$new()
#
# test_mir_file = milk_mir_spectra_factory$new()
# test_mir_file$read_data_file(file_name = "tests/testthat/test_spectra.csv")
# test_mir_file$add_animal_id(data.frame(CID = 101:129,BCD = 1:29,
#                                        stringsAsFactors = F))
# test_mir_file$check_cow_id(101:129)
# test_mir_file$remove_invalid_record()
#
# spectra_group$include_spectra(test_mir_file)
# spectra_group$calculate_wave()
# a = spectra_group$draw_variability()
# a = spectra_group$draw_against_wave_number(
#          y_value = -log(spectra_group$get_spectra_matrix[1,]),
#          y_lab = "abs")
# spectra_group$draw_anno_regions(gp0 = a)
# -----------------------------------------------------------------------------

# Define microwave_oven_factory
milk_mir_group_factory <- R6Class(
  "MilkMirGroup",
  private = list(
    ..n_rec           = 0,
    ## with length of sample_number          ==================================
    ..animal_id       = NULL,
    ..time_sampling   = NULL,         ## sampling time
    ..time_processing = NULL,         ## 1-3d after sampling, used in filenames
    ..time_analysis   = NULL,         ## read from file, real measuring time
    ..parity          = NULL,
    ## with length of pin_number             ==================================
    ..pin_number      = NULL,
    ..wave_number     = NULL,         ## Wave number = Pin number * 3.858
    ..wave_length     = NULL,         ## Wavelength = 10000 / Wave number μm
    ## n_animals * n_pin_number              ==================================
    ..spectra_matrix  = NULL,
    ## list                                  ==================================
    ..res_princomp    = NULL,
    ..res_pc_var      = NULL,
    ## df, each with length of sample_number ==================================
    ..pheno_df        = data.frame(),
    ## list                                  ==================================
    ..pheno_list      = list(),
    ## CONSTANTS                             ==================================
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
        stringsAsFactors = FALSE
    ),
    ..REMOVED_REGIONS_PIN = numeric(0),
    ..REMOVED_REGIONS_WN  = numeric(0)
    ## End                                   ==================================
  ),
  public = list(
    print = function(){
      cat("<this is a MilkMirGroup object of ",private$..n_rec, "animals. \n")
    },

    ## Data I/O ================================================================
    include_spectra = function(new_spectra){
      private$..animal_id       = c(private$..animal_id,
                                    new_spectra$get_animal_id)
      private$..time_sampling   = c(private$..time_sampling,
                                    new_spectra$get_time_sampling)
      private$..time_processing = c(private$..time_processing,
                                    new_spectra$get_time_processing)
      private$..time_analysis   = c(private$..time_analysis,
                                    new_spectra$get_time_analysis)
      private$..parity          = c(private$..parity,
                                    new_spectra$get_parity)
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

    add_pheno_col = function(key, data_vecter){
      if(length(data_vecter) != private$..n_rec){
        stop("The length of the vecter is not the same as the number of total records.")
      }
      private$..pheno_df[[key]] = data_vecter
    },

    add_pheno_list = function(key, data){
      private$..pheno_list[[key]] = data
    },

    export_data_frame = function(spectra = c("full","pca"),
                                 top_pcs = NULL,
                                 include_pheno_list = T){
      spectra = match.arg(spectra)
      attr_list = list()
      attr_list$spectra_type = spectra

      ### Spectra data
      if (spectra == "full"){
        data_spectra = private$..spectra_matrix
        colnames(data_spectra) = paste0("PIN_", private$..pin_number)
        attr_list$wave_number = private$..wave_number
        attr_list$pin_number = private$..pin_number
      } else {
        if (is.null(private$..res_princomp)){
          stop("PCA has not been conducted")
        }

        data_spectra = private$..res_princomp$scores
        colnames(data_spectra) = paste0("PC_", 1:length(private$..pin_number))
        attr_list$var_pca = private$..res_pc_var$prop_var

        if(!is.null(top_pcs)){
          if(!is.integer(top_pcs)){
            stop("top_pcs should be an integer")
          }
          data_spectra = data_spectra[,1:top_pcs]
          attr_list$var_pca = attr_list$var_pca[1:top_pcs]
        }
      }

      ### Phenotype data
      data_pheno = data.frame(CID       = private$..animal_id,
                              Samp_time = private$..time_sampling,
                              Proc_time = private$..time_processing,
                              Ana_time  = private$..time_analysis,
                              Parity    = private$..parity,
                              stringsAsFactors = FALSE)

      if (length(private$..pheno_df) > 0){
        data_pheno = cbind(data_pheno, private$..pheno_df)
      }

      ### Pheno list
      if (include_pheno_list)
        attr_list$pheno_list = private$..pheno_list

      if (nrow(data_pheno) != nrow(data_spectra)){
        print(nrow(data_pheno))
        stop(paste("The number of phenotype records is different",
                    "from the number of spectra records."))
      }
      data_all = cbind(data_pheno, data_spectra)
      attributes(data_all) = c(attributes(data_all), attr_list)
      return(data_all)
    },

    ## Data I/O ================================================================

    ## Analysis ================================================================
    analysis_pca = function(...){
      private$..res_princomp = princomp(private$..spectra_matrix, ...)

      vars = private$..res_princomp$sdev^2
      vars_prop = vars/sum(vars)
      private$..res_pc_var = data.frame(SD = private$..res_princomp$sdev,
                                        prop_var = vars_prop,
                                        cumu_var = cumsum(vars_prop))
    },
    ## Analysis ================================================================

    ## Spectra manipulation ====================================================
    calculate_wave = function(){
      private$..wave_number = private$..pin_number * 3.858
      private$..wave_length = 10000 / private$..wave_number
    },

    remove_points = function(min_val, max_val,
                             type = c("pin_number", "wave_number"),
                             copy_checked = F){
      if (!copy_checked){
        stop("This function will remove data. Please check the following actions:",
             "\n 1) Get a deep copy first.",
             "\n 2) set the copy_checked option to TRUE.")
      }
      match.arg(type)
      if (type == "pin_number"){
        if_remove = (private$..pin_number >= min_val) &
          (private$..pin_number <= max_val)
      } else {
        if_remove = (private$..wave_number >= min_val) &
          (private$..wave_number <= max_val)
      }
      
      private$..REMOVED_REGIONS_PIN = unique(c(private$..REMOVED_REGIONS_PIN, 
                                               private$..pin_number[if_remove]))
      private$..REMOVED_REGIONS_WN  = unique(c(private$..REMOVED_REGIONS_PIN, 
                                               private$..wave_number[if_remove]))
      private$..pin_number     = private$..pin_number[!if_remove]
      private$..wave_number    = private$..wave_number[!if_remove]
      private$..wave_length    = private$..wave_length[!if_remove]
      private$..spectra_matrix = private$..spectra_matrix[,!if_remove]
    },
    
    remove_samples = function(index_remove, copy_checked = F){
      if (!copy_checked){
        stop("This function will remove data. Please check the following actions:",
             "\n 1) Get a deep copy first.",
             "\n 2) set the copy_checked option to TRUE.")
      }
      ## check index
      if (any(index_remove) > private$..n_rec){
        stop("Index out of bound.")
      }
      private$..animal_id       = private$..animal_id[-index_remove]
      private$..time_sampling   = private$..time_sampling[-index_remove]
      private$..time_processing = private$..time_processing[-index_remove]
      private$..time_analysis   = private$..time_analysis[-index_remove]
      private$..parity          = private$..parity[-index_remove]
      private$..n_rec           = length(private$..animal_id)
      
      private$..spectra_matrix  = private$..spectra_matrix[-index_remove,]
      
      private$..res_princomp    = NULL
      private$..res_pc_var      = NULL
      private$..pheno_df        = private$..pheno_df[-index_remove,]
    },
    ## Spectra manipulation ====================================================

    ## Visualization ===========================================================
    draw_against_wave_number = function(y_value,
                                        y_lab,
                                        gp0 = ggplot(),
                                        print = TRUE, 
                                        pin_lim = c(240, 1299),
                                        wn_lim = pin_lim * 3.858,
                                        ...){
      plot_data = data.frame(pin_number  = private$..pin_number,
                             wave_number = private$..wave_number,
                             y_value = y_value)
      
      h = gp0
      if(length(private$..REMOVED_REGIONS_WN) == 0){
        h = h + geom_line(data = plot_data, aes(wave_number, y_value), ...)
      } else {
        plot_data_sorted = plot_data[order(plot_data$pin_number),]
        pin_match = (pin_lim[1]:pin_lim[2]) %in% plot_data_sorted$pin_number
        pin_match_recode = pin_match * 2 - 1
        break_points = which(c(pin_match_recode,0) * c(0, pin_match_recode) == -1)
        class_points = findInterval(1:length(pin_match_recode), 
                                    break_points)[pin_match]
        plot_data_sorted$plot_intervals = class_points
        ### ==========================================================
        # pin_1 = 2:80
        # pin_2 = c(2:16, 30:50, 65:75)
        # pin_match = (pin_1 %in% pin_2)
        # pin_match_recode = pin_match * 2 - 1
        # break_points = which(c(pin_match_recode,0) * c(0, pin_match_recode) == -1)
        # class_points = findInterval(1:length(pin_match_recode), 
        #                             break_points)[pin_1 %in% pin_2]
        # data.frame(pin_2, class_points)
        ### ==========================================================
        for (i in unique(plot_data_sorted$plot_intervals)){
          temp_data = plot_data_sorted[plot_data_sorted$plot_intervals == i,]
          h = h + geom_line(data = temp_data, aes(wave_number, y_value), ...)
        }
      }
      h = h + labs(y = y_lab) + theme_minimal()

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
    ## Visualization ===========================================================
  ),
  active = list(
    get_n_rec            = function(){
      private$..n_rec
    },
    get_animal_id        = function(){
      private$..animal_id
    },
    get_time_sampling    = function(){
      private$..time_sampling
    },
    get_time_processing  = function(){
      private$..time_processing
    },
    get_time_analysis    = function(){
      private$..time_analysis
    },
    get_parity           = function(){
      private$..parity
    },
    get_pin_number       = function(){
      private$..pin_number
    },
    get_wave_number      = function(){
      private$..wave_number
    },
    get_wave_length      = function(){
      private$..wave_length
    },
    get_spectra_matrix   = function(){
      private$..spectra_matrix
    },
    get_res_princomp     = function(){
      private$..res_princomp
    },
    get_res_pc_var       = function(){
      private$..res_pc_var
    },
    get_pheno_df         = function(){
      private$..pheno_df
    },
    get_pheno_list       = function(){
      private$..pheno_list
    },
    get_removed_region   = function(){
      list(private$..REMOVED_REGIONS_PIN, 
           private$..REMOVED_REGIONS_WN)
    }
  )
)
