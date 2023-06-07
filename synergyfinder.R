

library(synergyfinder)
library(tidyverse)
library(patchwork)
library(magrittr)


## func
run_synergyfinder <- function(data,
                              type=c('viability', 'inhibition'),
                              # replicates=TRUE, # no longer use
                              save_file='result_synergyfinder.xlsx',
                              save_dir='.') {
  
  # if(!all(c('block_id','drug_row','drug_col','conc_r','conc_c','response','conc_r_unit','conc_c_unit') %in% colnames(data))) {
  #   stop("'block_id','drug_row','drug_col','conc_r','conc_c','response','conc_r_unit','conc_c_unit', some columns not found!")
  # }
  
  type <- type[1]
  if(!(type %in% c('viability', 'inhibition'))) {
    stop("type must in 'viability' or 'inhibition'")
  }
  
  # remove NA rows
  data <- drop_na(data, block_id)
  
  # reshape
  data <- ReshapeData(
    data = data,
    data_type = type,
    impute = TRUE,
    impute_method = NULL,
    noise = TRUE,
    seed = 1)
  # head(data)
  
  # analyze
  res <- CalculateSynergy(
    data = data,
    method = c("ZIP", "HSA", "Bliss", "Loewe"),
    Emin = NA,
    Emax = NA,
    correct_baseline = "non") %>%
    # calculate sensitivity
    CalculateSensitivity()
  # head(res)
  
  # save result
  if(!is.null(save_file)) {
    writexl::write_xlsx(res, file.path(save_dir, save_file))
    
    # plot dose response
    suppressWarnings(
      PlotDoseResponse(
        data = res,
        block_ids = unique(res$drug_pairs$block_id),
        drugs = c(1,2),
        save_file = TRUE,
        file_type = "pdf")
    )
    
    for(block in unique(res$drug_pairs$block_id)) {
      # plot synergy
      p1 <- map(c("ZIP", "HSA", "Bliss", "Loewe"), 
                ~ PlotSynergy(
                  data = res,
                  type = "2D",
                  method = .x,
                  block_ids = block,
                  drugs = c(1,2),
                  dynamic = F,
                  save_file = F))
      
      drug1 <- res$drug_pairs %>% filter(block_id==block) %>% pull(drug1) %>% .[1]
      drug2 <- res$drug_pairs %>% filter(block_id==block) %>% pull(drug2) %>% .[1]
      pdf(str_glue('{save_dir}/{drug1}_{drug2}_Synergy_{block}.pdf'), width=6, height=5)
      print(p1)
      dev.off()
      
      # plot 2D surface
      p2 <- map(c("ZIP_synergy", "HSA_synergy", "Bliss_synergy", "Loewe_synergy"), 
                ~ Plot2DrugSurface(
                  data = res, 
                  plot_block = block, 
                  plot_value = .x, 
                  summary_statistic = 'mean', 
                  dynamic = F, 
                  interpolate_len = 3))
      
      drug1 <- res$drug_pairs %>% filter(block_id==block) %>% pull(drug1) %>% .[1]
      drug2 <- res$drug_pairs %>% filter(block_id==block) %>% pull(drug2) %>% .[1]
      pdf(str_glue('{save_dir}/{drug1}_{drug2}_Surface_{block}.pdf'), width=6, height=6)
      print(p2)
      dev.off()
      
      # synergy barometer
      ic50_1 <- res$drug_pairs %>% filter(block_id==block) %>% pull(ic50_1) %>% as.numeric()
      conc_1 <- filter(res$response, block_id==block) %>% pull(conc1) %>%  unique()
      c_1 <- conc_1[which(min_rank(abs(conc_1 - ic50_1))==1)]
      #
      ic50_2 <- res$drug_pairs %>% filter(block_id==block) %>% pull(ic50_2)
      conc_2 <- filter(res$response, block_id==block) %>% pull(conc2) %>%  unique() %>% as.numeric()
      c_2 <- conc_2[which(min_rank(abs(conc_2 - ic50_2))==1)]

      # barometer
      ic50_1 <- res$drug_pairs %>% filter(block_id==block) %>% pull(ic50_1) %>% as.numeric()
      conc_1 <- filter(res$response, block_id==block) %>% pull(conc1) %>%  unique()
      c_1 <- conc_1[which(min_rank(abs(conc_1 - ic50_1))==1)]
      ic50_2 <- res$drug_pairs %>% filter(block_id==block) %>% pull(ic50_2)
      conc_2 <- filter(res$response, block_id==block) %>% pull(conc2) %>%  unique() %>% as.numeric()
      c_2 <- conc_2[which(min_rank(abs(conc_2 - ic50_2))==1)]
      p3 <- PlotBarometer(
        data = res, 
        plot_block = block, 
        plot_concs = c(c_1, c_2),
        needle_text_offset = -2.5)
      ggsave(str_glue('{save_dir}//{drug1}_{drug2}_Barometer_{block}.pdf'), p3, height=6, width=6)
    }
    
    # sensitivity and synergy
    p <- map(c("ZIP", "HSA", "Bliss", "Loewe"), 
             ~ PlotSensitivitySynergy(
               data = res,
               plot_synergy = .x,
               point_size = 3,
               label_size = 8,
               show_labels = TRUE,
               dynamic = FALSE) )
    ggsave(str_glue('{save_dir}/synergy_sensitivity.pdf'), wrap_plots(p, nrow=1),
           height=4, width=16)
  }
  
  
  return(res)
}



## test here
if(F) {
  
  file <- 'C:/Users/haohe/Desktop/20230605 N358 H358R 2122 H23 Mia SynergyFinder Results Format.csv'
  dirname(file) %>% setwd()
  
  data <- read_csv(file)
  res <- run_synergyfinder(data, 'viability')
  
  if(F) {
    # data(mathews_screening_data)
    res <- run_synergyfinder(mathews_screening_data, 'viability')
    # data(ONEIL_screening_data)
    res <- run_synergyfinder(ONEIL_screening_data, 'inhibition') 
  }
  
}




