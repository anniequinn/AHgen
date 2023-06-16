setwd(dirname(rstudioapi::getSourceEditorContext()$path)); getwd()

USAH_3.0_template_baseline_vInfo_full <- 
  read_xlsx("USAH_3.0_template_baseline_vInfo-full_20230608.xlsx")

usethis::use_data(USAH_3.0_template_baseline_vInfo_full)
