# Helper function to export Excel workbook
export_excel <- function(inputList, filename) {
  
  require(openxlsx)
  
  wb <- openxlsx::createWorkbook()
  
  input <- inputList
  
  # sheetnames <- paste0("Sheet", seq_along(input))
  
  sheetnames <- names(input) %>% str_sub(1,20)
  
  lsn <- length(sheetnames)
  
  # internal function .create_unique_ids for exportExcel
  .create_unique_ids <- function(n, seed_no = 1, char_len = 3){
    set.seed(seed_no)
    pool <- c(letters, LETTERS, 0:9)
    
    res <- character(n) # pre-allocating vector is much faster than growing it
    for(i in seq(n)){
      this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
      while(this_res %in% res){ # if there was a duplicate, redo
        this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
      }
      res[i] <- this_res
    }
    res
  }
  
  snid <- .create_unique_ids(lsn, char_len = 3)
  
  sheetnames <- paste0(1:lsn, "_", snid, "_", sheetnames)
  
  Map(function(data, nameofsheet){
    
    openxlsx::addWorksheet(wb, nameofsheet)
    openxlsx::writeDataTable(wb, nameofsheet, data, rowNames = FALSE)
    
  }, input, sheetnames)
  
  openxlsx::saveWorkbook(wb, file = filename)
  
}