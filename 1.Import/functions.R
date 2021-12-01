FnImportScreenFileImport <- function(FileSlct,
                                     FileFormatReal,
                                     FileFormatSlct,
                                     FileSeparatorSlct,
                                     DecimalSeparatorSlct,
                                     FileSheetSlct,
                                     RangeCellSlct) {

  if (is.null(FileSlct)) {
    show_toast("File not selected", type = "error", position = "top-end", timer = 6000)
    stop()
  }
  
  if (FileFormatReal != FileFormatSlct & !(FileFormatReal %chin% c("xls", "xlsx") & FileFormatSlct == "xlsx")) {
    show_toast("Selected file format is not correct", type = "error", position = "top-end", timer = 6000)
    stop()
  }
  
  
  if (FileFormatReal %chin% c("xls", "xlsx") & FileFormatSlct == "xlsx") {
    
    # Excel sheet check
    if (length(excel_sheets(FileSlct)) != FileSheetSlct) {
      show_toast("Selected Excel sheet does not exist", type = "error", position = "top-end", timer = 6000)
      stop()
    }
    
    if (RangeCellSlct != "") {
      # Correct symbols check
      if (!(grepl("\\W*", RangeCellSlct) & grepl(":{1}", RangeCellSlct))) {
        show_toast("Selected range is not consistent with the input format",
                   type = "error",
                   position = "top-end",
                   timer = 6000)
        stop()
      }
      
      # Correct rectangular form check
      RangeCellRowVar <- gsub("[A-z]*", "", RangeCellSlct) %>% str_split(., ":", simplify = TRUE)
      RangeCellColumnVar <- gsub("\\d*", "", RangeCellSlct) %>% str_split(., ":", simplify = TRUE) %>% str_split(., "")
    
      for (i in seq_along(RangeCellColumnVar)) {
        RangeCellColumnVar[[i]] <- match(tolower(RangeCellColumnVar[[i]]), letters)
        RangeCellColumnVar[[i]] <- as.numeric(paste(RangeCellColumnVar[[i]], collapse = ""))
      }
    
      if (any(RangeCellRowVar == "") | RangeCellRowVar[1] > RangeCellRowVar[2] | RangeCellColumnVar[[1]] > RangeCellColumnVar[[2]]) {
        show_toast("Selected range is not a rectangle", type = "error", position = "top-end", timer = 6000)
        stop()
      }
    }
  }
  
  if (FileFormatSlct == "csv") {
    read.delim(FileSlct, sep = FileSeparatorSlct, dec = DecimalSeparatorSlct)
    
  } else if (FileFormatSlct == "xlsx") {
    if (RangeCellSlct == "") { 
      read_excel(FileSlct, FileSheetSlct)
    } else {
      read_excel(FileSlct, FileSheetSlct, RangeCellSlct)
    }
    
  } else if (FileFormatSlct == "txt") {
    read.table(FileSlct, dec = DecimalSeparatorSlct)
    
  } else if (FileFormatSlct == "sav") {
    read.spss(FileSlct, to.data.frame = TRUE)
    
  } else if (FileFormatSlct == "sas7bdat") {
    read.sas7bdat(FileSlct)
    
  } else if (FileFormatSlct == "dta") {
    read_dta(FileSlct)

  } else if (FileFormatSlct == "gdt") {
    read.gdt(FileSlct)
  }
  
}