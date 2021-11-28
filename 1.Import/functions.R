FnImportScreenFileImport <- function(FileSlct, FileFormatSlct, FileSeparatorSlct, DecimalSeparatorSlct, FileSheetSlct, RangeCellSlct) {
  
  if (FileFormatSlct == "csv") {
    read.delim(FileSlct, sep = FileSeparatorSlct, dec = DecimalSeparatorSlct)
    
  } else if (FileFormatSlct == "Excel") {
    if (RangeCellSlct == "") {
      read_excel(FileSlct, FileSheetSlct)
    } else {
      read_excel(FileSlct, FileSheetSlct, RangeCellSlct)
    }
    
  } else if (FileFormatSlct == "txt") {
    read.table(FileSlct, dec = DecimalSeparatorSlct)
    
  } else if (FileFormatSlct == "SPSS") {
    read.spss(FileSlct, to.data.frame = TRUE)
    
  } else if (FileFormatSlct == "SAS") {
    read.sas7bdat(FileSlct)
    
  } else if (FileFormatSlct == "STATA") {
    read_dta(FileSlct)

  } else if (FileFormatSlct == "Gretl") {
    read.gdt(FileSlct)
  }
  
}