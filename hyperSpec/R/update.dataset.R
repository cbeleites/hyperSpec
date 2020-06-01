##' Update data sets for hyperSpec package
##'
##'
##' @return directory of edited R files and associated test logs
##'
##' @author Erick Oduniyi
##'
##' @keywords programming utilities
##' @examples
##' @export
# TODO: consider deleting .txt files every time this function is ran
# TODO: add option to switch between verbose console output
# TODO: Check if the file even has examples 
# path_to_files <- "/Users/erickoduniyi/Desktop/hyperSpec/hyperSpec/hyperSpec/hyperSpec/R" 
# path_to_store <- "~/Desktop/R-edits/"
update.examples <- function(old_ds, new_ds, path_to_files, path_to_store) {
  
  # Check the difference between the two datasets
  old_ds_name <- deparse(substitute(old_ds))
  new_ds_name <- deparse(substitute(new_ds))
  ds_info <- check_ds(old_ds, new_ds, old_ds_name, new_ds_name)
  
  # Create directories where the files need to be saved
  file_info <- create_edits_dir(path_to_files, path_to_store, old_ds_name)
  
  # Edit .R files
  message("Would you like to edit .R files now?")
  choice <- select.list(c("Yes", "No"))
  if (choice == "Yes") {
    make_edits_R(ds_info, file_info)
  }
  
  # Check test for updated file
  message("Would you like to check test now?")
  choice <- select.list(c("Yes", "No"))
  if (choice == "Yes") {
    check_test_in_file(file_info)
  }
  
  # Check updated file examples
  message("Would you like to check examples now?")
  choice <- select.list(c("Yes", "No"))
  if (choice == "Yes") {
    check_examples(file_info)
  }
  
  # Check updated file vignettes
  message("Would you like to check vignettes now?")
  choice <- select.list(c("Yes", "No"))
  if (choice == "Yes") {
    check_vingette(file_info)
  }
  
  # Let the user know user.examples has completed
  message("update.examples.R has completed!!!")
}
# Helper(s) -----------------------------------------------

check_ds <- function(old_ds, new_ds, old_ds_name, new_ds_name) {
  
  # Check data sets
  message(paste("Checking differences between", old_ds_name, "and", new_ds_name))
  
  # Handle potential differences between datasets
  if (!setequal(colnames(old_ds), colnames(new_ds))) {
    col_diff_old <- setdiff(colnames(old_ds), colnames(new_ds))
    col_diff_new <- setdiff(colnames(new_ds), colnames(old_ds))
    ls_old_names <- paste(sort(col_diff_old), collapse = ", ")
    
    # Let user know about the difference between datasets
    message(paste(c("The following columns appear in", old_ds_name, "but not", paste0(new_ds_name, ":"), ls_old_names), collapse= " "))
    
    # Give the user options about substituting columns between datasets
    message(paste("Which column in", old_ds_name, "do you want to substitute?:"))
    sub_old <- select.list(sort(col_diff_old))
    if (sub_old != "") {
      message(paste("Substitute", sub_old, "for which column in", paste0(new_ds_name, "?:")))
      for_new <-select.list(sort(col_diff_new)) 
    }
  }
  
  # Prepare information about data sets to be passed on to subsequent functions
  ds_info <- list(ds_names = c(old_ds_name, new_ds_name), sub_vals = c(sub_old, for_new))
}

create_edits_dir <- function(path_to_files, path_to_store, old_ds_name) {
  
  # Set the location of the .R files that need to be updated:
  setwd(path_to_files)
  
  # Create a directories where the files need to be saved
  loc_2_be_saved <- path_to_store
  dir.create(loc_2_be_saved, showWarnings = FALSE)
  dir.create(paste0(loc_2_be_saved, "test-results"), showWarnings = FALSE)
  dir.create(paste0(loc_2_be_saved, "example-results"), showWarnings = FALSE)
  dir.create(paste0(loc_2_be_saved, "vignette-results"), showWarnings = FALSE)
  
  # Get all the .R files in the current directory
  R_files <- list.files()
  
  # Get files
  files2get <- vector()
  
  for (i in seq_along(R_files)) {
    
    # Read in the file
    source_code <- readLines(R_files[i])
    
    # Check if file references the old data set
    if (identical(grep(old_ds_name, source_code, value = TRUE), character(0))) {
      
      # Skip this file
      message(paste(R_files[i],"does not contain", old_ds_name, "data set..."))
      
    } else {
      
      # "Just throw it in the bag" - Fabolous ft. The-Dream
      files2get <- c(files2get, R_files[i])
    }
  }
  
  cat("-----------------------------------------------------------------------------------------","\n")
  # Prepare information about files
  # Collect subset of filenames
  R_files <- files2get
  file_info <- list(file_loc = loc_2_be_saved, R_files = R_files)
}

make_edits_R <- function(ds_info, file_info) {
  
  # Setup
  loc_2_be_saved <- file_info$file_loc
  filenames <- file_info$R_files
  old_ds_name <- ds_info$ds_names[1]
  new_ds_name <- ds_info$ds_names[2]
  sub_old <- ds_info$sub_vals[1]
  for_new <- ds_info$sub_vals[2]
  
  # Edit
  message("== Making edits =========================================================================")

  # Edit each file
  for (i in seq_along(filenames)) {
    
    # Read in the file
    cat(paste0("**Reading in file: ", filenames[i]), "\n")
    source_code <- readLines(filenames[i])
    
    # Prepare substitute pattern for specific case
    old <- paste0(old_ds_name,"\\$", sub_old)
    new <- paste0(new_ds_name,"$", for_new)
    
    # Specific case 
    cat(paste("Replacing occurences of", old, "with", new), "\n")
    code_edited <- gsub(old, new, source_code)
    
    # Generic case
    cat(paste("Replacing occurences of", old_ds_name, "with", new_ds_name), "\n")
    code_edited <- gsub(old_ds_name, new_ds_name, code_edited)
    
    # Output updated file
    cat(paste("Updating", filenames[i]), "\n")
    writeLines(code_edited, paste0(loc_2_be_saved, filenames[i]))
    
    # Move to the next file
    cat(paste0("**Done editing: ", filenames[i]), "\n")
  }
  
  # Tell user
  message("Done editing all files!!..")
  cat("-----------------------------------------------------------------------------------------","\n")
}

check_test_in_file <- function(file_info) {
  
  # Setup
  loc_2_be_saved <- file_info$file_loc
  filenames <- file_info$R_files
  
  # Check test
  message("== Checking unittest ====================================================================")
  
  # Check test of each file
  for (i in seq_along(filenames)) {
    
    # Run test in file and output the results to a text file
    test_res_here <- paste0(loc_2_be_saved, "test-results/", gsub(".R", ".txt", filenames[i]))
    verify_output(test_res_here, with_reporter(reporter = SummaryReporter$new(), start_end_reporter = TRUE, get.test(.aggregate)()))
    cat(paste0("**Logging results to...", gsub(".R", ".txt", filenames[i])), "\n")
  }
  
  # Tell user
  message("Done checking all test!!..")
  cat("-----------------------------------------------------------------------------------------","\n")
}

check_examples <- function(file_info) {
  
  # Setup
  filenames <- file_info$R_files
  
  # Check examples
  message("== Checking examples ====================================================================")
  
  # Check examples in each file
  for (i in seq_along(filenames)) {
    
    cat(paste0("**Logging results to...", gsub(".R", ".Rd", filename)), "\n")
    
    # Move to the next file
    message(paste0("Done checking examples for: ", filenames[i]))
  }
  
  # Tell user
  message("Done checking all examples!...")
  cat("-----------------------------------------------------------------------------------------","\n")
}

check_vingette <- function(file_info) {
  
  # Setup
  filenames <- file_info$R_files
  
  # Check vignette
  message("== Checking vignettes ===================================================================")
  cat(paste0("**Logging results to...", gsub(".R", ".txt", filename)), "\n")
  
  # Tell user
  message("Done checking vignettes!!..")
  cat("-----------------------------------------------------------------------------------------","\n")
}