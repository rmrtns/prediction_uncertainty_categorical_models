

save_patient_data <- function(id, patient_df) {
  base_dir <-
    "out/simulations_discordance_individual_results"  
  
  # Calculate the range for the subfolder based on the unique IDs
  subfolder_start <- floor((id - 1) / 200) * 200 + 1
  subfolder_end <- subfolder_start + 199
  
  # Create the subfolder directory if it doesn't exist
  subfolder_dir <-
    file.path(base_dir,
              paste0("IDs_", subfolder_start, "_to_", subfolder_end))
  
  if (!dir.exists(subfolder_dir)) {
    dir.create(subfolder_dir, recursive = TRUE)
  }
  
  # Save the patient data as an RDS file to the specified file path
  saveRDS(patient_df,
          file.path(subfolder_dir, paste0("patient_", id, ".rds")))
}
