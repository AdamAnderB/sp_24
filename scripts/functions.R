
library(here)

#folder creation
create_folder <- function(folder_name) {
  # Construct the path to create the folder one level up
  folder_path <- file.path("..", folder_name)
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created:", folder_path, "\n")
  } else {
    cat("The folder", folder_path, "already exists.\n")
  }
}

create_folder_same_level <- function(folder_name) {
  # Construct the path to create the folder at the same level as the current directory
  folder_path <- file.path(getwd(), folder_name)
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path)
    cat("Folder created:", folder_path, "\n")
  } else {
    cat("The folder", folder_path, "already exists.\n")
  }
}


save_ggplot <- function(plot, title = NULL, visuals_folder_name = NULL, same_level = FALSE, width = 7, height = 4) {
  # Check for 'title' variable and set filename
  if (is.null(title)) {
    if (exists("title", envir = parent.frame())) {
      title <- get("title", envir = parent.frame())
    } else {
      title <- "default_title"
    }
  }
  filename_clean <- str_replace_all(title, " ", "_")
  # Determine the visuals folder name
  if (is.null(visuals_folder_name)) {
    visuals_folder_name <- paste0(basename(here()), "_visuals")
  }
  # Construct the visuals path
  vis_path <- if(same_level) {
    visuals_folder_name
  } else {
    here(visuals_folder_name)
  }
  save_path <- file.path(vis_path, paste0(filename_clean, ".pdf"))
  # Check and create directory if needed
  if (!dir.exists(vis_path)) {
    dir.create(vis_path, recursive = TRUE)
  }
  # Save the plot
  ggsave(plot, file = save_path, width = width, height = height)
  # Return the plot
  return(plot)
}
