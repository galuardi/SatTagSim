modern_process_tracks <- function(tracks, inbox, rasbox = NULL, 
                                  rrows = 26 * 5, rcols = 29 * 5, 
                                  use_wts = NULL, missvec = NULL, fillvec = NULL) {
  
  # Load modern dependencies
  # Note: You will need to install these if you haven't: sf, terra, dplyr, tidyr, zoo
  library(sf)
  library(terra)
  library(dplyr)
  library(tidyr)
  library(zoo)
  library(abind)
  
  # --- 1. Helper Function: Wrap-around LOCF ---
  # Replaces the old `magic::shift` loop with a much faster Last Observation Carried Forward
  fill_month_par <- function(x) {
    if (all(is.na(x))) return(x)
    # Duplicate 'x' to handle circular wrap-around, fill NAs, and return the second half
    x_filled <- zoo::na.locf(c(x, x), na.rm = FALSE)
    return(x_filled[(length(x) + 1):(2 * length(x))])
  }
  
  # --- 2. Spatial Operations (Upgraded from sp/raster to sf/terra) ---
  # Safely cast older 'sp' objects to 'sf' to ensure backwards compatibility
  if (inherits(tracks, "Spatial")) tracks <- sf::st_as_sf(tracks)
  if (inherits(inbox, "Spatial")) inbox <- sf::st_as_sf(inbox)
  
  if (is.null(rasbox)) {
    # Replace raster() and rasterize() with modern terra equivalents
    rasbox <- terra::rast(terra::ext(inbox), nrow = rrows, ncol = rcols, crs = terra::crs(inbox))
    rasbox <- terra::rasterize(terra::vect(inbox), rasbox, field = "ID")
  }
  
  # Ensure Coordinate Reference Systems (CRS) match, then join
  tracks <- sf::st_transform(tracks, sf::st_crs(inbox))
  
  # Replaces sp::over() with sf::st_join()
  tracks <- sf::st_join(tracks, inbox)
  tracks$box <- as.numeric(tracks$ID)
  
  # Filter missing, get box vector, and drop geometry for tabular data manipulation
tracksdf <- tracks %>% 
  filter(!is.na(box)) %>% 
  # Extract X (Longitude) and Y (Latitude) from the geometry
  dplyr::mutate(
    Longitude = sf::st_coordinates(.)[, "X"],
    Latitude  = sf::st_coordinates(.)[, "Y"],
    Month = as.numeric(Month)
  ) %>% 
  # Now it is safe to drop the spatial geometry
  sf::st_drop_geometry()
  
  # Ensure every box has a unique ID based on its row position
inbox <- inbox %>%
  dplyr::mutate(box_id = row_number())

# Then use 'box_id' for all joins and summaries
boxvec <- sort(unique(inbox$box_id))
#   boxvec <- sort(unique(inbox$plotOrder))
  
  # --- 3. Data Summarization (Upgraded from plyr to dplyr) ---
  # Replaces confusing 4D array initializations and slow daply/ddply calls
  
  # Calculate U and V components
  uv_summary <- tracksdf %>%
  group_by(box, Month) %>%
  group_modify(~ {
    # 1. Create a clean, standard data frame for this group
    # 2. Ensure Month is numeric for the math inside get.uv
    group_data <- .x %>%
      dplyr::mutate(Month = as.numeric(as.character(.y$Month))) %>%
      dplyr::select(Day, Month, Year, Longitude, Latitude) %>%
      as.data.frame() # Force conversion from tibble to standard DF
    
    # 3. Call the function
    res <- get.uv(group_data)
    
    data.frame(u = res[1], v = res[2]) 
  }) %>%
  ungroup() %>%
  # Average across TagIDs
  group_by(box, Month) %>%
  dplyr::summarise(
    u_mean = mean(u, na.rm = TRUE) * -1,
    v_mean = mean(v, na.rm = TRUE),
    u_sd   = sd(u, na.rm = TRUE),
    v_sd   = sd(v, na.rm = TRUE),
    .groups = "drop"
  )
  
  # Calculate D components
  # Assuming get.kfD() returns a data frame with D and Dsd columns
  # --- 3. Calculate D (Diffusion) components ---
d_summary <- tracksdf %>%
  dplyr::mutate(Month = as.numeric(Month)) %>% 
  group_by(box) %>%
  # group_modify passes the data for each box to get.kfD
  group_modify(~ get.kfD(.x)) %>%
  ungroup() %>%
  # Average D and Dsd across all TagIDs within each Box/Month
  group_by(box, Month) %>%
  dplyr::summarise(
    D_mean = mean(D, na.rm = TRUE),
    D_sd   = mean(Dsd, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4. Merge everything into a master grid ---
# This ensures every box has 12 months, even if they contain NAs (to be filled)
full_grid <- tidyr::expand_grid(
  box = boxvec, 
  Month = 1:12
)

final_data <- full_grid %>%
  left_join(uv_summary, by = c("box", "Month")) %>%
  left_join(d_summary, by = c("box", "Month")) %>%
  arrange(box, Month) %>%
  group_by(box) %>%
  # Apply the wrap-around fill to all columns
  dplyr::mutate(across(c(u_mean, v_mean, u_sd, v_sd, D_mean, D_sd), fill_month_par)) %>%
  ungroup()

  # --- 5. Format to Matrices and Handle Missing Vectors ---
  # Helper function to convert our clean dataframe into the expected wide matrices
  to_matrix <- function(df, val_col) {
    mat <- df %>%
      dplyr::select(box, Month, all_of(val_col)) %>%
      pivot_wider(names_from = Month, values_from = all_of(val_col)) %>%
      arrange(box) %>%
      dplyr::select(-box) %>%
      as.matrix()
    rownames(mat) <- as.character(boxvec)
    return(mat)
  }
  
  u2      <- to_matrix(final_data, "u_mean")
  v2      <- to_matrix(final_data, "v_mean")
  u2_sd   <- to_matrix(final_data, "u_sd")
  v2_sd   <- to_matrix(final_data, "v_sd")
  dbox    <- to_matrix(final_data, "D_mean")
  dbox_sd <- to_matrix(final_data, "D_sd")
  
  # Manual replacements
  if (!is.null(missvec)) {
    for (i in seq_along(missvec)) {
      u2[missvec[i], ]      <- u2[fillvec[i], ]
      v2[missvec[i], ]      <- v2[fillvec[i], ]
      u2_sd[missvec[i], ]   <- u2_sd[fillvec[i], ]
      v2_sd[missvec[i], ]   <- v2_sd[fillvec[i], ]
      dbox[missvec[i], ]    <- dbox[fillvec[i], ]
      dbox_sd[missvec[i], ] <- dbox_sd[fillvec[i], ]
    }
    
    # Fill NAs in SDs column-wise with the column standard deviation
    for (i in 1:ncol(u2_sd)) u2_sd[is.na(u2_sd[, i]), i] <- sd(u2_sd[, i], na.rm = TRUE)
    for (i in 1:ncol(v2_sd)) v2_sd[is.na(v2_sd[, i]), i] <- sd(v2_sd[, i], na.rm = TRUE)
    dbox_sd[is.na(dbox_sd)] <- 0
  }
  
  # (Note: In your original code, tagwts was calculated but never returned or used. 
  # I've left the logic here just in case you need it downstream.)
  if (!is.null(use_wts)) {
    tagwts <- tracksdf %>%
      group_by(box, Month) %>%
      dplyr::summarise(count = n(), .groups = "drop")
  }
  
  # --- 6. Final Output ---
  par_array <- abind::abind(u2, v2, dbox, u2_sd, v2_sd, dbox_sd, 
                            along = 3, 
                            new.names = c("u", "v", "D", "u.sd", "v.sd", "D.sd"))
  
  return(par_array)
}

# simpar  =  make.par.array(tracks  =  nsfish, inbox  =  box11, rasbox  =  NULL, rrows  =  26*5, rcols  =  29*5, use_wts  =  NULL, missvec  =  c(3,7,10,11), fillvec  =  c(4, 8, 9, 9))

# simpar_test  =  modern_process_tracks(tracks  =  nsfish, inbox  =  box11, rasbox  =  NULL, rrows  =  26*5, rcols  =  29*5, use_wts  =  NULL, missvec  =  c(3,7,10,11), fillvec  =  c(4, 8, 9, 9))

# simpar_dl  =  make.par.array(tracks  =  dlfish_sp, inbox  =  my_boxes, rasbox  =  NULL, rrows  =  26*5, rcols  =  29*5, use_wts  =  NULL)

simpar_dl_test  =  modern_process_tracks(tracks  =  dlfish_sp
, inbox  =  my_boxes
, rasbox  =  NULL
, rrows  =  26*5
, rcols  =  29*5
, missvec  =  c(1)
, fillvec  =  c(2)
, use_wts  =  NULL
)


