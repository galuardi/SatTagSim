#' Get parameters modern method
#' alternate way of getting movement parameters 
#' Experimental only.. 
#' @param tracks
#' @param inbox
#' @param rasbox
#' @param rrows
#' @param rcols
#' @param use_wts
#' @param missvec
#' @param fillvec
#'
#' @returns
#'
#' @export
#' @examples
#' simpar_dl_test  =  modern_process_tracks(tracks  =  dlfish_sp
#', inbox  =  my_boxes
#', rasbox  =  NULL
#', rrows  =  26*5
#', rcols  =  29*5
#', missvec  =  c(1)
#', fillvec  =  c(2)
#', use_wts  =  NULL
#')
modern_process_tracks <- function(tracks, inbox, rasbox = NULL, 
                                  rrows = 26 * 5, rcols = 29 * 5, 
                                  use_wts = NULL, missvec = NULL, fillvec = NULL) {
  
  fill_month_par <- function(x) {
    if (all(is.na(x))) return(x)
    sn <- min(which(!is.na(x))) - 1
    y <- if (sn > 0) c(x[(sn + 1):length(x)], x[1:sn]) else x
    for (i in 2:length(y)) {
      if (is.na(y[i])) y[i] <- y[i - 1]
    }
    if (sn > 0) c(y[(length(y) - sn + 1):length(y)], y[1:(length(y) - sn)]) else y
  }
  
  if (inherits(tracks, "Spatial")) tracks <- sf::st_as_sf(tracks)
  if (!inherits(tracks, "sf")) {
    tracks <- sf::st_as_sf(as.data.frame(tracks), coords = c("Longitude", "Latitude"))
  }
  if (inherits(inbox, "Spatial")) inbox <- sf::st_as_sf(inbox)
  inbox <- sf::st_make_valid(inbox)
  
  if (is.na(sf::st_crs(tracks))) {
    sf::st_crs(tracks) <- sf::st_crs(inbox)
  } else if (!is.na(sf::st_crs(inbox))) {
    tracks <- sf::st_transform(tracks, sf::st_crs(inbox))
  }
  
  joined <- sf::st_join(tracks, inbox)
  joined$box <- as.numeric(joined$ID)
  
  coords <- sf::st_coordinates(joined)
  tracksdf <- sf::st_drop_geometry(joined) %>%
    dplyr::filter(!is.na(box)) %>%
    dplyr::mutate(
      Longitude = coords[!is.na(joined$box), 1],
      Latitude  = coords[!is.na(joined$box), 2],
      Month     = as.numeric(Month)
    )
  
  boxvec <- sort(unique(as.numeric(inbox$ID)))
  
  tracks_split <- split(tracksdf, list(tracksdf$box, tracksdf$TagID, tracksdf$Month), drop = TRUE)
  uv_list <- lapply(tracks_split, function(df) {
    res <- get.uv(df[, c("Day", "Month", "Year", "Longitude", "Latitude")])
    data.frame(box = df$box[1], TagID = df$TagID[1], Month = df$Month[1], u = res[1], v = res[2])
  })
  uv_by_tag <- do.call(rbind, uv_list)
  
  uv_summary <- uv_by_tag %>%
    dplyr::group_by(box, Month) %>%
    dplyr::summarise(
      u_mean = mean(u, na.rm = TRUE) * -1,
      v_mean = mean(v, na.rm = TRUE),
      u_sd   = sd(u, na.rm = TRUE),
      v_sd   = sd(v, na.rm = TRUE),
      .groups = "drop"
    )
  
  d_list <- lapply(split(tracksdf, tracksdf$box), function(df) {
    d_box <- get.kfD(df)
    d_box$box <- df$box[1]
    d_box
  })
  d_by_box <- do.call(rbind, d_list)
  d_summary <- d_by_box %>%
    dplyr::group_by(box, Month) %>%
    dplyr::summarise(
      D_mean = mean(D, na.rm = TRUE),
      D_sd   = mean(Dsd, na.rm = TRUE),
      .groups = "drop"
    )
  
  to_matrix <- function(df, val_col) {
    mat <- matrix(NA_real_, nrow = length(boxvec), ncol = 12,
                  dimnames = list(as.character(boxvec), 1:12))
    for (i in seq_len(nrow(df))) {
      b_idx <- match(df$box[i], boxvec)
      m_idx <- df$Month[i]
      if (!is.na(b_idx) && !is.na(m_idx) && m_idx >= 1 && m_idx <= 12) {
        mat[b_idx, m_idx] <- df[[val_col]][i]
      }
    }
    t(apply(mat, 1, fill_month_par))
  }
  
  u2      <- to_matrix(uv_summary, "u_mean")
  v2      <- to_matrix(uv_summary, "v_mean")
  u2_sd   <- to_matrix(uv_summary, "u_sd")
  v2_sd   <- to_matrix(uv_summary, "v_sd")
  dbox    <- to_matrix(d_summary, "D_mean")
  dbox_sd <- to_matrix(d_summary, "D_sd")
  
  if (!is.null(missvec)) {
    for (i in seq_along(missvec)) {
      u2[missvec[i], ]      <- u2[fillvec[i], ]
      v2[missvec[i], ]      <- v2[fillvec[i], ]
      u2_sd[missvec[i], ]   <- u2_sd[fillvec[i], ]
      v2_sd[missvec[i], ]   <- v2_sd[fillvec[i], ]
      dbox[missvec[i], ]    <- dbox[fillvec[i], ]
      dbox_sd[missvec[i], ] <- dbox_sd[fillvec[i], ]
    }
    for (i in 1:ncol(u2_sd)) u2_sd[is.na(u2_sd[, i]), i] <- sd(u2_sd[, i], na.rm = TRUE)
    for (i in 1:ncol(v2_sd)) v2_sd[is.na(v2_sd[, i]), i] <- sd(v2_sd[, i], na.rm = TRUE)
    dbox_sd[is.na(dbox_sd)] <- 0
  }
  
  if (!is.null(use_wts)) {
    tagwts <- tracksdf %>%
      dplyr::group_by(box, Month) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
  }
  
  par_array <- abind::abind(u2, v2, dbox, u2_sd, v2_sd, dbox_sd, 
                            along = 3, 
                            new.names = c("u", "v", "D", "u.sd", "v.sd", "D.sd"))
  return(par_array)
}

# simpar  =  make.par.array(tracks  =  nsfish, inbox  =  box11, rasbox  =  NULL, rrows  =  26*5, rcols  =  29*5, use_wts  =  NULL, missvec  =  c(3,7,10,11), fillvec  =  c(4, 8, 9, 9))

# simpar_test  =  modern_process_tracks(tracks  =  nsfish, inbox  =  box11, rasbox  =  NULL, rrows  =  26*5, rcols  =  29*5, use_wts  =  NULL, missvec  =  c(3,7,10,11), fillvec  =  c(4, 8, 9, 9))

# simpar_dl  =  make.par.array(tracks  =  dlfish_sp, inbox  =  my_boxes, rasbox  =  NULL, rrows  =  26*5, rcols  =  29*5, use_wts  =  NULL)




