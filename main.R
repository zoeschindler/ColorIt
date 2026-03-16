################################################################################
# PRETTY COLORING BASED ON FURTHEST POINT SAMPLING
################################################################################

# SETUP ========================================================================

# load packages
library(colorspace)
library(data.table)
library(RANN)
library(lidR)

# HELPERS ======================================================================

get_pal <- function(name = "rainbow") {
  # modified from coolors.co
  palettes <- list(
    "sky" = c("#f2c13a","#ef7239","#f23184","#955ae8","#5c96f5"),
    "sea" = c("#d9ed92","#99d98c","#52b69a","#168aad","#1e6091","#154366","#0C304C"),
    "cozy" = c("#41764c","#a7c957","#eadbb3","#d68b8b","#bc4749"),
    "fairy" = c("#cd92ef","#ffadcb","#fed2e2","#b1e2fc","#9bc1ff"),
    "winter" = c("#007DA3","#00afb9","#fdfcdc","#fdca9b","#ee6258"),
    "rainbow" = c("#f52e2e","#f5982e","#f5d42e","#dcf636","#acf636","#36f6a6","#36e9f6","#3d90ee","#7336f6","#c236f6"),
    "pastel" = c("#fcf7b7","#fed9bb","#ffbfc3","#f7aae7","#c4a6f5","#90bbf8","#7cd9f8","#7beff9","#85f9e0","#a4fdac"),
    "candy" = c("#9137ff","#ff5ce4","#ff4545","#fee440","#00bbf9","#00f5bc"),
    "boring" = c("#DEE2E6", "#191C1F")
  )
  return(colorRampPalette(palettes[[name]]))
}

dummy_input <- function(n = 20) {
  out <- c()
  for (i in 1:n) {
    curr <- data.table::data.table(
      "ID" = i,
      "X" = runif(1, min=-5, max=+5) + rnorm(5, sd = 0.2),
      "Y" = runif(1, min=-5, max=+5) + rnorm(5, sd = 0.2),
      "Z" = rnorm(5))
    out <- rbind(out, curr)
  }
  return(out)
}

# FUNCTIONS ====================================================================

color_ids <- function(
    df = dummy_input(),
    col = "sky",
    n_col = 10,
    n_neighbors = 10,
    instance_id = "ID",
    ground_id = 0,
    ground_color = "#ffffff") {
  
  # check inputs
  # TODO
  
  # assign color palette
  pal_fun <- ifelse(length(col) > 1, colorRampPalette(col), get_pal(col))
  pal <- pal_fun(n_col)
  
  # rename columns
  names(df)[names(df) == instance_id] <- "ID"
  
  # calculate ID centers
  centers <- df[ID != ground_id, .(X = mean(X), Y = mean(Y), Z = min(Z)), by = ID]
  
  # create color lookup
  color_RGB <- colorspace::hex2RGB(pal)@coords * 255
  color_LAB <- as(colorspace::hex2RGB(pal), "LAB")
  color_lookup <- data.table::data.table(
    "color_ID" = 1:n_col,
    "color_HEX" = pal,
    "R" = as.integer(color_RGB[,1]),
    "G" = as.integer(color_RGB[,2]),
    "B" = as.integer(color_RGB[,3])
  )
  
  # calculate LAB color distances (matrix)
  color_distances <- as.matrix(dist(color_LAB@coords))
  
  # calculate spatial distances (graphs)
  kdtree <- RANN::nn2(centers[, .(X, Y, Z)], k = n_neighbors)
  
  # get mean distance of n nearest neighbours per instance
  mean_distances <- rowMeans(kdtree$nn.dists)
  
  # sort IDs depending on average distance to neighbours
  # smallest distance first -> most close neighbours first
  sorted_center_ids <- centers[order(mean_distances), ID]
  
  # prepare output data
  out <- data.table::data.table(
    "ID" = centers$ID,
    "color_ID" = 0
  )
  
  # loop through all IDs
  for (curr_center_id in sorted_center_ids) {
    
    # extract nearest neighbours (or: within a certain distance?)
    kdd_idx <- which(centers$ID == curr_center_id)
    curr_nn_ids <- kdtree$nn.idx[kdd_idx,][-1]
    curr_nn_cols <- out$color_ID[out$ID %in% centers$ID[curr_nn_ids]]
    
    # check colors of nearest neighbours
    if (sum(curr_nn_cols) == 0) {
      
      # assign random color
      out[out$ID == curr_center_id,"color_ID"] <- sample(color_lookup[,color_ID], 1)
    } else {
      
      # get unused colors
      colors_unused <- setdiff(color_lookup$color_ID, unique(curr_nn_cols))
      colors_used <- setdiff(color_lookup$color_ID, colors_unused)
      
      # check if all colors have been used
      if (length(colors_unused) == 1) {
        
        # use left over color
        col_new <- colors_unused
        
      } else {
        
        # check if no colors left
        if (length(colors_unused) == 0) {
          
          # throw warning
          warning("not enough colors or too many neighbours")
          
          # set all colors to unused
          colors_unused <- color_lookup$color_ID
        }
        
        # get total color distances of unused to used colors
        if (length(colors_used) == 1) {
          col_dist_unused <- color_distances[colors_unused,colors_used]
        } else {
          col_dist_unused <- rowSums(color_distances[colors_unused,colors_used])
        }
        
        # get most different color
        col_new <- as.numeric(names(col_dist_unused[which.max(col_dist_unused)]))
      }
      
      # assign new color
      out[out$ID == curr_center_id,"color_ID"] <- col_new
    }
  }
  
  # assign RGB color to points
  out <- merge(out, color_lookup)
  out$color_ID <- NULL
  
  # add ground color
  ground_rgb <- as.integer(colorspace::hex2RGB(ground_color)@coords * 255)
  out <- rbind(
    out,
    data.table::data.table(
      "ID" = ground_id,
      "color_HEX" = ground_color,
      "R" = ground_rgb[1],
      "G" = ground_rgb[2],
      "B" = ground_rgb[3]
    ))
  
  # rename column
  names(out)[names(out) == "ID"] <- instance_id
  
  # return results
  return(out)
}

# EXECUTION ====================================================================

# create dummy data
df <- dummy_input(50)

# check data
ggplot2::ggplot(df) +
  ggplot2::geom_point(ggplot2::aes(x=X, y=Y, col = as.factor(ID)))

# add color values
df_col <- color_ids(df, col = "rainbow")
df <- merge(df, df_col)

# show results
plot(Y~X, col=color_HEX, data = df[,.(X = mean(X),Y =  mean(Y), color_HEX = unique(color_HEX)), by = ID], pch=16)
points(Y~X, col=color_HEX, data = df)

################################################################################

# read data
las <- readLAS("D:/github/trees.laz", select = "0")

# add color values
df <- color_ids(las@data, col = viridis::inferno(5), instance_id = "PredInstance", ground_color = "#ff0000")

# show results
las@data <- merge(las@data, df)
plot(las, color = "RGB")

# ==============================================================================