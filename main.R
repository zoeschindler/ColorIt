################################################################################
# PRETTY COLORING BASED ON FURTHEST POINT SAMPLING
################################################################################

# SETUP ========================================================================

# load packages
library(colorspace)
library(data.table)
library(RANN)

# HELPERS ======================================================================

get_pal <- function(name = "rainbow") {
  # modified from coolors.co
  palettes <- list(
    "vibrant_summer" = c("#ff595e","#ff924c","#ffca3a","#c5ca30","#8ac926","#52a675","#1982c4","#4267ac","#6a4c93"),
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
      "x" = runif(1, min=-5, max=+5) + rnorm(5),
      "y" = runif(1, min=-5, max=+5) + rnorm(5),
      "z" = rnorm(5))
    out <- rbind(out, curr)
  }
  return(out)
}

# FUNCTIONS ====================================================================

color_ids <- function(
    df = dummy_input(),
    col = "sky",
    ncol = 12,
    n_neighbors = 8) {

  # check inputs
  # TODO
  
  # assign color palette
  pal_fun <- ifelse(length(col) > 1, colorRampPalette(col), get_pal(col))
  pal <- pal_fun(ncol)
  
  # calculate ID centers
  centers <- df[, .(x = mean(x), y = mean(y), z = mean(z)), by = ID]
  
  # create color lookup
  color_RGB <- colorspace::hex2RGB(pal)
  color_LAB <- as(color_RGB, "LAB")
  color_lookup <- data.table::data.table(
    "color_ID" = 1:ncol,
    "color_HEX" = pal,
    "color_RGB" = split(color_RGB@coords, seq(nrow(color_RGB@coords))),
    "color_LAB" = split(color_LAB@coords, seq(nrow(color_LAB@coords)))
  )
  
  # calculate LAB color distances (matrix)
  lab_matrix <- do.call(rbind, color_lookup$color_LAB)
  color_distances <- as.matrix(dist(lab_matrix))
  
  # calculate spatial distances (graphs)
  kdtree <- RANN::nn2(centers[, .(x, y, z)], k = n_neighbors)
  
  # get mean distance of n nearest neighbours per instance
  mean_distances <- rowMeans(kdtree$nn.dists)
  
  # sort IDs depending on average distance to neighbours
  sorted_center_ids <- centers[order(-mean_distances), ID]
  
  # prepare output data
  out <- data.table::data.table(
    "ID" = centers$ID,
    "color_ID" = 0
  )
  
  # loop through all IDs
  for (curr_center_id in sorted_center_ids) {
    
    # extract nearest neighbours
    curr_nn_ids <- kdtree$nn.idx[curr_center_id,][-1]
    
    # check colors of nearest neighbours
    if (sum(out[out$ID %in% curr_nn_ids,"color_ID"]) == 0) {
      
      # assign random color
      out[out$ID == curr_center_id,"color_ID"] <- sample(color_lookup[,color_ID], 1)
    } else {
      
      # get unused colors
      colors_unused <- c() # TODO
      
      # check if all colors have been used
      if (length(colors_unused == 0)) {
        
        # new, most different color from neighbours
        # TODO
        
      } else if (length(colors_unused == 1)) {
        
        # use left over color
        out[out$ID == curr_center_id,"color_ID"] <- colors_unused
        
      } else {
        
        # for each color not yet assigned to a neighbour
        # TODO
        
        # calculate total color distance
        # TODO
        
        # get most different color (largest distance)
        # TODO
      }
    }
  }
  
  # assign RGB color to points
  
  # return results
  return(out)
}

# EXECUTION ====================================================================

# create dummy data
df <- dummy_input()

# check data
ggplot2::ggplot(df) +
  ggplot2::geom_point(ggplot2::aes(x=x, y=y, col = ID))

# add color values
df <- color_ids(df)
  
# show results

# ==============================================================================