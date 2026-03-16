################################################################################
# PRETTY COLORING BASED ON FURTHEST POINT SAMPLING
################################################################################

# SETUP ========================================================================

# load packages
library(colorspace)

# HELPERS ======================================================================

get_pal <- function(name = "rainbow") {
  # modified from coolors.co
  all_pal <- list(
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
  return(all_pal[[name]])
}

dummy_input <- function(n = 20) {
  out <- c()
  for (i in 1:n) {
    curr <- data.frame(
      "ID" = i,
      "x" = runif(1, min=-5, max=+5) + rnorm(5),
      "y" = runif(1, min=-5, max=+5) + rnorm(5),
      "z" = rnorm(5))
    out <- rbind(out, curr)
  }
  return(out)
}

distances <- function(mat, cols) {
  return(NA)
}

# FUNCTIONS ====================================================================

# dummy inputs
df = dummy_input()
col = "sky"
ncol = 10

# check inputs

# assign color palette
if (length(col) > 1) {
  pal <- colorRampPalette(col)
} else {
  pal <- get_pal(col)
}

# prepare output data
out <- c()

# calculate ID centers

# create lookup -> color ID, color RGB, color LAB

# calculate color distances (matrix)

# calculate spatial distances (graphs)
# -> kdtrees?

# get mean distance of n nearest neighbours per instance

# sort IDs depending on average distance to neighbours

# assign first ID an random color

# loop through all IDs

  # neighbours have no color -> random color

  # neighbours have color
    
    # check if all colors have been used

    # yes:

    # new, most different color from neighbours

    # no:

      # for each color not yet assigned to a neighbour

        # calculate total color distance

    # get most different color (largest distance)

  # assign ID the color

  # assign RGB color to points

# return results

# EXECUTION ====================================================================

# create dummy data
df <- dummy_input()

# check data
ggplot2::ggplot(df) +
  ggplot2::geom_point(ggplot2::aes(x=x, y=y, col = ID))

# add color values

# show results

# ==============================================================================