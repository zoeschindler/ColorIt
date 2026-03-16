################################################################################
# PRETTY COLORING BASED ON FURTHEST POINT SAMPLING
################################################################################

# SETUP ========================================================================

# load packages
library(colorspace)

# HELPERS ======================================================================

get_pal <- function(name = "rainbow") {
  all_pal <- list(
    "sky" = c(), # blue to orange
    "sea" = c(), # blue to green
    "cozy" = c(), # teal to orange
    "fairy" = c(), # pink to purple to blue
    "winter" = c(), # red to blue
    "rainbow" = c(), # normal rainbow
    "pastel" = c(), # pastel rainbow
    "candy" = c() # bright rainbow
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

# add color values

# show results

# ==============================================================================