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
  all_pal <- list(
    "vibrant_summer" = c("#ff595e","#ff924c","#ffca3a","#c5ca30","#8ac926","#52a675","#1982c4","#4267ac","#6a4c93"), # red to green to blue to purple
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
    curr <- data.table::data.table(
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
col = "vibrant_summer"
ncol = 10
n_neighbors = 8

# check inputs

# assign color palette
if (length(col) < ncol) {
  pal <- colorRampPalette(get_pal(col))
  pal <- pal(ncol)
} else {
  pal <- sample(get_pal(col), ncol)
}

# prepare output data
out <- c()

# calculate ID centers
centers <- df[, .(x = mean(x), y = mean(y), z = mean(z)), by = ID]

# create lookup -> color ID, color RGB, color LAB
color_RGB = colorspace::hex2RGB(pal)
color_LAB = as(colorspace::hex2RGB(pal), "LAB")

lookup <- data.table::data.table(
  "ID" = 1:ncol,
  "color_ID" = pal,
  "color_RGB" = split(color_RGB@coords, seq(nrow(color_RGB@coords))),
  "color_LAB" = split(color_LAB@coords, seq(nrow(color_LAB@coords)))
)

# calculate LAB color distances (matrix)
lab_matrix <- do.call(rbind, lookup$color_LAB)
color_distances <- as.matrix(dist(lab_matrix))

# calculate spatial distances (graphs)
# -> kdtrees?
kdtree <- RANN::nn2(centers[, .(x, y, z)], k = n_neighbors)

# get mean distance of n nearest neighbours per instance
mean_distances <- rowMeans(kdtree$nn.dists)

# sort IDs depending on average distance to neighbours
sorted_ids <- centers[order(-mean_distances), ID]

# assign first instance ID a random color


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