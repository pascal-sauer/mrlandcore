calcLUH3 <- function() {
  years <- seq(1965, 2010, 5)

  raw <- readSource("LUH3", "states", years)
  raw <- raw[[grep("pltns", names(raw), invert = TRUE)]] # skip forestry / plantations for now

  x <- as.magpie(raw)

  # caveat: this is using a static clustermap, probably want to take clustermap as arg eventually
  gridMapping <- calcOutput("ResolutionMapping", input = "magpie", target = "luh3", aggregate = FALSE)
  gridMapping$x.y.iso <- paste0(gridMapping$cellOriginal, ".", gridMapping$country)
  gridMapping <- gridMapping[, c("cell", "x.y.iso")]
  x <- toolAggregate(x, gridMapping)
  names(dimnames(x))[1] <- "x.y.iso"
  getItems(x, 2) <- sub("-.+", "", getItems(x, 2))

  # TODO add missing cells

  # match output of calcLUH2v2 - might want to delete this eventually
  names(dimnames(x)) <- c("x.y.iso", "t", "landuse")

  return(list(x = x,
              unit = "Mha",
              description = "LUH3 land use data",
              isocountries = FALSE))
}
