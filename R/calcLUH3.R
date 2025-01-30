calcLUH3 <- function() {
  years <- seq(1965, 2010, 5)

  raw <- readSource("LUH3", "states", years)
  raw <- raw[[grep("pltns", names(raw), invert = TRUE)]] # skip plantations (forestry) for now

  x <- as.magpie(raw)

  # aggregate to our cells
  # caveat: this is using a static clustermap, probably want to take clustermap as arg eventually
  gridMapping <- calcOutput("ResolutionMapping", input = "magpie", target = "luh3", aggregate = FALSE)
  gridMapping$x.y.iso <- paste0(gridMapping$cellOriginal, ".", gridMapping$country)
  gridMapping <- gridMapping[, c("cell", "x.y.iso")]
  x <- toolAggregate(x, gridMapping)
  names(dimnames(x))[1] <- "x.y.iso"
  getItems(x, 2) <- sub("-.+", "", getItems(x, 2))

  # add missing cells
  # LUH does not have data for 775 of our usual 67k cells, mostly in the following countries
  # RUS AUS CAN USA GRL KIR PYF ITA JPN FSM CHN COK MEX TUV MHL
  # 207  70  63  58  32  31  30  17  13  12  11  11  11  11  10
  clustermap <- readSource("MagpieFulldataGdx", subtype = "clustermap")
  missingCells <- setdiff(clustermap$cell, getItems(x, 1))
  stopifnot(length(missingCells) == 775)
  x <- magclass::add_columns(x, missingCells, dim = 1, fill = 0)
  stopifnot(setequal(clustermap$cell, getItems(x, 1)))

  # match output of calcLUH2v2 - might want to delete this eventually?
  names(dimnames(x)) <- c("x.y.iso", "t", "landuse")

  return(list(x = x,
              unit = "Mha",
              description = "LUH3 land use data",
              isocountries = FALSE))
}
