plotTreemap <- function(x) {
  # TODO remove these 2 lines
  x$cell <- paste(x$x, x$y)
  x <- x[x$cell %in% x$cell[1:100], ]

  xSorted <- sort(unique(x$x))
  ySorted <- sort(unique(x$y))
  xCoords <- seq(xSorted[1], max(x$x), xSorted[2] - xSorted[1])
  yCoords <- seq(ySorted[1], max(x$y), ySorted[2] - ySorted[1])

  grid::grid.newpage()
  grid::grid.rect()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(ncol = length(xCoords),
                                                               nrow = length(yCoords))))

  plotCellTreemap <- function(columnNumber, rowNumber) {
    xCell <- x[x$x == xCoords[columnNumber] & x$y == yCoords[rowNumber], ]
    if (nrow(xCell) == 0 || all(xCell$.value == 0)) {
      return()
    }
    vp <- grid::viewport(layout.pos.col = columnNumber, layout.pos.row = rowNumber)
    grid::pushViewport(vp)
    treemap::treemap(xCell,
                     index = "landuse",
                     vSize = ".value",
                     vColor = ".value",
                     title = "",
                     vp = vp)
    grid::popViewport()
  }

  for (iCol in seq_along(xCoords)) {
    for (iRow in seq_along(yCoords)) {
      plotCellTreemap(iCol, iRow)
    }
  }
}
