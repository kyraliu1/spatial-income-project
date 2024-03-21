color.bar.horizontal <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, length.out=nticks), title='', decimal.places=2, bar.height=5) {
  scale = (length(lut) - 1) / (max - min)
  
  # Calculate bar vertical position based on desired height
  y.bottom <- 5 - (bar.height / 2)
  y.top <- y.bottom + bar.height
  
  # Set up an empty plot area with space for the title
  plot(c(min, max), c(0, 10), type='n', bty='n', yaxt='n', ylab='', xaxt='n', xlab='', main='')
  
  # Format the tick labels to reduce the number of decimal places
  tick.labels <- format(ticks, nsmall=decimal.places)
  
  # Draw the ticks and labels on the bottom x-axis
  axis(1, at=ticks, labels=tick.labels, las=1) # las=1 makes the labels parallel to the axis
  
  # Draw the color rectangles
  for (i in 1:(length(lut)-1)) {
    x = (i - 1) / scale + min
    rect(x, y.bottom, x + 1/scale, y.top, col=lut[i], border=NA)
  }
  
  # Adding the title
  if (title != '') {
    title(main=title, line=1) # Adjust the line parameter as needed to position the title
  }
}

