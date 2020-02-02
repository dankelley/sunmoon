#' Draw the moon showing bright and shadow regions
#'
#' @param phase numeric value, ranging continuously from
#' 0 for the (dark) new moon, 1/4 for quarter moon (right side
#' lit), 1/2 for full moon, 3/4 for three-quarter moon (left
#' side lit, and 1 for new moon again.
#'
#' @param lit colour used for the lit portion (defaults to a yellow).
#'
#' @param shadow colour used for the shadowed portion (defaults to a light gray).
#'
#' @param text character value, to draw at the moon centre (mainly for debugging).
#'
#' @param write logical value indicating whether to write the polygon for
#' the lit portion to a text file with a name constructed by pasting together
#' `phase_`, the phase to two digits, and `.dat`.
drawMoon <- function(phase,
                     lit=rgb(200/255,150/255,10/255),
                     shadow=gray(0.5), # rgb(230/255,230/255,230/255),
                     text="",
                     write=FALSE)
{
    to180 <- function(x) {
        x <- x %% 360
        ifelse(x <= 180, x, x - 360)
    }
    ## orthographic projection
    XY <- function(lon, lat, R=1)
    {
        lon <- to180(lon)
        lambda <- pi * lon / 180
        phi <- pi * lat / 180
        x <- R * cos(phi) * sin(lambda)
        y <- R * sin(phi)
        list(x=x, y=y)
    }
    phase <- phase - floor(phase)      # put in range 0 to 1
    shadowLongitude <- 90 - 360 * phase
    shadowLongitude <- to180(shadowLongitude)
    ## draw edge-of-moon circle
    plot(c(-1,1), c(-1,1), xlab="", ylab="", axes=FALSE, asp=1, type="n")
    theta <- seq(0, 2*pi, pi/64)
    polygon(cos(theta), sin(theta), col=shadow)
    lat <- seq(-90, 90, 1)
    lhs <- XY(rep(-90, length(lat)), rev(lat))
    rhs <- XY(rep(90, length(lat)), rev(lat))
    ## polygon p will be drawn with the 'lit' colo
    if (phase > 0.5) {
        xy <- XY(rep(-180 + shadowLongitude, length(lat)), lat)
        p <- data.frame(x=c(xy$x, lhs$x), y=c(xy$y, lhs$y))
    } else {
        xy <- XY(rep(shadowLongitude, length(lat)), lat)
        p <- data.frame(x=c(xy$x, rhs$x), y=c(xy$y, rhs$y))
    }
    polygon(p, col=lit)
    if (write)
        write.table(round(p, 5), sprintf("phase_%.4f.dat", phase), row.names=FALSE)
    if (nchar(text) > 0)
        text(0, 0, text, cex=0.8)
    ## mtext(side=1, sprintf("%.2f %.0f", phase-floor(phase), shadowLongitude))
}

