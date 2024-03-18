library(lubridate)
library(oce)
library(shiny)

#' Moon rotation angle, from ch13 of Meeus book
#'
#' @param time
#' @param longitude
#' @param latitude
#' @references
#' * https://en.wikipedia.org/wiki/Position_of_the_Sun
#' * http://www.jgiesen.de/elevaz/basics/meeus.htm
moonRotationAngle <- function(t, longitude, latitude)
{
    to360 <- function(x)
    {
        while(x < 0)
            x <- x + 360
        x
    }
    testCase <- t == as.POSIXct("1992-04-12 00:00:00", tz="UTC")
    k <- pi / 180
    sa <- sunAngle(t, longitude=longitude, latitude=latitude)
    da <- sunDeclinationRightAscension(t, apparent=TRUE)
    delta0 <- da$declination
    alpha0 <- da$rightAscension
    if (testCase) {
        ## why do these not match up to all 4 digits after the decimal?
        expect_equal(alpha0, 20.6579, tol=0.0004, scale=1)
        expect_equal(delta0,  8.6964, tol=0.0003, scale=1)
    }
    ma <- moonAngle(t, longitude=longitude, latitude=latitude)
    alpha <- ma$rightAscension
    delta <- ma$declination
    if (testCase) {
        ## why do these not match so poorly?
        expect_equal(alpha, 134.6885, tol=0.0200, scale=1)
        expect_equal(delta,  13.7684, tol=0.0050, scale=1)
    }
    chi <- 1/k*atan2(cos(k*delta0) * sin(k*(alpha0-alpha)),
                     sin(k*delta0)*cos(k*delta) - cos(k*delta0)*sin(k*delta)*cos(k*(alpha0-alpha)))
    chi <- to360(chi)
    if (testCase) {
        expect_equal(chi, 285.0, tol=0.1, scale=1)
    }
    chi
}

#' Rotate vectors
#'
#' @param p numeric data frame with `x` and `y` coordinates.
#' @param angle numeric value giving the rotation angle in
#' degrees, positive for clockwise rotation.
#'
#' @return A data frame with rotated coordinates
#'
#' @examples
#' x <- 0:5
#' y <- 0:5
#' plot(x, y, asp=1, xlim=c(-7,7), ylim=c(-7,7))
#' xyr <- rotateXY(x, y, 45)
#' points(xyr$x, xyr$y, col=2, pch=20)
#' legend("topleft", pch=c(1,20), col=c(1,2), legend=c("Unrotated", "Rotated with angle=45"))
rotateXY <- function(p, angle=0)
{
    x <- p$x
    y <- p$y
    xy <- cbind(x, y)
    angle <- angle * pi / 180
    m <- cbind(c(cos(angle), sin(angle)), c(-sin(angle), cos(angle)))
    xyr <- xy %*% m
    data.frame(x=xyr[,1], y=xyr[,2])
}
#' Draw the moon showing bright and shadow regions
#'
#' @param phase numeric value, ranging continuously from
#' 0 for the (dark) new moon, 1/4 for quarter moon (right side
#' lit), 1/2 for full moon, 3/4 for three-quarter moon (left
#' side lit, and 1 for new moon again.
#'
#' @param phase numeric value, giving the rotation angle, in degrees
#' clockwise.
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
#'
#' @examples
#' drawMoon(phase=0.15, angle=15)
drawMoon <- function(phase, angle=0,
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
    XY <- function(longitude, latitude, R=1)
    {
        longitude <- to180(longitude)
        lambda <- pi * longitude / 180
        phi <- pi * latitude / 180
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
    polygon(rotateXY(p, angle), col=lit)
    if (write)
        write.table(round(p, 5), sprintf("phase_%.4f.dat", phase), row.names=FALSE)
    if (nchar(text) > 0)
        text(0, 0, text, cex=0.8)
    ## mtext(side=1, sprintf("%.2f %.0f", phase-floor(phase), shadowLongitude))
}


debug <- FALSE # set to TRUE while developing
msg <- function(...) if (debug) cat(file = stderr(), ...)
colMoon <- rgb(245 / 255, 199 / 255, 16 / 255)
colMoon <- rgb(200 / 255, 150 / 255, 10 / 255)
colSun <- "red"

locationsText <- "
name;lon;lat;tz
Halifax, Canada;-63.61;44.67;America/Halifax
Chennai, India;80.26;13.08;Asia/Kolkata
Resolute, Canada;-94.83;74.70;America/Resolute
San Francisco;-122.3973;37.8030;America/Los_Angeles
Aranuka Atoll;-173.6295;0.1905;Pacific/Tarawa
Chuuk, FSM;-151.8470;7.4467;Pacific/Chuuk
Tok, Alaska;-142.9856;63.3367;America/Anchorage
Saavedra, Chile;-73.4;-38.78306;America/Santiago"
locations <- read.delim(text = locationsText, sep = ";", header = TRUE, stringsAsFactors = FALSE)
locations <- locations[order(locations$name), ]
halifax <- grep("Halifax, Canada", locations$name)

year0 <- as.integer(format(Sys.Date(), "%Y"))
day0 <- as.integer(format(Sys.Date(), "%j"))

ui <- pageWithSidebar(
    headerPanel("Sun and moon sky traces"),
    sidebarPanel(
        sliderInput("yearOffset",
            "Year Offset",
            min = -65, max = 5, value = 0, step = 1
        ),
        sliderInput("dayOfYear",
            "Day of Year",
            min = 1, max = 366, value = day0, step = 1
        ),
        selectInput("location",
            "Location",
            choices = locations$name, selected = locations$name[halifax]
        ),
        width = 4
    ),
    mainPanel(plotOutput("sunMoonPlot"))
)

server <- function(input, output) {
    output$sunMoonPlot <- renderPlot(
        {
            angles <- function(day = Sys.Date(), tz = "UTC", lon = -63.61, lat = 44.67, sun = TRUE) {
                tlocal <- seq(as.POSIXct(paste(day, "00:00:00"), tz = tz), length.out = 24 * 60, by = "1 min")
                tUTC <- with_tz(tlocal, "UTC")
                a <- if (sun) sunAngle(tUTC, lon = lon, lat = lat) else moonAngle(tUTC, lon = lon, lat = lat)
                invisible <- a$altitude < 0
                a$altitude[invisible] <- NA
                a$azimuth[invisible] <- NA
                if (sun) {
                    a$illuminatedFraction <- rep(NA, length(tlocal))
                } else {
                    a$illuminatedFraction[invisible] <- NA
                }
                list(tlocal = tlocal, tUTC = tUTC, altitude = a$altitude, azimuth = a$azimuth, illuminatedFraction = a$illuminatedFraction)
            }

            day <- as.POSIXct(paste(input$yearOffset + year0, "-01-01 00:00:00", sep = ""), tz = "UTC") + (input$dayOfYear - 1) * 86400
            w <- which(input$location == locations$name)
            #lon <- locations$lon[w]
            #lat <- locations$lat[w]
            ## message("location ", lat, "N, ", lon, "E")
            m <- angles(day = day, tz = locations$tz[w], lon = locations$lon[w], lat = locations$lat[w], sun = FALSE)
            s <- angles(day = day, tz = locations$tz[w], lon = locations$lon[w], lat = locations$lat[w], sun = TRUE)
            par(mar = rep(0.5, 4))
            theta <- seq(0, 2 * pi, length.out = 24 * 10)
            radiusx <- cos(theta)
            radiusy <- sin(theta)

            ## Horizon and labels+lines for EW and NS
            plot(radiusx, radiusy, type = "l", col = "gray", asp = 1, axes = FALSE, xlab = "", ylab = "")
            lines(c(-1, 1), c(0, 0), col = "gray")
            lines(c(0, 0), c(-1, 1), col = "gray")
            D <- 1.06
            text(0, -D, "S", xpd = TRUE) # xpd so can go in margin
            text(-D, 0, "W", xpd = TRUE)
            text(0, D, "N", xpd = TRUE)
            text(D, 0, "E", xpd = TRUE)

            ## Moon trace
            mx <- (90 - m$altitude) / 90 * cos(pi / 180 * (90 - m$azimuth))
            my <- (90 - m$altitude) / 90 * sin(pi / 180 * (90 - m$azimuth))
            #DANmoon <<- data.frame(mx = mx, my = my)
            lines(mx, my, col = colMoon, lwd = 4)
            ## Moon labels
            mlt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz = locations$tz[w])
            ti <- unlist(lapply(mlt, function(X) which.min(abs(X - m$tlocal))))
            ok <- abs(m$tlocal[ti] - mlt) < 10 # avoid overdrawing endpoints
            points(mx[ti][ok], my[ti][ok], pch = 20, cex = 3, col = "white")
            text(mx[ti][ok], my[ti][ok], (1:23)[ok], cex = 3 / 4)

            ## Sun trace
            sx <- (90 - s$altitude) / 90 * cos(pi / 180 * (90 - s$azimuth))
            sy <- (90 - s$altitude) / 90 * sin(pi / 180 * (90 - s$azimuth))
            #DANsun <<- data.frame(sx = sx, sy = sy)
            lines(sx, sy, col = colSun, lwd = 4)
            ## Sun labels
            slt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz = locations$tz[w])
            #si <- unlist(lapply(slt, function(X) which.min(abs(X - s$tlocal))))
            ok <- abs(s$tlocal[ti] - slt) < 10 # avoid overdrawing endpoints
            points(sx[ti][ok], sy[ti][ok], pch = 20, cex = 3, col = "white")
            text(sx[ti][ok], sy[ti][ok], (1:23)[ok], cex = 3 / 4)
            ## Location/time legend in top-left corner
            mtext(
                paste(locations$name[w],
                    paste(abs(locations$lon[w]),
                        if (locations$lon[w] < 0) "W " else "E ",
                        abs(locations$lat[w]),
                        if (locations$lat[w] < 0) "S" else "N",
                        sep = ""
                    ),
                    format(day, "%b %d, %Y"),
                    sep = "\n"
                ),
                side = 3, adj = 0, line = -3
            )
            ## Trace-colour legend in top-right corner
            if (any(is.finite(s$altitude))) {
                mtext("Sun Trajectory", side = 3, adj = 1, line = -1, col = colSun, font = 2)
            } else {
                mtext("Sun below horizon", side = 3, adj = 1, line = -1, col = colSun)
            }
            if (any(is.finite(m$altitude))) {
                mtext("Moon Trajectory", side = 3, adj = 1, line = -2, col = colMoon, font = 2)
            } else {
                mtext("Moon below horizon", side = 3, adj = 1, line = -2, col = colMoon)
            }
            ## Check for eclipse, equinox, and solstice (but only if sun and moon are visible)
            if (any(is.finite(m$azimuth)) || any(is.finite(s$azimuth))) {
                ## Eclipse based on sun-moon distance
                mismatch <- sqrt((m$azimuth - s$azimuth)^2 + (m$altitude - s$altitude)^2)
                if (any(is.finite(mismatch))) {
                    mismatch[s$altitude < 0] <- NA # concentrate on visible sky
                    nearestApproachIndex <- which.min(mismatch)
                    if (length(nearestApproachIndex)) {
                        if (mismatch[nearestApproachIndex] <= 0.54) { # sun diameter 0.54deg
                            mtext(sprintf("ECLIPSE at %s", format(s$tlocal[nearestApproachIndex], "%H:%M")),
                                side = 3, adj = 1, line = -3
                            )
                        }
                    }
                }
                ## Equinox
                sunriseAzimuth <- s$azimuth[head(which(is.finite(s$azimuth)), 1)]
                ## message("next is sunriseAzimuth:");print(sunriseAzimuth)
                sunsetAzimuth <- s$azimuth[tail(which(is.finite(s$azimuth)), 1)]
                ## message("next is sunsetAzimuth:");print(sunsetAzimuth)
                dev <- 0.5 * (abs(sunriseAzimuth - 90) + abs(sunsetAzimuth - 270))
                ## message("next is dev:");print(dev)
                if (dev < 0.3) {
                    mtext("Equinox", side = 3, adj = 1, line = -4, font = 2)
                }
                if (debug) {
                    mtext(sprintf("Sunrise azimuth %.1fdeg", sunriseAzimuth), side = 3, adj = 1, line = -5)
                    mtext(sprintf("Sunset azimuth %.1fdeg", sunsetAzimuth), side = 3, adj = 1, line = -6)
                    mtext(sprintf("dev %.2fdeg", dev), side = 3, adj = 1, line = -7)
                }
                ## Solstice
                earthObliquity <- 23.43669 # https://en.wikipedia.org/wiki/Axial_tilt as of 2019-12-29
                noonSolarAltitude <- max(s$altitude, na.rm = TRUE)
                msg(vectorShow(noonSolarAltitude))
                winterSolsticeSolarAltitude <- 90 - (locations$la[w] + earthObliquity)
                msg(vectorShow(winterSolsticeSolarAltitude))
                summerSolsticeSolarAltitude <- 90 - (locations$lat[w] - earthObliquity)
                msg(vectorShow(summerSolsticeSolarAltitude))
                if (abs(noonSolarAltitude - winterSolsticeSolarAltitude) < 0.00155) {
                    mtext("Winter Solstice", side = 3, adj = 1, line = -4, font = 2)
                }
                if (abs(noonSolarAltitude - summerSolsticeSolarAltitude) < 0.002) {
                    mtext("Spring Solstice", side = 3, adj = 1, line = -4, font = 2)
                }
            }
            ## Draw moon-phase diagram, with margin text for % illuminated
            par(new = TRUE, mar = c(2, 25, 15, 1))
            noon <- as.POSIXct(paste(format(day, "%Y-%m-%d"), "12:00:00"), tz = "UTC")
            ma <- moonAngle(noon)
            drawMoon(ma$phase, lit = colMoon)
            mtext(paste0(
                "Moon ", round(100 * ma$illuminatedFraction),
                "% full\nat 12:00 UTC"
            ), cex = 0.8, col = colMoon, font = 2)
        },
        pointsize = 16
    )
}

shinyApp(ui, server)
