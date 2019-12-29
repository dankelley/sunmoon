library(lubridate)
library(oce)
library(shiny)
debug <- FALSE                         # set to TRUE while developing
msg <- function(...) if (debug) cat(file=stderr(), ...)

locationsText <- "
name;lon;lat;tz
Halifax, Canada;-63.61;44.67;America/Halifax
Chennai, India;80.26;13.08;Asia/Kolkata
Resolute, Canada;-94.83;74.70;America/Resolute
San Francisco;-122.3973;37.8030;America/Los_Angeles
"
locations <- read.delim(text=locationsText, sep=";", header=TRUE, stringsAsFactors=FALSE)

year0 <- as.integer(format(Sys.Date(), "%Y"))
day0 <- as.integer(format(Sys.Date(), "%j"))

ui <- pageWithSidebar(headerPanel(h4("Sun and moon sky traces")),
                      sidebarPanel(sliderInput("yearOffset",
                                               "Year Offset", min=-65, max=5, value=0, step=1),
                                   sliderInput("dayOfYear",
                                               "Day of Year", min=1, max=366, value=day0, step=1),
                                   selectInput("location",
                                               "Location", choices=locations$name, selected=locations$name[1]),
                                   width=4),
                      mainPanel(plotOutput("sunMoonPlot")))

server <- function(input, output) {
    output$sunMoonPlot <- renderPlot({
        angles <- function(day=Sys.Date(), tz="UTC", lon=-63.61, lat=44.67, sun=TRUE)
        {
            tlocal <- seq(as.POSIXct(paste(day, "00:00:00"), tz=tz), length.out=24*60, by="1 min")
            tUTC <- with_tz(tlocal, "UTC")
            a <- if (sun) sunAngle(tUTC, lon=lon, lat=lat) else moonAngle(tUTC, lon=lon, lat=lat)
            invisible <- a$altitude < 0
            a$altitude[invisible] <- NA
            a$azimuth[invisible] <- NA
            if (sun) {
                a$illuminatedFraction <- rep(NA, length(tlocal))
            } else {
                a$illuminatedFraction[invisible] <- NA
            }
            list(tlocal=tlocal, tUTC=tUTC, altitude=a$altitude, azimuth=a$azimuth, illuminatedFraction=a$illuminatedFraction)
        }

        day <- as.POSIXct(paste(input$yearOffset+year0, "-01-01 00:00:00", sep=""), tz="UTC") + (input$dayOfYear - 1) * 86400
        w <- which(input$location == locations$name)
        lon <- locations$lon[w]
        lat <- locations$lat[w]
        m <- angles(day=day, tz=locations$tz[w], lon=locations$lon[w], lat=locations$lat[w], sun=FALSE)
        s <- angles(day=day, tz=locations$tz[w], lon=locations$lon[w], lat=locations$lat[w], sun=TRUE)
        par(mar=rep(0.5, 4))
        theta <- seq(0, 2*pi, length.out=24 * 10)
        radiusx <- cos(theta)
        radiusy <- sin(theta)

        ## Horizon and labels+lines for EW and NS
        plot(radiusx, radiusy, type='l', col='gray', asp=1, axes=FALSE, xlab="", ylab="")
        lines(c(-1, 1), c(0, 0), col='gray')
        lines(c(0, 0), c(-1, 1), col='gray')
        D <- 1.06
        text( 0, -D, "S", xpd=TRUE) # xpd so can go in margin
        text(-D,  0, "W", xpd=TRUE)
        text( 0,  D, "N", xpd=TRUE)
        text( D,  0, "E", xpd=TRUE)

        ## Moon trace
        mx <- (90 - m$altitude) / 90 * cos(pi / 180 * (90 - m$azimuth))
        my <- (90 - m$altitude) / 90 * sin(pi / 180 * (90 - m$azimuth))
        lines(mx, my, col='blue', lwd=3)
        ## Moon labels
        mlt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz=locations$tz[w])
        ti <- unlist(lapply(mlt, function(X) which.min(abs(X-m$tlocal))))
        ok <- abs(m$tlocal[ti] - mlt) < 10 # avoid overdrawing endpoints
        points(mx[ti][ok], my[ti][ok], pch=20, cex=3, col='white')
        text(mx[ti][ok], my[ti][ok], (1:23)[ok], cex=3/4)

        ## Sun trace
        sx <- (90 - s$altitude) / 90 *  cos(pi / 180 * (90 - s$azimuth))
        sy <- (90 - s$altitude) / 90 *  sin(pi / 180 * (90 - s$azimuth))
        lines(sx, sy, col='red', lwd=3)
        ## Sun labels
        slt <- as.POSIXct(sprintf("%s %02d:00:00", day, 1:23), tz=locations$tz[w])
        si <- unlist(lapply(slt, function(X) which.min(abs(X-s$tlocal))))
        ok <- abs(s$tlocal[ti] - slt) < 10 # avoid overdrawing endpoints
        points(sx[ti][ok], sy[ti][ok], pch=20, cex=3, col='white')
        text(sx[ti][ok], sy[ti][ok], (1:23)[ok], cex=3/4)

        ##OLD ## Indicate approximate orientation of Northwest Arm, of Halifax Harbour.
        ##OLD if (w == 1) {
        ##OLD     nwaAzi <- (90-123) * pi / 180
        ##OLD     lines(cos(nwaAzi)*c(-1,1), sin(nwaAzi)*c(-1,1), col='gray', lwd=3)
        ##OLD }

        ## Marginal legends
        mtext(paste(locations$name[w],
                    paste(abs(locations$lon[w]),
                          if (locations$lon[w] < 0) "W " else "E ",
                          abs(locations$lat[w]),
                          if (locations$lat[w] < 0) "S" else "N",
                          sep=""),
                    format(day, "%b %d, %Y"),
                    sep="\n"),
              side=3, adj=0, line=-3)
        if (any(is.finite(s$altitude))) {
            mtext("Sun", side=3, adj=1, line=-1, col="red")
        } else  {
            mtext("Sun below horizon", side=3, adj=1, line=-1, col="red")
        }
        illuminatedFraction <- round(100*mean(m$illuminatedFraction, na.rm=TRUE))
        if (any(is.finite(m$altitude))) {
            mtext(sprintf("Moon (%.0f%% full)", illuminatedFraction),
                  side=3, adj=1, line=-2, col="blue")
        } else {
            mtext("Moon below horizon", side=3, adj=1, line=-2, col="blue")
        }
        ## Do these tests only if the sun or moon are visible above the horizon, i.e.
        ## they are ignored in high-latitude winters
        if (any(is.finite(m$azimuth)) || any(is.finite(s$azimuth))) {
            ## sun-moon distance for eclipse diagnosis
            mismatch <- sqrt((m$azimuth - s$azimuth)^2 + (m$altitude - s$altitude)^2)
            if (any(is.finite(mismatch))) {
                mismatch[s$altitude < 0] <- NA # concentrate on visible sky
                nearestIndex <- which.min(mismatch)
                if (length(nearestIndex)) {
                    ## sun diameter 0.54deg
                    if (mismatch[nearestIndex] <= 0.54) {
                        mtext(sprintf("ECLIPSE at %s", format(s$tlocal[nearestIndex], "%H:%M")),
                              side=3, adj=1, line=-3)
                    }
                }
            }
            sunriseAzimuth <- s$azimuth[head(which(is.finite(s$azimuth)), 1)]
            sunsetAzimuth <- s$azimuth[tail(which(is.finite(s$azimuth)), 1)]
            dev <- 0.5 * (abs(sunriseAzimuth-90) + abs(sunsetAzimuth-270))
            if (dev < 0.3)
                mtext("Equinox", side=3, adj=1, line=-4, font=2)
            if (debug) {
                mtext(sprintf("Sunrise azimuth %.1fdeg", sunriseAzimuth), side=3, adj=1, line=-5)
                mtext(sprintf("Sunset azimuth %.1fdeg", sunsetAzimuth), side=3, adj=1, line=-6)
                mtext(sprintf("dev %.2fdeg", dev), side=3, adj=1, line=-7)
            }
        }
        ## Solstice
        earthObliquity <- 23.43669     # https://en.wikipedia.org/wiki/Axial_tilt as of 2019-12-29
        noonSolarAltitude <- max(s$altitude, na.rm=TRUE)
        msg(vectorShow(noonSolarAltitude))
        winterSolsticeSolarAltitude <- 90 - (locations$la[w] + earthObliquity)
        msg(vectorShow(winterSolsticeSolarAltitude))
        summerSolsticeSolarAltitude <- 90 - (locations$lat[w] - earthObliquity)
        msg(vectorShow(summerSolsticeSolarAltitude))
        if (abs(noonSolarAltitude - winterSolsticeSolarAltitude) < 0.00155)
            mtext("Winter Solstice", side=3, adj=1, line=-4, font=2)
        if (abs(noonSolarAltitude - summerSolsticeSolarAltitude) < 0.002)
            mtext("Spring Solstice", side=3, adj=1, line=-4, font=2)
    }, pointsize=16)
}

shinyApp(ui, server)



