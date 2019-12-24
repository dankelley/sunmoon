library(lubridate)
library(oce)
library(shiny)

locationsText <- "
name;lon;lat;tz
Halifax, Canada;-63.61;44.67;America/Halifax
Chennai, India;80.26;13.08;Asia/Kolkata
Resolute, Canada;-94.83;74.70;America/Resolute
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
                                   width=3),
                      mainPanel(plotOutput("sunMoonPlot")))

server <- function(input, output) {
    output$sunMoonPlot <- renderPlot({
        angles <- function(day=Sys.Date(), tz="UTC", lon=-63.61, lat=44.67, sun=TRUE)
        {
            ##message("day[1]=", day[1], ", tz=", tz, ", lon=", lon, ", lat=", lat, ", sun=", sun)
            tlocal <- seq(as.POSIXct(paste(day, "00:00:00"), tz=tz), length.out=240, by="6 min")
            tUTC <- with_tz(tlocal, "UTC")
            a <- if (sun) sunAngle(tUTC, lon=lon, lat=lat) else moonAngle(tUTC, lon=lon, lat=lat)
            invisible <- a$altitude < 0
            a$altitude[invisible] <- NA
            a$azimuth[invisible] <- NA
            list(tlocal=tlocal, tUTC=tUTC, altitude=a$altitude, azimuth=a$azimuth)
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

        ## Indicate approximate orientation of Northwest Arm, of Halifax Harbour.
        if (w == 1) {
            nwaAzi <- (90-123) * pi / 180
            lines(cos(nwaAzi)*c(-1,1), sin(nwaAzi)*c(-1,1), col='gray', lwd=3)
        }

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
        mtext("Sun", side=1, adj=0, line=-3, col="red")
        mtext("Moon", side=1, adj=0, line=-2, col="blue")
        mtext("Local Hour", side=1, adj=0, line=-1)
    }, pointsize=16)
}

shinyApp(ui, server)



