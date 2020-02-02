## Test calendar drawing
source("calendar.R") # also sources drawMoonR

png("calendar_2020_jan.png")
calendar(2020, 1)
dev.off()
png("calendar_2020_feb.png")
calendar(2020, 2)
dev.off()

