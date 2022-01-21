
x <- seq(1:4)
y <- seq(1:4)

data <- expand.grid(x,y)
colnames(data) <- c("x","y")
data$z <- c(2,1,2,3,
            3,2,3,4,
            4,5,5,6,
            6,8,8,7)
data$z <- c(3,2,1,2,
            4,3,2,3,
            6,5,5,4,
            7,8,8,6) # opposite
data$z <- c(3,2,1,2,
            4,3,2,3,
            6,5,5,4,
            5,6,6,4) # last row smaller
plot(data$x, data$y, cex = 4)
text(y ~ x, labels=z, data=data, cex=0.9, font=2, col = "blue")

coef <- lm(data$z ~ data$x + data$y)$coef[2:3]

angle <- rad2deg(atan2(coef[1],coef[2]))
if(angle < 0) {angle = angle + 360}
if((360 - angle + 90)>360) {angle <- (360 - angle + 90 - 360)}
if((360 - angle + 90)<360) {angle <- (360 - angle + 90)}
angle <- as.numeric(angle)

mag <- sqrt((1/coef[1])^2+(1/coef[2])^2)
(mag <- as.numeric(mag))

x1 <- 2 + mag * cos(angle * pi / 180)
y1 <- 1 + mag * sin(angle * pi / 180)
segments(2, 1, as.numeric(x1), as.numeric(y1), lwd = 2)

x2 <- 2 + 1 * cos(110 * pi / 180)
y2 <- 1 + 1 * sin(110 * pi / 180)
segments(2, 1, as.numeric(x2), as.numeric(y2), lwd = 2)

angmax <- max(angle,110)
angmin <- min(angle,110)
anglem <- (abs(angmax - angmin)/2) + angmin
magm <- sum(mag,1)/2

xm <- 2 + magm * cos(anglem * pi / 180)
ym <- 1 + magm * sin(anglem * pi / 180)
segments(2, 1, as.numeric(xm), as.numeric(ym), lwd = 2, col = "red")

x3 <- 2 + 1.5 * cos(25 * pi / 180)
y3 <- 1 + 1.5 * sin(25 * pi / 180)
segments(2, 1, as.numeric(x3), as.numeric(y3), lwd = 2)

angv <- c(angle,110,25)
angm <- sum(angv)/length(angv)

magv <- c(mag,1,1.5)
magm <- sum(magv)/length(magv)

xm2 <- 2 + magm * cos(angm * pi / 180)
ym2 <- 1 + magm * sin(angm * pi / 180)
segments(2, 1, as.numeric(xm2), as.numeric(ym2), lwd = 2, col = "green")

## different cos and sins ---------------------------
x <- seq(1:4)
y <- seq(1:4)

data <- expand.grid(x,y)
colnames(data) <- c("x","y")
data$z <- c(2,1,2,3,
            3,2,3,4,
            4,5,5,6,
            6,8,8,7)
plot(data$x, data$y, cex = 4)
text(y ~ x, labels=z, data=data, cex=0.9, font=2)

coef <- lm(data$z ~ data$x + data$y)$coef[2:3]

angle <- rad2deg(atan2(coef[1],coef[2]))
if(angle < 0) {angle = angle + 360}
if((360 - angle + 90)>360) {angle <- (360 - angle + 90 - 360)}
if((360 - angle + 90)<360) {angle <- (360 - angle + 90)}
angle <- as.numeric(angle)

mag <- sqrt((1/coef[1])^2+(1/coef[2])^2)
mag <- as.numeric(mag)

x1 <- 2 + mag * cos(angle)
y1 <- 1 + mag * sin(angle)
segments(2, 1, as.numeric(x1), as.numeric(y1), lwd = 2)

x2 <- 2 + 1 * cos(110)
y2 <- 1 + 1 * sin(110)
segments(2, 1, as.numeric(x2), as.numeric(y2), lwd = 2)

angmax <- max(angle,110)
angmin <- min(angle,110)
anglem <- (abs(angmax - angmin)/2) + angmin
magm <- sum(mag,1)/2

xm <- 2 + magm * cos(anglem)
ym <- 1 + magm * sin(anglem)
segments(2, 1, as.numeric(xm), as.numeric(ym), lwd = 2, col = "red")

x3 <- 2 + 1.5 * cos(25)
y3 <- 1 + 1.5 * sin(25)
segments(2, 1, as.numeric(x3), as.numeric(y3), lwd = 2)

angv <- c(angle,110,25)
angm <- sum(angv)/length(angv)

magv <- c(mag,1,1.5)
magm <- sum(magv)/length(magv)

xm2 <- 2 + magm * cos(angm)
ym2 <- 1 + magm * sin(angm)
segments(2, 1, as.numeric(xm2), as.numeric(ym2), lwd = 2, col = "green")

## different values - bigger and going left!  -------------------
x <- seq(1:4)
y <- seq(1:4)
data <- expand.grid(x,y)
colnames(data) <- c("x","y")
data$z <- c(3,1,2,2,
            4,5,2,2,
            7,5,4,3,
            9,8,5,5)
plot(data$x, data$y, cex = 4)
text(y ~ x, labels=z, data=data, cex=0.9, font=2)

coef <- lm(data$z ~ data$x + data$y)$coef[2:3]

angle <- rad2deg(atan2(coef[1],coef[2]))
if(angle < 0) {angle = angle + 360}
if((360 - angle + 90)>360) {angle <- (360 - angle + 90 - 360)}
if((360 - angle + 90)<360) {angle <- (360 - angle + 90)}
(angle <- as.numeric(angle))

mag <- sqrt((1/coef[1])^2+(1/coef[2])^2)
mag <- as.numeric(mag)

x1 <- 2 + mag * cos(angle * pi / 180)
y1 <- 1 + mag * sin(angle * pi / 180)
segments(2, 1, as.numeric(x1), as.numeric(y1), lwd = 2)

x2 <- 2 + 1 * cos(110 * pi / 180)
y2 <- 1 + 1 * sin(110 * pi / 180)
segments(2, 1, as.numeric(x2), as.numeric(y2), lwd = 2)

angmax <- max(angle,110)
angmin <- min(angle,110)
anglem <- (abs(angmax - angmin)/2) + angmin
magm <- sum(mag,1)/2

xm <- 2 + magm * cos(anglem * pi / 180)
ym <- 1 + magm * sin(anglem * pi / 180)
segments(2, 1, as.numeric(xm), as.numeric(ym), lwd = 2, col = "red")

x3 <- 2 + 1.5 * cos(25 * pi / 180)
y3 <- 1 + 1.5 * sin(25 * pi / 180)
segments(2, 1, as.numeric(x3), as.numeric(y3), lwd = 2)

angv <- c(angle,110,25)
angm <- sum(angv)/length(angv)

magv <- c(mag,1,1.5)
magm <- sum(magv)/length(magv)

xm2 <- 2 + magm * cos(angm * pi / 180)
ym2 <- 1 + magm * sin(angm * pi / 180)
segments(2, 1, as.numeric(xm2), as.numeric(ym2), lwd = 2, col = "green")





