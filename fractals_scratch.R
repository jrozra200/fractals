library(dplyr)
library(ggplot2)
### START FRESH
how_deep <- 2

startx <- 0
starty <- 0
sidelength <- 1
degrees <- 0
line <- 1
start_line <- 1
data_points <- data.frame(point = c(1, 2, 1, 2),
                          line = line,
                          start_line = start_line,
                          segment = c(1, 1, 2, 2),
                          n = 1,
                          i = 0,
                          depth = 0,
                          side = 0,
                          degrees = degrees,
                          degrees_in = 0,
                          x = c(startx, 
                                startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                                startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                startx + (cos(degrees * pi / 180) * sidelength)),
                          y = c(starty, 
                                starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                                starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                starty + (sin(degrees * pi / 180) * sidelength)))
for(i in 1:how_deep){
        degrees <- degrees + 60
        n <- 1
        sidelength <- sidelength / 3
        
        if(i < 2){
                for(depth in 1:2^(i - 1)){
                        
                        for(side in 1:2){
                                startx <- ifelse(side == 1,
                                                 data_points$x[data_points$line == start_line & 
                                                                       data_points$segment == 1 & 
                                                                       data_points$point == 2],
                                                 data_points$x[data_points$line == line &
                                                                       data_points$segment == 2 & 
                                                                       data_points$point == 2])
                                starty <- ifelse(side == 1,
                                                 data_points$y[data_points$line == start_line & 
                                                                       data_points$segment == 1 & 
                                                                       data_points$point == 2],
                                                 data_points$y[data_points$line == line &
                                                                       data_points$segment == 2 & 
                                                                       data_points$point == 2])
                                line <- line + 1
                                
                                degrees_in <- case_when(
                                        4 - (depth %% 4) == 3 ~ ifelse(side == 1,
                                                                       degrees,
                                                                       degrees - 120),
                                        4 - (depth %% 4) == 4 ~ ifelse(side == 1,
                                                                       -1 * (degrees - 120),
                                                                       -1 *degrees),
                                        1 == 1 ~ ifelse(side == 1,
                                                        degrees - 120,
                                                        degrees - 240)
                                )
                                
                                
                                tmp <- data.frame(point = c(1, 2, 1, 2),
                                                  line = line,
                                                  start_line = start_line,
                                                  segment = c(1, 1, 2, 2),
                                                  n = n,
                                                  i = i,
                                                  depth = depth,
                                                  side = side,
                                                  degrees = degrees,
                                                  degrees_in = degrees_in,
                                                  x = c(startx, 
                                                        startx + (cos(degrees_in * pi / 180) * (sidelength / 3)), 
                                                        startx + (cos(degrees_in * pi / 180) * ((2 * sidelength) / 3)), 
                                                        startx + (cos(degrees_in * pi / 180) * sidelength)),
                                                  y = c(starty, 
                                                        starty + (sin(degrees_in * pi / 180) * (sidelength / 3)), 
                                                        starty + (sin(degrees_in * pi / 180) * ((2 * sidelength) / 3)), 
                                                        starty + (sin(degrees_in * pi / 180) * sidelength)))
                                data_points <- rbind(data_points, tmp)
                                n <- n + 1
                        }
                        start_line <- start_line + 1  
                }
        } else {
                for(triangles in 1:(2^(i - 1) / 4)){
                        for(depth in 1:4){
                                
                                for(side in 1:2){
                                        startx <- ifelse(side == 1,
                                                         data_points$x[data_points$line == start_line & 
                                                                               data_points$segment == 1 & 
                                                                               data_points$point == 2],
                                                         data_points$x[data_points$line == line &
                                                                               data_points$segment == 2 & 
                                                                               data_points$point == 2])
                                        starty <- ifelse(side == 1,
                                                         data_points$y[data_points$line == start_line & 
                                                                               data_points$segment == 1 & 
                                                                               data_points$point == 2],
                                                         data_points$y[data_points$line == line &
                                                                               data_points$segment == 2 & 
                                                                               data_points$point == 2])
                                        line <- line + 1
                                        
                                        degrees_in <- case_when(
                                                4 - (depth %% 4) == 3 ~ ifelse(side == 1,
                                                                               degrees,
                                                                               degrees - 120),
                                                4 - (depth %% 4) == 4 ~ ifelse(side == 1,
                                                                               -1 * (degrees - 120),
                                                                               -1 *degrees),
                                                1 == 1 ~ ifelse(side == 1,
                                                                degrees - 120,
                                                                degrees - 240)
                                        )
                                        
                                        
                                        tmp <- data.frame(point = c(1, 2, 1, 2),
                                                          line = line,
                                                          start_line = start_line,
                                                          segment = c(1, 1, 2, 2),
                                                          n = n,
                                                          i = i,
                                                          depth = depth,
                                                          side = side,
                                                          degrees = degrees,
                                                          degrees_in = degrees_in,
                                                          x = c(startx, 
                                                                startx + (cos(degrees_in * pi / 180) * (sidelength / 3)), 
                                                                startx + (cos(degrees_in * pi / 180) * ((2 * sidelength) / 3)), 
                                                                startx + (cos(degrees_in * pi / 180) * sidelength)),
                                                          y = c(starty, 
                                                                starty + (sin(degrees_in * pi / 180) * (sidelength / 3)), 
                                                                starty + (sin(degrees_in * pi / 180) * ((2 * sidelength) / 3)), 
                                                                starty + (sin(degrees_in * pi / 180) * sidelength)))
                                        data_points <- rbind(data_points, tmp)
                                        n <- n + 1
                                }
                                start_line <- start_line + 1  
                        }
                }
        }
        
}


ggplot(data_points, aes(x, y)) + 
        geom_point()

data_points


###################################


how_deep <- 3

startx <- 0
starty <- 0
sidelength <- 1
degrees <- 0
line <- 1
start_line <- 1
tri_at_level <- 0
data_points <- data.frame(point = c(1, 2, 1, 2),
                          line = line,
                          start_line = start_line,
                          segment = c(1, 1, 2, 2),
                          n = 1,
                          tri_at_level = tri_at_level,
                          i = 0,
                          depth = 0,
                          side = 0,
                          degrees = degrees,
                          degrees_in = 0,
                          x = c(startx, 
                                startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                                startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                startx + (cos(degrees * pi / 180) * sidelength)),
                          y = c(starty, 
                                starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                                starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                starty + (sin(degrees * pi / 180) * sidelength)))

for(i in 1:how_deep){
        print(paste("outside: ", i))
        sidelength <- sidelength / 3
        tri_at_level <- 1
        degrees <- degrees + 60
        n <- 1
        for(depth in 1:2^(i-1)){
                print(paste("insdide: ", depth))
                for(side in 1:2){
                        print(paste("deep insdide: ", side))
                        
                        startx <- data_points$x[data_points$line == start_line & 
                                                        data_points$segment == ifelse(side %% 2 == 0, 2, 1) & 
                                                        data_points$point == ifelse(side %% 2 == 0, 1, 2)]
                        starty <- data_points$y[data_points$line == start_line & 
                                                        data_points$segment == ifelse(side %% 2 == 0, 2, 1) & 
                                                        data_points$point == ifelse(side %% 2 == 0, 1, 2)]
                        line <- line + 1
                        
                        degrees_in <- ifelse((depth %% 2) == 1,
                                             ifelse(side == 1, degrees, 
                                                    degrees + 60),
                                             ifelse(side == 1, degrees + 240,
                                                    degrees + 180))
                        degrees_in <- ifelse(side == 2, 
                                             60 + unique(data_points$degrees_in[data_points$side == 1 & data_points$line == line - 1]),
                                             degrees - ((depth - 1) * 60))
                        
                        tmp <- data.frame(point = c(1, 2, 1, 2),
                                          line = line,
                                          start_line = start_line,
                                          segment = c(1, 1, 2, 2),
                                          n = n,
                                          tri_at_level = tri_at_level,
                                          i = i,
                                          depth = depth,
                                          side = side,
                                          degrees = degrees,
                                          degrees_in = degrees_in,
                                          x = c(startx, 
                                                startx + (cos(degrees_in * pi / 180) * (sidelength / 3)), 
                                                startx + (cos(degrees_in * pi / 180) * ((2 * sidelength) / 3)), 
                                                startx + (cos(degrees_in * pi / 180) * sidelength)),
                                          y = c(starty, 
                                                starty + (sin(degrees_in * pi / 180) * (sidelength / 3)), 
                                                starty + (sin(degrees_in * pi / 180) * ((2 * sidelength) / 3)), 
                                                starty + (sin(degrees_in * pi / 180) * sidelength)))
                        data_points <- rbind(data_points, tmp)
                        n <- n + 1
                }
                start_line <- start_line + 1  
                tri_at_level <- tri_at_level + 1
        }
}

ggplot(data_points, aes(x, y)) + 
        geom_point()

data_points$line <- data_points$line - 1

data_points

## ONE SINGLE SIDE
startx <- 0
starty <- 0
sidelength <- 1
degrees <- 0
line <- 1
data_points <- data.frame()

## SPLIT LINE INTO THREE PARTS

tmp <- data.frame(point = c(1, 2, 1, 2),
                  line = line,
                  segment = c(1, 1, 2, 2),
                  x = c(startx, 
                        startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                        startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        startx + (cos(degrees * pi / 180) * sidelength)),
                  y = c(starty, 
                        starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                        starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        starty + (sin(degrees * pi / 180) * sidelength)))
data_points <- rbind(data_points, tmp)

ggplot(data_points, aes(x, y)) + 
        geom_point()

##########################

startx <- data_points$x[data_points$line == line & 
                                data_points$segment == 1 & 
                                data_points$point == 2]
starty <- data_points$y[data_points$line == line & 
                                data_points$segment == 1 & 
                                data_points$point == 2]
sidelength <- sidelength / 3
degrees <- degrees + 60
line <- line + 1

tmp <- data.frame(point = c(1, 2, 1, 2),
                  line = line,
                  segment = c(1, 1, 2, 2),
                  x = c(startx, 
                        startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                        startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        startx + (cos(degrees * pi / 180) * sidelength)),
                  y = c(starty, 
                        starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                        starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        starty + (sin(degrees * pi / 180) * sidelength)))
data_points <- rbind(data_points, tmp)

ggplot(data_points, aes(x, y)) + 
        geom_point()

line <- line + 1
startx <- data_points$x[data_points$line == line - 2 & 
                                data_points$segment == 2 & 
                                data_points$point == 1]
starty <- data_points$y[data_points$line == line - 2 & 
                                data_points$segment == 2 & 
                                data_points$point == 1]

tmp <- data.frame(point = c(1, 2, 1, 2),
                  line = line,
                  segment = c(1, 1, 2, 2),
                  x = c(startx, 
                        startx + (cos((180 - degrees) * pi / 180) * (sidelength / 3)), 
                        startx + (cos((180 - degrees) * pi / 180) * ((2 * sidelength) / 3)), 
                        startx + (cos((180 - degrees) * pi / 180) * sidelength)),
                  y = c(starty, 
                        starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                        starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        starty + (sin(degrees * pi / 180) * sidelength)))
data_points <- rbind(data_points, tmp)

ggplot(data_points, aes(x, y)) + 
        geom_point()

#####################

startx <- data_points$x[data_points$line == line - 1 & 
                                data_points$segment == 1 & 
                                data_points$point == 2]
starty <- data_points$y[data_points$line == line - 1 & 
                                data_points$segment == 1 & 
                                data_points$point == 2]
sidelength <- sidelength / 3
degrees <- degrees + 60
line <- line + 1

tmp <- data.frame(point = c(1, 2, 1, 2),
                  line = line,
                  segment = c(1, 1, 2, 2),
                  x = c(startx, 
                        startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                        startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        startx + (cos(degrees * pi / 180) * sidelength)),
                  y = c(starty, 
                        starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                        starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                        starty + (sin(degrees * pi / 180) * sidelength)))
data_points <- rbind(data_points, tmp)

ggplot(data_points, aes(x, y)) + 
        geom_point()

line <- line + 1
startx <- data_points$x[data_points$line == line - 3 & 
                                data_points$segment == 2 & 
                                data_points$point == 1]
starty <- data_points$y[data_points$line == line - 3 & 
                                data_points$segment == 2 & 
                                data_points$point == 1]

tmp <- data.frame(point = c(1, 2, 1, 2),
                  line = line,
                  segment = c(1, 1, 2, 2),
                  x = c(startx, 
                        startx + (cos((degrees - 300) * pi / 180) * (sidelength / 3)), 
                        startx + (cos((degrees - 300) * pi / 180) * ((2 * sidelength) / 3)), 
                        startx + (cos((degrees - 300) * pi / 180) * sidelength)),
                  y = c(starty, 
                        starty + (sin((degrees - 300) * pi / 180) * (sidelength / 3)), 
                        starty + (sin((degrees - 300) * pi / 180) * ((2 * sidelength) / 3)), 
                        starty + (sin((degrees - 300) * pi / 180) * sidelength)))
data_points <- rbind(data_points, tmp)

ggplot(data_points, aes(x, y)) + 
        geom_point()

#############################




## DRAW A EQUILATERAL TRIANGLE
sidelength <- 1

starting_tri <- data.frame(corner = c(1, 2, 3),
                           x = c(0, sidelength, sidelength / 2),
                           y = c(0, 0, (sidelength * tan(60 * pi / 180)) / 2))

side <- data.frame(side = c(1, 2, 3),
                   starting_corner = c(1, 2, 1),
                   ending_corner = c(2, 3, 3))

points <- data.frame(x = starting_tri$x,
                     y = starting_tri$y, 
                     middle_third_x_min = NA,
                     middle_third_x_max = NA,
                     middle_third_y_min = NA,
                     middle_third_y_max = NA)

for(si in side$side){
        startx <- starting_tri$x[starting_tri$corner == side$starting_corner[side$side == si]]
        endx <- starting_tri$x[starting_tri$corner == side$ending_corner[side$side == si]]

        starty <- starting_tri$y[starting_tri$corner == side$starting_corner[side$side == si]]
        endy <- starting_tri$y[starting_tri$corner == side$ending_corner[side$side == si]]
        
        iterationx <- (endx - startx) / 1000
        iterationy <- (endy - starty) / 1000
        
        middle_third_x_min <- min(startx + ((endx - startx) / 3), 
                                  startx + (2 * (endx - startx) / 3))
        middle_third_x_max <- max(startx + ((endx - startx) / 3), 
                                  startx + (2 * (endx - startx) / 3))
        
        middle_third_y_min <- min(starty + ((endy - starty) / 3), 
                                  starty + (2 * (endy - starty) / 3))
        middle_third_y_max <- max(starty + ((endy - starty) / 3), 
                                  starty + (2 * (endy - starty) / 3))
        
        for(i in 1:1000){
                if(i == 1){
                        newx <- startx + iterationx
                        newy <- starty + iterationy
                } else {
                        newx <- newx + iterationx
                        newy <- newy + iterationy
                }
                
                if((newx > middle_third_x_min & newx < middle_third_x_max) | 
                   (newy > middle_third_y_min & newy < middle_third_y_max)) {
                        next
                } else {
                        tmp <- data.frame(x = newx, y = newy, 
                                          middle_third_x_min = middle_third_x_min,
                                          middle_third_x_max = middle_third_x_max,
                                          middle_third_y_min = middle_third_y_min,
                                          middle_third_y_max = middle_third_y_max)
                        points <- rbind(points, tmp)
                }
        }
}

ggplot(points, aes(x, y)) + 
        geom_point(shape = ".") + 
        scale_y_continuous(limits = c(0, 1))

holes <- unique(points[, 3:6])
holes <- holes[!is.na(holes$middle_third_x_min), ]
holes$side <- c(1, 2, 3)

starting_tri <- data.frame(corner = c(1, 2, 3),
                           x = c(0, sidelength, sidelength / 2),
                           y = c(0, 0, (sidelength * tan(60 * pi / 180)) / 2))

for(i in dim(holes)[1]){
        startx <- starting_tri$x[starting_tri$corner == side$starting_corner[side$side == si]]
        endx <- starting_tri$x[starting_tri$corner == side$ending_corner[side$side == si]]
        
        starty <- starting_tri$y[starting_tri$corner == side$starting_corner[side$side == si]]
        endy <- starting_tri$y[starting_tri$corner == side$ending_corner[side$side == si]]
        
        iterationx <- (endx - startx) / 1000
        iterationy <- (endy - starty) / 1000
        
        middle_third_x_min <- min(startx + ((endx - startx) / 3), 
                                  startx + (2 * (endx - startx) / 3))
        middle_third_x_max <- max(startx + ((endx - startx) / 3), 
                                  startx + (2 * (endx - startx) / 3))
        
        middle_third_y_min <- min(starty + ((endy - starty) / 3), 
                                  starty + (2 * (endy - starty) / 3))
        middle_third_y_max <- max(starty + ((endy - starty) / 3), 
                                  starty + (2 * (endy - starty) / 3))
        
        
        
        outside_point_y <- ifelse(i == 1, )
        
        starting_tri <- data.frame(corner = c(1, 2, 3),
                                   x = c(0, sidelength, sidelength / 2),
                                   y = c(0, 0, (sidelength * tan(60 * pi / 180)) / 2))
        
        side <- data.frame(side = c(1, 2, 3),
                           starting_corner = c(1, 2, 1),
                           ending_corner = c(2, 3, 3))
}

ggplot(points, aes(x, y)) + 
        geom_point(shape = ".") + 
        scale_y_continuous(limits = c(0, 1))


### GET IT STARTED
startx <- 0
endx <- 1

endy <- 0
starty <- 0

iterationx <- (endx - startx) / 1000
iterationy <- (endy - starty) / 1000

middle_third_x_min <- startx + ((endx - startx) / 3)
middle_third_x_max <- startx + (2 * (endx - startx) / 3)

middle_third_y_min <- starty + ((endy - starty) / 3)
middle_third_y_max <- starty + (2 * (endy - starty) / 3)

points <- data.frame(x = startx,
                     y = starty)
for(i in 1:1000){
        if(i == 1){
                newx <- startx + iterationx
                newy <- starty + iterationy
        } else {
                newx <- newx + iterationx
                newy <- newy + iterationy
        }
        
        if(newx > middle_third_x_min & newx < middle_third_x_max) {
                next
        } else {
                tmp <- data.frame(x = newx, y = newy)
                points <- rbind(points, tmp)
        }
}

### THEN GO FROM THERE - STARTING TO SEE THE RECURSIVE PATTERNS, BUT DON'T HAVE 
### MY HEAD WRAPPED AROUND IT YET

startx <- middle_third_x_min
endx <- middle_third_x_min + (middle_third_x_max - middle_third_x_min) / 2

starty <- middle_third_y_min
endy <- tan(60 * pi / 180) / 2

iterationx <- (endx - startx) / 1000
iterationy <- (endy - starty) / 1000

middle_third_x_min <- startx + ((endx - startx) / 3)
middle_third_x_max <- startx + (2 * (endx - startx) / 3)

middle_third_y_min <- starty + ((endy - starty) / 3)
middle_third_y_max <- starty + (2 * (endy - starty) / 3)

for(i in 1:1000){
        if(i == 1){
                newx <- startx + iterationx
                newy <- starty + iterationy
        } else {
                newx <- newx + iterationx
                newy <- newy + iterationy
        }
        
        if(newx > middle_third_x_min & newx < middle_third_x_max) {
                next
        } else {
                tmp <- data.frame(x = newx, y = newy)
                points <- rbind(points, tmp)
        }
}

xdist <- endx - startx
startx <- endx
endx <- startx + xdist

ydist <- endy - starty
starty <- endy
endy <- starty - ydist

iterationx <- (endx - startx) / 1000
iterationy <- (endy - starty) / 1000

middle_third_x_min <- startx + ((endx - startx) / 3)
middle_third_x_max <- startx + (2 * (endx - startx) / 3)

middle_third_y_min <- starty + ((endy - starty) / 3)
middle_third_y_max <- starty + (2 * (endy - starty) / 3)

for(i in 1:1000){
        if(i == 1){
                newx <- startx + iterationx
                newy <- starty + iterationy
        } else {
                newx <- newx + iterationx
                newy <- newy + iterationy
        }
        
        if(newx > middle_third_x_min & newx < middle_third_x_max) {
                next
        } else {
                tmp <- data.frame(x = newx, y = newy)
                points <- rbind(points, tmp)
        }
}


ggplot(points, aes(x, y)) + 
        geom_point(shape = ".") + 
        scale_y_continuous(limits = c(0, 1))
