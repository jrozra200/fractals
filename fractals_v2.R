## FRACTALS V2

## LEVEL 0: DRAW ONE LINE @ STARTING ANGLE | 0X

## LEVEL 1: DRAW TWO LINES @ STARTING ANGLE + 60 (line 1) & + 120 (line 2) | 1X/2X (+1) | S/S+X (+1)

## LEVEL 2A: DRAW TWO LINES @ STARTING ANGLE + 120 (line 1) & + 180 (line 2) | 2X/3X (+1) | S/S+X (+1)
## LEVEL 2B: DRAW TWO LINES @ STARTING ANGLE + 0 (line 2) & + 60 (line 4) | 1X/0X (-1) | S-X/S-2X (-1)

## LEVEL 3A: DRAW TWO LINES @ STARTING ANGLE + 180 (line 1) & + 60 (line 2) | 3X/4X (+1) | S/S+X (+1)
## LEVEL 3B: DRAW TWO LINES @ STARTING ANGLE + 60 (line 2) & + 120 (line 4) | 2X/4X (+2) | S-X/S+X (+2)
## LEVEL 3C: DRAW TWO LINES @ STARTING ANGLE + 60 (line 1) & + 120 (line 2) | 4X/2X (-2) | S+X/S-X (-2)
## LEVEL 3D: DRAW TWO LINES @ STARTING ANGLE + 120 (line 2) & + 0 (line 4) | 0X/-1X (-1) | S-3X/S-4X (-1)

library(ggplot2)
library(dplyr)

how_deep <- 3

## LEVEL 0: DRAW ONE LINE @ STARTING ANGLE

startx <- 0
starty <- 0
sidelength <- 1
degrees <- 0
level <- 0
triangle <- 0
twofer <- 0
line <- 1
data_points <- data.frame(level = level,
                          triangle = triangle,
                          twofer = twofer,
                          line = line,
                          degrees = degrees,
                          startx = startx,
                          starty = starty,
                          x = c(startx, 
                                startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                                startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                startx + (cos(degrees * pi / 180) * sidelength)),
                          y = c(starty, 
                                starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                                starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                starty + (sin(degrees * pi / 180) * sidelength)),
                          point = c(1:4))

for(level in 1:how_deep){
    sidelength <- sidelength / 3
    for(triangle in 1:(level)){
        for(twofer in 1:2 ^ (triangle - 1)){
            for(line in 1:2){
                startx <- ifelse((twofer %% 2) == 0,
                                 data_points$x[data_points$level == (level - 1) & 
                                                   data_points$line == twofer & 
                                                   data_points$point == (4 - line)],
                                 data_points$x[data_points$level == (level - 1) & 
                                                   data_points$line == twofer & 
                                                   data_points$point == (line + 1)])
                starty <- ifelse((twofer %% 2) == 0,
                                 data_points$y[data_points$level == (level - 1) & 
                                                   data_points$line == twofer & 
                                                   data_points$point == (4 - line)],
                                 data_points$y[data_points$level == (level - 1) & 
                                                   data_points$line == twofer & 
                                                   data_points$point == (line + 1)])
                degrees <- ifelse((twofer %% 2) == 0,
                                  180 - unique(data_points$degrees[data_points$level == level & 
                                                                       data_points$line == ifelse(line == 2, 1, 2) & 
                                                                       data_points$twofer == (twofer - 1)]),
                                  data_points$degrees[data_points$level == (level - 1) & 
                                                          data_points$line == twofer & 
                                                          data_points$point == (line + 1)] + 
                                      (60 * line))
                
                tmp <- data.frame(level = level,
                                  triangle = triangle,
                                  twofer = twofer,
                                  line = line,
                                  degrees = degrees,
                                  startx = startx,
                                  starty = starty,
                                  x = c(startx, 
                                        startx + (cos(degrees * pi / 180) * (sidelength / 3)), 
                                        startx + (cos(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                        startx + (cos(degrees * pi / 180) * sidelength)),
                                  y = c(starty, 
                                        starty + (sin(degrees * pi / 180) * (sidelength / 3)), 
                                        starty + (sin(degrees * pi / 180) * ((2 * sidelength) / 3)), 
                                        starty + (sin(degrees * pi / 180) * sidelength)),
                                  point = c(1:4))
                data_points <- rbind(data_points, tmp)
            }
        }
    }
}

ggplot(data_points, aes(x, y)) + 
    geom_point()

data_points
