### snowman_card.R ################################################################################
# BPG implementation of a snowman.

### PREAMBLE ######################################################################################
library(BoutrosLab.plotting.general);
library(MASS); # needed for multivariate normal

options(stringsAsFactors = FALSE);

### PARAMETERS ####################################################################################

# snowball radii
radii <- c(3, 2.5, 2.1);

# expected number of dots per unit cubed
density <- 200;

# measure of how far apart snowballs should be
d <- 0.2

# number of points in main part of hat and rim
hat.n <- 2000;
rim.n <- 200

# colours
snow.colours <- paste0('gray', 97:100);
hat.colours <- c(paste0('gray', 1:10), '#042B01', '#064001');
nose.colour <- 'firebrick';

### sd.from.radius ################################################################################
# Description: 
#   Calculate standard deviation required to receive desired radius. Not a scientific formula, but 
#   chosen because it gives roughly the right radius.
#  Input variable:
#   radius    desired radius
#  Output variable:
#   sd   standard deviation
sd.from.radius <- function(radius) {

  sd <- (radius^2)/4;

  return(sd);
}

### ASSEMBLE DATA #################################################################################

# calculate areas
areas <- pi*radii^2;

# calculate snowball centre coordinates
mu.x <- c(0, 0, 0);
mu.y <- c(0, sum(radii[1:2]) + d, sum(radii) + radii[2] + 1.8*d);


snowball.parameters <- data.frame(
  mu.x = c(0, 0, 0),
  mu.y = mu.y,
  sd = sapply(radii, sd.from.radius),
  n = round(areas*density)
  );


### MAKE SNOWBALLS ################################################################################

snowball.coordinates <- apply(
  snowball.parameters,
  1,
  function(parameter.row) {
    
    # get number of points
    n <- parameter.row['n'];
    
    # get mean
    mu <- parameter.row[c('mu.x', 'mu.y')];
    
    # get standard deviation
    # make diagonal matrix for multivariate normal.
    sigma <- diag(
      parameter.row['sd'],
      2
      );
    
    points <- mvrnorm(
      n = n,
      mu = mu, 
      Sigma = sigma
      );
    
    
    return(points);
  
    }
  );

snowball.coordinates <- data.frame(
  do.call(rbind, snowball.coordinates)
  );

names(snowball.coordinates) <- c('x', 'y');

# add colour
snowball.coordinates$colour <- sample(
  snow.colours,
  size = nrow(snowball.coordinates),
  replace = TRUE
  );

# add size
snowball.coordinates$cex <- sample(
  seq(0.7, 1.0, by = 0.01),
  size = nrow(snowball.coordinates),
  replace = TRUE
  );

### MAKE EYES #####################################################################################

# distance between eyes
eye.distance <- 0.9*radii[3];

# y coordinate of eyes
eye.height <- mu.y[3] + 0.25*radii[3]; 

# assemble data
eyes <- data.frame(
  x = c( -eye.distance/2, eye.distance/2 ),
  y = rep(eye.height, 2),
  cex = rep(1.5, 2),
  colour = rep('black', 2)
  );


### MAKE NOSE ####################################################################################

nose <- data.frame(
	x = 0, 
	y = mu.y[3] - 0.2*radii[3], # aiming for somewhere slightly below middle of top snowball
	cex = 1.9, 
	colour = nose.colour
	);

### MAKE HAT ######################################################################################

hat.height <- 2.3*radii[3];
hat.width <- 2.1*radii[3];

# generate x and y coordinates for points
hat <- data.frame(
  x = runif(
    hat.n, 
    min = -hat.width/2, 
    max = hat.width/2
    ), 
  y = runif(
    hat.n, 
    min = mu.y[3] + 0.95*radii[3], 
    max = mu.y[3] + radii[3] + hat.height
    )
  );


# make rim
rim.height <- 0.25*radii[3];
rim.width <- 2.9*radii[3];

rim <- data.frame(
  x = runif(
    rim.n, 
    min = -rim.width/2, 
    max = rim.width/2
    ),
  y = runif(
    rim.n, 
    min = mu.y[3] + 0.92*radii[3], 
    max = mu.y[3] + radii[3] + rim.height
    )
  );

# combine into single data frame
hat <- rbind(hat, rim);

# add colour
hat$colour <- sample(
	hat.colours,
	size = hat.n + rim.n,
	replace = TRUE
	);
  
# add size
hat$cex <- 1.2;


### COMBINE DATA FRAMES ###########################################################################

snowman.data <- rbind(
	snowball.coordinates, 
	eyes,
	nose,
	hat
	);


### PLOT ##########################################################################################
# Finally, Frosty can be assembled!

frosty <- create.scatterplot(
  y ~ x, 
  snowman.data,
  col = snowman.data$colour,
  cex = snowman.data$cex,
  # technicalities
  filename = 'frosty.png',
  resolution = 300, 
  height = 8,
  width = 5,
  # x-axis
  xlimits = c(-7, 7),
  xaxis.cex = 0.9,
  xlab.label = '',
  # y-axis
  yaxis.cex = 0.9,
  ylab.label = 'Happy Holidays!',
  ylab.cex = 1.4,
  ylimits = c(-6, 19),
  # background rectangle
  # unfortunately we can't see a white snowman on a white background
  add.rectangle = TRUE,
  xleft.rectangle = -20,
  xright.rectangle = 20,
  ybottom.rectangle = -20,
  ytop.rectangle = 20,
  col.rectangle = 'gainsboro'
  );









