### christmas_tree.R ##############################################################################
# BPG implememtation of a Christmas tree - with legend!


### PREAMBLE ######################################################################################
library(BoutrosLab.plotting.general);

# NOTE: quasi-random number generation also loads randtoolbox

options(
	stringsAsFactors = FALSE, 
	error = function() traceback(2) 
	); 

### PARAMETERS ####################################################################################

height <- 8;
width <- (height/8)*5;

# measure of how dense the leaf part of the tree should be
steps <- 130;

# stem 
stem.height <- height/8;
stem.width <- width/6;
stem.n <- 280; 


# number of decorations generated 
# Note: we only keep ones that fall on the tree, so the final number will be lower
n.decor <- 80;

# Method of generating decorations: quasi-random or pseudo-random.
# pseudo-random samples from a uniform distribution, quasi-random spreads the decorations out more.
decoration.method <- 'pseudo-random'; 


# point sizes
leaf.cex <- 0.75;
decoration.cex <- 1.5;


# colours to sample from for each of the components of the tree
leaf.colours <- c('#0F3B00', '#2B7314', '#207F00');
bark.colours <- c('#451700', '#592D17');
decoration.colours <- c('#F0BE0B', '#CC1D0C'); 

# probability of sampling each of the decoration colours. 
# Useful if you want more decorations of one colour
decoration.prob <- c(0.7, 0.3);

### FORMAT PARAMETERS #############################################################################

if( !(decoration.method %in% c('pseudo-random', 'quasi-random')) ) {
	stop('decoration.method must be either pseudo-random or quasi-random.');
}

stepheight <- height/steps;

# slope of tree
slope <- -(width/2)/height;

# combine colours for legend
combined.colours <- c(
	leaf.colours, 
	bark.colours, 
	decoration.colours
	);

# define labels for legend
colour.labels <- c(
	rep('leaves', length(leaf.colours)), 
	rep('bark', length(bark.colours)), 
	rep('ornament', length(decoration.colours))
	);

### tree.height ###################################################################################
# Description:
#	Calculates max height of the tree for a given x-coordinate
# Input variables:
#	x   x-coordinate in question
#	max.height 		height of tree at its highest
#	max.width 		width of tree at its widest
# Output variable:
#	max.y 			height of tree at x-coordinate
tree.height <- function(x, max.height = height, max.width = width) {

	max.y <- max.height - sign(x)*(2*max.height/max.width)*x;

	return(max.y);
}

### MAKE LEAVES ###################################################################################
# Cone shaped green base of tree.
   
leaf.data <- data.frame();
  
for(i in seq(0, height, by=stepheight)) {

	l <- -(width/(2*height))*(-height + i); # half-width given height
    n <- 40*l; 
    

    x  <- runif(
    	n, 
    	-l, 
    	l
    	);

    y <- runif(
    	n, 
    	i, 
    	i + stepheight
    	);


    colour <- sample(
    	leaf.colours, 
    	size = n, 
    	replace = TRUE
    	);
    	

    temp.data <- data.frame(x, y, colour)
    
    leaf.data <- rbind(leaf.data, temp.data);
  }

# add size of points
leaf.data$size <- leaf.cex;


### MAKE STEM #####################################################################################

x <- runif(stem.n, -stem.width/2, stem.width/2);
y <- runif(stem.n, -stem.height, 0);
  
stem.data <- data.frame(
	x,
	y, 
	colour = sample(
		bark.colours, 
		size = stem.n,
		replace = TRUE
		),
	size = rep(0.75, stem.n)
	);


### DECORATE TREE #################################################################################
# Christmas discussions of quasi-random vs pseudo-random numbers are highly encouraged!

if('quasi-random' == decoration.method) {
	library(randtoolbox); # needed for Sobol sequence

	# get low-discrepancy sequence
	numbers <- sobol(n.decor, 2);

	# convert to x- and y-coordinates
	x.decor <- width*numbers[, 1] - width/2;
	y.decor <- height*numbers[, 2]

} else if('pseudo-random' == decoration.method) {

	x.decor <- runif(
		n.decor, 
		-width/2, 
		width/2
		);

	y.decor <- runif(
		n.decor, 
		0, 
		height
		);
}
 
decoration.data <- data.frame(
	x = x.decor, 
	y = y.decor, 
	colour = sample(
		decoration.colours, 
		prob = c(0.7, 0.3),
		n.decor, 
		replace = TRUE
		), 
	size = rep(decoration.cex, n.decor)
	);
  
# limit decorations to those that fall on the tree
decoration.data <- subset(decoration.data, y < tree.height(x));
 

### LEGEND ########################################################################################

tree.legend.grob <- draw.key(
	key = list(
		points = list(
			pch = 19, 
			col = combined.colours,
			cex = 1.1
			),
		text = list(
			lab = colour.labels
			), 
		cex.title = 1,
		between = 0.7
		)
	);


### ASSEMBLE FINAL PLOT ###########################################################################

combined.data <- rbind(leaf.data, stem.data, decoration.data);

tree.plot <- create.scatterplot(
	y ~ x, 
	combined.data, 
	# technicalities
	filename = 'tree.png',
	resolution = 300,
	height = 8,
	# points
	col = combined.data$colour,
	cex = combined.data$size,
	# x-axis
	xaxis.cex = 0.8,
	xlab.label = '', 
	# y-axis
	yaxis.cex = 0.8,
	ylab.label = 'Happy Holidays!',
	ylab.cex = 1.4,
	legend = list(
		right = list(
			fun = tree.legend.grob
			)
		)
	);





  