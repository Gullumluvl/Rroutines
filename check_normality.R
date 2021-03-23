# Function to check if data is following normal distribution, and prints a few
# plots

check.norm  <- function(D, data.name="") {
	write(paste("data:", data.name, "(", deparse(substitute(D)), ")") , stdout())
	write(paste("data class:", data.class(D)), stdout())
	if (data.class(D) != "numeric") {
		write("Converting data to numeric", stdout())
		D <- as.numeric(D)
	}	
	S = summary(D)
	print(S)
	sd = sd(D, na.rm=T)
	write(paste("standard dev.:", sd), stdout())
	
	sht <- shapiro.test(D)
	write(paste(sht$method, " p-val =", sht$p.value), stdout())
	dev.new()
	oldpar <- par()
	layout(matrix(c(1,2,1,2), 2, 2))
	plot(density(D, na.rm=T), col='red', main=paste("Distribution of",
								data.name, "VS normal distribution"))
	
	x <- seq(S["Min."], S["Max."], length=500)
	y <- dnorm(x, mean=S["Mean"], sd=sd)
	points(x,y, type="l")
	
	qqnorm(D)
	qqline(D)
	write("---", stdout())
	par(oldpar)
}


