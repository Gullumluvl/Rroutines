# function showing the distribution of a variable

show.dist <- function(data, varname, groups, color=NULL, ytitle=varname, 
					  legendtitle=groups) {
	require(ggplot2)
	require(gridExtra)
	D <- data.frame(x=data[,varname], g=data[,groups])
	P1 <- ggplot(D, aes(g,x)) +
		geom_boxplot(mapping=aes(g, x, fill=g), outlier.color="transparent") +
		geom_jitter(position=position_jitter(width=.05)) +
		stat_summary(fun.y="mean", geom="point", shape=23, fill="white") +
		theme(legend.position="none",
			  axis.text.x=element_text(angle=-45, hjust=0)) +
		ylab(ytitle) + scale_x_discrete(element_blank())
	P2 <- ggplot(D) +
		geom_density(aes(x, group=g, color=g)) +
		coord_flip() +
		theme(legend.position=c(1,1),
			  legend.justification=c(1, 1),
			  axis.title.y=element_blank(),
			  axis.text.y=element_blank(), 
			  axis.ticks.y=element_blank(),
			  plot.background=element_blank()) +
		scale_color_discrete(name=legendtitle)
	if ( !is.null(color) ) {
		P1 <- P1 + scale_fill_manual(values=color)
		P2 <- P2 + scale_color_manual(name=legendtitle, values=color) #+scale_color_discrete(name=groups)
	}
	P1 <- ggplot_gtable(ggplot_build(P1))
	P2 <- ggplot_gtable(ggplot_build(P2))
	P2$heights <- P1$heights

	P <- arrangeGrob(P1, P2, nrow=1, ncol=2)
	grid.draw(P)
	return(P)
}

