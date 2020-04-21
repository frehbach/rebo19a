The supplementary material of this work includes the following Figures:

-convergencePlots.pdf
	Convergence plots for the parallel BO approaches on all 24 BBOB function + Robot application
	CMAES is implemented with the python package “cma” and call in R with the R-package “reticulate”
	Q-EI is implemented with the R-package “DiceOptim”
	MOI is implemented with the R-package “mlrbo”
	MK is implemented using the R-package “SPOT” 
	The experiments for the BBOB functions are run from left to right on 5, 10 and 20 input-dimensions.
	The y-Axis shows the best so far achieved objective function value on a logarithmic scale. 
	The center line indicates the median of the algorithm repeats. 
	The surrounding ribbon marks the lower and upper quartiles.
-rankedBoxPlots.pdf
	Boxplots for the parallel BO approaches in selected evaluation steps on all 24 BBOB function + Robot applications
	The rank of each algorithm is included on the right side of each Boxplot in red
	Kruskal-Wallis rank sum test is used to determine if a significant different exist between algorithms. 
	A post-hoc test according to Conover is used for multiple pairwise comparison.
	The Ranks obtained from the multiple pairwise comparison tests are shown on the boxplots
	The y-Axis shows the best so far achieved objective function value on a logarithmic scale. 
	The experiments for the BBOB functions are run from left to right on 5, 10 and 20 input-dimensions.
-SingleCoreConvergencePlots.pdf
	Convergence plots for the single-core BO approaches on all 24 BBOB function + Robot application
	The experiments for the BBOB functions are run from left to right on 5, 10 and 20 input-dimensions.
	All single-core BO are implemented using the R-package “SPOT” 
	CMAES is implemented with the python package “cma” and call in R with the R-package “reticulate”
	The y-Axis shows the best so far achieved objective function value on a logarithmic scale. 
	The center line indicates the median of the algorithm repeats. 
	The surrounding ribbon marks the lower and upper quartiles.
-SingleCoreRankedBoxPlots.pdf
	Boxplots for the single-core BO approaches in selected evaluation steps on all 24 BBOB function + Robot applications
	The rank of each algorithm is included on the right side of each Boxplot in red
	Kruskal-Wallis rank sum test is used to determine if a significant different exist between algorithms. 
	A post-hoc test according to Conover is used for multiple pairwise comparison.
	The Ranks obtained from the multiple pairwise comparison tests are shown on the boxplots
	The y-Axis shows the best so far achieved objective function value on a logarithmic scale. 
	The experiments for the BBOB functions are run from left to right on 5, 10 and 20 input-dimensions.	