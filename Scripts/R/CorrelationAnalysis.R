# The Raw Data file contains 5 columns:
# 1) "intra", equals 1 if the row belongs to intraoperative network and 0 if to postoperative network (we have 13 intra- and 13 postoperative rows)
# 2) "net_no", ranges from 1 to 13 for intra- and postoperative networks, indicating the segment number
# 3) "no_cases", indicates number of cases in the (network, segment) pair
# 4) "no_comps", indicates number of complications in the (network, segment) pair
# 5) "sw_indicator", indicates the small-world indicator value in the (network, segment) pair
# 6) "density", indicates network density value in the (network, segment) pair

setwd('path_to_rawdata_file') # setting the working directory
df <- read.csv(file="corr_data.txt", header=TRUE, sep="\t")
df$avg_comp <- df$no_comps / df$no_cases # calculating the average rate of complications

df_intra <- df[df$intra == 1,] # the intraoperative dataframe
df_post = df[df$intra == 0,] # the postoperative dataframe

features = c('avg_comp', 'sw_indicator', 'density')  # selecting the target columns for calculating the correlation

df_intra = df_intra[, features]
df_post = df_post[, features]

######
library(rrcov)
#install.packages("rrcov")

#### intraoperative network, correlation analysis ###
est <- CovRobust(df_intra)
est
round(getCorr(est),2) #getting the correlation matrix
### plotting the results
dev.off()
par(mai=c(1,1,0.52,0.42))
plot(CovRobust(df_intra), which="tolEllipsePlot", classic=TRUE)
#####################################################

#### postoperative network, correlation analysis ###
est <- CovRobust(df_post)
est
round(getCorr(est),2) #getting the correlation matrix
### plotting the results
dev.off()
par(mai=c(1,1,0.52,0.42))
plot(CovRobust(df_post), which="tolEllipsePlot", classic=TRUE)
#####################################################
