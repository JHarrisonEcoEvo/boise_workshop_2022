# NOTE: This is a very simple example of how to implement a random forest that I
# made like 6 or 7 years ago. It leaves much to be desired, but serves the
# purpose of being very simple and providing a good place to start.
# Note the lack of strong cross-validation or hyper-parameter tuning or
# any sort of rigourous feature selection.

#Rqndom Forest analysis of MeSa data

#get rid of sci notation, bc it is annoying
options(scipen=999)

#install.packages("randomForest")
library(randomForest)

#set seed for reproducible results
set.seed(1627)

#load data
dat = read.csv("~/Downloads/countDataEG.csv")

#scale and center all predictor variables
dat[,3:length(dat)] = scale(dat[,3:length(dat)], center=T, scale=T)		

#if you needed to impute you could use this.
# Looks like there are some NAs so we will impute here
dat2 = rfImpute(x=dat[,3:length(dat)],
                y=dat$count) 

##################################
#try to predict butterfly abundance
##################################

#here we are not subsetting data into training/validation but just using all of it
#lots more parameters to mess with in the rF function, but the main one to mess with is the mtry and nodesize parameters

rf = randomForest(
	x = dat2[,4:length(dat2)], 							#these are the predictors
	y = dat$count,						#response
	importance = T,						#save variable importance metrics
	ntree=4000,							#number of trees to make
	nodesize = 1,						#this is the number of rows of data in the terminal node, defaults at 5 for regression, but I lowered bc we don't have much data here
	mtry = length(dat2)/3				#this is m, the number of predictors to consider at each split. The default for regression is p/3 (which I just spelled out here for clarity. p is the number of predictors by convention)
	);rf
	
		
	#output shows m (the number of variables tried per split; m is notation used by convention)
	#confirms the use of regression vs classification
	#$ variation explained is percent variation explained in the out of bag data. This is not technically the same as R2, but for practical purposes it gets the same point across. It is actually this: 1- (sum(yi - yhati)^2/(sum(yi-ybar)^2)
	#this is why it can be negative when the model sucks
	#for proper notation see: http://stats.stackexchange.com/questions/7357/manually-calculated-r2-doesnt-match-up-with-randomforest-r2-for-testing
	
	#NOTE:normaally we would want to try predicting to validation data here to confirm model performance, instead of relying solely on OOB performance.
	

	
#variable importance plot, even though model sucked it is still cool that winter was most imprnt.
varImpPlot(rf, sort=T)	
#left panel shows %IncMSE which is the increase in mean squared error when a predictor variable is permutted in a tree, and the OOB data walked down the tree (for all trees). IncNodePurity gives a measure of how many wrong classifications are included in a node...so a purer node means it does a better job splitting the data.



#################
#Partial plots and prediction
#################


#gets predicted values, since we are doing a reg forest
out = predict(rf) #this lets you compare predicted vs. observed

#out <- predict(rf,test,'response') #this is the syntax you would use if you wanted to predict to new data...just put new data frame in place of "test"..response just stays response so that it knows to generate yhat


plot(out, dat$count) #yikes, terrible

#this gives error unless you add a constant. Not sure why, it didnt on other dataset which also had negative values. I think the error is bc before I had more data b4 so it didnt have try to mix negative and positive data in a terminal node
 
partialPlot(rf,
	pred.data = dat2,					#training data
	x.var = dat2[,5])	#variable for which u want to see the partial plot


#################
#interaction plots
#################

#for description of interaction ability of random forests see:
#"Wright, Marvin N., Andreas Ziegler, and Inke R. König.
#"Do little interactions get lost in dark random forests?." BMC bioinformatics 17.1 (2016): 145."

#there are two ways to do this in this script. The first uses the ICEbox package
#(https://arxiv.org/pdf/1309.6392.pdf) which is sort of cool, the second uses the
#plotmo package which makes the 3d plots

 #install.packages("plotmo")
 #install.packages("ICEbox")
library(plotmo)
library(ICEbox)

#with ice plot
nice = ice(object  = rf, 
			X = dat2[,4:length(dat2)], 
			y = dat$count, 
			predictor = "winter_avg_mintemp")

plot(nice) #if we see a bunch of line crossings then it would signal the
#presence of an interaction bc we would get different yhats for different x's based on some other unseen variable

#can add colors too to make it easier to look for interactions, 
#use this to plot colors based on a different variable that might be the interactor of interest


colorizer = function(x){
	#Create a function to generate a continuous color palette
	rbPal <- colorRampPalette(c('red','blue'))
	
	#This adds a column of color values
	# based on the y values
	return(rbPal(length(x))[as.numeric(cut(x,breaks = length(x)))])
}

Col = colorizer(dat2$winter_avg_mintemp) #input is whatever you want color ramp for

#for instance here we can look for interaction between winter mintemp and winterprecip
nice = ice(object  = rf, 
			X = dat2[,4:length(dat2)], 
			y = dat$count, 
			predictor = "winter_avg_mintemp")

plot(nice, 
	centered = T, 
	colorvec = colorizer(dat2$winter_avg_precip)
	)#so we just colorized by a different variable.


#plot partial derivative. See the link to the vignette above for a better description of this.
#Basically all curves should have same shape if no interactions are present.

dice(nice)
plot(dice(nice)) 

####plotmo method

#defaults give all the top variables and possible interations
plotmo(rf)

#to pick a specific subplot use this:
plotmo(rf, 
	trace=1, 	#gives output on what is happening, can change for more info
	all1=T, 	#Default is FALSE. Use TRUE to plot all predictors, not just those usually selected by plotmo. 
	degree2=6, 	#tell it which interaction to pick. 
	degree1=F, 	#turn off partial plots
	)

#####################################

# Build a classification model 

data(iris)

#set seed for reproducible results
set.seed(1627)

##################################
#try to predict iris species by traits
##################################
data(iris)
rf = randomForest(
  x = iris[,1:4], 							#these are the predictors
  y = iris$Species,						#response
  importance = T,						#save variable importance metrics
  ntree=4000,							#number of trees to make
  nodesize = 1,						#this is the number of rows of data in the terminal node, defaults at 5 for regression, but I lowered bc we don't have much data here
  mtry = 2				#this is m, the number of predictors to consider at each split. The default for regression is p/3 (which I just spelled out here for clarity. p is the number of predictors by convention)
);rf


