Here are some general findings leveraging different classification modeling approaches for only SPEED as the feature set.  Note no model parameter optimization and each base driver is contrasted to five randomly selected drivers.  Being relatively new to the machine learning field, any comments to the following is greatly appreciated:

	- Recursive Partitioning and Regression Trees [rpart() ~ 0.727] resulted in the highest score.  Is this because the approach splits the feature space into hyper-rectangles and false driver routes are like outliers in sparsely populated regions?
	- Support Vector Machine [svm() ~ 0.654] came in second. Not sure if the number of contrasting drivers influences the results as this method seems popular.  Does the call to svm() automatically create non-linear boundaries?
	- Logistic Regression [glm() ~ 0.648] came in third. Is assuming this has to do with a linear weighted combination of the features reasonable?
	- Neural Network [nnet() ~ 0.615] came in fourth.  Any guidance regarding increasing the number of inner-layers or number of iterations?
	- Linear Discriminate Analysis [lda()] wasn't pursued as my understanding is the method's underlying assumption is feature normal distribution...  not the case for quantile() features.  Reasonable?
	- K Nearest Neighbors [knn3()] was initially pursued, but maybe because I didn't leverage controls the approach classified too many false routes.  Reasonable?
	- Naive Bayes [nb()] yet to be considered...
I'm in the process of evaluating Acceleration, Centripetal Acceleration, and Heading Change feature sets...  I'll let you know how it goes ;-)