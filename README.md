Driver Telematics Analysis
==========================

For this competition, Kaggle participants must come up with a "telematic fingerprint" capable of distinguishing when a trip was driven by a given driver. The features of this driver fingerprint could help assess risk and form a crucial piece of a larger telematics puzzle.

#### FINAL SOLUTION:
An ensemble model of random forest, neural networks and gradient boosting. <br>
Have also tried other models like SVMs (Linear,Radial kernel), logistic regression, naive bayes, knn and kmeans, but no improvement found. <br>
Have also implemented a very simple trip match method to gain a small boost based on basic ensemble model. More advanced trip matching method can be developed if there are more time and more capable machines provided. <br>
Private ranking: 77th/1562 (5%) <br>

#### TIPS:
1. one driver in one car
2. units of x and y - meters
3. In Telematics ignition "on" to ignition "off" is defined as a "trip"
4. driver 1 on trip 8 took a 200 meter detour and remained stopped for ~ 30 seconds or so.   (see the attached image) Can we assume that this is a person dropping their kids off at school, or getting fast coffee as opposed to a person who stops for 30 minutes, the GPS shuts down and then resumes their travel and the GPS starts up again
5. Smoothing vs. Outlier Removal 
 - https://www.kaggle.com/c/axa-driver-telematics-analysis/forums/t/11487/smoothing-vs-outlier-removal-or-how-to-deal-with-the-hyperspace-jumps
 - smoothing_methods_designed_to_minimize_the_impact_of_gps_random_error_on_travel_distance_speed_and_acceleration_profile_estimates-trr.pdf
6. Rotation and scaling of data
7. number of turns, distribution of speed / acceleration
8. I'm just using basic trigonometry. Identify the slope of the journey ( last y coord over last x coord). Then use inverse tan to calculate overall angle of travel. Then can use this to rotate all journeys to begin and end on the axis. At that stage flipping journeys becomes easy, just mutiply y coord by -1.
Can use things like length of journey, max x, max y, average y to make a reasonable stab at similar journeys.
9. * what's a straight?
* how do you define a left turn? How many degrees turning over how long a distance?
* is speed important? What about all those tiny movements when it seems like the driver is maneuvering at a parking spot?
* once you get the journeys defined as strings, how well do they have to match for you to consider them the same journey? Is 1.1km straight the same as 1.2km straight?
Driver 1, trip 1 is a very good example to try and define the turns and straights. For matching (and flipped) trips (and return trips) you can find several from driver 1 as well: 102,167,183,197,200 and 63,83,120,148 are some sets that are pretty tough.
10.Another idea that may be worth investigating is to use the various spatial packages in R.
	- convert the xy points for each journey into a SpatialLines object (package sp).
	- use the elide routine from package maptools to rotate and flip the journeys
	- use gdistance from package rgeos to measure the distance or similarity between journeys


#### ALGORITHM:
1. GBM : The train MSE is will obviously go down with the number of tree's. So should I stop at maybe 150 tree's assuming that I am over-fitting after that.
2. SVM :  Is SVM better than GBM in terms of dealing with Noise.
3. RandomForest : Is this any better than GBM for noise?
4. NNET

#### VALUABLE TRY
1. Take the false trip data (all of it, all the data from all the other drivers) and run a k-means clustering with 200 or more clusters. Now use the centers of the resulting clusters as your target=0 data for all drivers.
2. Next idea is sort of like a "by-hand" boosting algorithm but with more weight added to the 0 labels only.
Take lots of 0 target trips and use them with the drivers' 200 trips (as target=1) to train a classifier. Now predict on just your 0 target trips and drop some percentage of them that had the lowest predicted probability as target=1. Now you're left with only those trips that are closest to the decision boundary. Retrain with the drivers' original trips and these zeros only.

#### SOME GOOD RESULTS
1. speed quantile features and gradient boost method
2. speed, acceleration and curve features
3. trimming outliers worked better than averaging them out 
4. Using Speed, Acceleration, Centripetal Acceleration, Heading Change, and et al. features processed by RandomForest: 0.83578.
5. > Using randomForest produced the best result (0.88315)
> Used 5 contrasting drivers, chosen at random (found what RobRob found: increasing drivers did not improve performance: tried with 10)
> Feature set includes distance, time and %les for speed, acceleration and turn( same as heading changes as mentioned by RobRob?)
> Logistic performed far worse, best score: 0.77173, same feature set
> as for outliers, using %les probably helps eliminate them if you don't use mean values and don't include anything in the first and last 1 percent (total distance traveled is the only one to be impacted, in case of "hyper space"  jumps). 
