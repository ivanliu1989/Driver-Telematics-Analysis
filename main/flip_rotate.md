1. Rotate: Let last point to x-axis & towards north
	- a = Last Point
	- w0 = arctan2(a) #theta
	- rotation matrix: np.array([[np.cos(theta),-np.sin(theta)],[np.sin(theta),np.cos(theta)]])
	- trip_data * rotation matrix

2. Flip: if majority point over x-axis, then flip	
	- sum sign of y coordinator
	- rotation matrix: [1,0],[0,-1]
	- trip_data * rotation matrix

3. 