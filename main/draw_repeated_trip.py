import os
import numpy as np
import pandas as pd
from sklearn.metrics.pairwise import paired_distances
from random import random,randrange
import matplotlib.pyplot as plt
from math import pi
import sys
  
def rotational(theta):
    # http://en.wikipedia.org/wiki/Rotation_matrix
    # Beyond rotation matrix, fliping, scaling, shear can be combined into a single affine transform
    # http://en.wikipedia.org/wiki/Affine_transformation#mediaviewer/File:2D_affine_transformation_matrix.svg
    return np.array([[np.cos(theta),-np.sin(theta)],[np.sin(theta),np.cos(theta)]])
    
class draw_repeated_trip:
    def __init__(self,dataPath,driver=None,threshold=0.05):   
        self.dataPath=dataPath
        self.driver=driver
        self.threshold=threshold 
    def flip(self,x):
        # flip a trip if more that half of coordinates have y axis value above 0 
        if np.sign(x[:,1]).sum() > 0:
            x = x.dot(np.array([[1,0],[0,-1]]))
        return x     
    def rotate_trip(self,trip):
        # take last element
        a=trip.iloc[-1]
        # get the degree to rotate  
        w0=np.arctan2(a.y,a.x) # from origin to last element angle
        # rotate using the rotational: equivalent to rotational(-w0).dot(trip.T).T
        return np.array(trip.dot(rotational(w0)))
     # get x,y coordinate    
    def getxy(self,a):
        (d,w)=a
        return d*np.cos(w),d*np.sin(w)
    # get r,w coordinate
    def getwd(self,a):
        [x,y]=a
        return np.hypot(x,y), np.arctan2(y,x)
    def update_trips(self,tripl,trip,tripname,tripcounter,tripother):
        if len(tripl)==0:
            tripl[tripname]=trip
            tripcounter[tripname]=0
            tripother[tripname]=[]
        else:
            for t in tripl:
                if self.sametrip(tripl[t],trip):
                    tripcounter[t]+=1
                    tripother[t].append(tripname)
                    for xx in tripother[t]:
                        tripcounter[xx]=tripcounter[t]
                    return tripl,tripcounter,tripother
            tripl[tripname]=trip
            tripcounter[tripname]=0
            tripother[tripname]=[]
        return tripl,tripcounter,tripother            

    def getdd(self,tx,ty):
	# calculate distances and pad shortest array
        # pad shortest array with it's last value
        gap = tx.shape[0]-ty.shape[0]
        if gap>0:
            ty=np.pad(ty, ((0,gap),(0,0)), 'edge')
        elif gap <0:
            tx=np.pad(tx, ((0,-gap),(0,0)), 'edge')
        else:
            pass
        # use any distance metric that you would like
        return paired_distances(tx,ty,metric='l2').sum()
    def sametrip(self,tx,ty):
        mm=int(tx.shape[0]*self.threshold)
        txx=np.vstack((tx[mm:,:],tx[-mm:,:]))
        dd=self.getdd(tx,txx)
        return  self.getdd(tx,ty)<=dd
    def check_trips(self,tripcounter,tripname):
        return tripcounter[tripname]>2     
    def draw(self):
        path=self.dataPath
        drivers=os.listdir(path)
        if self.driver!='-1':
           driver=self.driver
        else:
           driver=drivers[randrange(len(drivers))]
        print ('driver',driver)
        trips=os.listdir(path+'/'+driver)
        tripl={} # trip name -> trip data
        tripc={} # trip name -> number of same trips
        tripo={} # trip name -> list of names of other same trips
        for trip in trips:
            if '.csv' in trip:
                t1=pd.read_csv(path+'/'+driver+'/'+trip)
                t1r=self.flip(self.rotate_trip(t1))
                tripl,tripc,tripo=self.update_trips(tripl,t1r,trip,tripc,tripo)
        c=0
        for trip in trips:
            if '.csv' in trip:
                t1=pd.read_csv(path+'/'+driver+'/'+trip)
                plt.subplot(1,3,1)
                plt.plot(t1['x'],t1['y'])
                plt.title('all trips of driver '+driver)
                t1r=self.flip(self.rotate_trip(t1))
                if self.check_trips(tripc,trip):
                    plt.subplot(1,3,2)
                    plt.plot(t1['x'],t1['y'])
                    plt.title('repeated trips of driver '+driver)
                    plt.subplot(1,3,3)
                    plt.plot(t1r[:,0],t1r[:,1])
                    plt.title('repeated trips of driver '+driver+' after rotation')
                    c+=1
        print (c, 'repeated trips')

        plt.show()
          

if __name__ == "__main__":
    path=sys.argv[1] # ../../data/drivers
    driver=sys.argv[2]
    if len(sys.argv)==4:
        dd=draw_repeated_trip(dataPath=path,driver=driver,threshold=float(sys.argv[3]))
    else:
        dd=draw_repeated_trip(dataPath=path,driver=driver)
    dd.draw()