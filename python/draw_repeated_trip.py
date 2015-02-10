########################
# developed by rcarson
# aixueer4ever@gmail.com
########################
import os
import numpy as np
import pandas as pd
from random import random,randrange
import matplotlib.pyplot as plt
from math import pi
import sys
  
class draw_repeated_trip:
    def __init__(self,dataPath,driver=None,threshold=0.05):   
        self.dataPath=dataPath
        self.driver=driver
        self.threshold=threshold
    # flip a trip       
    def flip(self,x):
        if x[x[:,1]>0,1].shape[0]>x.shape[0]/2:
            x[:,1]=x[:,1]*(-1)
        return x 
    # get the degree to rotate      
    def getrw(self,a):
        (d,w)=self.getwd(a)
        return -w
    def rotate_one_point(self,a,w0):
        (d,w)=self.getwd(a)
        return self.getxy((d,w+w0))
    def rotate_trip(self,trip):
        xx=trip.shape[0]
        a=[i for i in trip.xs(xx-1)]
        w0=self.getrw(a)
        xx=[]
        for j in range(trip.shape[0]):
            a=[i for i in trip.xs(j)]
            xx.append(self.rotate_one_point(a,w0))
        return np.array(xx)
     # get x,y coordinate    
    def getxy(self,a):
        (d,w)=a
        return d*np.cos(w),d*np.sin(w)
    # get r,w coordinate
    def getwd(self,a):
        [x,y]=a
        if x>0:
            return (x**2+y**2)**0.5,np.arctan(y/x)
        elif x<0:
            if y>0:
                return (x**2+y**2)**0.5,pi+np.arctan(y/x)
            elif y<0:
                return (x**2+y**2)**0.5,-pi+np.arctan(y/x)
            else:
                return x,pi
        else:
            if y>0:
                return y,pi/2
            elif y<0:
                return y,-pi/2
            else:
                return 0,0
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
        while tx.shape[0]>ty.shape[0]:
            ty=np.vstack((ty,ty[-1,:]))
        while tx.shape[0]<ty.shape[0]:
            tx=np.vstack((tx,tx[-1,:]))
        dd=tx-ty
        return np.sum((dd[:,0]**2+dd[:,1]**2)**0.5)
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
        print 'driver',driver
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
        print c, 'repeated trips'

        plt.show()
          

if __name__ == "__main__":
    path=sys.argv[1] # ../../data/drivers
    driver=sys.argv[2]
    if len(sys.argv)==4:
        dd=draw_repeated_trip(dataPath=path,driver=driver,threshold=float(sys.argv[3]))
    else:
        dd=draw_repeated_trip(dataPath=path,driver=driver)
    dd.draw()

