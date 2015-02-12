#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector speedDist(NumericMatrix trip) {
    
    double distance;
    int nrow = trip.nrow();
    NumericVector dist(nrow-1);
    for (int i=1; i<nrow; i++) {
        distance = sqrt(pow((trip(i,0)-trip(i-1,0)),2.0) + pow((trip(i,1)-trip(i-1,1)),2.0));
        dist[i-1] = distance;
    }
    return dist;
}