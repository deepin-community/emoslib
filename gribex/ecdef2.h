C
C Copyright 1981-2016 ECMWF.
C
C This software is licensed under the terms of the Apache Licence 
C Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
C
C In applying this licence, ECMWF does not waive the privileges and immunities 
C granted to it by virtue of its status as an intergovernmental organisation 
C nor does it submit to any jurisdiction.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
C                   ECMWF local GRIB use definition 2.
C                  Cluster means and standard deviations.
C                   ----------------------------------
C
C         Words 38-41 as for definition 1.
C
C                    42 Number : Cluster number.
C
C                    43 Total  : Total number of clusters.
C
C                    44 Clustering method :-
C                               1 - Maximum linkage method
C                               2 - Mixed method
C                               3 - Small linkage method
C
C                    45 Start time step considered when clustering
C                       (Same units of time as forecast timesteps)
C
C                    46 End time step considered when clustering
C                       (Same units of time as forecast timesteps)
C
C                    47 Northern latitude of domain of clustering
C                    48 Western longitude of domain of clustering
C                    49 Southern latitude of domain of clustering
C                    50 Eastern longitude of domain of clustering
C                       (See Notes 1-4 below)
C
C                    51       : Number of cluster to which operational
C                               forecast belongs.
C
C                    52       : Number of cluster to which control
C                               forecast belongs.
C
C                    53   N   : Number of forecasts belonging to
C                               the cluster , including the
C                               control forecast.
C
C                 54-53+N     : List of N ensemble forecast numbers.
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
