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
C     ECMWF local GRIB use definition 18.
C
C     Multi-analysis ensemble data.
C     -----------------------------
C
C    KSEC1
C   element
C   -------
C
C     37       ECMWF local GRIB use definition identifier
C              18 = Multi-analysis ensemble data
C
C     38       Class  :  1 = Operations
C                        2 = Research
C
C     39       Type   :  2 = Analysis
C                        9 = Forecast
C                       10 = Control forecast
C
C     40       Stream : 1037 = multi-analysis ensemble data (MAED)
C                       1083 = multi-analysis wave data (MAWV)
C
C     41       Expver : Version number/experiment identifier.
C                       ( 4 Ascii characters, right justified)
C
C     42       Number : Ensemble forecast number:
C                       = 0 for a control forecast.
C                       Not used for analysis (set to zero).
C
C     43       Total  : Total number of forecasts in ensemble.
C                       (Set to 1 for analysis).
C
C     44       Data origin :
C                       = WMO identifier of centre providing the analysis
C                       = 255 for a consensus product
C
C     45       Model  : four-character identifier of forecast used
C                       = "ECMF" for ECMWF IFS
C                       = WMO centre CCCC identifier otherwise
C                       ( 4 Ascii characters, right justified)
C
C     46       Consensus count : indicates the composition of the starting
C                                analysis for a product (analysis, forecast, ..)
C                       = 0 for a product from one centre
C                           (ie not a consensus product)
C                       = n for a consensus product, where n is the number of
C                           analyses used in the consensus analysis.
C                           (nb. This allows the case n = 1.)
C
C     47 - 61  List   : WMO centre CCCC identifiers for the analyses used,
C                       eg "KWBC". ( 4 Ascii characters, right justified)
C                       Unused list entries are set to four blanks.
C                      
C
C
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C
