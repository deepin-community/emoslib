#!/bin/bash
set -eux


#./ginout_c lsmN$1 LSM_GG_0$1
##./ginout lsm.N$1 LSM_GG_0$1


#for grib in LSM_GG_0640*.grib
#do
#  out="$(basename $grib .grib)_CY41R1"
#  ./ginout_c "$grib" "$out"
#done
#
#md5sum /usr/local/apps/libemos/tables/interpolation/LSM_GG_* LSM_GG_0640* | sort


#for T in 95 159 255 319 399 511 639 799 1279 2047
#do
#  N=$(( $(grib_get -p Nj "41r1/climate.v012/${T}l_2/lsm")/2 ))
#  in="41r1/climate.v012/${T}l_2/lsm"
#  out="LSM_GG_"$(printf "%04d" $N)"_CY41R1"
#  ./ginout_c "$in" "$out"
#done


./ginout_c2 40/climate/1023l_2/lsm            40/LSM_GG_0510
./ginout_c2 40/climate/1279l_2/lsm            40/LSM_GG_0640
./ginout_c2 40/climate/159l_2/lsm             40/LSM_GG_0080
./ginout_c2 40/climate/2047l_2/lsm            40/LSM_GG_1024
./ginout_c2 40/climate/255l_2/lsm             40/LSM_GG_0128
./ginout_c2 40/climate/319l_2/lsm             40/LSM_GG_0160
./ginout_c2 40/climate/3999l_2/lsm            40/LSM_GG_2000
./ginout_c2 40/climate/399l_2/lsm             40/LSM_GG_0200
./ginout_c2 40/climate/511l_2/lsm             40/LSM_GG_0256
./ginout_c2 40/climate/639l_2/lsm             40/LSM_GG_0320
./ginout_c2 40/climate/799l_2/lsm             40/LSM_GG_0400
./ginout_c2 40/climate/95l_2/lsm              40/LSM_GG_0048
./ginout_c2 41r1/climate.v012/1279l_2/lsm   41r1/LSM_GG_0640
./ginout_c2 41r1/climate.v012/159l_2/lsm    41r1/LSM_GG_0080
./ginout_c2 41r1/climate.v012/2047l_2/lsm   41r1/LSM_GG_1024
./ginout_c2 41r1/climate.v012/255l_2/lsm    41r1/LSM_GG_0128
./ginout_c2 41r1/climate.v012/319l_2/lsm    41r1/LSM_GG_0160
./ginout_c2 41r1/climate.v012/399l_2/lsm    41r1/LSM_GG_0200
./ginout_c2 41r1/climate.v012/511l_2/lsm    41r1/LSM_GG_0256
./ginout_c2 41r1/climate.v012/639l_2/lsm    41r1/LSM_GG_0320
./ginout_c2 41r1/climate.v012/799l_2/lsm    41r1/LSM_GG_0400
./ginout_c2 41r1/climate.v012/95l_2/lsm     41r1/LSM_GG_0048

