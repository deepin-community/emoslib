#!/bin/sh

cd ../
path=`pwd`


BUFR_TABLES=$path/bufrtables/
export BUFR_TABLES

cd synop2bufr

path=`pwd`

PP_BASE=$path
export PP_BASE

./synop2bufr -i data/greek.gts -o data/greek.gts.bufr -c 96
