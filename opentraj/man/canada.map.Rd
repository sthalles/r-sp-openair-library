\name{canada.map}
\alias{canada.map}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Canada Map
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(canada.map)}
\format{
  The format is:
Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  ..@ data       :'data.frame':	13 obs. of  2 variables:
  .. ..$ CODE: Factor w/ 13 levels "CA01","CA02",..: 1 11 3 5 9 7 6 13 8 4 ...
  .. ..$ NAME: Factor w/ 13 levels "Alberta","British Columbia",..: 1 12 3 5 10 7 6 8 9 4 ...
  ..@ polygons   :List of 13
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -114.5 55.2
  .. .. .. .. .. .. ..@ area   : num 93.7
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:489, 1:2] -114 -114 -114 -114 -114 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -114.5 55.2
  .. .. .. ..@ ID       : chr "0"
  .. .. .. ..@ area     : num 93.7
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -105.9 54.4
  .. .. .. .. .. .. ..@ area   : num 90.4
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:98, 1:2] -110 -110 -110 -110 -110 -110 -110 -110 -110 -110 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -105.9 54.4
  .. .. .. ..@ ID       : chr "1"
  .. .. .. ..@ area     : num 90.4
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 2
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -94.8 59.1
  .. .. .. .. .. .. ..@ area   : num 0.0044
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -94.7 -94.7 -94.8 -94.8 -94.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -97.4 54.9
  .. .. .. .. .. .. ..@ area   : num 91.6
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:204, 1:2] -97.2 -97.9 -98.4 -99 -99.5 ...
  .. .. .. ..@ plotOrder: int [1:2] 2 1
  .. .. .. ..@ labpt    : num [1:2] -97.4 54.9
  .. .. .. ..@ ID       : chr "2"
  .. .. .. ..@ area     : num 91.6
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 76
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.1 49.7
  .. .. .. .. .. .. ..@ area   : num 0.0026
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -53.1 -53.1 -53.1 -53.1 -53.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.2 49.6
  .. .. .. .. .. .. ..@ area   : num 0.0237
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -54.3 -54.3 -54.3 -54.2 -54.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 49.6
  .. .. .. .. .. .. ..@ area   : num 0.00368
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -55.6 -55.7 -55.7 -55.7 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.6 49.5
  .. .. .. .. .. .. ..@ area   : num 0.00184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -55.6 -55.6 -55.6 -55.6 -55.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.6 48.8
  .. .. .. .. .. .. ..@ area   : num 0.00378
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -53.6 -53.6 -53.6 -53.6 -53.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.2 49.3
  .. .. .. .. .. .. ..@ area   : num 0.00225
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -55.2 -55.2 -55.2 -55.2 -55.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.8 49.5
  .. .. .. .. .. .. ..@ area   : num 0.00265
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -55.8 -55.8 -55.8 -55.8 -55.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.6 48.7
  .. .. .. .. .. .. ..@ area   : num 0.00252
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -53.6 -53.6 -53.6 -53.7 -53.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.6 48.5
  .. .. .. .. .. .. ..@ area   : num 0.00241
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -53.5 -53.5 -53.5 -53.6 -53.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52.7 48.1
  .. .. .. .. .. .. ..@ area   : num 0.00243
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -52.7 -52.8 -52.8 -52.8 -52.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -53.6 48.1
  .. .. .. .. .. .. ..@ area   : num 0.0234
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -53.7 -53.8 -53.8 -53.8 -53.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -52.9 47.6
  .. .. .. .. .. .. ..@ area   : num 0.00568
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -52.8 -52.9 -52.9 -52.9 -52.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54 47.6
  .. .. .. .. .. .. ..@ area   : num 0.00805
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -54 -54 -54 -54 -54.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.1 47.4
  .. .. .. .. .. .. ..@ area   : num 0.00225
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -54.1 -54.1 -54.1 -54.2 -54.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.2 47.5
  .. .. .. .. .. .. ..@ area   : num 0.0179
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -54.1 -54.1 -54.1 -54.2 -54.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.9 47.3
  .. .. .. .. .. .. ..@ area   : num 0.00231
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -55.9 -55.9 -55.9 -55.9 -55.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56 48.7
  .. .. .. .. .. .. ..@ area   : num 13.5
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:662, 1:2] -55.4 -55.3 -55.3 -55.2 -55.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -57 53.7
  .. .. .. .. .. .. ..@ area   : num 0.00154
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -57 -57 -57 -57.1 -57.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.5 54.1
  .. .. .. .. .. .. ..@ area   : num 0.00312
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -58.4 -58.5 -58.5 -58.5 -58.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 53.1
  .. .. .. .. .. .. ..@ area   : num 0.00271
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -55.7 -55.7 -55.7 -55.7 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 53
  .. .. .. .. .. .. ..@ area   : num 0.00177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -55.7 -55.7 -55.7 -55.7 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.5 57.9
  .. .. .. .. .. .. ..@ area   : num 0.00206
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -61.6 -61.5 -61.5 -61.5 -61.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.6 57.8
  .. .. .. .. .. .. ..@ area   : num 0.0166
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -61.6 -61.6 -61.5 -61.5 -61.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.6 57.7
  .. .. .. .. .. .. ..@ area   : num 0.00244
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -61.7 -61.6 -61.6 -61.6 -61.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.8 57.5
  .. .. .. .. .. .. ..@ area   : num 0.0185
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -61.8 -61.7 -61.7 -61.7 -61.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.6 57.5
  .. .. .. .. .. .. ..@ area   : num 0.0143
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -61.5 -61.5 -61.6 -61.6 -61.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.6 57.3
  .. .. .. .. .. .. ..@ area   : num 0.00275
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -61.6 -61.6 -61.5 -61.5 -61.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.3 57.3
  .. .. .. .. .. .. ..@ area   : num 0.0022
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -61.4 -61.4 -61.3 -61.3 -61.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.7 57.3
  .. .. .. .. .. .. ..@ area   : num 0.0023
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -61.7 -61.7 -61.7 -61.7 -61.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.9 56.9
  .. .. .. .. .. .. ..@ area   : num 0.00184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -60.9 -60.9 -60.8 -60.8 -60.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61 56.9
  .. .. .. .. .. .. ..@ area   : num 0.00175
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -61 -61 -61 -61 -60.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.1 56.7
  .. .. .. .. .. .. ..@ area   : num 0.00163
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -61.1 -61.1 -61.1 -61 -61 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61 56.6
  .. .. .. .. .. .. ..@ area   : num 0.00939
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -60.9 -61 -61 -61 -61 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.3 56.8
  .. .. .. .. .. .. ..@ area   : num 0.0525
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -61.3 -61.2 -61.2 -61.3 -61.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.3 56.5
  .. .. .. .. .. .. ..@ area   : num 0.0251
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -61 -61 -61 -61.1 -61.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.1 56.4
  .. .. .. .. .. .. ..@ area   : num 0.00546
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -61.1 -61.1 -61.1 -61.1 -61.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.6 56.5
  .. .. .. .. .. .. ..@ area   : num 0.00475
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -61.7 -61.7 -61.6 -61.6 -61.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.6 56.2
  .. .. .. .. .. .. ..@ area   : num 0.0022
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -60.6 -60.6 -60.6 -60.6 -60.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61 56.3
  .. .. .. .. .. .. ..@ area   : num 0.00199
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -61 -61 -61 -61 -60.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.4 56.4
  .. .. .. .. .. .. ..@ area   : num 0.0124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -61.5 -61.6 -61.5 -61.5 -61.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.3 56.3
  .. .. .. .. .. .. ..@ area   : num 0.00643
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -61.4 -61.4 -61.4 -61.4 -61.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.1 55.9
  .. .. .. .. .. .. ..@ area   : num 0.00229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -60.2 -60.1 -60.1 -60.1 -60.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.9 56.1
  .. .. .. .. .. .. ..@ area   : num 0.0282
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -61 -60.9 -60.9 -60.8 -60.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.6 56
  .. .. .. .. .. .. ..@ area   : num 0.00714
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -60.6 -60.6 -60.6 -60.7 -60.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.7 55.2
  .. .. .. .. .. .. ..@ area   : num 0.00193
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -58.7 -58.7 -58.7 -58.7 -58.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.9 55.4
  .. .. .. .. .. .. ..@ area   : num 0.00385
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -59.8 -59.8 -59.8 -59.9 -59.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.2 54.6
  .. .. .. .. .. .. ..@ area   : num 0.00473
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -57.2 -57.3 -57.3 -57.3 -57.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60 55.4
  .. .. .. .. .. .. ..@ area   : num 0.00332
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -60 -60.1 -60.1 -60 -60 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.1 54.5
  .. .. .. .. .. .. ..@ area   : num 0.00269
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -57.1 -57.1 -57.1 -57.1 -57.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.9 55.3
  .. .. .. .. .. .. ..@ area   : num 0.00237
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -59.8 -59.8 -59.9 -59.9 -59.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.8 55
  .. .. .. .. .. .. ..@ area   : num 0.00282
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -58.8 -58.8 -58.8 -58.8 -58.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.2 55.4
  .. .. .. .. .. .. ..@ area   : num 0.00472
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -60.2 -60.2 -60.2 -60.3 -60.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.7 55
  .. .. .. .. .. .. ..@ area   : num 0.00449
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -58.7 -58.6 -58.6 -58.6 -58.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.3 54.3
  .. .. .. .. .. .. ..@ area   : num 0.00238
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -57.3 -57.3 -57.3 -57.3 -57.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.3 53.8
  .. .. .. .. .. .. ..@ area   : num 0.00217
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -56.2 -56.2 -56.3 -56.3 -56.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -58.2 54.3
  .. .. .. .. .. .. ..@ area   : num 0.00232
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -58.2 -58.2 -58.2 -58.2 -58.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 53.5
  .. .. .. .. .. .. ..@ area   : num 0.00156
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -55.6 -55.7 -55.7 -55.7 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56 53.6
  .. .. .. .. .. .. ..@ area   : num 0.00555
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -56.1 -56 -56 -56 -55.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.8 53.5
  .. .. .. .. .. .. ..@ area   : num 0.00601
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -55.8 -55.8 -55.7 -55.7 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.9 55.9
  .. .. .. .. .. .. ..@ area   : num 0.0016
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -60.9 -60.8 -60.8 -60.9 -60.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.2 46.8
  .. .. .. .. .. .. ..@ area   : num 0.00266
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -56.2 -56.2 -56.2 -56.2 -56.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -56.3 47
  .. .. .. .. .. .. ..@ area   : num 0.0339
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -56.3 -56.4 -56.4 -56.3 -56.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.8 52.7
  .. .. .. .. .. .. ..@ area   : num 0.00223
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -55.7 -55.8 -55.8 -55.8 -55.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 52.5
  .. .. .. .. .. .. ..@ area   : num 0.00353
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -55.6 -55.6 -55.6 -55.6 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.3 51.9
  .. .. .. .. .. .. ..@ area   : num 0.00774
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -55.3 -55.3 -55.3 -55.2 -55.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.4 51.6
  .. .. .. .. .. .. ..@ area   : num 0.00258
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -55.4 -55.4 -55.4 -55.4 -55.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.5 51
  .. .. .. .. .. .. ..@ area   : num 0.00606
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -55.5 -55.5 -55.5 -55.5 -55.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.5 50.8
  .. .. .. .. .. .. ..@ area   : num 0.0079
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -55.4 -55.4 -55.4 -55.5 -55.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -57.2 50.8
  .. .. .. .. .. .. ..@ area   : num 0.003
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -57.2 -57.2 -57.2 -57.1 -57.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.6 50.2
  .. .. .. .. .. .. ..@ area   : num 0.00265
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -55.5 -55.5 -55.6 -55.6 -55.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -55.7 50.2
  .. .. .. .. .. .. ..@ area   : num 0.00186
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -55.7 -55.7 -55.7 -55.7 -55.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.4 49.6
  .. .. .. .. .. .. ..@ area   : num 0.00534
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -54.4 -54.4 -54.4 -54.4 -54.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.7 49.6
  .. .. .. .. .. .. ..@ area   : num 0.0053
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -54.7 -54.7 -54.7 -54.8 -54.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.7 49.5
  .. .. .. .. .. .. ..@ area   : num 0.0199
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -54.7 -54.6 -54.5 -54.5 -54.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -54.7 49.4
  .. .. .. .. .. .. ..@ area   : num 0.00396
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -54.7 -54.8 -54.8 -54.7 -54.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62 54.3
  .. .. .. .. .. .. ..@ area   : num 40.7
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5339, 1:2] -64.8 -64.8 -64.8 -64.8 -64.8 ...
  .. .. .. ..@ plotOrder: int [1:76] 76 17 34 62 43 35 2 11 74 25 ...
  .. .. .. ..@ labpt    : num [1:2] -62 54.3
  .. .. .. ..@ ID       : chr "3"
  .. .. .. ..@ area     : num 54.7
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -63.3 46.4
  .. .. .. .. .. .. ..@ area   : num 0.702
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:95, 1:2] -62.9 -62.9 -63 -63.1 -63.1 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -63.3 46.4
  .. .. .. ..@ ID       : chr "4"
  .. .. .. ..@ area     : num 0.702
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 3
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64 44.9
  .. .. .. .. .. .. ..@ area   : num 5.26
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:252, 1:2] -63.9 -63.9 -63.9 -63.7 -63.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.7 46.2
  .. .. .. .. .. .. ..@ area   : num 1.26
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:129, 1:2] -60.9 -61.1 -61.1 -61.1 -61 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -60.9 45.6
  .. .. .. .. .. .. ..@ area   : num 0.0141
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -60.9 -61 -61 -61 -60.8 ...
  .. .. .. ..@ plotOrder: int [1:3] 1 2 3
  .. .. .. ..@ labpt    : num [1:2] -64 44.9
  .. .. .. ..@ ID       : chr "5"
  .. .. .. ..@ area     : num 6.53
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 55
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.8 69.6
  .. .. .. .. .. .. ..@ area   : num 0.00338
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.9 69.5
  .. .. .. .. .. .. ..@ area   : num 0.00434
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -125.1 70.1
  .. .. .. .. .. .. ..@ area   : num 0.0076
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -125 -125 -125 -125 -125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -125.4 69.7
  .. .. .. .. .. .. ..@ area   : num 0.00379
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -125 -125 -125 -125 -125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -124 69.7
  .. .. .. .. .. .. ..@ area   : num 0.00361
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -124 -124 -124 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -117.1 70.6
  .. .. .. .. .. .. ..@ area   : num 0.0113
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -117 -117 -117 -117 -117 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -116.6 70.6
  .. .. .. .. .. .. ..@ area   : num 0.00826
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -116 -116 -117 -117 -117 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -116.3 70.6
  .. .. .. .. .. .. ..@ area   : num 0.0047
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -116 -116 -116 -116 -116 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -116.7 70.5
  .. .. .. .. .. .. ..@ area   : num 0.00436
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -117 -117 -117 -117 -117 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -116 70.6
  .. .. .. .. .. .. ..@ area   : num 0.00529
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -116 -116 -116 -116 -116 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -113.1 70.3
  .. .. .. .. .. .. ..@ area   : num 0.0083
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -113 -113 -113 -113 -113 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -112.7 70.3
  .. .. .. .. .. .. ..@ area   : num 0.00461
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -113 -113 -113 -113 -113 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -112 70.3
  .. .. .. .. .. .. ..@ area   : num 0.0179
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -112 -112 -112 -112 -112 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -111.9 75.9
  .. .. .. .. .. .. ..@ area   : num 0.00577
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -112 -112 -112 -112 -112 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -119.6 64.4
  .. .. .. .. .. .. ..@ area   : num 212
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:2476, 1:2] -125 -125 -124 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -111.7 77.7
  .. .. .. .. .. .. ..@ area   : num 1.98
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:48, 1:2] -110 -110 -111 -111 -111 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -111.3 78.5
  .. .. .. .. .. .. ..@ area   : num 1.01
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:33, 1:2] -110 -110 -111 -111 -111 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -110.2 72.6
  .. .. .. .. .. .. ..@ area   : num 0.0272
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -110 -110 -110 -110 -110 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -110.3 72.9
  .. .. .. .. .. .. ..@ area   : num 0.166
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -110 -110 -110 -111 -111 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -114.1 71.6
  .. .. .. .. .. .. ..@ area   : num 21.3
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:324, 1:2] -116 -116 -117 -117 -117 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -114.4 77.9
  .. .. .. .. .. .. ..@ area   : num 0.287
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -114 -114 -114 -115 -115 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -113.9 77.1
  .. .. .. .. .. .. ..@ area   : num 0.00779
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -114 -114 -114 -114 -114 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -113.5 77.1
  .. .. .. .. .. .. ..@ area   : num 0.00776
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -113 -114 -114 -114 -114 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -122.9 76.1
  .. .. .. .. .. .. ..@ area   : num 0.0102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -123 -123 -123 -123 -123 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -114.2 76.8
  .. .. .. .. .. .. ..@ area   : num 0.192
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -114 -114 -114 -115 -115 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -122.5 75.9
  .. .. .. .. .. .. ..@ area   : num 0.0078
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -122 -122 -123 -123 -123 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -121.1 75.8
  .. .. .. .. .. .. ..@ area   : num 0.0393
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -121 -121 -121 -121 -121 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -119.1 76.7
  .. .. .. .. .. .. ..@ area   : num 5.43
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:159, 1:2] -120 -119 -119 -119 -119 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -118.5 75.8
  .. .. .. .. .. .. ..@ area   : num 0.511
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:23, 1:2] -119 -119 -119 -119 -119 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -110.5 72.6
  .. .. .. .. .. .. ..@ area   : num 0.00664
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -111 -111 -110 -110 -110 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -124.5 73.9
  .. .. .. .. .. .. ..@ area   : num 0.0107
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -124 -125 -125 -125 -125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -124.7 73.7
  .. .. .. .. .. .. ..@ area   : num 0.00475
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -125 -125 -125 -125 -125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -124.3 73.6
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -124 -124 -124 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -121 73
  .. .. .. .. .. .. ..@ area   : num 19
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:167, 1:2] -120 -120 -120 -120 -119 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.7 69.6
  .. .. .. .. .. .. ..@ area   : num 0.00616
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135 69.7
  .. .. .. .. .. .. ..@ area   : num 0.00418
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -135 -135 -135 -135 -135 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.8 69.5
  .. .. .. .. .. .. ..@ area   : num 0.00505
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.5 69.5
  .. .. .. .. .. .. ..@ area   : num 0.00328
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -135 -135 -135 -135 -135 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.6 69.4
  .. .. .. .. .. .. ..@ area   : num 0.00346
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135 69.3
  .. .. .. .. .. .. ..@ area   : num 0.809
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:70, 1:2] -135 -135 -135 -135 -135 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -136.3 69.1
  .. .. .. .. .. .. ..@ area   : num 0.00256
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.5 69.3
  .. .. .. .. .. .. ..@ area   : num 0.00396
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -136 -136 -136 -136 -135 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -136.4 69.1
  .. .. .. .. .. .. ..@ area   : num 0.00394
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -134 69.6
  .. .. .. .. .. .. ..@ area   : num 0.00497
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -134 -134 -134 -134 -134 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -136 69
  .. .. .. .. .. .. ..@ area   : num 0.00319
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.4 70.6
  .. .. .. .. .. .. ..@ area   : num 0.0119
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -129 -129 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -136 69
  .. .. .. .. .. .. ..@ area   : num 0.00407
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -136 69
  .. .. .. .. .. .. ..@ area   : num 0.00415
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -133.6 69.5
  .. .. .. .. .. .. ..@ area   : num 0.00387
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -134 -134 -134 -134 -134 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -136.1 68.9
  .. .. .. .. .. .. ..@ area   : num 0.00627
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.8 68.9
  .. .. .. .. .. .. ..@ area   : num 0.00819
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.9 68.9
  .. .. .. .. .. .. ..@ area   : num 0.00756
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -136 -136 -136 -136 -136 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -113.7 75.5
  .. .. .. .. .. .. ..@ area   : num 8.36
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:225, 1:2] -110 -110 -110 -110 -110 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -110 75.9
  .. .. .. .. .. .. ..@ area   : num 0.000813
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -110 -110 -110 -110 -110 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -110.2 76.4
  .. .. .. .. .. .. ..@ area   : num 0.0968
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -110 -110 -110 -110 -110 ...
  .. .. .. ..@ plotOrder: int [1:55] 15 20 34 53 28 16 17 40 29 21 ...
  .. .. .. ..@ labpt    : num [1:2] -119.6 64.4
  .. .. .. ..@ ID       : chr "6"
  .. .. .. ..@ area     : num 271
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 412
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -92.2 62.3
  .. .. .. .. .. .. ..@ area   : num 0.00198
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -92.1 -92.1 -92.2 -92.2 -92.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -92.4 62.2
  .. .. .. .. .. .. ..@ area   : num 0.0037
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -92.3 -92.4 -92.4 -92.4 -92.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -92.6 62.1
  .. .. .. .. .. .. ..@ area   : num 0.00488
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -92.5 -92.6 -92.6 -92.6 -92.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -93 61.9
  .. .. .. .. .. .. ..@ area   : num 0.0148
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -93 -93 -93.1 -93.2 -93.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.4 64.3
  .. .. .. .. .. .. ..@ area   : num 0.00449
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -81.5 -81.5 -81.5 -81.5 -81.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -108 67
  .. .. .. .. .. .. ..@ area   : num 0.0067
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -108 -108 -108 -108 -108 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -107.8 66.9
  .. .. .. .. .. .. ..@ area   : num 0.0102
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -108 -108 -108 -108 -108 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.5 68.5
  .. .. .. .. .. .. ..@ area   : num 0.00454
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -74.4 -74.5 -74.5 -74.5 -74.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 68.3
  .. .. .. .. .. .. ..@ area   : num 0.0371
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -74.4 -74.4 -74.3 -74.2 -74.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.1 68.2
  .. .. .. .. .. .. ..@ area   : num 0.00556
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -74.1 -74.1 -74.1 -74.2 -74.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62.7 65.6
  .. .. .. .. .. .. ..@ area   : num 0.00417
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -62.8 -62.7 -62.6 -62.6 -62.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -63.1 65.6
  .. .. .. .. .. .. ..@ area   : num 0.00411
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -63 -63 -63.1 -63.1 -63.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62.9 67.1
  .. .. .. .. .. .. ..@ area   : num 0.0037
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -62.9 -62.9 -63 -63 -63 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62.8 67.1
  .. .. .. .. .. .. ..@ area   : num 0.005
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -62.8 -62.8 -62.8 -62.9 -62.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.7 72.2
  .. .. .. .. .. .. ..@ area   : num 0.00679
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -74.6 -74.7 -74.7 -74.7 -74.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.9 72.2
  .. .. .. .. .. .. ..@ area   : num 0.0103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -74.8 -74.8 -74.8 -74.8 -74.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.9 71.6
  .. .. .. .. .. .. ..@ area   : num 0.0266
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -72.9 -73 -73.1 -73 -73 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.3 71.6
  .. .. .. .. .. .. ..@ area   : num 0.016
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -73.4 -73.4 -73.4 -73.3 -73.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.5 71.7
  .. .. .. .. .. .. ..@ area   : num 0.00671
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -74.6 -74.6 -74.5 -74.4 -74.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.2 71.4
  .. .. .. .. .. .. ..@ area   : num 0.0496
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -73.1 -73.2 -73.3 -73.3 -73.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.1 71.1
  .. .. .. .. .. .. ..@ area   : num 0.00791
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -71.1 -71.1 -71.2 -71.2 -71.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.8 70.9
  .. .. .. .. .. .. ..@ area   : num 0.0929
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -72.1 -72.1 -72.1 -72 -71.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 78.7
  .. .. .. .. .. .. ..@ area   : num 0.0295
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -74.1 -74.3 -74.4 -74.3 -74.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 77.8
  .. .. .. .. .. .. ..@ area   : num 0.0191
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -77.7 -77.9 -77.8 -77.8 -77.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.5 77.4
  .. .. .. .. .. .. ..@ area   : num 0.00882
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -81.4 -81.5 -81.5 -81.5 -81.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.7 77
  .. .. .. .. .. .. ..@ area   : num 0.0106
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -78.6 -78.7 -78.8 -78.8 -78.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 72.5
  .. .. .. .. .. .. ..@ area   : num 0.00987
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.1 -80 -80 -80.1 -80.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 72.4
  .. .. .. .. .. .. ..@ area   : num 0.0169
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78.9 -79.1 -79 -79 -78.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.5 72.4
  .. .. .. .. .. .. ..@ area   : num 0.00777
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -79.4 -79.6 -79.6 -79.6 -79.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65 81.8
  .. .. .. .. .. .. ..@ area   : num 0.0108
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -64.8 -64.9 -64.9 -65 -65.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66.6 81.6
  .. .. .. .. .. .. ..@ area   : num 0.0153
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -66.3 -66.4 -66.5 -66.6 -66.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67 80.9
  .. .. .. .. .. .. ..@ area   : num 0.00721
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -66.9 -66.9 -67 -67.1 -67.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.5 83.1
  .. .. .. .. .. .. ..@ area   : num 0.0126
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -74.5 -74.6 -74.6 -74.7 -74.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.8 83
  .. .. .. .. .. .. ..@ area   : num 0.0225
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -77.6 -77.7 -77.8 -77.9 -77.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.6 60.7
  .. .. .. .. .. .. ..@ area   : num 0.00344
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -64.6 -64.6 -64.6 -64.6 -64.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.5 60.6
  .. .. .. .. .. .. ..@ area   : num 0.0035
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -64.6 -64.6 -64.6 -64.6 -64.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.5 60.4
  .. .. .. .. .. .. ..@ area   : num 0.034
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -64.8 -64.7 -64.7 -64.5 -64.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.9 64.3
  .. .. .. .. .. .. ..@ area   : num 0.00372
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -73.9 -73.9 -74 -74 -74 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.8 64.3
  .. .. .. .. .. .. ..@ area   : num 0.00624
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -73.8 -73.8 -73.8 -73.8 -73.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.5 64.2
  .. .. .. .. .. .. ..@ area   : num 0.00483
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -73.5 -73.5 -73.5 -73.5 -73.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.9 63.4
  .. .. .. .. .. .. ..@ area   : num 0.00447
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -67.9 -67.9 -68 -68 -68 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -73.3 64.2
  .. .. .. .. .. .. ..@ area   : num 0.00627
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -73.3 -73.4 -73.4 -73.4 -73.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.8 63.2
  .. .. .. .. .. .. ..@ area   : num 0.00386
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -67.8 -67.8 -67.8 -67.8 -67.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.2 62.5
  .. .. .. .. .. .. ..@ area   : num 0.00344
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -64.2 -64.2 -64.3 -64.2 -64.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67 63.1
  .. .. .. .. .. .. ..@ area   : num 0.0181
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -67.1 -67.1 -67.1 -67 -66.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68 63.3
  .. .. .. .. .. .. ..@ area   : num 0.00916
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -68.1 -68.1 -68 -68 -67.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -68.2 63.3
  .. .. .. .. .. .. ..@ area   : num 0.00535
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -68.2 -68.2 -68.2 -68.2 -68.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -75.6 64.3
  .. .. .. .. .. .. ..@ area   : num 0.00532
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -75.7 -75.7 -75.7 -75.7 -75.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66.5 62.9
  .. .. .. .. .. .. ..@ area   : num 0.00639
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -66.5 -66.6 -66.6 -66.5 -66.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65 62.6
  .. .. .. .. .. .. ..@ area   : num 0.0106
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -65 -65.1 -65 -65 -64.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.7 63.7
  .. .. .. .. .. .. ..@ area   : num 0.00327
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -71.7 -71.8 -71.8 -71.8 -71.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.6 63.6
  .. .. .. .. .. .. ..@ area   : num 0.00308
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -71.6 -71.6 -71.7 -71.7 -71.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.6 62.5
  .. .. .. .. .. .. ..@ area   : num 0.0715
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -64.4 -64.3 -64.4 -64.4 -64.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.8 63.8
  .. .. .. .. .. .. ..@ area   : num 0.0032
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -72.8 -72.8 -72.8 -72.8 -72.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.5 63.7
  .. .. .. .. .. .. ..@ area   : num 0.0105
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -72.4 -72.4 -72.4 -72.4 -72.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.6 63.5
  .. .. .. .. .. .. ..@ area   : num 0.00316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -71.5 -71.6 -71.6 -71.6 -71.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.8 63.7
  .. .. .. .. .. .. ..@ area   : num 0.00253
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -72.8 -72.8 -72.8 -72.8 -72.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.3 63.6
  .. .. .. .. .. .. ..@ area   : num 0.00345
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -72.2 -72.3 -72.3 -72.3 -72.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65.8 62.2
  .. .. .. .. .. .. ..@ area   : num 0.00114
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:5, 1:2] -65.9 -65.8 -65.8 -65.8 -65.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.8 64
  .. .. .. .. .. .. ..@ area   : num 0.0378
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -77.9 -78 -78 -78 -77.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65.8 62.1
  .. .. .. .. .. .. ..@ area   : num 0.00198
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -65.8 -65.9 -65.9 -65.8 -65.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.4 63
  .. .. .. .. .. .. ..@ area   : num 0.00462
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -71.4 -71.4 -71.4 -71.5 -71.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.1 63
  .. .. .. .. .. .. ..@ area   : num 0.00423
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -71 -71 -71.1 -71.1 -71.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65 61.9
  .. .. .. .. .. .. ..@ area   : num 0.0506
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -65 -65.2 -65.2 -65.2 -65.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.1 62.8
  .. .. .. .. .. .. ..@ area   : num 0.00383
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -71 -71.1 -71.1 -71.1 -71.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65.6 61.8
  .. .. .. .. .. .. ..@ area   : num 0.00296
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -65.6 -65.6 -65.7 -65.6 -65.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65.3 61.7
  .. .. .. .. .. .. ..@ area   : num 0.00322
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -65.3 -65.3 -65.4 -65.4 -65.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70 62.6
  .. .. .. .. .. .. ..@ area   : num 0.00229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -70 -70 -70 -70 -69.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -65.9 61.8
  .. .. .. .. .. .. ..@ area   : num 0.00329
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -65.8 -65.9 -65.9 -65.9 -65.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -70.6 62.7
  .. .. .. .. .. .. ..@ area   : num 0.137
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -70.6 -70.7 -70.8 -70.8 -70.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.9 63.5
  .. .. .. .. .. .. ..@ area   : num 0.142
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] -76.6 -76.7 -76.8 -76.9 -77.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.9 61.6
  .. .. .. .. .. .. ..@ area   : num 0.168
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -64.8 -64.9 -65.2 -65.4 -65.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.4 63.5
  .. .. .. .. .. .. ..@ area   : num 0.00481
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -78.5 -78.5 -78.5 -78.5 -78.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.9 63.3
  .. .. .. .. .. .. ..@ area   : num 0.224
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -77.9 -78 -78.2 -78.4 -78.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -84.3 64.4
  .. .. .. .. .. .. ..@ area   : num 7.94
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:169, 1:2] -81.9 -81.6 -81.3 -81.2 -81.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.5 63.7
  .. .. .. .. .. .. ..@ area   : num 0.0034
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -91.5 -91.5 -91.5 -91.5 -91.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.4 63.6
  .. .. .. .. .. .. ..@ area   : num 0.00912
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -91.3 -91.3 -91.4 -91.4 -91.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.8 63.6
  .. .. .. .. .. .. ..@ area   : num 0.0135
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -90.6 -90.6 -90.7 -90.7 -90.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.2 63.6
  .. .. .. .. .. .. ..@ area   : num 0.00268
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -91.1 -91.2 -91.2 -91.2 -91.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.5 63.5
  .. .. .. .. .. .. ..@ area   : num 0.00346
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -90.5 -90.5 -90.5 -90.5 -90.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91 63.5
  .. .. .. .. .. .. ..@ area   : num 0.00349
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -90.9 -91 -91 -91 -91 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -90.7 63.5
  .. .. .. .. .. .. ..@ area   : num 0.00311
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -90.7 -90.7 -90.7 -90.7 -90.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.7 63.1
  .. .. .. .. .. .. ..@ area   : num 0.00472
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -82.6 -82.6 -82.6 -82.6 -82.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -91.9 62.8
  .. .. .. .. .. .. ..@ area   : num 0.00357
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -91.9 -91.9 -91.9 -91.9 -92 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.9 62.6
  .. .. .. .. .. .. ..@ area   : num 1.01
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:49, 1:2] -83.7 -83.7 -83.7 -83.7 -83.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.6 57.8
  .. .. .. .. .. .. ..@ area   : num 0.00199
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -79.6 -79.6 -79.7 -79.7 -79.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 57.5
  .. .. .. .. .. .. ..@ area   : num 0.00809
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -79.7 -79.7 -79.7 -79.7 -79.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 57.4
  .. .. .. .. .. .. ..@ area   : num 0.00246
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -79.8 -79.8 -79.8 -79.8 -79.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.6 56.9
  .. .. .. .. .. .. ..@ area   : num 0.00496
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -79.6 -79.6 -79.6 -79.6 -79.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.3 56.6
  .. .. .. .. .. .. ..@ area   : num 0.00259
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -78.3 -78.3 -78.3 -78.3 -78.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 56.9
  .. .. .. .. .. .. ..@ area   : num 0.012
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -79.9 -79.9 -79.8 -79.8 -79.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 56.6
  .. .. .. .. .. .. ..@ area   : num 0.00254
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -78 -78 -78 -78.1 -78.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.8 56.6
  .. .. .. .. .. .. ..@ area   : num 0.0052
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -78.7 -78.7 -78.7 -78.8 -78.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.9 56.8
  .. .. .. .. .. .. ..@ area   : num 0.00582
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -79.9 -79.9 -79.9 -79.9 -79.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.2 56.6
  .. .. .. .. .. .. ..@ area   : num 0.00493
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -79.1 -79.1 -79.1 -79.1 -79.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.5 56.7
  .. .. .. .. .. .. ..@ area   : num 0.0109
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -79.5 -79.6 -79.5 -79.6 -79.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 56.3
  .. .. .. .. .. .. ..@ area   : num 0.00203
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78 -78 -78 -78 -78.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.6 56.3
  .. .. .. .. .. .. ..@ area   : num 0.00812
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -79.6 -79.6 -79.6 -79.6 -79.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.7 56.3
  .. .. .. .. .. .. ..@ area   : num 0.0428
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -78.8 -78.9 -78.9 -78.8 -78.8 ...
  .. .. .. .. .. [list output truncated]
  .. .. .. ..@ plotOrder: int [1:412] 409 360 110 123 135 182 211 75 124 181 ...
  .. .. .. ..@ labpt    : num [1:2] -98.5 65.9
  .. .. .. ..@ ID       : chr "7"
  .. .. .. ..@ area     : num 512
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 21
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.2 45.8
  .. .. .. .. .. .. ..@ area   : num 0.316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:68, 1:2] -82.1 -82.1 -82.2 -82.3 -82.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.3 53
  .. .. .. .. .. .. ..@ area   : num 0.374
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -80.7 -80.8 -80.8 -81 -81.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -83.9 46.2
  .. .. .. .. .. .. ..@ area   : num 0.0273
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -83.9 -83.9 -83.9 -83.9 -84 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.9 48.8
  .. .. .. .. .. .. ..@ area   : num 0.0283
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -88 -88.1 -88.1 -88.1 -88.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -85.8 47.8
  .. .. .. .. .. .. ..@ area   : num 0.0184
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -85.9 -85.9 -86 -86 -85.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -83.3 45.9
  .. .. .. .. .. .. ..@ area   : num 0.0117
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -83.3 -83.3 -83.3 -83.3 -83.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.8 46
  .. .. .. .. .. .. ..@ area   : num 0.00967
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -81.9 -81.9 -81.9 -81.7 -81.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 44.8
  .. .. .. .. .. .. ..@ area   : num 0.00616
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.1 -80.2 -80.2 -80.2 -80.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.7 45.9
  .. .. .. .. .. .. ..@ area   : num 0.00363
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -82.7 -82.7 -82.7 -82.7 -82.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -89.1 48.2
  .. .. .. .. .. .. ..@ area   : num 0.00295
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -89.2 -89.2 -89.1 -89.1 -89.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.3 51.4
  .. .. .. .. .. .. ..@ area   : num 0.00394
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -80.3 -80.3 -80.3 -80.3 -80.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87 48.7
  .. .. .. .. .. .. ..@ area   : num 0.00211
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -87 -87 -87.1 -87 -87 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.2 46
  .. .. .. .. .. .. ..@ area   : num 0.00204
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -82.2 -82.1 -82.1 -82.2 -82.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -82.3 46.1
  .. .. .. .. .. .. ..@ area   : num 0.00175
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -82.3 -82.4 -82.4 -82.3 -82.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -87.5 48.8
  .. .. .. .. .. .. ..@ area   : num 0.00185
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -87.5 -87.5 -87.5 -87.5 -87.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.9 44.9
  .. .. .. .. .. .. ..@ area   : num 0.0018
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -80.9 -80.9 -80.9 -80.9 -80.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 44.9
  .. .. .. .. .. .. ..@ area   : num 0.00152
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.1 -80.1 -80.1 -80.1 -80.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -84.8 47.3
  .. .. .. .. .. .. ..@ area   : num 0.00163
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -84.8 -84.8 -84.8 -84.8 -84.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.4 45.5
  .. .. .. .. .. .. ..@ area   : num 0.00128
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -80.4 -80.4 -80.4 -80.4 -80.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -88.1 48.9
  .. .. .. .. .. .. ..@ area   : num 0.00108
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -88.1 -88.1 -88.1 -88.1 -88.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -86.1 50.5
  .. .. .. .. .. .. ..@ area   : num 125
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:1787, 1:2] -79.5 -79.5 -79.5 -79.5 -79.5 ...
  .. .. .. ..@ plotOrder: int [1:21] 21 2 1 4 3 5 6 7 8 11 ...
  .. .. .. ..@ labpt    : num [1:2] -86.1 50.5
  .. .. .. ..@ ID       : chr "8"
  .. .. .. ..@ area     : num 126
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66.4 46.6
  .. .. .. .. .. .. ..@ area   : num 8.59
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:903, 1:2] -67.4 -67.4 -67.4 -67.4 -67.4 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -66.4 46.6
  .. .. .. ..@ ID       : chr "9"
  .. .. .. ..@ area     : num 8.59
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 1
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -135.5 63.6
  .. .. .. .. .. .. ..@ area   : num 87.5
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:2148, 1:2] -139 -139 -139 -139 -140 ...
  .. .. .. ..@ plotOrder: int 1
  .. .. .. ..@ labpt    : num [1:2] -135.5 63.6
  .. .. .. ..@ ID       : chr "10"
  .. .. .. ..@ area     : num 87.5
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 126
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -125.1 48.9
  .. .. .. .. .. .. ..@ area   : num 0.00261
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -125 -125 -125 -125 -125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.8 49.2
  .. .. .. .. .. .. ..@ area   : num 0.00416
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -124 -124 -124 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.7 49.1
  .. .. .. .. .. .. ..@ area   : num 0.00229
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -124 -124 -124 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.2 54.6
  .. .. .. .. .. .. ..@ area   : num 0.000409
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.9 54.6
  .. .. .. .. .. .. ..@ area   : num 0.0156
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.7 55.6
  .. .. .. .. .. .. ..@ area   : num 0.000261
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.5 55.3
  .. .. .. .. .. .. ..@ area   : num 0.000482
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:32, 1:2] -131 -131 -131 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.4 54.9
  .. .. .. .. .. .. ..@ area   : num 0.0126
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:18, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.5 54.9
  .. .. .. .. .. .. ..@ area   : num 0.000223
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:27, 1:2] -131 -131 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.1 48.5
  .. .. .. .. .. .. ..@ area   : num 0.0116
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -123 -123 -123 -123 -123 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.5 48.9
  .. .. .. .. .. .. ..@ area   : num 0.0072
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -123 -123 -123 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.5 48.8
  .. .. .. .. .. .. ..@ area   : num 0.0137
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -123 -124 -124 -124 -124 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.3 48.8
  .. .. .. .. .. .. ..@ area   : num 0.00298
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -123 -123 -123 -123 -123 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -125.7 49.6
  .. .. .. .. .. .. ..@ area   : num 3.92
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:286, 1:2] -125 -125 -125 -125 -125 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -123.2 48.8
  .. .. .. .. .. .. ..@ area   : num 0.00266
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -123 -123 -123 -123 -123 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -133.1 54.2
  .. .. .. .. .. .. ..@ area   : num 0.00323
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -133 -133 -133 -133 -133 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.3 54.8
  .. .. .. .. .. .. ..@ area   : num 0.00564
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.9 54.5
  .. .. .. .. .. .. ..@ area   : num 0.00323
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.8 54.4
  .. .. .. .. .. .. ..@ area   : num 0.00168
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.8 54.4
  .. .. .. .. .. .. ..@ area   : num 0.00166
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132.3 53.8
  .. .. .. .. .. .. ..@ area   : num 0.00363
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.5 54.3
  .. .. .. .. .. .. ..@ area   : num 0.00531
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -130 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.8 54.2
  .. .. .. .. .. .. ..@ area   : num 0.00533
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.6 54.1
  .. .. .. .. .. .. ..@ area   : num 0.00162
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.3 54.1
  .. .. .. .. .. .. ..@ area   : num 0.00327
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130 54
  .. .. .. .. .. .. ..@ area   : num 0.00492
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131 54
  .. .. .. .. .. .. ..@ area   : num 0.0588
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] -130 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.6 53.8
  .. .. .. .. .. .. ..@ area   : num 0.00311
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.5 53.8
  .. .. .. .. .. .. ..@ area   : num 0.00236
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -130 -130 -130 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132.6 53.1
  .. .. .. .. .. .. ..@ area   : num 0.00359
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -133 -133 -133 -133 -133 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132.5 53.7
  .. .. .. .. .. .. ..@ area   : num 0.864
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:105, 1:2] -132 -133 -133 -133 -133 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132.1 53.2
  .. .. .. .. .. .. ..@ area   : num 0.00177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.7 53.5
  .. .. .. .. .. .. ..@ area   : num 0.00217
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132 53
  .. .. .. .. .. .. ..@ area   : num 0.0033
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.3 53.7
  .. .. .. .. .. .. ..@ area   : num 0.0295
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:31, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130 53.5
  .. .. .. .. .. .. ..@ area   : num 0.00325
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129 53.8
  .. .. .. .. .. .. ..@ area   : num 0.00676
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -132 53
  .. .. .. .. .. .. ..@ area   : num 0.0164
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.8 52.9
  .. .. .. .. .. .. ..@ area   : num 0.00289
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.7 52.8
  .. .. .. .. .. .. ..@ area   : num 0.00213
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.1 53.6
  .. .. .. .. .. .. ..@ area   : num 0.0322
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.5 53.4
  .. .. .. .. .. .. ..@ area   : num 0.00416
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.3 53.4
  .. .. .. .. .. .. ..@ area   : num 0.00199
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130.2 53.4
  .. .. .. .. .. .. ..@ area   : num 0.127
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:21, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.9 53.6
  .. .. .. .. .. .. ..@ area   : num 0.161
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:32, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.6 52.7
  .. .. .. .. .. .. ..@ area   : num 0.0182
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.1 53.4
  .. .. .. .. .. .. ..@ area   : num 0.0162
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.4 53.3
  .. .. .. .. .. .. ..@ area   : num 0.00242
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.7 53.1
  .. .. .. .. .. .. ..@ area   : num 0.00501
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.5 52.6
  .. .. .. .. .. .. ..@ area   : num 0.00189
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.3 53.2
  .. .. .. .. .. .. ..@ area   : num 0.0232
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130 53
  .. .. .. .. .. .. ..@ area   : num 0.00221
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -130 53
  .. .. .. .. .. .. ..@ area   : num 0.00268
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -130 -130 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.5 53.1
  .. .. .. .. .. .. ..@ area   : num 0.0165
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -129 -129 -130 -130 -130 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.4 52.4
  .. .. .. .. .. .. ..@ area   : num 0.00508
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.4 52.8
  .. .. .. .. .. .. ..@ area   : num 0.00279
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.9 52.8
  .. .. .. .. .. .. ..@ area   : num 0.332
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:107, 1:2] -132 -132 -132 -132 -132 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -131.1 52.1
  .. .. .. .. .. .. ..@ area   : num 0.0166
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -131 -131 -131 -131 -131 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129 53
  .. .. .. .. .. .. ..@ area   : num 0.276
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:36, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.6 52.8
  .. .. .. .. .. .. ..@ area   : num 0.00772
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:22, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.2 52.6
  .. .. .. .. .. .. ..@ area   : num 0.0474
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.3 52.7
  .. .. .. .. .. .. ..@ area   : num 0.0124
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.4 52.6
  .. .. .. .. .. .. ..@ area   : num 0.012
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.7 52.6
  .. .. .. .. .. .. ..@ area   : num 0.0245
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.4 52.5
  .. .. .. .. .. .. ..@ area   : num 0.00353
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.4 52.4
  .. .. .. .. .. .. ..@ area   : num 0.00436
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.8 52.4
  .. .. .. .. .. .. ..@ area   : num 0.0123
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.2 52.4
  .. .. .. .. .. .. ..@ area   : num 0.00698
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:20, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.5 52.2
  .. .. .. .. .. .. ..@ area   : num 0.00337
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -128 -128 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.4 52.2
  .. .. .. .. .. .. ..@ area   : num 0.00256
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.2 52.3
  .. .. .. .. .. .. ..@ area   : num 0.00324
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.3 52.2
  .. .. .. .. .. .. ..@ area   : num 0.00446
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128 52.2
  .. .. .. .. .. .. ..@ area   : num 0.00632
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.3 52.1
  .. .. .. .. .. .. ..@ area   : num 0.00775
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128 52.1
  .. .. .. .. .. .. ..@ area   : num 0.0127
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128 52
  .. .. .. .. .. .. ..@ area   : num 0.00378
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -127.6 52.2
  .. .. .. .. .. .. ..@ area   : num 0.0757
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:25, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128 52
  .. .. .. .. .. .. ..@ area   : num 0.0427
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128 51.7
  .. .. .. .. .. .. ..@ area   : num 0.00221
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.1 51.6
  .. .. .. .. .. .. ..@ area   : num 0.0298
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -127.7 51.5
  .. .. .. .. .. .. ..@ area   : num 0.00417
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129.2 50.8
  .. .. .. .. .. .. ..@ area   : num 0.00224
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -129 50.8
  .. .. .. .. .. .. ..@ area   : num 0.00224
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.8 50.8
  .. .. .. .. .. .. ..@ area   : num 0.00261
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128.7 50.8
  .. .. .. .. .. .. ..@ area   : num 0.00379
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -129 -129 -129 -129 -129 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -127.6 51.1
  .. .. .. .. .. .. ..@ area   : num 0.00225
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -128 50.9
  .. .. .. .. .. .. ..@ area   : num 0.00554
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -127.8 50.9
  .. .. .. .. .. .. ..@ area   : num 0.00304
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -128 -128 -128 -128 -128 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.9 50.9
  .. .. .. .. .. .. ..@ area   : num 0.00316
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.8 50.8
  .. .. .. .. .. .. ..@ area   : num 0.0104
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.7 50.8
  .. .. .. .. .. .. ..@ area   : num 0.00214
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -127.1 50.7
  .. .. .. .. .. .. ..@ area   : num 0.00807
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.7 50.7
  .. .. .. .. .. .. ..@ area   : num 0.00177
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.8 50.6
  .. .. .. .. .. .. ..@ area   : num 0.00181
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.4 50.7
  .. .. .. .. .. .. ..@ area   : num 0.0305
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -126 -126 -126 -126 -126 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.6 50.6
  .. .. .. .. .. .. ..@ area   : num 0.00213
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -127 -127 -127 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.5 50.6
  .. .. .. .. .. .. ..@ area   : num 0.00522
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -126 -126 -126 -126 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -126.5 50.6
  .. .. .. .. .. .. ..@ area   : num 0.0103
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -126 -126 -126 -127 -127 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -125.9 50.4
  .. .. .. .. .. .. ..@ area   : num 0.00454
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -126 -126 -126 -126 -126 ...
  .. .. .. .. .. [list output truncated]
  .. .. .. ..@ plotOrder: int [1:126] 123 14 31 57 59 45 44 77 27 109 ...
  .. .. .. ..@ labpt    : num [1:2] -125 55
  .. .. .. ..@ ID       : chr "11"
  .. .. .. ..@ area     : num 132
  .. ..$ :Formal class 'Polygons' [package "sp"] with 5 slots
  .. .. .. ..@ Polygons :List of 66
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72 53.7
  .. .. .. .. .. .. ..@ area   : num 193
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:6290, 1:2] -77.2 -77.2 -77.2 -77.2 -77.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 62.1
  .. .. .. .. .. .. ..@ area   : num 0.618
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:36, 1:2] -79.8 -79.9 -79.9 -79.9 -80 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.8 47.5
  .. .. .. .. .. .. ..@ area   : num 0.0538
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:40, 1:2] -61.7 -61.6 -61.5 -61.4 -61.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.3 54.8
  .. .. .. .. .. .. ..@ area   : num 0.031
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -79 -79 -79.1 -79.2 -79.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -74.2 62.7
  .. .. .. .. .. .. ..@ area   : num 0.0456
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -74.2 -74.5 -74.6 -74.6 -74.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.4 52
  .. .. .. .. .. .. ..@ area   : num 0.0461
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:15, 1:2] -79.5 -79.6 -79.6 -79.6 -79.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -69.1 59.1
  .. .. .. .. .. .. ..@ area   : num 0.0318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:17, 1:2] -69.1 -69.1 -69.1 -69.1 -69 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -64.7 60.3
  .. .. .. .. .. .. ..@ area   : num 0.0272
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:55, 1:2] -64.9 -64.9 -64.9 -64.9 -64.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.5 60.8
  .. .. .. .. .. .. ..@ area   : num 0.0171
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78.6 -78.6 -78.7 -78.7 -78.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80 59.8
  .. .. .. .. .. .. ..@ area   : num 0.0173
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.1 -80.2 -80.2 -80 -80 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.7 47.5
  .. .. .. .. .. .. ..@ area   : num 0.00773
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -61.6 -61.6 -61.7 -61.7 -61.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.9 53.3
  .. .. .. .. .. .. ..@ area   : num 0.0168
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -79.9 -80 -80 -79.9 -79.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -67.6 58.4
  .. .. .. .. .. .. ..@ area   : num 0.0125
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:19, 1:2] -67.6 -67.6 -67.5 -67.5 -67.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.8 57.4
  .. .. .. .. .. .. ..@ area   : num 0.00785
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -76.7 -76.8 -76.8 -76.8 -76.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78 62.6
  .. .. .. .. .. .. ..@ area   : num 0.0101
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78 -77.9 -77.9 -77.9 -77.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.6 56.6
  .. .. .. .. .. .. ..@ area   : num 0.00803
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -76.6 -76.7 -76.7 -76.7 -76.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.3 59.6
  .. .. .. .. .. .. ..@ area   : num 0.0068
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -80.2 -80.2 -80.2 -80.3 -80.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 53.1
  .. .. .. .. .. .. ..@ area   : num 0.0073
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -79.7 -79.8 -79.8 -79.8 -79.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.3 50.6
  .. .. .. .. .. .. ..@ area   : num 0.00701
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:16, 1:2] -59.2 -59.2 -59.2 -59.2 -59.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.5 59.4
  .. .. .. .. .. .. ..@ area   : num 0.00722
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -80.5 -80.5 -80.5 -80.5 -80.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.7 57.1
  .. .. .. .. .. .. ..@ area   : num 0.00599
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -76.7 -76.7 -76.7 -76.7 -76.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.7 62.6
  .. .. .. .. .. .. ..@ area   : num 0.00447
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:14, 1:2] -77.8 -77.8 -77.8 -77.7 -77.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.4 59.6
  .. .. .. .. .. .. ..@ area   : num 0.00438
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -80.5 -80.4 -80.4 -80.4 -80.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.9 57.5
  .. .. .. .. .. .. ..@ area   : num 0.00397
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -76.9 -76.9 -76.9 -76.9 -76.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.7 57
  .. .. .. .. .. .. ..@ area   : num 0.00487
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -76.7 -76.7 -76.7 -76.6 -76.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 52.1
  .. .. .. .. .. .. ..@ area   : num 0.00377
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -78.9 -79 -79 -78.9 -78.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -71.5 61.3
  .. .. .. .. .. .. ..@ area   : num 0.00385
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:12, 1:2] -71.4 -71.5 -71.5 -71.5 -71.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.9 58.8
  .. .. .. .. .. .. ..@ area   : num 0.00393
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -78.9 -78.9 -78.9 -78.9 -79 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.8 58.4
  .. .. .. .. .. .. ..@ area   : num 0.00384
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -80.8 -80.9 -80.9 -80.9 -80.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.7 59.3
  .. .. .. .. .. .. ..@ area   : num 0.00381
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -80.7 -80.6 -80.6 -80.6 -80.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.1 58.7
  .. .. .. .. .. .. ..@ area   : num 0.00356
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -80.1 -80.1 -80.1 -80.1 -80.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -61.5 47.8
  .. .. .. .. .. .. ..@ area   : num 0.0031
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -61.4 -61.4 -61.5 -61.5 -61.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -81.1 54.3
  .. .. .. .. .. .. ..@ area   : num 0.00359
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -81.1 -81.1 -81.1 -81.1 -81.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.9 58.3
  .. .. .. .. .. .. ..@ area   : num 0.00341
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -78 -77.9 -77.9 -77.9 -77.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77.6 58.2
  .. .. .. .. .. .. ..@ area   : num 0.0034
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -77.7 -77.7 -77.6 -77.6 -77.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.7 52.4
  .. .. .. .. .. .. ..@ area   : num 0.00366
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -78.7 -78.7 -78.8 -78.7 -78.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.7 56.3
  .. .. .. .. .. .. ..@ area   : num 0.00322
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:13, 1:2] -76.7 -76.7 -76.7 -76.7 -76.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.5 52.6
  .. .. .. .. .. .. ..@ area   : num 0.0032
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -79.5 -79.5 -79.5 -79.5 -79.5 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72.4 62.1
  .. .. .. .. .. .. ..@ area   : num 0.00318
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -72.4 -72.4 -72.4 -72.4 -72.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -77 57.7
  .. .. .. .. .. .. ..@ area   : num 0.00298
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -77 -77.1 -77 -77 -77 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.6 59.6
  .. .. .. .. .. .. ..@ area   : num 0.00288
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -80.6 -80.6 -80.6 -80.6 -80.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 58.7
  .. .. .. .. .. .. ..@ area   : num 0.00268
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -79.8 -79.8 -79.8 -79.8 -79.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.3 59.3
  .. .. .. .. .. .. ..@ area   : num 0.00286
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -80.3 -80.3 -80.3 -80.3 -80.3 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -72 61.9
  .. .. .. .. .. .. ..@ area   : num 0.00303
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -72 -72 -72 -72 -72 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.8 59
  .. .. .. .. .. .. ..@ area   : num 0.00273
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.8 -80.8 -80.8 -80.8 -80.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.7 58.8
  .. .. .. .. .. .. ..@ area   : num 0.0028
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -78.6 -78.6 -78.6 -78.6 -78.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.1 60.8
  .. .. .. .. .. .. ..@ area   : num 0.00258
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78.1 -78.1 -78.1 -78.2 -78.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.4 52.3
  .. .. .. .. .. .. ..@ area   : num 0.00236
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -79.3 -79.3 -79.4 -79.4 -79.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.1 58.3
  .. .. .. .. .. .. ..@ area   : num 0.00257
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -78.1 -78.1 -78.1 -78.1 -78.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.7 59
  .. .. .. .. .. .. ..@ area   : num 0.00232
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.6 -80.7 -80.7 -80.7 -80.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.8 59.2
  .. .. .. .. .. .. ..@ area   : num 0.00234
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -80.7 -80.7 -80.7 -80.7 -80.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.6 58.5
  .. .. .. .. .. .. ..@ area   : num 0.00237
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78.6 -78.6 -78.6 -78.6 -78.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -80.7 59.4
  .. .. .. .. .. .. ..@ area   : num 0.00237
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -80.7 -80.7 -80.8 -80.8 -80.8 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.6 56.4
  .. .. .. .. .. .. ..@ area   : num 0.00201
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -76.7 -76.6 -76.6 -76.6 -76.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.8 56.2
  .. .. .. .. .. .. ..@ area   : num 0.00211
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -76.8 -76.8 -76.8 -76.8 -76.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -59.4 50.5
  .. .. .. .. .. .. ..@ area   : num 0.00209
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:11, 1:2] -59.4 -59.4 -59.4 -59.4 -59.4 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.1 52.5
  .. .. .. .. .. .. ..@ area   : num 0.00213
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:8, 1:2] -79.1 -79.1 -79.1 -79.1 -79.1 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.8 59.9
  .. .. .. .. .. .. ..@ area   : num 0.002
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -79.8 -79.8 -79.8 -79.8 -79.7 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.2 51.9
  .. .. .. .. .. .. ..@ area   : num 0.00183
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -79.2 -79.2 -79.2 -79.2 -79.2 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -78.6 58.6
  .. .. .. .. .. .. ..@ area   : num 0.0019
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -78.7 -78.7 -78.7 -78.6 -78.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.9 58.2
  .. .. .. .. .. .. ..@ area   : num 0.00194
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:10, 1:2] -79.8 -79.9 -79.9 -79.9 -79.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -79.6 53.5
  .. .. .. .. .. .. ..@ area   : num 0.00158
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:7, 1:2] -79.6 -79.6 -79.6 -79.6 -79.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -76.6 56.7
  .. .. .. .. .. .. ..@ area   : num 0.00175
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -76.7 -76.6 -76.6 -76.6 -76.6 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -66 59
  .. .. .. .. .. .. ..@ area   : num 0.00174
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:9, 1:2] -66 -66 -66 -66 -66 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -69.2 47.2
  .. .. .. .. .. .. ..@ area   : num 10.5
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:1017, 1:2] -66.9 -66.9 -66.9 -66.9 -66.9 ...
  .. .. .. .. ..$ :Formal class 'Polygon' [package "sp"] with 5 slots
  .. .. .. .. .. .. ..@ labpt  : num [1:2] -62.9 49.5
  .. .. .. .. .. .. ..@ area   : num 0.937
  .. .. .. .. .. .. ..@ hole   : logi FALSE
  .. .. .. .. .. .. ..@ ringDir: int 1
  .. .. .. .. .. .. ..@ coords : num [1:39, 1:2] -62.8 -63.5 -63.6 -63.7 -63.7 ...
  .. .. .. ..@ plotOrder: int [1:66] 1 65 66 2 3 6 5 7 4 8 ...
  .. .. .. ..@ labpt    : num [1:2] -72 53.7
  .. .. .. ..@ ID       : chr "12"
  .. .. .. ..@ area     : num 205
  ..@ plotOrder  : int [1:13] 8 7 13 12 9 1 3 2 11 4 ...
  ..@ bbox       : num [1:2, 1:2] -141 42 -52.5 83.1
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:2] "x" "y"
  .. .. ..$ : chr [1:2] "min" "max"
  ..@ proj4string:Formal class 'CRS' [package "sp"] with 1 slots
  .. .. ..@ projargs: chr "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(canada.map)
## maybe str(canada.map) ; plot(canada.map) ...
}
\keyword{datasets}
