list( APPEND pbio_src_files
  bufrgrib.h
  fort2c.h
  blokex.F
  pbbufr.F
  pbcrex.F
  pbgrib.F
  pbionum.F
  emosnum.F
  pbpseu.F
  PBGroutines.c
  extras.c
  fort2c.c
  lwsize.c
  mvchars.c
  pbio.c
  readany.c
  readbufr.c
  readcrex.c
  readgrib.c
  readnext.c
  readprod.c
  recheckLength.c
  gbyte_le.c

  # this is not a complete list of headers, but they seem to be sufficient
  PBGroutines.h
  fileRead.h
  sizeRoutines.h
)

foreach( src ${pbio_src_files} )
  list( APPEND pbio_srcs pbio/${src} )
endforeach()

set( pbio_srcs ${pbio_srcs} PARENT_SCOPE )

