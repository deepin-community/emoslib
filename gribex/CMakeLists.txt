
list( APPEND gribex_src_files
  ECMWFdefinitions.c
  ECMWFdefinitions.h
  abortx.F
  bufrin.F
  c2bitw.F
  c2cwid.F
  c2dosd.F
  c2gene.F
  c2ordr.F
  c2pack.F
  c2pkvw.F
  c2rnge.F
  c2rows.F
  calcop.F
  cheknum.F
  chktab2.F
  cmpck.F
  codeps.F
  comars.h
  comcomm.h
  comgrb.h
  confp.F
  confp2.F
  confp3.F
  confpa.F
  csect4.F
  csgnbt.c
  d2ordr.F
  d2rosd.F
  decext.F
  decfp.F
  decfp2.F
  decogb.F
  decogc.F
  decogd.F
  decogr.F
  decops.F
  decops2.F
  dggsec2.F
  dlasec2.F
  dllsec2.F
  dmesec2.F
  docsec2.F
  dpssec2.F
  dsect4a.F
  dsgnbt.c
  dshsec2.F
  dsvsec2.F
  ecdef1.F
  ecdef1.h
  ecdef10.F
  ecdef10.h
  ecdef11.F
  ecdef11.h
  ecdef12.F
  ecdef12.h
  ecdef13.F
  ecdef13.h
  ecdef14.F
  ecdef14.h
  ecdef15.F
  ecdef15.h
  ecdef16.F
  ecdef16.h
  ecdef17.F
  ecdef17.h
  ecdef18.F
  ecdef18.h
  ecdef19.F
  ecdef19.h
  ecdef2.F
  ecdef2.h
  ecdef3.F
  ecdef3.h
  ecdef4.F
  ecdef4.h
  ecdef5.F
  ecdef5.h
  ecdef50.F
  ecdef6.F
  ecdef6.h
  ecdef7.F
  ecdef7.h
  ecdef8.F
  ecdef8.h
  ecdef9.F
  ecdef9.h
  ecdf190.h
  ecdf191.F
  ecdf191.h
  ecloc1.F
  eggsec2.F
  elasec2.F
  ellsec2.F
  emesec2.F
  eocsec2.F
  epssec2.F
  eshsec2.F
  esvsec2.F
  exscal.F
  extmap.F
  findLocalDefinitionFile.c
  fortranInterface.c
  ftn1cr.F
  gbitmap.F
  gdecode.c
  gdecode1.c
  gdecode2.c
  gdecodeStruct.h
  genbin.F
  gendir.F
  getchd.F
  getfb2.F
  getfpd.F
  getib1.F
  getib2.F
  getib3.F
  getind.F
  getlgd.F
  getsetValues.c
  getsetValues.h
  getsys.F
  getusr.F
  grbcom.h
  grchk1.F
  grchk2.F
  grchk3.F
  grchk4.F
  gribex.F
  gribex.h
  gribin.F
  gribnum.F
  grpr190.c
  grprs0.F
  grprs1.F
  grprs1b.F
  grprs2.F
  grprs3.F
  grprs4.F
  grprs4w.F
  grsdbg.F
  grsdef.F
  grsmax.F
  grsmkp.F
  grsmok.F
  grsn2o.F
  grsref.F
  grsrnd.F
  grsubc.F
  grsvck.F
  grsx2o.F
  gscale.F
  handleLocalDefinitions.c
  handleLocalDefinitions.h
  inscal.F
  insmp1.F
  insmp2.F
  inxbit.F
  inxmap.F
  jfree.c
  jmalloc.c
  kwchk1.F
  kwloc1.F
  kwprs1.F
  maxmin.F
  maxmn2.F
  maxmni.F
  modval.F
  offset.F
  offset2.F
  orefdat.c
  packcf.F
  prtbin.F
  prtbk1.F
  prtbk2.F
  prtbl1.F
  prtbl2.F
  ptquasi.F
  qu2reg.F
  qu2reg3.F
  reclen.F
  ref2grb.F
  remsp.F
  repchr.F
  revero.F
  rorint.F
  rowina.F
  rowina2.F
  rowina3.F
  rtb.F
  scm0.F
  sencode.c
  sencode1.c
  sencode2.c
  setpar.F
  sfbits.h
  swap4.c
  tab2fil.F
  u2l1cr.F
  uncmpck.F
  unpkcf.F
  valpina.c
  vod2uv.F
  yyyy2cy.F

  # this is not a complete list of headers, but they seem to be sufficient
  ecdef10.h
  ecdef11.h
  ecdef12.h
  ecdef13.h
  ecdef14.h
  ecdef15.h
  ecdef16.h
  ecdef17.h
  ecdef18.h
  ecdef19.h
  ecdef50.h
  eocsec2.h
  gdecode.h
  gdecode1.h
  gdecode2.h
  grib_int_t.h
  sencode.h
  sencode1.h
  sencode2.h
)

# emoscyc.F is not used anywhere and creates unref symbol when used in shared library
# however, it has been part of the static library and so it seems safer to keep it there
if(LIB_TYPE STREQUAL "STATIC")
  list(APPEND gribex_src_files emoscyc.F)
endif()


foreach( src ${gribex_src_files} )
  list( APPEND gribex_srcs gribex/${src} )
endforeach()

set( gribex_srcs ${gribex_srcs} PARENT_SCOPE )

