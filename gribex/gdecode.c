/**
* Copyright 1981-2016 ECMWF.
*
* This software is licensed under the terms of the Apache Licence
* Version 2.0 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
*
* In applying this licence, ECMWF does not waive the privileges and immunities
* granted to it by virtue of its status as an intergovernmental organisation
* nor does it submit to any jurisdiction.
*/

#include "gdecode.h"

#define GRIB 0x47524942
#define BIT1 0x80
#define BIT2 0x40

#define DEBUGOFF 1
#define DEBUG1 (debugSet > DEBUGOFF )
#define DEBUG2 (debugSet > (DEBUGOFF + 1) )
static char * debugLevel;
static int debugSet = 0;


fortint findSectionOffsets(
  unsigned char * buffer,
  fortint* is0,
  fortint* is1,
  fortint* is2,
  fortint* is3,
  fortint* is4,
  fortint* iedition) {
long section0Offset, section1Offset, section2Offset;
long section3Offset, section4Offset, edition;
int large = 0;
int found = 0;
int code = 0;
unsigned char p, edit_num, flag23;
int section0Length = 8, section1Length, section2Length, section3Length,
    section4Length;
long total;
unsigned char * q = buffer;

/*
//  Read bytes until "GRIB" found
*/
    do {
      code = ( (code << 8) | ((*q++)<<0) ) & 0xFFFFFFFF;
      if (code == GRIB ) found = 1;
    } while ( ! found );
    section0Offset = (long) (q - buffer - 4);
/*
//  Find out which edition of GRIB is present (default is 1)
*/
    edition = 1;
    section1Offset = section0Offset + 8;
    if( (*(buffer+21-section0Offset) == '\0') &&
        (*(buffer+22-section0Offset) == '\0') ) {
/*
//    GRIB edition -1
*/
      edition = -1;
      section1Offset = section0Offset;
      section1Length = 20;
      section0Length = 4;
    }
    else {
      total = THREEBYTEINT(q); q += 3;
      if( total == 24 ) {
/*
//      Check for edition number
*/
        if( ONEBYTEINT(q) != 0) return 1;
/*
//      GRIB edition 0
*/
        q++;
        edition = 0;
        section1Length = 24;
        section1Offset = section0Offset + 4;
        section0Length = 4;
      }
/*
//    See if it is an extra large (wave) product
*/
      if( total > 0x800000 ) {
        total = (total&0x7fffff) * 120;
        large = 1;
      }
    }

    if( edition == 1 ) {
/*
//    Read length of section 1
*/
      q++;
      section1Length = THREEBYTEINT(q); q += 3;
      section1Offset = section0Offset + section0Length;
    }
/*
//  Now figure out if sections 2/3 are present
*/
    q += 4;
    flag23 = *q++;
    section2Length = flag23 & BIT1;
    section3Length = flag23 & BIT2;
/*
//  Advance to end of section 1
*/
    q = buffer + section1Offset + section1Length;
/*
//  Read section 2 length if it is present
*/
    if( section2Length ) {
      section2Offset = (fortint) (q - buffer);
      section2Length = THREEBYTEINT(q); q+= 3;
      q = buffer + section2Offset + section2Length;
    }
    else {
      section2Length = 0;
      section2Offset = 0;
    }
/*
//  Read section 3 length if it is present
*/
    if( section3Length ) {
      section3Offset = (fortint) (q - buffer);
      section3Length = THREEBYTEINT(q); q+= 3;
    q = buffer + section3Offset + section3Length;
    }
    else {
      section3Length = 0;
      section3Offset = 0;
    }
/*
//  Read section 4 length
*/
    section4Offset = (fortint) (q - buffer);
    section4Length = THREEBYTEINT(q); q+= 3;
    if( large ){
      total = total + section4Length;
      section4Length = total - section4Offset;
    }
/*
//  Advance to end of section 4
*/
    q = buffer + section4Offset + section4Length;
/*
//  Check section 5, ie 7777 group is in the expected place
*/
    if( FOURBYTEINT(q) != 0x37373737 ) {
      if(!large) {
             printf("7777 group not found\n");
             return 15;
      }
    }
/*
//  Success!
*/
    *is0 = (fortint) section0Offset;
    *is1 = (fortint) section1Offset;
    *is2 = (fortint) section2Offset;
    *is3 = (fortint) section3Offset;
    *is4 = (fortint) section4Offset;
    *iedition = (fortint) edition;

    return 0;
}

fortint prepareGrib( gribProduct ** grib, unsigned char * buffer) {
fortint is0, is1, is2, is3, is4, iedition;
long status;
unsigned char * p;
gribProduct * gp;

  if( *grib == NULL ) {
    gp = (gribProduct *) allocateMemory(sizeof(gribProduct));
    *grib = gp;
  }
  else
    gp = *grib;

  status = findSectionOffsets(buffer,&is0,&is1,&is2,&is3,&is4,&iedition);
  if( status ) {
    printf("prepareGrib: Problem finding GRIB section offsets\n");
    return (fortint) 1;
  }

  if( iedition != 1 ) {
    printf("prepareGrib: Only GRIB edition 1 allowed.\n");
    return (fortint) 1;
  }

  p = (unsigned char *) (buffer + is0);
  gp->g0 = (gribSection0 *) p;

  p = (unsigned char *) (buffer + is1);
  gp->g1 = (gribSection1 *) p;

  if( is2 == 0 )
    gp->g2 = NULL;
  else {
    p = (unsigned char *) (buffer + is2);
    gp->g2 = (gribSection2 *) p;
  }

  if( is3 == 0 )
    p = NULL;
  else {
    p = (unsigned char *) (buffer + is3);
    gp->g3 = (gribSection3 *) p;
  }

  p = (unsigned char *) (buffer + is4);
  gp->g4 = (gribSection4 *) p;

  return (fortint) 0;
}

fortint convertGRIBFloatToIEEE(unsigned char * valuePointer) {
fortint temp;
fortint sign, exponent, mantissa, newmantissa, newexponent;
/*
// The GRIB float format is defined in WMO manual on codes for
// FM 92 GRIB, 92.6.4:
//    R = (-1)^sign * 2^(-24) * mantissa * 16^(exponent-64)
*/

  temp = FOURBYTEINT(valuePointer);
  if( temp == 0 )
    return (fortint) 0;
  else {
    sign = temp & 0x80000000;
    exponent = (((temp & 0x7f000000)>>24) - 64);
    mantissa = (temp & 0xffffff);
    newmantissa = mantissa<<8;
    newexponent = exponent*4-1;
    while( !(newmantissa & 0x80000000) ) {
      newexponent--;
      newmantissa <<= 1;
    }
    newexponent += 127;
    return (fortint) (sign | (newexponent<<23) | ((newmantissa>>8)&0x7fffff) );
  }
}

fortdouble realValue(unsigned char * value) {
float realNumber;
fortint realNumberBits;

  realNumberBits = convertGRIBFloatToIEEE(value);
  memcpy((char *)&realNumber,(char *)&realNumberBits,4);
  return (fortdouble) realNumber;
}

fortdouble referenceValue(gribProduct * grib) {
  return realValue(grib->g4->referenceValue);
}

fortdouble RGREFVL(gribProduct ** grib) {
  return referenceValue( (*grib) );
}

fortint IGBTSPV(gribProduct ** grib) {
  return g4_bits( (*grib) );
}

fortdouble RGDSCAL(gribProduct ** grib) {
fortint value = g4_scale( (*grib) );
double two = 2.0;
fortdouble scale;
/*
// High order bit set => negative value
*/
  if( value & 0x8000 ) value = - (value & 0x7fff);
  if( value == 0 )
    scale = 1.0;
  else
    scale = pow(two,(double)value);
  return scale;
}

fortint IGUNUSD(gribProduct ** grib) {
  return (g4_flag( (*grib) ) & 0xf);
}

fortint GDECODE(gribProduct ** grib, unsigned char * buffer) {
fortint status;
/*
// See if DEBUG switched on.
*/
    if( ! debugSet ) {
      debugLevel = getenv("GDECODE_DEBUG");
      if( debugLevel == NULL )
        debugSet = DEBUGOFF;              /* off */
      else {
        int loop;
        for( loop = 0; loop < strlen(debugLevel) ; loop++ ) {
          if( ! isdigit(debugLevel[loop]) ) {
            printf("Invalid number string in GDECODE_DEBUG: %s\n", debugLevel);
            printf("GDECODE_DEBUG must comprise only digits [0-9].\n");
            debugSet = DEBUGOFF;
          }
        }
        debugSet = DEBUGOFF + atol( debugLevel );
      }
      if( DEBUG1 ) printf("GDECODE: GDECODE_DEBUG switched on, level = %s\n",
                          debugLevel);
    }

  status = prepareGrib( grib, buffer);

  if( DEBUG1 ) {
    if( status )
      printf("GDECODE: prepareGrib failed.\n");
    else
      printf("GDECODE: prepareGrib ran OK\n");
  }

  return status;
}

fortint IGLNGTH(gribProduct ** grib) {
  return g0_length( (*grib) );
}

fortint IGDATE(gribProduct ** grib) {
fortint date;
gribProduct * g = *grib;

  if( (g1_century(g) == 255) || (g1_year(g) == 255) || (g1_day(g) == 255) )
    return (fortint) g1_month(g);

  date = (g1_century(g)-1)*1000000 +
         g1_year(g)*10000 +
         g1_month(g)*100 +
         g1_day(g);

  return date;
}

fortint IGTIME(gribProduct ** grib) {
fortint time;
gribProduct * g = *grib;

  if( (g1_hour(g) == 255) || (g1_minute(g) == 255) ) return (fortint) -1;

  time = g1_hour(g)*100 +
         g1_minute(g);

  return time;
}

fortint IGSTEP(gribProduct ** grib) {
fortint timeunit, timerange, step;
gribProduct * g = *grib;

  switch( (int) g1_timeunit(g) ) {

    case 2:
      timeunit = 24;
      break;

    case 10:
      timeunit = 3;
      break;

    case 11:
      timeunit = 6;
      break;

    case 12:
      timeunit = 12;
      break;

    default:
      timeunit = 1;
      break;
  }

  switch( (int) g1_timerange(g) ) {

    case 0:
    case 1:
    case 113:
    case 114:
    case 115:
    case 116:
    case 117:
    case 118:
    case 123:
    case 124:
      step = g1_P1(g);
      break;

    case 10:
      step = (g1_P1(g)<<8) | (g1_P2(g)<<0);
      break;

    default:
      step = g1_P2(g);
      break;
 }

  step *= timeunit;

  return step;
}

fortint IGSTEP1(gribProduct ** grib) {
  return g1_P1( (*grib) );
}

fortint IGSTEP2(gribProduct ** grib) {
  return g1_P2( (*grib) );
}

fortint IGTABLE(gribProduct ** grib) {
  return g1_table( (*grib) );
}

fortint IGCENTR(gribProduct ** grib) {
  return g1_centre( (*grib) );
}

fortint IGPARAM(gribProduct ** grib) {
  return g1_parameter( (*grib) );
}

fortint IGLEVEL(gribProduct ** grib) {
fortint level;
gribProduct * g = *grib;

  switch( (int) g1_typeOfLevel(g) ) {

    case  20:
    case 100:
    case 103:
    case 105:
    case 107:
    case 109:
    case 111:
    case 113:
    case 115:
    case 117:
    case 119:
    case 125:
    case 127:
    case 160:
    case 210:
      level = (g1_level1(g)<<8) | (g1_level2(g)<<0);
      break;

    case 101:
    case 104:
    case 106:
    case 108:
    case 110:
    case 112:
    case 114:
    case 116:
    case 120:
    case 121:
    case 128:
    case 141:
      level = g1_level1(g)*1000 + g1_level2(g);
      break;

    default:
      level = 0;
      break;
  }

  return level;
}

fortint IGLEVL1(gribProduct ** grib) {
gribProduct * g = *grib;

  switch( (int) g1_typeOfLevel(g) ) {

    case 101:
    case 104:
    case 106:
    case 108:
    case 110:
    case 112:
    case 114:
    case 116:
    case 120:
    case 121:
    case 128:
    case 141:
      return g1_level1(g);

    default:
      return IGLEVEL(grib);
  }
}

fortint IGLEVL2(gribProduct ** grib) {
gribProduct * g = *grib;

  switch( (int) g1_typeOfLevel(g) ) {

    case 101:
    case 104:
    case 106:
    case 108:
    case 110:
    case 112:
    case 114:
    case 116:
    case 120:
    case 121:
    case 128:
    case 141:
      return g1_level2(g);

    default:
      return IGLEVEL(grib);
  }
}

fortint IGDEFIN(gribProduct ** grib) {
  if( ecmwfLocalDefinitionPresent((*grib)) )
    return g1_definition( (*grib) );

  if(  centreUsingECMWFLocalDefinition((*grib)) )
    return g1_definition( (*grib) );
  else {
    if( DEBUG1 ) printf("IGDEFIN: no ECMWF local definition found\n");
    return (fortint) -999999;
  }
}

fortint IGCLASS(gribProduct ** grib) {
  if( ecmwfLocalDefinitionPresent((*grib)) )
    return g1_class( (*grib) );

  if(  centreUsingECMWFLocalDefinition((*grib)) )
    return g1_class( (*grib) );
  else {
    if( DEBUG1 ) printf("GCLASS: no ECMWF class found\n");
    return (fortint) -999999;
  }
}

fortint IGTYPE(gribProduct ** grib) {
  if( ecmwfLocalDefinitionPresent((*grib)) )
    return g1_type( (*grib) );

  if(  centreUsingECMWFLocalDefinition((*grib)) )
    return g1_type( (*grib) );
  else {
    if( DEBUG1 ) printf("IGTYPE: no ECMWF type found\n");
    return (fortint) -999999;
  }
}

fortint IGSTREM(gribProduct ** grib) {
  if( ecmwfLocalDefinitionPresent((*grib)) )
    return g1_stream( (*grib) );

  if(  centreUsingECMWFLocalDefinition((*grib)) )
    return g1_stream( (*grib) );
  else {
    if( DEBUG1 ) printf("IGSTREM: no ECMWF stream found\n");
    return (fortint) -999999;
  }
}

fortint IGEXPVR(gribProduct ** grib) {
  if( ecmwfLocalDefinitionPresent((*grib)) )
    return g1_expver( (*grib) );

  if(  centreUsingECMWFLocalDefinition((*grib)) )
    return g1_expver( (*grib) );
  else {
    if( DEBUG1 ) printf("IGEXPVR: no ECMWF experiment version found\n");
    return (fortint) -999999;
  }
}

fortint IGREPRS(gribProduct ** grib) {
  return g2_datatype( (*grib) );
}

fortint IGNUMPV(gribProduct ** grib) {
  return g2_NV( (*grib) );
}

fortint GPV(gribProduct ** grib, fortdouble * list, fortint * sizeList ) {
gribProduct * g = *grib;
fortdouble * l = list;
int loop, numberOfVerticalCoordinates;
unsigned char * value;

  if( !(numberOfVerticalCoordinates = g2_NV(g)) ) return (fortint) 0;

  if( numberOfVerticalCoordinates > *sizeList ) return (fortint) -1;

  value = (unsigned char *)(g->g2) + g2_PV_PL(g) - 1;

  for( loop = 0; loop < numberOfVerticalCoordinates; loop++ ) {
    *(l+loop) = realValue(value);
    value += 4;
  }
  return (fortint) numberOfVerticalCoordinates;
}

fortint GPL(gribProduct ** grib, fortint * list, fortint * sizeList ) {
gribProduct * g = *grib;
fortint * l = list;
int loop, numberOfVerticalCoordinates, numberOfRows;
unsigned char * value;

  numberOfRows = g2_nj(g);
  if( numberOfRows > *sizeList ) return (fortint) -1;

  numberOfVerticalCoordinates = g2_NV(g);

  value = (unsigned char *)(g->g2) + g2_PV_PL(g) +
                                             numberOfVerticalCoordinates*4 - 1;

  for( loop = 0; loop < numberOfRows; loop++ ) {
    *(l+loop) = TWOBYTEINT(value);
    value += 2;
  }
  return (fortint) numberOfRows;
}

fortint IGNI(gribProduct ** grib) {
  if( generalLatLonGrid( (*grib) ) )
    if( g2_ni( (*grib) ) == 65535 )
      return (fortint) -1;
    else
     return g2_ni( (*grib) );
  else {
    if( DEBUG1 ) printf("IGNI: not a lat/long grid\n");
    return (fortint) -999999;
  }
}

fortint IGNJ(gribProduct ** grib) {
  if( generalLatLonGrid( (*grib) ) )
    if( g2_nj( (*grib) ) == 65535 )
      return (fortint) -1;
    else
      return g2_nj( (*grib) );
  else {
    if( DEBUG1 ) printf("IGNJ: not a lat/long grid\n");
    return (fortint) -999999;
  }
}

fortdouble RGNWLAT(gribProduct ** grib) {
fortint value;
  if( generalLatLonGrid( (*grib) ) ) {
    value = g2_firstLat( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGNWLAT: not a lat/long grid\n");
    return (fortdouble) -999999.0;
  }
}

fortdouble RGNWLON(gribProduct ** grib) {
fortint value;
  if( generalLatLonGrid( (*grib) ) ) {
    value = g2_firstLon( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGNWLON: not a lat/long grid\n");
    return (fortdouble) -999999.0;
  }
}

fortdouble RGSELAT(gribProduct ** grib) {
fortint value;
  if( generalLatLonGrid( (*grib) ) ) {
    value = g2_lastLat( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGSELAT: not a lat/long grid\n");
    return (fortdouble) -999999.0;
  }
}

fortdouble RGSELON(gribProduct ** grib) {
fortint value;
  if( generalLatLonGrid( (*grib) ) ) {
    value = g2_lastLon( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGSELON: not a lat/long grid\n");
    return (fortdouble) -999999.0;
  }
}

fortint IGRESCO(gribProduct ** grib) {
  if( generalLatLonGrid( (*grib) ) )
    return g2_resAndComp( (*grib) );
  else {
    if( DEBUG1 ) printf("IGRESCO: not a lat/long grid\n");
    return (fortint) -999999;
  }
}

fortdouble RGDI(gribProduct ** grib) {
fortint value;
  if( generalLatLonGrid( (*grib) ) ) {
    value = g2_di( (*grib) );
    if( value == 65535 )
      return (fortdouble) -1;
    else
      return ((fortdouble) value )/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGDI: not a lat/long grid\n");
    return (fortdouble) -999999.0;
  }
}

fortdouble RGDJ(gribProduct ** grib) {
fortint value;
  if( generalLatLonGrid( (*grib) ) ) {
    value = g2_dj( (*grib) );
    if( value == 65535 )
      return (fortdouble) -1;
    else
      return ((fortdouble) value )/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGDJ: not a lat/long grid\n");
    return (fortdouble) -999999.0;
  }
}

fortint IGGAUSS(gribProduct ** grib) {
  if( anyGaussianGrid( (*grib) ) )
    return g2_gaussNumber( (*grib) );
  else {
    if( DEBUG1 ) printf("IGGAUSS: not a gaussian grid\n");
    return (fortint) -999999;
  }
}

fortint IGSCANM(gribProduct ** grib) {
  if( generalLatLonGrid( (*grib) ) )
    return g2_scan( (*grib) );
  else {
    if( DEBUG1 ) printf("IGSCANM: not a lat/long grid\n");
    return (fortint) -999999;
  }
}

fortint IGJ(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g2_J( (*grib) );
  else {
    if( DEBUG1 ) printf("IGJ: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortint IGK(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g2_K( (*grib) );
  else {
    if( DEBUG1 ) printf("IGK: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortint IGM(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g2_M( (*grib) );
  else {
    if( DEBUG1 ) printf("IGM: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortint IGREPMO(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g2_repmode( (*grib) );
  else {
    if( DEBUG1 ) printf("IGREPMO: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortdouble RGIP(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return ((fortdouble) g4_ip( (*grib) )/1000.0);
  else {
    if( DEBUG1 ) printf("RGIP: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortint IGTJ(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g4_j( (*grib) );
  else {
    if( DEBUG1 ) printf("IGTJ: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortint IGTK(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g4_k( (*grib) );
  else {
    if( DEBUG1 ) printf("IGTK: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortint IGTM(gribProduct ** grib) {
  if( anySpectralField( (*grib) ) )
    return g4_m( (*grib) );
  else {
    if( DEBUG1 ) printf("IGTM: not a spectral field\n");
    return (fortint) -999999;
  }
}

fortdouble RGLATRP(gribProduct ** grib) {
fortint value;
  if( generalRotatedGrid( (*grib) ) ) {
    value = g2_latSouthPole( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGLATRP: not a rotated grid\n");
    return (fortint) -999999;
  }
}

fortdouble RGLONRP(gribProduct ** grib) {
fortint value;
  if( generalRotatedGrid( (*grib) ) ) {
    value = g2_lonSouthPole( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( DEBUG1 ) printf("RGLONRP: not a rotated grid\n");
    return (fortint) -999999;
  }
}

fortdouble RGROTAT(gribProduct ** grib) {
unsigned char * rotation;
  if( generalRotatedGrid( (*grib) ) ) {
    rotation = ((*grib)->g2)->grid.latlon.angleOfRotationOrStretchingFactor;
    return realValue(rotation);
  }
  else {
    if( DEBUG1 ) printf("RGROTAT: not a rotated grid\n");
    return (fortint) -999999;
  }
}

fortdouble RGLATSP(gribProduct ** grib) {
fortint value;
  if( generalStretchedAndRotatedGrid( (*grib) ) ) {
    value = g2_latStretching( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( generalStretchedGrid( (*grib) ) ) {
      value = g2_latSouthPole( (*grib) );
      if( value & 0x800000 ) value = - (value & 0x7fffff);;
      return ((fortdouble) value)/1000.0;
    }
    else {
      if( DEBUG1 ) printf("RGLATSP: not a stretched/rotated grid\n");
      return (fortint) -999999;
    }
  }
}

fortdouble RGLONSP(gribProduct ** grib) {
fortint value;
  if( generalStretchedAndRotatedGrid( (*grib) ) ) {
    value = g2_lonStretching( (*grib) );
    if( value & 0x800000 ) value = - (value & 0x7fffff);;
    return ((fortdouble) value)/1000.0;
  }
  else {
    if( generalStretchedGrid( (*grib) ) ) {
      value = g2_lonSouthPole( (*grib) );
      if( value & 0x800000 ) value = - (value & 0x7fffff);;
      return ((fortdouble) value)/1000.0;
    }
    else {
      if( DEBUG1 ) printf("RGLONSP: not a stretched/rotated grid\n");
      return (fortint) -999999;
    }
  }
}

fortdouble RGSFACT(gribProduct ** grib) {
unsigned char * factor;
  if( generalStretchedAndRotatedGrid( (*grib) ) ) {
    factor = ((*grib)->g2)->grid.latlon.stretchingFactor;
    return realValue(factor);
  }
  else
    if( generalStretchedGrid( (*grib) ) ) {
      factor = ((*grib)->g2)->grid.latlon.angleOfRotationOrStretchingFactor;
      return realValue(factor);
    }
    else {
      if( DEBUG1 ) printf("RGSFACT: not a stretched/rotated grid\n");
      return (fortint) -999999;
    }
}

fortint IGLEVTY(gribProduct ** grib) {
  if( DEBUG2) printf("IGLEVTY\n");
  return (fortint) g1_typeOfLevel((*grib));
}

fortdouble RGLEVTY(gribProduct ** grib) {
  if( DEBUG2) printf("RGLEVTY\n");
  return (fortdouble) IGLEVTY(grib);
}

fortint IGTUNIT(gribProduct ** grib) {
  if( DEBUG2) printf("IGTUNIT\n");
  return (fortint) g1_timeunit((*grib));
}

fortdouble RGTUNIT(gribProduct ** grib) {
  if( DEBUG2) printf("RGTUNIT\n");
  return (fortdouble) IGTUNIT(grib);
}

fortint IGTRIND(gribProduct ** grib) {
  if( DEBUG2) printf("IGTRIND\n");
  return (fortint) g1_timerange((*grib));
}

fortdouble RGTRIND(gribProduct ** grib) {
  if( DEBUG2) printf("RGTRIND\n");
  return (fortdouble) IGTRIND(grib);
}

fortint IGNUMAV(gribProduct ** grib) {
  if( DEBUG2) printf("IGNUMAV\n");
  return (fortint) g1_number((*grib));
}

fortdouble RGNUMAV(gribProduct ** grib) {
  if( DEBUG2) printf("RGNUMAV\n");
  return (fortdouble) IGNUMAV(grib);
}

fortint IGNUMMS(gribProduct ** grib) {
  if( DEBUG2) printf("IGNUMMS\n");
  return (fortint) g1_missing((*grib));
}

fortdouble RGNUMMS(gribProduct ** grib) {
  if( DEBUG2) printf("RGNUMMS\n");
  return (fortdouble) IGNUMMS(grib);
}

fortint IGSUBID(gribProduct ** grib) {
  if( DEBUG2) printf("IGSUBID\n");
  return (fortint) g1_subcentre((*grib));
}

fortdouble RGSUBID(gribProduct ** grib) {
  if( DEBUG2) printf("RGSUBID\n");
  return (fortdouble) IGSUBID(grib);
}

fortint IGUDECF(gribProduct ** grib) {
fortint scale;

  if( DEBUG2) printf("IGUDECF\n");

  scale = g1_scale((*grib));
  if( scale & 0x8000 ) scale = - (scale & 0x7fff);
  return scale;
}

fortdouble RGUDECF(gribProduct ** grib) {
  if( DEBUG2) printf("RGUDECF\n");
  return (fortdouble) IGUDECF(grib);
}

fortint IGDUMMY(gribProduct ** grib) {
  if( DEBUG2) printf("IGDUMMY\n");
  return 0;
}

fortdouble RGDUMMY(gribProduct ** grib) {
  if( DEBUG2) printf("RGDUMMY\n");
  return 0;
}

fortint numberOfValuesInSection4(gribProduct * grib) {
fortint loop, total = 0, count = 0;
unsigned char * value;

  if( generalLatLonGrid(grib) )
    if( directionIncrementsGiven(grib) )
      return g2_ni(grib)*g2_nj(grib);
    else {
      value = (unsigned char *) (grib->g2) + 4*g2_NV(grib) + g2_PV_PL(grib) - 1;
      for( loop = 0; loop < g2_nj(grib); loop++ ) {
        total += TWOBYTEINT(value);
        value += 2;
      }
      return total;
    }
  else if( anySpectralField(grib) )
    return (g2_J(grib)+1)*(g2_J(grib)+2);

  return 0;
}

fortint g4_offset(gribProduct * grib) {
int flag = g4_flag(grib);

  if(gridPoint(grib) &&
     simplePacking(grib) &&
     floatingPoint(grib) &&
     noAdditionalFlags(grib)) return 12;

  if((!gridPoint(grib))) {
    if(simplePacking(grib)) return 12;
  else
    return 19;
  }

  return 12;
}

fortint IGNVALU(gribProduct ** grib) {
  return numberOfValuesInSection4( (*grib) );
}

fortint GVALUES(
  gribProduct ** grib,
  fortdouble * array,
  fortint * arraySize,
  fortint * notMissing,
  fortdouble * userSuppliedMissingValue ) {
gribProduct * g = *grib;
fortdouble * arr = array;
fortint outerLoop, innerLoop, loop, numberOfValues, numberNotMissing;
unsigned char * value, * bitStart;
fortdouble IP;
double * powerFactor;
fortdouble scale, minimum, missingValue;
fortint packedBits,nextValueFirstBit,bitsPerValue;
fortint subsetTruncation, fieldTruncation;
unsigned char * nextUnpacked, * nextPacked;
unsigned char * primaryBitmap;
unsigned char * secondaryBitmap;
unsigned char * listOfWidths;
unsigned char * firstOrderPackedValues;
unsigned char * secondOrderPackedValues;
fortint primaryBit, secondaryBit;
fortint firstOrderValue, secondOrderValue, width;
fortint nextFirstOrderValue = 0;
fortint nextSecondOrderValue = 0;
fortint nextWidth = 0;
fortint nextPrimaryBit = 0;
fortint nextSecondaryBit = 0;
fortint firstOrderPackedValueSize = g4_bits(g);
fortint one = 1, eight = 8;
fortint primaryBitmapped = primaryBitmapPresent(g);

  if( !(numberOfValues = numberOfValuesInSection4(g)) ) {
    if( DEBUG1 ) printf("GVALUES: no values found in GRIB.\n");
    return (fortint) 0;
  }

  if( numberOfValues > *arraySize ) {
    if( DEBUG1 ) printf("GVALUES: user array too small for field values.\n");
    return (fortint) -1;
  }

  minimum = referenceValue(g),
  missingValue = *userSuppliedMissingValue;

  scale = (fortdouble) RGDSCAL(grib);
  value = (unsigned char *)(g->g4) + g4_offset(g) - 1;
  bitsPerValue = g4_bits(g);
  nextValueFirstBit = 0;

  if( anySpectralField(g) ) {
    numberNotMissing = numberOfValues;
/*
// Spectral simple packing
*/
    if( simplePacking(g) ) {
      GBYTE(value,&packedBits,&nextValueFirstBit,&bitsPerValue);
      *arr = realValue(value);
      value += 4;
      for( loop = 1; loop < numberOfValues; loop++ ) {
        GBYTE(value,&packedBits,&nextValueFirstBit,&bitsPerValue);
        nextValueFirstBit += bitsPerValue;
        *(arr+loop) = minimum + ((fortdouble) packedBits) * scale;
      }
    }
/*
// Spectral complex packing
*/
    else {
      fieldTruncation  = g2_J(g)+1;
      subsetTruncation = g4_j(g)+1;
      nextUnpacked = value;
      nextPacked = value + (subsetTruncation*(subsetTruncation+1))*4;
      loop = 0;
      for( outerLoop = 0; outerLoop < fieldTruncation; outerLoop++ ) {
        if(outerLoop < subsetTruncation) {
          for(innerLoop=outerLoop;innerLoop<fieldTruncation;innerLoop++) {
            if(innerLoop < subsetTruncation) {
              *(arr+loop) = realValue(nextUnpacked);
              loop++;
              nextUnpacked += 4;
              *(arr+loop) = realValue(nextUnpacked);
              loop++;
              nextUnpacked += 4;
            }
            else {
              GBYTE(nextPacked,&packedBits,&nextValueFirstBit,&bitsPerValue);
              nextValueFirstBit += bitsPerValue;
              *(arr+loop) = minimum + ((fortdouble) packedBits) * scale;
              loop++;
              GBYTE(nextPacked,&packedBits,&nextValueFirstBit,&bitsPerValue);
              nextValueFirstBit += bitsPerValue;
              *(arr+loop) = minimum + ((fortdouble) packedBits) * scale;
              loop++;
            }
          }
        }
        else {
          for(innerLoop=outerLoop;innerLoop<fieldTruncation;innerLoop++){
            GBYTE(nextPacked,&packedBits,&nextValueFirstBit,&bitsPerValue);
            nextValueFirstBit += bitsPerValue;
            *(arr+loop) = minimum + ((fortdouble) packedBits) * scale;
            loop++;
            GBYTE(nextPacked,&packedBits,&nextValueFirstBit,&bitsPerValue);
            nextValueFirstBit += bitsPerValue;
            *(arr+loop) = minimum + ((fortdouble) packedBits) * scale;
            loop++;
          }
        }
      }
/*
// NB. ECMWF bug misses storing unpacked one value in each line of subset
*/
      IP = RGIP(grib);
      powerFactor = (double *) allocateMemory(fieldTruncation*sizeof(double));
      powerFactor[0] = 1.0;
      for( loop = 1; loop < fieldTruncation; loop++ ) {
        *(powerFactor+loop) =
          (double) 1.0/pow((double)((loop+1)*(loop+2)),(double)IP);
      }

      loop = 0;
      for( outerLoop = 0; outerLoop < (subsetTruncation-1); outerLoop++ ) {
        for( innerLoop = outerLoop; innerLoop < fieldTruncation; innerLoop++ ) {
          if( innerLoop >= (subsetTruncation-1) ) {
            *(arr+loop)   *= powerFactor[innerLoop-1];
            *(arr+loop+1) *= powerFactor[innerLoop-1];
          }
          loop += 2;
        }
      }
      for(outerLoop=(subsetTruncation-1);outerLoop<fieldTruncation;outerLoop++){
        for( innerLoop = outerLoop; innerLoop < fieldTruncation; innerLoop++ ) {
          *(arr+loop)   *= powerFactor[innerLoop-1];
          *(arr+loop+1) *= powerFactor[innerLoop-1];
          loop += 2;
        }
      }
      freeMemory(powerFactor);
    }
  }
/*
// Grid-point simple packing
*/
  else {
    if( simplePacking(g) ) {
      primaryBit = 1;
      numberNotMissing = 0;

      if( primaryBitmapped ) primaryBitmap = (unsigned char*)(g->g3) + 6;

      for( loop = 0; loop < numberOfValues; loop++ ) {

        if( primaryBitmapped ) {
          primaryBit = getSingleMapBit(primaryBitmap,nextPrimaryBit);
          nextPrimaryBit++;
        }

        if( primaryBit ) {
          GBYTE(value,&packedBits,&nextValueFirstBit,&bitsPerValue);
          *(arr+loop) = minimum + ((fortdouble) packedBits) * scale;
          nextValueFirstBit += bitsPerValue;
          numberNotMissing++;
        }
        else
          *(arr+loop) = missingValue;
      }
    }
/*
// Grid-point second-order packing
*/
    else {
      listOfWidths = (unsigned char *)(g->g4) + g4_offset(g) + 10 - 1;
      GBYTE(listOfWidths,&width,&nextWidth,&eight);

      firstOrderPackedValues  = (unsigned char *)(g->g4) + g4_n1(g) - 1;
      secondOrderPackedValues = (unsigned char *)(g->g4) + g4_n2(g) - 1;

      numberNotMissing = 0;

      if( primaryBitmapped ) primaryBitmap = (unsigned char*)(g->g3) + 6;

      if( secondaryBitmapPresent(g) ) {
        if( differentWidths(g) )
          secondaryBitmap = (unsigned char *) listOfWidths + g4_p1(g);
        else
          secondaryBitmap = (unsigned char *) listOfWidths + 1;
      }

      for( loop = 0; loop < numberOfValues; loop++ ) {

        if( primaryBitmapped ) {
          primaryBit = getSingleMapBit(primaryBitmap,nextPrimaryBit);
          nextPrimaryBit++;
        }
        else
          primaryBit = 1;

        if( primaryBit ) {
          if( secondaryBitmapPresent(g) ) {
            secondaryBit = getSingleMapBit(secondaryBitmap,nextSecondaryBit);
            nextSecondaryBit++;
          }
          else
            secondaryBit = MULTIPLE(loop,g2_ni(g)) ;

          if( secondaryBit ) {
            GBYTE(firstOrderPackedValues,&firstOrderValue,
                  &nextFirstOrderValue,&firstOrderPackedValueSize);
            nextFirstOrderValue += firstOrderPackedValueSize;

            if( differentWidths(g) ) {
              GBYTE(listOfWidths,&width,&nextWidth,&eight);
              nextWidth += 8;
            }
          }

          if( width ) {
            GBYTE(secondOrderPackedValues,&secondOrderValue,
                  &nextSecondOrderValue,&width);
            nextSecondOrderValue += width;
          }
          else
            secondOrderValue = 0;

          *(arr+loop) = minimum +
            ((fortdouble)(firstOrderValue+secondOrderValue))*scale;
          numberNotMissing++;
        }
        else
          *(arr+loop) = missingValue;
      }
    }
  }

  *notMissing = numberNotMissing;
  if( DEBUG1 ) printf("GVALUES: number of field values = %d\n",numberOfValues);
  return (fortint) numberOfValues;
}

void GVEND(gribProduct ** grib) {
gribProduct * g;

  g = *grib;

  if( g->currentPoint.latitude != NULL ) {
    freeMemory(g->currentPoint.latitude);
    g->currentPoint.latitude = NULL;
  }

  if( g->currentPoint.longitudeIncrement != NULL ) {
    freeMemory(g->currentPoint.longitudeIncrement);
    g->currentPoint.longitudeIncrement = NULL;
  }

  if( g->expandedValues != NULL ) {
    freeMemory(g->expandedValues);
    g->expandedValues = NULL;
  }

  if( g->latitudeOffsets != NULL ) {
    freeMemory(g->latitudeOffsets);
    g->latitudeOffsets = NULL;
  }

  return;
}

fortint setupIrregularLongitudeIncrements(gribProduct ** grib, fortint nj) {
gribProduct * g = *grib;
fortint * numberOfPointsPerLatitude;
fortint nrows, loop, count;

  g->latitudeOffsets = (fortint *) allocateMemory((nj+1)*sizeof(fortint));
  numberOfPointsPerLatitude = (fortint *) allocateMemory(nj*sizeof(fortint));
  nrows = GPL(grib,numberOfPointsPerLatitude,&nj);
  if( nrows != nj ) return (fortint) -4;

  g->latitudeOffsets[0] = 0;
  for( loop = 0; loop < nj; loop++ ) {
    count = numberOfPointsPerLatitude[loop];
    g->latitudeOffsets[loop+1] = g->latitudeOffsets[loop] + count;
    if( count != 0 )
      g->currentPoint.longitudeIncrement[loop] =
        360.0 / (fortdouble) numberOfPointsPerLatitude[loop];
    else
      g->currentPoint.longitudeIncrement[loop] = 0.0;
  }
  freeMemory(numberOfPointsPerLatitude);

  return (fortint) 0;
}

fortint GVINIT(gribProduct ** grib, fortdouble * missingValue) {
gribProduct * g;
fortint regular, ni, nj, loop, status;
fortdouble latitudeStep, longitudeStep, north;

  if( grib == NULL) {
    if( DEBUG1 ) printf("GVINIT: grib empty. Was GDECODE called?\n");
    return (fortint) -1;
  }
  g = *grib;
/*
// Only gaussian or lat/lon grid point fields can be handled
*/
  if( !generalLatLonGrid(g) ) {
    if( DEBUG1 )
      printf("GVINIT: only gaussian or lat/lon grid point fields handled\n");
    return (fortint) -2;
  }

  g->numberOfValues    = numberOfValuesInSection4(g);
  g->value             = (unsigned char *)(g->g4) + g4_offset(g) - 1;
  g->bitStart          = (unsigned char *)(g->g3) + 6;
  g->bitsPerValue      = g4_bits(g);
  g->bitmapped         = primaryBitmapPresent(g);
  g->nextValueFirstBit = 0;
  g->nextBit           = 0;
  g->scale             = (fortdouble) RGDSCAL(grib);
  g->minimum           = referenceValue(g);
  g->missingValue      = *missingValue;

  ni = g2_ni(g);
  nj = g2_nj(g);
  g->currentPoint.latitude = (fortdouble *) allocateMemory(nj*sizeof(fortdouble));

  g->currentPoint.longitudeIncrement = (fortdouble *) allocateMemory(nj*sizeof(fortdouble));

  regular = directionIncrementsGiven(g);

  if( anyLatLonGrid(g) ) {
    north = RGNWLAT(grib);
    latitudeStep  = RGDJ(grib);
    longitudeStep = RGDI(grib);
    for( loop = 0; loop < nj; loop++ )
      g->currentPoint.latitude[loop] = north - (latitudeStep * (fortdouble) loop);
  }
  else {
    fortint zero = 0, minusOne = -1;
    status = IGGLAT(&nj,g->currentPoint.latitude,&zero,&minusOne);
    if( status != 0 ) return (fortint) -5;
    longitudeStep = 360.0 / (fortdouble) (2*nj);
  }

  if( regular )
    for( loop = 0; loop < nj; loop++ )
      g->currentPoint.longitudeIncrement[loop] = longitudeStep;
  else
    setupIrregularLongitudeIncrements(grib, nj);

  if( ! simplePacking(g) ) {
    fortint notMissing;
    fortint numberOfValues = g->numberOfValues;

    g->expandedValues = (fortdouble *) allocateMemory(numberOfValues*sizeof(fortdouble));

    status =
      GVALUES(grib,g->expandedValues,&numberOfValues,&notMissing,missingValue);
    if( status != numberOfValues ) return (fortint) -6;
  }

  return (fortint) (g->currentPointIndex = 1);
}

fortint GVECTOR(
  gribProduct ** grib,
  fortdouble * latitude,
  fortdouble * longitude,
  fortdouble * pointValue) {
gribProduct * g;
fortint loop, bit, packedBits, current, ni, nj, one = 1;
fortint regular, latitudeIndex, longitudeIndex, currentLatitude;

  if( grib == NULL) {
    if( DEBUG1 ) printf("GVECTOR: grib empty. Was GDECODE called?\n");
    return (fortint) -1;
  }

  g = *grib;
  current = ((g->currentPointIndex)++ - 1);
  if( current >= g->numberOfValues ) {
    if( DEBUG1 ) printf("GVECTOR: all field values already supplied\n");
    return (fortint) -2;
  }

  regular = directionIncrementsGiven(g);

  if( regular) {
    ni = g2_ni(g);
    nj = g2_nj(g);

    latitudeIndex = current/ni;
    longitudeIndex = current - ((current/ni)*ni);
    *latitude  = g->currentPoint.latitude[latitudeIndex];
    *longitude = g->currentPoint.longitudeIncrement[latitudeIndex]*longitudeIndex;
  }
  else {
    currentLatitude = -1;
    while( current >= g->latitudeOffsets[++currentLatitude]);
    latitudeIndex = currentLatitude - 1;
    longitudeIndex = current - g->latitudeOffsets[latitudeIndex];
    *latitude  = g->currentPoint.latitude[latitudeIndex];
    *longitude = g->currentPoint.longitudeIncrement[latitudeIndex]*longitudeIndex;
  }

  if( ! simplePacking(g) ) {
    *pointValue = *(g->expandedValues + current);
  }
  else {
    bit = 1;
    if( g->bitmapped ) {
      bit = getSingleMapBit(g->bitStart,g->nextBit);
      g->nextBit++;
    }

    if( bit ) {
      GBYTE(g->value,&packedBits,&(g->nextValueFirstBit),&(g->bitsPerValue));
      g->nextValueFirstBit += g->bitsPerValue;
      *pointValue = g->minimum + ((fortdouble) packedBits) * g->scale;
    }
    else
      *pointValue = g->missingValue;
  }

  return (current+1);
}

fortint getSingleMapBit(unsigned char * map, fortint bitNumber) {
fortint byteOffset = (bitNumber>>3);
fortint bitOffset = bitNumber - (byteOffset<<3);

  return (fortint) ((map[byteOffset]>>(7-bitOffset))&0x1);
}

void * allocateMemory(size_t size) {
void * memoryPointer = (void *) malloc(size);

  if( DEBUG2 ) printf("allocateMemory: allocated %zu memory bytes\n",size);
  if( memoryPointer == NULL ) {
    printf("allocateMemory: requested number of bytes = %zu\n", size);
    perror("allocateMemory: memory allocation failed");
    exit(1);
  }
  if( DEBUG2 )
    printf("allocateMemory: allocated memory pointer = %p\n", memoryPointer);
  return memoryPointer;
}

void freeMemory(void * memoryPointer) {
  if( DEBUG2 )
    printf("freeMemory: freeing memory pointer = %p\n", memoryPointer);
  if( memoryPointer != NULL ) free(memoryPointer);
  return;
}
