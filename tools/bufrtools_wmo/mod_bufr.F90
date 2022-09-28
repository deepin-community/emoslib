 MODULE bufr_module

       implicit none

!-- BUFR variable

    real*8   , parameter      :: rvind=1.7D38
    real*8   , parameter      :: eps=1.0D-8
    integer, parameter      :: nvind=2147483647

    integer, parameter :: jsup =   9
    integer, parameter :: jsec0=   3
    integer, parameter :: jsec1=  40
    integer, parameter :: jsec2=  4096
    integer, parameter :: jsec3=   4
    integer, parameter :: jsec4=   2
    integer, parameter :: jelem=160000
    integer, parameter :: jbufl=512000
#ifdef JBPW_64
    integer, parameter :: jbpw =  64
#else
    integer, parameter :: jbpw =  32
#endif
    integer, parameter :: jwork=4096000
    integer, parameter :: jkey=46
    integer, parameter :: jbyte=512000
    integer, parameter :: kelem=160000
    integer, parameter :: kvals=4096000

    integer, dimension(jbufl) :: kbuff, kbufr
    integer, dimension(jsup)  :: ksup
    integer, dimension(jsec0) :: ksec0
    integer, dimension(jsec1) :: ksec1
    integer, dimension(jsec2) :: ksec2
    integer, dimension(jsec3) :: ksec3
    integer, dimension(jsec4) :: ksec4
    integer, dimension(jkey)  :: key
    real*8,    dimension(kvals) :: values, vals
    integer, dimension(kelem) :: ktdlst
    integer, dimension(kelem) :: ktdexp
    integer, dimension(kelem) :: kdata
    integer                   :: kbox

    integer                   :: kbufl
    integer                   :: ktdlen
    integer                   :: ktdexl
    integer                   :: kapp
    integer                   :: klen
    integer, dimension(kvals) :: kboxr


    character(len= 64), dimension(kelem) :: cnames
    character(len= 24), dimension(kelem) :: cunits
    character(len= 80), dimension(kvals) :: cvals
    character(len= 24), dimension(kelem) :: cboxu
    character(len= 64), dimension(kelem) :: cboxn


 END MODULE bufr_module
