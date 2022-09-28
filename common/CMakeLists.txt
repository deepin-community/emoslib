list( APPEND common_src_files
  JPointer.h
  fortint.h
  fortreal.h
  grprs.h
)

foreach( src ${common_src_files} )
  list( APPEND emos_common_src_files common/${src} )
endforeach()

set( emos_common_src_files ${emos_common_src_files} PARENT_SCOPE )

