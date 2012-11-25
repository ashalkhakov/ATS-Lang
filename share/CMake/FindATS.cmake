CMAKE_MINIMUM_REQUIRED (VERSION 2.8)

MESSAGE (STATUS "*********************************")
MESSAGE (STATUS "Finding ATS")

INCLUDE (ATSCC)



FIND_PATH (
  ATS_HOME NAMES bin/atscc PATHS ENV ATSHOME
) # end of [FIND_PATH]

SET (ATS_INCLUDE_DIR ${ATS_HOME} ${ATS_HOME}/ccomp/runtime)
SET (ATS_LIBRARY ${ATS_HOME}/ccomp/lib)

INCLUDE (FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS (ATS DEFAULT_MSG ATS_LIBRARY ATS_INCLUDE_DIR)

SET (ATS_INCLUDE_DIRS ${ATS_INCLUDE_DIR})
SET (ATS_LIBRARIES ${ATS_LIBRARY})

MARK_AS_ADVANCED (ATS_INCLUDE_DIR ATS_LIBRARY)

SET (CMAKE_C_COMPILER atscc)
SET (ATSCC_FLAGS)

SET (ATSCC ${ATS_HOME}/bin/atscc)
SET (ATSOPT ${ATS_HOME}/bin/atsopt)

MESSAGE (STATUS "ATS Home: ${ATS_HOME}")
MESSAGE (STATUS "Includes: ${ATS_INCLUDE_DIRS}")
MESSAGE (STATUS "Libraries: ${ATS_LIBRARIES}")