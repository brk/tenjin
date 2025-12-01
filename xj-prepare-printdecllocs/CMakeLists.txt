cmake_minimum_required(VERSION 3.17)
project(xj-print-decl-locs)


find_package(LLVM REQUIRED CONFIG)
find_package(Clang CONFIG)

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

add_definitions(-DCLANG_LIBDIR_SUFFIX="${LLVM_LIBDIR_SUFFIX}")

add_executable(xj-print-decl-locs PrintDeclLocs.cpp)

set_target_properties(xj-print-decl-locs PROPERTIES
  CXX_STANDARD 17
  CXX_EXTENSIONS OFF
  )

target_link_libraries(xj-print-decl-locs libclang clang-cpp LLVM)
