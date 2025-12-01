cmake_minimum_required(VERSION 3.17)
project(xj-find-fn-ptr-decls)


find_package(LLVM REQUIRED CONFIG)
find_package(Clang CONFIG)

include_directories(${LLVM_INCLUDE_DIRS} ${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-rtti")

add_definitions(-DCLANG_LIBDIR_SUFFIX="${LLVM_LIBDIR_SUFFIX}")

add_executable(xj-find-fn-ptr-decls FindFnPtrDecls.cpp)

set_target_properties(xj-find-fn-ptr-decls PROPERTIES
  CXX_STANDARD 17
  CXX_EXTENSIONS OFF
  )

target_link_libraries(xj-find-fn-ptr-decls libclang clang-cpp LLVM)
