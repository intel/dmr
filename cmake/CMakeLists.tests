set(SRC_TEST_ALLOC test_dmr_target_alloc.F90)
set(SRC_TEST_FREE test_dmr_target_free.F90)
set(SRC_TEST_IS_PRESENT test_dmr_target_is_present.F90)

set_source_files_properties(
    ${SRC_TEST_ALLOC}
    ${SRC_TEST_FREE}
    ${SRC_TEST_IS_PRESENT}
    PROPERTIES LANGUAGE Fortran)

set_source_files_properties(
    ${SRC_TEST_ALLOC}
    ${SRC_TEST_FREE}
    ${SRC_TEST_IS_PRESENT}
    PROPERTIES Fortran_PREPROCESS ON)

set_source_files_properties(
    ${SRC_TEST_ALLOC}
    ${SRC_TEST_FREE}
    ${SRC_TEST_IS_PRESENT}
    PROPERTIES CMAKE_Fortran_FORMAT FREE)

add_executable(test_dmr_alloc ${SRC_TEST_ALLOC})
add_executable(test_dmr_free ${SRC_TEST_FREE})
add_executable(test_dmr_is_present ${SRC_TEST_IS_PRESENT})

set(testTargets
    test_dmr_alloc
    test_dmr_free
    test_dmr_is_present)

set_target_properties(test_dmr_alloc
    PROPERTIES OUTPUT_NAME "test_dmr_alloc.x")
set_target_properties(test_dmr_free
    PROPERTIES OUTPUT_NAME "test_dmr_free.x")
set_target_properties(test_dmr_is_present
    PROPERTIES OUTPUT_NAME "test_dmr_is_present.x")

foreach(TARGET ${testTargets})
    target_link_libraries(${TARGET}
        PRIVATE
            dmr
            compilerCustomConfigc)
    target_include_directories(${TARGET}
        PRIVATE
            "${CMAKE_CURRENT_BINARY_DIR}/../../../src/lib")
endforeach(TARGET)

foreach(TARGET ${testTargets})
    if(DMR_ENABLE_REAL_128 STREQUAL "ON")
       target_compile_definitions(${TARGET}
          PRIVATE
             "_real128")
    endif()
    if(DMR_ENABLE_OMP5.1 STREQUAL "ON")
       target_compile_definitions(${TARGET}
          PRIVATE
             "_OpenMP_5_1")
    endif()
    if(DMR_ENABLE_F2008 STREQUAL "ON")
       target_compile_definitions(${TARGET}
          PRIVATE
             "_F2008")
    endif()
    # Linking for OpenMP host
    target_link_libraries(${TARGET}
       INTERFACE
        	 OpenMP::OpenMP_Fortran)
endforeach(TARGET)

install(
    TARGETS ${testTargets}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
