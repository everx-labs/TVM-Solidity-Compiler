set(sources
    soltest2.cpp
)

add_executable(soltest2 ${sources})

target_link_libraries(soltest2 PRIVATE
        libsolc
        solidity
        solutil
        Boost::boost
        Boost::program_options
        Boost::unit_test_framework
)
