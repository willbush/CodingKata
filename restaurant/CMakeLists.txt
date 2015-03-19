cmake_minimum_required(VERSION 3.1)
project(restaurant)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    Menu.cpp
    Menu.h
    MenuItem.cpp
    MenuItem.h
    Order.cpp
    Order.h
    restaurant.cpp
    Table.cpp
    Table.h
    Tokenizer.cpp
    Tokenizer.h
    Waiter.cpp
    Waiter.h)

add_executable(restaurant ${SOURCE_FILES})