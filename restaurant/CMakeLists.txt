cmake_minimum_required(VERSION 3.1)
project(Restaurant)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    Menu.cpp
    Menu.h
    MenuItem.cpp
    MenuItem.h
    Order.cpp
    Order.h
    Restaurant.cpp
    Restaurant.h
    Table.cpp
    Table.h
    Tokenizer.cpp
    Tokenizer.h
    Waiter.cpp
    Waiter.h)

add_executable(Restaurant ${SOURCE_FILES})