#ifndef BIBLIOTEQUE_HPP
#define BIBLIOTEQUE_HPP

#include <vector>
#include <iostream>
#include <vector>
#include <functional>

#include <dlfcn.h>
#include <ff/farm.hpp>
#include <ff/node.hpp>

using namespace ff;



extern "C"{
ff_farm<>* create_accelerator(int, const char*, const char*);
void run_accelerator(ff_farm<>*);
void offloadacc(ff_farm<>*, void*);
void loadresacc(ff_farm<>*, void**);
void nomoretasks(ff_farm<>* );
void wait(ff_farm<>*);
}

#endif
