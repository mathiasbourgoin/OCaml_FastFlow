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
ff_ofarm* create_accelerator(int, const char*);
void run_accelerator(ff_ofarm*);
void offloadacc(ff_ofarm*, void*);
void loadresacc(ff_ofarm*, void**);
void nomoretasks(ff_ofarm* );
void wait(ff_ofarm*);
}

#endif
