#ifndef BIBLIOTEQUE_HPP
#define BIBLIOTEQUE_HPP

#include <vector>
#include <iostream>
#include <vector>
#include <functional>

#include <dlfcn.h>
#include <ff/farm.hpp>
#include <ff/parallel_for.hpp>
#include <ff/node.hpp>

using namespace ff;


typedef void(*userfun_t)(void*);

extern "C"{
//farm
ff_farm<>* create_farm(int, const char*, const char*);
void run_farm(ff_farm<>*);
void offloadacc(ff_farm<>*, void*);
void loadresacc(ff_farm<>*, void**);
void nomoretasks(ff_farm<>* );
void wait(ff_farm<>*);

//ofarm
ff_ofarm* create_ofarm(int, const char*, const char*);
void run_ofarm(ff_ofarm*);
void ooffloadacc(ff_ofarm*, void*);
void oloadresacc(ff_ofarm*, void**);
void onomoretasks(ff_ofarm* );
void owait(ff_ofarm*);

//parfor
ParallelFor* create_pf(int);
void parallel_for(ParallelFor* pf, long first, long last, long step, long grain,
  const char* lib, const char* fun, const long nw=FF_AUTO);

}

#endif
