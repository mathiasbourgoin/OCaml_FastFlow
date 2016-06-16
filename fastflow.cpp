#include <vector>
#include <iostream>
#include <vector>
#include <functional>

#include <dlfcn.h>

#include <ff/farm.hpp>
#include <ff/node.hpp>

#include "fastflow.hpp"
#include <assert.h>


ff_ofarm farm(true);



typedef void(*addw)(ff::ff_ofarm*,int, void*);
typedef void(*offload_t)(ff::ff_ofarm*, void*);
typedef void(*userfun_t)(void*);

void* handle_lib;
userfun_t userfun;


  //
// this is the generic worker wrapper
// code is loaded on farm initializazion
// (when the accelerator is created)
//
class Worker: public ff_node {
protected:
  void * (*fun)(void *);

public:
  // this is called after dloading the business logic code
  Worker(void* (*fun)(void *)): fun(fun) {}

  void * svc(void * t) {
    userfun(t);
  }
};


bool create_accelerator(int nworkers) {

  //
  // first of all load the dynamically generated worker function
  //
  handle_lib  = dlopen("./userfun.so",RTLD_LAZY);
  if (handle_lib==NULL){
    std::cerr << "FF-OCAML-ACCELERATOR: error while loading business logic code" << dlerror() << std::endl;
    return false;
  }


  userfun = (userfun_t) dlsym(handle_lib, "userfun");
  if (!userfun){
    std::cerr << "FF-OCAML-ACCELERATOR: error while looking for business logic function pointer (" << dlerror() << ")" << std::endl;
    return false;
  }

  // addw add_workers =  (addw) dlsym(handle_lib, "add_workers");
  // if(!add_workers) {
  //   std::cerr << "FF-OCAML-ACCELERATOR: error while looking for business logic function pointer (" << dlerror() << ")" << std::endl;
  //   return false;
  // }


  std::vector<ff_node *> w;
  for(int i=0;i<nworkers;++i)
    w.push_back(new Worker((void*(*)(void *)) userfun));
  farm.add_workers(w);


  return(true);
}

void run_accelerator() {
  farm.run();
  return;
}


void loadresacc(void ** ou) {
  farm.load_result(ou);
  return;
}




void nomoretasks() {
  void * eos = (void *)ff::FF_EOS;
  offloadacc(eos);
  return;
}


void offloadacc(void* task){
  farm.offload(task);
}

void wait(){
  farm.wait();
}
