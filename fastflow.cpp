#include "fastflow.hpp"
#include <assert.h>






typedef void(*addw)(ff::ff_farm<>*,int, void*);
typedef void(*offload_t)(ff::ff_farm<>*, void*);
typedef void(*userfun_t)(void*);

//void* handle_lib;
//userfun_t userfun;


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
    fun(t);
  }
};


ff_farm<>* create_accelerator(int nworkers, const char* lib, const char* fun) {
  ff_farm<>* farm = new ff_farm<>(true);
  userfun_t userfun;
  //
  // first of all load the dynamically generated worker function
  //printf("lib : %s\n", lib);
  void* handle_lib  = dlopen(lib,RTLD_LAZY);
  if (handle_lib==NULL){
    std::cerr << "FF-OCAML-ACCELERATOR: error while loading business logic code " << dlerror() << std::endl;
    exit (-1);
  }

  //printf("fun : %s\n", fun);
  userfun = (userfun_t) dlsym(handle_lib, fun);
  if (!userfun){
    std::cerr << "FF-OCAML-ACCELERATOR: error while looking for business logic function pointer (" << dlerror() << ")" << std::endl;
    exit (-1);
  }

  // addw add_workers =  (addw) dlsym(handle_lib, "add_workers");
  // if(!add_workers) {
  //   std::cerr << "FF-OCAML-ACCELERATOR: error while looking for business logic function pointer (" << dlerror() << ")" << std::endl;
  //   return false;
  // }

  std::vector<ff_node *> w;
  for(int i=0;i<nworkers;++i)
    w.push_back(new Worker((void*(*)(void *)) userfun));
  farm->add_workers(w);


  return(farm);
}

void run_accelerator(ff_farm<>* farm) {
  farm->run();
  return;
}


void loadresacc(ff_farm<>* farm, void ** ou) {
  farm->load_result(ou);
  return;
}




void nomoretasks(ff_farm<>* farm) {
  void * eos = (void *)ff::FF_EOS;
  offloadacc(farm, eos);
  return;
}





void offloadacc(ff_farm<>* farm, void* task){
  farm->offload(task);
}

void wait(ff_farm<>* farm){
  farm->wait();
}
