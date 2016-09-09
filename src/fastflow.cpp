#include "fastflow.hpp"
#include <assert.h>


using namespace ff;
//typedef void(*offload_t)(ff::ff_farm<>*, void*);

//void* handle_lib;
//userfun_t userfun;


// typename T = ff_farm<>
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


template <typename T> T* create_farm_(int nworkers, const char* lib, const char* fun) {
  T* farm = new T(true);
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
ff_farm<>* create_farm(int nworkers, const char* lib, const char* fun){
  return create_farm_<ff_farm<>>(nworkers, lib, fun);
}
ff_ofarm* create_ofarm(int nworkers, const char* lib, const char* fun){
  return create_farm_<ff_ofarm>(nworkers, lib, fun);
}

template <typename T> void run_farm_(T* farm) {
  farm->run();
  return;
}
void run_farm(ff_farm<>* farm){run_farm_<ff_farm<>>(farm);}
void run_ofarm(ff_ofarm* farm){run_farm_<ff_ofarm>(farm);}



template <typename T> void loadresacc_(T* farm, void ** ou) {
  farm->load_result(ou);
  return;
}
void loadresacc(ff_farm<>*f, void**o){loadresacc_<ff_farm<>>(f,o);}
void oloadresacc(ff_ofarm*f, void**o){loadresacc_<ff_ofarm>(f,o);}


template <typename T> void offloadacc_(T* farm, void* task){
  farm->offload(task);
}
void offloadacc(ff_farm<>*f, void*task){offloadacc_<ff_farm<>>(f,task);}
void ooffloadacc(ff_ofarm*f, void*task){offloadacc_<ff_ofarm>(f,task);}


template <typename T> void nomoretasks_(T* farm) {
  void * eos = (void *)ff::FF_EOS;
  offloadacc_<T>(farm, eos);
  return;
}
void nomoretasks(ff_farm<>* farm){nomoretasks_<ff_farm<>>(farm);}
void onomoretasks(ff_ofarm* farm){nomoretasks_<ff_ofarm>(farm);}


template <typename T> void wait_(T* farm){
  farm->wait();
}
void wait(ff_farm<>* farm){wait_<ff_farm<>>(farm);}
void owait(ff_ofarm* farm){wait_<ff_ofarm>(farm);}

/**************/
template <typename T> T* create_pf_(int nworkers) {
  return (new T(nworkers, true));
}

ParallelFor* create_pf (int nw){
  return create_pf_<ParallelFor>(nw);
}

typedef void(*iterfun_t)(long);

void parallel_for(ParallelFor* pf, long first, long last, long step, long grain,
  const char* lib, const char* fun, const long nw)
  {
    iterfun_t userfun;
    //
    // first of all load the dynamically generated worker function
    //printf("lib : %s\n", lib);
    void* handle_lib  = dlopen(lib,RTLD_LAZY);
    if (handle_lib==NULL){
      std::cerr << "FF-OCAML-ACCELERATOR: error while loading business logic code " << dlerror() << std::endl;
      exit (-1);
    }

    //printf("fun : %s\n", fun);
    userfun = (iterfun_t) dlsym(handle_lib, fun);
    if (!userfun){
      std::cerr << "FF-OCAML-ACCELERATOR: error while looking for business logic function pointer (" << dlerror() << ")" << std::endl;
      exit (-1);
    }
    pf->parallel_for(first, last, step, grain, [&](const long i){(*userfun)(i);}, nw);
  }
