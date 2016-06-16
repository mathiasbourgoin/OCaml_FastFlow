#ifndef BIBLIOTEQUE_HPP
#define BIBLIOTEQUE_HPP


using namespace ff;


extern "C"{
bool create_accelerator(int);
void run_accelerator();
void offloadacc(void*);
void loadresacc(void**);
void nomoretasks();
void wait();
}

#endif
