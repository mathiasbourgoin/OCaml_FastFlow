CXX = g++
CXXFLAGS = -std=c++11 -g -I.
LDFLAGS = -ldl -pthread CXX = g++
FASTFLOWPATH = ../../fastflow



all: test.pp.byte

lib : FastFlow.cmo FastFlow.cmx fastflow.so

FastFlow.cmo: FastFlow.ml
	ocamlfind ocamlc -g -noassert -thread -package spoc,sarek,ctypes.foreign -linkpkg FastFlow.ml  -c

FastFlow.cmx : FastFlow.ml
	ocamlfind ocamlopt -g -noassert -thread -package spoc,sarek,ctypes.foreign -linkpkg FastFlow.ml  -c

fastflow.so: fastflow.hpp fastflow.cpp
	$(CXX) $(CXXFLAGS) -I $(FASTFLOWPATH) -shared -fPIC -o fastflow.so fastflow.cpp


%.pp.ml : %.ml
	camlp4 -I +camlp4 -I `ocamlfind query sarek_syntax` -parser o -parser op -printer o kernels_int.cma $< > $@


%pp.byte : %pp.ml FastFlow.cmo
	ocamlfind ocamlc  -g -thread  -package ctypes,ctypes.foreign,spoc,sarek -linkpkg FastFlow.cmo -o $@ $<

%.pp.nat: %.pp.ml FastFlow.cmx
	ocamlfind ocamlopt  -g -thread  -package ctypes,ctypes.foreign,spoc,sarek -linkpkg FastFlow.cmx -o $@ $<

%.mli : %.ml
	ocamlfind ocamlopt -thread -package spoc,sarek,ctypes.foreign -linkpkg $<  -i > $@

test : test.pp.nat fastflow.so
	./test.pp.nat

install : lib
	@echo "\033[43m\033[30mInstalling OCaml-Fastflow\033[0m"
	@ocamlfind install fastflow *.cmo  *.cmx *.cmi *.hpp *.so *.o META

uninstall :
	@ocamlfind remove fastflow

clean:
	rm -rf *.so *.o *.cm* \#*\# *~ *.pp.* *.mli userfun*.cpp

.PHONY:clean test
