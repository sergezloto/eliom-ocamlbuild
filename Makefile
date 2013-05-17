ocamlbuild := ocamlbuild -use-ocamlfind -classic-display -j 1
main_server := $(addprefix _server/, server.cma server.cmxs)
main_client := $(addprefix _client/, client.js)

all: 
	$(ocamlbuild) $(main_server) $(main_client)

server:
	$(ocamlbuild) $(main_server)

client:
	$(ocamlbuild) $(main_client)

run:
	ocsigenserver -c ocsigen.conf
run.opt:
	ocsigenserver.opt -c ocsigen.conf

clean:
	$(ocamlbuild) -clean
