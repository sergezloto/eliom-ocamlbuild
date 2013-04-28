ocamlbuild := ocamlbuild -use-ocamlfind -classic-display
main_server := $(addprefix _server/, main.cma main.cmxs)
main_client := $(addprefix _client/, main.js)

all: server client
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
