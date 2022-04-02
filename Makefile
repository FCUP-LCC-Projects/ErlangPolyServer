files :=  lib_chan.erl lib_chan_mm.erl lib_chan_auth.erl lib_chan_cs.erl lib_md5.erl

test: compile
	erl -pa ../ -s polynomial_test test

test_server: compile conf
	erlc -IW ${files}
	erl -pa ../ -s polynomial_test test_server

test_client: compile
	erlc -IW ${files}
	erl -pa ../ -s polynomial_test test_client

server: compile
	erl -pa ../ -s poly_server start

node: compile
	erl -sname pc2122 -s poly_server start

compile:
	erlc -IW polynomial.erl poly_server.erl print.erl middle_server.erl polynomial_test.erl

client:
	erlc -IW ${files}
	erl

distributed: compile conf
	erlc -IW ${files}
	erl -pa ../ -s lib_chan start_server

conf:
	mkdir -p ${HOME}/.erlang_config/
	cp lib_chan.config ${HOME}/.erlang_config/

clean:
	rm -rf *.beam erl_crash.dump
