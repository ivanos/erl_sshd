APPS = kernel stdlib sasl erts ssl tools runtime_tools crypto inets \
	public_key mnesia syntax_tools compiler
COMBO_PLT = $(HOME)/.erl_sshd_combo_dialyzer_plt

.PHONY: all compile deps test clean distclean ct

all: compile

compile:
	./rebar get-deps compile

deps:
	./rebar get-deps

eunit: compile
	./rebar -v skip_deps=true eunit

ct: compile
	./rebar -v ct $(CTARGS)

distclean: clean
	./rebar delete-deps

clean:
	./rebar clean

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	dialyzer --plt $(COMBO_PLT) ebin

compile test clean: rebar

keys: compile
	rm -f priv/system_dir/ssh_host_rsa_key*
	ssh-keygen -t rsa -P "" -f priv/system_dir/ssh_host_rsa_key
	rm -f priv/user_dir/authorized_keys
	ssh-keygen -t rsa -P "" -f id_rsa
	chmod 600 id_rsa
	cp id_rsa.pub priv/user_dir/authorized_keys

rebar:
	wget -c http://github.com/rebar/rebar/wiki/rebar
	chmod +x $@
