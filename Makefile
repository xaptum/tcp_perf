BASEDIR = $(shell pwd)
REBAR = rebar3
APPNAME = tcp_perf
APPNAME_SERVER = $(APPNAME)_server
APPNAME_CLIENT = $(APPNAME)_client
RELPATH_SERVER = _build/server/rel/$(APPNAME)
RELPATH_CLIENT = _build/client/rel/$(APPNAME)
SHELL = /bin/bash
DEBUG=1

compile:
	$(REBAR) compile

recompile:
	find . -name ebin | xargs rm -rf
	$(REBAR) compile

release:
	$(REBAR) release

server:
	$(REBAR) as server release

client:
	$(REBAR) as client release

tar-server: server
	cd $(RELPATH_SERVER)/.. && rm -f $(APPNAME_SERVER).tar.gz && tar -cvf $(APPNAME_SERVER).tar $(APPNAME)/ && gzip $(APPNAME_SERVER).tar
	mv $(RELPATH_SERVER)/../$(APPNAME_SERVER).tar.gz $(BASEDIR)/_build/

tar-client: client
	cd $(RELPATH_CLIENT)/../ && rm -f $(APPNAME_CLIENT).tar.gz && tar -cvf $(APPNAME_CLIENT).tar $(APPNAME)/ && gzip $(APPNAME_CLIENT).tar
	mv $(RELPATH_CLIENT)/../$(APPNAME_CLIENT).tar.gz $(BASEDIR)/_build/

deb: release
# Create directory structure
	mkdir -p $(BASEDIR)/_build/debian/DEBIAN
	mkdir -p $(BASEDIR)/_build/debian/opt/xaptum/$(APPNAME_SERVER)
	mkdir -p $(BASEDIR)/_build/debian/opt/xaptum/$(APPNAME_CLIENT)

# Copy tcp_perf files
	cp -r $(RELPATH_SERVER)/. $(BASEDIR)/_build/debian/opt/xaptum/$(APPNAME_SERVER)/
	cp -r $(RELPATH_CLIENT)/. $(BASEDIR)/_build/debian/opt/xaptum/$(APPNAME_CLIENT)/

# Copy DEBIAN control files
	cp $(BASEDIR)/debian/control $(BASEDIR)/_build/debian/DEBIAN/

# Build debian package
	dpkg-deb --build $(BASEDIR)/_build/debian $(BASEDIR)/_build

clean:
	$(REBAR) clean
	rm -rf _build

start-server:
	$(BASEDIR)/$(RELPATH_SERVER)/bin/$(APPNAME) start

stop-server:
	$(BASEDIR)/$(RELPATH_SERVER)/bin/$(APPNAME) stop

attach-server:
	$(BASEDIR)/$(RELPATH_SERVER)/bin/$(APPNAME) attach

start-client:
	$(BASEDIR)/$(RELPATH_CLIENT)/bin/$(APPNAME) start

stop-client:
	$(BASEDIR)/$(RELPATH_CLIENT)/bin/$(APPNAME) stop

attach-client:
	$(BASEDIR)/$(RELPATH_CLIENT)/bin/$(APPNAME) attach
