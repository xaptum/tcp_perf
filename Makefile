BASEDIR = $(shell pwd)
REBAR = rebar3
APPNAME = tcp_perf
RELPATH = _build/default/rel/$(APPNAME)
SHELL = /bin/bash
DEBUG=1

compile:
	$(REBAR) compile

recompile:
	find . -name ebin | xargs rm -rf
	$(REBAR) compile

release:
	$(REBAR) release

console: release
	cd $(RELPATH) && ./bin/$(APPNAME) console

tar: release
	cd $(RELPATH)/../ && rm -f $(APPNAME).tar.gz && tar -cvf $(APPNAME).tar $(APPNAME)/ && gzip $(APPNAME).tar
	mv $(RELPATH)/../$(APPNAME).tar.gz $(BASEDIR)/_build/

deb: release
# Create directory structure
	mkdir -p $(BASEDIR)/_build/debian/DEBIAN
	mkdir -p $(BASEDIR)/_build/debian/usr/lib/systemd/system
	mkdir -p $(BASEDIR)/_build/debian/opt/xaptum/$(APPNAME)

# Copy tcp_perf files
	cp -r $(RELPATH)/. $(BASEDIR)/_build/debian/opt/xaptum/$(APPNAME)/

# Copy DEBIAN control files
	cp $(BASEDIR)/debian/control $(BASEDIR)/_build/debian/DEBIAN/
	cp $(BASEDIR)/debian/postinst $(BASEDIR)/_build/debian/DEBIAN/
	cp $(BASEDIR)/debian/prerm $(BASEDIR)/_build/debian/DEBIAN/

# Build debian package
	dpkg-deb --build $(BASEDIR)/_build/debian $(BASEDIR)/_build

clean:
	$(REBAR) clean
	rm -rf _build

start:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) start

stop:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) stop

attach:
	$(BASEDIR)/$(RELPATH)/bin/$(APPNAME) attach
