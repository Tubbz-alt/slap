# kate: default-dictionary us; tab-indents true; tab-width 4; indent-width 4; replace-tabs off; replace-tabs-save off; line-numbers on;
#

# installation directory
instdir = /usr/local/sbin

# Pascal compiler
PC = fpc
PCFLAGS_DEBUG   = -Tlinux -g
PCFLAGS_RELEASE = -Tlinux -Os -XX
PCFLAGS = $(PCFLAGS_DEBUG)
#PCFLAGS = $(PCFLAGS_RELEASE)

###

all: slap

sbtree.ppu: sbtree.pas
	$(PC) $(PCFLAGS) sbtree.pas

slist.ppu: slist.pas
	$(PC) $(PCFLAGS) slist.pas

slap: slap.pas sbtree.ppu slist.ppu
	$(PC) $(PCFLAGS) slap.pas

clean:
	@-rm *.{bak,o,ppu,map} slap >& /dev/null

release:
	@-rm *.{bak,o,ppu,map} slap >& /dev/null
	$(PC) $(PCFLAGS_RELEASE) slap.pas

push:
	git add .
	git commit -m "auto"
	git push

install:
	install -m 755 -o root -g root slap $(instdir)
