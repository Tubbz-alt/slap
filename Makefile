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

slackpack.ppu: slackpack.pas
	$(PC) $(PCFLAGS) slackpack.pas

slap: slap.pas sbtree.ppu slist.ppu slackpack.ppu
	$(PC) $(PCFLAGS) slap.pas

lazSlap:
	lazbuild laz/lazSlap.lpr

clean:
	@-rm *.{bak,o,ppu,map} slap >& /dev/null

pack:
	@-rm *.{bak,o,ppu,map} slap laz/lazSlap >& /dev/null
	@-mkdir release
	$(PC) $(PCFLAGS_RELEASE) slap.pas
	@-rm *.{bak,o,ppu,map} laz/lazSlap >& /dev/null
	lazbuild laz/lazSlap.lpr
	zip release/binaries.zip slap laz/lazSlap

push:
	git add .
	git commit -m "auto"
	git push

install:
	install -m 755 -o root -g root slap $(instdir)
