LIQUID=stack exec ./liquid
LH_REV=5b68dc72f628a4c16a77616fb32d8c685580ed2d

all: liquid .liquid/dilution.hs.html

.liquid/%.hs.html: %.hs ./liquid stack.yaml
	${LIQUID} $<

liquid: liquidhaskell Makefile
	cd liquidhaskell; git reset --hard ${LH_REV}; git submodule update; stack install
	cp ~/.local/bin/liquid .

liquidhaskell:
	git clone http://github.com/ucsd-progsys/liquidhaskell -b develop --recursive
