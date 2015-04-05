
all: .cabal-sandbox dist
	cabal build -j

dist:
	cabal configure

.cabal-sandbox:
	cabal sandbox init
	cabal install -j --enable-documentation --haddock-html --haddock-hoogle --only-dependencies

.PHONY: clean
clean:
	rm -rf dist

.PHONY: clean
cabal-clean: clean
	rm -rf cabal.sandbox.config .cabal-sandbox
