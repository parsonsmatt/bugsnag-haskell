stack = STACK_YAML="stack.yaml" stack
package = bugsnag-haskell

build:
	$(stack) build

ghci:
	$(stack) ghci $(package):lib

ghcid:
	ghcid -c "$(stack) ghci $(package):lib"
