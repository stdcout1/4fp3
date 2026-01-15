.PHONY: submission clean

SUBMISSION=cabal.project a1.cabal $(shell find src -name "*.hs") $(shell find test -name "*.hs") hie.yaml flake.nix flake.lock
SUBMISSION_NAME=a1.zip

submission: $(SUBMISSION)
	zip -r $(SUBMISSION_NAME) $(SUBMISSION)

clean:
	rm -f a1.zip && cabal clean
