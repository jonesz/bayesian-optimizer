.PHONY: test doc clean

test:
	make -C lib/github.com/jonesz/bayesian-optimizer

doc:
	futhark doc -o doc/ lib/github.com/jonesz/bayesian-optimizer

clean:
	$(RM) -rf doc/
	make clean -C lib/github.com/jonesz/bayesian-optimizer
