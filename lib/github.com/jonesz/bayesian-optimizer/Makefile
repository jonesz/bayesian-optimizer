test: kernel.fut
	futhark test kernel.fut optimizer.fut gp_test.fut

bench: kernel_bench.fut
	futhark bench kernel_bench.fut

.PHONY: clean

clean:
	$(RM) *.c kernel kernel_bench
	$(RM) optimizer
	$(RM) gp_test
