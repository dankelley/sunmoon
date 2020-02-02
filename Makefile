all: calendar_test.out
%.out: %.R
	R --no-save < $< @> $@
clean: force
	rm -rf *~ *.out
force:
