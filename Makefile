all: calendars_2020_2030.out calendar_test.out
%.out: %.R
	R --no-save < $< @> $@
clean: force
	rm -rf *~ *.out
force:
