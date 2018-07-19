%: src/bin/%.rs
	cargo build --release --bin $*
	cp target/release/$* $@

.PHONY: clean stats stats-lisp

clean:
	cargo clean
	rm -f $(STEPS)
	rm -f mal

stats: $(SOURCES)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"
stats-lisp: $(SOURCES_LISP)
	@wc $^
	@printf "%5s %5s %5s %s\n" `grep -E "^[[:space:]]*//|^[[:space:]]*$$" $^ | wc` "[comments/blanks]"

