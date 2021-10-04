.PHONY: test
test:
	dune exec --force tests/array_like_test.exe

.PHONY: bench_access
bench_access:
	dune exec --force --profile=release tests/bench_access.exe

.PHONY: cover
cover: clean
	dune exec --force --instrument-with bisect_ppx tests/array_like_test.exe
	bisect-ppx-report html
	bisect-ppx-report summary

.PHONY: doc
doc: odoc.css
	rm -rf _doc
	dune build @doc
	cp -r _build/default/_doc _doc
	[ -f odoc.css ] && cp -f odoc.css _doc/_html/odoc.css

.PHONY: clean
clean:
	dune clean
	rm -rf _doc
	rm -rf _coverage
	rm -f bisect*.coverage

.PHONY: width80
width80:
	find . -name '*.ml*' \
	  | grep -e '^./src' -e '^./tests' \
	  | grep -e '\.mli\?$$' \
	  | xargs grep --color -E -e '^.{80,}| $$' \
	  || echo 'OK'

.PHONY: print
print:
	find . -name '*.ml' \
	  | grep -e '^./src' \
	  | xargs grep --color -e 'print' \
	  || echo 'OK'
