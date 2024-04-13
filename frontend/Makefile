release: prod
	elm make src/Main.elm --optimize --output=gc.js \
		&& uglifyjs gc.js --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
		| uglifyjs --mangle --output gc.js

prod:
	cp -f Config.prod.elm src/Config.elm


develop: dev serve
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=gc.js

dev:
	cp -f Config.dev.elm src/Config.elm

serve:
	http-server . -p 8000 &
