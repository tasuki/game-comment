release: prod
	elm make src/Main.elm --optimize --output=gc.js

prod:
	cp -f Config.prod.elm src/Config.elm


develop: dev serve
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=gc.js

dev:
	cp -f Config.dev.elm src/Config.elm

serve:
	php -S localhost:8000 &
