release:
	elm make src/Main.elm --optimize --output=gc.js

develop:
	find . -name '*.elm' | entr elm make src/Main.elm --debug --output=gc.js

serve:
	php -S localhost:8000
