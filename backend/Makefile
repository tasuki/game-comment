.PHONY: release bin develop dev-config

release: bin
	sudo systemctl stop game-comment || true
	./bin/gc-migrate
	sudo systemctl start game-comment

# This is full of snakes! Instead of building, downloads the v2 binary.
# Haskell needs to go unfortunately...
bin:
	mkdir -p bin/
	# stack build --copy-bins --local-bin-path bin/
	curl -o bin/gc-migrate https://github.com/tasuki/game-comment/releases/download/v2/gc-migrate
	curl -o bin/gc-server https://github.com/tasuki/game-comment/releases/download/v2/gc-server

develop: dev-config
	fd | entr -s "pkill gc-server; stack run gc-server &"

dev-config:
	cp -f .env.dev .env
