.PHONY: release bin develop dev-config

release: bin
	sudo systemctl stop game-comment
	./bin/gc-migrate
	sudo systemctl start game-comment

# This is full of snakes! Instead of building, downloads the v2 binary.
# Haskell needs to go unfortunately...
bin:
	mkdir -p bin/
	# stack build --copy-bins --local-bin-path bin/
	curl -o bin/gc-migrate https://github.com/tasuki/game-comment/releases/latest/download/gc-migrate
	curl -o bin/gc-server https://github.com/tasuki/game-comment/releases/latest/download/gc-server
	chmod 755 bin/*

develop: dev-config
	fd | entr -s "pkill gc-server; stack run gc-server &"

dev-config:
	cp -f .env.dev .env
