.PHONY: release bin develop dev-config

release: bin
	sudo systemctl stop game-comment
	./bin/gc-migrate
	sudo systemctl start game-comment

bin:
	mkdir -p bin/
	stack build --copy-bins --local-bin-path bin/

develop: dev-config
	fd | entr -s "pkill gc-server; stack run gc-server &"

dev-config:
	cp -f .env.dev .env
