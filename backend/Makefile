.PHONY: release bin prod-config develop dev-config

release: prod-config bin
	sudo systemctl stop game-comment
	./bin/gc-migrate
	sudo systemctl start game-comment

bin:
	mkdir -p bin/
	stack build --copy-bins --local-bin-path bin/

prod-config:
	cp -f app/Config.prod.hs app/Config.hs


develop: dev-config
	fd | entr -s "pkill gc-server; stack run gc-migrate && stack run gc-server &"

dev-config:
	cp -f app/Config.dev.hs app/Config.hs
