release: prod
	mkdir -p bin/
	stack build --copy-bins --local-bin-path bin/
	sudo systemctl restart game-comment

prod:
	cp -f app/Config.prod.hs app/Config.hs


develop: dev
	fd | entr -s "pkill gc-backend; stack run &"

dev:
	cp -f app/Config.dev.hs app/Config.hs
