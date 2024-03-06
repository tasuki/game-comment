release: prod
	mkdir -p bin/
	stack build --copy-bins --local-bin-path bin/
	# TODO also restart the systemd service

prod:
	cp -f Config.prod.hs Config.hs


develop: dev
	fd | entr -s "pkill game-comment-ba; stack run &"

dev:
	cp -f Config.dev.hs Config.hs
