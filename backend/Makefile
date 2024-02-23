release: prod
	stack build

prod:
	cp -f Config.prod.hs Config.hs


develop: dev
	fd | entr -s "pkill game-comment-ba; stack run &"

dev:
	cp -f Config.dev.hs Config.hs
