db:
	@docker-compose down
	@docker-compose up -d

db-repl:
	@pgcli -h localhost -p 5432 -u test -W test -d test

clean:
	@git clean -xdf

shell:
	nix-shell -p ghc cabal-install zlib postgresql

ui:
	cd ./dashboard && yarn install && yarn run prod && yarn run lint

gen-nix:
	cabal2nix --no-haddock --no-check . > default.nix

build-img:
	docker build -t hakatime:latest .
